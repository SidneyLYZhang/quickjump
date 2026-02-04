{-# LANGUAGE OverloadedStrings #-}

module Config
  ( getConfigPath
  , loadConfig
  , loadConfigFrom
  , saveConfig
  , saveConfigTo
  , ensureConfigExists
  , findEntry
  , expandPath
  , getSortedEntries
  , mergeConfigs
  ) where

import           Control.Exception    (catch, throwIO)
import           Control.Monad        (unless, when)
import           Data.Aeson           (eitherDecode, encode)
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Bifunctor       (first)
import           Data.List            (sortOn)
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.ByteString.Lazy as BL
import           System.Directory     (createDirectoryIfMissing, doesFileExist,
                                       getHomeDirectory)
import           System.Environment   (lookupEnv)
import           System.FilePath      ((</>), takeDirectory)
import           System.IO.Error      (isDoesNotExistError)

import           Types

-- | 获取配置文件路径
getConfigPath :: IO FilePath
getConfigPath = do
  -- 首先检查环境变量
  mEnvPath <- lookupEnv "QUICKJUMP_CONFIG"
  case mEnvPath of
    Just path -> return path
    Nothing   -> do
      -- 默认使用 XDG 配置目录
      xdgConfig <- lookupEnv "XDG_CONFIG_HOME"
      configDir <- case xdgConfig of
        Just dir -> return dir
        Nothing  -> do
          home <- getHomeDirectory
          return $ home </> ".config"
      return $ configDir </> "quickjump" </> "config.json"

-- | 展开路径中的 ~ 和环境变量
expandPath :: FilePath -> IO FilePath
expandPath path = do
  -- 首先处理 ~ 展开
  expanded1 <- if take 1 path == "~"
    then do
      home <- getHomeDirectory
      return $ home </> drop 2 path
    else return path
  -- 然后处理环境变量（支持 Unix $VAR 和 Windows %VAR% 格式）
  expandEnvVars expanded1

-- | 展开环境变量
expandEnvVars :: FilePath -> IO FilePath
expandEnvVars path = do
  -- 处理 Unix 风格的环境变量 $VAR
  let expandUnixVars s = case s of
        '$':'{':rest -> 
          case break (=='}') rest of
            (var, '}':remaining) -> do
              mval <- lookupEnv var
              case mval of
                Just val -> (val ++) <$> expandEnvVars remaining
                Nothing -> (("${" ++ var ++ "}") ++) <$> expandEnvVars remaining
            _ -> ('$':) <$> expandEnvVars rest
        '$':rest -> 
          case span (\c -> isAlphaNum c || c == '_') rest of
            (var, remaining) -> do
              mval <- lookupEnv var
              case mval of
                Just val -> (val ++) <$> expandEnvVars remaining
                Nothing -> (('$' : var) ++) <$> expandEnvVars remaining
        c:cs -> (c:) <$> expandEnvVars cs
        [] -> return []
  -- 处理 Windows 风格的环境变量 %VAR%
  let expandWindowsVars s = case s of
        '%':rest -> 
          case break (=='%') rest of
            (var, '%':remaining) -> do
              mval <- lookupEnv var
              case mval of
                Just val -> (val ++) <$> expandEnvVars remaining
                Nothing -> (('%' : var ++ "%") ++) <$> expandEnvVars remaining
            _ -> ('%':) <$> expandEnvVars rest
        c:cs -> (c:) <$> expandEnvVars cs
        [] -> return []
  -- 根据操作系统选择展开方式
  if '%' `elem` path
    then expandWindowsVars path
    else expandUnixVars path
  where
    isAlphaNum c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

-- | 确保配置文件存在（如果不存在则创建默认配置）
ensureConfigExists :: IO Config
ensureConfigExists = do
  configPath <- getConfigPath
  exists <- doesFileExist configPath
  if exists
    then loadConfig
    else do
      putStrLn $ "Config not found, creating default at: " ++ configPath
      createDirectoryIfMissing True (takeDirectory configPath)
      saveConfig defaultConfig
      return defaultConfig

-- | 加载配置
loadConfig :: IO Config
loadConfig = do
  configPath <- getConfigPath
  loadConfigFrom configPath

-- | 从指定路径加载配置
loadConfigFrom :: FilePath -> IO Config
loadConfigFrom path = do
  expanded <- expandPath path
  result <- catch
    (Right <$> BL.readFile expanded)
    (\e -> if isDoesNotExistError e
           then return $ Left $ "Config file not found: " ++ expanded
           else throwIO e)
  case result of
    Left err   -> error err
    Right bs   ->
      case eitherDecode bs of
        Left err  -> error $ "Failed to parse config: " ++ err
        Right cfg -> return cfg

-- | 保存配置
saveConfig :: Config -> IO ()
saveConfig cfg = do
  configPath <- getConfigPath
  saveConfigTo configPath cfg

-- | 保存配置到指定路径
saveConfigTo :: FilePath -> Config -> IO ()
saveConfigTo path cfg = do
  expanded <- expandPath path
  createDirectoryIfMissing True (takeDirectory expanded)
  BL.writeFile expanded (encodePretty cfg)

-- | 查找条目
findEntry :: Text -> Config -> Maybe JumpEntry
findEntry name cfg = M.lookup name (entries cfg)

-- | 获取按优先级排序的条目列表
getSortedEntries :: Config -> [(Text, JumpEntry)]
getSortedEntries cfg =
  sortOn (priority . snd) $ M.toList (entries cfg)

-- | 合并两个配置（用于导入）
mergeConfigs :: Config -> Config -> Bool -> Config
mergeConfigs base new shouldMerge =
  if shouldMerge
    then base { entries = M.union (entries new) (entries base) }
    else new
