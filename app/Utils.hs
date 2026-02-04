{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( padRight
  , commandExists
  , formatTable
  , truncatePath
  ) where

import           Control.Exception  (catch)
import           Data.List          (intercalate, transpose)
import           System.Directory   (findExecutable)
import           System.IO.Error    (isDoesNotExistError)

-- | 右填充字符串到指定长度
padRight :: Int -> String -> String
padRight n s = s ++ replicate (max 0 (n - length s)) ' '

-- | 左填充字符串到指定长度
padLeft :: Int -> String -> String
padLeft n s = replicate (max 0 (n - length s)) ' ' ++ s

-- | 检查命令是否存在
commandExists :: String -> IO Bool
commandExists cmd = do
  result <- findExecutable cmd
  return $ case result of
    Just _  -> True
    Nothing -> False

-- | 截断路径显示
truncatePath :: Int -> String -> String
truncatePath maxLen path
  | length path <= maxLen = path
  | otherwise             = "..." ++ drop (length path - maxLen + 3) path

-- | 格式化表格
data TableCell = TableCell String Int  -- ^ 内容和对齐宽度

formatTable :: [[String]] -> String
formatTable rows =
  let -- 计算每列的最大宽度
      colWidths = map maximum $ transpose
                  [ map length row | row <- rows ]
      -- 格式化每一行
      formatRow row = intercalate "  "
        [ padRight w cell | (cell, w) <- zip row colWidths ]
  in intercalate "\n" $ map formatRow rows

-- | 安全的读取文件
safeReadFile :: FilePath -> IO (Maybe String)
safeReadFile path = do
  result <- catch
    (Just <$> readFile path)
    (\e -> if isDoesNotExistError e then return Nothing else return Nothing)
  return result

-- | 字符串居中
center :: Int -> String -> String
center width s =
  let padding = max 0 (width - length s)
      leftPad = padding `div` 2
      rightPad = padding - leftPad
  in replicate leftPad ' ' ++ s ++ replicate rightPad ' '

-- | 重复字符串
repeatString :: Int -> String -> String
repeatString n = concat . replicate n

-- | 高亮文本（终端颜色）
highlight :: String -> String
highlight s = "\ESC[1m" ++ s ++ "\ESC[0m"

-- | 绿色文本
green :: String -> String
green s = "\ESC[32m" ++ s ++ "\ESC[0m"

-- | 黄色文本
yellow :: String -> String
yellow s = "\ESC[33m" ++ s ++ "\ESC[0m"

-- | 红色文本
red :: String -> String
red s = "\ESC[31m" ++ s ++ "\ESC[0m"

-- | 蓝色文本
blue :: String -> String
blue s = "\ESC[34m" ++ s ++ "\ESC[0m"
