{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( Config(..)
  , JumpEntry(..)
  , Command(..)
  , QuickAction(..)
  , ConfigAction(..)
  , defaultConfig
  , emptyConfig
  ) where

import           Data.Aeson
import           Data.Map    (Map)
import qualified Data.Map    as M
import           Data.Text   (Text)
import           GHC.Generics

-- | 单个跳转条目
data JumpEntry = JumpEntry
  { path        :: FilePath  -- ^ 目标路径
  , description :: Maybe Text -- ^ 可选描述
  , priority    :: Int       -- ^ 优先级（数字越小越优先）
  } deriving (Show, Eq, Generic)

instance ToJSON JumpEntry where
  toJSON entry = object
    [ "path"        .= path entry
    , "description" .= description entry
    , "priority"    .= priority entry
    ]

instance FromJSON JumpEntry where
  parseJSON = withObject "JumpEntry" $ \v -> JumpEntry
    <$> v .:  "path"
    <*> v .:? "description"
    <*> v .:? "priority" .!= 100

-- | 配置文件结构
data Config = Config
  { version     :: Text                    -- ^ 配置版本
  , entries     :: Map Text JumpEntry      -- ^ 命名跳转条目
  , defaultPath :: Maybe FilePath          -- ^ 默认打开路径
  , editor      :: Maybe FilePath          -- ^ 首选编辑器
  , fileManager :: Maybe FilePath          -- ^ 首选文件管理器
  } deriving (Show, Eq, Generic)

instance ToJSON Config where
  toJSON cfg = object
    [ "version"      .= version cfg
    , "entries"      .= entries cfg
    , "default_path" .= defaultPath cfg
    , "editor"       .= editor cfg
    , "file_manager" .= fileManager cfg
    ]

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
    <$> v .:? "version" .!= "1.0"
    <*> v .:? "entries" .!= M.empty
    <*> v .:? "default_path"
    <*> v .:? "editor"
    <*> v .:? "file_manager"

-- | 快速操作类型
data QuickAction
  = QuickOpen Text           -- ^ 打开配置中的指定条目
  | QuickOpenPath FilePath   -- ^ 打开指定路径
  | QuickList                -- ^ 列出所有快速条目
  | QuickDefault             -- ^ 打开默认路径
  deriving (Show, Eq)

-- | 配置操作类型
data ConfigAction
  = ConfigAdd Text FilePath (Maybe Text)  -- ^ 添加条目: 名称 路径 [描述]
  | ConfigRemove Text                     -- ^ 删除条目
  | ConfigList                            -- ^ 列出所有条目
  | ConfigSetDefault FilePath             -- ^ 设置默认路径
  | ConfigSetEditor FilePath              -- ^ 设置编辑器
  | ConfigSetFileManager FilePath         -- ^ 设置文件管理器
  | ConfigExport FilePath                 -- ^ 导出配置到文件
  | ConfigImport FilePath Bool            -- ^ 导入配置 (文件路径, 是否合并)
  | ConfigEdit                            -- ^ 用编辑器打开配置文件
  | ConfigShow                            -- ^ 显示当前配置
  deriving (Show, Eq)

-- | 主命令类型
data Command
  = Jump Text Bool           -- ^ 跳转到指定条目 (名称, 是否静默)
  | JumpInteractive Bool     -- ^ 交互式选择跳转 (是否静默)
  | Quick QuickAction Bool   -- ^ 快速操作 (操作, 是否静默)
  | ConfigCmd ConfigAction Bool -- ^ 配置操作 (操作, 是否静默)
  | ShellIntegration         -- ^ 输出 shell 集成脚本
  | Version                  -- ^ 显示版本
  deriving (Show, Eq)

-- | 空配置
emptyConfig :: Config
emptyConfig = Config
  { version     = "1.0"
  , entries     = M.empty
  , defaultPath = Nothing
  , editor      = Nothing
  , fileManager = Nothing
  }

-- | 默认配置（带示例）
defaultConfig :: Config
defaultConfig = Config
  { version     = "1.0"
  , entries     = M.fromList
      [ ("home", JumpEntry
          { path = "~"
          , description = Just "Home directory"
          , priority = 1
          })
      , ("docs", JumpEntry
          { path = "~/Documents"
          , description = Just "Documents folder"
          , priority = 2
          })
      , ("downloads", JumpEntry
          { path = "~/Downloads"
          , description = Just "Downloads folder"
          , priority = 3
          })
      ]
  , defaultPath = Just "~"
  , editor      = Just "vim"
  , fileManager = Nothing
  }
