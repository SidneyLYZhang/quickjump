{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import           Data.Text            (Text)
-- import qualified Data.Text            as T
import           Options.Applicative
import           System.Environment   (getArgs, withArgs)
-- import           System.IO            (hPutStrLn, stderr)

import           Commands
import           Types

-- | 主函数
main :: IO ()
main = do
  args <- getArgs
  -- 如果没有参数，显示帮助
  if null args
    then withArgs ["--help"] runParser
    else runParser
  where
    runParser = do
      cmd <- execParser opts
      runCommand cmd

    opts = info (helper <*> versionOption <*> commandParser)
      ( fullDesc
     <> progDesc "QuickJump - Fast directory navigation tool"
     <> header "quickjump - A command line tool for quick directory jumping" )

-- | 静默模式选项
quietOption :: Parser Bool
quietOption = switch
  ( long "quiet"
  <> short 'q'
  <> help "Suppress output messages (quiet mode)" )

-- | 版本选项
versionOption :: Parser (a -> a)
versionOption = infoOption "quickjump 0.3.0.1"
  ( long "version"
  <> short 'v'
  <> help "Show version information" )

-- | 主命令解析器
commandParser :: Parser Command
commandParser = subparser
  ( command "jump" (info jumpParser
      ( progDesc "Jump to a configured directory" ))
  <> command "j" (info jumpParser
      ( progDesc "Alias for jump" ))
  <> command "quick" (info quickParser
      ( progDesc "Quick open a directory" ))
  <> command "k" (info quickParser
      ( progDesc "Alias for quick" ))
  <> command "config" (info configParser
      ( progDesc "Manage configuration" ))
  <> command "c" (info configParser
      ( progDesc "Alias for config" ))
  <> command "shell-integration" (info shellIntegrationParser
      ( progDesc "Output shell integration script" ))
  )
  <|> jumpParser  -- 默认命令是 jump

-- | 跳转命令解析器
jumpParser :: Parser Command
jumpParser = (Jump <$> argument str
      ( metavar "NAME"
     <> help "Name of the jump target" ))
  <*> quietOption
  <|> (JumpInteractive <$> flag' False
        ( long "interactive"
       <> short 'i'
       <> help "Interactive mode - select from list" ))

-- | 快速命令解析器
quickParser :: Parser Command
quickParser = (Quick <$> subparser
      ( command "open" (info (QuickOpen <$> argument str (metavar "NAME"))
          ( progDesc "Open a configured directory" ))
     <> command "list" (info (pure QuickList)
          ( progDesc "List all quick access targets" ))
     <> command "default" (info (pure QuickDefault)
          ( progDesc "Open the default directory" ))
     )
  <*> quietOption)
  <|> (Quick <$> (QuickOpen <$> strOption
          ( long "open"
         <> short 'o'
         <> metavar "NAME"
         <> help "Open the specified target" ))
      <*> quietOption)
  <|> (Quick <$> (QuickOpenPath <$> strOption
          ( long "path"
         <> short 'p'
         <> metavar "PATH"
         <> help "Open the specified path" ))
      <*> quietOption)
  <|> (Quick <$> flag' QuickList
          ( long "list"
         <> short 'l'
         <> help "List all targets" )
      <*> quietOption)
  <|> (Quick <$> flag' QuickDefault
          ( long "default"
         <> short 'd'
         <> help "Open default directory" )
      <*> quietOption)
  <|> (Quick <$> (QuickOpen <$> argument str (metavar "NAME" <> help "Target name or path"))
      <*> quietOption)

-- | 配置命令解析器
configParser :: Parser Command
configParser = ConfigCmd <$> subparser
      ( command "add" (info addParser
          ( progDesc "Add a new jump entry" ))
     <> command "remove" (info removeParser
          ( progDesc "Remove a jump entry" ))
     <> command "rm" (info removeParser
          ( progDesc "Alias for remove" ))
     <> command "list" (info (pure ConfigList)
          ( progDesc "List all entries" ))
     <> command "ls" (info (pure ConfigList)
          ( progDesc "Alias for list" ))
     <> command "set-default" (info setDefaultParser
          ( progDesc "Set the default path" ))
     <> command "set-editor" (info setEditorParser
          ( progDesc "Set the preferred editor" ))
     <> command "set-file-manager" (info setFileManagerParser
          ( progDesc "Set the preferred file manager" ))
     <> command "export" (info exportParser
          ( progDesc "Export configuration to file" ))
     <> command "import" (info importParser
          ( progDesc "Import configuration from file" ))
     <> command "edit" (info (pure ConfigEdit)
          ( progDesc "Edit configuration with editor" ))
     <> command "show" (info (pure ConfigShow)
          ( progDesc "Show current configuration" ))
     )
  <*> quietOption

-- | 添加条目解析器
addParser :: Parser ConfigAction
addParser = ConfigAdd
  <$> argument str (metavar "NAME" <> help "Entry name")
  <*> argument str (metavar "PATH" <> help "Directory path")
  <*> optional (strOption
      ( long "description"
      <> short 'd'
      <> metavar "DESC"
      <> help "Optional description" ))

-- | 删除条目解析器
removeParser :: Parser ConfigAction
removeParser = ConfigRemove
  <$> argument str (metavar "NAME" <> help "Entry name to remove")

-- | 设置默认路径解析器
setDefaultParser :: Parser ConfigAction
setDefaultParser = ConfigSetDefault
  <$> argument str (metavar "PATH" <> help "Default directory path")

-- | 设置编辑器解析器
setEditorParser :: Parser ConfigAction
setEditorParser = ConfigSetEditor
  <$> argument str (metavar "COMMAND" <> help "Editor command")

-- | 设置文件管理器解析器
setFileManagerParser :: Parser ConfigAction
setFileManagerParser = ConfigSetFileManager
  <$> argument str (metavar "COMMAND" <> help "File manager command")

-- | 导出配置解析器
exportParser :: Parser ConfigAction
exportParser = ConfigExport
  <$> argument str (metavar "FILE" <> help "Export file path")

-- | 导入配置解析器
importParser :: Parser ConfigAction
importParser = ConfigImport
  <$> argument str (metavar "FILE" <> help "Import file path")
  <*> switch
      ( long "merge"
      <> short 'm'
      <> help "Merge with existing config instead of replacing" )

-- | Shell 集成解析器
shellIntegrationParser :: Parser Command
shellIntegrationParser = pure ShellIntegration
