{-# LANGUAGE OverloadedStrings #-}

module Commands
  ( runCommand
  , runJump
  , runQuick
  , runConfigCmd
  , printShellIntegration
  ) where

import           Control.Monad        (forM_, unless, when)
import           Data.List            (intercalate)
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe, isJust)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           System.Directory     (doesDirectoryExist, doesFileExist)
import           System.Environment   (lookupEnv)
import           System.Exit          (exitFailure, exitSuccess)
import           System.FilePath      ((</>))
import           System.Info          (os)
import           System.IO            (hFlush, stdout)
import           System.Process       (callCommand, spawnCommand, waitForProcess)

import           Config
import           Types
import           Utils

-- | 运行主命令
runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  Jump name quiet           -> runJump name quiet
  JumpInteractive quiet     -> runJumpInteractive quiet
  Quick action quiet        -> runQuick action quiet
  ConfigCmd action quiet    -> runConfigCmd action quiet
  ShellIntegration          -> printShellIntegration
  Version                   -> putStrLn "quickjump version 0.1.0.0"

-- | 跳转到指定条目
runJump :: Text -> Bool -> IO ()
runJump name quiet = do
  cfg <- ensureConfigExists
  case findEntry name cfg of
    Nothing    -> do
      unless quiet $ do
        putStrLn $ "Unknown jump target: " ++ T.unpack name
        putStrLn "Use 'quickjump config list' to see available targets"
      exitFailure
    Just entry -> do
      expanded <- expandPath (path entry)
      exists <- doesDirectoryExist expanded
      if exists
        then do
          -- 输出 cd 命令供 shell 执行
          -- 使用引号包裹路径以处理包含空格的路径
          let cdCmd = case os of
                "mingw32" -> "cd \"" ++ expanded ++ "\""
                "mingw64" -> "cd \"" ++ expanded ++ "\""
                _         -> "cd " ++ show expanded
          putStrLn cdCmd
        else do
          unless quiet $ putStrLn $ "Directory does not exist: " ++ expanded
          exitFailure

-- | 交互式选择跳转
runJumpInteractive :: Bool -> IO ()
runJumpInteractive quiet = do
  cfg <- ensureConfigExists
  let sorted = getSortedEntries cfg
  if null sorted
    then unless quiet $ do
      putStrLn "No jump targets configured"
      putStrLn "Use 'quickjump config add <name> <path>' to add one"
    else do
      unless quiet $ do
        putStrLn "Available jump targets:"
        putStrLn ""
        forM_ (zip [1..] sorted) $ \(i, (name, entry)) -> do
          let desc = fromMaybe "" (description entry)
          putStrLn $ "  " ++ show (i :: Int) ++ ". " ++ T.unpack name
                  ++ " -> " ++ path entry
                  ++ if T.null desc then "" else " (" ++ T.unpack desc ++ ")"
      putStr "\nSelect target (number or name): "
      hFlush stdout
      selection <- getLine
      case reads selection of
        [(n, "")] | n > 0 && n <= length sorted ->
          runJump (fst $ sorted !! (n - 1)) quiet
        _ -> runJump (T.pack selection) quiet

-- | 运行快速操作
runQuick :: QuickAction -> Bool -> IO ()
runQuick action quiet = do
  cfg <- ensureConfigExists
  case action of
    QuickOpen name -> do
      case findEntry name cfg of
        Nothing    -> do
          unless quiet $ putStrLn $ "Unknown quick target: " ++ T.unpack name
          exitFailure
        Just entry -> openPath (path entry) cfg quiet

    QuickOpenPath p -> openPath p cfg quiet

    QuickList -> do
      let sorted = getSortedEntries cfg
      unless quiet $ do
        putStrLn "Quick access targets:"
        putStrLn ""
        forM_ sorted $ \(name, entry) -> do
          let desc = fromMaybe "" (description entry)
          putStrLn $ "  " ++ padRight 15 (T.unpack name)
                  ++ " -> " ++ padRight 30 (path entry)
                  ++ if T.null desc then "" else " # " ++ T.unpack desc

    QuickDefault ->
      case defaultPath cfg of
        Nothing  -> do
          unless quiet $ do
            putStrLn "No default path configured"
            putStrLn "Use 'quickjump config set-default <path>' to set one"
          exitFailure
        Just p   -> openPath p cfg quiet

-- | 打开路径（使用文件管理器或 cd）
openPath :: FilePath -> Config -> Bool -> IO ()
openPath p cfg quiet = do
  expanded <- expandPath p
  exists <- doesDirectoryExist expanded
  unless exists $ do
    unless quiet $ putStrLn $ "Directory does not exist: " ++ expanded
    exitFailure

  -- 尝试使用配置的文件管理器，或者自动检测
  let fm = fileManager cfg
  case fm of
    Just cmd -> runFileManager cmd expanded quiet
    Nothing  -> autoDetectAndOpen expanded quiet

-- | 自动检测并打开文件管理器
autoDetectAndOpen :: FilePath -> Bool -> IO ()
autoDetectAndOpen path quiet = do
  let (cmd, args) = case os of
        "darwin"  -> ("open", [path])
        "mingw32" -> ("explorer", [path])
        "mingw64" -> ("explorer", [path])
        "cygwin"  -> ("cygstart", [path])
        _         -> ("xdg-open", [path])  -- Linux and others

  -- 检查命令是否存在
  exists <- commandExists cmd
  if exists
    then do
      _ <- spawnCommand (unwords (cmd : map show args)) >>= waitForProcess
      return ()
    else do
      unless quiet $ do
        putStrLn $ "Cannot open file manager. Please configure one:"
        putStrLn $ "  quickjump config set-file-manager <command>"
        -- 输出 cd 命令作为备选
        putStrLn $ "cd " ++ show path

-- | 运行文件管理器
runFileManager :: FilePath -> FilePath -> Bool -> IO ()
runFileManager cmd path quiet = do
  expanded <- expandPath path
  let fullCmd = cmd ++ " " ++ show expanded
  _ <- spawnCommand fullCmd >>= waitForProcess
  return ()

-- | 运行配置命令
runConfigCmd :: ConfigAction -> Bool -> IO ()
runConfigCmd action quiet = do
  cfg <- ensureConfigExists
  case action of
    ConfigAdd name path mDesc -> do
      expanded <- expandPath path
      exists <- doesDirectoryExist expanded
      unless exists $ do
        unless quiet $ putStrLn $ "Warning: Directory does not exist: " ++ expanded
      let entry = JumpEntry
            { path = path
            , description = mDesc
            , priority = 100
            }
          newCfg = cfg { entries = M.insert name entry (entries cfg) }
      saveConfig newCfg
      unless quiet $ putStrLn $ "Added '" ++ T.unpack name ++ "' -> " ++ path

    ConfigRemove name -> do
      if M.member name (entries cfg)
        then do
          let newCfg = cfg { entries = M.delete name (entries cfg) }
          saveConfig newCfg
          unless quiet $ putStrLn $ "Removed '" ++ T.unpack name ++ "'"
        else do
          unless quiet $ putStrLn $ "No such entry: '" ++ T.unpack name ++ "'"
          exitFailure

    ConfigList -> do
      let sorted = getSortedEntries cfg
      unless quiet $ do
        if null sorted
          then putStrLn "No entries configured"
          else do
            putStrLn "Configured jump entries:"
            putStrLn ""
            forM_ sorted $ \(name, entry) -> do
              let desc = fromMaybe "" (description entry)
              putStrLn $ "  " ++ padRight 15 (T.unpack name)
                      ++ " -> " ++ padRight 30 (path entry)
                      ++ if T.null desc then "" else " # " ++ T.unpack desc

    ConfigSetDefault path -> do
      expanded <- expandPath path
      exists <- doesDirectoryExist expanded
      unless exists $ do
        unless quiet $ putStrLn $ "Warning: Directory does not exist: " ++ expanded
      let newCfg = cfg { defaultPath = Just path }
      saveConfig newCfg
      unless quiet $ putStrLn $ "Set default path to: " ++ path

    ConfigSetEditor cmd -> do
      let newCfg = cfg { editor = Just cmd }
      saveConfig newCfg
      unless quiet $ putStrLn $ "Set editor to: " ++ cmd

    ConfigSetFileManager cmd -> do
      let newCfg = cfg { fileManager = Just cmd }
      saveConfig newCfg
      unless quiet $ putStrLn $ "Set file manager to: " ++ cmd

    ConfigExport path -> do
      saveConfigTo path cfg
      unless quiet $ putStrLn $ "Config exported to: " ++ path

    ConfigImport path merge -> do
      imported <- loadConfigFrom path
      let merged = mergeConfigs cfg imported merge
      saveConfig merged
      unless quiet $
        if merge
          then putStrLn "Config imported and merged successfully"
          else putStrLn "Config imported (replaced existing)"

    ConfigEdit -> do
      let ed = fromMaybe (defaultEditor os) (editor cfg)
      configPath <- getConfigPath
      _ <- spawnCommand (ed ++ " " ++ show configPath) >>= waitForProcess
      return ()

    ConfigShow -> do
      configPath <- getConfigPath
      unless quiet $ do
        putStrLn $ "Config location: " ++ configPath
        putStrLn $ "Version: " ++ T.unpack (version cfg)
        putStrLn $ "Entries: " ++ show (M.size $ entries cfg)
        putStrLn $ "Default path: " ++ fromMaybe "(not set)" (defaultPath cfg)
        putStrLn $ "Editor: " ++ fromMaybe "(not set)" (editor cfg)
        putStrLn $ "File manager: " ++ fromMaybe "(auto-detect)" (fileManager cfg)

-- | 获取默认编辑器
defaultEditor :: String -> String
defaultEditor platform = case platform of
  "darwin"  -> "open -t"
  "mingw32" -> "notepad"
  "mingw64" -> "notepad"
  _         -> "vi"

-- | 打印 shell 集成脚本
printShellIntegration :: IO ()
printShellIntegration = do
  putStrLn $ shellScript os

-- | 获取对应 shell 的集成脚本
shellScript :: String -> String
shellScript platform = case platform of
  "mingw32" -> windowsPowerShellScript
  "mingw64" -> windowsPowerShellScript
  "cygwin"  -> bashScript
  "darwin"  -> bashScript
  _         -> bashScript

-- | Bash/Zsh 集成脚本
bashScript :: String
bashScript = intercalate "\n"
  [ "# QuickJump Shell Integration for Bash/Zsh"
  , "# Add this to your shell profile (.bashrc, .zshrc, etc.)"
  , "#"
  , "# eval \"$(quickjump shell-integration)\""
  , ""
  , "# Bash/Zsh function for directory jumping"
  , "qj() {"
  , "    local output=$(quickjump jump \"$1\")"
  , "    if [[ $output == cd* ]]; then"
  , "        eval \"$output\""
  , "    else"
  , "        echo \"$output\""
  , "    fi"
  , "}"
  , ""
  , "# Quick open function"
  , "qo() {"
  , "    quickjump quick \"$1\""
  , "}"
  , ""
  , "# Quiet mode function"
  , "qjq() {"
  , "    quickjump --quiet jump \"$1\""
  , "}"
  , ""
  , "# Tab completion for bash"
  , "if [ -n \"$BASH_VERSION\" ]; then"
  , "    _qj_complete() {"
  , "        local cur=\"${COMP_WORDS[COMP_CWORD]}\""
  , "        local entries=$(quickjump config list 2>/dev/null | grep '^  ' | awk '{print $1}')"
  , "        COMPREPLY=($(compgen -W \"$entries\" -- \"$cur\"))"
  , "    }"
  , "    complete -F _qj_complete qj"
  , "    complete -F _qj_complete qjq"
  , "fi"
  , ""
  , "# Tab completion for zsh"
  , "if [ -n \"$ZSH_VERSION\" ]; then"
  , "    _qj_complete() {"
  , "        local -a entries"
  , "        entries=(${(f)\"$(quickjump config list 2>/dev/null | grep '^  ' | awk '{print $1}')\"})"
  , "        _describe 'jump targets' entries"
  , "    }"
  , "    compdef _qj_complete qj"
  , "    compdef _qj_complete qjq"
  , "fi"
  ]

-- | Windows PowerShell 集成脚本
windowsPowerShellScript :: String
windowsPowerShellScript = intercalate "\n"
  [ "# QuickJump Shell Integration for PowerShell"
  , "# Add this to your PowerShell profile ($PROFILE)"
  , "#"
  , "# To edit your profile: notepad $PROFILE"
  , "#"
  , "# . (quickjump shell-integration | Out-String)"
  , ""
  , "# Function for directory jumping"
  , "function qj {"
  , "    param([string]$name)"
  , "    $output = quickjump jump $name"
  , "    if ($output -like 'cd *') {"
  , "        # Remove 'cd ' prefix and execute"
  , "        $path = $output -replace '^cd \"?([^\"\"]*)\"?$', '$1'"
  , "        Set-Location $path"
  , "    } else {"
  , "        Write-Output $output"
  , "    }"
  , "}"
  , ""
  , "# Quiet mode function"
  , "function qjq {"
  , "    param([string]$name)"
  , "    $output = quickjump --quiet jump $name"
  , "    if ($output -like 'cd *') {"
  , "        $path = $output -replace '^cd \"?([^\"\"]*)\"?$', '$1'"
  , "        Set-Location $path"
  , "    }"
  , "}"
  , ""
  , "# Quick open function"
  , "function qo {"
  , "    param([string]$name)"
  , "    quickjump quick $name"
  , "}"
  , ""
  , "# Tab completion"
  , "Register-ArgumentCompleter -CommandName qj -ScriptBlock {"
  , "    param($commandName, $parameterName, $wordToComplete, $commandAst, $fakeBoundParameter)"
  , "    $entries = quickjump config list 2>$null | Select-String '^  ' | ForEach-Object {"
  , "        $_.ToString().Trim().Split()[0]"
  , "    }"
  , "    $entries | Where-Object { $_ -like \"$wordToComplete*\" } | ForEach-Object {"
  , "        [System.Management.Automation.CompletionResult]::new($_, $_, 'ParameterValue', $_)"
  , "    }"
  , "}"
  , ""
  , "Register-ArgumentCompleter -CommandName qjq -ScriptBlock {"
  , "    param($commandName, $parameterName, $wordToComplete, $commandAst, $fakeBoundParameter)"
  , "    $entries = quickjump config list 2>$null | Select-String '^  ' | ForEach-Object {"
  , "        $_.ToString().Trim().Split()[0]"
  , "    }"
  , "    $entries | Where-Object { $_ -like \"$wordToComplete*\" } | ForEach-Object {"
  , "        [System.Management.Automation.CompletionResult]::new($_, $_, 'ParameterValue', $_)"
  , "    }"
  , "}"
  ]
