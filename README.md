# QuickJump

一个使用 Haskell 编写的快速目录跳转命令行工具，支持配置管理、快速打开和导入导出功能。

## 功能特性

- **快速目录跳转** - 根据配置快速跳转到常用目录
- **快速打开** - 使用文件管理器快速打开文件夹
- **配置管理** - 添加、删除、修改配置条目
- **导入导出** - 备份和分享配置
- **Shell 集成** - 提供 Bash/Zsh/PowerShell 补全支持
- **交互式选择** - 支持交互式选择跳转目标
- **静默模式** - 抑制输出消息，适合脚本使用
- **跨平台支持** - 支持 Windows、macOS 和 Linux

## 源码

- [Github](https://github.com/sidneylyzhang/quickjump)
- [Gitea](https://git.lyz.one/sidneyzhang/quickjump)

## 安装

### 使用 Cabal

```bash
git clone https://git.lyz.one/sidneyzhang/quickjump.git
cd quickjump
cabal build
cabal install
```

### 使用 Makefile

项目提供了 Makefile 来简化构建和压缩过程：

```bash
git clone https://git.lyz.one/sidneyzhang/quickjump.git
cd quickjump

# 构建并压缩（推荐）
make

# 或者分步执行
make build    # 仅构建
make compress # 压缩可执行文件
make install  # 安装
```

### 关于 UPX 压缩

Makefile 的 `compress` 目标会使用 UPX（Ultimate Packer for eXecutables）来压缩可执行文件，显著减小文件体积：

- 如果系统已安装 UPX，直接使用本地 UPX 进行压缩
- 如果系统未安装 UPX，会自动使用 `npx` 下载并运行最新版本的 UPX

UPX 压缩参数：
- `--best`：使用最佳压缩级别
- `--lzma`：使用 LZMA 算法（压缩率更高）

**注意**：使用 `npx` 运行 UPX 需要 Node.js 和 npm 环境。如果不想使用 UPX 压缩，可以直接运行 `make build` 或 `cabal build`。

查看所有可用的 Makefile 目标：

```bash
make help
```

## 配置 Shell 集成

### Linux/macOS (Bash/Zsh)

将以下命令添加到你的 `.bashrc` 或 `.zshrc`：

```bash
eval "$(quickjump shell-integration)"
```

### Windows (PowerShell)

将以下命令添加到你的 PowerShell 配置文件中：

```powershell
# 首先查看你的配置文件路径
echo $PROFILE

# 如果文件不存在，创建它
if (!(Test-Path $PROFILE)) { New-Item -Type File -Path $PROFILE -Force }

# 编辑配置文件
notepad $PROFILE
```

然后在配置文件中添加：

```powershell
. (quickjump shell-integration | Out-String)
```

这将启用：
- `qj` 命令用于目录跳转
- `qo` 命令用于快速打开
- Tab 自动补全（PowerShell 也支持）

## 使用方法

### 目录跳转

```bash
# 跳转到配置中的条目
quickjump jump home
quickjump j home

# 使用 Shell 集成函数（需要先配置 Shell 集成）
qj home

# 交互式选择跳转目标
quickjump jump --interactive
quickjump j -i

# 静默模式（不输出消息）
quickjump jump home --quiet
quickjump j home -q
```

### 快速打开

```bash
# 打开配置中的目录（使用文件管理器）
quickjump quick open home
quickjump k open home

# 使用 Shell 集成函数
qo home

# 打开指定路径
quickjump quick --path ~/Documents
quickjump k -p ~/Documents

# 打开默认目录
quickjump quick default
quickjump k -d

# 列出所有快速目标
quickjump quick list
quickjump k -l

# 静默模式
quickjump quick open home --quiet
quickjump k open home -q
```

### 配置管理

```bash
# 添加新条目
quickjump config add myproject ~/Projects/myproject
quickjump config add work ~/Work --description "Work directory"
quickjump c add myproject ~/Projects/myproject

# 列出所有条目
quickjump config list
quickjump config ls
quickjump c list

# 删除条目
quickjump config remove myproject
quickjump config rm myproject
quickjump c remove myproject

# 设置默认路径
quickjump config set-default ~/Documents
quickjump c set-default ~/Documents

# 设置首选编辑器
quickjump config set-editor vim
quickjump config set-editor "code --wait"
quickjump c set-editor vim

# 设置首选文件管理器
quickjump config set-file-manager nautilus  # Linux
quickjump config set-file-manager finder    # macOS
quickjump config set-file-manager explorer  # Windows
quickjump c set-file-manager explorer

# 编辑配置文件
quickjump config edit
quickjump c edit

# 显示当前配置
quickjump config show
quickjump c show

# 静默模式
quickjump config add myproject ~/Projects/myproject --quiet
quickjump c add myproject ~/Projects/myproject -q
```

### 导入导出

```bash
# 导出配置
quickjump config export ~/backup/quickjump-config.json
quickjump c export ~/backup/quickjump-config.json

# 导入配置（替换现有）
quickjump config import ~/backup/quickjump-config.json
quickjump c import ~/backup/quickjump-config.json

# 导入配置（合并）
quickjump config import ~/backup/quickjump-config.json --merge
quickjump config import ~/backup/quickjump-config.json -m
quickjump c import ~/backup/quickjump-config.json -m
```

### 其他命令

```bash
# 显示版本信息
quickjump --version
quickjump -v

# 显示帮助信息
quickjump --help
quickjump -h

# 显示特定命令的帮助
quickjump jump --help
quickjump config --help
```

## 配置文件

配置文件默认位于：

- **Linux/macOS**: `~/.config/quickjump/config.json`
- **Windows**: `%APPDATA%\quickjump\config.json`

可以通过环境变量 `QUICKJUMP_CONFIG` 自定义配置文件路径：

```bash
# Linux/macOS
export QUICKJUMP_CONFIG=/path/to/custom/config.json

# Windows PowerShell
$env:QUICKJUMP_CONFIG = "C:\path\to\custom\config.json"
```

### 配置格式

```json
{
  "version": "1.0",
  "entries": {
    "home": {
      "path": "~",
      "description": "Home directory",
      "priority": 1
    },
    "docs": {
      "path": "~/Documents",
      "description": "Documents folder",
      "priority": 2
    },
    "downloads": {
      "path": "~/Downloads",
      "description": "Downloads folder",
      "priority": 3
    }
  },
  "default_path": "~",
  "editor": "vim",
  "file_manager": null
}
```

### 配置字段说明

- `version`: 配置文件版本号
- `entries`: 跳转条目映射表
  - `path`: 目标路径（支持 `~` 和环境变量）
  - `description`: 可选描述信息
  - `priority`: 优先级（数字越小越优先）
- `default_path`: 默认打开的路径
- `editor`: 首选编辑器命令
- `file_manager`: 首选文件管理器命令

## 命令速查表

### 主命令

| 命令 | 简写 | 说明 |
|------|------|------|
| `quickjump jump <name>` | `quickjump j <name>` | 跳转到指定目录 |
| `quickjump jump --interactive` | `quickjump j -i` | 交互式选择跳转 |
| `quickjump quick open <name>` | `quickjump k open <name>` | 快速打开目录 |
| `quickjump quick --path <path>` | `quickjump k -p <path>` | 打开指定路径 |
| `quickjump quick default` | `quickjump k -d` | 打开默认目录 |
| `quickjump quick list` | `quickjump k -l` | 列出快速目标 |
| `quickjump config <action>` | `quickjump c <action>` | 配置管理 |
| `quickjump shell-integration` | - | 输出 Shell 集成脚本 |

### 全局选项

| 选项 | 简写 | 说明 |
|------|------|------|
| `--version` | `-v` | 显示版本信息 |
| `--help` | `-h` | 显示帮助信息 |
| `--quiet` | `-q` | 静默模式（抑制输出） |

### 配置子命令

| 命令 | 简写 | 说明 |
|------|------|------|
| `quickjump config add <name> <path>` | - | 添加条目 |
| `quickjump config remove <name>` | `quickjump config rm <name>` | 删除条目 |
| `quickjump config list` | `quickjump config ls` | 列出所有条目 |
| `quickjump config set-default <path>` | - | 设置默认路径 |
| `quickjump config set-editor <cmd>` | - | 设置编辑器 |
| `quickjump config set-file-manager <cmd>` | - | 设置文件管理器 |
| `quickjump config export <file>` | - | 导出配置 |
| `quickjump config import <file>` | - | 导入配置 |
| `quickjump config import <file> --merge` | `quickjump config import <file> -m` | 合并导入配置 |
| `quickjump config edit` | - | 编辑配置文件 |
| `quickjump config show` | - | 显示当前配置 |

### Shell 集成函数

| 函数 | 说明 |
|------|------|
| `qj <name>` | 跳转到指定目录 |
| `qjq <name>` | 静默模式跳转 |
| `qo <name>` | 快速打开目录 |

## 项目结构

```
quickjump/
├── quickjump.cabal    # Cabal 配置文件
├── CHANGELOG.md       # 版本变更记录
├── README.md          # 说明文档
└── app/
    ├── Main.hs        # 程序入口和 CLI 解析
    ├── Types.hs       # 数据类型定义
    ├── Config.hs      # 配置管理
    ├── Commands.hs    # 命令实现
    └── Utils.hs       # 工具函数
```

## 依赖

- base >= 4.18
- aeson >= 2.2
- aeson-pretty >= 0.8
- optparse-applicative >= 0.18
- directory >= 1.3
- filepath >= 1.4
- process >= 1.6
- text >= 2.0
- bytestring >= 0.11
- containers >= 0.6

## 许可证

MIT License

## 更新日志

查看 [CHANGELOG.md](CHANGELOG.md) 了解详细的版本更新历史。