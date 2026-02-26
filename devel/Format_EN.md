# Code Formatting Guide

## Environment Setup
Requires clang-format version 19.1.x.

### Linux
Install via apt:
```
sudo apt install clang-format-19
```
After restarting the terminal, verify:
```
clang-format-19 --version
```

### macOS
Note: should match LLVM 19.
Install via Homebrew:
```
brew install llvm@19
```
Add to your ~/.zshrc:
```
# set clang-format to version 19
export PATH="$(brew --prefix llvm@19)/bin:$PATH"
```
Restart the terminal and verify:
```
clang-format --version
```

### Windows
Install via scoop:
```
scoop install llvm@19.1.0
```
Restart the terminal and verify:
```
clang-format --version
```

## Using the script
From the repository root, run:
```
elvish bin/format
```