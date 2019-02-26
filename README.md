# dhall-lsp-server

**This project is under construction !!!**

This is a [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) server implementation for the [Dhall](https://dhall-lang.org) programming language.

**This project is under construction !!!**

## Installation

### From source

[Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/) should be installed. 

```bash
cd ./dhall-lsp-server
stack install
```
Stack should copy executables to the current user's executable directory. On macOS this is `/Users/<username>/.local/bin`. 
If you are using VSCode there's also an option in the [VSCode Dhall plugin](https://github.com/PanAeon/vscode-dhall-lsp-server) to specify the path to the executable directly, which might be useful if you have multiple executables or you can't use global PATH for some reason.


## TODO:

* Trace/Debug output
* validation errors
* multiple dhall versions
* goto definition
* doc on hover
* format
* lint
* file/workspace symbols
* rename

## Working with Markdown

**Note:** You can author your README using Visual Studio Code.  Here are some useful editor keyboard shortcuts:

* Split the editor (`Cmd+\` on macOS or `Ctrl+\` on Windows and Linux)
* Toggle preview (`Shift+CMD+V` on macOS or `Shift+Ctrl+V` on Windows and Linux)
* Press `Ctrl+Space` (Windows, Linux) or `Cmd+Space` (macOS) to see a list of Markdown snippets
