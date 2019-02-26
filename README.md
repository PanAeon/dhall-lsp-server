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

* Enable/Disable Trace/Debug output dynamically
* validation errors
* multiple dhall versions
* goto definition
* doc on hover
* format
* lint
* file/workspace symbols
* rename
* add timeout to operations
* cache imports
* import errors' location
* 