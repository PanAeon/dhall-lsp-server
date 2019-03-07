

:.:.:.:.:.::.:.:.:.:.::.:.:.:.:.::.:.:.:.:.::.:.:.:.:.:

**Project has been migrated** 

:.:.:.:.:.::.:.:.:.:.::.:.:.:.:.::.:.:.:.:.::.:.:.:.:.:

Got to the [dhall-haskell](https://github.com/dhall-lang/dhall-haskell) repo for new builds/documentation.
I'll update links in the vscode-dhall-lsp-plugin shortly.





## Installation

### From source

[Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/) should be installed. 

```bash
cd ./dhall-lsp-server
stack install
```

Stack will copy executables to the current user's executable directory. On macOS this is `/Users/<username>/.local/bin`. On linux this should be `/Home/<username>/.local/bin`.
If you are using VSCode there's also an option in the [VSCode Dhall plugin](https://github.com/PanAeon/vscode-dhall-lsp-server) to specify the path to the executable directly, which might be useful if you have multiple executables or you can't use global PATH for some reason.


