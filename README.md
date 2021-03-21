# Banana

Banana is a dummy imperative programming language developed for the purpose of this project. This is a compiler written in Haskell for the **Functional Programming with Haskell** course at **Innopolis University**.
For more details, check out this repo's [wiki](https://github.com/aabounegm/banana/wiki).

## Running

To compile the project, run `stack build`. Run `stack install` after that to make the executables globally available.

To run the unit tests, use `stack test`.

### Compiler

The compiler can run in 2 modes: **REPL** and **AOT**.
To play around with the interactive **REPL** mode, just run `stack exec banana-exe` without any parameter.
To compile an actual file, give the same command a path to a valid file: `stack exec banana-exe <path/to/file>`.

### Language Server

The language server is the engine running behind the VS Code extension. It provides diagnostic messages and hover information.
It is not supposed to be run manually, but through the VS Code extension only. However, if needed, it can run through the command `stack exec lsp-exe`.

#### Extension

To run the extension in debug mode, open the [extension](./extension) folder in a separate VS Code instance (`code ./extension`), run `npm install` to get all the client's dependencies, `npm run watch` to compile the TypeScript code, and finally press `F5` to launch a VS Code instance with the extension ready for testing.

_Note: when it is mature enough, the language server should be integrated into the main compiler to have a single executable with a flag to control the running mode._

## Useful links

### Readings/Tutorials
- [Real World Haskell](http://book.realworldhaskell.org/)
- [Implementing a JIT Compiled Language with Haskell and LLVM](https://www.stephendiehl.com/llvm/): A tutorial that builds a simple language from A to Z (adapted from the official [LLVM tutorial](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html) in C++)
- [Write You a Haskell](http://dev.stephendiehl.com/fun/): Building a Haskell-like language in Haskell
- [An Introduction to the Parsec library](https://kunigami.wordpress.com/2014/01/21/an-introduction-to-the-parsec-library/)
- [LSP specification](https://microsoft.github.io/language-server-protocol/specifications/specification-current/)
- [Language Server Extension Guide](https://code.visualstudio.com/api/language-extensions/language-server-extension-guide)

### Packages
- [llvm-hs](https://github.com/llvm-hs/llvm-hs/): LLVM bindings in Haskell
- [parsec](https://hackage.haskell.org/package/parsec): A popular parsing library
- [lsp](https://hackage.haskell.org/package/lsp): An implementation of the basic server components for LSP

### Example projects
- [Kaleidoscope](https://github.com/sdiehl/kaleidoscope): The source code from the Haskell/LLVM tutorial above
- [Write You a Haskell](https://github.com/sdiehl/write-you-a-haskell) (WIP): Source code from the above tutorial
- [GHC](https://gitlab.haskell.org/ghc/ghc): The official Glasgow Haskell Compiler source code
- [Official Haskell VS Code extension](https://github.com/haskell/vscode-haskell)
