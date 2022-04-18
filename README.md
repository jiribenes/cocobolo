## Cocobolo

### TODO

- [x] cabal project
- [x] Nix project (generating a Docker image)
- [ ] argument parser
- [ ] documentation
- [x] type parser for pointers and lists
- [ ] merge `TSafe cap t` in constraints with existing safety in `t`

### Usage

Use GHC 8.10.7 (either directly or via Nix).

Call `cabal update` to update your dependencies and then 
(as a temporary measure) call `cabal repl` in your shell to open the REPL.
Then call the function `runFile "examples/safeMap.ccb"` to test the example out.

### Notice

Some of the utility modules are copied from
the [Lily](https://github.com/jiribenes/lily) project by the same author.
