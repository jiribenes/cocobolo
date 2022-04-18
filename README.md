## Cocobolo

### TODO

- [x] cabal project
- [x] Nix project (generating a Docker image)
- [ ] argument parser
- [ ] documentation
- [x] type parser for pointers and lists
- [ ] merge `TSafe cap t` in constraints with existing safety in `t`

### Usage

Hope that you have all the necessary Cabal packages and call `cd src/ && ghci Main.hs`.
Then call the function `runFile "../examples/safeMap.ccb"` to test it out.

### Notice

Some of the utility modules are copied from
the [Lily](https://github.com/jiribenes/lily) project by the same author.
