# Cocobolo

Cocobolo is an experimental, proof-of-concept
programing language using granular capabilities.
It is a code artifact of my master thesis.

## Downloading the repository

```
git clone --recursive-submodules $REPO_URL
```

## Building

Please use the Nix package manager if possible,
it makes everything way, way easier.

Alternatively if you don't want to build the project yourself, 
you can just use a prebuilt Docker image, see [TODO]

### Nix

```
nix-build --attr cocobolo default.nix
```

The resulting executable is located in `result/bin/`.
If you wish to install it onto your system using Nix,
use `nix-env -f default.nix -iA cocobolo`.

You may also build a more static version of the executable by using `static-exe`
instead of `cocobolo` in the commands above.

#### Developing with Nix

Use `nix-shell shell.nix` to create a development shell --
a shell with all development dependencies such as Cabal, GHC, etc.

You can use normal Cabal commands when in Nix shell to work on Cocobolo: 

* To build Cocobolo using Cabal (better for development, incremental builds):
```
cabal new-build cocobolo
```

* To build and run Cocobolo using Cabal with some arguments in `$ARGS`:
```
cabal new-run cocobolo -- $ARGS
```

* To open a GHCi REPL using Cabal:
```
cabal new-repl cocobolo
```

#### Updating dependenies using Niv

The [niv](https://github.com/nmattia/niv) tool is used to
manage the version of nixpkgs. It is currently pinned
to a version of nixpkgs for which cocobolo builds and works.

To update the version of nixpkgs, use the command `niv update`
in the development nix-shell.

#### Creating and using a Docker image with Nix

Requires Nix, Docker.
Warning: Creates a ~300MB image.

1. Create and load the image using Nix
```
docker load -i $(nix-build --attr docker default.nix)
```

2. Run the image in current directory
```
docker run -itv $PWD:/data cocobolo-docker cocobolo [ARGS...]
```

_Example usage:_
```
docker run -itv $PWD:/data cocobolo-docker cocobolo infer examples/io.ccb
```

### Cabal

This repository is tested with Cabal 3.0, 3.2, 3.6 and GHC 8.10.7.
You can install them with [ghcup](https://haskell.org/ghcup).

In order to compile the repository, use the following commands:
```
cabal new-update
cabal new-build exe:cocobolo
```

In order to run Cocobolo with some arguments in `$ARGS`, use:
```
cabal new-run cocobolo
```

## Using

Assuming that `$COCO` is the way to invoke Cocobolo
as it might depend on which installation was chosen.

Use the following command to infer an example file `examples/io.ccb`:
```
$COCO infer examples/io.ccb
```

Use the following command to bring up automatically generated help:
```
$COCO --help
```
