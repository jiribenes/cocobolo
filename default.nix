{ sources ? import ./nix/sources.nix, nixpkgs ? sources.nixpkgs, compiler ? "ghc8107" }:
let
  pkgs = import nixpkgs { inherit config; };

  # use gitignore to scout which files are important and which aren't
  gitIgnore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  # custom package overrides
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              # this package:
              cocobolo = haskellPackagesNew.callCabal2nix "cocobolo" (gitIgnore ./.) { };
            };
          };
        };
      };
    };
  };

  # synonym for easier use:
  myHaskellPackages = pkgs.haskell.packages.${compiler};

  # development nix-shell
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p.cocobolo
    ];

    buildInputs = with myHaskellPackages; [
      # DEVELOPMENT TOOLS:
      ## build-related:
      cabal-install
      cabal2nix

      ## IDE-related:
      ghcid
      hlint
      haskell-language-server

      ## formatters
      brittany

      # Niv for pinning dependencies
      pkgs.niv
    ];

    # enable Hoogle (will be present in the shell)
    withHoogle = true;
  };

  # static-ish executable
  static-exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages.cocobolo);

  # Docker image for transitive closure of `static-exe`!
  docker = pkgs.dockerTools.buildImage {
    name = "cocobolo-docker";

    tag = "latest";

    created = "now"; # this technically breaks binary reproducibility, but it's useful for our purposes 

    extraCommands = ''
      mkdir ./data
      chmod 777 ./data
    '';

    config = {
      Cmd = [ "${static-exe}/bin/cocobolo" ];
      Env = [ "PATH=${static-exe}/bin" ];
      WorkingDir = "/data";
    };
  };

in
{
  inherit shell; # equivalent to `shell = shell`
  inherit static-exe;
  inherit docker;

  cocobolo = myHaskellPackages.cocobolo;
}
