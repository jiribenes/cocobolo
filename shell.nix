let
  # import the ./default.nix file
  default = import ./default.nix { };
in
default.shell # use its shell
