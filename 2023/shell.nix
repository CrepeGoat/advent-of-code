# We recommend using the flake.nix file, see https://nixos.wiki/wiki/Flakes for instructions.
# This file is kept to maintain compatibility with tools like lorri until they support flakes (https://github.com/target/lorri/issues/460).
{ system ? builtins.currentSystem }:

(builtins.getFlake "github:roc-lang/roc/3d8884a96d44e2101ce8932336b74e2b9416e029").devShell.${system}
