let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs { };
  lib = pkgs.lib;
in

# See https://docs.haskellstack.org/en/stable/nix_integration/#using-a-custom-shellnix-file
{ ghc }:

pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "haskell-stack-nix";
  # System dependencies needed at compilation time
  buildInputs = [
    pkgs.zlib
  ] ++ lib.optionals pkgs.stdenv.isDarwin (with pkgs.darwin.apple_sdk.frameworks; [
    Cocoa
    CoreServices
  ]);
}
