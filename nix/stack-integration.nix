let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs { };
  lib = pkgs.lib;
in

# See https://docs.haskellstack.org/en/stable/nix_integration/#using-a-custom-shellnix-file
{ ghc }:

let llvm = pkgs.llvm_9.dev;

    llvm-config-hack = pkgs.writeShellScriptBin "llvm-config" ''
      if [[ "$1" =~ "version" ]]
      then
        echo 9.0.1
      elif [[ "$1" =~ "cxx" ]]
      then
        echo "-I${llvm}/include -std=c++14 -stdlib=libc++ -fno-exceptions"
      elif [[ "$1" =~ "includedir" ]]
      then
        echo "-I${llvm}/include"
      elif [[ "$1" =~ "libdir" ]]
      then
        echo "${llvm}/lib"
      elif [[ "$1" =~ "libs" ]]
      then
        echo "-lLLVM-9"
       elif [[ "$1" =~ "cflags" ]]
      then
        echo "-I${llvm}/include"
      else
        exit $1
      fi
      exit 0
    '';


in pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "haskell-stack-nix";

  PATH = "${llvm-config-hack}/bin:$PATH";
  NIX_CFLAGS_COMPILE = "-Wno-deprecated -Wno-implicit-function-declaration -Wno-int-conversion";
  HAVE_GETRANDOM = "1";

  # System dependencies needed at compilation time
  buildInputs = [
    pkgs.zlib
    llvm-config-hack
    llvm
    pkgs.haskellPackages.alex
  ] ++ lib.optionals pkgs.stdenv.isDarwin (with pkgs.darwin.apple_sdk.frameworks; [
    Cocoa
    CoreServices
  ]);
}
