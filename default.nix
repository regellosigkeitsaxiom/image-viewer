{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, directory, glib, gtk2hs-buildtools
      , gtk3, Hclip, random, random-shuffle, stdenv, system-filepath
      , text, transformers
      }:
      mkDerivation {
        pname = "image-viewer";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base directory glib gtk2hs-buildtools gtk3 Hclip random
          random-shuffle system-filepath text transformers
        ];
        homepage = "http://github.com/githubuser/image-viewer#readme";
        description = "Simple project template from stack";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
