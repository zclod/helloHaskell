{enableProfiling ? false}:

let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {

          pipes-binary =
            haskellPackagesNew.callPackage ./pipes-binary.nix { };

          project =
            haskellPackagesNew.callPackage ./default.nix { };
        } // (if enableProfiling then {mkDerivation = args : haskellPackagesOld.mkDerivation (args // {enableLibraryProfiling = true;});
        } else {});
      };
    };
  };

pkgs = import <nixpkgs> { inherit config; };

in
  { project = pkgs.haskellPackages.callPackage ./default.nix { };
  }
