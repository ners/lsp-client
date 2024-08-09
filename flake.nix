{
  nixConfig = {
    extra-substituters = "https://cache.ners.ch/haskell";
    extra-trusted-public-keys = "haskell:WskuxROW5pPy83rt3ZXnff09gvnu80yovdeKDw5Gi3o=";
  };

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs = inputs:
    with builtins;
    let
      inherit (inputs.nixpkgs) lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else throw "foreach: expected list or attrset but got ${typeOf xs}"
      );
      hsSrc = root: with lib.fileset; toSource {
        inherit root;
        fileset = fileFilter (file: any file.hasExt [ "cabal" "hs" "md" ] || file.type == "directory") ./.;
      };
      pname = "lsp-client";
      src = hsSrc ./.;
      ghcs = [ "ghc92" "ghc94" "ghc96" "ghc98" ];
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = lib.composeManyExtensions [
            prev.haskell.packageOverrides
            (hfinal: hprev: {
              ${pname} = hfinal.callCabal2nix pname src { };
            })
          ];
        };
      };
    in
    foreach inputs.nixpkgs.legacyPackages
      (system: pkgs':
        let
          pkgs = pkgs'.extend overlay;
          hps =
            lib.filterAttrs (ghc: _: elem ghc ghcs) pkgs.haskell.packages
            // { default = pkgs.haskellPackages; };
          inherit (hps.default.${pname}) version;
          allPackages =
            pkgs.symlinkJoin {
              name = "${pname}-all-${version}";
              paths = map (hp: hp.${pname}) (attrValues hps);
            };
          release = with pkgs.haskell.lib; pkgs.linkFarm "${pname}-release-${version}" {
            sdist = sdistTarball hps.default.${pname};
            docs = documentationTarball hps.default.${pname};
          };
          default = pkgs.symlinkJoin {
            inherit (hps.default.${pname}) name;
            paths = [
              allPackages
              release
            ];
          };
        in
        {
          formatter.${system} = pkgs.nixpkgs-fmt;
          legacyPackages.${system} = pkgs'.extend lspOverlay;
          packages.${system} = { inherit default; };
          devShells.${system} =
            foreach hps (ghcName: hp: {
              ${ghcName} = hp.shellFor {
                packages = ps: [ ps.${pname} ];
                nativeBuildInputs = with hp; [
                  cabal-install
                  fourmolu
                  haskell-language-server
                ];
              };
            });
        }
      ) // {
      overlays.default = overlay;
    };
}
