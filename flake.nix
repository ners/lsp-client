{
  nixConfig = {
    extra-substituters = "https://cache.ners.ch/haskell";
    extra-trusted-public-keys = "haskell:WskuxROW5pPy83rt3ZXnff09gvnu80yovdeKDw5Gi3o=";
  };


  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nix-filter.url = "github:numtide/nix-filter";
    unix-compat = {
      url = "github:haskell-pkg-janitors/unix-compat";
      flake = false;
    };
  };

  outputs = inputs:
    with builtins;
    let
      inherit (inputs.nixpkgs) lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else error "foreach: expected list or attrset but got ${typeOf xs}"
      );
      hsSrc = pname: root: inputs.nix-filter {
        inherit root;
        include = [
          "app"
          "lib"
          "src"
          "test"
          (inputs.nix-filter.lib.matchExt "cabal")
          (inputs.nix-filter.lib.matchExt "md")
        ];
      };
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = lib.composeExtensions prev.haskell.packageOverrides (hfinal: hprev:
            with prev.haskell.lib.compose;
            {
              "${pname}" = hfinal.callCabal2nix pname (hsSrc pname ./.) {
                unix-compat = hfinal.callCabal2nix "unix-compat" inputs.unix-compat { };
              };
            }
          );
        };
      };
      pname = "lsp-client";
      ghcs = [ "ghc92" "ghc94" "ghc96" ];
    in
    foreach inputs.nixpkgs.legacyPackages
      (system: pkgs':
        let
          pkgs = pkgs'.extend overlay;
          hps = lib.filterAttrs (ghc: _: elem ghc ghcs) pkgs.haskell.packages;
        in
        {
          formatter.${system} = pkgs.nixpkgs-fmt;
          legacyPackages.${system} = pkgs;
          packages.${system}.default = pkgs.haskellPackages.${pname};
          checks.${system}.${pname} = pkgs.buildEnv {
            name = pname;
            paths = map (hp: hp.${pname}) (attrValues hps);
          };
          devShells.${system} =
            foreach (hps // { default = pkgs.haskellPackages; }) (ghcName: hp: {
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
