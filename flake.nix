{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nix-filter.url = "github:numtide/nix-filter";
    unix-compat = {
      url = "github:haskell-pkg-janitors/unix-compat";
      flake = false;
    };
  };

  outputs = inputs:
    let
      inherit (inputs.nixpkgs) lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else error "foreach: expected list or attrset but got ${builtins.typeOf xs}"
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
    in
    foreach inputs.nixpkgs.legacyPackages
      (system: pkgs':
        let pkgs = pkgs'.extend overlay; in
        {
          formatter.${system} = pkgs.nixpkgs-fmt;
          legacyPackages.${system} = pkgs;
          packages.${system}.default = pkgs.haskellPackages.${pname};
          devShells.${system} =
            foreach (pkgs.haskell.packages // { default = pkgs.haskellPackages; }) (ghcName: hp: {
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
