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
          "${pname}.cabal"
          "LICENCE"
          "CHANGELOG.md"
        ];
      };
      pname = "lsp-client";
    in
    foreach inputs.nixpkgs.legacyPackages (system: pkgs:
      let
        defaultGhc = builtins.replaceStrings ["-" "."] ["" ""] pkgs.haskellPackages.ghc.name;
      in
      lib.recursiveUpdate
        {
          formatter.${system} = pkgs.nixpkgs-fmt;
          packages.${system}.default = inputs.self.packages.${system}.${defaultGhc}.${pname};
          devShells.${system}.default = inputs.self.devShells.${system}.${defaultGhc};
        }
        (foreach (lib.filterAttrs (name: _: builtins.match "ghc[0-9]+" name != null) pkgs.haskell.packages)
          (ghcName: haskellPackages:
            let
              hp = haskellPackages.override {
                overrides = self: super: {
                  unix-compat = self.callCabal2nix "unix-compat" inputs.unix-compat { };
                  "${pname}" = self.callCabal2nix pname (hsSrc pname ./.) { };
                };
              };
            in
            {
              packages.${system}.${ghcName} = foreach [ pname ] (pname: {
                "${pname}" = hp.${pname};
              });
              devShells.${system}.${ghcName} = hp.shellFor {
                packages = ps: [ ps.${pname} ];
                nativeBuildInputs = with haskellPackages; [
                  cabal-install
                  fourmolu
                  haskell-language-server
                ];
              };
            }
          )
        ));
}
