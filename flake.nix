{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.nix-filter.url = "github:numtide/nix-filter";

  outputs = inputs:
    let
      lib = inputs.nixpkgs.lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else error "foreach: expected list or attrset"
      );

      pname = "lsp-client";
      src = inputs.nix-filter.lib {
        root = ./.;
        include = [
          "src"
          "test"
          "${pname}.cabal"
          "LICENCE"
        ];
      };
    in
    foreach inputs.nixpkgs.legacyPackages (system: pkgs:
      foreach (lib.filterAttrs  (name: _: builtins.match "ghc[0-9]+" name != null) pkgs.haskell.packages)
        (ghcName: haskellPackages:
          let
            hp = haskellPackages.override {
              overrides = self: super: builtins.trace "GHC ${super.ghc.version}" {
                "${pname}" =  super.callCabal2nix pname src { };
              } // lib.optionalAttrs (lib.versionAtLeast super.ghc.version "9.6") {
                fourmolu = super.fourmolu_0_12_0_0;
              };
            };
          in
          {
            packages.${system}.${ghcName}.${pname} = hp.${pname};
            devShells.${system}.${ghcName} = hp.shellFor {
              packages = ps: [ ps.${pname} ];
              nativeBuildInputs = with hp; [
                cabal-install
                fourmolu
                haskell-language-server
              ];
            };
          }
        ) //
      {
        formatter.${system} = pkgs.nixpkgs-fmt;
      }
    );
}
