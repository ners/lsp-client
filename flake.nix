{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.nix-filter.url = "github:numtide/nix-filter";

  outputs = inputs:
    let
      lib = inputs.nixpkgs.lib;
      attrsToList = with lib; mapAttrsToList nameValuePair;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (map f xs);
      foreachAttrs = attrs: f: with lib; pipe attrs [
        attrsToList
        (xs: foreach xs ({ name, value }: f name value))
      ];
      foreachFilterAttrs = attrs: p: f: with lib; pipe attrs [
        (filterAttrs p)
        (attrs: foreachAttrs attrs f)
      ];
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
    foreachAttrs inputs.nixpkgs.legacyPackages (system: pkgs:
      foreachFilterAttrs pkgs.haskell.packages (name: _: builtins.match "ghc[0-9]+" name != null)
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
