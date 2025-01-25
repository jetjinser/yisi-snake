{
  description = "A startup basic project";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    devshell.url = "github:numtide/devshell";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.devshell.flakeModule
      ];

      perSystem = { pkgs, ... }: {
        devshells.default = {
          packages = with pkgs; [
            nodejs
            pnpm
            typescript
            nodePackages.typescript-language-server
            vscode-langservers-extracted
            guile
            guile-hoot
            gnumake
            zip
          ];
          env = [{
            name = "GUILE_LOAD_PATH";
            prefix = "$DEVSHELL_DIR/${pkgs.guile.siteDir}";
          }];
        };
      };

      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
    };
}
