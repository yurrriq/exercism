{ ... }:

{
  perSystem = { config, pkgs, self', ... }: {
    devShells.fsharp = pkgs.mkShell {
      FONTCONFIG_FILE = pkgs.makeFontsConf {
        fontDirectories = [
          (
            pkgs.nerdfonts.override {
              fonts = [
                "FiraCode"
              ];
            }
          )
        ];
      };

      inputsFrom = [
        self'.devShells.default
      ];
      nativeBuildInputs = with pkgs; [
        dotnet-sdk_7
        (
          emacsWithPackagesFromUsePackage {
            alwaysEnsure = true;
            config = ./emacs.el;
          }
        )
        fantomas
        fsautocomplete
        fsharp
      ];
    };

    # treefmt = {
    #   programs = {
    #     fantomas = {
    #       enable = true;
    #       package = pkgs.fantomas;
    #     };
    #   };

    #   settings.formatter = {
    #     fantomas = {
    #       command = "${config.treefmt.programs.fantomas.package}/bin/fantomas";
    #       includes = [ "*.fs" ];
    #     };
    #   };
    # };
  };
}
