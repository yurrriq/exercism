{ ... }:

{
  perSystem = { config, lib, pkgs, self', ... }: {
    devShells.erlang = pkgs.mkShell {
      inputsFrom = [
        self'.devShells.default
      ];
      nativeBuildInputs = with pkgs; [
        (
          emacsWithPackagesFromUsePackage {
            alwaysEnsure = true;
            config = ./emacs.el;
          }
        )
        erlfmt
        erlang-ls
        erlang_nox
        rebar3
      ];
    };

    packages.exerlcism =
      let
        subdirectories =
          builtins.attrNames
            (lib.filterAttrs (_: type: type == "directory")
              (builtins.readDir ./.));
        exercises =
          builtins.filter
            (name: builtins.pathExists (./. + ("/" + name + "/rebar.config")))
            subdirectories;
        paths =
          map
            (name: pkgs.beamPackages.buildRebar3 {
              inherit name;
              # FIXME: read app.src?
              version = "0.0.1";
              src = ./. + "/${name}";
            })
            exercises;
      in
      pkgs.symlinkJoin {
        name = "exerlcism";
        inherit paths;
      };

    treefmt = {
      programs = {
        erlfmt = {
          enable = true;
          print-width = 80;
        };
      };

      settings.formatter = {
        erlfmt = {
          excludes = [
            "fsharp/**/*.config"
          ];
        };
      };
    };
  };
}
