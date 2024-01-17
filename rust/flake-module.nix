{ inputs, ... }:

{
  perSystem = { config, lib, pkgs, self', system, ... }: {
    devShells.rust = pkgs.mkShell {
      RUST_BACKTRACE = 1;

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
        (
          fenix.complete.withComponents [
            "cargo"
            "clippy"
            "rust-src"
            "rustc"
            "rustfmt"
          ]
        )
        rust-analyzer-nightly
      ];
    };

    packages.exersism =
      let
        subdirectories =
          builtins.attrNames
            (lib.filterAttrs (_: type: type == "directory")
              (builtins.readDir ./.));
        exercises =
          builtins.filter
            (name:
              !(lib.lists.elem name [
                "bowling"
                "poker"
                "scale-generator"
              ]) &&
              builtins.pathExists (./. + ("/" + name + "/Cargo.lock")))
            subdirectories;
        paths =
          map
            (pname: inputs.naersk.lib.${system}.buildPackage {
              inherit pname;
              root = ./. + "/${pname}";
              doCheck = true;
            })
            exercises;
      in
      pkgs.symlinkJoin {
        name = "exersism";
        inherit paths;
      };
  };
}
