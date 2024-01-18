{ inputs, ... }:

{
  perSystem = { config, pkgs, self', ... }: {
    devShells.gleam = pkgs.mkShell {
      inputsFrom = [
        self'.devShells.default
      ];
      nativeBuildInputs = with pkgs; [
        (
          emacsWithPackagesFromUsePackage {
            alwaysEnsure = true;
            config = ./emacs.el;
            extraEmacsPackages = epkgs: [
              epkgs.tree-sitter
              epkgs.tree-sitter-indent
              self'.packages.gleam-mode
            ];
          }
        )
        erlang_nox
        gleam
        rebar3
        tree-sitter
      ];
    };

    packages.gleam-mode = pkgs.runCommand "gleam-mode" { } ''
      mkdir -p $out/share/emacs/site-lisp
      cp -r ${inputs.gleam-mode} $out/share/emacs/site-lisp/gleam-mode
    '';
  };
}
