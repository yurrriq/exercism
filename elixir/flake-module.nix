{ ... }:

{
  perSystem = { config, pkgs, self', ... }: {
    devShells.elixir = pkgs.mkShell {
      inputsFrom = [
        self'.devShells.default
      ];
      nativeBuildInputs = with pkgs; [
        elixir
        elixir_ls
        (
          emacsWithPackagesFromUsePackage {
            alwaysEnsure = true;
            config = ./emacs.el;
          }
        )
        # rebar3
      ];
    };

    treefmt = {
      programs = {
        mix-format.enable = true;
      };
    };
  };
}
