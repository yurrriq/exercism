{ ... }:

{
  perSystem = { pkgs, self', ... }: {
    devShells.cpp = pkgs.mkShell {
      inputsFrom = [
        self'.devShells.default
      ];
      nativeBuildInputs = with pkgs; [
        ccls
        cmake
        doxygen
        (
          emacsWithPackagesFromUsePackage {
            alwaysEnsure = true;
            config = ./emacs.el;
          }
        )
        gcc13
      ];
    };

    treefmt = {
      programs = {
        clang-format.enable = true;
      };
    };
  };
}
