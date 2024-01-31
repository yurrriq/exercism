{ ... }:

{
  perSystem = { pkgs, self', ... }: {
    devShells.common-lisp = pkgs.mkShell {
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
        roswell
        (
          sbcl.withPackages (ps: with ps; [
            fiveam
          ])
        )
      ];
    };
  };
}
