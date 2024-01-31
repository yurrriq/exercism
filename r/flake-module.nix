{ ... }:

{
  perSystem = { pkgs, self', ... }: {
    devShells.r = pkgs.mkShell {
      inputsFrom = [
        self'.devShells.default
      ];
      nativeBuildInputs = with pkgs; [
        R
      ] ++ (with rPackages; [
        formatR
        testthat
      ]);
    };
  };
}
