{ ... }:

{
  perSystem = { lib, pkgs, self', ... }: {
    devShells.mips = pkgs.mkShell {
      inputsFrom = [
        self'.devShells.default
      ];
      nativeBuildInputs = with pkgs; [
        mars-mips
      ];
    };

    treefmt = {
      programs.asmfmt.enable = true;
      settings.formatter.asmfmt = {
        excludes = [ "runner.mips" ];
        includes = lib.mkForce [ "*.mips" ];
      };
    };
  };
}
