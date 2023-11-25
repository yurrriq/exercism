{ ... }:

{
  perSystem = { config, pkgs, self', system, ... }: {
    apps.mips.program = pkgs.writeShellApplication {
      name = "mips";
      runtimeInputs = with pkgs; [
        mars-mips
        openjdk19_headless
      ];
      text = ''
        java -jar ${pkgs.mars-mips}/share/java/mars-mips/mars-mips.jar "$@"
      '';
    };

    devShells.mips = pkgs.mkShell {
      inputsFrom = [
        self'.devShells.default
      ];
      nativeBuildInputs = with pkgs; [
        asmfmt
        mars-mips
        openjdk19_headless
      ];

      inherit (config.pre-commit.devShell) shellHook;
    };
  };
}
