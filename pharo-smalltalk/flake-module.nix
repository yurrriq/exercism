{ ... }:

{
  perSystem = { pkgs, self', ... }: {
    devShells.pharo-smalltalk = pkgs.mkShell {
      inputsFrom = [
        self'.devShells.default
      ];
      nativeBuildInputs = with pkgs; [
        coreutils
        curl
        pharo
        unzip
      ];
      shellHook = ''
        if ! test -f Pharo.image; then
            zip_file="$(mktemp)"
            curl -o "$zip_file" https://files.pharo.org/get-files/100/pharoImage-x86_64.zip
            unzip "$zip_file"
            ln -sf Pharo*.image Pharo.image
            rm "$zip_file"
        fi
      '';
    };
  };
}
