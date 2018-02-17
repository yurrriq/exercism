{ lib, bundlerEnv, ruby }:

bundlerEnv rec {
  name = "exercism";
  inherit ruby;

  gemdir = ./.;

  meta = with lib; {
    description = "";
    homepage = https://github.com/yurrriq/exercism/tree/ruby;
    license = with licenses; mit;
    maintainers = with maintainers; [ yurrriq ];
    inherit (ruby.meta) platforms;
  };
}