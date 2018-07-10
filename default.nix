{ mkDerivation
, base
, stdenv
, random
, development ? false
, hlint }:
let
  baseOption = { pname = "snek";
                 version = "0.1.0.0";
                 src = ./.;
                 isLibrary = false;
                 isExecutable = true;
                 executableHaskellDepends = [ base random ];
                 homepage = "https://github.com/fmnasution/snek";
                 license = stdenv.lib.licenses.bsd3; };
  option = if development
              then baseOption // { buildDepends = [ hlint ];}
              else baseOption;
in
  mkDerivation option
