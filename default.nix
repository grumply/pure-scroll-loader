{ mkDerivation, base, pure, pure-visibility, stdenv }:
mkDerivation {
  pname = "pure-scroll-loader";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure pure-visibility ];
  homepage = "github.com/grumply/pure-scroll-loader";
  license = stdenv.lib.licenses.bsd3;
}
