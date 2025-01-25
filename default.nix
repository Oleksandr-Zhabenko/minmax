{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.mkDerivation {
  pname = "minmax";
  version = "0.1.1.0";
  src = ./.;
  libraryHaskellDepends = [ pkgs.haskellPackages.base ];
  homepage = "https://hackage.haskell.org/package/minmax";
  description = "Functions to find both minimum and maximum (or several of them simultaneously) in one pass";
  license = pkgs.lib.licenses.mit;
  maintainers = [ pkgs.lib.maintainers.oleksandrzhabenko ];
}
