{ pkgs ? import <nixpkgs> {}, displayrUtils }:

pkgs.rPackages.buildRPackage {
  name = "flipTables";
  version = displayrUtils.extractRVersion (builtins.readFile ./DESCRIPTION); 
  src = ./.;
  description = ''Package for handling tables and related objects.'';
  propagatedBuildInputs = with pkgs.rPackages; [ 
    stringr
    ca
    flipTransformations
    verbs
    stringi
    lubridate
    flipTime
    flipU
  ];
}
