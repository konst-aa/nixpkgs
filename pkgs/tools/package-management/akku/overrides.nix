{ stdenv, pkgs, lib, akkuPackages }:
let
  broken = lib.addMetaAttrs { broken = true; };
  skipTests = pkg: old: { doCheck = false; };
in
{
  chez-srfi = pkg: old: {
    preCheck = ''
      SKIP='
      multi-dimensional-arrays.sps
      time.sps
      tables-test.ikarus.sps
      lazy.sps
      '
    '';
    # doCheck = false; # for development bc rebuilds take forever
  };
  akku-r7rs = pkg: old: {
    preBuild = ''
      # tests aren't exported modules
      rm -rf tests
    '';
  };

  scheme-langserver = pkg: old: {
    preInstall = ''
      # add the lsp executable to be installed
      echo "#!/usr/bin/env scheme-script" > .akku/bin/scheme-langserver
      cat run.ss >> .akku/bin/scheme-langserver
      chmod +x .akku/bin/scheme-langserver
    '';
    doCheck = false;
  };


  # broken tests
  xitomatl = skipTests;
  # chibi-optional = skipTests;
  ufo-threaded-function = skipTests;

  # unsupported schemes, it seems.
  loko-srfi = broken;
  ac-d-bus = broken;
}
