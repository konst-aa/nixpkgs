{ stdenv, pkgs, lib, akku, akkuPackages, curl, git, substituteAll }:
let
  joinOverrides =
    overrides: pkg: old:
      lib.attrsets.mergeAttrsList (map (o: o pkg old) overrides);
  addToBuildInputs =
    extras: pkg: old:
      { propagatedBuildInputs = old.propagatedBuildInputs ++ extras; };
  broken = lib.addMetaAttrs { broken = true; };
  skipTests = pkg: old: { doCheck = false; };
  # debugging
  showLibs = pkg: old: { preCheck = "echo $CHEZSCHEMELIBDIRS"; };
  runTests = pkgs: old: { doCheck = true; };
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
    doCheck = false; # for development bc rebuilds take forever
  };
  akku-r7rs = pkg: old: {
    preBuild = ''
      # tests aren't exported modules
      rm -rf tests
    '';
  };

  akku = joinOverrides [
    (addToBuildInputs [ curl git ])
    (pkg: old: {
      # hardcode-libcurl
      patches = akku.patches;
    })
  ];

  # circular dependency on wak-trc-testing !?
  wak-foof-loop = skipTests;

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
  ufo-threaded-function = skipTests;

  # unsupported schemes, it seems.
  loko-srfi = broken;
  ac-d-bus = broken;
}
