{ lib, newScope, fetchurl, fetchFromGitHub, writeShellApplication, akkuPackages }:

lib.makeScope newScope (self: {

  fetchegg = { pname, version, sha256, ... }:
    fetchurl {
      inherit sha256;
      url =
        "https://code.call-cc.org/egg-tarballs/5/${pname}/${pname}-${version}.tar.gz";
    };

  eggDerivation = self.callPackage ./eggDerivation.nix { };

  chicken = self.callPackage ./chicken.nix {
    bootstrap-chicken = self.chicken.override { bootstrap-chicken = null; };
  };

  akkuEggs = let
    small-compat = self.eggDerivation {
      name = "small-compat";
      src = fetchFromGitHub {
        owner = "konst-aa";
        repo = "small-compat";
        rev = "c8a45eecd2e3c09cc12e35427cd7779b707900d5";
        hash = "sha256-PtWIJmJ//F1jvdbpWNGfPL39wn9Oh2UJdigIewHgnhw=";
      };
      buildInputs = with self.chickenEggs; [
        r7rs
      ];
  };
  snow2egg_ = writeShellApplication {
    name = "snow2egg";
    runtimeInputs = [ self.chicken self.chickenEggs.srfi-1 ];
    text = "csi -s ${./snow2egg.scm} \"$@\"";
  };
  snow2egg = "${snow2egg_}/bin/snow2egg";
  in
  lib.recurseIntoAttrs ({ inherit small-compat; } // (lib.makeScope self.newScope (eggself:
  (lib.mapAttrs (pname_: akkuPackage:
  self.eggDerivation rec {
    inherit (akkuPackage) version src;
    checkPhase = ''
      csi -s tests/run.scm
    '';
    doCheck = false;
    name = "${akkuPackage.pname}-${version}";
    buildInputs =
      [
        self.chickenEggs.srfi-1
        self.chickenEggs.r7rs    # compat
        self.chickenEggs.srfi-64 # test suite
        small-compat
        # eggself.chibi-test
    ];
    preBuild = ''
      mkdir -p tests
      cat package.scm | ${snow2egg} > ${akkuPackage.pname}.egg
      rm package.scm
      for f in $(find . -type f -name '*.scm')
      do
        mv $f smth___
        echo '(import-for-syntax (r7rs))' > $f
        cat smth___ >> $f
      done
      rm -f smth___
      cat ${akkuPackage.pname}.egg
    '';
  })
  (lib.attrsets.filterAttrs
    (_: a: (if lib.attrsets.isDerivation a then a.r7rs else false))
    akkuPackages)
  )

  )));

  chickenEggs = lib.recurseIntoAttrs (lib.makeScope self.newScope (eggself:
    (lib.mapAttrs
      (pname:
        eggData@{ version, synopsis, dependencies, license, ... }:
        self.eggDerivation {
          name = "${pname}-${version}";
          src = self.fetchegg (eggData // { inherit pname; });
          buildInputs = map (x: eggself.${x}) dependencies;
          meta.homepage =
            "https://code.call-cc.org/cgi-bin/gitweb.cgi?p=eggs-5-latest.git;a=tree;f=${pname}/${version}";
          meta.description = synopsis;
          meta.license = (lib.licenses // {
            "agpl" = lib.licenses.agpl3Only;
            "artistic" = lib.licenses.artistic2;
            "bsd" = lib.licenses.bsd3;
            "bsd-1-clause" = lib.licenses.bsd1;
            "bsd-2-clause" = lib.licenses.bsd2;
            "bsd-3-clause" = lib.licenses.bsd3;
            "gpl" = lib.licenses.gpl3Only;
            "gpl-2" = lib.licenses.gpl2Only;
            "gplv2" = lib.licenses.gpl2Only;
            "gpl-3" = lib.licenses.gpl3Only;
            "gpl-3.0" = lib.licenses.gpl3Only;
            "gplv3" = lib.licenses.gpl3Only;
            "lgpl" = lib.licenses.lgpl3Only;
            "lgpl-2" = lib.licenses.lgpl2Only;
            "lgpl-2.0+" = lib.licenses.lgpl2Plus;
            "lgpl-2.1" = lib.licenses.lgpl21Only;
            "lgpl-2.1-or-later" = lib.licenses.lgpl21Plus;
            "lgpl-3" = lib.licenses.lgpl3Only;
            "lgplv3" = lib.licenses.lgpl3Only;
            "public-domain" = lib.licenses.publicDomain;
            "srfi" = lib.licenses.bsd3;
            "unicode" = lib.licenses.ucd;
            "zlib-acknowledgement" = lib.licenses.zlib;
          }).${license} or license;
        })
      (lib.importTOML ./deps.toml))));

  egg2nix = self.callPackage ./egg2nix.nix { };

})
