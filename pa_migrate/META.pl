#!/usr/bin/env perl

use strict ;
BEGIN { push (@INC, "..") }
use Version ;

our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ppx_migrate" preprocessor:
version = "$Version::version"
description = "pa_ppx_migrate deriver"

  requires(toploop) = "camlp5,pa_ppx.deriving"
  archive(toploop) = "pa_deriving_migrate.cmo"

    requires(syntax,preprocessor) = "camlp5,pa_ppx.deriving"
    archive(syntax,preprocessor,-native) = "pa_deriving_migrate.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_migrate.cmx"

  package "link" (
  requires(byte) = "camlp5,pa_ppx.deriving.link"
  archive(byte) = "pa_deriving_migrate.cmo"
  )
  requires = "camlp5,pa_ppx.deriving,pa_ppx.runtime"

EOF
