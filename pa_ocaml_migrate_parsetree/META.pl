#!/usr/bin/env perl

use strict ;
BEGIN { push (@INC, "..") }
use Version ;

our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ocaml_migrate_parsetree" library:
requires = "camlp5,fmt,bos"
version = "$Version::version"
description = "pa_ocaml_migrate_parsetree support"

# For linking
package "link" (
requires = "camlp5,fmt,bos"
archive(byte) = "pa_omp.cma"
archive(native) = "pa_omp.cmxa"
)

# For the toploop:
archive(byte,toploop) = "pa_omp.cma"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5,fmt,bos"
  archive(syntax,preprocessor,-native) = "pa_omp.cma"
  archive(syntax,preprocessor,native) = "pa_omp.cmxa"

EOF
