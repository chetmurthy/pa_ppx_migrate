# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile

DESTDIR=
RM=rm

LAUNCH=$(TOP)/tools/LAUNCH
OCAMLFIND=$(LAUNCH) ocamlfind
NOT_OCAMLFIND=$(LAUNCH) not-ocamlfind
MKCAMLP5=$(LAUNCH) mkcamlp5
PACKAGES := $(PACKAGES),pa_ppx.base,fmt,bos,pa_ppx.import
PACKAGES := $(PACKAGES),pa_ppx_migrate
SYNTAX = camlp5o
OCAMLCFLAGS += -ppopt -pa_import-I -ppopt .
OCAMLCFLAGS += -ppopt -pa_passthru-debug 

TARGET=pa_omp.cma
ML=all_ast.ml \
	rewrite_402_403.ml rewrite_403_402.ml \
	rewrite_402_404.ml rewrite_404_402.ml \
	rewrite_403_404.ml rewrite_404_403.ml \
	rewrite_404_405.ml rewrite_405_404.ml \
	rewrite_405_406.ml rewrite_406_405.ml \
	rewrite_406_407.ml rewrite_407_406.ml \
	rewrite_407_408.ml rewrite_408_407.ml \
	rewrite_408_409.ml rewrite_409_408.ml \
	rewrite_409_410.ml rewrite_410_409.ml \
	rewrite_410_411.ml rewrite_411_410.ml
CMO := $(ML:.ml=.cmo)
CMI := $(ML:.ml=.cmi)
CMX := $(ML:.ml=.cmx)
CMT := $(ML:.ml=.cmt)
CMTI := $(MLI:.mli=.cmti)

all: $(TARGET) $(TARGET:.cma=.cmxa) local-install

doc: $(CMT) $(CMTI)

META: META.pl
	./META.pl > META

all_ast.ml: all_ast.ORIG.ml $(wildcard gen/ast_*.ml) 
	cppo all_ast.ORIG.ml > all_ast.ml.NEW && mv all_ast.ml.NEW all_ast.ml

install::
	./META.pl $(DESTDIR)/lib > META
	mkdir -p $(DESTDIR)/lib
	$(NOT_OCAMLFIND) reinstall-if-diff pa_ppx_ocaml_migrate_parsetree -destdir $(DESTDIR)/lib META $(TARGET) $(TARGET:.cma=.cmxa) $(TARGET:.cma=.a) $(CMI) $(wildcard *.cmt*)
	$(RM) -f META

local-install::
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

clean::
	rm -rf META gen/*.cm* gen/*.o all_ast.ml

$(TARGET): $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) $(CMO) -a -o $(TARGET)

$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) $(CMO:.cmo=.cmx) -a -o $(TARGET:.cma=.cmxa)

$(TARGET): $(CMO)
$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)

EXTERNAL := $(shell $(OCAMLFIND) query -predicates byte -format '%m' $(PACKAGES))
$(CMO) $(CMI) $(CMX): $(EXTERNAL)

depend::
	echo "$(CMO) $(CMI) $(CMX): $(EXTERNAL)" > .depend.NEW
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -syntax camlp5o *.ml *.mli > .depend.NEW \
		&& mv .depend.NEW .depend

-include .depend
