ALL = mls mll
UMASK=umask 002;
MLCFLAGS=-bin-annot -pp camlp5o

SRCS =  ml_ops.ml ml_strict.ml ml_lazy.ml lexer.ml \
        parser_strict.ml parser_lazy.ml eval_strict.ml eval_lazy.ml

SOBJS = ml_ops.cmo ml_strict.cmo lexer.cmo parser_strict.cmo eval_strict.cmo
LOBJS = ml_ops.cmo ml_lazy.cmo lexer.cmo parser_lazy.cmo eval_lazy.cmo

CAMLC=ocamlc
LINK=$(CAMLC) -o

all: $(ALL)

pictures:
	echo "No pictures"

mls: $(SOBJS) mls.ml
	$(UMASK) $(LINK) $@ $(SOBJS) mls.ml

mll: $(LOBJS) mll.ml
	$(UMASK) $(LINK) $@ $(LOBJS) mll.ml

.mli.cmi:
	$(UMASK) $(CAMLC) $(MLCFLAGS) -c $<

.ml.cmo:
	$(UMASK) $(CAMLC) $(MLCFLAGS) -c $<

.SUFFIXES:

.SUFFIXES: .ml .mli .cmo .cmi .cmt

clean:
	/bin/rm -f *~ *.cmi *.cmo *.cmt $(ALL)

depend:
	$(UMASK) cp Makefile Makefile.bak
	(sed -e '/^#.*DEPEND.*automatic/,$$d' Makefile.bak; \
	 echo '### DEPENDENCIES: automatically generated. Do not edit'; \
	 ocamldep $(SRCS)) > Makefile


### DEPENDENCIES: automatically generated. Do not edit
ml_strict.cmo: ml_ops.cmo
ml_lazy.cmo: ml_ops.cmo
parser_strict.cmo: lexer.cmo ml_ops.cmo ml_strict.cmo
parser_lazy.cmo: lexer.cmo ml_ops.cmo ml_lazy.cmo
eval_strict.cmo: ml_ops.cmo lexer.cmo parser_strict.cmo ml_strict.cmo
eval_lazy.cmo: ml_ops.cmo lexer.cmo parser_lazy.cmo ml_lazy.cmo
