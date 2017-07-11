ALL =   mls mll
UMASK=umask 002;
SRCS =  ml_ops.ml ml_strict.ml ml_lazy.ml lexer.ml \
        parser_strict.ml parser_lazy.ml eval_strict.ml eval_lazy.ml

SOBJS = ml_ops.zo ml_strict.zo lexer.zo parser_strict.zo eval_strict.zo
LOBJS = ml_ops.zo ml_lazy.zo lexer.zo parser_lazy.zo eval_lazy.zo

CAMLC=camlc
LINK=$(CAMLC) -o

all: $(ALL)

pictures:


mls: $(SOBJS) mls.ml
	$(UMASK) $(LINK) $@ $(SOBJS) mls.ml

mll: $(LOBJS) mll.ml
	$(UMASK) $(LINK) $@ $(LOBJS) mll.ml

.mli.zi:
	$(UMASK) $(CAMLC) -c $<

.ml.zo:
	$(UMASK) $(CAMLC) -c $<

.SUFFIXES:

.SUFFIXES: .ml .mli .zo .zi

clean:
	/bin/rm -f *~ *.zi *.zo $(ALL)

depend:
	$(UMASK) cp Makefile Makefile.bak
	(sed -e '/^#.*DEPEND.*automatic/,$$d' Makefile.bak; \
	 echo '### DEPENDENCIES: automatically generated. Do not edit'; \
	 camldep $(SRCS)) > Makefile


### DEPENDENCIES: automatically generated. Do not edit
ml_strict.zo: ml_ops.zo 
ml_lazy.zo: ml_ops.zo 
parser_strict.zo: lexer.zo ml_ops.zo ml_strict.zo 
parser_lazy.zo: lexer.zo ml_ops.zo ml_lazy.zo 
eval_strict.zo: ml_ops.zo lexer.zo parser_strict.zo ml_strict.zo 
eval_lazy.zo: ml_ops.zo lexer.zo parser_lazy.zo ml_lazy.zo 
