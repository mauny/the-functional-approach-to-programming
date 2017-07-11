INCLUDE=-I ../Util
#MLGRAPH=/usr/local/lib/caml-light/MLgraph
UMASK=umask 002;

all: ml_exp1.zo ml1_parser.zo code_simulator.zo ml1_compiler.zo

pictures:


.ml.zo:
	$(UMASK) camlc $(INCLUDE) -c $<

.mli.zi:
	$(UMASK) camlc $(INCLUDE) -c $<

clean:
	/bin/rm -f *.zi *.zo

.SUFFIXES:
.SUFFIXES: .ml .mli .zi .zo
