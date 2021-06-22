INCLUDE=-I ../Util
MLGRAPH=../MLgraph.dir
UMASK=umask 002;
MLCFLAGS=-bin-annot -pp camlp5o

all: ml_exp1.cmo ml1_parser.cmo code_simulator.cmo ml1_compiler.cmo

pictures:
	echo "No pictures"

.ml.cmo:
	$(UMASK) ocamlc $(MLCFLAGS) $(INCLUDE) -c $<

.mli.cmi:
	$(UMASK) ocamlc $(MLCFLAGS) $(INCLUDE) -c $<

clean:
	/bin/rm -f *.cmi *.cmo *.cmt

.SUFFIXES:
.SUFFIXES: .ml .mli .cmi .cmo .cmt
