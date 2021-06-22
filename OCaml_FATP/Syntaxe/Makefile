INCLUDE=-I ../Util -I ../Arbres
#MLGRAPH=../MLgraph.lib/mlgraph
UMASK=umask 002;
MLCFLAGS=-pp camlp5o

all: astexpr.cmo parse_prelude.cmo recognize.cmo recognize_val.cmo predict.cmo comp_re.cmo

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
