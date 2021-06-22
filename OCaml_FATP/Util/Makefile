INCLUDE=-I ../MLgraph.lib
MLGRAPH=../MLgraph.lib/mlgraph
MLCFLAGS=-bin-annot -pp camlp5o

UMASK=umask 002;

all: orders.cmi lexxer.cmi prelud.cmo orders.cmo lexxer.cmo

# pictures:  options.cmo graph.cmo tree.cmo
pictures:
	echo "No pictures"

.ml.cmo:
	$(UMASK) ocamlc -c $(MLCFLAGS) $(INCLUDE) $<

.mli.cmi:
	$(UMASK) ocamlc -c $(MLCFLAGS) $(INCLUDE) $<


# tree.cmo: options.cmo graph.cmo

# graph.cmo: options.cmo


clean:
	/bin/rm -f *.cmi *.cmo *.cmt *.cmti

.SUFFIXES:
.SUFFIXES: .ml .mli .cmi .cmo .cmt .cmti
