INCLUDE=-I ../Util -I ../MLgraph.lib
MLGRAPH=../MLgraph.lib/mlgraph
UMASK=umask 002;
MLCFLAGS=-bin-annot -pp camlp5o -w A -w -4 -w -32 -w -45

all: binary_trees.cmo binary_trees_parser.cmo binary_trees_drawing.cmo dictionnaries.cmo sets.cmo

pictures: mkpictures
	$(UMASK) ./mkpictures

INTF=binary_trees.cmi binary_trees_drawing.cmi

.ml.cmo:
	$(UMASK) ocamlc $(MLCFLAGS) $(INCLUDE) -c $<

.mli.cmi:
	$(UMASK) ocamlc $(MLCFLAGS) $(INCLUDE) -c $<



mkpictures: arbres_pictures.cmo binary_trees.cmo  \
	    binary_trees_parser.cmo binary_trees_drawing.cmo \
	    ../Util/prelud.cmo ../Util/orders.cmo ../Util/lexxer.cmo
	$(UMASK) ocamlc $(MLCFLAGS) -I ../Util -o mkpictures $(MLGRAPH).cma \
	  ../Util/prelud.cmo \
	  ../Util/orders.cmo \
	  ../Util/lexxer.cmo \
	  binary_trees.cmo binary_trees_parser.cmo \
	  binary_trees_drawing.cmo arbres_pictures.cmo

arbres_pictures.cmo: binary_trees.cmo binary_trees_parser.cmo binary_trees_drawing.cmo

clean:
	/bin/rm -f *.cmi *.cmo *.cmt mkpictures

test:
	echo '#use "load.ml";;' | ocaml

.SUFFIXES:
.SUFFIXES: .ml .mli .cmi .cmo .cmt
