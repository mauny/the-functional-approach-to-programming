INCLUDE=-I ../Util -I ../Arbres -I ../MLgraph.lib
MLGRAPH=../MLgraph.lib/mlgraph
UMASK=umask 002;
MLCFLAGS= -bin-annot -pp camlp5o


INTF=poly_tree.cmi inttree.cmi exp.cmi

all: $(INTF) poly_tree.cmo inttree.cmo exp.cmo

pictures: mkpictures
	$(UMASK) ./mkpictures

.ml.cmo:
	$(UMASK) ocamlc $(MLCFLAGS) $(INCLUDE) -c $<

.mli.cmi:
	$(UMASK) ocamlc $(MLCFLAGS) $(INCLUDE) -c $<



mkpictures: $(INTF) pictures_struct.cmo  \
	    ../Util/prelud.cmo ../Arbres/binary_trees.cmo ../Util/lexxer.cmo \
	    ../Arbres/binary_trees_parser.cmo \
	    ../Arbres/binary_trees_drawing.cmo poly_tree.cmo
	$(UMASK) ocamlc -o mkpictures $(INCLUDE) $(MLGRAPH).cma  \
	  ../Util/orders.cmo \
	  ../Util/prelud.cmo \
	  ../Arbres/binary_trees.cmo \
	  ../Util/lexxer.cmo \
	  ../Arbres/binary_trees_parser.cmo \
	  ../Arbres/binary_trees_drawing.cmo \
	  poly_tree.cmo pictures_struct.cmo

clean:
	/bin/rm -f *.cmi *.cmo *.cmt *.cmti mkpictures

.SUFFIXES:
.SUFFIXES: .ml .mli .cmi .cmo .cmt .cmti
