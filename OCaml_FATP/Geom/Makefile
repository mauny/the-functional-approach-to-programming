INCLUDE=-I ../Util -I ../Arbres -I ../MLgraph.lib
MLGRAPH=../MLgraph.lib/mlgraph
UMASK=umask 002;
MLCFLAGS=-bin-annot -pp camlp5o -w A -w -4 -w -8 -w -32 -w -41 -w -44 -w -45

all: permutations.cmo complexs.cmo pavages1.cmo pavages2.cmo

pictures: mkpictures
	$(UMASK) ./mkpictures

.ml.cmo:
	$(UMASK) ocamlc $(MLCFLAGS) $(INCLUDE) -c $<

.mli.cmi:
	$(UMASK) ocamlc $(MLCFLAGS) $(INCLUDE) -c $<



mkpictures: geom_pictures.cmo pavages1_pictures.cmo  pavages2_pictures.cmo  \
	    circle_limit_pictures.cmo \
	    pavages1.cmo pavages2.cmo complexs.cmo permutations.cmo \
	    ../Util/prelud.cmo ../Util/orders.cmo ../Util/lexxer.cmo
	$(UMASK) ocamlc $(MLCFLAGS) -o mkpictures $(MLGRAPH).cma   \
	  ../Util/prelud.cmo ../Util/orders.cmo \
	  ../Util/lexxer.cmo ../Arbres/binary_trees.cmo ../Arbres/sets.cmo \
	  ../Arbres/binary_trees_parser.cmo ../Arbres/binary_trees_drawing.cmo \
	  permutations.cmo complexs.cmo pavages1.cmo pavages2.cmo \
	  pavages1_pictures.cmo pavages2_pictures.cmo \
	  circle_limit_pictures.cmo geom_pictures.cmo

pavages1_pictures.cmo: pavages1.cmo permutations.cmo

pavages2_pictures.cmo: pavages2.cmo complexs.cmo

pavages1.cmo: permutations.cmo

clean:
	/bin/rm -f *.cmi *.cmo *.cmt mkpictures

.SUFFIXES:
.SUFFIXES: .ml .mli .cmi .cmo .cmt
