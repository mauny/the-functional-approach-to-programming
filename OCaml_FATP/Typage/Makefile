INCLUDE=-I ../Util -I ../Termes -I ../Compil -I ../Arbres -I ../MLgraph.lib
UMASK=umask 002;
MLGRAPH=../MLgraph.lib/mlgraph
MLCFLAGS=-pp camlp5o -w -8


all: ml_type.cmo type_synth.cmo type_synth_destr.cmo

pictures:  pictures_typage.cmo mkpictures
	$(UMASK) ./mkpictures

mkpictures: $(INTF) pictures_typage.cmo  \
            ../Util/prelud.cmo ../Arbres/binary_trees.cmo ../Util/lexxer.cmo \
            ../Arbres/binary_trees_parser.cmo \
            ../Arbres/binary_trees_drawing.cmo
	$(UMASK) ocamlc  $(INCLUDE) -o mkpictures $(MLGRAPH).cma  \
	  ../Util/prelud.cmo \
	  ../Util/orders.cmo \
	  ../Arbres/binary_trees.cmo \
	  ../Util/lexxer.cmo \
	  ../Arbres/binary_trees_parser.cmo \
	  ../Arbres/binary_trees_drawing.cmo \
	  ../Termes/gentree.cmo \
	  pictures_typage.cmo

.ml.cmo:
	$(UMASK) ocamlc $(MLCFLAGS) $(INCLUDE) -c $<

.mli.cmi:
	$(UMASK) ocamlc $(MLCFLAGS) $(INCLUDE) -c $<

clean:
	/bin/rm -f *.cmi *.cmo *.cmt mkpictures

.SUFFIXES:
.SUFFIXES: .ml .mli .cmi .cmo .cmt
