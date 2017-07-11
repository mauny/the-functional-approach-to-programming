INCLUDE=-I ../Util -I ../Termes -I ../Compil -I ../Arbres
UMASK=umask 002;
MLGRAPH=/usr/local/lib/caml-light/MLgraph


all: ml_type.zo type_synth.zo type_synth_destr.zo

pictures:  pictures.zo mkpictures
	$(UMASK) ./mkpictures

mkpictures: $(INTF) pictures.zo  \
            ../Util/prelude.zo ../Arbres/binary_trees.zo ../Util/lexer.zo \
            ../Arbres/binary_trees_parser.zo \
            ../Arbres/binary_trees_drawing.zo
	$(UMASK) camlc  $(INCLUDE) -o mkpictures $(MLGRAPH).zo  \
          ../Util/option.zo ../Util/graph.zo ../Util/tree.zo \
          ../Util/prelude.zo \
          ../Util/orders.zo \
          ../Arbres/binary_trees.zo \
          ../Util/lexer.zo \
          ../Arbres/binary_trees_parser.zo \
          ../Arbres/binary_trees_drawing.zo \
          ../Termes/gentree.zo \
          pictures.zo

.ml.zo:
	$(UMASK) camlc $(INCLUDE) -c $<

.mli.zi:
	$(UMASK) camlc $(INCLUDE) -c $<

clean:
	/bin/rm -f *.zi *.zo mkpictures

.SUFFIXES:
.SUFFIXES: .ml .mli .zi .zo
