INCLUDE=-I ../Util -I ../Arbres
MLGRAPH=/usr/local/lib/caml-light/MLgraph
UMASK=umask 002;


INTF=poly_tree.zi inttree.zi exp.zi

all: $(INTF) poly_tree.zo inttree.zo exp.zo

pictures: mkpictures 
	$(UMASK) ./mkpictures

.ml.zo:
	$(UMASK) camlc $(INCLUDE) -c $<

.mli.zi:
	$(UMASK) camlc $(INCLUDE) -c $<



mkpictures: $(INTF) pictures.zo  \
            ../Util/prelude.zo ../Arbres/binary_trees.zo ../Util/lexer.zo \
            ../Arbres/binary_trees_parser.zo \
            ../Arbres/binary_trees_drawing.zo poly_tree.zo
	$(UMASK) camlc -o mkpictures $(MLGRAPH).zo  \
          ../Util/option.zo ../Util/graph.zo \
	  ../Util/orders.zo \
          ../Util/prelude.zo \
          ../Arbres/binary_trees.zo \
          ../Util/lexer.zo \
          ../Arbres/binary_trees_parser.zo \
          ../Arbres/binary_trees_drawing.zo \
          poly_tree.zo pictures.zo

clean:
	/bin/rm -f *.zi *.zo mkpictures

.SUFFIXES:
.SUFFIXES: .ml .mli .zi .zo

