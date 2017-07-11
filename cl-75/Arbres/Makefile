INCLUDE=-I ../Util
MLGRAPH=/usr/local/lib/caml-light/MLgraph
UMASK=umask 002;

all: binary_trees.zo binary_trees_parser.zo binary_trees_drawing.zo dictionnaries.zo sets.zo

pictures: mkpictures
	$(UMASK) ./mkpictures

INTF=binary_trees.zi binary_trees_drawing.zi

.ml.zo:
	$(UMASK) camlc $(INCLUDE) -c $<

.mli.zi:
	$(UMASK) camlc $(INCLUDE) -c $<



mkpictures: arbres_pictures.zo binary_trees.zo  \
            binary_trees_parser.zo binary_trees_drawing.zo \
            ../Util/prelude.zo ../Util/orders.zo ../Util/lexer.zo
	$(UMASK) camlc -I ../Util/ -o mkpictures $(MLGRAPH).zo \
          option.zo graph.zo \
	  prelude.zo \
          orders.zo \
          lexer.zo \
          binary_trees.zo binary_trees_parser.zo \
          binary_trees_drawing.zo arbres_pictures.zo

arbres_pictures.zo: binary_trees.zo binary_trees_parser.zo binary_trees_drawing.zo 

clean:
	/bin/rm -f *.zi *.zo mkpictures

.SUFFIXES:
.SUFFIXES: .ml .mli .zi .zo

