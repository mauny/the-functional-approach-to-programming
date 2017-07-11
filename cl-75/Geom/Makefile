INCLUDE=-I ../Util -I ../Arbres
MLGRAPH=/usr/local/lib/caml-light/MLgraph
UMASK=umask 002;

all: permutations.zo complex.zo pavages1.zo pavages2.zo

pictures: mkpictures
	$(UMASK) ./mkpictures

.ml.zo:
	$(UMASK) camlc $(INCLUDE) -c $<

.mli.zi:
	$(UMASK) camlc $(INCLUDE) -c $<



mkpictures: geom_pictures.zo pavages1_pictures.zo  pavages2_pictures.zo  \
            circle_limit_pictures.zo \
            pavages1.zo pavages2.zo complex.zo permutations.zo \
            ../Util/prelude.zo ../Util/orders.zo ../Util/lexer.zo
	camlc -o mkpictures $(MLGRAPH).zo   \
          ../Util/prelude.zo ../Util/orders.zo \
          ../Util/lexer.zo ../Arbres/binary_trees.zo ../Arbres/sets.zo\
          ../Arbres/binary_trees_parser.zo ../Arbres/binary_trees_drawing.zo \
          permutations.zo complex.zo pavages1.zo pavages2.zo\
          pavages1_pictures.zo pavages2_pictures.zo \
          circle_limit_pictures.zo geom_pictures.zo

pavages1_pictures.zo: pavages1.zo permutations.zo

pavages2_pictures.zo: pavages2.zo complex.zo

pavages1.zo: permutations.zo

clean:
	/bin/rm -f *.zi *.zo mkpictures

.SUFFIXES:
.SUFFIXES: .ml .mli .zi .zo

