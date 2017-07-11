INCLUDE=-I ../Util -I ../Arbres
MLGRAPH=/usr/local/lib/caml-light/MLgraph
UMASK=umask 002;

all: games.zo games_ane_rouge.zo games_solit.zo

pictures: mkpictures 
	$(UMASK) ./mkpictures

.ml.zo:
	$(UMASK) camlc $(INCLUDE) -c $<

.mli.zi:
	$(UMASK) camlc $(INCLUDE) -c $<



mkpictures: games_ane_rouge_pictures.zo games_solit_pictures.zo  \
            games.zo games_ane_rouge.zo games_solit.zo \
            ../Util/prelude.zo ../Util/orders.zo ../Util/lexer.zo
	$(UMASK) camlc -o mkpictures $(MLGRAPH).zo   \
	  ../Util/option.zo ../Util/graph.zo \
          ../Util/prelude.zo \
          ../Util/orders.zo \
          ../Util/lexer.zo ../Arbres/binary_trees.zo ../Arbres/sets.zo\
          games.zo games_ane_rouge.zo games_solit.zo\
          games_ane_rouge_pictures.zo games_solit_pictures.zo

games_ane_rouge_pictures.zo: games.zo games_ane_rouge.zo

games_solit_pictures.zo: games_solit.zo

clean:
	/bin/rm -f *.zi *.zo mkpictures

.SUFFIXES:
.SUFFIXES: .ml .mli .zi .zo

