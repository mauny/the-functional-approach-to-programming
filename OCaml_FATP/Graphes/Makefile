INCLUDE=-I ../Util -I ../Arbres -I ../MLgraph.lib
MLGRAPH=../MLgraph.lib/mlgraph
UMASK=umask 002;
MLCFLAGS=-pp camlp5o -w -8

all: games.cmo games_ane_rouge.cmo games_solit.cmo

pictures: mkpictures
	$(UMASK) ./mkpictures

.ml.cmo:
	$(UMASK) ocamlc $(MLCFLAGS) $(INCLUDE) -c $<

.mli.cmi:
	$(UMASK) ocamlc $(MLCFLAGS) $(INCLUDE) -c $<



mkpictures: games_ane_rouge_pictures.cmo games_solit_pictures.cmo  \
            games.cmo games_ane_rouge.cmo games_solit.cmo \
            ../Util/prelud.cmo ../Util/orders.cmo ../Util/lexxer.cmo
	$(UMASK) ocamlc -o mkpictures $(MLGRAPH).cma   \
	  ../Util/prelud.cmo \
	  ../Util/orders.cmo \
	  ../Util/lexxer.cmo ../Arbres/binary_trees.cmo ../Arbres/sets.cmo \
	  games.cmo games_ane_rouge.cmo games_solit.cmo \
	  games_ane_rouge_pictures.cmo games_solit_pictures.cmo

games_ane_rouge_pictures.cmo: games.cmo games_ane_rouge.cmo

games_solit_pictures.cmo: games_solit.cmo

clean:
	/bin/rm -f *.cmi *.cmo *.cmt mkpictures

.SUFFIXES:
.SUFFIXES: .ml .mli .cmi .cmo .cmt
