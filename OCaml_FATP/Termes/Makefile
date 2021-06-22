INCLUDE=-I ../Util -I ../Eval -I ../MLgraph.lib
MLGRAPH=../MLgraph.lib/mlgraph
UMASK=umask 002;
MLCFLAGS=-bin-annot -pp camlp5o

INTF=gentree.cmi termes.cmi


all: $(INTF) gentree.cmo termes.cmo type_synthesis.cmo

pictures: mkpictures
	$(UMASK) ./mkpictures


.ml.cmo:
	$(UMASK) ocamlc $(MLCFLAGS) $(INCLUDE) -c $<

.mli.cmi:
	$(UMASK) ocamlc $(MLCFLAGS) $(INCLUDE) -c $<



mkpictures: $(INTF) pictures_termes.cmo ../Util/prelud.cmo ../Util/lexxer.cmo \
	    gentree.cmo
	$(UMASK) ocamlc -o mkpictures $(MLGRAPH).cma   \
	  ../Util/prelud.cmo \
	  ../Util/lexxer.cmo \
	  gentree.cmo pictures_termes.cmo

type_synthesis.cmo: ../Eval/ml_ops.cmo ../Eval/ml_strict.cmo ../Eval/parser_strict.cmo

clean:
	/bin/rm -f *.cmi *.cmo *.cmt *.cmti mkpictures

.SUFFIXES:
.SUFFIXES: .ml .mli .cmi .cmo .cmt .cmti
