INCLUDE=-I ../Util -I .. -I ../MLgraph.lib
MLGRAPH=../MLgraph.lib/mlgraph
UMASK=umask 002;

all: circular_list.cmi circular_list.cmo queue.cmi queue.cmo \
	double_circular_list.cmi double_circular_list.cmo

pictures: mkpictures
	$(UMASK) ./mkpictures

.ml.cmo:
	$(UMASK) ocamlc -c $<

.mli.cmi:
	$(UMASK) ocamlc -c $<


mkpictures: pictures_imper.ml  \
	    ../Util/prelud.cmo circular_list.cmo
	$(UMASK) ocamlc -o mkpictures $(INCLUDE)  $(MLGRAPH).cma   \
	  ../Util/prelud.cmo \
	  circular_list.cmo pictures_imper.ml


clean:
	/bin/rm -f *.cmi *.cmo *.cmt mkpictures

.SUFFIXES:
.SUFFIXES: .ml .mli .cmi .cmo .cmt
