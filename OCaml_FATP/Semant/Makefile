INCLUDE=-I ../Util -I ../MLgraph.lib
MLGRAPH=..MLgraph.lib/mlgraph
UMASK=umask 002;

all:

pictures:  mkpictures
	$(UMASK) ./mkpictures


mkpictures: pictures_semant.ml ../Util/prelud.cmo
	$(UMASK) ocamlc -o mkpictures $(INCLUDE)  mlgraph.cma  \
	  pictures_semant.ml


clean:
	/bin/rm -f *.cmi *.cmo *.cmt mkpictures
