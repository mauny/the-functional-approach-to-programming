INCLUDE=-I ../Util
MLGRAPH=/usr/local/lib/caml-light/MLgraph
UMASK=umask 002;
all:

pictures:  mkpictures
	$(UMASK) mkpictures


mkpictures: pictures.ml ../Util/prelude.zo 
	$(UMASK) camlc -o mkpictures $(INCLUDE)  $(MLGRAPH).zo  \
          ../Util/option.zo ../Util/graph.zo \
	  ../Util/prelude.zo  pictures.ml


clean:
	/bin/rm -f *.zi *.zo mkpictures
