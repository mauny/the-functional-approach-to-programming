INCLUDE=-I ../Util -I ..
MLGRAPH=/usr/local/lib/caml-light/MLgraph
UMASK=umask 002;

all: circular_list.zi circular_list.zo queue.zi queue.zo double_circular_list.zi double_circular_list.zo

pictures: mkpictures
	$(UMASK) ./mkpictures

.ml.zo:
	$(UMASK) camlc -c $<

.mli.zi:
	$(UMASK) camlc -c $<


mkpictures: pictures.ml  \
            ../Util/prelude.zo circular_list.zo
	$(UMASK) camlc -o mkpictures $(INCLUDE)  $(MLGRAPH).zo   \
          ../Util/prelude.zo \
          circular_list.zo pictures.ml




clean:
	/bin/rm -f *.zi *.zo mkpictures

.SUFFIXES:
.SUFFIXES: .ml .mli .zi .zo

