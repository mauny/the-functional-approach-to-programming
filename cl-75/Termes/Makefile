INCLUDE=-I ../Util
MLGRAPH=/usr/local/lib/caml-light/MLgraph
UMASK=umask 002;

INTF=gentree.zi termes.zi


all: $(INTF) gentree.zo termes.zo type_synthesis.zo

pictures: mkpictures
	$(UMASK) ./mkpictures 


.ml.zo:
	$(UMASK) camlc $(INCLUDE) -c $<

.mli.zi:
	$(UMASK) camlc $(INCLUDE) -c $<



mkpictures: $(INTF) pictures.zo ../Util/prelude.zo ../Util/lexer.zo \
            gentree.zo
	$(UMASK) camlc -o mkpictures $(MLGRAPH).zo   \
          ../Util/option.zo ../Util/graph.zo ../Util/tree.zo \
	  ../Util/prelude.zo \
          ../Util/lexer.zo \
          gentree.zo pictures.zo

clean:
	/bin/rm -f *.zi *.zo mkpictures

.SUFFIXES:
.SUFFIXES: .ml .mli .zi .zo

