MLGRAPH=/usr/local/lib/caml-light/MLgraph
#INCLUDE=-I /usr/local/lib/caml-light

UMASK=umask 002;

all: orders.zi lexer.zi prelude.zo orders.zo lexer.zo

pictures:  option.zo graph.zo tree.zo

.ml.zo:
	$(UMASK) camlc -c  $(INCLUDE) $<

.mli.zi:
	$(UMASK) camlc -c  $(INCLUDE) $<


tree.zo: option.zo graph.zo

graph.zo: option.zo


clean:
	/bin/rm -f *.zi *.zo

.SUFFIXES:
.SUFFIXES: .ml .mli .zi .zo

