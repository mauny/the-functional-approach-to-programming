INCLUDE=-I ../Util
MLCFLAGS=-bin-annot
UMASK=umask 002;


all: defs.cmo

pictures:
	echo "No pictures"

.ml.cmo:
	$(UMASK) ocamlc -c  $(MLCFLAGS) $(INCLUDE) $<

.mli.cmi:
	$(UMASK) ocamlc -c  $(MLCFLAGS) $(INCLUDE) $<

clean:
	/bin/rm -f *.cmi *.cmo *.cmt *~

.SUFFIXES:
.SUFFIXES: .ml .mli .cmi .cmo .cmt
