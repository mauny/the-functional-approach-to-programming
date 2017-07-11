INCLUDE=-I ../Util
UMASK=umask 002;


all: defs.zo

pictures:
	echo

clean:
	/bin/rm -f *.zi *.zo *~

.ml.zo:
	$(UMASK) camlc -c  $(INCLUDE) $<

.mli.zi:
	$(UMASK) camlc -c  $(INCLUDE) $<

.SUFFIXES:
.SUFFIXES: .ml .mli .zi .zo

