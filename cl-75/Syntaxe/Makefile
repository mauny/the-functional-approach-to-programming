INCLUDE=-I ../Util -I ../Arbres
#MLGRAPH=/usr/local/lib/caml-light/MLgraph
UMASK=umask 002;

all: astexpr.zo parse_prelude.zo recognize.zo recognize_val.zo predict.zo comp_re.zo 

pictures:

.ml.zo:
	$(UMASK) camlc $(INCLUDE) -c $<

.mli.zi:
	$(UMASK) camlc $(INCLUDE) -c $<

clean:
	/bin/rm -f *.zi *.zo

.SUFFIXES:
.SUFFIXES: .ml .mli .zi .zo
