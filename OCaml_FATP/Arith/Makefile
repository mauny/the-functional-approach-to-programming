INCLUDE=-I ../Util
UMASK=umask 002;
MLCFLAGS=-bin-annot -pp camlp5o

all: arith_list_nat.cmo arith_circ_list_nat.cmo arith_big_int.cmo arith_rat.cmo arith_pi.cmo


pictures:
	echo "No pictures"

.ml.cmo:
	$(UMASK) ocamlc -c $(MLCFLAGS) $(INCLUDE) $<

.mli.cmi:
	$(UMASK) ocamlc -c $(MLCFLAGS) $(INCLUDE) $<

arith_big_int.cmo: arith_list_nat.cmo

arith_rat.cmo: arith_list_nat.cmo arith_big_int.cmo

arith_pi.cmo: arith_list_nat.cmo arith_big_int.cmo arith_rat.cmo

clean:
	/bin/rm -f *.cmi *.cmo *.cmt

.SUFFIXES:
.SUFFIXES: .ml .mli .cmi .cmo .cmt
