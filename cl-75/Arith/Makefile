INCLUDE=-I ../Util
UMASK=umask 002;

all: arith_list_nat.zo arith_circ_list_nat.zo arith_big_int.zo arith_rat.zo arith_pi.zo


pictures:


.ml.zo:
	$(UMASK) camlc -c  $(INCLUDE) $<

.mli.zi:
	$(UMASK) camlc -c  $(INCLUDE) $<

arith_big_int.zo: arith_list_nat.zo

arith_rat.zo: arith_list_nat.zo arith_big_int.zo

arith_pi.zo: arith_list_nat.zo arith_big_int.zo arith_rat.zo

clean:
	/bin/rm -f *.zi *.zo

.SUFFIXES:
.SUFFIXES: .ml .mli .zi .zo

