VERSION=0.3
LANGUAGE=V07
UMASK= umask 002;

all:
	$(UMASK) for dir in `cat DIRECTORIES`; do \
	(echo '*******' $$dir && cd $$dir && make all; echo); done

pictures:
	$(UMASK) for dir in `cat DIRECTORIES`; do \
	(echo '*******' $$dir && cd $$dir && make pictures; echo); done

clean:
	$(UMASK) for dir in `cat DIRECTORIES`; do \
	(echo '*******' $$dir && cd $$dir && make clean; echo); done

test:
	$(UMASK) for dir in `cat DIRECTORIES`; do \
	echo '-------------' "$$dir"; \
	(cd $$dir; echo '#use "load.ml" ;;' | ocaml) \
	done
