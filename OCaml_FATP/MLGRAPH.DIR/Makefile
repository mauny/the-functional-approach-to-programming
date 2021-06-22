
### The MLgraph library version 2.1

### Configuration section

MLC=ocamlc
MLCFLAGS=-c -bin-annot -w -14 -w -26
MLLIBR=
MLDEP=ocamldep
MLEXTOBJ=cmo
MLEXTOBJI=cmi
MLEXTOBJL=cma

LIBNAME=mlgraph
LIBLOAD=mlgraph
FILETEST= visages

 ##### Destination files for installation

LIBDIR=.
BINDIR=.
TEXDIR=.

LIBLOAD=mlgraph

GLIBDIR=../MLgraph.lib
#GLIBDIR=$(LIBDIR)/MLgraph.lib
GLIBDIRF=$(GLIBDIR)/Fonts
GLIBDIRH=$(GLIBDIR)/Headers
GLIBDIRB=$(GLIBDIR)/Bin
GLIBDIRL=$(GLIBDIR)/Lib

 ##### Copying commands

CP=cp
LN=ln -s                # or just `ln', if you don't have symbolic links
INSTALL=install -c
MV=mv

 ### End of configuration section


OBJS=compatibility.$(MLEXTOBJ) prelude.$(MLEXTOBJ) geometry.$(MLEXTOBJ) paint.$(MLEXTOBJ) frames.$(MLEXTOBJ) sketches.$(MLEXTOBJ) bitmaps.$(MLEXTOBJ)  fonts.$(MLEXTOBJ) texts.$(MLEXTOBJ) graphics_defaults.$(MLEXTOBJ) pictures.$(MLEXTOBJ)  cps.$(MLEXTOBJ)   trps.$(MLEXTOBJ) display.$(MLEXTOBJ) circletext.$(MLEXTOBJ) options.$(MLEXTOBJ) graph.$(MLEXTOBJ) tree.$(MLEXTOBJ) mlglatex.$(MLEXTOBJ) 

library:	$(LIBNAME).$(MLEXTOBJI) $(OBJS) $(LIBNAME).$(MLEXTOBJ)
	$(MLC) -a -o $(LIBNAME).$(MLEXTOBJL) $(OBJS) $(LIBNAME).$(MLEXTOBJ)

clean:
	rm -f *.$(MLEXTOBJ) *.$(MLEXTOBJI) *.gz *.cmt *.cmti visages.exe visages.ps
	rm -drf $(GLIBDIR)
	rm -rf ../PS/*
	
test:
	make clean
	make all
	make -e LIBDIR=./packagedir install
	$(MLC) -o $(FILETEST).exe $(LIBNAME).$(MLEXTOBJL) test.ml $(FILETEST).ml
	./$(FILETEST).exe

install:
	if test -d $(LIBDIR); then :; else mkdir $(LIBDIR); fi
	if test -d $(GLIBDIR); then :; else mkdir $(GLIBDIR); fi
	if test -d $(GLIBDIRH); then :; else mkdir $(GLIBDIRH); fi
	if test -d $(GLIBDIRF); then :; else mkdir $(GLIBDIRF); fi
	if test -d $(GLIBDIRB); then :; else mkdir $(GLIBDIRB); fi
	if test -d $(GLIBDIRL); then :; else mkdir $(GLIBDIRL); fi
	# $(INSTALL) -m 755 $(LIBNAME).$(MLEXTOBJ) $(LIBDIR)
	# $(INSTALL) -m 755 $(LIBNAME).$(MLEXTOBJI) $(LIBDIR)
	# $(INSTALL) -m 755 $(LIBNAME).$(MLEXTOBJL) $(LIBDIR)
	# $(INSTALL) -m 755 $(LIBLOAD) $(LIBDIR)
	
	# $(INSTALL) -m 755 mlgraph*.* $(GLIBDIR)
	$(INSTALL) -m 755 *.* $(GLIBDIR)
	$(INSTALL) -m 755 load.ml $(GLIBDIR)
	
	for i in *.fnt; do  		$(INSTALL) -m 755 $$i $(GLIBDIRF) ; 	done
	$(INSTALL) -m 755 x*.ml $(GLIBDIRL)
	$(INSTALL) -m 755 y*.ml $(GLIBDIRL)
	$(INSTALL) -m 755 createfonts.ps $(GLIBDIRB)
	$(INSTALL) -m 755 mlpictex $(GLIBDIRB)
	$(INSTALL) -m 755 cps_file.ps $(GLIBDIRH)
	$(INSTALL) -m 755 epsf.sty $(GLIBDIRH)
	$(INSTALL) -m 755 MLgraph.tex $(GLIBDIRH)

uninstall:
	rm -r $(GLIBDIR)
	rm $(LIBDIR)/$(LIBNAME).$(MLEXTOBJ)
	rm $(LIBDIR)/$(LIBNAME).$(MLEXTOBJI)
	rm $(LIBDIR)/$(LIBNAME).$(MLEXTOBJL)
	rm $(LIBDIR)/$(LIBLOAD)

all:
	make library
	make install
	
packagebin:
	make clean
	make -e LIBDIR=/usr/local/lib/ocaml/
	make -e LIBDIR=./packagedir install
	((cd ./packagedir; tar -cvf ../MLgraph-bin.OCUNIX.tar .); rm -rf ./packagedir; gzip ./MLgraph-bin.OCUNIX.tar)

packagesrc:
	make clean
	mkdir ./MLGRAPH.DIR
	for i in  *; do  if test -f $$i; then cp $$i ./MLGRAPH.DIR; fi; done
	tar -cvf ./MLgraph-src.tar ./MLGRAPH.DIR 
	rm -rf ./MLGRAPH.DIR; 
	gzip MLgraph-src.tar

.IGNORE:
		
.SUFFIXES :
.SUFFIXES : .mli .ml .$(MLEXTOBJI) .$(MLEXTOBJ)

.mli.$(MLEXTOBJI):
	$(MLC) $(MLCFLAGS) $<

.ml.$(MLEXTOBJ):
	$(MLC) $(MLCFLAGS) $<

depend:
	mv Makefile Makefile.bak; sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak > Makefile;  $(MLDEP) *.mli *.ml >> Makefile;



### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
bitmaps.cmo : \
    prelude.cmo \
    geometry.cmi \
    frames.cmi \
    compatibility.cmo \
    bitmaps.cmi
bitmaps.cmx : \
    prelude.cmx \
    geometry.cmx \
    frames.cmx \
    compatibility.cmx \
    bitmaps.cmi
bitmaps.cmi : \
    geometry.cmi \
    frames.cmi \
    compatibility.cmo
circletext.cmo : \
    texts.cmi \
    prelude.cmo \
    pictures.cmi \
    paint.cmo \
    geometry.cmi \
    compatibility.cmo
circletext.cmx : \
    texts.cmx \
    prelude.cmx \
    pictures.cmx \
    paint.cmx \
    geometry.cmx \
    compatibility.cmx
compatibility.cmo :
compatibility.cmx :
cps.cmo :
cps.cmx :
display.cmo : \
    trps.cmi \
    texts.cmi \
    sketches.cmi \
    prelude.cmo \
    pictures.cmi \
    paint.cmo \
    graphics_defaults.cmo \
    geometry.cmi \
    frames.cmi \
    fonts.cmi \
    cps.cmo \
    compatibility.cmo \
    bitmaps.cmi \
    display.cmi
display.cmx : \
    trps.cmx \
    texts.cmx \
    sketches.cmx \
    prelude.cmx \
    pictures.cmx \
    paint.cmx \
    graphics_defaults.cmx \
    geometry.cmx \
    frames.cmx \
    fonts.cmx \
    cps.cmx \
    compatibility.cmx \
    bitmaps.cmx \
    display.cmi
display.cmi : \
    pictures.cmi \
    frames.cmi
fonts.cmo : \
    prelude.cmo \
    compatibility.cmo \
    fonts.cmi
fonts.cmx : \
    prelude.cmx \
    compatibility.cmx \
    fonts.cmi
fonts.cmi : \
    compatibility.cmo
frames.cmo : \
    prelude.cmo \
    geometry.cmi \
    compatibility.cmo \
    frames.cmi
frames.cmx : \
    prelude.cmx \
    geometry.cmx \
    compatibility.cmx \
    frames.cmi
frames.cmi : \
    geometry.cmi
geometry.cmo : \
    prelude.cmo \
    compatibility.cmo \
    geometry.cmi
geometry.cmx : \
    prelude.cmx \
    compatibility.cmx \
    geometry.cmi
geometry.cmi :
graph.cmo : \
    texts.cmi \
    sketches.cmi \
    prelude.cmo \
    pictures.cmi \
    paint.cmo \
    options.cmi \
    graphics_defaults.cmo \
    geometry.cmi \
    frames.cmi \
    fonts.cmi \
    compatibility.cmo \
    graph.cmi
graph.cmx : \
    texts.cmx \
    sketches.cmx \
    prelude.cmx \
    pictures.cmx \
    paint.cmx \
    options.cmx \
    graphics_defaults.cmx \
    geometry.cmx \
    frames.cmx \
    fonts.cmx \
    compatibility.cmx \
    graph.cmi
graph.cmi : \
    texts.cmi \
    sketches.cmi \
    prelude.cmo \
    pictures.cmi \
    paint.cmo \
    options.cmi \
    graphics_defaults.cmo \
    geometry.cmi \
    frames.cmi \
    fonts.cmi \
    compatibility.cmo
graphics_defaults.cmo : \
    prelude.cmo \
    paint.cmo \
    frames.cmi \
    compatibility.cmo
graphics_defaults.cmx : \
    prelude.cmx \
    paint.cmx \
    frames.cmx \
    compatibility.cmx
load.cmo : \
    mlgraph.cmi
load.cmx : \
    mlgraph.cmx
mlglatex.cmo : \
    texts.cmi \
    sketches.cmi \
    prelude.cmo \
    pictures.cmi \
    paint.cmo \
    options.cmi \
    graphics_defaults.cmo \
    geometry.cmi \
    frames.cmi \
    fonts.cmi \
    display.cmi \
    compatibility.cmo
mlglatex.cmx : \
    texts.cmx \
    sketches.cmx \
    prelude.cmx \
    pictures.cmx \
    paint.cmx \
    options.cmx \
    graphics_defaults.cmx \
    geometry.cmx \
    frames.cmx \
    fonts.cmx \
    display.cmx \
    compatibility.cmx
mlgraph.cmo : \
    trps.cmi \
    tree.cmo \
    texts.cmi \
    sketches.cmi \
    prelude.cmo \
    pictures.cmi \
    paint.cmo \
    options.cmi \
    mlglatex.cmo \
    graphics_defaults.cmo \
    graph.cmi \
    geometry.cmi \
    frames.cmi \
    fonts.cmi \
    display.cmi \
    cps.cmo \
    compatibility.cmo \
    circletext.cmo \
    bitmaps.cmi \
    mlgraph.cmi
mlgraph.cmx : \
    trps.cmx \
    tree.cmx \
    texts.cmx \
    sketches.cmx \
    prelude.cmx \
    pictures.cmx \
    paint.cmx \
    options.cmx \
    mlglatex.cmx \
    graphics_defaults.cmx \
    graph.cmx \
    geometry.cmx \
    frames.cmx \
    fonts.cmx \
    display.cmx \
    cps.cmx \
    compatibility.cmx \
    circletext.cmx \
    bitmaps.cmx \
    mlgraph.cmi
mlgraph.cmi : \
    compatibility.cmo
options.cmo : \
    texts.cmi \
    prelude.cmo \
    pictures.cmi \
    paint.cmo \
    graphics_defaults.cmo \
    geometry.cmi \
    frames.cmi \
    fonts.cmi \
    compatibility.cmo \
    options.cmi
options.cmx : \
    texts.cmx \
    prelude.cmx \
    pictures.cmx \
    paint.cmx \
    graphics_defaults.cmx \
    geometry.cmx \
    frames.cmx \
    fonts.cmx \
    compatibility.cmx \
    options.cmi
options.cmi : \
    texts.cmi \
    prelude.cmo \
    pictures.cmi \
    paint.cmo \
    graphics_defaults.cmo \
    geometry.cmi \
    frames.cmi \
    fonts.cmi \
    compatibility.cmo
paint.cmo :
paint.cmx :
pictures.cmo : \
    texts.cmi \
    sketches.cmi \
    prelude.cmo \
    paint.cmo \
    graphics_defaults.cmo \
    geometry.cmi \
    frames.cmi \
    fonts.cmi \
    compatibility.cmo \
    bitmaps.cmi \
    pictures.cmi
pictures.cmx : \
    texts.cmx \
    sketches.cmx \
    prelude.cmx \
    paint.cmx \
    graphics_defaults.cmx \
    geometry.cmx \
    frames.cmx \
    fonts.cmx \
    compatibility.cmx \
    bitmaps.cmx \
    pictures.cmi
pictures.cmi : \
    texts.cmi \
    sketches.cmi \
    paint.cmo \
    graphics_defaults.cmo \
    geometry.cmi \
    frames.cmi \
    fonts.cmi \
    bitmaps.cmi
prelude.cmo : \
    compatibility.cmo
prelude.cmx : \
    compatibility.cmx
sketches.cmo : \
    geometry.cmi \
    frames.cmi \
    compatibility.cmo \
    sketches.cmi
sketches.cmx : \
    geometry.cmx \
    frames.cmx \
    compatibility.cmx \
    sketches.cmi
sketches.cmi : \
    geometry.cmi \
    frames.cmi
test.cmo : \
    mlgraph.cmi
test.cmx : \
    mlgraph.cmx
texts.cmo : \
    prelude.cmo \
    frames.cmi \
    fonts.cmi \
    compatibility.cmo \
    texts.cmi
texts.cmx : \
    prelude.cmx \
    frames.cmx \
    fonts.cmx \
    compatibility.cmx \
    texts.cmi
texts.cmi : \
    frames.cmi \
    fonts.cmi
tree.cmo : \
    texts.cmi \
    sketches.cmi \
    prelude.cmo \
    pictures.cmi \
    paint.cmo \
    options.cmi \
    graphics_defaults.cmo \
    graph.cmi \
    geometry.cmi \
    frames.cmi \
    fonts.cmi \
    compatibility.cmo
tree.cmx : \
    texts.cmx \
    sketches.cmx \
    prelude.cmx \
    pictures.cmx \
    paint.cmx \
    options.cmx \
    graphics_defaults.cmx \
    graph.cmx \
    geometry.cmx \
    frames.cmx \
    fonts.cmx \
    compatibility.cmx
trps.cmo : \
    texts.cmi \
    sketches.cmi \
    prelude.cmo \
    pictures.cmi \
    paint.cmo \
    graphics_defaults.cmo \
    geometry.cmi \
    frames.cmi \
    fonts.cmi \
    cps.cmo \
    compatibility.cmo \
    bitmaps.cmi \
    trps.cmi
trps.cmx : \
    texts.cmx \
    sketches.cmx \
    prelude.cmx \
    pictures.cmx \
    paint.cmx \
    graphics_defaults.cmx \
    geometry.cmx \
    frames.cmx \
    fonts.cmx \
    cps.cmx \
    compatibility.cmx \
    bitmaps.cmx \
    trps.cmi
trps.cmi : \
    texts.cmi \
    sketches.cmi \
    prelude.cmo \
    pictures.cmi \
    paint.cmo \
    graphics_defaults.cmo \
    geometry.cmi \
    frames.cmi \
    fonts.cmi \
    cps.cmo \
    compatibility.cmo \
    bitmaps.cmi
xCour-B.cmo : \
    mlgraph.cmi
xCour-B.cmx : \
    mlgraph.cmx
xCour-BO.cmo : \
    mlgraph.cmi
xCour-BO.cmx : \
    mlgraph.cmx
xCour-O.cmo : \
    mlgraph.cmi
xCour-O.cmx : \
    mlgraph.cmx
xCour.cmo : \
    mlgraph.cmi
xCour.cmx : \
    mlgraph.cmx
xHelv-B.cmo : \
    mlgraph.cmi
xHelv-B.cmx : \
    mlgraph.cmx
xHelv-BO.cmo : \
    mlgraph.cmi
xHelv-BO.cmx : \
    mlgraph.cmx
xHelv-O.cmo : \
    mlgraph.cmi
xHelv-O.cmx : \
    mlgraph.cmx
xHelv.cmo : \
    mlgraph.cmi
xHelv.cmx : \
    mlgraph.cmx
xSymb.cmo : \
    mlgraph.cmi
xSymb.cmx : \
    mlgraph.cmx
xTime-B.cmo : \
    mlgraph.cmi
xTime-B.cmx : \
    mlgraph.cmx
xTime-BI.cmo : \
    mlgraph.cmi
xTime-BI.cmx : \
    mlgraph.cmx
xTime-I.cmo : \
    mlgraph.cmi
xTime-I.cmx : \
    mlgraph.cmx
xTime-R.cmo : \
    mlgraph.cmi
xTime-R.cmx : \
    mlgraph.cmx
ycps_file.cmo : \
    mlgraph.cmi
ycps_file.cmx : \
    mlgraph.cmx
ycreatefonts.cmo :
ycreatefonts.cmx :
