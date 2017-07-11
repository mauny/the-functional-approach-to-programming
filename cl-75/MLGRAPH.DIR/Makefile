






























### The MLgraph library version 2.1

### Configuration section




MLC=camlc
MLCFLAGS=-c -O cautious
MLLIBR=camllibr
MLDEP=camldep
MLEXTOBJ=zo
MLEXTOBJI=zi
MLEXTOBJL=zo

LIBNAME=MLgraph
LIBLOAD=mlgraph.ml
FILETEST= visages

 ##### Destination files for installation

#LIBDIR=/usr/local/lib/caml-light
LIBDIR=.
#BINDIR=/usr/local/bin
BINDIR=.
TEXDIR=/usr/local/lib/tex/inputs

LIBLOAD=mlgraph.ml


GLIBDIR=$(LIBDIR)/MLgraph.lib
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


OBJS=compatibility.$(MLEXTOBJ) prelude.$(MLEXTOBJ) geometry.$(MLEXTOBJ) paint.$(MLEXTOBJ) frames.$(MLEXTOBJ) sketches.$(MLEXTOBJ) bitmaps.$(MLEXTOBJ) \
 fonts.$(MLEXTOBJ) texts.$(MLEXTOBJ) graphics_defaults.$(MLEXTOBJ) pictures.$(MLEXTOBJ)  cps.$(MLEXTOBJ) \
  trps.$(MLEXTOBJ) display.$(MLEXTOBJ) circletext.$(MLEXTOBJ) option.$(MLEXTOBJ) graph.$(MLEXTOBJ) tree.$(MLEXTOBJ) mlglatex.$(MLEXTOBJ) 

library:	$(LIBNAME).$(MLEXTOBJI) $(OBJS) $(LIBNAME).$(MLEXTOBJ)
	( mv $(LIBNAME).zo _$(LIBNAME).zo ; $(MLLIBR) -o $(LIBNAME).$(MLEXTOBJ) $(OBJS) _$(LIBNAME).$(MLEXTOBJ))

clean:
	rm -f *.$(MLEXTOBJ) *.$(MLEXTOBJI) *.gz

test:
	make -e LIBDIR=./packagedir install
	$(MLC) -o $(FILETEST).exe $(LIBNAME).$(MLEXTOBJL) test.ml $(FILETEST).ml
	$(FILETEST).exe

all: install

install: library
	if test -d $(LIBDIR); then :; else mkdir $(LIBDIR); fi
	if test -d $(GLIBDIR); then :; else mkdir $(GLIBDIR); fi
	if test -d $(GLIBDIRH); then :; else mkdir $(GLIBDIRH); fi
	if test -d $(GLIBDIRF); then :; else mkdir $(GLIBDIRF); fi
	if test -d $(GLIBDIRB); then :; else mkdir $(GLIBDIRB); fi
	if test -d $(GLIBDIRL); then :; else mkdir $(GLIBDIRL); fi
	$(INSTALL) -m 755 $(LIBNAME).$(MLEXTOBJ) $(LIBDIR)
	$(INSTALL) -m 755 $(LIBNAME).$(MLEXTOBJI) $(LIBDIR)
	$(INSTALL) -m 755 $(LIBNAME).$(MLEXTOBJL) $(LIBDIR)
	$(INSTALL) -m 755 $(LIBLOAD) $(LIBDIR)
	for i in *.fnt; do  \
		$(INSTALL) -m 755 $$i $(GLIBDIRF) ; \
	done
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

packagebin:
	make clean
	make -e LIBDIR=/usr/local/lib/caml-light
	make -e LIBDIR=./packagedir install
	((cd ./packagedir; tar -cvf ../MLgraph-bin.CLUNIX.tar .); rm -rf ./packagedir; gzip ./MLgraph-bin.CLUNIX.tar)

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
	mv Makefile Makefile.bak; sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak > Makefile; $(MLDEP) *.mli *.ml >> Makefile;



### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
bitmaps.zi: MLgraph.zi 
display.zi: MLgraph.zi pictures.zi 
fonts.zi: MLgraph.zi 
frames.zi: MLgraph.zi 
geometry.zi: MLgraph.zi 
graph.zi: prelude.zo sketches.zi compatibility.zo MLgraph.zi pictures.zi 
option.zi: prelude.zo compatibility.zo MLgraph.zi pictures.zi 
pictures.zi: texts.zi bitmaps.zi sketches.zi MLgraph.zi 
sketches.zi: MLgraph.zi 
texts.zi: MLgraph.zi 
trps.zi: texts.zi bitmaps.zi fonts.zi sketches.zi MLgraph.zi cps.zo \
    pictures.zi 
MLgraph.zo: MLgraph.zi cps.zo graphics_defaults.zo trps.zi fonts.zi \
    geometry.zi pictures.zi tree.zo bitmaps.zi prelude.zo graph.zi \
    display.zi circletext.zo texts.zi option.zi sketches.zi paint.zo \
    mlglatex.zo frames.zi 
bitmaps.zo: bitmaps.zi prelude.zo compatibility.zo MLgraph.zi geometry.zi \
    frames.zi 
circletext.zo: texts.zi prelude.zo compatibility.zo MLgraph.zi paint.zo \
    geometry.zi pictures.zi 
cps.zo: prelude.zo compatibility.zo 
display.zo: display.zi bitmaps.zi prelude.zo cps.zo graphics_defaults.zo \
    texts.zi trps.zi sketches.zi compatibility.zo fonts.zi MLgraph.zi \
    geometry.zi paint.zo frames.zi pictures.zi 
fonts.zo: fonts.zi prelude.zo compatibility.zo MLgraph.zi 
frames.zo: frames.zi prelude.zo compatibility.zo MLgraph.zi geometry.zi 
geometry.zo: geometry.zi prelude.zo compatibility.zo MLgraph.zi 
graph.zo: graph.zi prelude.zo texts.zi option.zi compatibility.zo \
    sketches.zi MLgraph.zi geometry.zi paint.zo pictures.zi 
graphics_defaults.zo: prelude.zo compatibility.zo MLgraph.zi paint.zo \
    frames.zi 
mlglatex.zo: prelude.zo display.zi texts.zi option.zi sketches.zi \
    compatibility.zo fonts.zi MLgraph.zi geometry.zi paint.zo frames.zi \
    pictures.zi 
mlgraph.zo: MLgraph.zi 
option.zo: option.zi prelude.zo compatibility.zo MLgraph.zi pictures.zi 
paint.zo: MLgraph.zi 
pictures.zo: pictures.zi bitmaps.zi prelude.zo graphics_defaults.zo \
    texts.zi compatibility.zo sketches.zi fonts.zi MLgraph.zi geometry.zi \
    paint.zo frames.zi 
prelude.zo: compatibility.zo 
sketches.zo: sketches.zi compatibility.zo MLgraph.zi geometry.zi frames.zi 
test.zo: MLgraph.zi 
texts.zo: texts.zi prelude.zo fonts.zi compatibility.zo MLgraph.zi \
    frames.zi 
tree.zo: prelude.zo graph.zi texts.zi option.zi sketches.zi \
    compatibility.zo fonts.zi MLgraph.zi geometry.zi paint.zo frames.zi \
    pictures.zi 
trps.zo: trps.zi bitmaps.zi prelude.zo cps.zo graphics_defaults.zo texts.zi \
    sketches.zi compatibility.zo fonts.zi MLgraph.zi geometry.zi paint.zo \
    frames.zi pictures.zi 
visages.zo: MLgraph.zi 
xCour-B.zo: MLgraph.zi 
xCour-BO.zo: MLgraph.zi 
xCour-O.zo: MLgraph.zi 
xCour.zo: MLgraph.zi 
xHelv-B.zo: MLgraph.zi 
xHelv-BO.zo: MLgraph.zi 
xHelv-O.zo: MLgraph.zi 
xHelv.zo: MLgraph.zi 
xSymb.zo: MLgraph.zi 
xTime-B.zo: MLgraph.zi 
xTime-BI.zo: MLgraph.zi 
xTime-I.zo: MLgraph.zi 
xTime-R.zo: MLgraph.zi 
ycps_file.zo: MLgraph.zi 
ycreatefonts.zo: MLgraph.zi 
