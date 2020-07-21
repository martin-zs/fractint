SHELL = /bin/sh
STRIP = strip
INSTALL = /usr/bin/install

# Architecture
# automatic detection
ARCH = `uname -m | tr "_" "-"`
# ARCH = pentium
# ARCH = x86_64
# ARCH = athlon64
# ARCH = i386
# ARCH = i686

# Optimization flags
OPT = -O2
# OPT = -O

ifndef PREFIX
PREFIX = /usr
endif
ifndef DESTDIR
DESTDIR = $(PREFIX)
endif

# SRCDIR should be a path to the directory that will hold fractint.hlp
# SRCDIR should also hold the .par, .frm, etc. files
SRCDIR = $(DESTDIR)/share/xfractint
SHRDIR = $(PREFIX)/share/xfractint
# BINDIR is where you put your X11 binaries
BINDIR = $(DESTDIR)/bin
# MANDIR is where you put your chapter 1 man pages
MANDIR = $(DESTDIR)/share/man/man1

HFD = ./headers
UDIR = ./sdl
COMDIR = ./common
DOSHELPDIR = ./dos_help

FDIR = formulas
IDIR = ifs
LDIR = lsystem
MDIR = maps
PDIR = pars
XDIR = extra

PWD = $(shell pwd)
BASEDIR = $(shell basename ${PWD})

NOBSTRING =
HAVESTRI =
DEBUG =

# For Red Hat Linux, uncomment the NOBSTRING line.
# For Cygwin, uncomment the NOBSTRING and HAVESTRI lines below.
#

NOBSTRING = -DNOBSTRING
#HAVESTRI = -DHAVESTRI
#DEBUG adds some sanity checking but will slow xfractint down
#DEBUG = -DEBUG
# If your compiler doesn't handle void *, define -DBADVOID
# If you get SIGFPE errors define -DFPUERR
DEFINES = -DXFRACT $(NOBSTRING) $(HAVESTRI) $(DEBUG)

# Uncomment this if you get errors about "stdarg.h" missing.
#DEFINES += -DUSE_VARARGS

# Uncomment this for Cygwin
#DEFINES += -DCYGWIN -DDIRENT

# For using nasm, set:
AS = /usr/bin/nasm
# Note that because of the differences between the assembler syntaxes,
#  nasm is the only one that will work.
#AS = foo

# Below is for Linux with output file type of elf, turn all warnings on
ifeq ($(ARCH),x86_64)
AFLAGS = -Ox -f elf64 -w+orphan-labels
else
AFLAGS = -Ox -f elf -w+orphan-labels
endif

#Maybe -D_CONST will fix problems with constant type in include files?
#If your version of Linux doesn't define SignalHandler add -DNOSIGHAND to CFLAGS
#If you have the nasm assembler on your system add -DNASM to CFLAGS

ifeq ($(AS),/usr/bin/nasm)

CFLAGS = -I$(HFD) $(DEFINES) -g $(shell sdl2-config --cflags) -DBIG_ANSI_C -DNASM -fno-builtin -no-pie
#CFLAGS = -I. -D_CONST $(DEFINES)
#CFLAGS = -I$(HFD) $(DEFINES) -g $(shell sdl2-config --cflags) -DBIG_ANSI_C \
#         -march=$(ARCH) -DNASM -fno-builtin -no-pie
#CFLAGS = -I. $(DEFINES) -g $(shell sdl2-config --cflags) -DBIG_ANSI_C -Os -DNASM -fno-builtin -no-pie

else

CFLAGS = -I$(HFD) $(DEFINES) -g $(shell sdl2-config --cflags) -DBIG_ANSI_C -fno-builtin -no-pie
#CFLAGS = -I$(HFD) $(DEFINES) -g $(shell sdl2-config --cflags) -DBIG_ANSI_C \
#         -march=$(ARCH) -fno-builtin -no-pie
#CFLAGS = -I. $(DEFINES) -g $(shell sdl2-config --cflags) -DBIG_ANSI_C -Os -fno-builtin -no-pie

endif

# Gcc is often the only compiler that works for this
#CC = gcc
CC = /usr/bin/gcc

# For Linux, use
# LIBS = $(shell sdl2-config --cflags) -lSDL2_ttf -lm

LIBS = $(shell sdl2-config --libs) -lSDL2_ttf -lm

OLDSRC = \
$(COMDIR)/3d.c $(COMDIR)/ant.c $(COMDIR)/bigflt.c $(COMDIR)/biginit.c \
$(COMDIR)/bignum.c $(COMDIR)/bignumc.c $(COMDIR)/calcfrac.c \
$(COMDIR)/cmdfiles.c $(COMDIR)/decoder.c $(COMDIR)/diskvid.c \
$(COMDIR)/editpal.c $(COMDIR)/encoder.c $(COMDIR)/evolve.c \
$(COMDIR)/fracsubr.c $(COMDIR)/fractalb.c $(COMDIR)/fractalp.c \
$(COMDIR)/fractals.c $(COMDIR)/fractint.c $(COMDIR)/framain2.c \
$(COMDIR)/frasetup.c $(COMDIR)/gifview.c $(COMDIR)/hcmplx.c \
$(COMDIR)/help.c $(COMDIR)/history.c $(COMDIR)/intro.c \
$(COMDIR)/jb.c $(COMDIR)/jiim.c $(COMDIR)/line3d.c \
$(COMDIR)/loadfile.c $(COMDIR)/loadmap.c $(COMDIR)/lorenz.c \
$(COMDIR)/lsys.c $(COMDIR)/lsysf.c $(COMDIR)/math_c.c \
$(COMDIR)/memory.c $(COMDIR)/miscfrac.c $(COMDIR)/miscovl.c \
$(COMDIR)/miscres.c $(COMDIR)/parser.c $(COMDIR)/parserfp.c \
$(COMDIR)/plot3d.c $(COMDIR)/prompts1.c $(COMDIR)/prompts2.c \
$(COMDIR)/realdos.c $(COMDIR)/rotate.c $(COMDIR)/slideshw.c \
$(COMDIR)/soi.c $(COMDIR)/soi1.c $(COMDIR)/stereo.c \
$(COMDIR)/testpt.c $(COMDIR)/zoom.c \
$(COMDIR)/Makefile

NEWSRC = \
$(UDIR)/calcmand.c $(UDIR)/calmanfp.c  $(UDIR)/faccess.c \
$(UDIR)/fpu087.c $(UDIR)/general.c $(UDIR)/sdl_scr.c \
$(UDIR)/sdl_snd.c $(UDIR)/video.c \
$(UDIR)/Makefile $(UDIR)/xfract_a-64.inc \
$(UDIR)/bignumax-64.asm $(UDIR)/calmanfx-64.asm $(UDIR)/fpu387ax-64.asm \
$(UDIR)/generalax-64.asm $(UDIR)/parserax-64.asm

HEADERS = \
$(HFD)/big.h $(HFD)/biginit.h $(HFD)/cmplx.h $(HFD)/externs.h \
$(HFD)/fractint.h $(HFD)/fractype.h $(HFD)/helpcom.h \
$(HFD)/lsys.h $(HFD)/port.h $(HFD)/prototyp.h

DOCS = debugfla.txt fractsrc.txt hc.txt

HELPFILES = \
$(DOSHELPDIR)/help.src $(DOSHELPDIR)/help2.src $(DOSHELPDIR)/help3.src \
$(DOSHELPDIR)/help4.src $(DOSHELPDIR)/help5.src

SRCFILES = $(OLDSRC) $(NEWSRC) $(HELPFILES) $(HEADERS) $(DOCS)

PARFILES = \
$(PDIR)/cellular.par $(PDIR)/demo.par $(PDIR)/fract18.par \
$(PDIR)/fract19.par $(PDIR)/fract200.par $(PDIR)/fractint.par \
$(PDIR)/icons.par $(PDIR)/lyapunov.par $(PDIR)/music.par \
$(PDIR)/newphoen.par $(PDIR)/orbits.par $(PDIR)/phoenix.par

FRMFILES = \
$(FDIR)/fractint.frm $(FDIR)/fract200.frm $(FDIR)/fract196.frm \
$(FDIR)/fract001.frm $(FDIR)/fract002.frm $(FDIR)/fract003.frm \
$(FDIR)/fract_sy.frm $(FDIR)/ikenaga.frm $(FDIR)/julitile.frm \
$(FDIR)/new_if.frm $(FDIR)/newton.frm

IFSFILES = $(IDIR)/fractint.ifs

LFILES = $(LDIR)/fractint.l $(LDIR)/penrose.l $(LDIR)/tiling.l

MAPFILES = \
$(MDIR)/altern.map $(MDIR)/blues.map $(MDIR)/chroma.map \
$(MDIR)/default.map $(MDIR)/firestrm.map $(MDIR)/froth3.map \
$(MDIR)/froth316.map $(MDIR)/froth6.map $(MDIR)/froth616.map \
$(MDIR)/gamma1.map $(MDIR)/gamma2.map $(MDIR)/glasses1.map \
$(MDIR)/glasses2.map $(MDIR)/goodega.map $(MDIR)/green.map \
$(MDIR)/grey.map $(MDIR)/grid.map $(MDIR)/headache.map \
$(MDIR)/landscap.map $(MDIR)/lyapunov.map $(MDIR)/neon.map \
$(MDIR)/paintjet.map $(MDIR)/royal.map $(MDIR)/topo.map $(MDIR)/volcano.map

XTRAFILES = \
$(XDIR)/all_maps.zip $(XDIR)/frmtut.zip $(XDIR)/if_else.zip \
$(XDIR)/phctutor.zip

OLDRUN = $(PARFILES) $(FRMFILES) $(IFSFILES) $(LFILES) $(MAPFILES)

NEWRUN = fractint.doc read.me

NEWFILES = $(UDIR)/$(NEWSRC) $(NEWRUN)

RUNFILES = $(OLDRUN) $(NEWRUN)

FILES = $(SRCFILES) $(RUNFILES)

OBJS = \
$(COMDIR)/3d.o $(COMDIR)/ant.o $(COMDIR)/bigflt.o  $(COMDIR)/biginit.o \
$(COMDIR)/bignum.o $(COMDIR)/bignumc.o $(COMDIR)/calcfrac.o \
$(COMDIR)/cmdfiles.o $(COMDIR)/decoder.o $(COMDIR)/diskvid.o \
$(COMDIR)/editpal.o $(COMDIR)/encoder.o $(COMDIR)/evolve.o \
$(COMDIR)/fracsubr.o $(COMDIR)/fractalb.o $(COMDIR)/fractalp.o \
$(COMDIR)/fractals.o $(COMDIR)/fractint.o $(COMDIR)/framain2.o \
$(COMDIR)/frasetup.o $(COMDIR)/gifview.o $(COMDIR)/hcmplx.o \
$(COMDIR)/help.o $(COMDIR)/history.o $(COMDIR)/intro.o $(COMDIR)/jb.o \
$(COMDIR)/jiim.o $(COMDIR)/line3d.o $(COMDIR)/loadfile.o \
$(COMDIR)/loadmap.o $(COMDIR)/lorenz.o $(COMDIR)/lsys.o \
$(COMDIR)/lsysf.o $(COMDIR)/math_c.o $(COMDIR)/memory.o \
$(COMDIR)/miscfrac.o $(COMDIR)/miscovl.o $(COMDIR)/miscres.o \
$(COMDIR)/parser.o $(COMDIR)/parserfp.o $(COMDIR)/plot3d.o \
$(COMDIR)/prompts1.o $(COMDIR)/prompts2.o $(COMDIR)/realdos.o \
$(COMDIR)/rotate.o $(COMDIR)/slideshw.o $(COMDIR)/soi.o \
$(COMDIR)/soi1.o $(COMDIR)/stereo.o $(COMDIR)/testpt.o \
$(COMDIR)/zoom.o


ifeq ($(AS),/usr/bin/nasm)

U_OBJS = \
$(UDIR)/calcmand.o $(UDIR)/calmanfp.o  $(UDIR)/faccess.o \
$(UDIR)/fpu087.o $(UDIR)/general.o $(UDIR)/sdl_scr.o \
$(UDIR)/sdl_snd.o $(UDIR)/video.o \
$(UDIR)/bignumax-64.o $(UDIR)/calmanfx-64.o $(UDIR)/fpu387ax-64.o \
$(UDIR)/generalax-64.o $(UDIR)/parserax-64.o

else

U_OBJS = \
$(UDIR)/calcmand.o $(UDIR)/calmanfp.o  $(UDIR)/faccess.o \
$(UDIR)/fpu087.o $(UDIR)/general.o $(UDIR)/sdl_scr.o \
$(UDIR)/sdl_snd.o $(UDIR)/video.o \

endif

HOBJS = $(DOSHELPDIR)/hc.o unix.o

#Need to prevent lex from doing fractint.l -> fractint.c
.SUFFIXES:
.SUFFIXES: .o .c .s .h .asm

xfractint: fractint.hlp $(SRCFILES)
	if [ -f $(DOSHELPDIR)/helpdefs.h ] ; then mv -f $(DOSHELPDIR)/helpdefs.h $(HFD) ; fi
	cd common ; ${MAKE} all "CC=${CC}" "CFLAGS= -I.${HFD} ${CFLAGS} ${OPT}" "SRCDIR=${SHRDIR}" \
	          "HFD=.${HFD}"
	cd sdl ; ${MAKE} all "CC=${CC}" "CFLAGS= -I.${HFD} ${CFLAGS} ${OPT}" "SRCDIR=${SHRDIR}" \
	          "AS=${AS}" "AFLAGS=${AFLAGS}" "HFD=.${HFD}"
	$(CC) -o xfractint $(CFLAGS) $(OPT) $(OBJS) $(U_OBJS) $(LIBS)
#	strip xfractint

# tar: $(FILES)
#	tar cvfj xfractint.tar.bz2 $(FILES)

tar: clean
	cd .. ; tar cvfj $(BASEDIR).tar.bz2 $(BASEDIR)

tidy:
	rm -f $(HOBJS)
	cd common ; ${MAKE} tidy
	cd sdl ; ${MAKE} tidy

clean:
	rm -f build-stamp *~ */*~ core
	rm -f $(HOBJS) fractint.doc fractint.hlp hc fractint xfractint
	rm -f $(HFD)/helpdefs.h
	cd $(COMDIR) ; ${MAKE} clean
	cd $(UDIR) ; ${MAKE} clean

install: xfractint fractint.hlp
	$(STRIP) xfractint
	$(INSTALL) -d $(BINDIR) $(MANDIR) $(SRCDIR)/$(PDIR) $(SRCDIR)/$(FDIR) \
		$(SRCDIR)/$(IDIR) $(SRCDIR)/$(LDIR) $(SRCDIR)/$(MDIR) $(SRCDIR)/$(XDIR)
	$(INSTALL) xfractint -T $(BINDIR)/xfractint;
	$(INSTALL) -m 644 -T $(MANDIR)/xfractint.1;
	$(INSTALL) -m 644 -t $(SRCDIR) fractint.hlp sstools.ini $(DOCS)
	$(INSTALL) -m 644 -t $(SRCDIR)/$(PDIR) $(PARFILES)
	$(INSTALL) -m 644 -t $(SRCDIR)/$(FDIR) $(FRMFILES)
	$(INSTALL) -m 644 -t $(SRCDIR)/$(IDIR) $(IFSFILES)
	$(INSTALL) -m 644 -t $(SRCDIR)/$(LDIR) $(LFILES)
	$(INSTALL) -m 644 -t $(SRCDIR)/$(MDIR) $(MAPFILES)
	$(INSTALL) -m 644 -t $(SRCDIR)/$(XDIR) $(XTRAFILES)

uninstall:
	cd $(SRCDIR); rm -f $(PARFILES)
	cd $(SRCDIR); rm -f $(FRMFILES)
	cd $(SRCDIR); rm -f $(IFSFILES)
	cd $(SRCDIR); rm -f $(LFILES)
	cd $(SRCDIR); rm -f $(MAPFILES)
	cd $(SRCDIR); rm -f $(XTRAFILES)
	cd $(SRCDIR); rm -f fractint.hlp sstools.ini $(DOCS)
	cd $(SRCDIR); rmdir $(PDIR) $(FDIR) $(IDIR) $(LDIR) $(MDIR) $(XDIR)
# only next 2 lines might need su
	cd $(SRCDIR); cd ..; rmdir $(SRCDIR)
	rm -f $(BINDIR)/xfractint $(MANDIR)/xfractint.1

fractint.hlp: hc $(DOSHELPDIR)/$(HELP)
	cd $(DOSHELPDIR); ../hc /c; mv fractint.hlp ..

fractint.doc: doc

doc: hc $(HELPFILES)
	cd $(DOSHELPDIR) ; ../hc /p ; mv -f fractint.doc ..

hc:	$(HOBJS)
	$(CC) -o hc $(CFLAGS) $(HOBJS)

unix.o: $(DOSHELPDIR)/unix.c
	$(CC) $(CFLAGS) $(OPT) -DSRCDIR=\"$(SHRDIR)\" -c $(DOSHELPDIR)/unix.c

copy: $(FILES)
	mv $(FILES) backup

# DO NOT DELETE THIS LINE -- make depend depends on it.

hc.o: $(DOSHELPDIR)/hc.c $(HFD)/helpcom.h $(HFD)/port.h

