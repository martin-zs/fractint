SHELL=/bin/sh

ifeq ($(AS),/usr/bin/nasm)

OBJS = \
calcmand.o calmanfp.o faccess.o fpu087.o general.o sdl_scr.o \
sdl_snd.o video.o \
bignumax-64.o calmanfx-64.o fpu387ax-64.o generalax-64.o parserax-64.o

else

OBJS = \
calcmand.o calmanfp.o faccess.o fpu087.o general.o sdl_scr.o \
sdl_snd.o video.o

endif

#Need to prevent lex from doing fractint.l -> fractint.c
.SUFFIXES:
.SUFFIXES: .o .c .s .h .asm

all: $(OBJS)

tidy:
	rm -f $(OBJS)

clean:
	rm -f $(OBJS)

ifeq ($(AS),/usr/bin/nasm)
bignumax-64.o: bignumax-64.asm
	$(AS) $(AFLAGS) bignumax-64.asm
calmanfx-64.o: calmanfx-64.asm
	$(AS) $(AFLAGS) calmanfx-64.asm
fpu387ax-64.o: fpu387ax-64.asm
	$(AS) $(AFLAGS) fpu387ax-64.asm
generalax-64.o: generalax-64.asm
	$(AS) $(AFLAGS) generalax-64.asm
parserax-64.o: parserax-64.asm
	$(AS) $(AFLAGS) parserax-64.asm
endif

copy: $(FILES)
	mv $(FILES) backup

# DO NOT DELETE THIS LINE -- make depend depends on it.

calcmand.o: calcmand.c ${HFD}/port.h

calmanfp.o: calmanfp.c ${HFD}/port.h ${HFD}/fractype.h

faccess.o: faccess.c ${HFD}/port.h

fpu087.o: fpu087.c ${HFD}/port.h

general.o: general.c ${HFD}/port.h

sdl_scr.o: sdl_scr.c ${HFD}/port.h ${HFD}/prototyp.h ${HFD}/helpdefs.h

sdl_snd.o: sdl_snd.c ${HFD}/port.h ${HFD}/prototyp.h

video.o: video.c ${HFD}/port.h ${HFD}/prototyp.h

ifeq ($(AS),/usr/bin/nasm)
bignumax-64.o: bignumax-64.asm xfract_a-64.inc

calmanfx-64.o: calmanfx-64.asm xfract_a-64.inc

fpu387ax-64.o: fpu387ax-64.asm xfract_a-64.inc

generalax-64.o: generalax-64.asm xfract_a-64.inc

parserax-64.o: parserax-64.asm xfract_a-64.inc
endif

