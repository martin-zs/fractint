SHELL=/bin/sh

ifeq ($(AS),/usr/bin/nasm)

OBJS = \
calcmand.o calmanfp.o diskvidu.o fpu087.o fracsuba.o general.o \
tplus_a.o unix.o unixscr.o video.o \
calmanfx.o

else

OBJS = \
calcmand.o calmanfp.o diskvidu.o fpu087.o fracsuba.o general.o \
tplus_a.o unix.o unixscr.o video.o

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
calmanfx.o: calmanfx.asm
	$(AS) $(AFLAGS) calmanfx.asm
endif

unix.o: unix.c
	$(CC) $(CFLAGS) -DSRCDIR=\"$(SRCDIR)\" -c unix.c

copy: $(FILES)
	mv $(FILES) backup

# DO NOT DELETE THIS LINE -- make depend depends on it.

calcmand.o: calcmand.c ${HFD}/port.h

calmanfp.o: calmanfp.c ${HFD}/port.h ${HFD}/fractype.h

ifeq ($(AS),/usr/bin/nasm)
calmanfx.o: calmanfx.asm xfract_a.inc
endif

diskvidu.o: diskvidu.c ${HFD}/port.h

fpu087.o: fpu087.c ${HFD}/port.h

fracsuba.o: fracsuba.c ${HFD}/port.h

general.o: general.c ${HFD}/port.h

tplus_a.o: tplus_a.c

unix.o: unix.c ${HFD}/port.h

unixscr.o: unixscr.c ${HFD}/port.h ${HFD}/prototyp.h ${HFD}/helpdefs.h

video.o: video.c ${HFD}/port.h ${HFD}/prototyp.h
