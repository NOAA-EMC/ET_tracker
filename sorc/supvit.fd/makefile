SHELL=          /bin/sh
#LIBS=           -L/nwprod/lib -lw3nco_d
LIBS=           $(W3NCO_LIBd)
FC=             ifort
FFLAGS=        -i4 -r8
LDFLAGS=       -g -O0

SRCS=    supvit_modules.f supvit_main.f
OBJS=    supvit_modules.o supvit_main.o

CMD	= ../../exec/supvit

all:	$(CMD)

$(CMD):	$(OBJS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)
clean:
	-rm -f  *.o  *.mod
install:
	-mv supvit ../../exec/
