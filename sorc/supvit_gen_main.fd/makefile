SHELL=          /bin/sh
#LIBS=           -L/nwprod/lib -lw3nco_d
LIBS     = $(W3NCO_LIBd)
##LIBS=          
FC=             ifort
FFLAGS=        -i4 -r8 
LDFLAGS=       

SRCS=    supvit_gen.modules.f supvit_gen.main.f
OBJS=    supvit_gen.modules.o supvit_gen.main.o

CMD       = supvit_gen

all:	$(CMD)

$(CMD):	$(OBJS)
	$(FC) $(FFLAGS)$(LDFLAGS) -g  -o $(@) $(OBJS) $(LIBS)
clean:
	-rm -rf *.o *.mod
install:
	-mv $(CMD) ../../exec
