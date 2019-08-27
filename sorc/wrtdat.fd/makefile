SHELL=          /bin/sh
LIBS=          
FC=             ifort
FFLAGS=         
LDFLAGS=       

SRCS=    wrtdat.f
OBJS=    wrtdat.o

CMD	= wrtdat.x

all:	$(CMD)

$(CMD):	$(OBJS)
	$(FC) $(FFLAGS)$(LDFLAGS) -g  -o $(@) $(OBJS) $(LIBS)
clean:
	-rm -rf *.o *.mod
install:
	-mv $(CMD) ../../exec
