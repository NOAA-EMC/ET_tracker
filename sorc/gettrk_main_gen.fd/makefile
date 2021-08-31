SHELL=  /bin/sh
#------------------------------------------------------------------------
# Makefile for the gettrk_main.gen.f program.
# EMC, Guang Ping Lou 09/19/2018
#-------------------------------------------------------------------------

ISIZE = 4
RSIZE = 8
INC      = $(G2_INCd)
LIBS     = $(W3EMC_LIBd) \
           $(W3NCO_LIBd) \
           $(G2_LIBd) \
           $(BACIO_LIB4) \
           $(JASPER_LIB) \
           $(PNG_LIB)    \
           $(Z_LIB)

FCOMP=  ifort -nofree
FFLAGS= -O0 -g -fpe0 -I $(INC) -i$(ISIZE) -r$(RSIZE) -traceback

gettrk_gen: gettrk_main.gen.f gettrk_modules.gen.f
	$(FCOMP) $(FFLAGS) gettrk_modules.gen.f gettrk_main.gen.f -o gettrk_gen $(LIBS) 

##gettrk_modules.gen.o: gettrk_modules.gen.f
##	$(FCOMP) $(FFLAGS) gettrk_modules.gen.f -o gettrk_modules.gen.o

clean:
	-rm -f  *.o  *.mod

install:
	-mv gettrk_gen ../../exec
