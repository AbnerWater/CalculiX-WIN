
CFLAGS = -Wall -O3 -g -I ../../SPOOLES.2.2 -DARCH="Linux" -DSPOOLES -DARPACK -DMATRIXSTORAGE -DNETWORKOUT
FFLAGS = -Wall -O3 -g  

CC=gcc
FC=gfortran

.c.o :
	$(CC) $(CFLAGS) -c $<
.f.o :
	$(FC) $(FFLAGS) -c $<

include Makefile.inc

SCCXMAIN = ccx_2.15.c

OCCXF = $(SCCXF:.f=.o)
OCCXC = $(SCCXC:.c=.o)
OCCXMAIN = $(SCCXMAIN:.c=.o)

DIR=../../SPOOLES.2.2
LIBDIR=c:/msys64/mingw64/x86_64-w64-mingw32/lib

LIBS = \
       $(DIR)/spooles.a \
       ../../ARPACK/libarpack_MINGW64.a \
	   $(LIBDIR)/libpthread.a\
	   $(LIBDIR)/libm.a \

ccx_2.15.exe: $(OCCXMAIN) ccx_2.15.a  $(LIBS)
	./date.pl; $(CC) $(CFLAGS) -c ccx_2.15.c; $(FC) -fopenmp -Wall -O3 -o $@ $(OCCXMAIN) ccx_2.15.a $(LIBS)

ccx_2.15.a: $(OCCXF) $(OCCXC)
	ar vr $@ $?
                                                                               
