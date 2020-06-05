
CFLAGS = -Wall -O3 -fopenmp -I ../../../SPOOLES.2.2 -DARCH="Linux" -DSPOOLES -DARPACK -DMATRIXSTORAGE -DUSE_MT=1 
FFLAGS = -Wall -O3 -fopenmp 

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

DIR=../../../SPOOLES.2.2
LIBDIR=c:/msys64/mingw64/x86_64-w64-mingw32/lib

LIBS = \
       $(DIR)/MT/src/spoolesMT.a \
       $(DIR)/spooles.a \
       ../../../ARPACK/libarpack_MINGW64.a \
       $(LIBDIR)/libpthread.dll.a\
	   $(LIBDIR)/libm.a \

ccx_2.15_MT.exe: $(OCCXMAIN) ccx_2.15_MT.a  $(LIBS)
	./date.pl; $(CC) $(CFLAGS) -c ccx_2.15.c; $(FC) -fopenmp -Wall -O3 -o $@ $(OCCXMAIN) ccx_2.15_MT.a $(LIBS)

ccx_2.15_MT.a: $(OCCXF) $(OCCXC)
	ar vr $@ $?
                                                                               
