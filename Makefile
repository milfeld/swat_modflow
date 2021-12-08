PROJ_NAME=SWAT_MODFLOW_rel64
CC=mpicc
#FC=gfortran
FC=mpif90
FOR_FILES=$(shell find . -name "*.f*")
MOD_FILES=$(shell find . -name "*.mod*")
S_OBJ=$(patsubst %.f*, %.o, $(FOR_FILES))

DISABLE="-diag-disable 8291,10006"

FFLAG=-c -standard-semantics -fmessage-length=0 -ffixed-line-length-80  -funderscoring -fbacktrace -ffpe-trap=invalid,zero,overflow
RFLAG=-O3
LONGFIX=-ffixed-line-length-132
LONGFREE=-ffree-line-length-200
ARCH32=-m32
ARCH64=-m64
#CXXFLAGS=-c -g -Wall 
CXXFLAGS= -c
SUBDIRS := $(wildcard */.)

subobjall: $(SUBDIRS)
$(SUBDIRS):
	$(MAKE) -C $@

#.PHONY: subobjall $(SUBDIRS)

#${FC} ${ARCH64} -fopenmp -I${MKLROOT}/include $^ -static -Wl,-L${MKLROOT}/lib/intel64 -lmkl_intel_lp64 -lmkl_core -lmkl_gnu_thread -L/usr/lib64 -lpthread -lm -ldl  -o $@ 
#all: $(subobjall) $(PROJ_NAME)
all: $(subobjall) $(S_OBJ) $(PROJ_NAME)
	@echo Compiling Application $(PROJ_NAME)
	@echo
	#@./$(PROJ_NAME)

$(PROJ_NAME): $(S_OBJ)
	@echo Linking objects...
	${FC} ${DISABLE} ${ARCH64} -fopenmp -mkl -fpp $^ -o $@ 

%.o: %.f*
	@echo Compiling and generating object $@ ...
	${FC} ${DISABLE} ${ARCH64} ${FFLAG} ${RFLAG} ${LONGFREE} ${LONGFREE} $< $(CXXFLAGS) -o $@

main.o: main.f
	@echo Compiling and generating object $@ ...
	${FC} ${DISABLE} ${ARCH64} ${FFLAG} ${RFLAG} ${LONGFIX} main.f -o main.o 

clean:
	@echo Removing secondary things
	@rm -f *.o $(PROJ_NAME)
	@echo Done!
