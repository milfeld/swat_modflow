PROJ_NAME=SWAT_MODFLOW_rel64

 CC=mpicc
 FC=mpif90

#CC=gcc
#FC=gfortran

 IO = -DSHM_IO  -fpp  #comment this line for standard (POSIX) IO

 NO_DIAG="-diag-disable 8291,10006"

   FFLAG=-c -standard-semantics -fmessage-length=0 -ffixed-line-length-80  -funderscoring -fbacktrace -ffpe-trap=invalid,zero,overflow
   RFLAG=-O3
 LONGFIX=-ffixed-line-length-132
 LONGFREE=-ffree-line-length-200
   ARCH64=-m64

#MOD_SRC=$(shell find . -name '*mod.f*' -exec basename {} \; ) # Find all module (...mod.f*) files
#MOD_TMP=$(patsubst %.f,   %.o, $(MOD_SRC))                    # .f   -> .o
#MOD_OBJ=$(patsubst %.f90, %.o, $(MOD_TMP))                    # .f90 -> .o

#SRC=$(shell find . -name '*.f*' -exec basename {} \; | grep -v 'mod.f' ) # All *.f* files (non *mod.f*)
 SRC=$(shell find . -name '*.f*' -exec basename {} \; ) # All *.f* files (non *mod.f*)
 TMP=$(patsubst %.f,   %.o, $(SRC))                                       # .f   -> .o
 OBJ=$(patsubst %.f90, %.o, $(TMP))                                       # .f90 -> .o

 C_SRC=$(shell find . -name '*.c' -exec basename {} \; ) # All C files (.c)
 C_OBJ=$(patsubst %.c, %.o, $(C_SRC))

 VPATH = swat:smrt:modflow:rt3d

$(PROJ_NAME): $(C_OBJ) $(MOD_OBJ) $(OBJ)
	@echo LINKING
	${FC} ${NO_DIAG} ${IO} ${ARCH64}               -fopenmp -mkl -fpp $^ -o $@ 

%.o: %.f90
	@echo Compiling and generating object $@ ...
	${FC} -c ${NO_DIAG} ${IO} ${ARCH64} ${FFLAG} ${RFLAG}             $^
%.o: %.f
	@echo Compiling and generating object $@ ...
	${FC} -c ${NO_DIAG} ${IO} ${ARCH64} ${FFLAG} ${RFLAG} ${LONGFREE} $^

%main.o: %main.f
	@echo Compiling and generating object $@ ...
	${FC} -c ${NO_DIAG} ${IO} ${ARCH64} ${FFLAG} ${RFLAG} ${LONGFIX}  $^

%.o: %.c
	@echo compiling $@ ...
	${CC} -c  $^

clean:
	@echo Removing secondary things
	@rm -f *.o $(PROJ_NAME)
	@echo Done!

clean_all:
	@echo Removing secondary things
	@rm -f       ./*.o       ./*.mod  a.out
	@rm -f    swat/*.o    swat/*.mod
	@rm -f    smrt/*.o    smrt/*.mod
	@rm -f    rt3d/*.o    rt3d/*.mod
	@rm -f modflow/*.o modflow/*.mod
