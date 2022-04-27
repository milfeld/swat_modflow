# Instructions for Building and Testing SWAT_SHM on Stampede2.
* The following script can be used to build and run swat on Stampede2, using shared memory for the 250K numbered files (of the form #########.<type> in the data set).
* The 250K files have been "pre-processed" into 15 shared-memory files with the names swat_data_<type>.  There are also 15 swat_meta_<type> files.  (These are in the dev_shm.tar on Stampede -- they are too big for gitHub.)
* These shm files can be created from the original dataset by making the write_shm executable (execute make in the mk_shm_files directory).  Set SWAT_DATA to the path for the data directory.  In the data directory have a subdirectory for each type of data, named "data_<type>" with the #########.<type> files (e.g. data_sol will have all the #########.sol files, etc.).  Then execute write_shm. (The preprocessed files are used below.)
* In the test below, a Fortran stop statement halts execution after reading the initial numbered data sets.  Comment out the stop (in main.f) for performing the simulation.
  ** Kent Milfeld  4/27/2022

```
#!/bin/sh

#            Reset to the default Intel compiler.
ml intel

#            Make working directories for SWAT BUILD & DATA
cd $SCRATCH
mkdir TEST_SWAT TEST_SWAT_DATA

#            Go to BUILD dir.   Clone swat_modflow.
cd TEST_SWAT
git clone https://github.com/milfeld/swat_modflow

#            Get the SHM files, and put them in /dev/shm
#            These are the 250K numbered files (with names of the form
#            #########.<type>.) wrapped up into shared memory stream files.
tar -xvf /scratch/00770/milfeld/SWAT_DATA/dev_shm.tar
cp -p DEV_SHM/* /dev/shm


#            Go into the build directory.
#            IMPORTANT, checkout the shm branch
cd swat_modflow
git checkout shm

# THIS NEXT COMMAND: run twice, first time will show error-- that is OK
#            1st run tries to compile all module files, but
#            only non-dependent modules will be compiled.
#     ****** Don't worry about the ERRORS .
#            The 2nd run build_modules_first.sh will compile
#            everything (modules with/without dependencies).


build_modules_first.sh  #Compiles modules without dependencies
build_modules_first.sh  #Compiles dependent files that were not
#                       #compiled in the first build_modules_first.sh run.

#            Compile code with 10 processors.
make -j 10

#            This set the environment variable SWAT_HOME
#            to the present directory (and TOP_DIR to basename).

source sourceme_in_swat_home


#            Go to the DATA directory
#            Copy the "normal" input files here.
#            They are in data_swat data_swatmf, and data_mflow.
#            Also makes data_out.

cd $SCRATCH/TEST_SWAT_DATA
tar -xvf /scratch/00770/milfeld/SWAT_DATA/data_dir.tar

#            This sets environment variables SWAT_DATA and SWAT_OUTPUT
#            to the present directory.
#            (Make sure you are in the data directory)
source $SWAT_HOME/sourceme_in_input_dir

SWAT_MODFLOW_rel64
```
