#!/bin/bash

# Normal IO input files, non-numbered files,
# (not numbered files which are of the form ##########.<type> where type is chm, wus, etc)
# are sorted according to the methods which use them (or SOURCE DIRectory where their reader
# file is found.

#
#   INPUT FILES

#     SOURCE DIR     Input File Directory
#     swat           data_swat
#     smrt (swatmf)  data_swatmf
#     modflow        data_mflow

#   OUTPUT FILES
#                    Output File Directory
#                    data_out

#   SHM INPUT FILES
#     *.<type>       data_<type>
#     *.chm          data_chm    e.g.
#     *.gw           data_gw     e.g.

# Files that should be in the directories
# data_out, data_swat, data_swatmf, and  data_mflow
# are listed in are listed in the arrays:

#                 Array                 types of files
#           data_input_swat_list     data_swat files
#           data_input_mflow_list    data_modflow files
#           data_input_swatmf_list   data_smrt files
 
#           data_output_swat_list    data_out files
#           data_output_mflow_list   data_out files
#           data_output_swatmf_list  data_out files

#  Even though output from each of the different sources
#  are in different data_output... lists, presently all
#  are assigned to data_out.

#  File_checker Checks location of input and output files,
#  reports if they are found.  If they are in the toplevel
#  directory they are moved to an appropriate subdirectory
#  (associated with the source files that use them).


#  If there are numbered files in the directory, they
#  can be sorted into appropriate directories, by responding
#  yes to the request to sort and move them.
#  SHM files (numbered files) with suffixes
#  chm gw hru lwq mgt pnd res rte sdr sep sol sub swq wgn wus.
#  They go (or are already) in  directories data_chm, data_gw, etc.)

# Parameters:
# TOP_DIR           Top Level where Data is Located - Default=PWD
# PRINT_NOT_FOUNDS  set to "no" to NOT show files which are not found - Default=yes
# FIND_DIR          Assigned directories - no need to change this
  
# CHOICES:
#  MOVE files to one of FIND_DIR="data_out data_swat data_swatmf data_mflow"
      MV_TO_DIR=yes
      SUBDIRS=yes
#  KEEP files in top leve directory (TOPDIR)
     #MV_TO_DIR=no
     #SUBDIRS=no
#

# OUTPUT
# Check to make sure files are in their correct place.
#   "@" == file found in correct location
#   "X" == file is not in  correct location
#   "-" == file not found (probably output not present before run,
#                          or file not used in this simulation)
#
#   If file found in TOP_DIR (PWD, or whatever is set below)
#   file is moved to standard location
#
#                         Oct. 21 2021  Kent milfeld@tacc.utexas.edu
# These files are not assigned:
#     interface_modflow_db.json, JB1_out.tar.gz, schema.ini
#     swat2012.exe, SWAT-MODFLOW.exe
#     runswat.bat
#     Tmp1.Tmp, Tmp2.Tmp and links tmp1.tmp, tmp2.tmp

types=( chm gw hru lwq mgt pnd res rte sdr sep sol sub swq wgn wus )


if [[ $1 == clean ]]; then
   for suffix in ${types[@]}; do
     rm -rf data_$suffix
   done
   rm -rf  data_swat
   rm -rf  data_swatmf
   rm -rf  data_mflow
   rm -rf  data_out
   exit 0
fi


read -p " => mv numbered files (#########.<type> to data_<type> directories? (yes|no) [no]: " yes_no
yes_no=${yes_no:-no}
[[ $yes_no == y ]] && yes_no=yes
echo " -- answ: $yes_no"
mv_numbered_files=$yes_no

echo " -> numbered files --start: `date`"

if [[ $mv_numbered_files == yes ]]; then
   echo " -->                            This may take some time (${types[@]} files x ~20s/file)"
   for suffix in ${types[@]}; do
     echo "                                        Working on $suffix"
     rm -rf data_$suffix
     mkdir  data_$suffix
     mv *[0-9].$suffix data_$suffix
     #cd     data_$suffix
     #tar xf $DIR/swat_data.tar "*[0-9].$suffix"
     #cd ..
   done
fi


for suffix in ${types[@]}; do
  non_NF=$(/bin/ls -1 data_$suffix | egrep -v  ".*[0-9].$suffix" | wc -l)
      NF=$(/bin/ls -1 data_$suffix | wc -l)
  printf " --> File count: #########.%-3s = %5d,  others = %5d\n" $suffix  $NF $non_FN
done

echo " -> numbered files --  end: `date`"


echo " ------------------------- End of numbered file sorting ------------------------"

echo " -> non-numbered files --start: `date`"

  echo  ""
  echo  " -> @ == mean file found in correct location"
  echo  " -> X == file is not in  correct location"
  echo  " -> - == file not found (probably output not present before run,"
  echo  "                     or file not used in this simulation)"

  [[ $SUBDIRS   == yes ]] && echo " -> Check if files need to be moved. (SUBDIRS=$SUBDIRS)."
  [[ $SUBDIRS   == no  ]] && echo " -> Don't check if files need to be moved. (SUBDIRS=$SUBDIRS)."
  [[ $MV_TO_DIR == yes ]] && echo " -> Moving files to appropriate subdirectory (MV_TO_DIR=$MV_TO_DIR)."
  [[ $MV_TO_DIR == no  ]] && echo " -> Files NOT moved to subdirectory. (MV_TO_DIR=$MV_TO_DIR)"


PRINT_NOT_FOUNDS=yes
TOP_DIR=`pwd`

if [[ $SUBDIRS == yes ]]; then
   FIND_DIR="data_out data_swat data_swatmf data_mflow"
   for dir in $FIND_DIR; do   #make sure directory is present
     mkdir -p $dir
   done
else
   FIND_DIR="."
fi


##v  Code:  Lists and processing   v##

data_input_swat_list=( ATMO.ATM basins.bsn basins.wwq cst.cst fert.dat fig.fig file.cio hru.dat input.std lup.dat pcp1.pcp pest.dat plant.dat rch.dat rsv.dat septwq.dat slr.slr sub.dat till.dat tmp1.tmp Tmp1.Tmp tmp2.tmp Tmp2.Tmp urban.dat wnd.wnd lupd.dat rhfile petfile )

data_output_swat_list=( swat output watout.dat chan.deg auto_irrig_hrus bmp-ri.out bmp-sedfil.out hyd.out output.pst output.rch output.rsv output.sed output.std septic.out output.snu output.snw output.hru outputb.hru outputb.sub outputb.rch output.wql hourq.out wbl.out output.vel output.dep output.mgt output.wtr output.pot output2.std output2.rch output2.hru output2.rsv air_soil.out output.sub cswat_profile.txt cswat_profile.txt cswat_daily.txt final_n_balance.txt final_yields.txt swat.qst )

data_input_mflow_list=( modflow.obs modflow.mfn wabash1.bas wabash1.dis wabash1.hds wabash1.list wabash1.nwt wabash1.oc wabash1.rch wabash1.riv wabash1.upw )
data_output_mflow_list=( Init_head.dat swatmf_out_MF_obs Netrech.txt )

data_input_swatmf_list=( swatmf_dhru2grid.txt swatmf_dhru2hru.txt swatmf_grid2dhru.txt swatmf_link.txt swatmf_river2grid.txt )
data_output_swatmf_list=( swatmf_out_MF_gwsw swatmf_out_MF_gwsw_monthly swatmf_out_MF_gwsw_yearly swatmf_out_MF_head_monthly swatmf_out_MF_head_yearly swatmf_out_MF_recharge swatmf_out_MF_recharge_monthly swatmf_out_MF_recharge_yearly swatmf_out_MF_riverstage swatmf_out_SWAT_channel swatmf_out_SWAT_gwsw swatmf_out_SWAT_gwsw_monthly swatmf_out_SWAT_gwsw_yearly swatmf_out_SWAT_recharge swatmf_out_SWAT_recharge_monthly swatmf_out_SWAT_recharge_yearly swatmf_log swatmf_out_RT_rivno3 swatmf_out_SWAT_rivno3 swatmf_out_RT_rechno3 swatmf_out_SWAT_rechno3 swatmf_out_RT_rivP swatmf_out_SWAT_rivP swatmf_out_RT_rechP swatmf_out_SWAT_rechP swatmf_out_RT_cno3_monthly swatmf_out_RT_cno3_yearly swatmf_out_RT_cp_monthly swatmf_out_RT_cp_yearly sub_km swatmf_out_pumped_sub swatmf_out_pumped_hru swatmf_out_pumping )

file_check() {
   local file_list=($@)
   
   for file in ${file_list[@]} ; do
      if [[ $SUBDIRS == yes ]]; then
        /bin/ls  $TOP_DIR/$file >& /dev/null
        if [[ $? == 0 ]]; then            #found in top level directory
            printf " %32s  X  found in  %s (Top Dir)\n" $file $TOP_DIR
            if [[ $MV_TO_DIR == yes ]]; then
               printf "                                      Moving to %s\n" $DIR_DATA
               mv $file $DIR_DATA
            fi
            continue
        fi
      fi
   
      pathname=$(find $FIND_DIRS -name $file )
#echo pathname $pathname
      if [[ x$pathname == x ]] ; then   #file not found
         [[ $PRINT_NOT_FOUNDS == yes ]] &&
         printf " %32s  -  not found\n" $file
      else
         dir=`dirname $pathname`
         [[ $SUBDIRS == yes ]] && COMPARE_DIR="./$DIR_DATA"
         [[ $SUBDIRS == no  ]] && COMPARE_DIR="."
#echo "dir=$dir"
#echo "against: $COMPARE_DIR"
#echo "SUBDIRS: $SUBDIRS"
#exit
         if [[ $dir == $COMPARE_DIR ]]; then
            printf " %32s  @\n" $file
         else
            printf " %32s  X  found in  %s\n" $file $dir
         fi
      fi
   done
}   


echo " SWAT ========================================================================="
DIR_DATA=data_swat; [[ $SUBDIRS == no ]] && DIR_DATA=.
echo "   INPUT  DIRECTORY: $DIR_DATA"
echo "   ---------------"
file_check "${data_input_swat_list[@]}"

echo "   ----------------------------------------------------------------------------"
DIR_DATA=data_out; [[ $SUBDIRS == no ]] && DIR_DATA=.
echo "   OUTPUT DIRECTORY: $DIR_DATA"
echo "   ---------------"
file_check "${data_output_swat_list[@]}"

echo "==============================================================================="
echo ""; echo ""



echo " MODFLOW======================================================================="
DIR_DATA=data_mflow; [[ $SUBDIRS == no ]] && DIR_DATA=.
echo "   INPUT  DIRECTORY: $DIR_DATA"
echo "   ---------------"
file_check "${data_input_mflow_list[@]}"

echo "-------------------------------------------------------------------------------"
DIR_DATA=data_out; [[ $SUBDIRS == no ]] && DIR_DATA=.
echo "   OUTPUT DIRECTORY: $DIR_DATA"
echo "   ---------------"
file_check "${data_output_mflow_list[@]}"



echo " SWATMF (SMRT) ================================================================"
echo ""; echo ""
DIR_DATA=data_swatmf; [[ $SUBDIRS == no ]] && DIR_DATA=.
echo "   INPUT  DIRECTORY: $DIR_DATA"
echo "   ---------------"
file_check "${data_input_swatmf_list[@]}"

echo "-------------------------------------------------------------------------------"
DIR_DATA=data_out; [[ $SUBDIRS == no ]] && DIR_DATA=.
echo "   OUTPUT DIRECTORY: $DIR_DATA"
echo "   ---------------"
file_check "${data_output_swatmf_list[@]}"

echo "==============================================================================="
echo " -> non-numbered files --  end: `date`"
echo " ----------End of non-numbered swat/smrt/multiflow input file sorting ----------"
