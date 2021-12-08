#!/bin/bash
# Generate the module files
list='
swat/io_mod.f90
swat/shm_mod.f90
modflow/mf_gwf2lpf7.f
modflow/mf_gwf2mnw17_NWT.f
modflow/mf_gwf2mnw27_NWT.f
modflow/mf_gwf2mnw2i7.f
modflow/mf_gwf2rch7.f
modflow/mf_gwf2res7.f
modflow/mf_gwf2riv7_NWT.f
modflow/mf_gwf2sfr7_NWT.f
modflow/mf_gwf2str7.f
modflow/mf_gwf2sub7.f
modflow/mf_gwf2swr7_NWT.f
modflow/mf_gwf2swt7.f
modflow/mf_gwf2upw1.f
modflow/mf_gwf2wel7_NWT.f
modflow/mf_gwfsfrmodule_NWT.f
modflow/mf_gwfsfrmodule_NWT.f 
modflow/mf_gwfuzfmodule_NWT.f 
modflow/mf_lmt7_NWT.f
modflow/mf_mach_mod.f90
modflow/mf_mach_mod.f90 
modflow/mf_modules.f90
modflow/mf_modules.f90 
modflow/mf_nogmg.f
modflow/mf_NWT1_gmres.f90
modflow/mf_NWT1_ilupc_mod.f90
modflow/mf_NWT1_ilupc_mod.f90 
modflow/mf_NWT1_module.f
modflow/mf_NWT1_module.f 
modflow/mf_NWT1_xmd.f
modflow/mf_NWT1_xmdlib.f
modflow/mf_obs2bas7.f
modflow/mf_obs2chd7.f
modflow/mf_obs2drn7.f
modflow/mf_obs2ghb7.f
modflow/mf_obs2riv7.f
modflow/mf_obs2str7.f
modflow/mf_pcg7_NWT.f
modflow/mf_rt_link.f
modflow/mf_sip7_NWT.f
rt3d/rt_modparm.f 
smrt/smrt_parm.f
swat/modparm.f 
modflow/mf_de47_NWT.f
modflow/mf_gwf2bas7_NWT.f
modflow/mf_gwf2bcf7.f
modflow/mf_gwf2chd7.f
modflow/mf_gwf2drn7_NWT.f
modflow/mf_gwf2drt7.f
modflow/mf_gwf2ets7.f
modflow/mf_gwf2evt7.f
modflow/mf_gwf2fhb7.f
modflow/mf_gwf2gag7.f
modflow/mf_gwf2ghb7_NWT.f
modflow/mf_gwf2hfb7_NWT.f
modflow/mf_gwf2huf7.f
modflow/mf_gwf2hydmod7.f
modflow/mf_gwf2ibs7.f
modflow/mf_gwf2lak7_NWT.f
'
for i in $list; do
echo "							Working on $i"
 ifort -c -diag-disable 8291  $i
done
