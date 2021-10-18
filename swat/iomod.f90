module io
  character(:),  PUBLIC,allocatable  ::  work_dir
  character(:),  PUBLIC,allocatable  ::  data_dir
  character(:),  PUBLIC,allocatable  ::  conf_dir
  character(:),  PUBLIC,allocatable  ::  out_dir

  character(:),  PUBLIC,allocatable  ::  data_out

  character(:),  PUBLIC,allocatable  ::  data_swat
  character(:),  PUBLIC,allocatable  ::  data_swatmf
  character(:),  PUBLIC,allocatable  ::  data_mflow

  character(:),  PUBLIC,allocatable  ::  data_chm
  character(:),  PUBLIC,allocatable  ::  data_gw
  character(:),  PUBLIC,allocatable  ::  data_hru
  character(:),  PUBLIC,allocatable  ::  data_lwq
  character(:),  PUBLIC,allocatable  ::  data_mgt
  character(:),  PUBLIC,allocatable  ::  data_pnd
  character(:),  PUBLIC,allocatable  ::  data_res
  character(:),  PUBLIC,allocatable  ::  data_rte
  character(:),  PUBLIC,allocatable  ::  data_sdr
  character(:),  PUBLIC,allocatable  ::  data_sep
  character(:),  PUBLIC,allocatable  ::  data_sol
  character(:),  PUBLIC,allocatable  ::  data_sub
  character(:),  PUBLIC,allocatable  ::  data_swq
  character(:),  PUBLIC,allocatable  ::  data_wgn
  character(:),  PUBLIC,allocatable  ::  data_wus
  character(120),PRIVATE :: dir
  integer,       PRIVATE :: length
contains
  subroutine get_io_dirs()

!SWAT_WORK DIR
   dir=repeat(" ",sizeof(dir))
   call get_environment_variable("SWAT_WORK",dir,LENGTH=length)
   call check_dir_length()
                         !Append / at end, use "." for local directory
   if (length == 0) then
      dir(1:2)="./"
      length=2
   else
      dir(length+1:length+1)= '/'
      length=length+1
   endif
   allocate(character(len=length) :: work_dir)   !work_dir has exact len of string
   work_dir(1:length)=dir(1:length)              !avoids using trim in open stmnt

   print*,"Set work_dir to ",work_dir

!SWAT_DATA DIR

   dir=repeat(" ",sizeof(dir))
   call get_environment_variable("SWAT_DATA",dir,LENGTH=length)
   call check_dir_length()
                         !Append / at end, use "." for local directory
   if (length == 0) then
      dir(1:2)="./"
      length=2
   else
      dir(length+1:length+1)= '/'
      length=length+1
   endif

   allocate(character(len=length) :: data_dir)  !data_dir has exact len of string
   data_dir(1:length)=dir(1:length)             !avoids using trim in open stmnt

   allocate(character(len=length+9) :: data_chm); data_chm(1:length+9)=dir(1:length)//"data_chm/"
   allocate(character(len=length+9) :: data_gw ); data_gw( 1:length+8)=dir(1:length)//"data_gw/"
   allocate(character(len=length+9) :: data_hru); data_hru(1:length+9)=dir(1:length)//"data_hru/"
   allocate(character(len=length+9) :: data_lwq); data_lwq(1:length+9)=dir(1:length)//"data_lwq/"
   allocate(character(len=length+9) :: data_mgt); data_mgt(1:length+9)=dir(1:length)//"data_mgt/"
   allocate(character(len=length+9) :: data_pnd); data_pnd(1:length+9)=dir(1:length)//"data_pnd/"
   allocate(character(len=length+9) :: data_res); data_res(1:length+9)=dir(1:length)//"data_res/"
   allocate(character(len=length+9) :: data_rte); data_rte(1:length+9)=dir(1:length)//"data_rte/"
   allocate(character(len=length+9) :: data_sdr); data_sdr(1:length+9)=dir(1:length)//"data_sdr/"
   allocate(character(len=length+9) :: data_sep); data_sep(1:length+9)=dir(1:length)//"data_sep/"
   allocate(character(len=length+9) :: data_sol); data_sol(1:length+9)=dir(1:length)//"data_sol/"
   allocate(character(len=length+9) :: data_sub); data_sub(1:length+9)=dir(1:length)//"data_sub/"
   allocate(character(len=length+9) :: data_swq); data_swq(1:length+9)=dir(1:length)//"data_swq/"
   allocate(character(len=length+9) :: data_wgn); data_wgn(1:length+9)=dir(1:length)//"data_wgn/"
   allocate(character(len=length+9) :: data_wus); data_wus(1:length+9)=dir(1:length)//"data_wus/"

   allocate(character(len=length+10) :: data_swat  ); data_swat(  1:length+10)=dir(1:length)//"data_swat/"
   allocate(character(len=length+12) :: data_swatmf); data_swatmf(1:length+12)=dir(1:length)//"data_swatmf/"
   allocate(character(len=length+11) :: data_mflow ); data_mflow( 1:length+11)=dir(1:length)//"data_mflow/"

   print*,"Set data directories in ",data_dir


!SWAT_OUTPUT DIR
   dir=repeat(" ",sizeof(dir))
   call get_environment_variable("SWAT_OUTPUT",dir,LENGTH=length)
   call check_dir_length()
                         !Append / at end, use "." for local directory
   if (length == 0) then
      dir(1:2)="./"
      length=2
   else
      dir(length+1:length+1)= '/'
      length=length+1
   endif

   allocate(character(len=length) :: out_dir)   !data_dir has exact len of string
   out_dir(1:length)=dir(1:length)              !avoids using trim in open stmnt

   allocate(character(len=length+9)  :: data_out)      !data_dir has exact len of string
   data_out(1:length+9)=dir(1:length)//"data_out/"     !avoids using trim in open stmnt

   print*,"Set data_out to  SWAT_OUTPUT +  data_out: ",data_out


!SWAT_CONFIGURE DIR

   dir=repeat(" ",sizeof(dir))
   call get_environment_variable("SWAT_CONF",dir,LENGTH=length)
   call check_dir_length()
                         !Append / at end, use "." for local directory
   if (length == 0) then
      dir(1:2)="./"
      length=2
   else
      dir(length+1:length+1)= '/'
      length=length+1
   endif
   allocate(character(len=length) :: conf_dir) !conf_dir has exact len of string
   conf_dir(1:length)=dir(1:length)            !avoids using trim in open stmnt
   print*,"Set conf_dir to ",conf_dir

  end subroutine

  subroutine check_dir_length()
   if (length+1  > len(dir)) then
      print*," ERROR: SWAT Directory name in SWAT_XXXX is too long"
      print*,"        dir code variable size is", len(dir)
      print*,"        Characters in directory name of SWAT_XXXX ENV Var ",length
      print*,"        TRUNCATED RETURNED NAME : ", dir
      print*,"           Shorted name (e.g. use a link) or"
      print*,"           Move data to shorter-named path, or"
      print*,"           increase dir variable size in module dirs"
      print*," ABORTING EXECUTION"
      stop
   endif
  end subroutine

end module
