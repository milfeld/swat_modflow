module verbosity
use ISO_C_BINDING, only: C_INT
integer(C_INT), bind(C)    ::  c_verbose=0
integer(C_INT)             ::  f_verbose=0
  interface
    subroutine set_c_verbosity(f_verbose) bind(C)
      import                             :: C_INT, c_verbose
      integer(C_INT), value,  intent(in) :: f_verbose
    end subroutine
  end interface
contains
  subroutine set_verbosity()
     character(len=1) :: value
   !!call get_environment_variable("VERBOSE",value)
     call get_environment_variable("SWAT_VERBOSE",value)
     if(57>=ichar(value) .and. ichar(value)>=48) read(value,'(i1)') f_verbose
  end subroutine

  subroutine get_swat_data_dir(directory,len_dir)
     character(len=*) :: directory
     integer          :: len_dir
     integer          :: diff
     character(len=6) :: more_chars

     call get_environment_variable("SWAT_DATA_DIR",directory,length=len_dir)

     diff=len_dir - len(directory)
     if(len_dir == 0) stop " ERROR: SWAT_DATA_DIR must be set."
     if(diff>0) then
       write(*,'(a,i4,a)') " ERROR: SWAT_DATA_DIR name is too long. Storage for full path name is", &
                                    len(directory)," characters."
       write(*,'(a,i4,a)') "  Increase size of swat_data_dir character string by",diff," characters."
       write(*,'(a     )') "  OR Shorten the directory name (somehow). See SWAT_DATA_DIR ENV value."
       stop
     endif
  end subroutine

end module

module shm
use, intrinsic :: iso_c_binding, only : C_PTR, C_F_POINTER

integer,parameter :: KI8 = selected_int_kind(15)

integer,parameter :: MAX_TYPE=15,    MAX_SBN=1500, MAX_HRU=64
integer,parameter :: MAX_FILE=35000, MAX_LINE=500
integer,parameter :: MAX_SBNxMAX_HRU=MAX_SBN*MAX_HRU

integer,parameter :: MAX_DATA_CHARS_in_FILE =6400

integer      :: file_ct(MAX_TYPE), line_cnt, type_no

!integer(KI8) :: offset(MAX_FILE+1), offndx(MAX_TYPE*MAX_SBN*MAX_HRU+1), start(MAX_LINE+1), end(MAX_LINE)
integer(KI8) :: offset(MAX_SBN*MAX_HRU+1,MAX_TYPE)  ! See writer.c
integer(KI8) :: offndx(MAX_SBN*MAX_HRU  ,MAX_TYPE)  ! See writer.c

integer(KI8) :: start(MAX_LINE+1)
integer(KI8) ::   end(MAX_LINE)   !?????????

integer(KI8) :: startCHM(MAX_LINE+1), startGW(MAX_LINE+1),startHRU(MAX_LINE+1),startLWQ(MAX_LINE+1),startMGT(MAX_LINE+1)
integer(KI8) :: startPND(MAX_LINE+1),startRES(MAX_LINE+1),startRTE(MAX_LINE+1),startSDR(MAX_LINE+1),startSEP(MAX_LINE+1)
integer(KI8) :: startSOL(MAX_LINE+1),startSUB(MAX_LINE+1),startSWQ(MAX_LINE+1),startWGN(MAX_LINE+1),startWUS(MAX_LINE+1)

integer(KI8) ::   endCHM(MAX_LINE+1),   endGW(MAX_LINE+1),  endHRU(MAX_LINE+1),  endLWQ(MAX_LINE+1),  endMGT(MAX_LINE+1)
integer(KI8) ::   endPND(MAX_LINE+1),  endRES(MAX_LINE+1),  endRTE(MAX_LINE+1),  endSDR(MAX_LINE+1),  endSEP(MAX_LINE+1)
integer(KI8) ::   endSOL(MAX_LINE+1),  endSUB(MAX_LINE+1),  endSWQ(MAX_LINE+1),  endWGN(MAX_LINE+1),  endWUS(MAX_LINE+1)
                                                                   ! +1 because FS added a new line at end of each file.

!!integer      :: shm_data_fd
!!type(C_PTR)  :: shm_data_ptr
integer      ::  shm_data_fd(MAX_TYPE)
type(C_PTR)  :: shm_data_ptr(MAX_TYPE)

type(c_ptr)  :: cptr
type(C_PTR)  :: cptrCHM,  cptrGW,   cptrHRU,  cptrLWQ,  cptrMGT,  cptrPND,  cptrRES,  cptrRTE                
type(C_PTR)  :: cptrSDR,  cptrSEP,  cptrSOL,  cptrSUB,  cptrSWQ,  cptrWGN,  cptrWUS 

character(len=MAX_DATA_CHARS_in_FILE),pointer  :: data
character(len=MAX_DATA_CHARS_in_FILE),pointer  ::          dataDPD, dataWPD, dataRIB, dataSFB, dataLID  !in readpnd
integer(KI8)                                   ::             kDPD,    kWPD,    kRIB,    kSFB,    kLID  !in readpnd

character(len=MAX_DATA_CHARS_in_FILE),pointer  :: dataCHM, dataGW,  dataHRU, dataLWQ, dataMGT, dataPND, dataRES, dataRTE 
character(len=MAX_DATA_CHARS_in_FILE),pointer  :: dataSDR, dataSEP, dataSOL, dataSUB, dataSWQ, dataWGN, dataWUS
integer(KI8)                                   ::    kCHM,    kGW,     kHRU,    kLWQ,    kMGT,    kPND,    kRES,    kRTE
integer(KI8)                                   ::    kSDR,    kSEP,    kSOL,    kSUB,    kSWQ,    kWGN,    kWUS 

end module

module c_shm_api
use, intrinsic :: iso_c_binding, only : C_INT, C_LONG, C_PTR, C_CHAR, C_F_POINTER, C_NULL_CHAR

  interface
    subroutine get_shm_meta(file_ct, offset, offndx, type) bind(C)
      import               :: C_INT, C_LONG
      integer(C_INT)       :: file_ct(*)  !not (:)
      integer(C_LONG)      :: offset(*)   !not (:)
      integer(C_LONG)      :: offndx(*)   !not (:)
      integer(C_INT),value :: type
    end subroutine
  end interface

  interface
    function get_shm_data_ptr(shm_data_fd) result(shm_data_ptr) bind(C)
      import               :: C_INT, C_PTR
      integer(C_INT)       :: shm_data_fd(*)  !not (:)
         type(C_PTR)       :: shm_data_ptr
    end function
  end interface

  interface
    function get_shm_data_type_ptr(shm_data_fd,itype) result(shm_data_ptr) bind(C)
      import               :: C_INT, C_PTR
      integer(C_INT)       :: shm_data_fd(*)  !not (:)
      integer(C_INT),value :: itype
         type(C_PTR)       :: shm_data_ptr
    end function
  end interface

! call get_file_data(shm_ptr, offset, offndx, file_name, data, start, line_cnt)
  interface
   !subroutine get_file_data(shm_ptr, offset, offndx, file_name, data, start, line_cnt, type_no) bind(C)
   !function get_file_data(shm_ptr, offset, offndx, file_name, start, line_cnt, type_no) result(data) bind(C)
    function get_file_data(shm_ptr, offset, offndx, file_name, start, end, line_cnt, type_no) result(data) bind(C)
      import                              :: C_INT, C_LONG, C_PTR, C_CHAR
         type(C_PTR ), value, intent(in)  :: shm_ptr
      integer(C_LONG),        intent(in)  :: offset(*) !not (:)
      integer(C_LONG),        intent(in)  :: offndx(*) !not (:)

      character(kind=C_CHAR), intent(in)   :: file_name(*)

      integer(C_LONG),        intent(out)  :: start(*) !not (:)
      integer(C_LONG),        intent(out)  ::   end(*) !not (:)
      integer(C_INT ),        intent(out)  :: line_cnt, type_no
 
         type(C_PTR )                      :: data
    end function
   !end subroutine
  end interface

  interface
    subroutine print_data(shm_data_ptr) bind(C)
      import                       :: C_PTR
      type(C_PTR),value,intent(in) :: shm_data_ptr
    end subroutine
  end interface

  interface
    subroutine shm_open(file)
      import            :: C_F_POINTER, C_NULL_CHAR
      character(len=*)  :: file
      character(len=14) :: file_name
      integer           :: sufx_len, pt_ndx
    end subroutine
  end interface


end module c_shm_api     !http://fortranwiki.org/fortran/show/iso_c_binding

   subroutine shm_init()
   
     use verbosity
     use shm
     use c_shm_api
     use, intrinsic :: iso_c_binding, only : C_F_POINTER

integer :: isbn, sh_no, Index

     integer :: type
   
     call set_verbosity()
     call set_c_verbosity(f_verbose)
   
     do type=1,MAX_TYPE
     ! print*,'here 0 before get_shm_meta       typeC= ', type-1
       call get_shm_meta(file_ct(type),offset(1,type),offndx(1,type),type-1) !Get meta data in shared mem
!print*,"XHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH0"
!if(type == 12) then
!print*,"XHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH1"
!      do isbn=1,1277
!       !sh_no=MAX_HRU*isbn+1 !??????????? 1 based
!        sh_no=64*isbn+1 !??????????? 1 based
!        Index= Offndx(sh_no,type)+1
!        write(*,'( "XitpeF= 12, isbn= ",i8,"  sh_no= ",i8,"   OFFNDX= ",i8,"      Offset=",i8 )') &
!                                isbn,         sh_no,          Offndx(sh_no,type), Offset(Index,type)
!     enddo
!endif 
!print*,"XHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH2"

     ! print*,'here 1 before get_shm_data_type  typeC=', type-1
       shm_data_ptr(type)=get_shm_data_type_ptr(shm_data_fd(type),type-1)    !Open Access to shared mem


     ! print*,'here 2 before  c_f_pointer       typeC=', type-1
       call c_f_pointer(shm_data_ptr(type), data)                          !translate C ptr to F90 ptr

     end do
   
   end subroutine shm_init

   subroutine shm_open(file)
   
   ! get_file_data
   !               Sets cptr to location of file_name's content in shared memory.
   !               Sets type
   ! case
   !                Sets data<type> to cptr
   !                Sets    k<type> to start location

      use shm
      use c_shm_api, ONLY: get_file_data
      use, intrinsic :: iso_c_binding, only : C_F_POINTER,  C_NULL_CHAR

      implicit none

      character(len=*)  :: file
      character(len=14) :: file_name
      integer           :: sufx_len, pt_ndx
      integer           :: i, typeF
 
integer ::  ish_no, Jndex, isbn

 logical:: prCHM=.false., prGW=.false., prHRU=.false., prLWQ=.false., prMGT=.false., prPND=.false., prRES=.false., prRTE=.false.
 logical:: prSDR=.false., prSEP=.false.,prSOL=.false., prSUB=.false., prSWQ=.false., prWGN=.false., prWUS=.false.
!logical:: prCHM=.true., prGW=.true., prHRU=.true., prLWQ=.true., prMGT=.true., prPND=.true., prRES=.true., prRTE=.true.
!logical:: prSDR=.true., prSEP=.true.,prSOL=.true., prSUB=.true., prSWQ=.true., prWGN=.true., prWUS=.true.

   !             Dot gives location of file name.
   !             9 characters before=<sub_no><hru_no>
   !             2 or 3 characters after is suffix name
      pt_ndx=index(file,".")

      sufx_len=3   !! exception, 2-character suffix (gw)
      if( index(file(pt_ndx+1:pt_ndx+2),"gw") /= 0 ) sufx_len=2

      file_name = file(pt_ndx-9:pt_ndx+sufx_len)  !extract file_name

      file_name = trim(file_name)//C_NULL_CHAR    !put \0 for C call

      if( file(pt_ndx+1:pt_ndx+sufx_len) == "sno" .or.  file(pt_ndx+1:pt_ndx+sufx_len) == "ops" ) then
         write(*,'(" -> ERROR: SWAT File Type (",a,") have not been implemented yet.")') file_name(pt_ndx+1:pt_ndx+sufx_len)
         stop
      endif
   !  print*," OOOOOOa file --> ", file," <-------"             !rmKFM
   !  print*," OOOOOOb file= ", file(pt_ndx+1:pt_ndx+sufx_len)  !rmKFM

      select case( file(pt_ndx+1:pt_ndx+sufx_len) )
      case ("chm"); 
                 typeF=1
                 cptrCHM = get_file_data(shm_data_ptr( 1), offset(1,typeF), offndx(1,typeF), file_name,startCHM,   endCHM, line_cnt, type_no)
                 call c_f_pointer(cptrCHM, dataCHM); kCHM=0
                 if(prCHM) then;  do i=1,line_cnt; write(*,'(" >>",i4," ", a)') i,dataCHM(startCHM(i):endCHM(i)); end do; endif

      case ( "gw"); 
                 typeF=2
                 cptrGW  = get_file_data(shm_data_ptr( 2), offset(1,typeF), offndx(1,typeF), file_name, startGW,    endGW, line_cnt, type_no)
                 call c_f_pointer(cptrGW,  dataGW);  kGW=0
                 if(prGW ) then;  do i=1,line_cnt; write(*,'(" >>",i4," ", a)') i, dataGW( startGW(i): endGW(i)); end do; endif

      case ("hru"); 
                 typeF=3
                 cptrHRU = get_file_data(shm_data_ptr( 3), offset(1,typeF), offndx(1,typeF), file_name,startHRU,   endHRU, line_cnt, type_no)
                 call c_f_pointer(cptrHRU, dataHRU); kHRU=0
                 if(prHRU) then;  do i=1,line_cnt; write(*,'(" >>",i4," ", a)') i,dataHRU(startHRU(i):endHRU(i)); end do; endif

      case ("lwq"); 
                 typeF=4
                 cptrLWQ = get_file_data(shm_data_ptr( 4), offset(1,typeF), offndx(1,typeF), file_name,startLWQ,   endLWQ, line_cnt, type_no)
                 call c_f_pointer(cptrLWQ, dataLWQ); kLWQ=0
                 if(prLWQ) then;  do i=1,line_cnt; write(*,'(" >>",i4," ", a)') i,dataLWQ(startLWQ(i):endLWQ(i)); end do; endif

      case ("mgt"); 
                 typeF=5
                 cptrMGT = get_file_data(shm_data_ptr( 5), offset(1,typeF), offndx(1,typeF), file_name,startMGT,   endMGT, line_cnt, type_no)
                 call c_f_pointer(cptrMGT, dataMGT); kMGT=0
                 if(prMGT) then;  do i=1,line_cnt; write(*,'(" >>",i4," ", a)') i,dataMGT(startMGT(i):endMGT(i)); end do; endif

      case ("pnd"); 
                 typeF=6
                 cptrPND = get_file_data(shm_data_ptr( 6), offset(1,typeF), offndx(1,typeF), file_name,startPND,   endPND, line_cnt, type_no)
                 call c_f_pointer(cptrPND, dataPND); kPND=0
                 if(prPND) then;  do i=1,line_cnt; write(*,'(" >>",i4," ", a)') i,dataPND(startPND(i):endPND(i)); end do; endif

      case ("res"); 
                 typeF=7
                 cptrRES = get_file_data(shm_data_ptr( 7), offset(1,typeF), offndx(1,typeF), file_name,startRES,   endRES, line_cnt, type_no)
                 call c_f_pointer(cptrRES, dataRES); kRES=0
                 if(prRES) then;  do i=1,line_cnt; write(*,'(" >>",i4," ", a)') i,dataRES(startRES(i):endRES(i)); end do; endif

      case ("rte"); 
                 typeF=8
                 cptrRTE = get_file_data(shm_data_ptr( 8), offset(1,typeF), offndx(1,typeF), file_name,startRTE,   endRTE, line_cnt, type_no)
                 call c_f_pointer(cptrRTE, dataRTE); kRTE=0
                 if(prRTE) then;  do i=1,line_cnt; write(*,'(" >>",i4," ", a)') i,dataRTE(startRTE(i):endRTE(i)); end do; endif

      case ("sdr"); 
                 typeF=9
                 cptrSDR = get_file_data(shm_data_ptr( 9), offset(1,typeF), offndx(1,typeF), file_name,startSDR,   endSDR, line_cnt, type_no)
                 call c_f_pointer(cptrSDR, dataSDR); kSDR=0
                 if(prSDR) then;  do i=1,line_cnt; write(*,'(" >>",i4," ", a)') i,dataSDR(startSDR(i):endSDR(i)); end do; endif

      case ("sep"); 
                 typeF=10
                 cptrSEP = get_file_data(shm_data_ptr(10), offset(1,typeF), offndx(1,typeF), file_name,startSEP,   endSEP, line_cnt, type_no)
                 call c_f_pointer(cptrSEP, dataSEP); kSEP=0
                 if(prSEP) then;  do i=1,line_cnt; write(*,'(" >>",i4," ", a)') i,dataSEP(startSEP(i):endSEP(i)); end do; endif

      case ("sol"); 
                 typeF=11
                 cptrSOL = get_file_data(shm_data_ptr(11), offset(1,typeF), offndx(1,typeF), file_name,startSOL,   endSOL, line_cnt, type_no)
                 call c_f_pointer(cptrSOL, dataSOL); kSOL=0
                 if(prSOL) then;  do i=1,line_cnt; write(*,'(" >>",i4," ", a)') i,dataSOL(startSOL(i):endSOL(i)); end do; endif

      case ("sub"); 
                 typeF=12
 if(typeF == 120) then
 print*," v case 12"
       do isbn=1,1277
         ish_no=MAX_HRU*isbn+1 !??????????? 1 based
         Jndex= Offndx(ish_no,typeF)+1
         write(*,'( "Aitpef= 12, isbn= ",i5,"  sh_no= ",i6,"   OFFNDX= ",i5,"       Offset=",i8 )') &
                                 isbn,        ish_no,       offndx(ish_no,typeF), offset(Jndex,typeF)
      enddo
 print*," ^ case 12 write offndx offset",file, file_name
 endif 

                 cptrSUB = get_file_data(shm_data_ptr(12), offset(1,typeF), offndx(1,typeF), file_name,startSUB,   endSUB, line_cnt, type_no)
                 call c_f_pointer(cptrSUB, dataSUB); kSUB=0
                 if(prSUB) then;  do i=1,line_cnt; write(*,'(" >>",i4," ", a)') i,dataSUB(startSUB(i):endSUB(i)); end do; endif
 !print*," ^ case 12 get_file_data"
                 if(prSUB) then;  do i=1,line_cnt; write(*,'(i4," ",i5," - ",i5)')i,startSUB(i),endSUB(i); end do; endif
 !print*," ^ case 12 write startSUB,endSUB prSUB"

      case ("swq"); 
                 typeF=13
                 cptrSWQ = get_file_data(shm_data_ptr(13), offset(1,typeF), offndx(1,typeF), file_name,startSWQ,   endSWQ, line_cnt, type_no)
                 call c_f_pointer(cptrSWQ, dataSWQ); kSWQ=0
                 if(prSWQ) then;  do i=1,line_cnt; write(*,'(" >>",i4," ", a)') i,dataSWQ(startSWQ(i):endSWQ(i)); end do; endif

      case ("wgn"); 
                 typeF=14
                 cptrWGN = get_file_data(shm_data_ptr(14), offset(1,typeF), offndx(1,typeF), file_name,startWGN,   endWGN, line_cnt, type_no)
                 call c_f_pointer(cptrWGN, dataWGN); kWGN=0
                 if(prWGN) then;  do i=1,line_cnt; write(*,'(" >>",i4," ", a)') i,dataWGN(startWGN(i):endWGN(i)); end do; endif

      case ("wus"); 
                 typeF=15
                 cptrWUS = get_file_data(shm_data_ptr(15), offset(1,typeF), offndx(1,typeF), file_name,startWUS,   endWUS, line_cnt, type_no)
                 call c_f_pointer(cptrWUS, dataWUS); kWUS=0
                 if(prWUS) then;  do i=1,line_cnt; write(*,'(" >>",i4," ", a)') i,dataWUS(startWUS(i):endWUS(i)); end do; endif

      case default
               write(*,'(" -> ERROR: file TYPE (fortran,1-N_TYPES) ",i2," not found (",a,").")' ) typeF, file_name(pt_ndx+1:pt_ndx+sufx_len)
               stop
      end select

   end subroutine shm_open
