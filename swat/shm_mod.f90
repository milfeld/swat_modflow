module verbosity
use ISO_C_BINDING, only: C_INT
integer(C_INT), bind(C)    ::  c_verbose=0
integer(C_INT)             ::  f_verbose=0
  interface
    subroutine set_c_verbosity(f_verbose) bind(C)
      import  C_INT, c_verbose
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

integer,parameter :: MAX_TYPE=15,    MAX_SBN=1500, MAX_HRU=14
integer,parameter :: MAX_FILE=60000, MAX_LINE=500
integer,parameter :: MAX_SBNxMAX_HRU=MAX_SBN*MAX_HRU

integer,parameter :: MAX_DATA_CHARS_in_FILE =6400

integer      :: file_ct(MAX_TYPE), line_cnt, type_no

!integer(KI8) :: offset(MAX_FILE+1), offndx(MAX_TYPE*MAX_SBN*MAX_HRU+1), start(MAX_LINE+1), end(MAX_LINE)
integer(KI8) :: offset(MAX_TYPE*MAX_SBN*MAX_HRU+1,MAX_TYPE)  !????????+1  Maxsbn x  Maxhru
integer(KI8) :: offndx(MAX_TYPE*MAX_SBN*MAX_HRU+1,MAX_TYPE)  !????????+1

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
    subroutine get_shm_meta(file_ct, offset, offndx) bind(C)
      import               :: C_INT, C_LONG
      integer(C_INT)       :: file_ct(*)  !not (:)
      integer(C_LONG)      :: offset(*)   !not (:)
      integer(C_LONG)      :: offndx(*)   !not (:)
    end subroutine
  end interface

  interface
    function get_shm_data_ptr(shm_data_fd,itype) result(shm_data_ptr) bind(C)
      import               :: C_INT, C_PTR
      integer(C_INT)       :: shm_data_fd(*)  !not (:)
      integer(C_INT)       :: itype
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
      import                 :: C_PTR
      type(C_PTR),value,intent(in) :: shm_data_ptr
    end subroutine
  end interface

  interface
    subroutine shm_open(file)
      import C_F_POINTER, C_NULL_CHAR
      character(len=*)  :: file
      character(len=14) :: file_name
      integer           :: sufx_len, pt_ndx
    end subroutine
  end interface


end module c_shm_api     !http://fortranwiki.org/fortran/show/iso_c_binding
