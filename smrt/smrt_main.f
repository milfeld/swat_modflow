      program main

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine links sets up the SWAT-MODFLOW-RT3D linking subroutines
!!    and "events" and then calls SWAT-MODFLOW through "events"
!!
        use mf_rt_link, only: rt_active !MODFLOW-RT3D Linkage
        use smrt_parm, only: mf_active !SWAT-MODFLOW linkage

        use io_dirs
#ifdef SHM_IO
        use shm
#endif
        implicit none
       integer :: isbn, Index,type, ish_no
        ! Set up directories from SWAT_DATA & SWAT_OUTPUT env vars(defaults ./)
        call get_io_dirs()

#ifdef SHM_IO
        print*," v shm_init"
        call shm_init()
        print*," ^ shm_init"
!       type=12
!       do isbn=1,1277
!         ish_no=MAX_HRU*isbn+1 !??????????? 1 based
!         Index= Offndx(ish_no,type)+1
!         write(*,'("Wi isbn",i5," sh_no",i8," NDX",i8," SET=",i8)')    
!    1         isbn, ish_no, Offndx(ish_no,type), Offset(Index,type)
!       enddo
#endif

        ! Read the SWAT-MODFLOW input files
        call smrt_read_link
 
        ! Run SWAT's main subroutine
        call swat_main

        ! Close swat-modflow (close files, deallocate variables, etc.)s
        call smrt_close
        call mf_close
        call rt_close
      end program main
