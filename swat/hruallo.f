      subroutine hruallo

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!   This subroutine calculates the number of management operation types, etc.
!!   used in the simulation. These values are used to allocate array sizes for
!!   processes occurring in the HRU.

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    mapp        |none        |max number of applications
!!    mcr         |none        |max number of crops grown per year
!!    mcut        |none        |max number of cuttings per year
!!    mgr         |none        |max number of grazings per year
!!    mlyr        |none        |max number of soil layers
!!    mnr         |none        |max number of years of rotation
!!    pstflg(:)   |none        |flag for types of pesticide used in watershed
!!                             |array location is pesticide ID number
!!                             |0: pesticide not used
!!                             |1: pesticide used
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ap_af       |none        |number of autofertilizer operations in mgt file
!!    ap_ai       |none        |number of autoirrigation operations in mgt file
!!    ap_cc       |none        |number of continuous cuuting operations in mgt
!!    ap_cf       |none        |number of continuous fertilization operations in mgt
!!    ap_ci       |none        |number of continuous irrigation operations in mgt
!!    ap_f        |none        |number of fertilizer operations in mgt file
!!    ap_i        |none        |number of irrigation operations in mgt file
!!    ap_p        |none        |number of pesticide operations in mgt file
!!    ap_r        |none        |number of release/impound operations in mgt file
!!    ap_s        |none        |number of sweep operations in mgt file
!!    ap_t        |none        |number of tillage operations in mgt file
!!    chmfile     |NA          |HRU soil chemical data file name (.chm)
!!    cut         |none        |number of harvest only operations in mgt file
!!    depth(:)    |mm          |depth to bottom of soil layer
!!    eof         |none        |end of file flag (=-1 if eof, else =0)
!!    grz         |none        |number of grazing operations in mgt file
!!    hkll        |none        |number of harvest/kill operations in mgt file
!!    hru         |none        |number of HRUs in subbasin
!!    hrufile     |NA          |name of HRU general data file name (.hru)
!!    ii          |none        |counter
!!    j           |none        |counter
!!    k           |none        |counter
!!    kll         |none        |number of kill operations in mgt file
!!    lyrtot      |none        |total number of layers in profile
!!    mgt_op      |none        |manangement operation code
!!    mgt1i       |none        |sixth parameter in mgt file operation line
!!    mgtfile     |NA          |HRU management data file name (.mgt)
!!    plt         |none        |number of plant operations in mgt file
!!    pstnum      |none        |pesticide ID number from database file
!!    rot         |none        |number of years in rotation used in HRU
!!    solfile     |NA          |HRU soil data file name (.sol)
!!    titldum     |NA          |input lines in .sub that are not processed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: caps

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm
      use io_dirs, only: data_swat, data_sol, data_mgt, data_chm

#ifdef SHM_IO
      use shm
      use  c_shm_api
      integer*8 :: kk
      character :: shm_eof
#endif

      character (len=13) :: hrufile, mgtfile, solfile, chmfile
      character (len=80) ::  titldum
      integer :: eof, j, k, lyrtot, rot, plt, ap_f, ap_p, ap_t, ap_i
      integer :: grz, cut, mgt1i, pstnum, ii, ap_r, ap_s, kll, hkll
      integer :: ap_ai, ap_af, mgt_op, ap_cf, ap_cc, ap_ci, jj
      integer :: iopera_sub
      real :: depth(25)

#ifdef SHM_IO
      shm_eof = achar(28)  ! ANSII FS (File Separator)
#endif

      do j= mhru1, mhru
      mgtfile = ""
      solfile = ""
      chmfile = ""

#ifdef SHM_IO
        kk    =    kSUB
#     define read(x,y,z) kk=kk+1; READ(dataSUB(startSUB(kk):endSUB(kk)),y,iostat=eof)
#endif
      read (25,5300,iostat=eof)hrufile, mgtfile, solfile, chmfile, ilnds
      !rm KFM print*,"hrufile= ",hrufile
      !rm KFM print*,"mgtfile= ",mgtfile
      !rm KFM print*,"solfile= ",solfile
      !rm KFM print*,"chmfile= ",chmfile
      !rm KFM print*,"ilnds  = ",ilnds

      if (eof < 0) return
      if (ilnds > 0) then 
        ils_nofig = 1
      end if      
        call caps(mgtfile)
        call caps(solfile)
        call caps(chmfile)

#ifdef SHM_IO
#     define read(x,y) kk=kk+1; READ( dataSOL(startSOL(kk):endSOL(kk)),y )
#     define open(x,y) call shm_open(y)
         kk   =     kSOL
#endif

!!km    open (9,file=data_swat//solfile,recl=350)
        open (9,file=data_sol//solfile)         !!kfm removed recl=350
!     print*," ^ open solfile in hruallo: ",solfile  !! rm KFM
        !! calculate # of soil layers in profile
          depth = 0.
          lyrtot = 0
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6100) (depth(k), k = 1, 25)
          do k = 1, 25
            if (depth(k) <= 0.001) lyrtot = k - 1
            if (depth(k) <= 0.001) exit
          end do
          mlyr = Max(mlyr,lyrtot)
#ifndef SHM_IO
          close (9)
#endif
#     undef  read(x,y)
#     undef  open(x,y)


#ifdef SHM_IO
#     define open(x,y) call shm_open(y)
#     define read(x,y,z) kk=kk+1; READ( dataMGT(startMGT(kk):endMGT(kk)),y,z )
#     define iff(x)                 if( dataMGT(startMGT(kk):endMGT(kk)) == shm_eof )
        kk      =     kMGT
#else
#     define iff(x) if( x )
#endif

        open (10,file=data_mgt//mgtfile)
        
      
!!  calculate max number of operations per hru
        iopera_sub = 1
        mcri = 0
        do kl = 1, 30
          read (10,6000,iostat=eof) titldum      !!kfm added iostat
        end do

        
        do kl = 1, 1000
          read (10,6300,iostat=eof) mgt_op, mgt1i
          iff (eof < 0) exit                     !!kfm changed to iff
          if  (mgt_op == 1) then
            mcri = mcri + 1
          end if
          if (mgt_op == 4 .and. mgt1i > 0) pstflg(mgt1i) = 1
          iopera_sub = iopera_sub + 1
        end do
        iopera = Max(iopera,iopera_sub)
        mcr = Max(mcr,mcri)

#ifndef SHM_IO
        close (10)            !!   nubz test
#endif

#     undef  read(x,y)
#     undef  open(x,y)
#     undef   iff(x)

#ifdef SHM_IO
#     define open(x,y) call shm_open(y)
#     define read(x,y,z) kk=kk+1; READ( dataCHM(startCHM(kk):endCHM(kk)),y,z )
#     define iff(x)                 if( dataCHM(startCHM(kk):endCHM(kk)) == shm_eof )
        kk      =     kCHM
#else
#     define iff(x) if( x )
#endif


        open (11,file=data_chm//chmfile)
          eof = 0
          do 
            do k = 1, 11
              read (11,6000,iostat=eof) titldum
              iff (eof < 0) exit   !kfm changed to iff
            end do
            iff (eof < 0) exit     !kfm changed to iff
            do
              pstnum = 0
              read (11,*,iostat=eof) pstnum
              iff (eof < 0) exit   !kfm changed to iff
              if (pstnum > 0) pstflg(pstnum) = 1
            end do
            iff (eof < 0) exit     !kfm changed to iff
          end do
      close (11)
      end do    ! hru loop
      
      return
 5000 format (6a)
 5001 format (a1,9x,5i6)
 5002 format(a)
 5100 format (20a4)
 5200 format (10i4)
 5300 format (4a13,52x,i6)
 6000 format (a80)
 6100 format (27x,25f12.2)
 6200 format (1x,i3)
 6300 format (16x,i2,1x,i4)
      end
