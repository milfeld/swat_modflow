      subroutine readwus

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine reads data from the HRU/subbasin water use input file
!!    (.wus). The water use file extracts water from the subbasin and it is
!!    considered to be lost from the watershed. These variables should be used
!!    to remove water transported outside the watershed.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    wudeep(:,:) |10^4 m^3/day  |average daily water removal from the deep
!!                               |aquifer for the month
!!    wupnd(:,:)  |10^4 m^3/day  |average daily water removal from the pond
!!                               |for the month
!!    wurch(:,:)  |10^4 m^3/day  |average daily water removal from the reach 
!!                               |for the month
!!    wushal(:,:) |10^4 m^3/day  |average daily water removal from the shallow 
!!                               |aquifer for the month
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    eof         |none          |end of file flag
!!    mon         |none          |counter
!!    titldum     |NA            |title line of .wus file
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

#ifdef SHM_IO
#     define read(x,y,z) k=k+1; READ( dataWUS(startWUS(k):endWUS(k)),y,z )
#     define iff(x)               if( dataWUS(startWUS(k):endWUS(k)) == shm_eof )
#else
#     define iff(x) if( x )
#endif

      use parm

#ifdef SHM_IO
      use shm
      integer*8 :: k
      character :: shm_eof
#endif

      character (len=80) :: titldum
      integer :: eof, mon, j
      real :: swupnd(12), swush(12), swudp(12)

#ifdef SHM_IO
      shm_eof = achar(28)  ! ANSII FS (File Separator)
         k    =     kWUS
#endif

      eof = 0
      swupnd = 0.
      swush = 0.
      swudp = 0.

      do
      read (105,5300,iostat=eof) titldum
      iff (eof < 0) exit
      read (105,5300,iostat=eof) titldum
      iff (eof < 0) exit
      read (105,5300,iostat=eof) titldum
      iff (eof < 0) exit
      read (105,5100,iostat=eof) (swupnd(mon),mon = 1,6)
      iff (eof < 0) exit
      read (105,5100,iostat=eof) (swupnd(mon),mon = 7,12)
      iff (eof < 0) exit
      read (105,5100,iostat=eof) (wurch(mon,i),mon = 1,6)
      iff (eof < 0) exit
      read (105,5100,iostat=eof) (wurch(mon,i),mon = 7,12)
      iff (eof < 0) exit
      read (105,5100,iostat=eof) (swush(mon),mon = 1,6)
      iff (eof < 0) exit
      read (105,5100,iostat=eof) (swush(mon),mon = 7,12)
      iff (eof < 0) exit
      read (105,5100,iostat=eof) (swudp(mon),mon = 1,6)
      iff (eof < 0) exit
      read (105,5100,iostat=eof) (swudp(mon),mon = 7,12)
      exit
      end do

      do j = 1, hrutot(i)
        ihru = 0
        ihru = nhru + j
        do mon = 1, 12
          wupnd(mon,ihru) = swupnd(mon)
          wushal(mon,ihru) = swush(mon)
          wudeep(mon,ihru) = swudp(mon)
        end do
      end do

#ifndef SHM_IO
      close (105)
#endif

      return
 5100 format (6f10.1)
 5300 format (a80)
      end
