      subroutine swat_main()
!!    This is the main program that reads input, calls the main simulation
!!    model, and writes output.
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!         ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    date        |NA            |date simulation is performed where leftmost
!!                               |eight characters are set to a value of
!!                               |yyyymmdd, where yyyy is the year, mm is the 
!!                               |month and dd is the day
!!    isproj      |none          |special project code:
!!                               |1 test rewind (run simulation twice)
!!    time        |NA            |time simulation is performed where leftmost
!!                               |ten characters are set to a value of
!!                               |hhmmss.sss, where hh is the hour, mm is the 
!!                               |minutes and ss.sss is the seconds and
!!                               |milliseconds
!!    values(1)   |year          |year simulation is performed
!!    values(2)   |month         |month simulation is performed
!!    values(3)   |day           |day in month simulation is performed
!!    values(4)   |minutes       |time difference with respect to Coordinated
!!                               |Universal Time (ie Greenwich Mean Time)
!!    values(5)   |hour          |hour simulation is performed
!!    values(6)   |minutes       |minute simulation is performed
!!    values(7)   |seconds       |second simulation is performed
!!    values(8)   |milliseconds  |millisecond simulation is performed
!!    zone        |NA            |time difference with respect to Coordinated
!!                               |Universal Time (ie Greenwich Mean Time)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    prog        |NA            |program name and version
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    i           |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: date_and_time
!!    SWAT: getallo, allocate_parms, readfile, readfig
!!    SWAT: readbsn, std1, readwwq, readinpt, std2, storeinitial
!!    SWAT: openwth, headout, simulate, finalbal, writeaa, pestw 


!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      use smrt_parm
      use io_dirs

      implicit none
      prog = "SWAT Mar 17 2015    VER 2012/Rev 636_smrt"
      write (*,1000)
 1000 format(1x,"               SWAT2012               ",/,             
     &          "               Rev. 636_smrt          ",/,             
     &          "      Soil & Water Assessment Tool    ",/,             
     &          "               PC Version             ",/,             
     &          " Program reading from file.cio . . . executing",/)

!! Set up directories from SWAT_DATA & SWAT_OUTPUT env vars(defaults ./)

!!km  call get_io_dirs()  
!! process input
		
      print*, '           v BEGIN   getallo'
      call getallo
      print*, '           v BEGIN   allocate_parms'
      call allocate_parms
      print*, '           v BEGIN   readfile'
      call readfile
      print*, '           v BEGIN   readbsn'
      call readbsn
      print*, '           v BEGIN   readwwq'
      call readwwq
      print*, '           v BEGIN   readfcst'
      if (fcstyr > 0 .and. fcstday > 0) call readfcst
      print*, '           v BEGIN   readplant'
      call readplant             !! read in the landuse/landcover database
      print*, '           v BEGIN   readtill'
      call readtill              !! read in the tillage database
      print*, '           v BEGIN   readpest'
      call readpest              !! read in the pesticide database
      print*, '           v BEGIN   readfert'
      call readfert              !! read in the fertilizer/nutrient database
      print*, '           v BEGIN   readurban'
      call readurban             !! read in the urban land types database
      print*, '           v BEGIN   readseptwq'
      call readseptwq            !! read in the septic types database     
      print*, '           v BEGIN   readlup'
      call readlup
      print*, '           v BEGIN   readfig'
      call readfig
      print*, '           v BEGIN   readatmodep'
      call readatmodep
      print*, '           v BEGIN   readsmrt_init_mf'
     
      print*, '           v BEGIN   smrt_init_mf',mf_active
      if(mf_active) call smrt_init_mf !rtb MODFLOW

      print*, '           v BEGIN   readinpt'
      call readinpt
      print*, '           v BEGIN   readstd1'
      call std1
      print*, '           v BEGIN   readstd2'
      call std2
      print*, '           v BEGIN   readopenwth'
      call openwth
      print*, '           v BEGIN   readheadout'
      call headout
      print*, '           ^   END   reading inputs'

      !! convert integer to string for output.mgt file
      subnum = ""
      hruno = ""
      do i = 1, mhru
        write (subnum(i),fmt=' (i5.5)') hru_sub(i)
        write (hruno(i),fmt=' (i4.4)') hru_seq(i)  
      end do

      if (isproj == 2) then 
        hi_targ = 0.0
      end if

!! save initial values
      if (isproj == 1) then
        scenario = 2
        call storeinitial
      else if (fcstcycles > 1) then
        scenario =  fcstcycles
        call storeinitial
      else
        scenario = 1
      endif
        if (iclb /= 4) then
      do iscen = 1, scenario

     
        !! simulate watershed processes
        call simulate

        !! perform summary calculations
        call finalbal
        call writeaa
        call pestw

        !!reinitialize for new scenario
        if (scenario > iscen) call rewind_init
      end do
         end if
      do i = 101, 109       !Claire 12/2/09: change 1, 9  to 101, 109.
        close (i)
      end do
      close(124)
      write (*,1001)
 1001 format (/," Execution successfully completed ")
	
        iscen=1
!! file for Mike White to review to ensure simulation executed normally
      open (9999,file='fin.fin')
      write (9999,*) 'Execution successful'
      close (9999)
      
	stop
      end
