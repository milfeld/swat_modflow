      MODULE GWFFHBMODULE
        INTEGER, SAVE, POINTER ::NBDTIM,NFLW,NHED,IFHBCB,NFHBX1,
     1                           NFHBX2,IFHBSS
        INTEGER, SAVE, POINTER, DIMENSION(:,:)  ::IFLLOC
        INTEGER, SAVE, POINTER, DIMENSION(:,:)  ::IHDLOC
        REAL,    SAVE, POINTER, DIMENSION(:)    ::BDTIM
        REAL,    SAVE, POINTER, DIMENSION(:,:)  ::FLWRAT
        REAL,    SAVE, POINTER, DIMENSION(:,:)  ::SBHED
        REAL,    SAVE, POINTER, DIMENSION(:,:)  ::BDFV
        REAL,    SAVE, POINTER, DIMENSION(:,:)  ::BDHV
        REAL,    SAVE, POINTER, DIMENSION(:)  ::FHBXWT
        CHARACTER(LEN=16), SAVE, POINTER, DIMENSION(:) ::FHBXNM
      TYPE GWFFHBTYPE
        INTEGER, POINTER ::NBDTIM,NFLW,NHED,IFHBCB,NFHBX1,
     1                           NFHBX2,IFHBSS
        INTEGER, POINTER, DIMENSION(:,:)  ::IFLLOC
        INTEGER, POINTER, DIMENSION(:,:)  ::IHDLOC
        REAL,    POINTER, DIMENSION(:)    ::BDTIM
        REAL,    POINTER, DIMENSION(:,:)  ::FLWRAT
        REAL,    POINTER, DIMENSION(:,:)  ::SBHED
        REAL,    POINTER, DIMENSION(:,:)  ::BDFV
        REAL,    POINTER, DIMENSION(:,:)  ::BDHV
        REAL,    POINTER, DIMENSION(:)  ::FHBXWT
        CHARACTER(LEN=16), POINTER, DIMENSION(:) ::FHBXNM
      END TYPE
      TYPE(GWFFHBTYPE), SAVE  ::GWFFHBDAT(10)
      END MODULE


      SUBROUTINE GWF2FHB7AR(IN,IGRID)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE AND READ DATA FOR FLOW AND HEAD BOUNDARY
C     PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,ITRSS,IBOUND
      USE GWFFHBMODULE,ONLY:NBDTIM,NFLW,NHED,IFHBCB,NFHBX1,NFHBX2,
     1                      IFHBSS,IFLLOC,IHDLOC,BDTIM,FLWRAT,SBHED,
     2                      BDFV,BDHV,FHBXWT,FHBXNM
C
      CHARACTER*80 LINE
      CHARACTER*1 DSH1
      DATA DSH1/'-'/
C     ------------------------------------------------------------------
      ALLOCATE(NBDTIM,NFLW,NHED,IFHBCB,NFHBX1,NFHBX2,IFHBSS)
      ALLOCATE(FHBXWT(10))
      ALLOCATE(FHBXNM(10))
C
C1------IDENTIFY PACKAGE
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'FHB1 -- SPECIFIED FLOW PACKAGE, VERSION 1,12/3/96',
     &' INPUT READ FROM',I3)
C
C2------READ NUMBER OF TIMES, NUMBER OF SPECIFIED-FLOW CELLS AND
C2------UNIT OR FLAG FOR CELL-BY-CELL FLOW TERMS, NUMBER OF 
C2------AUXILIARY VARIABLES.
      READ(IN,*) NBDTIM,NFLW,NHED,IFHBSS,IFHBCB,NFHBX1,NFHBX2
C
C3------PRINT NBDTIM, STOP IF NO TIMES ARE TO BE SPECIFIED 
      IF(NFLW.LT.1.AND.NHED.LT.1) THEN
         WRITE(IOUT,4)
 4       FORMAT(1X,'SPECIFIED FLOW AND HEAD BOUNDARY OPTION ',
     &   'CANCELLED.',/,1X,'NO BOUNDARY CELLS WERE SPECIFIED.')
         IN=0
         RETURN
      ENDIF
      IF(NBDTIM.LT.1) THEN
         WRITE(IOUT,6)
 6       FORMAT(1X, 'SIMULATION ABORTING.  NOT ENOUGH TIMES ',
     &   'SPECIFIED FOR FHB1 PACKAGE.')
         CALL USTOP(' ')
      ELSE IF(NBDTIM.EQ.1) THEN
         WRITE(IOUT,8)
 8       FORMAT(1X,' SPECIFIED FLOW AND HEAD VALUES WILL REMAIN ',
     &   'CONSTANT FOR ENTIRE SIMULATION.')
      ELSE
         WRITE(IOUT,10) NBDTIM
 10      FORMAT(1H ,'TOTAL OF',I5,' TIMES WILL BE USED TO DEFINE ',
     &   'VARIATIONS IN FLOW AND HEAD.')
      ENDIF
C
C4------PRINT NFLW AND NHED AND STEADY-STATE OPTION
CERB  This section was changed to use ITRSS instead of ISS
CERB  I also eliminated use of Hollerith constants -- ERB 10/8/2002
      WRITE(IOUT,12) NFLW
 12   FORMAT(1X,'FLOW WILL BE SPECIFIED AT A TOTAL OF',I5,' CELLS.')
      WRITE(IOUT,14) NHED
 14   FORMAT(1X,'HEAD WILL BE SPECIFIED AT A TOTAL OF',I5,' CELLS.')
      IF(ITRSS.NE.0) THEN
        WRITE(IOUT,15)
 15     FORMAT(1X,'FHB STEADY-STATE OPTION FLAG WILL BE IGNORED,'/,
     &          1X,'AT LEAST ONE STRESS PERIOD IS TRANSIENT.')
      ELSE
        IF(IFHBSS.EQ.0) THEN
          WRITE(IOUT,16)
 16       FORMAT(1X,'FLOW, HEAD, AND AUX VARIABLES AT TIME=0 WILL BE ',
     &        /,1X,'USED IN STEADY-STATE SIMULATIONS.')
        ELSE
          WRITE(IOUT,18)
 18       FORMAT(1X,'FLOW, HEAD, AND AUX VARIABLES WILL BE ',
     &      'INTERPOLATED',/,1X,'IN SIMULATIONS.')
        ENDIF
      ENDIF
CERB  End of changed section
C
C5------IF CELL-BY-CELL FLOW TERMS ARE TO BE SAVED THEN PRINT UNIT #
      IF(IFHBCB.GT.0) WRITE(IOUT,20) IFHBCB
 20   FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE RECORDED ON UNIT',I3)
      IF(IFHBCB.LT.0) WRITE(IOUT,24)
 24   FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL NOT 0')
C
C6------READ AUXILIARY VARIABLES
      IF(NFHBX1.GT.5.OR.NFHBX2.GT.5) THEN
         WRITE(IOUT,*) ' ABORTING. A MAXIMUM OF 5 AUXILIARY VARIABLES',
     &   ' CAN BE DEFINED BY FHB.'
         CALL USTOP(' ')
      ENDIF
      WRITE(IOUT,26) NFHBX1
 26   FORMAT(1X,I2,' AUXILIARY VARIABLES FOR SPECIFIED-FLOW CELLS WILL',
     & /,'  BE DEFINED BY FHB FOR USE BY OTHER PACKAGES.')
      IF(NFHBX1.LT.1) GO TO 38
      WRITE(IOUT,28)
 28   FORMAT('       NAME      WEIGHTING FACTOR',/,1X,32('-'))
      DO 30 NX=1,NFHBX1
      READ(IN,'(A)') LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      FHBXNM(NX)=LINE(ISTART:ISTOP)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,FHBXWT(NX),IOUT,IN)
      WRITE(IOUT,29) FHBXNM(NX),FHBXWT(NX)
 29   FORMAT(1X,A16,F11.2)
      IF(FHBXWT(NX).LT.0.0.OR.FHBXWT(NX).GT.1.0) THEN
      WRITE(IOUT,*) ' Aborting. Weights for Auxiliary variables cannot'
      WRITE(IOUT,*) ' be less than 0.0 or greater than 1.0.'
      CALL USTOP(' ')
      ENDIF
 30   CONTINUE
 38   WRITE(IOUT,126) NFHBX2
 126  FORMAT(1X,I2,' AUXILIARY VARIABLES FOR SPECIFIED-HEAD CELLS WILL',
     & /,'  BE DEFINED BY FHB FOR USE BY OTHER PACKAGES.')
      IF(NFHBX2.LT.1) GO TO 200
      WRITE(IOUT,28)
      DO 130 NX=1,NFHBX2
      READ(IN,'(A)') LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      FHBXNM(5+NX)=LINE(ISTART:ISTOP)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,FHBXWT(5+NX),IOUT,IN)
      WRITE(IOUT,129) FHBXNM(5+NX),FHBXWT(5+NX)
 129  FORMAT(1X,A16,F11.2)
      IF(FHBXWT(5+NX).LT.0.0.OR.FHBXWT(5+NX).GT.1.0) THEN
      WRITE(IOUT,*) ' Aborting. Weights for Auxiliary variables cannot'
      WRITE(IOUT,*) ' be less than 0.0 or greater than 1.0.'
      CALL USTOP(' ')
      ENDIF
 130  CONTINUE
C7------ALLOCATE SPACE FOR ARRAYS BDTIM, IFLLOC, FLWRAT, BDFV,
C7------IHDLOC, SBHED, AND BDHV
 200  IFHBD3=NBDTIM*(1+NFHBX1)
      IFHBD4=2+NFHBX1
      IFHBD5=NBDTIM*(1+NFHBX2)
      IF(NFLW.GT.0) THEN
         ALLOCATE(FLWRAT(IFHBD3,NFLW))
         ALLOCATE(BDFV(IFHBD4,NFLW))
         ALLOCATE(IFLLOC(4,NFLW))
      ELSE
         ALLOCATE(FLWRAT(1,1))
         ALLOCATE(BDFV(1,1))
         ALLOCATE(IFLLOC(1,1))
      END IF
      IF(NHED.GT.0) THEN
         ALLOCATE(SBHED(IFHBD5,NHED))
         ALLOCATE(BDHV(NFHBX2,NHED))
         ALLOCATE(IHDLOC(4,NHED))
      ELSE
         ALLOCATE(SBHED(1,1))
         ALLOCATE(BDHV(1,1))
         ALLOCATE(IHDLOC(1,1))
      END IF
      ALLOCATE(BDTIM(NBDTIM))
C
C1------READ TIMES AT WHICH SPECIFIED FLOW AND HEAD VALUES WILL BE READ
      READ(IN,*) IFHBUN,CNSTM,IFHBPT
      WRITE(IOUT,310) IFHBUN,CNSTM
310   FORMAT(1X,'TIMES FOR SPECIFIED-FLOW AND HEAD VALUES WILL BE READ',
     & ' ON UNIT',I4,' AND',/,
     &' MULTIPLIED BY',G12.4,'.')
      READ(IFHBUN,*) (BDTIM(L),L=1,NBDTIM)
      DO 312 L=1,NBDTIM
      BDTIM(L)=BDTIM(L)*CNSTM
312   CONTINUE
C
C2------IF DESIRED, PRINT TABLE OF TIMES
      IF(IFHBPT.GT.0) THEN
         WRITE(IOUT,320) NBDTIM
320      FORMAT(1X,I5,' TIMES FOR SPECIFYING FLOWS AND HEADS:')
         WRITE(IOUT,322) (L,L=1,NBDTIM)
322      FORMAT(16X,I8,4I12)
         ND=MIN0(60,NBDTIM*12)
         WRITE(IOUT,324) (DSH1,M=1,ND)
324      FORMAT(17X,60A1)
         WRITE(IOUT,326) (BDTIM(L),L=1,NBDTIM)
326      FORMAT(17X,5G12.4)
      ENDIF
C
C3------MAKE SURE THAT FIRST TIME IS ZERO AND THAT TIMES INCREASE
      ICHK1=0
      ICHK2=0
      IF(BDTIM(1).NE.0.0) THEN
         WRITE(IOUT,330)
330      FORMAT(1X,'STARTING TIME FOR SPECIFIED FLOWS AND HEADS MUST',
     &   ' BE ZERO. ABORTING.')
         ICHK1=1
      ENDIF
      DO 40 L=2,NBDTIM
      IF(BDTIM(L).LT.BDTIM(L-1)) THEN
         WRITE(IOUT,32)
 32      FORMAT(1X,'TIMES FOR SPECIFIED FLOWS MUST INCREASE.',
     &   '  ABORTING.')
         ICHK2=1
         GO TO 42
      ENDIF
 40   CONTINUE
 42   IF(ICHK1.EQ.1.OR.ICHK2.EQ.1) CALL USTOP(' ')
C
C4A-----READ CELL INDICIES AND SPECIFIED-FLOW RATES
      IF(NFLW.LT.1) GO TO 70
      READ(IN,*) IFHBUN,CNSTM,IFHBPT
      WRITE(IOUT,50) IFHBUN,CNSTM
 50   FORMAT(/,1X,'CELL INDICIES AND SPECIFIED-FLOW RATES ',
     & 'WILL BE READ ON UNIT',I4,'. RATES WILL',/,
     & 1X,'BE MULTIPLIED BY',G12.4,'.')
      IF(IFHBPT.GT.0) THEN
         WRITE(IOUT,52)
 52      FORMAT(1X,'LAYER  ROW  COL IAUX              FLOW RATES')
         ND=MIN0(79,19+NBDTIM*12)
         WRITE(IOUT,54) (DSH1,M=1,ND)
 54      FORMAT(1X,78A1)
      ENDIF
      DO 59 N=1,NFLW
      READ(IFHBUN,*) (IFLLOC(I,N),I=1,4),(FLWRAT(L,N),L=1,NBDTIM)
      DO 56 L=1,NBDTIM
      FLWRAT(L,N)=FLWRAT(L,N)*CNSTM
 56   CONTINUE
C
C4B-----IF DESIRED, PRINT TABLE OF SPECIFIED-FLOW CELL LOCATIONS
C4B-----AND RATES
      IF(IFHBPT.GT.0) THEN
         WRITE(IOUT,58) (IFLLOC(I,N),I=1,4),(FLWRAT(L,N),L=1,NBDTIM)
 58      FORMAT(1X,I4,3I5,5G12.4,/,(20X,5G12.4))
      ENDIF
 59   CONTINUE
C
C5A------READ VALUES OF AUXILIARY VARIABLES FOR SPECIFIED-FLOW CELLS
      IF(NFHBX1.LT.1) GO TO 70
      DO 69 NX=1,NFHBX1
      NS=NBDTIM*NX
      READ(IN,*) IFHBUN,CNSTM,IFHBPT
      WRITE(IOUT,61) FHBXNM(NX),IFHBUN,CNSTM
 61   FORMAT(/,1X,A16,
     & 'FOR SPECIFIED-FLOW CELLS WILL BE READ ON UNIT',I4,'.',/,
     &  ' VALUES WILL BE MULTIPLIED BY',G12.4,'.')
      IF(IFHBPT.GT.0) THEN
         WRITE(IOUT,62) FHBXNM(NX)
 62      FORMAT(1X,'LAYER  ROW  COL IAUX  ',A16)
         WRITE(IOUT,54) (DSH1,M=1,ND)
      ENDIF
      DO 68 N=1,NFLW
      READ(IFHBUN,*) (FLWRAT(NS+L,N),L=1,NBDTIM)
      DO 66 L=1,NBDTIM
      FLWRAT(NS+L,N)=FLWRAT(NS+L,N)*CNSTM
 66   CONTINUE
C
C5B------IF DESIRED, PRINT TABLE OF AUXILIARY VARIABLE VALUES AT
C5B------SPECIFIED-FLOW CELL LOCATIONS
      IF(IFHBPT.GT.0) THEN
         WRITE(IOUT,58) (IFLLOC(I,N),I=1,4),
     &                        (FLWRAT(NS+L,N),L=1,NBDTIM)
      ENDIF
 68   CONTINUE
 69   CONTINUE
C
C6------READ CELL INDICIES AND SPECIFIED-HEAD VALUES
 70   IF(NHED.LT.1) GO TO 300
      READ(IN,*) IFHBUN,CNSTM,IFHBPT
      WRITE(IOUT,71) IFHBUN,CNSTM
 71   FORMAT(/,1X,'CELL INDICIES AND SPECIFIED-HEAD VALUES ',
     & 'WILL BE READ ON UNIT',I4,'. HEAD VALUES',/,
     & 1X,'WILL BE MULTIPLIED BY',G12.4,'.')
      IF(IFHBPT.GT.0) THEN
         WRITE(IOUT,72)
 72      FORMAT(1X,'LAYER  ROW  COL IAUX             HEAD VALUES')
         ND=MIN0(79,19+NBDTIM*12)
         WRITE(IOUT,74) (DSH1,M=1,ND)
 74      FORMAT(1X,79A1)
      ENDIF
      DO 80 N=1,NHED
      READ(IFHBUN,*) (IHDLOC(I,N),I=1,4),(SBHED(L,N),L=1,NBDTIM)
      DO 75 L=1,NBDTIM
      SBHED(L,N)=SBHED(L,N)*CNSTM
 75   CONTINUE
C
C7------AT SPECIFIED-HEAD LOCATIONS, SET IBOUND TO NEGATIVE NUMBER.
C7------IGNORE SPECIFIED-HEAD CONDITIONS AT CELLS WHERE IBOUND IS ZERO
      K=IHDLOC(1,N)
      I=IHDLOC(2,N)
      J=IHDLOC(3,N)
      IF(IBOUND(J,I,K).NE.0) THEN
         IBOUND(J,I,K)=-IABS(IBOUND(J,I,K))
      ELSE
         WRITE(IOUT,76) IHDLOC(2,N),IHDLOC(3,N),IHDLOC(1,N)
 76      FORMAT(1X,'SPECIFIED-HEAD VALUE IGNORED AT ROW',I5,', COLUMN',
     &   I5,', AND LAYER',I5,'.')
      ENDIF
C
C8------IF DESIRED, PRINT TABLE OF SPECIFIED-FLOW CELL LOCATIONS
C8------AND RATES
      IF(IFHBPT.GT.0) THEN
         IF(IBOUND(J,I,K).NE.0) 
     &   WRITE(IOUT,58) (IHDLOC(I,N),I=1,4),(SBHED(L,N),L=1,NBDTIM)
      ENDIF
 80   CONTINUE
C
C9A------READ VALUES OF AUXILIARY VARIABLES FOR SPECIFIED-HEAD CELLS
      IF(NFHBX2.LT.1) GO TO 300
      DO 169 NX=1,NFHBX2
      NS=NBDTIM*NX
      READ(IN,*) IFHBUN,CNSTM,IFHBPT
      WRITE(IOUT,161) FHBXNM(5+NX),IFHBUN,CNSTM
 161  FORMAT(/,1X,A16,
     & 'FOR SPECIFIED-HEAD CELLS WILL BE READ ON UNIT',I4,'.',/,
     &  ' VALUES WILL BE MULTIPLIED BY',G12.4,'.')
      IF(IFHBPT.GT.0) THEN
         WRITE(IOUT,62) FHBXNM(5+NX)
         WRITE(IOUT,54) (DSH1,M=1,ND)
      ENDIF
      DO 168 N=1,NHED
      READ(IFHBUN,*) (SBHED(NS+L,N),L=1,NBDTIM)
      DO 166 L=1,NBDTIM
      SBHED(NS+L,N)=SBHED(NS+L,N)*CNSTM
 166  CONTINUE
C
C9B------IF DESIRED, PRINT TABLE OF AUXILIARY VARIABLE VALUES AT
C9B------SPECIFIED-HEAD CELL LOCATIONS
      IF(IFHBPT.GT.0) THEN
         WRITE(IOUT,58) (IHDLOC(I,N),I=1,4),
     &                        (SBHED(NS+L,N),L=1,NBDTIM)
      ENDIF
 168  CONTINUE
 169  CONTINUE
C
C10-----RETURN
 300  CALL SGWF2FHB7PSV(IGRID)
      RETURN
      END
      SUBROUTINE GWF2FHB7AD(IGRID)
C     ******************************************************************
C     COMPUTE SPECIFIED FLOWS AND HEADS AT CURRENT TIME STEP
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:ITRSS,HNEW,HOLD
      USE GWFBASMODULE,ONLY:DELT,TOTIM
      USE GWFFHBMODULE,ONLY:NBDTIM,NFLW,NHED,IFHBCB,NFHBX1,NFHBX2,
     1                      IFHBSS,IFLLOC,IHDLOC,BDTIM,FLWRAT,SBHED,
     2                      BDFV,BDHV,FHBXWT
C     ------------------------------------------------------------------
      CALL SGWF2FHB7PNT(IGRID)
C
C1------IF THIS IS A STEADY-STATE SIMULATION OR A TRANSIENT SIMULATION
C1------WITH CONSTANT SPECIFIED FLOWS AND HEADS, SET VALUES AND RETURN
      IF((ITRSS.EQ.0.AND.IFHBSS.EQ.0).OR.NBDTIM.EQ.1) THEN
         IF(NFLW.LT.1) GO TO 6
         DO 5 NF=1,NFLW
         BDFV(1,NF)=FLWRAT(1,NF)
         IF(NFHBX1.LT.1) GO TO 5
         DO 4 NX=1,NFHBX1
         N1=2+NX
         N2=1+NX*NBDTIM
         BDFV(N1,NF)=FLWRAT(N2,NF)
 4       CONTINUE
 5       CONTINUE
 6       IF(NHED.LT.1) RETURN
         DO 10 NH=1,NHED
         K=IHDLOC(1,NH)
         I=IHDLOC(2,NH)
         J=IHDLOC(3,NH)
         HNEW(J,I,K)=SBHED(1,NH)
         HOLD(J,I,K)=SBHED(1,NH)
         IF(NFHBX2.LT.1) GO TO 10
         DO 8 NX=1,NFHBX2
         N2=1+NX*NBDTIM
         BDHV(NX,NH)=SBHED(N2,NH)
 8       CONTINUE
 10      CONTINUE
         RETURN
      ENDIF
C
C2------FIND ARRAY INDICES OF TIMES AROUND TIME AT START OF CURRENT
C2------TIME STEP
      IF(NFLW.LT.1) GO TO 200
      T2=TOTIM
      T1=TOTIM-DELT
      DO 20 L=2,NBDTIM
      IF(T1.LE.BDTIM(L)) THEN
         IB1=L-1
         IB2=L
         GO TO 40
      ENDIF
 20   CONTINUE
      IB1=NBDTIM-1
      IB2=NBDTIM
C
C3------COMPUTE FACTOR FOR INTERPOLATION OR EXTRAPOLATION OF FLOW AT
C3------START OF CURRENT TIME STEP
 40   QFACT1=(T1-BDTIM(IB1))/(BDTIM(IB2)-BDTIM(IB1))
C
C4------FIND ARRAY INDICES OF TIMES AROUND TIME AT END OF CURRENT
C4------TIME STEP
      DO 60 L=IB2,NBDTIM
      IF(T2.LE.BDTIM(L)) THEN
         IB3=L-1
         IB4=L
         GO TO 70
      ENDIF
 60   CONTINUE
      IB4=NBDTIM
      IB3=NBDTIM-1
C
C5------COMPUTE FACTOR FOR INTERPOLATION OR EXTRAPOLATION OF FLOW AT
C5------END OF CURRENT TIME STEP
 70   QFACT2=(T2-BDTIM(IB3))/(BDTIM(IB4)-BDTIM(IB3))
C
C6------COMPUTE SPECIFIED FLOW RATES FOR THIS TIME STEP
      NPI=IB4-IB2
      DO 90 NF=1,NFLW
      QA=FLWRAT(IB1,NF)
      QB=FLWRAT(IB2,NF)
      QC=FLWRAT(IB3,NF)
      QD=FLWRAT(IB4,NF)
      Q1=(QA+QFACT1*(QB-QA))
      Q2=(QC+QFACT2*(QD-QC))
      IF(NPI.EQ.0) THEN
         BDFV(1,NF)=0.5*(Q1+Q2)
      ELSE
         TP=T1
         QP=Q1
         SUM1=0.0
         DO 80 NI=IB2,IB3
         QN=FLWRAT(NI,NF)
         DDT=BDTIM(NI)-TP
         SUM1=SUM1+DDT*0.5*(QN+QP)
         TP=BDTIM(NI)
         QP=QN 
 80      CONTINUE
         DDT=T2-TP
         SUM1=SUM1+DDT*0.5*(Q2+QP)
         BDFV(1,NF)=SUM1/DELT
      ENDIF
 90   CONTINUE
C
C7-----COMPUTE VALUES OF AUXILIARY VARIABLES FOR SPECIFIED-FLOW
C7-----CELLS FOR CURRENT TIME STEP
      IF(NFHBX1.LT.1) GO TO 200
      DO 190 NX=1,NFHBX1
      N1=2+NX
      N2=NX*NBDTIM
      TT=TOTIM-(1.-FHBXWT(NX))*DELT
      DO 120 L=2,NBDTIM
      IF(TT.LE.BDTIM(L)) THEN
         IB1=L-1
         IB2=L
         GO TO 140
      ENDIF
 120  CONTINUE
      IB1=NBDTIM-1
      IB2=NBDTIM
 140  XFACT=(TT-BDTIM(IB1))/(BDTIM(IB2)-BDTIM(IB1))
      DO 150 NF=1,NFLW
      XX=FLWRAT(N2+IB1,NF)+XFACT*(FLWRAT(N2+IB2,NF)-FLWRAT(N2+IB1,NF))
      BDFV(N1,NF)=XX
 150  CONTINUE
 190  CONTINUE
C8------FIND ARRAY INDICES OF TIMES AROUND TIME AT END OF CURRENT
C8------TIME STEP, COMPUTE FACTOR OF INTERPOLATION OR EXTRAPOLATION
C8------OF HEAD
 200  IF(NHED.LT.1) RETURN
      TT=TOTIM
      DO 220 L=2,NBDTIM
      IF(TT.LE.BDTIM(L)) THEN
         IB1=L-1
         IB2=L
         GO TO 240
      ENDIF
 220  CONTINUE
      IB1=NBDTIM-1
      IB2=NBDTIM
 240  HFACT=(TT-BDTIM(IB1))/(BDTIM(IB2)-BDTIM(IB1))
C
C9------AT EACH SPECIFIED-HEAD LOCATION, INTERPOLATE OR EXTRAPOLATE
C9------HEAD. SET HNEW AND HOLD EQUAL TO COMPUTED HEAD
      DO 250 NH=1,NHED
      K=IHDLOC(1,NH)
      I=IHDLOC(2,NH)
      J=IHDLOC(3,NH)
      HH=SBHED(IB1,NH)+HFACT*(SBHED(IB2,NH)-SBHED(IB1,NH))
      HNEW(J,I,K)=HH
      HOLD(J,I,K)=HH
 250  CONTINUE
C
C10----COMPUTE VALUES OF AUXILIARY VARIABLES FOR FOR SPECIFIED-HEAD
C10----CELLS FOR CURRENT TIME STEP
      IF(NFHBX2.LT.1) RETURN
      DO 390 NX=1,NFHBX2
      N2=NX*NBDTIM
      TT=TOTIM-(1.-FHBXWT(5+NX))*DELT
      DO 320 L=2,NBDTIM
      IF(TT.LE.BDTIM(L)) THEN
         IB1=L-1
         IB2=L
         GO TO 340
      ENDIF
 320  CONTINUE
      IB1=NBDTIM-1
      IB2=NBDTIM
 340  XFACT=(TT-BDTIM(IB1))/(BDTIM(IB2)-BDTIM(IB1))
      DO 350 NF=1,NHED
      XX=SBHED(N2+IB1,NF)+XFACT*(SBHED(N2+IB2,NF)-SBHED(N2+IB1,NF))
      BDHV(NX,NF)=XX
 350  CONTINUE
 390  CONTINUE
C
C11------RETURN
      RETURN
      END
      SUBROUTINE GWF2FHB7FM(IGRID)
C     ******************************************************************
C     SUBTRACT SPECIFIED Q FROM RHS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IBOUND,RHS
      USE GWFFHBMODULE,ONLY:NFLW,IFLLOC,BDFV
C     ------------------------------------------------------------------
      CALL SGWF2FHB7PNT(IGRID)
C
C1------PROCESS EACH SPECIFIED-FLOW LOCATION IN THE LIST.
      IF(NFLW.LE.0) RETURN
      DO 100 L=1,NFLW
      IR=IFLLOC(2,L)
      IC=IFLLOC(3,L)
      IL=IFLLOC(1,L)
      Q=BDFV(1,L)
C
C1A-----IF THE CELL IS INACTIVE THEN BYPASS PROCESSING.
      IF(IBOUND(IC,IR,IL).LE.0) GO TO 100
C
C1B-----IF THE CELL IS VARIABLE HEAD THEN SUBTRACT Q FROM
C       THE RHS ACCUMULATOR.
      RHS(IC,IR,IL)=RHS(IC,IR,IL)-Q
  100 CONTINUE
C
C2------RETURN
      RETURN
      END
      SUBROUTINE GWF2FHB7BD(KSTP,KPER,IGRID)
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR SPECIFIED FLOWS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,BUFF
      USE GWFBASMODULE,ONLY:MSUM,VBVL,VBNM,ICBCFL,DELT,PERTIM,TOTIM,
     1                      IAUXSV
      USE GWFFHBMODULE,ONLY:NBDTIM,NFLW,NHED,IFHBCB,NFHBX1,NFHBX2,
     1                      IFHBSS,IFLLOC,IHDLOC,BDTIM,FLWRAT,SBHED,
     2                      BDFV,BDHV,FHBXWT,FHBXNM
C
      CHARACTER*16 TEXT
      DOUBLE PRECISION RATIN,RATOUT,QQ
C
      DIMENSION XFACE(1)
      DATA TEXT/' SPECIFIED FLOWS'/
C     ------------------------------------------------------------------
      CALL SGWF2FHB7PNT(IGRID)
C
C1------CLEAR RATIN AND RATOUT ACCUMULATORS.
      ZERO=0.
      RATIN=ZERO
      RATOUT=ZERO
      IBD=0
      IF(IFHBCB.LT.0 .AND. ICBCFL.NE.0) IBD=-1
      IF(IFHBCB.GT.0) IBD=ICBCFL
C
C2A----IF CELL-BY-CELL FLOWS WILL BE SAVED AS A LIST, WRITE HEADER.
      IF(IBD.EQ.2) THEN
        NAUX=1
        IF(IAUXSV.EQ.0) NAUX=0
        CALL UBDSV4(KSTP,KPER,TEXT,NAUX,'IFACE           ',IFHBCB,
     1          NCOL,NROW,NLAY,NFLW,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      END IF
C
C2B----CLEAR THE BUFFER.
      DO 50 IL=1,NLAY
      DO 50 IR=1,NROW
      DO 50 IC=1,NCOL
      BUFF(IC,IR,IL)=ZERO
   50 CONTINUE
C
C3A----IF THERE ARE NO SPECIFIED-FLOW CELLS, DO NOT ACCUMULATE FLOW
      IF(NFLW.EQ.0) GO TO 200
C
C3B-----PROCESS SPECIFIED-FLOW CELLS ONE AT A TIME.
   60 DO 100 L=1,NFLW
C
C3C-----GET LAYER, ROW, AND COLUMN NUMBERS
      IR=IFLLOC(2,L)
      IC=IFLLOC(3,L)
      IL=IFLLOC(1,L)
      Q=ZERO
C
C3D-----IF THE CELL IS NO-FLOW OR CONSTANT-HEAD, IGNORE IT.
      IF(IBOUND(IC,IR,IL).LE.0)GO TO 97
C
C4A-----GET FLOW RATE FROM SPECIFIED-FLOW LIST
      Q=BDFV(1,L)
      QQ=Q
C
C4B-----PRINT THE INDIVIDUAL RATES IF REQUESTED(IFHBCB<0).
      IF(IBD.LT.0) THEN
        WRITE(IOUT,900) TEXT,KPER,KSTP,L,IL,IR,IC,Q
  900   FORMAT(1H0,4A4,'   PERIOD',I3,'   STEP',I3,' SEQ NO',I4,
     1    '   LAYER',I3,'   ROW ',I4,'   COL',I4,'   RATE',G15.7)
      ENDIF
C
C4C-----ADD FLOW RATE TO BUFFER.
      BUFF(IC,IR,IL)=BUFF(IC,IR,IL)+Q
C
C5A-----SEE IF FLOW RATE IS NEGATIVE, ZERO, OR POSITIVE.
      IF(Q.GE.ZERO) THEN
C
C5B-----FLOW RATE IS POSITIVE (RECHARGE). ADD IT TO RATIN.
        RATIN=RATIN+QQ
      ELSE
C
C5C-----FLOW RATE IS NEGATIVE(DISCHARGE). ADD IT TO RATOUT.
        RATOUT=RATOUT-QQ
      END IF
C
C6------IF CELL-BY-CELL FLOWS ARE BEING SAVED AS A LIST, WRITE FLOW.
C6------RETURN THE ACTUAL FLOW IN THE BDFV ARRAY.
   97 IF(IBD.EQ.2) THEN
        XFACE(1)=IFLLOC(4,L)
        CALL UBDSVB(IFHBCB,NCOL,NROW,IC,IR,IL,Q,
     1                    XFACE,1,NAUX,1,IBOUND,NLAY)
      END IF
      BDFV(2,L)=Q
  100 CONTINUE
C
C7------IF CELL-BY-CELL FLOWS WILL BE SAVED AS A 3-D ARRAY,
C7------CALL UBUDSV TO SAVE THEM
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,IFHBCB,BUFF,NCOL,NROW,
     1                          NLAY,IOUT)
C
C8------MOVE RATES, VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
 200  RIN=RATIN
      ROUT=RATOUT
      VBVL(3,MSUM)=RIN
      VBVL(4,MSUM)=ROUT
      VBVL(1,MSUM)=VBVL(1,MSUM)+RIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
      VBNM(MSUM)=TEXT
C
C9------INCREMENT BUDGET TERM COUNTER (MSUM).
      MSUM=MSUM+1
C
C10-----RETURN
      RETURN
      END
      SUBROUTINE GWF2FHB7DA(IGRID)
C  Deallocate FHB DATA
      USE GWFFHBMODULE
C
        DEALLOCATE(GWFFHBDAT(IGRID)%NBDTIM)
        DEALLOCATE(GWFFHBDAT(IGRID)%NFLW)
        DEALLOCATE(GWFFHBDAT(IGRID)%NHED)
        DEALLOCATE(GWFFHBDAT(IGRID)%IFHBCB)
        DEALLOCATE(GWFFHBDAT(IGRID)%NFHBX1)
        DEALLOCATE(GWFFHBDAT(IGRID)%NFHBX2)
        DEALLOCATE(GWFFHBDAT(IGRID)%IFHBSS)
        DEALLOCATE(GWFFHBDAT(IGRID)%FHBXWT)
        DEALLOCATE(GWFFHBDAT(IGRID)%FHBXNM)
        DEALLOCATE(GWFFHBDAT(IGRID)%BDTIM)
        DEALLOCATE(GWFFHBDAT(IGRID)%FLWRAT)
        DEALLOCATE(GWFFHBDAT(IGRID)%BDFV)
        DEALLOCATE(GWFFHBDAT(IGRID)%IFLLOC)
        DEALLOCATE(GWFFHBDAT(IGRID)%SBHED)
        DEALLOCATE(GWFFHBDAT(IGRID)%BDHV)
        DEALLOCATE(GWFFHBDAT(IGRID)%IHDLOC)
C
      RETURN
      END
      SUBROUTINE SGWF2FHB7PNT(IGRID)
C  Set pointers to FHB data for grid.
      USE GWFFHBMODULE
C
        NBDTIM=>GWFFHBDAT(IGRID)%NBDTIM
        NFLW=>GWFFHBDAT(IGRID)%NFLW
        NHED=>GWFFHBDAT(IGRID)%NHED
        IFHBCB=>GWFFHBDAT(IGRID)%IFHBCB
        NFHBX1=>GWFFHBDAT(IGRID)%NFHBX1
        NFHBX2=>GWFFHBDAT(IGRID)%NFHBX2
        IFHBSS=>GWFFHBDAT(IGRID)%IFHBSS
        FHBXWT=>GWFFHBDAT(IGRID)%FHBXWT
        FHBXNM=>GWFFHBDAT(IGRID)%FHBXNM
        BDTIM=>GWFFHBDAT(IGRID)%BDTIM
        FLWRAT=>GWFFHBDAT(IGRID)%FLWRAT
        BDFV=>GWFFHBDAT(IGRID)%BDFV
        IFLLOC=>GWFFHBDAT(IGRID)%IFLLOC
        SBHED=>GWFFHBDAT(IGRID)%SBHED
        BDHV=>GWFFHBDAT(IGRID)%BDHV
        IHDLOC=>GWFFHBDAT(IGRID)%IHDLOC
C
      RETURN
      END
      SUBROUTINE SGWF2FHB7PSV(IGRID)
C  Save pointers to FHB data for grid.
      USE GWFFHBMODULE
C
        GWFFHBDAT(IGRID)%NBDTIM=>NBDTIM
        GWFFHBDAT(IGRID)%NFLW=>NFLW
        GWFFHBDAT(IGRID)%NHED=>NHED
        GWFFHBDAT(IGRID)%IFHBCB=>IFHBCB
        GWFFHBDAT(IGRID)%NFHBX1=>NFHBX1
        GWFFHBDAT(IGRID)%NFHBX2=>NFHBX2
        GWFFHBDAT(IGRID)%IFHBSS=>IFHBSS
        GWFFHBDAT(IGRID)%FHBXWT=>FHBXWT
        GWFFHBDAT(IGRID)%FHBXNM=>FHBXNM
        GWFFHBDAT(IGRID)%BDTIM=>BDTIM
        GWFFHBDAT(IGRID)%FLWRAT=>FLWRAT
        GWFFHBDAT(IGRID)%BDFV=>BDFV
        GWFFHBDAT(IGRID)%IFLLOC=>IFLLOC
        GWFFHBDAT(IGRID)%SBHED=>SBHED
        GWFFHBDAT(IGRID)%BDHV=>BDHV
        GWFFHBDAT(IGRID)%IHDLOC=>IHDLOC
C
      RETURN
      END