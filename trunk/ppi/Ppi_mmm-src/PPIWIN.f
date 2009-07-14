c
c----------------------------------------------------------------------X
c
      SUBROUTINE PPIWIN(INDAT,IRATYP,ICORD,XMIN,XMAX,YMIN,YMAX,
     X                  GXMIN,GXMAX,GYMIN,GYMAX,ANGTOL,FXMN,FXMX,ICVRT,
     X                  X0,Y0,AZCOR,IARCS,IAZC,AZMIN,AZMAX,AZROT,ITPFLG)
C
C  SET PLOTTING WINDOW PARAMETERS FOR CONSTANT ELEVATION ANGLE SCANS
C     COVERING AN AZIMUTH SECTOR .LT. 360 DEG (PPI - SCAN TYPE INDEX = 1)
C
C     GXMIN,MAX - MINIMUM AND MAXIMUM USER X-DISTANCE (KM) FOR PLOTTING
C     GYMIN,MAX -    "     "     "      "  Y    "       "   "     "
C     FXMN,FXMX -    "     "     "    FIXED ANGLE TO BE PLOTTED
C     XMIN,XMAX -    "     "     "    X-DISTANCE (KM) FROM RADAR
C     YMIN,YMAX -    "     "     "    Y-   "       "    "    "
C         ICVRT - FALSE, WINDOW RELATIVE TO RADAR
C                 TRUE,     "       "     " EXP. ORIGIN OR ANOTHER RADAR
C     ANGTOL    - ANGLE TOLERANCE; IF ABS(FIXED-ACTUAL) ANGLE .GT. ANGTOL
C                 THIS BEAM IS NOT STORED (SEE ROUTINES RDFF AND RDUF)
C     IARCS     - NUMBER OF CONSTANT HEIGHT LINES TO BE DRAWN ON THE PLOT
C     AZROT     - ORIGINAL ANGLES ARE ROTATED THIS AMOUNT (NEW=OLD+AZROT)
C
C     XMIN      - IF INDAT(2) EQUALS 'TURN OFF', ITPFLG=0 AND
C                 THIS TYPE OF SCAN WILL NO LONGER BE PLOTTED
C
      DIMENSION XMIN(8),XMAX(8),YMIN(8),YMAX(8),GXMIN(8),GXMAX(8),
     +GYMIN(8),GYMAX(8),ANGTOL(8),FXMN(8),FXMX(8),IARCS(8),
     +AZMIN(8),AZMAX(8),AZROT(8),ITPFLG(8)
      CHARACTER*8 INDAT(10)
      CHARACTER*8 IRATYP,ICORD,IORIGIN
      REAL NSMIN,NSMAX,NS
      DIMENSION COR(4),EW(4),NS(4)
      LOGICAL ICVRT,COLRFIL,IAZC
      DATA EPS/0.01/

      IF(INDAT(2).EQ.'TURN OFF')THEN
         ITPFLG(1)=0
         RETURN
      ELSE
         ITPFLG(1)=1
      END IF
      READ(INDAT,50)XMIN(1),XMAX(1),YMIN(1),YMAX(1),D1,D2,D3,RARCS,
     +              AZROT(1)
50    FORMAT(/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0)
      IF(INDAT(6).NE.'        ')THEN
         FXMN(1)=D1-EPS
      ELSE
         FXMN(1)=0.
      END IF
      IF(INDAT(7).NE.'        ')THEN
         FXMX(1)=D2+EPS
      ELSE
         FXMX(1)=90.
      END IF
      IF(INDAT(8).NE.'        ')THEN
         ANGTOL(1)=D3
      ELSE
         ANGTOL(1)=0.5
      END IF
      IARCS(1)=RARCS
      IF(IRATYP.EQ.ICORD.OR.(X0.EQ.0..AND.Y0.EQ.0.))THEN
         ICVRT=.FALSE.
         IORIGIN='AT'
         GXMIN(1)=XMIN(1)
         GXMAX(1)=XMAX(1)
         GYMIN(1)=YMIN(1)
         GYMAX(1)=YMAX(1)
      ELSE
         ICVRT=.TRUE.
         IORIGIN='NOT AT'
         GXMIN(1)=XMIN(1)
         GXMAX(1)=XMAX(1)
         GYMIN(1)=YMIN(1)
         GYMAX(1)=YMAX(1)
         COR(1)=XMIN(1)
         COR(2)=XMAX(1)
         COR(3)=YMIN(1)
         COR(4)=YMAX(1)
         K=0
         DO 60 I=1,2
         DO 60 J=3,4
         K=K+1
         THETA=ATAN2(COR(I)-X0,COR(J)-Y0)+AZCOR
         IF(THETA.EQ.AZCOR) GO TO 57
         R=(COR(I)-X0)/SIN(THETA-AZCOR)
         GO TO 58
   57    R=(COR(J)-Y0)/COS(THETA-AZCOR)
   58    EW(K)=R*SIN(THETA)
         NS(K)=R*COS(THETA)
   60    CONTINUE
         XMIN(1)=AMIN1(EW(1),EW(2),EW(3),EW(4))
         XMAX(1)=AMAX1(EW(1),EW(2),EW(3),EW(4))
         YMIN(1)=AMIN1(NS(1),NS(2),NS(3),NS(4))
         YMAX(1)=AMAX1(NS(1),NS(2),NS(3),NS(4))
      END IF
      CALL AZLIM(XMIN(1),XMAX(1),YMIN(1),YMAX(1),AZMIN(1),AZMAX(1),
     +     IAZC)
      PRINT 200,GXMIN(1),GXMAX(1),GYMIN(1),GYMAX(1),FXMN(1),FXMX(1),
     +     ANGTOL(1),AZROT(1),AZCOR,IARCS(1),ICORD,IORIGIN,IRATYP
 200  FORMAT(/1X,'PPIWIN:                XMIN,XMAX,YMIN,YMAX-',4F7.1,
     +       /1X,'        FXMN,FXMX,ANGTOL,AZROT,AZCOR,IARCS-',5F7.1,I7,
     +       /1X,'ORIGIN: ',A8,A8,' RADAR NAME: ',A8)
      IF(GXMIN(1).GE.GXMAX(1) .OR. 
     +   GYMIN(1).GE.GYMAX(1) .OR.
     +   FXMN(1) .GT.FXMX(1))THEN
         WRITE(6,299)
 299     FORMAT(1X,'****ILL-DEFINED PLOT WINDOW****')
         STOP
      END IF
      RETURN
      END
