c
c----------------------------------------------------------------------X
c
      SUBROUTINE COPWIN(INDAT,IRATYP,ICORD,XMIN,XMAX,YMIN,YMAX,
     X                  GXMIN,GXMAX,GYMIN,GYMAX,ANGTOL,FXMN,FXMX,ICVRT,
     X                  X0,Y0,AZCOR,IARCS,IAZC,AZMIN,AZMAX,AZROT,ITPFLG)
C
C  SET PLOTTING WINDOW PARAMETERS FOR CONSTANT COPLANE ANGLE SCANS
C     COVERING AN AZIMUTH SECTOR .LT. 360 DEG (COP - SCAN TYPE INDEX = 2)
C
C     GXMIN,MAX - MINIMUM AND MAXIMUM USER X-DISTANCE (KM) FOR PLOTTING
C     GYMIN,MAX -    "     "     "      "  Y    "       "   "     "
C     FXMN,FXMX -    "     "     "    FIXED ANGLE TO BE PLOTTED
C     XMIN,XMAX -    "     "     "    X-DISTANCE (KM) FROM RADAR
C     YMIN,YMAX -    "     "     "    Y-   "       "    "    "
C         ICVRT - FALSE, WINDOW RELATIVE TO RADAR
C                 TRUE,     "       "     " EXP. ORIGIN OR ANOTHER RADAR
C
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
         ITPFLG(2)=0
         RETURN
      ELSE
         ITPFLG(2)=1
      END IF
      READ(INDAT,50)XMIN(2),XMAX(2),YMIN(2),YMAX(2),D1,D2,D3,RARCS,
     +              AZROT(2)
50    FORMAT(/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0)
      IF(INDAT(6).NE.'        ')THEN
         FXMN(2)=D1-EPS
      ELSE
         FXMN(2)=0.
      END IF
      IF(INDAT(7).NE.'        ')THEN
         FXMX(2)=D2+EPS
      ELSE
         FXMX(2)=90.
      END IF
      IF(INDAT(8).NE.'        ')THEN
         ANGTOL(2)=D3
      ELSE
         ANGTOL(2)=0.5
      END IF
      IARCS(2)=RARCS
      IF(IRATYP.EQ.ICORD.OR.(X0.EQ.0..AND.Y0.EQ.0.))THEN
         ICVRT=.FALSE.
         IORIGIN='AT'
         GXMIN(2)=XMIN(2)
         GXMAX(2)=XMAX(2)
         GYMIN(2)=YMIN(2)
         GYMAX(2)=YMAX(2)
      ELSE
         ICVRT=.TRUE.
         IORIGIN='NOT AT'
         GXMIN(2)=XMIN(2)
         GXMAX(2)=XMAX(2)
         GYMIN(2)=YMIN(2)
         GYMAX(2)=YMAX(2)
         COR(1)=XMIN(2)
         COR(2)=XMAX(2)
         COR(3)=YMIN(2)
         COR(4)=YMAX(2)
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
         XMIN(2)=AMIN1(EW(1),EW(2),EW(3),EW(4))
         XMAX(2)=AMAX1(EW(1),EW(2),EW(3),EW(4))
         YMIN(2)=AMIN1(NS(1),NS(2),NS(3),NS(4))
         YMAX(2)=AMAX1(NS(1),NS(2),NS(3),NS(4))
      END IF
      CALL AZLIM(XMIN(2),XMAX(2),YMIN(2),YMAX(2),AZMIN(2),AZMAX(2),
     +     IAZC)
      PRINT 200,GXMIN(2),GXMAX(2),GYMIN(2),GYMAX(2),FXMN(2),FXMX(2),
     +     ANGTOL(2),AZROT(2),AZCOR,IARCS(2),ICORD,IORIGIN,IRATYP
 200  FORMAT(/1X,'COPWIN:                XMIN,XMAX,YMIN,YMAX-',4F7.1,
     +       /1X,'        FXMN,FXMX,ANGTOL,AZROT,AZCOR,IARCS-',5F7.1,I7,
     +       /1X,'ORIGIN: ',A8,A8,' RADAR NAME: ',A8)
      IF(GXMIN(2).GE.GXMAX(2) .OR. 
     +   GYMIN(2).GE.GYMAX(2) .OR.
     +   FXMN(2) .GT.FXMX(2))THEN
         WRITE(6,299)
 299     FORMAT(1X,'****ILL-DEFINED PLOT WINDOW****')
         STOP
      END IF
      RETURN
      END
