c
c----------------------------------------------------------------------X
c
      SUBROUTINE SETGRAY(INDAT,GRAYTYP,GSTR,GRAYEST,PLTABL,NFRAME)

C  Sets parameters for grayscale colors
C
C     GRAYTYP - The rate of decrease of RGB mix from 1-->0
C               'LIN ','QUAD','SQRT','LOG ',or 'COS '
C     GSTR    - Controls the lightest gray (White, GSTR = 1)
C     GRAYEST -     "     "  darkest    "  (Black, GRAY = 0)
C
      PARAMETER (NCRV=61)

      CHARACTER*8 INDAT(10)
      CHARACTER*6 IFMTX,IFMTY
      CHARACTER GRAYTYP*4,LABLS*3,LABX*11,LABY*8,LABT*16,PLOT*4,PLTABL*6
      CHARACTER*1 BGFLAG

      DATA PI2,PI2I/1.570796,0.6366198/
      DATA FL,FR,FB,FT/0.15,0.95,0.1,0.9/
      DATA UL,UR,UB,UT/0.0,70.0,0.0,1.2/

      DIMENSION XCRV(NCRV),YCRV(NCRV),TCRV(NCRV)

      WRITE(6,11)(INDAT(I),I=2,10)
 11   FORMAT(1X,'SETGRAY: ',9A8)
      READ(INDAT,13)GRAYTYP,GSTR,GRAYEST,PLOT,PLTABL
 13   FORMAT(/A4,4X/F8.0/F8.0/A4,4X/A6)
      IF(GRAYTYP.NE.'LIN '.AND.
     +   GRAYTYP.NE.'QUAD'.AND.
     +   GRAYTYP.NE.'SQRT'.AND.
     +   GRAYTYP.NE.'LOG '.AND.
     +   GRAYTYP.NE.'COS ')GRAYTYP='LIN '
      IF(GSTR.GT.1.0)GSTR=1.0
      IF(GSTR.LT.0.5)GSTR=0.5
      IF(GRAYEST.LT.0.0)GRAYEST=0.0
      IF(GRAYEST.GT.0.5)GRAYEST=0.5
      write(*,*)graytyp,gstr,grayest,plot,'    ',pltabl

C  SET GRAY COLOR TABLE - WHITE BACKGROUND, BLACK FOREGROUND
C     Indices 2-62
C     
      DARKEST=GRAYEST/GSTR
      ANORM = ALOG10(61.0)
      FRLIN  = 1.0 - DARKEST
      FRQUAD = SQRT(FRLIN)
      FRSQRT = FRLIN**2.0
      FRCOS  = PI2I*ACOS(DARKEST)
      FRLOG  = (63.0-10.0**(ANORM*DARKEST))/62.0
      
      DO 20 IC=2,62
            
         XIC=(IC-2.0)/60.0
         
         IF(GRAYTYP.EQ.'LIN ')THEN
            GRAY = 1.0 - FRLIN*XIC
         ELSE IF(GRAYTYP.EQ.'QUAD')THEN
            GRAY = 1.0 - (FRQUAD*XIC)**2.0
         ELSE IF(GRAYTYP.EQ.'SQRT')THEN
            GRAY = 1.0 - (FRSQRT*XIC)**0.5
         ELSE IF (GRAYTYP.EQ.'LOG ')THEN
            GRAY = ALOG10(63.0-FRLOG*IC)/ANORM
         ELSE IF (GRAYTYP.EQ.'COS ')THEN
            GRAY = COS(PI2*FRCOS*XIC)
         END IF
         IF(GRAY.LT.0.0)GRAY=0.0
         
         XCRV(IC-1) = FLOAT(IC)
         YCRV(IC-1) = GSTR*GRAY
         TCRV(IC-1) = XIC
         
 20   CONTINUE

      IF(PLOT.NE.'PLOT')RETURN

C  Plot grayness (YCRV) as a function of color index (XCRV)
C
      CALL SFLUSH
      CALL MAJMIN(UL,UR,IFMTX,MJRX,MNRX,IPLX)
      CALL MAJMIN(UB,UT,IFMTY,MJRY,MNRY,IPLY)
      CALL SET(FL,FR,FB,FT,UL,UR,UB,UT,1)
      CALL LABMOD(IFMTX,IFMTY,IPLX,IPLY,12,12,8,8,0)
      CALL PERIML(MJRX,MNRX,MJRY,MNRY)

      DO 30 I=1,NCRV
         XP1=XCRV(I)
         YP1=YCRV(I)
c         XP2=XCRV(I+1)
c         YP2=YCRV(I+1)
c         CALL LINE(XP1,YP1,XP2,YP2)
         CALL PLCHMQ(XP1,YP1,'x',10.0,0.0,0.0)
c         XP1=XCRV(I)
c         YP1=TCRV(I)
c         CALL PLCHMQ(XP1,YP1,'x',10.0,0.0,0.0)
 30   CONTINUE
      CALL DASHDB (O'170360')
      CALL LINED(UL,GSTR,UR,GSTR)
      CALL LINED(UL,GRAYEST,UR,GRAYEST)

      XP1=XCRV(1)
      YP1=YCRV(1)
      CALL PLCHMQ(XP1,YP1,'-----LIGHTEST',10.0,90.0,-1.0)
      XP1=XCRV(NCRV)
      YP1=YCRV(NCRV)
      CALL PLCHMQ(XP1,YP1,'------DARKEST',10.0,90.0,-1.0)

      LABX='COLOR INDEX'
      LABY='GRAYNESS'
      WRITE(LABT,31)GRAYTYP
 31   FORMAT('GRAYTONES (',A4,')')
      CALL SET (0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,0)
      CALL PLCHMQ(0.55,0.95,LABT,20.0,0.0,0.0)
      CALL PLCHMQ(0.55,0.05,LABX,15.0,0.0,0.0)
      CALL PLCHMQ(0.05,0.5,LABY,15.0,90.0,0.0)

      NPLT=0
      FYB=0.085
      LABLS='ALL'
      CALL MYFRAME(NFRAME,NPLT,FYB,LABLS)

C     Plot color table frames - set PLTABL = 'PLTABL'
C
      write(*,*)'tstcol: ',pltabl,gstr,grayest,graytyp
      IF(PLTABL.EQ.'PLTABL')THEN
         IGRPLT=0
         BGFLAG='B'
         CALL SETCOL(IGRPLT,BGFLAG,GRAYTYP,GSTR,GRAYEST)
         CALL TSTCOL(GRAYTYP,GSTR,GRAYEST,NFRAME,LABLS)
      END IF

      RETURN
      END

