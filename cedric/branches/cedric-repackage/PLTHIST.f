      SUBROUTINE PLTHIST(IBINS, NBINS, XMIN, XMAX, NPPTS, TPERC, HMIN,
     X     HINC, ICOL, IYAXIS, MED, IMED)
C
C     PLOTS HISTO BARS
C
C     IBINS--  ARRAY CONTAINING COUNTS FOR EACH BIN
C     NBINS--  NUMBER OF BINS
C     CLASS--  ARRAY CONTAINING MIDPOINT VALUES OF EACH BIN
C     NPPTS--  TOTAL NUMBER OF POINTS IN DATA SET FOR WHICH HISTO
C              IS TO BE PLOTTED (THIS COULD BE A 2-D SLICE OR
C              THE WHOLE VOLUME)
C     IYAXIS-- 0 ==> LINEAR, 1==> LOG
C     TPERC--  User-specified maximum for linear vertical axis
C
C     Note: Use CALL PLCHMQ when (X,Y) location is already fractional
C           since CPUX and CPUY are obsolete. (LJM - Jul 30, 2012).
C           Use CALL MY_PLCHMQ when (X,Y) location is PAUs.
C
      PARAMETER (MAXBIN=1003)
      DIMENSION IBINS(MAXBIN), CLASS(MAXBIN)
      DIMENSION XP(4),YP(4)
      REAL MED
      CHARACTER*8  IFMTX,IFMTY
      CHARACTER*16 CFMTX,CFMTY
      CHARACTER*80 LABEL
      LOCPLT(R)=1023.*R

C     SETUP GRID BOUNDARIES IN FRACTIONAL COORDINATES
      XL=.0625
      XR=.8625
      YB=.0625
      YT=.8025
      IF (IYAXIS.EQ.0) THEN
         YMIN=0.0
         YMAX=TPERC
         NDIG1=0
         NDIG2=0
      ELSE
         YMIN=0.1
         YMAX=100.
         NDIG1=0
         NDIG2=1
      END IF
c-----print *,'PLTHIST: iyaxis,tperc,ymin,ymax=',iyaxis,tperc,ymin,ymax
      
      CALL SET(XL,XR,YB,YT,XMIN,XMAX,YMIN,YMAX,(IYAXIS+1))
      CALL GSCLIP (1)

      IF (NPPTS.GT.0) THEN

C     DRAW ALL HISTO BARS
         DO I=1,NBINS
            PERC=IBINS(I)/FLOAT(NPPTS)*100.
            XPOS1=HMIN+HINC*(I-1) - HINC/2.
            XPOS2=HMIN+HINC*(I-1) + HINC/2.
            IF (IYAXIS.EQ.0) THEN
               YPOS1=0.0
               YPOS2=PERC
            ELSE
               YPOS1=0.1
               IF (PERC.NE.0.0) THEN
                  YPOS2=PERC
               ELSE
                  YPOS2=0.1
               END IF
            END IF
            CALL LINE (XPOS1,YPOS1,XPOS1,YPOS2)
            CALL LINE (XPOS1,YPOS2,XPOS2,YPOS2)
            CALL LINE (XPOS2,YPOS2,XPOS2,YPOS1)
            IF (YPOS2.GT.TPERC) THEN
               CALL PLCHMQ(XPOS1,TPERC,'**',12.,0.,-1.)
            END IF
            IF (ICOL.NE.-1) THEN
               XP(1)=XPOS1
               YP(1)=YPOS1
               XP(2)=XPOS1
               YP(2)=YPOS2
               XP(3)=XPOS2
               YP(3)=YPOS2
               XP(4)=XPOS2
               YP(4)=YPOS1
               IF (IYAXIS.EQ.1) THEN
                  YP(1)=ALOG10(YP(1))
                  YP(2)=ALOG10(YP(2))
                  YP(3)=ALOG10(YP(3))
                  YP(4)=ALOG10(YP(4))
               END IF
               CALL GSFACI(ICOL)
               CALL GFA(4,XP,YP)
            END IF
         END DO
      END IF
         
C
C     DRAW GRID BOX
C
      CALL MAJMIN(XMIN,(XMAX-XMIN),IFMTX,MAJORX,MINORX,NDIG1,ISZ1,
     .            YMIN,(YMAX-YMIN),IFMTY,MAJORY,MINORY,NDIG2,ISZ2)
      WRITE (CFMTX,100)IFMTX
 100  FORMAT(2A8)
      WRITE (CFMTY,100)IFMTY
      IF (IYAXIS.EQ.1) THEN
         MAJORY=1
         MINORY=8
      END IF

      CALL LABMOD (CFMTX,CFMTY,NDIG1,NDIG2,ISZ1,ISZ2,4,4,0)
      CALL PERIML (MAJORX,MINORX,MAJORY,MINORY)
C
C     PUT ON ASCISSA AND ORDINATE LABELS
C
      CALL GSCLIP (0)

      IY=LOCPLT(YB-(YT-YB)*.05)
      LABEL='CLASS MIDVALUES'
      CALL MY_PLCHMQ(370,IY,LABEL,12.,0.,-1.)
      IY=LOCPLT(YB+(YT-YB)*.25)
      LABEL='PERCENT OCCURRENCE'
      CALL MY_PLCHMQ(15,IY,LABEL,12.,90.,-1.)

C     Replaced CALL GPL with CALL LINE between two points.
C     and labeled the median line (CALL PLCHMQ).  
C     (LJM - July 30, 2012).
C
      IF (IMED.EQ.1) THEN
         XP(1)=MED
         XP(2)=XP(1)
         YP(1)=YMIN
         YP(2)=0.95*YMAX
         CALL LINE (XP(1),YP(1),XP(2),YP(2))
         YP(2)=0.96*YMAX
         CALL PLCHMQ(XP(1),YP(2),'MEDIAN',12.,0.,0.)
      END IF

      RETURN

      END


