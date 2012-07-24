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
C
      PARAMETER (MAXBIN=1003)
      DIMENSION IBINS(MAXBIN), CLASS(MAXBIN)
      DIMENSION XP(4),YP(4)
      REAL MED
      CHARACTER*8  IFMTX,IFMTY
      CHARACTER*16 CFMTX,CFMTY

C     SETUP GRID BOUNDARIES IN FRACTIONAL COORDINATES
C
      XL=.0625
      XR=.8625
      YB=.0625
      YT=.8025
      XSIDE=XR-XL
      YSIDE=YT-YB
      IF (IYAXIS.EQ.0) THEN
         YMIN=0.0
         YMAX=TPERC
      ELSE
         YMIN=0.1
         YMAX=100.
      END IF
      
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
c     Debug jul 11, 2012 PLOTS
            print *,'PLTHIST: xpos1,ypos2,tperc=',xpos1,ypos2,tperc
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

      IF (IMED.EQ.1) THEN
C     DRAW MEDIAN LINE
         XP(1)=MED
         XP(2)=XP(1)
         YP(1)=0.
         YP(2)=TPERC-TPERC/10.
         CALL GPL(2,XP,YP)
C     LABEL MEDIAN LINE
         CALL PLCHMQ(XP(1),(YP(2)-YP(2)/10.),'MEDIAN',12.,90.,0.)
      END IF

C
C     PUT ON ASCISSA AND ORDINATE LABELS
C
C     Input to PLTCHMQ must be reals; CPUX and CPUY are obsolete
C     Converted all [CPUX(IX),CPUY(IY)] to [RX,RY] so that CALL
C     PLCHMQ uses fractional coordinates for labels (LJM - 07/23/2012)
C
      LL=1
      CALL GETSET(FL,FR,FB,FT,UL,UR,UB,UT,LL)
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)

c     Debug jul 11, 2012 PLOTS
      print *,'PLTHIST: fl,fr,fb,ft=',fl,fr,fb,ft

      CALL GSCLIP (0)
      RX=FL+0.5*XSIDE
      RY=FB-0.045
      CALL PLCHMQ(RX,RY,'CLASS MIDVALUES',12.,0.,0.)
      RX=FL-0.045
      RY=FB+0.5*YSIDE
      CALL PLCHMQ(RX,RY,'PERCENT OCCURRENCE',12.,90.,0.)
      CALL SET(FL,FR,FB,FT,UL,UR,UB,UT,1)

      RETURN

      END


