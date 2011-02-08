c
c----------------------------------------------------------------------X
c
      SUBROUTINE LABCOL_NON (CL, NL,IGRPLT)
C
C  DRAW AND LABEL THE COLOR BAR ALONG THE RIGHT HAND SIDE OF THE PLOT
C     COLOR BAR EXTENDS THE VERTICAL LENGTH OF THE PLOT WINDOW
C
      CHARACTER*6 LAB
      COMMON /CONTOUR/ LMX,LMN
      COMMON /COTABLE/ ICOL(100)
      COMMON /PLTWIN/XRT(26),YTP(26),SIDEX(26),SIDEY(26),DXL,DXR,DYB,
     + DYT,NROW,NCOL,NWIN,IWIN,LABX(26),LABY(26),ILFLG,XBTP(26),
     + YBTP(26),SIZBX,XSHIFT,YSHIFT
      DIMENSION CL(NL),XPT(5),YPT(5)
      DATA SPMX/0.04/

C     Corners of color bar
C                3
C               2 4
C                1
C
c      XPT(2) = XRT(IWIN)+0.005
c      XPT(4) = XRT(IWIN)+0.035
      XPT(2) = XRT(IWIN)+0.002
      XPT(4) = XRT(IWIN)+0.034
      XPT(1) = 0.5*(XPT(2)+XPT(4))
      XPT(3) = 0.5*(XPT(2)+XPT(4))
      SPACE=(DYT-DYB)/NL
      IF(SPACE.GT.SPMX)SPACE=SPMX
      ISKP=NL/20+1
      Y1=DYT
      IF(LMX.LT.LMN)LMX=LMN
      IF(LMN.GT.NL)LMN=NL
      IF(LMX.GT.NL)LMX=NL
      DO 50 K=LMN,LMX
         FLTCL=CL(K)-INT(CL(K))
         IF(FLTCL.EQ.0.0)THEN
            WRITE (LAB, 11)INT(CL(K))
   11       FORMAT(I6)
            XOFF=0.015
         ELSE
            WRITE (LAB, 13)CL(K)
   13       FORMAT(F6.1)
            XOFF=0.040
c            XOFF=0.025
         END IF
         YPT(3) = Y1
         YPT(1) = Y1 - SPACE
         YPT(2) = 0.5*(YPT(1)+YPT(3))
         YPT(4) = 0.5*(YPT(1)+YPT(3))
         CALL FAREA (ICOL(K),XPT, YPT, 5)

C*****DRAW PERIMETER AROUND COLOR BAR
C
         CALL CURVE (XPT,YPT,5)

         IF(MOD(ISKP+K-1,ISKP).EQ.0)THEN
c            CALL PLCHMQ(XPT(4)+XOFF,YPT(3),LAB,12.,0.0,1.0)
            CALL PLCHMQ(XPT(4)+XOFF,YPT(3),LAB,10.,0.0,1.0)
         END IF
         Y1 = Y1 -SPACE
   50 CONTINUE
      RETURN
      END
