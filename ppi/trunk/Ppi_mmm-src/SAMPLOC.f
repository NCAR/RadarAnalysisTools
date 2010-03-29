c
c----------------------------------------------------------------------X
c
      SUBROUTINE SAMPLOC(ICOLTYP,DIGCOLR,DIGSIZE)

C     Plot POINT or digitize field values at range-angle locations.
C        Digitized values = True values - nint(digoff)
C
      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'
      INCLUDE 'input.inc'
      INCLUDE 'swth.inc'
      INCLUDE 'colors.inc'

      COMMON /INPUTCH/NAMFLD(MXF),IRATYP,ICORD
      CHARACTER*8 NAMFLD,IRATYP,ICORD
      COMMON /ORIGINCH/NETWORK
      CHARACTER*8 NETWORK
      COMMON/ORIGIN/X0,Y0,H0,AZCOR,BAZ,XRD,YRD
      COMMON/BCN/RNGMIN,RNGMAX,TYMN,TYMX,IBSCAN,TYMSCL

      CHARACTER ICOLTYP*8,LABL*4,LABD*40,DIGCOLR*2

      DATA RE/17000.0/
      DATA DTR/0.017453293/
      DATA FX,FY/0.04,0.04/

      DSIZ=DIGSIZE
      IF(IBSCAN.EQ.1)THEN
         GXMN=TYMN
         GXMX=TYMX
         GYMN=RNGMIN
         GYMX=RNGMAX
      ELSE
         GXMN=GXMIN(ITPOLD)
         GXMX=GXMAX(ITPOLD)
         GYMN=GYMIN(ITPOLD)
         GYMX=GYMAX(ITPOLD)
      END IF

C     Add offset label for digitized values where
C        digitized value = true value - digmx
C
      IF(ICOLTYP(1:4).EQ.'DIGT')THEN
         IVOFF=NINT(DIGOFF)
         VOFF=FLOAT(IVOFF)
c         write(*,*)'samploc: ',digmin,digmax,voff,ivoff
         WRITE(LABD,11)IVOFF,NAMFLD(IFL)
 11      FORMAT('Add ',I4,' to digitized values of ',A8)
         CALL GETSET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)
         CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)

c        disable plotting this inside panel
c
c         IF(ITPOLD.EQ.3)THEN
c            XP=FL+0.025
c            YP=FT-0.030
c            XP=FL
c            YP=FB-0.095
c            CALL PLCHMQ(XP,YP,LABD,10.0,0.0,-1.0)
c         ELSE
c            XP=FR
c            YP=FB-0.035
c            XP=FR
c            YP=FT+0.01
c            XP=FL
c            YP=FB-0.095
c            CALL PLCHMQ(XP,YP,LABD,10.0,0.0,-1.0)
c         END IF

         XP=FX
         YP=FY
         CALL PLCHMQ(XP,YP,LABD,10.0,0.0,-1.0)

         CALL SET (FL,FR,FB,FT,UL,UR,UB,UT,LLL)

C        Change line widths for lines and text 
C        and set color for digitized values.
C
         JLW=2000
         CALL SFLUSH
         CALL GETUSV('LW',ILW)
         CALL SETUSV('LW',JLW)

         IF(DIGCOLR.EQ.'WW')THEN
            CALL GSPLCI (1)
         ELSE IF(DIGCOLR.EQ.'BB')THEN
            CALL GSPLCI (IBLACK)
         ELSE IF(DIGCOLR.EQ.'GG')THEN
            CALL GSPLCI (IGRAY)
         ELSE IF(DIGCOLR.EQ.'ww')THEN
            CALL GSPLCI (IWHITE)
         ELSE IF(DIGCOLR.EQ.'rr')THEN
            CALL GSPLCI (IRED)
         ELSE IF(DIGCOLR.EQ.'gg')THEN
            CALL GSPLCI (IGREEN)
         ELSE IF(DIGCOLR.EQ.'bb')THEN
            CALL GSPLCI (IBLUE)
         ELSE IF(DIGCOLR.EQ.'cy')THEN
            CALL GSPLCI (ICYAN)
         ELSE IF(DIGCOLR.EQ.'mg')THEN
            CALL GSPLCI (IMAGENTA)
         ELSE IF(DIGCOLR.EQ.'yy')THEN
            CALL GSPLCI (IYELLOW)
         END IF
      END IF

      IF(IBSCAN.EQ.1)GO TO 400
      IF(ITPOLD.EQ.3)GO TO 200

C*****HERE FOR ALL SCANS EXCEPT RHI AND BSCAN
C     IF (CVRT), THEN ROTATE (X,Y) BY (C) DEG TO ACCOUNT FOR CONVERGENCE
C     OF LONGITUDE LINES AND TRANSLATE TO (X,Y) RELATIVE TO ANOTHER RADAR.
C
      SINC=SIN(DTR*AZCOR)
      COSC=COS(DTR*AZCOR)

      DO 150 IAZ=1,NANG(ISW),ITERBM
         SINAZ=SIN(DTR*AZA(IAZ,ISW))
         COSAZ=COS(DTR*AZA(IAZ,ISW))
         COSEL=COS(DTR*ELA(IAZ,ISW))

         DO 130 IGT=MNGATE,MXGATE,ITERGT

            X1=RNG(IGT,ISW)*COSEL*SINAZ
            Y1=RNG(IGT,ISW)*COSEL*COSAZ

            IF(X1.LT.XMIN(ITPOLD).OR.X1.GT.XMAX(ITPOLD).OR.
     +         Y1.LT.YMIN(ITPOLD).OR.Y1.GT.YMAX(ITPOLD))GO TO 130
            IF(ICVRT)THEN
               XR1=X1*COSC-Y1*SINC
               YR1=X1*SINC+Y1*COSC
               X1=X0+XR1
               Y1=Y0+YR1
            END IF

c     NINT operation for testing nearest Cartesian grid point
c
            XY_QUANT=5.0
            X1=XY_QUANT*NINT(X1/XY_QUANT)
            Y1=XY_QUANT*NINT(Y1/XY_QUANT)
c            write(3,1700)j,i,azrad,rng(i,isw),x,y
c 1700       format('SAMPLOC: j,i,ar,xy=',2i5,4f8.3)
            XPT=X1
            YPT=Y1
            IF((XPT.GE.GXMN .AND. XPT.LE.GXMX).AND.
     +         (YPT.GE.GYMN .AND. YPT.LE.GYMX))THEN
               IF(ICOLTYP.EQ.'SAMPLOC ')THEN
                  CALL POINT(XPT,YPT)
               ELSE IF(ICOLTYP(1:4).EQ.'DIGT')THEN
                  RVAL=DAT(IGT,IAZ,IFL)
                  IF(RVAL.NE.BDVAL)THEN
                     IF(RVAL.GE.DIGMIN.AND.RVAL.LE.DIGMAX)THEN
                        IVAL=NINT(RVAL-VOFF)
                        WRITE(LABL,115)IVAL
 115                    FORMAT(I4)
                        CALL PLCHMQ(XPT,YPT,LABL,DSIZ,0.0,1.0)
                     ELSE IF(RVAL.LT.DIGMIN)THEN
                        WRITE(LABL,117)
 117                    FORMAT('   -')
                        CALL PLCHMQ(XPT,YPT,LABL,DSIZ,0.0,1.0)
                     ELSE IF(RVAL.GT.DIGMAX)THEN
                        WRITE(LABL,119)
 119                    FORMAT('   +')
                        CALL PLCHMQ(XPT,YPT,LABL,DSIZ,0.0,1.0)
                     END IF
                  END IF
               END IF
            END IF
 130     CONTINUE
 150  CONTINUE

C     Restore text/line and color back width=ILW and color=foreground (1).
C
      CALL SFLUSH
      CALL GSPLCI(1)
      CALL GSTXCI(1)
      CALL SETUSV('LW',ILW)
      RETURN

C*****HERE FOR RHI SCANS
C
 200  CONTINUE

      DO 350 IAZ=1,NANG(ISW),ITERBM
         COSEL=COS(DTR*AZA(IAZ,ISW))
         TANEL=TAN(DTR*AZA(IAZ,ISW))

         DO 330 IGT=MNGATE,MXGATE,ITERGT
            X1=RNG(IGT,ISW)*COSEL
            Y1=X1*TANEL+X1*X1/RE
            IF(X1.LT.GXMN .OR. X1.GT.GXMX .OR.
     +         Y1.LT.GYMN .OR. Y1.GT.GYMX)GO TO 330
            IF(ICOLTYP.EQ.'SAMPLOC ')THEN
               CALL POINT(X1,Y1)
            ELSE IF(ICOLTYP(1:4).EQ.'DIGT')THEN
               RVAL=DAT(IGT,IAZ,IFL)
               IF(RVAL.NE.BDVAL)THEN
                  IF(RVAL.GE.DIGMIN.AND.RVAL.LE.DIGMAX)THEN
                     IVAL=NINT(RVAL-VOFF)
                     WRITE(LABL,115)IVAL
                     CALL PLCHMQ(X1,Y1,LABL,DSIZ,0.0,1.0)
                  ELSE IF(RVAL.LT.DIGMIN)THEN
                     WRITE(LABL,117)
                     CALL PLCHMQ(X1,Y1,LABL,DSIZ,0.0,1.0)
                  ELSE IF(RVAL.GT.DIGMAX)THEN
                     WRITE(LABL,119)
                     CALL PLCHMQ(X1,Y1,LABL,DSIZ,0.0,1.0)
                  END IF
               END IF
            END IF
 330     CONTINUE
 350  CONTINUE

C     Restore text/line and color back width=ILW and color=foreground (1).
C
      CALL SFLUSH
      CALL GSPLCI(1)
      CALL GSTXCI(1)
      CALL SETUSV('LW',ILW)
      RETURN

C*****HERE FOR BSCAN DISPLAY
C
  400 CONTINUE

      DO 600 IAZ=1,NANG(ISW),ITERBM
         X1=TYMSCL*(IAZ)
         DO 530 IGT=MNGATE,MXGATE,ITERGT
            Y1=RNG(IGT,ISW)
            IF(ICOLTYP.EQ.'SAMPLOC ')THEN
               CALL POINT(X1,Y1)
            ELSE IF(ICOLTYP(1:4).EQ.'DIGT')THEN
               RVAL=DAT(IGT,IAZ,IFL) 
               IF(RVAL.NE.BDVAL)THEN
                  IF(RVAL.GE.DIGMIN.AND.RVAL.LE.DIGMAX)THEN
                     IVAL=NINT(RVAL-VOFF)
                     WRITE(LABL,115)IVAL
                     CALL PLCHMQ(X1,Y1,LABL,DSIZ,0.0,1.0)
                  ELSE IF(RVAL.LT.DIGMIN)THEN
                     WRITE(LABL,117)
                     CALL PLCHMQ(X1,Y1,LABL,DSIZ,0.0,1.0)
                  ELSE IF(RVAL.GT.DIGMAX)THEN
                     WRITE(LABL,119)
                     CALL PLCHMQ(X1,Y1,LABL,DSIZ,0.0,1.0)
                  END IF
               END IF
            END IF
 530     CONTINUE
 600  CONTINUE

C     Restore text/line and color back width=ILW and color=foreground (1).
C
      CALL SFLUSH
      CALL GSPLCI(1)
      CALL GSTXCI(1)
      CALL SETUSV('LW',ILW)
      RETURN

      END
