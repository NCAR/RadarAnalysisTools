c
c----------------------------------------------------------------------X
c
      SUBROUTINE PLT_RGLOC(ICOLTYP,DIGCOLR,DIGSIZE,ROTATE,X_QUANT,
     X                     Y_QUANT,A_QUANT)

C     Plot POINT or digitize field values at range-angle locations.
C        Digitized values = True values - nint(digoff)
C
C     GXMIN,MAX - MINIMUM AND MAXIMUM USER X-DISTANCE (KM) FOR PLOTTING
C     GYMIN,MAX -    "     "     "      "  Y    "       "   "     "
C     FXMN,FXMX -    "     "     "    FIXED ANGLE TO BE PLOTTED
C     XMIN,XMAX -    "     "     "    X-DISTANCE (KM) FROM RADAR
C     YMIN,YMAX -    "     "     "    Y-   "       "    "    "
C         ICVRT - FALSE, COORDINATES RELATIVE TO RADAR
C                 TRUE,  COORDINATES RELATIVE TO EXP. ORIGIN OR ANOTHER RADAR
C     ROTATE    - Controls orientation of the Reorder grid box
C                 NO   - Orient relative to interpolation grid
C                 YES  - Orient relative to radar beam angle
C                 NONE - No grid boxes, only sample locations
C     X_QUANT - Dimension of box in X-direction
C     Y_QUANT - Dimension of box in Y-direction
C     A_QUANT - Range modification of box dimensions
C               ADIM = HRNG*A_QUANT(radians)
C
      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'
      INCLUDE 'input.inc'
      INCLUDE 'swth.inc'
      INCLUDE 'colors.inc'

      COMMON /INPUTCH/NAMFLD(MXF),IRATYP,ICORD
      CHARACTER*8 NAMFLD,IRATYP,ICORD
      CHARACTER*3 ROTATE
      DATA ANGXAX/90.0/
      COMMON /ORIGINCH/NETWORK
      CHARACTER*8 NETWORK
      COMMON/ORIGIN/X0,Y0,H0,AZCOR,BAZ,XRD,YRD
      COMMON/BCN/RNGMIN,RNGMAX,TYMN,TYMX,IBSCAN,TYMSCL

      CHARACTER ICOLTYP*8,LABL*4,LABD*40,DIGCOLR*2

      DATA RE/17000.0/
      DATA DTR/0.017453293/
      DATA FX,FY/0.04,0.04/
      DATA CSIZ1,CSIZ2/4.0,10.0/
      DATA HRNG_MIN/15.0/
      CHARACTER*6 SMRK,LAB6
      SAVE ILW

c      do mmm=1,100
c         print *,'SAMPLOC: icoltyp,digcolr,digsize=',
c     +        icoltyp,digcolr,digsize
c      call sflush
c      end do

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
      print *,'SAMPLOC: itpold=',itpold
      print *,'         GXMIN-MAX=',gxmin(itpold),gxmax(itpold)
      print *,'         GYMIN-MAX=',gymin(itpold),gxmax(itpold)
      print *,'         xmin,xmax=',xmin(itpold),xmax(itpold)
      print *,'         ymin,ymax=',ymin(itpold),ymax(itpold)

C     Add offset label for digitized values where
C        digitized value = true value - digmx
C
      IF(ICOLTYP(1:4).EQ.'DIGT')THEN
         IVOFF=NINT(DIGOFF)
         VOFF=FLOAT(IVOFF)
         write(*,*)'samploc: ',digmin,digmax,voff,ivoff
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
         JLW=1200
         CALL SFLUSH
         CALL GETUSV('LW',ILW)
         CALL SETUSV('LW',JLW)
         print *,'SAMPLOC: Default line thickness ',ilw,
     +        ' reset to ',jlw
      
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

c      do mmm=1,100
         print *,'SAMPLOC: made it to here,mmm= ',mmm
         print *,'SAMPLOC: dtr,azcor=',dtr,azcor
         print *,'SAMPLOC: isw,nang,iterbm=',isw,nang(isw),iterbm
         print *,'SAMPLOC: mngate,mxgate,itergt=',mngate,mxgate,itergt
         print *,'SAMPLOC: itpold,xmin,xmax,ymin,ymax=',itpold,
     +        xmin(itpold),xmax(itpold),ymin(itpold),ymax(itpold)
         print *,'SAMPLOC: icvrt=',icvrt
         print *,'SAMPLOC:    x0,y0,h0=',x0,y0,h0
         print *,'SAMPLOC: xrd,yrd,baz=',xrd,yrd,baz
         print *,'SAMPLOC: x_quant,y_quant,a_quant=',
     +        x_quant,y_quant,a_quant
         call sflush
c      end do

C     Create some test complex characters for marking range-gate
C     sampling locations as well as regular grid points.
C
c-----CALL COMPLX_CHAR
         
C     Complex character for gridded data
C     SMRK='&KGL&E' - Circled dot
C          '&PRU&+' - Plus sign
C
c      SMRK='&PRU&+'
      SMRK='&KGL&E'
      WRITE(LAB6,13)SMRK
 13   FORMAT(A6)

c     Range gates are placed at the nearest
c     Cartesian grid point (X_QUANT,Y_QUANT km away). 
c
      IF(X_QUANT*Y_QUANT .NE. 0.0)THEN
         DELX=4.0*X_QUANT
         DELY=4.0*Y_QUANT
         XDIM=1.0*X_QUANT
         YDIM=1.0*Y_QUANT
         NY=1+(GYMAX(ITPOLD)-GYMIN(ITPOLD))/DELY
         NX=1+(GXMAX(ITPOLD)-GXMIN(ITPOLD))/DELX

         IF(ROTATE.EQ.'YES' .OR. ROTATE.EQ.'NO')THEN
            CALL GSPLCI(IRED)
            DO J=2,NY-1
               Y=GYMIN(ITPOLD)+(J-1)*DELY
c              print *,'SAMPLOC: j,y=',j,y
               DO I=2,NX-1
                  X=GXMIN(ITPOLD)+(I-1)*DELX
c                 print *,'         i,x=',i,x
                  IF((X.GE.GXMN .AND. X.LE.GXMX).AND.
     +               (Y.GE.GYMN .AND. Y.LE.GYMX))THEN


c     Modify box dimensions with horizontal distance from radar
c
                     HRNG=SQRT((X-XRD)*(X-XRD)+(Y-YRD)*(Y-YRD))
                     IF(A_QUANT.GT.0.0.AND.HRNG.GE.1.0)THEN
                        XA_DIM=HRNG*A_QUANT*DTR
                        YA_DIM=HRNG*A_QUANT*DTR
                     ENDIF
                     XB_DIM=AMAX1(XDIM,XA_DIM)
                     YB_DIM=AMAX1(YDIM,XA_DIM)

                     CALL GRID_BOX(X,Y,XB_DIM,YB_DIM,XRD,YRD,ROTATE,
     +                    ANGXAX,LAB6,CSIZ2)
                     CALL GRID_BOX(X,Y,XDIM,YDIM,XRD,YRD,ROTATE,
     +                    ANGXAX,LAB6,CSIZ2)
c                    CALL POINT(X,Y)
                  ENDIF
               END DO
            END DO
            CALL SFLUSH
            CALL GSPLCI(1)
         ENDIF
      ENDIF

c     CALL GSPLCI(IRED) - sets line and text color
c     since PLCHHG and POINT use line drawing
      CALL GSPLCI(IRED)
c      CALL PLCHHQ (-45.0,0.0,'CHIL',CSIZ1,0.0,0.0)
c      CALL POINT(-45.0,0.0)
c      CALL LINE(-45.0,-20.0,-45.0,10.0)
c      CALL LINE(-55.0,0.0,-25.0,0.0)
      CALL SFLUSH
      CALL GSPLCI(1)

      DO 150 IAZ=1,NANG(ISW),ITERBM
c         do mmm=1,100
c         print *,'SAMPLOC: mmm,iaz,aza,ela=',mmm,iaz,aza(iaz,isw),
c         print *,'SAMPLOC: iaz,aza,ela=',iaz,aza(iaz,isw),
c     +           ela(iaz,isw)
         call sflush
c         end do
         SINAZ=SIN(DTR*AZA(IAZ,ISW))
         COSAZ=COS(DTR*AZA(IAZ,ISW))
         COSEL=COS(DTR*ELA(IAZ,ISW))

         DO 130 IGT=MNGATE,MXGATE,ITERGT
c            do mmm=1,100
c            print *,'SAMPLOC: mmm,igt,rng=',mmm,igt,rng(igt,isw)
c            print *,'SAMPLOC: igt,rng=',igt,rng(igt,isw)
c            call sflush
c            end do

            X1=RNG(IGT,ISW)*COSEL*SINAZ
            Y1=RNG(IGT,ISW)*COSEL*COSAZ
            HRNG=SQRT((X1-XRD)*(X1-XRD)+(Y1-YRD)*(Y1-YRD))
            IF(HRNG.LT.HRNG_MIN)GO TO 130
c            do mmm=1,100
c            print *,'SAMPLOC: mmm,x1,y1=',mmm,x1,y1
c            call sflush
c            end do

            IF(X1.LT.XMIN(ITPOLD).OR.X1.GT.XMAX(ITPOLD).OR.
     +         Y1.LT.YMIN(ITPOLD).OR.Y1.GT.YMAX(ITPOLD))GO TO 130

c     Convert range gate xy-location  
c
            IF(ICVRT)THEN
               XR1=X1*COSC-Y1*SINC
               YR1=X1*SINC+Y1*COSC
               X1=X0+XR1
               Y1=Y0+YR1
            END IF
c            do mmm=1,100
c            print *,'SAMPLOC: mmm,x1,y1=',mmm,x1,y1
c            call sflush
c            end do

c     NINT operation for testing nearest Cartesian grid point
c            This operation will place the range gate at the
c            nearest Cartesian grid point (X_QUANT, Y_QUANT km away). 
c
            IF(X_QUANT*Y_QUANT .NE. 0.0)THEN
               XPT=X_QUANT*NINT(X1/X_QUANT)
               YPT=Y_QUANT*NINT(Y1/Y_QUANT)
            ELSE
               XPT=X1
               YPT=Y1
            ENDIF
c            do mmm=1,100
c            write(3,1700)iaz,igt,aza(iaz,isw),rng(igt,isw),x1,y1
c 1700       format('SAMPLOC: j,i,ar,xy=',2i5,4f8.3)
c            call sflush
c            end do
            IF((XPT.GE.GXMN .AND. XPT.LE.GXMX).AND.
     +         (YPT.GE.GYMN .AND. YPT.LE.GYMX))THEN
               IF(ICOLTYP.EQ.'SAMPLOC ')THEN
                  CALL POINT(X1,Y1)

c     NOTE: Only plot the range gates (X1,Y1), not 
c     quantized points (XPT,YPT). (LJM 4/18/2011).
c
c                  CALL POINT(XPT,YPT)
c                  IF(ROTATE.EQ.'YES' .OR. ROTATE.EQ.'NO')THEN
c                     CALL PLCHHQ(XPT,YPT,LAB6,CSIZ1,0.0,0.0)
c                  ENDIF
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
      print *,'SAMPLOC: Set line thickness back, ilw',ilw
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
c               CALL POINT(X1,Y1)
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
