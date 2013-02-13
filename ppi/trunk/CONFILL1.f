c
c----------------------------------------------------------------------X
c
      SUBROUTINE CONFILL1(IGRPLT,IGRYCON,PLTSW,DELAMX)
C
C  DOES AREA COLOR FILL ONE RAY AT A TIME.  POLYGONS ARE COMPUTED AS A
C  RANGE GATE DEEP AND A SCAN ANGLE INCREMENT WIDE.  NO LINEAR INTER-
C  POLATION BETWEEN BEAMS IS INVOLVED.  PSEUDO-RHI'S (IFLD=-4) EXTRACTED
C  WITH THE FXSWATH FUNCTION ARE COLOR FILLED AFTER HORIZONTAL ADVECTION.
C     IF IAZC =  .TRUE., THEN ANGLES ARE LEFT IN THE RANGE    0 TO 360
C               .FALSE.,   "     "    "   PUT  "  "    "   -180 TO 180
C
      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'
      INCLUDE 'input.inc'
      INCLUDE 'swth.inc'
      CHARACTER*8 IGRYCON
      LOGICAL FOF,UNI,PLTSW

      COMMON /INPUTCH/NAMFLD(MXF),IRATYP,ICORD
      CHARACTER*8 NAMFLD,IRATYP,ICORD
      COMMON /ELEV/ELT(2)
      COMMON/BCN/RNGMIN,RNGMAX,TYMN,TYMX,IBSCAN,TYMSCL
      COMMON/MAX/ZMAX,XX,YY,ZZ
      COMMON/COTABLE/ICOL(100)
      COMMON /ORIGINCH/NETWORK
      CHARACTER*8 NETWORK
      COMMON/ORIGIN/X0,Y0,H0,AZCOR,BAZ,XRD,YRD
      COMMON /DLINEC/X1,Y1,X2,Y2
      COMMON /CONTOUR/ LMX,LMN

      DIMENSION SINAZ(2),COSAZ(2),COSEL(2),TANEL(2)
      DIMENSION XP(20),YP(20)

      DATA RE/17000.0/
      DATA DTR/0.017453293/

      EQUIVALENCE (R1,I1),(R2,I2),(R3,I3),(R4,I4)
      EQUIVALENCE (CLV,ICLV)

C     IN-LINE INTERPOLATION FUNCTION
C
C     WGT(RI,RJ)=(RI-CLV)/(RI-RJ)

      IF(IBSCAN.EQ.1)THEN
         GXMN=TYMN
         GXMX=TYMX
         GYMN=RNGMIN
         GYMX=RNGMAX
      ELSE
         GXMN=GXMIN(ITPOLD)+DROLD
         GXMX=GXMAX(ITPOLD)-DROLD
         GYMN=GYMIN(ITPOLD)+DROLD
         GYMX=GYMAX(ITPOLD)-DROLD
      END IF

c-----debug (ljm)
c     print *,'CFIL1:  gx,gy=',gxmn,gxmx,gymn,gymx
c     print *,'CFIL1:   nang=',isw,nang(isw)
c     print *,'CFIL1: ifl,rg=',ifl,rng(1,1),r0,dr,drold,itergt
c     print *,'CFIL1: mn,mxg=',mngate,mxgate
c     print *,'CFIL1:   azim=',azmin(itpold),azmax(itpold)
c-----debug (ljm)

      LMN=100
      LMX=0
      ZMAX=-999.0
      DRNG=0.5*DROLD*ITERGT

      IF(IBSCAN.EQ.1)GO TO 400

      NAZH=NANG(ISW)/2
      AZDIF=AZA(NAZH,ISW)-AZA(NAZH-1,ISW)
      DIR=1.0
      IF((AZDIF.LT.0..AND.AZDIF.GT.-180.).OR.AZDIF.GT.350.)DIR=-1.

      IF(ITPOLD.EQ.3)GO TO 300

C*****HERE FOR ALL SCANS EXCEPT FOR RHI AND BSCAN
C     IF (CVRT), THEN ROTATE (X,Y) BY (C) DEG TO ACCOUNT FOR CONVERGENCE
C     OF LONGITUDE LINES AND TRANSLATE TO (X,Y) RELATIVE TO ANOTHER RADAR.
C
      SINC=SIN(DTR*AZCOR)
      COSC=COS(DTR*AZCOR)
      MXG=MXGATE-ITERGT

      DO 50 IAZ=2,NANG(ISW)-1

C     IF THE ANGLE IS NOT WITHIN PLOT WINDOW BOUNDS OR
C     IF THE ANGULAR INCREMENT IS TOO BIG, DO NOT AREA FILL
C
C
      AZ1=0.5*(AZA(IAZ-1,ISW)+AZA(IAZ,ISW))
      IF(ABS(AZA(IAZ-1,ISW)-AZA(IAZ,ISW)).GT.180.)AZ1=0.5*
     +  (AMIN1(AZA(IAZ-1,ISW),AZA(IAZ,ISW))+360.+
     +   AMAX1(AZA(IAZ-1,ISW),AZA(IAZ,ISW)))

      AZ2=0.5*(AZA(IAZ,ISW)+AZA(IAZ+1,ISW))
      IF(ABS(AZA(IAZ+1,ISW)-AZA(IAZ,ISW)).GT.180.)AZ2=0.5*
     +  (AMIN1(AZA(IAZ+1,ISW),AZA(IAZ,ISW))+360.+
     +   AMAX1(AZA(IAZ+1,ISW),AZA(IAZ,ISW)))
      IF(.NOT.(IAZC))THEN
         IF(AZ1.GT.180.)AZ1=AZ1-360.
         IF(AZ2.GT.180.)AZ2=AZ2-360.
      END IF


      IF( (AZ1 .LT. AZMIN(ITPOLD) .OR.
     +     AZ1 .GT. AZMAX(ITPOLD)).AND.
     +    (AZ2 .LT. AZMIN(ITPOLD) .OR.
     +     AZ2 .GT. AZMAX(ITPOLD))) GO TO 50

      AINCR=ABS(AZA(IAZ+1,ISW)-AZA(IAZ-1,ISW))
      IF(AINCR.GT.180.0)AINCR=ABS(360.0-AINCR)
c      write(*,*)'        ',dir,az1,az2,aincr,2.0*delamx
      IF(AINCR.GT. (2.0*DELAMX) )GO TO 50

C     Change azimuths if either 180 or 360 crossing
C
      IF(AZ1.LT.0.0 .AND. AZ2.GT.0.0)AZ1=AZ1+360.0
      IF(AZ2.LT.0.0 .AND. AZ1.GT.0.0)AZ2=AZ2+360.0
      IF(ABS (AZ1-AZ2).GT.180.0)THEN
         IF(AZ1.GT.270.0 .AND. AZ2.LT.90.0)AZ1=AZ1-360.0
         IF(AZ2.GT.270.0 .AND. AZ1.LT.90.0)AZ2=AZ2-360.0
      END IF
c      write(*,*)'        ',dir,az1,az2,aincr,2.0*delamx

      IF(IFLD(IFL).EQ.-4)THEN
         SINAZ(1)=SIN(DTR*AZ1)
         SINAZ(2)=SIN(DTR*AZ2)
         COSAZ(1)=COS(DTR*AZ1)
         COSAZ(2)=COS(DTR*AZ2)
      ELSE
         SINAZ(1)=SIN(DTR*(AZ1-0.05*DIR))
         SINAZ(2)=SIN(DTR*(AZ2+0.05*DIR))
         COSAZ(1)=COS(DTR*(AZ1-0.05*DIR))
         COSAZ(2)=COS(DTR*(AZ2+0.05*DIR))
      END IF

C     Ranges for swath'd fields are horizontal, not slant
C     ranges so don't project onto constant horizontal
C     planes from constant elevation angles surface.
C
c      IF(ISW.EQ.2)THEN
c         COSEL(1)=1.0
c         COSEL(2)=1.0
c      ELSE
         COSEL(1)=0.5*(COS(DTR*ELA(IAZ-1,ISW))+COS(DTR*ELA(IAZ,ISW)))
         COSEL(2)=0.5*(COS(DTR*ELA(IAZ+1,ISW))+COS(DTR*ELA(IAZ,ISW)))
c      END IF

      IDUB=0
      RNG1=RNG(MNGATE,ISW)-DRNG
      IF(RNG1.LE.0.0)RNG1=0.001
      X1=SINAZ(1)*COSEL(1)*RNG1
      X4=SINAZ(2)*COSEL(2)*RNG1
      Y1=COSAZ(1)*COSEL(1)*RNG1
      Y4=COSAZ(2)*COSEL(2)*RNG1
      IF(ICVRT)THEN
         XR1=X1*COSC-Y1*SINC
         YR1=X1*SINC+Y1*COSC
         X1=X0+XR1
         Y1=Y0+YR1
         XR4=X4*COSC-Y4*SINC
         YR4=X4*SINC+Y4*COSC
         X4=X0+XR4
         Y4=Y0+YR4
      END IF

C     CALCULATE HORIZONTAL TRANSLATION INCREMENTS FOR PSEUDO-RHI SCANS
C
      IF(IFLD(IFL).EQ.-4)THEN
         T1=0.5*(JBSWT(IAZ)+JBSWT(IAZ-1))
         T4=0.5*(JBSWT(IAZ)+JBSWT(IAZ+1))
         DX1=0.001*VELSWT*(T1-JBSWT(1))
         DX4=0.001*VELSWT*(T4-JBSWT(1))
         X1=X1-DX1
         X4=X4-DX4
      END IF

      NBOX=0
      IPCL=-99

      DO 150 I=MNGATE,MXG,ITERGT

      X2=X1
      X3=X4
      Y2=Y1
      Y3=Y4
      IF(IPCL.EQ.-99)THEN
         XB2=X1
         XB3=X4
         YB2=Y1
         YB3=Y4
      END IF
      X1=SINAZ(1)*COSEL(1)*(RNG(I,ISW)+DRNG)
      X4=SINAZ(2)*COSEL(2)*(RNG(I,ISW)+DRNG)
      Y1=COSAZ(1)*COSEL(1)*(RNG(I,ISW)+DRNG)
      Y4=COSAZ(2)*COSEL(2)*(RNG(I,ISW)+DRNG)
      IF(ICVRT)THEN
         XR1=X1*COSC-Y1*SINC
         YR1=X1*SINC+Y1*COSC
         X1=X0+XR1
         Y1=Y0+YR1
         XR4=X4*COSC-Y4*SINC
         YR4=X4*SINC+Y4*COSC
         X4=X0+XR4
         Y4=Y0+YR4
      END IF

C     TRANSLATE PSEUDO-RHI SCANS HORIZONTALLY
C
      IF(IFLD(IFL).EQ.-4)THEN
         X1=X1-DX1
         X4=X4-DX4
      END IF

      IF(X1.LT.GXMN .OR. X1.GT.GXMX .OR.
     +   X2.LT.GXMN .OR. X2.GT.GXMX .OR.
     +   X3.LT.GXMN .OR. X3.GT.GXMX .OR.
     +   X4.LT.GXMN .OR. X4.GT.GXMX)GO TO 150
      IF(Y1.LT.GYMN .OR. Y1.GT.GYMX .OR.
     +   Y2.LT.GYMN .OR. Y2.GT.GYMX .OR.
     +   Y3.LT.GYMN .OR. Y3.GT.GYMX .OR.
     +   Y4.LT.GYMN .OR. Y4.GT.GYMX)GO TO 150
      IF(DAT(I,IAZ,IFL).GT.ZMAX)THEN
         ZMAX=DAT(I,IAZ,IFL)
         XX=X1
         YY=Y1
      END IF
      IF(DAT(I,IAZ,IFL).LT.CL(1))THEN
         ICL=0
         GO TO 140
      ELSE IF(DAT(I,IAZ,IFL).GT.CL(NL))THEN
         ICL=NL
         GO TO 140
      END IF
      DO N=1,NL-1
         IF(DAT(I,IAZ,IFL).GE.CL(N).AND.DAT(I,IAZ,IFL).LE.CL(N+1))THEN
            ICL= N
            GO TO 140
         END IF
      END DO
 140  CONTINUE
      IF(IPCL.EQ.-99)IPCL=ICL
      IF(ICL.EQ.IPCL)THEN
         NBOX=NBOX+1
         XB1=X1
         XB4=X4
         YB1=Y1
         YB4=Y4
         GO TO 150
      ELSE
         XP(1)=X2
         XP(2)=X3
         XP(3)=XB3
         XP(4)=XB2
         YP(1)=Y2
         YP(2)=Y3
         YP(3)=YB3
         YP(4)=YB2
         IF(IPCL.GT.0)THEN
            IF(IPCL.LT.LMN) LMN=IPCL
            IF(IPCL.GT.LMX) LMX=IPCL
            CALL FAREA(ICOL(IPCL),XP,YP,5)
         END IF
c         IF(IFTIME.GE.70000)PRINT 973,(XP(IJ),YP(IJ),IJ=1,5)
c 973     FORMAT(1X,'XP,YP=',10F7.1)
         IPCL=ICL
         NBOX=0
         XB2=X2
         XB3=X3
         YB2=Y2
         YB3=Y3
      END IF
150   CONTINUE
      IF(NBOX.GT.0)THEN
         XP(1)=XB1
         XP(2)=XB2
         XP(3)=XB3
         XP(4)=XB4
         YP(1)=YB1
         YP(2)=YB2
         YP(3)=YB3
         YP(4)=YB4
         NBOX=0
         IF(IPCL.GT.0)THEN
            IF(IPCL.LT.LMN) LMN=IPCL
            IF(IPCL.GT.LMX) LMX=IPCL
            CALL FAREA(ICOL(IPCL),XP,YP,5)
         END IF
      END IF
50    CONTINUE

C     Save min/max color-fill indices before calling CONTUR
C     since CONTUR will also generate min/max contour levels
C
      LMNF=LMN
      LMXF=LMX
      IF(IGRYCON.EQ.'C')THEN
         JLW=1000
         CALL CONTUR(IGRPLT,JLW,DELAMX)
      END IF
      LMN=LMNF
      LMX=LMXF
      RETURN

C*****HERE FOR RHI COLOR FILL
C
300   MXG=MXGATE-ITERGT

      DO 180 IAZ=2,NANG(ISW)-1

C     IF THE ANGULAR INCREMENT IS TOO BIG, DO NOT AREA FILL
C
      
      AINCR=0.5*ABS(AZA(IAZ+1,ISW)-AZA(IAZ-1,ISW))

      IF(AINCR.GT.180.0)AINCR=360.0-AINCR
      IF(AINCR.GT.DELAMX)GO TO 180

      AZ1=0.5*(AZA(IAZ-1,ISW)+AZA(IAZ,ISW))
      IF(ABS(AZA(IAZ-1,ISW)-AZA(IAZ,ISW)).GT.180.)AZ1=0.5*
     +  (AMIN1(AZA(IAZ-1,ISW),AZA(IAZ,ISW))+360.+
     +   AMAX1(AZA(IAZ-1,ISW),AZA(IAZ,ISW)))

      AZ2=0.5*(AZA(IAZ,ISW)+AZA(IAZ+1,ISW))
      IF(ABS(AZA(IAZ+1,ISW)-AZA(IAZ,ISW)).GT.180.)AZ2=0.5*
     +  (AMIN1(AZA(IAZ+1,ISW),AZA(IAZ,ISW))+360.+
     +   AMAX1(AZA(IAZ+1,ISW),AZA(IAZ,ISW)))

      IF(IFLD(IFL).EQ.-4)THEN
         COSEL(1)=COS(DTR*AZ1)
         COSEL(2)=COS(DTR*AZ2)
         TANEL(1)=TAN(DTR*AZ1)
         TANEL(2)=TAN(DTR*AZ2)
      ELSE
         COSEL(1)=COS(DTR*(AZ1-0.05*DIR))
         COSEL(2)=COS(DTR*(AZ2+0.05*DIR))
         TANEL(1)=TAN(DTR*(AZ1-0.05*DIR))
         TANEL(2)=TAN(DTR*(AZ2+0.05*DIR))
      END IF
      IDUB=0
      RNG1=RNG(MNGATE,ISW)-DRNG
      IF(RNG1.LE.0.0)RNG1=0.001
      X1=COSEL(1)*RNG1
      X4=COSEL(2)*RNG1
      Y1=(X1)*TANEL(1)+X1*X1/RE+H0
      Y4=(X4)*TANEL(2)+X4*X4/RE+H0

C     CALCULATE HORIZONTAL TRANSLATION INCREMENTS FOR PSEUDO-RHI SCANS
C
      IF(IFLD(IFL).EQ.-4)THEN
         T1=0.5*(JBSWT(IAZ)+JBSWT(IAZ-1))
         T2=0.5*(JBSWT(IAZ)+JBSWT(IAZ+1))
         DX1=0.001*VELSWT*(T1-JBSWT(1))
         DX4=0.001*VELSWT*(T2-JBSWT(1))
         X1=X1-DX1
         X4=X4-DX4
      END IF

      NBOX=0
      IPCL=-99

      DO 250 I=MNGATE,MXG,ITERGT

      X2=X1
      X3=X4
      Y2=Y1
      Y3=Y4
      IF(IPCL.EQ.-99)THEN
         XB2=X1
         XB3=X4
         YB2=Y1
         YB3=Y4
      END IF
      X1=COSEL(1)*(RNG(I,ISW)+DRNG)
      X4=COSEL(2)*(RNG(I,ISW)+DRNG)
      Y1=(X1)*TANEL(1)+X1*X1/RE+H0
      Y4=(X4)*TANEL(2)+X4*X4/RE+H0

C     TRANSLATE PSEUDO-RHI SCANS HORIZONTALLY
C
      IF(IFLD(IFL).EQ.-4)THEN
         X1=X1-DX1
         X4=X4-DX4
      END IF

      IF(X1.LT.GXMN .OR. X1.GT.GXMX .OR.
     +   X2.LT.GXMN .OR. X2.GT.GXMX .OR.
     +   X3.LT.GXMN .OR. X3.GT.GXMX .OR.
     +   X4.LT.GXMN .OR. X4.GT.GXMX)GO TO 250
      IF(Y1.LT.GYMN .OR. Y1.GT.GYMX .OR.
     +   Y2.LT.GYMN .OR. Y2.GT.GYMX .OR.
     +   Y3.LT.GYMN .OR. Y3.GT.GYMX .OR.
     +   Y4.LT.GYMN .OR. Y4.GT.GYMX)GO TO 250
      IF(DAT(I,IAZ,IFL).GT.ZMAX)THEN
         ZMAX=DAT(I,IAZ,IFL)
         XX=X1
         YY=Y1
      END IF
      IF(DAT(I,IAZ,IFL).LT.CL(1))THEN
         ICL=0
         GO TO 240
      ELSE IF(DAT(I,IAZ,IFL).GT.CL(NL))THEN
         ICL=NL
         GO TO 240
      END IF
      DO N=1,NL-1
         IF(DAT(I,IAZ,IFL).GE.CL(N).AND.DAT(I,IAZ,IFL).LE.CL(N+1))THEN
            ICL= N
            GO TO 240
         END IF
      END DO
 240  CONTINUE
      IF(IPCL.EQ.-99)IPCL=ICL
      IF(ICL.EQ.IPCL)THEN
         NBOX=NBOX+1
         XB1=X1
         XB4=X4
         YB1=Y1
         YB4=Y4
         GO TO 250
      ELSE
         XP(1)=X2
         XP(2)=X3
         XP(3)=XB3
         XP(4)=XB2
         YP(1)=Y2
         YP(2)=Y3
         YP(3)=YB3
         YP(4)=YB2
         IF(IPCL.GT.0)THEN
            IF(IPCL.LT.LMN) LMN=IPCL
            IF(IPCL.GT.LMX) LMX=IPCL
            CALL FAREA(ICOL(IPCL),XP,YP,5)
         END IF
         IPCL=ICL
         NBOX=0
         XB2=X2
         XB3=X3
         YB2=Y2
         YB3=Y3
      END IF
250   CONTINUE
      IF(NBOX.GT.0)THEN
         XP(1)=XB1
         XP(2)=XB2
         XP(3)=XB3
         XP(4)=XB4
         YP(1)=YB1
         YP(2)=YB2
         YP(3)=YB3
         YP(4)=YB4
         NBOX=0
         IF(IPCL.GT.0)THEN
            IF(IPCL.LT.LMN) LMN=IPCL
            IF(IPCL.GT.LMX) LMX=IPCL
            CALL FAREA(ICOL(IPCL),XP,YP,5)
         END IF
      END IF
180   CONTINUE
C     Save min/max color-fill indices before calling CONTUR
C     since CONTUR will also generate min/max contour levels
C
      LMNF=LMN
      LMXF=LMX
      IF(IGRYCON.EQ.'C')THEN
         JLW=1000
         CALL CONTUR(IGRPLT,JLW,DELAMX)
      END IF
      LMN=LMNF
      LMX=LMXF
      RETURN

C*****HERE FOR BSCAN COLOR FILL
C
  400 CONTINUE

      DO 450 IAZ=1,NANG(ISW)-1

      X1=TYMSCL*(IAZ-0.5)
      X4=TYMSCL*(IAZ+0.5)
      Y1=RNG(MNGATE,ISW)-DRNG
      IF(Y1.LT.GYMN)Y1=GYMN
      Y4=Y1
      X2=X1
      X3=X4
      NBOX=0
      IPCL=-99

      DO 550 I=MNGATE,MXGATE-ITERGT

      Y2=Y1
      Y3=Y4
      IF(IPCL.EQ.-99)THEN
         XB2=X1
         XB3=X4
         YB2=Y1
         YB3=Y4
      END IF
      Y1=RNG(I,ISW)+DRNG
      Y4=Y1

      IF(X1.LT.GXMN .OR. X1.GT.GXMX .OR.
     +   X2.LT.GXMN .OR. X2.GT.GXMX .OR.
     +   X3.LT.GXMN .OR. X3.GT.GXMX .OR.
     +   X4.LT.GXMN .OR. X4.GT.GXMX)GO TO 550
      IF(Y1.LT.GYMN .OR. Y1.GT.GYMX .OR.
     +   Y2.LT.GYMN .OR. Y2.GT.GYMX .OR.
     +   Y3.LT.GYMN .OR. Y3.GT.GYMX .OR.
     +   Y4.LT.GYMN .OR. Y4.GT.GYMX)GO TO 550
      IF(DAT(I,IAZ,IFL).GT.ZMAX)THEN
         ZMAX=DAT(I,IAZ,IFL)
         XX=X1
         YY=Y1
      END IF
      IF(DAT(I,IAZ,IFL).LT.CL(1))THEN
         ICL=0
         GO TO 540
      ELSE IF(DAT(I,IAZ,IFL).GT.CL(NL))THEN
         ICL=NL
         GO TO 540
      END IF
      DO N=1,NL-1
         IF(DAT(I,IAZ,IFL).GE.CL(N).AND.DAT(I,IAZ,IFL).LE.CL(N+1))THEN
            ICL= N
            GO TO 540
         END IF
      END DO
 540  CONTINUE
      IF(IPCL.EQ.-99)IPCL=ICL
      IF(ICL.EQ.IPCL)THEN
         NBOX=NBOX+1
         XB1=X1
         XB4=X4
         YB1=Y1
         YB4=Y4
         GO TO 550
      ELSE
         XP(1)=X2
         XP(2)=X3
         XP(3)=XB3
         XP(4)=XB2
         YP(1)=Y2
         YP(2)=Y3
         YP(3)=YB3
         YP(4)=YB2
         IF(IPCL.GT.0)THEN
            IF(IPCL.LT.LMN) LMN=IPCL
            IF(IPCL.GT.LMX) LMX=IPCL
            CALL FAREA(ICOL(IPCL),XP,YP,5)
         END IF
         IPCL=ICL
         NBOX=0
         XB2=X2
         XB3=X3
         YB2=Y2
         YB3=Y3
      END IF
550   CONTINUE
      IF(NBOX.GT.0)THEN
         XP(1)=XB1
         XP(2)=XB2
         XP(3)=XB3
         XP(4)=XB4
         YP(1)=YB1
         YP(2)=YB2
         YP(3)=YB3
         YP(4)=YB4
         NBOX=0
         IF(IPCL.GT.0)THEN
            IF(IPCL.LT.LMN) LMN=IPCL
            IF(IPCL.GT.LMX) LMX=IPCL
            CALL FAREA(ICOL(IPCL),XP,YP,5)
         END IF
      END IF
450   CONTINUE

C     Save min/max color-fill indices before calling CONTUR
C     since CONTUR will also generate min/max contour levels
C
      LMNF=LMN
      LMXF=LMX
      IF(IGRYCON.EQ.'C')THEN
         JLW=1000
         CALL CONTUR(IGRPLT,JLW,DELAMX)
      END IF
      LMN=LMNF
      LMX=LMXF
      RETURN

      END
