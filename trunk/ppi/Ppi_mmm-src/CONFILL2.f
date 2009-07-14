c
c----------------------------------------------------------------------X
c
      SUBROUTINE CONFILL2(IGRPLT,IGRYCON,PLTSW,DELAMX)
C
C  DOES AREA COLOR FILL TWO RAYS AT A TIME.  POLYGONS ARE COMPUTED BY
C  LINEAR (CONTOUR) INTERPOLATION BETWEEN TWO BEAMS.  PSEUDO-RHI'S
C  (IFLD=-4) EXTRACTED WITH THE FXSWATH FUNCTION ARE COLOR FILLED AFTER
C  HORIZONTAL ADVECTION.
C
C*****NOTE: BSCAN DISPLAY NOT IMPLEMENTED
C
C     Contouring cell:         2------1  Beam 1
C                              |      |
C                              |      |
C                              3------4  Beam 2
C
C     IF A CELL CONTAINS ANY BDVAL'S, DO NOT COLOR FILL.
C     Reset the number of consecutive boxes (NBOX) back to zero.
C     This may have fixed the occassional strips of constant color.
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
      COMMON/MAX/ZMAX,XX,YY,ZZ
      COMMON/COTABLE/ICOL(100)
      COMMON /ORIGINCH/NETWORK
      CHARACTER*8 NETWORK
      COMMON/ORIGIN/X0,Y0,H0,AZCOR,BAZ,XRD,YRD
      COMMON /DLINEC/X1,Y1,X2,Y2
      COMMON /CONTOUR/ LMX,LMN

      DIMENSION XP(20),YP(20)
      DIMENSION SINAZ(2),COSAZ(2),COSEL(2),TANEL(2)

      DATA RE/17000.0/
      DATA DTR/0.017453293/

      EQUIVALENCE (R1,I1),(R2,I2),(R3,I3),(R4,I4)
      EQUIVALENCE (CLV,ICLV)

C     IN-LINE INTERPOLATION FUNCTION
C
      WGT(RI,RJ)=(RI-CLV)/(RI-RJ)

      GXMN=GXMIN(ITPOLD)+DROLD
      GXMX=GXMAX(ITPOLD)-DROLD
      GYMN=GYMIN(ITPOLD)+DROLD
      GYMX=GYMAX(ITPOLD)-DROLD

c-----debug (ljm)
c     print *,'CFIL2:  gx,gy=',gxmn,gxmx,gymn,gymx
c     print *,'CFIL2:   nang=',isw,nang(isw)
c     print *,'CFIL2: ifl,rg=',ifl,rng(1,1),ro,dr,drold,itergt
c     print *,'CFIL2: mn,mxg=',mngate,mxgate
c     print *,'CFIL2:   azim=',azmin(itpold),azmax(itpold)
c-----debug (ljm)

      LMN=100
      LMX=0
      ZMAX=-999.0
      NAZH=NANG(ISW)/2
      AZDIF=AZA(NAZH,ISW)-AZA(NAZH-1,ISW)
      DIR=1.0
      IF((AZDIF.LT.0..AND.AZDIF.GT.-180.).OR.AZDIF.GT.350.)DIR=-1.

      IF(ITPOLD.EQ.3)GO TO 300

C*****HERE FOR ALL SCANS EXCEPT RHI
C     IF (CVRT), THEN ROTATE (X,Y) BY (C) DEG TO ACCOUNT FOR CONVERGENCE
C     OF LONGITUDE LINES AND TRANSLATE TO (X,Y) RELATIVE TO ANOTHER RADAR.
C
      SINC=SIN(DTR*AZCOR)
      COSC=COS(DTR*AZCOR)

      DO 15 IAZ=1,NANG(ISW)-1

C     IF THE ANGLES ARE NOT WITHIN PLOT WINDOW BOUNDS OR
C     IF THE ANGULAR INCREMENT IS TOO BIG, DO NOT AREA FILL
C
      AZ1=AZA(IAZ,ISW)
      AZ2=AZA(IAZ+1,ISW)
      IF(.NOT.(IAZC))THEN
         IF(AZ1.GT.180.)AZ1=AZ1-360.
         IF(AZ2.GT.180.)AZ2=AZ2-360.
      END IF

      IF( (AZ1   .LT. AZMIN(ITPOLD) .OR.
     +     AZ1   .GT. AZMAX(ITPOLD)).AND.
     +    (AZ2   .LT. AZMIN(ITPOLD) .OR.
     +     AZ2   .GT. AZMAX(ITPOLD))) GO TO 15

      AINCR=ABS(AZA(IAZ+1,ISW)-AZA(IAZ,ISW))
      IF(AINCR.GT.180.0)AINCR=360.0-AINCR
      IF(AINCR.GT.DELAMX)GO TO 15

      IF(IFLD(IFL).EQ.-4)THEN
         SINAZ(1)=SIN(DTR*AZA(IAZ  ,ISW))
         SINAZ(2)=SIN(DTR*AZA(IAZ+1,ISW))
         COSAZ(1)=COS(DTR*AZA(IAZ  ,ISW))
         COSAZ(2)=COS(DTR*AZA(IAZ+1,ISW))
      ELSE
         SINAZ(1)=SIN(DTR*(AZA(IAZ  ,ISW)-0.05*DIR))
         SINAZ(2)=SIN(DTR*(AZA(IAZ+1,ISW)+0.05*DIR))
         COSAZ(1)=COS(DTR*(AZA(IAZ  ,ISW)-0.05*DIR))
         COSAZ(2)=COS(DTR*(AZA(IAZ+1,ISW)+0.05*DIR))
      END IF

C     Ranges for swath'd fields are horizontal, not slant
C     ranges so don't project onto constant horizontal
C     planes from constant elevation angles surface.
C
c      IF(ISW.EQ.2)THEN
c         COSEL(1)=1.0
c         COSEL(2)=1.0
c      ELSE
         COSEL(1)=COS(DTR*ELA(IAZ  ,ISW))
         COSEL(2)=COS(DTR*ELA(IAZ+1,ISW))
c      END IF

      IDUB=0
      R1=DAT(MNGATE,IAZ,IFL)
      R4=DAT(MNGATE,IAZ+1,IFL)
      X1=SINAZ(1)*COSEL(1)*RNG(MNGATE,ISW)
      X4=SINAZ(2)*COSEL(2)*RNG(MNGATE,ISW)
      Y1=COSAZ(1)*COSEL(1)*RNG(MNGATE,ISW)
      Y4=COSAZ(2)*COSEL(2)*RNG(MNGATE,ISW)
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
C     OBTAINED BY SAMPLING SEVERAL AZIMUTHAL SCANS AT A PARTICULAR AZIMUTH.
C
      IF(IFLD(IFL).EQ.-4)THEN
         DX1=0.001*VELSWT*(JBSWT(IAZ)-JBSWT(1))
         DX4=0.001*VELSWT*(JBSWT(IAZ+1)-JBSWT(1))
         X1=X1-DX1
         X4=X4-DX4
      END IF

      MXG=MXGATE-ITERGT

      DO 250 IGT=MNGATE,MXG,ITERGT

      R2=R1
      R3=R4
      X2=X1
      X3=X4
      Y2=Y1
      Y3=Y4
      R1=DAT(IGT+ITERGT,IAZ,IFL)
      R4=DAT(IGT+ITERGT,IAZ+1,IFL)
      X1=SINAZ(1)*COSEL(1)*RNG(IGT+ITERGT,ISW)
      X4=SINAZ(2)*COSEL(2)*RNG(IGT+ITERGT,ISW)
      Y1=COSAZ(1)*COSEL(1)*RNG(IGT+ITERGT,ISW)
      Y4=COSAZ(2)*COSEL(2)*RNG(IGT+ITERGT,ISW)
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

C     IF A CELL CONTAINS ANY BDVAL'S, DO NOT COLOR FILL.
C     Reset the number of consecutive boxes (NBOX) back to zero.
C     This may have fixed the occassional strips of constant color.
C
      IF(R1.EQ.BDVAL.OR.R2.EQ.BDVAL.OR.
     +   R3.EQ.BDVAL.OR.R4.EQ.BDVAL)THEN
         NBOX=0
         GO TO 250
      END IF

      IF(X1.LT.GXMN .OR. X1.GT.GXMX .OR.
     +   X2.LT.GXMN .OR. X2.GT.GXMX .OR.
     +   X3.LT.GXMN .OR. X3.GT.GXMX .OR.
     +   X4.LT.GXMN .OR. X4.GT.GXMX)GO TO 250
      IF(Y1.LT.GYMN .OR. Y1.GT.GYMX .OR.
     +   Y2.LT.GYMN .OR. Y2.GT.GYMX .OR.
     +   Y3.LT.GYMN .OR. Y3.GT.GYMX .OR.
     +   Y4.LT.GYMN .OR. Y4.GT.GYMX)GO TO 250

      IF(R1.GT.ZMAX)THEN
         ZMAX=R1
         XX=X1
         YY=Y1
      END IF
      NC=1

      DO 210 ICN=1,NL

      CLV=CL(ICN)
      IF(ICN.EQ.1)THEN
        ICL1=0
        ICNL=0
      ELSE
        ICL1=ICOL(ICN-1)
        ICNL=ICN-1
      END IF
      ICL2=ICOL(ICN)
      IS1=0
      IS2=0
      IS3=0
      IS4=0
      IF(R1.LT.CLV)IS1=8
      IF(R2.LT.CLV)IS2=4
      IF(R3.LT.CLV)IS3=2
      IF(R4.LT.CLV)IS4=1
      I=1+IS1+IS2+IS3+IS4
C      I=1+AND(SHIFTR((R1-CLV),60),10B)+AND(SHIFTR((R2-CLV),61),4B)
C     +   +AND(SHIFTR((R3-CLV),62), 2B)+AND(SHIFTR((R4-CLV),63),1B)
      GO TO (210,10,20,30,40,50,60,70,80,90,100,110,120,130,140,
     +       150),I
   10 W=WGT(R1,R4)
      XP(1)=X1*(1.-W)+X4*W
      YP(1)=Y1*(1.-W)+Y4*W
      W=WGT(R3,R4)
      XP(2)=X3*(1.-W)+X4*W
      YP(2)=Y3*(1.-W)+Y4*W
      XP(3)=X3
      YP(3)=Y3
      XP(4)=X2
      YP(4)=Y2
      XP(5)=X1
      YP(5)=Y1
      CALL FAREA(ICL2,XP,YP,6)
      IF(NC.EQ.1 .AND. ICL1.GT.0)THEN
        XP(3)=X4
        YP(3)=Y4
        CALL FAREA(ICL1,XP,YP,4)
      END IF
      GO TO 200
20    W=WGT(R4,R3)
      XP(1)=X4*(1.-W)+X3*W
      YP(1)=Y4*(1.-W)+Y3*W
      W=WGT(R2,R3)
      XP(2)=X2*(1.-W)+X3*W
      YP(2)=Y2*(1.-W)+Y3*W
      XP(3)=X2
      YP(3)=Y2
      XP(4)=X1
      YP(4)=Y1
      XP(5)=X4
      YP(5)=Y4
      CALL FAREA(ICL2,XP,YP,6)
      IF(NC.EQ.1 .AND. ICL1.GT.0)THEN
        XP(3)=X3
        YP(3)=Y3
        CALL FAREA(ICL1,XP,YP,4)
      END IF
      GO TO 200
30    W=WGT(R1,R4)
      XP(1)=X1*(1.-W)+X4*W
      YP(1)=Y1*(1.-W)+Y4*W
      W=WGT(R2,R3)
      XP(2)=X2*(1.-W)+X3*W
      YP(2)=Y2*(1.-W)+Y3*W
      XP(3)=X2
      YP(3)=Y2
      XP(4)=X1
      YP(4)=Y1
      CALL FAREA(ICL2,XP,YP,5)
      IF(NC.EQ.1 .AND. ICL1.GT.0)THEN
        XP(3)=X3
        YP(3)=Y3
        XP(4)=X4
        YP(4)=Y4
        CALL FAREA(ICL1,XP,YP,5)
      END IF
      GO TO 200
40    W=WGT(R2,R3)
      XP(1)=X2*(1.-W)+X3*W
      YP(1)=Y2*(1.-W)+Y3*W
      W=WGT(R2,R1)
      XP(2)=X2*(1.-W)+X1*W
      YP(2)=Y2*(1.-W)+Y1*W
      XP(3)=X1
      YP(3)=Y1
      XP(4)=X4
      YP(4)=Y4
      XP(5)=X3
      YP(5)=Y3
      CALL FAREA(ICL2,XP,YP,6)
      IF(NC.EQ.1 .AND. ICL1.GT.0)THEN
        XP(3)=X2
        YP(3)=Y2
        CALL FAREA(ICL1,XP,YP,4)
      END IF
      GO TO 200
50    W=WGT(R2,R1)
      XP(1)=X2*(1.-W)+X1*W
      YP(1)=Y2*(1.-W)+Y1*W
      W=WGT(R1,R4)
      XP(2)=X1*(1.-W)+X4*W
      YP(2)=Y1*(1.-W)+Y4*W
      XS1=XP(1)
      YS1=YP(1)
      XS2=XP(2)
      YS2=YP(2)
      XP(3)=X1
      YP(3)=Y1
      CALL FAREA(ICL2,XP,YP,4)
      W=WGT(R3,R4)
      XP(1)=X3*(1.-W)+X4*W
      YP(1)=Y3*(1.-W)+Y4*W
      W=WGT(R2,R3)
      XP(2)=X2*(1.-W)+X3*W
      YP(2)=Y2*(1.-W)+Y3*W
      XP(3)=X3
      YP(3)=Y3
      CALL FAREA(ICL2,XP,YP,4)
      IF(NC.EQ.1 .AND. ICL1.GT.0)THEN
        XP(3)=X2
        YP(3)=Y2
        XP(4)=XS1
        YP(4)=YS1
        XP(5)=XS2
        YP(5)=YS2
        XP(6)=X4
        YP(6)=Y4
        CALL FAREA(ICL1,XP,YP,7)
      END IF
      GO TO 200
60    W=WGT(R1,R2)
      XP(1)=X1*(1.-W)+X2*W
      YP(1)=Y1*(1.-W)+Y2*W
      W=WGT(R3,R4)
      XP(2)=X3*(1.-W)+X4*W
      YP(2)=Y3*(1.-W)+Y4*W
      XP(3)=X4
      YP(3)=Y4
      XP(4)=X1
      YP(4)=Y1
      CALL FAREA(ICL2,XP,YP,5)
      IF(NC.EQ.1 .AND. ICL1.GT.0)THEN
        XP(3)=X3
        YP(3)=Y3
        XP(4)=X2
        YP(4)=Y2
        CALL FAREA(ICL1,XP,YP,5)
      END IF
      GO TO 200
70    W=WGT(R1,R2)
      XP(1)=X1*(1.-W)+X2*W
      YP(1)=Y1*(1.-W)+Y2*W
      W=WGT(R1,R4)
      XP(2)=X1*(1.-W)+X4*W
      YP(2)=Y1*(1.-W)+Y4*W
      XP(3)=X1
      YP(3)=Y1
      CALL FAREA(ICL2,XP,YP,4)
      IF(NC.EQ.1 .AND. ICL1.GT.0)THEN
        XP(3)=X4
        YP(3)=Y4
        XP(4)=X3
        YP(4)=Y3
        XP(5)=X2
        YP(5)=Y2
      CALL FAREA(ICL1,XP,YP,6)
      END IF
      GO TO 200
80    W=WGT(R1,R2)
      XP(1)=X1*(1.-W)+X2*W
      YP(1)=Y1*(1.-W)+Y2*W
      W=WGT(R1,R4)
      XP(2)=X1*(1.-W)+X4*W
      YP(2)=Y1*(1.-W)+Y4*W
      XP(3)=X4
      YP(3)=Y4
      XP(4)=X3
      YP(4)=Y3
      XP(5)=X2
      YP(5)=Y2
      CALL FAREA(ICL2,XP,YP,6)
      IF(NC.EQ.1 .AND. ICL1.GT.0)THEN
        XP(3)=X1
        YP(3)=Y1
        CALL FAREA(ICL1,XP,YP,4)
      END IF
      GO TO 200
90    W=WGT(R1,R2)
      XP(1)=X1*(1.-W)+X2*W
      YP(1)=Y1*(1.-W)+Y2*W
      W=WGT(R3,R4)
      XP(2)=X3*(1.-W)+X4*W
      YP(2)=Y3*(1.-W)+Y4*W
      XP(3)=X3
      YP(3)=Y3
      XP(4)=X2
      YP(4)=Y2
      CALL FAREA(ICL2,XP,YP,5)
      IF(NC.EQ.1 .AND. ICL1.GT.0)THEN
        XP(3)=X4
        YP(3)=Y4
        XP(4)=X1
        YP(4)=Y1
        CALL FAREA(ICL1,XP,YP,5)
      END IF
      GO TO 200
100   W=WGT(R2,R3)
      XP(1)=X2*(1.-W)+X3*W
      YP(1)=Y2*(1.-W)+Y3*W
      W=WGT(R1,R2)
      XP(2)=X1*(1.-W)+X2*W
      YP(2)=Y1*(1.-W)+Y2*W
      XS1=XP(1)
      YS1=YP(1)
      XS2=XP(2)
      YS2=YP(2)
      XP(3)=X2
      YP(3)=Y2
      CALL FAREA(ICL2,XP,YP,4)
      W=WGT(R3,R4)
      XP(1)=X3*(1.-W)+X4*W
      YP(1)=Y3*(1.-W)+Y4*W
      W=WGT(R1,R4)
      XP(2)=X1*(1.-W)+X4*W
      YP(2)=Y1*(1.-W)+Y4*W
      XP(3)=X4
      YP(3)=Y4
      CALL FAREA(ICL2,XP,YP,4)
      IF(NC.EQ.1 .AND. ICL1.GT.0)THEN
        XP(3)=X1
        YP(3)=Y1
        XP(4)=XS2
        YP(4)=YS2
        XP(5)=XS1
        YP(5)=YS1
        XP(6)=X3
        YP(6)=Y3
        CALL FAREA(ICL1,XP,YP,7)
      END IF
      GO TO 200
110   W=WGT(R2,R3)
      XP(1)=X2*(1.-W)+X3*W
      YP(1)=Y2*(1.-W)+Y3*W
      W=WGT(R2,R1)
      XP(2)=X2*(1.-W)+X1*W
      YP(2)=Y2*(1.-W)+Y1*W
      XP(3)=X2
      YP(3)=Y2
      CALL FAREA(ICL2,XP,YP,4)
      IF(NC.EQ.1 .AND. ICL1.GT.0)THEN
        XP(3)=X1
        YP(3)=Y1
        XP(4)=X4
        YP(4)=Y4
        XP(5)=X3
        YP(5)=Y3
        CALL FAREA(ICL1,XP,YP,6)
      END IF
      GO TO 200
120   W=WGT(R1,R4)
      XP(1)=X1*(1.-W)+X4*W
      YP(1)=Y1*(1.-W)+Y4*W
      W=WGT(R2,R3)
      XP(2)=X2*(1.-W)+X3*W
      YP(2)=Y2*(1.-W)+Y3*W
      XP(3)=X3
      YP(3)=Y3
      XP(4)=X4
      YP(4)=Y4
      CALL FAREA(ICL2,XP,YP,5)
      IF(NC.EQ.1 .AND. ICL1.GT.0)THEN
        XP(3)=X2
        YP(3)=Y2
        XP(4)=X1
        YP(4)=Y1
        CALL FAREA(ICL1,XP,YP,5)
      END IF
      GO TO 200
130   W=WGT(R4,R3)
      XP(1)=X4*(1.-W)+X3*W
      YP(1)=Y4*(1.-W)+Y3*W
      W=WGT(R2,R3)
      XP(2)=X2*(1.-W)+X3*W
      YP(2)=Y2*(1.-W)+Y3*W
      XP(3)=X3
      YP(3)=Y3
      CALL FAREA(ICL2,XP,YP,4)
      IF(NC.EQ.1 .AND. ICL1.GT.0)THEN
        XP(3)=X2
        YP(3)=Y2
        XP(4)=X1
        YP(4)=Y1
        XP(5)=X4
        YP(5)=Y4
        CALL FAREA(ICL1,XP,YP,6)
      END IF
      GO TO 200
140   W=WGT(R1,R4)
      XP(1)=X1*(1.-W)+X4*W
      YP(1)=Y1*(1.-W)+Y4*W
      W=WGT(R3,R4)
      XP(2)=X3*(1.-W)+X4*W
      YP(2)=Y3*(1.-W)+Y4*W
      XP(3)=X4
      YP(3)=Y4
      CALL FAREA(ICL2,XP,YP,4)
      IF(NC.EQ.1 .AND. ICL1.GT.0)THEN
        XP(3)=X3
        YP(3)=Y3
        XP(4)=X2
        YP(4)=Y2
        XP(5)=X1
        YP(5)=Y1
        CALL FAREA(ICL1,XP,YP,6)
      END IF
      GO TO 200
150   IF(NC.EQ.1)THEN
        NBOX=NBOX+1
        ICSV=ICL1
        IF(NBOX.EQ.1)THEN
           XB1=X1
           XB2=X2
           XB3=X3
           XB4=X4
           YB1=Y1
           YB2=Y2
           YB3=Y3
           YB4=Y4
        ELSE
           XB1=X1
           XB4=X4
           YB1=Y1
           YB4=Y4
        END IF
      ELSE
        IF(NBOX.GT.0)THEN
           NBOX=0
           XP(1)=XB1
           XP(2)=XB2
           XP(3)=XB3
           XP(4)=XB4
           YP(1)=YB1
           YP(2)=YB2
           YP(3)=YB3
           YP(4)=YB4
           CALL FAREA(ICSV,XP,YP,5)
         END IF
      END IF
      GO TO 250
200   CONTINUE
      NC = NC + 1
      IF(ICNL.GT.0 .AND. ICNL.LT.LMN) LMN=ICNL
      IF(ICN.LT.LMN) LMN=ICN
      IF(ICN.GT.LMX) LMX=ICN
210   CONTINUE
      IF(NC.EQ.1 .AND. ICL2.GT.0)THEN
         XP(1)=X1
         XP(2)=X2
         XP(3)=X3
         XP(4)=X4
         YP(1)=Y1
         YP(2)=Y2
         YP(3)=Y3
         YP(4)=Y4
         CALL FAREA(ICL2,XP,YP,5)
      END IF
250   CONTINUE
      IF(NBOX.GT.0)THEN
         NBOX=0
         XP(1)=XB1
         XP(2)=XB2
         XP(3)=XB3
         XP(4)=XB4
         YP(1)=YB1
         YP(2)=YB2
         YP(3)=YB3
         YP(4)=YB4
         CALL FAREA(ICSV,XP,YP,5)
      END IF
15    CONTINUE

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
300   CONTINUE

      DO 600 IAZ=1,NANG(ISW)-1

C     IF THE ANGULAR INCREMENT IS TOO BIG, DO NOT AREA FILL
C
      AINCR=ABS(AZA(IAZ+1,ISW)-AZA(IAZ,ISW))
      IF(AINCR.GT.180.0)AINCR=360.0-AINCR
      IF(AINCR.GT.DELAMX)GO TO 600

      IF(IFLD(IFL).EQ.-4)THEN
         COSEL(1)=COS(DTR*AZA(IAZ  ,ISW))
         COSEL(2)=COS(DTR*AZA(IAZ+1,ISW))
         TANEL(1)=TAN(DTR*AZA(IAZ  ,ISW))
         TANEL(2)=TAN(DTR*AZA(IAZ+1,ISW))
      ELSE
         COSEL(1)=COS(DTR*(AZA(IAZ  ,ISW)-0.05*DIR))
         COSEL(2)=COS(DTR*(AZA(IAZ+1,ISW)+0.05*DIR))
         TANEL(1)=TAN(DTR*(AZA(IAZ  ,ISW)-0.05*DIR))
         TANEL(2)=TAN(DTR*(AZA(IAZ+1,ISW)+0.05*DIR))
      END IF
      IDUB=0
      R1=DAT(MNGATE,IAZ,IFL)
      R4=DAT(MNGATE,IAZ+1,IFL)
      X1=COSEL(1)*RNG(MNGATE,ISW)
      X4=COSEL(2)*RNG(MNGATE,ISW)
      Y1=(X1)*TANEL(1)+X1*X1/RE+H0
      Y4=(X4)*TANEL(2)+X1*X1/RE+H0

C     CALCULATE HORIZONTAL TRANSLATION INCREMENTS FOR PSEUDO-RHI SCANS
C
      IF(IFLD(IFL).EQ.-4)THEN
         DX1=0.001*VELSWT*(JBSWT(IAZ)-JBSWT(1))
         DX4=0.001*VELSWT*(JBSWT(IAZ+1)-JBSWT(1))
         X1=X1-DX1
         X4=X4-DX4
      END IF

      MXG=MXGATE-ITERGT

      DO 550 IGT=MNGATE,MXG,ITERGT

      R2=R1
      R3=R4
      X2=X1
      X3=X4
      Y2=Y1
      Y3=Y4
      R1=DAT(IGT+ITERGT,IAZ,IFL)
      R4=DAT(IGT+ITERGT,IAZ+1,IFL)
      X1=COSEL(1)*RNG(IGT+ITERGT,ISW)
      X4=COSEL(2)*RNG(IGT+ITERGT,ISW)
      Y1=(X1)*TANEL(1)+X1*X1/RE+H0
      Y4=(X4)*TANEL(2)+X1*X1/RE+H0

C     TRANSLATE PSEUDO-RHI SCANS HORIZONTALLY
C
      IF(IFLD(IFL).EQ.-4)THEN
         X1=X1-DX1
         X4=X4-DX4
      END IF

C     IF A CELL CONTAINS ANY BDVAL'S, DO NOT COLOR FILL.
C     Reset the number of consecutive boxes (NBOX) back to zero.
C     This may have fixed the occassional strips of constant color.
C
      IF(R1.EQ.BDVAL.OR.R2.EQ.BDVAL.OR.
     +   R3.EQ.BDVAL.OR.R4.EQ.BDVAL)THEN
         NBOX=0
         GO TO 550
      END IF

      IF(X1.LT.GXMN .OR. X1.GT.GXMX .OR.
     +   X2.LT.GXMN .OR. X2.GT.GXMX .OR.
     +   X3.LT.GXMN .OR. X3.GT.GXMX .OR.
     +   X4.LT.GXMN .OR. X4.GT.GXMX)GO TO 550
      IF(Y1.LT.GYMN .OR. Y1.GT.GYMX .OR.
     +   Y2.LT.GYMN .OR. Y2.GT.GYMX .OR.
     +   Y3.LT.GYMN .OR. Y3.GT.GYMX .OR.
     +   Y4.LT.GYMN .OR. Y4.GT.GYMX)GO TO 550

      IF(R1.GT.ZMAX)THEN
         ZMAX=R1
         XX=X1
         YY=Y1
      END IF
      NC=1

      DO 510 ICN=1,NL

      CLV=CL(ICN)
      IF(ICN.EQ.1)THEN
        ICL1=0
        ICNL=0
      ELSE
        ICL1=ICOL(ICN-1)
        ICNL=ICN-1
      END IF
      ICL2=ICOL(ICN)
      IS1=0
      IS2=0
      IS3=0
      IS4=0
      IF(R1.LT.CLV)IS1=8
      IF(R2.LT.CLV)IS2=4
      IF(R3.LT.CLV)IS3=2
      IF(R4.LT.CLV)IS4=1
      I=1+IS1+IS2+IS3+IS4
C      I=1+AND(SHIFTR((R1-CLV),60),10B)+AND(SHIFTR((R2-CLV),61),4B)
C     +   +AND(SHIFTR((R3-CLV),62), 2B)+AND(SHIFTR((R4-CLV),63),1B)
      GO TO (510,310,320,330,340,350,360,370,380,390,400,410,420,
     +       430,440,450),I
  310 W=WGT(R1,R4)
      XP(1)=X1*(1.-W)+X4*W
      YP(1)=Y1*(1.-W)+Y4*W
      W=WGT(R3,R4)
      XP(2)=X3*(1.-W)+X4*W
      YP(2)=Y3*(1.-W)+Y4*W
      XP(3)=X3
      YP(3)=Y3
      XP(4)=X2
      YP(4)=Y2
      XP(5)=X1
      YP(5)=Y1
      CALL FAREA(ICL2,XP,YP,6)
      IF(NC.EQ.1 .AND. ICL1.GT.0)THEN
        XP(3)=X4
        YP(3)=Y4
        CALL FAREA(ICL1,XP,YP,4)
      END IF
      GO TO 500
320   W=WGT(R4,R3)
      XP(1)=X4*(1.-W)+X3*W
      YP(1)=Y4*(1.-W)+Y3*W
      W=WGT(R2,R3)
      XP(2)=X2*(1.-W)+X3*W
      YP(2)=Y2*(1.-W)+Y3*W
      XP(3)=X2
      YP(3)=Y2
      XP(4)=X1
      YP(4)=Y1
      XP(5)=X4
      YP(5)=Y4
      CALL FAREA(ICL2,XP,YP,6)
      IF(NC.EQ.1 .AND. ICL1.GT.0)THEN
        XP(3)=X3
        YP(3)=Y3
        CALL FAREA(ICL1,XP,YP,4)
      END IF
      GO TO 500
330   W=WGT(R1,R4)
      XP(1)=X1*(1.-W)+X4*W
      YP(1)=Y1*(1.-W)+Y4*W
      W=WGT(R2,R3)
      XP(2)=X2*(1.-W)+X3*W
      YP(2)=Y2*(1.-W)+Y3*W
      XP(3)=X2
      YP(3)=Y2
      XP(4)=X1
      YP(4)=Y1
      CALL FAREA(ICL2,XP,YP,5)
      IF(NC.EQ.1 .AND. ICL1.GT.0)THEN
        XP(3)=X3
        YP(3)=Y3
        XP(4)=X4
        YP(4)=Y4
        CALL FAREA(ICL1,XP,YP,5)
      END IF
      GO TO 500
340   W=WGT(R2,R3)
      XP(1)=X2*(1.-W)+X3*W
      YP(1)=Y2*(1.-W)+Y3*W
      W=WGT(R2,R1)
      XP(2)=X2*(1.-W)+X1*W
      YP(2)=Y2*(1.-W)+Y1*W
      XP(3)=X1
      YP(3)=Y1
      XP(4)=X4
      YP(4)=Y4
      XP(5)=X3
      YP(5)=Y3
      CALL FAREA(ICL2,XP,YP,6)
      IF(NC.EQ.1 .AND. ICL1.GT.0)THEN
        XP(3)=X2
        YP(3)=Y2
        CALL FAREA(ICL1,XP,YP,4)
      END IF
      GO TO 500
350   W=WGT(R2,R1)
      XP(1)=X2*(1.-W)+X1*W
      YP(1)=Y2*(1.-W)+Y1*W
      W=WGT(R1,R4)
      XP(2)=X1*(1.-W)+X4*W
      YP(2)=Y1*(1.-W)+Y4*W
      XS1=XP(1)
      YS1=YP(1)
      XS2=XP(2)
      YS2=YP(2)
      XP(3)=X1
      YP(3)=Y1
      CALL FAREA(ICL2,XP,YP,4)
      W=WGT(R3,R4)
      XP(1)=X3*(1.-W)+X4*W
      YP(1)=Y3*(1.-W)+Y4*W
      W=WGT(R2,R3)
      XP(2)=X2*(1.-W)+X3*W
      YP(2)=Y2*(1.-W)+Y3*W
      XP(3)=X3
      YP(3)=Y3
      CALL FAREA(ICL2,XP,YP,4)
      IF(NC.EQ.1 .AND. ICL1.GT.0)THEN
        XP(3)=X2
        YP(3)=Y2
        XP(4)=XS1
        YP(4)=YS1
        XP(5)=XS2
        YP(5)=YS2
        XP(6)=X4
        YP(6)=Y4
        CALL FAREA(ICL1,XP,YP,7)
      END IF
      GO TO 500
360   W=WGT(R1,R2)
      XP(1)=X1*(1.-W)+X2*W
      YP(1)=Y1*(1.-W)+Y2*W
      W=WGT(R3,R4)
      XP(2)=X3*(1.-W)+X4*W
      YP(2)=Y3*(1.-W)+Y4*W
      XP(3)=X4
      YP(3)=Y4
      XP(4)=X1
      YP(4)=Y1
      CALL FAREA(ICL2,XP,YP,5)
      IF(NC.EQ.1 .AND. ICL1.GT.0)THEN
        XP(3)=X3
        YP(3)=Y3
        XP(4)=X2
        YP(4)=Y2
        CALL FAREA(ICL1,XP,YP,5)
      END IF
      GO TO 500
370   W=WGT(R1,R2)
      XP(1)=X1*(1.-W)+X2*W
      YP(1)=Y1*(1.-W)+Y2*W
      W=WGT(R1,R4)
      XP(2)=X1*(1.-W)+X4*W
      YP(2)=Y1*(1.-W)+Y4*W
      XP(3)=X1
      YP(3)=Y1
      CALL FAREA(ICL2,XP,YP,4)
      IF(NC.EQ.1 .AND. ICL1.GT.0)THEN
        XP(3)=X4
        YP(3)=Y4
        XP(4)=X3
        YP(4)=Y3
        XP(5)=X2
        YP(5)=Y2
      CALL FAREA(ICL1,XP,YP,6)
      END IF
      GO TO 500
380   W=WGT(R1,R2)
      XP(1)=X1*(1.-W)+X2*W
      YP(1)=Y1*(1.-W)+Y2*W
      W=WGT(R1,R4)
      XP(2)=X1*(1.-W)+X4*W
      YP(2)=Y1*(1.-W)+Y4*W
      XP(3)=X4
      YP(3)=Y4
      XP(4)=X3
      YP(4)=Y3
      XP(5)=X2
      YP(5)=Y2
      CALL FAREA(ICL2,XP,YP,6)
      IF(NC.EQ.1 .AND. ICL1.GT.0)THEN
        XP(3)=X1
        YP(3)=Y1
        CALL FAREA(ICL1,XP,YP,4)
      END IF
      GO TO 500
390   W=WGT(R1,R2)
      XP(1)=X1*(1.-W)+X2*W
      YP(1)=Y1*(1.-W)+Y2*W
      W=WGT(R3,R4)
      XP(2)=X3*(1.-W)+X4*W
      YP(2)=Y3*(1.-W)+Y4*W
      XP(3)=X3
      YP(3)=Y3
      XP(4)=X2
      YP(4)=Y2
      CALL FAREA(ICL2,XP,YP,5)
      IF(NC.EQ.1 .AND. ICL1.GT.0)THEN
        XP(3)=X4
        YP(3)=Y4
        XP(4)=X1
        YP(4)=Y1
        CALL FAREA(ICL1,XP,YP,5)
      END IF
      GO TO 500
400   W=WGT(R2,R3)
      XP(1)=X2*(1.-W)+X3*W
      YP(1)=Y2*(1.-W)+Y3*W
      W=WGT(R1,R2)
      XP(2)=X1*(1.-W)+X2*W
      YP(2)=Y1*(1.-W)+Y2*W
      XS1=XP(1)
      YS1=YP(1)
      XS2=XP(2)
      YS2=YP(2)
      XP(3)=X2
      YP(3)=Y2
      CALL FAREA(ICL2,XP,YP,4)
      W=WGT(R3,R4)
      XP(1)=X3*(1.-W)+X4*W
      YP(1)=Y3*(1.-W)+Y4*W
      W=WGT(R1,R4)
      XP(2)=X1*(1.-W)+X4*W
      YP(2)=Y1*(1.-W)+Y4*W
      XP(3)=X4
      YP(3)=Y4
      CALL FAREA(ICL2,XP,YP,4)
      IF(NC.EQ.1 .AND. ICL1.GT.0)THEN
        XP(3)=X1
        YP(3)=Y1
        XP(4)=XS2
        YP(4)=YS2
        XP(5)=XS1
        YP(5)=YS1
        XP(6)=X3
        YP(6)=Y3
        CALL FAREA(ICL1,XP,YP,7)
      END IF
      GO TO 500
410   W=WGT(R2,R3)
      XP(1)=X2*(1.-W)+X3*W
      YP(1)=Y2*(1.-W)+Y3*W
      W=WGT(R2,R1)
      XP(2)=X2*(1.-W)+X1*W
      YP(2)=Y2*(1.-W)+Y1*W
      XP(3)=X2
      YP(3)=Y2
      CALL FAREA(ICL2,XP,YP,4)
      IF(NC.EQ.1 .AND. ICL1.GT.0)THEN
        XP(3)=X1
        YP(3)=Y1
        XP(4)=X4
        YP(4)=Y4
        XP(5)=X3
        YP(5)=Y3
        CALL FAREA(ICL1,XP,YP,6)
      END IF
      GO TO 500
420   W=WGT(R1,R4)
      XP(1)=X1*(1.-W)+X4*W
      YP(1)=Y1*(1.-W)+Y4*W
      W=WGT(R2,R3)
      XP(2)=X2*(1.-W)+X3*W
      YP(2)=Y2*(1.-W)+Y3*W
      XP(3)=X3
      YP(3)=Y3
      XP(4)=X4
      YP(4)=Y4
      CALL FAREA(ICL2,XP,YP,5)
      IF(NC.EQ.1 .AND. ICL1.GT.0)THEN
        XP(3)=X2
        YP(3)=Y2
        XP(4)=X1
        YP(4)=Y1
        CALL FAREA(ICL1,XP,YP,5)
      END IF
      GO TO 500
430   W=WGT(R4,R3)
      XP(1)=X4*(1.-W)+X3*W
      YP(1)=Y4*(1.-W)+Y3*W
      W=WGT(R2,R3)
      XP(2)=X2*(1.-W)+X3*W
      YP(2)=Y2*(1.-W)+Y3*W
      XP(3)=X3
      YP(3)=Y3
      CALL FAREA(ICL2,XP,YP,4)
      IF(NC.EQ.1 .AND. ICL1.GT.0)THEN
        XP(3)=X2
        YP(3)=Y2
        XP(4)=X1
        YP(4)=Y1
        XP(5)=X4
        YP(5)=Y4
        CALL FAREA(ICL1,XP,YP,6)
      END IF
      GO TO 500
440   W=WGT(R1,R4)
      XP(1)=X1*(1.-W)+X4*W
      YP(1)=Y1*(1.-W)+Y4*W
      W=WGT(R3,R4)
      XP(2)=X3*(1.-W)+X4*W
      YP(2)=Y3*(1.-W)+Y4*W
      XP(3)=X4
      YP(3)=Y4
      CALL FAREA(ICL2,XP,YP,4)
      IF(NC.EQ.1 .AND. ICL1.GT.0)THEN
        XP(3)=X3
        YP(3)=Y3
        XP(4)=X2
        YP(4)=Y2
        XP(5)=X1
        YP(5)=Y1
        CALL FAREA(ICL1,XP,YP,6)
      END IF
      GO TO 500
450   IF(NC.EQ.1 .AND. ICL1.GT.0)THEN
        XP(1)=X1
        YP(1)=Y1
        XP(2)=X2
        YP(2)=Y2
        XP(3)=X3
        YP(3)=Y3
        XP(4)=X4
        YP(4)=Y4
        CALL FAREA(ICL1,XP,YP,5)
        IF(ICNL.GT.0 .AND. ICNL.LT.LMN) LMN=ICNL
      END IF
      GO TO 550
500   CONTINUE
      NC = NC + 1
      IF(ICNL.GT.0 .AND. ICNL.LT.LMN) LMN=ICNL
      IF(ICN.LT.LMN) LMN=ICN
      IF(ICN.GT.LMX) LMX=ICN
510   CONTINUE
      IF(NC.EQ.1 .AND. ICL2.GT.0)THEN
         XP(1)=X1
         XP(2)=X2
         XP(3)=X3
         XP(4)=X4
         YP(1)=Y1
         YP(2)=Y2
         YP(3)=Y3
         YP(4)=Y4
         CALL FAREA(ICL2,XP,YP,5)
      END IF
550   CONTINUE
600   CONTINUE

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
