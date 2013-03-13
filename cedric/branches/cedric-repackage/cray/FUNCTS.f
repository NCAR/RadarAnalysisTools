      SUBROUTINE FUNCTS(A,B,NI,NJ,LEV,VEX,IS,CONA,CONEXT,
     X                  IEDW,NAX,ZAP,IFLAT)
C
C        PERFORMS ALGEBRAIC MANIPULATION ON CONSTANT PLANES OF
C                 DATA FIELDS.
C
      DIMENSION A(NI,NJ),B(NI,NJ),VEX(NI,NJ),IEDW(2,3),
     X     CONA(1),CONEXT(1),NAX(3)
      PARAMETER (NFMAX=25,NID=510)
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X                IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X                NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF
      DATA IR1,IR2/ 3, 11 /
      DATA EPS/0.005/
      DATA EPS2/1.0E-6/
      ZIP=ZAP
      ATR=ATAN(1.)/45.
      RTA=1./ATR
      C1=CONA(1)
      C2=CONA(2)
      C3=CONA(3)
      C4=CONA(4)
      K1=NAX(1)
      K2=NAX(2)
      K3=NAX(3)
      DX=CSP(3,K1)*2.0
      DY=CSP(3,K2)*2.0
      DCON=CSP(1,K3) + (LEV-1) * CSP(3,K3)
      I1=IEDW(1,K1)
      I2=IEDW(2,K1)
      J1=IEDW(1,K2)
      J2=IEDW(2,K2)
      IF(IS.GT.8)GO TO 5
C
C       INITIALIZATION FOR DERIVATIVE FUNCTIONS
C
      DXI=1./DX
      DYI=1./DY
      IF(I1.LE.1) I1=2
      IF(J1.LE.1) J1=2
      IF(I2.GE.NI) I2=NI-1
      IF(J2.GE.NJ) J2=NJ-1
      IF(I1.GT.I2.OR.J1.GT.J2) RETURN
      DO 2 J=1,NJ
      VEX(1,J)=ZIP
    2 VEX(NI,J)=ZIP
      DO 3 I=1,NI
      VEX(I,1)=ZIP
    3 VEX(I,NJ)=ZIP
    5 CONTINUE
      GO TO ( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     X       110,120,130,140,150,160,170,180,190,200,
     X       210,220,230,240,250,260,270,280,290,300,
     X       310,320,330,340,350,360,370,380,380,400,
     X       410,420,430,440,450,460,470,480,490,500,
     X       510,520,530,540,550,560,570,580,590,600,
     X       610,620,630,640,650,660), IS
   10 CONTINUE
      DXI=1./DX
      DO  15 I=I1,I2
      IM=I-1
      IP=I+1
      DO  15 J=J1,J2
         VEX(I,J)=CVMGT((A(IP,J)-A(IM,J))*DXI,ZIP,
     X            A(IP,J).NE.ZIP.AND.A(IM,J).NE.ZIP)
   15 CONTINUE
      RETURN
   20 CONTINUE
      DYI=1./DY
      DO  25 J=J1,J2
      JM=J-1
      JP=J+1
      DO  25 I=I1,I2
         VEX(I,J)=CVMGT((A(I,JP)-A(I,JM))*DYI,ZIP,
     X                 A(I,JP).NE.ZIP.AND.A(I,JM).NE.ZIP)
   25 CONTINUE
      RETURN
   30 CONTINUE
      DXI=1./DX
      DYI=1./DY
      DO  35 J=J1,J2
      JM=J-1
      JP=J+1
      DO  35 I=I1,I2
      IM=I-1
      IP=I+1
        VEX(I,J)=CVMGT((A(IP,J)-A(IM,J))*DXI+(B(I,JP)-B(I,JM))*DYI,ZIP,
     X     A(IP,J).NE.ZIP.AND.A(IM,J).NE.ZIP.AND.B(I,JP).NE.ZIP.AND.
     X     B(I,JM).NE.ZIP)
   35 CONTINUE
      RETURN
   40 CONTINUE
      DXI=1./DX
      DYI=1./DY
      DO  45 J=J1,J2
      JM=J-1
      JP=J+1
      DO  45 I=I1,I2
      IM=I-1
      IP=I+1
        VEX(I,J)=CVMGT((A(IP,J)-A(IM,J))*DXI-(B(I,JP)-B(I,JM))*DYI,ZIP,
     X     A(IP,J).NE.ZIP.AND.A(IM,J).NE.ZIP.AND.B(I,JP).NE.ZIP.AND.
     X     B(I,JM).NE.ZIP)
   45 CONTINUE
      RETURN
   50 CONTINUE
      DXI=1./DX
      DYI=1./DY
      DO  55 J=J1,J2
      JM=J-1
      JP=J+1
      DO  55 I=I1,I2
      IM=I-1
      IP=I+1
        VEX(I,J)=CVMGT((A(I,JP)-A(I,JM))*DYI-(B(IP,J)-B(IM,J))*DXI,ZIP,
     X     A(I,JP).NE.ZIP.AND.A(I,JM).NE.ZIP.AND.B(IP,J).NE.ZIP.AND.
     X     B(IM,J).NE.ZIP)
   55 CONTINUE
      RETURN
   60 CONTINUE
      DXI=1./DX
      DO  65 I=I1,I2
      IM=I-1
      IP=I+1
      DO  65 J=J1,J2
         VEX(I,J)=CVMGT(A(I,J)*(B(IP,J)-B(IM,J))*DXI,ZIP,
     X            A(I,J).NE.ZIP.AND.B(IP,J).NE.ZIP.AND.B(IM,J).NE.ZIP)
   65 CONTINUE
      RETURN
   70 CONTINUE
      DYI=1./DY
      DO  75 J=J1,J2
      JM=J-1
      JP=J+1
      DO  75 I=I1,I2
         VEX(I,J)=CVMGT(A(I,J)*(B(I,JP)-B(I,JM))*DYI,ZIP,
     X            A(I,J).NE.ZIP.AND.B(I,JP).NE.ZIP.AND.B(I,JM).NE.ZIP)
   75 CONTINUE
      RETURN
   80 CONTINUE
      DXI=1./DX
      DYI=1./DY
      DO  85 J=J1,J2
      JM=J-1
      JP=J+1
      DO  85 I=I1,I2
      IM=I-1
      IP=I+1
        VEX(I,J)=CVMGT((A(IP,J)-A(IM,J))*DXI*(B(I,JP)-B(I,JM))*DYI,ZIP,
     X     A(IP,J).NE.ZIP.AND.A(IM,J).NE.ZIP.AND.B(I,JP).NE.ZIP.AND.
     X     B(I,JM).NE.ZIP)
   85 VEX(I,J)=VX
      RETURN
   90 CONTINUE
      DO 95  J=J1,J2
      DO 95  I=I1,I2
         VEX(I,J)=CVMGT(C1*A(I,J)*B(I,J),ZIP,
     X            A(I,J).NE.ZIP.AND.B(I,J).NE.ZIP)
   95 CONTINUE
      RETURN
  100 CONTINUE
      DO 105 J=J1,J2
      DO 105 I=I1,I2
      VX=ZIP
      IF(A(I,J).LT.0.0.OR.A(I,J).EQ.ZIP) GO TO 105
      VX=0.0
      IF(A(I,J).NE.0.0) VX=SQRT(A(I,J))
  105 VEX(I,J)=VX
      RETURN
  110 CONTINUE
      DO 115 J=J1,J2
      DO 115 I=I1,I2
      VX=ZIP
        IF(A(I,J).NE.ZIP) VX=C1*10.**(C2*A(I,J))
  115 VEX(I,J)=VX
      RETURN
  120 CONTINUE
      DO 125 J=J1,J2
      DO 125 I=I1,I2
         VEX(I,J)=CVMGT(B(I,J),A(I,J),A(I,J).EQ.ZIP)
  125 CONTINUE
      RETURN
  130 CONTINUE
      DO 135 J=J1,J2
      DO 135 I=I1,I2
         VEX(I,J)=CVMGT(A(I,J)*A(I,J)+B(I,J)*B(I,J),ZIP,
     X            A(I,J).NE.ZIP.AND.B(I,J).NE.ZIP)
  135 CONTINUE
      RETURN
  140 CONTINUE
      DO 145 J=J1,J2
      Z=CVMGT(CSP(1,3)+(J-1)*CSP(3,3),DCON,K3.NE.3)
      RHO=C1*EXP(-C2*Z)
      DO 145 I=I1,I2
         VEX(I,J)=CVMGT(RHO*A(I,J),ZIP,A(I,J).NE.ZIP)
  145 CONTINUE
      RETURN
  150 CONTINUE
      DO 155 J=J1,J2
      DO 155 I=I1,I2
         VEX(I,J)=CVMGT(C1*A(I,J)+C2*B(I,J),ZIP,
     X            A(I,J).NE.ZIP.AND.B(I,J).NE.ZIP)
  155 CONTINUE
      RETURN
  160 CONTINUE
      DO 165 J=J1,J2
      DO 165 I=I1,I2
         VEX(I,J)=CVMGT(C1*A(I,J)-C2*B(I,J),ZIP,
     X            A(I,J).NE.ZIP.AND.B(I,J).NE.ZIP)
  165 CONTINUE
      RETURN
  170 CONTINUE
      DO 175 J=J1,J2
      DO 175 I=I1,I2
      VX=ZIP
        IF(A(I,J).NE.ZIP.AND.B(I,J).NE.ZIP.AND.B(I,J).NE.0.0)
     X                          VX=C1*A(I,J)/B(I,J)
  175 VEX(I,J)=VX
      RETURN
  180 CONTINUE
      JUNK = 0.0
      DO 185 J=J1,J2
      Z=CVMGT(CSP(1,3)+(J-1)*CSP(3,3),DCON,K3.NE.3)
      IF (C1.NE.0.0) THEN
         RHI=1./(C1*EXP(-C2*Z))
         JUNK=0.0
      ELSE
         JUNK=-1.
      END IF
      DO 185 I=I1,I2
         IF (JUNK.EQ.0.0) THEN
            VEX(I,J)=CVMGT(RHI*A(I,J),ZIP,
     X           A(I,J).NE.ZIP)
         ELSE
            VEX(I,J)=ZIP
         END IF
  185 CONTINUE
      RETURN
  190 CONTINUE
      DO 195 J=J1,J2
      DO 195 I=I1,I2
         VEX(I,J)=CVMGT(C1*A(I,J)+C2,ZIP,
     X            A(I,J).NE.ZIP)
  195 CONTINUE
      RETURN
  200 CONTINUE
      DO 205 J=J1,J2
      DO 205 I=I1,I2
         VEX(I,J)=CVMGT(ABS(A(I,J)),ZIP,
     X            A(I,J).NE.ZIP)
  205 CONTINUE
      RETURN
  210 CONTINUE
      DO 215 J=J1,J2
      DO 215 I=I1,I2
         VEX(I,J)=CVMGT(A(I,J),ZIP,
     X            B(I,J).NE.ZIP)
  215 CONTINUE
      RETURN
  220 CONTINUE
      DO 225 J=J1,J2
      DO 225 I=I1,I2
         VEX(I,J)=CVMGT(A(I,J),ZIP,
     X            B(I,J).NE.ZIP.AND.C1.GT.B(I,J))
  225 CONTINUE
      RETURN
  230 CONTINUE
      DO 235 J=J1,J2
      DO 235 I=I1,I2
         VEX(I,J)=CVMGT(A(I,J),ZIP,
     X            B(I,J).NE.ZIP.AND.C1.LT.B(I,J))
  235 CONTINUE
      RETURN
  240 CONTINUE
      DO 245 J=J1,J2
      DO 245 I=I1,I2
      VX=ZIP
      C2AIJ=C2*A(I,J)
      IF(C2AIJ.EQ.0.0.AND.A(I,J).NE.ZIP) VX= ZIP
        IF(C2AIJ.GT.0.0.AND.A(I,J).NE.ZIP) VX=C1*ALOG(C2AIJ)
  245 VEX(I,J)=VX
      RETURN
  250 CONTINUE
      DO 255 J=J1,J2
      DO 255 I=I1,I2
      VX=ZIP
        IF(A(I,J).NE.ZIP) VX=C1*EXP(A(I,J))
  255 VEX(I,J)=VX
      RETURN
  260 CONTINUE
      DO 265 J=J1,J2
      DO 265 I=I1,I2
      VX=ZIP
        IF(A(I,J).NE.ZIP.AND.A(I,J).GE.0.0) VX=C1*(A(I,J)**C2)
  265 VEX(I,J)=VX
      RETURN
  270 CONTINUE
      RETURN
  280 CONTINUE
C
C        (X,Y) DISTANCE- BRANCH TO GENERAL CALCULATION
C
      GO TO 380
C
  290 CONTINUE
      DO 295 J=J1,J2
      Z=CVMGT(CSP(1,3)+(J-1)*CSP(3,3),DCON,K3.NE.3)
      CON=C1*Z+C2
      DO 295 I=I1,I2
  295 VEX(I,J)=CON
      RETURN
  300 CONTINUE
      DO 305 J=J1,J2
      DO 305 I=I1,I2
      VX=ZIP
        IF(A(I,J).NE.ZIP) VX=SIN(C1*ATR*A(I,J)-(C2*ATR))
  305 VEX(I,J)=VX
      RETURN
  310 CONTINUE
      DO 315 J=J1,J2
      DO 315 I=I1,I2
         VEX(I,J)=CVMGT(A(I,J),ZIP,
     X            B(I,J).EQ.ZIP)
  315 CONTINUE
      RETURN
  320 CONTINUE
C
C        (X,Y,Z) DISTANCE- BRANCH TO GENERAL CALCULATION
C
      GO TO 380
C
  330 CONTINUE
      DO 335 J=J1,J2
      DO 335 I=I1,I2
      VX=ZIP
        IF(A(I,J).NE.ZIP) VX=COS(C1*ATR*A(I,J)-(C2*ATR))
  335 VEX(I,J)=VX
      RETURN
  340 CONTINUE
      CALL RANSET(IR1)
      IR1=IR1+2
      CON=C2-C1
      DO 345 J=J1,J2
      DO 345 I=I1,I2
      VX=RANF()
  345 VEX(I,J)=CON*VX+C1
      RETURN
  350 CONTINUE
      DO 355 J=J1,J2
      DO 355 I=I1,I2
         VEX(I,J)=CVMGT(AMAX1(A(I,J),B(I,J)),ZIP,
     X                  A(I,J).NE.ZIP.AND.B(I,J).NE.ZIP)
  355 CONTINUE
      RETURN
  360 CONTINUE
      DO 365 J=J1,J2
      DO 365 I=I1,I2
         VEX(I,J)=CVMGT(AMIN1(A(I,J),B(I,J)),ZIP,
     X                  A(I,J).NE.ZIP.AND.B(I,J).NE.ZIP)
  365 CONTINUE
      RETURN
  370 CONTINUE
      DO 375 J=J1,J2
      DO 375 I=I1,I2
      VX=ZIP
        AIJ=A(I,J)
        BIJ=B(I,J)
         IF(AIJ.EQ.ZIP.OR.BIJ.EQ.ZIP) GO TO 374
         VX=0.0
         IF(AIJ.NE.0.0.OR.BIJ.NE.0.0) VX=RTA*ATAN2(BIJ,AIJ)
  374 VEX(I,J)=VX
  375 CONTINUE
      RETURN
  380 CONTINUE
C
C        COMPUTE AZIMUTH (38) OR ELEVATION (39)
C

C
C     CHECK FOR FLAT EARTH MODE
C
      IF (IFLAT.EQ.1) THEN
         AKZI=0.0
      ELSE
         AKZI=1./17001.697
      END IF
      ANGR=AMOD((450.-CONEXT(1)),360.)*ATR
C      ASNF=SIN(ANGR)
C      ACSF=COS(ANGR)
      ASNF=0.0
      ACSF=1.0
      XORTR=CONEXT(2)-C1
      YORTR=CONEXT(3)-C2
      DO 385 J=J1,J2
         Z=CVMGT(CSP(1,3)+(J-1)*CSP(3,3),DCON,K3.NE.3)
         YTMP=CVMGT(CSP(1,2)+(J-1)*CSP(3,2),DCON,K3.EQ.3)
         ZRTR=Z-C3
         ZRTRSQ=ZRTR*ZRTR
         DO 384 I=I1,I2
            X=CVMGT(CSP(1,1)+(I-1)*CSP(3,1),DCON,K3.NE.1)
            Y=CVMGT(CSP(1,2)+(I-1)*CSP(3,2),YTMP,K3.EQ.1)
            XT=ACSF*X-ASNF*Y+XORTR
            YT=ASNF*X+ACSF*Y+YORTR
            IF(IS.EQ.28) THEN
C        (X,Y) DISTANCE
               VX=SQRT(XT*XT+YT*YT)
            ELSE IF(IS.EQ.32) THEN
C        RANGE CALCULATION
               VX=SQRT(XT*XT+YT*YT+ZRTRSQ)
            ELSE IF(IS.EQ.38) THEN
C        AZIMUTH
               VX=0.0
               IF(XT.NE.0.0.OR.YT.NE.0.0)VX=ATAN2(XT,YT)*RTA
               IF(VX.LT.0.0) VX=VX+360.0
            ELSE IF(IS.EQ.39) THEN
C        ELEVATION
               RNGSQ=XT*XT+YT*YT+ZRTRSQ
               RNG=SQRT(RNGSQ)
               ZADJ=ZRTR+(ZRTRSQ-RNGSQ)*AKZI
               IF(RNG .GT. EPS) THEN
                  ARC=ZADJ/RNG
                  IF(ABS(ARC).LE.1.0) VX=RTA*ASIN(ARC)
               ELSE
                  VX = 0.0
               END IF
            END IF
  384    VEX(I,J)=VX
  385 CONTINUE
      RETURN
  400 CONTINUE
      DO 405 J=J1,J2
      DO 405 I=I1,I2
  405 VEX(I,J)=C1
      RETURN
  410 CONTINUE
      DO 415 J=J1,J2
      DO 415 I=I1,I2
  415 VEX(I,J)=ZIP
      RETURN
  420 CONTINUE
      DO 425 I=I1,I2
      X=CVMGT(CSP(1,1)+(I-1)*CSP(3,1),DCON,K3.NE.1)
      CON=C1 * X +C2
      DO 425 J=J1,J2
  425 VEX(I,J)=CON
      RETURN
  430 CONTINUE
      DO 435 J=J1,J2
      CON=CVMGT(CSP(1,2)+(J-1)*CSP(3,2),DCON,K3.EQ.3)
      DO 435 I=I1,I2
      Y=CVMGT(CSP(1,2)+(I-1)*CSP(3,2),CON,K3.EQ.1)
  435 VEX(I,J)=C1 * Y +C2
      RETURN
  440 CONTINUE
      K=0
      AVG=0.0
      DO 445 J=J1,J2
      DO 445 I=I1,I2
      IF(A(I,J).EQ.ZIP) GO TO 445
         K=K+1
         AVG=AVG+A(I,J)
  445 CONTINUE
      IF(K.EQ.0) THEN
         AVG=ZIP
      ELSE
         AVG=C1*(AVG/FLOAT(K))
      END IF
      DO 446 J=J1,J2
      DO 446 I=I1,I2
  446 VEX(I,J)=AVG
      RETURN
  450 CONTINUE
      DO 455 J=J1,J2
      DO 455 I=I1,I2
      VX=ZIP
      C2AIJ=C2*A(I,J)
         IF(C2AIJ.EQ.0.0.AND.A(I,J).NE.ZIP) VX= ZIP
        IF(C2AIJ.GT.0.0.AND.A(I,J).NE.ZIP) VX=C1*ALOG10(C2AIJ)
  455 VEX(I,J)=VX
      RETURN
  460 CONTINUE
      IF(C1.EQ.0.0) C1=1.0
      DO 465 J=J1,J2
      DO 465 I=I1,I2
      VX=ZIP
         IF(A(I,J).NE.ZIP) VX=AMOD(A(I,J),C1)
  465 VEX(I,J)=VX
      RETURN
  470 CONTINUE
      DO 475 J=J1,J2
      DO 475 I=I1,I2
         VEX(I,J)=CVMGT(AMAX1(A(I,J),C1),ZIP,A(I,J).NE.ZIP)
  475 CONTINUE
      RETURN
  480 CONTINUE
      DO 485 J=J1,J2
      DO 485 I=I1,I2
         VEX(I,J)=CVMGT(AMIN1(A(I,J),C1),ZIP,A(I,J).NE.ZIP)
  485 CONTINUE
      RETURN
  490 CONTINUE
      ANGR=AMOD(450.-C1,360.)*ATR
      ASNF=SIN(ANGR)
      ACSF=COS(ANGR)
      DO 495 J=J1,J2
      DO 495 I=I1,I2
         VX=CVMGT(A(I,J)*ACSF+B(I,J)*ASNF,ZIP,
     X            A(I,J).NE.ZIP.AND.B(I,J).NE.ZIP)
  495 VEX(I,J)=VX
      RETURN
  500 CONTINUE
      DO 505 J=J1,J2
      DO 505 I=I1,I2
         VEX(I,J)=ZIP
         IF (A(I,J).EQ.ZIP) GO TO 505
         KNT = 0
         IWORD=A(I,J)
         IF (IWORD.EQ.0) GOTO 504
         DO 503 M=1,16
            KNT = KNT + ICEDAND(IWORD,1)
            IWORD=ICEDSHFT(IWORD,-1)
            IF (IWORD.EQ.0) GOTO 504
 503     CONTINUE
 504     VEX(I,J)=KNT
 505  CONTINUE
      RETURN
  510 CONTINUE
      CALL RANSET(IR1)
      IR1=IR1+2
      IF(C2.LE.0.0) C2=1.0
      PI2=ATAN(1.)*8.0
      DO 515 J=J1,J2
      DO 515 I=I1,I2
         U1=RANF()
         U2=RANF()
         IF (U1.LT.EPS2) U1=EPS2
         VX= SQRT(-2.0*ALOG(U1)) * COS(PI2*U2)
  515 VEX(I,J)= C2*VX + C1
      RETURN
  520 CONTINUE
      IF(C1.EQ.0.0) C1=1.0
      FMIN= 1.E+20
      DO 525 J=J1,J2
      DO 525 I=I1,I2
         FMIN=CVMGT(A(I,J),FMIN,A(I,J).NE.ZIP.AND.A(I,J).LT.FMIN)
  525 CONTINUE
      IF(FMIN .EQ. 1.E+20) FMIN=ZIP
      IF(FMIN .NE. ZIP) FMIN=FMIN*C1
      DO 526 J=J1,J2
      DO 526 I=I1,I2
  526 VEX(I,J)=FMIN
      RETURN
  530 CONTINUE
      IF(C1.EQ.0.0) C1=1.0
      FMAX= -1.E+20
      DO 535 J=J1,J2
      DO 535 I=I1,I2
         FMAX=CVMGT(A(I,J),FMAX,A(I,J).NE.ZIP.AND.A(I,J).GT.FMAX)
  535 CONTINUE
      IF(FMAX .EQ. -1.E+20) FMAX=ZIP
      IF(FMAX .NE. ZIP) FMAX=FMAX*C1
      DO 536 J=J1,J2
      DO 536 I=I1,I2
  536 VEX(I,J)=FMAX
      RETURN
  540 CONTINUE
      DO 545 J=J1,J2
      DO 545 I=I1,I2
  545 VEX(I,J)=CVMGT(C2,A(I,J),B(I,J).EQ.C1)
      RETURN
  550 CONTINUE
      DO 555 J=J1,J2
      DO 555 I=I1,I2
  555 VEX(I,J)=CVMGT(ZIP,A(I,J),
     X               B(I,J).LT.C1.OR.B(I,J).GT.C2.OR.B(I,J).EQ.ZIP)
      RETURN
  560 CONTINUE
      DO 565 J=J1,J2
      DO 565 I=I1,I2
  565 VEX(I,J)=CVMGT(ZIP,A(I,J),
     X              (B(I,J).GE.C1.AND.B(I,J).LE.C2).OR.B(I,J).EQ.ZIP)
      RETURN
  570 CONTINUE
      COUNT=0.0
      DO 575 J=J1,J2
      DO 575 I=I1,I2
         ADD=CVMGT(1.0,0.0,A(I,J).NE.ZIP)
  575 COUNT=COUNT+ADD
      SCOUNT=C1*COUNT
      DO 576 J=J1,J2
      DO 576 I=I1,I2
  576 VEX(I,J)=SCOUNT
      RETURN
  580 CONTINUE
      DO 588 J=J1,J2
         K=0
         AVG=0.0
         DO 585 I=I1,I2
            IF(A(I,J).NE.ZIP) THEN
               AVG = AVG+A(I,J)
               K=K+1
            END IF
  585    CONTINUE
         IF(K.GT.0) AVG = AVG/FLOAT(K)
         IF(K.EQ.0) AVG = ZIP
         DO 586 I=I1,I2
            VEX(I,J) = AVG
  586    CONTINUE
  588 CONTINUE
      RETURN
  590 CONTINUE
      DO 598 I=I1,I2
         K=0
         AVG=0.0
         DO 595 J=J1,J2
            IF(A(I,J).NE.ZIP) THEN
               AVG = AVG+A(I,J)
               K=K+1
            END IF
  595    CONTINUE
         IF(K.GT.0) AVG = AVG/FLOAT(K)
         IF(K.EQ.0) AVG = ZIP
         DO 596 J=J1,J2
            VEX(I,J) = AVG
  596    CONTINUE
  598 CONTINUE
      RETURN
 600  CONTINUE
C
C     SHIFT IN I DIRECTION
C
      DO 602 J=J1,J2
         DO 603 I=I1,I2
            IF (INT(I+C1).GE.1 .AND. INT(I+C1).LE.NI) THEN
               VEX(I,J)=A(INT(I+C1),J)
            ELSE
               VEX(I,J)=ZIP
            END IF
 603     CONTINUE
 602  CONTINUE
      RETURN
 610  CONTINUE
C
C     SHIFT IN J DIRECTION
C
      DO 612 J=J1,J2
         DO 613 I=I1,I2
            IF (INT(J+C1).GE.1 .AND. INT(J+C1).LE.NJ) THEN
               VEX(I,J)=A(I,INT(J+C1))
            ELSE
               VEX(I,J)=ZIP
            END IF
 613     CONTINUE
 612  CONTINUE
      RETURN
 620  CONTINUE
C
C     SHIFT IN I,J DIRECTIONS
C
      DO 622 J=J1,J2
         DO 623 I=I1,I2
            IF (INT(I+C1).GE.1 .AND. INT(I+C1).LE.NI .AND.
     X          INT(J+C2).GE.1 .AND. INT(J+C2).LE.NJ) THEN
               VEX(I,J)=A(INT(I+C1),INT(J+C2))
            ELSE
               VEX(I,J)=ZIP
            END IF
 623     CONTINUE
 622  CONTINUE
      RETURN
 630  CONTINUE
C
C     CREATE A LATITUDE FIELD
C
      IF (C1.EQ.0.0 .AND. C2.EQ.0.0) THEN
         REFLAT=ABS(ID(33)+(ID(34)/60.)+(ID(35)/3600.))
         REFLON=ABS(ID(36)+(ID(37)/60.)+(ID(38)/3600.))
      ELSE
         REFLAT=C1
         REFLON=C2
      END IF
      ANGXAX=ID(40)/REAL(ID(69))
      DO J=J1,J2
         DO I=I1,I2
            XVAL=CSP(1,1)+(I-1)*CSP(3,1)
            YVAL=CSP(1,2)+(J-1)*CSP(3,2)
            CALL XY2LLDRV(XLAT,XLON,XVAL,YVAL,REFLAT,REFLON,
     .           ANGXAX)
            VEX(I,J)=XLAT
         END DO
      END DO
      RETURN
 640  CONTINUE
C
C     CREATE A LONGITUDE FIELD
C
      IF (C1.EQ.0.0 .AND. C2.EQ.0.0) THEN
         REFLAT=ABS(ID(33)+(ID(34)/60.)+(ID(35)/3600.))
         REFLON=ABS(ID(36)+(ID(37)/60.)+(ID(38)/3600.))
      ELSE
         REFLAT=C1
         REFLON=C2
      END IF
      ANGXAX=ID(40)/REAL(ID(69))
      DO J=J1,J2
         DO I=I1,I2
            XVAL=CSP(1,1)+(I-1)*CSP(3,1)
            YVAL=CSP(1,2)+(J-1)*CSP(3,2)
            CALL XY2LLDRV(XLAT,XLON,XVAL,YVAL,REFLAT,REFLON,
     .           ANGXAX)
            VEX(I,J)=XLON
         END DO
      END DO
      RETURN
 650  CONTINUE
C     COMPUTE 1-D STD. DEV.
      DO 652 J=J1,J2
         FACC=0.0
         FSQR=0.0
         N=0
         DO 654 I=I1,I2
            IF (A(I,J).NE.ZIP) THEN
               FACC=FACC+A(I,J)
               FSQR=FSQR+A(I,J)**2
               N   =N+1
            END IF
 654     CONTINUE
         IF (N.GT.0) THEN
            FAVG=FACC/FLOAT(N)
            FSQR=FSQR/FLOAT(N)
            FSTD=FSQR-FAVG**2
            IF (FSTD.LT.0.0) FSTD=0.0
            FSTD=SQRT(FSTD)
         ELSE
            FSTD=ZIP
         END IF
         DO I=I1,I2
            VEX(I,J)=FSTD
         END DO
 652  CONTINUE
      RETURN
 660  CONTINUE
C     COMPUTE 1-D STD. DEV.
      DO 662 I=I1,I2
         FACC=0.0
         FSQR=0.0
         N=0
         DO 664 J=J1,J2
            IF (A(I,J).NE.ZIP) THEN
               FACC=FACC+A(I,J)
               FSQR=FSQR+A(I,J)**2
               N   =N+1
            END IF
 664     CONTINUE
         IF (N.GT.0) THEN
            FAVG=FACC/FLOAT(N)
            FSQR=FSQR/FLOAT(N)
            FSTD=FSQR-FAVG**2
            IF (FSTD.LT.0.0) FSTD=0.0
            FSTD=SQRT(FSTD)
         ELSE
            FSTD=ZIP
         END IF
         DO J=J1,J2
            VEX(I,J)=FSTD
         END DO
 662  CONTINUE
      RETURN
      END



