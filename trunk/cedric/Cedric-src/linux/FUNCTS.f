      SUBROUTINE FUNCTS(A,B,NI,NJ,LEV,VEX,IS,CONA,CONEXT,
     X                  IEDW,NAX,ZAP,IFLAT,LATLON,IFAX,SNDNAME)
C
C        PERFORMS ALGEBRAIC MANIPULATION ON CONSTANT PLANES OF
C                 DATA FIELDS.  SEE THE LIST OF FUNCTIONS IN
C                 FUNCTN.
C     CONEXT - (1) +X-axis angle (deg), (2) Origin X (km), (3) Origin Y (km)
C     CONA   - Constants C1-4 for functions
C     IFLAT  - (0) Curved earth, (1) Flat earth
C

      INCLUDE 'CEDRIC.INC'
      INCLUDE 'vadwinds.inc'
      PARAMETER (MXFC=7)
      DIMENSION AK(MXFC),BK(MXFC)

      PARAMETER (MXSTCK=60,MAXCON=4) 
      PARAMETER (PI=3.141592654)
      PARAMETER (RAD_EARTH=6370.12)
      PARAMETER (DEGRAD=0.01745329)
      CHARACTER*1 IFAX 
      REAL EARTH_RADIANS
      REAL LATVAL,COSLAT,SX,SY
      REAL LONVALN,LATVALN
      DIMENSION A(NI,NJ),B(NI,NJ),VEX(NI,NJ),IEDW(2,3),
     X     CONA(MAXCON*MXSTCK),CONEXT(MAXCON),NAX(3)

      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X                IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X                NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF

      COMMON /LEVELS/ VALLEV(MAXZLEV),VALNYQ(MAXZLEV),VNYQ_VOL
      COMMON /AXUNTS/ IUNAXS,LABAXS(3,3),SCLAXS(3,3),AXNAM(3)
      CHARACTER*4 AXNAM
      COMMON /ORIGIN/ORIGIN_LAT,ORIGIN_LON,LATLON_KARD
      REAL ORIGIN_LAT,ORIGIN_LON
      REAL RDEG,RMIN,RSEC

      PARAMETER (MXFUN=14)
      CHARACTER*8 LFUN(MXFUN)
      DATA LFUN/'PRES    ','TEMP    ','DEWPT   ',
     X          'RH      ','U       ','V       ',
     X          'SPD     ','DIR     ','ASCENT  ',
     X          'RADVEL  ','TCLOUD  ','DTEMP   ',
     X          'CLDMIX  ','CLDLWC  '/
      CHARACTER*8 SNDNAME

      DATA IR1,IR2/ 3, 11 /
      DATA EPS/0.005/
      DATA EPS2/1.0E-6/
      DATA ABSC/273.16/
      LOGICAL LATLON

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
c      DCON=CSP(1,K3) + (LEV-1) * CSP(3,K3)
      DCON=VALLEV(LEV)
      RCON=ATR*DCON
      TCON=TAN(RCON)
      SF=1./FLOAT(ID(68))
      XRAD=SF*ID(315)
      YRAD=SF*ID(316)
      ZRAD=0.001*ID(317)
c      print *,'FUN-rad x,y,z=',xrad,yrad,zrad
C
C     CHECK FOR FLAT EARTH MODE AND SET ANGLES FOR +X AXIS (ANGR,ANGXAX)
C
      IF (IFLAT.EQ.1) THEN
         AKZI=0.0
      ELSE
         AKZI=1./17001.697
      END IF
      ANGR=AMOD((450.-CONEXT(1)),360.)*ATR
      ANGXAX=FLOAT(ID(40))/FLOAT(ID(69))

      I1=IEDW(1,K1)
      I2=IEDW(2,K1)
      J1=IEDW(1,K2)
      J2=IEDW(2,K2)
      IF(IS.GT.8)GO TO 5
C
C       INITIALIZATION FOR DERIVATIVE FUNCTIONS
C
      EARTH_RADIANS = (PI * RAD_EARTH)/180.
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
c      print *,'FUNCTS: Before goto branch:  is=',is
      GO TO ( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     X       110,120,130,140,150,160,170,180,190,200,
     X       210,220,230,240,250,260,270,280,290,300,
     X       310,320,330,340,350,360,370,380,380,400,
     X       410,420,430,440,450,460,470,480,490,500,
     X       510,520,530,540,550,560,570,580,590,600,
     X       610,620,630,640,650,660,670,680,690,700,
     X       710,720,730,740,750,760,770), IS

   10 CONTINUE
C-----(DF/DI):

      DO  15 I=I1,I2
        IM=I-1
        IP=I+1
        DO  15 J=J1,J2
          IF(LATLON) THEN
             LATVAL = CSP(1,2) + (J-1)*CSP(3,2)
             COSLAT = COS(LATVAL * DEGRAD)
             SX = EARTH_RADIANS * COSLAT
          ELSE
             SX = 1.0
          END IF
          DXI = 1./(DX * SX)
          VEX(I,J) = CVMGT((A(IP,J)-A(IM,J))*DXI,ZIP,
     X             A(IP,J).NE.ZIP.AND.A(IM,J).NE.ZIP)
   15 CONTINUE
      RETURN

   20 CONTINUE
C-----(DF/DJ):

      IF(LATLON) THEN
        SY = EARTH_RADIANS
      ELSE
        SY = 1
      ENDIF
      DYI=1./(DY * SY)
      DO  25 J=J1,J2
        JM=J-1
        JP=J+1
        DO  25 I=I1,I2
            VEX(I,J)=CVMGT((A(I,JP)-A(I,JM))*DYI,ZIP,
     X                 A(I,JP).NE.ZIP.AND.A(I,JM).NE.ZIP)
   25 CONTINUE
      RETURN

   30 CONTINUE
C-----(DDI+DDJ): DF1/DI+DF2/DJ

      DO  35 J=J1,J2
        JM=J-1
        JP=J+1
        DO  35 I=I1,I2
          IF(LATLON) THEN
             LATVAL = CSP(1,2) + (J-1)*CSP(3,2)
             COSLAT = COS(LATVAL * DEGRAD)
             SX = EARTH_RADIANS * COSLAT
             SY = EARTH_RADIANS
          ELSE
             SX = 1
             SY = 1
          END IF
          DXI=1./(DX * SX)
          DYI=1./(DY * SY)
          IM=I-1
          IP=I+1
        VEX(I,J)=CVMGT((A(IP,J)-A(IM,J))*DXI+(B(I,JP)-B(I,JM))*DYI,ZIP,
     X     A(IP,J).NE.ZIP.AND.A(IM,J).NE.ZIP.AND.B(I,JP).NE.ZIP.AND.
     X     B(I,JM).NE.ZIP)
   35 CONTINUE
      RETURN

   40 CONTINUE
C-----(DDI-DDJ): DF1/DI-DF2/DJ

      DO  45 J=J1,J2
        JM=J-1
        JP=J+1
        DO  45 I=I1,I2
          IF(LATLON) THEN
             LATVAL = CSP(1,2) + (J-1)*CSP(3,2)
             COSLAT = COS(LATVAL * DEGRAD)
             SX = EARTH_RADIANS * COSLAT
             SY = EARTH_RADIANS
          ELSE
             SX = 1
             SY = 1
          END IF
          DXI=1./(DX * SX)
          DYI=1./(DY * SY)
          IM=I-1
          IP=I+1
        VEX(I,J)=CVMGT((A(IP,J)-A(IM,J))*DXI-(B(I,JP)-B(I,JM))*DYI,ZIP,
     X     A(IP,J).NE.ZIP.AND.A(IM,J).NE.ZIP.AND.B(I,JP).NE.ZIP.AND.
     X     B(I,JM).NE.ZIP)
   45 CONTINUE
      RETURN

   50 CONTINUE
C-----(DDJ-DDI): DF1/DJ-DF2/DI

      DO  55 J=J1,J2
          JM=J-1
          JP=J+1
          DO  55 I=I1,I2
            IF(LATLON) THEN
               LATVAL = CSP(1,2) + (J-1)*CSP(3,2)
               COSLAT = COS(LATVAL * DEGRAD)
               SX = EARTH_RADIANS * COSLAT
               SY = EARTH_RADIANS
            ELSE
               SX = 1
               SY = 1
            END IF
            DXI=1./(DX * SX)
            DYI=1./(DY * SY)
            IM=I-1
            IP=I+1
        VEX(I,J)=CVMGT((A(I,JP)-A(I,JM))*DYI-(B(IP,J)-B(IM,J))*DXI,ZIP,
     X     A(I,JP).NE.ZIP.AND.A(I,JM).NE.ZIP.AND.B(IP,J).NE.ZIP.AND.
     X     B(IM,J).NE.ZIP)
   55 CONTINUE
      RETURN

   60 CONTINUE
C-----(*D/DI): F1*DF2/DI

      DO  65 I=I1,I2
          IM=I-1
          IP=I+1
          DO  65 J=J1,J2
            IF(LATLON) THEN
               LATVAL = CSP(1,2) + (J-1)*CSP(3,2)
               COSLAT = COS(LATVAL * DEGRAD)
               SX = EARTH_RADIANS * COSLAT
            ELSE 
               SX = 1
            END IF
            DXI=1./(DX * SX)
         VEX(I,J)=CVMGT(A(I,J)*(B(IP,J)-B(IM,J))*DXI,ZIP,
     X            A(I,J).NE.ZIP.AND.B(IP,J).NE.ZIP.AND.B(IM,J).NE.ZIP)
   65 CONTINUE
      RETURN

   70 CONTINUE
C-----(*/DJ): F1*DF2/DJ

      DO  75 J=J1,J2
      JM=J-1
      JP=J+1
      DO  75 I=I1,I2
          IF(LATLON) THEN
            SY = EARTH_RADIANS  
          ELSE
            SY = 1
          END IF       
         DYI=1./(DY * SY)
         VEX(I,J)=CVMGT(A(I,J)*(B(I,JP)-B(I,JM))*DYI,ZIP,
     X            A(I,J).NE.ZIP.AND.B(I,JP).NE.ZIP.AND.B(I,JM).NE.ZIP)
   75 CONTINUE
      RETURN

   80 CONTINUE
C-----(DDI*DDJ): (DF1/DI)*(DF2/DJ)

      DO  85 J=J1,J2
         JM=J-1
         JP=J+1
         DO  85 I=I1,I2
            IF(LATLON) THEN
               LATVAL = CSP(1,2) + (J-1)*CSP(3,2)
               COSLAT = COS(LATVAL * DEGRAD)
               SX = EARTH_RADIANS * COSLAT
               SY = EARTH_RADIANS
            ELSE
               SX = 1
               SY = 1
            END IF
            DXI=1./(DX * SX)
            DYI=1./(DY * SY)
      IM=I-1
      IP=I+1
        VEX(I,J)=CVMGT((A(IP,J)-A(IM,J))*DXI*(B(I,JP)-B(I,JM))*DYI,ZIP,
     X     A(IP,J).NE.ZIP.AND.A(IM,J).NE.ZIP.AND.B(I,JP).NE.ZIP.AND.
     X     B(I,JM).NE.ZIP)
   85 VEX(I,J)=VX
      RETURN

   90 CONTINUE
C-----(*): C1*F1*F2

      DO 95  J=J1,J2
      DO 95  I=I1,I2
         VEX(I,J)=CVMGT(C1*A(I,J)*B(I,J),ZIP,
     X            A(I,J).NE.ZIP.AND.B(I,J).NE.ZIP)
   95 CONTINUE
      RETURN

  100 CONTINUE
C-----(SQRT): SQRT(F1)

      DO 105 J=J1,J2
      DO 105 I=I1,I2
      VX=ZIP
      IF(A(I,J).LT.0.0.OR.A(I,J).EQ.ZIP) GO TO 105
      VX=0.0
      IF(A(I,J).NE.0.0) VX=SQRT(A(I,J))
  105 VEX(I,J)=VX
      RETURN

  110 CONTINUE
C-----(TENLOG): C1*10**(C2*F1)

      DO 115 J=J1,J2
      DO 115 I=I1,I2
      VX=ZIP
        IF(A(I,J).NE.ZIP) VX=C1*10.**(C2*A(I,J))
  115 VEX(I,J)=VX
      RETURN

  120 CONTINUE
C-----(ORELSE):

      DO 125 J=J1,J2
      DO 125 I=I1,I2
         VEX(I,J)=CVMGT(B(I,J),A(I,J),A(I,J).EQ.ZIP)
  125 CONTINUE
      RETURN

  130 CONTINUE
C-----(SQ+SQ): F1**2+F2**2

      DO 135 J=J1,J2
      DO 135 I=I1,I2
         VEX(I,J)=CVMGT(A(I,J)*A(I,J)+B(I,J)*B(I,J),ZIP,
     X            A(I,J).NE.ZIP.AND.B(I,J).NE.ZIP)
  135 CONTINUE
      RETURN

  140 CONTINUE
C-----(RHOWGT): F1*C1*EXP**(-C2*Z)

      DO 145 J=J1,J2
      Z=CVMGT(CSP(1,3)+(J-1)*CSP(3,3),DCON,K3.NE.3)
      RHO=C1*EXP(-C2*Z)
      DO 145 I=I1,I2
         VEX(I,J)=CVMGT(RHO*A(I,J),ZIP,A(I,J).NE.ZIP)
  145 CONTINUE
      RETURN

  150 CONTINUE
C-----(+): C1*F1+C2*F2

      DO 155 J=J1,J2
      DO 155 I=I1,I2
         VEX(I,J)=CVMGT(C1*A(I,J)+C2*B(I,J),ZIP,
     X            A(I,J).NE.ZIP.AND.B(I,J).NE.ZIP)
  155 CONTINUE
      RETURN

  160 CONTINUE
C-----(-): C1*F1-C2*f2

      DO 165 J=J1,J2
      DO 165 I=I1,I2
         VEX(I,J)=CVMGT(C1*A(I,J)-C2*B(I,J),ZIP,
     X            A(I,J).NE.ZIP.AND.B(I,J).NE.ZIP)
  165 CONTINUE
      RETURN

  170 CONTINUE
C-----(/): C1*F1/F2

      DO 175 J=J1,J2
      DO 175 I=I1,I2
      VX=ZIP
        IF(A(I,J).NE.ZIP.AND.B(I,J).NE.ZIP.AND.B(I,J).NE.0.0)
     X                          VX=C1*A(I,J)/B(I,J)
  175 VEX(I,J)=VX
      RETURN

  180 CONTINUE
C-----(RHONORM): F1/(C1*EXP(-C2*Z))

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
C-----(LINEAR): C1*F1+C2


      DO 195 J=J1,J2
      DO 195 I=I1,I2
         VEX(I,J)=CVMGT(C1*A(I,J)+C2,ZIP,
     X            A(I,J).NE.ZIP)
  195 CONTINUE
      RETURN

  200 CONTINUE
C-----(ABS): |F1|

      DO 205 J=J1,J2
      DO 205 I=I1,I2
         VEX(I,J)=CVMGT(ABS(A(I,J)),ZIP,
     X            A(I,J).NE.ZIP)
  205 CONTINUE
      RETURN

  210 CONTINUE
C-----(ONLYIF): F1 only if F2 .ne. BAD

      DO 215 J=J1,J2
      DO 215 I=I1,I2
         VEX(I,J)=CVMGT(A(I,J),ZIP,
     X            B(I,J).NE.ZIP)
  215 CONTINUE
      RETURN

  220 CONTINUE
C-----(ONLYIFC>): F1 only if F2 .lt. C1

      DO 225 J=J1,J2
      DO 225 I=I1,I2
         VEX(I,J)=CVMGT(A(I,J),ZIP,
     X            B(I,J).NE.ZIP.AND.C1.GT.B(I,J))
  225 CONTINUE
      RETURN

  230 CONTINUE
C-----(ONLYIFC<): F1 only if F2 .gt. C1

      DO 235 J=J1,J2
      DO 235 I=I1,I2
         VEX(I,J)=CVMGT(A(I,J),ZIP,
     X            B(I,J).NE.ZIP.AND.C1.LT.B(I,J))
  235 CONTINUE
      RETURN

  240 CONTINUE
C-----(LN): C1*ln(C2*F1)

      DO 245 J=J1,J2
      DO 245 I=I1,I2
      VX=ZIP
      C2AIJ=C2*A(I,J)
      IF(C2AIJ.EQ.0.0.AND.A(I,J).NE.ZIP) VX= ZIP
        IF(C2AIJ.GT.0.0.AND.A(I,J).NE.ZIP) VX=C1*ALOG(C2AIJ)
  245 VEX(I,J)=VX
      RETURN

  250 CONTINUE
C-----(EXP): C1*(exp**F1)

      DO 255 J=J1,J2
      DO 255 I=I1,I2
      VX=ZIP
        IF(A(I,J).NE.ZIP) VX=C1*EXP(A(I,J))
  255 VEX(I,J)=VX
      RETURN

  260 CONTINUE
C-----(POWER): C1*(F1**C2)

      DO 265 J=J1,J2
      DO 265 I=I1,I2
      VX=ZIP
        IF(A(I,J).NE.ZIP.AND.A(I,J).GE.0.0) VX=C1*(A(I,J)**C2)
  265 VEX(I,J)=VX
      RETURN

  270 CONTINUE
C-----(RELPLANE): F1 at (Current level + C1)
      RETURN

  280 CONTINUE
C-----(XYDIST): SQRT[(X-C1)**2 + (Y-C2)**2] - BRANCH TO GENERAL CALCULATION
      GO TO 380

  290 CONTINUE
C-----(FUNZ): C1*Z+C2

      DO 295 J=J1,J2
      Z=CVMGT(CSP(1,3)+(J-1)*CSP(3,3),DCON,K3.NE.3)
      CON=C1*Z+C2
      DO 295 I=I1,I2
  295 VEX(I,J)=CON
      RETURN

  300 CONTINUE
C-----(SIN): sin(C1*F1 - C2), F1 in degrees

      DO 305 J=J1,J2
      DO 305 I=I1,I2
      VX=ZIP
        IF(A(I,J).NE.ZIP) VX=SIN(C1*ATR*A(I,J)-(C2*ATR))
  305 VEX(I,J)=VX
      RETURN

  310 CONTINUE
C-----(ONLYIFNO): F1 only if F2=BAD

      DO 315 J=J1,J2
      DO 315 I=I1,I2
         VEX(I,J)=CVMGT(A(I,J),ZIP,
     X            B(I,J).EQ.ZIP)
  315 CONTINUE
      RETURN

  320 CONTINUE
C-----(RANGE): SQRT[(X-C1)**2+(Y-C2)**2+(Z-C3)] -BRANCH TO GENERAL CALCULATION

      GO TO 380

  330 CONTINUE
C-----(COS): cos(C1*F1 - C2), F1 in degress

      DO 335 J=J1,J2
      DO 335 I=I1,I2
      VX=ZIP
        IF(A(I,J).NE.ZIP) VX=COS(C1*ATR*A(I,J)-(C2*ATR))
  335 VEX(I,J)=VX
      RETURN

  340 CONTINUE
C-----(RANDOM): F1 from uniform random number in (C1,C2) interval

      IWORD = CRAND(IR1)
      IR1=IR1+2
      CON=C2-C1
      DO 345 J=J1,J2
      DO 345 I=I1,I2
      VX=CRAND(0)
  345 VEX(I,J)=CON*VX+C1
      RETURN

  350 CONTINUE
C-----(MAX): max(F1,F2)

      DO 355 J=J1,J2
      DO 355 I=I1,I2
         VEX(I,J)=CVMGT(AMAX1(A(I,J),B(I,J)),ZIP,
     X                  A(I,J).NE.ZIP.AND.B(I,J).NE.ZIP)
  355 CONTINUE
      RETURN

  360 CONTINUE
C-----(MIN): min(F1,F2)
      DO 365 J=J1,J2
      DO 365 I=I1,I2
         VEX(I,J)=CVMGT(AMIN1(A(I,J),B(I,J)),ZIP,
     X                  A(I,J).NE.ZIP.AND.B(I,J).NE.ZIP)
  365 CONTINUE
      RETURN

  370 CONTINUE
C-----(ATAN2): inverse tan(F2/F1)

      DO 375 J=J1,J2
      DO 375 I=I1,I2
      VX=ZIP
        AIJ=A(I,J)
        BIJ=B(I,J)
         IF(AIJ.EQ.ZIP.OR.BIJ.EQ.ZIP) GO TO 374
         VX=0.0
         IF(AIJ.NE.0.0.OR.BIJ.NE.0.0) VX=RTA*ATAN2(BIJ,AIJ)
         IF(VX.LT.0.0)VX=VX+360.0
  374 VEX(I,J)=VX
  375 CONTINUE
      RETURN

  380 CONTINUE
C-----GENERAL CALCULATION FOR RADAR GEOMETRY
C     #28-(XYDIST), #32-(RANGE), #38-(AZ), and #39-(EL)
C
      IF(LATLON) THEN
         REFLAT = ID(33)+(ID(34)/60.)+(ID(35)/(3600.*ID(68)))
         RDEG = ID(36)
         RMIN = ID(37)/60.
         RSEC = ID(38)/(3600.*ID(68))
         REFLON = ABS(RDEG) + ABS(RMIN) + ABS(RSEC)
      END IF

      ASNF=0.0
      ACSF=1.0
      XORTR=CONEXT(2)-C1
      YORTR=CONEXT(3)-C2

      DO 385 J=J1,J2
         Z=CVMGT(CSP(1,3)+(J-1)*CSP(3,3),DCON,K3.NE.3)
         YTMP=CVMGT(CSP(1,2)+(J-1)*CSP(3,2),DCON,K3.EQ.3)
         DO 384 I=I1,I2
            IF(LATLON) THEN
               LONVALN  = CSP(1,1) + (I-1)*CSP(3,1)
               LATVALN  = CSP(1,2) + (J-1)*CSP(3,2)
               CALL LL2XYDRV(LATVALN,LONVALN,XVAL,YVAL,REFLAT,
     X                       REFLON,ANGXAX)
               X=XVAL
               Y=YVAL
            ELSE
               X=CVMGT(CSP(1,1)+(I-1)*CSP(3,1),DCON,K3.NE.1)
               Y=CVMGT(CSP(1,2)+(I-1)*CSP(3,2),YTMP,K3.EQ.1)
            END IF
            XT=ACSF*X-ASNF*Y+XORTR
            YT=ASNF*X+ACSF*Y+YORTR
            S2=XT*XT+YT*YT
            HT=SQRT(S2)
            IF(IFLAT.EQ.1)THEN
               ZCOR=0.0
            ELSE
               ZCOR=S2*AKZI
            END IF

            IF (AXNAM(3).EQ.'E')THEN
               ZRTR=HT*TCON-C3
            ELSE
               ZRTR=Z-C3
            ENDIF
            ZRTR=ZRTR-ZCOR
            ZRTRSQ=ZRTR*ZRTR
            
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
               ZADJ=ZRTR +(ZRTRSQ-RNGSQ)*AKZI
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
C-----(CON): constant

      DO 405 J=J1,J2
      DO 405 I=I1,I2
  405 VEX(I,J)=C1
      RETURN

  410 CONTINUE
C-----(BAD): bad value

      DO 415 J=J1,J2
      DO 415 I=I1,I2
  415 VEX(I,J)=ZIP
      RETURN

  420 CONTINUE
C-----(FUNX): C1*X+C2

      DO 425 I=I1,I2
      X=CVMGT(CSP(1,1)+(I-1)*CSP(3,1),DCON,K3.NE.1)
      CON=C1 * X +C2
      DO 425 J=J1,J2
  425 VEX(I,J)=CON
      RETURN

  430 CONTINUE
C-----(FUNY): C1*Y+C2

      DO 435 J=J1,J2
      CON=CVMGT(CSP(1,2)+(J-1)*CSP(3,2),DCON,K3.EQ.3)
      DO 435 I=I1,I2
      Y=CVMGT(CSP(1,2)+(I-1)*CSP(3,2),CON,K3.EQ.1)
  435 VEX(I,J)=C1 * Y +C2
      RETURN

  440 CONTINUE
C-----(PROFILE): C1*[areal mean(F1)]

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
C-----(LOGTEN): linear to log => C1*log(C2*F1)

      DO 455 J=J1,J2
      DO 455 I=I1,I2
      VX=ZIP
      C2AIJ=C2*A(I,J)
         IF(C2AIJ.EQ.0.0.AND.A(I,J).NE.ZIP) VX= ZIP
        IF(C2AIJ.GT.0.0.AND.A(I,J).NE.ZIP) VX=C1*ALOG10(C2AIJ)
  455 VEX(I,J)=VX
      RETURN

  460 CONTINUE
C-----(MODULO): mod(F1,C1) = 0 when F1=C1

      IF(C1.EQ.0.0) C1=1.0
      DO 465 J=J1,J2
      DO 465 I=I1,I2
      VX=ZIP
         IF(A(I,J).NE.ZIP) VX=AMOD(A(I,J),C1)
  465 VEX(I,J)=VX
      RETURN

  470 CONTINUE
C-----(FLOOR): F1, only if F1 > C1; otherwise C1

      DO 475 J=J1,J2
      DO 475 I=I1,I2
         VEX(I,J)=CVMGT(AMAX1(A(I,J),C1),ZIP,A(I,J).NE.ZIP)
  475 CONTINUE
      RETURN

  480 CONTINUE
C-----(CEILING): F1, only if F1 < C1; otherwise C1

      DO 485 J=J1,J2
      DO 485 I=I1,I2
         VEX(I,J)=CVMGT(AMIN1(A(I,J),C1),ZIP,A(I,J).NE.ZIP)
  485 CONTINUE
      RETURN

  490 CONTINUE
C-----(PROJECT): project components (U,V)=(F1,F2) onto direction C1

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
C-----(BITCOUNT): counts 1's within F1

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
C-----(NORMAL): F1 from normal distribution with mean (C1) and stdev (C2)

      IWORD= CRAND(IR1)
      IR1=IR1+2
      IF(C2.LE.0.0) C2=1.0
      PI2=ATAN(1.)*8.0
      DO 515 J=J1,J2
      DO 515 I=I1,I2
         U1=CRAND(0)
         U2=CRAND(0)
         IF (U1.LT.EPS2) U1=EPS2
         VX= SQRT(-2.0*ALOG(U1)) * COS(PI2*U2)
  515 VEX(I,J)= C2*VX + C1
      RETURN

  520 CONTINUE
C-----(PROFMIN): set all in fixed axis direction to min(F1)

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
C-----(PROFMAX): set all in fixed axis direction to max(F1)

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
C-----(IFNOC1C2): F1, if F2 .ne. C1; otherwise C2

      DO 545 J=J1,J2
      DO 545 I=I1,I2
  545 VEX(I,J)=CVMGT(C2,A(I,J),B(I,J).EQ.C1)
      RETURN

  550 CONTINUE
C-----(INSIDE): F1, if C1 .le. F2 .le. C2

      DO 555 J=J1,J2
      DO 555 I=I1,I2
  555 VEX(I,J)=CVMGT(ZIP,A(I,J),
     X               B(I,J).LT.C1.OR.B(I,J).GT.C2.OR.B(I,J).EQ.ZIP)
      RETURN

  560 CONTINUE
C-----(OUTSIDE): F1, if F2 .lt. C1 or F2 .gt. C2

      DO 565 J=J1,J2
      DO 565 I=I1,I2
  565 VEX(I,J)=CVMGT(ZIP,A(I,J),
     X              (B(I,J).GE.C1.AND.B(I,J).LE.C2).OR.B(I,J).EQ.ZIP)
      RETURN

  570 CONTINUE
C-----(COUNT): C1*[number of good values(F1)]

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
C-----(MEANAX1): At J, compute mean along I of F1 and store at each I

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
C-----(MEANAX2): At I, compute mean along J of F1 and store at each I

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
C-----(RELI): shift field F1 in I direction by C1 grid points

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
C-----(RELJ): shift field F1 in J direction by C1 grid points

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
C-----(RELIJ): shift field F1 in I,J direction by C1,C2 grid points

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
C-----[LAT(X,Y)]: CREATE A LATITUDE FIELD FROM CARTESIAN (X,Y)

      IF (C1.EQ.0.0 .AND. C2.EQ.0.0) THEN
         REFLAT=ABS(ID(33)+(ID(34)/60.)+(ID(35)/3600.))
         RDEG = ID(36)
         RMIN = ID(37)/60.
         RSEC = ID(38)/(3600.*ID(68))
         REFLON = ABS(RDEG) + ABS(RMIN) + ABS(RSEC)
      ELSE
         REFLAT=C1
         REFLON=C2
      END IF

      DO J=J1,J2
         DO I=I1,I2
            XVAL=CSP(1,1)+(I-1)*CSP(3,1)
            YVAL=CSP(1,2)+(J-1)*CSP(3,2)
            CALL XY2LLDRV(XLAT,XLON,XVAL,YVAL,REFLAT,REFLON,
     X           ANGXAX)
            VEX(I,J)=XLAT
         END DO
      END DO
      RETURN

 640  CONTINUE
C-----[LON(X,Y)]: CREATE A LONGITUDE FIELD FROM CARTESIAN (X,Y)

      IF (C1.EQ.0.0 .AND. C2.EQ.0.0) THEN
         REFLAT=ABS(ID(33)+(ID(34)/60.)+(ID(35)/3600.))
         RDEG = ID(36)
         RMIN = ID(37)/60.
         RSEC = ID(38)/(3600.*ID(68))
         REFLON = ABS(RDEG) + ABS(RMIN) + ABS(RSEC)
      ELSE
         REFLAT=C1
         REFLON=C2
      END IF

      DO J=J1,J2
         DO I=I1,I2
            XVAL=CSP(1,1)+(I-1)*CSP(3,1)
            YVAL=CSP(1,2)+(J-1)*CSP(3,2)
            CALL XY2LLDRV(XLAT,XLON,XVAL,YVAL,REFLAT,REFLON,
     X           ANGXAX)
            VEX(I,J)=XLON
         END DO
      END DO
      RETURN

 650  CONTINUE
C-----(SDEVAX1): compute std dev along I holding J,K fixed

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
C-----(SDEVAX2): compute std dev along J holding I,K fixed

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

 670  CONTINUE
C-----(XCART): CREATE A CARTESIAN X-COORDINATE WRT (C1,C2) FROM (LAT,LON) GRID

      IF (C1.EQ.0.0 .AND. C2.EQ.0.0) THEN
         REFLAT=ABS(ID(33)+(ID(34)/60.)+(ID(35)/(3600. * ID(68))))
         RDEG = ID(36)
         RMIN = ID(37)/60.
         RSEC = ID(38)/(3600.*ID(68))
         REFLON = ABS(RDEG) + ABS(RMIN) + ABS(RSEC)
      ELSE
         REFLAT=C1
         REFLON=C2
      END IF

      DO J=J1,J2
         DO I=I1,I2
            LONVALN = CSP(1,1)+(I-1)*CSP(3,1)
            LATVALN = CSP(1,2)+(J-1)*CSP(3,2)
            CALL LL2XYDRV(LATVALN,LONVALN,XVAL,YVAL,REFLAT,
     X                     REFLON,ANGXAX)
            VEX(I,J)=XVAL
         END DO
      END DO      
      RETURN

 680  CONTINUE
C-----(YCART): CREATE A CARTESIAN Y-COORDINATE WRT (C1,C2) FROM (LAT,LON) GRID

      IF (C1.EQ.0.0 .AND. C2.EQ.0.0) THEN
         REFLAT=ABS(ID(33)+(ID(34)/60.)+(ID(35)/(3600.*ID(68))))
         RDEG = ID(36)
         RMIN = ID(37)/60.
         RSEC = ID(38)/(3600.*ID(68))
         REFLON = ABS(RDEG) + ABS(RMIN) + ABS(RSEC)
      ELSE
         REFLAT=C1
         REFLON=C2
      END IF

      DO J=J1,J2
         DO I=I1,I2
            LONVALN = CSP(1,1)+(I-1)*CSP(3,1)
            LATVALN = CSP(1,2)+(J-1)*CSP(3,2)
            CALL LL2XYDRV(LATVALN,LONVALN,XVAL,YVAL,REFLAT,
     X                     REFLON,ANGXAX)
            VEX(I,J)=YVAL
         END DO
      END DO    
      RETURN

 690  CONTINUE
C-----(USTOPO): CREATE US TOPOGRAPHIC HEIGHTS (decameters) - will open topo.dat

      IF (C1.EQ.0.0 .AND. C2.EQ.0.0) THEN
         REFLAT=ABS(ID(33)+(ID(34)/60.)+(ID(35)/(3600.*ID(68))))
         RDEG = ID(36)
         RMIN = ID(37)/60.
         RSEC = ID(38)/(3600.*ID(68))
         REFLON = ABS(RDEG) + ABS(RMIN) + ABS(RSEC)
      ELSE
         REFLAT=C1
         REFLON=C2
      END IF

      CALL USTOPO(VEX,NI,NJ,I1,I2,J1,J2,CSP,LATLON,REFLAT,REFLON,
     X     ANGXAX,ZIP)
      RETURN

 700  CONTINUE
C-----(ZINDEX): C1*ZINDEX+C2

      Z=LEV
      CON=C1*Z+C2
      DO 705 J=J1,J2
      DO 705 I=I1,I2
  705 VEX(I,J)=CON
      RETURN

 710  CONTINUE
C-----(ZCART): CREATE A CARTESIAN Z-COORDINATE WRT (C1,C2,C3) FROM (LAT,LON) GRID

      IF (C1.EQ.0.0 .AND. C2.EQ.0.0) THEN
         REFLAT=ABS(ID(33)+(ID(34)/60.)+(ID(35)/(3600. * ID(68))))
         RDEG = ID(36)
         RMIN = ID(37)/60.
         RSEC = ID(38)/(3600.*ID(68))
         REFLON = ABS(RDEG) + ABS(RMIN) + ABS(RSEC)
      ELSE
         REFLAT=C1
         REFLON=C2
      END IF

      IF(C3.EQ.0.0)THEN
         Z0=ZRAD
      ELSE
         Z0=C3
      END IF

      DO J=J1,J2
         DO I=I1,I2
            LONVALN = CSP(1,1)+(I-1)*CSP(3,1)
            LATVALN = CSP(1,2)+(J-1)*CSP(3,2)
            CALL LL2XYDRV(LATVALN,LONVALN,XVAL,YVAL,REFLAT,
     X                     REFLON,ANGXAX)
            S2=XVAL*XVAL+YVAL*YVAL
            HT=SQRT(S2)
            IF(IFLAT.EQ.1)THEN
               ZCOR=0.0
            ELSE
               ZCOR=S2*AKZI
            END IF
            IF (AXNAM(3).EQ.'E')THEN
               ZRTR=HT*TCON+Z0
            ELSE
               ZRTR=Z+Z0
            ENDIF
            ZRTR=ZRTR+ZCOR
            VEX(I,J)=ZRTR
         END DO
      END DO      
      RETURN

 720  CONTINUE
C-----(VADFLD): COMPUTE RADIAL VELOCITIES FROM A PREVIOUS VAD ANALYSIS
C-----GENERAL CALCULATION FOR RADAR GEOMETRY
C     #28-(XYDIST), #32-(RANGE), #38-(AZ), and #39-(EL)
C
      print *,'VADFLD: cona = ',(cona(i),i=1,4)
      print *,'      conext = ',conext
      print *,'    id(33-35)= ',id(33),id(34),id(35)
      print *,'    id(36-38)= ',id(36),id(37),id(38)
      print *,'        axnam= ',axnam
      print *,' rbeg-end-del= ',rbeg,rend,rdel
      print *,'   vtype,kfit= ',vtype,kfit
      print *,'lev,dcon,rta,atr= ',lev,dcon,rta,atr
      do ir=1,50
         write(6,1700)lev,ir,u0(ir,lev),v0(ir,lev),div(ir,lev),
     +        err(ir,lev)
 1700    format('lev,ir,uvde=',2i4,4f8.3)
      end do
      IF(LATLON) THEN
         REFLAT = ID(33)+(ID(34)/60.)+(ID(35)/(3600.*ID(68)))
         RDEG = ID(36)
         RMIN = ID(37)/60.
         RSEC = ID(38)/(3600.*ID(68))
         REFLON = ABS(RDEG) + ABS(RMIN) + ABS(RSEC)
      END IF

      ASNF=0.0
      ACSF=1.0
      XORTR=CONEXT(2)-C1
      YORTR=CONEXT(3)-C2

      DO 725 J=J1,J2
         Z=CVMGT(CSP(1,3)+(J-1)*CSP(3,3),DCON,K3.NE.3)
         YTMP=CVMGT(CSP(1,2)+(J-1)*CSP(3,2),DCON,K3.EQ.3)
         DO 724 I=I1,I2
            VEX(I,J)=ZIP
            IF(LATLON) THEN
               LONVALN  = CSP(1,1) + (I-1)*CSP(3,1)
               LATVALN  = CSP(1,2) + (J-1)*CSP(3,2)
               CALL LL2XYDRV(LATVALN,LONVALN,XVAL,YVAL,REFLAT,
     X                       REFLON,ANGXAX)
               X=XVAL
               Y=YVAL
            ELSE
               X=CVMGT(CSP(1,1)+(I-1)*CSP(3,1),DCON,K3.NE.1)
               Y=CVMGT(CSP(1,2)+(I-1)*CSP(3,2),YTMP,K3.EQ.1)
            END IF
            XT=ACSF*X-ASNF*Y+XORTR
            YT=ASNF*X+ACSF*Y+YORTR
            S2=XT*XT+YT*YT
            HT=SQRT(S2)
            IF(IFLAT.EQ.1)THEN
               ZCOR=0.0
            ELSE
               ZCOR=S2*AKZI
            END IF

            IF (AXNAM(3).EQ.'E')THEN
               ZRTR=HT*TCON-C3
            ELSE
               ZRTR=Z-C3
            ENDIF
            ZRTR=ZRTR-ZCOR
            ZRTRSQ=ZRTR*ZRTR
            
C        (X,Y) DISTANCE
            HRNG=SQRT(XT*XT+YT*YT)
C        RANGE CALCULATION
            SRNG=SQRT(XT*XT+YT*YT+ZRTRSQ)
C        AZIMUTH
            VX=0.0
            IF(XT.NE.0.0.OR.YT.NE.0.0)VX=ATAN2(XT,YT)*RTA
            IF(VX.LT.0.0) VX=VX+360.0
            AZM=VX
C        ELEVATION
            RNGSQ=XT*XT+YT*YT+ZRTRSQ
            RNG=SQRT(RNGSQ)
            ZADJ=ZRTR +(ZRTRSQ-RNGSQ)*AKZI
            IF(RNG .GT. EPS) THEN
               ARC=ZADJ/RNG
               IF(ABS(ARC).LE.1.0) VX=RTA*ASIN(ARC)
            ELSE
               VX = 0.0
            END IF
            ELEV=VX
C     IR - vertical profile index which needs to be calculated
C
            IR=NINT(1.0+(SRNG-RBEG)/RDEL)
            IF(IR.GT.NR)GO TO 724
            UE = U0(IR,LEV)
            VE = V0(IR,LEV)
            DIVL = DIV(IR,LEV)
            STRL = STR(IR,LEV)
            SHRL = SHR(IR,LEV)
            VORL = 0.0

            ANGR=AZM*ATR
            SINAZ = SIN(ANGR)
            COSAZ = COS(ANGR)
            COSE = COS(ELEV*ATR)
            
            VRVAD=ZIP
c            IF(VTYPE(1:4).EQ.'FOUR')THEN
c               A0 = AVAD0(IR,LEV)
c               IF(A0.NE.BDVAL)THEN
c                  VRVAD=A0
c                  DO K=1,KFIT
c                     AK(K)=AVAD(IR,LEV,K)
c                     BK(K)=BVAD(IR,LEV,K)
c                     VRVAD=VRVAD+AK(K)*COS(ANGR*K)+BK(K)*SIN(ANGR*K)
c                  END DO
c               END IF
c            ELSE IF(VTYPE(1:4).EQ.'LSQR')THEN
c               IF(UE.NE.ZIP)THEN
c                  UXY=UE+0.5*(DIVL+STRL)*X
c     X                  +0.5*(SHRL-VORL)*Y
c                  VXY=VE+0.5*(SHRL+VORL)*X
c     X                  +0.5*(DIVL-STRL)*Y
c                  VRVAD=(UXY*SINAZ+VXY*COSAZ)*COSE
c                END IF
c            END IF
            IF(UE.NE.ZIP)THEN
               UXY=UE+0.5*(DIVL+STRL)*X
     X              +0.5*(SHRL-VORL)*Y
               VXY=VE+0.5*(SHRL+VORL)*X
     X              +0.5*(DIVL-STRL)*Y
               VRVAD=(UXY*SINAZ+VXY*COSAZ)*COSE
            END IF
            VEX(I,J)=VRVAD
            write(6,1705)lev,ir,x,y,hrng,srng,azm,elev,ue,ve,
     +           divl,strl,shrl,vorl,vrvad
 1705       format('lv,ir,xyhrae=',2i4,6f7.2,' uvdthv=',6f7.2,
     +           ' vr=',f7.2)
  724    CONTINUE
  725 CONTINUE
      RETURN

 730  CONTINUE
C-----(SOUND): Extract fields from a previously read MGLASS sounding
C*-------*-------*-------*-------*-------*-------*-------*-------*-------
CFUNCTIONNAME    1.0                                                     FULL
C********NAMOUT  P       FIELD   NGRD            C1      C2      C3      C4
C        Vrad    SOUND   RADVEL                  X0      Y0      Z0      0.0
C        Vrad    SOUND   RADVEL                                          0.0
C        Temp    SOUND   TEMP
C        Uenv    SOUND   U
C        Venv    SOUND   V
CEND
C*-------*-------*-------*-------*-------*-------*-------*-------*-------
C
C     Find the index (IFUN) of for the SOUNDing function (SNDNAME)
C     within a long list (LFUN) of length MXFUN.
C
      IFUN=PPI_IFIND(SNDNAME,LFUN,MXFUN)
      print *,'CALL SOUND: sndname,ifun=',sndname,ifun
      print *,'               ni,nj,zip=',ni,nj,zip
      print *,'             i1,i2,j1,j2=',i1,i2,j1,j2
      print *,'                c1,c2,c3=',c1,c2,c3
      print *,'                axnam(3)=',axnam(3)

      IF(IFUN.EQ.0)THEN
         PRINT *,'*** ERROR - UNKNOWN SOUNDING FIELD ***'
         STOP
      END IF

C-----Calculate MSL height for current (X,Y) location as input to SOUND
C
      IF(LATLON) THEN
         REFLAT = ID(33)+(ID(34)/60.)+(ID(35)/(3600.*ID(68)))
         RDEG = ID(36)
         RMIN = ID(37)/60.
         RSEC = ID(38)/(3600.*ID(68))
         REFLON = ABS(RDEG) + ABS(RMIN) + ABS(RSEC)
      END IF

      ASNF=0.0
      ACSF=1.0
      XORTR=CONEXT(2)-C1
      YORTR=CONEXT(3)-C2

      DO J=J1,J2
         Z=CVMGT(CSP(1,3)+(J-1)*CSP(3,3),DCON,K3.NE.3)
         YTMP=CVMGT(CSP(1,2)+(J-1)*CSP(3,2),DCON,K3.EQ.3)
         DO I=I1,I2
            IF(LATLON) THEN
               LONVALN  = CSP(1,1) + (I-1)*CSP(3,1)
               LATVALN  = CSP(1,2) + (J-1)*CSP(3,2)
               CALL LL2XYDRV(LATVALN,LONVALN,XVAL,YVAL,REFLAT,
     X                       REFLON,ANGXAX)
               X=XVAL
               Y=YVAL
            ELSE
               X=CVMGT(CSP(1,1)+(I-1)*CSP(3,1),DCON,K3.NE.1)
               Y=CVMGT(CSP(1,2)+(I-1)*CSP(3,2),YTMP,K3.EQ.1)
            END IF
            XT=ACSF*X-ASNF*Y+XORTR
            YT=ASNF*X+ACSF*Y+YORTR
            S2=XT*XT+YT*YT
            HT=SQRT(S2)
            AZ=0.0
            IF(XT.NE.0.0.OR.YT.NE.0.0)AZ=ATAN2(XT,YT)*RTA
            IF(AZ.LT.0.0) AZ=AZ+360.0
            IF(IFLAT.EQ.1)THEN
               ZCOR=0.0
            ELSE
               ZCOR=S2*AKZI
            END IF

            IF (AXNAM(3).EQ.'E')THEN
               ZRTR=HT*TCON+C3
               ZRTR=ZRTR-ZCOR
               EL=Z
            ELSE
               ZRTR=Z
               ZRTRSQ=(Z-C3)*(Z-C3)
               RNGSQ=XT*XT+YT*YT+ZRTRSQ
               RNG=SQRT(RNGSQ)
               ZADJ=ZRTR-C3 +(ZRTRSQ-RNGSQ)*AKZI
               IF(RNG .GT. EPS) THEN
                  ARC=ZADJ/RNG
                  IF(ABS(ARC).LE.1.0) EL=RTA*ASIN(ARC)
               ELSE
                  EL = 0.0
               END IF
            ENDIF

            VEX(I,J)=ZIP
            CALL SOUND(IFUN,AZ,EL,ATR,ZRTR,ZIP,FZ)
c            print *,'Snding: x,y,z,fz=',xt,yt,zrtr,fz
            VEX(I,J)=FZ
           
         END DO
      END DO
      RETURN

  740 CONTINUE
C-----(+): THETA(T,P) - potential temperature (dry adiabat)
C                       T in deg K, P in mb

      DO 745 J=J1,J2
      DO 745 I=I1,I2
         VX=ZIP
         T=A(I,J)
         P=B(I,J)
         IF(T.EQ.ZIP .OR. P.EQ.ZIP)GO TO 745
         IF(P.GT.1000.0)P=1000.0
         VX=OD(T,P)
         VEX(I,J)=VX
  745 CONTINUE
      RETURN

  750 CONTINUE
C-----(+): THETA_E(T,P) - Equivalent potential temperature 
C                         (saturated adiabat)
C                         T in deg K, P in mb

      DO 755 J=J1,J2
      DO 755 I=I1,I2
         VX=ZIP
         T=A(I,J)
         P=B(I,J)
         IF(T.EQ.ZIP .OR. P.EQ.ZIP)GO TO 755
         IF(P.GT.1000.0)P=1000.0
         VX=OS(T,P)
         VEX(I,J)=VX
  755 CONTINUE
      RETURN

  760 CONTINUE
C-----(+): MIXRAT(T,P) - mixing ratio (g/kg)
C                       T in deg K, P in mb
      DO 765 J=J1,J2
      DO 765 I=I1,I2
         VX=ZIP
         T=A(I,J)
         P=B(I,J)
         IF(T.EQ.ZIP .OR. P.EQ.ZIP)GO TO 765
         IF(P.GT.1000.0)P=1000.0
         VX=W(T,P)
         VEX(I,J)=VX
  765 CONTINUE
      RETURN

  770 CONTINUE
C-----(+): TDRY(T,P) - dry air temperature (deg C)
C                      T in deg K, P in mb

      DO 775 J=J1,J2
      DO 775 I=I1,I2
         VX=ZIP
         T=A(I,J)
         P=B(I,J)
         IF(T.EQ.ZIP .OR. P.EQ.ZIP)GO TO 775
         IF(P.GT.1000.0)P=1000.0
         VX=TDRY(T,P)-ABSC
         VEX(I,J)=VX
  775 CONTINUE
      RETURN

      END
