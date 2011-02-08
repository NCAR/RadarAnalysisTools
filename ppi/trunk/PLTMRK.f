c
c----------------------------------------------------------------------X
c
      SUBROUTINE PLTMRK(NMRK,XMRK,YMRK,ZMRK,AMRK,IMRK,NET,NNET,SMRK,
     X     XMN,XMX,YMN,YMX,JMRK,IGRPLT,BGFLAG,OLAT,OLON,ANGXAX,CMRK,
     X     PLT_NMRK,PLT_TIME,REC_DATA,MXK,MXT,KMRK,HCIRC,NEX_DATE,
     X     HOUR_BEG,HOUR_END,NHOURS,PLTCLOCK,PERCENT,PLTPERCNT)
C
C  PLOT MESONETORK POSITIONS OVERLAID ON CONTOUR OR COLOR PLOTS, ONLY
C     IF THE MESONETWORK IS WITHIN THE PLOT SPACE (XMN,XMX,YMN,YMX)
C     COLOR FILL A BOX AND PLOT THE CHARACTER STRING INSIDE IT.
C
C     XMRK,YMRK,ZMRK  - STATION POSITION (KM,KM,KM)
C     AMRK            - Angular convergence relative to origin. (See GETMRK)
C     IMRK            - NUMBER OF MESONETWORK POSITIONS
C     JMRK            - (1) SYMBOL ONLY,  (2) NAME ONLY,  (3) BOTH
C                       (4) SYMBOL IN BOX,(5) NAME IN BOX,(6) BOTH IN BOX
C                       (7) SYMBOL PLUS TRUE NORTH LINE
C     CMRK            - Character size for station names.
C     FOR EACH OF (20) POSSIBLE NETWORKS:
C                 NET - NUMBER OF NETWORKS
C                NNET - NUMBER OF STATIONS IN NTH NETWORK
C                SMRK - PLOTTING SYMBOL (COMPLEX CHARACTER SET)
C
C     XMN,XMX,YMX,YMX - (X,Y) BOUNDS OF THE PLOT DOMAIN
C     ICOLBX          - COLOR INDICES FOR THE BOX AND CHARACTERS
C
C  24-HR CLOCK SHOWING DATA AVAILABILITY FROM NEXRAD INVENTORY FILE
C     See PLT_CLOCK for NEXRAD station clock.
C     See PLTNET    for simple circle around specified stations.
C
C     NEX_DATE - Date of requested NEXRAD data 
C     HOUR_BEG - Beginning hour of data
C     HOUR_END - Ending hour of data
C     NHOURS   - Number of hours covered by all radars 
C     PLT_NMRK - Specific landmark names to be plotted
C     PLT_TIME - Number of volume scans realized for each hour
C                of a 72 hour period for each NEXRAD ordered
C     REC_DATA - (Y) Some data was received, 
C                (N) No data was received, but it was ordered
C     HCIRC - Radius of 24-hr circle
C
      INCLUDE 'colors.inc'
      CHARACTER LAB1*1,LAB6*6,LAB7*7,LAB9*9,BGFLAG*1
      CHARACTER*7 NMRK(MXK),PLT_NMRK(MXK)
      CHARACTER*6 SMRK(20)
      CHARACTER*1 PLT_TIME(MXK,MXT),REC_DATA(MXK)
      CHARACTER*11 NEX_DATE
      INTEGER HOUR_BEG,HOUR_END

      CHARACTER*3 PERCENT(MXK)

      INTEGER GETLEN
      LOGICAL PLTNMRK,PLTCLOCK,PLTPERCNT

      COMMON/COTABLE/ICOL(100)
      COMMON /DASHPAT/IDPAT(5,3),JDPAT,LWSTART,LWINC

      DIMENSION XMRK(MXK),YMRK(MXK),ZMRK(MXK),AMRK(MXK)
      DIMENSION NNET(20)
      DIMENSION FXP(5),FYP(5)
      DIMENSION XCIRC(25),YCIRC(25),XFILL(4),YFILL(4)
      DATA TORAD,TODEG,RNG/0.017453293,57.29577951,111.137/
      DATA XSIZ/12.0/

      IF(HCIRC.EQ.0.0)THEN
c         HCIRC=50.0
         HCIRC=5.0
      END IF

C     CHANGE LINE AND TEXT COLORS AND WIDTHS, CHANGE TO
C     THE COMPLEX CHARACTER SET, AND TURN ON COLOR AREA FILL
C        IGRPLT - (0) NOT GRAYTONES, (1) GRAYTONES
C
      IF(CMRK.LE.0.0)THEN
         CSIZ=XSIZ
      ELSE
         CSIZ=CMRK
      END IF
      CALL GETUSV('LW',ILW)
      JLW=1000
      CALL SETLIN(IGRPLT,BGFLAG,IBLACK,IWHITE,JLW)

      IF(IGRPLT.NE.1)THEN
         IF(BGFLAG.EQ.'W')THEN
            ICOLBX=IBLACK
         ELSE
            ICOLBX=IWHITE
         END IF
      ELSE
         ICOLBX=IBLACK
      END IF
      CALL PCSETI ('CD',0)
      CALL GSFAIS(1)

C     NETWORK COUNTERS:
C        IN - NUMBER OF THE CURRENT NETWORK (IN=1,NET)
C        NN - NUMBER OF STATIONS WITHIN CURRENT NETWORK (NN=1,NNET(IN))
C        JN - COUNTER FOR STATION NUMBER WITHIN CURRENT NETWORK (JN=1,NN)
C
      IN=1
      JN=1
      NN=NNET(IN)

C     PLOT SYMBOLS AND NAMES INSIDE COLORED BOXES OR SYMBOLS AND NAMES ONLY
C
      IF(JMRK.GE.4.AND.JMRK.LE.6)THEN

C        USE FRACT COORD TO PLOT SYMBOLS AND NAMES INSIDE COLORED BOXES
C        JMRK - (4) SYMBOL ONLY, (5) NAME ONLY, (6) SYMBOL AND NAME
C
         CALL GETSET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)
         CALL SET(FL,FR,FB,FT,FL,FR,FB,FT,LLL)
         FDX=FR-FL
         FDY=FT-FB
         UDX=UR-UL
         UDY=UT-UB

C        CONVERT SIZE OF A BLANK CHARACTER (CSIZ) TO FRACTIONS OF 1024.

         WSIZ=CSIZ/1024.
         HSIZ=2.0*WSIZ

         DO 20 I=1,IMRK
            X1=XMRK(I)
            Y1=YMRK(I)
            IF(X1.LT.XMN .OR. X1.GT.XMX)GO TO 18
            IF(Y1.LT.YMN .OR. Y1.GT.YMX)GO TO 18

            IF(KMRK.LT.IMRK)THEN
               PLTNMRK=.FALSE.
               DO K=1,KMRK
                  IF(NMRK(I).EQ.PLT_NMRK(K))THEN
                     PLTNMRK=.TRUE.
                     LMRK=K
                     IF(REC_DATA(K).EQ.'N')THEN
                        RAD_CLOCK=0.5*HCIRC
                     ELSE
                        RAD_CLOCK=HCIRC
                     END IF
                  END IF
               END DO
            END IF

C     Draw 24-hr clock with HCIRC km radius around NEXRAD station.
C     Each line represents some data was received, beginning at that hour.
C     
            PLTNMRK=.FALSE.
            IF(PLTNMRK)THEN
               IF(PLTPERCNT)THEN
                  XP = XMRK(I)
                  YP = YMRK(I) + HCIRC
                  CALL PLCHMQ (XP,YP,PERCENT(LMRK),CSIZ,0.0,-1.0)
               ELSE
                  N=1
                  DO J=0,360,15
                     AZ = TORAD*FLOAT(J)
                     XCIRC(N) = XMRK(I) + RAD_CLOCK*SIN(AZ)
                     YCIRC(N) = YMRK(I) + RAD_CLOCK*COS(AZ)
                     XC1 = XCIRC(N)
                     YC1 = YCIRC(N)
                     XC2 = XMRK(I) + (RAD_CLOCK+20.0)*SIN(AZ)
                     YC2 = YMRK(I) + (RAD_CLOCK+20.0)*COS(AZ)
                     IF(MOD(N-1,3).EQ.0)CALL LINE(XC1,YC1,XC2,YC2)
                     IF(N.GE.2)THEN
                        XC3 = XCIRC(N-1)
                        YC3 = YCIRC(N-1)
                        CALL LINE(XC1,YC1,XC3,YC3)
                     END IF
                     N = N + 1
                  END DO
                  IF(REC_DATA(LMRK).EQ.'Y')THEN
                     DO LT=1,NHOURS
                        IF(PLT_TIME(LMRK,LT).NE.' ' .AND.
     X                       PLT_TIME(LMRK,LT).NE.'.' .AND.
     X                       PLT_TIME(LMRK,LT).NE.'d' .AND.
     X                       PLT_TIME(LMRK,LT).NE.'x')THEN
                           XC1=XMRK(I)
                           YC1=YMRK(I)
                           MT=LT+HOUR_BEG-1
                           MMRK=1+MOD(MT,24)
                           XC2=XCIRC(MMRK)
                           YC2=YCIRC(MMRK)
                           IDAY=1+MOD(MT/24,3)
                           CALL DASHDB (IDPAT(5,IDAY))
                           CALL LINED(XC1,YC1,XC2,YC2)
                        END IF
                     END DO
                  END IF
               END IF
            END IF

C           GET LENGTH OF CHARACTER STRING TO BE PLOTTED

            IF(JMRK.EQ.6)THEN
               KK=GETLEN(NMRK(I))+2
            ELSE IF(JMRK.EQ.5)THEN
               KK=GETLEN(NMRK(I))
            ELSE
               KK=1
            END IF

C           GET THE BOUNDS OF THE BOX AROUND A CHARACTER STRING,
C           FILL IT WITH COLOR (INDEX=ICOLBX) AND WRITE STRING.
C
            FX1=FL+(X1-UL)*FDX/UDX
            FY1=FB+(Y1-UB)*FDY/UDY
            FXL=FX1-1.25*WSIZ
            FXR=FX1+(FLOAT(KK)+0.25)*WSIZ
            FYB=FY1-0.5*HSIZ
            FYT=FY1+0.5*HSIZ
            FXP(1)=FXL
            FXP(2)=FXR
            FXP(3)=FXR
            FXP(4)=FXL
            FYP(1)=FYB
            FYP(2)=FYB
            FYP(3)=FYT
            FYP(4)=FYT
            CALL FAREA(ICOLBX,FXP,FYP,5)
            IF(JMRK.EQ.6)THEN
               WRITE(LAB9,11)NMRK(I)
   11          FORMAT(1X,A7,1X)
               CALL PLCHMQ (FX1,FY1,LAB9,CSIZ,0.0,-1.0)
               WRITE(LAB6,13)SMRK(IN)
   13          FORMAT(A6)
               CALL PLCHHQ (FX1,FY1,LAB6,CSIZ,0.0,0.0)
            ELSE IF (JMRK.EQ.5)THEN
               WRITE(LAB7,15)NMRK(I)
   15          FORMAT(A7)
               CALL PLCHMQ (FX1,FY1,LAB7,CSIZ,0.0,-1.0)
            ELSE IF (JMRK.EQ.4)THEN
               WRITE(LAB6,13)SMRK(IN)
               CALL PLCHHQ (FX1,FY1,LAB6,CSIZ,0.0,0.0)
            END IF

   18       CONTINUE
            IF(JN.GE.NN)THEN
               JN=1
               IN=IN+1
               NN=NNET(IN)
            ELSE
               JN=JN+1
            END IF
   20    CONTINUE
         CALL SET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)

      ELSE

C        STAY WITH USER COORD TO PLOT SYMBOLS AND NAMES ONLY
C        JMRK - (1) SYMBOL ONLY, (2) NAME ONLY, (3) SYMBOL AND NAME
C               (7) SYMBOL PLUS TRUE NORTH
         DO 30 I=1,IMRK
            X1=XMRK(I)
            Y1=YMRK(I)
            IF(X1.LT.XMN .OR. X1.GT.XMX)GO TO 28
            IF(Y1.LT.YMN .OR. Y1.GT.YMX)GO TO 28

c            print *,'PLTMRK: kmrk, imrk=',kmrk,imrk
            IF(KMRK.LT.IMRK)THEN
               PLTNMRK=.FALSE.
               DO K=1,KMRK
                  IF(NMRK(I).EQ.PLT_NMRK(K))THEN
                     PLTNMRK=.TRUE.
                     LMRK=K
                     IF(REC_DATA(K).EQ.'N')THEN
                        RAD_CLOCK=0.5*HCIRC
                     ELSE
                        RAD_CLOCK=HCIRC
                     END IF
                  END IF
               END DO
            END IF

C     Draw 24-hr clock with HCIRC km radius around NEXRAD station.
C     Each line represents some data was received, beginning at that hour.
C
            PLTNMRK=.FALSE.
            IF(PLTNMRK)THEN
               IF(PLTPERCNT)THEN
                  XP = XMRK(I)
                  YP = YMRK(I) + HCIRC
                  CALL PLCHMQ (XP,YP,PERCENT(LMRK),CSIZ,0.0,-1.0)
               ELSE
                  N=1
                  DO J=0,360,15
                     AZ = TORAD*FLOAT(J)
                     XCIRC(N) = XMRK(I) + RAD_CLOCK*SIN(AZ)
                     YCIRC(N) = YMRK(I) + RAD_CLOCK*COS(AZ)
                     XC1 = XCIRC(N)
                     YC1 = YCIRC(N)
                     XC2 = XMRK(I) + (RAD_CLOCK+20.0)*SIN(AZ)
                     YC2 = YMRK(I) + (RAD_CLOCK+20.0)*COS(AZ)
                     IF(MOD(N-1,3).EQ.0)CALL LINE(XC1,YC1,XC2,YC2)
                     IF(N.GE.2)THEN
                        XC3 = XCIRC(N-1)
                        YC3 = YCIRC(N-1)
                        CALL LINE(XC1,YC1,XC3,YC3)
                     END IF
                     N = N + 1
                  END DO
                  IF(REC_DATA(LMRK).EQ.'Y')THEN
                     DO LT=1,NHOURS
                        IF(PLT_TIME(LMRK,LT).NE.' ' .AND.
     X                       PLT_TIME(LMRK,LT).NE.'.' .AND.
     X                       PLT_TIME(LMRK,LT).NE.'d' .AND.
     X                       PLT_TIME(LMRK,LT).NE.'x')THEN
                           XC1=XMRK(I)
                           YC1=YMRK(I)
                           MT=LT+HOUR_BEG-1
                           MMRK=1+MOD(MT,24)
                           XC2=XCIRC(MMRK)
                           YC2=YCIRC(MMRK)
                           IDAY=1+MOD(MT/24,3)
                           CALL DASHDB (IDPAT(5,IDAY))
                           CALL LINED(XC1,YC1,XC2,YC2)
                        END IF
                     END DO
                  END IF
               END IF
            END IF

            IF(JMRK.EQ.3)THEN
               WRITE(LAB9,21)NMRK(I)
   21          FORMAT(1X,A7,1X)
               CALL PLCHMQ (X1,Y1,LAB9,CSIZ,0.0,-1.0)
               WRITE(LAB6,23)SMRK(IN)
   23          FORMAT(A6)
               CALL PLCHHQ (X1,Y1,LAB6,CSIZ,0.0,0.0)
            ELSE IF (JMRK.EQ.2)THEN
               WRITE(LAB7,25)NMRK(I)
   25          FORMAT(A7)
               CALL PLCHMQ (X1,Y1,LAB7,CSIZ,0.0,-1.0)
            ELSE IF (JMRK.EQ.1.OR.JMRK.EQ.7)THEN
               WRITE(LAB6,23)SMRK(IN)
               CALL PLCHHQ (X1,Y1,LAB6,CSIZ,0.0,0.0)

C              Find the station (lat,lon) from (x1,y1) and then 
C              (x2,y2) corresponding to station (lat + 1 deg, lon). 
C
               IF(JMRK.EQ.7)THEN
                  CALL XY2LLDRV(PLAT,PLON,X1,Y1,OLAT,OLON,ANGXAX)
                  PLAT=PLAT+1.0
                  CALL LL2XYDRV(PLAT,PLON,X2,Y2,OLAT,OLON,ANGXAX)
                  CALL LINE (X1,Y1,X2,Y2)
               END IF

            END IF

   28       CONTINUE
            IF(JN.GE.NN)THEN
               JN=1
               IN=IN+1
               NN=NNET(IN)
            ELSE
               JN=JN+1
            END IF
   30    CONTINUE
      END IF


C     RESTORE LINE WIDTHS AND COLORS
C
      CALL SETLIN(IGRPLT,BGFLAG,IBLACK,IWHITE,ILW)
      RETURN
      END
