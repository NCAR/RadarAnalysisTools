      SUBROUTINE PLTSTA(NMRK,XMRK,YMRK,ZMRK,IMRK,NET,NNET,SMRK,
     X                  SYMMRK,ISTADEF,LABFLG,ITIT,PWIND,NCWORD,
     X                  ZLEV)
C
C     PLOT MESONETORK POSITIONS OVERLAID ON CONTOUR OR COLOR PLOTS.
C     COLOR FILL A BOX AND PLOT THE CHARACTER STRING INSIDE IT.
C
C     XMRK,YMRK,ZMRK  - STATION POSITION (KM,KM,KM)
C     IMRK            - NUMBER OF MESONETWORK POSITIONS
C     SYMMRK          - (1) SYMBOL ONLY,  (2) NAME ONLY,  (3) BOTH
C                       (4) SYMBOL IN BOX,(5) NAME IN BOX,(6) BOTH IN BOX
C                     - Add station altitude to options (1-3) --> (7-9)
C     FOR EACH OF (20) POSSIBLE NETWORKS:
C                 NET - NUMBER OF NETWORKS
C                NNET - NUMBER OF STATIONS IN NTH NETWORK
C                SMRK - PLOTTING SYMBOL (COMPLEX CHARACTER SET)
C
C     ICOLBX          - COLOR INDICES FOR THE BOX AND CHARACTERS
C
      INCLUDE 'CEDRIC.INC'
      PARAMETER (MXK=1000,MXNET=20)
c      PARAMETER (CSIZ=15.0)
      PARAMETER (CSIZ=10.0)
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X     IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X     NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF
      COMMON /RANGEC/ XBEG,XEND,YBEG,YEND,XRANGE(2),YRANGE(2),DELRG(2)
      COMMON /AXUNTS/ IUNAXS,LABAXS(3,3),SCLAXS(3,3),AXNAM(3)
      CHARACTER*4 AXNAM
      CHARACTER*1 IAXIS(3)
      CHARACTER*16 CFMTX,CFMTY
      CHARACTER*8 NOW,IFMTX(2),IFMTY(2),ITIT(5)
      CHARACTER*40 CITIT
      CHARACTER*80 JTIT
      DATA IAXIS/'X','Y','Z'/
      CHARACTER LAB1*1,LAB5*13,LAB6*6,LAB7*7,LAB9*9,BGFLAG*1
      CHARACTER*7 NMRK(MXK)
      CHARACTER*6 SMRK(MXNET)
      INTEGER GETLEN

      DIMENSION XMRK(MXK),YMRK(MXK),ZMRK(MXK)
      DIMENSION NNET(MXNET),PWIND(2,3),NCWORD(3)
      DIMENSION FXP(5),FYP(5)

      LOCPLT(R)=1023.*R

      CALL GSFAIS(1)

C     NETWORK COUNTERS:
C        IN - NUMBER OF THE CURRENT NETWORK (IN=1,NET)
C        NN - NUMBER OF STATIONS WITHIN CURRENT NETWORK (NN=1,NNET(IN))
C        JN - COUNTER FOR STATION NUMBER WITHIN CURRENT NETWORK (JN=1,NN)
C
      IN=1
      JN=1
      NN=NNET(IN)
      L1=NCWORD(1)
      L2=NCWORD(2)
      L3=NCWORD(3)
      CALL RGINI(FL,FR,FB,FT,PWIND,CSP,NCX,NCWORD)
      XL=FL
      XR=FR
      YB=FB
      YT=FT
C     
C     SAVE INTO A FLASH BUFFER
C
      CALL GFLAS1(4)
      CALL GSCLIP(0)
      CALL SET(FL,FR,FB,FT,PWIND(1,L1),PWIND(2,L1),PWIND(1,L2),
     X         PWIND(2,L2),1)
      XMN=PWIND(1,L1)
      XMX=PWIND(2,L1)
      YMN=PWIND(1,L2)
      YMX=PWIND(2,L2)
      UL=XMN
      UR=XMX
      UB=YMN
      UT=YMX
C     PLOT SYMBOLS AND NAMES INSIDE COLORED BOXES OR SYMBOLS AND NAMES ONLY
C
c-----added for debugging (ljm 11/11/98)
c      print *,'Pltsta: imrk,symmrk=',imrk,symmrk,
c     +     ' xmnmx,ymnmx=',xmn,xmx,ymn,ymx
c-----added for debugging (ljm 11/11/98)
      IF(SYMMRK.GE.4. .AND. SYMMRK.LE.6.)THEN

C        USE FRACT COORD TO PLOT SYMBOLS AND NAMES INSIDE COLORED BOXES
C        SYMMRK - (4) SYMBOL ONLY, (5) NAME ONLY, (6) SYMBOL AND NAME
C
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

C           GET LENGTH OF CHARACTER STRING TO BE PLOTTED

            IF(SYMMRK.EQ.6.)THEN
               IF (SMRK(IN)(6:6).NE.' ') THEN
                  KK=GETLEN(NMRK(I))+1
               ELSE
                  KK=GETLEN(NMRK(I))
               END IF
            ELSE IF(SYMMRK.EQ.5.)THEN
               KK=GETLEN(NMRK(I))
            ELSE IF(SYMMRK.EQ.4.)THEN
               KK=1
            END IF

C           GET THE BOUNDS OF THE BOX AROUND A CHARACTER STRING,
C           FILL IT WITH COLOR (INDEX=ICOLBX) AND WRITE STRING.
C
            FX1=FL+(X1-UL)*FDX/UDX
            FY1=FB+(Y1-UB)*FDY/UDY
            IF (SYMMRK.EQ.5 .OR. (SYMMRK.EQ.6 .AND. 
     X           SMRK(IN)(6:6).EQ.' ')) THEN
               FXL=FX1-.25*WSIZ
            ELSE
               FXL=FX1-.70*WSIZ
            END IF
            FXR=FX1+(FLOAT(KK))*WSIZ
            FYB=FY1-0.35*HSIZ
            FYT=FY1+0.35*HSIZ
            FXP(1)=FXL
            FXP(2)=FXR
            FXP(3)=FXR
            FXP(4)=FXL
            FYP(1)=FYB
            FYP(2)=FYB
            FYP(3)=FYT
            FYP(4)=FYT
            CALL FAREA(0,FXP,FYP,5)
            IF(SYMMRK.EQ.6.)THEN
               IF (SMRK(IN)(6:6).NE.' ') THEN
                  WRITE(LAB9,11)NMRK(I)
 11               FORMAT(1X,A7,1X)
                  CALL PLCHMQ (FX1,FY1,LAB9,CSIZ,0.0,-1.0)
                  WRITE(LAB6,13)SMRK(IN)
 13               FORMAT(A6)
                  CALL PLCHHQ (FX1,FY1,LAB6,CSIZ,0.0,0.0)
               ELSE
                  WRITE(LAB7,15)NMRK(I)
                  CALL PLCHMQ (FX1,FY1,LAB7,CSIZ,0.0,-1.0)
               END IF
            ELSE IF (SYMMRK.EQ.5.)THEN
               WRITE(LAB7,15)NMRK(I)
 15            FORMAT(A7)
               CALL PLCHMQ (FX1,FY1,LAB7,CSIZ,0.0,-1.0)
            ELSE IF (SYMMRK.EQ.4.)THEN
               WRITE(LAB6,13)SMRK(IN)
               CALL PLCHHQ (FX1,FY1,LAB6,CSIZ,0.0,0.0)
            END IF
            
 18         CONTINUE
            IF(JN.GE.NN)THEN
               JN=1
               IN=IN+1
               NN=NNET(IN)
            ELSE
               JN=JN+1
            END IF
 20      CONTINUE
         CALL SET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)

      ELSE

c--------added for debugging (ljm 11/11/98)
c         print *,'Pltsta: imrk=',imrk,' xmnmx,ymnmx=',xmn,xmx,ymn,ymx
c--------added for debugging (ljm 11/11/98)

         DO 50 I=1,IMRK
            X1=XMRK(I)
            Y1=YMRK(I)

c--------added for debugging (ljm 11/11/98)
c            write(6,1770)i,nmrk(i),xmrk(i),ymrk(i)
c 1770       format(2x,'i,name=',i8,2x,a8,2x,'  xy=',2f8.2)
c--------added for debugging (ljm 11/11/98)

            IF(X1.LT.XMN .OR. X1.GT.XMX)GO TO 48
            IF(Y1.LT.YMN .OR. Y1.GT.YMX)GO TO 48
            
C        STAY WITH USER COORD TO PLOT SYMBOLS AND NAMES ONLY.C       
C        SYMMRK - (1) SYMBOL ONLY, (2) NAME ONLY, (3) SYMBOL AND NAME
C        ADD STATION HEIGHT (IZMRK meters) to SYMMRK = (1-3) ==> (7-9)

            IZMRK=NINT(1000.0*ZMRK(I))
            IF(SYMMRK.EQ.3. .OR. SYMMRK.EQ.9.)THEN
               IF (SMRK(IN)(6:6).NE.' ') THEN
                  WRITE(LAB9,21)NMRK(I)
 21               FORMAT(1X,A7,1X)
                  CALL PLCHMQ (X1,Y1,LAB9,CSIZ,0.0,-1.0)
                  WRITE(LAB6,22)SMRK(IN)
 22               FORMAT(A6)
                  CALL PLCHHQ (X1,Y1,LAB6,CSIZ,0.0,0.0)
                  IF(SYMMRK.EQ.9)THEN
                     WRITE(LAB5,23)IZMRK
 23                  FORMAT(5X,'-',I4.4)
                     CALL PLCHHQ (X1,Y1,LAB5,CSIZ,0.0,-1.0)
                  END IF
               ELSE
                  WRITE(LAB7,25)NMRK(I)
                  CALL PLCHMQ (X1,Y1,LAB7,CSIZ,0.0,-1.0)
                  IF(SYMMRK.EQ.9)THEN
                     WRITE(LAB5,24)IZMRK
 24                  FORMAT(4X,'-',I4.4)
                     CALL PLCHHQ (X1,Y1,LAB5,CSIZ,0.0,-1.0)
                  END IF
               END IF
            ELSE IF (SYMMRK.EQ.2. .OR. SYMMRK.EQ.8)THEN
               WRITE(LAB7,25)NMRK(I)
 25            FORMAT(A7)
               CALL PLCHMQ (X1,Y1,LAB7,CSIZ,0.0,-1.0)
               IF(SYMMRK.EQ.8)THEN
                  WRITE(LAB5,24)IZMRK
                  CALL PLCHMQ (X1,Y1,LAB5,CSIZ,0.0,-1.0)
               END IF
            ELSE IF (SYMMRK.EQ.1. .OR. SYMMRK.EQ.7)THEN
               WRITE(LAB6,22)SMRK(IN)
               CALL PLCHHQ (X1,Y1,LAB6,CSIZ,0.0,0.0)
               IF(SYMMRK.EQ.7)THEN
                  WRITE(LAB5,27)IZMRK
 27               FORMAT(1X,'-',I4.4)
                  CALL PLCHMQ (X1,Y1,LAB5,CSIZ,0.0,-1.0)
               END IF
            END IF
            
 48         CONTINUE
            IF(JN.GE.NN)THEN
               JN=1
               IN=IN+1
               NN=NNET(IN)
            ELSE
               JN=JN+1
            END IF
 50      CONTINUE
      END IF

      CALL GFLAS2
      IF (ISTADEF.EQ.1) THEN
C     
C     PRODUCE A COMPLETELY LABELED STATION LOCATION PLOT
C     
         CALL GSCLIP(0)
         WRITE (CITIT,500)ITIT
 500     FORMAT(5A8)
         CALL DATEE(NOW)
         CF=1./ID(69)
         XOR=ID(40)*CF
         SF=1./ID(68)
         XREL=ID(41)*SF
         YREL=ID(42)*SF
         CALL MAJMIN(XRANGE(1),DELRG(1),IFMTX,MAJORX,MINORX,NDIG1,ISZ1,
     X               YRANGE(1),DELRG(2),IFMTY,MAJORY,MINORY,NDIG2,ISZ2)
C         CALL MAJMIN(DELRG(1),IFMTX,MAJORX,MINORX)
C         CALL MAJMIN(DELRG(2),IFMTY,MAJORY,MINORY)
         WRITE (CFMTX,510)IFMTX
 510     FORMAT(2A8)
         WRITE (CFMTY,510)IFMTY

C     Always draw grid and grid labels
         CALL LABMOD(CFMTX,CFMTY,NDIG1,NDIG2,ISZ1,ISZ2,4,4,0)
         CALL PERIML(MAJORX,MINORX,MAJORY,MINORY)

C     JUST DRAW A BOX AROUND PLOT-NO TICK MARKS
c            CALL TICKS(0,0)
c            CALL PERIM(0,0,0,0)
c            CALL TICKS(12,8)
            
         WRITE (JTIT,106)(ID(I),I=116,121),(ID(I),I=125,127),
     X        (ID(I),I=13,15),AXNAM(L3),ZLEV,
     X        LABAXS(L3,IUNAXS)
 106     FORMAT(I2.2,'/',I2.2,'/',I2.2,6X,I2.2,2(':',I2.2),'-',
     X        I2.2,2(':',I2.2),7X,3A2,7X,A2,'=',F7.2,' ',A4)
         IF (LABFLG.GT.5) THEN
            CALL PLCHMQ(CPUX(60),CPUY(1010),JTIT(1:66),12.,0.,-1.)
            WRITE (JTIT,107)NOW
 107        FORMAT('(AS OF ',A8,')')
            CALL PLCHMQ(CPUX(10),CPUY(985),JTIT(1:16),12.,0.,-1.)
            WRITE (JTIT,105)XREL,YREL,XOR
 105        FORMAT('ORIGIN=(',F7.2,',',F7.2,') KM   X-AXIS=',F5.1,
     X           ' DEG')
            CALL PLCHMQ(CPUX(430),CPUY(985),JTIT(1:46),12.,0.,-1.)
            CALL PLCHMQ(CPUX(200),CPUY(960),CITIT,12.,0.,-1.)
            WRITE (JTIT,108)AXNAM(L2),LABAXS(L2,IUNAXS)
 108        FORMAT(A1,' ',A4)
         END IF
         LOCY=LOCPLT(YB+(YT-YB)*0.15)
         IF (LABFLG.GT.5) THEN
            CALL PLCHMQ(CPUX(10),CPUY(LOCY),JTIT(1:6),12.,90.,-1.)
            WRITE (JTIT,108)AXNAM(L1),LABAXS(L1,IUNAXS)
         END IF
         LOCY=LOCPLT(YB)-50
         IF (LABFLG.GT.5) THEN
            CALL PLCHMQ(CPUX(150),CPUY(LOCY),JTIT(1:6),12.,0.,-1.)
         END IF
         
         CALL GFLAS3(4)
         IF (LABFLG.GT.5) THEN
            CALL MYFRAME
         ELSE
            CALL FRAME
         END IF
      END IF

      RETURN
      END
