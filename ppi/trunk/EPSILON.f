      SUBROUTINE EPSILON(DAT,AZA,ELA,IOUT,IIN1,NAZ,DROLD,MNGATE,
     +   MXGATE,C1,C2,C3,C4,BDVAL,IDATE,ITIME,R0,AZMNWIN,
     +   AZMXWIN,RMNWIN,RMXWIN,SKP,DTRN,DIR,SFCON,AVGI,ITP,
     +   PLTFLG,TMP1,TMP2,MXR,MXA,MXF)

C ROUTINE TO CALCULATE THE STRUCTURE FUNCTION OR THE KINETIC ENERGY
C DISSIPATION RATE.  STRUCTURE FUNCTION (D) IS DEFINED AS
C  D= <[V(R+DELR)-V(R)]**2> WHERE V IS THE RADIAL VELOCITY AT
C     SOME RANGE, R AND DELR IS SOME INCREMENTAL R SPECIFYING
C     THE SCALE SIZE.  D IS RELATED TO THE DISSIPATION RATE (e) BY
C     D = C*(e**2/3)*DELR**2/3 WHERE C IS A CONSTANT EQUAL APPROXIMATELY
C     TO 2.  HERE WE CALCULATE e BY FINDING D FOR A NUMBER OF DELR
C     AND THEN FITTING A LEAST SQUARES LINE TO SOLVE FOR e.  

C C1=MINIMUM DELTA RANGE FOR STRUCTURE FUNCTION CALCULATION (M)
C C2=MAXIMUM DELTA RANGE FOR STRUCTURE FUNCTION CALCULATION (M)
C C3=INCREMENT IN RANGE DIRECTION 
C C4=INCREMENT IN AZIMUTH DIRECTION

C    IF C1 .LT.0. RETURN STRUCTURE FUNCTION
C    IF C1 .GT.0. RETURN EPSILON.
C PLTFLG= PLOT FLAG FOR SCATTER PLOTS OF STRUCTURE FUNCTION VERSUS
C         RANGE SEPARATION.  =0, NO PLOTS OTHERWISE PRODUCE PLOTS.
C AZMNWIN,AZMXWIN,RMNWIN,RMXWIN= AZIMUTH AND RANGE WINDOW FOR PRODUCING
C     SCATTER PLOTS OF STRUCTURE FUNCTION VERSUS RANGE SEPARATION
C AZSKP,RNGSKP= AZIMUTH AND RANGE SKIPPING FACTOR FOR SCATTER PLOTS.
C RNGMAX,SFMAX= MAXIMUM RANGE SEPARATION AND STRUCTURE FUNCTION OF 
C     SCATTER PLOTS.

      DIMENSION DAT(MXR,MXA,MXF),AZA(MXA,2),ELA(MXA,2)
      DIMENSION STRFUN(50),DELR(50),STRE(50),DELRE(50)
      DIMENSION TMP1(MXR),TMP2(MXR,MXA)
      DATA C/2.0/
      DATA TORAD/0.017453293/
      
      CHARACTER*1 DIR,DTRN

      CHARACTER*100 LAB
      CHARACTER*6 IFMT

      C=SFCON
      IF(C1.GE.0.)THEN
         IEPS=1
      ELSE
         C1=-C1
         IEPS=0
      END IF
      IHR=0
      IHA=0
      ISKP=NINT(SKP)
      IPLT=NINT(PLTFLG)
      IR1=NINT(C1/(1000.*DROLD))
      IR2=NINT(C2/(1000.*DROLD))
      IF(IR1.LT.1)IR1=1
      IF(DIR.NE.'A')IHR=NINT(0.5*FLOAT(IR2)-0.0001)
      IDELR=NINT(C3)
      IDELA=NINT(C4)
      DEL2=IDELR*IDELA
      DO 10 I=MNGATE,MXGATE
         DO 10 J=1,NAZ
 10         DAT(I,J,IOUT)=BDVAL
      DO 60 I=MNGATE+IDELR+IHR,MXGATE-IDELR-IHR
         RANGE=R0+(I-1)*DROLD
         IF(DIR.EQ.'A')THEN
            IR1=NINT(C1/(RANGE*AVGI*TORAD*1000.))
            IR2=NINT(C2/(RANGE*AVGI*TORAD*1000.))
            IF(IR1.LT.1)IR1=1
            IF(IR2.LT.1)IR2=1
            IF(IR2.GT.50)GO TO 60
            IHA=NINT(0.5*FLOAT(IR2)-0.0001)
         END IF
         DO 50 J=1+IDELA+IHA,NAZ-IDELA-IHA
            AZI=AZA(J,1)
            IF(AZI.LT.0.)AZI=AZI+360.
            RNPTS=0.
            IF(DTRN.EQ.'Y')THEN
               IF(DIR.NE.'A')THEN
                  CALL DETRND(DAT,IIN1,I,J,I-IDELR-IHR,I+IDELR+IR-IHR,
     +                        J-IDELA,J+IDELA,AZA,DROLD,R0,ITP,
     +                        BDVAL,TMP2,0,MXR,MXA,MXF)
               ELSE
                  CALL DETRND(DAT,IIN1,I,J,I-IDELR,I+IDELR,
     +               J-IDELA-IHA,J+IDELA+IR-IHA,AZA,DROLD,R0,ITP,
     +               BDVAL,TMP2,0,MXR,MXA,MXF)
               END IF
            END IF
                  

            DO 100 IR=IR1,IR2

               IHAF=NINT(0.5*FLOAT(IR)-0.0001)
               RNPTS1=0.
               SUM=0.

               IF(DIR.NE.'A')THEN


               R=C1+(IR-IR1)*1000.*DROLD
               DO 150 I1=I-IDELR-IHAF,I+IDELR-IHAF
                  DO 150 J1=J-IDELA,J+IDELA
                     IF(DAT(I1,J1,IIN1).EQ.BDVAL.OR.
     +                  DAT(I1+IR,J1,IIN1).EQ.BDVAL)GO TO 150
                     IF((I1+IR).GT.MXGATE)GO TO 150
                     RNPTS1=RNPTS1+1.
                     D1=DAT(I1,J1,IIN1)
                     D2=DAT(I1+IR,J1,IIN1)
                     IF(DTRN.EQ.'Y')THEN
                        D1=D1-TMP2(I1,J1)
                        D2=D2-TMP2(I1+IR,J1)
                     END IF
                     SUM=SUM+(D2-D1)
     +               **2
 150           CONTINUE
               
               ELSE


               R=1000.*RANGE*SIN(TORAD*IR*AVGI)
               DO 160 I1=I-IDELR,I+IDELR
                  DO 160 J1=J-IDELA-IHAF,J+IDELA-IHAF
                     IF(DAT(I1,J1,IIN1).EQ.BDVAL.OR.
     +                  DAT(I1,J1+IR,IIN1).EQ.BDVAL)GO TO 160
                     IF((J1+IR).GT.NAZ)GO TO 160
                     RNPTS1=RNPTS1+1.
                     D1=DAT(I1,J1,IIN1)
                     D2=DAT(I1,J1+IR,IIN1)
                     IF(DTRN.EQ.'Y')THEN
                        D1=D1-TMP2(I1,J1)
                        D2=D2-TMP2(I1,J1+IR)
                     END IF
                     SUM=SUM+(D2-D1)
     +               **2
 160           CONTINUE

               END IF

               
               IF(RNPTS1.GT.DEL2)THEN

                  DAT(I,J,IOUT)=SUM/RNPTS1
                  RNPTS=RNPTS+1.
                  IP=NINT(RNPTS)
                  STRFUN(IP)=SUM/RNPTS1
                  DELR(IP)=R
               END IF
 100        CONTINUE
C  COMPUTE EPSILON
            IF(IEPS.EQ.1)THEN
               IP=NINT(RNPTS)

               IF(IP.LE.2)THEN
                  EPS=BDVAL
               ELSE
                  EMAX=0.
                  NSTP=IP-2
                  SFMAX=0.
                  DO 210 ISTP=1,NSTP
                  SUMSF=0.
                  SUMR=0.
                  SUMSF2=0.
                  SUMR2=0.
                  SUMSFR=0.
                  RNPTS=0.
                  DO 200 K=1,ISTP+2
                     SF=(STRFUN(K)/C)**1.5
                     RNPTS=RNPTS+1.
                     IF(SF.GT.SFMAX)SFMAX=SF
                     SUMSF=SUMSF+SF
                     SUMR=SUMR+DELR(K)
                     SUMSF2=SUMSF2+SF*SF
                     SUMR2=SUMR2+DELR(K)*DELR(K)
                     SUMSFR=SUMSFR+DELR(K)*SF
 200              CONTINUE
                  IF((RNPTS*SUMR2-SUMR*SUMR).EQ.0.)GO TO 50
                  B=(RNPTS*SUMSFR-SUMSF*SUMR)/
     +              (RNPTS*SUMR2-SUMR*SUMR)
                  EPS=B
                  STRE(ISTP)=1000.*EPS
                  IF(STRE(ISTP).GT.EMAX)EMAX=STRE(ISTP)
                  DELRE(ISTP)=DELR(ISTP+2)
                  A=SUMSF/RNPTS-B*SUMR/RNPTS
 210              CONTINUE


                  IF(IPLT.EQ.1)THEN
                     IF(RANGE.GE.RMNWIN.AND.
     +                  RANGE.LE.RMXWIN.AND.
     +                  AZI.GE.AZMNWIN.AND.
     +                  AZI.LE.AZMXWIN.AND.
     +                  MOD(I,ISKP).EQ.0.AND.
     +                  MOD(J,ISKP).EQ.0)THEN
                        IF(SFMAX.LT.5.0)THEN
                           YMAX=5.0
                        ELSE
                           IDG=INT(SFMAX/10.)+1
                           YMAX=10.*IDG
                        END IF
                        CALL SET(0.1,0.9,0.1,0.9,0.,C2,0.,YMAX,1)
                        CALL LABMOD('(F5.0)','(F5.1)',5,5,14,14,8,8,0)
                        CALL MAJMIN(0.,C2,IFMT,MJX,MNX,IDIG)
                        CALL MAJMIN(0.,YMAX,IFMT,MJY,MNY,IDIG)
                        CALL PERIML(MJX,MNX,MJY,MNY)
                        SUMERR=0.
                        DO 250 K=1,IP
                           SF=(STRFUN(K)/C)**1.5
                           X1=DELR(K)
                           Y1=A+B*X1
                           SUMERR=SUMERR+(SF-Y1)*(SF-Y1)
                           IF(SF.LE.YMAX)
     +                       CALL PLCHMQ(DELR(K),SF,'X',8.,0.,0.)
                           IF(K.NE.IP)THEN
                              X2=DELR(K+1)
                              Y2=A+B*X2
                              IF(Y1.GE.0..AND.Y2.GE.0..AND.
     +                           Y1.LE.YMAX.AND.
     +                           Y2.LE.YMAX)CALL LINE(X1,Y1,X2,Y2)
                           END IF
 250                       CONTINUE
                        ERR=SUMERR/FLOAT(IP)
                        CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
                        AZLAB=AZA(J,1)
                        IF(AZLAB.LT.0.)AZLAB=AZLAB+360.
                        WRITE(LAB,260)IDATE,ITIME,AZLAB,RANGE,
     +                   ELA(J,1),B,A,ERR
                        CALL PLCHMQ(0.5,0.95,LAB,10.,0.,0.)
 260                    FORMAT(2I7,2X,'AZ,RNG,EL=',3F7.1,2X,
     +                 'B,A,ERR=',3F10.4)
                        CALL PLCHMQ(0.5,0.05,'RANGE SPACING (M)',
     +                    14.,0.,0.)
                        CALL PLCHMQ(0.025,0.5,'(D/C)**3/2',14.,90.,0.)
                        CALL FRAME
                        IF(EMAX.LT.5.0)THEN
                           YMAX=5.0
                        ELSE
                           IDIG=INT(EMAX/10.)+1
                           YMAX=10.*IDIG
                        END IF
                        CALL MAJMIN(0.,YMAX,IFMT,MJY,MNY,IDIG)
                        CALL SET(0.1,0.9,0.1,0.9,0.,C2,0.,YMAX,1)
                        CALL PERIML(MJX,MNX,MJY,MNY)
                        DO 300 K=1,NSTP
 300                       CALL PLCHMQ(DELRE(K),STRE(K),'X',8.,0.,0.)
                        CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
                        CALL PLCHMQ(0.5,0.95,LAB,10.,0.,0.)
                        CALL PLCHMQ(0.5,0.05,'RANGE SPACING (M)',
     +                     14.,0.,0.)
                        CALL PLCHMQ(0.025,0.5,'EPSILON (CM**3/S**2',
     +                    14.,90.,0.)
                        CALL FRAME                        
                     END IF
                  END IF
               END IF
               DAT(I,J,IOUT)=EPS
            END IF
 50   CONTINUE
 60   CONTINUE
      RETURN
      END
                  

                     












