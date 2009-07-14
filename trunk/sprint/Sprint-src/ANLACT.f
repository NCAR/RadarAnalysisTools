      SUBROUTINE ANLACT(FNUM,P1,P2,P3,P4,P10,NRG,KST,J1,
     X     J2,NGFLD,ISCALE,K,EL,TILTT,Y0,RG1,AZZ,
     X     DRG,ICOPLANE,ROTANG,XORR,YORR,ZORR)
C     
C     THIS SUBROUTINE REPLACES A FIELD FROM THE INPUT DATA TAPE WITH
C     THE OUTPUT OF AN ANALYTICAL FUNCTION.
C     
C     FNUM IS ANALYTICAL FUNCTION NUMBER (ANALYT command)
C     P1,P2,P3,P4 ARE PARAMETERS FOR THE FUNCTIONS
C
C     FNUM FUNCTION   DESCRIPTION             P1    P2    P3    P4
C     ---- --------   -----------            ----  ----  ----  ----
C      01  CON        Constant value         Value
C      02  VR         Radial velocity        U     V     0.0
C      03  VR         Radial velocity        U     V     W
C      04  RANDOM     Uniform random number  Min   Max
C      05  NORMAL     Gaussian random number Mean  Std
C      06  COSPROD    Cosine product         Ampl  x-Wvl y-Wvl z-Wvl
C      07  VRS        Vr satisfy continuity  Ampl  x-Wvl y-Wvl z-Wvl
C      08  RANGE      Slant ranges
C      09  ELEV       Elevation angles
C      10  AZIM       Azimuth angles
C      11  SQUARE     Square-wave product    Ampl  x-Wvl y-Wvl z-Wvl
C                     Airborne only
C      12  ROTANG     Rotation angle
C      13  TILT       Track rel. tilt angle
C      14  RANGEXZ    Range in x-z plane
C      15  YDIST      Y-Distance along track
C
C      00             ANALYT command included, but function asked 
C                     for doesn't exist.
C      P10='ACTUAL' ==> Use actual elevation angle in calculations
C     
      INCLUDE 'SPRINT.INC'
c      PARAMETER (MAXRNG=1024)
c      PARAMETER (NIOB=85000,MAXIN=8500,MAXLEN=MAXIN/4)

      INTEGER FNUM

      COMMON /IO/KPCK(NIOB),KOUT(MAXIN),IBUF(MAXIN),NBUF(MAXLEN,4),
     X     IZ8(17),ILSTREC
      COMMON /TRANS/ X1,X2,XD,Y1,Y2,YD,Z1,Z2,ZD,NX,NY,NZ,XORG,YORG,
     X     ANGXAX,ZRAD,AZLOW,BAD,ASNF,ACSF,IAXORD(3),NPLANE,EDIAM
      CHARACTER*8 P10
      DATA PI /3.14159265/
      DATA DTR /0.0174533/

      IF (FNUM.EQ.0)THEN
         WRITE(*,*)
     X   '***ANALYT COMMAND INCLUDED BUT NO FUNCTION SPECIFIED***'
         RETURN
      END IF

      ISCALE=100

      IF (FNUM.EQ.1) THEN
C     
C     REPLACE FIELD WITH A CONSTANT
C     
         DO 10 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1 .OR. J.GT.J2) GOTO 10
            IF (J.GT.NGFLD) GOTO 10
            KOUT(KST+J)=NINT(P1*ISCALE)
            K=K+1
 10     CONTINUE

      ELSE IF (FNUM.EQ.2) THEN
C     
C     REPLACE FIELD WITH 2 TERM RADIAL VELOCITY
C     
         VRVAL=(P1*SIN(AZZ*DTR) + P2*COS(AZZ*DTR))*
     X        COS(EL*DTR)
         DO 20 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1 .OR. J.GT.J2) GOTO 20
            IF(J.GT.NGFLD) GO TO 20
            KOUT(KST+J)=NINT(VRVAL*ISCALE)
            K=K+1
 20     CONTINUE
         
      ELSE IF (FNUM.EQ.3) THEN
C     
C     REPLACE FIELD WITH 3 TERM RADIAL VELOCITY
C     
         VRVAL=P1*SIN(AZZ*DTR)*COS(EL*DTR) +
     X        P2*COS(AZZ*DTR)*COS(EL*DTR) +
     X        P3*SIN(EL*DTR)
         DO 30 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1 .OR. J.GT.J2) GOTO 30
            IF(J.GT.NGFLD) GO TO 30
            KOUT(KST+J)=NINT(VRVAL*ISCALE)
            K=K+1
 30     CONTINUE

      ELSE IF (FNUM.EQ.4) THEN
C     
C     RANDOM UNIFORM DIST. BETWEEN P1 AND P2
C     
         CON = P2-P1
         DO 40 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1. OR. J.GT.J2) GOTO 40
            IF (J.GT.NGFLD) GOTO 40
            VX=RANF()
            KOUT(KST+J)=NINT((CON*VX + P1)*ISCALE)
            K=K+1
 40     CONTINUE
         
      ELSE IF (FNUM.EQ.5) THEN
C     
C     NORMAL DIST. WITH P1 MEAN AND P2 STAN. DEVIATION
C     
         IF (P2.LE.0.0) P2=1.0
         PI2=ATAN(1.)*8.0
         DO 50 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1. OR. J.GT.J2) GOTO 50
            IF (J.GT.NGFLD) GOTO 50
            U1=RANF()
            U2=RANF()
            VX=SQRT(-2.0*ALOG(U1))*COS(PI2*U2)
            KOUT(KST+J)=NINT((P2*VX + P1)*ISCALE)
            K=K+1
 50     CONTINUE
         
      ELSE IF (FNUM.EQ.6) THEN
C     
C     REPLACE WITH COSINE PRODUCT
C     
         IF (P2.LE.0.0) THEN
            T1=0.0
         ELSE
            T1=2*PI/P2
         END IF
         
         IF (P3.LE.0.0) THEN
            T2=0.0
         ELSE
            T2=2*PI/P3
         END IF
         
         IF (P4.LE.0.0) THEN
            T3=0.0
         ELSE
            T3=2*PI/P4
         END IF

         DO 60 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1. OR. J.GT.J2) GOTO 60
            IF (J.GT.NGFLD) GOTO 60
            R=RG1+(J-1)*DRG
            X=R*SIN(AZZ*DTR)*COS(EL*DTR) + XORR
            Y=R*COS(AZZ*DTR)*COS(EL*DTR) + YORR
            Z=R*SIN(EL*DTR) + ZORR
            KOUT(KST+J)=NINT((P1*COS(T1*X)*COS(T2*Y)*COS(T3*Z))*ISCALE)
            K=K+1
 60     CONTINUE

      ELSE IF (FNUM.EQ.7) THEN
C
C     RADIAL VELOCITY THAT SATISFIES THE CONTINUITY EQUATION
C
         IF (P2.EQ.0.0) THEN
            T1=0.0
         ELSE 
            T1=2*PI/P2
         END IF
         IF (P3.EQ.0.0) THEN
            T2=0.0
         ELSE
            T2=2*PI/P3
         END IF
         IF (P4.EQ.0.0) THEN
            T3=0.0
         ELSE
            T3=2*PI/P4
         END IF
         DO 70 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1. OR. J.GT.J2) GOTO 70
            IF (J.GT.NGFLD) GOTO 70
            R=RG1+(J-1)*DRG
            X=R*SIN(AZZ*DTR)*COS(EL*DTR) + XORR
            Y=R*COS(AZZ*DTR)*COS(EL*DTR) + YORR
            Z=R*SIN(EL*DTR) + ZORR
            U=P1*P2/(4*PI)*SIN(T1*X)*COS(T2*Y)*COS(T3*Z)
            V=P1*P3/(4*PI)*COS(T1*X)*SIN(T2*Y)*COS(T3*Z)
            W=-P1*P4/(2*PI)*COS(T1*X)*COS(T2*Y)*SIN(T3*Z)
            VAL=U*SIN(AZZ*DTR)*COS(EL*DTR) + 
     X          V*COS(AZZ*DTR)*COS(EL*DTR) + W*SIN(EL*DTR)
            KOUT(KST+J)=NINT(VAL*ISCALE)
            K=K+1
 70     CONTINUE

      ELSE IF (FNUM.EQ.8) THEN
C
C     REPLACE WITH RANGE
C
         IF (P1.EQ.0.0) P1=1.0

         DO 80 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1. OR. J.GT.J2) GOTO 80
            IF (J.GT.NGFLD) GOTO 80
            R=RG1+(J-1)*DRG
            KOUT(KST+J)=NINT((P1*R)*ISCALE)
            K=K+1
 80     CONTINUE
         
      ELSE IF (FNUM.EQ.9) THEN
C
C     REPLACE WITH ELEVATION
C
         IF (P1.EQ.0.0) P1=1.0
         DO 90 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1. OR. J.GT.J2) GOTO 90
            IF (J.GT.NGFLD) GOTO 90
            KOUT(KST+J)=NINT((EL*P1)*ISCALE)
            K=K+1
 90     CONTINUE

      ELSE IF (FNUM.EQ.10) THEN
C
C     REPLACE WITH HORIZONTAL AZIMUTH
C
         IF (P1.EQ.0.0) P1=1.0
         DO 100 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1. OR. J.GT.J2) GOTO 100
            IF (J.GT.NGFLD) GOTO 100
            KOUT(KST+J)=NINT((AZZ*P1)*ISCALE)
            K=K+1
 100    CONTINUE

      ELSE IF (FNUM.EQ.11) THEN
C
C     REPLACE WITH PRODUCT OF THREE SQUARE WAVES
C
         IF (P2.LE.0.0) THEN
            T1=0.0
         ELSE
            T1=2*PI/P2
         END IF
         
         IF (P3.LE.0.0) THEN
            T2=0.0
         ELSE
            T2=2*PI/P3
         END IF
         
         IF (P4.LE.0.0) THEN
            T3=0.0
         ELSE
            T3=2*PI/P4
         END IF

         DO 110 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1. OR. J.GT.J2) GOTO 110
            IF (J.GT.NGFLD) GOTO 110
            R=RG1+(J-1)*DRG
            X=R*SIN(AZZ*DTR)*COS(EL*DTR) + XORR
            Y=R*COS(AZZ*DTR)*COS(EL*DTR) + YORR
            Z=R*SIN(EL*DTR) + ZORR
            SIGN1=1
            SIGN2=1
            SIGN3=1
            SIGN1=SIGN(SIGN1,COS(T1*X))
            SIGN2=SIGN(SIGN3,COS(T2*Y))
            SIGN3=SIGN(SIGN3,COS(T3*Z))
            KOUT(KST+J)=NINT((P1*SIGN1*SIGN2*SIGN3)*ISCALE)
            K=K+1
 110     CONTINUE

      ELSE IF (FNUM.EQ.12) THEN
C
C     REPLACE FIELD WITH ROTATION ANGLE
C
         ISCALE=64
         DO 120 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1 .OR. J.GT.J2) GOTO 120
            IF (J.GT.NGFLD) GOTO 120
            KOUT(KST+J)=NINT(ROTANG*ISCALE)
            K=K+1
 120    CONTINUE
         
      ELSE IF (FNUM.EQ.13) THEN
C
C     REPLACE FIELD WITH TRACK RELATIVE TILT ANGLE
C
         DO 130 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1 .OR. J.GT.J2) GOTO 130
            IF (J.GT.NGFLD) GOTO 130
            KOUT(KST+J)=NINT(TILTT*ISCALE)
            K=K+1
 130    CONTINUE

      ELSE IF (FNUM.EQ.14) THEN
C
C     REPLACE FIELD WITH RANGE IN X-Z PLANE (Y IS DIRECTION OF TRACK)
C
         DO 140 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1 .OR. J.GT.J2) GOTO 140
            IF (J.GT.NGFLD) GOTO 140
            R=RG1+(J-1)*DRG
            RGXZ=R*COS(TILTT*DTR)
            KOUT(KST+J)=NINT(RGXZ*ISCALE)
            K=K+1
 140    CONTINUE
         
      ELSE IF (FNUM.EQ.15) THEN
C
C     REPLACE FIELD WITH Y DISTANCE (DISTANCE ALONG TRACK)
C
         DO 150 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1 .OR. J.GT.J2) GOTO 150
            IF (J.GT.NGFLD) GOTO 150
            R=RG1+(J-1)*DRG
            Y=Y0+SIN(TILTT*DTR)*R
            KOUT(KST+J)=NINT(Y*ISCALE)
            K=K+1
 150    CONTINUE
         
      ELSE
         WRITE(*,*)'+++UNKNOWN ANALYTICAL FUNCTION IN ANLACT+++'
         STOP
      END IF
         
      RETURN

      END
