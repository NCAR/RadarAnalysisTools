c
c----------------------------------------------------------------------X
c
      SUBROUTINE DETRND(DAT,IIN1,IC,JC,I1,I2,J1,J2,AZA,DROLD,R0,
     X     ITP,BDVAL,TMP2,IFLG,NPROC,VEST,VNYQ,MXR,MXA,MXF)

C     Routine to find the local trend by a linear least-squares
C     fit over the dataset subregion (I1,I2) by (J1,J2), centered
C     on the (IC,JC) location.
C
C     VNYQ   - NYQUIST VELOCITY FOR LOCAL UNFOLDING
C     NPROC  - SPECIAL PROCESSING (LINEAR, UNFOLD)
C
      DIMENSION DAT(MXR,MXA,MXF),AZA(MXA,2),TMP2(MXR,MXA)
      DATA TORAD/0.017453293/
      LOGICAL CENTRAL
      CHARACTER*8 NPROC

C     Get (X,Y) coordinates for the central value
C
c      IC=0.5*(I1+I2)
c      JC=0.5*(J1+J2)
      R=R0+(IC-1)*DROLD
      COSAZ=COS(AZA(JC,1)*TORAD)
      SINAZ=SIN(AZA(JC,1)*TORAD)
      XC=R*SINAZ
      YC=R*COSAZ
      
      IFLG=1

C     INITIALIZE SUMMATION ARRAYS FOR LEAST-SQUARES PLANE FIT.
C     COMPUTE COEFFICIENTS FOR LINEAR LEAST-SQUARES FIT.
C
      RNPTS=0.
      SX=0.
      SY=0.
      SX2=0.
      SY2=0.
      SD=0.
      SDX=0.
      SDY=0.
      SXY=0.

C     If coefficients are calculated as a central point expansion,
C     then the function must also be evaluated as central point
C     expansion. 
C
      CENTRAL = .TRUE.

      IF(ITP.NE.3)THEN

C     HERE FOR PPIS
C
         DO 50 J=J1,J2
            COSAZ=COS(AZA(J,1)*TORAD)
            SINAZ=SIN(AZA(J,1)*TORAD)
            DO 40 I=I1,I2
               IF(DAT(I,J,IIN1).EQ.BDVAL)GO TO 40
               IF(NPROC.EQ.'LINEAR')THEN
                  D=10.0**(0.1*DAT(I,J,IIN1))
               ELSE IF(NPROC.EQ.'UNFOLD')THEN
                  D=VUNF(DAT(I,J,IIN1),VNYQ,VEST)
               ELSE
                  D=DAT(I,J,IIN1)
               END IF
               R=R0+(I-1)*DROLD
               XI=R*SINAZ
               YI=R*COSAZ
               IF(CENTRAL)THEN
                  X=XI-XC
                  Y=YI-YC
               ELSE
                  X=XI
                  Y=YI
               END IF
               RNPTS=RNPTS+1.
               SX=SX+X
               SY=SY+Y
               SXY=SXY+X*Y
               SX2=SX2+X*X
               SY2=SY2+Y*Y
               SD=SD+D
               SDX=SDX+X*D
               SDY=SDY+Y*D
 40         CONTINUE
 50      CONTINUE

c--------IF(RNPTS.LT.5.)RETURN

         T1=SX2*SY2-SXY*SXY
         T2=SX*SY2-SXY*SY
         T3=SX*SXY-SX2*SY
         DENOM=RNPTS*T1-SX*T2+SY*T3
         IF(DENOM.LE.0.00001)RETURN

         IFLG=0
         ANUM=SD*T1-SDX*T2+SDY*T3
         BNUM=RNPTS*(SDX*SY2-SDY*SXY)-SD*(SX*SY2-SXY*SY)+
     +        SY*(SX*SDY-SY*SDX)
         CNUM=RNPTS*(SX2*SDY-SXY*SDX)-SX*(SX*SDY-SY*SDX)+
     +        SD*(SX*SXY-SY*SX2)
         A=ANUM/DENOM
         B=BNUM/DENOM
         C=CNUM/DENOM

         DO 100 J=J1,J2
            COSAZ=COS(AZA(J,1)*TORAD)
            SINAZ=SIN(AZA(J,1)*TORAD)
            DO 90 I=I1,I2
               R=R0+(I-1)*DROLD
               TMP2(I,J)=BDVAL
c               IF(DAT(I,J,IIN1).EQ.BDVAL)GO TO 90
               X=R*SINAZ
               Y=R*COSAZ
               IF(CENTRAL)THEN
                  TMP2(I,J)=A+B*(X-XC)+C*(Y-YC)
               ELSE
                  TMP2(I,J)=A+B*X+C*Y
               END IF
 90         CONTINUE
 100     CONTINUE

         RETURN

      ELSE

C HERE FOR RHIS
C
         CENTRAL = .FALSE.

         DO 150 J=J1,J2
            COSAZ=COS(AZA(J,1)*TORAD)
            SINAZ=SIN(AZA(J,1)*TORAD)
            DO 140 I=I1,I2
               IF(DAT(I,J,IIN1).EQ.BDVAL)GO TO 140
               IF(NPROC.EQ.'LINEAR')THEN
                  D=10.0**(0.1*DAT(I,J,IIN1))
               ELSE IF(NPROC.EQ.'UNFOLD')THEN
                  D=VUNF(DAT(I,J,IIN1),VNYQ,VEST)
               ELSE
                  D=DAT(I,J,IIN1)
               END IF
               R=R0+(I-1)*DROLD
               X=R*COSAZ
               Y=R*SINAZ
               RNPTS=RNPTS+1.
               SX=SX+X
               SY=SY+Y
               SXY=SXY+X*Y
               SX2=SX2+X*X
               SY2=SY2+Y*Y
               SD=SD+D
               SDX=SDX+X*D
               SDY=SDY+Y*D
 140        CONTINUE
 150     CONTINUE

c--------IF(RNPTS.LT.5.)RETURN

         T1=SX2*SY2-SXY*SXY
         T2=SX*SY2-SXY*SY
         T3=SX*SXY-SX2*SY
         DENOM=RNPTS*T1-SX*T2+SY*T3
         IF(DENOM.LE.0.00001)RETURN

         IFLG=0
         ANUM=SD*T1-SDX*T2+SDY*T3
         BNUM=RNPTS*(SDX*SY2-SDY*SXY)-SD*(SX*SY2-SXY*SY)+
     +        SY*(SX*SDY-SY*SDX)
         CNUM=RNPTS*(SX2*SDY-SXY*SDX)-SX*(SX*SDY-SY*SDX)+
     +        SD*(SX*SXY-SY*SX2)
         A=ANUM/DENOM
         B=BNUM/DENOM
         C=CNUM/DENOM
C         IF(I1.EQ.30)PRINT 230,A,B,C
 230     FORMAT(1X,'A,B,C= ',3F10.4)

         DO 200 J=J1,J2
            COSAZ=COS(AZA(J,1)*TORAD)
            SINAZ=SIN(AZA(J,1)*TORAD)
            DO 190 I=I1,I2
               R=R0+(I-1)*DROLD
               TMP2(I,J)=BDVAL
c               IF(DAT(I,J,IIN1).EQ.BDVAL)GO TO 190
               X=R*COSAZ
               Y=R*SINAZ
               TMP2(I,J)=A+B*X+C*Y
 190        CONTINUE
 200     CONTINUE
         RETURN
         
      END IF

      END

