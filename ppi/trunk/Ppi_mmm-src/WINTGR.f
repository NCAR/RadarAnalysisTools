c
c----------------------------------------------------------------------X
c
      SUBROUTINE WINTGR(DAT,IOUT,IIN1,IIN2,C1,C2,C3,C4,BDVAL,
     X                  MNGATE,MXGATE,NANG,ANG,AVGI,H0,R0,DROLD,
     X                  TMP1,TMP2,MXR,MXA,MXF)
C
C  FUNCTION - INTGRAT: F(OUT)=INTEGRAL OF F(IN) IN THE ANGLE DIRECTION
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "   (RADIAL CONVERGENCE)
C     IIN2   -  INPUT   "      "   (RADIAL VELOCITY)
C     C1     - INTEGRATION DIRECTION (UP,DN)=(+1,-1) AND DENSITY SLOPE
C     C2     - FRACTION OF CONVERGENCE FOR BOUNDARY CONDITION
C     C3,C4  - HEIGHT INTERVAL FOR INTEGRATION
C     IDIR   - DIRECTION OF THE INTEGRATION (UP,DN)=(+1,-1) (FROM C1)
C     JDIR   -     "      "  "  SCAN (UP,DN)=(+1,-1) (FROM AVGI)
C     TMP1   - TEMPORARY STORAGE ARRAY
C     DATIN1 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN1
C
      DIMENSION TMP1(MXR),TMP2(MXR,MXA)
      DIMENSION DAT(MXR,MXA,MXF),ANG(MXA,2)

      DATA RE,REI/17000.0,1.17647E-04/
      DATA TORAD,TODEG/0.017453293,57.29577951/
      DATA AK,WMAX/0.1,50.0/

      IDIR=C1
      AK=ABS(C1-IDIR)
      JDIR=NINT(AVGI/ABS(AVGI))
      KDIR=IDIR*JDIR
      IF(KDIR.GE.0.0)THEN
         J1=1
         J2=NANG
      ELSE
         J1=NANG
         J2=1
      END IF
      DO 100 I=1,MXR
         IF(I.LT.MNGATE.OR.I.GT.MXGATE)THEN
            DO 80 J=1,MXA
               DAT(I,J,IOUT)=BDVAL
   80       CONTINUE
         ELSE
            RNG=R0+(I-1)*DROLD
            JINT=0
            KINT=0
            DO 90 J=J1,J2,KDIR
               SINA=SIN(TORAD*ANG(J,1))
               COSA=COS(TORAD*ANG(J,1))
               Z=H0+RNG*SINA+0.5*RNG*RNG*COSA*COSA*REI
               IF(Z.LT.C3.OR.Z.GT.C4)THEN
                  TMP1(J)=BDVAL
                  JINT=0
                  KINT=0
                  GO TO 90
               END IF

C              JINT=0: SET BOUNDARY CONDITION AT CURRENT LEVEL
C              JINT=1: CONTINUE INTEGRATION UNTIL ENCOUNTER BDVAL
C
               IF(AK.EQ.0.0)THEN
                  DENS=1.0
               ELSE
                  DENS=EXP(-AK*Z)
               END IF
               IF(JINT.EQ.0)THEN
                  IF(DAT(I,J,IIN1).EQ.BDVAL.OR.
     +               DAT(I,J,IIN2).EQ.BDVAL)THEN
                     TMP1(J)=BDVAL
                     JINT=0
                     GO TO 90
                  ELSE
                     DATIN=DENS*DAT(I,J,IIN1)
                     TMP1(J)=C2*RNG*TORAD*KDIR*ABS(AVGI)*DATIN
     +                     -DENS*SINA*DAT(I,J,IIN2)/COSA
                     JINT=1
                     KINT=1
                     GO TO 90
                  END IF
               ELSE
                  DATC=DAT(I,J,IIN1)
                  DATP=DAT(I,J-KDIR,IIN1)
                  IF(DATC.EQ.BDVAL.OR.
     +               DATP.EQ.BDVAL.OR.KINT.EQ.0)THEN
                     TMP1(J)=BDVAL
                     KINT=0
                     GO TO 90
                  ELSE
                     DEL=RNG*TORAD*(ANG(J,1)-ANG(J-KDIR,1))
                     TMP1(J)=TMP1(J-KDIR)+0.5*DEL*DENS*(DATC+DATP)
                  END IF
               END IF
   90       CONTINUE

C           MOVE THE INTEGRATION INTO OUTPUT ARRAY AND GO TO THE NEXT RANGE
C
            IF(KDIR.GT.0)THEN
               J1=1
               J2=MXA
            ELSE
               J1=MXA
               J2=1
            END IF
            DO 94 J=J1,J2,KDIR
               IF(J.GT.NANG)THEN
                  DAT(I,J,IOUT)=BDVAL
               ELSE
                  IF(AK.EQ.0.0)THEN
                     DENS=1.0
                  ELSE
                     SINA=SIN(TORAD*ANG(J,1))
                     COSA=COS(TORAD*ANG(J,1))
                     Z=H0+RNG*SINA+0.5*RNG*RNG*COSA*COSA*REI
                     DENS=EXP(-AK*Z)
                  END IF
                  DAT(I,J,IOUT)=TMP1(J)/DENS
                  IF(ABS(DAT(I,J,IOUT)).GE.WMAX)DAT(I,J,IOUT)=BDVAL
               END IF
   94       CONTINUE
         END IF

  100 CONTINUE
      RETURN
      END
