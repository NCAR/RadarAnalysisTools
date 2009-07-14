c
c----------------------------------------------------------------------X
c
      SUBROUTINE GRAD_MAG(DAT,IOUT,IIN1,C1,C2,BDVAL,MNGATE,MXGATE,NGTS,
     X     R0,DROLD,AZA,ISW,NANG,FXOLD,ITPOLD,MXR,MXA,MXF)
C
C  FUNCTION - 2D GRADIENT:
C             F(I,J,OUT)=SQRT[(DF/DR)^2 + (DF/DA)^2]
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "
C     C2     - FINITE DIFFERENCE DISTANCE (RANGE)
C     AZA    - AZIMUTH (ELEVATION) ANGLES OF RADAR BEAMS
C     ITPOLD    - SCANNING MODE [RHI (3): AZA CONTAINS ELEVATION ANGLE;
C                                         ELA     "     AZIMUTH    "  ]
      DIMENSION DAT(MXR,MXA,MXF),AZA(MXA,2)
      DATA TORAD,TODEG/0.017453293,57.29577951/

      CON=1.0/(2.0*C2*DROLD)
      IC2=NINT(C2)

      IF(ITPOLD.EQ.3)THEN
         COSE=1.0
      ELSE
         COSE=COS(TORAD*FXOLD)
      ENDIF
      print *,'GRAD: itpold,fxold,cose=',itpold,fxold,cose

      IMN=MAX0(IC2+1,MNGATE)
      IMX=MIN0(NGTS-IC2,MXGATE)

      DO J=1,MXA
         DO I=1,MXR
            DAT(I,J,IOUT)=BDVAL
         END DO
      END DO

      DO 100 J=2,NANG-1
         AINCR=AZA(J+1,ISW)-AZA(J-1,ISW)
         IF(AINCR.GT.180.0)AINCR=360.0-AINCR
         DO 90 I=IMN,IMX
            DAT(I,J,IOUT)=BDVAL
            
            DAT1=DAT(I-IC2,J,IIN1)
            DAT2=DAT(I+IC2,J,IIN1)
            IF(DAT1.NE.BDVAL.AND.DAT2.NE.BDVAL)THEN
               DF_DR=CON*(DAT2-DAT1)
            ELSE
               DF_DR=BDVAL
            END IF
            
            DAT1=DAT(I,J-1,IIN1)
            DAT2=DAT(I,J+1,IIN1)
            RNG=R0+(I-1)*DROLD
            DENOM=RNG*COSE*TORAD*AINCR
            IF(DENOM.EQ.0.0)GO TO 90
            IF(DAT1.NE.BDVAL.AND.DAT2.NE.BDVAL)THEN
               DF_DA=(DAT2-DAT1)/DENOM
            ELSE
               DF_DA=BDVAL
            END IF

            IF(DF_DR.NE.BDVAL.AND.DF_DA.NE.BDVAL)THEN
               DAT(I,J,IOUT)=SQRT(DF_DR*DF_DR+DF_DA*DF_DA)
            END IF

 90      CONTINUE
 100  CONTINUE

      RETURN
      END
