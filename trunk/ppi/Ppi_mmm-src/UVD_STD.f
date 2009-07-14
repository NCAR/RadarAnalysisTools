c
c----------------------------------------------------------------------X
c
      SUBROUTINE UVD_STD(DAT,IOUT,NIN1,NGRD,XRAD,YRAD,KRAD,X0,Y0,BDVAL,
     X     RNG,AZA,MNGATE,MXGATE,MANG,ISW,FXOLD,ITPOLD,MXR,MXA,MXF)
C
C  FUNCTION - UVDSTD: F(OUT)=Std(u,v, or div) for KRAD radars at
C                            (XRAD,YRAD) locations.
C
C     RNG       - Distance (km) to the range gates
C     AZA       - Angle which varies during the scan (azimuth or elevation)
C     FXOLD     - Fixed angle during scan.
C     ITPOLD    - Scanning mode [RHI (3): AZA contains elevation angle;
C                              otherwise, AZA contains azimuth angle]
C     KRAD      - Number of radars (k .le. 5)
C     XRAD,YRAD - (X,Y) coordinates for kth radar
C
C     IOUT   - Output field number
C
      DIMENSION DAT(MXR,MXA,MXF),RNG(MXR,2),AZA(MXA,2)
      DIMENSION XRAD(5),YRAD(5)
      DATA TORAD,TODEG/0.017453293,57.29577951/
      DATA DXY,EPS/1.0,1.0E-05/
      CHARACTER*3 NGRD
      CHARACTER*8 NIN1

C     Note: If ISW=1, coordinates only at original sampling locations
C              ISW=2, coordinates at regular range-angle grid
C     Others  - AZA contains azimuth   angle and FXOLD = elevation
C
      IF(NGRD.EQ.'REG')THEN
         ISW=2
      ELSE
         ISW=1
      END IF

      DO 100 J=1,MANG

C        RHI scan:
C
         IF(ITPOLD.EQ.3)THEN
            AZRAD=FXOLD
            ELRAD=AZA(J,ISW)

C        All other scans:
C
         ELSE
            AZRAD=AZA(J,ISW)
            ELRAD=FXOLD
         END IF
         SINA=SIN(AZRAD*TORAD)
         COSA=COS(AZRAD*TORAD)
         SINE=SIN(ELRAD*TORAD)
         COSE=COS(ELRAD*TORAD)

         DO 90 I=MNGATE,MXGATE
            DAT(I,J,IOUT)=BDVAL
            HRNG=COSE*RNG(I,ISW)
            X=X0+HRNG*SINA
            Y=Y0+HRNG*COSA

            A1=0.0
            A2=0.0
            B1=0.0
            B2=0.0

            DO K=1,KRAD
               XR=X-XRAD(K)
               YR=Y-YRAD(K)
               XXR=XR*XR
               YYR=YR*YR
               XYR=XR*YR
               A1=A1+XXR
               A2=A2+XYR
               B1=B1+XYR
               B2=B2+YYR
            END DO
            DENOM=A1*B2-A2*B1
            IF(DENOM.LE.EPS)THEN
               DAT(I,J,IOUT)=BDVAL
               GO TO 90
            END IF

            U_VAR=0.0
            V_VAR=0.0
            DO K=1,KRAD
               XR=X-XRAD(K)
               YR=Y-YRAD(K)
               XXR=XR*XR
               YYR=YR*YR
               HRNG=XXR+YYR
               WU=(B2*XR-B1*YR)/DENOM
               WV=(A1*YR-A2*XR)/DENOM
               U_VAR=U_VAR+WU*WU*HRNG
               V_VAR=V_VAR+WV*WV*HRNG
            END DO

            D_VAR=(U_VAR+V_VAR)/(2.0*DXY*DXY)
            IF(U_VAR.GE.0.0)THEN
               U_STD=SQRT(U_VAR)
            ELSE
               U_STD=BDVAL
            END IF
            IF(V_VAR.GE.0.0)THEN
               V_STD=SQRT(V_VAR)
            ELSE
               V_STD=BDVAL
            END IF
            IF(D_VAR.GE.0.0)THEN
               D_STD=SQRT(D_VAR)
            ELSE
               D_STD=BDVAL
            END IF

            IF(NIN1.EQ.'USTD')THEN
               DAT(I,J,IOUT)=U_STD
            ELSE IF(NIN1.EQ.'VSTD')THEN
               DAT(I,J,IOUT)=V_STD
            ELSE IF(NIN1.EQ.'DSTD')THEN
               DAT(I,J,IOUT)=D_STD
            END IF

 90      CONTINUE
 100  CONTINUE

      RETURN
      END
