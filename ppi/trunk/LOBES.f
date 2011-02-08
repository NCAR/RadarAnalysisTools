c
c----------------------------------------------------------------------X
c
      SUBROUTINE LOBES(DAT,IOUT,NGRD,C1,C2,C3,C4,X0,Y0,BDVAL,RNG,AZA,
     X     MNGATE,MXGATE,MANG,ISW,FXOLD,ITPOLD,MXR,MXA,MXF,NSCTP)
C
C  FUNCTION - LOBES: F(OUT)=ANGLE BETWEEN BEAMS FOR RADARS AT
C                           (X1,Y1)=(C1,C2) AND (X2,Y2)=(C3,C4)
C
C                     Left
C            1-------------------2--->
C                     Right
C
C     RNG       - Distance (km) to the range gates
C     AZA       - Angle which varies during the scan (azimuth or elevation)
C     FXOLD     - Fixed angle during scan.
C     ITPOLD    - Scanning mode [RHI (3): AZA contains elevation angle;
C                              otherwise, AZA contains azimuth angle]
C     X0,Y0     - (X,Y) coordinates for this radar
C
C     IOUT   - Output field number
C
      DIMENSION DAT(MXR,MXA,MXF),RNG(MXR,2),AZA(MXA,2)
      DATA RE,TORAD,TODEG/17000.0,0.017453293,57.29577951/
      CHARACTER*3 NGRD
      CHARACTER*8 NSCTP

C     Azimuth angle of radar #2 at (C3,C4) from radar #1 at (C1,C2)
C
C               y
C               | (C3,C4)
C               |   /
C               |  /
C               | /
C               |/________x
C            (C1,C2)
C
C     If NSCTP = 'LEFT'  - AZR < 0.0
C                'RIGHT' - AZR > 0.0
C                'BOTH'  - All locations
      call sflush
c-----do m=1,100
c--------print *,'LOBES: ngrd,x0,y0=',ngrd,x0,y0
c--------call sflush
c--------print *,'LOBES: c1-c4=',c1,c2,c3,c4 
c--------call sflush
c-----end do
c      
      DX=C3-C1
      DY=C4-C2
      IF(DY.EQ.0.0.AND.DX.EQ.0.0)THEN
         BAZ=180.0
      ELSE IF(DY.EQ.0.0.AND.DX.GT.0.0)THEN
         BAZ=90.0
      ELSE IF(DY.EQ.0.0.AND.DX.LT.0.0)THEN
         BAZ=270.0
      ELSE
         BAZ=TODEG*ATAN2(DX,DY)
      END IF
      IF(BAZ.LT.0.0)BAZ=BAZ+360.0
      
C     Note: If ISW=1, coordinates only at original sampling locations
C              ISW=2, coordinates at regular range-angle grid
C     Others  - AZA contains azimuth   angle and FXOLD = elevation
C
      IF(NGRD.EQ.'REG')THEN
         ISW=2
      ELSE
         ISW=1
      END IF

      print *,'LOBES:    iout,ngrd=',iout,ngrd
      print *,'LOBES:      c1-2-34=',c1,c2,c3,c4
      print *,'LOBES:        dx,dy=',dx,dy
      print *,'LOBES:    mn-mggate=',mngate,mxgate
      print *,'LOBES:     mang,isw=',mang,isw
      print *,'LOBES: fxold,itpold=',fxold,itpold
      print *,'LOBES:  mxr,mxa,mxf=',mxr,mxa,mxf
      print *,'LOBES:        nsctp=',nsctp

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

C           Azimuth angle of (X,Y) from (C1,C2)
C
            XP1=X-C1
            YP1=Y-C2
            IF(YP1.EQ.0.0.AND.XP1.EQ.0.0)THEN
               AZ1=180.0
            ELSE IF(YP1.EQ.0.0.AND.XP1.GT.0.0)THEN
               AZ1=90.0
            ELSE IF(YP1.EQ.0.0.AND.XP1.LT.0.0)THEN
               AZ1=270.0
            ELSE
               AZ1=TODEG*ATAN2(XP1,YP1)
            END IF
            IF(AZ1.LT.0.0)AZ1=AZ1+360.0

C           Azimuth angle of (X,Y) from (C3,C4)
C
            XP2=X-C3
            YP2=Y-C4
            IF(YP2.EQ.0.0.AND.XP2.EQ.0.0)THEN
               AZ2=180.0
            ELSE IF(YP2.EQ.0.0.AND.XP2.GT.0.0)THEN
               AZ2=90.0
            ELSE IF(YP2.EQ.0.0.AND.XP2.LT.0.0)THEN
               AZ2=270.0
            ELSE
               AZ2=TODEG*ATAN2(XP2,YP2)
            END IF
            IF(AZ2.LT.0.0)AZ2=AZ2+360.0

            AZD1=ABS(AZ2-AZ1)
            AZD2=ABS(AZD1-360.0)
            AZD3=AMIN1(AZD1,AZD2)
            AZD4=180.0-AZD3
            AZD=AMIN1(AZD3,AZD4)
            DAT(I,J,IOUT)=AZD

C     If NSCTP = 'LEFT'  - AZR < 0
C                'RIGHT' - AZR > 0
C                'BOTH'  - All locations
C
            AZR=AZ1-BAZ
            IF(AZR.GT. 180.0)AZR=AZR-360.0
            IF(AZR.LT.-180.0)AZR=AZR+360.0
            IF(NSCTP(1:4).EQ.'LEFT'  .AND.AZR.GT.0.0)DAT(I,J,IOUT)=BDVAL
            IF(NSCTP(1:5).EQ.'RIGHT' .AND.AZR.LT.0.0)DAT(I,J,IOUT)=BDVAL

 90      CONTINUE
 100  CONTINUE

      call sflush
      RETURN
      END
