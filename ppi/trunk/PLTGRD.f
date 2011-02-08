c
c----------------------------------------------------------------------X
c
      SUBROUTINE PLTGRD(DAT,IFLU,IFLV,IRSKIP,IASKIP,MNGATE,MXGATE,
     X                  NANG,BDVAL,XMN,XMX,YMN,YMX,USTRM,VSTRM,
     X                  VECSCL,AZA,ELA,X0,Y0,H0,R0,DROLD,IVECCOL,
     X                  ITPOLD,MXR,MXA,MXF)
C
C  DRAW THE VECTOR GRADIENT FROM TX,TY AND SCALED BY VECSCL DF/DR PER KM
C     IFLU   - FIELD INDEX FOR  RADIAL COMPONENT OF GRADIENT OF F(R,A)
C     IFLV   -   "     "    "  ANGULAR     "      "     "     "   "

C     IRSKIP - RANGE GATE SKIPPING FACTOR
C     IASKIP - ELEVATION ANGLE SKIPPING FACTOR
C     VECSCL- SCALED LENGTH OF THE ARROW HEAD
C     USTRM  - AMOUNT TO BE SUBTRACTED FROM  RADIAL COMPONENT
C     VSTRM  -    "    "  "     "        "  ANGULAR     "
C
      DIMENSION DAT(MXR,MXA,MXF),AZA(MXA,2),ELA(MXA,2)
      DATA RE,REI/17000.0,1.17647E-04/
      DATA SIN23,COS23,HSCL/0.391,0.921,0.25/
      DATA TORAD,TODEG/0.017453293,57.29577951/

      IF(ITPOLD.EQ.3)THEN
         XR=0.0
         YR=0.0
      ELSE
         XR=X0
         YR=Y0
      END IF
      CALL SFLUSH
      CALL GSPLCI(IVECCOL)
      DO 100 J=1,NANG,IASKIP
         IF(ITPOLD.EQ.3)THEN
            SINA=COS(AZA(J,1)*TORAD)
            COSA=SIN(ELA(J,1)*TORAD)
            COSE=1.0
         ELSE
            SINA=SIN(AZA(J,1)*TORAD)
            COSA=COS(AZA(J,1)*TORAD)
            COSE=COS(ELA(J,1)*TORAD)
         END IF
         DO 90 I=MNGATE,MXGATE,IRSKIP
            U=DAT(I,J,IFLU)
            V=DAT(I,J,IFLV)
            IF(U.EQ.BDVAL.OR.V.EQ.BDVAL)GO TO 90
            RNG=R0+(I-1)*DROLD
            HRNG=RNG*COSE
            TX=XR+HRNG*SINA
            TY=YR+HRNG*COSA
            IF(TY.LT.YMN.OR.TY.GT.YMX)GO TO 90
            IF(TX.LT.XMN.OR.TX.GT.XMX)GO TO 90
            HX=TX+COSE*(U*SINA+V*COSA)/VECSCL
            HY=TY+COSE*(U*COSA-V*SINA)/VECSCL
            IF(HX.LT.XMN.OR.HX.GT.XMX)GO TO 90
            IF(HY.LT.YMN.OR.HY.GT.YMX)GO TO 90
            UU=COSE*(U*SINA+V*COSA)
            VV=COSE*(U*COSA-V*SINA)
            DX=HX-TX
            DY=HY-TY
            EX1=HX-HSCL*(DX*COS23-DY*SIN23)
            EY1=HY-HSCL*(DY*COS23+DX*SIN23)
            EX2=HX-HSCL*(DX*COS23+DY*SIN23)
            EY2=HY-HSCL*(DY*COS23-DX*SIN23)
            CALL LINE(TX,TY,HX,HY)
            CALL LINE(HX,HY,EX1,EY1)
            CALL LINE(HX,HY,EX2,EY2)
   90    CONTINUE
  100 CONTINUE
      RETURN
      END
