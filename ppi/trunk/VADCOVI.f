c
c----------------------------------------------------------------------X
c
      SUBROUTINE VADCOVI(IOUT,IIN1,IIN2,C1,C2,C3,C4,NPRNT,NAMDBZ,H0)
C
C  FUNCTION - COMPUTE VARIANCE AND COVARIANCE TERMS USING CURRENT-SWEEP
C             RADIAL VELOCITIES AND VAD-DERIVED RADIAL VELOCITIES
C             F(I,J,OUT)=[VR(I,J,IIN1)-VRVAD(I,J,IIN2)]**2
C
C     IOUT   - OUTPUT FIELD NUMBER (CONTAINS SQUARED RESIDUAL)
C     IIN1   -  INDEX OF CURRENT-SWEEP RADIAL VELOCITY FIELD
C     IIN2   -  INDEX OF VAD FIELD TO BE USED IN VARIANCE/COVARIANCE
C     C1     - MINIMUM NUMBER OF GOOD DATA POINTS FOR VAD ANALYSIS
C     C2     - MAXIMUM ALLOWED AZIMUTH GAP         "   "      "
C     C3     -    "       "    RMS DIFFERENCE BETWEEN INPUT AND VAD WINDS
C     C4     - ORDER OF FIT [LINEAR INCLUDES A(1), A(2), B(1), B(2)]
C     NPRNT  - PRINT FLAG FOR VAD WINDS ('    ') NO, ('PRNT') YES,
C              ('FILE') YES and COEFFICIENTS TO ASCII FILE (fort.99).
C     NAMFLD - NAME LIST OF ALL FUNCTION-DERIVED FIELDS
C     NAMVD  -   "    "   " ONLY   VAD-DERIVED     "
C     MVD    - NUMBER OF VAD-DERIVED FIELDS (MXVD=10)
C  VAD MEAN OUTPUT QUANTITIES STORED IN COMMON/VADWINDS/:
C     U0,V0  - HORIZONTAL WINDS       FOR THE ITH RANGE GATE
C     SPD    -     "      WIND SPEED   "   "   "    "     "
C     DIR    -     "       "   DIREC   "   "   "    "     "
C     CON    -     "      CONVERGENCE  "   "   "    "     "
C     WVD    - VERTICAL WIND (CON)     "   "   "    "     "
C     STR    - STRETCHING DEFORMATION  "   "   "    "     "
C     SHR    - SHEARING        "       "   "   "    "     "
C     ERR    - RMS DIFFERENCE BETWEEN FOURIER FIT AND INPUT RADIAL VEL
C     DBZ    - MEAN REFLECTIVITY FACTOR FOR THE ITH RANGE GATE
C     U_VD   - AMOUNT TO SUBTRACT FROM U-COMPONENT
C     V_VD   -    "    "     "      "  V    "
C     AZMVD  - AZIMUTH OF POSITIVE U DIRECTION
C
C     VARUVW - SUM OF (U,V,W) VARIANCES
C     COV_UW - COVARIANCE (U,W)
C     COV_VW - COVARIANCE (V,W)
C     COV_UV - COVARIANCE (U,V)
C
      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'
      INCLUDE 'input.inc'
      INCLUDE 'swth.inc'
      INCLUDE 'vadwinds.inc'

      COMMON /INPUTCH/NAMFLD(MXF),IRATYP,ICORD
      CHARACTER*8 NAMFLD,IRATYP,ICORD

      DATA TORAD,TODEG,PI/0.017453293,57.29577951,3.141592654/
      DATA CNT0,GAP0,RMS0/15.0,30.0,999.9/
      DATA EPS/1.0E-4/

      DIMENSION RINTGR(4),CNT(4)
      LOGICAL PLTSW
      CHARACTER*8 NPRNT,AVTYP,NAMDBZ
      CHARACTER*4 TYPOUT

      CHARACTER*8 AVNAM
      DATA AVNAM/'??????? '/

      IF(FXOLD.GT.60.0)THEN
         WRITE(6,5)FXOLD
 5       FORMAT(1X,'*** NO VAR/COV ANALYSIS: E> ',F6.2,' DEG ***')
         RETURN
      END IF

C  FIND INDEX FOR THE VAD FIELD NAME BEING USED AS INPUT (IIN2)
C
      KVD=IFIND(NAMFLD(IIN2),NAMVD,MXVD)

      IF(C1.GT.0.0)THEN
         CNTMN=C1
      ELSE
         CNTMN=CNT0
      END IF
      IF(C2.GT.0.0)THEN
         TGAP=C2
      ELSE
         TGAP=GAP0
      END IF
      IF(C3.GT.0.0)THEN
         RMSMX=C3
      ELSE
         RMSMX=RMS0
      END IF
      
      SINE=SIN(TORAD*FXOLD)
      COSE=COS(TORAD*FXOLD)
      COEF1=1.0/(PI*COS(TORAD*FXOLD)**2)
      COEF2=1.0/(4.0*SIN(2.0*TORAD*FXOLD))
      COEF3=1.0/(4.0*COS(TORAD*FXOLD)**2)

      MING=MAX0(MNGATE,1)
      MAXG=MIN0(MXGATE,MXG,MXR)
      DO 10 I=1,MAXG
         VARUVW(I,KVD)=BDVAL
         COV_UW(I,KVD)=BDVAL
         COV_VW(I,KVD)=BDVAL
         COV_UV(I,KVD)=BDVAL
 10   CONTINUE

C     COMPUTE AVERAGE OR INTEGRAL THROUGH CURRENT SWEEP
C
      IF(IFLD(IIN1).LT.0.AND.IFLD(IIN2).LT.0)THEN
         ISW=2
         PLTSW=.TRUE.
         GSPC=DRSW
         IFLD(IOUT)=-1
      ELSE
         ISW=1
         PLTSW=.FALSE.
         GSPC=DROLD
         IF(NAMFLD(IOUT).EQ.NAMFLD(IIN1))THEN
            IFLD(IOUT)=IFLD(IIN1)
         ELSE
            IFLD(IOUT)=0
         END IF
      END IF
      AVTYP='SNGLSWP '
      ITIM1=IFTIME
      ITIM2=ITIME
      WRITE(*,*)"VADCOVI: ",NAMFLD(IOUT),NAMFLD(IIN1),NAMFLD(IIN2),
     +     PLTSW,NPRNT

      IF(IFLD(IIN1).EQ.-5.OR.IFLD(IIN1).EQ.-6)THEN
         CALL AVRAGE(DAT,MXR,MXA,MXF,BDVAL,MNGATE,MXGATE,NANG,
     X        NAMFLD,IFLD,IIN1,AVNAM)
         AVTYP='AVERAGE '
         ITIM1=ITIME1
         ITIM2=ITIME2
      END IF

      IF(IFLD(IIN2).EQ.-5.OR.IFLD(IIN2).EQ.-6)THEN
         CALL AVRAGE(DAT,MXR,MXA,MXF,BDVAL,MNGATE,MXGATE,NANG,
     X        NAMFLD,IFLD,IIN2,AVNAM)
         AVTYP='AVERAGE '
         ITIM1=ITIME1
         ITIM2=ITIME2
      END IF

      IF(NPRNT.EQ.'FILE')THEN
         CALL LABELFL(PLTSW)
         WRITE(99,13)MSCAN,AVTYP,ITIM1,ITIM2,
     +        NAMFLD(IIN2),IIN2,IFLD(IIN2),
     +        NAMFLD(IIN1),IIN1,IFLD(IIN1),
     +        NAMFLD(IOUT),IOUT,IFLD(IOUT)
 13      FORMAT(2X,'SCAN #',I3,2X,A8,I6.6,'-',I6.6,
     +        2X,'COVI (NAME,INDEX,TYPE): VAD=',A8,2I4,
     +        '  IN=',A8,2I4,' OUT=',A8,2I4,' *VADCOVI*')
      END IF

C     LOOP OVER ALL GATES AND ANGLES - COMPUTE VARIANCE AND COVARIANCE
C
      DO 100 I=MING,MAXG
         IF(RNG(I,ISW).LE.EPS)GO TO 100
         Z=H0+RNG(I,ISW)*SINE
         DO K=1,4
            RINTGR(K)=0.0
            CNT(K)=0.0
         END DO
         GAPMX=-999.0
         GAPMN=999.0
         ALFT=BDVAL

C        LOOP OVER ALL ANGLES AND ACCUMULATE SEPARATE INTEGRALS
C           RINTGR(1):   0, 90 deg; RINTGR(2):  90,180 deg
C           RINTGR(3): 180,270 deg; RINTGR(4): 270,360 deg
C
         DO 90 J=1,NANG(ISW)
            ANG=AZA(J,ISW)
            IF(ANG.LT.0.0)ANG=ANG+360.0
            IF(DAT(I,J,IIN1).NE.BDVAL.AND.
     +         DAT(I,J,IIN2).NE.BDVAL)THEN
               DAT(I,J,IOUT)=(DAT(I,J,IIN1)-DAT(I,J,IIN2))**2
               IF(ANG .GE. 0.0 .AND. ANG .LE. 90.0)THEN
                  RINTGR(1)=RINTGR(1)+DAT(I,J,IOUT)
                  CNT(1)=CNT(1)+1.0
               ELSE IF(ANG .GE.  90.0 .AND. ANG .LE. 180.0)THEN
                  RINTGR(2)=RINTGR(2)+DAT(I,J,IOUT)
                  CNT(2)=CNT(2)+1.0
               ELSE IF(ANG .GE. 180.0 .AND. ANG .LE. 270.0)THEN
                  RINTGR(3)=RINTGR(3)+DAT(I,J,IOUT)
                  CNT(3)=CNT(3)+1.0
               ELSE IF(ANG .GE. 270.0 .AND. ANG .LE. 360.0)THEN
                  RINTGR(4)=RINTGR(4)+DAT(I,J,IOUT)
                  CNT(4)=CNT(4)+1.0
               END IF
               IF(ALFT.EQ.BDVAL)THEN
                  ALFT=ANG
               ELSE
                  GAP=ABS(ANG-ALFT)
                  ALFT=ANG
                  IF(GAP.GT.180.0)GAP=ABS(GAP-360.0)
                  IF(GAP.LT.GAPMN)GAPMN=GAP
                  IF(GAP.GT.GAPMX)GAPMX=GAP
               END IF
            ELSE
               DAT(I,J,IOUT)=BDVAL
            END IF
 90      CONTINUE
         
C        CALCULATE VARIANCE and COVARIANCE TERMS FOR THIS RANGE
C     
         IF(GAPMX.LE.TGAP)THEN

            DO K=1,4
               IF(CNT(K).GE.CNTMN)THEN
                  RINTGR(K)=RINTGR(K)/CNT(K)
               ELSE
                  RINTGR(K)=BDVAL
               END IF
            END DO

            IF(RINTGR(1) .NE. BDVAL .AND.
     +         RINTGR(2) .NE. BDVAL .AND.
     +         RINTGR(3) .NE. BDVAL .AND.
     +         RINTGR(4) .NE. BDVAL)THEN

               VARUVW(I,KVD)=COEF1*(RINTGR(1)+RINTGR(2)
     +                             +RINTGR(3)+RINTGR(4))
               COV_UV(I,KVD)=COEF3*(RINTGR(1)-RINTGR(2)
     +                             +RINTGR(3)-RINTGR(4))
               COV_UW(I,KVD)=COEF2*(RINTGR(1)+RINTGR(2)
     +                             -RINTGR(3)-RINTGR(4))
               COV_VW(I,KVD)=COEF2*(RINTGR(1)-RINTGR(2)
     +                             -RINTGR(3)+RINTGR(4))
               IF(ABS(VARUVW(I,KVD)).GT.ABS(BDVAL))VARUVW(I,KVD)=BDVAL
               IF(ABS(COV_UV(I,KVD)).GT.ABS(BDVAL))COV_UV(I,KVD)=BDVAL
               IF(ABS(COV_VW(I,KVD)).GT.ABS(BDVAL))COV_VW(I,KVD)=BDVAL
               IF(ABS(COV_UW(I,KVD)).GT.ABS(BDVAL))COV_UW(I,KVD)=BDVAL

               IF(NPRNT.EQ.'PRNT'.OR.NPRNT.EQ.'FILE')THEN
                  WRITE(6,93)I,RNG(I,ISW),Z,
     +                 VARUVW(I,KVD),COV_UV(I,KVD),
     +                 COV_UW(I,KVD),COV_VW(I,KVD)
 93               FORMAT(1X,'    IRZ=',I4,2F8.3,' VARUVW=',F8.2,
     +                 ' COV(UV)=',F8.2,' COV(UW)=',F8.2,
     +                 ' COV(VW)=',F8.2)
               END IF

            ELSE

               VARUVW(I,KVD)=BDVAL
               COV_UW(I,KVD)=BDVAL
               COV_VW(I,KVD)=BDVAL
               COV_UV(I,KVD)=BDVAL

            END IF

         ELSE

            VARUVW(I,KVD)=BDVAL
            COV_UW(I,KVD)=BDVAL
            COV_VW(I,KVD)=BDVAL
            COV_UV(I,KVD)=BDVAL

         END IF

 100  CONTINUE
      
C     LINEARLY INTERPOLATE TO FILL MISSING GATES
C
      DO 110 I=1,MAXG
         IF(I.EQ.1 .OR. I.EQ.MAXG)GO TO 104
         IL=I-1
         IR=I+1
         IF(VARUVW( I,KVD).EQ.BDVAL.AND.
     +      VARUVW(IL,KVD).NE.BDVAL.AND.
     +      VARUVW(IR,KVD).NE.BDVAL)THEN
            VARUVW(I,KVD)=0.5*(VARUVW(IL,KVD)+VARUVW(IR,KVD))
            COV_UW(I,KVD)=0.5*(COV_UW(IL,KVD)+COV_UW(IR,KVD))
            COV_VW(I,KVD)=0.5*(COV_VW(IL,KVD)+COV_VW(IR,KVD))
            COV_UV(I,KVD)=0.5*(COV_UV(IL,KVD)+COV_UV(IR,KVD))
         END IF

 104     CONTINUE
         Z=H0+RNG(I,ISW)*SINE
         IF(NPRNT.EQ.'FILE')THEN
            WRITE(99,105)KVD,I,RNG(I,ISW),Z,U0(I,KVD),V0(I,KVD),
     +           SPD(I,KVD),DIR(I,KVD),ERR(I,KVD),DBZ(I,KVD),
     +           VARUVW(I,KVD),COV_UV(I,KVD),COV_UW(I,KVD),
     +           COV_VW(I,KVD)
 105        FORMAT('I=',I2,I3,F6.2,F6.3,10F8.2)
          END IF

 110  CONTINUE

C     RESTORE AVERAGE ACCUMULATORS
C
      IF(IFLD(IIN1).EQ.-5.OR.IFLD(IIN1).EQ.-6)THEN
         CALL UNAVRAGE(DAT,MXR,MXA,MXF,BDVAL,MNGATE,MXGATE,NANG,
     X        NAMFLD,IFLD,IIN1,AVNAM)
      END IF

      IF(IFLD(IIN2).EQ.-5.OR.IFLD(IIN2).EQ.-6)THEN
         CALL UNAVRAGE(DAT,MXR,MXA,MXF,BDVAL,MNGATE,MXGATE,NANG,
     X        NAMFLD,IFLD,IIN2,AVNAM)
      END IF

      RETURN
      END
