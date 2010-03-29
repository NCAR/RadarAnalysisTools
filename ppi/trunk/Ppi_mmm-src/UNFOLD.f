c
c----------------------------------------------------------------------X
c
      SUBROUTINE UNFOLD(DAT,IOUT,IIN1,IIN2,C1,C2,C3,C4,UF_TYP,BDVAL,
     X           MNGATE,MXGATE,R0,DROLD,NANG,AZA,ELA,AZROT,ITPOLD,VNYQ,
     X           AVGI,IAZC)
C
C  FUNCTION - UNFOLD: F(OUT)=UNFOLD RADIAL VELOCITY USING METHOD (UF_TYP)
C
C  INPUTS:
C        C1 - IF (C1 .NE. 0.0), USE IT AS THE NYQUIST VELOCITY
C        C2 - REFERENCE VELOCITY IN THE FORM (VR.0) OR (U.V) OR (VR.AR)
C        C3 - RANGE WINDOW IN THE FORM (R1.R2) IN (KM)
C        C4 - ANGLE    "    "  "    "  (A1.A2) IN (DEG)
C    UF_TYP - THE UNFOLDING METHOD TO BE USED
C             'TEMPL' - ONLY NEED INPUT NAME (#IIN2) OF TEMPLATE FIELD
C             'CONST' - C2 CONTAINS THE CONSTANT REFERENCE VELOCITY
C             'UFMAP' - C2 CONTAINS THE CONSTANT REFERENCE VELOCITY
C           '(+U,+V)' - UF_TYP CONTAINS THE SIGNS AND C2 CONTAINS (U,V)
C           '(-U,+V)' -    "       "     "    "    "   "     "      "
C           '(+U,-V)' -    "       "     "    "    "   "     "      "
C           '(-U,-V)' -    "       "     "    "    "   "     "      "
C              'BEAM' - C2 CONTAINS THE REFERENCE VELOCITY AND STARTING ANGLE
C
C  VARIABLES:
C     AZA,ELA - AZIMUTH AND ELEVATION ANGLES
C     AZROT   - ROTATION ANGLE OF SCAN (SEE RDFF AND RDUF)
C     VMEA    - MEASURED RADIAL VELOCITY
C     VUNF    - TRUE (UNFOLDED) VELOCITY
C     VREF    - AN ESTIMATE OF THE TRUE VELOCITY
C     VNYQQ   - MAXIMUM UNAMBIGUOUS VELOCITY (EITHER VNYQ OR C4)
C     KFAC    - INTEGER UNFOLDING FACTOR
C     VUNF    - VMEA+2*KFAC*VNYQQ;KFAC=0,+/-1,+/-2,+/-3,....
C
C      IOUT   - OUTPUT FIELD NUMBER
C      IIN1   -  INPUT   "      "
C      IIN2   -  INPUT   "      "   (USED AS A REFERENCE)
C      DATIN1 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN1
C
      INCLUDE 'dim.inc'
      CHARACTER*8 UF_TYP
      LOGICAL IAZC
      DIMENSION DAT(MXR,MXA,MXF),AZA(MXA,2),ELA(MXA,2),AZROT(8)
      DIMENSION VR(MXR)
      DATA TORAD,TODEG/0.017453293,57.29577951/
      DATA DCON,EPS/5.0,0.1/

      print *,'UNFOLD: vnyq=',vnyq,c1
      IF(C1.NE.0.0)THEN
         VNYQQ=C1
      ELSE
         VNYQQ=VNYQ
      END IF

C     EXTRACT THE RANGE-ANGLE WINDOW BOUNDS. 
C     SET TO FULL WINDOW IF NOT SPECIFIED.
C
      R1=INT(C3)
      R2=1000.0*(C3-INT(C3))
      IF(R1.EQ.0.0 .AND. R2.EQ.0.0)THEN
         R1=R0+(MNGATE-1)*DROLD
         R2=R0+(MXGATE-1)*DROLD
      END IF
      A1=INT(C4)
      A2=1000.0*(C4-INT(C4))
      IF(A1.EQ.0.0 .AND. A2.EQ.0.0)THEN
         A1=0.0
         A2=360.0
      END IF
      WRITE(6,1770)UF_TYP,VNYQQ,C2,R1,R2,A1,A2,MNGATE,MXGATE,NANG
 1770 FORMAT(1X,'UNFOLD-TYP,NYQ,C2,R12,A12,MNXG,NANG: ',A8,F6.2,F8.3,
     +     4F6.1,3I6)

      IF(UF_TYP(3:3).EQ.'U'.AND.UF_TYP(6:6).EQ.'V')THEN
         UMN=INT(C2)
         VMN=1000.0*(C2-INT(C2))
         IF(UF_TYP(2:2).EQ.'-')UMN=-1.0*UMN
         IF(UF_TYP(5:5).EQ.'-')VMN=-1.0*VMN
C     WRITE(6,1771)UMN,VMN
C1771 FORMAT(1X,'                       UMN,VMN=',2F8.1)
      END IF

      IF(UF_TYP.EQ.'BEAM    ')THEN
C        'BEAM':
C               FIND THE INDEX OF THE STARTING ANGLE (AREF) AND PUT IT
C               WITHIN PROPER INTERVAL, EITHER (-180,180) OR (0,360).
C               UNFOLD GATES IN THAT BEAM USING (VREF) AS A REFERENCE.
C               PROPAGATE THIS BEAM AS A REFERENCE FOR OTHER BEAMS.
C
         VREF=INT(C2)
         AREF=1000.0*(C2-INT(C2))
         IF(IAZC)THEN
            IF(AREF.LT.0.0)AREF=AREF+360.0
         ELSE
            IF(AREF.GT.180.0)AREF=AREF-360.
         END IF
         ADIF=DCON*AVGI
C        WRITE(6,1773)VREF,AREF,ADIF
C1773    FORMAT(1X,'    BEAM: VR,AZ,AD=',3F8.3)
         DO 40 J=1,NANG
            ANG=AZA(J,1)
            IF(ABS(AREF-ANG).GT.ADIF)GO TO 40
            IF(J.EQ.NANG)THEN
               JJ=1
            ELSE
               JJ=J
            END IF
            VMEAN=0.0
            VSUM=0.0
            CNT=0.0
            DO 30 I=MNGATE,MXGATE
               VMEA=DAT(I,JJ,IIN1)
               DAT(I,JJ,IOUT)=BDVAL
               IF(VMEA.NE.BDVAL)THEN
                  IF(ABS(VMEA-VREF).GT.VNYQQ)THEN
                     FAC=(VREF-VMEA)/(2.0*VNYQQ)
                     KFAC=NINT(FAC)
                     VUNF=VMEA+2.0*KFAC*VNYQQ
                  ELSE
                     VUNF=VMEA
                  END IF
                  DAT(I,JJ,IOUT)=VUNF
                  VSUM=VSUM+VUNF
                  CNT=CNT+1.0
               END IF
   30       CONTINUE
            IF(CNT.GE.2.0)THEN
               VMEAN=VSUM/CNT
            END IF
            GO TO 44
   40    CONTINUE
   44    J1=JJ-1
         J2=JJ+1
C        WRITE(6,1774)J1,JJ,J2,AZA(J1,1),AZA(JJ,1),AZA(J2,1),VMEAN,CNT
C1774    FORMAT(1X,'    BEAM: 1J2,1A2,VM,C=',3I8,5F8.2)

C        NOW UNFOLD ALL BEAMS BEFORE THE STARTING ANGLE BY LOOPING J
C        BACKWARDS UNLESS THE STARTING ANGLE INDEX JJ .LE. 1.
C        IF SO, BY-PASS THIS LOOP.
C
         IF(J1.LE.1)GO TO 64
         DO 60 J=J1,1,-1
C           IF(MOD(J,10).EQ.0)WRITE(6,1775)J,AZA(J,1),VMEAN,CNT
C1775       FORMAT(1X,'    BEAM: J,AZ,VM,CNT=',I8,3F8.2)
            VSUM=0.0
            CNT=0.0
            DO 50 I=MNGATE,MXGATE
               IF(DAT(I,J+1,IOUT).EQ.BDVAL)THEN
                  VREF=VMEAN
               ELSE
                  VREF=DAT(I,J+1,IOUT)
               END IF
               VMEA=DAT(I,J,IIN1)
               DAT(I,J,IOUT)=BDVAL
               IF(VMEA.NE.BDVAL)THEN
                  IF(ABS(VMEA-VREF).GT.VNYQQ)THEN
                     FAC=(VREF-VMEA)/(2.0*VNYQQ)
                     KFAC=NINT(FAC)
                     VUNF=VMEA+2.0*KFAC*VNYQQ
                  ELSE
                     VUNF=VMEA
                  END IF
                  DAT(I,J,IOUT)=VUNF
                  VSUM=VSUM+VUNF
                  CNT=CNT+1.0
               END IF
   50       CONTINUE
            IF(CNT.GE.2.0)THEN
               VMEAN=VSUM/CNT
            END IF
   60    CONTINUE

C        NOW UNFOLD ALL BEAMS BEFORE THE STARTING ANGLE BY LOOPING J
C        FORWARDS UNLESS THE STARTING ANGLE INDEX JJ .GE. ANNG.
C        IF SO, BY-PASS THIS LOOP.
C
   64    CONTINUE
         IF(J2.GE.NANG)GO TO 84
         DO 80 J=J2,NANG,1
C           IF(MOD(J,10).EQ.0)WRITE(6,1775)J,AZA(J,1),VMEAN,CNT
            VSUM=0.0
            CNT=0.0
            DO 70 I=MNGATE,MXGATE
               VREF=DAT(I,J-1,IOUT)
               VMEA=DAT(I,J,IIN1)
               DAT(I,J,IOUT)=BDVAL
               IF(VMEA.NE.BDVAL)THEN
                  IF(ABS(VMEA-VREF).GT.VNYQQ)THEN
                     FAC=(VREF-VMEA)/(2.0*VNYQQ)
                     KFAC=NINT(FAC)
                     VUNF=VMEA+2.0*KFAC*VNYQQ
                  ELSE
                     VUNF=VMEA
                  END IF
                  DAT(I,J,IOUT)=VUNF
                  VSUM=VSUM+VUNF
                  CNT=CNT+1.0
               END IF
   70       CONTINUE
            IF(CNT.GE.2.0)THEN
               VMEAN=VSUM/CNT
            END IF
   80    CONTINUE
   84    CONTINUE

      ELSE IF(UF_TYP.EQ.'TEMPL   ')THEN
C        'TEMPL':  UNFOLD ALL POINTS WITHIN (R,A) WINDOW USING
C                  FIELD #IIN2 AS THE REFERENCE.
C
         DO 100 J=1,NANG
            ANG=AZA(J,1)
            IF(ANG.LT.0.0)THEN
               TANG=ANG+360.0
            ELSE
               TANG=ANG
            END IF
            DO 90 I=MNGATE,MXGATE
               VMEA=DAT(I,J,IIN1)
               IF(DAT(I,J,IIN2).EQ.BDVAL)THEN
                  DAT(I,J,IOUT)=VMEA
                  GO TO 90
               END IF
               RNG=R0+(I-1)*DROLD
               IF( RNG.LT.R1 .OR.  RNG.GT.R2 .OR.
     +            TANG.LT.A1 .OR. TANG.GT.A2)THEN
                  DAT(I,J,IOUT)=DAT(I,J,IIN1)
                  GO TO 90
               END IF
               VREF=DAT(I,J,IIN2)
               DAT(I,J,IOUT)=BDVAL
               IF(VMEA.NE.BDVAL)THEN
                  IF(ABS(VMEA-VREF).GT.VNYQQ)THEN
                     FAC=(VREF-VMEA)/(2.0*VNYQQ)
                     KFAC=NINT(FAC)
                     VUNF=VMEA+2.0*KFAC*VNYQQ
                  ELSE
                     VUNF=VMEA
                  END IF
                  DAT(I,J,IOUT)=VUNF
               END IF
   90       CONTINUE
  100    CONTINUE

      ELSE IF(UF_TYP.EQ.'CONST   ')THEN
C        'CONST':  UNFOLD ALL POINTS WITHIN (R,A) WINDOW
C                  USING (VREF) AS A CONSTANT REFERENCE.
C
         VREF=INT(C2)
         DO 120 J=1,NANG
            ANG=AZA(J,1)
            IF(ANG.LT.0.0)THEN
               TANG=ANG+360.0
            ELSE
               TANG=ANG
            END IF
            DO 110 I=MNGATE,MXGATE
               RNG=R0+(I-1)*DROLD
               IF( RNG.LT.R1 .OR.  RNG.GT.R2 .OR.
     +            TANG.LT.A1 .OR. TANG.GT.A2)THEN
                  DAT(I,J,IOUT)=DAT(I,J,IIN1)
                  GO TO 110
               END IF
               VMEA=DAT(I,J,IIN1)
               DAT(I,J,IOUT)=BDVAL
               IF(VMEA.NE.BDVAL)THEN
                  IF(ABS(VMEA-VREF).GT.VNYQQ)THEN
                     FAC=(VREF-VMEA)/(2.0*VNYQQ)
                     KFAC=NINT(FAC)
                     VUNF=VMEA+2.0*KFAC*VNYQQ
                  ELSE
                     VUNF=VMEA
                  END IF
                  DAT(I,J,IOUT)=VUNF
               END IF
  110       CONTINUE
  120    CONTINUE

      ELSE IF(UF_TYP.EQ.'UFMAP   ')THEN
C        'UFMAP':  SET OUTPUT FIELD WITHIN (R,A) WINDOW TO VREF.
C
         VREF=INT(C2)
         DO 140 J=1,NANG
            ANG=AZA(J,1)
            IF(ANG.LT.0.0)THEN
               TANG=ANG+360.0
            ELSE
               TANG=ANG
            END IF
            DO 130 I=MNGATE,MXGATE
               RNG=R0+(I-1)*DROLD
               IF( RNG.LT.R1 .OR.  RNG.GT.R2 .OR.
     +            TANG.LT.A1 .OR. TANG.GT.A2)THEN
                  GO TO 130
               ELSE
                  DAT(I,J,IOUT)=VREF
               END IF
  130       CONTINUE
  140    CONTINUE

      ELSE
C        '(U,V)':  UNFOLD ALL POINTS WITHIN (R,A) WINDOW
C                  USING VR(UMN,VMN) AS A REFERENCE.
C
         DO 160 J=1,NANG
            ANG=AZA(J,1)
            IF(ANG.LT.0.0)THEN
               TANG=ANG+360.0
            ELSE
               TANG=ANG
            END IF
            SINA=SIN((ANG-AZROT(ITPOLD))*TORAD)
            COSA=COS((ANG-AZROT(ITPOLD))*TORAD)
            COSE=COS(ELA(J,1)*TORAD)
            VREF=(UMN*SINA+VMN*COSA)*COSE
            DO 150 I=MNGATE,MXGATE
               RNG=R0+(I-1)*DROLD
               IF( RNG.LT.R1 .OR.  RNG.GT.R2 .OR.
     +            TANG.LT.A1 .OR. TANG.GT.A2)THEN
                  DAT(I,J,IOUT)=DAT(I,J,IIN1)
                  GO TO 150
               END IF
               VMEA=DAT(I,J,IIN1)
               DAT(I,J,IOUT)=BDVAL
               IF(VMEA.NE.BDVAL)THEN
                  IF(ABS(VMEA-VREF).GT.VNYQQ)THEN
                     FAC=(VREF-VMEA)/(2.0*VNYQQ)
                     KFAC=NINT(FAC)
                     VUNF=VMEA+2.0*KFAC*VNYQQ
                  ELSE
                     VUNF=VMEA
                  END IF
                  DAT(I,J,IOUT)=VUNF
               END IF
  150       CONTINUE
  160    CONTINUE

      END IF
      RETURN
      END






