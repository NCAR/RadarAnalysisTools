c
c----------------------------------------------------------------------X
c
      SUBROUTINE SMTH2D(DAT,IOUT,IIN1,IFLTYP,FSPACE,NPROC,C3,C4,BDVAL,
     X                  MNGATE,MXGATE,NGTS,NANG,VNYQ,ITPOLD,ANG,DROLD,
     X                  AVGI,R0,NSCTP,TMP1,TMP2)
C
C  FUNCTION - PERFORM TWO-DIMENSIONAL SMOOTHING (FILTERING)
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "
C     TMP2   - TEMPORARY STORAGE ARRAY
C     VNYQ   - NYQUIST VELOCITY FOR LOCAL UNFOLDING DURING FILTERING
C     IFLTYP - FILTER WEIGHTS (UNIFORM, TRIANGULAR, CRESSMAN, QUADRATIC,
C              EXPONENTIAL, OR LINEAR LEAST-SQUARES)
C     NPROC  - SPECIAL PROCESSING (LINEAR, UNFOLD)
C     DROLD,DA  - RANGE GATE SPACING (KM), AVERAGE ANGLE SPACING (DEG)
C     FSPACE - FILTER SPACE ('RADR' OR 'CART')
C     C3,C4  - FILTER DIMENSIONS: RADR - (2*C3+1) GATES BY (2*C4+1) BEAMS
C                                 CART - LINEAR RADII (KM BY KM)
C     NSCTP  - IF 'FILL',    THEN FILL IN WHEN CENTRAL POINT IS BDVAL
C              IF 'NO FILL', THEN DON'T FILL IN WHEN CENTRAL POINT IS BDVAL
C     DDX,DDY- RADIUS OF THE FILTER IS SQRT(DDX*DDX+DDY*DDY)
C              RADR - [DDX=(C3+1)*DROLD, DDY=(C4+1)*DA*TORAD*RANGE)
C              CART - [DDX= C3,       DDY= C4]
C     DXX,DYY- TOTAL DIMENSIONS OF FILTER AREA
C              RADR - (DDX=2*C3+1, DDY=2*C4+1) (GATES BY BEAMS)
C              CART - (DDX=2*C3,   DDY=2*C4)   (KM BY KM)
C
      INCLUDE 'dim.inc'


      CHARACTER*8 NPROC,FSPACE
      CHARACTER*8 NFILT(6),IFLTYP,NSCTP
      DIMENSION DAT(MXR,MXA,MXF),ANG(MXA,2)
      DIMENSION TMP1(MXR),TMP2(MXR,MXA)
      DIMENSION AM(3),BM(3),CM(3),DM(3),IQUAD(4)

      COMMON /FILCH/FLSPAC(MXF)
      CHARACTER*8 FLSPAC
      COMMON/FIL/IFILTER(MXF),IGATE(MXF),DXX(MXF),DYY(MXF)
      DATA NFILT/'UNI','TRI','CRE','QUA','EXP','LSQ'/
      DATA TORAD,TODEG/0.017453293,57.29577951/
      DATA FDA,CNTMN,EPS/1.05,3.0,0.00001/
      DATA NQUAD,PTSMIN/2.0,3.0/

      DA=FDA*ABS(AVGI)
      INDX=IFIND(IFLTYP,NFILT,6)
      IF(INDX.EQ.0)THEN
         WRITE(6,5)
    5    FORMAT(1X,'*** WARNING - UNKNOWN FILTER, RESET TO TRI ***')
         INDX=2
         IFLTYP='TRI'
      END IF
      IFILTER(IOUT)=INDX
      FLSPAC(IOUT)=FSPACE
      IGATE(IOUT)=3
      IF(FSPACE.EQ.'RADR')THEN
         DXX(IOUT)=2.0*NINT(C3)+1.0
         DYY(IOUT)=2.0*NINT(C4)+1.0
      ELSE
         DXX(IOUT)=2.0*C3
         DYY(IOUT)=2.0*C4
      END IF
      IF(IFLTYP.EQ.'LSQ'.AND. C3*C4.EQ.0.0)THEN
         PRINT *,
     x   '***ERROR - BOTH LSQ FILTER RADII MUST BE NON-ZERO***'
         STOP
      END IF

C     Execute outer loops (J,I) over original data locations
C
      DO 200 J=1,NANG
         SINIJ=SIN(ANG(J,1)*TORAD)
         COSIJ=COS(ANG(J,1)*TORAD)
c         DO 190 I=MNGATE,MXGATE
         DO 190 I=1,NGTS
            TMP2(I,J)=BDVAL
            IF(DAT(I,J,IIN1).EQ.BDVAL.AND.
     +           NSCTP.EQ.'NO FILL')GO TO 190

C           SET RANGE OF INDICES AND FILTER RADII FOR LOCAL SUMMATION.
C           NOTE: FOR RHI (ITPOLD.EQ.3), ANG CONTAINS ELEVATION ANGLE;
C                 ALL OTHER SCAN TYPES, ANG CONTAINS AZIMUTH ANGLE.
C
            RIJ=R0+(I-1)*DROLD
            IF(ITPOLD.EQ.3)THEN
               XIJ=RIJ*COSIJ
               YIJ=RIJ*SINIJ
            ELSE
               XIJ=RIJ*SINIJ
               YIJ=RIJ*COSIJ
            END IF
            IF(RIJ.LE.0.0)GO TO 190

C           Note: The filter radius (BIGR) should be zero one
C                 sample point away from filter radii (C3,C4).
C           For (C3,C4)=(1,1), want filter=0 at (+/-2)
C              Indices= -2  -1  0   +1  +2
C                       +---+---+---+---+ -> Range
C              Filter = 0   f   C   f   0 with values f and C.
C
            IF(FSPACE.EQ.'RADR')THEN
               ID=NINT(C3)
               IF(ID.LE.0)THEN
                  DDX=0.0
               ELSE
                  DDX=(C3+1.0)*DROLD
               END IF
               JD=NINT(C4)
               IF(JD.LE.0)THEN
                  DDY=0.0
               ELSE
                  DDY=(C4+1.0)*DA*TORAD*RIJ
               END IF
            ELSE
               ID=NINT(C3/DROLD)
               IF(ID.LE.0)THEN
                  DDX=0.0
               ELSE
                  DDX=C3
               END IF
               JD=NINT(C4/(RIJ*DA*TORAD))
               IF(JD.LE.0)THEN
                  DDY=0.0
               ELSE
                  DDY=C4
               END IF
            END IF

            I1=I-ID
            I2=I+ID
            IF(I1.LT.1)I1=1
            IF(I2.LT.1)I2=1
            IF(I1.GT.NGTS)I1=NGTS
            IF(I2.GT.NGTS)I2=NGTS

            J1=J-JD
            J2=J+JD
            IF(J1.LT.1)J1=1
            IF(J2.LT.1)J2=1
            IF(J1.GT.NANG)J1=NANG
            IF(J2.GT.NANG)J2=NANG

            BIGR=DDX*DDX+DDY*DDY
            IF(BIGR.EQ.0.0)THEN
               WRITE(6,7)
    7          FORMAT(1X,'*** ERROR:',
     +              ' CHECK FILTER SPECS, BIGR IS ZERO ***')
               STOP
            END IF

C           EXECUTE THE INNER SUMMATIONS TO OBTAIN WEIGHTED SUM OF
C           SAMPLES WITHIN SQRT(BIGR) OF THE CURRENT (I,J) SAMPLE POINT.
C
            IF(NPROC.EQ.'UNFOLD')THEN
               IF(DAT(I,J,IIN1).NE.BDVAL)THEN
                  VEST=DAT(I,J,IIN1)
               ELSE
                  VEST=0.0
               END IF
            ELSE IF(NPROC.EQ.'UF_MEAN')THEN
               SUM=0.0
               CNT=0.0
               DO 160 JJ=J1,J2
                  DO 150 II=I1,I2
                     IF(DAT(II,JJ,IIN1).EQ.BDVAL)GO TO 150
                     SUM=SUM+DAT(II,JJ,IIN1)
                     CNT=CNT+1.0
 150              CONTINUE
 160           CONTINUE
               IF(CNT.GE.CNTMN)VEST=SUM/CNT
            ELSE
               VEST=0.0
            END IF

C           Clear both simple distance-weighting and linear 
C           least-squares weights before executing inner loops
C     
            CNT=0.0
            SUMW=0.0
            WSUM=0.0
            DO L=1,3
               AM(L)=0.0
               BM(L)=0.0
               CM(L)=0.0
               DM(L)=0.0
            END DO
            DO N=1,4
               IQUAD(N)=0
            END DO

C           Execute the inner loops (JJ,II)
C           surrounding a (J,I) data location.
C
            DO 180 JJ=J1,J2
               SINA=SIN(ANG(JJ,1)*TORAD)
               COSA=COS(ANG(JJ,1)*TORAD)
               DO 170 II=I1,I2
                  IF(DAT(II,JJ,IIN1).EQ.BDVAL)GO TO 170
                     
C                 CHECK IF THE (II,JJ) POINT IS 
C                 INSIDE THE FILTER REGION
C
                  RNG=R0+(II-1)*DROLD
                  IF(RNG.LE.0.0)GO TO 170
                  IF(ITPOLD.EQ.3)THEN
                     XX=RNG*COSA
                     YY=RNG*SINA
                  ELSE
                     XX=RNG*SINA
                     YY=RNG*COSA
                  END IF
                  XD=XX-XIJ
                  YD=YY-YIJ
                  SMALR=(XX-XIJ)**2.0+(YY-YIJ)**2.0
                  IF(SMALR.GT.BIGR)GO TO 170

C                 PROCEED WITH SUMMATION OF (II,JJ) LOCATIONS
C                 SURROUNDING CURRENT (I,J) LOCATION
C
                  IF(NPROC.EQ.'LINEAR')THEN
                     DMEA=10.0**(0.1*DAT(II,JJ,IIN1))
                  ELSE IF(NPROC.EQ.'UNFOLD')THEN
                     DMEA=VUNF(DAT(II,JJ,IIN1),VNYQ,VEST)
                  ELSE IF(NPROC.EQ.'UF_MEAN')THEN
                     DMEA=VUNF(DAT(II,JJ,IIN1),VNYQ,VEST)
                  ELSE
                     DMEA=DAT(II,JJ,IIN1)
                  END IF

                  IF(INDX.LE.5)THEN
C                    Simple distance-weighted schemes
C     
                     GO TO (11,12,13,14,15),INDX
 11                  DWT=1.0
                     GO TO 16
 12                  DWT=1-SQRT(SMALR/BIGR)
                     GO TO 16
 13                  DWT=(BIGR-SMALR)/(BIGR+SMALR)
                     GO TO 16
 14                  DWT=1-(SMALR/BIGR)
                     GO TO 16
 15                  DWT=EXP(-4.0*SMALR/BIGR)
                     
 16                  WSUM=WSUM+DMEA*DWT
                     SUMW=SUMW+DWT
                     CNT=CNT+1.0
                     
                  ELSE
C                    Linear least-squares scheme
C     
                     IX=II-I
                     IY=JJ-J
                     XD=XX-XIJ
                     YD=YY-YIJ
                     IF(IX.GE.0.AND.IY.GT.0) IQUAD(1)=1
                     IF(IX.GT.0.AND.IY.LE.0) IQUAD(2)=1
                     IF(IX.LE.0.AND.IY.LT.0) IQUAD(3)=1
                     IF(IX.LT.0.AND.IY.GE.0) IQUAD(4)=1
                     AM(1)=AM(1)+1.0
                     AM(2)=AM(2)+IX
                     AM(3)=AM(3)+IY
                     BM(2)=BM(2)+IX*IX
                     BM(3)=BM(3)+IX*IY
                     CM(3)=CM(3)+IY*IY
                     DM(1)=DM(1)+DMEA
                     DM(2)=DM(2)+IX*DMEA
                     DM(3)=DM(3)+IY*DMEA
                  END IF

 170           CONTINUE
 180        CONTINUE

            IF(INDX.LE.5)THEN
C              Simple distance-weighted schemes
C
               IF(CNT.LT.CNTMN.OR.SUMW.LT.EPS)GO TO 190
               TMP2(I,J)=WSUM/SUMW
               
            ELSE
C              Linear least-squares scheme
C
               KQ=0
               DO K=1,4
                  KQ=KQ+IQUAD(K)
               END DO
c--------------IF(KQ.LT.NQUAD) GO TO 190
               IF(AM(1).LT.PTSMIN) GO TO 190
               BM(1)=AM(2)
               CM(1)=AM(3)
               CM(2)=BM(3)
               T1=BM(2)*CM(3)-BM(3)*CM(2)
               T2=BM(1)*CM(3)-BM(3)*CM(1)
               T3=BM(1)*CM(2)-BM(2)*CM(1)
               DENO=AM(1)*T1-AM(2)*T2+AM(3)*T3
               IF(DENO.LE.EPS) GO TO 190
               ANUM=DM(1)*T1-DM(2)*T2+DM(3)*T3
               TMP2(I,J)=ANUM/DENO
            END IF

 190     CONTINUE
 200  CONTINUE

C     MOVE FILTERED VALUES INTO OUTPUT ARRAY;
C     UNDO ANY SPECIAL PRE-FILTER TRANSFORMATION
C
      DO J=1,NANG
c         DO I=MNGATE,MXGATE
         DO I=1,NGTS
            IF(NPROC.EQ.'LINEAR'.AND.TMP2(I,J).NE.BDVAL)THEN
               IF(TMP2(I,J).GT.0.0)THEN
                  DAT(I,J,IOUT)=10.0*ALOG10(TMP2(I,J))
               ELSE
                  DAT(I,J,IOUT)=BDVAL
               END IF
            ELSE
               DAT(I,J,IOUT)=TMP2(I,J)
            END IF
         END DO
      END DO

      RETURN
      END
