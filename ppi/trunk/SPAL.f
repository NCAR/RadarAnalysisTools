c
c----------------------------------------------------------------------X
c
      SUBROUTINE SPAL (T,N,P,DUM,NA,IDT,SR,FF)
C
C DIMENSION OF           T(MT),  P(MT/2),  DUM(MT/2)
C ARGUMENTS              WHERE MT = 2**K+4, AND K IS THE SMALLEST
C                        INTEGER SUCH THAT 2**K .GE. N, (I.E., T MUST
C                        BE DIMENSIONED AT LEAST 4 MORE THAN THE
C                        SMALLEST POWER OF 2 WHICH WILL INCLUDE ALL
C                        POINTS IN THE TIME SERIES).
C
C LATEST REVISION        MARCH 1985
C
C PURPOSE                SPAL COMPUTES POWER SPECTRAL ESTIMATES FOR A
C                        ONE-DIMENSIONAL STATIONARY TIME SERIES, AND
C                        PROVIDES SEVERAL OPTIONS FOR DETRENDING THE
C                        TIME SERIES DATA.
C
C USAGE                  THIS PACKAGE CONTAINS ONE USER ENTRY, SPAL.
C                        CALL SPAL (T,N,P,DUM,NA,IDT,SR,FF)
C
C ARGUMENTS
C
C ON INPUT               T
C                          A REAL VECTOR VARIABLE WITH DIMENSION MT.
C                          (SEE -DIMENSION OF ARGUMENTS-.)  T CONTAINS
C                          THE TIME SERIES DATA WHICH IS ASSUMED TO BE
C                          AT CONSTANT TIME INTERVALS.  THE TIME SERIES
C                          MUST INCLUDE AT LEAST 8 POINTS.
C
C                        N
C                          AN INTEGER INPUT VARIABLE SET EQUAL TO THE
C                          ACTUAL NUMBER OF DATA POINTS IN THE TIME
C                          SERIES.
C
C                        DUM
C                          A REAL ARRAY WITH DIMENSION AT LEAST MT/2.
C                          (SEE -DIMENSION OF ARGUMENTS-.)  IT IS A
C                          SCRATCH ARRAY USED INTERNALLY FOR WORKING
C                          STORAGE.
C
C                        NA
C                          AN INTEGER INPUT VARIABLE SET EQUAL TO THE
C                          NUMBER OF FREQUENCY VALUES TO BE AVERAGED IN
C                          CALCULATING THE SPECTRAL ESTIMATES.  IF
C                          NA = 0, THEN THE VALUE OF 5 IS ASSUMED.  THIS
C                          IS APPROXIMATELY EQUIVALENT TO AN
C                          AUTOCORRELATION LAG OF 20%.
c                          11/92 - ljm changed to NA = 0, no smoothing.
C
C                        IDT
C                          AN INPUT INTEGER FLAG WHICH INDICATES THE
C                          TYPE OF DETRENDING OF THE TIME SERIES DATA TO
C                          BE PERFORMED.
C                          = 0  CAUSES NO DETRENDING.
C                          = 1  CAUSES REMOVAL OF THE MEAN.
C                          = 2  CAUSES LINEAR DETRENDING.
C                          = 3  CAUSES QUADRATIC DETRENDING.
C
C                        SR
C                          THE -SAMPLE RATE- FOR THE INPUT TIME SERIES;
C                          THAT IS, THE CONSTANT TIME INTERVAL BETWEEN
C                          POINTS IN THE TIME SERIES.
C
C ON OUTPUT              P
C                          A REAL VECTOR ARRAY WITH DIMENSION AT LEAST
C                          MT/2.  ON OUTPUT, P CONTAINS THE POWER
C                          SPECTRAL ESTIMATES.
C
C                        FF
C                          A REAL OUTPUT VARIABLE WHICH CONTAINS THE
C                          FREQUENCY SPACING FOR THE OUTPUT SPECTRAL
C                          ESTIMATES.
C
C SPECIAL CONDITIONS     NONE
C
C I/O                    NONE
C
C PRECISION              SINGLE
C
C REQUIRED LIBRARY       Q8QST4, WHICH IS AUTOMATICALLY LOADED ON
C FILES                  NCAR'S CRAY MACHINES.
C
C LANGUAGE               FORTRAN
C
C PORTABILITY            FORTRAN 66
C
C HISTORY                SPAL IS A PACKAGE FROM THE LOS ALAMOS
C                        SCIENTIFIC LABORATORY, LOS ALAMOS, NEW MEXICO.
C                        IT WAS OBTAINED BY NCAR IN THE EARLY 1970'S.
C REVISION HISTORY---
C
C OCTOBER 1992     DELETED REFERENCES TO OLD Q8QST4 LIBRARY FILES
C     LOGICAL Q8Q4
C     SAVE Q8Q4
C     DATA Q8Q4 /.TRUE./
C
C     IF (Q8Q4) THEN
C         CALL Q8QST4('LOCLIB','SPAL','SPAL','VERSION 09')
C         Q8Q4 = .FALSE.
C     ENDIF
C***********************************************************************
      DIMENSION       T(2)       ,P(1)       ,DUM(1)     ,C(3)
C
      FN = N
      FN1 = N-1
      SLP = 0.
      SEC = 0.
      M = ALOG(FN1)/ALOG(2.)+1.0
      IF (M .LT. 3) M = 3
      MA = M-1
      IDE = IDT+1
      IF (IDE.GT.4 .OR. IDE.LT.1) IDE = 4
      IF (IDT) 101,109,101
  101 SUM = 0.
      DO 102 I=1,N
         SUM = SUM+T(I)
  102 CONTINUE
      SUM = SUM/FN
      DO 103 I=1,N
         T(I) = T(I)-SUM
  103 CONTINUE
      EX = -SR*FN/2.
      EXS = EX
      GO TO (109,109,104,106),IDE
  104 SUMY = 0.
      SUMY2 = 0.
      SUMX = 0.
      SUMX2 = 0.
      SUMXY = 0.
      DO 105 I=1,N
         EX = EX+SR
         SUMX = SUMX+SR
         SUMY = SUMY+T(I)
         SUMY2 = SUMY2+T(I)**2
         SUMX2 = SUMX2+EX**2
         SUMXY = SUMXY+EX*T(I)
  105 CONTINUE
      DT = FN*SUMX2-SUMX**2
      FICP = SUMX2*SUMY-SUMX*SUMXY
      FICP = FICP/DT
      SLP = (SUMXY*FN-SUMX*SUMY)/DT
      GO TO 107
  106 CALL SPQUAD (D,T,C,N,SR,EX+SR)
      SLP = C(2)
      SEC = C(3)
      FICP = C(1)
C
C REMOVE TREND
C
  107 FI = EXS
      DO 108 I=1,N
         FI = FI+SR
         V = SLP*FI+SEC*FI**2+FICP
         T(I) = T(I)-V
  108 CONTINUE
  109 CALL TAPER (T,N)
      MB = 2**M
      MR = MB-N
      TT = MB*SR
      FF = 1./TT
      IF (MR) 114,114,110
  110 MR2 = MR/2+1
      DO 111 I=1,N
         K = N-I+1
         L = K+MR2
         T(L) = T(K)
  111 CONTINUE
      DO 112 I=1,MR2
         T(I) = 0.
  112 CONTINUE
      MR4 = N+MR2+1
      IF (MR4 .GE. MB) GO TO 114
      DO 113 I=MR4,MB
         T(I) = 0.
  113 CONTINUE
  114 KK = MB/8+1
      CALL RFFT (T,MA,DUM,DUM(KK),IFE)
      L = 0
      SCL = SR/2/N
      DO 115 I=1,MB,2
         L = L+1
         PR = T(I)*MB
         PI = T(I+1)*MB
         P(L) = (PI**2+PR**2)*SCL
  115 CONTINUE
      IF (NA .LE. 0) RETURN
c-ljm-IF (NA .NE. 0) GO TO 116
c-ljm-FMB = MB
c-ljm-FN = N
c-ljm-NA = FMB*05./FN
  116 NA2 = NA/2+1
      MPS = MB/2
      CALL SMSPCT (P,MPS,NA,DUM)
      RETURN
      END
