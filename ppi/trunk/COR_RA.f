c
c----------------------------------------------------------------------X
c
      SUBROUTINE COR_RA(DAT,IOUT,IIN1,IIN2,C1,C2,BDVAL,MNGATE,MXGATE,
     X                  NANG,C,DROLD,AVGI)
C
C  FUNCTION - COR_RA: F(OUT)=CROSS-CORRELATION[F(IN1),F(IN2)]
C             Two-dimensional (range, angle) cross-correlation
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "   (FIELD ONE)
C     IIN2   -  INPUT   "      "   (  "   TWO)
C     NR     - (NUMBER OF RANGE GATES)
C     NR2    - (   "    "   "     "  )/2 = (MAXIMUM LAG)
C     NR4    - (   "    "   "     "  )/4 = (# REQUIRED FOR CORRELATION)
C     NA     - (NUMBER OF ANGLES)
C     NA2    - (   "    "   "   )/2 = (MAXIMUM LAG)
C     NA4    - (   "    "   "   )/4 = (# REQUIRED FOR CORRELATION)
C     RLAG   - Maximum lag radius (5 km) at midrange
C
      INCLUDE 'dim.inc'
      DIMENSION C(MXR,MXA),A(MXR,MXA),B(MXR,MXA)
      DIMENSION DAT(MXR,MXA,MXF)
      DATA RLAG/5.0/
      DATA TORAD,TODEG/0.017453293,57.29577951/

      print *,'COR_RA: iin1,iin2,iout=',iin1,iin2,iout

C     Range indexing parameters
C
      NR=MXGATE-MNGATE+1
      NR2=NR/2
      NR4=NR/4
      N1=NR
      IS=1
      IF=NR
      IC=NR2
      LAGX=NR2/4
      MXLAGX=NINT(RLAG)/DROLD
      IF(LAGX.GT.MXLAGX)LAGX=MXLAGX
C      IF(LAGX.LT.NR2/4)LAGX=NR2/4

C     Angle indexing parameters
C
      MNANG=1
      MXANG=NANG
      NA=MXANG-MNANG+1
      NA2=NA/2
      NA4=NA/4
      N2=NA
      JS=1
      JF=NA
      JC=NA2
      LAGY=NA2/4
      RMID=0.5*(MXGATE+MNGATE)*DROLD
      DELANG=TODEG*RLAG/RMID
      MXLAGY=NINT(DELANG/AVGI)
      IF(LAGY.GT.MXLAGY)LAGY=MXLAGY
c      IF(LAGY.LT.NA2/4)LAGY=NA2/4

      BAD=BDVAL

      print *,'COR_RA: mngate,mxgate,bad=',mngate,mxgate,bad
      print *,'         nang,mnang,mxang=',nang,mnang,mxang
      print *,'          rmid,drold,avgi=',rmid,drold,avgi
      print *,'              n1,is,if,ic=',n1,is,if,ic
      print *,'        mxlagx,nr2/4,lagx=',mxlagx,nr2/4,lagx
      print *,'              n2,js,jf,jc=',n2,js,jf,jc
      print *,'        mxlagy,na2/4,lagy=',mxlagy,na2/4,lagy

c      IF(BAD.EQ.BDVAL)RETURN

      DO J=1,NANG
         DO I=MNGATE,MXGATE
            A(I,J)=BDVAL
            B(I,J)=BDVAL
            C(I,J)=BDVAL
            A(I,J)=DAT(I,J,IIN1)
            B(I,J)=DAT(I,J,IIN2)
         END DO
      END DO

      CALL CORCAL(C,A,B,N1,N2,IS,IF,JS,JF,IC,JC,LAGX,LAGY,
     X     BAD,MXR,MXA)

C     MOVE THE CROSS-CORRELATION FUNCTION INTO
C     OUTPUT ARRAY AND GO TO THE NEXT BEAM
C     
      DO L=1,MXR
c        LL=MNGATE+L-1
         LL=L
         DO M=1,MXA
c            MM=MNANG+M-1
            MM=M
            IF(L.LE.NR .OR. M.LE.NA)THEN
               DAT(LL,MM,IOUT)=C(L,M)
            ELSE
               DAT(LL,MM,IOUT)=BDVAL
            END IF
         END DO
      END DO

      RETURN
      END

c
c----------------------------------------------------------------------X
c
      SUBROUTINE CORCAL(C,A,B,N1,N2,IS,IF,JS,JF,IC,JC,LAGX,LAGY,
     X     BAD,MXR,MXA)
C
C     CALCULATES THE CROSS-CORRELATION BETWEEN TWO (2-D) FIELDS.
C     Adapted from the CEDRIC code to do a range-angle correlation
C     on data in radar space.
C
C  FUNCTION - CORCAL: F(OUT)=2D CROSS-CORRELATION[F(IN1),F(IN2)]
C
C     C    - CROSS-CORRELATION OUTPUT FIELD
C     A    - REFERENCE INPUT FIELD
C     B    - LAG INPUT FIELD
C     N1   - I-DIMENSION OF FIELDS (Range direction)
C     N2   - J-DIMENSION OF FIELDS (Angle direction)
C     IS   - STARTING I-INDEX
C     IF   - ENDING I-INDEX
C     JS   - STARTING J-INDEX
C     JF   - ENDING J-INDEX
C     IC   - I-INDEX OF ZERO LAG IN OUTPUT FIELD
C     JC   - J-INDEX OF ZERO LAG IN OUTPUT FIELD
C     LAGX - (+/-) LAG ALONG I
C     LAGY - (+/-) LAG ALONG J
C     BAD  - MISSING DATA FLAG
C
C
      DIMENSION C(MXR,MXA),A(MXR,MXA),B(MXR,MXA)
      DATA CNTMIN /2.0/
      DATA EPS /1.E-8/
C
C        CALCULATE INDICES OF THE OUTPUT FIELD
C
      LJ1=JC-LAGY
      LJ2=JC+LAGY
      LI1=IC-LAGX
      LI2=IC+LAGX
C
C        LOOP FOR EACH LAG CORRELATION
C
      DO 30 JP=LJ1,LJ2
         LJ=JP-JC
         JA1=MAX0(JS,JS+LJ)
         JA2=MIN0(JF,JF+LJ)
         JBINIT=MAX0(JS,JS-LJ)
      DO 25 IP=LI1,LI2
         LI=IP-IC
         IA1=MAX0(IS,IS+LI)
         IA2=MIN0(IF,IF+LI)
         IBINIT=MAX0(IS,IS-LI)
C
C        ZERO OUT CORRELATION CALCULATION ACCUMULATORS
C
         CNT=0.0
         PAB=0.0
         SUMA=0.0
         SUMB=0.0
         SUMSQA=0.0
         SUMSQB=0.0
C
C        LOOP FOR ALL (I,J) LOCATIONS IN THE OVERLAPPING REGION
C
         JB=JBINIT
         DO 15 JA=JA1,JA2
         IB=IBINIT
         DO 10 IA=IA1,IA2
C
C           VECTORIZED CODE
C
            IF(A(IA,JA).NE.BAD.AND.B(IB,JB).NE.BAD)THEN
               OK=1.0
            ELSE
               OK=0.0
            END IF
            CNT=CNT+OK
            PAB=PAB+A(IA,JA)*B(IB,JB)*OK
            SUMA=SUMA+A(IA,JA)*OK
            SUMB=SUMB+B(IB,JB)*OK
            SUMSQA=SUMSQA+A(IA,JA)*A(IA,JA)*OK
            SUMSQB=SUMSQB+B(IB,JB)*B(IB,JB)*OK
         IB=IB+1
   10    CONTINUE
         JB=JB+1
   15    CONTINUE
         C(IP,JP)=BAD
         IF(CNT.LT.CNTMIN) GO TO 25
C
C        COMPUTE CORRELATION COEFFICIENT FOR THIS LAG
C
         CNTI=1./CNT
         SUMA=SUMA*CNTI
         SUMB=SUMB*CNTI
         T1=SUMSQA*CNTI-SUMA*SUMA
         T2=SUMSQB*CNTI-SUMB*SUMB
         IF(T1.LT.EPS.OR.T2.LT.EPS) GO TO 25
         DENO=SQRT(T1)*SQRT(T2)
         IF(DENO.LT.EPS) GO TO 25
         C(IP,JP)=(PAB*CNTI-SUMA*SUMB)/DENO
   25 CONTINUE
   30 CONTINUE
      RETURN
      END
