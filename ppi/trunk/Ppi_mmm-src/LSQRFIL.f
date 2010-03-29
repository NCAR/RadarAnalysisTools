c
c----------------------------------------------------------------------X
c
      SUBROUTINE LSQRFIL(DAT,IOUT,IIN1,C1,C2,C3,C4,BDVAL,MNGATE,MXGATE,
     X     NGTS,NANG,TMP1,TMP2,MXR,MXA,MXF)
C
C  FUNCTION - BOUNDED LOCAL LINEAR LEAST-SQUARES FILLING OF FIELD (IIN1)
C             THE LOCAL REGION HAS MAXIMUM RANGE-ANGLE DIMENSIONS = 2*C1+1 
C             GATES AND BEAMS.  IF C2 QUADRANTS AND C3 MEASUREMENTS
C             ARE OBTAINED BEFORE C1 IS ENCOUNTERED THE SEARCH OUTWARD
C             STOPS AND THE LEAST-SQUARES SOLUTION IS OBTAINED.
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "   FOR OUTPUT INTO F(IOUT)
C     TMP2   - TEMPORARY STORAGE ARRAY
C
      DIMENSION TMP1(MXR),TMP2(MXR,MXA)
      DIMENSION DAT(MXR,MXA,MXF)
      DIMENSION AM(3),BM(3),CM(3),DM(3),IQUAD(4)
      DATA EPS/0.00001/

      ITMAX=MAX0(1,NINT(C1))
      NQUAD=NINT(C2)
      PTSMIN=C3
      print *,'LSQRFIL: itmax,nquad,ptsmin=',itmax,nquad,ptsmin
      print *,'LSQRFIL: nang,mngate,mxgate=',nang,mngate,mxgate
      bad=0.0
      good=0.0
      total=0.0
      DO 20 J=1,NANG
         DO 10 I=MNGATE,MXGATE
            total=total+1.0
            TMP2(I,J)=DAT(I,J,IIN1)
            if(tmp2(i,j).eq.bdval)then
               bad=bad+1.0
            else
               good=good+1.0
            end if
 10      CONTINUE
 20   CONTINUE
      print *,'LSQRFIL:     total,good,bad=',total,good,bad

C     OUTER LOOP OVER ALL CENTRAL INDICES (II,JJ).  COMPUTE THE LOCAL
C     LEAST-SQUARES, LINEAR FIT OF DAT(IIN1) WITHIN REGION (2*ITER+1,
C     2*ITER+1) SURROUNDING THE CENTRAL LOCATION.  STORE THIS FIT IN 
C     FIELD(IOUT) WHEN CENTRAL VALUE IS MISSING, OTHERWISE TRANSFER 
C     FIELD(IIN1).
C     
      DO 120 JJ=1,NANG
         DO 110 II=MNGATE,MXGATE
            DAT(II,JJ,IOUT)=BDVAL
            IF(C4.EQ.0.0)THEN
               IF(TMP2(II,JJ).NE.BDVAL)THEN
                  DAT(II,JJ,IOUT)=TMP2(II,JJ)
                  GO TO 110
               END IF
            END IF

C     PROCEED OUTWARD FROM CENTRAL INDEX UNTIL QUADRANT AND NUMBER
C     OF MEASUREMENT CONDITIONS ARE SATISFIED OR ITMAX IS REACHED.
C
            DO 100 ITER=1,ITMAX
               J1=MAX0(     1,JJ-ITER)
               J2=MIN0(  NANG,JJ+ITER)
               I1=MAX0(MNGATE,II-ITER)
               I2=MIN0(MXGATE,II+ITER)
               DO 52 L=1,3
                  AM(L)=0.0
                  BM(L)=0.0
                  CM(L)=0.0
                  DM(L)=0.0
 52            CONTINUE
               DO 54 N=1,4
                  IQUAD(N)=0
 54            CONTINUE

C     INNER LOOP FOR LEAST-SQUARES FITTING OVER NON-BAD VALUES
C     
               DO 70 I=I1,I2
                  IX=I-II
                  DO 60 J=J1,J2
                     IY=J-JJ
                     IF(IABS(IX).NE.ITER.AND.IABS(IY).NE.ITER) GO TO 60
                     IF(TMP2(I,J).EQ.BDVAL) GO TO 60
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
                     DM(1)=DM(1)+TMP2(I,J)
                     DM(2)=DM(2)+IX*TMP2(I,J)
                     DM(3)=DM(3)+IY*TMP2(I,J)
 60               CONTINUE
 70            CONTINUE

C     CALCULATE LST-SQR SOLUTION IF THE CONDITIONS OF THE NUMBERS OF 
C     QUADRANTS AND MEASUREMENTS ARE SATISFIED; OTHERWISE, CONTINUE
C     OUTWARD FROM THE CURRENT CENTRAL INDEX.
C
               KQ=0
               DO 80 K=1,4
                  KQ=KQ+IQUAD(K)
 80            CONTINUE
               IF(KQ.LT.NQUAD) GO TO 100
               IF(AM(1).LT.PTSMIN) GO TO 100
               BM(1)=AM(2)
               CM(1)=AM(3)
               CM(2)=BM(3)
               T1=BM(2)*CM(3)-BM(3)*CM(2)
               T2=BM(1)*CM(3)-BM(3)*CM(1)
               T3=BM(1)*CM(2)-BM(2)*CM(1)
               DENO=AM(1)*T1-AM(2)*T2+AM(3)*T3
               IF(DENO.LE.EPS) GO TO 100
               ANUM=DM(1)*T1-DM(2)*T2+DM(3)*T3
               DAT(II,JJ,IOUT)=ANUM/DENO
               GO TO 110
 100        CONTINUE

 110     CONTINUE
 120  CONTINUE

      write(*,*)'lsqrfil: ',itmax,nquad,ptsmin
      RETURN
      END
