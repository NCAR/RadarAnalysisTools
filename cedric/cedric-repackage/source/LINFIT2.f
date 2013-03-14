      SUBROUTINE LINFIT2(C,A,B,NI,NJ,IBEG,IEND,JBEG,JEND,
     X                  ITMAX,NQUAD,MINPTS,BAD)
C
C        PERFORMS GLOBAL LEAST-SQUARES DATA FILLING OF A BOUNDED REGION
C
      DIMENSION A(NI,NJ),C(NI,NJ),AM(3),BM(3),CM(3),DM(3)
      DIMENSION B(NI,NJ)
      DATA EPS/0.00001/
C
C     INITALIZE OUTPUT FIELD TO BAD
C
      CALL CONFLD(C,(NI*NJ),BAD)

      PTSMIN=MINPTS
      IO=NINT((IEND-IBEG)/2.)
      JO=NINT((JEND-JBEG)/2.)
      DO 15 L=1,3
         AM(L)=0.0
         BM(L)=0.0
         CM(L)=0.0
         DM(L)=0.0
 15   CONTINUE
      DO 30 L=1,MAX(IO,JO)+1
         J1=MAX0( JBEG,JO-L)
         J2=MIN0( JEND,JO+L)
         I1=MAX0( IBEG,IO-L)
         I2=MIN0( IEND,IO+L)
         DO 20 J=J1,J2
            IY=J-JO
            DO 25 I=I1,I2
               IX=I-IO
               IF(IABS(IX).NE.L.AND.IABS(IY).NE.L) GO TO 25
               IF(A(I,J).EQ.BAD) GO TO 25
               AM(1)=AM(1)+1.0
               AM(2)=AM(2)+IX
               AM(3)=AM(3)+IY
               BM(2)=BM(2)+IX*IX
               BM(3)=BM(3)+IX*IY
               CM(3)=CM(3)+IY*IY
               DM(1)=DM(1)+A(I,J)
               DM(2)=DM(2)+IX*A(I,J)
               DM(3)=DM(3)+IY*A(I,J)
 25         CONTINUE
 20      CONTINUE
 30   CONTINUE
      IF (AM(1).LT.PTSMIN) THEN
         RETURN
      ELSE
C
C     CALCULATE COEFFICIENTS 
C
         BM(1)=AM(2)
         CM(1)=AM(3)
         CM(2)=BM(3)
         T1=BM(2)*CM(3)-BM(3)*CM(2)
         T2=BM(1)*CM(3)-BM(3)*CM(1)
         T3=BM(1)*CM(2)-BM(2)*CM(1)
         T4=AM(2)*CM(3)-AM(3)*CM(2)
         T5=AM(1)*CM(3)-AM(3)*CM(1)
         T6=AM(1)*CM(2)-AM(2)*CM(1)
         T7=AM(2)*BM(3)-AM(3)*BM(2)
         T8=AM(1)*BM(3)-AM(3)*BM(1)
         T9=AM(1)*BM(2)-AM(2)*BM(1)
         DENO=AM(1)*T1-AM(2)*T2+AM(3)*T3
         IF(DENO.LE.EPS) RETURN
         FNUM=DM(1)*T1-DM(2)*T2+DM(3)*T3
         F0=FNUM/DENO
         ANUM=-(DM(1)*T4 - DM(2)*T5 + DM(3)*T6)
         AVAL=ANUM/DENO
         BNUM=(DM(1)*T7 - DM(2)*T8 + DM(3)*T9)
         BVAL=BNUM/DENO
C
C     NOW EVALUATE THE LINEAR FUNCTION AT ALL GRID POINTS 
C
         DO J=JBEG,JEND
            DO I=IBEG,IEND
               IF (B(I,J).EQ.BAD) THEN
                  C(I,J)=F0 + AVAL*(I-IO) + BVAL*(J-JO)
               END IF
            END DO
         END DO

      END IF

      RETURN

      END

