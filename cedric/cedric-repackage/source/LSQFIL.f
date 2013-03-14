      SUBROUTINE LSQFIL(C,A,NI,NJ,IBEG,IEND,JBEG,JEND,ITMAX,BAD)
C
C        PERFORMS LEAST-SQUARES DATA SMOOTHING OF A BOUNDED REGION
C           C- (INPUT) IS THE ORIGINAL DATA FIELD CONTAINING BAD VALUES
C              (OUTPUT) CONTAINS SMOOTH ESTIMATES AT ALL NON-BAD LOCATIONS
C           A- (INPUT) CONTAINS A FILLED VERSION OF (INPUT) C.
C
      DIMENSION A(NI,NJ),C(NI,NJ),AM(3),BM(3),CM(3),DM(3)
      DATA EPS/0.00001/
      DO 50 JO=JBEG,JEND
      DO 50 IO=IBEG,IEND
         IF(C(IO,JO).EQ.BAD) GO TO 50
         DO 15 L=1,3
            AM(L)=0.0
            BM(L)=0.0
            CM(L)=0.0
            DM(L)=0.0
   15    CONTINUE
            J1=MAX0( 1,JO-ITMAX)
            J2=MIN0(NJ,JO+ITMAX)
            I1=MAX0( 1,IO-ITMAX)
            I2=MIN0(NI,IO+ITMAX)
            DO 20 J=J1,J2
               IY=J-JO
            DO 20 I=I1,I2
               IX=I-IO
               IF(A(I,J).EQ.BAD) GO TO 20
               AM(1)=AM(1)+1.0
               AM(2)=AM(2)+IX
               AM(3)=AM(3)+IY
               BM(2)=BM(2)+IX*IX
               BM(3)=BM(3)+IX*IY
               CM(3)=CM(3)+IY*IY
               DM(1)=DM(1)+A(I,J)
               DM(2)=DM(2)+IX*A(I,J)
               DM(3)=DM(3)+IY*A(I,J)
   20       CONTINUE
            BM(1)=AM(2)
            CM(1)=AM(3)
            CM(2)=BM(3)
            T1=BM(2)*CM(3)-BM(3)*CM(2)
            T2=BM(1)*CM(3)-BM(3)*CM(1)
            T3=BM(1)*CM(2)-BM(2)*CM(1)
            DENO=AM(1)*T1-AM(2)*T2+AM(3)*T3
            IF(DENO.LE.EPS) GO TO 50
            ANUM=DM(1)*T1-DM(2)*T2+DM(3)*T3
            C(IO,JO)=ANUM/DENO
   50 CONTINUE
      RETURN
      END
