!>
!!        PERFORMS LEAST-SQUARES DATA FILLING OF A BOUNDED REGION

SUBROUTINE BNDFIL(C,A,B,NI,NJ,IBEG,IEND,JBEG,JEND,ITMAX,NQUAD,MINPTS,BAD)

    DIMENSION A(NI,NJ),C(NI,NJ),AM(3),BM(3),CM(3),DM(3),IQUAD(4)
    DIMENSION B(NI,NJ)
    DATA EPS/0.00001/

    PTSMIN=MINPTS
    DO JO=JBEG,JEND
        DO IO=IBEG,IEND
            IF(A(IO,JO).NE.BAD.OR.B(IO,JO).NE.BAD) CYCLE
            AM(:)=0.0
            BM(:)=0.0
            CM(:)=0.0
            DM(:)=0.0
            IQUAD(:)=0

            DO L=1,ITMAX
                J1=MAX0( 1,JO-L)
                J2=MIN0(NJ,JO+L)
                I1=MAX0( 1,IO-L)
                I2=MIN0(NI,IO+L)
                DO J=J1,J2
                    IY=J-JO
                    DO I=I1,I2
                        IX=I-IO
                        IF(IABS(IX).NE.L.AND.IABS(IY).NE.L) CYCLE
                        IF(A(I,J).EQ.BAD) CYCLE
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
                        DM(1)=DM(1)+A(I,J)
                        DM(2)=DM(2)+IX*A(I,J)
                        DM(3)=DM(3)+IY*A(I,J)
                    ENDDO
                ENDDO

                KQ=0
                DO K=1,4
                    KQ=KQ+IQUAD(K)
                ENDDO
                IF(KQ.LT.NQUAD) CYCLE
                IF(AM(1).LT.PTSMIN) CYCLE
                BM(1)=AM(2)
                CM(1)=AM(3)
                CM(2)=BM(3)
                T1=BM(2)*CM(3)-BM(3)*CM(2)
                T2=BM(1)*CM(3)-BM(3)*CM(1)
                T3=BM(1)*CM(2)-BM(2)*CM(1)
                DENO=AM(1)*T1-AM(2)*T2+AM(3)*T3
                IF(DENO.LE.EPS) CYCLE
                ANUM=DM(1)*T1-DM(2)*T2+DM(3)*T3
                C(IO,JO)=ANUM/DENO
                EXIT
            END DO
        ENDDO
    ENDDO


END SUBROUTINE BNDFIL
