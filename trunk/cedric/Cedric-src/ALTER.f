      SUBROUTINE ALTER(IA,IB,NRP)
C
C     TESTS AND CHANGES NEGATIVE 16 BIT NUMBERS TO NEGATIVE 32 BIT
C
      INTEGER CVMGP
      DIMENSION IA(1),IB(1)
      DO 20 I=1,NRP
         IX=IA(I)
         IB(I)=CVMGP(IX-65536,IX,IX-32768)
 20   CONTINUE
      RETURN
      END
