c
c----------------------------------------------------------------------X
c
      SUBROUTINE FFT (A,M,INV,S,IFSET,IFERR)
      DIMENSION       A(1)       ,INV(1)     ,S(1)       ,N(3)       ,
     1                M(3)       ,NP(3)      ,W(2)       ,W2(2)      ,
     2                W3(2)
      EQUIVALENCE     (N1,N(1))  ,(N2,N(2))  ,(N3,N(3))
      IF (IABS(IFSET)-1) 166,166,101
  101 MTT = MAX0(M(1),M(2),M(3))-2
      ROOT2 = SQRT(2.)
      IF (MTT-MT) 103,103,102
  102 IFERR = 1
      RETURN
  103 IFERR = 0
      M1 = M(1)
      M2 = M(2)
      M3 = M(3)
      N1 = 2**M1
      N2 = 2**M2
      N3 = 2**M3
      IF (IFSET) 104,104,106
  104 NX = N1*N2*N3
      FN = NX
      DO 105 I=1,NX
         A(2*I-1) = A(2*I-1)/FN
         A(2*I) = -A(2*I)/FN
  105 CONTINUE
  106 NP(1) = N1*2
      NP(2) = NP(1)*N2
      NP(3) = NP(2)*N3
      DO 136 ID=1,3
         IL = NP(3)-NP(ID)
         IL1 = IL+1
         MI = M(ID)
         IF (MI) 136,136,107
  107    IDIF = NP(ID)
         KBIT = NP(ID)
         MEV = 2*(MI/2)
         IF (MI-MEV) 112,112,108
  108    KBIT = KBIT/2
         KL = KBIT-2
         DO 110 I=1,IL1,IDIF
            KLAST = KL+I
            DO 109 K=I,KLAST,2
               KD = K+KBIT
               T = A(KD)
               A(KD) = A(K)-T
               A(K) = A(K)+T
               T = A(KD+1)
               A(KD+1) = A(K+1)-T
               A(K+1) = A(K+1)+T
  109       CONTINUE
  110    CONTINUE
         IF (MI-1) 136,136,111
  111    LFIRST = 3
         JLAST = 1
         GO TO 113
  112    LFIRST = 2
         JLAST = 0
  113    DO 135 L=LFIRST,MI,2
            JJDIF = KBIT
            KBIT = KBIT/4
            KL = KBIT-2
            DO 115 I=1,IL1,IDIF
               KLAST = I+KL
               DO 114 K=I,KLAST,2
                  K1 = K+KBIT
                  K2 = K1+KBIT
                  K3 = K2+KBIT
                  T = A(K2)
                  A(K2) = A(K)-T
                  A(K) = A(K)+T
                  T = A(K2+1)
                  A(K2+1) = A(K+1)-T
                  A(K+1) = A(K+1)+T
                  T = A(K3)
                  A(K3) = A(K1)-T
                  A(K1) = A(K1)+T
                  T = A(K3+1)
                  A(K3+1) = A(K1+1)-T
                  A(K1+1) = A(K1+1)+T
                  T = A(K1)
                  A(K1) = A(K)-T
                  A(K) = A(K)+T
                  T = A(K1+1)
                  A(K1+1) = A(K+1)-T
                  A(K+1) = A(K+1)+T
                  R = -A(K3+1)
                  T = A(K3)
                  A(K3) = A(K2)-R
                  A(K2) = A(K2)+R
                  A(K3+1) = A(K2+1)-T
                  A(K2+1) = A(K2+1)+T
  114          CONTINUE
  115       CONTINUE
            IF (JLAST) 134,134,116
  116       JJ = JJDIF+1
            ILAST = IL+JJ
            DO 118 I=JJ,ILAST,IDIF
               KLAST = KL+I
               DO 117 K=I,KLAST,2
                  K1 = K+KBIT
                  K2 = K1+KBIT
                  K3 = K2+KBIT
                  R = -A(K2+1)
                  T = A(K2)
                  A(K2) = A(K)-R
                  A(K) = A(K)+R
                  A(K2+1) = A(K+1)-T
                  A(K+1) = A(K+1)+T
                  AWR = A(K1)-A(K1+1)
                  AWI = A(K1+1)+A(K1)
                  R = -A(K3)-A(K3+1)
                  T = A(K3)-A(K3+1)
                  A(K3) = (AWR-R)/ROOT2
                  A(K3+1) = (AWI-T)/ROOT2
                  A(K1) = (AWR+R)/ROOT2
                  A(K1+1) = (AWI+T)/ROOT2
                  T = A(K1)
                  A(K1) = A(K)-T
                  A(K) = A(K)+T
                  T = A(K1+1)
                  A(K1+1) = A(K+1)-T
                  A(K+1) = A(K+1)+T
                  R = -A(K3+1)
                  T = A(K3)
                  A(K3) = A(K2)-R
                  A(K2) = A(K2)+R
                  A(K3+1) = A(K2+1)-T
                  A(K2+1) = A(K2+1)+T
  117          CONTINUE
  118       CONTINUE
            IF (JLAST-1) 134,134,119
  119       JJ = JJ+JJDIF
            DO 133 J=2,JLAST
               I = INV(J+1)
               IC = NT-I
               W(1) = S(IC)
               W(2) = S(I)
               I2 = 2*I
               I2C = NT-I2
               IF (I2C) 122,121,120
  120          W2(1) = S(I2C)
               W2(2) = S(I2)
               GO TO 123
  121          W2(1) = 0.
               W2(2) = 1.
               GO TO 123
  122          I2CC = I2C+NT
               I2C = -I2C
               W2(1) = -S(I2C)
               W2(2) = S(I2CC)
  123          I3 = I+I2
               I3C = NT-I3
               IF (I3C) 126,125,124
  124          W3(1) = S(I3C)
               W3(2) = S(I3)
               GO TO 130
  125          W3(1) = 0.
               W3(2) = 1.
               GO TO 130
  126          I3CC = I3C+NT
               IF (I3CC) 129,128,127
  127          I3C = -I3C
               W3(1) = -S(I3C)
               W3(2) = S(I3CC)
               GO TO 130
  128          W3(1) = -1.
               W3(2) = 0.
               GO TO 130
  129          I3CCC = NT+I3CC
               I3CC = -I3CC
               W3(1) = -S(I3CCC)
               W3(2) = -S(I3CC)
  130          ILAST = IL+JJ
               DO 132 I=JJ,ILAST,IDIF
                  KLAST = KL+I
                  DO 131 K=I,KLAST,2
                     K1 = K+KBIT
                     K2 = K1+KBIT
                     K3 = K2+KBIT
                     R = A(K2)*W2(1)-A(K2+1)*W2(2)
                     T = A(K2)*W2(2)+A(K2+1)*W2(1)
                     A(K2) = A(K)-R
                     A(K) = A(K)+R
                     A(K2+1) = A(K+1)-T
                     A(K+1) = A(K+1)+T
                     R = A(K3)*W3(1)-A(K3+1)*W3(2)
                     T = A(K3)*W3(2)+A(K3+1)*W3(1)
                     AWR = A(K1)*W(1)-A(K1+1)*W(2)
                     AWI = A(K1)*W(2)+A(K1+1)*W(1)
                     A(K3) = AWR-R
                     A(K3+1) = AWI-T
                     A(K1) = AWR+R
                     A(K1+1) = AWI+T
                     T = A(K1)
                     A(K1) = A(K)-T
                     A(K) = A(K)+T
                     T = A(K1+1)
                     A(K1+1) = A(K+1)-T
                     A(K+1) = A(K+1)+T
                     R = -A(K3+1)
                     T = A(K3)
                     A(K3) = A(K2)-R
                     A(K2) = A(K2)+R
                     A(K3+1) = A(K2+1)-T
                     A(K2+1) = A(K2+1)+T
  131             CONTINUE
  132          CONTINUE
               JJ = JJDIF+JJ
  133       CONTINUE
  134       JLAST = 4*JLAST+3
  135    CONTINUE
  136 CONTINUE
      NTSQ = NT*NT
      M3MT = M3-MT
      IF (M3MT) 138,137,137
  137 IGO3 = 1
      N3VNT = N3/NT
      MINN3 = NT
      GO TO 139
  138 IGO3 = 2
      N3VNT = 1
      NTVN3 = NT/N3
      MINN3 = N3
  139 JJD3 = NTSQ/N3
      M2MT = M2-MT
      IF (M2MT) 141,140,140
  140 IGO2 = 1
      N2VNT = N2/NT
      MINN2 = NT
      GO TO 142
  141 IGO2 = 2
      N2VNT = 1
      NTVN2 = NT/N2
      MINN2 = N2
  142 JJD2 = NTSQ/N2
      M1MT = M1-MT
      IF (M1MT) 144,143,143
  143 IGO1 = 1
      N1VNT = N1/NT
      MINN1 = NT
      GO TO 145
  144 IGO1 = 2
      N1VNT = 1
      NTVN1 = NT/N1
      MINN1 = N1
  145 JJD1 = NTSQ/N1
      JJ3 = 1
      J = 1
      DO 162 JPP3=1,N3VNT
         IPP3 = INV(JJ3)
         DO 161 JP3=1,MINN3
            GO TO (146,147),IGO3
  146       IP3 = INV(JP3)*N3VNT
            GO TO 148
  147       IP3 = INV(JP3)/NTVN3
  148       I3 = (IPP3+IP3)*N2
            JJ2 = 1
            DO 160 JPP2=1,N2VNT
               IPP2 = INV(JJ2)+I3
               DO 159 JP2=1,MINN2
                  GO TO (149,150),IGO2
  149             IP2 = INV(JP2)*N2VNT
                  GO TO 151
  150             IP2 = INV(JP2)/NTVN2
  151             I2 = (IPP2+IP2)*N1
                  JJ1 = 1
                  DO 158 JPP1=1,N1VNT
                     IPP1 = INV(JJ1)+I2
                     DO 157 JP1=1,MINN1
                        GO TO (152,153),IGO1
  152                   IP1 = INV(JP1)*N1VNT
                        GO TO 154
  153                   IP1 = INV(JP1)/NTVN1
  154                   I = 2*(IPP1+IP1)+1
                        IF (J-I) 155,156,156
  155                   T = A(I)
                        A(I) = A(J)
                        A(J) = T
                        T = A(I+1)
                        A(I+1) = A(J+1)
                        A(J+1) = T
  156                   CONTINUE
                        J = J+2
  157                CONTINUE
                     JJ1 = JJ1+JJD1
  158             CONTINUE
  159          CONTINUE
               JJ2 = JJ2+JJD2
  160       CONTINUE
  161    CONTINUE
         JJ3 = JJ3+JJD3
  162 CONTINUE
      IF (IFSET) 163,165,165
  163 DO 164 I=1,NX
         A(2*I) = -A(2*I)
  164 CONTINUE
  165 RETURN
  166 MT = MAX0(M(1),M(2),M(3))-2
      MT = MAX0(2,MT)
      IF (MT-20) 168,168,167
  167 IFERR = 1
      GO TO 165
  168 IFERR = 0
      NT = 2**MT
      NTV2 = NT/2
      THETA = .7853981634
      JSTEP = NT
      JDIF = NTV2
      S(JDIF) = SIN(THETA)
      DO 171 L=2,MT
         THETA = THETA/2.
         JSTEP2 = JSTEP
         JSTEP = JDIF
         JDIF = JSTEP/2
         S(JDIF) = SIN(THETA)
         JC1 = NT-JDIF
         S(JC1) = COS(THETA)
         JLAST = NT-JSTEP2
         IF (JLAST-JSTEP) 171,169,169
  169    DO 170 J=JSTEP,JLAST,JSTEP
            JC = NT-J
            JD = J+JDIF
            S(JD) = S(J)*S(JC1)+S(JDIF)*S(JC)
  170    CONTINUE
  171 CONTINUE
C
C SET UP INV(J) TABLE
C
      MTLEXP = NTV2
      LM1EXP = 1
      INV(1) = 0
      DO 173 L=1,MT
         INV(LM1EXP+1) = MTLEXP
         DO 172 J=2,LM1EXP
            JJ = J+LM1EXP
            INV(JJ) = INV(J)+MTLEXP
  172    CONTINUE
         MTLEXP = MTLEXP/2
         LM1EXP = LM1EXP*2
  173 CONTINUE
      IF (IFSET) 101,165,101
      END
