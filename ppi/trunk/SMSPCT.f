c
c----------------------------------------------------------------------X
c
      SUBROUTINE SMSPCT (P,MPS,NA,DUM)
      DIMENSION       P(1)       ,DUM(1)
      IF (MOD(NA,2)) 102,101,102
  101 NA = NA+1
  102 FA = NA
      N2 = NA/2
      N3 = N2+1
      DUM(1) = P(1)
      IF (N2-2) 106,103,103
  103 L = 0
      DO 105 I=2,N2
         DUM(I) = 0.
         L = L+1
         IS = I-L
         IL = I+L
         DO 104 J=IS,IL
            DUM(I) = DUM(I)+P(J)
  104    CONTINUE
         R = IL-IS+1
         DUM(I) = DUM(I)/R
  105 CONTINUE
  106 N4 = MPS-N2
      N5 = N4+1
      MP1 = MPS-1
      DO 108 I=N3,N4
         IS = I-N2
         IL = I+N2
         DUM(I) = 0.
         DO 107 J=IS,IL
            DUM(I) = DUM(I)+P(J)
  107    CONTINUE
         DUM(I) = DUM(I)/FA
  108 CONTINUE
      L = N2
      IF (N2-2) 112,109,109
  109 DO 111 I=N5,MP1
         L = L-1
         IS = I-L
         IL = I+L
         R = IL-IS+1
         DUM(I) = 0.
         DO 110 J=IS,IL
            DUM(I) = DUM(I)+P(J)
  110    CONTINUE
         DUM(I) = DUM(I)/R
  111 CONTINUE
  112 DUM(MPS) = (P(MPS)+P(MPS-1))/2.
      DO 113 I=1,MPS
         P(I) = DUM(I)
  113 CONTINUE
      RETURN
      END
