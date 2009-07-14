      SUBROUTINE UNCODE(NUM,N1,N2,N3)
C
C        GIVEN A SEVEN DIGIT NUMBER
C           DIGITS 1-3 => N1
C                  4-5 => N2
C                  6-7 => N3
C
c-----print *,NUM
      N1=NUM/10000
      N2=(NUM-(10000*N1))/100
      N3=NUM-(10000*N1+100*N2)
      RETURN
      END
