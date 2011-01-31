      SUBROUTINE HEDSHF(IN,IO,N)
C
C        SHIFTS ALL RELEVANT INFORMATION TO THE RIGHTMOST 16 BITS
C               DATA IS TRANSFERRED FROM IN TO IO.
C
      DIMENSION IN(N), IO(N)
      DO 10 I=1,N
         IO(I)=IN(I)
   10 CONTINUE
C
C        SHIFT ALL ALPHA INFOMATION TO LOWER 16 BITS
C
      CALL SHIRBL(IO(  1),20)
      CALL SHIRBL(IO( 43),16)
      CALL SHIRBL(IO( 62), 1)
      CALL SHIRBL(IO( 66), 1)
      CALL SHIRBL(IO( 71),24)
      CALL SHIRBL(IO(101), 4)
      CALL SHIRBL(IO(151), 1)
C
C        FIELD NAMES
C
      K=IO(175)
      I=176
      DO 20 J=1,K
         CALL SHIRBL(IO(I),4)
         I=I+5
   20 CONTINUE
C
C        LANDMARK NAMES
C
      K=IO(302)
      I=306
      DO 30 J=1,K
         CALL SHIRBL(IO(I),3)
         I=I+6
   30 CONTINUE
      RETURN
      END