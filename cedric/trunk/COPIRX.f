      SUBROUTINE COPIRX(IO,RI,N)
C
C        COPIES ONE ARRAY TO ANOTHER
C
C               RI- INPUT ARRAY
C               RO- OUTPUT ARRAY
C
      DIMENSION RI(1),IO(1)
      DO 10 I=1,N
         IO(I)=INT(RI(I))
   10 CONTINUE
      RETURN
      END
