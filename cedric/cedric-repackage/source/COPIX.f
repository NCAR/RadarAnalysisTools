      SUBROUTINE COPIX(IO,II,N)
C
C        COPIES ONE ARRAY TO ANOTHER
C
C               II- INPUT ARRAY
C               IO- OUTPUT ARRAY
C
      DIMENSION II(1),IO(1)
      DO 10 I=1,N
         IO(I)=II(I)
   10 CONTINUE
      RETURN
      END
