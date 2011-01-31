      program tst_logical

      DATA MASK/ O'100000'/

      do i=1,32
         j=2**(i-1)
         print *,'i,j=',i,j
      end do
      j=2**15+2**31
      print *,'j=',j

      INITCB = 0
      print *,'mask=',mask
      do i=1,2
         initcb = icedor(initcb,icedshft(mask,(i-1)*16))
      end do
      print *,'initcb=',initcb

      stop
      end


      FUNCTION ICEDOR(I1,I2)
C
C     PERFORM A LOGICAL OR
C
      ICEDOR = OR(I1,I2)
      RETURN
      END

      FUNCTION ICEDSHFT(I1,I2)
C
C     SHIFT I1 I2 PLACES TO THE LEFT IF I2 >0, TO THE RIGHT IF I2 < 0
C
      ICEDSHFT = ISHFT(I1,I2)
      RETURN
      END

