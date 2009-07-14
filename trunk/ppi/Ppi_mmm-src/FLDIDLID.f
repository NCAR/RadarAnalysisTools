      SUBROUTINE FLDIDLID(NAMFLD,NFLDS,IFLD)
      INCLUDE 'dim.inc'

      DIMENSION IFLD(MXF)
      CHARACTER*8 NAMFLD(MXF)

      DO 10 I=1,NFLDS
         IF(IFLD(I).LE.-1)GO TO 90
         IFLD(I)=0
         IF(NAMFLD(I).EQ.'DZ      '.OR.
     +      NAMFLD(I).EQ.'VEL     '.OR.
     +      NAMFLD(I).EQ.'SW      ')IFLD(I)=I
   90    CONTINUE
         WRITE(6,93)I,NAMFLD(I),IFLD(I)
   93    FORMAT(1X,'     #',I2,' - ',A8,' IFLD = ',I2)
 10      CONTINUE
      RETURN
      END
