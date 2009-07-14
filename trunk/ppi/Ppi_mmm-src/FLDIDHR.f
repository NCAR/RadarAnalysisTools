c
c----------------------------------------------------------------------X
c
      SUBROUTINE FLDIDHR(NAMFLD,NFLDS,IFLD)
      INCLUDE 'dim.inc'

      DIMENSION IFLD(MXF)
      CHARACTER*8 NAMFLD(MXF)

      WRITE(6,11)NFLDS
   11 FORMAT(1X,/,' Fields to be processed = ',I3,
     +       /,1X,' IFLD: Number associated with the requested field',
     +       /,1X,'       ( #) Number of field on the input tape    ',
     +       /,1X,'       ( 0) Field is derived                     ',
     +       /,1X,'       (-1)   "    " to be swathed               ',
     +       /,1X,'       (-2)   "    " to be integrated in time    ',
     +       /,1X,'       (-3)   "    " the integration time        ',
     +       /,1x,'       (-4)   "    " to be extracted pseudo-rhi  ',
     +       /,1x,'       (-5)   "    " to be averaged sweep-to-sweep',
     +       /,1x,'       (-6)   "    " is the number averaged'/)

      DO 20 I=1,NFLDS
         IF(IFLD(I).LE.-1)GO TO 12
         IFLD(I)=0
         IF(NAMFLD(I).EQ.'DZ      ')IFLD(I)=I
 12      CONTINUE
         WRITE(6,13)I,NAMFLD(I),IFLD(I)
 13      FORMAT(1X,'     #',I2,' - ',A8,' IFLD = ',I2)
 20   CONTINUE
      RETURN
      END
