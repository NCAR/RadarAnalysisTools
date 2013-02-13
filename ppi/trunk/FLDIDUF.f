
c----------------------------------------------------------------------X
c
      SUBROUTINE FLDIDUF(NAMFLD,NFLDS,NAMUF,NFLD,IFLD)
C
C  DETERMINE IF REQUESTED FIELDS ARE WITHIN THE INPUT FILE
C
C  INPUT:
C        NAMFLD - NAME OF FIELD REQUESTED
C        NFLDS  - TOTAL NUMBER OF FIELDS REQUESTED
C        NAMUF  - NAME OF FIELD WITHIN THE UNIVERSAL FORMAT INPUT FILE
C        NFLD   - NUMBER OF FIELDS WITHIN THE INPUT FILE
C  OUTPUT:
C        IFLD   - FIELD NUMBER ON TAPE (#) OF THE REQUESTED FIELD
C                 ( 0) IF THE REQUESTED FIELD IS NOT ON THE INPUT TAPE
C                 ( #) IF THE REQUESTED FIELD IS ON THE INPUT TAPE
C                 (-1) IF THE REQUESTED FIELD IS TO BE 'SWATHED'
C                 (-2) IF THE REQUESTED FIELD IS TO BE INTEGRATED
C                 (-3) IF THE REQUESTED FIELD IS THE INTEGRATION TIME
C                 (-4) IF THE REQUESTED FIELD IS EXTRACTED AS PSEUDO-RHI
C                 (-5) IF THE REQUESTED FIELD IS TO BE AVERAGED SWEEP-TO-SWEEP
C                 (-6) IF THE REQUESTED FIELD IS THE NUMBER OF SWEEPS AVERAGED
C                      SEE ROUTINES SWATH, INTEGR, ISOCHRON, FXSWATH
C
      DIMENSION IFLD(NFLDS)
      CHARACTER*8 NAMFLD(NFLDS),NAMUF(NFLD)

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

      DO 100 I=1,NFLDS
         IF(IFLD(I).LE.-1)GO TO 90
         IFLD(I)=0
         DO 80 J=1,NFLD
   80    IF(NAMFLD(I).EQ.NAMUF(J))IFLD(I)=J
   90    CONTINUE
         WRITE(6,93)I,NAMFLD(I),IFLD(I)
   93    FORMAT(1X,'     #',I2,' - ',A8,' IFLD = ',I2)
         CALL SFLUSH
  100 CONTINUE
      RETURN
      END






