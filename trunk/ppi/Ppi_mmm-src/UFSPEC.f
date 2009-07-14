c
c----------------------------------------------------------------------X
c
      SUBROUTINE UFSPEC(NAMOUT,NMOUTUF,IUFUN,NUMUF,NUMUN,IWRUF,NAMFLD,
     X     NFLDS,INDAT,GRDTYP,IWFOR)
C     
C     Routine to set up units and field names for Universal Format (UF)
C     output, either in Fortran- or COS-blocked records.  See UFWRITE.
C     
C     NAMFLD  - Names of all internal (to this program) fields.
C     NFLDS   - Number of internal fields.
C
C     IUFUN   - Unit numbers (e.g. 21, 22, ...) to be used for UF output.
C     NUMUN   _ Total number of output units to be used for writing output.
C
C     NUMUF   - Number of UF fields to be written.
C     NAMOUT  - Names of those internal fields to be written.
C     NMOUTUF - Names that output UF fields are to have.
C     Note: When specifying UF output names on DEC, right-justify the name;
C           otherwise, the UF output name is left-justified. 
C           For example, use an output name = '......DZ' on DEC machines,
C                                       and = 'DZ......' on other machines.
C     IWRUF   - UF writing flag: (0) not writing, (1) writing UF output.
C     IWFOR   - Output blocking flag: (0) COS-blocked, (1) Fortran-blocked.
C               Note: If running on a Cray, Fortran-blocking is COS-blocking
C                     so set INDAT(2) = 'Fortran'
C     GRDTYP - Type of grid (range-angle) to be written, either SWATH
C              as used for swaths, integration, gridding, etc or INPUT 
C              (the original sampling locations).
C
      INCLUDE 'dim.inc'
      
      PARAMETER (MXUF=30,MXUN=30)
      DIMENSION IUFUN(MXUN)
      CHARACTER*8 NAMOUT(MXUF),NMOUTUF(MXUF)
      CHARACTER*8 NAMFLD(MXF),INDAT(10),GRDTYP
      CHARACTER*4 NAMSTATS
      DATA NAMSTATS/'    '/
      
      NUMUF=1
      NUMUN=0
      IWRUF=0

C     READ OUTPUT UNIT NUMBERS UNTIL P1='END'.
C
 10   READ(5,15)INDAT(1),UNIT
 15   FORMAT(A8,F8.0)
c      WRITE(6,16)INDAT(1),UNIT
c 16   FORMAT('Kardin=',A8,F8.0)
      IF(INDAT(1).NE.'END')THEN
         NUMUN=NUMUN+1
         IUFUN(NUMUN)=UNIT
         IF(NUMUN.GT.MXUN)THEN
            WRITE(6,17)MXUN
 17         FORMAT(1X,
     +           '****MAXIMUM NUMBER OF UF OUTPUT UNITS EXCEEDED****',
     +           ' NUMBER MUST BE .LE. ',I3)
            STOP
         END IF
         GO TO 10
      END IF
      
C     READ NAMES OF FIELDS TO OUTPUT (NAMOUT) AND THE NAMES THEY
C     ARE TO HAVE IN THE UF HEADER (NMOUTUF).  Read until P1='END'.
C     
 20   READ(5,25)INDAT(1),NAMOUT(NUMUF),NMOUTUF(NUMUF)
 25   FORMAT(3A8)
      WRITE(6,26)INDAT(1),NAMOUT(NUMUF),NMOUTUF(NUMUF)
 26   FORMAT('Kardin=',3A8)

CANNE     NMOUTUF(NUMUF)=SHIFTR(NMOUTUF(NUMUF),48)
      IF(INDAT(1).NE.'END')THEN
         CALL FIELD(NAMOUT(NUMUF),NAMFLD,NFLDS,NAMSTATS)
         NUMUF=NUMUF+1
         IF(NUMUF.GT.MXUF)THEN
            WRITE(6,27)MXUF
 27         FORMAT(1X,
     +           '****MAXIMUM NUMBER OF OUTPUT UF FIELDS EXCEEDED****',
     +           ' NUMBER MUST BE .LE. ',I3)
            STOP
         END IF
         GO TO 20
      END IF
      NUMUF=NUMUF-1

      IF(NUMUN.GT.0.AND.NUMUF.GT.0)THEN
         IWRUF=1
         WRITE(6,50)(IUFUN(N),N=1,NUMUN)
 50      FORMAT(/,1X,'Data will be written in Universal Format (UF)',
     +        /,1X,'Using units ',20I3)
         DO 100 I=1,NUMUF
            WRITE(6,60)I,NAMOUT(I),NMOUTUF(I)
 60         FORMAT(1X,I4,2X,'Output field: ',A8,2X,'UF name of ',
     +           'output field: ',A8)
 100     CONTINUE
      END IF

C     Set flag (IWFOR) for writing UF as 
C        Fortran- (1) or COS-blocked (0) records.  See UFWRITE routine.
C
      IF(INDAT(2)(1:1).EQ.'F')THEN
         IWFOR=1
         WRITE(6,107)
 107     FORMAT(' UF output records will be Fortran-blocked')
      ELSE
         IWFOR=0
         WRITE(6,109)
 109     FORMAT(' UF output records will be COS-blocked')
      END IF

C     GRDTYP - Range-angle grid (SWATH or INPUT) to be written, 
C
      GRDTYP=INDAT(3)
      IF(GRDTYP.EQ.'SWATH')THEN
         WRITE(6,111)
 111     FORMAT(' Range-angles are from regular SWATH grid',/)
      ELSE
         WRITE(6,113)
 113     FORMAT(' Range-angles are from original INPUT grid',/)
      END IF

      RETURN
      END
