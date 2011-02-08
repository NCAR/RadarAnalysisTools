c
c----------------------------------------------------------------------X
c
      SUBROUTINE MAJMIN(AXMN,AXMX,IFMT,MAJOR,MINOR,IDIGITS)
C                                         R. VAUGHAN      MARCH 1977
C                          MODIFICATION:  L. JAY MILLER   AUGUST 1990
C                                                         NOVEMBER 1992
C   INPUT:
C       AXMN,AXMX - MIN/MAX VALUES OF THE AXIS
C       DELRG     - AXIS LENGTH OVER WHICH MAJOR AND MINOR
C                   DIVISIONS ARE TO BE CALCULATED.
C       FXMN,FXMX - FRACTIONAL PARTS OF MIN/MAX VALUES
C
C   OUTPUT:
C       IFMT    - FORMAT OF THE VALUES AT MAJOR DIVISIONS
C       MAJOR   - MAJOR DIVISIONS OF AXIS
C       MINOR   - MINOR     "      "   "
C       IDIGITS - NUMBER OF DIGITS FOR AXIS LABEL AT MAJOR DIVISIONS
C
C
      PARAMETER (NF=50)
      CHARACTER*6 IFMT
      DIMENSION FLIM(NF)
      DATA FLIM / 1000., 900.,800.,700.,600.,500.,400.,300.,200.,100.,
     +             100.,  90., 80., 70., 60., 50., 40., 30., 20., 10.,
     +             10. ,  9. , 8. , 7. , 6. , 5. , 4. , 3. , 2.,  1. ,
     +             1.  , .9  ,.8  ,.7  ,.6  ,.5  ,.4  ,.3  ,.2 , .1  ,
     +            .1   ,.09 ,.08 ,.07 ,.06 ,.05 ,.04 ,.03 ,.02, .01  /
      DATA EPS/0.0001/

      DELRG=AXMX-AXMN
      FXMX=AXMX-INT(AXMX)
      FXMN=AXMN-INT(AXMN)

      DO 5 I=1,NF
         MAJOR=DELRG/FLIM(I)+EPS
         IF(MAJOR.GT.0)FRACT=ABS(NINT(DELRG/MAJOR)-(DELRG/MAJOR))
         IF(FRACT.GT.EPS .AND. FXMX .EQ. 0.0 .AND. FXMN .EQ. 0.0)GO TO 5
         IF(ABS(MAJOR*FLIM(I)-DELRG).LE.EPS.AND.MAJOR.GT.2)GO TO 10
    5 CONTINUE
      MAJOR=1
      GO TO 20
   10 CONTINUE
      IF(MAJOR.GT.10) GO TO 20

C  GOOD VALUE FOR MAJOR
C
      DMNR=DELRG/MAJOR
c      print *,'MAJMIN: major,delrg,dmnr=',major,delrg,dmnr
      DO 15 I=1,NF
         MINOR=DMNR/FLIM(I)+EPS
c         print *,'MAJMIN: i,flim(i),minor=',i,flim(i),minor
         IF(ABS(MINOR*FLIM(I)-DMNR).LE.EPS.AND.
     +           MINOR.GT.MAJOR/2.AND.MINOR.GT.2) GO TO 30
   15 CONTINUE
      MINOR=1
      GO TO 30

C  NOT SO GOOD VALUE OF MAJOR
C
   20 CONTINUE
      MINOR=MAJOR
      MAJOR=1
   30 CONTINUE
      IDIGITS=1.001+ALOG10(AMAX1(ABS(AXMN),ABS(AXMX)))
      IF(AXMN.LT.0.0.OR.AXMX.LT.0.0)IDIGITS=IDIGITS+1
      FRACT=ABS(NINT(DELRG/MAJOR)-(DELRG/MAJOR))
      IF(FRACT.LT.EPS .AND. (FXMX .EQ. 0.0 .AND. FXMN .EQ. 0.0))THEN
         IF(IDIGITS.GT.9)IDIGITS=9
         WRITE(IFMT,33)IDIGITS
   33    FORMAT('(I',I1,')')
      ELSE
         IDEC=1.001+ABS(ALOG10(AMAX1(ABS(FXMN),ABS(FXMX))))
         IDIGITS=IDIGITS+IDEC+1
         IF(IDIGITS.GT.9)IDIGITS=9
         WRITE(IFMT,35)IDIGITS,IDEC
   35    FORMAT('(F',I1,'.',I1,')')
      END IF

      RETURN
      END
