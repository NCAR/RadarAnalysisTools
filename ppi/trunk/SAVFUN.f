c
c----------------------------------------------------------------------X
c
      SUBROUTINE SAVFUN(INDAT,IFMT,SFUN,NAMFLD,IFLD,NFLDS,MXNF,NF,DAT,
     X                  BDVAL,AZA,ELA,NANG,MVD,NAMVD,NAMINVD,MXVD,
     X                  MXR,MXA,MXF,SCANTYP)
C
C  SAVE ALL INFORMATION FOR LATER EXECUTION OF FUNCTION STACK
C     MXNF - MAXIMUM NUMBER OF FUNCTIONS ALLOWED
C     NF   - ACTUAL     "    "     "     TO BE EXECUTED
C     SFUN - ARRAY OF FUNCTION CHARACTERISTICS
C     NAMVD   - OUTPUT NAMES FROM VAD ANALYSIS
C     NAMINVD -  INPUT   "    FOR  "     "
C     SCANTYP - TYPE OF SCAN TO BE SWATHED.
C     Note: Several functions use swath coordinates if NIN2(1:3)='REG' 
C           so set IFLD=-1; otherwise, set IFLD=0.
C
      INCLUDE 'functions.inc'

      CHARACTER*8 INDAT(10),SFUN(10,MXNF),IFMT
      CHARACTER*8 NAMVD(MXVD),NAMINVD(MXVD)
      CHARACTER*8 NAMFLD(MXF),NFUN,NOUT,NIN1,NIN2,NAMDBZ
      CHARACTER*4 NAMOUT
      CHARACTER*3 SCANTYP
      DATA NAMOUT/'    '/
      DIMENSION DAT(MXR,MXA,MXF),AZA(MXA,2),ELA(MXA,2),NANG(2)
      DIMENSION IFLD(MXF)

      READ(INDAT,21)NFUN,NOUT,NIN1,NIN2
 21   FORMAT(/A8/A8/A8/A8)
      IFUN=IFIND(NFUN,LFUN,NUMFUN)
      IF(IFUN.EQ.0)THEN
         PRINT *,'*** FATAL ERROR - UNKNOWN FUNCTION ***'
         WRITE(6,23)(INDAT(I),I=1,10)
 23      FORMAT(1X,10A8)
         WRITE(6,25)
 25   FORMAT(/,28X,'*** KNOWN FUNCTIONS ***')
         WRITE(*,*)LFUN
         STOP
      END IF
      NF=NF+1
      WRITE(6,15)NF,(INDAT(I),I=2,10)
 15   FORMAT(1X,'FUNCT: NF=',I6,4X,9A8)
      IF(NF.GT.MXNF)THEN
         WRITE(6,31)MXNF
 31      FORMAT(1X,'*** ERROR: EXCEEDED',I4,' FUNCTIONS ***')
         STOP
      END IF
      DO 50 I=1,10
 50      SFUN(I,NF)=INDAT(I)

C     Add NOUT to the list of field names (NAMFLD) if it is not already in the 
C     list.  Increase the number of fields (NFLDS).  For NAM = 'STATS', NAMOUT 
C     contains the 1st four characters of the input field which will be used 
C     as a prefix in field names.
C
C     Initialize indices for 1st (IFL1) and 2nd (IFL2) input fields to zero,
C     then set them according to the specified function name (NFUN).
C
      IFL1=0
      IFL2=0

C     AREA is only function that doesn't have an output field name.
C
      NAMOUT=NIN2(1:4)
      IF(NFUN.NE.'AREA    ')THEN
         IF(NFUN.EQ.'GAM_DSD ')THEN
            CALL FIELD(NFUN,NAMFLD,NFLDS,NOUT)
         ELSE
           CALL FIELD(NOUT,NAMFLD,NFLDS,NAMOUT)
        END IF
      END IF
      IFL=IFIND(NOUT,NAMFLD,MXF)

C     Functions with specially-derived outputs 
C     that are only at REG range-angle locations.
C
      IF(NFUN.EQ.'SWATH   ')IFLD(IFL)=-1
      IF(NFUN.EQ.'GRID    ')IFLD(IFL)=-1
      IF(NFUN.EQ.'INTEGR  ')IFLD(IFL)=-2
      IF(NFUN.EQ.'FXSWATH ')IFLD(IFL)=-4
      IF(NFUN.EQ.'AVRAGE  ')IFLD(IFL)=-5

C     Functions requiring an output field name and the 2nd input field
C     name specifies range-angle grid (REG or original scan) to be used.
C
      IF(NFUN.EQ.'VADFLD  '.OR.
     +   NFUN.EQ.'COORD   '.OR.
     +   NFUN.EQ.'SOUND   '.OR.
     +   NFUN.EQ.'COSWIND '.OR.
     +   NFUN.EQ.'HTOPO   '.OR.
     +   NFUN.EQ.'LOBES   '.OR.
     +   NFUN.EQ.'UVDSTD  '.OR.
     +   NFUN.EQ.'NORMAL  '.OR.
     +   NFUN.EQ.'RANDOM  '.OR.
     +   NFUN.EQ.'SCANERR '.OR.
     +   NFUN.EQ.'USTOPO  ')THEN
         IFL=IFIND(NOUT,NAMFLD,MXF)
         IF(NIN2(1:3).EQ.'REG')THEN
            IFLD(IFL)=-1
         ELSE
            IFLD(IFL)=0
         END IF
      END IF

C     Functions requiring at least one input field name.
C        Exclude those that don't need a first input field,
C        either radar measured or previously defined.
C
      IF(NFUN.NE.'ANLYTIC '.AND.
     +   NFUN.NE.'COORD   '.AND.
     +   NFUN.NE.'SOUND   '.AND.
     +   NFUN.NE.'RADVEL  '.AND.
     +   NFUN.NE.'COSWIND '.AND.
     +   NFUN.NE.'HTOPO   '.AND.
     +   NFUN.NE.'LOBES   '.AND.
     +   NFUN.NE.'UVDSTD  '.AND.
     +   NFUN.NE.'NORMAL  '.AND.
     +   NFUN.NE.'RANDOM  '.AND.
     +   NFUN.NE.'SCANERR '.AND.
     +   NFUN.NE.'GRIDACT '.AND.
     +   NFUN.NE.'USTOPO  ')THEN
         CALL FIELD(NIN1,NAMFLD,NFLDS,NAMOUT)
         IFL1=IFIND(NIN1,NAMFLD,MXF)
         IFL2=0
      END IF

C     Get name of radar field to be used for gridding along aircraft track
C
      IF(NFUN.EQ.'GRIDACT '.AND. NIN2.EQ.'RADR    ')THEN
         CALL FIELD(NIN1,NAMFLD,NFLDS,NAMOUT)
         IFL1=IFIND(NIN1,NAMFLD,MXF)
      END IF

C     Get name of dBZ field to be used in VAD analysis
C
      IF(NFUN.EQ.'VAD     ')THEN
         READ(INDAT(10)(3:8),51)NAMDBZ
 51      FORMAT(A6)
         write(*,*)'namdbz=',namdbz
         IF(NAMDBZ.NE.'        ')CALL FIELD(NAMDBZ,NAMFLD,NFLDS,NAMOUT)
      END IF

      IF(NFUN.EQ.'INTEGR  '.OR.NFUN.EQ.'AVRAGE  ')THEN
         NOUT=NIN2
         CALL FIELD(NOUT,NAMFLD,NFLDS,NAMOUT)
         CALL FIELD(NIN1,NAMFLD,NFLDS,NAMOUT)
         IFL1=IFIND(NIN1,NAMFLD,MXF)
      END IF

C     Functions requiring a second input field name.
C        Exclude those that don't need a second input field.
C
      IF(NFUN.NE.'AREA    '.AND.
     +   NFUN.NE.'AVRAGE  '.AND.
     +   NFUN.NE.'INTEGR  '.AND.
     +   NFUN.NE.'CEILING '.AND.
     +   NFUN.NE.'CSUBN   '.AND.
     +   NFUN.NE.'DELETE  '.AND.
     +   NFUN.NE.'D/DR    '.AND.
     +   NFUN.NE.'D/DA    '.AND.
     +   NFUN.NE.'ETA     '.AND.
     +   NFUN.NE.'FLOOR   '.AND.
     +   NFUN.NE.'FXSWATH '.AND.
     +   NFUN.NE.'GRID    '.AND.
     +   NFUN.NE.'ISOCHRN '.AND.
     +   NFUN.NE.'LINEAR  '.AND.
     +   NFUN.NE.'LOGTEN  '.AND.
     +   NFUN.NE.'LSQRFIL '.AND.
     +   NFUN.NE.'LWC_DZ  '.AND.
     +   NFUN.NE.'VT_DBZ  '.AND.
     +   NFUN.NE.'POWER   '.AND.
     +   NFUN.NE.'TENLOG  '.AND.
     +   NFUN.NE.'RA_SHFT '.AND.
     +   NFUN.NE.'REFLECT '.AND.
     +   NFUN.NE.'R_SHFT  '.AND.
     +   NFUN.NE.'SIGPOW  '.AND.
     +   NFUN.NE.'SWATH   '.AND.
     +   NFUN.NE.'SNR     '.AND.
     +   NFUN.NE.'VAD     '.AND.
     +   NFUN.NE.'VADFLD  '.AND.
     +   NFUN.NE.'VADCOVI '.AND.
     +   NFUN.NE.'VADCOVF '.AND.
     +   NFUN.NE.'WINTGR  '.AND.
     +   NFUN.NE.'XTREMA  '.AND.
     +   NFUN.NE.'ZSLAB   '.AND.
     +   NFUN.NE.'CORRANA '.AND.
     +   NFUN.NE.'ELTOPO  '.AND.
     +   NFUN.NE.'FILTER  '.AND.
     +   NFUN.NE.'FILT2D  '.AND.
     +   NFUN.NE.'GRIDACT '.AND.
     +   NFUN.NE.'STDEV   '.AND.
     +   NFUN.NE.'ANLYTIC '.AND.
     +   NFUN.NE.'COORD   '.AND.
     +   NFUN.NE.'SOUND   '.AND.
     +   NFUN.NE.'RADVEL  '.AND.
     +   NFUN.NE.'COSWIND '.AND.
     +   NFUN.NE.'HTOPO   '.AND.
     +   NFUN.NE.'LOBES   '.AND.
     +   NFUN.NE.'UVDSTD  '.AND.
     +   NFUN.NE.'NORMAL  '.AND.
     +   NFUN.NE.'RANDOM  '.AND.
     +   NFUN.NE.'SCANERR '.AND.
     +   NFUN.NE.'USTOPO  ')THEN
         CALL FIELD(NIN2,NAMFLD,NFLDS,NAMOUT)
         IFL2=IFIND(NIN2,NAMFLD,MXF)
      END IF         

      IF(NFUN.EQ.'SWATH   '.AND. 
     +   NOUT.NE.'STATS   '.AND.
     +   NOUT.NE.'ANGLE   '.AND.
     +   NOUT.NE.'HEIGHT  ')THEN
         CALL FIELD(NIN2,NAMFLD,NFLDS,NAMOUT)
         IFL2=IFIND(NIN2,NAMFLD,MXF)
         IFLD(IFL2)=-1
      END IF

C     If both input fields use regular range-angle grid (ISW=2),
C     then the output must also use the regular range-angle grid.
C     Note: Cannot generate an output field from input fields when
C           one is located at the original sample grid points while
C           the other is located at the regular range-angle points.
C
      IF((IFL1.NE.0 .AND. IFLD(IFL1).LT.0).OR.
     +   (IFL2.NE.0 .AND. IFLD(IFL2).LT.0))THEN
         IFL=IFIND(NOUT,NAMFLD,MXF)
         IFLD(IFL)=-1
      END IF
      WRITE(6,103)NOUT,IFL,IFLD(IFL),IFL1,IFL2
 103  FORMAT(1X,'   INDX,TYPE,INDX12=',8X,A8,4I6)

      IF(NFUN.EQ.'EPSILON ')THEN
         NF=NF+1
         READ(5,105)(INDAT(I),I=1,10)
 105     FORMAT(10A8)
c         WRITE(6,107)(INDAT(I),I=1,10)
c 107     FORMAT('Kardin=',10A8)
         DO 110 I=1,10
 110        SFUN(I,NF)=INDAT(I)
      END IF
      IF(NFUN.EQ.'AREA    ')CALL INITAREA
      IF(NFUN.EQ.'SWATH   '.OR.
     +   NFUN.EQ.'ISOCHRN '.OR.
     +   NFUN.EQ.'INTEGR  '.OR.
     +   NFUN.EQ.'AVRAGE  '.OR.
     +   NFUN.EQ.'GRID    '.OR.
     +   NFUN.EQ.'FXSWATH ')THEN
         SCANTYP=INDAT(10)(1:3)
         CALL SWATH(INDAT,IFMT,NAMFLD,NFLDS,IFLD,NOUT)
         RETURN
      END IF

C  FILL VAD NAME ARRAY
C
      IF(NFUN.EQ.'VAD     '.OR.
     +   NFUN.EQ.'VADFLD  ')THEN
         MVD=MVD+1
         IF(MVD.GT.MXVD)THEN
            WRITE(6,121)MXVD
 121        FORMAT(1X,'*** WARNING: EXCEEDS',I3,' VADS ***')
            MVD=MXVD
         ELSE
            NAMVD(MVD)=NOUT
            NAMINVD(MVD)=NIN1
C            write(*,*)'savf:',namvd(mvd),naminvd(mvd),mvd
         END IF
      END IF

      RETURN
      END


