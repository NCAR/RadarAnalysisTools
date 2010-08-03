      SUBROUTINE INTERP(KRD,TFIELD,TLIMITS,NTHRSH,ISIDE)
C
C        READS THE CARD CONTAINING INFORMATION DEFINING THE
C        FIELDS TO BE INTERPOLATED AND THE METHODS TO BE EMPLOYED.
C
C
C  CARD 1:
C
C   VARIABLE   DESCRIPTION                  FIELD   NOTES
C   --------   -----------                  -----   -----
C
C   KOMM       'INT'                         P1     COMMAND
C   METH       INTERPOLATION METHOD          P2     B - BILINEAR (DEFAULT)
C                                                   C - CLOSEST POINT
C   NGAVG      NUMBER OF GATES TO AVERAGE    P3     MUST BE >= 0
C                 ALONG RANGE
C   MING       MINIMUM NUMBER OF GOOD POINTS P4
C                 TO ACCEPT ALONG RANGE
C   DMIN       MAXIMUM ACCEPTABLE DISTANCE   P5     IN KM
C                 TO RELOCATE A CLOSEST
C                 POINT ESTIMATE
C   RNGC1      VARY NUMBER TO AVG LINEARLY   P6
C   RNGC0      INITIAL NUMBER GATES TO AVG   P7
C              # Gates = RNGC1*Range + RNGC0
C   MINDEC     MIN # GATES FOR RANGE-VARYING P8
C              AVERAGING.
C
C  CARD 2: (AS MANY ADDITIONAL CARDS AS THERE ARE FIELDS)
C
C               ALL FIELD CARDS EXCEPT TIME, AZ, or EL
C               --- ----- ----- ------ ----- --- -- --
C
C   THRESH     THRESHOLD ON THIS FIELD       P6
C
C   LLIMIT     LOWER LIMIT FOR THRESHOLDING  P7 
C
C   ULIMIT     UPPER LIMIT FOR THRESHOLDING  P8
C
C   SIDE       IN/OUT SIDE THRESHOLD LIMITS  P9
C
C               POWER FIELD CARD
C               ----- ----- ----
C
C   FLDNAM     NAME OF FIELD                 P2     4 CHARACTERS LONG
C   ITRANS     TRANSFORMATION CRITERIA;      P3
C                 'LI'                              TO LINEAR UNITS
C                 'NO'                              NO CONVERSION (DEFAULT)
C   IDERIV                                   P4
C              'TRA'                                TRANSFORM TO DBZ
C              'CRE'                                CREATE A DBZ FIELD
C              'NON'                                NO DBZ (DEFAULT)
C   NAMDBZ     NAME OF THE DBZ FIELD         P5     MUST BE SPECIFIED FOR
C                                                      IDERIV.EQ.'TRA' OR
C                                                      IDERIV.EQ.'CRE'
C               DBZ FIELD CARD
C               --- ----- ----
C
C   FLDNAM     NAME OF FIELD                 P2     4 CHARACTERS LONG
C   ITRANS     TRANSFORMATION CRITERIA;      P3
C                 'LI'                              TO LINEAR UNITS
C                 'NO'                              NO CONVERSION (DEFAULT)
C
C               VELOCITY FIELD CARD
C               -------- ----- ----
C
C   FLDNAM     NAME OF FIELD                 P2     4 CHARACTERS LONG
C   IVLFLG                                   P3
C              'MISSING'                            SET ALL NCAR-FLAGGED
C                                                      BAD VELOCITIES TO
C                                                      -32768, FOR DISCARDING
C              'NO'                                 LEAVE THE VELOCITIES
C                                                      ALONE (DEFAULT)
C   IUNFLD                                   P4
C              'UNFOLD'                             ONLY ALLOWED TO UNFOLD
C                                                      ONE VELOCITY FIELD
C              'NO'                                 DO NOT UNFOLD
C
C               ALL OTHER FIELD CARDS
C               --- ----- ----- -----
C
C   FLDNAM     NAME OF FIELD                 P2     4 CHARACTERS LONG
C
      INCLUDE 'SPRINT.INC'
c      PARAMETER (MAXFLD=16)
      LOGICAL LUFLAG, IS360
     
      COMMON /CFLD/NFLDS
      COMMON /CFLDC/ IFIELD(MAXFLD), INTINF(MAXFLD,3)
      CHARACTER*8 INTINF,IFIELD

      COMMON/ADJUST/INFLD(MAXFLD,3),SCLIF(MAXFLD,2),NIF,IOFLD(MAXFLD,3),
     X     SCLOF(MAXFLD,2),NOF,SCLAZ,SCLRNG,SCLEL,UNSCAZ,UNSCRG,UNSCEL,
     X     LOWAZ,IZAD,IS360,MINAZ,MAXAZ,METSPC(MAXFLD),IWFLD(MAXFLD),
     X     NFLI,SCLNYQ,UNSNYQ
      COMMON /FNAMES/ CINFLD(MAXFLD),CIOFLD(MAXFLD)
      CHARACTER*8 CINFLD, CIOFLD
      CHARACTER MGMESS*12,MCGMES*30,MCGMIN*21
      DIMENSION GCOEF(2),TLIMITS(2,MAXFLD)
      DIMENSION ISIDE(MAXFLD)
      CHARACTER KPRNT(2)*13
      CHARACTER*1 LKTBL(2),METH,IBL,IUNFLD
      CHARACTER*2 ITRANS,IVLFLG
      CHARACTER*3 KOMSTK,IDERIV
      CHARACTER*8 TFIELD(2,MAXFLD),CFIELD,CTEMP1,CTEMP2,NAMDBZ
      CHARACTER*8 KRD(10),SIDE,ISWICH(2),IACTIV,NAMTIM
      DATA LKTBL/'C','B'/
      DATA KPRNT/'CLOSEST POINT','BILINEAR'/
      DATA NAMTIM/'TIME'/
      DATA ISWICH/'ACTIVE','INACTIVE'/
      DATA IBL/' '/
      CTEMP1= ' '
      CTEMP2= ' '
      ICNT=0
C
      DO 5 I=1,MAXFLD
         METSPC(I)=0
    5 CONTINUE
C
C        DECODE METHOD INFORMATION
C
      READ (KRD,101)METH,TMPAVG,TMING,DMIN,GCOEF,GCMIN
 101  FORMAT(/A1,7X/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0)
C
      NGAVG = TMPAVG
      MING = TMING
C
      NGAVG = MAX0(NGAVG,0)
      DMIN = AMAX1(DMIN,0.0)
C
      MGMESS = 'GATE SPACING'
C
      IF (DMIN.GT.0.0) THEN
         WRITE (MGMESS,111)DMIN
111      FORMAT(F7.2,' KM  ')
      END IF
C
C
C   SET TO BILINEAR AS DEFAULT
C
      IF (METH.EQ.'C') THEN
         METSPC(1) = 2
         METSPC(2) = 0
         METSPC(3) = 0
      ELSE
         METSPC(1) = 1
C
      IF(NGAVG.LE.0) THEN
         METSPC(2)=0
         METSPC(3)=0
      ELSE
         METSPC(2)=NGAVG
         MING=MAX0(MING,1)
         METSPC(3)=MIN0(MING,NGAVG)
         IF(GCOEF(1).EQ.0.0) GCOEF(2)=0.0
         METSPC(7)=GCOEF(1)*1000.0
         METSPC(8)=GCOEF(2)*1000.0
         METSPC(9)=AMAX1(GCMIN,1.0)
      END IF
      END IF
      METSPC(6)=DMIN*UNSCRG
      PRINT 885
885   FORMAT(//5X,'SUMMARY OF INTERP COMMAND ')
      PRINT 887
887   FORMAT(5X,'------- -- ------ ------- ')
C
      K = LOCATEC(METH,LKTBL,2)
      MCGMES='NOT BEING USED'
      MCGMIN=MCGMES
      IF(GCOEF(1).NE.0.0) THEN
         WRITE (MCGMES,886)GCOEF
  886    FORMAT(F8.3,' * RANGE (KM) ',SP,F8.3,S)
         WRITE (MCGMIN,884)METSPC(9)
  884    FORMAT(I8,' FEWER',7X)
      END IF
      PRINT 888, KPRNT(K),MCGMES,MCGMIN,NGAVG,MING,MGMESS
888   FORMAT(/8X,'            INTERPOLATION METHOD: ',A13
     X      /8X,'COMPUTED NO. OF GATES TO AVERAGE: ',A
     X      /8X,'                      TO  ACCEPT: ',A
     X      /8X,' NOMINAL NO. OF GATES TO AVERAGE: ',I4
     X      /8X,'                      TO  ACCEPT: ',I4
     X       /8X,' MAX DISTANCE TO RELOCATE POINTS: ',A12/)
      PRINT 998
998   FORMAT(/20X,'FIELD CARDS ')
      PRINT 999
999   FORMAT(20X,'----- ----- '/)
C
C        READ IN FIELDS
C
      I=0
      LUFLAG = .FALSE.
C
221   CONTINUE
      CALL KARDIN(KRD)
C
      READ (KRD,222)KOMSTK
222   FORMAT(A3,5X)
C
      IF (KOMSTK.EQ.'END') GO TO 900
C
C   ELSE, DECODE THE FIELD CARD FOR THE FIELD NAME, AND SAVE
C     THE INITIAL INFO CONTAINED IN ALL THE OTHER FIELDS
C
C
C   TEST FOR MORE THAN MAXFLD FIELD CARDS:
C
      I = I+1
      IF (I.GT.MAXFLD) THEN
         PRINT 113,MAXFLD
113      FORMAT(/1X,'+++  ERROR, MAXIMUM NUMBER OF FIELDS ',
     X              'IS ',I3,' +++')
         STOP
      END IF
C
      READ (KRD,223)IFIELD(I)
 223  FORMAT(/A8)
C
C   CHECK FOR SAME FIELD BEING SPECIFIED TWICE
C
      IF (I.GT.1) THEN
      IM1 = I-1
      DO 25 J = 1,IM1
         IF (IFIELD(I).EQ.IFIELD(J)) THEN
            PRINT 333, IFIELD(J)
333         FORMAT(/1X,'+++  ERROR, THE FIELD ',A4,' HAS BEEN ',
     X                'SPECIFIED TWICE  +++')
            STOP
         END IF
25    CONTINUE
      END IF
C
      PRINT 555, I,IFIELD(I)
555   FORMAT(/20X,'FIELD # ',I2,': ',A8)
C
C   CHECK THE FIELD TYPE IN ORDER TO DECODE THE CARDS DIFFERENTLY
C     FOR THE DIFFERENT FIELDS. NEED TO INVOKE THE FUNCTION ITPFLDC.
C
      KFLDTP = ITPFLDC(IFIELD(I))
      INTINF(I,1) = IBL
      INTINF(I,2) = IBL
      INTINF(I,3) = IBL

      IF (KFLDTP.EQ.0) THEN
C
C     Undetermined field type
C
         PRINT 224, IFIELD(I)
224      FORMAT(/1X,'+++  WARNING - UNABLE TO DETERMINE FIELD TYPE ',
     X        'FOR FIELD = ',A8,' +++')
         READ (KRD,700)CFIELD,TLLIMIT,TULIMIT,SIDE
 700     FORMAT(/////A8/F8.2/F8.2/A8)
         IF (CFIELD.NE.' ') THEN
C
C     WILL BE THRESHOLDING THIS FIELD; GRAB THE PERTINENT INFO
C     
            IF (TULIMIT.LT.TLLIMIT) THEN
               WRITE(*,713)
 713           FORMAT(/,5X,'+++LOWER THRESHOLD LIMIT CANNOT BE ',
     X                'GREATER THAN UPPER THRESHOLD LIMIT+++')
               STOP
            END IF
            ICNT=ICNT+1
            WRITE(TFIELD(1,ICNT),710)IFIELD(I)
 710        FORMAT(A8)
            IF (SIDE.EQ.'OUTSIDE') THEN
               ISID=2
               PRINT 723, CFIELD,TLLIMIT,TULIMIT
 723           FORMAT(/,23X,'THIS FIELD WILL BE SET TO BAD IF ',A8,
     X                ' GREATER THAN OR EQUAL TO ',F8.2,' AND LESS ',
     X                'THAN OR EQUAL TO ',F8.2)
            ELSE
               ISID=1
               PRINT 715, CFIELD,TLLIMIT,TULIMIT
 715           FORMAT(/,23X,'THIS FIELD WILL BE SET TO BAD IF ',A8,
     X                ' LESS THAN ',F8.2,' OR GREATER THAN ',F8.2)
            END IF
            ISIDE(ICNT)=ISID
            IF (CFIELD.EQ.CTEMP1 .OR. CFIELD.EQ.CTEMP2) THEN
               TFIELD(2,ICNT)=CFIELD
               TLIMITS(1,ICNT)=TLLIMIT
               TLIMITS(2,ICNT)=TULIMIT
            ELSE IF (CTEMP1.EQ.' ') THEN
               CTEMP1=CFIELD
               TFIELD(2,ICNT)=CFIELD
               TLIMITS(1,ICNT)=TLLIMIT
               TLIMITS(2,ICNT)=TULIMIT
            ELSE IF (CTEMP2.EQ.' ') THEN
               CTEMP2=CFIELD
               TFIELD(2,ICNT)=CFIELD
               TLIMITS(1,ICNT)=TLLIMIT
               TLIMITS(2,ICNT)=TULIMIT
            ELSE 
               PRINT 720
 720           FORMAT(/5X,'+++ ONLY 2 THRESHOLD FIELDS ALLOWED +++')
               STOP 
            END IF
         END IF
            
      ELSE IF (KFLDTP.EQ.1) THEN
C
C     Power field (dBM)
C
         READ (KRD,225)ITRANS,IDERIV,NAMDBZ,CFIELD,TLLIMIT,TULIMIT,SIDE
 225     FORMAT(//A2,6X/A3,5X/A8/A8/F8.2/F8.2/A8)
         IF (CFIELD.NE.' ') THEN
C
C     WILL BE THRESHOLDING THIS FIELD
C     
            IF (TULIMIT.LT.TLLIMIT) THEN
               WRITE(*,713)
               STOP
            END IF
            ICNT=ICNT+1
            WRITE(TFIELD(1,ICNT),710)IFIELD(I)
            IF (SIDE.EQ.'OUTSIDE') THEN
               ISID=2
               PRINT 723, CFIELD,TLLIMIT,TULIMIT
            ELSE
               ISID=1
               PRINT 715, CFIELD,TLLIMIT,TULIMIT
            END IF
            ISIDE(ICNT)=ISID
            IF (CFIELD.EQ.CTEMP1 .OR. CFIELD.EQ.CTEMP2) THEN
               TFIELD(2,ICNT)=CFIELD
               TLIMITS(1,ICNT)=TLLIMIT
               TLIMITS(2,ICNT)=TULIMIT
            ELSE IF (CTEMP1.EQ.' ') THEN
               CTEMP1=CFIELD
               TFIELD(2,ICNT)=CFIELD
               TLIMITS(1,ICNT)=TLLIMIT
               TLIMITS(2,ICNT)=TULIMIT
            ELSE IF (CTEMP2.EQ.' ') THEN
               CTEMP2=CFIELD
               TFIELD(2,ICNT)=CFIELD
               TLIMITS(1,ICNT)=TLLIMIT
               TLIMITS(2,ICNT)=TULIMIT
            ELSE 
               PRINT 720
               STOP 
            END IF
         END IF
C
C   TEST THE POWER FIELD CARD:
C
         IF (ITRANS.EQ.'LI') THEN
            INTINF(I,1) = 'L'
            IACTIV = ISWICH(1)
         ELSE
            ITRANS = 'NO'
            IACTIV = ISWICH(2)
         END IF
         PRINT 230, IACTIV
230      FORMAT(23X,'TRANSFORMATION TO LINEAR UNITS: ',A8)
C
         IF (IDERIV.EQ.'TRA') THEN
            INTINF(I,2) = NAMDBZ
            INTINF(I,3) = 'D'
            PRINT 330
330         FORMAT(23X,'THE DBM FIELD WILL BE TRANSFORMED TO DBZ. ')
         ELSE IF (IDERIV.EQ.'CRE') THEN
            INTINF(I,2) = NAMDBZ
            PRINT 331
331         FORMAT(23X,'A DBZ FIELD WILL BE CREATED. ')
         ELSE
            IDERIV = 'NON'
            PRINT 332
332         FORMAT(23X,'NO DBZ FIELD WILL BE CREATED. ')
         END IF
C
C   PRINT AN ERROR MESSAGE IF THEY DIDN'T SPECIFY THE NAME OF
C     THE DBZ. ONLY FOR THE CASES OTHER THAN 'NONE'.
C
         IF (IDERIV.NE.'NON') THEN
            IF (NAMDBZ.EQ.' ') THEN
               PRINT 334
334            FORMAT(/1X,'+++  ERROR, DBZ FIELD NAME MUST ',
     X                   'BE SUPPLIED  +++')
               STOP
            END IF
            PRINT 335, NAMDBZ
335         FORMAT(23X,'DBZ FIELD NAME: ',A8)
         END IF
         PRINT 592

      ELSE IF (KFLDTP.EQ.2) THEN
C
C     Reflectivity field (dBZ) - may be derived from a power field
C
         READ (KRD,226)ITRANS,CFIELD,TLLIMIT,TULIMIT,SIDE
 226     FORMAT(//A2,6X///A8/F8.2/F8.2/A8)
         IF (CFIELD.NE.' ') THEN
C
C     WILL BE THRESHOLDING THIS FIELD
C     
            IF (TULIMIT.LT.TLLIMIT) THEN
               WRITE(*,713)
               STOP
            END IF
            ICNT=ICNT+1
            WRITE(TFIELD(1,ICNT),710)IFIELD(I)
            IF (SIDE.EQ.'OUTSIDE') THEN
               ISID=2
               PRINT 723, CFIELD,TLLIMIT,TULIMIT
            ELSE
               ISID=1
               PRINT 715, CFIELD,TLLIMIT,TULIMIT
            END IF
            ISIDE(ICNT)=ISID
            IF (CFIELD.EQ.CTEMP1 .OR. CFIELD.EQ.CTEMP2) THEN
               TFIELD(2,ICNT)=CFIELD
               TLIMITS(1,ICNT)=TLLIMIT
               TLIMITS(2,ICNT)=TULIMIT
            ELSE IF (CTEMP1.EQ.' ') THEN
               CTEMP1=CFIELD
               TFIELD(2,ICNT)=CFIELD
               TLIMITS(1,ICNT)=TLLIMIT
               TLIMITS(2,ICNT)=TULIMIT
            ELSE IF (CTEMP2.EQ.' ') THEN
               CTEMP2=CFIELD
               TFIELD(2,ICNT)=CFIELD
               TLIMITS(1,ICNT)=TLLIMIT
               TLIMITS(2,ICNT)=TULIMIT
            ELSE 
               PRINT 720
               STOP 
            END IF
         END IF
C
C   TEST THE DBZ FIELD CARD:
C
         IF (ITRANS.EQ.'LI') THEN
            INTINF(I,1) = 'L'
            IACTIV = ISWICH(1)
         ELSE
            ITRANS = 'NO'
            IACTIV = ISWICH(2)
         END IF
         PRINT 230, IACTIV
         PRINT 592

      ELSE IF (KFLDTP.EQ.3) THEN
C
C     Velocity field - may be unfolded with generation of a QUAL field
C
         READ (KRD,227)IVLFLG,IUNFLD,CFIELD,TLLIMIT,TULIMIT,SIDE
 227     FORMAT(//A2/A1//A8/F8.2/F8.2/A8)
         IF (CFIELD.NE.' ') THEN
C
C     WILL BE THRESHOLDING THIS FIELD
C     
            IF (TULIMIT.LT.TLLIMIT) THEN
               WRITE(*,713)
               STOP
            END IF
            ICNT=ICNT+1
            WRITE(TFIELD(1,ICNT),710)IFIELD(I)
            IF (SIDE.EQ.'OUTSIDE') THEN
               ISID=2
               PRINT 723, CFIELD,TLLIMIT,TULIMIT
            ELSE
               ISID=1
               PRINT 715, CFIELD,TLLIMIT,TULIMIT
            END IF
            ISIDE(ICNT)=ISID
            IF (CFIELD.EQ.CTEMP1 .OR. CFIELD.EQ.CTEMP2) THEN
               TFIELD(2,ICNT)=CFIELD
               TLIMITS(1,ICNT)=TLLIMIT
               TLIMITS(2,ICNT)=TULIMIT
            ELSE IF (CTEMP1.EQ.' ') THEN
               CTEMP1=CFIELD
               TFIELD(2,ICNT)=CFIELD
               TLIMITS(1,ICNT)=TLLIMIT
               TLIMITS(2,ICNT)=TULIMIT
            ELSE IF (CTEMP2.EQ.' ') THEN
               CTEMP2=CFIELD
               TFIELD(2,ICNT)=CFIELD
               TLIMITS(1,ICNT)=TLLIMIT
               TLIMITS(2,ICNT)=TULIMIT
            ELSE 
               PRINT 720
               STOP 
            END IF
         END IF
C
C   TEST THE VELOCITY FIELD CARD:
C
         IF (IVLFLG.EQ.'MI') THEN
            INTINF(I,2) = 'D'
            IACTIV = ISWICH(1)
         ELSE
            IVLFLG = 'NO'
            IACTIV = ISWICH(2)
         END IF
         PRINT 232, IACTIV
232      FORMAT(23X,'DISCARDING OF ALL NCAR-FLAGGED BAD VELOCITIES: ',
     X          A8)
C
         IACTIV = ISWICH(2)
         IF(IUNFLD.EQ.'U'.OR.IUNFLD.EQ.'Q') THEN
            PRINT 234
  234       FORMAT(23X,'A CORRESPONDING QUAL FIELD WILL BE CREATED. ')
            IF (LUFLAG) THEN
               PRINT 444
444            FORMAT(/1X,'+++  ONLY ONE QUAL FIELD MAY BE GENERATED',
     X                    '  +++'/)
               STOP
            END IF
            IF(IUNFLD.EQ.'U') IACTIV=ISWICH(1)
            INTINF(I,1)=IUNFLD
            LUFLAG = .TRUE.
         END IF
         PRINT 233, IACTIV
 233     FORMAT(23X,'UNFOLDING OF THIS VELOCITY FIELD: ',A8)
         PRINT 592
C
      ELSE IF (KFLDTP.EQ.4.OR.KFLDTP.EQ.5.OR.KFLDTP.EQ.6.OR.KFLDTP.EQ.7)
C
C     Spectral width, normalized coherent power, time and geometry fields
C
     X        THEN
         PRINT 592
C
      END IF
C
C
      GO TO 221
C
900   CONTINUE
      NFLDS = I
      PRINT 592
592   FORMAT(1X)
      L = LOCATEC(NAMTIM,IFIELD,NFLDS)
      IF (L.GT.0.AND.L.LT.NFLDS) THEN
C
C   PUT TIME FIELD AT END OF LIST:
C
         CALL SWAPEMC(IFIELD(L),IFIELD(NFLDS))
         CALL SWAPEMC(INTINF(L,1),INTINF(NFLDS,1))
         CALL SWAPEMC(INTINF(L,2),INTINF(NFLDS,2))
         CALL SWAPEMC(INTINF(L,3),INTINF(NFLDS,3))
C
      END IF
C
      NTHRSH=ICNT
      RETURN
      END
