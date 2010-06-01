      SUBROUTINE REMAP(KRD,IBUF,RBUF,LCMB2,IPR,NST,IFLAT,
     X                 RTOP,RBOT,ROUT,ITEMP,LATLON)
C     
C     REMAPS AN EXISTING COORDINATE SYSTEM INTO A NEW USER DEFINED
C     COORDINATE SYSTEM. USERS CAN REMAP FROM COPLANE TO CARTESIAN 
C     OR FROM CARTESIAN TO CARTESIAN 
C     OR FROM CONSTANT ELEVATION TO CARTESIAN.
C
C     Checks user-specified CTEMP1 against houskeeping words 16 and 17
C        which can be ('CRT','COPL','ELEV','LLE ', or 'LLZ ').
C
C-----Coordinate transformations are specified by ICORD flag.
C     ICORD: (0) CRT (XYZ) --> CRT (XYZ)
C            (1) COP (XYC) --> CRT (XYZ)
C            (2) ELE (XYE) --> CRT (XYZ)
C            (3) LLE       --> LLZ
C            (4) ELE (XYE) --> LLE
C            (5) CRT (XYZ) --> LLZ
C            (6) LLE       --> ELE (XYE)
C            (7) LLZ       --> CRT (XYZ)
C        Note: Elevation angles can be unequally spaced.  Uses values
C              (VALLEV) from the level headers rather than equal spaced.
C              Cartesian and Coplane assume equally-spaced z- and c-levels.
C     
C                MIN(I=1)   MAX(I=2)   DEL(I=3)
C     X (J=1):   CSP(1,1)   CSP(2,1)   CSP(3,1)
C     Y (J=2):   CSP(1,2)   CSP(2,2)   CSP(3,2)
C     Z (J=3):   CSP(1,3)   CSP(2,3)   CSP(3,3)
C
      INCLUDE 'CEDRIC.INC'
C
C     MAXMEMORY = 255 * 255 * 35 * 25 = MAXX/2 * MAXY/2 * MXCRT * NFMAX 
C
      PARAMETER(MAXMEMORY = (MAXX*MAXY/4)*MXCRT*NFMAX)

      DIMENSION IBUF(MAXPLN),RBUF(MAXPLN,6),ISPEC(NFMAX)
      CHARACTER*(*) KRD(10)
      DIMENSION CSPN(3,3),NCXN(3),MAXAX(3),LCMB2(1),RTOP(1),RBOT(1)
      DIMENSION ROUT(1),ITEMP(1)
      COMMON /AXUNTS/ IUNAXS,LABAXS(3,3),SCLAXS(3,3),AXNAM(3)
      CHARACTER*4 AXNAM
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X     IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X     NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF

C     Aircraft track and surface network locations
C
      PARAMETER (MXL=20000)
      COMMON /AIRTRCK/ XACT(MXL),YACT(MXL),ZACT(MXL),BEGACT,DELACT,
     X                 NACT,DTAC,UACT(MXL),VACT(MXL),WACT(MXL),IACTWND
      PARAMETER(MXK=1000,MXNET=20)
      COMMON /STALOC/ XSTA(MXK),YSTA(MXK),ZSTA(MXK),IMRK

      COMMON /LEVELS/ VALLEV(MAXZLEV),VALNYQ(MAXZLEV),VNYQ_VOL
      CHARACTER IXOR*12
      CHARACTER*1 CIBL
      CHARACTER*2 CTEMP
      CHARACTER*8 CTEMP1,CTEMP2,CTEMP3,NAMIN(3),NAMOUT(3)
      CHARACTER*8 CTEMP4
      CHARACTER*3 XYUNITS,ZUNITS
      INTEGER  CERROR,CORDCANG
      LOGICAL  LATLON
      EQUIVALENCE (NCX(1),NX), (NCX(2),NY), (NCX(3),NZ)
      DATA MAXAX/0,0,MAXZLEV/
      DATA CIBL/' '/
      DATA CTEMP1/'????????'/
      DATA CTEMP3/'????????'/
      DATA CTEMP4/'        '/
      DATA EPS/0.01/
      DATA DE/12751.273/
      DATA CERROR/0/

      print *,'REMAP - maxx/2,maxy2= ',maxx/2,maxy/2
      print *,'        mxcrt,nfmax = ',mxcrt,nfmax
      print *,'          maxmemory = ',maxmemory
      SF=1./FLOAT(ID(68))
      CF=1./FLOAT(ID(69))
      ATR=ATAN(1.)/45.
      NF=ID(175)
      VNYQ=0.0
C
C     DETERMINE MEMORY USE FOR REMAP
C
      IF (INMEM.EQ.0) THEN
         MEMUSE=0
      ELSE IF (INMEM.EQ.1 .AND. WORDSZ.EQ.32) THEN
         MEMUSE=1
      ELSE IF (INMEM.EQ.1 .AND. WORDSZ.EQ.64) THEN
         MEMUSE=2
      ELSE
         WRITE(*,12)WORDSZ
 12      FORMAT(/,5X,'+++ UNSUPPORTED WORDSZ IN REMAP. WORDSZ=',I8)
         CALL FLUSH_STDOUT         
      END IF
C     
C     PARSE THE INPUT CARD FOR OPTIONS AND SPECIFICATIONS
C     
      READ(KRD,100)CTEMP1,ANGXAX,XORG,YORG,ZORG,CTEMP2,RELMAX,CTEMP3
 100  FORMAT(/A8/F8.2/F8.2/F8.2/F8.2/A8/F8.2/A8)
      print *,'"REMAP:p2-10=',ctemp1,angxax,xorg,yorg,zorg,
     +     ctemp2,relmax,ctemp3
C
C     CHECK THE INPUT COORDINATE SYSTEM ENTERED ON THE REMAP CARD 
C     COMMAND WITH WHATS IN THE CEDRIC ID WORDS 16 -17.
C
      write(ctemp4,610)id(16)
      print *,'REMAP-check: parm(2),id(16)=',ctemp1,ctemp4
      write(ctemp4,610)id(17)
      print *,'REMAP-check: parm(2),id(17)=',ctemp1,ctemp4
      print *,'REMAP-check: ',icord,cerror,ctemp3,cordcang
      CALL REMAPCRD(CTEMP1,ID(16),ID(17),ICORD,
     X                CERROR,CTEMP3,CORDCANG)
      cerror=0
      print *,'REMAP-check after REMAPCRD: ',ctemp1,id(16),id(17),
     +     icord,cerror,ctemp3,cordcang

      IF(CERROR .NE. 0) THEN
         CALL CEDERX(CERROR,1)
      ENDIF

      IROT=0
      ANGUSR=ID(40)*CF
      IF (ANGXAX.EQ.0.0)THEN
         ANGXAX=ANGUSR
      ELSE IF (ANGXAX.LE.0.0 .OR. ANGXAX.GE.180.0) THEN
         CALL CEDERX(558,1)
         RETURN
      ELSE
         ANGR=AMOD(360.+ANGUSR-ANGXAX,360.)
         IF (ABS(ANGR).GE.EPS) IROT=1
      END IF
      
      THETA=(ANGXAX-ANGUSR)*ATR

C     ROTATE AIRCRAFT WINDS AND LOCATION, IF PRESENT
C
      IF (NACT.GT.0) THEN
         DO I=1,NACT
            XACT(I)=XACT(I)-XORG
            YACT(I)=YACT(I)-YORG
            ZACT(I)=ZACT(I)-ZORG
            XN=XACT(I)*COS(THETA) - YACT(I)*SIN(THETA)
            YN=XACT(I)*SIN(THETA) + YACT(I)*COS(THETA)
            XACT(I)=XN
            YACT(I)=YN
         END DO
         IF (IACTWND.EQ.1 .OR. IACTWND.EQ.4) THEN
            DO I=1,NACT
               UN=UACT(I)*COS(THETA) - VACT(I)*SIN(THETA)
               VN=UACT(I)*SIN(THETA) + VACT(I)*COS(THETA)
               UACT(I)=UN
               VACT(I)=VN
            END DO
         END IF
      END IF

C     ROTATE STATION LOCATIONS, IF NEEDED
C
      IF (IMRK.GT.0) THEN
         DO I=1,IMRK
            XSTA(I)=XSTA(I)-XORG
            YSTA(I)=YSTA(I)-YORG
            ZSTA(I)=ZSTA(I)-ZORG
            UN=XSTA(I)*COS(THETA) - YSTA(I)*SIN(THETA)
            VN=XSTA(I)*SIN(THETA) + YSTA(I)*COS(THETA)
            XSTA(I)=UN
            YSTA(I)=VN
         END DO
      END IF
C     
C     IF NEW GRID WILL BE SPECIFIED, READ IN THAT CARD
C     
      IF (CTEMP2.EQ.'NEWGRID') THEN
         INWGRD=1
         CALL KARDIN(KRD)
         READ(KRD,130)((CSPN(I,J),I=1,3),J=1,3)
 130     FORMAT(/F8.2/F8.2/F8.2/F8.2/F8.2/F8.2/F8.2/F8.2/F8.2)

C     Check if user specified X: Min(1,1), Max(2,1), and Del(3,1)
C     If not (all are 0), set NEWGRID values to OLDGRID values.
C
         IF(CSPN(1,1) .EQ. 0.0 .AND. 
     X      CSPN(2,1) .EQ. 0.0 .AND. 
     X      CSPN(3,1).EQ.0.0)THEN
            DO I=1,3
               CSPN(I,1)=CSP(I,1)
            END DO
         END IF

C     Check if user specified Y: Min(1,2), Max(2,2), and Del(3,2)
C     If not (all are 0), set NEWGRID values to OLDGRID values.
C
         IF(CSPN(1,2) .EQ. 0.0 .AND. 
     X      CSPN(2,2) .EQ. 0.0 .AND. 
     X      CSPN(3,2).EQ.0.0)THEN
            DO I=1,3
               CSPN(I,2)=CSP(I,2)
            END DO
         END IF

C     Check if user specified Z: Min(1,3), Max(2,3), and Del(3,3)
C     If not (all are 0), set NEWGRID values to OLDGRID values.
C
         IF(CSPN(1,3) .EQ. 0.0 .AND. 
     X      CSPN(2,3) .EQ. 0.0 .AND. 
     X      CSPN(3,3).EQ.0.0)THEN
            DO I=1,3
               CSPN(I,3)=CSP(I,3)
            END DO
         END IF

         DO 30 J=1,3
C     
C     CHECK GRID SPECIFICATION
C     
            TEST=CSPN(3,J)
            IF(TEST.LT.0.0) THEN
               DIV=1.0
               CSPN(3,J) = -0.001
            ELSE IF(TEST.EQ.0.0) THEN
               DIV=1.0
            ELSE
               DIV=1./CSPN(3,J)
            END IF
     
            RNUM=ABS((CSPN(2,J)-CSPN(1,J))*DIV)+1.001
            NCXN(J)=RNUM
            IF(ABS(FLOAT(NCXN(J))-RNUM).GT.0.01) THEN
               WRITE(IPR,205) AXNAM(J)
 205           FORMAT(5X,'+++  ',A2,'AXIS MUST CONTAIN AN INTEGRAL',
     X              ' NUMBER OF POINTS  +++')
               CALL CEDERX(503,1)
               RETURN
            END IF
            IF(NCXN(J).EQ.1.AND.CSPN(3,J).GT.0.0) CSPN(3,J)=0.0
            IF(NCXN(J).NE.1.AND.CSPN(3,J).LE.0.0) THEN
               WRITE(IPR,201) AXNAM(J)
 201           FORMAT(5X,'+++  ',A2,'AXIS SPACING MUST BE POSITIVE',  
     X              '+++')
               CALL CEDERX(503,1)
               RETURN
            END IF
            IF(J.EQ.3.AND.(NCXN(J).LE.0.OR.NCXN(J).GT.MAXAX(J))) THEN
               WRITE(IPR,202) AXNAM(J),MAXAX(J)
 202           FORMAT(5X,'+++  NUMBER OF POINTS ALONG THE  ',A2,
     X              'AXIS MUST BE IN THE RANGE OF  1 TO ',I3,'  +++')
               CALL CEDERX(503,1)
               RETURN
            END IF
 30      CONTINUE
        
         IF ((NCXN(1)*NCXN(2)*NCXN(3)*NF).GE. MAXMEMORY) THEN
            WRITE(*,33)
 33         FORMAT(5X,'+++YOUR GRID SIZE AND NUMBER OF FIELDS ',
     X             'REQUESTED REQUIRE TOO MUCH MEMORY.+++',/,5X,
     X             '+++ PLEASE RESIZE YOUR REQUEST.+++')
            RETURN
         END IF

         NPLNEW=NCXN(1)*NCXN(2)
         IF(NPLNEW.LE.0.OR.NPLNEW.GT.MAXPLN) THEN
            WRITE(IPR,203) MAXPLN
 203        FORMAT(5X,'+++  NUMBER OF POINTS/PLANE MUST BE IN THE ',
     X           'RANGE OF  1 TO ',I5,'  +++')
            CALL CEDERX(503,1)
            RETURN
         END IF
      ELSE
         INWGRD=0
         NPLNEW=NCX(1)*NCX(2)
         DO 50 J=1,3
            NCXN(J)=NCX(J)
            DO 55 I=1,3
               CSPN(I,J)=CSP(I,J)
 55         CONTINUE
 50      CONTINUE
      END IF
C
C     SEE IF ONLY A 2-D INTERPOLATION IS BEING DONE (X-Y PLANES)
C
      ZDO=CSP(3,3)
      ZDN=CSPN(3,3)
      Z1O=CSP(1,3)
      Z1N=CSPN(1,3)

      IF (ZDO.NE.0.0) THEN
         ITEST1=INT((ZDN/ZDO) + .0001)
         ITEST2=NINT(ZDN/ZDO)
         ITEST3=INT((Z1N-Z1O)/ZDO + .0001)
         ITEST4=NINT((Z1N-Z1O)/ZDO)
         IF (ITEST1.EQ.ITEST2 .AND. ITEST3.EQ.ITEST4) THEN
            IDIM2=1
         ELSE
            IDIM2=0
         END IF
      ELSE
         IF (Z1N.EQ.Z1O) THEN
            IDIM2=1
         ELSE
            WRITE(*,70)
 70         FORMAT(/,5X,'+++INVALID OUTPUT Z SPECIFICATION IN REMAP+++'
     X           )
            RETURN
         END IF
      END IF
      print *,'REMAP: idim2 (0=>3d), (1=>2d)=',idim2
      print *,'REMAP: xorg,yorg,zorg==',xorg,yorg,zorg
      
      IINT=0
      IF (XORG.NE.0.0 .OR. YORG.NE.0.0 .OR. ZORG.NE.0.0) THEN
C     
C     A TRANSLATION OF DATA WILL BE REQUIRED; CHECK IF INTEGRAL SHIFT
C     
         ITRANS=1
         IF (ICORD.EQ.0 .AND. IROT.EQ.0 .AND. INWGRD.EQ.0) THEN
            IF (CSP(3,1).NE.0.0) THEN
               XSHFT=XORG/CSP(3,1)
               IXSHFT=NINT(XSHFT)
               DXSH=ABS(XSHFT-IXSHFT)
            ELSE
               DXSH=0.0
            END IF
            
            IF (CSP(3,2).NE.0.0) THEN
               YSHFT=YORG/CSP(3,2)
               IYSHFT=NINT(YSHFT)
               DYSH=ABS(YSHFT-IYSHFT)
            ELSE
               DYSH=0.0
            END IF
            
            IF (CSP(3,3).NE.0.0) THEN
               ZSHFT=ZORG/CSP(3,3)
               IZSHFT=NINT(ZSHFT)
               DZSH=ABS(ZSHFT-IZSHFT)
            ELSE
               DZSH=0.0
            END IF
            
            IF(DXSH.LE.EPS .AND. DYSH.LE.EPS .AND. DZSH.LE.EPS) IINT=1
         END IF
      ELSE
         ITRANS=0
      END IF
 182  CONTINUE
C     
C     SUMMARIZE REMAP REQUEST
C     
      WRITE(IPR,161)
 161  FORMAT(/' EDIT FILE PRIOR TO REMAPPING ...')
      CALL IMHSUM(IPR,ID)
      IXOR='  UNCHANGED'
      IF (IROT.EQ.1) WRITE(IXOR,172)ANGXAX
 172  FORMAT(F6.2,'  DEG.')
      WRITE(IPR,104) IXOR,XORG,YORG,ZORG,ID(16),ID(17)
 104  FORMAT(//3X,'REMAPPING SPECIFICATIONS FOR THE NEW ',
     X     ' COORDINATE SYSTEM ...'/
     X     8X,'1-  X-AXIS ORIENTATION REL. TO NORTH:',3X,A12/
     X     8X,'2-  POSITION OF THE NEW ORIGIN: (',F7.2,',',
     X     F7.2,',',F7.2,')'/
     X     8X,'3-  ORIGINAL COORDINATE SYSTEM:',3X,A2,A2/
     X     3X,'ALL FIELDS WILL BE REMAPPED.')
         
      IF (ITRANS.EQ.1) THEN
         WRITE(IPR,107)
 107     FORMAT(3X,'COORDINATE SYSTEM WILL BE TRANSLATED.')
      ELSE
         WRITE(IPR,117)
 117     FORMAT(3X,'COORDINATE SYSTEM WILL NOT BE TRANSLATED.')
      END IF
      IF (IROT.EQ.1) THEN
         WRITE(IPR,108)
 108     FORMAT(3X,'COORDINATE SYSTEM WILL BE ROTATED ABOUT Z-AXIS.')
      ELSE
         WRITE(IPR,118)
 118     FORMAT(3X,'COORDINATE SYSTEM WILL NOT BE ROTATED ABOUT Z.')
      END IF

      IF (IINT.EQ.1) THEN
         WRITE(IPR,109)
 109     FORMAT(3X,'COORD. SYSTEM WILL BE SHIFTED, NO INTERPOLATION ',
     X        'WILL BE DONE.'/)
      ELSE
         WRITE(IPR,119)
 119     FORMAT(3X,'BILINEAR/CLOSEST POINT INTERPOLATION ',
     X        'WILL BE DONE.'/)
      END IF
C     
C     IF NEW GRID, WRITE OUT NEW INFO
C     
      IF (CORDCANG .EQ. 1) THEN
         print *,'Remap: axnam,ctemp3=',axnam,ctemp3
         CALL UPDATE_AXIS_NAMES(AXNAM,CTEMP3,XYUNITS,ZUNITS)
         READ(XYUNITS,110)LABAXS(1,1)
         READ(XYUNITS,110)LABAXS(2,1)
         READ(ZUNITS,110)LABAXS(3,1)
 110     FORMAT(A3)          
      END IF

      IF (INWGRD.EQ.1) THEN
         IF(ICORD.EQ.3  .OR. ICORD.EQ.4 .OR. ICORD.EQ.5)THEN
            LATLON = .TRUE.
            WRITE(IPR,111)
 111        FORMAT(/'  NEW LON LAT COORDINATE SYSTEM SPECIFICATIONS...'
     X        /3X,'AXIS',5X,'MINIMUM (DEG)',5X,'MAXIMUM (DEG)',6X,
     X        'DELTA(DEG)',4X,'NO. OF PTS.')
         ELSE IF(ICORD.EQ.1 .OR. ICORD.EQ.2 .OR. ICORD.EQ.7)THEN
            LATLON = .FALSE.
            AXNAM(3)='Z'
            WRITE(IPR,112)
 112      FORMAT(/'  NEW CARTESIAN COORDINATE SYSTEM SPECIFICATIONS...'
     X        /3X,'AXIS',5X,'MINIMUM (KM)',5X,'MAXIMUM (KM)',6X,
     X        'DELTA (KM)',4X,'NO. OF PTS.')
         ELSE IF(ICORD.EQ.6)THEN
            LATLON = .FALSE.
            AXNAM(3)='E'
            WRITE(IPR,113)
 113      FORMAT(/'  NEW CARTESIAN COORDINATE SYSTEM SPECIFICATIONS...'
     X        /3X,'AXIS',5X,'MINIMUM (KM)',5X,'MAXIMUM (KM)',6X,
     X        'DELTA(DEG)',4X,'NO. OF PTS.')

         END IF         

         DO 25 I=1,3
            WRITE(IPR,114) AXNAM(I),CSPN(1,I),CSPN(2,I),CSPN(3,I),
     X                     NCXN(I)
 114        FORMAT(5X,A1,6X,F10.3,7X,F10.3,8X,F8.3,7X,I5)
 25      CONTINUE
      END IF
C
C     CHECK FOR SPECIAL PROCESSING (UNFOLDING, LINEAR TRANS, OR VECTOR ROT.)
C
      DO 590 I=1,NFMAX
         ISPEC(I)=0
 590  CONTINUE

 570  CALL KARDIN(KRD)
      IF (KRD(1).EQ.'END') GOTO 170
      IF (KRD(2).EQ.'LINEAR') THEN
C
C     WANT TO TRANSFORM FIELD FROM LOG10 TO LIN FOR INTERP. THEN BACK TO LOG
C
         IF (KRD(3).EQ.' ') THEN
            WRITE(*,560)
 560        FORMAT(/,5X,'+++CANNOT HAVE BLANK ENTRY FOR FIELD ',
     X              'NAME+++')
            RETURN
         END IF
C
C     LOCATE FIELD NUMBER AND TURN ON SPEC FLAG FOR THAT NUMBER
C
         DO 134 I=1,NFMAX
            WRITE(CTEMP1,137)(NAMF(J,I),J=1,4)
 137        FORMAT(4A2)
            IF (CTEMP1.EQ.KRD(3)) THEN
               J=MAPVID(I,1)
               ISPEC(J)=1
               WRITE(*,145)KRD(3)
 145           FORMAT(5X,A8,' WILL BE TRANSFORMED TO LINEAR UNITS',
     X                ' BEFORE INTERPOLATION AND BACK TO LOG10 AFTER')
               GOTO 150
            END IF
 134     CONTINUE
C
C     COULDN'T LOCATE FIELD IN LIST; PRINT ERROR AND EXIT
C
         WRITE(*,157)KRD(3)
 157     FORMAT(/,5X,'++COULDN''T FIND ',A8, 'FOR LINEAR ',
     X        'TRANSFORMATION.+++')
         RETURN
      ELSE IF (KRD(2).EQ.'UNFOLD') THEN
C
C     WANT TO DO LOCAL UNFOLDING OF FIELD BEFORE INTERPOLATION
C
         IF (KRD(3).EQ.' ') THEN
            WRITE(*,550)
 550        FORMAT(/,5X,'+++CANNOT HAVE BLANK FIELD FOR VELOCITY ',
     X              'NAME+++')
            RETURN
         END IF
C
C     LOCATE FIELD NUMBER AND TURN ON SPEC FLAG FOR THAT NUMBER
C
         DO 500 I=1,NFMAX
            WRITE(CTEMP1,137)(NAMF(J,I),J=1,4)
            IF (CTEMP1.EQ.KRD(3)) THEN
C
C     LOCATED FIELD
C
               J=MAPVID(I,1)
               ISPEC(J)=2
               WRITE(*,520)KRD(3)
 520           FORMAT(5X,A8,' WILL BE UNFOLDED LOCALLY BEFORE ',
     X              'INTERPOLATION')
               IF (KRD(4).EQ.' ' .AND. ID(303).EQ.1) THEN
C
C     USE NYQUIST VELOCITY IN ID HEADER
C     
                  VNYQ=FLOAT(ID(304))/FLOAT(ID(68))
                  WRITE(*,525)VNYQ
 525              FORMAT(5X,'NYQUIST VELOCITY=',F8.2)
               ELSE
C
C     USE USER-SUPPLIED NYQUIST VELOCITY
C
                  WRITE(CTEMP1,610)KRD(4)
 610              FORMAT(A8)
                  READ(CTEMP1,620)VNYQ
 620              FORMAT(F8.2)
                  WRITE(*,525)VNYQ
               END IF
C
C     CHECK TO SEE IF VNYQ IS ZERO
C
               IF (VNYQ.EQ.0.0) THEN
                  WRITE(*,622)
 622              FORMAT('ERROR: NYQUIST VELOCITY ZERO; CANNOT UNFOLD')
                  CALL DIVZERO(1.0)
               END IF
               GOTO 150
            END IF
 500     CONTINUE
         WRITE(*,530)KRD(3)
 530     FORMAT(/,5X,'+++COULDN''T FIND ',A8,' FOR UNFOLDING+++')
         RETURN
      ELSE IF (KRD(2).EQ.'ROTATE') THEN
C
C     ROTATE A VECTOR
C
         NAMIN(1)=KRD(3)
         NAMIN(2)=KRD(4)
         NAMIN(3)=KRD(5)
         NAMOUT(1)=KRD(6)
         NAMOUT(2)=KRD(7)
         NAMOUT(3)=KRD(8)
C
C     CHECK FOR ANY UNSPECIFIED FIELD NAMES
C
         IF (ICORD.EQ.2 .OR. ICORD.EQ.3 .OR. 
     X       ICORD.EQ.4 .OR. ICORD.EQ.6)THEN
            WRITE(*,535)
 535        FORMAT(/,5X,'+++CANNOT ROTATE VECTORS FOR CONSTANT '
     X            ,'ELEVATION COORDINATE+++')
         ELSE IF ((ICORD.EQ.1) .AND. 
     X            (NAMIN(1).EQ.' ' .OR. NAMIN(2).EQ.' ' 
     X        .OR. NAMIN(3).EQ.' ' .OR. NAMOUT(1).EQ.' ' 
     X        .OR. NAMOUT(2).EQ.' ' .OR. NAMOUT(3).EQ.' ')) THEN
            WRITE(*,540)
 540        FORMAT(/,5X,'+++CANNOT HAVE BLANK FIELD FOR VECTOR ',
     X            'COMPONENT NAME+++')
            RETURN
         ELSE IF ((ICORD.EQ.0) .AND. (NAMIN(1).EQ.' ' .OR. 
     X           NAMIN(2).EQ.' ' .OR. NAMOUT(1).EQ.' ' .OR.
     X           NAMOUT(2).EQ.' ')) THEN
            WRITE(*,540)
            RETURN
         END IF
         CALL ROVECT(ITRANS,IROT,NCX(1),NCX(2),NCX(3),CSPN,
     X        RBUF(1,1),RBUF(1,3),ANGXAX,ANGUSR,IBUF,ICORD,NAMIN,
     X        NAMOUT,ISTAT)
         IF (ISTAT.LT.0) RETURN
      ELSE
         WRITE(*,600)KRD(2)
 600     FORMAT(/,5X,'+++ERROR: UNRECOGNIZED REMAP SUBCOMMAND:',A8)
         RETURN
      END IF
 150  CONTINUE
C
C     GO AND TRY AND READ ANOTHER CARD
C
      GOTO 570
 170  CONTINUE

C
C     WE ARE NOW READY TO DO THE INTERPOLATION 
C
C-----ICORD: (0) CRT (XYZ) --> CRT (XYZ)
C            (1) COP (XYC) --> CRT (XYZ)
C            (2) ELE (XYE) --> CRT (XYZ)
C            (3) LLE       --> LLZ
C            (4) ELE (XYE) --> LLE
C            (5) CRT (XYZ) --> LLZ
C            (6) LLE       --> ELE (XYE)
C            (7) LLZ       --> CRT (XYZ)

      if(icord.eq.0)print *,'REMAP (0) CRT (XYZ) --> CRT (XYZ)'
      if(icord.eq.1)print *,'REMAP (1) COP (XYC) --> CRT (XYZ)'
      if(icord.eq.2)print *,'REMAP (2) ELE (XYE) --> CRT (XYZ)'
      if(icord.eq.3)print *,'REMAP (3) LLE       --> LLZ'
      if(icord.eq.4)print *,'REMAP (4) ELE (XYE) --> LLE'
      if(icord.eq.5)print *,'REMAP (5) CRT (XYZ) --> LLZ'
      if(icord.eq.6)print *,'REMAP (6) LLE       --> ELE (XYE)'
      if(icord.eq.7)print *,'REMAP (7) LLZ       --> CRT (XYZ)'
      print *,'REMAP: iint=',iint

      IF (ICORD.EQ.0) THEN

C-----Three-dimensional interpolation
C     ICORD: (0) CRT (XYZ) --> CRT (XYZ)
C
         IF (IINT.EQ.0) THEN

C           Old grid locations .ne. New grid locations

            CALL CRINTRP(IROT,ITRANS,INWGRD,XORG,YORG,
     X           ZORG,CSPN,NCXN,NPLNEW,ANGXAX,ANGUSR,RBUF(1,1),
     X           RBUF(1,4),RELMAX,LCMB2,ISPEC,VNYQ,RTOP,RBOT,ROUT,
     X           ITEMP,IBUF,IDIM2,MEMUSE,LATLON,DE,ICORD)
         ELSE
     
C           Old grid is integral shift of New grid
     
            CALL SHIFTIT(XORG,YORG,ZORG,RBUF(1,1),IBUF,RBUF(1,2),
     X           IXSHFT,IYSHFT,IZSHFT,NCX(1),NCX(2))
         END IF

      ELSE IF (ICORD.EQ.1 .OR. ICORD.EQ.2 .OR. ICORD.EQ.3) THEN

C-----One-dimensional interpolation in z-direction between elev or coplanes
C     ICORD: (1) COP (XYC) --> CRT (XYZ)
C            (2) ELE (XYE) --> CRT (XYZ)
C            (3) LLE       --> LLZ

         CALL COINTRP(IROT,ITRANS,INWGRD,XORG,YORG,ZORG,CSPN,NCXN,
     X        NPLNEW,ANGXAX,ANGUSR,RBUF(1,1),RBUF(1,4),RELMAX,LCMB2,
     X        ISPEC,VNYQ,IFLAT,RTOP,RBOT,ROUT,ITEMP,IBUF,MEMUSE,
     X        ICORD,LATLON,DE)
         
      ELSE IF(ICORD.EQ.4 .OR. ICORD.EQ.5 .OR. 
     X        ICORD.EQ.6 .OR. ICORD.EQ.7) THEN

C-----Two-dimensional interpolation in quasi-horizontal planes
C     Vertical coordinate (elev or height) remains same from input --> output
C     ICORD: (4) ELE (XYE) --> LLE
C            (5) CRT (XYZ) --> LLZ
C            (6) LLE       --> ELE (XYE)
C            (7) LLZ       --> CRT (XYZ)

         CALL LLINTRP(IROT,ITRANS,INWGRD,XORG,YORG,
     X        ZORG,CSPN,NCXN,NPLNEW,ANGXAX,ANGUSR,RBUF(1,1),
     X        RBUF(1,4),RELMAX,LCMB2,ISPEC,VNYQ,RTOP,RBOT,ROUT,
     X        ITEMP,IBUF,IDIM2,MEMUSE,ICORD,DE)            
      END IF
C     
C     NOW THAT INTERP IS DONE, UPDATE ALL PERTINENT ID WORDS
C     Find necessary scaling factor for new coordinates
C
      XN1=CSPN(1,1)
      XN2=CSPN(2,1)
      YN1=CSPN(1,2)
      YN2=CSPN(2,2)
      QMAXCD=AMAX1(ABS(XN1),ABS(YN1),ABS(XN2),ABS(YN2))
      IF (QMAXCD .LT. 327.67) SF=100.
      IF (QMAXCD .GE. 327.67) SF=10.
      IF (QMAXCD .GE. 3276.7) SF=1.
      ID(68)=NINT(SF)
      ID(69)=64
      PRINT *,'  REMAP: GENERAL SCALING FACTOR =',ID(68)
      PRINT *,'  REMAP: ANGLE   SCALING FACTOR =',ID(69)
      
      IF (ICORD .GT. 0) THEN
C
C     UPDATE CEDRIC HEADER WORDS (ID #16 and #17)
C
         IF(ICORD.EQ.1 .OR. ICORD.EQ.2 .OR. ICORD.EQ.7) THEN
            CTEMP='CR'
            READ(CTEMP,13)ID(16)
            CTEMP='T '
            READ(CTEMP,13)ID(17)
            CTEMP='KM'
            READ(CTEMP,13)LABAXS(3,1)
 13         FORMAT(A2)
            AXNAM(3)='Z'
            IF (ITRANS.EQ.1) THEN
C
C     CALCULATE NEW ORIGIN LAT AND LON
C
               ORLAT=ID(33)+ID(34)/60.0+ID(35)/(3600.0 * ID(68))
               ORLON=ID(36)+ID(37)/60.0+ID(38)/(3600.0 * ID(68))
               CALL XY2LLDRV(PLAT,PLON,XORG,YORG,ORLAT,ORLON,
     X              ANGUSR)
               ID(33)=INT(PLAT)
               ID(34)=INT((PLAT-ID(33))*60.0)
               ID(35)=NINT(((PLAT-ID(33))*60.0 - ID(34))*60.0*ID(68))
               ID(36)=INT(PLON)
               ID(37)=INT((PLON-ID(36))*60.0)
               ID(38)=NINT(((PLON-ID(36))*60.0 - ID(37))*60.0*ID(68))
            END IF
         END IF

         IF(ICORD .EQ. 3) THEN
            CTEMP = 'LL'
            READ(CTEMP,13)ID(16)
            CTEMP = 'Z '
            READ(CTEMP,13)ID(17)
         ELSE IF(ICORD .EQ. 4) THEN
            CTEMP = 'LL'
            READ(CTEMP,13)ID(16)
            CTEMP = 'E '
            READ(CTEMP,13)ID(17)
         ELSE IF(ICORD .EQ. 5) THEN
            CTEMP = 'LL'
            READ(CTEMP,13)ID(16)
            CTEMP = 'Z '
            READ(CTEMP,13)ID(17)
         ELSE IF(ICORD .EQ. 6) THEN
            CTEMP = 'EL'
            READ(CTEMP,13)ID(16)
            CTEMP = 'EV'
            READ(CTEMP,13)ID(17)
         END IF
         ID(39)=ID(39)+ZORG*1000
         ID(40)=NINT(ANGXAX/CF)
         ID(301)=NPLNEW
         ID(96)=(ID(301)-1)/ID(65) + 1
         ID(97)=ID(96)*ID(175)
         ID(106)=NCXN(3)
         ID(98)=ID(97)*ID(106)
         ID(99)=ID(98)+ID(106)+1
         ID(100)=ID(98)+1
         ID(158)=(ANGXAX-90.0)/CF
         
         DO 225 I=1,2
            J=(I-1)*5
            ID(160+J)=CSPN(1,I)/SF
            ID(161+J)=CSPN(2,I)/SF
            ID(162+J)=NCXN(I)
            ID(163+J)=CSPN(3,I)*1000.
 225     CONTINUE
         ID(170)=CSPN(1,3)*1000.
         ID(171)=CSPN(2,3)*1000.
         ID(172)=NCXN(3)
         ID(173)=CSPN(3,3)*1000.
         ID(311)=ID(311) + ZORG*1000.
C     
C     FIGURE OUT THE RADAR COORDINATES IN THE NEW COORD. SYSTEM
C     
         DO 250 I=1,ID(303)
            J=(I-1)*6
            XRAD=ID(315+J)*SF
            YRAD=ID(316+J)*SF
            ZRAD=ID(317+J)/1000.
C     
C     ROTATE TO BASELINE ANGLE COORD. SYSTEM
C     
C            BASANG=(ANGUSR-90)*ATR
C            XRAD1=XRAD*COS(BASANG) - YRAD*SIN(BASANG)
C            YRAD1=XRAD*SIN(BASANG) + YRAD*COS(BASANG)
C     
C     TRANSLATE 
C     
            XRAD=XRAD-XORG
            YRAD=YRAD-YORG
            ZRAD=ZRAD -ZORG
C     
C     ROTATE TO NEW USER SPECIFIED ANGXAX
C     
            THETA=(ANGXAX-ANGUSR)*ATR
            XRADN=XRAD*COS(THETA) - YRAD*SIN(THETA)
            YRADN=XRAD*SIN(THETA) + YRAD*COS(THETA)
            
            ID(315+J)=XRADN/SF
            ID(316+J)=YRADN/SF
            ID(317+J)=ZRAD*1000.
 250     CONTINUE
C     
C     CHANGE OTHER ELEMENTS OF COMMON BLOCK 'VOLUME'
C     
         DO 275 I=1,3
            NCX(I)=NCXN(I)
            DO 300 J=1,3
               CSP(I,J)=CSPN(I,J)
 300        CONTINUE
 275     CONTINUE
         DO I=1,NCX(3)
            VALLEV(I)=CSP(1,3) + (I-1)*CSP(3,3)
         END DO
         NPLANE=NPLNEW
         CALL UPDHED(0)
         
         WRITE(IPR,276)
 276     FORMAT(/'SUMMARY OF FINAL COORDINATE SYSTEM...'/)
         CALL IMHSUM(IPR,ID)
      ELSE IF (ICORD.EQ.0) THEN
         IF (IINT.EQ.0) THEN
            IF (ITRANS.EQ.1) THEN
C
C     CALCULATE NEW ORIGIN LAT AND LON
C
               ORLAT=ID(33)+ID(34)/60.0+ID(35)/(3600.0 * ID(68))
               ORLON=ID(36)+ID(37)/60.0+ID(38)/(3600.0 * ID(68))
               CALL XY2LLDRV(PLAT,PLON,XORG,YORG,ORLAT,ORLON,
     X              ANGUSR)
               ID(33)=INT(PLAT)
               ID(34)=INT((PLAT-ID(33))*60.0)
               ID(35)=NINT(((PLAT-ID(33))*60.0 - ID(34))*60.0*ID(68))
               ID(36)=INT(PLON)
               ID(37)=INT((PLON-ID(36))*60.0)
               ID(38)=NINT(((PLON-ID(36))*60.0 - ID(37))*60.0*ID(68))
            END IF
            ID(39)=ID(39)+ZORG*1000
            ID(40)=NINT(ANGXAX/CF)
            ID(301)=NPLNEW
            ID(96)=(ID(301)-1)/ID(65) + 1
            ID(97)=ID(96)*ID(175)
            ID(106)=NCXN(3)
            ID(98)=ID(97)*ID(106)
            ID(99)=ID(98)+ID(106)+1
            ID(100)=ID(98)+1
            ID(158)=(ANGXAX-90.0)/CF
            
            DO 226 I=1,2
               J=(I-1)*5
               ID(160+J)=CSPN(1,I)/SF
               ID(161+J)=CSPN(2,I)/SF
               ID(162+J)=NCXN(I)
               ID(163+J)=CSPN(3,I)*1000.
 226        CONTINUE
            ID(170)=CSPN(1,3)*1000.
            ID(171)=CSPN(2,3)*1000.
            ID(172)=NCXN(3)
            ID(173)=CSPN(3,3)*1000.
            ID(311)=ID(311) + ZORG*1000.
C     
C     FIGURE OUT THE RADAR COORDINATES IN THE NEW COORD. SYSTEM
C     
            DO 255 I=1,ID(303)
               J=(I-1)*6
               XRAD=ID(315+J)*SF
               YRAD=ID(316+J)*SF
               ZRAD=ID(317+J)/1000.
C     
C     TRANSLATE 
C     
               XRAD=XRAD -XORG
               YRAD=YRAD -YORG
               ZRAD=ZRAD -ZORG
C     
C     ROTATE TO NEW USER SPECIFIED ANGXAX
C     
               THETA=(ANGXAX-ANGUSR)*ATR
               XRADN=XRAD*COS(THETA) - YRAD*SIN(THETA)
               YRADN=XRAD*SIN(THETA) + YRAD*COS(THETA)
               
               ID(315+J)=XRADN/SF
               ID(316+J)=YRADN/SF
               ID(317+J)=ZRAD*1000.
 255        CONTINUE
C     
C     CHANGE OTHER ELEMENTS OF COMMON BLOCK 'VOLUME'
C     
            DO 280 I=1,3
               NCX(I)=NCXN(I)
               DO 305 J=1,3
                  CSP(I,J)=CSPN(I,J)
 305           CONTINUE
 280        CONTINUE
            DO I=1,NCX(3)
               VALLEV(I)=CSP(1,3) + (I-1)*CSP(3,3)
            END DO
            
            NPLANE=NPLNEW
            CALL UPDHED(0)
            
            WRITE(IPR,231)
 231        FORMAT(/'SUMMARY OF FINAL COORDINATE SYSTEM...'/)
            CALL IMHSUM(IPR,ID)
         ELSE IF (IINT.EQ.1) THEN
            IF (ITRANS.EQ.1) THEN
C
C     CALCULATE NEW ORIGIN LAT AND LON
C
               ORLAT=ID(33)+ID(34)/60.0+ID(35)/(3600.0 * ID(68))
               ORLON=ID(36)+ID(37)/60.0+ID(38)/(3600.0 * ID(68))
               CALL XY2LLDRV(PLAT,PLON,XORG,YORG,ORLAT,ORLON,
     X              ANGUSR)
               ID(33)=INT(PLAT)
               ID(34)=INT((PLAT-ID(33))*60.0)
               ID(35)=NINT(((PLAT-ID(33))*60.0 - ID(34))*60.0*ID(68))
               ID(36)=INT(PLON)
               ID(37)=INT((PLON-ID(36))*60.0)
               ID(38)=NINT(((PLON-ID(36))*60.0 - ID(37))*60.0*ID(68))      
            END IF
            ID(39)=ID(39)+ZORG*1000
C     
C     FIGURE OUT THE RADAR COORDINATES IN THE NEW COORD. SYSTEM
C     
            DO 257 I=1,ID(303)
               J=(I-1)*6
               XRAD=ID(315+J)*SF
               YRAD=ID(316+J)*SF
               ZRAD=ID(317+J)/1000.
C     
C     TRANSLATE 
C     
               XRAD=XRAD -XORG
               YRAD=YRAD -YORG
               ZRAD=ZRAD -ZORG
               
               ID(315+J)=XRAD/SF
               ID(316+J)=YRAD/SF
               ID(317+J)=ZRAD*1000.
 257        CONTINUE
            CALL UPDHED(0)
            CALL IMHSUM(IPR,ID)
            
         END IF
      END IF
C
C     RESET THE WINDOW NOW
C
      CALL WINDIJ(KRD,1)
         
      RETURN
      END


