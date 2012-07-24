      SUBROUTINE FETCHZ(INUNIT,RBUF,ITEM,NPLANE,LASTLV,LASTFD,NLEV,
     X     ZLEV,NAMFLD,BAD,IHED,ILHD)
C     
C     
C     FETCHZ- ACCESSES DESIGNATED FIELDS AT A REQUESTED LEVEL
C     
C     FORMAL PARAMETERS..........
C     INUNIT - LOGICAL UNIT NUMBER OF INPUT TAPE
C     RBUF   - BUFFER TO BE FILLED WITH DATA FROM DESIGNATED FIELDS
C     ITEM   - TEMPORARY STORAGE
C     NPLANE - NUMBER OF DATA POINTS PER PLANE (EACH FIELD)
C     LASTLV - LAST LEVEL NUMBER ACCESSED IN CURRENT VOLUME
C     LASTFD - LAST FIELD NUMBER ACCESSED IN MOST RECENT LEVEL
C     NLEV   - REQUESTED LEVEL NUMBER
C     ZLEV   - HEIGHT (KM) OF REQUESTED LEVEL  (RETURNED)
C     NAMFLD - NAME OF DATA FIELD TO RETURN
C     BAD- SET UNUSABLE DATA TO BAD FOR PROCESSING
C     IHED   - 510 WORD CARTESIAN HEADER
C     ILHD   - 10 WORD LEVEL HEADER
C     Note:  For constant elevation surfaces and coplanes, the level
C            header contains information about the current level coordinate
C            and Nyquist velocity.  NEXRADs have different Nyquist velocities 
C            at different elevations.
C
C           ILHD(1) - "LE
C           ILHD(2) - "VE"
C           ILHD(3) - "L "
C           ILHD(4) - Coordinate of current level * 1000 (meters or degrees)
C           ILHD(5) - Index of current level
C           ILHD(6) - Number of fields
C           ILHD(7) - Number of grid points per plane (NX*NY)
C           ILHD(8) - Number of records per field
C           ILHD(9) - Number of records per plane
C           ILHD(10)- Nyquist velocity * 100
C     
C      FILETYP is called to set the data file format flag (ICDF)
C
C                CEDRIC input file format         CEDRIC.INC flags
C      ICDF: (1) pure binary                      set ICDF = CEDPURE
C            (2) revised netCDF                   set ICDF = CDFFMT
C            (3) USWRP NEXRAD gridded data        set ICDF = USWRPGD
C            (4) original RAP-generated MDV data  set ICDF = MDVFMT
C            (5) old netCDF                       set ICDF = OLDCEDRIC
C     
      INCLUDE 'CEDRIC.INC'
      COMMON /IOTYPE/ ICDF
      COMMON /CDFNET/ ICDFID(MXCDF),IDIMDAT(4,MXCDF),IDIMID(1,MXCDF),
     X     IVAR(NFMAX+1,MXCDF),IFILE,ISYNFLG,ICDUNT(MXCDF),IUSWRP
      COMMON /LEVELS/ VALLEV(MAXZLEV),VALNYQ(MAXZLEV),VNYQ_VOL
      COMMON /FMTTYPE/ WRFFLG
      INTEGER WRFFLG,NUMFLD,MDVNX,MDVNY


      COMMON /AXUNTS/ IUNAXS,LABAXS(3,3),SCLAXS(3,3),AXNAM(3)
      CHARACTER*4 AXNAM
      COMMON /CREAXS/ CSPN(3,3),NCXN(3),NPLNEW
      DIMENSION RBUF(NPLANE),ITEM(NPLANE),IHED(NID),ILHD(10)
      CHARACTER*2 NAMFLD(4)
      INTEGER CVMGP,SCOUNT
      DATA IZIPAK /32768/

C     LJM - May need to change ZSCALE to 100.0 for some SPRINT files
C           that used GRIDPPI and elevation angle > 32.768 deg.
C
      DATA ZSCALE /1000.0/

      NST=0

C     It is yet to be determined why FILETYP is being called here.  The
C     call to FILETYP has been in the code since the 970416 version or
C     earlier.  See /filbert3/ljmill/Cedric-old/Cedric-970416/FETCHZ.f
C     LJM 03/05/2003 
C
c-----print *,'FETCHZ-before filetyp: inunit,icdf,nst=',inunit,icdf,nst
      print *,'FETCHZ: level = ',nlev,'  Field=',namfld
      CALL FILETYP(INUNIT,ICDF,NST)
c-----print *,'FETCHZ- after filetyp: inunit,icdf,nst=',inunit,icdf,nst

C     FILETYP returns ICDF=2 (netCDF) even when the input is MDV. Patch
C     here with temporary reassignment to prevent misleading error
C              +++ERROR ACCESSING UNIT IN CEDCDF+++
C     03/05/2003:
C     This problem has now been fixed in CIN.c (void filetyp).  FILETYP
C     returns ICDF=4, not ICDF=2, when the input data file format is MDV.

c      if(icdf.eq.2)icdf=4
c      print *,'        inunit,icdf,cdffmt,nst=',inunit,icdf,cdffmt,nst
c
      IF (NST.NE.0) GOTO 204
C     
C     LOCATE POSITION OF REQUESTED FIELD
C     
      NUMFLD=LOCFLDID(NAMFLD,IHED(176),5,IHED(175),4)
      IF(NUMFLD.LE.0)GO TO 204
      SCALE=1.0/IHED(180+(NUMFLD-1)*5)

      IF (ICDF.EQ.0) THEN
C
C     FILE IS COS BLOCKED BINARY
C

C
C        CALCULATE HEIGHT (KM) OF REQUESTED LEVEL
C
         ZLEV=(IHED(170) + (NLEV-1)*IHED(173)) * 0.001
C
C        POSITION TAPE TO CORRECT LEVEL
C
         IF (NLEV.EQ.LASTLV) GOTO 10
         IF (LASTLV.EQ.0) LASTFD=IHED(175)
         NSKIP=(IHED(175)-LASTFD)*IHED(96)
         CALL SKPREC(INUNIT,NSKIP)
         LASTLV=LASTLV+1
         LASTFD=0
         NSKIP=(IHED(97)+1)*(NLEV-LASTLV)
         CALL SKPREC(INUNIT,NSKIP)
C
C        INITIALIZATION PROCEDURE FOR THIS LEVEL
C
         CALL PLANST(INUNIT,NLEV,ILHD,NST,ICDF)
         IF(NST.NE.0)GO TO 203
         IF (NPLNEW.EQ.0) THEN
            IF (AXNAM(3).NE.'E') THEN
               VALLEV(NLEV)=ZLEV
               VALNYQ(NLEV)=VNYQ_VOL
            ELSE
C     IF CONSTANT SCAN SURFACES, SET LEVEL VALUE and NYQUIST VELOCITY
C     FROM LEVEL HEADER
               VALLEV(NLEV)=ILHD(4)/ZSCALE
               VALNYQ(NLEV)=ILHD(10)/100.

            END IF
            print *,'   FETCHZ: lev,nyq=',
     +           nlev,vallev(nlev),valnyq(nlev)
         END IF
C
C        READ IN DESIGNATED FIELD
C
 10      CONTINUE
         SCALE=1.0/IHED(180+(NUMFLD-1)*5)
         ISKP=(NUMFLD-LASTFD-1)*IHED(96)
         CALL SKPREC(INUNIT,ISKP)
         CALL DATAIN(INUNIT,RBUF,NPLANE,ITEM,BAD,SCALE,NST)
         IF(NST.NE.0)GO TO 204
         LASTFD=NUMFLD
         LASTLV=NLEV
         RETURN

      ELSE IF (ICDF.EQ.1) THEN
C
C     PROCESS PURE FORMAT FILES
C
C     
C     CALCULATE HEIGHT (KM) OF REQUESTED LEVEL
C     
         ZLEV=(IHED(170) + (NLEV-1)*IHED(173)) * 0.001
C     
C     POSITION TAPE TO CORRECT LEVEL AND FIELD
C     
         NSKIP=0
         IF (NLEV.EQ.LASTLV) GOTO 20
         IF (LASTLV.EQ.0) LASTFD=IHED(175)
         NSKIP=(IHED(175)-LASTFD)*NPLANE*2
         IF (NSKIP.GT.0) CALL CSKPREC(INUNIT,NSKIP)
         LASTLV=LASTLV+1
         LASTFD=0
C
C     THE 20 IN THE FOLLOWING STATEMENT IS TO SKIP OVER LEVEL HEADERS 
C
         NSKIP=(NLEV-LASTLV)*(IHED(175)*NPLANE*2 + 20)
         IF (NSKIP.NE.0)  CALL CSKPREC(INUNIT,NSKIP)
         CALL PLANST(INUNIT,NLEV,ILHD,NST,ICDF)
         IF (NST.NE.0) GOTO 203
         IF (NPLNEW.EQ.0) THEN
            IF (AXNAM(3).NE.'E') THEN
               VALLEV(NLEV)=ZLEV
            ELSE
               VALLEV(NLEV)=ILHD(4)/ZSCALE
               VALNYQ(NLEV)=ILHD(10)/100.
            END IF
            print *,'   FETCHZ: lev,nyq=',
     +           nlev,vallev(nlev),valnyq(nlev)
         END IF
 20      CONTINUE
         NSKIP = (NUMFLD-LASTFD-1)*NPLANE*2
         SCALE=1.0/IHED(180+(NUMFLD-1)*5)
c     IF (NSKIP.GT.0)
c     X     WRITE(*,*)'***NLEV,LASTLV,NUMFLD,NSKIP,SCALE,IHED(175),
c     X     NPLANE,LASTFD=',NLEV,LASTLV,NUMFLD,NSKIP,SCALE,IHED(175),
c     X     NPLANE,LASTFD
         IF (NSKIP.NE.0) CALL CSKPREC(INUNIT,NSKIP)
         CALL CFETCHZ(INUNIT,RBUF,NPLANE,ITEM,BAD,SCALE,NST,NSKIP)
         CALL GBYTES(RBUF,ITEM,0,16,0,NPLANE)
         DO 15 I=1,NPLANE
            RBUF(I)=BAD
            IF (ITEM(I).EQ.IZIPAK) GOTO 15
            IX=ITEM(I)
            ITEM(I)=CVMGP(IX-65536,IX,IX-32768)
            RBUF(I)=ITEM(I)*SCALE
 15      CONTINUE
         LASTFD = NUMFLD
         LASTLV = NLEV
         RETURN

      ELSE IF(ICDF.EQ.CDFFMT) THEN
C
C     PROCESS NETCDF (CEDRIC- OR WRF-GENERATED) FILES
C
         IF(WRFFLG .EQ. 0) THEN
 520        CALL FTCHCDF(RBUF,NLEV,NUMFLD,BAD,SCALE,IHED(162),
     X           IHED(167),NPLANE,INUNIT)
         ENDIF
         IF(WRFFLG .EQ. 1) THEN  
c            print *,'FETCHZ: WRF variable=',nlev,namfld,numfld,IHED(172)
            CALL FTCHWRF(NLEV,NUMFLD,IHED(162),IHED(167),
     X           IHED(172),IHED,NAMFLD)
         ENDIF
         RETURN
         
      ELSE IF(ICDF.EQ.MDVFMT) THEN
C
C     PROCESS MDV FILES
C
         call gmdvnx(mdvnx,mdvny)
         CALL FTCHMDV(RBUF,NLEV,NUMFLD,IHED(162),IHED(167),
     x                MDVNX,MDVNY,INUNIT)
         RETURN
      END IF
C     
C     ERROR EXITS
C     
 203  CONTINUE
      CALL TAPMES(INUNIT,NST)
 403  FORMAT(///5X,'FETCHZ.'/15X,'UNABLE TO LOCATE DATA AT LEVEL: ',
     X     I2,5X,'HEIGHT: ',F5.1,' KM'/////)
      CALL CEDERX(544,1)
      CALL FLUSH_STDOUT
 204  CONTINUE
      CALL TAPMES(INUNIT,NST)
      PRINT 404, NAMFLD,NLEV,ZLEV
 404  FORMAT(///5X,'FETCHZ..'/15X,'UNABLE TO LOCATE FIELD :   ',A10,
     X     10X,'LEVEL: ',I2,5X,'HEIGHT: ',F5.1,' KM'/////)
      CALL CEDERX(545,1)
      CALL FLUSH_STDOUT
      END
