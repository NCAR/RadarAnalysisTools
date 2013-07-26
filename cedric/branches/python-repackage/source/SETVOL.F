      SUBROUTINE SETVOL(INUNIT,NAMVOL,IBTIM,IETIM,LPR,LASTLV,
     X                  IREW,IHED,ISTAT,GFIELD)
C
C
C        SETVOL- POSITIONS INPUT TAPE TO BEGINNING OF REQUESTED VOLUME
C                HEADER INFORMATION
C
C                  DOUG RHOADES    EXT. 617   NCAR   JUL., 1982
C
C
C        FORMAL PARAMETERS..........
C        INUNIT- LOGICAL UNIT NUMBER OF INPUT TAPE
C        NAMVOL- REQUESTED VOLUME NAME (8 CHARS)
C        LASTLV- NUMBER OF LAST LEVEL REFERENCED THIS VOLUME  (SET TO 0)
C        IREW- 'YES'  TO REWIND INPUT TAPE BEFORE PROCESSING REQUESTED VOLUME
C                     IREW MUST = 'YES' THE FIRST TIME SETVOL IS CALLED
C        IHED- TAPE/VOLUME HEADER PRECEDING REQUESTED VOLUME
C        ILHD- FILE HEADER PRECEDING EACH LEVEL  (FILLED IN PLANIN)
C
C     ICDF - (0) Unknown format, (1) pure binary, (2) new netCDF, 
C            (3) Gridded Archive 2, (4) MDV, (5) old netCDF
C
      INCLUDE 'CEDRIC.INC'      
      COMMON /IOTYPE/ ICDF
      COMMON /FMTTYPE/ WRFFLG
      INTEGER WRFFLG  
      CHARACTER*1 IREW
      CHARACTER*4 INYVOL
      CHARACTER*8 CTEMP,NAMTAP,NAMVOL,GFIELD(NFMAX)
      DIMENSION IHED(NID),ITEM(NID)
      DATA NMAX/510/
      DATA INYVOL/'NEXT'/

      ISTAT=0
C
C        LOCATE REQUESTED VOLUME
C
      CALL FILETYP(INUNIT,ICDF,NST)
      print *,'SETVOL: inunit,icdf,nst=',inunit,icdf,nst
      print *,'SETVOL:  seeking namvol=',namvol
      IF (NST.NE.0) GOTO 200


      IF (ICDF.LT.0) THEN
         WRITE(*,73)
 73      FORMAT(/,5X,'+++ UNRECOGNIZED INPUT DATA FORMAT +++')
         ISTAT=1
         RETURN
      END IF
      irew = 'Y'
      print *,'SETVOL:            irew=',irew
      IF (IREW.EQ.'Y')THEN
         IF (ICDF.EQ.0) REWIND INUNIT
         IF (ICDF.EQ.1) IREWW=1
      END IF
      LASTLV=0
    5 CONTINUE
      print *,' '
      print *,'SETVOL - data format is recognized, read file header'
      IF (ICDF.EQ.1 .OR. ICDF.EQ.0) THEN
         print *,'SETVOL - call crthin: inunit=',inunit
         CALL CRTHIN(INUNIT,IHED,NMAX,NST,ITEM,IREWW)
         IF(NST.NE.0) GO TO 200
         CALL CHECKHI(IHED)
         ITIME=(IHED(119)*100+IHED(120))*100+IHED(121)
         WRITE(LPR,101) (IHED(I),I=13,15),(IHED(I),I=101,104),ITIME
 101     FORMAT(5X,'RADAR: ',3A2,5X,'VOLUME FOUND: ',4A2,5X,'TIME: ',I6)
         IF(NAMVOL.EQ.INYVOL) THEN
            print *,'SETVOL:  seeking inyvol=',inyvol
            print *,'SETVOL:   with ib-ietim=',ibtim,ietim
            IF(ITIME.GT.IETIM) GO TO 201
            IF(ITIME.GE.IBTIM) GO TO 20
         ELSE
            WRITE (CTEMP,102)(IHED(I),I=101,104)
            READ (CTEMP,500)NAMTAP
 500        FORMAT(A8)
 102        FORMAT(4A2)
            print *,'SETVOL:  seeking namvol=',namvol
            print *,'SETVOL:    found volnam=',namtap
            IF(NAMVOL.EQ.NAMTAP) GO TO 20
         END IF
C     
C     SKIP TO NEXT VOLUME AND CONTINUE SEARCH
C     
         IF (ICDF.EQ.0) THEN
            print *,'SETVOL - call skpvol: inunit=',inunit
            CALL SKPVOL(INUNIT,1)
         ELSE IF (ICDF.EQ.1) THEN
            NST=0
            print *,'SETVOL - call cskpvol: inunit=',inunit
            CALL CSKPVOL(INUNIT,1,NST)
            IF (NST.NE.0) GOTO 200
         END IF
         GO TO 5
C
C        Maybe input is a netCDF file
C
 20      CONTINUE

      ELSE IF (ICDF .EQ. CDFFMT) THEN
         print *,'SETVOL - icdf,cdffmt,wrfflg=',icdf,cdffmt,wrfflg
         IF(WRFFLG .EQ. 0) THEN
            print *,'SETVOL - call cdfopn: inunit=',inunit
            CALL CDFOPN(INUNIT,IHED,NST)
         ELSE IF(WRFFLG .EQ. 1) THEN
            print *,'SETVOL - call wrfopn: inunit=',inunit
c-----------CALL WRFOPN(INUNIT,IHED,NST,IBTIM,IETIM)
         ENDIF
C     
         IF (NST.NE.0) RETURN
      ELSE IF (ICDF .EQ. MDVFMT) THEN

         print *,'SETVOL - call mdvopen: inunit,icdf,mdvfmt=',
     +        inunit,icdf,mdvfmt
         CALL MDVOPEN(INUNIT,IHED,NST)
         IF (NST.NE.0) RETURN
      END IF
C
C        VOLUME HAS BEEN LOCATED
C
      print *,'SETVOL - inunit,icdf,mdvfmt=',
     +     inunit,icdf,mdvfmt
      RETURN
C
C        ERROR EXITS
C
  200 CONTINUE
      CALL TAPMES(INUNIT,NST)
  201 CONTINUE
      CALL CEDERX(522,1)
      ISTAT=1
      RETURN
      END


C*****************************************************************
      SUBROUTINE CHECKHI(IHED)

      INCLUDE 'CEDRIC.INC'
      DIMENSION IHED(NID)
      INTEGER X,Y,Z,RECALCULATE,LEVELS

C     CHECK NUMBER OF POINTS ALONG X

      IF(IHED(162) .GT. MAXX) THEN
         PRINT *,"THE NUMBER OF X GRID POINTS (",IHED(162),
     X           ") IS GREATER THAN ", MAXX
         CALL FLUSH_STDOUT
      END IF

C     CHECK NUMBER OF POINTS ALONG Y     
      IF(IHED(167) .GT. MAXY) THEN
         PRINT *,"THE NUMBER OF X GRID POINTS (",IHED(167),
     X           ") IS GREATER THAN ", MAXY
         CALL FLUSH_STDOUT
      END IF


C     CHECK THE NUMBER OF Z LEVELS
      PRINT *,'CHECKHI: maxzlev=',maxzlev
      IF(IHED(172) .GT. MAXZLEV) THEN
         PRINT *,"THE NUMBER OF Z LEVELS (",IHED(172),
     X           ") IS GREATER THAN ", MAXZLEV
         CALL FLUSH_STDOUT
      END IF


C     CHECK THE NUMBER OF FIELDS.
      IF(IHED(175) .GT. NFMAX) THEN
         PRINT *,"THE NUMBER OF FIELDS IS GREATER THAN ",NFMAX
         CALL FLUSH_STDOUT
      END IF       
      
      
C     CHECK THE TOTAL X + Y GRID POINTS
      ICHECK = IHED(162) + IHED(167)
      IF( ICHECK .GT. MAXAXIS) THEN
          PRINT *,"THE NUMBER OF GRID POINTS X + Y EXCEEDS ",MAXAXIS
          CALL FLUSH_STDOUT      
      END IF 


C     CHECK THE TOTAL NUMBER OF GRID POINTS IN A PLANE
      ICHECK = IHED(162) * IHED(167)
      IF(ICHECK .GT. MAXPLN) THEN
          PRINT *,"THE NUMBER OF GRID POINTS IN A PLANE EXCEEDS ",
     X            MAXAXIS
          CALL FLUSH_STDOUT      
      END IF    


CIF THE NUMBER OF POINTS IN A PLANE IS GREATER THAN 65536 THEN 
CHEADER INFORMATION WILL NEED TO BE RECALCULATED SINCE 65536
CIS THE LARGEST NUMBER THAT CAN BE STORED IN A 16 BIT WORD.
      X = IHED(164)
      Y = IHED(169)
      Z = IHED(174)
      RECALCULATE = 0
      IF(X .EQ. 1 .AND. Y .EQ. 2 .AND. Z .EQ. 3) THEN
         ICHECK = IHED(162) * IHED(167)
         IF(ICHECK .NE. IHED(301) ) THEN
            IHED(301) = ICHECK
            print *,"             "
            PRINT *,"RECALCULATING NUMBER OF GRID POINTS"
            PRINT *,"NUMBER OF GRID POINTS IN XY PLANE =",IHED(301)
            RECALCULATE = 1
            LEVELS = IHED(172)
         END IF
      ELSE IF (X .EQ. 2 .AND. Y .EQ. 3 .AND. Z .EQ. 1) THEN          
         ICHECK = IHED(162) * IHED(172)         
         IF(ICHECK .NE. IHED(301) ) THEN
            IHED(301) = ICHECK
            PRINT *,"RECALCULATING NUMBER OF GRID POINTS"
            PRINT *,"NUMBER OF GRID POINTS IN XZ PLANE =",IHED(301)
            RECALCULATE = 1
            LEVELS = IHED(167)
         END IF
      ELSE IF (X .EQ. 3 .AND. Y .EQ. 1 .AND. Z .EQ. 2) THEN
         ICHECK = IHED(167) * IHED(172)  
         IF(ICHECK .NE. IHED(301) ) THEN
            IHED(301) = ICHECK
            PRINT *,"RECALCULATING NUMBER OF GRID POINTS"
            PRINT *,"NUMBER OF GRID POINTS IN YZ PLANE =",IHED(301)
            RECALCULATE = 1
            LEVELS = IHED(167)
         END IF 
      END IF


      IF(RECALCULATE .EQ. 1) THEN
         IHED(96)  = (IHED(301) - 1)/(3200 - 1)
         IHED(97) = IHED(96) * IHED(175)
         IHED(98) = IHED(97) * LEVELS
         IHED(99) = IHED(98) + LEVELS + 1  
      END IF
       
      RETURN
      END
