      SUBROUTINE CEDREAD(IUN,NX,NY,NZ,NFLDS,NPLIN,BAD,ID)
C
C     THIS SUBROUTINE READS IN A CEDRIC FORMAT DISK VOLUME AND
C     RETURNS ID HEADER INFORMATION ABOUT THE VOLUME AS WELL AS
C     THE DATA FOR THE VARIOUS FIELDS AND LEVELS IN THE VOLUME.
C
C     ID     -  510 WORD ID HEADER FOR VOLUME
C     ITEM   -  INTEGER SCRATCH ARRAY
C     RBUF   -  FLOAT ARRAY IN WHICH DATA VALUES FOR A GIVEN FIELD AND LEVEL
C               ARE RETURNED 
C     TBUF   -  FLOATING POINT SCRATCH ARRAY
C     ILHD   -  10 WORD LEVEL HEADER; USED FOR INTERNAL PROCESSING AND
C               NOT IMPORTANT TO UNDERSTAND
C     FLDNAM -  USED TO STORE NAME OF FIELDS. EACH FIELD NAME IS 
C               EIGHT CHARACTERS LONG 
C     IUN    -  UNIT NUMBER OF INPUT FILE; FILE SEARCHED FOR WILL BE
C               CALLED 'fort.iun'. 
C
      INCLUDE 'CEDRIC.INC'
      DIMENSION ID(NID),ITEM(MAXPLN),RBUF(MAXPLN),ILHD(10)
      DIMENSION TBUF(MAXPLN)
      CHARACTER*8 FLDNAM
C
C     READ FROM FORTRAN INPUT UNIT = IUN
C
      LBF=NID
      LPR=6
C
C     DON'T REWIND; IF IREW=1, REWIND STREAM TO BEGINNING
C
      IREW=0
C
C     READ IN 16 BIT PACKED CARTESIAN HEADER; HEADER RETURNED INTO ID
C
      print *,'CEDHEAD: before CRTHIN'
      CALL CRTHIN(IUN,ID,LBF,NST,ITEM,IREW,NID)
      print *,'CEDHEAD: after CRTHIN'

      print *,'CEDREAD:  wordsz=',wordsz
      print *,'CEDREAD:  maxbuf=',maxbuf
      NFL=ID(175)
      print *,'CEDREAD: id(175)=',id(175)
      NPLANE=ID(301)
      print *,'CEDREAD: id(301)=',id(301)
      ID(451)=(NPLANE-1)/(WORDSZ/16)+1
      print *,'CEDREAD: id(451)=',id(451)
      ID(452)=ID(106)*ID(451)
      print *,'CEDREAD: id(106)=',id(106)
      print *,'CEDREAD: id(452)=',id(452)
      ID(453)=MIN0(MAXFLD,MAXBUF/ID(452))
      print *,'CEDREAD: id(453)=',id(453)
C
C     SUMMARIZE VOLUME INFORMATION
C
      print *,'CEDHEAD: before IMHSUM'
      CALL IMHSUM(LPR,ID)
      print *,'CEDHEAD: after IMHSUM'
C
C     PULL IN DATA FOR ALL FIELDS AND LEVELS NOW
C
      NX=ID(162)
      NY=ID(167)
      NZ=ID(172)
      NFLDS=ID(175)
      NPLIN=ID(162)*ID(167)
      BAD=-1000.0
      LASTLV=0
      LASTFD=0
      DO 150 KOT=1,NZ
         DO 140 LF=1,NFLDS
            INF=171+(5*LF)
            WRITE(FLDNAM,113)ID(INF),ID(INF+1),ID(INF+2),ID(INF+3)
 113        FORMAT(4A2)

C
C     THE FOLLOWING FUNCTION RETURNS THE DATA FOR LEVEL KOT AND
C     FIELD LF IN ARRAY TBUF
C
            CALL FETCHZ(IUN,TBUF,ITEM,NPLIN,LASTLV,LASTFD,KOT,ZLEV,
     X                  FLDNAM,BAD,ID,ILHD)

            DO J=1,NY
               DO I=1,NX
                  IND=I + (J-1)*NX
                  RBUF(IND)=TBUF(IND)
               END DO
            END DO

C
C     USER MUST SUPPLY THE SUBROUTINE FOR THIS NEXT CALL.
C     ESSENTIALLY, IT MUST BE ABLE TO STORE THE DATA FOR Z-LEVEL KOT
C     AND FIELD LF INTO WHATEVER INTERNAL STRUCTURES YOU HAVE.
C

            CALL PLACEPLANE(RBUF,LF,KOT,FLDNAM,NX,NY)
 140     CONTINUE
 150  CONTINUE


      CALL CSKPVOL(IUN,1,NST)
      RETURN

      END