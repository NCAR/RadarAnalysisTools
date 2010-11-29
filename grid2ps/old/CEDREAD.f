      SUBROUTINE CEDREAD(id,nid,iun,panel)
C
C     THIS SUBROUTINE READS IN A CEDRIC FORMAT DISK VOLUME AND
C     RETURNS ID HEADER INFORMATION ABOUT THE VOLUME AS WELL AS
C     THE DATA FOR THE VARIOUS FIELDS AND LEVELS IN THE VOLUME.
C
C     ID     -  510 WORD ID HEADER FOR VOLUME
C     ITEM   -  INTEGER SCRATCH ARRAY
C     RBUF   -  FLOAT ARRAY IN WHICH DATA VALUES FOR A GIVEN FIELD AND LEVEL
C               ARE RETURNED 
C     ILHD   -  10 WORD LEVEL HEADER; USED FOR INTERNAL PROCESSING AND
C               NOT IMPORTANT TO UNDERSTAND
C     FLDNAM -  USED TO STORE NAME OF FIELDS. EACH FIELD NAME IS 
C               EIGHT CHARACTERS LONG AND IS STORED AS A 4 ELEMENT
C               ARRAY WHERE EACH ELEMENT IS TWO CHARACTERS.
C     IUN    -  UNIT NUMBER OF INPUT FILE; FILE SEARCHED FOR WILL BE
C               CALLED 'fort.iun'. 

C
	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'

*      PARAMETER (NID=510)
	Integer id(nid),ilhd_index,panel,fieldnum,x,y
      PARAMETER (MAXPLN=65536,MAXX=256,MAXY=256)
*      DIMENSION ID(NID),ITEM(MAXPLN),RBUF(MAXX,MAXY),ILHD(10)
      DIMENSION ITEM(MAXPLN),RBUF(MAXX,MAXY),ILHD(10)
      DIMENSION SRBUF(MAXPLN)
      CHARACTER*2 FLDNAM(4)

      Data NPLIN,LASTLV,LASTFD,KOT,ZLEV,BAD,ILHD /0,0,0,0,0.0,0.0,10*0/
      Data fieldnum /0/

C     READ FROM A FILE CALLED fort.10
C
*      IUN=20
      LBF=NID
      LPR=6

C Initialize variables for multiple files
      	NPLIN=0
	LASTLV=0
	LASTFD=0
	KOT=0
	ZLEV=0.0
	BAD=0.0
	DO ilhd_index=1,10
	 ILHD(ilhd_index)=0
	ENDDO 


C DON'T REWIND; IF IREW=1, REWIND STREAM TO BEGINNING
	IREW=0

C READ IN 16 BIT PACKED CARTESIAN HEADER; HEADER RETURNED INTO ID
	CALL CRTHIN(IUN,ID,LBF,NST,ITEM,IREW)

C SUMMARIZE VOLUME INFORMATION
	CALL IMHSUM(LPR,ID)

	IF (fix_axis.EQ.'Z'.OR.fix_axis.EQ.'z') THEN
	   fix_beg=id(170)/float(1000) !convert to km
	   fix_inc=id(173)/float(1000) !convert to km
	ELSEIF (fix_axis.EQ.'Y'.OR.fix_axis.EQ.'y') THEN
	   fix_beg=id(165)/float(id(68)) !scale factor
	   fix_inc=id(168)/float(1000)   !convert to km
	ELSEIF (fix_axis.EQ.'X'.OR.fix_axis.EQ.'x') THEN
	   fix_beg=id(160)/float(id(68)) !scale factor
	   fix_inc=id(163)/float(1000)   !convert to km
	ENDIF

C PULL IN DATA FOR ALL FIELDS AND LEVELS NOW
	NZ=ID(172)
	NFLDS=ID(175)
	NPLIN=ID(162)*ID(167)
	BAD=-1000.0
	DO 150 KOT=1,NZ
	  fieldnum=0
	  DO 140 LF=1,NFLDS
	     INF=171+(5*LF)
	     WRITE(FLDNAM(1),113)ID(INF)
	     WRITE(FLDNAM(2),113)ID(INF+1)
	     WRITE(FLDNAM(3),113)ID(INF+2)
	     WRITE(FLDNAM(4),113)ID(INF+3)
113          FORMAT(A2)

C TRY TO FIND A FIELD MATCH
	     fieldnum=1
C200	     IF (fieldnum.LT.5) THEN  !WHILE LOOP
	     DO fieldnum=1,4
   	     IF (fld_name(1,fieldnum).EQ.fldnam(1).AND.fld_name(2,fieldnum).
     X          EQ.fldnam(2).AND.fld_name(3,fieldnum).EQ.fldnam(3).AND.
     X          fld_name(4,fieldnum).EQ.fldnam(4)) THEN
C CALCULATE LEVEL
                   IF (level_flag.EQ.'D'.OR.level_flag.EQ.'d') THEN
                     level=nint(((plots(panel)-fix_beg)/fix_inc)+1)
   	           ENDIF

C GET A LEVEL OF DATA
	          CALL FETCHZ(IUN,SRBUF,ITEM,NPLIN,LASTLV,LASTFD,KOT,ZLEV,
     X                        FLDNAM,BAD,ID,ILHD)

C STORE A LEVEL OF DATA IN RBUF
		  DO Y=1,ID(167)
		    DO X=1,ID(162)
		      RBUF(x,y) = SRBUF(ID(162)*(Y-1)+X)
		    ENDDO
		  ENDDO

C FIXED AXIS=Z
	          IF (fix_axis.EQ.'Z'.OR.fix_axis.EQ.'z') THEN
	            IF (level.EQ.kot) THEN
		      DO Y=1,ID(167)
		        DO X=1,ID(162)
		          fld_data(x,y,fieldnum)=rbuf(x,y)
		        ENDDO
		      ENDDO
		    ENDIF
C FIXED AXIS=Y
	          ELSEIF (fix_axis.EQ.'Y'.OR.fix_axis.EQ.'y') THEN
		    DO X=1,ID(162)
		      fld_data(x,kot,fieldnum)=rbuf(x,level)
		    ENDDO
C FIXED AXIS=X
	          ELSEIF (fix_axis.EQ.'X'.OR.fix_axis.EQ.'x') THEN
		    DO Y=1,ID(167)
		       fld_data(y,kot,fieldnum)=rbuf(level,y)
		    ENDDO
	          ENDIF
*		  fieldnum=5
*		  go to 200  !END WHILE
*	        ELSE
*		  fieldnum=fieldnum+1
*	          go to 200 !END WHILE
		ENDIF	
	     ENDDO
 
140  	CONTINUE
150  	CONTINUE

	Close (unit=iun)
        scalefld(3)=1
        scalefld(4)=1

      END
