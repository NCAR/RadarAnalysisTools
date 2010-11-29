C Subroutines to read & massage the data supplied by user
**************************************************
	Subroutine ReadUserDat (id, nid, panel)

        Implicit none

        Include 'common1.h'
        Include 'common2.h'
        Include 'common3.h'
	Include 'structure.h'

	Integer nid,id(nid),panel
	Integer i

C This subroutine reads in a 510 element header array that contains
C information regarding the data (array id) and stores the data
C into a 3 dimensional array fld_data(x, y, fld), where x is the
C horizontal grid, y is the vertical grid and fld is the field index.
C fld ranges from 1 to 4 where 
C   1 = horizontal wind component
C   2 = vertical wind component
C   3 = 1st contour field
C   4 = 2nd contour field

C Initialize the header array
	DO i = 1, nid
	   id(i) = -999
	ENDDO

C Open the data file
	Open (unit=601,file=data_file,status="unknown")

C Fill in the following elements of the CEDRIC header array..
C	id(16) = SCAN_MODE (char 1 & 2) ... see below
C	id(17) = SCAN_MODE (char 3 & 4) ... see below
C	id(33) = COORDINATE ORIGIN LATITUDE DEGREES 
C	id(34) = COORDINATE ORIGIN LATITUDE MINUTES 
C	id(35) = COORDINATE ORIGIN LATITUDE SECONDS * 100 
C	id(36) = COORDINATE ORIGIN LONGITUDE DEGREES 
C	id(37) = COORDINATE ORIGIN LONGITUDE MINUTES 
C	id(38) = COORDINATE ORIGIN LONGITUDE SECONDS * 100
C	id(40) = DEGREES CLOCKWISE FROM NORTH TO X-AXIS * 64 
C	id(41) = X COORDINATE OF HORIZONTAL AXIS ORIGIN * 100 [id(68)]
C	id(42) = Y COORDINATE OF HORIZONTAL AXIS ORIGIN * 100 [id(68)]
C	id(68) = GENERAL SCALING FACTOR (SF = 100)
C	id(69) = ANGLE SCALING FACTOR (CF = 64)
C	id(116) = YEAR BEGIN 
C	id(117) = MONTH BEGIN 
C	id(118) = DAY BEGIN 
C	id(119) = HOUR BEGIN 
C	id(120) = MINUTE BEGIN 
C	id(121) = SECOND BEGIN 
C	id(122) = YEAR END 
C	id(123) = MONTH END 
C	id(124) = DAY END 
C	id(125) = HOUR END 
C	id(126) = MINUTE END 
C	id(127) = SECOND END 
C	id(160) = MINIMUM X (KM) * 100 [id(68)]
C	id(161) = MAXIMUM X (KM) * 100 [id(68)]
C	id(162) = NUMBER OF X'S
C	id(163) = SPACING IN X (M)
C	id(165) = MINIMUM Y (KM) * 100 [id(68)]
C	id(166) = MAXIMUM Y (KM) * 100 [id(68)]
C	id(167) = NUMBER OF Y'S
C	id(168) = SPACING IN Y (M)
C	id(165) = MINIMUM Z (M) 
C	id(166) = MAXIMUM Z (M)
C	id(167) = NUMBER OF Z'S
C	id(168) = SPACING IN Z (M)

C Note: only need id(16), id(17) & id(40) if plotting a
C flight track
C 	SCAN_MODE = 'PRI'; id(16) = 20562; id(17) = 18720	
C 	SCAN_MODE = 'PPI'; id(16) = 20560; id(17) = 18720	
C 	SCAN_MODE = 'COP'; id(16) = 17231; id(17) = 20512	
C 	SCAN_MODE = 'FIX'; id(16) = 17993; id(17) = 22560	
C 	SCAN_MODE = 'VAD'; id(16) = 22081; id(17) = 17440	
C 	SCAN_MODE = 'CPL'; id(16) = 17232; id(17) = 19488	
C 	SCAN_MODE = 'CPN'; id(16) = 17232; id(17) = 20000	
C 	SCAN_MODE = 'CRT'; id(16) = 17234; id(17) = 21536	
C 	SCAN_MODE = 'AIR'; id(16) = 16713; id(17) = 21024 
C 	SCAN_MODE = 'POLA'; id(16) = 20559; id(17) = 19521 

C Finally, fill in the data array fld_data(x,y,fld)

	Close(unit = 601)

	Return
	End
**************************************************
	Subroutine MassageData (id, nid)

        Implicit none

        Include 'common1.h'
        Include 'common2.h'
        Include 'common3.h'
	Include 'structure.h'

	Integer nid, id(nid)
	Integer x, y, fld

C This subroutine manipulates the data 
	DO x = 1, ii
	   DO y = 1, jj
	      DO fld = 1,4
C		 fld_data(x, y, fld) = fld_data(x, y, fld)*10
	      ENDDO
	   ENDDO
	ENDDO

	Return
	End
