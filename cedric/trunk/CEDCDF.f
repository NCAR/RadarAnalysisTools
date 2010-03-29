      SUBROUTINE CDFOPN(IUNIT,IBUF,NST)
C
C     THIS SUBROUTINE OPENS A NETCDF FILE FOR READING AND RETURNS HEADER
C
      INCLUDE '/opt/local/netcdf-3/include/netcdf.inc'
      INCLUDE 'CEDRIC.INC'
      PARAMETER (MAXVARS = 50)
      PARAMETER (NCMAXDIM = 100)
      PARAMETER (NCMAXVAR = 2000)
      COMMON /CDFNET/ ICDFID(MXCDF),IDIMDAT(4,MXCDF),
     X     IDIMID(1,MXCDF),IVAR(NFMAX+1,MXCDF),IFILE,ISYNFLG,
     X     ICDUNT(MXCDF),IUSWRP
      COMMON /LEVELS/ VALLEV(MAXZLEV),VALNYQ(MAXZLEV),VNYQ_VOL
      CHARACTER*3 TYPEGRID
      CHARACTER*2 CTEMP(2)
      CHARACTER*8 CIBL
      CHARACTER*8 FLDNAM(NFMAX),VOLNAME,NAMLND(MAXLAND)
      CHARACTER*8 RADARNME
      CHARACTER*8 THETIME,THEDATE
      DIMENSION ISTART(1),ICOUNT(1)
      INTEGER RCODE,FILETYPE,VLEVELS
      INTEGER VARSCNT
      INTEGER ISCLFLD(NFMAX)
      INTEGER IGENSCL,SWAP,CUNIT,GRID
      INTEGER VARIDS(NCMAXVAR)
      INTEGER DIMSIZES(NCMAXDIM)
      INTEGER ILND,GLOBLATT
      REAL    X(MAXLAND),Y(MAXLAND),Z(MAXLAND)
      REAL    REFNYQ

CITEM IS A 510 WORD ARRAY TO STORE HEADER VALUES FOR PLACING IN
CIBUF
      INTEGER ITEM(NID)
      REAL   ALTITUDE
      INTEGER IBUF(NID)
      DATA CIBL/'        '/
      Ns=0

      IGENSCL = 100

      IFILE=IFILE+1
      ICDUNT(IFILE)=IUNIT
COPEN THE NETCDF FILE,GET THE IMPORTANT DIMENSION SIZES
CAND GET THE VARIABLE IDS.
      CALL GRIDRESL(GRID)
      CALL COPENCDF(IUNIT,CUNIT,VARSCNT,DIMSIZES,VARIDS,
     X              FILETYPE,GRID,GLOBLATT)


      ICDFID(IFILE) = CUNIT
      print *,'CDFOPN: ifile,icdunt=',ifile,icdunt(ifile)

C-------------------------------------------------------------------
C
C     GET HEADER INFO
C      
C--------------------------------------------------------------------
       DO I = 1,NID
          ITEM(I) = 0
       END DO

C FIELD INFORMATION
      IF(FILETYPE .GT. 0) THEN
         CALL CDFFLDI(CUNIT,FLDNAM,VARIDS,ISCLFLD,DIMSIZES,
     x                VARSCNT,FILETYPE,ITEM(175))
         IFLD = 176
         NFLD = ITEM(175)
         DO  I = 1,NFLD
             IVAR(I,IFILE) = VARIDS(I + 1)
             READ(FLDNAM(I),10),(ITEM(IFLD + (I - 1)*5+ J-1),J=1,4)
 10      FORMAT(4A2)
            ITEM(IFLD + (I - 1)*5 + 4) = ISCLFLD(I)
         END DO


CGET LEVEL,GRID AND POSITION INFORMATION
         CALL CDFLVLI(CUNIT,TYPEGRID,VALLEV,VALNYQ,REFNYQ,
     x                VLEVELS,VARIDS,DIMSIZES)
         ITEM(304) = REFNYQ * IGENSCL

         CALL GRIDINFO(CUNIT,ITEM,TYPEGRID,VLEVELS,VARIDS,
     x                DIMSIZES,GRID)
        CALL CDFPOSN(CUNIT,ITEM,VARIDS)
CGET DATES,TIMES AND VOLUME NAME.
         CALL CDFTIMEI(CUNIT,ITEM,VARIDS,DIMSIZES,VOLNAME,
     x                 THEDATE,THETIME,GLOBLATT,FILETYPE)
         READ(THEDATE,10)(ITEM(I),I = 51,54)
         READ(THETIME,10)(ITEM(I),I = 55,58)
 20      FORMAT(A8)

         READ(VOLNAME,10)(ITEM(I),I=101,104)

CGET LANDMARK INFORMATION.
         CALL CDFLANDI(CUNIT,ITEM,NAMLND,DIMSIZES,VARIDS,
     X                 RADARNME,X,Y,Z)
         ILND=306
         DO I = 1,ITEM(302)
            READ(NAMLND(I),25)(ITEM(ILND + (I-1)*6 + J-1),J= 1,3)
            ITEM(ILND + (I - 1)*6 + 3) = NINT(X(I) * IGENSCL)
            ITEM(ILND + (I - 1)*6 + 4) = NINT(Y(I) * IGENSCL)
            ITEM(ILND + (I - 1)*6 + 5) = Z(I)
         END DO
 25      FORMAT(3A2)
         READ (RADARNME,25)(ITEM(I),I=13,15) 

CGENERAL HEADER INFORMATION
         CALL PUTINFO(CUNIT,ITEM,FILETYPE,VARIDS)
         ITEM(309) = 0
         ITEM(310) = 0
C------------------------------------------------------------
      ELSE
         IDH = NCVID(ICDFID(IFILE), 'IDHEAD', RCODE)
         IF (RCODE.NE.0.0) THEN
            PRINT *,"UNABLE TO GET CEDRIC 512 WORD HEADER ID"
            CALL CEDERX(587,1)
            NST=1
            RETURN
          ENDIF
     
          ISTART(1)=1
          ICOUNT(1)=NID
          CALL NCVGT(ICDFID(IFILE),IDH,ISTART,ICOUNT,ITEM,RCODE)
          IF (RCODE.NE.0.0) THEN
             PRINT *,"UNABLE TO GET CEDRIC 512 WORD HEADER"
             CALL CEDERX(588,1)
             NST=1
             RETURN
          END IF

         IF(ITEM(301) .GT. MAXPLN) THEN
              PRINT *,"NUMBER OF GRID POINTS IN A PLANE EXCEEDS ",
     X        MAXPLN
              STOP
          ENDIF
       ENDIF
C---------------------------------------------------------------------           
C
C     HANDLE BYTE ORDER FOR CHARACTERS
C
      DO I=1,510
         IBUF(I)=ITEM(I)
      END DO
      CALL CBYTE(MBYTE)

      IF (MBYTE .EQ. 1 ) THEN
C
C     LITTLE ENDIAN
C
         IF(FILETYPE .EQ. 0) THEN
            CALL SWAPCHAR(ITEM(1),IBUF(1),20)
            CALL SWAPCHAR(ITEM(43),IBUF(43),16)
            CALL SWAPCHAR(ITEM(62),IBUF(62),1)
            CALL SWAPCHAR(ITEM(66),IBUF(66),1)
            CALL SWAPCHAR(ITEM(71),IBUF(71),24)
            CALL SWAPCHAR(ITEM(101),IBUF(101),4)
            CALL SWAPCHAR(ITEM(151),IBUF(151),1)
            DO I=1,25
               IN=176+(I-1)*5
               CALL SWAPCHAR(ITEM(IN),IBUF(IN),4)
            ENDDO 
            IF(ITEM(301).LE.0)
     X          ITEM(301)=ITEM(301)+MAXPLN
            IBUF(301)=ITEM(301)
            DO I=1,15
               IN=306+(I-1)*6
               CALL SWAPCHAR(ITEM(IN),IBUF(IN),3)
            END DO
         END IF
         IBUF(451)=0
         IBUF(452)=0
         IBUF(453)=0
      ELSE IF (MBYTE.EQ.0) THEN
C
C     BIG ENDIAN
C
         IF(NFLD .EQ. 0 .AND. FILETYPE .EQ. 0) THEN
            CALL SHILBL(IBUF(1),20)
            CALL SHILBL(IBUF(43),16)
            CALL SHILBL(IBUF(62),1)
            CALL SHILBL(IBUF(66),1)
            CALL SHILBL(IBUF(71),24)
            CALL SHILBL(IBUF(101),4)
            CALL SHILBL(IBUF(151),1)
            DO 272 I=1,25
               IN=176+(I-1)*5
               CALL SHILBL(IBUF(IN),4)
 272        CONTINUE
            IF(IBUF(301).LE.0)
     X          IBUF(301)=IBUF(301)+MAXPLN
            DO 402 I=1,15
               IN=306+(I-1)*6
               CALL SHILBL(IBUF(IN),3)
 402        CONTINUE
         ENDIF
         IBUF(451)=0
         IBUF(452)=0
         IBUF(453)=0
       END IF   

      NST=0
      RETURN
      END
C*********************************************************************************
      SUBROUTINE FTCHCDF(RBUF,NLEV,NUMFLD,BAD,SCALE,
     X                   NTX,NTY,NPLANE,IUNIT)
C
C     FETCHES A Z-LEVEL FOR A GIVEN FIELD
C
      INCLUDE 'CEDRIC.INC'
      COMMON /IOTYPE/ ICDF

      COMMON /CDFNET/ ICDFID(MXCDF),IDIMDAT(4,MXCDF),
     X     IDIMID(1,MXCDF),IVAR(NFMAX+1,MXCDF),IFILE,ISYNFLG,
     X     ICDUNT(MXCDF),IUSWRP
      DIMENSION RBUF(NPLANE),ISTART(4),ICOUNT(4)
      INTEGER FUNIT,VARID,WRITE_TYPE,FORMAT
      REAL RBUF2(MAXPLN)
      CHARACTER*2 NAME(4)
      DATA ZIPAK/-32768./

C     FOR SNTHESIS, MULTIPLE FILES CAN BE OPEN AT ONCE; SPEC. PROC. NEEDS
C     TO BE DONE TO HANDLE THAT.
 
      IUN=IFIND(IUNIT,ICDUNT,IFILE,1)
c      print *,'FTCHCDF: After IFIND'
c      print *,'FTCHCDF: IUNIT=',iunit
c      print *,'FTCHCDF: ICDUNT=',icdunt
c      print *,'FTCHCDF: IFILE=',ifile
      IF (IUN.LE.0) THEN
         PRINT *,"+++ERROR ACCESSING UNIT IN CEDCDF+++",IUNIT,ICDUNT
         STOP
      END IF

CINITIALIZE THE ARRAYS TO ALL BAD.
      DO I = 1,NPLANE
         RBUF(I)  = ZIPAK
         RBUF2(I) = ZIPAK
      END DO
      call FORMATTP(format,write_type)

      if(write_type .eq. OLDCEDRIC) then
          ISTART(1)=1
          ISTART(2)=1
          ISTART(3)=NLEV
          ISTART(4)=1
          ICOUNT(1)=NTY
          ICOUNT(2)=NTX
          ICOUNT(3)=1
          ICOUNT(4)=1
CIf the write type is 0 then this is an old syle cedric NETCDF file.
          CALL NCVGT(ICDFID(IUN),IVAR(NUMFLD,IUN),ISTART,ICOUNT,
     x               RBUF2,RCODE)
           IF (RCODE.NE.0.0) THEN
               WRITE(*,25)
 25            FORMAT(/,5X,'+++ERROR READING DATA FROM netCDF FILE+++')
               STOP
           END IF
       ELSE
            FUNIT = ICDFID(IUN)
            VARID = IVAR(NUMFLD,IUN)
            CALL CDFGDATA(FUNIT,VARID,RBUF2,NLEV,NTX,NTY,FORMAT)
       END IF


         DO I=1,NTX*NTY
            RBUF(I) = RBUF2(I)
         END DO
      RETURN
      END

C*****************************************************************      
      SUBROUTINE CDFPUT(INPUT,NFL,NUMZ,NX,NY)
C     
C     THIS SUBROUTINE PUTS THE DATA VALUES INTO THE netCDF FILE
C     
      INCLUDE '/opt/local/netcdf-3/include/netcdf.inc'
      INCLUDE 'CEDRIC.INC'
      COMMON /CDFNET/ ICDFID(MXCDF),IDIMDAT(4,MXCDF),
     X     IDIMID(1,MXCDF),IVAR(NFMAX+1,MXCDF),IFILE,ISYNFLG,
     X     ICDUNT(MXCDF),IUSWRP
      INTEGER RCODE,IDH,UNIT
      INTEGER ISTART(4),ICOUNT(4)
      REAL INPUT(MAXPLN)
      
      IDH = IVAR(NFL,IFILE)
      NST=0
C     
C     PREPARE TO WRITE OUT ONE LEVEL FOR ONE FIELD
C     

      CALL WCDFDATA(ICDFID(IFILE),INPUT,NX,NY,NUMZ,IDH)
      RETURN
      END
      
C**************************************************************      
      SUBROUTINE NETCLOS()
C     
C     THIS SUBROUTINE CLOSES THE netCDF FILE
C     
      
      INCLUDE 'CEDRIC.INC'
      COMMON /CDFNET/ ICDFID(MXCDF),IDIMDAT(4,MXCDF),
     X     IDIMID(1,MXCDF),IVAR(NFMAX+1,MXCDF),IFILE,ISYNFLG,
     X     ICDUNT(MXCDF),IUSWRP

      INTEGER RCODE

      DO I=1,IFILE
         IF(ICDFID(I) .NE. 0) CALL NCCLOS(ICDFID(I),RCODE)
         IFILE = IFILE - 1
         ICDFID(I) = 0
      END DO

      RETURN
      END


C**************************************************************
C THE FOLLOWING ROUTINES ARE USED FOR PUTTING INFORMATION INTO
C THE CEDRIC 512 WORD HEADER.  THESE ARE CALLED WHEN READING
C THE USWRP GRIDDED DATA SETS WHICH ARE IN NETCDF FORMAT.
C**************************************************************
CPut info put misc information into the header
      SUBROUTINE PUTINFO(CUNIT,ITEM,FILETYPE,VARIDS)


      INCLUDE 'CEDRIC.INC'
      PARAMETER (NCMAXVAR = 2000)
      DIMENSION ITEM(NID),VARIDS(NCMAXVAR)
      CHARACTER*8 CTEMP,VOLHEAD
      CHARACTER*8 SOURCE
      CHARACTER*4 PROJ_NAME
      CHARACTER*80 PERSON
      INTEGER   FILETYPE
      CHARACTER*6 SCINAME,TAPE
      CHARACTER*2 INSTALLATION
      INTEGER CUNIT
  

      INSTALLATION = "WK"
      READ(INSTALLATION,5)ITEM(62)

      SCINAME = '      '
      READ(SCINAME,10)(ITEM(I),I=10,12)

      TAPE    = '      '
      READ(TAPE,10)(ITEM(I),I=18,20)

      PERSON = '       '
      CALL GETENV('QSUB_REQNAME',PERSON)
      READ(PERSON,10)(ITEM(I),I=45,47)

      PERSON = '       '
      CALL GETENV('LOGNAME',PERSON)
      READ (PERSON,10)(ITEM(I),I=48,50)


      IF(FILETYPE .EQ. USWRPGD) THEN
         CTEMP = "GRIDDED "
         READ(CTEMP,20)(ITEM(I),I=1,4)

         CTEMP = "CEDRIC"
         READ(CTEMP,10)(ITEM(I),I=5,7)

         CTEMP = "USWRP"
         READ(CTEMP,15)(ITEM(I),I=8,9)

         SOURCE = 'NCDC     '
         READ(SOURCE,20)(ITEM(I),I=71,74)
         CALL USWRPI(CUNIT)
      ELSE
         CALL GCDFGENI(cunit,source,proj_name,volhead,varids)
         READ(VOLHEAD,20)(ITEM(I),I=1,4)
         CTEMP = "CEDRIC"
         READ(CTEMP,10)(ITEM(I),I=5,7)
         READ (proj_name,10)(ITEM(I),I=8,9) 
         READ(source,20)(ITEM(I),I=71,74) 
      END IF

 5    FORMAT(A2)
 10   FORMAT(3A2) 
 15   FORMAT(2A2)
 20   FORMAT(4A2)

      ITEM(174) = 3
         
CBASANG
      ITEM(40) = 90 * 64


      ITEM(41) = 0
      ITEM(42) = 0
      ITEM(61) = 510
      ITEM(63) = 16
      ITEM(64) = 2
      ITEM(65) = 3200
      ITEM(67) = -32768
      ITEM(68) = 100
      ITEM(69) = 64
      ITEM(39) = 0

      RETURN
      END
C*******************************************************************

      SUBROUTINE GRIDINFO(CUNIT,ITEM,TYPEGRID,VLEVELS,VARIDS,
     X                    DIMSIZES,GRID)

      INCLUDE 'CEDRIC.INC'
      PARAMETER (NCMAXDIM = 100)
      PARAMETER (NCMAXVAR = 2000)
      COMMON /LEVELS/ VALLEV(MAXZLEV),VALNYQ(MAXZLEV),VNYQ_VOL
      COMMON /CDFNET/ ICDFID(MXCDF),IDIMDAT(4,MXCDF),
     X     IDIMID(1,MXCDF),IVAR(NFMAX+1,MXCDF),IFILE,ISYNFLG,
     X     ICDUNT(MXCDF),IUSWRP
      INTEGER ITEM(NID)
      INTEGER NUMX,NUMY,CUNIT
      REAL    XMAX,YMAX,ZMIN,ZMAX
      INTEGER numxyz(3),GRID
      INTEGER XSPACING,YSPACING,ZSPACING
      INTEGER IZMIN,IZMAX,VLEVELS
      INTEGER IGENSCL,IANGSCL
      INTEGER VARIDS(NCMAXVAR)
      INTEGER DIMSIZES(NCMAXDIM)
      INTEGER MINXY(2), MAXXY(2)
      INTEGER SPACING(NFMAX)
      CHARACTER*3 TYPEGRID
      CHARACTER*2 CSCAN1,CSCAN2

      IGENSCL = 100
      IANGSCL = 64

C--------------THE ELEVATIONS OR Z LEVELS--------------------------------
      ITEM(98) = ITEM(97) * VLEVELS
      ITEM(99) = ITEM(98) + VLEVELS + 1
C SCAN MODE 
      CALL SCANMODE(TYPEGRID,CSCAN1,CSCAN2)
      READ(CSCAN1,77) ITEM(16)
      READ(CSCAN2,77) ITEM(17)
 77   FORMAT(A2)      


      ZMIN = VALLEV(1) * 100
      ZMAX = VALLEV(VLEVELS) * 100

      IZMIN = INT(ZMIN)
      IZMAX = INT(ZMAX)
      IEL1 = VALLEV(1)*100
      IEL2 = VALLEV(2)*100
      ZSPACING = IEL2 - IEL1


C------------------THE X AND Y GRID INFORMATION--------------------------

      call CDFXYZI(cunit,numxyz,spacing,minxy,maxxy,
     x             varids,dimsizes,grid)


      numx = numxyz(1)
      numy = numxyz(2)
      xspacing = SPACING(1)
      yspacing = SPACING(2)



C---------PUT INFORMATION INTO THE CEDRIC HEADER(ITEM)--------

      ITEM(160) = MINXY(1)
      ITEM(161) = MAXXY(1)
      ITEM(162) = NUMX
      ITEM(163) = XSPACING
      ITEM(164) = 1


      ITEM(165) = MINXY(2)
      ITEM(166) = MAXXY(2)
      ITEM(167) = NUMY
      ITEM(168) = YSPACING   
      ITEM(169) = 2
      
      ITEM(154) = VLEVELS
      ITEM(106) = VLEVELS
      ITEM(170) = IZMIN * 10
      ITEM(171) = IZMAX * 10
      ITEM(172) = VLEVELS
      ITEM(173) = ZSPACING * 10
      ITEM(174) = 3
      ITEM(301) = NUMX*NUMY
      IF(ITEM(301) .GT. MAXPLN) THEN
         PRINT *,"NUMBER OF GRID POINTS IN A PLANE EXCEEDS ",MAXPLN
         STOP
      ENDIF      
      ITEM(96)  = ITEM(301)/3200
      IF(ITEM(96) .EQ. 0) ITEM(96) = 1
      ITEM(97)  = ITEM(96) * ITEM(175)
      ITEM(106) = VLEVELS
      ITEM(98)  = ITEM(97)*ITEM(106)
      ITEM(99)  = ITEM(98) + ITEM(106) + 1
      RETURN
      END      
C------------------------------------------------------------------

      SUBROUTINE WIDVARS(LOUTNM)

      INCLUDE '/opt/local/netcdf-3/include/netcdf.inc'
      INCLUDE 'CEDRIC.INC'

      PARAMETER (NCMAXDIM = 100)
      PARAMETER (NCMAXVAR = 2000)
      CHARACTER*8 LOUTNM
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X     IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X     NCXORD(3),NFL,NPLANE,BAD
      COMMON /LEVELS/ VALLEV(MAXZLEV),VALNYQ(MAXZLEV),VNYQ_VOL
      COMMON /CDFNET/ ICDFID(MXCDF),IDIMDAT(4,MXCDF),
     X     IDIMID(1,MXCDF),IVAR(NFMAX+1,MXCDF),IFILE,ISYNFLG,
     X     ICDUNT(MXCDF),IUSWRP
      CHARACTER*2 NAMF
      CHARACTER*8 FLDNM(NFMAX),namlnd(MAXLAND)
      CHARACTER*80 GRIDTYPE
      INTEGER TEMP(400)
      INTEGER K,K1,K2
      REAL  X(MAXLAND),Y(MAXLAND),Z(MAXLAND)
      REAL  SF
      INTEGER SCALE(NFMAX),FVARS(NFMAX)
      INTEGER START,COUNT
      INTEGER DIMIDS(NCMAXDIM)
      INTEGER VARIDS(NCMAXVAR)
      START = 1
      COUNT = 1

C     WRITE OUT NETCDF DIMENSIONS

      CALL WCDFDIMS(ICDFID(IFILE),CSP,NCX,ID(175),
     x              ID(68),ID(69),ID(303),ID(302),
     x              DIMIDS,GRIDTYPE,LOUTNM)


C     TURN OFF THE AUTOMATIC FILLING
      OMODE = NCSFIL(ICDFID(IFILE),NCNOFILL,RCODE)

C     GET NETCDF VARIABLE IDS FOR MOST OUTPUT VARIABLES
      CALL CDFVIDS(ICDFID(IFILE),DIMIDS,VARIDS)

C     WRITE OUT THE 510 WORD HEADER TEXT INFORMATION
      CALL WCDFIDTX(ID,ICDFID(IFILE),DIMIDS,VARIDS)

      DO I = 1,400
         TEMP(I) = ID(I)
      END DO

C     WRITE OUT THE DATE TIME INFORMATION
      CALL WCDFDTTI(ICDFID(IFILE),TEMP,VARIDS)

C     WRITE OUT GRID INFORMATION
      CALL WCDFGRDI(ICDFID(IFILE),TEMP,VALLEV,DIMIDS,VARIDS,
     x              GRIDTYPE)


CWRITE OUT THE NETCDF MISCELANOUS VARIABLES.   
         CALL WCDFMISC(ICDFID(IFILE),TEMP,DIMIDS,VARIDS)


C       WRITE LANDMARK INFORMATION
        N = ID(302)
        K1 = 306
        SF = 1./ID(68)
        DO K = 1, N
           WRITE(NAMLND(K),10) ID(K1),ID(K1+1),ID(K1+2) 
            X(K) = ID(K1 + 3) * SF
            Y(K) = ID(K1 + 4) * SF
            Z(K) = ID(K1 + 5)
            K1 = K1 + 6
        END DO
        
 10     FORMAT(3A2) 
         CALL WCDFLDMK(ICDFID(IFILE),ID(302),NAMLND,X,Y,Z,
     X                 VARIDS)



C      WRITE FIELD INFORMATION
         N = ID(175)
         DO I=1,N
            FLDNM(I) = '        '
            WRITE(FLDNM(I),15)ID(176+(I-1)*5),ID(177+(I-1)*5),
     X                      ID(178+(I-1)*5),ID(179+(I-1)*5)
            SCALE(I) =  ID(175+I*5)
 15         FORMAT(4A2)
         END DO

         CALL WCDFFLDI(ICDFID(IFILE),FLDNM,N,SCALE,
     x                 DIMIDS,FVARS)

         DO I = 1, ID(175)
            IVAR(I,IFILE) = FVARS(I)
         END DO          

         RETURN
         END
C------------------------------------------------------------------
CWRITE NETCDF ID TEXT
      SUBROUTINE WCDFIDTX(ID,FUNIT,DIMIDS,VARIDS)

      INCLUDE '/opt/local/netcdf-3/include/netcdf.inc'
      INCLUDE 'CEDRIC.INC'
      PARAMETER (NCMAXDIM = 100)
      PARAMETER (NCMAXVAR = 2000)
      DIMENSION ID(NID)
      
      INTEGER FUNIT,IDH(10),RCODE,COUNT,BADLL
      INTEGER DIMIDS(NCMAXDIM),IVAR,NUM
      INTEGER VARIDS(NCMAXVAR)
      INTEGER*2 ITEMP
      COMMON /UNITS/ LIN,LOUT,LPR,LSPOOL
      COMMON /AXUNTS/ IUNAXS,LABAXS(3,3),SCLAXS(3,3),AXNAM(3)
      CHARACTER*4 AXNAM
      REAL SF

      ITEMP = 0
      IVAR = 1
      NUM = 6
      CALL WCDFTEXT(FUNIT,VARIDS,ID(13),ID(14),ID(15),
     X              ITEMP,NUM,IVAR)
      
      NUM = 4
      IVAR = 2
      CALL WCDFTEXT(FUNIT,VARIDS,ID(8),ID(9),ITEMP,ITEMP,
     X              NUM,IVAR)

      NUM = 6      
      IVAR = 3
      CALL WCDFTEXT(FUNIT,VARIDS,ID(10),ID(11),ID(12),
     X              ITEMP,NUM,IVAR)

      IVAR = 4
      CALL WCDFTEXT(FUNIT,VARIDS,ID(48),ID(49),ID(50),
     X              ITEMP,NUM,IVAR)


      IVAR = 5
      NUM = 8
      CALL WCDFTEXT(FUNIT,VARIDS,ID(71),ID(72),ID(73),
     X              ID(74),NUM,IVAR)

      IVAR = 6
      CALL WCDFTEXT(FUNIT,VARIDS,ID(101),ID(102),ID(103),
     X              ID(104),NUM,IVAR)            


      iyear = id(116)
      if(id(116) .gt. 100) iyear = MOD(ID(116),100)

      WRITE(LPR,25) (ID(I),I=1,4),ID(117),ID(118),iyear,
     X   (ID(I),I=71,74),(ID(I),I=10,12),(ID(I),I=119,121),
     X   (ID(I),I=13,15),(ID(I),I=48,50),(ID(I),I=125,127),
     X   (ID(I),I=5,7),  (ID(I),I=51,54),(ID(I),I=101,104),
     X    ID(8),ID(9),(ID(I),I=55,58),(ID(I),I=16,20),
     X    (ID(I),I=45,47)

 25    FORMAT(/' MUDRAS (.MUD)  VOLUME HEADER',15X,4A2
     X   /'  GENERAL INFORMATION...'
     X/' DATE:      ',I2.2,2('/',I2.2),5X,
     X 'SOURCE:  ',4A2,3X,'SCIENTIST: ',3A2,
     X/' BEG TIME:  ',I2.2,2(':',I2.2),5X,
     X 'RADAR:   ',3A2,5X,'SUBMITTER: ',3A2,
     X/' END TIME:  ',I2.2,2(':',I2.2),5X,
     X 'PROGRAM: ',3A2,5X,'DATE RUN:  ',4A2,
     X/' VOL. NAME: ', 4A2,      5X,
     X 'PROJECT: ',2A2,7X,'TIME RUN:  ',4A2,
     X/' COORD SYS: ', 2A2,      9X,'TAPE:    ',3A2,5X,
     X 'SEQUENCE:  ',3A2)


      WRITE(LPR,30) ID(62),ID(96),ID(301),ID(63),ID(97),ID(106),
     X                ID(65),ID(99),ID(67),ID(451),ID(452),ID(453)
 30   FORMAT(/'  DATA CHARACTERISTICS...'
     X/' COMPUTER:   ',3X,A2,5X,'RECS/FIELD:  
     X',I3,5X,'PTS/FIELD:  ',I6
     X/' BITS/DATUM: ',I5,   5X,'RECS/PLANE:  ',
     X  I5,5X,'NO. PLANES: ',I6
     X/' BLOCK SIZE: ',I5,   5X,'RECS/VOLUME: ',
     X I5,5X,'BAD DATA:   ',I6
     X/' WORDS/PLANE:',I5,   5X,'WORDS/FIELD: ',
     X I5,5X,'MAX FIELDS: ',I6)   
  
      N=ID(175)
      WRITE(LPR,35) N
 35   FORMAT(/'  FIELDS PRESENT: ',I2,' ...'
     X       /4X,'NO.',3X,'NAME',7X,'SCALE FACTOR')
      K2=175
      DO I=1,N
         K1=K2+1
         K2=K2+5
      WRITE(LPR,40) I,ID(K1),ID(K1+1),ID(K1+2),ID(K1+3),ID(K1+4)
 40   FORMAT(4X,I3,3X,4A2,5X,I5)
      END DO

      SF=1./ID(68)
      N=ID(302)
      WRITE(LPR,45) N,ID(303)
 45   FORMAT(/'  LANDMARKS PRESENT: ',I2,5X,'(',I2,' RADAR) ...'
     X   /4X,'NO.',3X,'NAME',6X,'X (KM)',4X,'Y (KM)',4X,'Z (KM)')
      K=306
      DO I=1,N
        R1=ID(K+3)*SF
        R2=ID(K+4)*SF
        R3=ID(K+5)*0.001
        WRITE(LPR,50) I,ID(K),ID(K+1),ID(K+2), R1,R2,R3
 50     FORMAT(4X,I3,3X,3A2,2F10.2,F10.3)
        K=K+6
      END DO

      IF(ID(303).NE.1) GO TO 51
C
C        WRITE OUT RADAR SPECS IF SINGLE RADAR
C
      R1=ID(304)*SF
      R2=ID(305)*SF
      WRITE(LPR,55) R1,R2
 55   FORMAT('  NYQUIST VELOCITY:',F8.2,7X,'RADAR CONSTANT:',F8.2)
 51   CONTINUE

      R1=ID(35)*SF
      R2=ID(38)*SF
      WRITE(LPR,111) ID(33),ID(34),R1,ID(36),ID(37),R2
  111 FORMAT(/'  ORIGIN  LATITUDE:',I8,' DEG',I6,' MIN',F9.2,' SEC'
     X       /9X,      'LONGITUDE:',I8,' DEG',I6,' MIN',F9.2,' SEC')


      WRITE(LPR,65)
 65   FORMAT(/'  CARTESIAN COORDINATE SYSTEM SPECIFICATIONS...'
     X/3X,'AXIS',11X,'MINIMUM    ',5X,'MAXIMUM     ',5X,'DELTA ',
     X7X,'NO. OF PTS.')
      K=160
C        CALCULATE KILOMETERS FROM METERS FOR Z-AXIS
      CKM=1.0
      DO I=1,3
C        MARK BRADFORD PATCH TO ACCOUNT FOR ID(68).NE.100
         IF(I.EQ.3) CKM=FLOAT(ID(68))/1000.
         R1=ID(K)*SF*CKM
         R2=ID(K+1)*SF*CKM
         R3=ID(K+3)*0.001
      WRITE(LPR,70) AXNAM(I),R1,LABAXS(I,1),R2,LABAXS(I,1),
     X                R3,LABAXS(I,1),ID(K+2)
 70   FORMAT(5X,A1,6X,F10.3,1X,A3,3X,F10.3,1X,A3,4X,F8.3,1X,A3,3X,I5)
         K=K+5
      END DO
      L1=ID(164)
      L2=ID(169)
      L3=ID(174)
      CF=1./ID(69)
      R1=ID(40)*CF
      XOR=ID(41)*SF
      YOR=ID(42)*SF
      PRINT *,"  "
      PRINT *,"  GENERAL SCALING FACTOR = ",ID(68)
      PRINT *,"    ANGLE SCALING FACTOR = ",ID(69)
      WRITE(LPR,80) AXNAM(L1),AXNAM(L2),AXNAM(L3),R1,XOR,YOR
 80   FORMAT(/3X,'AXIS ORDER IS   ',3A3,
     X    /3X,'ANGLE OF THE X-AXIS RELATIVE TO NORTH: ',F9.2,4X,'DEG.',
     X    /3X,'(X,Y)  AXIS ARE SPECIFIED RELATIVE TO:  (',
     X                F7.2,',',F7.2,')')

      RETURN
      END


