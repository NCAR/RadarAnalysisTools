      SUBROUTINE READVL (KRD,IBUF,RBUF,MAP,LIN,LPR,ICORD,
     X                   GFIELD,LATLON)
C
C        READ IN WHOLE Z-VOLUME AND PUT INTO LARGE CORE MEMORY
C

      INCLUDE 'CEDRIC.INC'
      COMMON /CREAXS/ CSPN(3,3),NCXN(3),NPLNEW
      COMMON /IOTYPE/ ICDF
      COMMON /CDFNET/ ICDFID(MXCDF),IDIMDAT(4,MXCDF),
     X     IDIMID(1,MXCDF),IVAR(NFMAX+1,MXCDF),IFILE,ISYNFLG,
     X     ICDUNT(MXCDF),IUSWRP
      COMMON /MDV/ MDVXMIN,MDVXMAX,MDVYMIN,MDVYMAX,MDVSP,MDVFLG
      REAL MDVXMIN,MDVXMAX,MDVYMIN,MDVYMAX,MDVSP
      REAL RETIM
      INTEGER MDVFLG
      COMMON /FMTTYPE/ WRFFLG
      INTEGER WRFFLG,WRFZ,wrfdbg
      COMMON /WRF/ WRFINTRP,CALPLACD,wrfdbg,XMIN,XMAX,YMIN,YMAX,
     X             ZMIN,ZMAX,DX,DY,DZ
      COMMON /LEVELS/ VALLEV(MAXZLEV),VALNYQ(MAXZLEV),VNYQ_VOL
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X                IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X                NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF
      COMMON /AXUNTS/ IUNAXS,LABAXS(3,3),SCLAXS(3,3),AXNAM(3)
      CHARACTER*4 AXNAM
      INTEGER COORD,calplacd,WRFINTRP
      DIMENSION ILHD(10),IBUF(MAXPLN),RBUF(MAXPLN)
      CHARACTER*(*) KRD(10)
      CHARACTER*8 INVOL,GFIELD(NFMAX)
      CHARACTER*1 IREW
      CHARACTER*2 CTEMP4(4),CSTEMP(2)
      CHARACTER*3 ITEST
      CHARACTER*4 CTEMP1,CTEMP2,CTEMP3
      DIMENSION MAP(MAXAXIS,3)
      EQUIVALENCE (ID(453),MAXFLD)
      LOGICAL LATLON

      print *,'READVL: KRD=',KRD
      print *,'READVL: ibuf(1),rbuf(1),map(1,3)=',
     +     ibuf(1),rbuf(1),map(1,3)
      print *,'READVL: lin,lpr,icord,nfmax=',
     +     lin,lpr,icord,nfmax
      print *,'READVL: gfield=',gfield
      calplacd = 1
      IFILE=0
      IETIM=240000
      GFIELD(1) = '        '
      GFIELD(2) = '        '     
      MDVFLG = 0  
      WRFFLG = 0    
      READ (KRD,101)RIN,INVOL,RBTIM,RETIM,IREW,(GFIELD(I),I=1,2)
 101  FORMAT(/F8.0/A8/F8.0/F8.0/A1/A8/A8)
C--------MDV INTERPOLATION PATH
      if(GFIELD(1) .EQ. "MDV") THEN
        CALL KARDIN(KRD)
        CALL COMCHK(IPR,KRD)
        READ(KRD,104)MDVXMIN,MDVXMAX,MDVYMIN,MDVYMAX,MDVSP
 104    FORMAT(/F8.0/F8.0/F8.0/F8.0/F8.0)
        MDVFLG = 1
        CALL KARDIN(KRD)
        CALL COMCHK(IPR,KRD)
        READ (KRD,105)ITEST
 105    FORMAT(A3)
        IF(ITEST .NE. "END") THEN
           PRINT *,"PLEASE END MDV READVL WITH AN END"
           STOP
        ENDIF

C     Make sure user-specified grid is well-formed
C
        print *,'READVL: grid=',MDVXMIN,MDVXMAX,MDVYMIN,MDVYMAX,MDVSP
        MX = 1.001+ (MDVXMAX-MDVXMIN)/MDVSP
        MY = 1.001+ (MDVYMAX-MDVYMIN)/MDVSP
        MDVXMAX = MDVXMIN + (MX-1)*MDVSP
        MDVYMAX = MDVYMIN + (MY-1)*MDVSP
        print *,'READVL: grid=',MDVXMIN,MDVXMAX,MDVYMIN,MDVYMAX,MDVSP
      ENDIF
C-----WRF MODEL PATH
      IF(GFIELD(1) .EQ. "WRF") THEN
c--------call rundate(krd)
         WRFFLG = 1
      ENDIF
      
      wrfdbg = 0
      if(gfield(2) .eq."DEBUG") wrfdbg = 1
      IF(MDVFLG .NE.1 .AND. WRFFLG .NE. 1) THEN
         CALL SVGDFLDN(GFIELD(1),GFIELD(2))
      ENDIF
      LIN=RIN
      IF(LIN.LE.0) THEN
         CALL CEDERX(521,1)
         RETURN
      END IF
      IBTIM=RBTIM
C      IF (RETIM.GT.0.0) IETIM=RETIM
      IF (INVOL.EQ.'        ') INVOL='NEXT    '
 100  CONTINUE
      DO I = 1,NID
         INPID(I) = 0
      END DO
      CALL SETVOL(LIN,INVOL,IBTIM,IETIM,LPR,LASTLV,IREW,INPID,
     X            ISTAT,GFIELD)
      IF (ISTAT.NE.0) RETURN
 57   format(3a4)
 557  format(3a2)
      IF (INPID(174).NE.3) THEN
         CALL CEDERX(529,1)
         RETURN
      END IF
C
C        VOLUME FOUND - COPY INPID TO ID
C
      CALL COPIX(ID,INPID,NID)

C
C     SET AXIS LABELS, IF REQUESTED
C
      IF (ICORD.EQ.0) THEN
         WRITE(CSTEMP(1),5) ID(16)
 5       FORMAT(A2)
         WRITE(CSTEMP(2),5) ID(17)
         CALL SAVCRDSY(CSTEMP)
         CALL SAXISLBL(AXNAM,CTEMP1,CTEMP2,CTEMP3,COORD)
         CTEMP1(4:4)=' '
         CTEMP2(4:4)=' '
         CTEMP3(4:4)=' '
         READ(CTEMP1,10)LABAXS(1,1)
         READ(CTEMP2,10)LABAXS(2,1)
         if(wrfintrp .eq. 1) then
            ctemp3 = "MB  "
            axnam(3) = "P"
         endif 
         if(wrfintrp .eq. 2) then
            ctemp3 = "GPT "
            axnam(3) = "Z"
         endif
         READ(CTEMP3,10)LABAXS(3,1)
 10      FORMAT(A3)
         IF(COORD .EQ. 1) THEN
            LATLON = .TRUE.
         ELSE
            LATLON = .FALSE.
         END IF
         print *,'READVL: lin,icdf=',lin,icdf
         print *,'READVL: axnam,ctemp,latlon=',axnam,
     +        ctemp1,ctemp2,ctemp3,latlon
      END IF

      CALL NEWHED(0)
      IF (NPLNEW.NE.0) THEN
C
C                 RESTRUCTURE
C
         WRITE(LPR,102)
  102    FORMAT(//' INPUT VOLUME SUMMARY ...')
         CALL IMHSUM(LPR,INPID)
            CALL COPRX(CSP,CSPN,9)
            CALL COPIX(NCX,NCXN,3)
            NPLANE=NPLNEW
            CALL UPDHED(0)
            CALL IDCMPR(INPID,ID,NID,IST)
            IF (IST.NE.0) THEN
               print *,'READVL: right after IDCMPR, ist=',ist
               CALL CEDERX(512,1)
               RETURN
            END IF
      ENDIF

      CALL CRTMAP(INPID,NID,CSP,NCX,MAP,MAXAXIS,JST)
      IF (JST.EQ.2) THEN
         print *,'READVL: right after CRTMAP, jst=',jst
         CALL CEDERX(512,1)
         RETURN
      END IF
      CALL DSINIT(0)
C
C           PULL IN DATA FOR ALL FIELDS
C
      NFLDS=INPID(175)
      IF(NFLDS.GT.MAXFLD) THEN
         CALL CEDERX(505,1)
         RETURN
      END IF
      NTX=INPID(162)
      NTY=INPID(167)
      NPLIN=INPID(301)
      NZ=NCX(3)

      SF=1./INPID(68)
      VNYQ_VOL=INPID(304)*SF
      CKM=FLOAT(INPID(68))/1000.
      K=170
      ZMIN=INPID(K)*SF*CKM
      ZMAX=INPID(K+1)*SF*CKM
      ZINC=INPID(K+3)*0.001
      print *,'READVL: nplin,nflds,jst=',nplin,nflds,jst
      print *,'READVL:        nx,ny,nz=',ntx,nty,nz
      print *,'READVL:  zmin,zmax,zinc=',zmin,zmax,zinc
      print *,'READVL:        vnyq_vol=',vnyq_vol
      DO 150 KOT=1,NZ
         KIN=MAP(KOT,3)
         DO 140 LF=1,NFLDS
            IF(KIN.LE.0) GO TO 120
            INF=171+(5*LF)
            WRITE(CTEMP4(1),113)INPID(INF)
 113        FORMAT(A2)
            WRITE(CTEMP4(2),113)INPID(INF+1) 
            WRITE(CTEMP4(3),113)INPID(INF+2)
            WRITE(CTEMP4(4),113)INPID(INF+3)
            IF (JST.NE.0) THEN
C
C                    RESHUFFLE
C
               CALL FETCHZ(LIN,IBUF,RBUF,NPLIN,LASTLV,LASTFD,KIN,
     X              ZLEV,CTEMP4,BAD,INPID,ILHD)
               CALL RESHUF(RBUF,NCX(1),NCX(2),IBUF,NTX,NTY,MAP,MAXAXIS,
     X              BAD)
            ELSE
               CALL FETCHZ(LIN,RBUF,IBUF,NPLIN,LASTLV,LASTFD,
     X              KIN,ZLEV,CTEMP4,BAD,INPID,ILHD)
            END IF
            GOTO 125
 120        CONTINUE
            CALL CONFLD(RBUF,NPLANE,BAD)
 125        CONTINUE
            if(calplacd .eq. 1) then
               CALL PLACED(0,ID,KOT,LF,IBUF,RBUF,NCX(1),
     X                    NCX(2),3,BAD,NST)
            endif
    
 140     CONTINUE
 150  CONTINUE
C
C        POSITION PAST END OF VOLUME SCAN 
C
      IF (ICDF.EQ.0 .OR. ICDF.EQ.1) CALL SKPVOL(LIN,1)
      WRITE(LPR,103)
 103  FORMAT(//' EDIT FILE SUMMARY ...')
      CALL IMHSUM(LPR,ID)
      WRITE(99,103)
      CALL IMHSUM(99,ID)
c-----print *,"after IMHSUM"
C
C     CLOSE WRF NETCDF FILE
C     
      IF(WRFFLG .EQ. 1) THEN
c--------CALL CLOSETMP()
c--------CALL RMTEMP()
      ENDIF

      IF (ICDF.EQ.2) THEN
            CALL NETCLOS()
      END IF
      RETURN
      END


C------------------------------------------------------------
      SUBROUTINE GETVAR(KRD)

      CHARACTER*(*) KRD(10)
      CHARACTER*8 NAMES(25),TEMP
      CHARACTER*3 ITEST
      
      CALL initfldn(NAMES)
      
      READ (KRD,100) TEMP,(NAMES(I),I=1,9)
 100  FORMAT(A8/A8/A8/A8/A8/A8/A8/A8/A8/A8)

      CALL KARDIN(KRD)
      CALL COMCHK(IPR,KRD)
      READ(KRD,105) ITEST
 105  FORMAT(A3)
      IF(ITEST .NE. "END") THEN
         READ (KRD,100) TEMP,(NAMES(I),I=10,18)
      ENDIF
      IF(ITEST .EQ. "END") goto 200

      CALL KARDIN(KRD)
      CALL COMCHK(IPR,KRD)
      READ(KRD,105) ITEST
      IF(ITEST .NE. "END") THEN
         READ (KRD,100) TEMP,(NAMES(I),I=19,25)
      ENDIF
      IF(ITEST .EQ. "END") goto 200


      CALL KARDIN(KRD)
      CALL COMCHK(IPR,KRD)
      READ(KRD,105) ITEST
      IF(ITEST .NE. "END") THEN
         print *,"please end the getvar block with an end"
         stop
      endif

 200  CONTINUE
      CALL ZNUMFLDS()
      CALL SUSRFLDN(NAMES)

      return
      end
