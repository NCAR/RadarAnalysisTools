      SUBROUTINE CEDHEAD(SCINAME,SUBNAME,BASANG,VOLNAM,IBEGYR,
     X     IBEGMNT,IBEGDAY,IBEGHR,IBEGMIN,IBEGSEC,IENDYR,
     X     IENDMNT,IENDDAY,IENDHR,IENDMIN,IENDSEC,XMIN,
     X     XMAX,NUMX,ISPCX,YMIN,YMAX,NUMY,ISPCY,ZMIN,
     X     ZMAX,NUMZ,ISPCZ,NFLD,FLDNAM,ISCLFLD,NUMLND,
     X     NUMRAD,NAMLND,XLND,YLND,ZLND,MXLND,RADSTN,SOURCE,
     X     PROJECT,TAPE,RADCON,VNYQ,LATDEG,LATMIN,LATSEC,
     X     LONDEG,LONMIN,LONSEC,BAD)

C     This routine fills some of the 510-word CEDRIC header.
C     Replace with your own information.
C
      INCLUDE 'CEDRIC.INC'
      CHARACTER*4 PROJECT
      CHARACTER*6 SCINAME,SUBNAME,NAMLND(MXLND),RADSTN,TAPE
      CHARACTER*8 VOLNAM,FLDNAM(MAXFLD),SOURCE
      DIMENSION ISCLFLD(MAXFLD),XLND(MXLND),YLND(MXLND),ZLND(MXLND)
      DATA SCFAC/1000.0/

      SCINAME = 'LJMILL'
      SUBNAME = 'ANDERS'
      BASANG  = 90.0
      VOLNAM  = 'DiskFile'
      SOURCE  = 'F06527  '
      IBEGYR  = 92
      IBEGMNT = 3
      IBEGDAY = 9
      PROJECT = 'HARP'
      TAPE    = 'TPNAME'
      RADCON  = 68.23
      VNYQ    = 20.47
      BAD     = -1000.0

      IBEGHR  = 9
      IBEGMIN = 12
      IBEGSEC = 23
      IENDYR  = 92
      IENDMNT = 3
      IENDDAY = 9
      IENDHR  = 10
      IENDMIN = 15
      IENDSEC = 47

C     Latitude (N) and longitude (W) of Hilo airport Hawaii

      LATDEG = 19
      LATMIN = 43
      LATSEC = 24
      LONDEG = 155
      LONMIN = 03
      LONSEC = 29
      
      XMIN  = -100.
      XMAX  = 100.
      SPCX  = 0.5
      ISPCX = SPCX*SCFAC
      NUMX  = 1.0 + (XMAX-XMIN)/SPCX

      YMIN  = -150.
      YMAX  = 150.
      SPCY  = 1.0
      ISPCY = SPCY*SCFAC
      NUMY  = 1.0 + (YMAX-YMIN)/SPCY

      ZMIN  = 1.0*SCFAC
      ZMAX  = 10.0*SCFAC
      SPCZ  = 1.0
      ISPCZ = SPCZ*SCFAC
      NUMZ  = 1.0 + (ZMAX-ZMIN)/ISPCZ

      NFLD=3
      FLDNAM(1)  = 'Xgrid'
      ISCLFLD(1) = 100
      FLDNAM(2)  = 'Ygrid'
      ISCLFLD(2) = 100
      FLDNAM(3)  = 'Zgrid'
      ISCLFLD(3) = 100

      RADSTN     = 'CP-4  '
      NUMLND = 2
      NUMRAD = 1
      NAMLND(1) = 'ORIGIN'
      NAMLND(2) = 'CP-4  '
      XLND(1) = 0.0
      XLND(2) = 5.0
      YLND(1) = 0.0
      YLND(2) = 7.3
      ZLND(1) = 0.0
      ZLND(2) = 0.0

      RETURN
      END

      
