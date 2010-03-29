      SUBROUTINE WINSET(IWIND,PWIND,IWOP)
C
C        SETS USER SUPPLIED WINDOW ARRAYS (IWIND,PWIND) EITHER
C             TO FULL GRID OR TO ALTERNATE WINDOW DEPENDING UPON
C             THE VALUE OF IWOP: =W, THEN USE ALTERNATE WINDOW.
C

      INCLUDE 'CEDRIC.INC'      
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X                IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X                NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF
      COMMON /SETWND/ ISETW(2,3),PSETW(2,3),ISETFL(NFMAX)
      DIMENSION IWIND(2,3),PWIND(2,3)
      CHARACTER*1 IWOP,IWSET
      DATA IWSET/'W'/
C
C     COMMON block variables returned from LAT_LON or uses defaults.
C
      COMMON /HEMISPHERE/LATSPHERE,LONSPHERE,LAT_SIGN,LON_SIGN,LATLON
      CHARACTER*8 LATSPHERE,LONSPHERE
      REAL LAT_SIGN,LON_SIGN
      LOGICAL LATLON

c      print *,'Winset: isetw1=',latlon,iwop,iwset,isetw(1,1),isetw(2,1)
c      print *,'        isetw2=',latlon,iwop,iwset,isetw(1,2),isetw(2,2)
c      print *,'        isetw3=',latlon,iwop,iwset,isetw(1,3),isetw(2,3)
c      print *,'Winset: psetw1=',latlon,iwop,iwset,psetw(1,1),psetw(2,1)
c      print *,'        psetw2=',latlon,iwop,iwset,psetw(1,2),psetw(2,2)
c      print *,'        psetw3=',latlon,iwop,iwset,psetw(1,3),psetw(2,3)
      IF(IWOP.NE.IWSET) THEN
C
C        SET TO FULL WINDOW
C
         DO 10 L=1,3
            IWIND(1,L)=1
            IWIND(2,L)=NCX(L)
            IF(L.LE.2 .AND. LATLON)THEN
               PWIND(1,L)=0.1*NINT(10.0*CSP(1,L))
               PWIND(2,L)=0.1*NINT(10.0*CSP(2,L))
            ELSE
               PWIND(1,L)=CSP(1,L)
               PWIND(2,L)=CSP(2,L)
            END IF
c            print *,'Full window:',iwind(1,l),iwind(2,l)
c            print *,'            ',csp(1,l),csp(2,l)
c            print *,'            ',pwind(1,l),pwind(2,l)
   10    CONTINUE
      ELSE
C
C        SET TO ALTERNATE WINDOW
C
         DO 20 L=1,3
            IWIND(1,L)=ISETW(1,L)
            IWIND(2,L)=ISETW(2,L)
            IF(L.LE.2 .AND. LATLON)THEN
               PWIND(1,L)=0.1*NINT(10.0*PSETW(1,L))
               PWIND(2,L)=0.1*NINT(10.0*PSETW(2,L))
            ELSE
               PWIND(1,L)=PSETW(1,L)
               PWIND(2,L)=PSETW(2,L)
            END IF
c            print *,'Alt window:',iwind(1,l),iwind(2,l)
c            print *,'           ',psetw(1,l),psetw(2,l)
c            print *,'           ',pwind(1,l),pwind(2,l)
   20    CONTINUE
      END IF
      RETURN
      END




