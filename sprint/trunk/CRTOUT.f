      SUBROUTINE CRTOUT(ICART,ICTAB,ICRTST,INPTST,IOP,XGRID,YGRID)
C
C        CRTOUT- GENERATES THE CARTESIAN COORDINATE SYSTEM BASED UPON THE
C             SPECIFICATIONS IN COMMON BLOCK /TRANS/ AND REORDERS IT IN
C             INCREASING AZIMUTH.
C     IOP=0 ==> (R,A,E -> CART OR COPL -> CART)
C     IOP=1 ==> RHI -> CART (PART 1)
C     IOP=2 ==> RHI -> CART (PART 2)
C     IOP=3 ==> AIRBORNE -> CART
C
C     Note: NPREC and MAXPLN are the same variable
C     February, 2000: Changed IPUTCP and IGETCP2 to allow as many as
C                     511 grid points.  This meant that integer scaled
C                     azimuths can no use only 13 bits, with indices in
C                     the next 9 bits each of 32-bit word (ICART).  Don't
C                     pack azimuth into sign bit.  Previously, 8 bits were
C                     used for the indices, allowing up to 255 x 255 grid
C                     points.  Scaling for azimuth changed from 32 --> 20
C                     in INITAL.f since now can only allow integer azimuth
C                     no bigger than 8191 (13 bits).  Largest grid could
C                     now be 511 x 511, but no bigger.
C
      INCLUDE 'SPRINT.INC'
c-----PARAMETER (MAXEL=150,NID=129+3*MAXEL)
c-----PARAMETER (MAXPLN=65536)
c-----PARAMETER (MAXFLD=16)

      COMMON /IDBLK/ID(NID)
      DIMENSION ICART(MAXPLN),ICTAB(4),AZC(4)
      DIMENSION XGRID(MAXPLN),YGRID(MAXPLN)
      LOGICAL IS360
      COMMON /SCAN/ ICOPLANE,IFLGBAS,IPPI,ILLE,ILLZ
      COMMON /RHIS/ IRHICRS,LOWAZ2,MAXAZ2,MINAZ2,ICART2(MAXPLN),
     X     ICTAB2(4),KZV(3),IDSV
      COMMON /TRANS/ X1,X2,XD,Y1,Y2,YD,Z1,Z2,ZD,NX,NY,NZ,XORG,YORG,
     X     ANGXAX,ZRAD,AZLOW,BAD,ASNF,ACSF,IAXORD(3),NPLANE,EDIAM
      COMMON /SETAZM/ USAZ1,USAZ2,USGAP,USINC,IFOR36,IFORDR
C
C        COMMON BLOCK  /ADJUST/
C
C        SCLDBZ- SCALE FACTOR-  PACKED QUANTITY TO REFLECTIVITY FACTOR (DBZ)
C        SCLAZ-                 PACKED QUANTITY TO AZIMUTH (DEG)
C        SCLRNG-                PACKED QUANTITY TO GATE NUMBER
C        UNSCDB- INVERSE OF  SCLDBZ
C        UNSCAZ- INVERSE OF  SCLAZ
C        UNSCRG- INVERSE OF  SCLRNG
C
      COMMON/ADJUST/INFLD(MAXFLD,3),SCLIF(MAXFLD,2),NIF,IOFLD(MAXFLD,3),
     X     SCLOF(MAXFLD,2),NOF,SCLAZ,SCLRNG,SCLEL,UNSCAZ,UNSCRG,UNSCEL,
     X     LOWAZ,IZAD,IS360,MINAZ,MAXAZ,METSPC(MAXFLD),IWFLD(MAXFLD),
     X     NFLI,SCLNYQ,UNSNYQ
      COMMON /FNAMES/ CINFLD(MAXFLD),CIOFLD(MAXFLD)
      CHARACTER*8 CINFLD, CIOFLD
C
C     Common blocks containing lat/lon for the origin and the radar.
C     Set in routine USRORIGIN with ORIGIN command.
C
      COMMON /CINPC/ NMRAD,ITAP,TAPE_UNIT
      CHARACTER*4 NMRAD
      CHARACTER*8 ITAP
      CHARACTER*8 TAPE_UNIT
      COMMON /CINP/IUN,ISKP,ORLAT,ORLON
      COMMON /RINP/RADLAT,RADLON,RADALT

      DATA SLOPDG/2.0/
      DATA EPS/1.E-8/
      DATA DTR /0.0174533/
      DATA ICOPLKP,IRHIDN/-1,-1/
      DATA NMOD/20/

C        FUNCTIONS FOR CONVERTING (X,Y) GRID TO RADAR SPACE
C
      FXC(X,Y) = ((ACSF*X) - (ASNF*Y)) + XORTR
      FYC(X,Y)=  ((ASNF*X) + (ACSF*Y)) + YORTR

c-----debug (ljm)
      if(debug)then
         write(8,*) 'CRTOUT: begin debugging printout'
         write(8,*) 'Crtout: icrtst  =',icrtst
         write(8,*) 'Crtout: inptst  =',inptst
         write(8,*) 'Crtout: icoplkp =',icoplkp
         write(8,*) 'Crtout: irhidn  =',irhidn
         write(8,*) 'Crtout: icoplane=',icoplane
         write(8,*) 'Crtout: ppi-lle-llz=',ippi,ille,illz
         write(8,*) 'Crtout: before reordering of grid, iop=',iop
         write(8,*) 'Crtout-radar: ',nmrad,radlat,radlon,angxax
         write(8,*) 'Crtout-orign: ',orlat,orlon,xorg,yorg,zrad
         write(8,*) 'Crtout-xgrid: ',x1,x2,xd,nx
         write(8,*) 'Crtout-ygrid: ',y1,y2,yd,ny
         write(8,*) 'Crtout-zgrid: ',z1,z2,zd,nz
      end if
c-----debug (ljm)

C
C        CHECK IF REORDERING IS NECESSARY 
C
      IF (IOP.EQ.3) THEN
C
C     AIRBORNE --> CART: DO A TEMPORARY AXIS SWITCH TO FOOL CRTOUT
C

         T1=Y1
         T2=Y2
         TD=YD

         Y1=Z1
         Y2=Z2
         YD=ZD

         Z1=T1
         Z2=T2
         ZD=TD

         NT=NY
         NY=NZ
         NZ=NT
         
         XORG=0.0
         YORG=ZRAD
         ZRAD=0.0
      ELSE IF (IOP.EQ.2) THEN
C
C     RHI --> CART: FOR RHI SCANS, INTERCHANGE X->Z, Y->X, Z->Y TO TAKE
C                   ADVANTAGE OF EXISTING COPLANE INTERPOLATION CODE.
C
         T1=X1
         T2=X2
         TD=XD

         X1=Y1
         X2=Y2
         XD=YD
         
         Y1=Z1
         Y2=Z2
         YD=ZD

         Z1=T1
         Z2=T2
         ZD=TD

         NT=NX
         NX=NY
         NY=NZ
         NZ=NT

         IF (ANGXAX.NE.90.0) THEN
            DHETA=(ANGXAX-90.0)*DTR
            XORR=FLOAT(ID(47))/100.*COS(DHETA) - FLOAT(ID(48))/100.
     X           *SIN(DHETA)
            YORR=FLOAT(ID(47))/100.*SIN(DHETA) + FLOAT(ID(48))/100.
     X           *COS(DHETA)
         ELSE
            XORR=FLOAT(ID(47))/100.
            YORR=FLOAT(ID(48))/100.
         END IF

         TORG=XORR
         XORG=YORR
         YORG=ZRAD
         ZRAD=TORG
      END IF

c-----debug (ljm)
      if(debug)then
         write(8,*) 'Crtout: after reordering of grid, iop=',iop
         write(8,*) 'Crtout-orign: ',orlat,orlon,torg,xorg,yorg,zrad
         write(8,*) 'Crtout-xgrid: ',x1,x2,xd,nx
         write(8,*) 'Crtout-ygrid: ',y1,y2,yd,ny
         write(8,*) 'Crtout-zgrid: ',z1,z2,zd,nz
      end if
c-----debug (ljm)

      IF(ICRTST.EQ.0.AND.INPTST.EQ.0.AND.ICOPLKP.EQ.ICOPLANE.AND.
     X     ((ICOPLANE.EQ.4.AND.IRHIDN.EQ.1) .OR. ICOPLANE.NE.4)) THEN

c-----debug (ljm)
         if(debug)then
            if(is360)then
               write(8,*) 'Crtout: 360 deg scan'
            else
               write(8,*) 'Crtout: not a 360 deg scan'
            end if
            write(8,*) 'CRTOUT: #1 - returning to UFCART'
            write(8,*) 'CRTOUT: #1 - end debugging printout'
         end if
c-----debug (ljm)

         RETURN
      END IF

      IF (IOP.EQ.1) THEN
C
C     RHI --> CART: FOR RHI SCANS, SPECIAL MODS HAD TO BE INTRODUCED TO DEAL 
C     WITH THE FIXED ANGLE CROSSING 90 OR 270. THIS CODE IS FOR THAT PURPOSE.
C
         T1=X1
         T2=X2
         TD=XD
         
         X1=Z1
         X2=Z2
         XD=ZD

         Z1=Y1
         Z2=Y2
         ZD=YD
         
         Y1=T1
         Y2=T2
         YD=TD

         NT=NX
         NX=NZ
         NZ=NY
         NY=NT

         IF (ANGXAX.NE.90.0) THEN
            DHETA=(ANGXAX-90.0)*DTR
            XORR=FLOAT(ID(47))/100.*COS(DHETA) - FLOAT(ID(48))/100.
     X           *SIN(DHETA)
            YORR=FLOAT(ID(47))/100.*SIN(DHETA) + FLOAT(ID(48))/100.
     X           *COS(DHETA)
         ELSE
            XORR=FLOAT(ID(47))/100.
            YORR=FLOAT(ID(48))/100.
         END IF

         TORG=XORR
         XORG=ZRAD
         ZRAD=YORR
         YORG=TORG
         IRHIDN=-1
      END IF

c-----debug (ljm)
      if(debug)then
         write(8,*) 'Crtout: after reordering of grid, iop=',iop
         write(8,*) 'Crtout-orign: ',orlat,orlon,torg,xorg,yorg,zrad
         write(8,*) 'Crtout-xgrid: ',x1,x2,xd,nx
         write(8,*) 'Crtout-ygrid: ',y1,y2,yd,ny
         write(8,*) 'Crtout-zgrid: ',z1,z2,zd,nz
      end if
c-----debug (ljm)
      
      IF(ICOPLANE .NE. 5) THEN 
         ICOPLKP=ICOPLANE
         INPTST=0
         ICRTST=0
      END IF
      IF (ICOPLANE.EQ.4) THEN
         ANGXAXT=ANGXAX
         ANGXAX=90.0
      END IF
      IF (IOP.EQ.2) IRHIDN=1
C
C        ATR- DEGREES TO RADIANS CONVERSION FACTOR
C        RTA- RADIANS TO DEGREES CONVERSION FACTOR
C
      ATR=ATAN(1.)/45.
      RTA=1./ATR
      IZAD=360. * UNSCAZ
C
C        COMPUTE BEGINNING AND ENDING AZIMUTH OF CARTESIAN COORDINATE
C             SYSTEM RELATIVE TO THE RADAR
C
      IF (ICOPLANE.NE.5) THEN
         ANGR=AMOD((450.-ANGXAX),360.)*ATR
         ASNF=SIN(ANGR)
         ACSF=COS(ANGR)
      ELSE
         ASNF=0.
         ACSF=1.
      END IF

c      IF(IPPI.EQ.1 .AND. ILLE.EQ.1)THEN

      write(8,*) 'Crtout: icoplane=',icoplane
      write(8,*) 'Crtout: ppi-lle-llz=',ippi,ille,illz

      IF(ILLE.EQ.1 .OR. ILLZ.EQ.1)THEN
C
C     (R,A,E) --> (Lon,Lat,Elev or Height): Will always be 360 deg 
C        and oriented with angle of +X-direction eastward (ANGXAX=90)
C
         IS360=.TRUE.
         LOWAZ=0
         MINAZ=0
         MAXAZ=IZAD
         AZLOW=0.0
         write(8,*) 'Crtout: (RAE --> LLE or LLZ) always 360 - go to 10'
         GO TO 10
      END IF
C
C        XORTR,YORTR IS LOWER LEFT CORNER OF CARTESIAN COORDINATE SYSTEM
C                    RELATIVE TO THE RADAR AT (XORG,YORG).
C
      XORTR = X1*ACSF - Y1*ASNF - XORG
      YORTR = X1*ASNF + Y1*ACSF - YORG
      write(8,*) 'Crtout: xortr,yortr =',xortr,yortr
C
C        AZLOW- CORRECTION FACTOR FOR 360 DEGREE CROSSOVER
C             0- IF NO CROSSOVER,  180- IF CROSSOVER EXISTS
C
      AZLOW=0.0
      XLL=FXC(0.,0.)
      YLL=FYC(0.,0.)
      XTL=FXC(0.,Y2-Y1)
      YTL=FYC(0.,Y2-Y1)
      XTR=FXC(X2-X1,Y2-Y1)
      YTR=FYC(X2-X1,Y2-Y1)
      XLR=FXC(X2-X1,0.)
      YLR=FYC(X2-X1,0.)
      write(8,*) 'Crtout:     xll,yll =',xll,yll
      write(8,*) 'Crtout:     xlr,ylr =',xlr,ylr
      write(8,*) 'Crtout:     xtl,ytl =',xtl,ytl
      write(8,*) 'Crtout:     xtr,ytr =',xtr,ytr
C 360 SCANS ONLY
      write(8,*) 'Crtout:       ifor36=',ifor36
      IF (IFOR36.NE.0) GOTO 2
      IS360=.FALSE.
      XRAD= -ACSF*XORTR-ASNF*YORTR + X1
      YRAD=  ASNF*XORTR-ACSF*YORTR + Y1
      write(8,*) 'Crtout:   xrad,yrad =',xrad,yrad
      IF(XRAD.LT.X1.OR.XRAD.GT.X2.OR.YRAD.LT.Y1.OR.YRAD.GT.Y2) GO TO 3
 2    CONTINUE
      IS360=.TRUE.
      LOWAZ=0
      MINAZ=0
      MAXAZ=IZAD
      GO TO 10
    3 CONTINUE
C  SECTOR SCANNING ONLY
      AZC(1)=ATAN2(XLL,YLL)*RTA
      AZC(2)=ATAN2(XTL,YTL)*RTA
      AZC(3)=ATAN2(XTR,YTR)*RTA
      AZC(4)=ATAN2(XLR,YLR)*RTA
      AZ1=1000.
      AZ2=-1000.
      DO 4 I=1,4
         IF(AZC(I).LT.0.0) AZC(I)=AZC(I)+360.
         IF(AZC(I).LT.AZ1) AZ1=AZC(I)
         IF(AZC(I).GT.AZ2) AZ2=AZC(I)
    4 CONTINUE
      write(8,*) 'Crtout:    azc(1-4) =',azc
      write(8,*) 'Crtout:     az1,az2 =',az1,az2
C
C        CHECK FOR 360 DEGREE CROSSOVER
C
      IF(AZ2-AZ1.LE.180.) GO TO 6
      AZ1=1000.
      AZ2=-1000.
      DO 5 I=1,4
         IF(AZC(I).GT.180.) AZC(I)=AZC(I)-360.
         IF(AZC(I).LT.AZ1) AZ1=AZC(I)
         IF(AZC(I).GT.AZ2) AZ2=AZC(I)
    5 CONTINUE
      IF(AZ1.LT.0.0) AZ1=AZ1+360.
      IF(AZ2.LT.0.0) AZ2=AZ2+360.
      AZLOW=180.
      GO TO 8
    6 CONTINUE
      IF(AZ1-SLOPDG.GE.0.0) GO TO 7
      AZLOW=270.
      GO TO 8
    7 CONTINUE
      IF(AZ2+SLOPDG.LE.360.) GO TO 8
      AZLOW=90.
    8 CONTINUE
      IF(AZ1.LT.AZLOW) AZ1=AZ1+360.
      IF(AZ2.LT.AZLOW) AZ2=AZ2+360.
      AZTL=AMIN1(AZ1,AZ2)
      AZTR=AMAX1(AZ1,AZ2)
      MINAZ=(AZTL-SLOPDG)*UNSCAZ+0.5
      MAXAZ=(AZTR+SLOPDG)*UNSCAZ
      LOWAZ=AZLOW*UNSCAZ
   10 CONTINUE

c-----debug (ljm)
      if(debug)then
         write(8,*) 'Crtout: continue all interpolations'
         write(8,*) 'Crtout: rta,unscaz,sclaz=',rta,unscaz,sclaz
         write(8,*) 'Crtout: azlow,lowaz,izad=',azlow,lowaz,izad
         write(8,*) 'Crtout:          az1,az2=',az1,az2
         write(8,*) 'Crtout:        aztl,aztr=',aztl,aztr
         write(8,*) 'Crtout:      minaz,maxaz=',minaz,maxaz
      end if
c-----debug (ljm)
C
C     CALCULATE AND CONVERT CARTESIAN COORDINATES TO 
C        SPHERICAL POLAR SYSTEM RELATIVE TO RADAR
C
C     OPEN DESTINATION FILE OF CONVERTED CARTESIAN LOCATIONS AND
C        INITIALIZE HOUSEKEEPING TABLE FOR THIS VOLUME
C
C     Regular cartesian (XY) grid:
C        (X,Y)   = XY-coordinate relative to lower-left corner of grid
C        (XT,YT) = XY-coordinate relative to the radar
C        AZ      = azimuth angle of (XT,YT)
C     Regular longitude/latitude grid:
C        (X,Y) ==> (GLON,GLAT) LonLat-coordinate of grid
C        Convert this (GLAT,GLON) to (XT,YT) relative to the radar 
C           at (RADLAT,RADLON).
C        (XT,YT) = XY-coordinate of (GLON,GLAT) relative to the radar
C        AZ      = azimuth angle of (XT,YT)
C
      ICTAB(1)=0
      ICTAB(2)=0
      ICTAB(3)=0
      ICTAB(4)=0
      N=0
      Y= -YD
      DO 15 J=1,NY
         IF(ILLE.EQ.1 .OR. ILLZ.EQ.1)THEN
            GLAT = Y1 + (J-1)*YD
         ELSE
            Y=Y+YD
            YTRM1=XORTR-ASNF*Y
            YTRM2=YORTR+ACSF*Y
         END IF

         X= -XD
         DO 13 I=1,NX
            IF(ILLE.EQ.1 .OR. ILLZ.EQ.1)THEN
               GLON = X1 + (I-1)*XD
               CALL LL2XYDRV(GLAT,-GLON,XT,YT,RADLAT,-RADLON,ANGXAX)
            ELSE
               X=X+XD
               XT=ACSF*X+YTRM1
               YT=ASNF*X+YTRM2
            END IF
C     360 SCANS ONLY
c            IF(ABS(XT).LT.0.1.AND.ABS(YT).LT.0.1)GO TO 13
            IF(ABS(XT).LT.0.001.AND.ABS(YT).LT.0.001)THEN
               AZ=0.0
            ELSE
               AZ=ATAN2(XT,YT)*RTA
            END IF
            IF(AZ.LT.AZLOW) AZ=AZ+360.
c            IAZ=AZ*UNSCAZ + 0.5
            IAZ=NINT(AZ*UNSCAZ)
            IF(IAZ.LT.LOWAZ) IAZ=IAZ+IZAD
            N=N+1
            XGRID(N)=XT
            YGRID(N)=YT
            if(i.eq. 1 .and. j.eq. 1)then
               print *,'  CRTOUT: IAZ=NINT(AZ*UNSCAZ) - sclaz,unscaz=',
     +              sclaz,unscaz
               write(6,1766)xt,yt,az,iaz
 1766          format(3x,'Lower  left corner (x,y,az,iaz)=',3f10.3,i10)
            end if
            if(i.eq. 1 .and. j.eq. ny)then
               write(6,1767)xt,yt,az,iaz
 1767          format(3x,'Upper  left corner (x,y,az,iaz)=',3f10.3,i10)
            end if
            if(i.eq. nx .and. j.eq. ny)then
               write(6,1768)xt,yt,az,iaz
 1768          format(3x,'Upper right corner (x,y,az,iaz)=',3f10.3,i10)
            end if
            if(i.eq. nx .and. j.eq. 1)then
               write(6,1769)xt,yt,az,iaz
 1769          format(3x,'Lower right corner (x,y,az,iaz)=',3f10.3,i10)
            end if
C
C           IPUTCP    packs IAZ,J,I into 32 bit word for   sorting (SINSRT)
C           IGETCP2 unpacks IAZ,J,I from 32 bit word after sorting (SINSRT)
C           Debugging added to check packing, unpacking and sorting.
C
            CALL IPUTCP(ICART(N),IAZ,I,J)
c            if(debug)then
               if((i.eq.  1 .and. j.eq.  1) .or.
     +            (i.eq.  1 .and. j.eq. ny) .or.
     +            (i.eq. nx .and. j.eq. ny) .or.
     +            (i.eq. nx .and. j.eq.  1))then
                  print *,'    Packed grid info [nji,iaz,icart(n)]=',
     +                 n,j,i,iaz,icart(n)
                  call igetcp2(icart(n),jaz,ix,jy)
                  azj=jaz*sclaz
                  write(6,1770)n,j,i,yt,xt,iaz,jaz,jy,ix,az,azj,
     x                 abs(az-azj)
 1770             format(5x,'Put: n,j,i,y,x,iaz,jaz=',i6,2i4,2f8.1,2i6,
     x                 2x,'Get: j,i,az,azj,dif=',2i4,2f8.3,f8.3)
               end if
c            end if
 13      CONTINUE
 15   CONTINUE
C
C        Sort ICART containing packed grid locations and indices 
C        into increasing azimuth order.
C        After sorting, the (XGRID,YGRID) values associated with
C        the (I,J) indices will be accessed using IJ=I+(J-1)*NX.
C        This means those two arrays don't need to be sorted.
C
c-----------debug (ljm)
      if(debug)then
         do nn=1,n
            if(nn.eq. 1 .or. nn.eq. n .or.
     +           mod(nn,nmod) .eq. 0)then
               call igetcp2(icart(nn),iaz,ix,jy)
               write(8,1771)nn,icart(nn),jy,ix,iaz,iaz*sclaz
 1771          format(1x,'Before sinsrt - nn,icart,jy,ix,iaz,az= ',
     +              i6,i12,2i4,i8,f10.5)
            end if
         end do
      end if
C
C        SORT (X,Y) GRID LOCATIONS IN INCREASING AZIMUTH ORDER
C           ICART(N) (Bits 01-16) = Integer scaled azimuth
C           ICART(N) (Bits 17-24) = Y index (J)
C           ICART(N) (Bits 25-32) = X index (I)
C        After sorting, the (XGRID,YGRID) values associated with
C        the (I,J) indices will be accessed using IJ=I+(J-1)*NX.
C        This means those two arrays don't need to be sorted.
C

      CALL SINSRT(ICART,1,N)

c-----------debug (ljm)
      if(debug)then
         do nn=1,n
            if(nn.eq. 1 .or. nn.eq. n .or.
     +           mod(nn,nmod) .eq. 0)then
               call igetcp2(icart(nn),iaz,ix,jy)
               write(8,1773)nn,icart(nn),jy,ix,iaz,iaz*sclaz
 1773          format(1x,'After sinsrt - nn,icart,jy,ix,iaz,az= ',
     +              i6,i12,2i4,i8,f10.5)
            end if
         end do
      end if
c-----------debug (ljm)

C     GENERATE TABLES AND IF NECESSARY, WRITE OUT TO DISK
C
      K1=1
      K2=MAXPLN
      IF(K2.GT.N) K2=N
      KB=K2-K1+1
      ICTAB(1)=1
      ICTAB(2)=KB
      CALL IGETCP2(ICART(K1),ICTAB(3),IX,IY)
      CALL IGETCP2(ICART(K2),ICTAB(4),IX,IY)

      IF (IOP.EQ.3) THEN
C
C     AIRBORNE --> CART: CONVERT GRID SPECS BACK TO ORIGINAL ONES
C
         T1=Y1
         T2=Y2
         TD=YD
         
         Y1=Z1
         Y2=Z2
         YD=ZD

         Z1=T1
         Z2=T2
         ZD=TD

         NT=NY
         NY=NZ
         NZ=NT

         YORG=0.0


      ELSE IF (IOP.EQ.1) THEN
C
C     RHI --> CART: SAVE ANGLE INFO IN SEPARATE VARIABLES
C
         LOWAZ2=LOWAZ
         MAXAZ2=MAXAZ
         MINAZ2=MINAZ

C
C     CONVERT GRID SPECS BACK TO ORIGINAL ONES
C
         T1=X1
         T2=X2
         TD=XD

         X1=Y1
         X2=Y2
         XD=YD

         Y1=Z1
         Y2=Z2
         YD=ZD

         Z1=T1
         Z2=T2
         ZD=TD

         NT=NX
         NX=NY
         NY=NZ
         NZ=NT

         TORG=XORG
         XORG=YORG
         YORG=ZRAD
         ZRAD=TORG

         ANGXAX=ANGXAXT
         IS360=.FALSE.
      ELSE IF (IOP.EQ.2) THEN
         ANGXAX=ANGXAXT

C     SHOULD NOT BE TRUE FOR RHI SCANS
C
         IS360=.FALSE.
      END IF
      
c-----------debug (ljm)
      if(debug)then
         if(is360)then
            write(8,*) 'Crtout: 360 deg scan'
         else
            write(8,*) 'Crtout: not a 360 deg scan'
         end if
         write(8,*) 'CRTOUT: angxax,angxaxt=',angxax,angxaxt
         write(8,*) 'CRTOUT: #2 - returning to UFCART'
         write(8,*) 'CRTOUT: #2 - end debugging printout'
      end if
c-----------debug (ljm)

      RETURN
      END






