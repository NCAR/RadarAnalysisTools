      SUBROUTINE CRTTCP(ICART,ICTAB,ICRTST,INPTST)
C
C     THIS SUBROUTINE GENERATES THE 2-D PLANES FOR COPLANE INTERPOLATION
C     AND ASSIGNS EACH (X,Y) GRID POINT AN AZIMUTH VALUE. THESE POINTS
C     ARE THEN SORTED IN ORDER OF INCREASING AZIMUTH.
C
      INCLUDE 'SPRINT.INC'
c-----PARAMETER (MAXFLD=16)

      DIMENSION ICART(MAXPLN),ICTAB(4),AZC(4)
      COMMON /TRANS/ X1,X2,XD,Y1,Y2,YD,Z1,Z2,ZD,NX,NY,NZ,XORG,YORG,
     X   ANGXAX,ZRAD,AZLOW,BAD,ASNF,ACSF,IAXORD(3),NPLANE,EDIAM
      COMMON /SETAZM/ USAZ1,USAZ2,USGAP,USINC,IFOR36,IFORDR
      LOGICAL IS360
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
      COMMON /SCAN/ ICOPLANE,IFLGBAS,IPPI,ILLE,ILLZ
      DATA SLOPDG/2.0/
      DATA EPS/1.E-8/
      DATA NMOD/20/      
C
C     FUNCTIONS FOR CONVERTING AN (X,Y) POINT RELATIVE TO THE ORIGIN
C     TO ONE RELATIVE TO THE RADAR
C
      FXC(X,Y) = X + XORTR
      FYC(X,Y) = Y + YORTR
C
C        CHECK IF REORDERING IS NECESSARY
C
      IF(ICRTST.EQ.0.AND.INPTST.EQ.0) RETURN
      INPTST=0
      ICRTST=0
C
C        ATR- DEGREES TO RADIANS CONVERSION FACTOR
C        RTA- RADIANS TO DEGREES CONVERSION FACTOR
C
      ATR=ATAN(1.)/45.
      RTA=1./ATR
      IZAD=360. * UNSCAZ

      ANGR=(ANGXAX-90.)*ATR
      ASNFKP=SIN(ANGR)
      ACSFKP=COS(ANGR)
      ASNF=0.0
      ACSF=1.0

C
C     FIND RADAR COORDINATES IN ROTATED COORDINATE SYSTEM
C
      XRAD=XORG*ACSFKP - YORG*ASNFKP
      YRAD=XORG*ASNFKP + YORG*ACSFKP

C
C     FIND COORDINATE OF LOWER LEFT HAND PART OF GRID, RELATIVE TO RADAR
C
      XORTR = X1-XRAD
      YORTR = Y1-YRAD

      AZLOW=0.0
C
C     THE FOLLOWING REPRESENT THE COORDINATES OF THE 4 CORNERS OF
C     THE GRID, RELATIVE TO THE RADAR POSITION
C
      XLL=FXC(0.,0.)
      YLL=FYC(0.,0.)
      XTL=FXC(0.,Y2-Y1)
      YTL=FYC(0.,Y2-Y1)
      XTR=FXC(X2-X1,Y2-Y1)
      YTR=FYC(X2-X1,Y2-Y1)
      XLR=FXC(X2-X1,0.)
      YLR=FYC(X2-X1,0.)
      
      IS360=.FALSE.

C
C     FIND AZIMUTHAL LIMITS OF 2-D GRID
C
      IF (XLL.LE.0.0) XLL=0.001
      IF (YLL.EQ.0.0) YLL=0.001
      IF (XTL.LE.0.0) XTL=0.001
      AZC(1)=ATAN2(XLL,YLL)*RTA
      IF (YTL.EQ.0.0) YTL=0.001
      AZC(2)=ATAN2(XTL,YTL)*RTA
      IF (YTR.EQ.0.0) YTR=0.001
      AZC(3)=ATAN2(XTR,YTR)*RTA
      IF (YLR.EQ.0.0) YLR=0.001
      AZC(4)=ATAN2(XLR,YLR)*RTA
      DO 10 I=1,4
         IF (AZC(I).LT.0) AZC(I)=AZC(I)+180.0
 10   CONTINUE
      AZ1=1000.
      AZ2=-1000.
      DO 4 I=1,4
         IF(AZC(I).LT.AZ1) AZ1=AZC(I)
         IF(AZC(I).GT.AZ2) AZ2=AZC(I)
    4 CONTINUE
      AZTL=AMIN1(AZ1,AZ2)
      AZTR=AMAX1(AZ1,AZ2)
      MINAZ=(AZTL)*UNSCAZ+0.5
      MAXAZ=(AZTR)*UNSCAZ
      LOWAZ=AZLOW*UNSCAZ
C
C     CALCULATE AND CONVERT CARTESIAN COORDINATES TO POLAR COORDINATES RELATIVE
C     TO RADAR
C          (X,Y) = coplane xy-coordinate relative to lower-left corner of grid
C        (XT,YT) = coplane xy-coordinate relative to the radar
C            AZ  = coplane azimuth angle of (XT,YT)
C
      ICTAB(1)=0
      ICTAB(2)=0
      ICTAB(3)=0
      ICTAB(4)=0
      N=0
      Y=-YD
      DO 15 J=1,NY
         Y=Y+YD
         YTRM1=XORTR
         YTRM2=YORTR+Y
         X=-XD
         DO 13 I=1,NX
            X=X+XD
            XT=X+YTRM1
            YT=YTRM2
            IF (YT.EQ.0.0) YT=0.001
            AZ=ATAN2(XT,YT)*RTA
            IF(AZ.LT.0) AZ=AZ+180.0
            IAZ=AZ*UNSCAZ+0.5
            IF(IAZ.LT.LOWAZ) IAZ=IAZ+IZAD
            N=N+1
            if(i.eq. 1 .and. j.eq. 1)then
               print *,'  CRTTCP: IAZ=NINT(AZ*UNSCAZ) - sclaz,unscaz=',
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
            if((i.eq.  1 .and. j.eq.  1) .or.
     +         (i.eq.  1 .and. j.eq. ny) .or.
     +         (i.eq. nx .and. j.eq. ny) .or.
     +         (i.eq. nx .and. j.eq.  1))then
               print *,'    Packed grid info [nji,iaz,icart(n)]=',
     +              n,j,i,iaz,icart(n)
               call igetcp2(icart(n),jaz,ix,jy)
               azj=jaz*sclaz
               write(6,1770)n,j,i,yt,xt,iaz,jaz,jy,ix,az,azj,
     x              abs(az-azj)
 1770          format(5x,'Put: n,j,i,y,x,iaz,jaz=',i6,2i4,2f8.1,2i6,
     x              2x,'Get: j,i,az,azj,dif=',2i4,2x,2f8.3,f8.3)
            end if
 13      CONTINUE
 15   CONTINUE
C
C     SORT (X,Y) POSITIONS IN INCREASING AZIMUTH
C           ICART(N) (Bits 01-16) = Integer scaled azimuth
C           ICART(N) (Bits 17-24) = Y index (J)
C           ICART(N) (Bits 25-32) = X index (I)
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
c-----------debug (ljm)

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
C
C     GENERATE TABLES AND WRITE TO DISK IF NECESSARY
C
      K1=1
      K2=MAXPLN
      IF(K2.GT.N) K2=N
      KB=K2-K1+1
      ICTAB(1)=1
      ICTAB(2)=KB
      CALL IGETCP2(ICART(K1),ICTAB(3),IX,IY)
      CALL IGETCP2(ICART(K2),ICTAB(4),IX,IY)
      RETURN
      END


