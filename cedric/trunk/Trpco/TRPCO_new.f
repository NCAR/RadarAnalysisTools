      SUBROUTINE TRPCO(WXYC,IXYC,NNX,NNY,NNZ,CSPN,NLEV,RELMAX,ICORD,
     X     LCMB2,ISPEC,VNYQ,RTOP,RBOT,ROUT,IBUF,NX,NY,ITEMP,IDIM2,
     X     MEMUSE,XORG,YORG,Z1,
     X     ZC,LATLON,ORLAT,ORLON,ANGXAX,IROT,ANGUSR,
     X     ITRANS,XOR,YOR,ZOR,ZRAD,IFLAT,DE,X1,Y1,XDI,YDI,ZDI,
     X     EPS,IZLEV)
C     
C     This subroutine does the actual interpolation from the original 
C     coordinate system ('CRT','COPL','ELEV', or 'LLE ') to a constant
C     height one at either cartesian (XY) or lon-lat (LL) grid points.
C
C     Input coordinate system is specified by ICORD.  See REMAP.
C        (0) Cartesian - levels of constant msl height z (x,y,z)
C        (1) Coplane   - levels of constant coplane angle c (xc,yc,c)
C        (2) CartEle   - levels of constant elevation angle e (x,y,e)
C        (3) LonLatEle - levels of constant elevation angle e (l,l,e)
C        Note: Elevation angles can be unequally spaced.  Uses values
C              (VALLEV) from the level headers rather than equal spaced.
C     
C     COINTRP: NX,NY,NZ      --> NNX,NNY,NNZ - numbers of new grid points
C                          I --> NLEV        - current new grid level
C             NCX(1), NCX(2) --> NX,NY       - numbers of old grid points
C

      INCLUDE 'CEDRIC.INC'
      COMMON LCMB(1)
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X     IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X     NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF
      COMMON /LEVELS/ VALLEV(MAXZLEV),VALNYQ(MAXZLEV),VNYQ_VOL
      DIMENSION WXYC(MAXP,MAXP,3),IXYC(MAXP,MAXP,3),CSPN(3,3)
      DIMENSION IPHOLD(NFMAX),IVAL(8),VAL(8),LCMB2(1),ISPEC(NFMAX)
      DIMENSION RBOT(NX,NY),RTOP(NX,NY),ROUT(NNX,NNY)
      DIMENSION ITEMP(1)
      DIMENSION IBUF(1)
      DATA IBIT,NBITS,NSKIP/0,16,0/
      INTEGER CVMGP
      
      LOGICAL LATLON
      COMMON /SCRATCH_GRD/ XGRD(MAXX,MAXY),YGRD(MAXX,MAXY)
c     DIMENSION FLD(MAXX,MAXY,MAXZLEV)
      DIMENSION FLD(301,301,41)

      character*8 tyme

      print *,'Trpco - xorg,yorg,zrad,z1='
      print *,'       ',xorg,yorg,zrad,z1
      print *,'Trpco - zc,latlon,orlat,orlon,angxax,irot,angusr='
      print *,'       ',zc,latlon,orlat,orlon,angxax,irot,angusr
      print *,'Trpco - itrans,xor,yor,zor,zrad,iflat,de='
      print *,'       ',itrans,xor,yor,zor,zrad,iflat,de
      print *,'Trpco - x1,y1=',x1,y1
      print *,'Trpco - xdi,ydi,zdi,eps,izlev='
      print *,'       ',xdi,ydi,zdi,eps,izlev

      IBAD=ID(67)
      BAD=FLOAT(IBAD)
      N1=NCX(1)
      N2=NCX(2)
      NZ=NCX(3)
      ATR=ATAN(1.)/45.
      NF=ID(175)
      VNYQ2=VNYQ*2.0
      IVOL=1
      ZLEV=VALLEV(NLEV)
C     
C     SETUP STRUCTURE OF NEW EDIT FILE
C     
      NNPLANE=NNX*NNY
C     NUMBER OF WORDS/PLANE/FIELD
      NWPF=(NNPLANE-1)/(WORDSZ/16) + 1
C     NUMBER OF WORDS/FIELD
      NWF=NWPF*NNZ
      DO 50 I=1,NFMAX
         IPHOLD(I)=1+NWF*(I-1)
 50   CONTINUE
      NXX=NNX
      FLNX=NNX*16.0/WORDSZ
      INNX=INT(NNX*16.0/WORDSZ)
      IDIFF = (FLNX - REAL(INNX))*(WORDSZ/16.0)
      IF (MEMUSE.EQ.0 .OR. MEMUSE.EQ.1) THEN
         IF (IDIFF.NE.0) THEN
            NXX = NNX + (WORDSZ/16.0) - IDIFF
            NWPF = (NXX*NNY - 1)/(WORDSZ/16.0) + 1
            NNPLANE=NXX*NNY
         END IF
      END IF
      print *,'Trpco: nnx,nny,nnz,memuse=',nnx,nny,nnz,memuse
      print *,'Trpco: flnx,innx,idiff=',flnx,innx,idiff
      print *,'Trpco: nxx,nwpf,nnplane=',nxx,nwpf,nnplane
      print *,'Trpco: nx,ny=',nx,ny
      IBOT=0
      ITOP=0
C     
C     START LOOP OVER ALL FIELDS
C     
      call clock(tyme)
      print *,'Trpco-fields loop: nf=',nf,' t=',tyme
      print *,'Trpco-fields loop: ibot,ncx=',ibot,ncx

      DO 100 I=1,NF
         DO 30 KK=1,NCX(3)
            call clock(tyme)
            write(*,1773)kk,i,tyme,memuse
 1773       format('Trpco-call fetchd for plane   kk=',i4,
     +           ' fld#=',i4,' t=',a8,' memuse=',i1)
            CALL FETCHD(IBOT,ID,KK,I,IBUF,RBOT,N1,N2,3,
     X           BAD,ZLEV,NST)
            IF (NST.NE.0) THEN
               WRITE(*,*)'+++ERROR FETCHING IN TRPCO 1+++'
               CALL FLUSH_STDOUT
            END IF
            DO JJ=1,NCX(2)
               DO II=1,NCX(1)
                  FLD(II,JJ,KK)=RBOT(II,JJ)
               END DO
            END DO
 30      CONTINUE
C     
C     ONLY INTERPOLATE FIELDS WITH NON-ZERO SCALE FACTORS (ACTIVE FIELDS)
C     
         IBOT=0
         ITOP=0
         SCALE=1./ID(175+I*5)
         call clock(tyme)
         print *,'Trpco: fld#=',i,' t=',tyme
         print *,'Trpco-newgrid loop 1: nnx*nny=',nnx,nny,nnx*nny

C     Outer loop over new grid points
C     
	 DO 120 J=1,NNY
            DO 130 K=1,NNX
               FVAL=BAD
               L=0
               M=0
C*********C*********C*********C*********C*********C*********C***********
               YC=CSPN(1,2) + (J-1)*CSPN(3,2)
               XC=CSPN(1,1) + (K-1)*CSPN(3,1)
               Z=ZC
               Y=YC
               X=XC

               IF (LATLON) THEN
C     ppi l,l,e --> l,l,z (convert grid lon-lat to distances in km)
                  IF(IZLEV.EQ.1 .AND. I.EQ.1)THEN
                     PLAT=Y
                     PLON=-X
                     CALL LL2XYDRV(PLAT,PLON,X,Y,ORLAT,ORLON,ANGXAX)
                     XGRD(K,J)=X
                     YGRD(K,J)=Y
                  ELSE
                     X=XGRD(K,J)
                     Y=YGRD(K,J)
                  END IF
               END IF

               IF (IROT.EQ.1) THEN
                  THETA=(ANGUSR-ANGXAX)*ATR
                  XP=X*COS(THETA) - Y*SIN(THETA)
                  YP=X*SIN(THETA) + Y*COS(THETA)
                  X=XP
                  Y=YP
               END IF
               IF (ITRANS.EQ.1) THEN
                  X=X+XOR
                  Y=Y+YOR
                  Z=Z+ZOR
               END IF
C     
C     CORRECT Z FOR HEIGHT OF RADAR AND CURVATURE OF EARTH
C     IFLAT: (0) CURVED EARTH - Use 4/3 Earth's radius
C            (1) FLAT EARTH   - No correction
C     
               Z=Z-ZRAD
               S2=(X-XORG)**2.0 + (Y-YORG)**2.0 
               IF (IFLAT.EQ.1) THEN
                  CCOR=0.0
               ELSE
                  CCOR=3.*S2/(4.*DE)
               END IF
               Z=Z-CCOR

               IF (ICORD.EQ.0)THEN
C
C     CARTESIAN --> CARTESIAN (Assumes z-levels are equally spaced.)
C
                  RN=(Z-Z1)*ZDI+1.0+EPS
                  N=INT(RN)

               ELSE IF (ICORD.EQ.1)THEN
C
C     COPLANE --> CARTESIAN (Assumes coplane angles are equally spaced.)
C
                  IF (Z.LE.0.0 .OR. X.LE.0.0) THEN
                     C=-1.0
                     XC=-99.0
                     YC=-99.0
                  ELSE 
                     C=ATAN2(Z,X)
                     XC=X*COS(C) + Z*SIN(C)
                     YC=Y
                     C=C/ATR
                  END IF
                  CMIN=CSP(1,3)
                  CMAX=CSP(2,3)
                  IF(C.LT.CMIN .OR. C.GT.CMAX)GO TO 200
                  RN=(C-Z1)*ZDI+1.0+EPS
                  N=INT(RN)
                  
               ELSE IF (ICORD.EQ.2 .OR. ICORD.EQ.3)THEN
C     
C     ELEVATION --> CARTESIAN (Assumes elevation angles are unequally spaced.)
C                   Find index (N) for VALLEV below the current level.
C                   RN - fractional distance of E above VALLEV(N)
C
                  IF(S2.GE.0.0)THEN
                     H=SQRT(S2)
                  ELSE
                     H=0.0
                  END IF
                  IF (Z.EQ.0.0 .OR. H.EQ.0.0) THEN
                     E=0.0
                  ELSE 
                     E=ATAN2(Z,H)/ATR
                  END IF
                  
                  N=0
                  IF(E.LT.VALLEV(1) .OR. E.GT.VALLEV(NCX(3)))GO TO 200
                  DO II=1,NCX(3)-1
                     IF(E.GE.VALLEV(II) .AND. E.LT.VALLEV(II+1))THEN
                        N=II
                        ED=VALLEV(II+1)-VALLEV(II)
                        IF (ED.GT.0.0) THEN
                           EDI=1./ED
                        ELSE 
                           EDI=1.0
                        END IF
                        RN=(E-VALLEV(N))*EDI
                        GO TO 10
                     END IF
                  END DO
 10               CONTINUE
               END IF
C     
C     CALCULATE WEIGHTING FACTORS AND INDICES FOR BILINEAR XY-INTERPOLATION
C
               IF(ICORD.EQ.0)THEN
                  L=IXYC(K,J,1)
                  M=IXYC(K,J,2)
                  N=IXYC(K,J,3)
               ELSE
               WXYC(K,J,1)=0.0
               WXYC(K,J,2)=0.0
               WXYC(K,J,3)=0.0
               IXYC(K,J,1)=0
               IXYC(K,J,2)=0
               IXYC(K,J,3)=0
               RL=(XC-X1)*XDI+1.0+EPS
               RM=(YC-Y1)*YDI+1.0+EPS
               L=INT(RL)
               M=INT(RM)

               WXYC(K,J,1)=RL-FLOAT(L)
               WXYC(K,J,2)=RM-FLOAT(M)
               IF (ICORD.EQ.1)THEN
                  WXYC(K,J,3)=RN-FLOAT(N)
               ELSE IF (ICORD.EQ.2 .OR. ICORD.EQ.3)THEN
                  WXYC(K,J,3)=RN
               END IF
               IXYC(K,J,1)=L
               IXYC(K,J,2)=M
               IXYC(K,J,3)=N

c--------------debug (ljm)
c               if(i.eq.1)then
c                  if( (j .eq. 1  .and. k .eq. 1) .or.
c     +                (j .eq. NY .and. k .eq. NX).or.
c     +                (mod(j,1) .eq. 0 .and. mod(k,1) .eq. 0) ) then
c                     wx=wxyc(k,j,1)
c                     wy=wxyc(k,j,2)
c                     wz=wxyc(k,j,3)
c                     if(e.ge.vallev(1) .and. e.le.vallev(ncx(3)))then
c                        write(*,1770)k,j,izlev,xc,yc,x,y,h,z,e,wx,wy,wz
c 1770                   format(' KJI=',3i4,' LL=',2f10.4,
c     +                       ' XYHZE=',5f10.4,3f8.4)
c                     end if
c                  end if  
c               end if
c--------------debug (ljm)

               END IF
               IF ((L.LT.1 .OR. L.GT.NX) .OR.
     X             (M.LT.1 .OR. M.GT.NY) .OR.
     X             (N.LT.1 .OR. N.GT.NZ)) GOTO 200
               print *,'TRPCO: nml=',n,m,l
               
               AL=WXYC(K,J,1)
               BL=WXYC(K,J,2)
               CL=WXYC(K,J,3)

               IF (IDIM2.EQ.1) THEN
C     
C     2-D INTERPOLATION ONLY
C     
C     
C     IF WE ARE OUTSIDE OF ORIGINAL GRID IN ANY DIRECTION, DON'T INTERP.
C     
                  IF (L.EQ.NX) THEN
                     IF (AL.LT.1.0E-5) THEN
                        L=NX-1
                        AL=1.0 - 1.0E-5
                     ELSE IF (AL.GT.1.0E-5) THEN
                        GOTO 200
                     END IF
                  END IF
                  IF (M.EQ.NY) THEN
                     IF (BL.LT.1.0E-5) THEN
                        M=NY-1
                        BL=1.0 - 1.0E-5
                     ELSE IF (BL.GT.1.0E-5) THEN
                        GOTO 200
                     END IF
                  END IF

C     MEMUSE: (0) Data is on disk, (1) Data is in 32-bit memory,
C             (2) Data is in 64-bit memory
C
                  IF (MEMUSE.EQ.0) THEN
                     IF (IBOT.NE.N) THEN
                        call clock(tyme)
                        write(*,1771)j,k,n,i,tyme
 1771                   format('Trpco-move data for plane   jkn=',3i4,
     +                       ' fld#=',i4,' t=',a8)
c-----                        CALL FETCHD(IBOT,ID,N,I,IBUF,RBOT,N1,N2,3,
c-----     X                       BAD,ZLEV,NST)
c-----                        IF (NST.NE.0) THEN
c-----                           WRITE(*,*)'+++ERROR FETCHING IN TRPCO 1+++'
c-----                           CALL FLUSH_STDOUT
c-----                        END IF
                        do jj=1,n2
                           do ii=1,n1
                              rbot(ii,jj)=fld(ii,jj,n)
                           end do
                        end do
                        IBOT=N
                     END IF
C     
C     OBTAIN FOUR VALUES FROM BOTTOM LEVEL
C     
                     VAL(1)=RBOT(L,M)
                     VAL(2)=RBOT(L+1,M)
                     VAL(3)=RBOT(L,M+1)
                     VAL(4)=RBOT(L+1,M+1)
                  ELSE
                     LOCD=ID(400+I)
                     LOCD=LOCD+(N-1)*ID(451)
                     IF (LOCD.GT.MAXLCM) GOTO 200
                     ISKP1=((M-1)*NX + (L-1))*16
                     ISKP2=((M-1)*NX + L)*16
                     ISKP3=(M*NX + (L-1))*16
                     ISKP4=(M*NX + L)*16
                     CALL GBYTES(LCMB(LOCD),IVAL(1),ISKP1,16,0,1)
                     CALL GBYTES(LCMB(LOCD),IVAL(2),ISKP2,16,0,1)
                     CALL GBYTES(LCMB(LOCD),IVAL(3),ISKP3,16,0,1)
                     CALL GBYTES(LCMB(LOCD),IVAL(4),ISKP4,16,0,1)
                  
                     DO 75 ILP=1,4
                        IVAL(ILP)=CVMGP(IVAL(ILP)-65536,IVAL(ILP),
     X                       IVAL(ILP)-32768)
                        VAL(ILP)=CVMGT(IVAL(ILP)*SCALE,BAD,
     X                       IVAL(ILP).NE.IBAD)
 75                  CONTINUE
                  END IF
                  
C     
C     CHECK TO SEE IF ANY SPECIAL PROCESSING IS TO BE DONE
C     
                  IF (ISPEC(I).NE.0) THEN
                     IF (ISPEC(I).EQ.1) THEN
C     
C     TRANFORM TO LINEAR VALUES
C     
                        DO 83 ILP=1,4
                           IF (VAL(ILP).NE.BAD) VAL(ILP)=
     X                          10**(VAL(ILP)/10.0)
 83                     CONTINUE
                     ELSE IF (ISPEC(I).EQ.2) THEN
C     
C     DO LOCAL UNFOLDING
C     
                        DO 88 ILP=1,4
                           IF (VAL(ILP).NE.BAD) THEN
                              VALREF=VAL(ILP)
                              GOTO 93
                           END IF
 88                     CONTINUE
 93                     CONTINUE
                        DO 98 ILP=1,4
                           IF (VAL(ILP).EQ.BAD) GOTO 98
                           DIF=VALREF-VAL(ILP)
                           IF (ABS(DIF).GT.VNYQ) THEN
                              KN=NINT((VALREF - VAL(ILP))/VNYQ2)
                              VAL(ILP)=VAL(ILP) + KN*VNYQ2
                           END IF
 98                     CONTINUE
                     END IF
                  END IF
                  
C     
C     DO THE INTERPOLATION
C     
                  IC1=0
                  IF (VAL(1).NE.BAD .AND. VAL(2).NE.BAD) THEN
C     
C     BILINEAR METHOD
C     
                     VALX=VAL(1)*(1-AL) + VAL(2)*AL
                  ELSE
C     
C     CLOSEST POINT
C     
                     IC1=1
                     IF ((AL.GE.0.5 .AND. VAL(2).NE.BAD) .OR.
     X                    VAL(1).EQ.BAD) THEN
C     
C     CHECK IF DIST. TO BE RELOCATED IS <= WHAT THE USER WILL ALLOW
C     
                        IF (((1-AL)*CSP(3,1)).LE.RELMAX) THEN
                           VALX=VAL(2)
                        ELSE
                           VALX=BAD
                        END IF
                     ELSE
                        IF ((AL*CSP(3,1)).LE.RELMAX) THEN
                           VALX=VAL(1)
                        ELSE
                           VALX=BAD
                        END IF
                     END IF
                  END IF
                  
                  IC2=0
                  IF (VAL(3).NE.BAD .AND. VAL(4).NE.BAD) THEN
                     VALY=VAL(3)*(1-AL) + VAL(4)*AL
                  ELSE
                     IC2=1
                     IF ((AL.GE.0.5 .AND. VAL(4).NE.BAD) .OR.
     X                    VAL(3).EQ.BAD) THEN
                        IF (((1-AL)*CSP(3,1)).LE.RELMAX) THEN
                           VALY=VAL(4)
                        ELSE
                           VALY=BAD
                        END IF
                     ELSE
                        IF ((AL*CSP(3,1)).LE.RELMAX) THEN
                           VALY=VAL(3)
                        ELSE
                           VALY=BAD
                        END IF
                     END IF
                  END IF
                  
                  IC3=0
                  IF ((VALX.NE.BAD .AND. VALY.NE.BAD) .AND. 
     X                 (IC1.NE.1 .OR. IC2.NE.1)) THEN
                     VALL=VALX*(1-BL) + VALY*BL
                  ELSE
                     IC3=1
                     IF ((BL.GE.0.5 .AND. VALY.NE.BAD) .OR.
     X                    VALX.EQ.BAD) THEN
                        IF (((1-BL)*CSP(3,2)).LE.RELMAX) THEN
                           VALL=VALY
                        ELSE
                           VALL=BAD
                        END IF
                     ELSE
                        IF ((BL*CSP(3,2)).LE.RELMAX) THEN
                           VALL=VALX
                        ELSE
                           VALL=BAD
                        END IF
                     END IF
                  END IF
                  FVAL=VALL
               ELSE
C
C     3-D INTERPOLATION
C
                  
c     
c     THE FOLLOWING CODE IS TO DEAL WITH INTERPOLATIONS AT THE EDGES OF
C     THE GRID, WHEN THE OLD EDGE LIES ON TOP OF THE NEW EDGE.
C     
                  
                  IF (L.EQ.NX) THEN
                     IF (AL.LT.1.0E-5) THEN
                        L=NX-1
                        AL=1.0 - 1.0E-5
                     ELSE IF (AL.GT.1.0E-5) THEN
                        GOTO 200
                     END IF
                  END IF
                  IF (M.EQ.NY) THEN
                     IF (BL.LT.1.0E-5) THEN
                        M=NY-1
                        BL=1.0 - 1.0E-5
                     ELSE IF (BL.GT.1.0E-5) THEN
                        GOTO 200
                     END IF
                  END IF
                  IF (N.EQ.NZ) THEN
                     IF (CL.LT.1.0E-5) THEN
                        N=NZ-1
                        CL=1.0 - 1.0E-5
                     ELSE IF (CL.GT.1.0E-5) THEN
                        GOTO 200
                     END IF
                  END IF

C     MEMUSE: (0) Data is on disk, (1) Data is in 32-bit memory,
C             (2) Data is in 64-bit memory
C
                  IF (MEMUSE.EQ.0) THEN
C     
C     GET THE TWO Z, COPLANE, OR ELEVATION LEVELS WE NEED
C     
                     IF ((IBOT.NE.N .AND. ITOP.NE.N+1) .AND. 
     X                    (ITOP.NE.N)) THEN
c                        call clock(tyme)
c                        write(*,1771)j,k,n,i,tyme
c-----                        CALL FETCHD(IBOT,ID,N,I,IBUF,RBOT,N1,N2,3,
c-----     X                       BAD,ZLEV,NST)
c-----                        IF (NST.NE.0) THEN
c-----                           WRITE(*,*)'+++ERROR FETCHING IN TRPCO 1+++'
c-----                           CALL FLUSH_STDOUT
c-----                        END IF
c                        write(*,1772)j,k,n+1,i,tyme
c 1772                   format('Trpco-move data for plane jkn+1=',3i4,
c     +                       ' fld#=',i4,' t=',a8)
c-----                        CALL FETCHD(ITOP,ID,N+1,I,IBUF,RTOP,N1,N2,
c-----     X                    3,BAD,ZLEV,NST)
                        do jj=1,n2
                           do ii=1,n1
                              rbot(ii,jj)=fld(ii,jj,n)
                              rtop(ii,jj)=fld(ii,jj,n+1)
                           end do
                        end do
                     ELSE IF ((IBOT.NE.N .AND. ITOP.NE.N+1) .AND.
     X                       (ITOP.EQ.N)) THEN
                        DO 145 LY=1,NY
                           DO 135 LX=1,NX
                              RBOT(LX,LY)=RTOP(LX,LY)
 135                       CONTINUE
 145                    CONTINUE
c                        call clock(tyme)
c                        write(*,1772)j,k,n+1,i,tyme
c-----                        CALL FETCHD(ITOP,ID,N+1,I,IBUF,RTOP,N1,N2,3,
c-----     X                       BAD,ZLEV,NST)
                        do jj=1,n2
                           do ii=1,n1
                              rtop(ii,jj)=fld(ii,jj,n+1)
                           end do
                        end do
                     END IF
                     IBOT=N
                     ITOP=N+1
C     
C     OBTAIN FOUR VALUES FROM BOTTOM LEVEL
C     
                     VAL(1)=RBOT(L,M)
                     VAL(2)=RBOT(L+1,M)
                     VAL(3)=RBOT(L,M+1)
                     VAL(4)=RBOT(L+1,M+1)
C     
C     OBTAIN FOUR VALUES FROM TOP LEVEL
C     
                     VAL(5)=RTOP(L,M)
                     VAL(6)=RTOP(L+1,M)
                     VAL(7)=RTOP(L,M+1)
                     VAL(8)=RTOP(L+1,M+1)
                  ELSE
                     LOCD=ID(400+I)
                     LOCD=LOCD+(N-1)*ID(451)
                     IF (LOCD.GT.MAXLCM) GOTO 200
                     ISKP1=((M-1)*NX + (L-1))*16
                     ISKP2=((M-1)*NX + L)*16
                     ISKP3=(M*NX + (L-1))*16
                     ISKP4=(M*NX + L)*16
                     CALL GBYTES(LCMB(LOCD),IVAL(1),ISKP1,16,0,1)
                     CALL GBYTES(LCMB(LOCD),IVAL(2),ISKP2,16,0,1)
                     CALL GBYTES(LCMB(LOCD),IVAL(3),ISKP3,16,0,1)
                     CALL GBYTES(LCMB(LOCD),IVAL(4),ISKP4,16,0,1)
                  
C     
C     OBTAIN 4 DATA VALUES ABOVE GRID POINT
C     
                     LOCD=ID(400+I)
                     LOCD=LOCD+N*ID(451)
                     IF (LOCD.GT.MAXLCM) GOTO 200
                     CALL GBYTES(LCMB(LOCD),IVAL(5),ISKP1,16,0,1)
                     CALL GBYTES(LCMB(LOCD),IVAL(6),ISKP2,16,0,1)
                     CALL GBYTES(LCMB(LOCD),IVAL(7),ISKP3,16,0,1)
                     CALL GBYTES(LCMB(LOCD),IVAL(8),ISKP4,16,0,1)
                  
                     DO 175 ILP=1,8
                        IVAL(ILP)=CVMGP(IVAL(ILP)-65536,IVAL(ILP),
     X                       IVAL(ILP)-32768)
                        VAL(ILP)=CVMGT(IVAL(ILP)*SCALE,BAD,
     X                       IVAL(ILP).NE.IBAD)
 175                 CONTINUE
                  END IF
C     
C     CHECK TO SEE IF ANY SPECIAL PROCESSING IS TO BE DONE
C     
                  IF (ISPEC(I).NE.0) THEN
                     IF (ISPEC(I).EQ.1) THEN
C     
C     TRANFORM TO LINEAR VALUES
C     
                        DO 80 ILP=1,8
                           IF (VAL(ILP).NE.BAD) VAL(ILP)=
     X                          10**(VAL(ILP)/10.0)
 80                     CONTINUE
                     ELSE IF (ISPEC(I).EQ.2) THEN
C     
C     DO LOCAL UNFOLDING
C     
                        DO 85 ILP=1,8
                           IF (VAL(ILP).NE.BAD) THEN
                              VALREF=VAL(ILP)
                              GOTO 90
                           END IF
 85                     CONTINUE
 90                     CONTINUE
                        DO 95 ILP=1,8
                           IF (VAL(ILP).EQ.BAD) GOTO 95
                           DIF=VALREF-VAL(ILP)
                           IF (ABS(DIF).GT.VNYQ) THEN
                              KN=NINT((VALREF - VAL(ILP))/VNYQ2)
                              VAL(ILP)=VAL(ILP) + KN*VNYQ2
                           END IF
 95                     CONTINUE
                     END IF
                  END IF
C     
C     DO THE INTERPOLATION
C     
                  IC1=0
                  IF (VAL(1).NE.BAD .AND. VAL(2).NE.BAD) THEN
C     
C     BILINEAR METHOD
C     
                     VALX=VAL(1)*(1-AL) + VAL(2)*AL
                  ELSE
C     
C     CLOSEST POINT
C     
                     IC1=1
                     IF ((AL.GE.0.5 .AND. VAL(2).NE.BAD) .OR.
     X                    VAL(1).EQ.BAD) THEN
C     
C     CHECK IF DIST. TO BE RELOCATED IS <= WHAT THE USER WILL ALLOW
C     
                        IF (((1-AL)*CSP(3,1)).LE.RELMAX) THEN
                           VALX=VAL(2)
                        ELSE
                           VALX=BAD
                        END IF
                     ELSE
                        IF ((AL*CSP(3,1)).LE.RELMAX) THEN
                           VALX=VAL(1)
                        ELSE
                           VALX=BAD
                        END IF
                     END IF
                  END IF
                  
                  IC2=0
                  IF (VAL(3).NE.BAD .AND. VAL(4).NE.BAD) THEN
                     VALY=VAL(3)*(1-AL) + VAL(4)*AL
                  ELSE
                     IC2=1
                     IF ((AL.GE.0.5 .AND. VAL(4).NE.BAD) .OR.
     X                    VAL(3).EQ.BAD) THEN
                        IF (((1-AL)*CSP(3,1)).LE.RELMAX) THEN
                           VALY=VAL(4)
                        ELSE
                           VALY=BAD
                        END IF
                     ELSE
                        IF ((AL*CSP(3,1)).LE.RELMAX) THEN
                           VALY=VAL(3)
                        ELSE
                           VALY=BAD
                        END IF
                     END IF
                  END IF
                  
                  IC3=0
                  IF ((VALX.NE.BAD .AND. VALY.NE.BAD) .AND. 
     X                 (IC1.NE.1 .OR. IC2.NE.1)) THEN
                     VALL=VALX*(1-BL) + VALY*BL
                  ELSE
                     IC3=1
                     IF ((BL.GE.0.5 .AND. VALY.NE.BAD) .OR.
     X                    VALX.EQ.BAD) THEN
                        IF (((1-BL)*CSP(3,2)).LE.RELMAX) THEN
                           VALL=VALY
                        ELSE
                           VALL=BAD
                        END IF
                     ELSE
                        IF ((BL*CSP(3,2)).LE.RELMAX) THEN
                           VALL=VALX
                        ELSE
                           VALL=BAD
                        END IF
                     END IF
                  END IF
                  
C     
C     NOW DO THE SAME THING FOR THE UPPER POINTS
C     
                  IC1=0
                  
                  IF (VAL(5).NE.BAD .AND. VAL(6).NE.BAD) THEN
C     
C     BILINEAR METHOD
C     
                     VALX=VAL(5)*(1-AL) + VAL(6)*AL
                  ELSE
C     
C     CLOSEST POINT
C     
                     IC1=1
                     IF ((AL.GE.0.5 .AND. VAL(6).NE.BAD) .OR.
     X                    VAL(5).EQ.BAD) THEN
                        IF (((1-AL)*CSP(3,1)).LE.RELMAX) THEN
                           VALX=VAL(6)
                        ELSE
                           VALX=BAD
                        END IF
                     ELSE
                        IF((AL*CSP(3,1)).LE.RELMAX) THEN
                           VALX=VAL(5)
                        ELSE
                           VALX=BAD
                        END IF
                     END IF
                  END IF
                  
                  IC2=0
                  IF (VAL(7).NE.BAD .AND. VAL(8).NE.BAD) THEN
                     VALY=VAL(7)*(1-AL) + VAL(8)*AL
                  ELSE
                     IC2=1
                     IF ((AL.GE.0.5 .AND. VAL(8).NE.BAD) .OR.
     X                    VAL(7).EQ.BAD) THEN
                        IF (((1-AL)*CSP(3,1)).LE.RELMAX) THEN
                           VALY=VAL(8)
                        ELSE
                           VALY=BAD
                        END IF
                     ELSE
                        IF ((AL*CSP(3,1)).LE.RELMAX) THEN
                           VALY=VAL(7)
                        ELSE
                           VALY=BAD
                        END IF
                     END IF
                  END IF
                  
                  IC4=0
                  IF ((VALX.NE.BAD .AND. VALY.NE.BAD) .AND. 
     X                 (IC1.NE.1 .OR. IC2.NE.1)) THEN
                     VALU=VALX*(1-BL) + VALY*BL
                  ELSE
                     IC4=1
                     IF ((BL.GE.0.5 .AND. VALY.NE.BAD) .OR.
     X                    VALX.EQ.BAD) THEN
                        IF (((1-BL)*CSP(3,2)).LE.RELMAX) THEN
                           VALU=VALY
                        ELSE
                           VALU=BAD
                        END IF
                     ELSE
                        IF ((BL*CSP(3,2)).LE.RELMAX) THEN
                           VALU=VALX
                        ELSE
                           VALU=BAD
                        END IF
                     END IF
                  END IF
                  
C     
C     FINAL INTERPOLATION BETWEEN ELEVATION LEVELS
C     
                  IF ((VALL.NE.BAD .AND. VALU.NE.BAD) .AND.
     X                 (IC3.NE.1 .OR. IC4.NE.1)) THEN
                     FVAL=VALL*(1-CL) + VALU*CL
                  ELSE
                     IF (ICORD.EQ.2 .OR. ICORD.EQ.3) THEN
                        DISTA=VALLEV(ITOP)-VALLEV(IBOT)
c                     ELSE IF (ICORD.EQ.1) THEN
c                        DISTA=CSP(3,3)*ATR*(CSP(1,1) + CSP(3,1)*(K-1))
                     ELSE
                        DISTA=CSP(3,3)
                     END IF
                     IF ((CL.GE.0.5 .AND. VALU.NE.BAD) .OR.
     X                    VALL.EQ.BAD) THEN
                        IF (((1-CL)*DISTA).LE.RELMAX) THEN
                           FVAL=VALU
                        ELSE
                           FVAL=BAD
                        END IF
                     ELSE
                        IF ((CL*DISTA).LE.RELMAX) THEN
                           FVAL=VALL
                        ELSE
                           FVAL=BAD
                        END IF
                     END IF
                  END IF
                  
                  
               END IF
               
               IF (ISPEC(I).EQ.1) THEN
C     
C     UNDO LINEAR TRANSFORMATION
C     
                  IF (FVAL.NE.BAD) THEN
                     IF (FVAL.LE.0.0) THEN
                        WRITE(*,*)'***AL,BL,CL,FVAL=',AL,BL,CL,FVAL
                        write(*,*)'val=',val
                     ELSE
                        FVAL=10.0*ALOG10(FVAL)
                     END IF
                  END IF
               END IF
C     
C     STORE THE VALUE IN OUTPUT ARRAY 
C     
 200           CONTINUE
               IF (MEMUSE.EQ.0 .OR. MEMUSE.EQ.1) THEN
                  ROUT(K,J)=FVAL
               ELSE
                  IVD=MAPVID(I,2)
                  LOCDN=IPHOLD(I)
                  LOCDN=LOCDN + (NLEV-1)*NWPF
                  ISKIP=((J-1)*NNX + (K-1))*16
                  JVAL=IBAD
                  IF (FVAL.EQ.BAD) GOTO 152
                  ICHECK=NINT(FVAL/SCALE)
                  IF(IABS(ICHECK).GE.32768) THEN
c                     KBAD(I)=KBAD(I)+1
                     ICHECK=IBAD
                  END IF
                  JVAL=ICHECK
 152              CALL SBYTES(LCMB2(LOCDN),JVAL,ISKIP,16,0,1)
               END IF
 130        CONTINUE
 120     CONTINUE
         IF (MEMUSE.EQ.0 .OR. MEMUSE.EQ.1) THEN
C     
C     STORE OUTPUT ARRAY NOW
C     
         LOCD = 1 + NWPF*NNZ*(I - 1)
         MULT=ID(175+I*5)
         KBAD=0
         call clock(tyme)
         print *,'Trpco-newgrid loop 2: nnx*nny=',nnx,nny,nnx*nny,
     +        ' t=',tyme
         DO 15 IY=1,NNY
            DO 17 IX=1,NNX
               IBUF(IX+(IY-1)*NNX)=IBAD
               IF (ROUT(IX,IY).EQ.BAD) GOTO 17
               ICHECK=NINT(ROUT(IX,IY)*MULT)
               IF (IABS(ICHECK).GE.32768) GOTO 14
               IBUF(IX+(IY-1)*NNX)=ICHECK
               GOTO 17
 14            KBAD=KBAD+1
 17         CONTINUE
 15      CONTINUE
         IF (KBAD.NE.0) THEN
            I1=171+I*5
            I2=I1+3
            PRINT 101, KBAD,(ID(IB),IB=I1,I2),NLEV
 101        FORMAT(' +++  ',I5,' VALUES OUT OF RANGE IN FIELD: ',4A2,
     X           ' AT PLANE=',I3,'  +++')
         ENDIF
         LOCD=LOCD + (NLEV-1)*NWPF
         IF (IDIFF.NE.0) THEN
C     
C     PUT IN PADDING
C     
            KL=1
            LJ=1
            call clock(tyme)
            print *,'Trpco-newgrid loop 3: nnx*nny=',nnx,nny,nnx*nny,
     +           ' t=',tyme
            DO 500 IY=1,NNY
               DO 520 IX=1,NNX
                  ITEMP(LJ)=IBUF(KL)
                  KL=KL+1
                  LJ=LJ+1
 520           CONTINUE
               LJ=LJ+NXX-NNX
 500        CONTINUE
            call clock(tyme)
            print *,'Trpco-newgrid loop 4: nnplane=',nnplane,
     +           ' t=',tyme
            DO 550 IJ=1,NNPLANE
               IBUF(IJ)=ITEMP(IJ)
 550        CONTINUE
         END IF
         
         CALL PUTD(IBUF,NNPLANE,LOCD,IBIT,NBITS,NSKIP,IRP,3,ID,
     X        NXX,NNY,NNZ,NLEV,ITEMP,NWPF,LCMB2,MEMUSE,IVOL)
         END IF
 100  CONTINUE
      
      RETURN
      END



