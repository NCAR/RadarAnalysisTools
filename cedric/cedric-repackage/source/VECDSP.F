      SUBROUTINE VECDSP(IBUF,RBUF,WBUF,LEV,RLEV,PLWIND,NCWORD,IFLVEC,
     X                  ITIT,VECTIN,IFLASH,MXFLSH,ITYP,NST,NVECCOL,
     X                  ISKPI,ISKPJ,LABFLG)
C
C        GENERATES THE VECTOR DISPLAY PLOT (OVERLAY AND WITH LABELING)
C                  EITHER A VECTOR OR A STREAMLINE PLOT CAN BE PRODUCED
C                  DEPENDING UPON THE VALUE OF VECTIN(2)- SPEED REFERENCE.
C                  ITYP=ANYTHING BUT ZERO, OVERLAY ONLY
C                      =1, OVERLAY + FULL PLOT WITH LABELING
C                   NST=0, EVERYTHING O.K.
C                      =1, INSUFFICIENT PARAMETERIZATION
C
      INCLUDE 'CEDRIC.INC'      
      COMMON /AXSTRB/ AXSFAC(3)
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X                IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X                NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF
      COMMON /AXUNTS/ IUNAXS,LABAXS(3,3),SCLAXS(3,3),AXNAM(3)
      CHARACTER*4 AXNAM
      COMMON /RANGEC/ XBEG,XEND,YBEG,YEND,XRANGE(2),YRANGE(2),DELRG(2)
      CHARACTER*1 IAXIS(3)
      CHARACTER*2 NAME(4,2)
      DIMENSION IBUF(1),RBUF(1),IFLASH(MXFLSH),PLWIND(2,3),NCWORD(3),
     X          IFLVEC(2),VECTIN(5),WBUF(1),VLBUF(7)
      CHARACTER*16 CFMTX,CFMTY
      CHARACTER*8 NOW,IFMTX(2),IFMTY(2),ITIT(5)
      CHARACTER*80 JTIT
      CHARACTER*80 CITIT
      DATA IAXIS/'X','Y','Z'/
      DATA VLBUF/20.,10.,5.,2.,1.,.5,.1/
      DATA IN/0/
      DIMENSION ICOLMAP(10)
      DATA ICOLMAP/1,61,5,8,25,38,32,2,60,0/
      CHARACTER*1 BLANK
      PARAMETER (BLANK=' ')

      CHARACTER*80 CLL,CLR,CUL,CUR
      DATA CLL,CLR,CUL,CUR /'CLL','CLR','CUL','CUR'/

C     Patch to allow for turning off framing (LJM 7/30/2011)
C     Do this to get several radial velocities as vectors
C     onto a single plot.  To do this reset FRAMIT to .FALSE.
C
      LOGICAL FRAMIT
      DATA FRAMIT /.TRUE./

C     Conversion from fractional to PAU
C
      LOCPAU(FRACT)=NINT(FRACT*1024.)

C     Conversion from PAU to fractional
C
      FRAC(IPAU)=FLOAT(IPAU)/1024.
      
C     Get the initial graphics window mapping, i.e.
C     both fractional and user coordinates.  Then
C     CALL SET to set window to fractional.
C
      LinLin=1
      CALL GETSET(FL,FR,FB,FT,UL,UR,UB,UT,LinLin)
c      print *,'VECDSP: initial graphics window mapping'
c      print *,'VECDSP: fl,fr,fb,ft=',fl,fr,fb,ft
c      print *,'VECDSP: ul,ur,ub,ut=',ul,ur,ub,ut
c      print *,'VECDSP: set graphics window to fractional'
      CALL SET(FL,FR,FB,FT,FL,FR,FB,FT,1)

c     Convert fractional window to PAUs
c
c      print *,'VECDSP: Convert fractional window to PAUs'
      IL=LOCPAU(FL)
      IR=LOCPAU(FR)
      JB=LOCPAU(FB)
      JT=LOCPAU(FT)
c      print *,'VECDSP: il,ir,jb,jt=',il,ir,jb,jt      
c      print *,'VECDSP: cll,clr,cul,cur=',cll,clr,cul,cur
c      CALL MY_PLCHMQ(IL,JB,CLL(1:3),12.,0.,-1.)
c      CALL MY_PLCHMQ(IR,JB,CLR(1:3),12.,0.,-1.)
c      CALL MY_PLCHMQ(IL,JT,CUL(1:3),12.,0.,-1.)
c      CALL MY_PLCHMQ(IR,JT,CUR(1:3),12.,0.,-1.)

c     Convert PAU window back to fractionals
c
c      print *,'VECDSP: Convert PAU window back to fractionals'
      RL=FRAC(IL)
      RR=FRAC(IR)
      RB=FRAC(JB)
      RT=FRAC(JT)
c      print *,'VECDSP: rl,rr,rb,rt=',rl,rr,rb,rt      
c      CALL PLCHMQ(RL,RB,CLL(1:3),12.,45.,-1.)
c      CALL PLCHMQ(RR,RB,CLR(1:3),12.,45.,-1.)
c      CALL PLCHMQ(RL,RT,CUL(1:3),12.,45.,-1.)
c      CALL PLCHMQ(RR,RT,CUR(1:3),12.,45.,-1.)

      WRITE (CITIT(1:80),500)(ITIT(I),I=1,5),BLANK
 500  FORMAT(5A8,39A1,'X')
      print *,'VECDSP: itit =',itit
      print *,'VECDSP: citit=',citit
C
C        INITIALIZE PARAMETERIZATION
C
      IVDENS=VECTIN(1)
      VMS=VECTIN(2)
C
C        PARAMETERIZATION IS SUFFICIENT- PROCEED WITH DISPLAY
C
      L1=NCWORD(1)
      L2=NCWORD(2)
      L3=NCWORD(3)
      LOC=1
C
C        FETCH IN THE TWO VECTOR FIELDS
C
      DO 3 I=1,2
         M=IFLVEC(I)
         CALL COPCX(NAME(1,I),NAMF(1,M),4)
         IFLD=MAPVID(M,1)
         CALL FETCHD(IN,ID,LEV,IFLD,IBUF,RBUF(LOC),N1,N2,
     X               L3,BAD,RLEV,NST)
         IF(NST.NE.0) RETURN
         LOC=LOC+N1*N2
    3 CONTINUE
c      print *,'VECDSP: before RGINI -     plwinds=',
c     +        PLWIND(1,L1),PLWIND(2,L1),PLWIND(1,L2),PLWIND(2,L2)
      CALL RGINI(XL,XR,YB,YT,PLWIND,CSP,NCX,NCWORD)
c      print *,'VECDSP: after RGINI - xl,xr,yb,yt=',xl,xr,yb,yt
      LL=1
      CALL GETSET(FL,FR,FB,FT,UL,UR,UB,UT,LL)
c      print *,'VECDSP: after RGINI graphics window mapping'
c      print *,'VECDSP: fl,fr,fb,ft=',fl,fr,fb,ft
c      print *,'VECDSP: ul,ur,ub,ut=',ul,ur,ub,ut
C
C        SAVE THE VECTOR BACKGROUND IN IFLASH BUFFER
C
      CALL GFLAS1(1)
      CALL GSCLIP(0)
      WRITE (JTIT,102)((NAME(I,J),I=1,4),J=1,2),IVDENS
  102 FORMAT(4A2,' ',4A2,' EVERY ',I2,' PT ')
c      print *,'VECDSP: jtit(1:30)=',jtit(1:30)
      LOCX=LOCPAU(FL)+212
      LOCY=LOCPAU(YB)-50
c      print *,'VECDSP: fl,locx,ub,locy=',fl,locx,ub,locy
c      RX1=CPUX(LOCX)
c      RY1=CPUY(LOCY)
c         print *,'VECDSP:   using CPUX and CPUY - rx1,ry1=',
c     +     rx1,ry1
      RX1=RCPUX(LOCX)
      RY1=RCPUY(LOCY)
c         print *,'VECDSP  using RCPUX and RCPUY - rx1,ry1=',
c     +     rx1,ry1
      RX1=FRAC(LOCX)
      RY1=FRAC(LOCY)
c         print *,'VECDSP      using  FRAC(IPAU) - rx1,ry1=',
c     +     rx1,ry1
      IF (LABFLG.GT.5) THEN
         CALL PLCHMQ(RX1,RY1,JTIT(1:30),12.,0.,-1.)
         CALL MY_PLCHMQ(LOCX,LOCY,JTIT(1:30),12.,0.,-1.)
      END IF

      IF(VMS.LE.0.0) THEN
C
C        STREAMLINE PLOT
C
         NVEC=1
         LOC=1+N1*N2
C
C        SAVE WINDOW LIMITS SINCE KILDAT READJUSTS THEM AUTOMATICALLY
C
         XBSV=XBEG
         XESV=XEND
         YBSV=YBEG
         YESV=YEND
         CALL KILDAT(RBUF(  1),N1,N2,XBEG,XEND,YBEG,YEND,NPX,NPY,
     X               ISKPI,ISKPJ)
         CALL KILDAT(RBUF(LOC),N1,N2,XBSV,XESV,YBSV,YESV,NPX,NPY,
     X               ISKPI,ISKPJ)
         CALL SET(XL,XR,YB,YT,XBEG,XEND,YBEG,YEND,1)
c         print *,'VECDSP: after SET - xl,xr,yb,yt        =',
c     +        xl,xr,yb,yt
c         print *,'VECDSP: after SET - xbeg,xend,ybeg,yend=',
c     +        xbeg,xend,ybeg,yend

C
C     IF GRID SPACING IN TWO DIRECTIONS IS NOT IDENTICAL, SCALE THE
C     COMPONENT THAT CORRESPONDS TO THE DIRECTION WITH THE COARSER
C     SPACING.
         IF (CSP(3,L1).GT.CSP(3,L2)) THEN
            DO I=1,(N1*N2)
               RBUF(I)=RBUF(I)*CSP(3,L2)/CSP(3,L1)
            END DO
         ELSE IF (CSP(3,L2).GT.CSP(3,L1)) THEN
            DO I=1,(N1*N2)
               RBUF(LOC+I-1)=RBUF(LOC+I-1)*CSP(3,L1)/CSP(3,L2)
            END DO
         END IF
         CALL GQPLCI(IERROR,IOLDCOL)
         IF (IERROR.NE.0) THEN
            WRITE(*,*)'***ERROR CALLING GQPLCI IN VECDSP***'
            CALL FLUSH_STDOUT
         END IF
         CALL SFLUSH
         IF (NVECCOL.GT.0) CALL GSPLCI(ICOLMAP(NVECCOL))
         CALL STRMLN(RBUF,RBUF(LOC),WBUF,N1,NPX,NPY,1,IVDENS,BAD,IER)
         CALL SFLUSH
         CALL GSPLCI(IOLDCOL)
      ELSE
C
C        VECTOR PLOT
C
         VKM=VECTIN(3)
         IAROW=VECTIN(4)
         HPRO=VECTIN(5)
         OMS=VKM/VMS
c         print *,'VECDSP: framit,vkm,vms,oms=',framit,vkm,vms,oms
         INC=IVDENS
         ILV=-1
         IF(INC.GT.0) GO TO 20
            ILV=IABS(INC+1)
            INC=1
   20    CONTINUE
         CALL SET(XL,XR,YB,YT,
     X          PLWIND(1,L1),PLWIND(2,L1),PLWIND(1,L2),PLWIND(2,L2),1)
c         print *,'VECDSP: after SET - xl,xr,yb,yt          =',
c     +        xl,xr,yb,yt
c         print *,'VECDSP: after SET - plwind(L1),plwind(l2)=',
c     +        PLWIND(1,L1),PLWIND(2,L1),PLWIND(1,L2),PLWIND(2,L2)
C
C        DRAW U REFERENCE VECTOR
C
c        KUPX and KUPY convert user (geometric) coordinates to PAUs
c        CPUX and CPUX convert PAUs to user (geometric) coordinates
c        PLWIND(2,L1) - User's X-coordinate of lower-right corner
c        PLWIND(1,L2) - User's Y-coordinate of lower-right corner
c        (MX,MY) - PAUs relative user's XY-coordinates of 
c                  lower-right corner
c
         MX=KUPX(PLWIND(2,L1)) + 125
         MY=KUPY(PLWIND(1,L2)) + 15
c         print *,'VECDSP: mx,my=',mx,my

         DO 10 I=1,7
            VLEN=VLBUF(I)
            U_MX=RCPUX(MX)
            MV1=KUPX(U_MX - VLEN*OMS)
            RP=KUPX(PLWIND(2,L1))
c           print*,'VECDSP-kupx: i,vlbuf(i),vlen,mv1,rp=',
c    +           i,vlbuf(i),vlen,mv1,rp
            IF (MV1.GE.KUPX(PLWIND(2,L1))) GOTO 15
   10    CONTINUE
   15    CONTINUE
         IF (VLEN.GE.1.0) THEN
            WRITE (JTIT,103)NINT(VLEN)
 103        FORMAT(I3,' M/S')
         ELSE
            WRITE(JTIT,174)VLEN
 174        FORMAT(F3.1,' M/S')
         END IF
         CALL GSCLIP(0)
         MV2=MX
         RX=FLOAT(MV2)/1023.
         RY=FLOAT(MY-17)/1023.
c         print *,'VECDSP: jtit(1:7)=',jtit(1:7)
c         print *,'VECDSP: mv2,my-17=',mv2,my-17
c         print *,'VECDSP:     rx,ry=',rx,ry
c         CALL PLCHMQ(RX,RY,JTIT(1:7),12.,0.,1.)
         CALL MY_PLCHMQ(MV2-80,MY-25,JTIT(1:7),12.,0.,-1.)
         CALL GETSET(FL,FR,FB,FT,UL,UR,UB,UT,LL)
c         print *,'VECDSP: ul,ur,ub,ut=',ul,ur,ub,ut
c         print *,'VECDSP: fl,fr,fb,ft=',fl,fr,fb,ft
c         print *,'VECDSP: before ref vector - mv1,my,mv2,my=',
c     +        mv1,my,mv2,my
         RX1=RCPUX(MV1)
         RY1=RCPUY(MY)
         RX2=RCPUX(MV2)
         RY2=RCPUY(MY)
c         print *,'VECDSP: using CPUX and CPUY - rx1,ry1,rx2,ry2=',
c     +        rx1,ry1,rx2,ry2 
         CALL LINE(RCPUX(MV1),RCPUY(MY),RCPUX(MV2),RCPUY(MY))
         CALL LINE(RCPUX(MV1),RCPUY(MY),RCPUX(MV1+8),RCPUY(MY-12))
         CALL LINE(RCPUX(MV1),RCPUY(MY),RCPUX(MV1+8),RCPUY(MY+8))
         CALL VECDRW(RBUF,N1,N2,CSP,PLWIND,L1,L2,INC,ILV,OMS,HPRO,
     X               IAROW,BAD,NVEC,NVECCOL,ISKPI,ISKPJ)
C
C     DRAW V REFERENCE VECTOR IF DIFFERENCE IN SCALING
C
         IF (AXSFAC(3).NE.1.0) THEN
            MY1=MY
            MY2=MY+(MV2-MV1)
            CALL LINE (RCPUX(MV2),RCPUY(MY1),RCPUX(MV2),RCPUY(MY2))
            CALL LINE (RCPUX(MV2),RCPUY(MY2),RCPUX(MV2+8),RCPUY(MY2-8))
            CALL LINE (RCPUX(MV2),RCPUY(MY2),RCPUX(MV2-8),RCPUY(MY2-8))
            VLEN=VLEN/AXSFAC(3)
            WRITE (JTIT,155)VLEN
 155        FORMAT(F4.1,' M/S')
c            print *,'VECDSP: mv2+10,my1=',mv2+10,my1
            CALL MY_PLCHMQ(MV2+10,MY1,JTIT(1:8),12.,90.,-1.)
         END IF
      END IF
C
      CALL GFLAS2
      IF(NVEC.NE.0) GO TO 30
C
C        NO VECTORS PLOTTED
C
         CALL GFLAS1(1)
         WRITE (JTIT,104)((NAME(I,J),I=1,4),J=1,2)
  104    FORMAT('NO ',4A2,',',4A2,' VECTORS AT THIS LEVEL')
         CALL MY_PLCHMQ(275,LOCY,JTIT(1:42),12.,0.,-1.)
         CALL GFLAS2
   30 CONTINUE
      IF(ITYP.NE.1) GO TO 90
C
C        PRODUCE A COMPLETELY LABELED VECTOR PLOT
C
      CALL MAJMIN(XRANGE(1),DELRG(1),IFMTX,MAJORX,MINORX,NDIG1,ISZ1,
     X            YRANGE(1),DELRG(2),IFMTY,MAJORY,MINORY,NDIG2,ISZ2)
C      CALL MAJMIN(DELRG(2),IFMTY,MAJORY,MINORY)
      CALL SET(XL,XR,YB,YT,XRANGE(1),XRANGE(2),YRANGE(1),YRANGE(2),1)
      WRITE (CFMTX,510)IFMTX
 510  FORMAT(2A8)
      WRITE (CFMTY,510)IFMTY

C     Always draw grid and grid labels
C
      CALL LABMOD(CFMTX,CFMTY,NDIG1,NDIG2,ISZ1,ISZ2,4,4,0)
      CALL PERIML(MAJORX,MINORX,MAJORY,MINORY)

      CALL DATEE(NOW)
      CF=1./ID(69)
      XOR=ID(40)*CF
      SF=1./ID(68)
      XREL=ID(41)*SF
      YREL=ID(42)*SF
      WRITE (JTIT,106)(ID(I),I=116,121),(ID(I),I=125,127),
     X                    (ID(I),I=13,15),AXNAM(L3),RLEV,
     X                     LABAXS(L3,IUNAXS)
  106 FORMAT(I2.2,'/',I2.2,'/',I2.2,6X,I2.2,2(':',I2.2),'-',
     X       I2.2,2(':',I2.2),7X,3A2,7X,A2,'=',F7.2,' ',A4)
      IF (LABFLG.GT.5) THEN
         CALL MY_PLCHMQ(60,1010,JTIT(1:66),12.,0.,-1.)
         WRITE (JTIT,107)NOW
 107     FORMAT('(AS OF ',A8,')')
         CALL MY_PLCHMQ(10,985,JTIT(1:16),12.,0.,-1.)
         WRITE (JTIT,105)XREL,YREL,XOR
 105     FORMAT('ORIGIN=(',F7.2,',',F7.2,') KM   X-AXIS=',F5.1,' DEG')
         CALL MY_PLCHMQ(430,985,JTIT(1:46),12.,0.,-1.)
         CALL MY_PLCHMQ(200,960,CITIT,12.,0.,-1.)
         WRITE (JTIT,108)AXNAM(L2),LABAXS(L2,IUNAXS)
 108     FORMAT(A1,' ',A4)
      END IF
c      print *,'Vecdsp: axnam',axnam
      LOCY=LOCPAU(YB+(YT-YB)*0.15)
      IF (LABFLG.GT.5) THEN
         CALL MY_PLCHMQ(10,LOCY,JTIT(1:6),12.,90.,-1.)
         WRITE (JTIT,108)AXNAM(L1),LABAXS(L1,IUNAXS)
      END IF
      LOCY=LOCPAU(YB)-50
      IF (LABFLG.GT.5) THEN
         CALL MY_PLCHMQ(150,LOCY,JTIT(1:6),12.,0.,-1.)
      END IF
C
C        FLUSH VECTOR OVERLAY TO DEVICE
C        VMS .LT. 0 ==> No call frame (multiple vectors per plot)
C
      CALL GFLAS3(1)
      IF (LABFLG.GT.5) THEN
         IF(FRAMIT)CALL MYFRAME
      ELSE
         IF(FRAMIT)CALL FRAME
      END IF
   90 CONTINUE
C
C        NORMAL TERMINATION
C
      NST=0
      RETURN
      END
