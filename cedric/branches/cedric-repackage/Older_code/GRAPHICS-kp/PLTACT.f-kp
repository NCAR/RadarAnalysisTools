      SUBROUTINE PLTACT(XACT,YACT,ZACT,BEGACT,DELACT,IBEGCNT,IENDCNT,
     X     ZLEV,AIROPS,IAIRDEF,PWIND,NCWORD,ITIT,UACT,VACT,IWND,REFV,
     X     IAIRCOL,LABFLG,TMAJOR,TMINOR,VFREQ,AIRTHK)
C     
C     THIS SUBROUTINE PLOTS AN AIRCRAFT TRACK THAT WAS READ IN WITH
C     THE READAIR COMMAND.
C     
C     XACT,YACT,ZACT  - POSITION OF AIRCRAFT
C     BEGACT          - START TIME FOR AIRCRAFT (SECONDS)
C     DELACT          - TIME INCREMENT BETWEEN POSITIONS (SECONDS)
C     ZLEV            - CURRENT Z LEVEL
C     AIRTHK          - Line thickness for aircraft track
C     AIROPS          - OPTIONS FOR PLOTTING TRACK
C     IAIRDEF         - 1==> DRAW IT; -1 ==> JUST DEFINE FOR SUBSEQ. OVERLAY
C     REF             - KM/(M/S)
C     

      INCLUDE 'CEDRIC.INC'
      PARAMETER (MXL=20000,NSYMBS=5)
      COMMON /AXSTRB/ AXSFAC(3)
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X     IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X     NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF
      COMMON /AXUNTS/ IUNAXS,LABAXS(3,3),SCLAXS(3,3),AXNAM(3)
      CHARACTER*4 AXNAM
      COMMON /RANGEC/ XBEG,XEND,YBEG,YEND,XRANGE(2),YRANGE(2),DELRG(2)
      CHARACTER*1 IAXIS(3)
      CHARACTER*16 CFMTX,CFMTY
      CHARACTER*8 NOW,IFMTX(2),IFMTY(2),ITIT(5)
      CHARACTER*40 CITIT
      CHARACTER*80 JTIT
      DIMENSION XACT(MXL),YACT(MXL),ZACT(MXL),PWIND(2,3),
     X     NCWORD(3),UACT(MXL),VACT(MXL),VLBUF(7),ICOLMAP(10)
      DATA VLBUF/20.,10.,5.,2.,1.,.5,.1/
      DATA ICOLMAP/1,61,5,8,25,38,32,2,60,0/
      DATA IAXIS/'X','Y','Z'/
      CHARACTER*8 AIROPS,DEFOPS,STIME
      CHARACTER*6 SYMBS1(5),SYMBS2(5)
      DATA CL,CT,ST/320.0, 0.92388, 0.382683/
      DATA DEFOPS/'00110000'/
      DATA EPS/0.5/
      DATA SLOP/0.01/
      DATA CLRMN,CLREL,RMN,RMX/0.5,0.25,160.,6400./
      DATA SYMBS1/'&KGL&G', '&KRU&+', '&KGU&D', '&KGL&E', '&KRL&X'/
      DATA SYMBS2/'&KGL&G', '&KRU&+', '&KGU&D', '&KGL&E', '&KRL&X'/
      DATA SYMSIZ1,SYMSIZ2/20.0,20.0/

      LOCPLT(R)=1023.*R
      
C      print *,'Pltact-1: labflg,iairdef=',labflg,iairdef
      CALL GQPLCI(IERROR,IOLDCOL)
      IF (IERROR.NE.0)THEN
         WRITE(*,*)'***ERROR CALLING GQPLCI IN PLTACT***'
         CALL FLUSH_STDOUT
      END IF
      CALL SFLUSH
      IF (IAIRCOL.GT.0) CALL GSPLCI(ICOLMAP(IAIRCOL))
      IF (AIROPS.EQ.' ') AIROPS=DEFOPS
      READ(AIROPS,15)IDASH,IPLTTIM,IPLTVEC,IMAJPAT,IMINPAT,NLIM
 15   FORMAT(6I1)
      
      IF (IPLTVEC.EQ.0) IWND=0
      IMAJPAT=IMAJPAT+1
      IMINPAT=IMINPAT+1
      IF (IMAJPAT.GT.NSYMBS) IMAJPAT=1
      IF (IMINPAT.GT.NSYMBS) IMINPAT=1

      L1=NCWORD(1)
      L2=NCWORD(2)
      L3=NCWORD(3)
      CALL RGINI(XL,XR,YB,YT,PWIND,CSP,NCX,NCWORD)
      DZ=CSP(3,L3)
      ZLIM=DZ*NLIM
C     HANDLE A CASE WITH JUST ONE LEVEL (DZ=0.0) ; SET TO LARGE VALUE
      IF (ZLIM.EQ.0.0 .AND. NLIM.EQ.0) ZLIM=1000.
C     
C     SAVE INTO FLASH BUFFER
C     
      CALL GFLAS1(3)
      CALL GSCLIP(1)
      CALL SET(XL,XR,YB,YT,PWIND(1,L1),PWIND(2,L1),PWIND(1,L2),
     X     PWIND(2,L2),1)
      CALL DASHDB(O'170360')

C     Get default line thickness (ILW) and reset to JLW.
C     Restore line thickness before leaving routine.
C
      CALL GETUSV('LW',ILW)
      JLW=AIRTHK*ILW
      IF(JLW.LT.ILW)JLW=ILW
      CALL SETUSV('LW',JLW)
C      print *,'PLTACT: ilw,jlw,labflg=',ilw,jlw,labflg

      IFIR=0
      DO I=IBEGCNT,IENDCNT
         IF (IFIR.EQ.0 .OR. I.EQ.IENDCNT)THEN
C     
C     WANT TO LABEL FIRST POINT OF TRACK THAT IS *INSIDE GRID* AND
C     LAST POINT OF TRACK WITH THEIR TIMES.
C     
            IHR= (BEGACT + DELACT*(I-1))/3600.
            IMN= (BEGACT + DELACT*(I-1) -(IHR*3600))/60.
            ISC= (BEGACT + DELACT*(I-1) -(IHR*3600.)-(IMN*60.))
            IF (ISC.GE.60)THEN
               ISC=ISC-60
               IMN=IMN+1
            END IF
            IF (IMN.GE.60)THEN
               IMN=IMN-60
               IHR=IHR+1
            END IF
            WRITE(STIME,10) IHR,IMN,ISC
C 10         FORMAT(I2.2,':',I2.2,':',I2.2)
 10         FORMAT(3I2.2)
            IF (I.LT.IENDCNT)THEN
               IF (YACT(I+1).GT.YACT(I))THEN
                  YPOS=CFUY(CUFY(YACT(I)) - .02)
               ELSE
                  YPOS=CFUY(CUFY(YACT(I)) + .02)
               ENDIF
            ELSE
               YPOS=CFUY(CUFY(YACT(I)) + .02)
            END IF
            IF (XACT(I).GE.PWIND(1,L1) .AND. XACT(I).LE.PWIND(2,L1)
     X           .AND. YACT(I).GE.PWIND(1,L2) .AND. YACT(I).LE.
     X           PWIND(2,L2) .AND. ZACT(I).GT.(ZLEV-ZLIM) .AND.
     X           ZACT(I).LT.(ZLEV+ZLIM))THEN

C---------don't know why, but need one or the other calls
C         to finish the grid and labels for aircraft track plots
C         moved '.' plot to lower edge of plot
C         
               CALL GSCLIP(0)
               IF (IPLTTIM.NE.0)THEN
                  CALL PLCHMQ(XACT(I),YPOS,STIME,8.,0.,0.)
               ELSE
c                  CALL PLCHMQ(XACT(I),YPOS,'.',1.,0.,0.)
                  CALL PLCHMQ(XACT(I),0.0,'.',1.,0.,0.)
               END IF
               CALL GSCLIP(1)
               IF (IFIR.EQ.0) IFIR=I
C     PLACE TICK MARKS ALONG TRACK
               IF (TMAJOR.NE.0.0)THEN
                  IF (MOD(INT((I-IFIR)*DELACT),INT(TMAJOR)).EQ.0)THEN
                     CALL PLCHHQ(XACT(I),YACT(I),SYMBS2(IMAJPAT),
     X                    SYMSIZ2,0.,0.)
                  ELSE IF (TMINOR.NE.0.0)THEN
                     IF (MOD(INT((I-IFIR)*DELACT),INT(TMINOR)).EQ.0)THEN
                        CALL PLCHHQ(XACT(I),YACT(I),SYMBS1(IMINPAT),
     X                       SYMSIZ1,0.,0.)
                     END IF
                  END IF
               ELSE IF (TMINOR.NE.0.0)THEN
                  IF (MOD(INT((I-IFIR)*DELACT),INT(TMINOR)).EQ.0)THEN
                     CALL PLCHHQ(XACT(I),YACT(I),SYMBS1(IMINPAT),
     X                    SYMSIZ1,0.,0.)
                  END IF
               END IF
               IF (I.EQ.IENDCNT .AND. ILASTPT.EQ.1)THEN
C     
C     CONNECT TO PREVIOUS POINT
C     
                  IF ((IDASH.EQ.0 .AND. ZACT(I).LT.ZLEV) .OR. 
     X                 IDASH.EQ.1 .AND. IENDCNT.GT.1)THEN
                     CALL LINED(XACT(I-1),YACT(I-1),XACT(I),YACT(I))
                  ELSE IF(IDASH.EQ.2 .AND. IENDCNT.GT.1)THEN
                     CALL LINE (XACT(I-1),YACT(I-1),XACT(I),YACT(I))
                  END IF
               END IF
               ILASTPT=1
            ELSE
               ILASTPT=0
            END IF
         ELSE
C
C     NO TIME LABEL, JUST PLOT THE POINT
C
            IF (ZACT(I).GT.(ZLEV-ZLIM) .AND. ZACT(I).LT.
     X           (ZLEV+ZLIM))THEN
               IF (TMAJOR.NE.0.0)THEN
                  IF (MOD(INT((I-IFIR)*DELACT),INT(TMAJOR)).EQ.0)THEN
                     CALL PLCHHQ(XACT(I),YACT(I),SYMBS2(IMAJPAT),
     X                    SYMSIZ2,0.,0.)
                  ELSE IF (TMINOR.NE.0.0)THEN
                     IF (MOD(INT((I-IFIR)*DELACT),INT(TMINOR)).EQ.0)THEN
                        CALL PLCHHQ(XACT(I),YACT(I),SYMBS1(IMINPAT),
     X                           SYMSIZ1,0.,0.)
                     END IF
                  END IF
               ELSE IF (TMINOR.NE.0.0)THEN
                  IF (MOD(INT((I-IFIR)*DELACT),INT(TMINOR)).EQ.0)THEN
                     CALL PLCHHQ(XACT(I),YACT(I),SYMBS1(IMINPAT),
     X                 SYMSIZ1,0.,0.)
                  END IF
               END IF
               IF ((IDASH.EQ.0 .AND. ZACT(I).LT.ZLEV) .OR. IDASH.EQ.1
     X           .AND. ILASTPT.EQ.1 .AND. IENDCNT.GT.1)THEN
                  CALL LINED(XACT(I-1),YACT(I-1),XACT(I),YACT(I))
               ELSE IF(IDASH.EQ.2 .AND. IENDCNT.GT.1)THEN
                  CALL LINE (XACT(I-1),YACT(I-1),XACT(I),YACT(I))
               END IF
               ILASTPT=1
            ELSE
               ILASTPT=0
            END IF
         END IF
         IF (IWND.NE.0 .AND. VFREQ.NE.0.0 .AND. ILASTPT.EQ.1)THEN
            IF (MOD(INT((I-IFIR)*DELACT),INT(VFREQ)).EQ.0)THEN
C
C     DRAW VECTOR
C
            U=UACT(I)
            V=VACT(I)
            IF (U.NE.BAD .AND. V.NE.BAD)THEN
               XND=XACT(I)+U*REFV
               YND=YACT(I)+V*REFV
               CALL FL2INT(XACT(I),YACT(I),N1,N2)
               CALL FL2INT(XND,YND,N3,N4)
               CALL PLOTIF(CMFX(N1),CMFY(N2),0)
               CALL PLOTIF(CMFX(N3),CMFY(N4),1)
               DX=N3-N1
               DY=N4-N2
               R=SQRT(DX*DX+DY*DY)
               IF(R.LT.EPS) GO TO 40
               C1=CL/R
C     
C     PROPORTIONAL ARROWHEADS
C     
               IF(R.LT.CLRMN) GO TO 40
               C1=CLREL
               IF(R.LT.RMN) C1=RMN*CLREL/R
               IF(R.GT.RMX) C1=RMX*CLREL/R
               N5=N3-C1*(CT*DX-ST*DY)
               N6=N4-C1*(CT*DY+ST*DX)
               N7=N3-C1*(CT*DX+ST*DY)
               N8=N4-C1*(CT*DY-ST*DX)
               CALL PLOTIF(CMFX(N5),CMFY(N6),0)
               CALL PLOTIF(CMFX(N3),CMFY(N4),1)
               CALL PLOTIF(CMFX(N7),CMFY(N8),1)
 40            CONTINUE
            END IF
         END IF
         END IF
      END DO
      IF (IWND.NE.0)THEN
C
C        DRAW U REFERENCE VECTOR
C
         MX=KUPX(PWIND(2,L1)) + 125
         MY=KUPY(PWIND(1,L2)) + 15
            
         DO 11 I=1,7
            VLEN=VLBUF(I)
            MV1=KUPX(CPUX(MX) - VLEN*REFV)
            IF (MV1.GE.KUPX(PWIND(2,L1))) GOTO 13
 11      CONTINUE
 13      CONTINUE
         IF (VLEN.GE.1.0)THEN
            WRITE (JTIT,103)VLEN
 103        FORMAT(F3.0,' M/S')
         ELSE
            WRITE(JTIT,174)VLEN
 174        FORMAT(F3.1,' M/S')
         END IF
         CALL GSCLIP(0)
         MV2=MX
         CALL PLCHMQ(CPUX(MV2),CPUY(MY-17),JTIT(1:7),12.,0.,
     X        1.)
         CALL LINE(CPUX(MV1),CPUY(MY),CPUX(MV2),CPUY(MY))
         CALL LINE(CPUX(MV1),CPUY(MY),CPUX(MV1+8),CPUY(MY-12))
         CALL LINE(CPUX(MV1),CPUY(MY),CPUX(MV1+8),CPUY(MY+8))
C     
C     DRAW V REFERENCE VECTOR IF DIFFERENCE IN SCALING
C     
         IF (AXSFAC(3).NE.1.0)THEN
            MY1=MY
            MY2=MY+(MV2-MV1)
            CALL LINE (CPUX(MV2),CPUY(MY1),CPUX(MV2),CPUY(MY2))
            CALL LINE (CPUX(MV2),CPUY(MY2),CPUX(MV2+8),CPUY(MY2-8))
            CALL LINE (CPUX(MV2),CPUY(MY2),CPUX(MV2-8),CPUY(MY2-8))
            VLEN=VLEN/AXSFAC(3)
            WRITE (JTIT,155)VLEN
 155        FORMAT(F4.1,' M/S')
            CALL PLCHMQ(CPUX(MV2+10),CPUY(MY1),JTIT(1:8),12.,90.,-1.)
         END IF
      END IF
      CALL GSCLIP(0)
      CALL SETUSV('LW',ILW)
C      print *,'PLTACT-2: labflg=',labflg
      IF (LABFLG.GT.5)THEN
         CALL PLCHHQ(CPUX(220),CPUY(985),SYMBS2(IMAJPAT),SYMSIZ2,0.,-1.)
         WRITE(JTIT,200)INT(TMAJOR)
 200     FORMAT(' EVERY ' ,I4,' SEC')
         CALL PLCHMQ(CPUX(227),CPUY(985),JTIT(1:15),12.,0.,-1.)
      END IF
      CALL SFLUSH
      CALL GSPLCI(IOLDCOL)
      CALL GFLAS2

      IF (IAIRDEF.EQ.1)THEN
C     
C     PRODUCE A COMPLETELY LABELED AIRCRAFT TRACK PLOT
C     
         WRITE (CITIT,500)ITIT
 500     FORMAT(5A8)
         CALL DATEE(NOW)
         CF=1./ID(69)
         XOR=ID(40)*CF
         SF=1./ID(68)
         XREL=ID(41)*SF
         YREL=ID(42)*SF
         CALL MAJMIN(XRANGE(1),DELRG(1),IFMTX,MAJORX,MINORX,NDIG1,ISZ1,
     X               YRANGE(1),DELRG(2),IFMTY,MAJORY,MINORY,NDIG2,ISZ2)
C         CALL MAJMIN(DELRG(1),IFMTX,MAJORX,MINORX)
C         CALL MAJMIN(DELRG(2),IFMTY,MAJORY,MINORY)
         WRITE (CFMTX,510)IFMTX
 510     FORMAT(2A8)
         WRITE (CFMTY,510)IFMTY
C         print *,'PLTACT-3: labflg=',labflg

C     Always draw grid and grid labels
         CALL LABMOD(CFMTX,CFMTY,NDIG1,NDIG2,ISZ1,ISZ2,4,4,0)
         CALL PERIML(MAJORX,MINORX,MAJORY,MINORY)

C     JUST DRAW A BOX AROUND PLOT-NO TICK MARKS
c            CALL TICKS(0,0)
c            CALL PERIM(0,0,0,0)
c            CALL TICKS(12,8)
            
         WRITE (JTIT,106)(ID(I),I=116,121),(ID(I),I=125,127),
     X        (ID(I),I=13,15),AXNAM(L3),ZLEV,
     X        LABAXS(L3,IUNAXS)
 106     FORMAT(I2.2,'/',I2.2,'/',I2.2,6X,I2.2,2(':',I2.2),'-',
     X        I2.2,2(':',I2.2),7X,3A2,7X,A2,'=',F7.2,' ',A4)
C         print *,'JTIT=',jtit
C         print *,'PLTACT-4: labflg=',labflg
         IF (LABFLG.GT.5)THEN
            CALL PLCHMQ(CPUX(60),CPUY(1010),JTIT(1:66),12.,0.,-1.)
            WRITE (JTIT,107)NOW
 107        FORMAT('(AS OF ',A8,')')
            CALL PLCHMQ(CPUX(10),CPUY(985),JTIT(1:16),12.,0.,-1.)
            WRITE (JTIT,105)XREL,YREL,XOR
 105        FORMAT('ORIGIN=(',F7.2,',',F7.2,') KM   X-AXIS=',F5.1,
     X           ' DEG')
            CALL PLCHMQ(CPUX(430),CPUY(985),JTIT(1:46),12.,0.,-1.)
            CALL PLCHMQ(CPUX(200),CPUY(960),CITIT,12.,0.,-1.)
            WRITE (JTIT,108)AXNAM(L2),LABAXS(L2,IUNAXS)
 108        FORMAT(A1,' ',A4)
         END IF
         LOCY=LOCPLT(YB+(YT-YB)*0.15)
C         print *,'PLTACT-5: labflg=',labflg
         IF (LABFLG.GT.5)THEN
            CALL PLCHMQ(CPUX(10),CPUY(LOCY),JTIT(1:6),12.,90.,-1.)
            WRITE (JTIT,108)AXNAM(L1),LABAXS(L1,IUNAXS)
         END IF
         LOCY=LOCPLT(YB)-50
C         print *,'PLTACT-6: labflg=',labflg
         IF (LABFLG.GT.5)THEN
            CALL PLCHMQ(CPUX(150),CPUY(LOCY),JTIT(1:6),12.,0.,-1.)
         END IF
         
         CALL GFLAS3(3)
C         print *,'PLTACT-7: labflg=',labflg
         IF (LABFLG.GT.5)THEN
            CALL MYFRAME
         ELSE
            CALL FRAME
         END IF
      END IF

      RETURN
      
      END
      
      
      
