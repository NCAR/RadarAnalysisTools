      SUBROUTINE SYNNER(LASTLV,IBUF,NPS,MXKP,TBUF,NPLIN,RBUF,NX,NY,NZ,
     X                  IWEQ,ITEQ,IDSAV,MAP,DTEST,ITEQN,ITANAL,
     X                  WDADV,WSADV,KEEPW,KEEPPE,NMF,NST,IACTC)
C
C        READS THE SYNTHESIS INPUT FILES, COMBINES THE INFORMATION TO
C              PRODUCE U,V,W,CT FIELDS AND WRITES RESULTS TO IOUT.
C
      INCLUDE 'CEDRIC.INC'      
      PARAMETER (MXRAD=14,MXFS=10,MXADV=MXRAD+1)
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X                IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X                NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF
      COMMON /SYNBLK/ LSYN(MXRAD),NAMRAD(MXRAD),INVOLD(MXRAD),
     X                NFLDIN(MXFS,MXRAD,2),NRQF,IRADTB(MXRAD),NRADS
      CHARACTER*8 NAMRAD,INVOLD,NFLDIN
      COMMON /ARBRNE/ IABFLG(MXRAD),NFAZEL(2,MXRAD),SCAZEL(2,MXRAD)
      CHARACTER*8 NFAZEL
      COMMON /ADVECT/ ITVOLM(MXADV),NFLTIM(MXADV),TADV(MXADV),
     X                ISHAD(MXADV),JSHAD(MXADV),IADTYP(MXADV)
      CHARACTER*8 NFLTIM
      CHARACTER*2 ITEST,ITEQ
      COMMON /CDFNET/ ICDFID(MXCDF),IDIMDAT(4,MXCDF),IDIMID(1,MXCDF),
c    X     IVAR(NFMAX+1,MXCDF),IFILE,ISYNFLG,GFIELD(NFMAX),IUSWRP
     X     IVAR(NFMAX+1,MXCDF),IFILE,ISYNFLG,ICDUNT(MXCDF),IUSWRP

CTHE ORIGINAL RBUF IS DIMENSIONED IN SYNDRV AS IBUF(MAXBSZ).
CMAXBSZ IS DEFINED IN CEDRIC.f TO BE (MXCRT-2) * MAXPLN
CMAXPLN AND MXCRT ARE DEFINED IN CEDRIC.INC
      DIMENSION RBUF(NX,NY,MXCRT-2)


      DIMENSION IBUF(NPS,2),TBUF(NPLIN,1),RC(3,MXRAD),
     X          LASTLV(MXRAD),WEIGHT(MXRAD),IWALL(2,3),
     X          IDSAV(NID,MXRAD),NORD(3),ILHD(10),I2BAD(2),
     X          DTEST(3),LASTFD(MXRAD),MAP(MAXAXIS,3,MXRAD)
      LOGICAL KEEPW,KEEPPE

C      DATA I2BAD /-32768,-32768/
      DATA I4BAD/ O'20000100000' /
      CHARACTER*2 NAM2(4)
      CHARACTER*8 CTEMP,NAMFLD
      DATA NORD/1,2,3/
      DATA SCLADV,ATR/100.,0.0174533/


C
C        X,Y,Z POSITIONS OF THE RADARS
C
      SFI=1./FLOAT(ID(68))
      ANGXAX=FLOAT(ID(40))/FLOAT(ID(69))
      XORG=ID(41)*SFI
      YORG=ID(42)*SFI

C     Extract WEIGHT from NFLDIN(2,N,2) which was set to
C     IENDC1 ('   1.000') in SYNDRV ==> WEIGHT(N) = 1.0
C
      DO 5 N=1,NRADS
         RC(1,N)=IDSAV(315,N)*SFI-XORG
         RC(2,N)=IDSAV(316,N)*SFI-YORG
         RC(3,N)=IDSAV(317,N)*0.001
         WRITE (CTEMP,17)NFLDIN(2,N,2)
         READ (CTEMP,201)WEIGHT(N)
  201    FORMAT(F8.3)
    5 CONTINUE
C
C        LOOP FOR EACH Z-LEVEL
C
      DO 50 KOT=1,NZ
         ZLEV=CSP(1,3)+(KOT-1)*CSP(3,3)
         DO 40 N=1,NRADS
            LOOP=IRADTB(N)
            TADV(N)=0.0
            ISHAD(N)=0
            JSHAD(N)=0
            KIN=MAP(KOT,3,N)
            CSPXIN=IDSAV(163,N)*0.001
            CSPYIN=IDSAV(168,N)*0.001
            IF(KIN.LE.0) THEN
               CALL CONFLD(RBUF(1,1,N),NPLANE,BAD)
               CALL CONFLD(RBUF(1,1,N+NRADS),NPLANE,BAD)
               IF(LOOP.LE.0) GO TO 40
               DO 6 L=1,LOOP
                  WRITE (CTEMP,17)NFLDIN(NRQF+L,N,2)
                  READ (CTEMP,202)NAM2
                  JFLD=LOCFLDID(NAM2,ID(176),5,NFL,4)
C+++++ TEMPORARY
c                  IF(JFLD.EQ.0) STOP 1303
                  IF(JFLD.EQ.0) CALL FLUSH_STDOUT
                  CALL PLACED(IOUT,ID,KOT,JFLD,IBUF,RBUF(1,1,N),
     X                        NX,NY,3,BAD,NST)
    6          CONTINUE
               GO TO 40
            END IF
C
C           FETCH DATA FROM EACH RADAR
C
            KSEAR=IDSAV(175,N)
            NTX=IDSAV(162,N)
            NTY=IDSAV(167,N)
            NPXY=IDSAV(301,N)
C
C           ZERO OUT AZ/EL BUFFER IF AIRBORNE
C
            IF(IABFLG(N).GT.0) CALL CONFLD(TBUF(1,MXKP+2),NPXY,0.0)
C
C           LOAD IN ALL FIELDS FOR THIS RADAR
C
           I1=171
           DO 10 L=1,KSEAR
            I1=I1+5
            I2=I1+3
            WRITE (CTEMP,202)(IDSAV(I,N),I=I1,I2)
            READ (CTEMP,500) NAMFLD
 500        FORMAT(A8)
  202       FORMAT(4A2)

            ITRF=IFINDC(NAMFLD,NFLDIN(NRQF+1,N,1),LOOP,0)
C
C           CHECK IF THIS IS A TRANSFERRED FIELD
C
            IF(ITRF.GT.0) THEN
 223           FORMAT(A2)
               WRITE(NAM2(1),223)IDSAV(I1,N)
               WRITE(NAM2(2),223)IDSAV(I1+1,N)
               WRITE(NAM2(3),223)IDSAV(I1+2,N)
               WRITE(NAM2(4),223)IDSAV(I1+3,N)

               CALL FETCHZ(LSYN(N),TBUF(1,ITRF),IBUF,NPXY,LASTLV(N),
     X                     LASTFD(N),KIN,ZLVF,NAM2,BAD,
     X                     IDSAV(1,N),ILHD)
            END IF
C
C           CHECK IF THIS IS A VELOCITY FIELD
C
            IF(NAMFLD.EQ.NFLDIN(1,N,1)) THEN
               IF(ITRF.EQ.0) THEN
               WRITE(NAM2(1),223)IDSAV(I1,N)
               WRITE(NAM2(2),223)IDSAV(I1+1,N)
               WRITE(NAM2(3),223)IDSAV(I1+2,N)
               WRITE(NAM2(4),223)IDSAV(I1+3,N)
               CALL FETCHZ(LSYN(N),TBUF(1,MXKP+1),IBUF,NPXY,LASTLV(N),
     X                     LASTFD(N),KIN,ZLVF,NAM2,BAD,
     X                     IDSAV(1,N),ILHD)
               ELSE
                  CALL COPRX(TBUF(1,MXKP+1),TBUF(1,ITRF),NPXY)
               END IF
C
C           CHECK IF THIS IS A TIME FIELD
C
            ELSE IF(IADTYP(N).GT.0.AND.NAMFLD.EQ.NFLTIM(N)) THEN
               IF(ITRF.EQ.0) THEN
               WRITE(NAM2(1),223)IDSAV(I1,N)
               WRITE(NAM2(2),223)IDSAV(I1+1,N)
               WRITE(NAM2(3),223)IDSAV(I1+2,N)
               WRITE(NAM2(4),223)IDSAV(I1+3,N)
               CALL FETCHZ(LSYN(N),TBUF(1,MXKP+2),IBUF,NPXY,
     X              LASTLV(N),LASTFD(N),KIN,ZLVF,NAM2,BAD,
     X              IDSAV(1,N),ILHD)
               ELSE
                  CALL COPRX(TBUF(1,MXKP+2),TBUF(1,ITRF),NPXY)
               END IF
C
C           CHECK IF THIS IS AIRBORNE DOPPLER
C
            ELSE IF(IABFLG(N).GT.0) THEN
               IFLD=IFINDC(NAMFLD,NFAZEL(1,N),2,0)
               IF(IFLD.GT.0) THEN
                  IF(ITRF.EQ.0) THEN
                     WRITE(NAM2(1),223)IDSAV(I1,N)
                     WRITE(NAM2(2),223)IDSAV(I1+1,N)
                     WRITE(NAM2(3),223)IDSAV(I1+2,N)
                     WRITE(NAM2(4),223)IDSAV(I1+3,N)
                  CALL FETCHZ(LSYN(N),IBUF(1,2),IBUF,NPXY,LASTLV(N),
     X                     LASTFD(N),KIN,ZLVF,NAM2,BAD,
     X                     IDSAV(1,N),ILHD)
                  ELSE
                     CALL COPRXI(IBUF(1,2),TBUF(1,ITRF),NPXY)
                  END IF
                  SFAE=1./SCAZEL(IFLD,N)
                  CALL IABPCK(TBUF(1,MXKP+2),IBUF(1,2),NPXY,
     X                        IFLD,SFAE,BAD)
               END IF
C
            END IF
C
   10    CONTINUE
         IF(IADTYP(N).EQ.1) THEN
C
C           UNIFORM ADVECTION PARAMETERIZATION FOR THIS RADAR
C
            WRITE (CTEMP,17)NFLTIM(N)
            READ (CTEMP,203)ITEST
  203       FORMAT(A2)
            BAST=ICONSC(ITVOLM(N))-ICONSC(ITANAL)
            AR=(WDADV+(90.0-ANGXAX))*ATR
            UAD=WSADV*SIN(AR)/FLOAT(IDSAV(163,N))
            VAD=WSADV*COS(AR)/FLOAT(IDSAV(168,N))
            IF(ITEST.EQ.ITEQ) THEN
               WRITE (CTEMP,17)NFLTIM(N)
               READ (CTEMP,204)TADV(N)
  204          FORMAT(2X,F6.0)
            ELSE
               IWALL(1,1)=1
               IWALL(2,1)=IDSAV(162,N)
               IWALL(1,2)=1
               IWALL(2,2)=IDSAV(167,N)
               IWALL(1,3)=1
               IWALL(2,3)=IDSAV(172,N)
               CALL STRID(TBUF(1,MXKP+2),NTX,NTY,IWALL,NORD,BAD,
     X                    TADV(N),FSTD,NP,M1,M2,M3,M4,FMN,FMX,0)
            END IF
            TREL=BAST+TADV(N)
            ISHAD(N)=NINT(TREL*UAD)
            JSHAD(N)=NINT(TREL*VAD)
         ELSE IF(IADTYP(N).EQ.2) THEN
C
C           DIFFERENTIAL ADVECTION PARAMETERIZATION FOR THIS RADAR
C
            WRITE (CTEMP,17)NFLTIM(N)
            READ (CTEMP,203)ITEST
            IF(ITEST.EQ.ITEQ) THEN
               WRITE (CTEMP,17)NFLTIM(N)
               READ (CTEMP,204)TCON
               CALL CONFLD(TBUF(1,MXKP+2),NPXY,TCON)
            END IF
         END IF
C
C        WRITE ALL TRANSFERRED FIELDS TO OUTPUT UNIT
C
         IF(LOOP.GT.0) THEN
            DO 15 L=1,LOOP
               IF(IADTYP(N).EQ.1) THEN
C
C              UNIFORM ADVECTION PRIOR TO OUTPUT
C
               CALL DOSHFT(TBUF(1,L),IBUF,NTX,NTY,BAD,
     X                     ISHAD(N),JSHAD(N))
C
               ELSE IF(IADTYP(N).EQ.2) THEN
C
C              DIFFERENTIAL ADVECTION PRIOR TO OUTPUT
C
               CALL DIFADV(IBUF,TBUF(1,L),TBUF(1,MXKP+2),NTX,NTY,
     X                     ITANAL,ITVOLM(N),WDADV,WSADV,CSPXIN,
     X                     CSPYIN,ANGXAX,BAD,0,IBUF,SCLADV,I4BAD,NST)
C
               ELSE
C
C              NO ADVECTION PRIOR TO OUTPUT
C
               CALL COPRXI(IBUF,TBUF(1,L),NPXY)
C
               END IF
C
C              MAP TO OUTPUT GRID AND WRITE OUT TO LCM
C
               CALL RESHUF(IBUF(1,2),NX,NY,IBUF,NTX,NTY,MAP(1,1,N),
     X                     MAXAXIS,BAD)
               WRITE (CTEMP,17)NFLDIN(NRQF+L,N,2)
 17            FORMAT(A8)
               READ (CTEMP,202)NAM2
               JFLD=LOCFLDID(NAM2,ID(176),5,NFL,4)
C+++++  TEMPORARY
c      IF(JFLD.EQ.0) STOP 1303
      IF(JFLD.EQ.0) CALL FLUSH_STDOUT
               CALL PLACED(IOUT,ID,KOT,JFLD,IBUF,IBUF(1,2),
     X                     NX,NY,3,BAD,NST)
   15       CONTINUE
         END IF
C
C        PREPARE INFORMATION FROM THIS RADAR FOR SYNTHESIS
C
         IF(IADTYP(N).EQ.1) THEN
C
C        UNIFORM ADVECTION
C
         CALL DOSHFT(TBUF(1,MXKP+1),IBUF,NTX,NTY,BAD,ISHAD(N),JSHAD(N))
C
         ELSE IF(IADTYP(N).EQ.2) THEN
C
C        DIFFERENTIAL ADVECTION
C
         CALL DIFADV(IBUF,TBUF(1,MXKP+1),TBUF(1,MXKP+2),NTX,NTY,ITANAL,
     X               ITVOLM(N),WDADV,WSADV,CSPXIN,CSPYIN,ANGXAX,
     X               BAD,1,IBUF(1,2),SCLADV,I4BAD,NST)
         CALL RESHUFR(RBUF(1,1,NRADS+N),NX,NY,IBUF(1,2),NTX,NTY,
     X               MAP(1,1,N),MAXAXIS,BAD)
C
         ELSE IF(IABFLG(N).GT.0) THEN
C
C        AIRBORNE DOPPLER
C
         CALL COPRXI(IBUF,TBUF(1,MXKP+1),NPXY)
         CALL RESHUFR(RBUF(1,1,NRADS+N),NX,NY,TBUF(1,MXKP+2),NTX,NTY,
     X               MAP(1,1,N),MAXAXIS,BAD)
C
         ELSE
C
C        NO ADVECTION
C
         CALL COPRXI(IBUF,TBUF(1,MXKP+1),NPXY)
C
         END IF
C
         CALL RESHUFR(RBUF(1,1,N),NX,NY,IBUF,NTX,NTY,MAP(1,1,N),
     X               MAXAXIS,BAD)
C
   40    CONTINUE
C
C        DATA FROM ALL RADARS LOADED FOR THIS LEVEL- PROCEED WITH SYNTHESIS
C
         CALL CALUVW(RBUF,NX,NY,NRADS,NRQF,NPLANE,RC,CSP,ANGXAX,ZLEV,
     X               DTEST,ITEQN,WEIGHT,SCLADV,KEEPW,KEEPPE,BAD,IACTC,
     X               KOT)
         IF (IACTC.EQ.0) THEN
C
C     FOR CARTESIAN SYNTHESIS, WRITE OUT ALL FIELDS
C
            DO 45 IFLD=1,NMF
               CALL PLACED(IOUT,ID,KOT,IFLD,IBUF,RBUF(1,1,IFLD),
     X              NX,NY,3,BAD,NST)
 45         CONTINUE
         ELSE
C
C     FOR COPLANE SYNTHESIS, DON'T WRITE OUT CT,EWU,EWV
C
            CALL PLACED(IOUT,ID,KOT,1,IBUF,RBUF(1,1,1),
     X           NX,NY,3,BAD,NST)
            CALL PLACED(IOUT,ID,KOT,2,IBUF,RBUF(1,1,2),
     X           NX,NY,3,BAD,NST)
            CALL PLACED(IOUT,ID,KOT,4,IBUF,RBUF(1,1,4),
     X           NX,NY,3,BAD,NST)
            CALL PLACED(IOUT,ID,KOT,5,IBUF,RBUF(1,1,5),
     X           NX,NY,3,BAD,NST)
         END IF

            
C
   50 CONTINUE
      NST=0
      RETURN
      END
