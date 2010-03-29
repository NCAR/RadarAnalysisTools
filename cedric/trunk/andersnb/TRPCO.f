      SUBROUTINE TRPCO(WXYC,IXYC,NNX,NNY,NNZ,CSPN,NLEV,RELMAX,ICORD,
     X     LCMB2,ISPEC,VNYQ,RTOP,RBOT,ROUT,IBUF,NX,NY,ITEMP,IDIM2,
     X     MEMUSE)
C     
C     THIS SUBROUTINE DOES THE ACTUAL INTERPOLATION 
C     
      INCLUDE 'CEDRIC.INC'
      PARAMETER (MAXP=256)
      PARAMETER (NFMAX=25,NID=510)
      COMMON LCMB(1)
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X     IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X     NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF
      DIMENSION WXYC(MAXP,MAXP,3),IXYC(MAXP,MAXP,3),CSPN(3,3)
      DIMENSION IPHOLD(NFMAX),IVAL(8),VAL(8),LCMB2(1),ISPEC(NFMAX)
      DIMENSION RBOT(NX,NY),RTOP(NX,NY),ROUT(NNX,NNY)
      DIMENSION ITEMP(1)
      DIMENSION IBUF(1)
      DATA IBIT,NBITS,NSKIP/0,16,0/
      INTEGER CVMGP
      
      IBAD=ID(67)
      NZ=NCX(3)
      ATR=ATAN(1.)/45.
      NF=ID(175)
      VNYQ2=VNYQ*2.0
      IVOL=1
      N1=NCX(1)
      N2=NCX(2)
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
      IBOT=0
      ITOP=0
      
C     
C     START LOOP OVER ALL FIELDS
C     
      DO 100 I=1,NF
C     
C     ONLY INTERPOLATE FIELDS WITH NON-ZERO SCALE FACTORS (ACTIVE FIELDS)
C     
         IBOT=0
         ITOP=0
         SCALE=1./ID(175+I*5)
         DO 130 K=1,NNX
            DO 120 J=1,NNY
               FVAL=BAD
               L=IXYC(K,J,1)
               M=IXYC(K,J,2)
               N=IXYC(K,J,3)
               IF ((L.LT.1 .OR. L.GT.NX) .OR.
     X              (M.LT.1 .OR. M.GT.NY) .OR.
     X              (N.LT.1 .OR. N.GT.NZ)) GOTO 200
               
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
                  IF (MEMUSE.EQ.0) THEN
                     IF (IBOT.NE.N) THEN
                        CALL FETCHD(IBOT,ID,NID,N,I,IBUF,RBOT,N1,N2,3,
     X                       BAD,ZLEV,NST)
                        IF (NST.NE.0) THEN
                           WRITE(*,*)'+++ERROR FETCHING IN TRPCO 1+++'
                           STOP
                        END IF
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
                  IF (MEMUSE.EQ.0) THEN
C     
C     GET THE TWO Z (OR COPLANE) LEVELS WE NEED
C     
                     IF ((IBOT.NE.N .AND. ITOP.NE.N+1) .AND. 
     X                    (ITOP.NE.N)) THEN
                        CALL FETCHD(IBOT,ID,NID,N,I,IBUF,RBOT,N1,N2,3,
     X                    BAD,ZLEV,NST)
                        IF (NST.NE.0) THEN
                           WRITE(*,*)'+++ERROR FETCHING IN TRPCO 1+++'
                           STOP
                        END IF
                        CALL FETCHD(ITOP,ID,NID,N+1,I,IBUF,RTOP,N1,N2,
     X                    3,BAD,ZLEV,NST)
                     ELSE IF ((IBOT.NE.N .AND. ITOP.NE.N+1) .AND.
     X                       (ITOP.EQ.N)) THEN
                        DO 145 LY=1,NY
                           DO 135 LX=1,NX
                              RBOT(LX,LY)=RTOP(LX,LY)
 135                       CONTINUE
 145                    CONTINUE
                        CALL FETCHD(ITOP,ID,NID,N+1,I,IBUF,RTOP,N1,N2,3,
     X                       BAD,ZLEV,NST)
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
                     IF (ICORD.EQ.1) THEN
                        DISTA=CSP(3,3)*ATR*(CSP(1,1) + CSP(3,1)*(K-1))
                     ELSE IF (ICORD.EQ.0) THEN
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
 120        CONTINUE
 130     CONTINUE
         IF (MEMUSE.EQ.0 .OR. MEMUSE.EQ.1) THEN
C     
C     STORE OUTPUT ARRAY NOW
C     
         LOCD = 1 + NWPF*NNZ*(I - 1)
         MULT=ID(175+I*5)
         KBAD=0
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
            DO 500 IY=1,NNY
               DO 520 IX=1,NNX
                  ITEMP(LJ)=IBUF(KL)
                  KL=KL+1
                  LJ=LJ+1
 520           CONTINUE
               LJ=LJ+NXX-NNX
 500        CONTINUE
            DO 550 IJ=1,NNPLANE
               IBUF(IJ)=ITEMP(IJ)
 550        CONTINUE
         END IF
         
         CALL PUTD(IBUF,NNPLANE,LOCD,IBIT,NBITS,NSKIP,IRP,3,ID,NID,
     X        NXX,NNY,NNZ,NLEV,ITEMP,NWPF,LCMB2,MEMUSE,IVOL)
         END IF
 100  CONTINUE
      
C     
C     PRINT OUT NUMBER OF VALUES THAT COULDN'T BE SCALED
C     
C     DO 210 I=1,NFMAX
C     IF (KBAD(I).NE.0) THEN
C     J1=171+I*5
C     J2=J1+3
C     PRINT 101, KBAD(I),(ID(J),J=J1,J2),NLEV
C     101        FORMAT(' +++  ',I5,' VALUES OUT OF RANGE IN FIELD: ',
C     X           4A2,' AT PLANE=',I3,'  +++')
C     END IF
C     210  CONTINUE
      
      RETURN
      
      
      END
