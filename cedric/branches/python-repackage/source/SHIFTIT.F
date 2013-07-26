      SUBROUTINE SHIFTIT(XORG,YORG,ZORG,RBUF,IBUF,OBUF,IXSHFT,IYSHFT,
     X                   IZSHFT,NX,NY)
C
C     THIS SUBROUTINE DOES THE SHIFTING OF DATA.
C     NO INTERPOLATION IS DONE.
C

      INCLUDE 'CEDRIC.INC'
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X     IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X     NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF
      DIMENSION IBUF(MAXPLN),RBUF(NX,NY),OBUF(NX,NY)

      NX=NCX(1)
      NY=NCX(2)
      NZ=NCX(3)

      IF (IZSHFT.GE.0) THEN
         DO 50 M=1,NFL
            DO 100 I=NZ,1,-1
               IZ=I+IZSHFT
               IF (IZ.GT.NZ) GOTO 100
               CALL FETCHD(0,ID,I,M,IBUF,RBUF,N1,N2,3,BAD,ZLEV,NST)
               IF (NST.NE.0) THEN
                  WRITE(*,*)'***INVALID STATE 1 IN SHIFTIT***'
                  CALL FLUSH_STDOUT
               END IF
               CALL CONFLD(OBUF,NPLANE,BAD)
               DO 150 J=1,NY
                  JJ=J+IYSHFT
                  IF (JJ.LT.1 .OR. JJ.GT.NY) GOTO 150
                  DO 175 K=1,NX
                     KK=K+IXSHFT
                     IF (KK.LT.1 .OR. KK.GT.NX) GOTO 175
                     OBUF(K,J)=RBUF(KK,JJ)
 175              CONTINUE
 150           CONTINUE
               CALL PLACED(0,ID,IZ,M,IBUF,OBUF,N1,N2,3,BAD,NST)
               IF (NST.NE.0) THEN
                  WRITE(*,*)'***INVALID STATE 2 IN SHIFTIT***'
                  CALL FLUSH_STDOUT
               END IF
C
C     FILL OLD Z LEVEL OF DATA JUST MOVED WITH ALL BAD VALUES
C
               IF (I.NE.IZ) THEN
                  CALL CONFLD(OBUF,NPLANE,BAD)
                  CALL PLACED(0,ID,I,M,IBUF,OBUF,N1,N2,3,BAD,NST)
               END IF
 100        CONTINUE
 50      CONTINUE
         
      ELSE IF (IZSHFT.LT.0) THEN
         DO 55 M=1,NFL
            DO 105 I=1,NZ
               IZ=I+IZSHFT
               IF (IZ.LT.1) GOTO 105
               CALL FETCHD(0,ID,I,M,IBUF,RBUF,N1,N2,3,BAD,ZLEV,NST)
               IF (NST.NE.0) THEN
                  WRITE(*,*)'***INVALID STATE 3 IN SHIFTIT***'
                  CALL FLUSH_STDOUT
               END IF
               CALL CONFLD(OBUF,NPLANE,BAD)
               DO 155 J=1,NY
                  JJ=J+IYSHFT
                  IF (JJ.LT.1 .OR. JJ.GT.NY) GOTO 155
                  DO 180 K=1,NX
                     KK=K+IXSHFT
                     IF (KK.LT.1 .OR. KK.GT.NX) GOTO 180
                     OBUF(K,J)=RBUF(KK,JJ)
 180              CONTINUE
 155           CONTINUE
               CALL PLACED(0,ID,IZ,M,IBUF,OBUF,N1,N2,3,BAD,NST)
               IF (NST.NE.0) THEN
                  WRITE(*,*)'***INVALID STATE 4 IN SHIFTIT***'
                  CALL FLUSH_STDOUT
               END IF
C
C     FILL OLD Z LEVEL OF DATA JUST MOVED WITH ALL BAD VALUES
C
               CALL CONFLD(OBUF,NPLANE,BAD)
               CALL PLACED(0,ID,I,M,IBUF,OBUF,N1,N2,3,BAD,NST)
 105        CONTINUE
 55      CONTINUE

      END IF

      RETURN

      END
