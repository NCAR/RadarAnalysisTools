      SUBROUTINE PCONVG(U,V,RBUF,NX,NY,NDER,XYDELI,BAD,CSP,IACTC)
C
C        CALCULATES THE CONVERGENCE USING U,V COMPONENTS
C        NDER CONSECUTIVE VALUES MUST BE PRESENT TO COMPUTE
C             THE PARTIAL ALONG A LINEAR AXIS. PARTIAL WILL
C             BE PRESENT AT NDER LOCATIONS SINCE EDGE WEIGHTING
C             IS PERFORMED.
C
C        MODIFIED TO PERFORM EDGE WEIGHTING-- C. MOHR 8/87
C
      DIMENSION U(NX,NY),V(NX,NY),RBUF(NX,NY),KPOS(5),WX(5,5,5),
     X          XYDELI(2),CSP(3,3)
      DATA KPOS/0,1,2,0,3/
      DATA WX/25*0.,-1.,1.,0.,0.,0.,-1.,1.,0.,0.,0.,15*0.,
     X        -1.5,2.,-.5,0.,0.,-.5,0.,0.5,0.,0.,0.5,-2.,1.5,0.,0.,
     X        35*0.,
     X        -2.08333333,4.,-3.,1.333333,-0.25,
     X        -0.25,-0.8333333,1.5,-0.5,0.08333333,
     X        0.08333333,-0.6666667,0.,0.6666667,-0.08333333,
     X        -0.08333333,0.5,-1.5,0.8333333,0.25,
     X        0.25,-1.333333,3.,-4.,2.08333333/
      DATA NDMAX/5/

C
C        PARTIAL ACROSS THE X-DIMENSION
C
      DO 20 JJ=1,NY
         J=JJ
         KLO=0
      DO 20 II=1,NX
         I=II
         IF(U(I,J).NE.BAD) THEN
            IF(KLO.EQ.0)KLO=I
            IF(I.NE.NX)GO TO 20
            I=NX+1
         END IF
         IF(KLO.EQ.0)GO TO 16
         KHI=I-1
         K=I-KLO
         IF(K.LE.2) THEN
C                                        MUST BE AT LEAST 3 CONSEC. PTS.
            RBUF(KLO,J)=BAD
            RBUF(KHI,J)=BAD
         ELSE
C                                        GOT AT LEAST 3, 5 FALLS BACK TO
C                                        3 IF NDER=5.
            NPTS=MIN0(K,NDER)
            IF(NPTS.EQ.4)NPTS=3
            IDEL=NPTS-KPOS(NPTS)
            DO 10 M=KLO,KHI
               KL=M-KLO
               KR=KHI-M
               IF(KL.LT.IDEL) THEN
C                                        LEFT  OF CENTER WEIGHTING
                  MPOS=KL+1
               ELSE IF(KR.LT.IDEL) THEN
C                                        RIGHT OF CENTER WEIGHTING
                  MPOS=NPTS-KR
               ELSE
C                                              CENTER WEIGHTING
                  MPOS=KPOS(NPTS)
               END IF
               IGO=M-MPOS
               SUM=0.0
               DO 9 IP=1,NPTS
                  IDX=IP+IGO
                  SUM=SUM+U(IDX,J)*WX(IP,MPOS,NPTS)
                  IF (IP.EQ.MPOS .AND. IACTC.EQ.1) THEN
C
C     CORRECTION FOR COPLANE SPACE
C
                     XX=CSP(1,1) + (IDX-1)*CSP(3,1)
                     IF (XX.NE.0.0) THEN
                        CORR=U(IDX,J)/XX
                     ELSE
                        RBUF(M,J)=BAD
                        GOTO 23
                     END IF
                  END IF
    9          CONTINUE
               SUM=SUM*XYDELI(1)
               IF (IACTC.EQ.1) THEN
                  RBUF(M,J)=SUM + CORR
               ELSE
                  RBUF(M,J)=SUM
               END IF
 23            CONTINUE
   10       CONTINUE
         END IF
         IF(I.GT.NX)GO TO 20
         KLO=0
C
   16    CONTINUE
            RBUF(I,J)=BAD
C
   20 CONTINUE
C
C        PARTIAL ACROSS THE Y-DIMENSION
C
      DO 40 II=1,NX
         I=II
         KLO=0
      DO 40 JJ=1,NY
         J=JJ
         IF(V(I,J).NE.BAD) THEN
            IF(KLO.EQ.0)KLO=J
            IF(J.NE.NY) GO TO 40
            J=NY+1
         END IF
         IF(KLO.EQ.0)GO TO 36
         KHI=J-1
         K=J-KLO
         IF(K.LE.2) THEN
C                                        MUST BE AT LEAST 3 CONSEC. PTS.
            RBUF(I,KLO)=BAD
            RBUF(I,KHI)=BAD
         ELSE
C                                        GOT AT LEAST 3, 5 FALLS BACK TO
C                                        3 IF NDER=5.
            NPTS=MIN0(K,NDER)
            IF(NPTS.EQ.4)NPTS=3
            IDEL=NPTS-KPOS(NPTS)
            DO 30 M=KLO,KHI
               KL=M-KLO
               KR=KHI-M
               IF(KL.LT.IDEL) THEN
C                                        LEFT  OF CENTER WEIGHTING
                  MPOS=KL+1
               ELSE IF(KR.LT.IDEL) THEN
C                                        RIGHT OF CENTER WEIGHTING
                  MPOS=NPTS-KR
               ELSE
C                                              CENTER WEIGHTING
                  MPOS=KPOS(NPTS)
               END IF
               IGO=M-MPOS
               SUM=0.0
               DO 29 IP=1,NPTS
                  IDX=IP+IGO
                  SUM=SUM+V(I,IDX)*WX(IP,MPOS,NPTS)
   29          CONTINUE
               IF(RBUF(I,M).NE.BAD)
     X                      RBUF(I,M)= - (RBUF(I,M)+(SUM*XYDELI(2)))
   30       CONTINUE
         END IF
         IF(J.GT.NY)GO TO 40
         KLO=0
C
   36    CONTINUE
            RBUF(I,J)=BAD
C
   40 CONTINUE
C
      RETURN
      END
