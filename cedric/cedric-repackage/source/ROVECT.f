      SUBROUTINE ROVECT(ITRANS,IROT,NX,NY,NZ,CSPN,RBUF1,RBUF2,
     X     ANGXAX,ANGUSR,IBUF,ICORD,NAMIN,NAMOUT,ISTAT)
C     
C     THIS SUBROUTINE HANDLES THE TRANSFORMATION (ROTATION) OF VECTOR COMPONENTS 
C     FROM ONE COORDINATE SYSTEM TO ANOTHER.
C     
      INCLUDE 'CEDRIC.INC'
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X     IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X     NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF,NAME(4)
      DIMENSION CSPN(3,3),RBUF1(NX,NY),IBUF(NX,NY)
      DIMENSION RBUF2(NX,NY)
      CHARACTER*8 CTEMP1
      CHARACTER*8 NAMIN(3), NAMOUT(3)
      
      ISTAT=0
      NX=NCX(1)
      NY=NCX(2)
      NZ=NCX(3)
      ATR=ATAN(1.)/45.
C     
C     FIND FIELD NUMBERS OF VECTOR COMPONENTS
C     
      IUIFLD=0
      IVIFLD=0
      IWIFLD=0
      IUOFLD=0
      IVOFLD=0
      IWOFLD=0
      DO 10 I=1,4
         NAME(I)=' '
 10   CONTINUE
      DO 100 I=1,NFMAX
         WRITE(CTEMP1,50)(NAMF(J,I),J=1,4)
 50      FORMAT(4A2)
         IF (CTEMP1.EQ.NAMIN(1)) THEN 
            IUIFLD=I
            ISCLU=SCLFLD(I)
         END IF
         IF (CTEMP1.EQ.NAMIN(2)) THEN 
            IVIFLD=I
            ISCLV=SCLFLD(I)
         END IF
         IF (CTEMP1.EQ.NAMIN(3) .AND. NAMIN(3).NE.' ') THEN 
            IWIFLD=I
            ISCLW=SCLFLD(I)
         END IF
         IF (CTEMP1.EQ.NAMOUT(1)) IUOFLD=I
         IF (CTEMP1.EQ.NAMOUT(2)) IVOFLD=I
         IF (CTEMP1.EQ.NAMOUT(3)) IWOFLD=I
 100  CONTINUE

C
C     CHECK FOR MISSING INPUT VECTOR COMPONENTS
C
      IF (IUIFLD.EQ.0) THEN
         WRITE(*,60)NAMIN(1)
 60      FORMAT(/,5X,'+++COULD NOT FIND INPUT VECTOR COMPONENT:',A4,
     X               ' FOR ROTATION+++')
         ISTAT=1
         RETURN
      ELSE IF (IVIFLD.EQ.0) THEN
         WRITE(*,60)NAMIN(2)
         ISTAT=1
         RETURN
      ELSE IF (IWIFLD.EQ.0 .AND. ICORD.EQ.1) THEN
         WRITE(*,60)NAMIN(3)
         ISTAT=1
         RETURN
      ENDIF
C
C     CHECK FOR MISSING OUTPUT VECTOR COMPONENTS AND CREATE NEW FIELDS IF NEC.
C
      IF (IUOFLD.EQ.0) THEN
         CTEMP1=NAMOUT(1)
         DO 70 I=1,4
            K=(I*2.)-1
            READ(CTEMP1(K:K+1),72)NAME(I)
 72         FORMAT(A2)
 70      CONTINUE
         J=IADFLD(NAME,ISCLU,6)
         IF (J.LT.0) THEN
            ISTAT=1
            RETURN
         END IF
         IUOFLD=J
      END IF
      IF (IVOFLD.EQ.0) THEN
         CTEMP1=NAMOUT(2)
         DO 75 I=1,4
            K=(I*2)-1
            READ(CTEMP1(K:K+1),72)NAME(I)
 75      CONTINUE
         J=IADFLD(NAME,ISCLV,6)
         IF (J.LT.0) THEN
            ISTAT=1
            RETURN
         END IF
         IVOFLD=J
      END IF
      IF (IWOFLD.EQ.0 .AND. IWIFLD.NE.0) THEN
         CTEMP1=NAMOUT(3)
         DO 80 I=1,4
            K=(I*2)-1
            READ(CTEMP1(K:K+1),72)NAME(I)
 80      CONTINUE
         J=IADFLD(NAME,ISCLW,6)
         IF (J.LT.0) THEN
            ISTAT=1
            RETURN
         END IF
         IWOFLD=J
      END IF

      IUIFLD=MAPVID(IUIFLD,1)
      IUOFLD=MAPVID(IUOFLD,1)

      IVIFLD=MAPVID(IVIFLD,1)
      IVOFLD=MAPVID(IVOFLD,1)

      IF (IWIFLD.NE.0)  IWIFLD=MAPVID(IWIFLD,1)
      IF (IWOFLD.NE.0)  IWOFLD=MAPVID(IWOFLD,1)
      
C     
C     FIRST, HANDLE THE ROTATION FROM COPLANE TO CARTESIAN WITH
C     Y AXIS ALONG BASELINE.
C     
      IF (ICORD.EQ.1) THEN
         DO 200 I=1,NZ
            CALL FETCHD(0,ID,I,IUIFLD,IBUF,RBUF1,N1,N2,3,BAD,ZLEV,
     X                  NST)
            CALL FETCHD(0,ID,I,IWIFLD,IBUF,RBUF2,N1,N2,3,BAD,ZLEV,
     X                  NST)
            IF (NST.NE.0) THEN
               WRITE(*,*)'***INVALID STATE 1 IN ROVECT ***'
               CALL FLUSH_STDOUT
            END IF
            
            DO 250 J=1,NY
               DO 300 K=1,NX
                  IF(RBUF1(K,J).EQ.BAD .OR. RBUF2(K,J).EQ.BAD) THEN
                     RBUF1(K,J)=BAD
                     RBUF2(K,J)=BAD
                     GOTO 300
                  END IF
                  THETA=(CSP(1,3) + (I-1)*CSP(3,3))*ATR
                  UP=RBUF1(K,J)*COS(THETA) - RBUF2(K,J)*SIN(THETA)
                  WP=RBUF1(K,J)*SIN(THETA) + RBUF2(K,J)*COS(THETA)
                  RBUF1(K,J)=UP
                  RBUF2(K,J)=WP
 300           CONTINUE
 250        CONTINUE
            
            CALL PLACED(0,ID,I,IUOFLD,IBUF,RBUF1,N1,N2,3,BAD,NST)         
            CALL PLACED(0,ID,I,IWOFLD,IBUF,RBUF2,N1,N2,3,BAD,NST)
            IF (NST.NE.0) THEN
               WRITE(*,*)'***INVALID STATE 2 IN ROVECT ***'
               CALL FLUSH_STDOUT
            END IF
 200     CONTINUE
      END IF
      
 320  CONTINUE
C     
C     NOW DO THE TRANSFORMATION FROM CARTESIAN TO CARTESIAN, IF NEEDED
C     
      IF (ICORD.EQ.0 .AND. IWIFLD.NE.IWOFLD .AND. IWIFLD.NE.0) THEN
C
C     NEED TO COPY OLD W FIELD TO NEW ONE EVEN THOUGH IT IS NOT
C     AFFECTED BY ROTATIONS.
C
         DO 600 I=1,NZ
            CALL FETCHD(0,ID,I,IWIFLD,IBUF,RBUF1,N1,N2,3,BAD,ZLEV,
     X                  NST)
            CALL PLACED(0,ID,I,IWOFLD,IBUF,RBUF1,N1,N2,3,BAD,NST)         
 600     CONTINUE
      END IF
            
            
      IF (IROT.EQ.1) THEN
         DO 400 I=1,NZ
            CALL FETCHD(0,ID,I,IUIFLD,IBUF,RBUF1,N1,N2,3,BAD,ZLEV,
     X                  NST)
            CALL FETCHD(0,ID,I,IVIFLD,IBUF,RBUF2,N1,N2,3,BAD,ZLEV,
     X                  NST)
            IF (NST.NE.0) THEN
               WRITE(*,*)'***INVALID STATE 3 IN ROVECT ***'
               CALL FLUSH_STDOUT
            END IF
            DO 450 J=1,NY
               DO 500 K=1,NX
                  IF(RBUF1(K,J).EQ.BAD .OR. RBUF2(K,J).EQ.BAD) THEN
                     RBUF1(K,J)=BAD
                     RBUF2(K,J)=BAD
                     GOTO 500
                  END IF
                  THETA=(ANGXAX-ANGUSR)*ATR
                  UP=RBUF1(K,J)*COS(THETA) - RBUF2(K,J)*SIN(THETA)
                  VP=RBUF1(K,J)*SIN(THETA) + RBUF2(K,J)*COS(THETA)
                  RBUF1(K,J)=UP
                  RBUF2(K,J)=VP
 500           CONTINUE
 450        CONTINUE
            CALL PLACED(0,ID,I,IUOFLD,IBUF,RBUF1,N1,N2,3,BAD,NST)         
            CALL PLACED(0,ID,I,IVOFLD,IBUF,RBUF2,N1,N2,3,BAD,NST)
            IF (NST.NE.0) THEN
               WRITE(*,*)'***INVALID STATE 4 IN ROVECT ***'
               CALL FLUSH_STDOUT
            END IF
 400     CONTINUE
      END IF

 510  CONTINUE
      RETURN
      END
      
      
