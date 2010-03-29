      SUBROUTINE GETD(IBUF,NPLANE,LOCD,IBIT,NBITS,NSKIP,IRP,IFIXAX,
     X     ID,NX,NY,NZ,LEV,ITEMP,NWL,LCMB,MEM,IVOL)
C     
C     THIS SUBROUTINE DETERMINES WHERE THE DATA SHOULD BE READ FROM,
C     WHETHER FROM MEMORY OR FROM DISK.
C     
      INCLUDE 'CEDRIC.INC'
      DIMENSION ID(NID),ITEMP(MAXPLN)
      DIMENSION IBUF(MAXPLN)
      DIMENSION LCMB(1)
      
      MEM=0
      ITER=0
      ISPEC=0
      
      IF (IFIXAX.EQ.3) THEN
C     
C     GETTING A Z LEVEL
C     
         MINNEED = LOCD 
         MAXNEED = MINNEED + NWL - 1
         
C     
C     GET IT FROM DISK
C     
         CALL CGET(ITEMP,LOCD,IBIT,NBITS,NSKIP,NPLANE,ITER,MAXNEED,
     X        MINNEED,MEMUSE,IFIXAX,NX,NY,LEV,IVOL)
         CALL GBYTES(ITEMP,IBUF,0,NBITS,0,NPLANE)
      ELSE IF (IFIXAX.EQ.2) THEN
C     
C     GETTING A Y-LEVEL
C     
         MINNEED = LOCD - NWL
         IFAD = INT(IRP/(WORDSZ/16))
         K = 1
         DO 60 I=1,NZ
            MINNEED = MINNEED + NWL
            MAXNEED = MINNEED + IFAD - 1
            
C     
C     GET IT FROM DISK
C     
            CALL CGET(ITEMP,LOCD,IBIT,NBITS,NSKIP,IRP,ITER,
     X           MAXNEED,MINNEED,MEMUSE,IFIXAX,NX,NY,LEV,IVOL)
            CALL GBYTES(ITEMP,IBUF(K),0,NBITS,0,IRP)
            LOCD = LOCD + NWL
            K = K + IRP
 60      CONTINUE
         
         
      ELSE IF (IFIXAX.EQ.1) THEN
C     
C     GETTING AN X-LEVEL
C     
         MINNEED = LOCD - NWL
         IFAD = INT(IRP/(WORDSZ/16))
         K = 1
         DO 70 I=1,NZ
            MINNEED = MINNEED + NWL
            MAXNEED = MINNEED + IFAD 
            
C     
C     GET IT FROM DISK
C     
            IF (NSKIP.NE.0) THEN
               CALL CGET(ITEMP,LOCD,IBIT,NBITS,NSKIP,IRP,ITER,
     X              MAXNEED,MINNEED,MEMUSE,IFIXAX,NX,NY,LEV,IVOL)
               DO 75 J=0,IRP-1
                  IBUF(K+J)=ITEMP(J+1)
 75            CONTINUE
            ELSE
               CALL CGET(ITEMP,LOCD,IBIT,NBITS,NSKIP,IRP,ITER,
     X              MAXNEED,MINNEED,MEMUSE,IFIXAX,NX,NY,LEV,IVOL)
               CALL GBYTES(ITEMP,IBUF(K),0,NBITS,0,IRP)
            END IF
            LOCD = LOCD + NWL
            K = K + IRP
 70      CONTINUE
         
      END IF
      
      RETURN
      
      END
      
