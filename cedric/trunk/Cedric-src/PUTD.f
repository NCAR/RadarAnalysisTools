      SUBROUTINE PUTD(IBUF,NPLANE,LOCD,IBIT,NBITS,NSKIP,IRP,IFIXAX,
     X     ID,NX,NY,NZ,LEV,ITEMP,NWL,LCMB,MEMUSE,IVOL)
C     
C     THIS SUBROUTINE DETERMINES WHERE THE DATA SHOULD BE WRITTEN,
C     WHETHER TO MEMORY OR TO DISK.
C     

      INCLUDE 'CEDRIC.INC'
      DIMENSION ID(NID),ITEMP(MAXPLN)
      DIMENSION IBUF(1),LCMB(1)
      
      MEM=0
      ITER=0
      IEXTRA=0
      IF (IFIXAX.EQ.3) THEN
C     
C     PUTTING A Z LEVEL
C     
         MINNEED = LOCD 
         MAXNEED = MINNEED + NWL - 1
         
         IF (MEM.EQ.1) THEN
C     
C     EDIT VOLUME IS IN MEMORY
C     
            CALL SBYTES(LCMB(LOCD),IBUF,IBIT,NBITS,NSKIP,NPLANE)
         ELSE
C     
C     EDIT VOLUME IS ON DISK
C     
            IEXTRA=NWL*(WORDSZ/16) - NPLANE
            IF (NSKIP.EQ.0) CALL SBYTES(ITEMP,IBUF,0,NBITS,0,NPLANE)
            CALL CPUT(ITEMP,LOCD,IBIT,NBITS,NSKIP,NPLANE,ITER,MAXNEED,
     X           MINNEED,MEMUSE,IEXTRA,IFIXAX,NX,NY,LEV,IVOL)
            
         END IF
      ELSE IF (IFIXAX.EQ.2) THEN
C     
C     PUTTING A Y-LEVEL
C     
         MINNEED=LOCD - NWL
         IFAD = INT(IRP/(WORDSZ/16))
         K = 1
         DO 60 I=1,NZ
            MINNEED = MINNEED + NWL
            MAXNEED = MINNEED + IFAD -1
            
            IF (MEM.EQ.1) THEN
C     
C     PUT IT IN MEMORY
C     
               ITER = IRP
               CALL SBYTES(LCMB(LOCD),IBUF(K),IBIT,NBITS,NSKIP,ITER)
               
            ELSE
C     
C     PUT IT ON DISK
C     
               IF (NSKIP.EQ.0) CALL SBYTES(ITEMP,IBUF(K),0,NBITS,0,IRP)
               CALL CPUT(ITEMP,LOCD,IBIT,NBITS,NSKIP,IRP,ITER,
     X              MAXNEED,MINNEED,MEMUSE,IEXTRA,IFIXAX,NX,NY,LEV,IVOL)
            END IF
            LOCD = LOCD + NWL
            K = K + IRP
 60      CONTINUE
         
         
      ELSE IF (IFIXAX.EQ.1) THEN
C     
C     PUTTING AN X-LEVEL
C     
         MINNEED = LOCD - NWL
         IFAD = INT(IRP/(WORDSZ/16))
         K = 1
         DO 70 I=1,NZ
            MINNEED = MINNEED + NWL
            MAXNEED = MINNEED + IFAD 
            
            IF (MEM.EQ.1) THEN
C     
C     PUT IT IN MEMORY
C     
               ITER = IRP
               CALL SBYTES(LCMB(LOCD),IBUF(K),IBIT,NBITS,NSKIP,ITER)
               
            ELSE
C     
C     PUT IT ON DISK
C     
               IF (NSKIP.EQ.0) THEN
                  CALL SBYTES(ITEMP,IBUF(K),0,NBITS,0,IRP)
                  CALL CPUT(ITEMP,LOCD,IBIT,NBITS,NSKIP,IRP,ITER,
     X                 MAXNEED,MINNEED,MEMUSE,IEXTRA,IFIXAX,NX,NY,
     X                 LEV,IVOL)
               ELSE
                  CALL CPUT(IBUF(K),LOCD,IBIT,NBITS,NSKIP,IRP,ITER,
     X                 MAXNEED,MINNEED,MEMUSE,IEXTRA,IFIXAX,NX,NY,
     X                 LEV,IVOL)
               END IF
            END IF
            LOCD = LOCD + NWL
            K = K + IRP
 70      CONTINUE
         
         
      END IF
      
      RETURN
      
      
      
      END
