c
c----------------------------------------------------------------------X
c
      SUBROUTINE AVRAGE(DAT,MXR,MXA,MXF,BDVAL,MNGATE,MXGATE,NANG,
     X     NAMFLD,IFLD,IFL,AVNAM)
C
C  ROUTINE TO COMPUTE THE CURRENT SWEEP VALUES FROM ACCUMULATORS
C     ENTRY POINT UNAVRAGE TO RESTORE ACCUMULATORS
C
      CHARACTER*8 NAMFLD(MXF)
      CHARACTER*8 AVNAM,TNAME
      DIMENSION DAT(MXR,MXA,MXF),IFLD(MXF),NANG(2)

C     COMPUTE AVERAGE THROUGH CURRENT SWEEP
C
      IF(IFLD(IFL).EQ.-5)THEN
         DO 12 I=1,NANG(2)
            DO 10 J=MNGATE,MXGATE
               IF(DAT(J,I,IFL+1).GE.1.0)THEN
                  DAT(J,I,IFL)=DAT(J,I,IFL)/DAT(J,I,IFL+1)
               ELSE
                  DAT(J,I,IFL)=BDVAL
               END IF
 10         CONTINUE
 12      CONTINUE

C     SET ZERO COUNTs TO BDVAL
C
      ELSE IF(IFLD(IFL).EQ.-6)THEN
         DO 16 I=1,NANG(2)
            DO 14 J=MNGATE,MXGATE
               IF(DAT(J,I,IFL).EQ.0.0)DAT(J,I,IFL)=BDVAL
 14         CONTINUE
 16      CONTINUE
         
      ELSE IF(AVNAM(5:8).EQ.'mean' .OR. AVNAM(5:8).EQ.'sdev')THEN
         
C     NOTE: When NOUT='STATS   ' in the SWATH function, four fields are 
C           created: NIN2(1:4) + ('max','min','mean','sdev', and 'npts')
C           which are stored in DAT field locations IFL, IFL+1, IFL+2, 
C           IFL+3, and IFL+4.  When plotting MEAN, the summation is in 
C           IFL and the count is in IFL+2.  When plotting SDEV, the 
C           summation is in IFL-1, the sum of squared values in IFL, 
C           and the count is in IFL+1.  Also see FIELD and SAVFUN.
C        Compute average and standard deviation through current sweep
C
         WRITE(TNAME,17)AVNAM(1:4)
 17      FORMAT(A4,'max')
         IFL_STATS=IFIND(TNAME,NAMFLD,MXF)
         IFL0=IFL_STATS
         IFL1=IFL_STATS+1
         IFL2=IFL_STATS+2
         IFL3=IFL_STATS+3
         IFL4=IFL_STATS+4
         write(6,1770)namfld(ifl ),namfld(ifl0),namfld(ifl1),
     +        namfld(ifl2),namfld(ifl3),namfld(ifl4)
 1770    format('     Avg: ',6(a8,2x))
         
         DO 20 J=1,NANG(2)
            DO 19 I=MNGATE,MXGATE
               SUM  =DAT(I,J,IFL_STATS+2)
               SUMSQ=DAT(I,J,IFL_STATS+3)
               CNT  =DAT(I,J,IFL_STATS+4)
               IF(CNT .GE. 1.0 .AND. CNT .NE. BDVAL .AND.
     X            SUM .NE. BDVAL .AND. SUMSQ .NE. BDVAL)THEN
                  D1_BAR=SUM/CNT
                  DAT(I,J,IFL_STATS+2)=D1_BAR
                  D2_BAR=SUMSQ/CNT
                  VAR=D2_BAR-(D1_BAR*D1_BAR)
                  IF(VAR .GE. 0.0)THEN
                     DAT(I,J,IFL_STATS+3)=SQRT(VAR)
                  END IF
               END IF
 19         CONTINUE
 20      CONTINUE
         
      END IF

      RETURN
c
C-----ENTRY POINT TO RESTORE ACCUMULATORS------------------------------X
c
      ENTRY UNAVRAGE(DAT,MXR,MXA,MXF,BDVAL,MNGATE,MXGATE,NANG,
     X     NAMFLD,IFLD,IFL,AVNAM)

C     RESTORE SUMMATION ACCUMULATOR THROUGH CURRENT SWEEP
C
      IF(IFLD(IFL).EQ.-5)THEN
         DO 32 I=1,NANG(2)
            DO 30 J=MNGATE,MXGATE
               IF(DAT(J,I,IFL).NE.BDVAL)THEN
                  DAT(J,I,IFL)=DAT(J,I,IFL)*DAT(J,I,IFL+1)
               ELSE
                  DAT(J,I,IFL)=0.0
               END IF
 30         CONTINUE
 32      CONTINUE

C     RESTORE COUNTs ACCUMULATOR THROUGH CURRENT SWEEP
C
      ELSE IF(IFLD(IFL).EQ.-6)THEN
         DO 36 I=1,NANG(2)
            DO 34 J=MNGATE,MXGATE
               IF(DAT(J,I,IFL).EQ.BDVAL)DAT(J,I,IFL)=0.0
 34         CONTINUE
 36      CONTINUE

      ELSE IF(AVNAM(5:8).EQ.'mean' .OR. AVNAM(5:8).EQ.'sdev')THEN

C        Restore summations from average and standard deviatin through current sweep
C
         WRITE(TNAME,27)AVNAM(1:4)
 27      FORMAT(A4,'max')
         IFL_STATS=IFIND(TNAME,NAMFLD,MXF)
         IFL0=IFL_STATS
         IFL1=IFL_STATS+1
         IFL2=IFL_STATS+2
         IFL3=IFL_STATS+3
         IFL4=IFL_STATS+4
         write(6,1771)namfld(ifl ),namfld(ifl0),namfld(ifl1),
     +        namfld(ifl2),namfld(ifl3),namfld(ifl4)
 1771    format('   Unavg: ',6(a8,2x))
         
         DO 40 J=1,NANG(2)
            DO 38 I=MNGATE,MXGATE
               D1_BAR=DAT(I,J,IFL_STATS+2)
               STDEV =DAT(I,J,IFL_STATS+3)
               CNT   =DAT(I,J,IFL_STATS+4)
               IF(CNT .GE. 1.0 .AND. CNT .NE. BDVAL .AND.
     X            D1_BAR .NE. BDVAL .AND. STDEV .NE. BDVAL)THEN
                  SUM=CNT*D1_BAR
                  VAR=STDEV*STDEV
                  SUMSQ=CNT*(VAR + (D1_BAR*D1_BAR))
                  DAT(I,J,IFL_STATS+2)=SUM
                  DAT(I,J,IFL_STATS+3)=SUMSQ
               END IF
 38         CONTINUE
 40      CONTINUE
         
      END IF

      RETURN
      END





