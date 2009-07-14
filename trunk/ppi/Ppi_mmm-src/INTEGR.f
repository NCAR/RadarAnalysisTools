c
c----------------------------------------------------------------------X
c
      SUBROUTINE INTEGR
C
C  ROUTINE TO COMPUTE THE CURRENT TIME-AVERAGE VALUES WITH
C     ENTRY POINT UNINTEGR TO RESTORE SUMMATION VALUES
C     Note: IFL must be set in calling routine
C
      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'
      INCLUDE 'input.inc'
      INCLUDE 'swth.inc'
      COMMON /INPUTCH/NAMFLD(MXF),IRATYP,ICORD
      CHARACTER*8 NAMFLD,IRATYP,ICORD
      DATA EPS/1.0E-16/

      CALL MNMX(DROLD)
      MN=MAX0(NINT(MNGATE*DROLD/DRSW),MNGATE)
      MX=MIN0(NINT(MXGATE*DROLD/DRSW),MXGATE)

C     COMPUTE TIME AVERAGE THROUGH CURRENT SWEEP
C
      IF(IFLD(IFL).EQ.-2)THEN
         DO 12 I=1,NANG(2)
            DO 10 J=MN,MX
               IF(DAT(J,I,IFL).NE.BDVAL.AND.
     +            DAT(J,I,IFL+1).NE.BDVAL.AND.
     +            DAT(J,I,IFL+1).GE.EPS)THEN
                  DAT(J,I,IFL)=DAT(J,I,IFL)/DAT(J,I,IFL+1)
               ELSE
                  DAT(J,I,IFL)=BDVAL
               END IF
 10         CONTINUE
 12      CONTINUE

C     SET ZERO TIMEs TO BDVAL
C
      ELSE IF(IFLD(IFL).EQ.-3)THEN
         DO 22 I=1,NANG(2)
            DO 20 J=MN,MX
               IF(DAT(J,I,IFL).LT.EPS)DAT(J,I,IFL)=BDVAL
 20         CONTINUE
 22      CONTINUE
      END IF

      RETURN
c
c-----ENTRY POINT TO RESTORE SUMMATION VALUES--------------------------X
c
      ENTRY UNINTEGR
      CALL MNMX(DROLD)
      MN=MAX0(NINT(MNGATE*DROLD/DRSW),MNGATE)
      MX=MIN0(NINT(MXGATE*DROLD/DRSW),MXGATE)

C     RESTORE SUMMATION ACCUMULATOR THROUGH CURRENT SWEEP
C
      IF(IFLD(IFL).EQ.-2)THEN
         DO 32 I=1,NANG(2)
            DO 30 J=MN,MX
               IF(DAT(J,I,IFL).NE.BDVAL.AND.
     +            DAT(J,I,IFL+1).NE.BDVAL)THEN
                  DAT(J,I,IFL)=DAT(J,I,IFL)*DAT(J,I,IFL+1)
               ELSE
                  DAT(J,I,IFL)=BDVAL
               END IF
 30         CONTINUE
 32      CONTINUE

C     RESTORE TIMEs ACCUMULATOR THROUGH CURRENT SWEEP
C
      ELSE IF(IFLD(IFL).EQ.-3)THEN
         DO 42 I=1,NANG(2)
            DO 40 J=MN,MX
               IF(DAT(J,I,IFL).EQ.BDVAL)DAT(J,I,IFL)=BDVAL
 40         CONTINUE
 42      CONTINUE
      END IF

      RETURN
      END
