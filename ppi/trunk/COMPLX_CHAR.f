      SUBROUTINE COMPLX_CHAR

C     Test creating some complex characters for plotting
C        KRL: Star (-), Circled point (E), Circle + (G)
C   
      CHARACTER*6 SMRK,LAB6

      CSIZ=12.0
      SMRK='&KRL&-'
      WRITE(LAB6,13)SMRK
 13   FORMAT(A6)
      Y1=80.0
      X1=-40.0
      CALL PLCHHQ (X1,Y1,LAB6,CSIZ,0.0,0.0)

      SMRK='&KRL&E'
      WRITE(LAB6,13)SMRK
      X1=-30.0
      CALL PLCHHQ (X1,Y1,LAB6,CSIZ,0.0,0.0)
      SMRK='&KRL&G'
      WRITE(LAB6,13)SMRK
      X1=-20.0
      CALL PLCHHQ (X1,Y1,LAB6,CSIZ,0.0,0.0)
      SMRK='&KRL&W'
      WRITE(LAB6,13)SMRK
      X1=-10.0
      CALL PLCHHQ (X1,Y1,LAB6,CSIZ,0.0,0.0)
      SMRK='&KRL&X'
      WRITE(LAB6,13)SMRK
      X1=0.0
      CALL PLCHHQ (X1,Y1,LAB6,CSIZ,0.0,0.0)
      SMRK='&KRL&Y'
      WRITE(LAB6,13)SMRK
      X1=10.0
      CALL PLCHHQ (X1,Y1,LAB6,CSIZ,0.0,0.0)
      SMRK='&KRL&Z'
      WRITE(LAB6,13)SMRK
      X1=20.0
      CALL PLCHHQ (X1,Y1,LAB6,CSIZ,0.0,0.0)

      SMRK='&KGL&E'
      WRITE(LAB6,13)SMRK
      Y1=70.0
      X1=-40.0
      CALL PLCHHQ (X1,Y1,LAB6,CSIZ,0.0,0.0)
      SMRK='&KGL&G'
      WRITE(LAB6,13)SMRK
      X1=-30.0
      CALL PLCHHQ (X1,Y1,LAB6,CSIZ,0.0,0.0)
      SMRK='&KGL&Y'
      WRITE(LAB6,13)SMRK
      X1=-20.0
      CALL PLCHHQ (X1,Y1,LAB6,CSIZ,0.0,0.0)
      SMRK='&PRU&+'
      WRITE(LAB6,13)SMRK
      X1=-10.0
      CALL PLCHHQ (X1,Y1,LAB6,CSIZ,0.0,0.0)
      SMRK='&PRU&-'
      WRITE(LAB6,13)SMRK
      X1=0.0
      CALL PLCHHQ (X1,Y1,LAB6,CSIZ,0.0,0.0)
      SMRK='&PRU&*'
      WRITE(LAB6,13)SMRK
      X1=10.0
      CALL PLCHHQ (X1,Y1,LAB6,CSIZ,0.0,0.0)
      SMRK='&PGU&Z'
      WRITE(LAB6,13)SMRK
      X1=20.0
      CALL PLCHHQ (X1,Y1,LAB6,CSIZ,0.0,0.0)

      RETURN
      END
