      FUNCTION ITPFLD(NAMIN)
C
C        NAMIN IS A LEFT-JUSTIFIED 4 CHAR 64 BIT WORD
C
C        RETURNS AN INTEGER VALUE FOR CERTAIN IDENTIFIABLE FIELD TYPES
C           0-UNDETERMINED
C           1-POWER (DBM)
C           2-REFLECTIVITY (DBZ)
C           3-VELOCITY
C           4-SPECTRAL WIDTH
C           5-NORMALIZED COHERENT POWER
C           6-TIME
C           .
C           . NOT SPECIFIED YET
C
      PARAMETER (ITYP=6,LEV=19)
      DIMENSION IFLDS(ITYP,LEV)
      CHARACTER*8 CTEMP1
      DATA IFLDS/'DM','DZ','VE','SW','CR','TI',
     X           'SM','SZ','VF','VA','CO','TI',
     X           'XM','SM','VU','SD','CF','TM',
     X           'DB','ZR','VT','S2','CF','TM',
     X           'XM','ZD','VR','SD','CF','TM',
     X           'XM','ZH','VR','SD','CF','TM',
     X           'XM','ZV','VR','SD','CF','TM',
     X           'XM','XH','VR','SD','NC','TM',
     X           'XM','XX','VR','SD','CF','TM',
     X           'XM','XZ','VR','SD','CF','TM',
     X           'XM','TY','VR','SD','CF','TM',
     X           'XM','TZ','VR','SD','CF','TM',
     X           'XM','TS','VR','SD','CF','TM',
     X           'XM','TT','VR','SD','CF','TM',
     X           'XM','SN','VR','SD','CF','TM',
     X           'XM','SN','VR','SP','CF','TM',
     X           'DM','CZ','VE','SW','CR','TI',
     X           'DM','DR','VE','SW','CR','TI',
     X           'DM','KD','VE','SW','CR','TI'/

      WRITE (CTEMP1,500)NAMIN
 500  FORMAT(A4)
      READ (CTEMP1,101)NAMF
 101  FORMAT (A2)
      DO 12 I=1,ITYP
         K=I
         DO 10 J=1,LEV
            IF (NAMF.EQ.IFLDS(I,J)) GOTO 20
 10      CONTINUE
 12   CONTINUE
      K=0
 20   CONTINUE
      ITPFLD=K
      RETURN
      END
