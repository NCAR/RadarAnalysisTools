c
c----------------------------------------------------------------------X
c
      SUBROUTINE SETLWC(INDAT)
C
C     Compute vertical profile of adiabatic liquid water content from 
C     input cloud base conditions of (p,t,z)=(PB,TB,ZB).  Routine ADIABLWC
C     returns height (PALT) and adiabatic liquid water content (ADBLWC) 
C     arrays at 2-mb increments from cloud base pressure (P).  
C     LWC_DZ:  Interpolates for ADLWC at current range gate height (HT).
C     Default (p,t,z) from Florida CaPE91
C     NC - Counter for the current adiabatic profile
C
      CHARACTER*8 INDAT(10)

      PARAMETER (NAMX=500,NCMX=10)
      COMMON/LWC/PBASE(NCMX),TBASE(NCMX),ZBASE(NCMX),B1(NCMX),
     X     B2(NCMX),B3(NCMX),HMAX(NCMX),NAL(NCMX),PALT(NAMX,NCMX),
     X     ADBLWC(NAMX,NCMX),ADBDBZ(NAMX,NCMX),ADBDIA(NAMX,NCMX),
     X     ADBCON(NAMX,NCMX),NC
      DIMENSION ENVPRS(NAMX),ADBTMP(NAMX),DEWTK(NAMX)

      DATA R1,R2,R3/943.0,22.4,0.6/
      DATA S1,S2,S3/0.85,0.85,212.0/

      NC=NC+1
      WRITE(6,11)(INDAT(I),I=2,10)
 11   FORMAT(1X,'SETLWC: ',9A8)
      READ(INDAT,13)PB,TB,ZB,B1(NC),B2(NC),B3(NC)
 13   FORMAT(/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0)
      IF(PB.EQ.0.0.AND.TB.EQ.0.0.AND.ZB.EQ.0.0)THEN
         PBASE(NC)=R1
         TBASE(NC)=R2
         ZBASE(NC)=R3
      ELSE
         PBASE(NC)=PB
         TBASE(NC)=TB
         ZBASE(NC)=ZB
      END IF
      IF(B1(NC).EQ.0.0.AND.B2(NC).EQ.0.0.AND.B3(NC).EQ.0.0)THEN
         B1(NC)=S1
         B2(NC)=S2
         B3(NC)=S3
      END IF

      NAL(NC)=0
      CALL ADIABLWC(PBASE,TBASE,ZBASE,B3,PALT,ADBLWC,ADBDBZ,ADBDIA,
     X     ADBCON,HMAX,NAMX,NCMX,NC,NAL,ENVPRS,ADBTMP,DEWTK)

      RETURN
      END








