
      SUBROUTINE UFDUMP(IBUF,NWDS,MAXIN,NST,SCALE,IPREC,NAZZ)
C
C     Routine to dump Universal Format (UF) housekeeping
C     STATUS OF READ: (Set in RAYIN, entry NWRAY)
C            NST = (0) Good read, (1) End-of-file, (3) End-of-data
C     MANDATORY UF HEADER BLOCK (45 16-BIT WORDS):
C      (2 ASCII CHARACTERS/WORD - 8 CHARACTER NAMES IN 4 WORDS)
C-----------------------------------------------------------------------
C   1. UF (ASCII)       2. RECORD LENGTH     3. FWA NON-MANDATORY
C   4. FWA LU HEADER    5. FWA DATA HEADER   6. RECORD NUMBER IN FILE
C   7. VOLUME NUMBER    8. RAY NUMBER        9. RECORD NUMBER IN RAY
C  10. SWEEP NUMBER    11. RADAR NAME (2)   12. RADAR NAME (2)
C  13. RADAR NAME (2)  14. RADAR NAME (2)   15. SITE NAME (2)
C  16. SITE NAME (2)   17. SITE NAME (2)    18. SITE NAME (2)
C  19. DEG LAT         20. MIN LAT          21. SEC*64 LAT
C  22. DEG LONG (+ E)  23. MIN LONG (+ E)   24. SEC*64 LONG (+ E)
C  25. RADAR HGT (M)   26. YEAR (2 DIGITS)  27. MONTH
C  28. DAY             29. HOUR             30. MINUTES
C  31. SECONDS         32. TIME ZONE (UT..) 33. AZIMUTH (DEG*64)
C  34. ELEV (DEG*64)   35. SWEEP MODE       36. FIXED ANGL (DEG*64)
C  37. DEG*64 / SEC    38. DATE WRITTEN     39. MONTH WRITTEN
C  40. DAY WRITTEN     41. GENERATOR NAME   42. GENERATOR NAME
C  43. GENERATOR NAME  44. GENERATOR NAME   45. MISSING DATA FLAG
C-----------------------------------------------------------------------
C
      DIMENSION IBUF(MAXIN)
      CHARACTER*8 NAMUF

      IF (NST.EQ.0)THEN

C        Find the velocity field NAMUF(1:1)='V' field and set VNYQ.
C
         IDATH = IBUF(5)
         NFLD  = IBUF(IDATH+2)
         DO 54 I=1,NFLD
            J=IDATH+1+2*I
            WRITE(NAMUF,11)IBUF(J)
 11         FORMAT(A2,6X)

C           IFLDHD - ADDRESS OF FIRST WORD IN DATA FIELD HEADER
C           NDATAHD- Number of 16-bit wods in field header block
C           I      - INDEX OF THE UF FIELD
C
            IFLDHD=IBUF(J+1)
            UF_SCL=FLOAT(IBUF(IFLDHD+1))
            IF(NAMUF(1:1).EQ.'V')THEN
               VNYQ=IBUF(IFLDHD+19)/UF_SCL
            END IF
 54      CONTINUE
         
C        OBTAIN SOME BASIC HOUSEKEEPING INFORMATION
C
         NRF   = IBUF(6)
         IVOL  = IBUF(7)
         IRY   = IBUF(8)
         IRC   = IBUF(9)
         ISWP  = IBUF(10)
         ALAT  = FLOAT(IBUF(19)) + IBUF(20)/60. + IBUF(21)/(64.*3600.)
         ALON  = FLOAT(IBUF(22)) + IBUF(23)/60. + IBUF(24)/(64.*3600.)
         IF(IBUF(26).GT.99)IBUF(26)=IBUF(26)-100
         IYR   = IBUF(26)
         IMON  = IBUF(27)
         IDAY  = IBUF(28)
         IBAD  = IBUF(45)
         AZ    = IBUF(33)*SCALE

         EL    = IBUF(34)*SCALE
         IF(EL.GT.180.0)EL=EL-360.0
         MODE  = IBUF(35)
         FXANG = IBUF(36)*SCALE
         NFH   = IBUF(IDATH+4)
         RMIN  = FLOAT(IBUF(NFH+2))+0.001*FLOAT(IBUF(NFH+3))
         IGSP  = IBUF(NFH+4)
         NRNG  = IBUF(NFH+5)
         IRN   = RMIN
         IRX   = RMIN+0.001*(NRNG-1)*IGSP+0.5
         IDATE=IBUF(26)*10000+IBUF(27)*100+IBUF(28)
         IF(IDATE.GT.991231)THEN
            JDATE=1000000*(IDATE/1000000)
            IDATE=IDATE-JDATE
         END IF
         ITIME=IBUF(29)*10000+IBUF(30)*100+IBUF(31)

         WRITE(7,771)IDATE,ITIME,AZ,EL,FXANG,NRNG,IGSP,IRN,IRX,
     +        VNYQ,IVOL,NRF,ISWP,IRC,MODE,NAZZ,IPREC
 771     FORMAT(1X,'D=',I6.6,' T=',I6.6,' A=',F5.1,' E=',F5.1,
     +        ' F=',F5.1,' Ng=',I4,' Gs=',I4,' R=',I3,'-',I4,
     +        ' Ny=',F5.1,' Vl=',I3,' Vr=',I5,' Sw=',I4,
     +        ' Br=',I1,' Md=',I1,' Na=',I4,' Tr=',I5)
      ELSE IF(NST.EQ.1)THEN
         NAZZ=NAZZ-1
         IF(NAZZ.LT.0)NAZZ=0
         WRITE(7,772)IPREC,NAZZ
 772     FORMAT(1X,'UFDUMP: End-of-file: iprec,nang=',I4,I5)
      ELSE IF(NST.EQ.2)THEN
         NAZZ=NAZZ-1
         IF(NAZZ.LT.0)NAZZ=0
         WRITE(7,773)IPREC,NAZZ
 773     FORMAT(1X,'UFDUMP: End-of-data: iprec,nang=',I4,I5)
      END IF     
      RETURN
      END
