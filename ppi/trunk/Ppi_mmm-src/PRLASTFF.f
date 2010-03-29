c
c----------------------------------------------------------------------X
c
      SUBROUTINE PRLASTFF(IHSK,NAZZ,IPREC)
C
C  FOF FIELD FORMAT:
C     PRINTS HOUSKEEPING FROM THE PREVIOUS BEAM FOR END-OF-SWEEP
C
      DIMENSION IHSK(256)
      DATA CF/182.044444/

C     OBTAIN SOME BASIC HOUSEKEEPING INFORMATION
C
      IDATE = 10000*IHSK(4)+100*IHSK(5)+IHSK(6)
      ITIME = IHSK(7)*10000+IHSK(8)*100+IHSK(9)
      LREC  = IHSK(1)
      AZ    = IHSK(10)/CF
      EL    = IHSK(11)/CF
      IF(EL.GT.180.0)EL=EL-360.0
      DR    = IHSK(14)/1000.
      NGTS  = IHSK(15)
      VNYQ  = FLOAT(IHSK(21))*FLOAT(IHSK(20))*.0000025
      ITP   = IHSK(26)
      FXANG = IHSK(31)/CF
      IVOL  = IHSK(48)
      ITF   = IHSK(61)
      IDPROC= IHSK(62)
      IDRADR= IHSK(63)
      LRECN = AND(IHSK(67),255)
      NFLD  = IHSK(68)
      IF(IDRADR.EQ.2.AND.IDPROC.EQ.6)THEN
         IZDR=AND(IHSK(247),1)
         IF(IZDR.EQ.1)VNYQ=0.5*VNYQ
      END IF
      WRITE(6,771)IDATE,ITIME,AZ,EL,FXANG,ITP,ITF,NFLD,NGTS,
     +            DR,VNYQ,IVOL,NAZZ,LRECN,LREC,IPREC
  771 FORMAT(1X,' D=',I6.6,' T=',I6.6,' A=',F6.2,' E=',F6.2,
     +          ' Fx=',F6.2,' M=',I1,' Fl=',I1,' Nfl=',I1,' Ng=',I4,
     +          ' Dr=',F5.3,' Nq=',F5.2,' Vol=',I3,' Na=',I4,
     +          ' Lrn=',I2,' Lr=',I5,' Tr=',I5,'es')
      RETURN
      END

