c
c----------------------------------------------------------------------X
c
      SUBROUTINE CONDAVG(DAT,IOUT,IIN1,IIN2,C1,C2,C3,C4,BDVAL,MNGATE,
     X     MXGATE,NANG,MXR,MXA,MXF)
C
C  FUNCTION - CONDAVG: F(OUT)={F(IIN1)-AVG[F(IIN1)]} + {F(IIN2)+C4}
C                      Offset F(OUT) to new avg according to 
C                      conditional field value [F(IIN2)] corrected
C                      by C4.  Adjusts F(OUT) in each C3 interval of
C                      F(IIN2) from C1 to C2 to have an average value
C                      of F(IIN2) + C4.
C
C     IOUT   - Output field number
C     IIN1   -  Input   "      "   (Field one)
C     IIN2   -  Input   "      "   (  "   two)
C     DATIN1 - Temporary variable to allow IOUT=IIN1
C     DATIN2 - Temporary variable to allow IOUT=IIN2
C     C1     - Lower bound of conditional averaging
C     C2     - Upper   "    "      "          "
C     C3     - Increment    "      "          "
C     C4     - Correction to average is F(IIN1) is F(IIN2) + C4
C
      DIMENSION DAT(MXR,MXA,MXF)

      PARAMETER (MXS=500)
      DIMENSION CNT(MXS),SUM(MXS),AVG(MXS)

      BNDLFT=C1
      BNDRHT=C2
      DATINC=C3
      DATADJ=C4
      NINC=1.001+(BNDRHT-BNDLFT)/DATINC
      IF(NINC.GT.MXS)NINC=MXS
      DELTA=0.5*C3
c      write(*,*)"iin1,iin2,iout=",iin1,iin2,iout
c      write(*,*)"lft-rht,inc,adj=",bndlft,bndrht,datinc,datadj
c      write(*,*)"       ninc,del=",ninc,delta

C     Clear accumulator arrays before doing averages.
C
      DO N=1,MXS
         SUM(N)=0.0
         CNT(N)=0.0
         AVG(N)=BDVAL
      END DO

C     Pass through the data once and accumulate for an average
C     of DAT(IIN1) where [DATLFT .le. DAT(IIN2) .lt. DATRHT].
C
      DO 100 J=1,NANG
         DO 90 I=MNGATE,MXGATE
            DAT(I,J,IOUT)=DAT(I,J,IIN1)
            DATIN1=DAT(I,J,IIN1)
            DATIN2=DAT(I,J,IIN2)
            IF(DATIN2.EQ.BDVAL)GO TO 90
            IF(DATIN1.EQ.BDVAL)GO TO 90

C           Loop over accumulator bins for correct one for current 
C           conditional value [F(IIN2)] and increment accumulators.
C
            DO N=1,NINC
               DATMID=BNDLFT+(N-1)*DATINC
               DATLFT=DATMID-DELTA
               DATRHT=DATMID+DELTA
               IF(DATIN2.GE.DATLFT.AND.DATIN2.LT.DATRHT)THEN
                  SUM(N)=SUM(N)+DATIN1
                  CNT(N)=CNT(N)+1.0
c                  if(mod(j,5).eq.0 .and. mod(i,5).eq.0)then
c                  write(6,1770)j,i,n,cnt(n),datmid,datin2,datin1
c 1770             format('jin,cnt,dm21=',3i8,4f8.1)
c                  end if
               END IF
            END DO
 90      CONTINUE
 100  CONTINUE

C     Compute averages within each of the NINC conditional bins.
C
      DO N=1,NINC
         DATMID=BNDLFT+(N-1)*DATINC
         DATLFT=DATMID-DELTA
         DATRHT=DATMID+DELTA
         IF(CNT(N).GT.0)THEN
            AVG(N)=SUM(N)/CNT(N)
         ELSE
            AVG(N)=BDVAL
         END IF
c         write(6,1771)n,cnt(n),datlft,datmid,datrht,avg(n)
c 1771    format('n,cnt,dlmr=',i8,f8.0,4f8.1)
      END DO

C     Now pass through the data a second time and adjust 
C     DAT(I,J,IIN1) to these new averages (DATMID + DATADJ).
C
      DO 120 J=1,NANG
         DO 110 I=MNGATE,MXGATE
            DATIN1=DAT(I,J,IIN1)
            DATIN2=DAT(I,J,IIN2)
            IF(DATIN2.EQ.BDVAL)GO TO 110
            IF(DATIN1.EQ.BDVAL)GO TO 110
            
C           Loop over accumulator bins for correct one for current 
C           conditional value [F(IIN2)] and adjust to new average.
C
            DO N=1,NINC
               DATMID=BNDLFT+(N-1)*DATINC
               DATLFT=DATMID-DELTA
               DATRHT=DATMID+DELTA
               IF(DATIN2.GE.DATLFT.AND.DATIN2.LT.DATRHT)THEN
                  IF(AVG(N).NE.BDVAL)THEN
                     DAT(I,J,IOUT)=(DATIN1-AVG(N))+(DATMID+DATADJ)
                  END IF
c                  if(mod(j,5).eq.0 .and. mod(i,5).eq.0)then
c                  write(6,1772)j,i,n,avg(n),datmid,datin1,dat(i,j,iout)
c 1772             format('jin,avg,dm10=',3i8,4f8.1)
c                  end if
               END IF
            END DO

 110     CONTINUE
 120  CONTINUE

      RETURN
      END




