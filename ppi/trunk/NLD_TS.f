c
c----------------------------------------------------------------------X
c
      SUBROUTINE NLD_TS(TMN,TMX,HMN,HMX,XMN,XMX,YMN,YMX,TS_MN,TS_MX)
C
C     Routine to read addition command lines associated with
C     GETNLD
C
      READ(5,5)(JNDAT(I),I=1,10)
 5    FORMAT(10A8)
c      WRITE(6,51)(JNDAT(I),I=1,10)
c 51   FORMAT('Kardin=',10A8)
      IF(JNDAT(2).NE.'TSERIES ')THEN
         WRITE(6,6)
 6       FORMAT('*** GETNLD - NO TSERIES LINE, FOLLOWED BY END ***')
         STOP
      END IF
      READ(JNDAT,7)TMN,TMX,HMN,HMX,XMN,XMX,YMN,YMX
 7    FORMAT(//F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0)
      READ(5,5)(JNDAT(I),I=1,10)
c      WRITE(6,51)(JNDAT(I),I=1,10)
      IF(JNDAT(1).NE.'END     ')THEN
         WRITE(6,6)
         STOP
      END IF

C     Convert TMN=HHMMSS and TMX=HHMMSS to seconds
C
      IHR=INT(TMN/10000.0)
      IMN=(TMN-10000.0*INT(TMN/10000.0))/100.0
      ISC=TMN-IHR*10000.0-IMN*100.0
      TS_MN=IHR*3600.0+IMN*60.0+ISC
      print *,'TMN,IHR,IMN,ISC,TS_MN=',TMN,IHR,IMN,ISC,TS_MN

      IHR=INT(TMX/10000.0)
      IMN=(TMX-10000.0*INT(TMX/10000.0))/100.0
      ISC=TMX-IHR*10000.0-IMN*100.0
      TS_MX=IHR*3600.0+IMN*60.0+ISC
      print *,'TMX,IHR,IMN,ISC,TS_MX=',TMX,IHR,IMN,ISC,TS_MX
      print *,'HMN,HMX,XMN,XMX,YMN,YMX=',HMN,HMX,XMN,XMX,YMN,YMX

      RETURN
      END




