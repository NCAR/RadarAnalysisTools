      program tst
      
C     adjust XY-window when DX .NE. DY
C
      XMIN=-300.0
      XMAX=300.0
      YMIN=-150.0
      YMAX=150.0
      SIDE=0.9
      DX = XMAX-XMIN
      DY = YMAX-YMIN
      RAT_XY=DX/DY
      RAT_YX=DY/DX
      IF(RAT_XY.GT.RAT_YX)THEN
         XSIDE=SIDE
         YSIDE=SIDE*RAT_YX
      ELSE
         XSIDE=SIDE*RAT_XY
         YSIDE=SIDE
      END IF
      print *,'XMIN,XMAX,DX=',XMIN,XMAX,DX
      print *,'YMIN,YMAX,DY=',YMIN,YMAX,DY
      print *,'SIDE,XSIDE,YSIDE=',SIDE,XSIDE,YSIDE
      XMIN=-100.0
      XMAX=100.0
      YMIN=-150.0
      YMAX=150.0
      SIDE=0.9
      DX = XMAX-XMIN
      DY = YMAX-YMIN
      RAT_XY=DX/DY
      RAT_YX=DY/DX
      IF(RAT_XY.GT.RAT_YX)THEN
         XSIDE=SIDE
         YSIDE=SIDE*RAT_YX
      ELSE
         XSIDE=SIDE*RAT_XY
         YSIDE=SIDE
      END IF
      print *,'XMIN,XMAX,DX=',XMIN,XMAX,DX
      print *,'YMIN,YMAX,DY=',YMIN,YMAX,DY
      print *,'SIDE,XSIDE,YSIDE=',SIDE,XSIDE,YSIDE
      IF(XMIN.EQ.-100.0)STOP

C     calculate LMA bins in the vertical
C
      HMX = 20.0
      HMN = 0.0
      HIN = 0.5
      NBIN = (HMX-HMN)/HIN
      print *,'NBIN=',nbin
      DO I=1,NBIN
         HBMN=HMN+(I-1)*HIN
         HBMX=HBMN+HIN
         print *,'I,HBMN,HBMX=',i,hbmn,hbmx
      END DO
      do i=1,15
         Z=4.0+(i-1)*0.1
         J=1.1+(Z-HMN)/HIN
         print *,'Z,J=',z,j
      end do
      STOP

C     Convert HHMMSS to seconds
C
      TMN=201000
      TMX=215615
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
      IF(TMN.EQ.201000)STOP

      DO I = 0,360,1
         AZMIN = FLOAT(I)
         AZMAX = FLOAT(I)
         AZMN = 10.0*INT(AZMIN/10.0)-10.0
         AZMX = 10.0*INT(AZMAX/10.0)+10.0
         WRITE(6,11)AZMIN,AZMN,AZMAX,AZMX
 11      FORMAT(1x,'AzMin=',2f8.0,' AzMax='2f8.0)
      END DO
      DO I = 0,90,1
         ELMIN = FLOAT(I)
         ELMAX = FLOAT(I)
         ELMN = 5.0*INT(ELMIN/5.0)-5.0
         ELMX = 5.0*INT(ELMAX/5.0)+5.0
         WRITE(6,13)ELMIN,ELMN,ELMAX,ELMX
 13      FORMAT(1x,'ElMin=',2f8.0,' ElMax='2f8.0)
      END DO

      tnld=74717.0
      IHR=INT(TNLD/3600.0)
      IMN=INT((TNLD-FLOAT(IHR*3600))/60.0)
      ISC=INT((TNLD-FLOAT(IHR*3600)-FLOAT(IMN*60)))
      IHMS=IHR*10000+IMN*100+ISC
      print *,tnld,ihr,imn,isc,ihms
      stop
      end
