      program tst

      dimension item(510)
      character*8 ctemp

      DO I = 1,510
         ITEM(I) = 0
      END DO
      
      CTEMP = "WRFMODEL"
      READ(CTEMP,25)(ITEM(I),I=1,4)
      print *,'ITEM(1-4)=',ctemp,' ',(item(i),i=1,4)

      CTEMP = "CEDRIC"
      READ(CTEMP,10)(ITEM(I),I=5,7)
      print *,'ITEM(5-7)=',ctemp,' ',(item(i),i=5,7)

      CTEMP = "WRF  "
      READ(CTEMP,15)(ITEM(I),I=8,9)     
      print *,'ITEM(8-9)=',ctemp,' ',(item(i),i=8,9)

      CTEMP = "??????"
      READ(CTEMP,30)(ITEM(I),I=10,12)
      print *,'ITEM(10-12)=',ctemp,' ',(item(i),i=10,12)

      CTEMP = "NONE  "
      READ (CTEMP,30)(ITEM(I),I=13,15)  
      print *,'ITEM(13-15)=',ctemp,' ',(item(i),i=13,15)

      CTEMP = "WRF  " 
      READ(CTEMP,25)(ITEM(I),I=71,74)  
      print *,'ITEM(71-74)=',ctemp,' ',(item(i),i=71,74)

 10   FORMAT(4A2)
 15   FORMAT(2A2)
 25   FORMAT(4A2)
 30   FORMAT(3A2)

      vp = 0.0
      arg1 = (243.5*log(vp)-440.8)
      rbuf = arg1/(19.48-log(vp))
      print *,'vp,arg1,rbuf=',vp,arg1,rbuf
      stop
      end
