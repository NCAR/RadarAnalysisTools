c
c----------------------------------------------------------------------X
c
      SUBROUTINE LABTIM(ITIME,XP,YP,CSIZ)
      CHARACTER*8 LAB

c      print *,'LABTIM: xp,yp,csiz=',xp,yp,csiz
      IHR=ITIME/10000
      IMIN=(ITIME-IHR*10000)/100
      ISEC=ITIME-IHR*10000-IMIN*100
      WRITE(LAB,103)IHR,IMIN,ISEC
103   FORMAT(2(I2.2,':'),I2.2)
c      CALL PLCHMQ(.19,.988,LAB,12.0,0.,-1.0)
      CALL PLCHMQ(XP,YP,LAB,CSIZ,0.,-1.0)
      RETURN
      END
