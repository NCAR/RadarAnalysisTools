c
c----------------------------------------------------------------------X
c
      FUNCTION F(V)
      COMMON /TERPC/U1,V1,U2,V2,UD,VD
      COMMON /TERPC1/DU,DV
      F=(V-V2)*DU/DV+U2
      RETURN
      END
