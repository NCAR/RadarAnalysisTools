c
c----------------------------------------------------------------------X
c
      FUNCTION G(U)
      COMMON /TERPC/U1,V1,U2,V2,UD,VD
      COMMON /TERPC1/ DU,DV
      G=(U-U2)*DV/DU+V2
      RETURN
      END
