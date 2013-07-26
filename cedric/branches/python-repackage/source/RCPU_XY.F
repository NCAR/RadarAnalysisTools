      FUNCTION RCPUX(IX)
c
c     Replaces obsolete RCPUX within the NCAR Graphics, where
c     RCPUX was a conversion from PAUs to user coordinates.
c
c     Transformation from plotter address units (PAUs)
c     to user coordinate system.  First convert input
c     PAU to fractional, then to user coordinate system.
c        Assumption: calling routine has already done a
c        conversion from fractional to PAU if needed
c        using the in-line function LOCPLT(R) = 1023.*R
c     Input:
c           IX - Integer PAUs (1 --> 1024)
c     Output:
c           RCPUX - Fractional value (0 --> 1) 
c
      LL=1
      CALL GETSET(FL,FR,FB,FT,UL,UR,UB,UT,LL)
c      print *,'RCPUX: fl,fr,fb,ft=',fl,fr,fb,ft
c      print *,'RCPUX: ul,ur,ub,ut=',ul,ur,ub,ut
c      CALL SFLUSH

c     Convert input PAUs to fractional coordinate, then
c     convert fractional coordinate to user coordinate.
c
      FX = FLOAT(IX)/1023.
      SLOPE = (UR-UL)/(FR-FL)
      RCPUX = UL+(FX-FL)*SLOPE

c      print *,'RCPUX: ix,fx=',ix,fx
c      print *,'RCPUX: slope=',slope
c      print *,'RCPUX: fx,slope,rcpux =',fx,slope,rcpux
c      CALL SFLUSH

      RETURN
      END

      FUNCTION RCPUY(IY)
c
c     Replaces obsolete RCPUY within the NCAR Graphics, where
c     RCPUY was a conversion from PAUs to user coordinates.
c
c     Transformation from plotter address units (PAUs)
c     to user coordinate system.  First convert input
c     PAU to fractional, then to user coordinate system.
c        Assumption: calling routine has already done a
c        conversion from fractional to PAU if needed
c        using the in-line function LOCPLT(R) = 1023.*R
c     Input:
c           IY - Integer PAUs (1 --> 1024)
c     Output:
c           RCPUY - Fractional value (0 --> 1) 
c
      LL=1
      CALL GETSET(FL,FR,FB,FT,UL,UR,UB,UT,LL)
c      print *,'RCPUY: fl,fr,fb,ft=',fl,fr,fb,ft
c      print *,'RCPUY: ul,ur,ub,ut=',ul,ur,ub,ut
c      CALL SFLUSH

c     Convert input PAUs to fractional coordinate, then
c     convert fractional coordinate to user coordinate.
c
      FY = FLOAT(IY)/1023.
      SLOPE = (UT-UB)/(FT-FB)
      RCPUY = UB+(FY-FB)*SLOPE

c      print *,'RCPUY: iy,fy=',iy,fy
c      print *,'RCPUY: slope=',slope
c      print *,'RCPUY: fy,slope,rcpuy =',fy,slope,rcpuy
c      CALL SFLUSH

      RETURN
      END

