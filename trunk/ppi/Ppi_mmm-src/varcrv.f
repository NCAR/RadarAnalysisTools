
C
C Copyright 1993 National Center for Atmospheric Research
C All Rights Reserved
C
C
C
C *************************************************************************
C *                                                                       *
C *                                turbalg                                *
C *                                                                       *
C *************************************************************************
C
C                        22 November 1993
C
C       Description: This module implements the NEXRAD turbulence 
C       algorithm developed by A. R. Bohne. The purpose of the algorithm
C       is to measure turbulent air motions.
C
C
C       See Also: Turbulence Algorithm Description NX-DR-01-023/10
C
C
C       Author: G. M. Cunning
C
C
C       Modification History:
C
C
C
C
C
C
C *************************************************************************
C *                                                                       *
C *                                varcrv                                 *
C *                                                                       *
C *************************************************************************
C
C       void variance_curve -- this function computes the y-intercept,
C	slope, and quasi asymptotic value of the normalized spectrum
C	width.
C
C           22 November 1993
C
C
C       Method: the function computes two different sets of values based
C	on the value of the turbulence outerscale bing less than or 
C	greater than a threshold value.
C
C       Inputs:  
C	tos: turbulence outerscale
C
C       Outputs:
C	yint: y-intercept of normalized spectrum width
C	slope: slope of normalized spectrum width
C	asmyp: quasi-asymptotic value of normalized spectrum width
C
C       Author: G. M. Cunning
C
C       Modification History:
C

      subroutine varcrv(tos,yint,slope,asmyp)


C
C tosthold: turbulence outer scale threshold in km.
C

	parameter( tosthold = 2.0 )

        if (tos .le. tosthold) then
          yint = 0.01*(7.2467 + 1.110*tos - 0.3267*tos**2.0)
          slope = 0.01*(0.2950 + 0.004461*tos + 0.02496*tos**2.0)
          asmyp = 0.01*(6.6933 + 25.150*tos - 2.5933*tos**2.0)

        else
          yint = 0.01*(8.083 + 0.0450*tos - 0.00333*tos**2.0)
          slope = 0.01*(0.3060 + 0.00475*tos - 0.000375*tos**2.0)
          asmyp = 0.01*(15.4577 + 16.5125*tos - 0.47042*tos**2.0)
        endif

        return

      end
