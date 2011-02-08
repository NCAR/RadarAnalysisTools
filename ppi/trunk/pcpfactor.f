



C *************************************************************************
C *                                                                       *
C *                             pcpfactor                                 *
C *                                                                       *
C *************************************************************************
C
C       double precip_factor -- computes the preciptation facor. It is a
C	dimensionless quanitity that corrects the turbulence calculation
C	for the presence of irregularly shaped tracer particles.
C
C          22 November 1993
C
C
C       Method:  
C
C       Inputs: 
C	range: slant range
C	refl: reflectivity factor in dBZe
C
C       Outputs:
C	result: the precipitation factor
C
C       Author: G. M. Cunning
C
C       Modification History:
C
C

      subroutine pcpfactor(range,refl,precip)




C 
C slantthold: threshold for computation of precipitation factor.
C

	parameter( slantthold = 40.0 )

        if (range .le. slantthold) then
          pmod = 1.42281 - 0.0392503*range + 0.0015615*range**2
     X           - 0.0000195982*range**3.0
          amod = (1.0 - (refl + 7.0)/89.0)*(0.305867 - 0.0314095*range +
     X           0.0014036*range**2.0 - 0.000019305*range**3.0)
          result = pmod - amod
        else
          pmod = 1.1
	  amod = 0.06*(1.0 - (refl + 7.0)/89.0)
	  result = pmod - amod
	endif

        precip = result

        return

        end
