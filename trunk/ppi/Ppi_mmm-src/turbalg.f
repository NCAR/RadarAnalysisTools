

C *************************************************************************
C *                                                                       *
C *                              turbalg                                  *
C *                                                                       *
C *************************************************************************
C
C       void turbalg -- This module implements the NEXRAD turbulence 
C       algorithm developed by A. R. Bohne. The purpose of the algorithm
C       is to measure turbulent air motions.
C
C          22 November 1993
C
C
C       Method: 
C
C       Inputs: 
C
C        tos :    the estimated maximum diameter of turbulent eddies for a
C                 particular meteorlogical event (1-4), in km
C
C        range :  slant range to the center of sample volume, in km 
C                          
C        swidth : standard deviation of doppler spectrum for  
C                 a data ray, in m/s
C
C        refl :   the effective radar reflectivity factor of a sample volume,
C                 in dBZe
C
C	ngates :  number of gates along the radial
C
C 
C
C       Outputs:
C        
C	cturb :   the turbulence that has been corrected for precipitation 
C                 effects, in cm^(2/3)/sec
C
C
C       Author: G. M. Cunning
C
C
C       Modification History:
C


      subroutine turbalg(IOUT,IIN1,IIN2,MNGATE,MXGATE,NAZ,C1,C2)




C
C set constants
C


C yint: y-intercept of the curve that of the curve that   
C estimates norm_spectrum_width, in km^(2/3).
C                                         
C slope: the slope, between ranges 0 to 5 km, of the curve that      
C estimates norm_spectrum_width, in km^(2/3)/km.
C                               
C asymp: the quasi-asymptotic value, at a range of 220 km,    
C of thecurve that estimates norm_spectrum_width, in km^(2/3).
C                    
C nswidth: the normalized spectrum width variance,          
C in km^(2/3).
C 
C ucturb: the turbulence that has not been corrected for              
C precipitation effects, in km^(4/3)/hr^2.
C                       
C precip: a dimensionless factor that accounts for imperfect tracers, 
C imperfect tracers, when estimating turbulence.
C 
C alpha: a varible used in calculating norm_spectrum_width.
C
C beta: a varible used in calculating norm_spectrum_width.               
C
C beamwidth: angular width of radar beam between the half-power 
C points, in radians.
C
C kolmogrov: dimensionless constant used to determine turbulence 
C under local isotropic conditions .
C
C turbconv: conversion factor between km^(2/3)/hr and cm^(2/3)/sec.
C 
C mpstokpmh: convert from m/s to km/hr
C
C k: the index for slant range                                   
C

C 
C dimension arrays 
C
      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'


        dimension nswidth(MXR)

        REAL nswidth,mpstokmh

        DATA KOLMOGROV,TURBCONV,MPSTOKMH/1.35,0.598454,0.28/
        DATA BW,DEFTOS/0.017,2.0/

        TOS=C1
        IF(TOS.LE.0.)TOS=DEFTOS
        BEAMWIDTH=C2
        IF(BEAMWIDTH.LE.0.)BEAMWIDTH=BW




C
C compute y-intercept, slope, and asymptotic value for normalized
C spectrum width.
C

        call varcrv(tos,yint,slope,asymp)

C 
C compute the normalized spectrum width
C

        alpha = (3.559707*beamwidth**2.0)/tos**2.0

        do 10 k = MNGATE,MXGATE

          beta = exp(-alpha*rng(k,1)**2.0)
          nswidth(k) = beta*(yint + beta*slope*rng(k,1)) + asymp*(1.0 -
     X                 beta**2.0)

 10     continue

C
C compute turbulence
C

        DO 20 J=1,NAZ
        do 20 k = MNGATE,MXGATE

C
C compute the uncorrected turbulence
C
C convert spectrum width, standard deviation in m/s, to a variance in
C km^2/hr^2.
C

      ucturb = (mpstokmh*DAT(K,J,IIN2))**2.0/(kolmogrov*nswidth(k))

C 
C compute the corrected turbulence
C
           call pcpfactor(rng(k,1),dat(k,j,iin1),precip)
           dat(k,j,iout) = turbconv*(ucturb*precip)**(0.5)


	
 20     continue

        return
      end
