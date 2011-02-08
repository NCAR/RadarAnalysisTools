      subroutine csu_init(iflag_loc,ngates,elev,
     &                  zdrbias,RCcf)

      include 'csu_driver.h'
      integer iflag_loc,ngates
      real elev,zdrbias,RCcf
      real r, temp, sin_el

c This subroutine merely initialzes the plfile arrays 

c There is however, a correction of reflectivity for gas (oxygen
c and water vapor) attemuation which was not in version 2.1!!
c Th gas atten is taken from Doviak and Zrnic and adjusted for C-band!!
c the gas attenuation in dB is stored in plfile(i,70)

c The 'correct' beam height is now done which was not done for version 2.1

       Reff = 8483.0 !km effective radius of earth with refraction
                   !   physical 6374 km
       sin_el = sin(elev*0.0174532)

c The input arrays are as follows:

c uz:  uncorrected Zh from Cpol/Lassen---store in plfile(i,1)
c zdr: raw Zdr from Cpol/Lassen----------store in plfile(i,2)
c phi_raw: raw phidp---------------------store in plfile(i,3)
c rhov: 'raw' rhohv----------------------------store in plfile(i,5)


c****************WARNING!!*********************************
c The zdrbias and radar constant bias must be known accurately
c otherise the gamma dsd parameter estimates and pol tuned R
c will be incorrect.
c However, the atten correction schemes, Kdp, spec Atten, and 
c and some of the rainrates will be OK
c***********************************************************
c zdrbias: any system bias that needs to be applied to the raw Zdr
c          ie adjusted Zdr=raw Zdr+zdrbias  dB

c RCcf:    any radar constant adjustment for Zh ie
c          adjusted Zh=rawZh+RCcf   , dB

c The plfile 2D array is now filled in:
c Note: ngates is number of gates
c       gatewid is the range resolution cell, 300 m for Cpol
c       rangeg1 is the range to first gate, m
c       The above 3 parameters are provided by Lassen
c Note: The range array is stored in plfile(i,0) in km.

        do i=1,ngates
          plfile(i,0)=range(i)
          plfile(i,1)=zh(i)+RCcf
          plfile(i,2)=zdr(i)+zdrbias
c Generate Zv field

          plfile(i,7)=plfile(i,1)-plfile(i,2)
          plfile(i,3)=phi_raw(i)
          plfile(i,5)=rho(i)

c Estimate SNR using radar constant of 74.26 and noise floor of -112 dBm
c (according to the code 'ufreadspol.f')

          if(plfile(i,0).gt.0.) then
             plfile(i,11)=plfile(i,1)+37.74-20.*alog10(plfile(i,0))
          else
             plfile(i,11)=0
          endif

cccc   Calculate the corrected height for the whole ray taking into account
cccc   the curvature of earth and the refraction of atmosphare (ytwang)

          r = plfile(i,0)
          temp = r*r+Reff*Reff+2.0*r*Reff*sin_el
          corr_height(i) = sqrt(temp)-Reff

c adjust for gaseous attenuation from Doviak and Zrnic
c note tempgas is in dB, elevation in degrees and r in km!!
c equation valid only for elevation<10 deg and r<200 km
c right now only doing for tropic atmosphere and not in Colorado!!

          if(elev.lt.10.and.r.lt.200.and.iflag_loc.eq.1) then
             tempgas1=0.4+3.45*exp(-elev/1.8)
             tempgas2=27.8+154*exp(-elev/2.2)
             plfile(i,70)=tempgas1*(1-exp(-r/tempgas2))
             plfile(i,1)=plfile(i,1)+plfile(i,70)
             plfile(i,7)=plfile(i,7)+plfile(i,70)
          else
             plfile(i,70)=0
          endif

        enddo

        return
        end

