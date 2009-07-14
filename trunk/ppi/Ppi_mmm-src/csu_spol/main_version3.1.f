       program csudriver
c      Main program to use and test csu_kdp_version3.1 routine
c      This is the version dedicated to S-POL
c      Compared to previous version, the program got updated with
c      Attenucation correction and Rainrate estimation

       include 'csu_driver.h'
       integer nsize
       integer iflag_loc

c file 'testray.dat' contains an ascii test file of
c input data: range in km, raw Zh, raw Zdr, raw phidp and raw rhohv

       open(55,file='testray.dat') 

c FIle 'results.ray' cotains the input data plus output of csu_kdp_version3.1
c i.e. attenuation corrected Zh, Zdr, Kdp and specific attenuation from
c iterative Testud method  

       open(3,file='results.ray',status='unknown')

c NOTE: nsize can change depending on size of testray file.

       nsize=1006

c read in the testray input data:range in km, raw Zh, raw Zdr, raw phidp
c and raw rhohv

       do i=1,nsize
          read(55,1001) range(i),zh(i),zdr(i),phi_raw(i),rho(i)
1001      format(5(2x,f10.4))
       enddo

c range1=first 'good' range in km
c gatewid=gate spacing for CPOL=300 m
c zdrbias=system zdrbias (note: corrected Zdr=raw Zdr+zdrbias)
c For the rainrate alogrithms and gamma dsd parameter estimation
c the zdrbias must be know quite accurately (within +-0.2 dB)
c RCcf= radar constant adjustment for system gain
c NOTE: corrected Zh=raw Zh + RCcf(in db)

       zdrbias=0.00
       RCcf=0.0

c NOTE: must give correct elevation angle for the test beam
c       in degrees

       elev = 0.5

c iflag_loc=1 means Tropic and 2 means Colorado

       iflag_loc=2

 
c First set up the initialization routine

       call csu_init(iflag_loc,nsize,elev,zdrbias,RCcf)

c Now call the major routine that calculates Kdp,
c does attenuation correction of Zh and Zdr, calculates
c gamma dsd paramters, 'pol tuned' Z-R, and other rainrate
c estimates

       call csu_kdp(iflag_loc,nsize,elev)

      write(6,*)'write result'

c write output file for test beam
c range in km, raw Zh, raw Zdr, raw phidp, raw rhohv
c atten_corrected Zh, atten_corrected Zdr, Kdp and spe Atten (db/km)

      do 95 k=1,nsize
  95   write(3,710) plfile(k,0),
c    + plfile(k,1),plfile(k,2),plfile(k,3),plfile(k,5),
c    + plfile(k,20),plfile(k,21),plfile(k,33),plfile(k,27)
     + plfile(k,20),plfile(k,21),plfile(k,61),plfile(k,62),
     + plfile(k,14),plfile(k,23),plfile(k,50),plfile(k,24)

  710 format(9(3x,f9.4)) 



       end
c_____________________________________________________________

