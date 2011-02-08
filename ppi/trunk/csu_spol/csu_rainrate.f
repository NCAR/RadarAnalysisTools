        subroutine rainrate_beta(length,iflag_loc,
     +       igood_data,ibegin_arr)

        integer length,igood_data(-5:1900),ibegin_arr(20)
        real mu,fir3gain,fir3coef(0:20)
        real kdpval,kdp_test,htL,htU
        integer mask,loop,fir3order,iflag_loc
        real z1(-90:1900),z2(-90:1900)
        real init1,init2,ext1,ext2

        common /FIR/ fir3order,fir3gain,fir3coef
        common /LOC/ htL,htU

        include 'csu_driver.h'

c*** This routine calculates Gorgucci's beta, Do and Nw
c     using attenuation-corrected values of Zh and Zdr
c    from kdpchill routine.  is set to 0 if height< htL (km)

c    NOTE:
c    Kdp is in plfile(i,33)
c    Zh_corr for attenu is in plfile(i,20)
c    Zdr_corr for atten is in plfile(i,21)
c    good data mask is in plfile(i,41) 0-bad, 1=good
c    beta is in plfile(i,23): slope of a/b=1.03-beta*D
c    Pruppacher Beard fit beta=0.062 mm **-1

c    Do (mm) is in plfile(i,14)

c   log10(Nw) is in plfile(i,50); Nw in mm**-1 m**-3
c   FOR MP value of 8000: log(Nw)=3.9
c NOTE; Nw is normalized intercept paratmeter of gamma dsd
c  See Eq(7.61) on pp 410 of Bring/Chandra book

c    R(Ah,Zdr)  is in plfile(i,24)
c    R(Kdp,Zdr) is in plfile(i,17)
c    R(Kdp) is in plfile(i,26)

c    mu from Gorgucci is in plfile(i,64)
c    mu is shape paramtere of gamma dsd (mu=0 means exp shape)

c    pol tuned coeff of Z-R is in plfile(i,65)
c    coeff=150 for tropical rain and 300 for stratifrom rain for example
c    Note: we continuosuly calculate the coeff of Z=aR**1.5 
c    R from tuned Z-R is in plfile(i,66)

c    phidp derived from Zh,Zdr is stored in plfile(i,34)
c    it is a lower bound based on AB-BC shapes with 10 std dev of canting
c    phidp from Zh (beta=0.06) is stored in plfile(i,67)
c    it is upper bound based on PB shapes with 10 deg std canting
c    These phidp bounds can be compared against measured phidp to set
c    the radar constant adjustment AFTER zdrbias is established


        nad2=33
        DFRc = 0

c get 2*delta_r for simulated phidp from Nw, Do

        delta_2r=plfile(3,0)-plfile(1,0)

        do i=1,length
           plfile(i, 9)=0.0  ! HDR dB
           plfile(i,14)=0.0  ! Do
           plfile(i,23)=0.0  ! Beta
           plfile(i,50)=0.0  ! log(Nw)
           plfile(i,24)=0.0  ! R(A,Zdr)
           plfile(i,17)=0.0  ! R(Kdp, Zdr)
           plfile(i,26)=0.0  ! R(Kdp)
           plfile(i,64)=0.0  ! mu(Do,zdr,beta)
           plfile(i,65)=0.0  ! coef of Z-R
           plfile(i,66)=0.0  ! tuned R(Z)
           plfile(i,67)=0.0  ! phidp from Zh,Zdr (AB_BC shapes)
           plfile(i,34)=0.0  ! phidp derived Zh,Zdr (PB shapes)
           plfile(i,61)=0.0     ! preset smooth/corr zh
           plfile(i,62)=0.0     ! preset smooth/corr zdr
        enddo
  
c FIR3 filtering only in good data region (comparable to Phidp FIR3 filtering)
        loop = 1
        do 300 i = 1,length
           mask = plfile(i,41)
           if (loop.eq.1) then ! search the good data
              if (mask.eq.1) then ! good data
                 ibegin = i
                 loop = 2
              endif
           elseif (loop.eq.2) then ! search the end of good data
              if (mask.eq.0)  then ! bad data
                 iend = i-1
                 loop = 3
              elseif (i.eq.length-1) then ! good data reaches the end
                 iend = i
                 loop = 3
              endif
           else ! we located a cell here
c             z(i-ic) = plfile(i-ic,inp)
              do k = ibegin,iend
                 z1(k) = 10.0**(0.1*plfile(k,20)) ! ZH in linear scale
                 z2(k) = 10.0**(0.1*plfile(k,21)) ! ZD in linear scale
              enddo
              loop = 1
c             FIR3 filtering
              init1=z1(ibegin)+z1(ibegin+1)+z1(ibegin+2)+z1(ibegin+3)
              init2=z2(ibegin)+z2(ibegin+1)+z2(ibegin+2)+z2(ibegin+3)
              ext1=z1(iend)+z1(iend-1)+z1(iend-2)+z1(iend-3)
              ext2=z2(iend)+z2(iend-1)+z2(iend-2)+z2(iend-3)
              do k = 1,29+ibegin !Set the initial conditions
                 z1(ibegin-k) = init1/4.0
                 z2(ibegin-k) = init2/4.0
              enddo
              do k = 1,length-iend+30 !Extend data record
                 z1(iend+k) = ext1/4.0
                 z2(iend+k) = ext2/4.0
              enddo
              do k=ibegin,iend
                 acc1=0.0
                 acc2=0.0
                 do j=0,fir3order
                    acc1=acc1+fir3coef(j)*z1(k-fir3order/2+j)
                    acc2=acc2+fir3coef(j)*z2(k-fir3order/2+j)
                 enddo
                 plfile(k,61)=10.0*log10(acc1*fir3gain)
                 plfile(k,62)=10.0*log10(acc2*fir3gain)
              enddo
           endif
300     continue


  
 
        do 1000 i=1,length
           height=corr_height(i)
           n=length



          kdpval=plfile(i,33)
c USe HDR to make sure of pure rain before estimating Do,Nw
c Using rainline for oscillating drops, see Chap 7 of B & C book

          if (igood_data(i) .eq. 1) then
             zhmean_log = plfile(i,61)
             zdrmean_log = plfile(i,62)
          else
             zhmean_log = plfile(i,20)
             zdrmean_log = plfile(i,21)
          endif
          zhlin=10.**(0.1*zhmean_log)
          zdrlin=10.**(0.1*zdrmean_log)

          if(zdrmean_log.le.0.0) then
             f_zdr = 27.
          elseif(zdrmean_log.le.1.67) then
             f_zdr=19.5*zdrmean_log+27
          else
             f_zdr=60.
          endif
          HDR=zhmean_log-f_zdr
          plfile(i,9)=HDR

c set value of dev_thres in main menu of PLT under option 6 
c the variable is called "DfrC" 
c default threshold is 0 dB
c in tropics set threshold to 5 dB 
c to bypass threshold if certain there is no hail
c then set threshold DFRc to a high number ie 20 dB (eg in Kwajalein)

          if(HDR.le.DFRc) then 

c Do only within good data mask! ytwang
             if (igood_data(i).ne.1) then
                goto 1001
             endif

c NOTE: thresholds for computing beta are Zh>35 dBZ, Zdr>0.2 dB, Kdp>0.30 deg/km
c NOTE; Have checked std dev of Zdr and Kdp after averaging in range.
c       Thresholds seem OK for SPOL/Brazil data even in stratifrom rain

             if(zhmean_log.ge.35. .and. zdrmean_log.ge.0.2 
     +       .and. kdpval.ge.0.30 .and. height.le.htL) then

c revised coeffs from Gorgucci et al "Estimation of raindrop size
c distribution parameters from polarimetric radar measurements" 2001
c 4/10/01

                plfile(i,23)=2.08*(zhlin**-0.365)*
     +          (kdpval**0.38)*(zdrlin**0.965)
                beta_gor=plfile(i,23)


c Revised relations from Gorgucci as of 6/14/01!!
c There is no difference as far as Do retrieval is concerned
c between the "old" and "new" parameterizatiosn!! 6/21/01
                a_gor=0.56
                b_gor=0.064
                c_gor=0.024*(beta_gor**(-1.42))

c First compute Do in mm, store in plfile(i,14)

                plfile(i,14)=a_gor*
     +          (zhlin**b_gor)*(zdrlin**c_gor)

c Now compute log10(Nw), store in plfile(i,50) using simulation
c results from Gorgucci as of 9/19/00 Note units of Nw: m^-3 mm^-1


c New relations from Gorgucci as of 6/14/01

                a_gor=3.29
                b_gor=0.058
                c_gor=-0.023*(beta_gor**(-1.389))

                plfile(i,50)=a_gor*
     +          (zhlin**b_gor)*(zdrlin**c_gor)


c Now compute Rainrate(Zh,Zdr) using Gorgucci's beta-paramterization
c Store in plfile(i,24). See eq(14) of Gorgucci et al JTech submitted May 2000

                tempzdr=-0.585*(beta_gor**(-0.703))
                plfile(i,24)=0.105*(beta_gor**0.865)*
     +          (zhlin**0.93)*(zdrlin**tempzdr)

c Now compute R(Kdp,Zdr) based on Gorgucci's beta formulation See eq(18) 

                tempkdp=1.337*(beta_gor**0.117)
                tempzdr=-0.014*(beta_gor**(-1.674))
                plfile(i,17)=0.481*(beta_gor**(-1.795))*
     +          (kdpval**tempkdp)*(zdrlin**tempzdr)

c Now compute R(Kdp) based on Gorgucci's beta formulation
c Eq(16) of Gorgucci et al JTech submitted May 2000

                tempkdp=1.596*(beta_gor**0.175)
                plfile(i,26)=0.44*(beta_gor**(-1.612))*
     +          (kdpval**tempkdp)

c Compute mu from Gorgucci method (see 'Methodology paper'
c store in plfile(i,64)

       a_gor=203*(beta_gor**1.89)
       b_gor=2.23*(beta_gor**0.039)
       c_gor=3.16*(beta_gor**-0.0463)
       d_gor=0.374*(beta_gor**-0.355)

       test=(a_gor*plfile(i,14)**b_gor)/(zdrlin-1)
       plfile(i,64)=test-(c_gor*(zdrlin**d_gor))

c Now compute R based on Z=F(mu)F(Nw)R**1.5
c ie the coeff of Z-R is now "tuned" polarimetrically
c First compute this coefficient and store in plfile(i,65)

      mu=plfile(i,64)
      f_mu=(3.67+mu)**(mu+4)
      f_mu=f_mu*6/(3.67**4)
      f_mu=f_mu/gammln(mu+4)

      f_Z=f_mu*gammln(mu+7)
      f_Z=f_Z/(3.67+mu)**(7+mu)

      f_R=f_mu*0.6E-03*3.141592*3.78
      f_R=f_R*gammln(4.67+mu)/(3.67+mu)**(4.67+mu)
      a_coeff=f_Z/(f_R**1.5)
      a_coeff=a_coeff*(10**(-0.5*plfile(i,50)))
      plfile(i,65)=a_coeff

c Now compute tuned Z-R and store in plfile(i,66)

      plfile(i,66)=(zhlin/a_coeff)**(0.6667)


c If thresholds are not satisfied 
c then simplify the estimation of Do and Nw
c removing condition of Zh<35 below, Bringi 5/24/02
          
             elseif( zdrmean_log.ge.0.2
     +       .and. height.le.htL .and. zhmean_log.gt.0) then

c First compute Do in mm, store in plfile(i,14)
c Do=f(Zdr) is based on "Methodology...." article 
c HAS BEEN REVISED!!

         plfile(i,14)=1.81*(zdrmean_log)**(0.486)


c23456789-123456789-123456789-123456789-123456789-123456789-12 
c Now compute log10(Nw), store in plfile(i,50) Note units of Nw: m^-3 mm^-1
c Based on retrieval of Nw based on a power law fit
c to 2D-video data in conv rain and 2D/JW in strat rain
 
                temp_Nw=21.*zhlin/(plfile(i,14)**7.3529)
                plfile(i,50)=alog10(temp_Nw)

 



c ie the coeff of Z-R is now "tuned" polarimetrically
c compute this coefficient and store in plfile(i,65)
c assuming avg mu =3 from 2D video data from many regions

      mu=3.
      f_mu=(3.67+mu)**(mu+4)
      f_mu=f_mu*6/(3.67**4)
      f_mu=f_mu/gammln(mu+4)

      f_Z=f_mu*gammln(mu+7)
      f_Z=f_Z/(3.67+mu)**(7+mu)

      f_R=f_mu*0.6E-03*3.141592*3.78
      f_R=f_R*gammln(4.67+mu)/(3.67+mu)**(4.67+mu)
      a_coeff=f_Z/(f_R**1.5)
      a_coeff=a_coeff*(10**(-0.5*plfile(i,50)))
      plfile(i,65)=a_coeff
      plfile(i,66)=(zhlin/a_coeff)**(0.6667)

c Store above R in plfile(i,24) to composite it
      plfile(i,24)=plfile(i,66)


            elseif(zhmean_log.lt.35.and.zdrmean_log.lt.0.2
     +      .and. height.le.htL.and.zhmean_log.gt.0) then


c New formula for Do=f(Zh,Zdr) assumed to be applicable for
c Zdr below threshold of 0.2 dB. This will give variable Nw rather
c then constant Nw.
c NOTE: The formula below is extended to apply for  Zdr<0.2 dB
c       even for negative values!! Must have accurate Zdr calibration!!
c random errors of 1 dB for Zh and 0.15 dB for Zdr have been added
c in the simulations and then 'fit' is done.      
c 0<Zh<35 dBZ and Do<1.5 mm is used to truncate simulations 

           plfile(i,14)=.6096*(zhlin**.0516)*(zdrlin**3.111)


c Now Nw is not a constant

                temp_Nw1=21.*zhlin/(plfile(i,14)**7.3529)
                plfile(i,50)=alog10(temp_Nw1)




c Now compute R based on Z=F(mu)F(Nw)R**1.5
c ie the coeff of Z-R is now "tuned" polarimetrically
c First compute this coefficient and store in plfile(i,65)
c assume avg mu =3 from 2D video data from many regions
      mu=3.
      f_mu=(3.67+mu)**(mu+4)
      f_mu=f_mu*6/(3.67**4)
      f_mu=f_mu/gammln(mu+4)

      f_Z=f_mu*gammln(mu+7)
      f_Z=f_Z/(3.67+mu)**(7+mu)

      f_R=f_mu*0.6E-03*3.141592*3.78
      f_R=f_R*gammln(4.67+mu)/(3.67+mu)**(4.67+mu)
      a_coeff=f_Z/(f_R**1.5)
c Use new variable Nw below!!
      a_coeff=a_coeff*(10**(-0.5*plfile(i,50)))
      plfile(i,65)=a_coeff
      plfile(i,66)=(zhlin/a_coeff)**(0.6667)
c store above R in plfile(i,24) to composite it.

      plfile(i,24)=plfile(i,66)


c getting rid of one 'eleseif' statement, Bringi, 5/24/02



c The else below goes with if (Zh>35,Zdr>0,Kdp>0.2) then
            else
               plfile(i,23)=0.0
               plfile(i,14)=0.0
               plfile(i,50)=0.0
               plfile(i,24)=0.0
               plfile(i,17)=0.0
               plfile(i,26)=0.0
               plfile(i,65)=0.0
               plfile(i,66)=0.0
               plfile(i,67)=0.0

            endif ! endif for "if(Zh>35,Zdr>0,Kdp>0.2) way above"    

c The elseif below goes with "if(HDR>DFRc) then" way above 
c The code below is for rainrate in rain/hail mixture using Kdp-R alogrithm
c Valid for tropical rain eg Brazil
c Height < htU (upper height is used here, as Kdp is not sensitive to ice)
c htU is 6 km for tropics  and 3 km for Colorado

         elseif(igood_data(i).eq.1.and.zhmean_log.ge.35
     +   .and.kdpval.ge.0.3.and.height.le.htU) then 

c place Rainrate from Kdp in plfile(i,24) which was previosly reserved
c for R(Zh,Zdr,beta) Below is valid for tropical rain (Brazil)

            plfile(i,24)=56.2*(kdpval**0.8)

c composite the tuned Z-R rainrate with above for continuity
            plfile(i,66)=plfile(i,24)

cThe else below goes with "if(HDR<DFRc) then"
         else
            plfile(i,23)=0.0
            plfile(i,14)=0.0
            plfile(i,50)=0.0
            plfile(i,24)=0.0
            plfile(i,17)=0.0
            plfile(i,26)=0.0
            plfile(i,65)=0.0
            plfile(i,66)=0.0
            plfile(i,67)=0.0
         endif ! goes with "if(HDR<DFRc) then" 

c Simulations are based on random Nw, Do and Kdp from
c using Andsager-Beard and Beard Chuang with std 10 deg
c stored in plfile(i,34) which is a lower bound
c and PB shapes for an upper bound in plfile(i,67)

1001    continue

c calcuate simulated phidp using Kdp=f(zh,zdr)

           zh_lin=10.**(.1*plfile(i,20))
           zdr_lin =10.**(.1*plfile(i,21))


        if(plfile(i,21).gt.0.0)then
           kdp_test=2.808E-05*zh_lin**(0.981)*zdr_lin**(-1.6943) 
        kdp_test_high=8.733E-05*zh_lin**(0.968)*zdr_lin**(-3.2410)
        else
           kdp_test=0.0
           kdp_test_high=0
        endif



        if(i.lt.ibegin_arr(1)) then
          plfile(i,34)=plfile(ibegin_arr(1),32)
          plfile(i,67)=plfile(ibegin_arr(1),32)
        else
          plfile(i,34)=delta_2r*kdp_test+plfile(i-1,34)
          plfile(i,67)=delta_2r*kdp_test_high+plfile(i-1,67)
        endif


1000  continue  ! goes with "do i=1,length" way above 

      return  
      end
c

c This is the gamma function
c This is the gamma function for rainrate computation
      function gammln(xx)
c
      real*8 cof(6),stp,half,one,fpf,x,tmp,ser
c
      data cof,stp/76.18009173d0, -86.50532033d0, 24.01409822d0,
     &   -1.231739516d0,.120858033d-2,-.536382d-5,2.50662827465d0/
      data half,one,fpf/0.5d0,1.0d0,5.5d0/
c
      x=xx-one
      tmp=x+fpf
      tmp=(x+half)*log(tmp)-tmp
      ser=one
      do 11 j=1,6
         x=x+one
         ser=ser+cof(j)/x
 11      continue
      gammln=exp(tmp+log(stp*ser))
      return
      end

