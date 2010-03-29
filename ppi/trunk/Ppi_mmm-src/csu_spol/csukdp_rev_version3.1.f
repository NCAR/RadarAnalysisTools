        subroutine csu_kdp(iflag_loc,length,elevation)

c THIS IS VERSION 3.1 completed in JUNE 2002 BY Bringi and Yanting Wang

c THERE ARE MANY IMPROVEMENTS COMPARED TO VERSION 2.1 done by Bringi in 1999
c MAINLY IN THE ATTENUATION CORRECTION FOR ZDR (SPEEDED UP).

c A NEW 'csu_rainrate.f' FOR RAINRATE HAS BEEN ADDED. This routine calculates 
c Do,Nw and mu for normlaized gamma dsd. Note that the mu estimate is not validated.
c A 'pol-tuned'  Z=aR**1.5 estimator has been added where 'a' is continuusly
c changing based on dsd changes. In version 2.1 the rainrates were computed
cin  the csu_kdp routine itself. Now they are in a new routine 'csu_rainrate.f'

c The new dsd parameter estimates and pol-tuned Z-R estimates assume that 
c the Zdrbias and system radar constant are accurate to within +-0.2 dB and 
c +- 1 dB respectively. IF NOT, THEN THE ESTMATES WILL BE BIASED!! Be warned!!

c Software 'bugs' in version 2.1 have been fixed (non fatal varieties).
c The height above ground has been corrected .

c The phipd unfolding routine has been fine tuned. 

c Peter May' consencus Kdp estimator is built in now. It is used only
c when the Hubbert/Bringi Kdp estimator fails.

c The storm cells are assumed to be atleast 10 gates long (or 3 km)
c with 'good' data. Smaller cells will be 'missed' or classified as
c 'bad' data segment. To handle smaller cells, change the 'mgood' parameter
c to 5 (from default of 10). 

        integer firsttime,count,igood_data(-5:1900)
        integer ibegin_arr(20),iend_arr(20)
        integer fir3order,malpha,mopt
        real x(-90:1900),y(-90:1900),z(-200:1900),z_cns(1900)
        real xx(31),yy(31),fir3coef(0:20)
        real init0,init,thres,kdp,meanzdr_corr
        real fir3gain,elevation
        real meanzdr,Ir1r0,meanzh_corr
        real kdp_orig(1900),AhAv_Sm(1900),zh_lin(1900)
        real atten(1900,20),phidp_test(1900,20),gamma(20),sumerror(20)
        real error(1900,20),phidpn(1900,20),temp(23),valuen(1900,20)
        real zhlinn(1900,20),value(1900),phidp(1900)
        real r_begin_arr(20),r_end_arr(20)
        real phinit0,htL,htU

        integer iflag_loc,length
        real*4 reduce_wt(20),increase_wt(20)
c       Add for Cons Kdp
        real phi_u(1900), phi_i(1900), kdp_c(1900)

        include 'csu_driver.h'
        data reduce_wt/.95,.9,.85,.8,.75,.7,.65,.6,.55,.5,.45,.4
     &  ,.35,.3,.25,.2,.15,.1,.05,0./
        data increase_wt/1.05,1.1,1.15,1.2,1.25,1.3,1.35,1.4,1.45
     &  ,1.5,1.55,1.6,1.65,1.7,1.75,1.8,1.85,1.9,1.95,2.0/
        data gamma/.0025,.004,.006,.008,.010,.012,.013,.014,.015,.017,
     &  .019,.020,.021,.022,.024,0.026,.028,.03,.034,.038/

        common /FIR/ fir3order,fir3gain,fir3coef
        common /LOC/ htL,htU

c NEXT THREE DATA STATEMENTS CONTAIN THE FIR FILTER ORDER, GAIN AND COEFICIENTS
       data fir3order/20/
       data fir3gain/1.044222/
       data (fir3coef(i), i=0,20)
     +  /1.625807356e-2, 2.230852545e-2, 2.896372364e-2,
     +   3.595993808e-2, 4.298744446e-2, 4.971005447e-2,
     +   5.578764970e-2, 6.089991897e-2, 6.476934523e-2,
     +   6.718151185e-2, 6.80010000e-2, 6.718151185e-2,
     +   6.476934523e-2, 6.089991897e-2, 5.578764970e-2,
     +   4.971005447e-2, 4.298744446e-2, 3.595993808e-2,
     +   2.896372364e-2, 2.230852545e-2, 1.625807356e-2/

        malpha = 1    !loop for ZH atten corr
        mopt = 8       !optimal value for gamma
        phinit0 = -999 !automatically decide initial phase
c-------------------------

        nad1 = 31          ! label(31)='Pdpadap'
        nfl  = 32          ! label(32)='Pdpadfl'
        nad2 = 33          ! label(33)='Kdpadap'
        iSNR = 11
        thres = 3.
        madflt = 2
c       thrreshold based on Spol data 
        thrs_phi=10   !     threshold for sd_dev of phidp

c Note: iflag_loc gives location flag
c       1: Darwin data  2: SCSMEX data
c Note: thrcrr is the rhohv threshold
c       htL/htU are the lower and upper heights
c       defining the melting layer , in km
c       alpha is defined by Ah=alpha*Kdp
c       beta is defined by  Ah-Av=beta*Kdp
c       The settings below will override the
c       default values set in plt.f main menu.

        if(iflag_loc.eq.1) then
c This refers to Tropic
          thrcrr=0.9
          htL=3.
          htU=6.
        endif

          if(iflag_loc.eq.2) then
c This refers to Colorado
           thrcrr=0.85
           htL=1.5
           htU=2.5
          endif


        beta=0.0034
        alpha=gamma(mopt)
c       print *,'adpm,lp,afa,bta,idx:',madflt,malpha,alpha,beta,mopt


c       Pre filter raw Zh and raw Zv and then put in Zdr array
c       call ADflt('IIR5',1,1)
c       call ADflt('IIR5',7,7)
c       call ADflt('IIR5',5,5)

        do i=1,length
c NOTE: zdrbias is included in kdp_inp_version3.1.f program
c       else, must be included here
           plfile(i,2)=plfile(i,1)-plfile(i,7)
        enddo


        inp = 3
        ikdpst = 5
        firsttime = 1
        mgood     = 10       ! number of good data to enter cell
        mbad      = 5        ! number of bad data to exit cell
        count     = 0
        kbegin    = length
        iend      = length
        loop      = 1
 
        do i=-5,length
           plfile(i,nad1)=0.0   ! preset Pdpadap
           plfile(i,nfl)=0.0    ! preset Pdpadfl
           plfile(i,nad2)=0.0   ! preset Kdpadap
           plfile(i,16)=-100.   ! "clean" Zdr
           plfile(i,25)=0.0     ! preset PDP derived from Testud
           plfile(i,48)=0.0     ! preset  Nw-testud method
           plfile(i,23)=0.      ! preset beta:axis ratio,Gorgucci
           plfile(i,60)=0.0     ! preset R(Ah,Nw) Testud
           plfile(i,20)=plfile(i,1)     ! preset Zh_corr by Testud Method
           plfile(i,21)=plfile(i,2)     ! preset Zdr_corr by Smythe/Illing method
           plfile(i,27)=0.0     ! preset Ah by Testud Method
           plfile(i,38)=0.0     ! preset Ah-AV by Smythe/Illing method
           plfile(i,6)=0.       ! preset Cons Kdp
           plfile(i,22)=0.0     ! preset integrated phidp from cons Kdp
           igood_data(i)=0      ! start from -5 to keep rainrate_beta correct
        enddo


        tempcorr=0.0
        tempfix=0.0
        anitial=0.0
        avrg2 = 0.0
        j_arr=0
        k_arr=0
        epslon_over=0.1
        epslon_under=-0.1

        
c$$$$$$$ UNWRAP DIFF PHASE $$$$$$$$
        jj_test = 0
        inwrap = 0
        extra = 0
        avgpdp = 0
        jcount = 0
        if (phinit0 .ne. -999) then
           ibegin = 1
           avgbg = phinit0
        else
           ibegin = 0
           avgbg = 0
        endif  ! to adjuest the reference phase by observation (ytwang)

        do 8 jj = 1,length

           plfile(jj,63) = plfile(jj,3)  ! save the raw phidp to checking unfold

           do jjj = 0,mgood-1
              yy(jjj+1) = plfile(jj+jjj,inp)
           enddo
           call msr(phimn,sd_phidp,yy,mgood)
           plfile(jj,49) = sd_phidp

        if(inwrap.eq.1) then
           if(plfile(jj,3).lt.(avgbg-80.+extra)) then
              plfile(jj,3)=plfile(jj,3)+180.
           endif
              do jjj = 1,5
                 xx(jjj) = plfile(jj-jjj,0)
                 yy(jjj) = plfile(jj-jjj,3)
              enddo
              call LSE(phimn_slp,bb,xx,yy,5)
              if(phimn_slp.gt.-5.0.and.phimn_slp.le.20.) then
                 extra=extra+(plfile(jj,0)-plfile(jj-1,0))*phimn_slp
              endif
        else 
           if(ibegin.eq.1) then
              if(sd_phidp.lt.(thrs_phi+10)) then
                 avgpdp=(plfile(jj,3)+plfile(jj+1,3))/2.0
                 if((avgpdp-avgbg).gt.80.0) then 
c    +              (avgpdp-avgbg).lt. -30 ) then 
                    inwrap=1
                 endif
              endif
           else
              if(sd_phidp.lt.(thrs_phi+10)
     +          .and.plfile(jj,0).ge.1.5) then
                jcount=jcount+1
                if(jcount.eq.5) then
                   sum=0.
                   do ln=0,4
                     sum=sum+plfile(jj-ln,3)
                   enddo
                   avgbg=sum/5.0
                   ibegin=1 
                endif
              else
                jcount=0
              endif
           endif
        endif
8       continue
c$$$$$$$$$$$$$$$$$$$$$$$$$$$$


c ---------------------------------------------------------------
c      Calculate Consensus Kdp, integrated Phi

c       Find out the gate_spacing for Cons Kdp
c       Set som constant for Cons Kdp
        n = length
        rinc = (plfile(101,0) - plfile(100,0))*1000
        mfl = 11
        nn = 13
        dev =1.5

        do i=1,n
c         z(i-201) = plfile(i,1)      !z(i) start at -200
          phi_u(i) = plfile(i,inp)
        enddo

        call process_kdp(n,rinc,mfl,nn,dev,plfile(1,1),  !z,
     +                  phi_u,kdp_c,x,phi_i,y)

        do i=1,n
          plfile(i,6) = kdp_c(i)
          plfile(i,22) = phi_i(i)
        enddo
c ---------------------------------------------------------------


cccc    x(i) -- IIR5 pre-filtered data;
cccc    z(i) -- updated profile;
cccc    y(i) -- filtered profile.
        ibegin = length
        do i=-5,length
           z(i)=0.0
           x(i)=0.0
        enddo


c Account for gaseous absorption using Doviak and Zrnic equations

c23456789-123456789-123456789-123456789-123456789-123456789-123456789-12
c--------- FIND THE start AND stop BINs FOR FILTERING ---------------
        plfile(0,iSNR) = plfile(1,iSNR)
        do 1000 i=1,length

           if(plfile(i,0).gt.146.5 .or. loop.eq.0) then ! cannot find a robust way
              if (loop.eq.2) then  ! In good data, seeking bad data
                 iend = i-1
                 k_arr=k_arr+1
                 iend_arr(k_arr)=iend
                 r_end_arr(k_arr)=plfile(iend,0)
              endif             ! else already in bad data
              z(i) = avrg2
              loop = 0
              goto 1000
           endif
           SNR = plfile(i,iSNR)
           SNRslevel = 3.
           goto (100,200),loop

c23456789-123456789-123456789-123456789-123456789-123456789-123456789-12
c ********** Find begin of GOOD data loop ***********
100        z(i) = avrg2

           if (plfile(i,ikdpst).ge.thrcrr
     +     .and.SNR.gt.SNRslevel.and.plfile(i,49).lt.thrs_phi) then
c change std of phidp threshold from 5 to thrs_phi Bringi 12/03/01 (same next)
              count = count+1
              if (count.eq.mgood) then
                 do  il = 0, mgood-1
                    z(i-il) = plfile(i-il,inp)
                    plfile(i-il,16) = plfile(i-il,2) ! preserve good Zdr
                 enddo

                 if (firsttime.eq.1) then
                    ibegin   = i-mgood+1 ! begin of the 1st encountered cell
                    j_arr=j_arr+1
                    ibegin_arr(j_arr)=ibegin
                    r_begin_arr(j_arr)=plfile(ibegin,0)
                    init0=(z(ibegin)+z(ibegin+1)+
     +              z(ibegin+2)+z(ibegin+3))/4.
                    init = init0    ! Recorded for local trend

                    flip   = 0.
                    delavg = init0 - phinit0

                 else
                    mc = i-mgood+1  ! begin of the successive encountered cells
                    j_arr=j_arr+1
                    ibegin_arr(j_arr)=mc
                    r_begin_arr(j_arr)=plfile(mc,0)
                    avrg1 = (z(mc+1)+z(mc+2)+z(mc+3)+z(mc+4))/4.
                    delavg = avrg1-avrg2

                 endif

c                deal with the case the first good_data need flip (ytwang)
                 if (phinit0.ne.-999 .or. firsttime.eq.0) then

                    if (abs(delavg).gt.65.) then
                       if (delavg.gt.250.) then
                          flip = -360.
                       elseif (delavg.lt.-250) then
                          flip = 360.
                       elseif (delavg.gt.140.) then
                          flip = -180.
                       elseif (delavg.lt.-140.) then
                          flip = 180.
                       elseif (delavg.gt.65) then ! (ytwang)
                          flip = -delavg !+phi_i(mc)-phi_i(iend)  !-90.
                       elseif (delavg.lt.-65) then
                          flip = -delavg !+phi_i(mc)-phi_i(iend)  !90.
                       endif
c    Notice: it is NOT robust for int_phi_dp because it makes all the decreasing flat
c    in the phidp. If there is a long segment of bad data, the phidp may decreasing 
c    first and then increasing by same amount. Hence int_phi_dp will introduces an 
c    additional error into the result. (ytwang) dec.18, 2001
                       do  kf = 0, mgood-1
                          z(i-kf) = z(i-kf) + flip
                       enddo
                    else
                       flip = 0.
                    endif

                endif


                if (firsttime.eq.1) then
                    firsttime = 0
                    init0 = init0 + flip
                    init = init0
                else
                    avrg1 = avrg1 + flip
c               ??? Tied the bad data accroding to the monotonous increasing property of Phidp
c                   why monotonous incressing? decide the bad data by local trend! Yanting
                    if (avrg1.lt.init.and.avrg2.gt.init) then
                       avrg1 = avrg2
                    elseif (avrg1.gt.init.and.avrg2.lt.init) then
                       avrg2 = init
                    elseif (abs(delavg).gt.15..and.avrg1.lt.avrg2) then
                       avrg1 = avrg2
                    endif
                    init = avrg1     ! Save local trend
                    rmc =plfile(mc,0)
                    rend=plfile(iend,0)
                    d1 = rmc-rend
                    d2 = (avrg1-avrg2)/d1
                    d3 = (rend*avrg1-rmc*avrg2)/d1
                    do 11 ij = iend+1, mc
                       rij   = plfile(ij,0)
                       z(ij) = rij*d2 - d3
11                  continue
                 endif


                 loop  = 2
                 count = 0
                 mgood = 10 
                 iend  = length
              endif
           else
              count = 0
           endif
           goto 1000

c23456789-123456789-123456789-123456789-123456789-123456789-123456789-12
c ************* Find END of GOOD DATA loop ****************  

200        z(i) = plfile(i,inp) + flip
           plfile(i,16) = plfile(i,2) ! preserve good Zdr
c          Add to integrate Phidp (PDPder) from Kdp derived from Zh & Zdr

           if(i.eq.length) then
              iend=length
              go to 203
           endif
           if (plfile(i,ikdpst).lt.thrcrr.or.SNR.lt.SNRslevel
     +     .or.plfile(i,49).gt.thrs_phi) then
              count = count + 1
              if (count.eq.mbad) then  !Insert test to preserve hail/BB signal.
                 zhmn = 0.
                 do 288 jj = 0, mbad-1
                    zhmn = 10.**(0.1*plfile(i-jj,1))+zhmn
                    yy(jj+1) = plfile(i-jj,ikdpst)
                    xx(jj+1) = plfile(i-jj,inp)
288              continue
                 zhmn = zhmn/float(mbad)
                 zhmn_log=10.*alog10(zhmn)
c Changing zh_mean value in BB or hail to 30 dBZ; 2/1/02 Bringi
c NOTE: BB with mean Zh<30 dBZ may be classified as "bad" data
                 if (zhmn_log.gt.30.) then
                    call msr(ymn,sd,yy,mbad)
                    call msr(amean,test_sd_phidp,xx,mbad)
c rhohv in BB could go as low as 0.75(equivalent LDR= - 9 dB)
           if (ymn.ge..75.and.test_sd_phidp.lt.(thrs_phi+5)) then
c changing std dev of phidp threshold in "hail" region to thrs_phi+5!! Bringi 12/03/01

          print *,'hail/BB; rhohv,zh',ymn,zhmn_log,test_sd_phidp

                       count = 0
                       goto 1000
                    endif
                 endif
                 iend = i-mbad
                 k_arr=k_arr+1
                 iend_arr(k_arr)=iend
                 r_end_arr(k_arr)=plfile(iend,0)
                 do 210 jj = 0, mbad-1  !Inserted to clean the bad Zdr & Rhv.
                    plfile(i-jj,16) = -100. ! bad value for Zdr
                    plfile(i-jj,15) = 0.    ! bad value for Rhv
210              continue
203              continue
                 avrg2=(z(iend)+z(iend-1)+z(iend-2)+z(iend-3))/4.
                 if (iend.eq.length) goto 1000
                 z(i)   = avrg2
                 z(i-1) = avrg2
                 loop = 1
                 count = 0
              endif
           else
              count = 0
           endif
1000    continue
c23456789-123456789-123456789-123456789-123456789-123456789-123456789-12

c       Save max value of k_arr and j_arr
        k_max_arr=k_arr
        j_max_arr=j_arr
c---------------- END of FINDING start AND stop BINs -------------------

        if(ibegin.eq.length) then  !NO good data in whole ray. RETURN.
           print *, 'No good data in entire beam!'
c          Simply put "raw" values of Zh and Zdr in plfile(i,20,21)
           do i=1,length
c             Do only if rhovh>thrcrr and SNR>3 dB.
              if(plfile(i,5).gt.thrcrr.and.plfile(i,11).gt.5.) then
c             if(plfile(i,5).gt.thrcrr.and.plfile(i,11).gt.SNRslevel) then ! Yanting
                 plfile(i,20)=plfile(i,1)
                 plfile(i,21)=plfile(i,2)
              endif
           enddo
           go to 600
        endif

        if (kbegin.eq.length) then
           kbegin = ibegin
        endif
   
        ext = avrg2
        do i=1,89+kbegin   !Set the initial conditions
           z(kbegin-i) = init0
        enddo
        do i=1,n-iend+30   !Extend data record
           z(iend+i)=ext
        enddo
        do i=-90,length+30 !Adjust raw data array
           x(i)= z(i)
        enddo
 
        irl=0
c23456789-123456789-123456789-123456789-123456789-123456789-123456789-12
c------------- MAIN LOOP of Phidp Adaptive Filtering --------------------
        do 9999 mloop=1,madflt
c          TIE DOWN THE INITIAL and EXTENDING DATA RECORD
           do i=irl,kbegin+89
              z(kbegin-i)=init0
           enddo
           do i=1,n-iend+30
              z(iend+i)=ext
           enddo
c          FIR3 FILTER SECTION  ( change back from FIR1 12/8/92. )
           do i=-5,n+5
              acc=0.0
              do j=0,fir3order
                 acc=acc+fir3coef(j)*z(i-fir3order/2+j)
              enddo
              y(i)=acc*fir3gain
           enddo
c          END of FIR3 FILTERING
           do i=1,n
              delt=abs(x(i)-y(i))
              if (delt.ge.thres) then
                 z(i)=y(i)
              else
                 z(i)=x(i)
              endif
           enddo
9999    continue 
c*****************END LOOP for Phidp Adaptive Filtering****************************

c       PUT KDP,DELTA,PDPCORRECTED,PDPCORRECTED-FILTERED into PLFILE
        do 90 i=-5,n+3
           plfile(i,nad1)= z(i)
           plfile(i,nfl) = y(i) 
90      continue

c       CALCULATE KDP
102     delta_2r=plfile(3,0)-plfile(1,0)
        do 103 i=1,n
c          Set "good data" range mask based on new Zdr stored in plfile(i,16)
           if(plfile(i,16).gt.-100.) then
              igood_data(i)=1
              plfile(i,41)=1.
           else
              igood_data(i)=0
              plfile(i,41)=0.
           endif 


c          Check Zh range
c          default value for nadp is 10
           nadp=10
           if(i.gt.15.and.i.lt.n-15) then
              if(plfile(i,1).lt.35.) nadp=30
              if(plfile(i,1).ge.35.and.plfile(i,1).lt.45.)
     +                               nadp=20
              if(plfile(i,1).gt.45.) nadp=10
           endif
           do jj=0,nadp
              xx(jj+1)=plfile(i-nadp/2+jj,0)
              yy(jj+1)=plfile(i-nadp/2+jj,nfl)
           enddo

c          Improved Kdp base on LSE fit to Adap flt Phidp
           call LSE(aa,bb,xx,yy,nadp+1)
           plfile(i,nad2) = aa/2.
103     continue
c*******************END Kdp CALCULATION******************************************
c23456789-123456789-123456789-123456789-123456789-123456789-123456789-12

c Seek for the reasonable start point for attenuation correction AND
c Implement standard attenuation correction, i.e., using default alpha & beta

c j_max_arr refers to the good data region, no good data, then bypass (ytwang)
       if (j_max_arr.eq.0) then
          go to 600
       endif

c Checking for those instances where good data mask extends from last
c bad data segment through iend=length. In such a case make iend1=length!
       if ((j_max_arr-k_max_arr).eq.1) then
          k_max_arr = k_max_arr+1
          iend_arr(k_max_arr) = length
       endif
       iend=min0(iend,iend_arr(k_max_arr))
         

       cos_el2=cos(elevation*0.0174532)**2
       sin_el =sin(elevation*0.0174532)

c Note: By definition phidp_cons starts from zero.
c To avoid large errors in phdip_cons near the radar
c First find 10 consecutive gates where pdp_sd < thrs_phi  deg 
c This should define the beginning of "good" sector.
       ib=0
       jcount=0
       do 601  i=1,ibegin   ! x1
          pdp_sd=plfile(i,49)

          if(pdp_sd.lt.thrs_phi) then
             jcount=jcount+1
             if(jcount.eq.10)then
                do il=0,9
                   z_cns(i-il)=plfile(i-il,22)
                enddo
                ib=i-9

c Compute avg of phidp_cons in 4 good gates. This will be the
c initial value of the phidp used later in atten correction.
                anitial=(z_cns(ib)+z_cns(ib+1)+
     +          z_cns(ib+2)+z_cns(ib+3))/4.
                go to 602
             else
                go to 601
             endif
          else
             jcount=0
          endif
601    continue
602    continue

c do standard attenuation correction, based on that result (1st guess)
c computer HDR for accurate correction in the case of large phidp (ytwang)
       if (ib.gt.0) then
          ibasic = ib
       else
          ibasic = ibegin
          anitial=(plfile(ibegin,22)+plfile(ibegin+1,22)+
     +          plfile(ibegin+2,22)+plfile(ibegin+3,22))/4.
       endif
       call basic_zh_corr(length,cos_el2,ibasic,anitial,alpha)
       call basic_zdr_corr(length,cos_el2,ibasic,anitial,beta)

c Smoothing IIR5 raw Zh and Zdr for attenuation correction later
c Used to set the desired value of Zdr for Adp correction  Bringi 12/10/01
        do 105 i=1,length
           sumzh=0.0
           sumzv=0.0
           jcount=0

           do jj=0,10
              index = i-5+jj
c note: converting to Zh and Zv to linear scale
c yy contains linear Zh and zz contains linear Zv

              if(igood_data(index).eq.1) then
                 sumzh=sumzh+10.**(0.1*plfile(index,20))
                 sumzv=sumzv+10.**
     +           (0.1*(plfile(index,20)-plfile(index,21)))
                 jcount=jcount+1
              endif
           enddo

           if(sumzh.gt.0.0 .and. igood_data(i).eq.1) then
              zhmean_lin=sumzh/float(jcount)
              zvmean_lin=sumzv/float(jcount)
              zhmean_log =10.*log10(zhmean_lin)
              zdrmean_log =10.*log10(zhmean_lin/zvmean_lin)
           else
              zhmean_log = 0
              zdrmean_log = 0
           endif

c USe HDR to make sure of pure rain before setting zdr_avg
c Using rainline for oscillating drops, see Chap 7 of B & C book
c NOTE: HDR here is based on  Zh and Zdr values corrected
c       for attenuation in simple way. It is only a first estimate of HDR 

          if(zdrmean_log.le.0.0) then
             f_zdr = 27.
          elseif(zdrmean_log.le.1.67) then
             f_zdr=19.5*zdrmean_log+27
          else
             f_zdr=60.
          endif
          HDR=zhmean_log-f_zdr
          plfile(i,9)=HDR

 105   continue

c23456789-123456789-123456789-123456789-123456789-123456789-123456789-12
c____________________________________________________________
c Next part is copied and attached from Dr. Bringi's version

c ytwang modify the following part
c23456789-123456789-123456789-123456789-123456789-123456789-123456789-12
c*******BEGIN ADAPTIVE TESTUD MEHOD FOR ATTEN CORRECTION OF REFLECTIVITY********

c Note: delta_r is the actual range gate spacing!
       delta_r=plfile(2,0)-plfile(1,0)

       k_iter=j_max_arr
       k1_iter=1
       k2_iter=0

1001   continue
c working below for multicell storm by ytwang

      ibegin1 = max0(ibegin,ibegin_arr(k_iter))
      iend1 = min0(iend,iend_arr(k_iter))
      
      if ((iend1-ibegin1).lt.20) then !if(j_max_arr.eq.1) then
         ih_start = 0
      else  ! checking hail in this cell
               
c********Begin detection of hail using HDR>20 dB*******
c        For mgood consecutive good gates
         jcount_h=0
         ih_start=0
         do 603 i=ibegin1,iend1
            HDR=plfile(i,9)
            if(HDR.ge.20) then
               jcount_h=jcount_h+1
               if(jcount_h.eq.mgood)then
                  ih_start=i-mgood+1
                  go to 604
               else
                  go to 603
               endif
            else
               jcount_h=0
            endif
603      continue
604      continue
         if (ih_start.eq.0) go to 613 ! no hail in this cell
         
c********Find end of Hail region using HDR<10 dB for 
c        mbad      consecutive gates 
         jcount_h=0
         ih_end=0
         do 610 i=ih_start,iend1
            HDR=plfile(i,9)
            if(HDR.le.10) then
               jcount_h=jcount_h+1
               if(jcount_h.eq.mbad) then
                  ih_end=i-mbad+1
                  go to 611
               else
                  go to 610
               endif
            else
               jcount_h=0
            endif
610      continue
611      continue
         if (ih_end.eq.0) ih_end=iend1


c Find location index of min ZDr within the hail region
c and check if its negative, then use it as a reference value
c for Adp correction 
         test_zdr=plfile(ih_start,2)  ! upgraded by ytwang dec-19,01
         index_zdr=ih_start
         do i=ih_start+1,ih_end
            if(plfile(i,2).lt.test_zdr) then
               test_zdr=plfile(i,2)
               index_zdr=i
            endif
         enddo 

613      continue

      endif ! goes with if(delta_i>20) above     



c Compute delta_phi and meanzdr in last 10 good gates

       sum_final=0.0
       do 151 k=iend1-mgood+1,iend1
          sum_final=sum_final+plfile(k,32)
151    continue
       phidpfinal= sum_final/mgood

       sum_phidp=0.0
       do 152 k=ibegin,ibegin+mgood-1
          sum_phidp=sum_phidp+plfile(k,32)
152    continue
       phidpinit= sum_phidp/mgood

c delta_phi is the change in phidp across the beam from ibegin to iend
       delta_phi=phidpfinal-phidpinit


c23456789-123456789-123456789-123456789-123456789-123456789-123456789-12

c if delta_phi<25 deg at S-band  then don't do any correction by Testud Method
c Just do simple atten correction based on May's phidp_cons
c See after statement 600 at end of routine.This will take care of
c case where rain cell from 1:ibegin exists.

       if(delta_phi.le.25.) then
          print*,'delta_phi<25: standard correction acceptable'
          go to 600
       endif

c****************NOTE: Beyond this delta_phi > 25 deg!! 
c if the height at ibegin exceeds htU then don't do any correction
c using the Testud method as we don't know what atten/diff atten
c is caused by ice. The testud method is only valid in rain

       height_ibegin=corr_height(ibegin)
       if(height_ibegin.ge.htU)  then
          go to 600
       endif


c First do simple correction from 1:ibegin as there may be a cell here.
c Use phipd_consencus from May to do this. Stored in plfile(i,22)
c Note that phidp_cons is calculated in this routine (Huang implemented it)

       ! change ibegin1 to ibegin around here (ytwang)
       tempcorr=0.0
       tempfix=0.0
       temp_final_zh=0.0
       temp_final_zdr=0.0
       if(ib.lt.ibegin.and.ib.gt.0) then

          do i=ib,ibegin
             pdp_sd=plfile(i,49)


c Set Kdp from May's estimate in plfile(i,33)
c May's estimator is stored in plfile(i,6)

             if(plfile(i,20).ge.35.and.plfile(i,6).gt.0.
     +       and.pdp_sd.lt.thrs_phi) then
                plfile(i,33)=plfile(i,6)
             endif
          enddo  ! ib:ibegin1
c tempcorr is the change in phidp from ib to ibegin
c based on concensus phidp 

          tempcorr=plfile(ibegin,22)-anitial

       endif

c Store final atten/diff atten in dB for use later on.

       if(tempcorr.gt.0) then
          temp_final_zh=tempcorr*alpha/cos_el2
          temp_final_zdr=tempcorr*beta
       else
          temp_final_zh=0.0
          temp_final_zdr=0.0
       endif


c23456789-123456789-123456789-123456789-123456789-123456789-123456789-12
c Now start Testud Method
c Note 1:n means entire beam within which ibegin and iend represent the "good" data

       do i=1,n
          zh_lin(i)=10.**(0.1*plfile(i,1))
          phidp(i)=plfile(i,32)
          kdp_orig(i)=plfile(i,33)
          value(i)=0.0
       enddo

c "b_exp" is defined in Ah=a(No^(1-b))*Zh^b where b=b_exp.
c From 2D video analysis of Brazil convective rain R> 5 mm/h 
c Analysis of 2D video re-done on 8/31/00
c ***************************************************************************

       b_exp=0.741   

c***************************************************************************

c Initialize gamma-values where Ah=gamma*Kdp. It is same as alpha.
c Note that gamma is defined in the data statement
c Make elevation angle correction since Testud's equations are
c only valid for elev=0. Note that Ah is assumed to be indpendent
c of elevation angle, but Kdp depends on elevation angle.

       loop = malpha ! set to 20 to restore the MSE estimator (Yanting)
       if (loop .eq. 1) gamma(1) = gamma(mopt)

       do 320 j=1,loop
          temp(j)=10.**(0.1*b_exp*gamma(j)*delta_phi/cos_el2)
          temp(j)=temp(j)-1

          do 321 k=1,n
             zhlinn(k,j)=zh_lin(k)
             phidpn(k,j)=phidp(k)
321       continue
320    continue


       do 302 i=ibegin,iend1-1
          sum=0.0
          ifix=i
          do 303 j=ifix,iend1
             sum=sum+(zh_lin(j)**b_exp)*b_exp*0.46*delta_r
303       continue
          value(i)=sum ! sum|value::I(r;rM)
302    continue
       Ir1r0=value(ibegin) ! ibegin1 -> ibegin
       value(iend1)=0.0
c Force value(iend1)=0 by definition 3/20/01 Bringi
c*******************************************************************

c Set the initial value for valuen, atten and phidp_test
c Combine the loop here and the two next (ytwang)
       do k=1,loop
          do i=ibegin,iend1
             valuen(i,k)=value(i)
c Find Ah at each gate and for each gamma-value
             atten(i,k)=temp(k)*(zhlinn(i,k)**b_exp)
             atten(i,k)=atten(i,k)/(Ir1r0+temp(k)*valuen(i,k))
c For each gamma-value compute the cumulative phidp by integrating
c Kdp=Ah/gamma with range. Do a correction for elevation angle
c dependence of Kdp since Kdp=Ah/gamma is valid at elev=0.
             sum = 0.0
             do j=ibegin,i
                sum=sum+atten(j,k)*delta_r
             enddo
             phidp_test(i,k)=phidpinit+(cos_el2*sum*2./gamma(k))
          enddo
          phidp_test(ibegin,k)=phidpinit ! Reset the initial one
       enddo

c For each gamma-value, find abs(deviation) between the measured
c phidp and the phidp calculated from Ah at each gate along the beam
c then find sum of these errors for the entire beam.

       do 310 j=1,loop
          sumerr=0.0
          do 311 i=ibegin,iend1
             error(i,j)=abs(phidp_test(i,j)-phidpn(i,j))
             sumerr=sumerr+error(i,j)
311       continue
          sumerror(j)=sumerr
310    continue

c Find optimal gamma-value that gives minimum error. Not guaranteed
c that an optimal gamma can be found. If the minimum error can't be
c found in the gamma-interval (.002-.04) then select default value
c of 0.014 valid for Brazil dsds.

       jmin=1
       amintest=sumerror(1)
       do 312 j=1,loop
          if(sumerror(j).lt.amintest) then
             amintest=sumerror(j)
             jmin=j
          endif
312    continue

c check if first value of gamma is chosen as the optimal
c value. This usually means that a minimum was not found.
c Also check if last value of gamma was chosen. In either
c case set gamma_opt=.014.

c23456789-123456789-123456789-123456789-123456789-123456789-123456789-12

       if(jmin.eq.1.or.jmin.eq.loop) then

c make sure that further down if default value of
c gamma =.014 is used that the index in the atten arrays
c are set correctly !!

          gamma_opt=alpha
          tempx=10.**(.1*b_exp*gamma_opt*delta_phi) ! Similar with 'temp'
          temp_Nw=1-10.**(-.1*b_exp*gamma_opt*delta_phi)
       else
          gamma_opt=gamma(jmin)
          tempx=10.**(.1*b_exp*gamma_opt*delta_phi)
          temp_Nw=1-10.**(-.1*b_exp*gamma_opt*delta_phi)
       endif


c Store the optimum phipd calculated in plfile(i,25)

       do i=ibegin,iend1


          if((jmin.eq.1.or.jmin.eq.loop).and.loop.gt.1) then

c If gamma values in data statement are changed
c then make sure the default value of gamma=.014
c points to the correct index in phidp_test and atten arrays!!

             plfile(i,25)=phidp_test(i,8)
             plfile(i,27)=atten(i,8)
          else
             plfile(i,25)=phidp_test(i,jmin)
             plfile(i,27)=atten(i,jmin)
          endif
       enddo


c23456789-123456789-123456789-123456789-123456789-123456789-123456789-12

c Calculate corrected Zh at each gate
c Make sure that atten/diff atten from ib:ibegin-1 is added in.
       testfix=0.0
       test=0.0
       do 314 i=ibegin+1,iend1
          sum=0.0
          ifix=i
          do 315 k=ibegin,ifix
             if((jmin.eq.1.or.jmin.eq.loop).and.loop.gt.1) then
c*** If gamma values are changed in data statement
c*** then make sure default value of gamma=.014 points
c*** to correct index in atten array below!!
                sum=sum+atten(k,8)*delta_r
             else
                sum=sum+atten(k,jmin)*delta_r
             endif
315       continue

c Here, add in the attenuation from 1:ibegin previously
c stored in temp_final_zh. This is in dB, so is "sum" above.

          test=2.0*sum+temp_final_zh

c Do correction of Zh only if height of gate < htU where htU is set=6 km
c For gates located along the beam whose height is > 6 km the
c total attenuation is fixed by testfix below.

          height=corr_height(i)
          if(abs(height-htU).le.0.1) testfix=test
          if(height.le.htU) then
             plfile(i,20)=plfile(i,1)+test !10.*alog10(zh_lin(i)*test)
          else
             plfile(i,20)=plfile(i,1)+testfix !10.*alog10(zh_lin(i)*testfix)
          endif
314    continue

c Store the final dB values for later use
       test_dB=test
       testfix_dB=testfix


c Complete the attenuation correction for gates from iend1+1 to n.
c Note that phidpfinal is the "initial" value of phidp at iend1!
c Use standard atten correction method here using filtered phidp
c stored in plfile(i,32).
c Make sure that test1 and test_dB are added in. Then add in
c any additional correction from iend1 to end of beam.
       height_end1=corr_height(iend1+1)
       temp_end=0.0
       tempx_end=0.0

       do i=iend1+1,n
          height=corr_height(i)
          if(abs(height-htU).le.0.1) tempx_end=
     +    plfile(i,32)-phidpfinal
          temp_end=plfile(i,32)-phidpfinal

          if (height_end1.le.htU.and.height.le.htU) then
             plfile(i,20)=plfile(i,1)+test_dB
             if(temp_end.gt.0.) then
                plfile(i,20)=plfile(i,20)+temp_end*gamma_opt/cos_el2
             endif
          elseif(height.gt.htU.and.height_end1.le.htU) then
             plfile(i,20)=plfile(i,1)+test_dB
             if(tempx_end.gt.0.) then
                plfile(i,20)=plfile(i,20)+tempx_end*gamma_opt/cos_el2
             endif
          else
             plfile(i,20)=plfile(i,1)+testfix_dB
          endif
       enddo
c (ytwang) The ZH correction is completed here.

c Compute meanzh in last mgood "good" gates so that
c the desired value of Zdr there can be estimated.
c Compute mean HDR in last mgood gates but note that
c HDR is first guess since Zdr is corrected by simple method

1002   continue   ! ytwang checking the values in hail

       sum_zh=0.0
       sum_zv=0.0 
c using smoothed ZH and Zdr fields here, Bringi 12/10/01
       do k=iend1-mgood+1,iend1
          sum_zh=sum_zh+10.**(0.1*plfile(k,1))
          sum_zv=sum_zv+10.**(0.1*(plfile(k,1)-plfile(k,2)))
       enddo
       meanzdr=10.*alog10(sum_zh/sum_zv)

       sum_zh_corr=0.0
       sum_HDR=0.0
       do k=iend1-mgood+1,iend1
          sum_zh_corr=sum_zh_corr+10.**(0.1*plfile(k,20))
          sum_HDR=sum_HDR+10.**(0.1*plfile(k,9))
       enddo
       meanzh_corr=10.*alog10(sum_zh_corr/mgood)
       mean_HDR=10.*alog10(sum_HDR/mgood)

c find height agl of the gate where meanzh_corr is calculated
       height_meanzh=corr_height(iend1-mgood/2)

c Set up the desired  zdr_avg value in rain based upon Zh.
       if(meanzh_corr.le.20.) then
          zdr_avg=0.0

      elseif(meanzh_corr.gt.20.and.meanzh_corr.le.35.) then   
          zdr_avg=(meanzh_corr**1.6)*10.**(-2.69)

       elseif(meanzh_corr.gt.35.and.meanzh_corr.le.50.) then
          zdr_avg=(meanzh_corr**3.1)*10.**(-4.86)
c Adjustment made to "follow" lower bound of Zdr for a given Zh
          zdr_avg=zdr_avg-0.25

       else
c added to take care if meanzh_corr>50 dBZ, put ceiling at 2.3 dB
c Bringi 12/7/01
          zdr_avg=2.3
 
       endif

c if the above height exceeds the height of the freezing level
c then avg zdr there is forced to 0. This is assumed to be the
c same as the height above which no attenuation correction is done.
c Also, check if mean_HDR>5 dB, as this would signify hail region
       if(height_meanzh.ge.htL.or.mean_HDR.ge.5) then
          zdr_avg=0.0
       endif


c CHECK IF meanzdr is > desired Zdr!!! 
c Cannot us Ah-Av to reduce the raw Zdr , it can only increase raw Zdr
c and if meanZdr is already > desired Zdr then can't use this method.
c Decide to use default or simple method with no constraints!!

c simply go to 600 where the simple method is used,          
c Ah-Av=beta*delta_phi
c Inserted on 12/7/01, bringi
c NOTE: k_max_arr gives the number of "good" data segments

c next block was upgraded by ytwang
        if(meanzdr.gt.zdr_avg.and.delta_phi.ge.50.and
     +     .j_max_arr.ge.2) then ! should be j_ (means good data start)

           if (k1_iter.ne.k2_iter .and. ih_start.ne.0) then ! we have not check the hail
              iend1 = index_zdr+5
              k2_iter = k2_iter+1
              go to 1002
           elseif (k_iter.gt.1) then ! even hail the condition not met
              k_iter = k_iter-1
              k2_iter = k1_iter
              k1_iter = k1_iter+1
              go to 1001
           else  ! have already reached to first cell
              go to 600
           endif
        elseif(meanzdr.lt.zdr_avg)then
           go to 902 
        else
           go to 600
        endif   ! goes with if(meanzdr.gt....) above


c23456789-123456789-123456789-123456789-123456789-123456789-123456789-12

c*******************************************************************
c Now start correction for Zdr.

902    continue


c where Ah-Av=const_zdr*Ah , and beta is defined as Ah-Av=beta*Kdp


       const_zdr=1

       do 316 i=ibegin,iend1  ! ibegin1 -> ibegin
          height=corr_height(i)
          if(height.le.htL) then
 
c Normally set htL=4 km which is near the melting level
c for the tropics, byt htL=1.5 km for Colorado
c Also, an adjustment is made of elevation angles > 0 to
c the relation Ah-Av=Ah*const_zdr as the const_zdr is strictly
c valid only for elev angle=0.0. The adjustment is valid for
c Rayleigh scattering.
             AhAv_Sm(i)=plfile(i,27)*const_zdr*cos_el2
             plfile(i,38)=AhAv_Sm(i) ! Adp

          elseif(height.gt.htL.and.height.le.htU) then

c A linearly decreasing weight is applied in the melting layer
c otherwise diff atten in bright band may be overestimated.
             AhAv_Sm(i)=plfile(i,27)*const_zdr*((htU-height)/
     +       (htU-htL))*cos_el2
             plfile(i,38)=AhAv_Sm(i)

c It is better to use the Smythe/Illin method between htL and htU
c in the melting layer. Note that we use weightcoeff=const_zdr*
c gamma_opt here.
             if(kdp_orig(i).ge.0.5)then
                AhAv_Sm(i)=const_zdr*gamma_opt*kdp_orig(i)
                plfile(i,38)=AhAv_Sm(i)
             endif

          else
             AhAv_Sm(i)=0.0
             plfile(i,38)=AhAv_Sm(i)
          endif
316    continue

       testfix1=0.0
       test1=0.0

       do 318 i=ibegin+1,iend1
          sum=0.0
          ifix=i
          do 319 k=ibegin,ifix
             sum=sum+AhAv_Sm(k)*delta_r
319       continue

c Add in the total diff atten from 1:ibegin-1 stored
c in temp_final_zdr which is in dB, as is "sum" above.
          test1=2.*sum+temp_final_zdr

c Store corrrected Zdr in plfile(i,21). Raw zdr is is plfile(i,2).
          height=corr_height(i)
          if(abs(height-htU).le.0.1) testfix1=test1
          if(height.le.htU) then
             plfile(i,21)=plfile(i,2)+test1
             if(plfile(i,16).gt.-100.)plfile(i,16)=plfile(i,21)
          else
             plfile(i,21)=plfile(i,2)+testfix1
             if(plfile(i,16).gt.-100.)plfile(i,16)=plfile(i,21)
          endif

318    continue

c23456789-123456789-123456789-123456789-123456789-123456789-123456789-12


c Compute meanzdr in last 10 "good" gates to see if
c desired value in zdr_avg was achieved.

       sum_zh=0.0
       sum_zv=0.0
       do 154 k=iend1-9,iend1
          sum_zh=sum_zh+10.**(0.1*plfile(k,20))
          sum_zv=sum_zv+10.**(0.1*(plfile(k,20)-plfile(k,21)))
154    continue
       meanzdr_corr=10.*alog10(sum_zh/sum_zv)


c Iterate the Zdr correction so that the meanzdr_corr tends to
c zdr_avg which is the desired value . This was necessary since the
c calculated Ah-Av above tends to "over correct" the raw zdr esp
c Do iteration only if delta_phi> 50 deg otherwise
c not worth the effort.


c This is for iterative Testud's method of deriving Ah-Av
       kcount_zdr=0
       kcount_zdr1=0
       weight_adj=weightcoeff

901    test_zdr=meanzdr_corr-zdr_avg

c The following takes care of "overshooting" the desired  zdr
c*****************IF, THEN, ELSEIF,ELSE, ENDIF*********
       if(test_zdr.gt.epslon_over) then
          kcount_zdr=kcount_zdr+1

          const_adj=(zdr_avg-meanzdr)/(meanzdr_corr-meanzdr)
          const_zdr=const_zdr*const_adj

c this code takes care of "undershooting" the desired zdr
       elseif(test_zdr.lt.epslon_under) then
          kcount_zdr1=kcount_zdr1+1

          const_adj=(zdr_avg-meanzdr)/(meanzdr_corr-meanzdr)
          const_zdr=const_zdr*const_adj

       else
          go to 700
       endif
c***************************************************

c23456789-123456789-123456789-123456789-123456789-123456789-123456789-12

cTHIS IS  A LONG IF, THEN,ELSE,ENDIF  sectiom marked out 
c**************************************************
       if(kcount_zdr.gt.19.or.kcount_zdr1.gt.19) then
          go to 700
       else

          do i=ibegin,iend1
             height=corr_height(i)
             AhAv_Sm(i)=plfile(i,38)*const_adj ! plfile(i,27)*const_adj
             plfile(i,38)=AhAv_Sm(i)
          enddo

          testfix1=0.0
          test1=0.0
          do 418 i=ibegin+1,iend1
             sum=0.0
             ifix=i
             do 419 k=ibegin,ifix
                sum=sum+AhAv_Sm(k)*delta_r
419          continue

c add in any diff atten from 1:ibegin-1 computed earlier
             test1=2.*sum+temp_final_zdr
             
             height=corr_height(i)
             if(abs(height-htU).le.0.1) testfix1=test1
             if(height.le.htU) then
                plfile(i,21)=plfile(i,2)+test1
                if(plfile(i,16).gt.-100.) plfile(i,16)=plfile(i,21)
             else
                plfile(i,21)=plfile(i,2)+testfix1
                if(plfile(i,16).gt.-100.) plfile(i,16)=plfile(i,21)
             endif
418       continue

c Find meanzdr_corr in last 10 "good" gates.
          sum_zh=0.0
          sum_zv=0.0
          do k=iend1-9,iend1
             sum_zh=sum_zh+10.**(0.1*plfile(k,20))
             sum_zv=sum_zv+10.**(0.1*(plfile(k,20)-plfile(k,21)))
          enddo
          meanzdr_corr=10.*alog10(sum_zh/sum_zv)




           if (const_zdr.le.0.125 .or. const_zdr.gt.1.5) then
              print *,'WARNING: const_zdr out of range! <0.125, 1.5>'
           endif

             go to 901

       endif
c*****************************************************************


700    continue ! Complete correction of  Zdr only  for gates from iend1+1:n

          height_end1=corr_height(iend1+1)
          temp_end=0.0
          tempx_end=0.0

          do i=iend1+1,n
             height=corr_height(i)
             if(abs(height-htU).le.0.1)
     +       tempx_end=plfile(i,32)-phidpfinal
             temp_end=plfile(i,32)-phidpfinal

             if (height_end1.le.htU.and.height.le.htU) then
                plfile(i,21)=plfile(i,2)+test1
                if(plfile(i,16).gt.-100.)plfile(i,16)=plfile(i,21)

                if(temp_end.gt.0.) then
                   plfile(i,21)=plfile(i,21)+temp_end*beta
                   if(plfile(i,16).gt.-100.)plfile(i,16)=plfile(i,21)
                endif

             elseif(height.gt.htU.and.height_end1.le.htU) then
                plfile(i,21)=plfile(i,2)+test1
                if(plfile(i,16).gt.-100.)plfile(i,16)=plfile(i,21)

                if(tempx_end.gt.0.) then
                   plfile(i,21)=plfile(i,21)+tempx_end*beta
                   if(plfile(i,16).gt.-100.)plfile(i,16)=plfile(i,21)
                endif

             else
                plfile(i,21)=plfile(i,2)+testfix1
                if(plfile(i,16).gt.-100.)plfile(i,16)=plfile(i,21)
             endif
          enddo


c23456789-123456789-123456789-123456789-123456789-123456789-123456789-12

600    continue

c Call rainrate_beta here (at this point the Zh and Zdr 
c are corrected by the standard method (not Testud's)
       call rainrate_beta(length,iflag_loc,igood_data,ibegin_arr)
       return

       end
c end  kdpchill
c_____________________________________________________________



c ------------------------------------------------------------
c Subroutine: simple zh correction
c Changes: seperate subroutine for zh and zdr correction
c         
       subroutine basic_zh_corr(ngates,cos_el2,ib,anitial,alpha)
       real cos_el2,anitial,alpha
       integer ngates,ib
       common /LOC/ htL,htU
       include 'csu_driver.h'

       tempcorr=0.0
       tempfix=0.0
       temp_final_zh=0.0


       if(ib.gt.0) then
          do i=ib,ngates
             height=corr_height(i)

             if(abs(height-htU).le.0.1) then
                tempfix=plfile(i,22)-anitial
             endif
             tempcorr=plfile(i,22)-anitial

             if (tempcorr.gt.0.and.height.le.htU) then
                plfile(i,20)=plfile(i,1)+tempcorr*alpha/cos_el2
c                plfile(i,27) = alpha*plfile(i,6)  ! under test (no need later)
             elseif(tempfix.gt.0.and.height.gt.htU) then
                plfile(i,20)=plfile(i,1)+tempfix*alpha/cos_el2
             else
                plfile(i,20)=plfile(i,1)
             endif
          enddo
c      else  ! same story: no information about i here
c         plfile(i,20)=plfile(i,1)
       endif
       
       return
       end ! basic_zh_corr

c ------------------------------------------------------------
c Subroutine: simple zdr correction
c Changes: seperate subroutine for zh and zdr correction
c       
       subroutine basic_zdr_corr(ngates,cos_el2,ib,anitial,beta)
       real cos_el2,anitial,beta
       integer ngates,ib
       real conskdp
       common /LOC/ htL,htU
       include 'csu_driver.h'

       tempcorr=0.0
       tempfix=0.0
       temp_final_zdr=0.0

       if(ib.gt.0) then
          do i=ib,ngates
             height=corr_height(i)

             if(abs(height-htU).le.0.1) then
                tempfix=plfile(i,22)-anitial
             endif
             tempcorr=plfile(i,22)-anitial
  
             if (tempcorr.gt.0.and.height.le.htU) then
                plfile(i,21)=plfile(i,2)+tempcorr*beta
                if(plfile(i,16).gt.-100.)
     +          plfile(i,16)=plfile(i,2)+tempcorr*beta
                conskdp = plfile(i,6)
                if(conskdp.gt.0) plfile(i,38) = beta*conskdp ! for ldr correction
             elseif(tempfix.gt.0.and.height.gt.htU) then
                plfile(i,21)=plfile(i,2)+tempfix*beta
                if(plfile(i,16).gt.-100.)
     +          plfile(i,16)=plfile(i,2)+tempfix*beta
             else
                plfile(i,21)=plfile(i,2)
                if(plfile(i,16).gt.-100.)plfile(i,16)=plfile(i,2)
             endif
          enddo
c      else
c         plfile(i,21)=plfile(i,2) ! same story: no information about i
c         if(plfile(i,16).gt.-100.)plfile(i,16)=plfile(i,2) 
       endif
       
       return
       end ! basic_zdr_corr

c_____________________________________________________________



      subroutine LSE(a,b,x,y,n)

cccc    This is a Linear Least Square Estimate subroutine to fit a linear
cccc    equation for (xi,yi) (i=1,...,n), so that
cccc                            yi = a * xi + b
cccc    INPUTs: x(i), y(i), n, (i=1,...,n ).
cccc    OUTPUTs: a ( slope ), b ( intercept ).
cccc                                                Li Liu   Sep. 23, 92

      real x(500),y(500),a,b
      real xsum,ysum,xxsum,xysum,det

      xsum = 0.
      ysum = 0.
      xxsum = 0.
      xysum = 0.
      total = float(n)
      do 10 i = 1,n
      if (x(i).gt.1.e35.or.y(i).gt.1.e35) then
         total = total-1.
      else
         xsum =  xsum + x(i)
         ysum =  ysum + y(i)
         xxsum = xxsum + x(i)*x(i)
         xysum = xysum + x(i)*y(i)
      endif
10    continue
      det = total * xxsum - xsum**2
      a = ( total*xysum - xsum*ysum ) / det
      b = ( ysum*xxsum - xsum*xysum ) / det
        return
      end
c-----------------------------------------------------------------

      subroutine msr(ymn,sd,y,n)

cccc  To calculate the mean (ymn) and standard deviation (sd, or,
cccc  mean square root, msr) of the array y(i) (i=1,...,n).
cccc                                               Li Liu  Sep. 19, 95

      real y(500),ymn,sd

      ysum  = 0.
      yysum = 0.
      total = float(n)
      do 10 i = 1,n
      if (abs(y(i)).gt.1.e35) then
         total = total-1.
      else
         ysum =  ysum + y(i)
      endif
10      continue
      ymn = ysum/total

      do 20 i = 1,n
      if (abs(y(i)).lt.1.e35) then
         yysum =  yysum + (y(i)-ymn)**2
      endif
20      continue
      sd = sqrt(yysum/total)

        return
      end
c_____________________________________________________________


        subroutine ADflt(fltname,inp,outp)

cccc    This is an IIR5 filtering routine .
        include 'csu_driver.h'

        real x(-90:ndim),y(-90:ndim)
        real denc(0:50),numc(0:50)
        integer adorder,addelay,inp,outp
        character*4 fltname

cccc    SET FILTER COEF. ONLY IIR5 HERE!!!
        adorder=3
        adgain=5.947559112e-2
        addelay=2
        data numc/1.,3.,3.,1.,47*0./
        data denc/1.,-1.022829379,6.133352363e-1,-1.147011281e-1,47*0./

cccc    PUT Dfr DATA INTO A SCATCH ARRAY
        do 10 i=1,length
           x(i)=plfile(i,inp)
10      continue

cccc    DO THE FILTERING

cccc    SET THE INITIAL CONDITIONS
40      iinit=(x(1)+x(2)+x(3))/3.
        do 50 i=1,5*adorder+1
            x(1-i)=iinit
            y(1-i)=iinit
50      continue
cccc    EXTEND DATA RECORD
        ext=(x(length)+x(length-1)+x(length-2))/3
        m=adorder*5
        do 100 i=1,m+2
            x(length+i)=ext
100     continue

cccc    MAIN FILTERING ROUTINE

        do 60 i=-5*adorder,length+m/2
             acc=0.0
             acc1=0.0
cccc         DENOMINATOR POLYNOMIAL
             do 70 k=1,adorder
                  acc=acc+(-denc(k))*y(i-k)
70           continue
cccc         NUMERATOR POLYNOMIAL
             do 80 k=1,adorder+1
                  acc1=acc1+numc(k-1)*x(i+1-k)
80           continue
             y(i)=acc+acc1*adgain
60      continue

        if(adorder.gt.3) m=m+1

        do 90 i=1,length
           plfile(i,outp)=y(i+addelay)
90      continue

        return
        end

c end of ADflt
c_______________________________________________________________________


