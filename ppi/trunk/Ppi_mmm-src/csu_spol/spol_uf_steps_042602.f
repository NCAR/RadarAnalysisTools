      PROGRAM ufkdp

c     This program reads in Universal Format (UF) polarimetric radar data
c     for the purpose of calculating the specific differential phase, Kdp.
c     First, the differential phase is analyzed for blocks of good data.
c     Bad or missing phidp data that is in/around the good phidp is then
c     filled/padded for the purpose of running a 21 pt filter.
c     The differential phase is then filtered to remove the effects of
c     noise and to separate the backscatter differential phase from the
c     propagation differential phase.  This beta version is testing various
c     filters such as running mean filter and the finite impulse response
c     FIR filter from Hubbert and Bringi (1995).  The filtered differential
c     phase (i.e., the propagation phase) is then used to estimate Kdp via
c     a finite difference approximation.

c     Lawrence D. Carey
c     Department of Atmospheric Science
c     Colorado State University
c     Fort Collins, CO 80523-1371
c     carey@olympic.atmos.colostate.edu
c     (970) 491-6944
c
c     UF READ/WRITE sub-routine library (libufroutines.a) by Paul Hein of CSU
c
c     TO COMPILE:
c     f77 spol_uf_kdp.f /usr/local/lib/libufroutines.a -o spol_uf_kdp.e


c     12/96: Initial BETA VERSION (C-band - C-pol data from MCTEX)
c      6/97: Adapted for S-BAND (11cm) (e.g., CSU-CHILL)
c     10/98: Corrected to insure that differential phase is extended into
c            low reflectivity regions with phase too noisy for Kdp calculation.
c            This is important for attenuation correction.  This also makes
c            Kdp field smoother by replacing bad data values with zero where Zh
c            is not bad data.
c     06/99: Adapted for S-POL. Added subroutine to calculate standard 
c            deviation of differential phase.  This can be used in addition to
c            rhoHV to determine good data for filtering and Kdp calculation
c            (e.g., Ryzhkov and Zrnic, 1998).  I no longer use a hard Zh
c            threshold for finding good/bad data.  Instead, I use a Zh boundary
c            to define two pairs of rhoHV and SD(phidp) threshold.  One pair
c            for 'high' reflectivity and one pair for 'low' reflectivity. The
c            low reflectivity thresholds are more restrictive.  Also, I no
c            longer output all original fields since this becomes size
c            prohibitive.  All non-phase data are clutter/noise thresholded
c            using rhoHV and SD(phidp) (similar to phase data).  Program now
c            has adjustable parameters to remove any biases in Zdr,LDR,or Zh.
c     11/99  1) Fixed bug in good_bad and fill_pad that allowed bdpts-1 # of
c            gates in good data zones to go unfilled (i.e., data that did not
c            meet thresholds were not removed and filled).
c            2) Added code to dealias differential phase data in S-POL.
c            3) Added new phase thresholding which is variable with Zh.  This
c            insures very high confidence in any calculated Kdp.  Commented
c            out of good_bad routine but utilized in finding noisy Kdp around
c            the edges of cores.
c            4) Only output UF if R<=100 km and H <= 20 km  above radar level.
c            5) Utilize Zdr as a function of Zh to identify any clear air
c            (insect) signal that passed through the rhoHV and stddev(phidp)
c            thresholds.
c            6) Correct Zh, Zdr for propagation effects using empirical phidp
c            method.
c     06/00  1) Added code to set Zdr to bad data if in partially blocked zone.
c            2) Added code to correct Zh for partial blocking.
c   
c     05/01  1) Modifications for use with STEPS SPOL data 
c		a) comment out partial blockage correction
c		b) return attenuation correction coefficients to Bringi et al 90
c		c) commented out system phase offset correction...need to 
c                  determine system phase for STEPS (if any)
c               d) fixed file name structure 
c     04/02  1) Removed LBA enhancements for getting rid of bad data
c		a) removed Zh-Zdr differentiation between precip and insects
c		b) removed code to remove partial beam filling phase problems

c     NOTE:  MUST CHANGE parameter statements in subroutines to
c     match the ones in the main program.

c     adjustable parameters for determining location of good data
c     blocks. ** NOTE: ngd=2*(gdpts-1) **

      integer gdpts,bdpts,ngd
      parameter(gdpts=7,ngd=12,bdpts=2)

c Set array size large enough
      parameter (NVAR=20, NFMAX=48, NGATE=1200, NTESTP=1018)

c Adjustable thresholds for finding good data and filtering
      real*4 rhthrsh
      real*4 rhnth,dpsdnth
      integer*2 ftype
      real*4 zdrbias,ldrbias,zhbias

c     rhthrsh: threshold for finding good phase data
c     ftype:  filter type (1-4)
c             1. 21-pt FIR filter - Hubbert and Bringi (1995)
c             2. 21-pt running mean filter (3.15 km)
c             3. 17-pt running mean filter (2.55 km)
c             4. 13-pt running mean filter (1.95 km)
c     rhnth:  rh threshold for removing clutter/noise from non-phase data
c     dpsdnth: DP STD DEV threshold for removing clutter/noise from 
c              non-phase data and also for finding good phase data
c     zdrbias:  bias in Zdr (dB) data (if any)
c     ldrbias:  bias in LDR (dB) data (if any)
c     zhbias:   bias in Zh (dB) data (if any)

      parameter (rhthrsh=.7,ftype=1,rhnth=0.6,dpsdnth=15.,
     +zdrbias=0.000,ldrbias=0.000,zhbias=0.0)

c     constants for height/range calculation
      real*4 a,pi,ke
      parameter(a=6.370E3,pi=3.1415927,ke=1.3333333)

      integer*2 mhead(45),fhead(NFMAX),lenfh
      integer*2 nfields,krec
      integer*4 iin,iout,ifld
      real*4 fdata(NGATE)
      character*2 fname(NVAR)
      character*80 infile, outfile

      real*4 dpnew(NGATE),dp(NGATE),kdnew(NGATE),dz(NGATE),rh(NGATE)
      real*4 denew(NGATE),dpsd(NGATE),vr(NGATE),dm(NGATE)
      real*4 dr(NGATE),ld(NGATE)
      integer*2 hdpnew(NFMAX),lenfhn
      integer*2 hrh(NFMAX),hdz(NFMAX),hdr(NFMAX),hld(NFMAX),hvr(NFMAX)
      integer*2 lenrh,lendz,lendr,lenld,lenvr
      integer*2 i, istat
      character*8 generat
      integer*2 numgd,gdstrt(50),gdend(50)
      real*4 fildat(NGATE+20),fildp(NGATE),sdpsd,cdpsd
      real*4 dzdat(NGATE+20),drdat(NGATE+20),kddat(NGATE+20)
      real*4 fildz(NGATE),fildr(NGATE),filkd(NGATE)
      real*4 r,h,theta
      real*4 th1,th2,sigphdp(NGATE)
      logical drflag,kdgflag,pass1,kdbflag(NGATE)
      real*4 dplo,dphi
      integer*2 kntdp,idp,jdp,nmgt
      
      open(unit=18,file='uffiles',status='unknown')

c Initialize - single uf
5     krec = 0
      numgd=0
      sdpsd=0.
      cdpsd=0.

      read(18,*,end=999,err=888) infile
c      outfile=infile(1:18)//'ppuf'
      outfile=infile(1:19)//'ppuf'
 
c-Open uf file to read
      print *, 'Opening ',infile
      call ufopen(infile, iin)
c+Open uf file to write
      call ufcreate(outfile, iout)

c Read UF rays

 10   continue
         krec = krec + 1

c-Read in ray headers
         call ufreadrayhdr(iin,mhead,nfields,fname,istat)
         if (istat.ne.1) then
            write(6,*) 'Finishing at ray # ',krec
            print *,istat,"<--Status Sweep-->",mhead(10)
            print *, 'End of File or Error while reading UF file header'
            goto 250
         endif

         if(MOD(krec,100).eq.0.or.krec.eq.1)then
           write(6,*) 'Processing ray # ',krec
           write(6,1011) mhead(26)-100,mhead(27),mhead(28),mhead(29),
     +                   mhead(30),mhead(31)
1011       format( i2,'/',i2,'/',i2,3x,i2,':',i2,':',i2)
           write(6,*)
         endif

c-One MUST read all the fields and they will be read in order 
         do 34 ifld = 1,nfields
c-Read in a data field header and field data
            call ufreadrayfld(iin,lenfh,fhead,fdata,istat)
            if (istat.ne.1) then
              print *,istat,"<--Status Sweep-->",mhead(10)
              print *,'Error or End of File while reading UF file field'
              goto 250
            endif

c     A test pulse in the S-pol data exists in every sweep from gate 1019-1040.
c     The main program will remove the test pulse from all output and processed
c     variables.

c     horizontal reflectivity
            if (fname(ifld).eq.'DZ') then
c     header
               lendz=lenfh
               do 12 i = 1, lendz
12               hdz(i) = fhead(i)
c     data
               do 14 i = 1, fhead(6)
c                remove the test pulse from Zh which runs from gate 1019 to
c                gate 1040 in S-pol data
                 if(i.le.NTESTP)then
c                  correct for gaseous attenuation Battan (1973) p. 66-67
                   if(fdata(i).ne.mhead(45))then
                     dz(i)=fdata(i) + 0.014*( fhead(3)+fhead(4)/1000.+
     +                     (i-1)*fhead(5)/1000.) - zhbias
                   else
                     dz(i)=fdata(i)
                   endif
                 else
                   dz(i)=mhead(45)
                 endif
14             continue
            endif

c     correlation coefficient
            if (fname(ifld).eq.'RH') then
c     header
               lenrh=lenfh 
               do 16 i = 1, lenrh
16               hrh(i) = fhead(i)
c     data
               do 18 i = 1, fhead(6)
c                remove test pulse
                 if(i.le.NTESTP)then
                   rh(i)=fdata(i)
                 else
                   rh(i)=mhead(45)
                 endif
18             continue
            endif

c     differential reflectivity
            if (fname(ifld).eq.'ZD') then
c     header
               lendr=lenfh
               do 19 i = 1, lendr
19               hdr(i) = fhead(i)
c     data
               do 20 i = 1, fhead(6)
c                remove test pulse
                 if(i.le.NTESTP)then
c                  remove any bias in Zdr
                   if(fdata(i).ne.mhead(45))then
                     dr(i)=fdata(i)-zdrbias
                   else
                     dr(i)=fdata(i)
                   endif
                 else
                   dr(i)=mhead(45)
                 endif
20             continue
            endif

c     linear depolarization ratio
            if (fname(ifld).eq.'LD') then
c     header
               lenld=lenfh
               do 21 i = 1, lenld
21               hld(i) = fhead(i)
c     data
               do 22 i = 1, fhead(6)
c                remove test pulse
                 if(i.le.NTESTP)then
c                  remove any bias in LDR
                   if(fdata(i).ne.mhead(45))then
                     ld(i)=fdata(i)-ldrbias
                   else
                     ld(i)=fdata(i)
                   endif
                 else
                   ld(i)=mhead(45)
                 endif
22             continue
            endif

c     radial velocity
            if (fname(ifld).eq.'VE') then
c     header
               lenvr=lenfh
               do 23 i = 1, lenvr
23               hvr(i) = fhead(i)
c     data
               do 24 i = 1, fhead(6)
c                remove test pulse
                 if(i.le.NTESTP)then
                   vr(i)=fdata(i)
                 else
                   vr(i)=mhead(45)
                 endif
24             continue
            endif

c     measured total differential phase
            if (fname(ifld).eq.'PH') then
               lenfhn = lenfh
c     header
               do 30 i = 1, lenfhn
30               hdpnew(i) = fhead(i)
c     data
               do 32 i = 1, hdpnew(6)
c                remove test pulse
                 if(i.le.NTESTP)then
                   dp(i)=fdata(i)
                 else
                   dp(i)=mhead(45)
                 endif
32            continue
            endif

c     power (dBm)
            if (fname(ifld).eq.'DM') then
c     data
               do 33 i = 1, fhead(6)
c                remove test pulse
                 if(i.le.NTESTP)then
                   dm(i)=fdata(i)
                 else
                   dm(i)=mhead(45)
                 endif
33             continue
            endif

34       continue

c-Read in last 4 bytes of the ray
         call ufreadrayend(iin,istat)
         if (istat.ne.1) then
           print *,istat,"<--Status Sweep-->",mhead(10)
           print *,'Error or End of File while reading UF file ray end'
           goto 250
         endif

c        set standard deviation of differential phase to zero in order to
c        take a first try at finding good/bad phase using rhoHV alone for
c        the purpose of unfolding the phase data (S-POL phase folds at 
c        +160 deg and wraps around to -20 deg for a total interval of 180deg).

         do 37 i=1,hdpnew(6)
           dpsd(i)=0.0
37       continue

c        Find good/bad phase data using rhoHV alone.  This is a 1st rough
c        guess/pass of where good phase data is located.  These good data 
c        blocks will be used to unfold phase if necessary.

         pass1=.true.
         call good_bad(dp,hdpnew,mhead,dz,rh,dpsd,rhthrsh,
     +   dpsdnth,gdstrt,gdend,numgd,pass1)
         pass1=.false.

      
c        Check for possible presence of fold by looking for incremental 
c        increase in PHIdp along ray to 140 deg where rhoHV is good.
         dplo=10.
         dphi=20.
         kntdp=0
         do 40 j=1,numgd
           do 39 i=gdstrt(j),gdend(j)

             if(rh(i).ge.rhthrsh.and.dp(i).gt.dplo.and.
     +       dp(i).le.dphi.and.kntdp.lt.13)then
               dplo=dplo+10.
               dphi=dphi+10.
               kntdp=kntdp+1
               jdp=j
               idp=i    
             endif

39         continue
40       continue 
        

c        unfold differential phase data.  S-POL data folds at 160 deg to
c        -20 deg (unambiguous range of 180).  This routine only corrects
c        single folded phase data for a maximum corrected unambiguous phase
c        of 340 deg.  This should be sufficient over typical maximum ranges 
c        utilized for this program (100 - 150 km).  Only call this subroutine
c        if the maximum phase in areas of high rhoHV >= 145 deg.

         if(kntdp.eq.13)then
           call unfold(idp,jdp,dp,hdpnew,gdstrt,gdend,numgd,mhead,
     +     rh,krec,rhthrsh,dz,dpnew)
c           write(95,*)
         else
           do 44 i=1,hdpnew(6)
             dpnew(i)=dp(i)
44         continue
         endif


c        calculate standard deviation of the differential phase at each
c        gate in a ray

         call dp_stddev(dpnew,hdpnew,mhead,rh,rhthrsh,dz,dpsd)


c        find good data to filter
         call good_bad(dpnew,hdpnew,mhead,dz,rh,dpsd,rhthrsh,
     +   dpsdnth,gdstrt,gdend,numgd,pass1)


c        set thresholds for lse slopes of fill/pad

c        for phidp: FILL
c        NOTE:  assumes 150m gate spacing 10.7 cm wavelength and Balakrishnan
c        and Zrnic (1985) or Doviak and Zrnic (1993) R=39.72*Kdp^0.866
c        if implied rain rate from slope is in excess of
c        100 mm/h (S-band), then do not use slope. Instead
c        continue prior point (zero slope).
         th1=0.871

c        for phidp: PAD
c        if implied rain rate from slope is in excess of
c        10 mm/h (S-band), then do not use slope. Instead
c        draw straight line. (zero slope). 10 mm/h is used
c        as cutoff since we should be at edge of rain core.
c        NOTE:  assumes 150m gate spacing and Balakrishnan
c        and Zrnic (1985) or Doviak and Zrnic (1993) R=39.72*Kdp^0.866
         th2=0.061

         drflag=.false.

c        PHIDP: fill and pad in and around good data
         call fill_pad(drflag,th1,th2,dpnew,hdpnew,mhead,krec,
     +   gdstrt,gdend,numgd,fildat)

c        filter good phidp data
         call filter(fildat,hdpnew,mhead,numgd,gdstrt,
     +   gdend,ftype,krec,fildp)


c        for Zdr:  FILL and PAD thresholds same 
         th1=0.1
         th2=0.1
c        set switch so that negative Zdr is not created by FILL/PAD
         drflag=.true.

c        ZDR: fill and pad in and around good data
         call fill_pad(drflag,th1,th2,dr,hdr,mhead,krec,
     +   gdstrt,gdend,numgd,drdat)

c        filter Zdr data where phidp good
         call filter(drdat,hdpnew,mhead,numgd,gdstrt,
     +   gdend,ftype,krec,fildr)

c        for Zh:  FILL and PAD same
         th1=2.0    
         th2=2.0
         drflag=.false.

c        DZ: fill and pad in and around good data
         call fill_pad(drflag,th1,th2,dz,hdz,mhead,krec,    
     +   gdstrt,gdend,numgd,dzdat)

c        filter Zh data where phidp good
         call filter(dzdat,hdpnew,mhead,numgd,gdstrt,
     +   gdend,ftype,krec,fildz)

c        estimate delta from raw and filtered phidp

         call deltacalc(dpnew,fildp,mhead,numgd,gdstrt,gdend,
     +   hdpnew,fhead,denew)

c        calculate Kdp in good data blocks

         call kdpcalc(fildp,mhead,numgd,gdstrt,gdend,
     +   hdpnew,fhead,kdnew)

c        for Kdp:  FILL and PAD thresholds same 
         th1=0.1
         th2=0.1
c        set switch so that negative Kdp is not created by FILL/PAD
         drflag=.true.

c        KDP: fill and pad in and around good data
         call fill_pad(drflag,th1,th2,kdnew,hdpnew,mhead,krec,
     +   gdstrt,gdend,numgd,kddat)

c        filter Kdp data where phidp good
         call filter(kddat,hdpnew,mhead,numgd,gdstrt,
     +   gdend,ftype,krec,filkd)

c     set Zdr to bad data and correct Zh in blocked azimuths
c     comment out for STEPS since created for SPOL radar during TRMM-LBA
c         call blockage(dm,dz,dr,mhead,hdz)
  
      
c        Using SD(phidp) and rhoHV, threshold the remaining non-phase fields
c        in order to remove clutter and clear air "noise" from them.  The
c        goal here is to preserve echo dominated by weather targets, with
c        an emphasis on precipitation.  I also clean-up the measured
c        total differential phase before output to UF.  An additional test
c        utilizes Zdr in low reflectivity to eliminate remaining clear air
c        echo and that slips through rhoHV and SD(phidp) tests.

      do 55 i=1,hdpnew(6)

         if(dpsd(i).eq.mhead(45).or.rh(i).eq.mhead(45))then
           dz(i)=mhead(45)
           dr(i)=mhead(45)
           fildp(i)=mhead(45)
           kdnew(i)=mhead(45)
           ld(i)=mhead(45)
           rh(i)=mhead(45)
           denew(i)=mhead(45)
           fildz(i)=mhead(45)
           fildr(i)=mhead(45)
	   filkd(i)=mhead(45)
	   vr(i)=mhead(45) 
         endif

         if(rh(i).lt.rhnth.or.dpsd(i).gt.dpsdnth)then
           dz(i)=mhead(45)
           dr(i)=mhead(45)
           fildp(i)=mhead(45)
           kdnew(i)=mhead(45)
           ld(i)=mhead(45)
           rh(i)=mhead(45)
           denew(i)=mhead(45)
           fildz(i)=mhead(45)
           fildr(i)=mhead(45)
	   filkd(i)=mhead(45)
	   vr(i)=mhead(45) 
         endif

c   *** COMMENT OUT FOR STEPS *** 
c         if(dz(i).lt.35.0.and.dz(i).ne.mhead(45))then
c
c           if(dz(i).lt.0.0)then
c             drtest=2.0
c           elseif(dz(i).le.15.0)then
c             drtest=0.2*dz(i)+2.0
c           else
c             drtest=5.0
c           endif   
c
c           if(dr(i).gt.drtest)then
c             dz(i)=mhead(45)
c             dr(i)=mhead(45)
c             fildp(i)=mhead(45)
c             kdnew(i)=mhead(45)
c             ld(i)=mhead(45)
c             rh(i)=mhead(45)
c             denew(i)=mhead(45)
c             fildz(i)=mhead(45)
c             fildr(i)=mhead(45)
c	     filkd(i)=mhead(45)
c	     vr(i)=mhead(45) 
c           endif

c         endif


c     This code will limit output data by max range and and beam height.
    
c     elevation angle in degrees
         theta=mhead(34)/64.0
c     range in km
         r=fhead(3)+fhead(4)/1000.+(i-1)*fhead(5)/1000.
c     height in km
         h=(r**2+(ke*a)**2+2*r*ke*a*sin(theta*pi/180.))**0.5-ke*a

c     keep all data for this application (hmax=25.0 km, rmax=250 km)

         if(h.gt.25.0.or.r.gt.250.0)then
           dz(i)=mhead(45)
           dr(i)=mhead(45)
           fildp(i)=mhead(45)
           kdnew(i)=mhead(45)
           ld(i)=mhead(45)
           rh(i)=mhead(45)
           denew(i)=mhead(45)
           fildz(i)=mhead(45)
           fildr(i)=mhead(45)
	   filkd(i)=mhead(45)
	   vr(i)=mhead(45) 
         endif

55    continue

c        extend phidp to low reflectivity regions for attenuation
c        correction

c QC
c         write(63,*) krec

         call phidpext(fildp,dz,hdpnew,mhead,numgd,gdstrt,
     +gdend)


c     Calculate sigma PHIdp threshold based on reflectivity.  This was derived
c     empirically from 139 radar volumes containing moderate-to-heavy rain
c     during TRMM-LBA Brazil (20, 23, 26, 31 Jan and 7, 15, and 17 Feb 99) 
c     Looked at the PDF of Zh vs sigma PHIdp as function of Kdp.  Drew a 
c     subjective boundary to eliminate the noise.

      do 135 i=1,hdpnew(6)
        if(dz(i).le.35.0)then
          sigphdp(i)=1.0
        elseif(dz(i).le.50.0)then
          sigphdp(i)=(dz(i)-35.0)*(7./15.)+1.0
        else
          sigphdp(i)=12.0
        endif
135   continue	

c     Clean up Kdp around the edges of cores before writing out to UF
c     If Kdp(i) is within 10 gates (1.5 km) of clear air/bad Kdp and
c     |Kdp(i)|>=0.5 deg/km and std dev PHIDP (i).gt.sigphdp (strict
c     threshold), then set Kp(i) to bad data.  Need to look at how this behaves
c     in partially blocked rays.

      nmgt=10

      do 200 i=1,hdpnew(6)
        if((i-nmgt).le.0)then
          j1=i-1
        else
          j1=nmgt
        endif
        if((hdpnew(6)-(i+nmgt)).le.0)then
          j2=hdpnew(6)-i
        else
          j2=nmgt
        endif
        kdbflag(i)=.false.
        do 150 m=i-j1,i+j2
          if((abs(kdnew(i)).ge.0.5).and.dpsd(i).gt.sigphdp(i).and.
     +    kdnew(m).eq.mhead(45))then
            kdbflag(i)=.true.
          endif
150     continue
200   continue

c  *** COMMENT OUT FOR STEPS

c      do 220 i=1,hdpnew(6)
c        if(kdbflag(i))then
c          kdnew(i)=mhead(45)
c        endif
c220   continue

c     correct Zh and Zdr for propagation effects.  Corrected values are
c     passed back as same variable.  NOTE:  ONLY CORRECT IF PHIDP IS
c     >= 60.0 DEG.

      call propfix(fildp,dz,fildz,dr,fildr,mhead,hdpnew)


c*Change character data in integer*2 mhead
c        KDP_UF generation source
         generat = "CSUATMOS"
         mhead(41) = ichar(generat(1:1))*256+ichar(generat(2:2))
         mhead(42) = ichar(generat(3:3))*256+ichar(generat(4:4))
         mhead(43) = ichar(generat(5:5))*256+ichar(generat(6:6))
         mhead(44) = ichar(generat(7:7))*256+ichar(generat(8:8))

c        site-name
         generat = "TRMM-LBA"
         mhead(15) = ichar(generat(1:1))*256+ichar(generat(2:2))
         mhead(16) = ichar(generat(3:3))*256+ichar(generat(4:4))
         mhead(17) = ichar(generat(5:5))*256+ichar(generat(6:6))
         mhead(18) = ichar(generat(7:7))*256+ichar(generat(8:8))

c        hardwire generation year
         mhead(38)=00

c+       Initialize ray to write routines

         call ufstartray(mhead(45))

c        Build ray data and headers

c        add horizontal reflectivity
         fname(1)="DZ"
         call ufbuildrayfld(lendz,hdz,dz)

c        add differential reflectivity
         fname(2)="DR"
         call ufbuildrayfld(lendr,hdr,dr)

c        add specific differential phase
         fname(3)="KD"
         call ufbuildrayfld(lenfhn,hdpnew,kdnew)	 
        
c        add linear depolarization ratio
         fname(4)="LD"
         call ufbuildrayfld(lenld,hld,ld)

c        add correlation coefficient
         fname(5)="RH"
         call ufbuildrayfld(lenrh,hrh,rh)

c        THE REST OF THE VARS ARE COMMENTED OUT FOR THIS APPLICATION

c        add radial velocity
c         fname(6)="VR"
c         call ufbuildrayfld(lenvr,hvr,vr)	 

c        add differential propagation phase
c         fname(7)="DP"
c         call ufbuildrayfld(lenfhn,hdpnew,fildp)

c        add the backscatter differential phase 
c         fname(8)="DE"
c         call ufbuildrayfld(lenfhn,hdpnew,denew)

c        add the measured differential phase
c         fname(9)="PH"
c         call ufbuildrayfld(lenfhn,hdpnew,dp)

c        add filtered horizontal reflectivity
c         fname(10)="FZ"
c         call ufbuildrayfld(lendz,hdz,fildz)

c        add filtered differential reflectivity
c         fname(11)="FR"
c         call ufbuildrayfld(lendr,hdr,fildr)
	 
c        add filtered specific differential phase
c         fname(12)="FK"
c         call ufbuildrayfld(lenfhn,hdpnew,filkd)

c+Write out the ray we have prepared with proper headers
         call ufwriteray(iout,krec,mhead,fname)

c-Loop back up to read the next ray
      goto 10

c-Done with input uf file so close it
250   call ufclose(iin)
c+Done with output uf file so close it
      call ufclose(iout)

      goto 5

888   write(*,*) 'Error reading input uf file names'
      goto 2000
999   write(*,*) 'Done processing uf files'


2000  close(18)

c All done
      stop
      end

c     This subroutine unfolds differential phase data up to 
c     1 fold (alias) in the S-POL data.  The S-POL data folds at 160.
c     So, the maximum resolvable differential phase is 180+160=340 deg.

      subroutine unfold(idp,jdp,dp,hdpnew,gdstrt,gdend,numgd,mhead,
     +rh,krec,rhthrsh,dz,dpnew)

c Set array size large enough
      parameter (NVAR=16, NFMAX=48, NGATE=1200)

      real*4 dp(NGATE),dpnew(NGATE),rh(NGATE),dz(NGATE)
      real*4 rhthrsh
      integer*2 hdpnew(NFMAX),mhead(45)
      integer*2 numgd,gdstrt(50),gdend(50),krec
      integer*2 idp,jdp

      do 100 i=1,hdpnew(6)
          dpnew(i)=dp(i)
100   continue

      do 500 j=1,numgd

        if(j.ge.jdp)then

          if(j.eq.1)then

            do 200 i=gdstrt(j),gdend(j)-1

              if(i.ge.idp)then
                if(rh(i).ge.rhthrsh.and.rh(i).ne.mhead(45).and.
     +          rh(i+1).ge.rhthrsh.and.rh(i+1).ne.mhead(45).and.
     +          dpnew(i).gt.145.0)then
                  if((dpnew(i)-dpnew(i+1)).gt.145.)then
c                    write(95,*) 'Unfolding @ ray ',krec,' gate ',i
                    dpnew(i+1)=dpnew(i+1)+180.0
                  endif
                endif
              endif
        
200         continue

          else

            if(idp.eq.gdstrt(j))then

              k=gdend(j-1)
              if(rh(k).ge.rhthrsh.and.rh(k).ne.mhead(45).and.
     +        rh(idp).ge.rhthrsh.and.rh(idp).ne.mhead(45).and.
     +        dpnew(k).gt.145.0)then
                if((dpnew(k)-dpnew(idp)).gt.145.)then
                  dpnew(idp)=dpnew(idp)+180.0
c                  write(95,*) 'Unfolding @ ray ',krec,' gate ',i
                endif
              endif
            endif

            do 300 i=gdstrt(j),gdend(j)-1
     
              if(i.ge.idp)then
                if(rh(i).ge.rhthrsh.and.rh(i).ne.mhead(45).and.
     +          rh(i+1).ge.rhthrsh.and.rh(i+1).ne.mhead(45).and.
     +          dpnew(i).gt.145.0)then
                  if((dpnew(i)-dpnew(i+1)).gt.145.)then
                    dpnew(i+1)=dpnew(i+1)+180.0
c                    write(95,*) 'Unfolding @ ray ',krec,' gate ',i
                  endif
                endif
              endif

300         continue

          endif

        endif

500   continue         
         
      return
      end            


c     This subroutine calculates the standard deviation of the differential
c     phase at each gate.  21 gates (3.15 km) are used for the calculation.
c     10 gates forward and reverse are typically used.  For first and last 
c     10 gates, the averaging interval is shifted.  If more than 10 bad 
c     data points exist in the averaging interval, then no std dev is 
c     calculated and it is set to bad value.

      subroutine  dp_stddev(dpnew,hdpnew,mhead,rh,rhthrsh,dz,dpsd)

c Set array size large enough
      parameter (NVAR=16, NFMAX=48, NGATE=1200)

      real*4 dpnew(NGATE),dpsd(NGATE),rh(NGATE),dz(NGATE)
      integer*2 hdpnew(NFMAX),mhead(45)
      real*4 dpsum,dpsum2,rhthrsh
      integer knt

      do 100 i=1,10
        knt=0
        dpsum=0.
        dpsum2=0.
        do 95 j=1,21
          if(dpnew(j).ne.mhead(45).and.rh(j).ge.rhthrsh.and.
     +    dz(j).ne.mhead(45))then
            dpsum=dpsum+dpnew(j)
            dpsum2=dpsum2+dpnew(j)**2
            knt=knt+1
          endif
95      continue
        if(knt.ge.11)then
          dpsd(i)=sqrt(abs(dpsum2/knt-(dpsum/knt)**2))
        else
          dpsd(i)=mhead(45)
        endif
100   continue

      do 200 i=11,hdpnew(6)-10
        knt=0
        if(dpnew(i).ne.mhead(45).and.rh(i).ge.rhthrsh.and.
     +    dz(i).ne.mhead(45))then   
          dpsum=dpnew(i) 
          dpsum2=dpnew(i)**2
          knt=knt+1
        else
          dpsum=0.
          dpsum2=0.
        endif
        do 190 j=1,10
          if(dpnew(i-j).ne.mhead(45).and.rh(i-j).ge.rhthrsh.and.
     +    dz(i-j).ne.mhead(45))then
            dpsum=dpsum+dpnew(i-j)
            dpsum2=dpsum2+dpnew(i-j)**2
            knt=knt+1
          endif
          if(dpnew(i+j).ne.mhead(45).and.rh(i+j).ge.rhthrsh.and.
     +    dz(i+j).ne.mhead(45))then
            dpsum=dpsum+dpnew(i+j)
            dpsum2=dpsum2+dpnew(i+j)**2
            knt=knt+1
          endif 
190     continue
        if(knt.ge.11)then
          dpsd(i)=sqrt(abs(dpsum2/knt-(dpsum/knt)**2))
        else
          dpsd(i)=mhead(45)
        endif
200   continue


      do 300 i=hdpnew(6)-9,hdpnew(6)
        knt=0
        dpsum=0.
        dpsum2=0.
        do 295 j=hdpnew(6)-20,hdpnew(6)
          if(dpnew(j).ne.mhead(45).and.rh(j).ge.rhthrsh.and.
     +    dz(j).ne.mhead(45))then
            dpsum=dpsum+dpnew(j)
            dpsum2=dpsum2+dpnew(j)**2
            knt=knt+1
          endif
295     continue
        if(knt.ge.11)then
          dpsd(i)=sqrt(abs(dpsum2/knt-(dpsum/knt)**2))
        else
          dpsd(i)=mhead(45)
        endif
300   continue

      return
      end

c     This subroutine locates blocks of good data where good data
c     is defined as a minimum of 12 consecutive good data gates.
c     Good data blocks end when 2 consecutive bad data gates occur.
c     So, up to 1 consecutive bad data gates can occur within good
c     data blocks.  It will be filled by the fill_pad routine.

      subroutine good_bad(dpnew,hdpnew,mhead,dz,rh,dpsd,rhthrsh,
     +dpsdnth,gdstrt,gdend,numgd,pass1)

c     adjustable parameters for determining location of good data
c     blocks. ** NOTE: ngd=2*(gdpts-1) **

      integer gdpts,bdpts,ngd
      parameter(gdpts=7,ngd=12,bdpts=2)

c Set array size large enough
      parameter (NVAR=16, NFMAX=48, NGATE=1200)

      real*4 dpnew(NGATE),rh(NGATE),dz(NGATE),dpsd(NGATE) 
      integer*2 hdpnew(NFMAX),mhead(45)
      integer*2 gdcnt,bdcnt,numgd,gdstrt(50),gdend(50)
      real*4 rhthrsh,dpsdnth
      logical bdflag(NGATE),pass1

c     initialize

c     This flag will keep track of bad data inside and outside of good data 
c     blocks. Start with false.  By bad data here I mean data at gates that do
c     not meet given quality thresholds.
      do 60 i=1,hdpnew(6)
        bdflag(i)=.false.
60    continue

c     maximum of 50 good data intervals (cells) per ray....
      do 90 i=1,50
        gdstrt(i)=0
        gdend(i)=0
90    continue

      gdcnt=0
      numgd=0
      bdcnt=0

      do 100 i=1,hdpnew(6)

        if(dpnew(i).ne.mhead(45).and.dz(i).ne.mhead(45).and.
     +  rh(i).ge.rhthrsh.and.dpsd(i).le.dpsdnth.and.
     +  dpsd(i).ne.mhead(45).and.rh(i).ne.mhead(45))then

          gdcnt=gdcnt+1

          if(gdcnt.eq.gdpts)then
            numgd=numgd+1
            gdstrt(numgd)=i-(gdpts-1)
            bdcnt=0
          elseif(gdcnt.gt.gdpts)then
            bdcnt=0
          endif

        else

c         mark it as 'bad' since did not meet quality criteria
          bdflag(i)=.true.

          if(gdcnt.ge.gdpts)then
            bdcnt=bdcnt+1
            if(bdcnt.lt.bdpts)then
              gdcnt=gdcnt+1
            else
              gdend(numgd)=i-bdpts
              gdcnt=0
              bdcnt=0
            endif
          else
            gdcnt=0
            bdcnt=0
          endif
      
        endif

        if(i.eq.hdpnew(6))then
          if(gdstrt(numgd).ne.0.and.gdend(numgd).eq.0)then
            gdend(numgd)=i
          endif
        endif

100   continue

c     For gates that did not meet quality criteria, set phase to bad data
c     value, inside good data blocks.  fill_pad routine will fill inside
c     good data blocks.  Do not do this during the 1st guess/pass using rhoHV
c     only.  Also, do not apply outside of good data blocks.

      if(.not.pass1)then
        do 300 j=1,numgd
          do 200 i=gdstrt(j),gdend(j)
            if(bdflag(i))then
              dpnew(i)=mhead(45)
            endif
200       continue
300     continue
      endif

      return
      end


c     This subroutine fills bad data points (up to a max. of
c     2 consecutive gates) within good data and pads good data
c     up to a maximum of 10 gates on either side of good data
c     for the purpose of filtering.

      subroutine fill_pad(drflag,th1,th2,dpnew,hdpnew,mhead,krec,
     +gdstrt,gdend,numgd,fildat)

c     adjustable parameters for determining location of good data
c     blocks. ** NOTE: ngd=2*(gdpts-1) **

      integer gdpts,bdpts,ngd
      parameter(gdpts=7,ngd=12,bdpts=2)

c Set array size large enough
      parameter (NVAR=16, NFMAX=48, NGATE=1200)

      real*4 dpnew(NGATE)
      real*4 fildat(NGATE+20)
      integer*2 hdpnew(NFMAX),mhead(45),krec
      integer*2 numgd,gdstrt(50),gdend(50),filstrt(50),filend(50)
      integer l,i,j,k
      real lsedat(ngd),lsegate(ngd),slope,yint,r2
      logical drflag
      real*4 th1,th2

c     initialize data to be filtered

      do 100 i=11,hdpnew(6)+10
100     fildat(i)=dpnew(i-10)

c     put bad data on either end in case not padded

      do 125 i=1,10
125     fildat(i)=mhead(45)

      do 150 i=hdpnew(6)+11,hdpnew(6)+20
150     fildat(i)=mhead(45)

c     shift good data blocks accordingly
      do 175 i=1,numgd
         filstrt(i)=gdstrt(i)+10
         filend(i)=gdend(i)+10 
175   continue          

c     fill in bad data points embedded within good data blocks
    
      do 300 i=1,numgd

        do 250 j=filstrt(i),filend(i)

          if(fildat(j).eq.mhead(45))then
            l=0 
            m=j+(gdpts-1)
            if(m.gt.filend(i))then
              m=filend(i)
            endif
            do 200 k=j-(gdpts-1),m

              if(fildat(k).ne.mhead(45))then
                l=l+1
                lsedat(l)=fildat(k)
                lsegate(l)=k
              endif

200         continue

            call lse(lsedat,lsegate,l,slope,yint,r2)

            if(abs(slope).le.th1)then
              fildat(j)=slope*j+yint
              if(drflag.and.fildat(j).lt.0)then
                fildat(j)=0.0
              endif
            else
              fildat(j)=fildat(j-1)
            endif

c QC
c            if(r2.lt.0.04)then
c              write(60,*) 'suspect fill at gate=',j,' rec=',krec
c            else
c              write(60,*) 'OK fill at gate=',j,' rec=',krec 
c            endif

          endif   

250     continue
300   continue      

c     pad either end of good data blocks for filtering purposes

      do 400 i=1,numgd

c       pad beginning of all good data in ray
        if(i.eq.1)then
          l=0
          do 310 j=filstrt(i),filstrt(i)+(gdpts-2)

            if(fildat(j).ne.mhead(45))then
              l=l+1
              lsedat(l)=fildat(j)
              lsegate(l)=j
            endif

310       continue
          call lse(lsedat,lsegate,l,slope,yint,r2)

c QC       
c          if(r2.lt.0.04)then
c            write(60,*) 'suspect pad at gate=',filstrt(i),' rec=',krec
c          else
c            write(60,*) 'OK pad at gate=',j,' rec=',krec
c          endif


          if(abs(slope).le.th2)then
            do 320 k=filstrt(i)-10,filstrt(i)-1
              fildat(k)=slope*k+yint
              if(drflag.and.fildat(k).lt.0)then
                fildat(k)=0.0
              endif
320         continue
          else
            do 325 k=filstrt(i)-10,filstrt(i)-1
              fildat(k)=fildat(filstrt(i))
325         continue
          endif

c         if 1st good data block is also last then pad end of data          

          if(i.eq.numgd)then

            l=0
            do 330 j=filend(i)-(gdpts-2),filend(i)

              if(fildat(j).ne.mhead(45))then
                l=l+1
                lsedat(l)=fildat(j)
                lsegate(l)=j
              endif

330         continue
            call lse(lsedat,lsegate,l,slope,yint,r2)

c QC
c            if(r2.lt.0.04)then
c              write(60,*) 'suspect pad at gate=',filend(i),' rec=',krec
c            else
c              write(60,*) 'OK pad at gate=',j,' rec=',krec
c            endif

            if(abs(slope).le.th2)then
              do 340 k=filend(i)+1,filend(i)+10
                fildat(k)=slope*k+yint
                if(drflag.and.fildat(k).lt.0)then
                  fildat(k)=0.0
                endif
340           continue
            else
              do 345 k=filend(i)+1,filend(i)+10
                fildat(k)=fildat(filend(i))
345           continue
            endif           
  
          endif

c       pad all other data btwn good blocks if not first block
        else
          if((filstrt(i)-filend(i-1)).ge.20)then
            l=0
            do 350 j=filend(i-1)-(gdpts-2),filend(i-1)
 
              if(fildat(j).ne.mhead(45))then
                l=l+1
                lsedat(l)=fildat(j)
                lsegate(l)=j
              endif 

350         continue
            call lse(lsedat,lsegate,l,slope,yint,r2)

c QC
c            if(r2.lt.0.04)then
c              write(60,*) 'suspect pad at gate=',filend(i-1),' rec=',krec
c            else 
c              write(60,*) 'OK pad at gate=',j,' rec=',krec
c            endif


            if(abs(slope).le.th2)then
              do 360 k=filend(i-1)+1,filend(i-1)+10
                fildat(k)=slope*k+yint
                if(drflag.and.fildat(k).lt.0)then
                  fildat(k)=0.0
                endif
360           continue
            else
              do 362 k=filend(i-1)+1,filend(i-1)+10
                fildat(k)=fildat(filend(i-1))
362           continue
            endif 

            l=0 
            do 365 j=filstrt(i),filstrt(i)+(gdpts-2)
              
              if(fildat(j).ne.mhead(45))then
                l=l+1
                lsedat(l)=fildat(j)
                lsegate(l)=j
              endif

365         continue
            call lse(lsedat,lsegate,l,slope,yint,r2)

c QC
c            if(r2.lt.0.04)then
c              write(60,*) 'suspect pad at gate=',filstrt(i),' rec=',krec
c            else 
c              write(60,*) 'OK pad at gate=',j,' rec=',krec
c            endif


            if(abs(slope).le.th2)then
              do 370 k=filstrt(i)-10,filstrt(i)-1
                fildat(k)=slope*k+yint
                if(drflag.and.fildat(k).lt.0)then
                  fildat(k)=0.0
                endif
370           continue
            else
              do 372 k=filstrt(i)-10,filstrt(i)-1
                fildat(k)=fildat(filstrt(i))
372           continue
            endif 

          else 
            l=0    
            do 375 j=filend(i-1)-(gdpts-2),filend(i-1)
              
              if(fildat(j).ne.mhead(45))then
                l=l+1
                lsedat(l)=fildat(j)
                lsegate(l)=j
              endif 
375         continue

            do 380 j=filstrt(i),filstrt(i)+(gdpts-2)

              if(fildat(j).ne.mhead(45))then
                l=l+1
                lsedat(l)=fildat(j)
                lsegate(l)=j
              endif 
380         continue
            call lse(lsedat,lsegate,l,slope,yint,r2) 

c QC
c            if(r2.lt.0.04)then
c              write(60,*) 'suspect pad at gate=',filend(i-1),' rec=',krec
c            else 
c              write(60,*) 'OK pad at gate=',j,' rec=',krec
c            endif

            if(abs(slope).le.th2)then
              do 390 k=filend(i-1)+1,filstrt(i)-1
                fildat(k)=slope*k+yint
                if(drflag.and.fildat(k).lt.0)then
                  fildat(k)=0.0
                endif
390           continue
            else
              do 391 k=filend(i-1)+1,filstrt(i)-1
                fildat(k)=(fildat(filend(i-1))+fildat(filstrt(i)))/2
391           continue
            endif

          endif

c       pad end of all good data in ray if it is final good data block
          if(i.eq.numgd)then
            l=0
            do 393 j=filend(i)-(gdpts-2),filend(i)

              if(fildat(j).ne.mhead(45))then
                l=l+1
                lsedat(l)=fildat(j)
                lsegate(l)=j
              endif

393         continue
            call lse(lsedat,lsegate,l,slope,yint,r2)

c QC
c            if(r2.lt.0.04)then
c              write(60,*) 'suspect pad at gate=',filend(i),' rec=',krec
c            else 
c              write(60,*) 'OK pad at gate=',j,' rec=',krec
c            endif


            if(abs(slope).le.th2)then
              do 396 k=filend(i)+1,filend(i)+10
                fildat(k)=slope*k+yint
                if(drflag.and.fildat(k).lt.0)then
                  fildat(k)=0.0
                endif
396           continue
            else
              do 398 k=filend(i)+1,filend(i)+10 
                fildat(k)=fildat(filend(i))
398           continue
            endif

          endif   
        endif   
400   continue
      
      return
      end 


      subroutine lse(lsedat,lsegate,l,slope,yint,r2)

c     adjustable parameters for determining location of good data
c     blocks. ** NOTE: ngd=2*(gdpts-1) **

      integer gdpts,bdpts,ngd
      parameter(gdpts=7,ngd=12,bdpts=2)

c Set array size large enough
      parameter (NVAR=16, NFMAX=48, NGATE=1200)

      integer l
      real lsedat(ngd),lsegate(ngd),slope,yint,r2

      sumd=0.0
      sumd2=0.0
      sumg=0.0
      sumg2=0.0
      sumgd=0.0

      do 450 i=1,l

        sumd=sumd+lsedat(i)
        sumd2=sumd2+lsedat(i)**2
        sumg=sumg+lsegate(i)
        sumg2=sumg2+lsegate(i)**2
        sumgd=sumgd+lsegate(i)*lsedat(i)

450   continue

      slope=(l*sumgd-sumg*sumd)/(l*sumg2-sumg**2)
      yint=(sumd-slope*sumg)/l
      r2=(l*sumgd-sumg*sumd)**2
      r2=r2/(l*sumg2-sumg**2)/(l*sumd2-sumd**2)

      return
      end


c     This subroutine does the actual filtering using a 21 pt
c     low pass filter.  For now, use filter coefficients from
c     the Finite Impulse Response (FIR) filter discussed in
c     Hubbert and Bringi (1993,1995).

      subroutine filter(fildat,hdpnew,mhead,numgd,gdstrt,
     +gdend,ftype,krec,fildp)

c     adjustable parameters for determining location of good data
c     blocks. ** NOTE: ngd=2*(gdpts-1) **

      integer gdpts,bdpts,ngd
      parameter(gdpts=7,ngd=12,bdpts=2)

c Set array size large enough
      parameter (NVAR=16, NFMAX=48, NGATE=1200)

      integer*2 mhead(45),hdpnew(NFMAX),krec
      integer*2 numgd,gdstrt(50),gdend(50),ftype
      real*4 fildat(NGATE+20),fildp(NGATE)
      real fc(4,21)

c   reads in the filter coefficients based on user input to ftype

c     reads in the 21-pt FIR filter from Hubbert and Bringi (1995)

      DATA (fc(1,j),j=1,21)
     +/.01626, .02231, .02896, .03596, .04299, .04971,
     +.05579, .06090, .06477, .06718, .06800, .06718,
     +.06477, .06090, .05579, .04971, .04299, .03596,
     +.02896, .02231, .01626/

c     reads in the 21pt running mean filter coefficients

      DATA (fc(2,j),j=1,21)
     +/.04762, .04762, .04762, .04762, .04762, .04762,
     +.04762, .04762, .04762, .04762, .04762, .04762,
     +.04762, .04762, .04762, .04762, .04762, .04762,
     +.04762, .04762, .04762/

c     reads in the 17pt running mean filter coefficients

      DATA (fc(3,j),j=1,21)
     +/.00000, .00000, .05882, .05882, .05882, .05882,
     +.05882, .05882, .05882, .05882, .05882, .05882,
     +.05882, .05882, .05882, .05882, .05882, .05882,
     +.05882, .00000, .00000/

c     reads in the 13pt running mean filter coefficients

      DATA (fc(4,j),j=1,21)
     +/.00000, .00000, .00000, .00000, .07692, .07692,
     +.07692, .07692, .07692, .07692, .07692, .07692,
     +.07692, .07692, .07692, .07692, .07692, .00000,
     +.00000, .00000, .00000/

c     fill filtered data with bad data to start

      do 490 j=1,hdpnew(6)
490     fildp(j)=mhead(45)
     
      do 700 i=1,numgd

        do 600 m=gdstrt(i)+10,gdend(i)+10 

          fildp(m-10)=0.0

          do 500 n=1,21
  
            if(fildat(m+(n-11)).ne.mhead(45))then
              fildp(m-10)=fildp(m-10)+fc(ftype,n)*fildat(m+(n-11))
            else
              write(*,*) '**FILTERING BAD DATA. CHECK fill_pad!**'
              write(*,*) 'Ray = ',krec,'   Gate = ',m+(n-11)
            endif
              
500       continue

600     continue

700   continue

      return 
      end 

      subroutine kdpcalc(dpnew,mhead,numgd,gdstrt,gdend,
     +hdpnew,fhead,kdnew)

c     adjustable parameters for determining location of good data
c     blocks. ** NOTE: ngd=2*(gdpts-1) **

      integer gdpts,bdpts,ngd
      parameter(gdpts=7,ngd=12,bdpts=2)

c Set array size large enough
      parameter (NVAR=16, NFMAX=48, NGATE=1200)

      integer*2 mhead(45),hdpnew(NFMAX),fhead(NFMAX)
      integer*2 numgd,gdstrt(50),gdend(50)
      real*4 dpnew(NGATE),kdnew(NGATE)

c     initialize Kdp with bad data
      do 725 j=1,hdpnew(6)
725     kdnew(j)=mhead(45)

c     calculate Kdp in good data blocks
      do 800 i=1,numgd

        do 750 m=gdstrt(i),gdend(i)  

          if(m.eq.gdstrt(i))then 

c           single sided (forward) finite difference; one-way propagation
            kdnew(m)=(dpnew(m+1)-dpnew(m))/(2.*(fhead(5)/1000.))
      
          elseif(m.eq.gdend(i))then

c           single sided (backward) finite difference; one-way propagation
            kdnew(m)=(dpnew(m)-dpnew(m-1))/(2.*(fhead(5)/1000.))

          else

c           centered finite difference; one-way propagation
            kdnew(m)=(dpnew(m+1)-dpnew(m-1))/(2.*(2*fhead(5)/1000.))

          endif

750     continue

800   continue

      return
      end

c     This subroutine estimates the backscatter differential phase.

      subroutine deltacalc(dpnew,fildp,mhead,numgd,gdstrt,gdend,
     +hdpnew,fhead,denew)

c     adjustable parameters for determining location of good data
c     blocks. ** NOTE: ngd=2*(gdpts-1) **

      integer gdpts,bdpts,ngd
      parameter(gdpts=7,ngd=12,bdpts=2)

c Set array size large enough
      parameter (NVAR=16, NFMAX=48, NGATE=1200)

      integer*2 mhead(45),hdpnew(NFMAX),fhead(NFMAX)
      integer*2 numgd,gdstrt(50),gdend(50)
      real*4 dpnew(NGATE),denew(NGATE),fildp(NGATE)

c     initialize delta with bad data
      do 900 j=1,hdpnew(6)
900     denew(j)=mhead(45)


c     calculate delta in good data blocks
      do 950 i=1,numgd
        do 925 m=gdstrt(i),gdend(i)
          denew(m)=dpnew(m)-fildp(m)
925     continue
950   continue

      return
      end

c     extends phidp into low reflectivity region for
c     proper attenuation correction and smoother Kdp fields in
c     these areas.

      subroutine phidpext(fildp,dz,hdpnew,mhead,numgd,gdstrt,
     +gdend)

c Set array size large enough
      parameter (NFMAX=48, NGATE=1200)

      integer*2 mhead(45),hdpnew(NFMAX)
      integer*2 numgd,gdstrt(50),gdend(50)
      real*4 dz(NGATE),fildp(NGATE)

      if(numgd.ge.1)then

c QC
c        do 998 m=1,hdpnew(6)
c          write(63,997) m,dz(m),fildp(m)
c997       format(i5,2(f10.2))
c998     continue

        do 1300 i=1,numgd

          if(i.eq.1)then
            if(gdstrt(i).ne.1)then
              do 1000 m=1,gdstrt(i)-1
                if(dz(m).ne.mhead(45))then
                  if(fildp(m).eq.mhead(45))then
                    fildp(m)=fildp(gdstrt(i))
                  endif
                endif
1000          continue
            endif
          else
            do 1100 m=gdend(i-1)+1,gdstrt(i)-1
              if(dz(m).ne.mhead(45))then
                if(fildp(m).eq.mhead(45))then
                  fildp(m)=fildp(gdend(i-1))
                endif
              endif
1100        continue
          endif

          if(i.eq.numgd)then
            if(gdend(i).ne.hdpnew(6))then
              do 1200 m=gdend(i)+1,hdpnew(6)
                if(dz(m).ne.mhead(45))then
                  if(fildp(m).eq.mhead(45))then
                    fildp(m)=fildp(gdend(i))
                  endif
                endif
1200          continue
            endif
          endif

1300    continue

c QC
c        write(63,*)
c        do 1305 m=1,hdpnew(6)
c          write(63,997) m,dz(m),fildp(m)
c1305    continue
c        write(63,*)
c        write(63,*)

      endif

      return
      end

c     This subroutine corrects for horizontal attenuation in Zh and
c     differential attenuation in Zdr using the technique of Ryzhkov and
c     Zrnic (1995) and Carey et al. (2000).
      
      subroutine propfix(fildp,dz,fildz,dr,fildr,mhead,hdpnew)

c Set array size large enough
      parameter (NFMAX=48, NGATE=1200)

      integer*2 mhead(45),hdpnew(NFMAX)
      real*4 dz(NGATE),fildp(NGATE),dr(NGATE)
      real*4 fildz(NGATE),fildr(NGATE)

      do 200 i=1,hdpnew(6)

c       correct for system phase.  system phase is estimated at ?? deg for
c       S-pol during STEPS

c       COMMENT OUT UNTIL WE KNOW WHAT SYSTEM PHASE SHIFT IS ?? (e.g., it was
c        6.5 degrees for SPOL TRMM-LBA

C        if(fildp(i).ne.mhead(45))then
c          if(fildp(i).lt.6.5)then
c            fildp(i)=0.0
c          else
c            fildp(i)=fildp(i)-6.5
c          endif
c        endif
	        
c       ONLY CORRECT FOR PROPAGATION EFFECTS IF DIFFERENTIAL PROPAGATION
c       PHASE EQUALS or EXCEEDS 60 DEGREES.  THIS MEANS THAT ZHCOR>~1.0 dB
c       AND ZDRCOR>~0.25 dB.  OTHERWISE, THE CORRECTION CAN MAKE RAIN RATE
c       WORSE THAN NOT CORRECTING AT ALL
 
        if(fildp(i).ne.mhead(45).and.fildp(i).ge.60.0.and.
     +  fildp(i).le.340.0)then

c       take correction coefficients from Bring et al. (1990)

          if(dz(i).ne.mhead(45))then
            dz(i)=dz(i)+fildp(i)*0.0165
          endif

          if(fildz(i).ne.mhead(45))then
            fildz(i)=fildz(i)+fildp(i)*0.0165
          endif

          if(dr(i).ne.mhead(45))then
            dr(i)=dr(i)+fildp(i)*0.00334
          endif

          if(fildr(i).ne.mhead(45))then
            fildr(i)=fildr(i)+fildp(i)*0.00334
          endif

        endif

200   continue

      return
      end

c     Set Zdr to bad data in partially or completely blocked azimuths.  Correct
c     Zh in partially blocked azimuths.

c     This subroutine is specific to SPOL radar during TRMM-LBA (1/99 - 2/99)
c     DON"T USE THESE #'s FOR ANY OTHER RADAR/EXPERIMENT !!!!

      subroutine blockage(dm,dz,dr,mhead,hdz)

c Set array size large enough
      parameter (NFMAX=48, NGATE=1200)

      integer*2 mhead(45),hdz(NFMAX)
      real*4 dz(NGATE),dm(NGATE),dr(NGATE)
      real*4 zhcor1(360),zhcor2(360)
      real az
      integer iaz

      DATA zhcor1
     +/0.00,0.14,2.05,0.76,0.00,0.02,0.00,0.01,1.23,1.10,0.81,0.97,0.32,
     +0.00,1.06,0.84,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,4.30,3.86,0.00,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
     +0.00,0.00,0.00,0.00,0.07,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.14,
     +0.00,0.00,0.00,0.00,1.32,0.00,0.00,0.00,0.00,0.00,0.00,0.16,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.08,0.04,1.41,0.26,1.60,2.60,1.88,1.78,
     +2.58,1.23,0.00,0.47,2.77,1.23,0.00,0.00,1.88,0.00,0.00,0.00,0.00,
     +0.00,0.00,0.00,0.00,0.00,2.00,5.58,7.29,0.01,0.99,1.38,0.49,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,2.15,0.00,0.00,0.00,
     +0.00,1.64,0.14,0.00,0.00,0.00,0.00,0.00,0.00,1.85,0.71,0.22,0.45,
     +1.93,0.34,0.00,0.00,0.00,0.07,0.06,0.00,1.02,0.26,0.12,0.02,0.00,
     +0.00,0.82,0.00,0.00,0.00,0.00,0.00,0.02,1.05,0.21,2.89,1.85,1.41,
     +0.03,0.00,0.96,0.00,3.16,5.34,10.79,16.46,24.43,34.01,49.70,49.70,
     +49.70,49.70,49.70,49.70,23.21,20.12,26.22,30.09,27.00,26.09,25.41,
     +22.47,18.80,14.91,9.77,6.79,10.85,6.71,7.15,9.03,8.33,4.05,10.08,
     +21.42,20.12,17.03,16.72,16.71,14.74,13.44,16.30,20.12,16.12,13.30,
     +13.16,15.51,16.87,13.95,12.87,13.45,7.56,6.89,7.83,11.50,15.10,
     +12.07,5.51,4.44,2.96,1.98,0.72,2.18,1.95,2.47,5.38,8.11,7.47,2.84,
     +0.01,0.15,0.82,2.13,1.63,3.65,2.10,2.58,2.35,2.25,0.87,0.00,0.55,
     +3.82,2.23,0.60,0.00,0.46,0.48,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
     +0.00,0.00,0.00,0.00/

      DATA zhcor2
     +/0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,1.36,0.00,0.00,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.16,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
     +0.00,0.06,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.57,0.00,0.00,0.00,
     +0.01,0.00,0.00,0.01,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.51,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.63,3.14,7.55,16.73,24.54,30.06,
     +31.61,35.02,29.18,20.74,9.51,4.49,10.32,12.41,8.50,8.22,7.92,6.11,
     +5.82,4.52,2.70,0.89,3.40,1.28,1.46,4.79,4.02,0.00,1.30,6.33,5.23,
     +5.01,4.22,4.33,4.57,1.56,4.87,9.35,4.53,2.16,4.33,4.56,4.94,4.40,
     +7.65,4.49,0.84,2.51,2.36,2.44,4.27,2.09,0.00,0.00,0.11,0.00,0.00,
     +0.00,0.00,0.00,0.00,1.22,0.89,0.00,0.00,0.00,0.00,0.00,0.00,2.45,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
     +0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00/
     
     
      az=real(mhead(33))/64.
      iaz=aint(az)+1

      do 100 i=1,hdz(6)
        if((real(mhead(34)/64.0)).lt.0.9)then
	  if(zhcor1(iaz).lt.36.0.and.dm(i).gt.-105.0)then
            dz(i)=dz(i)+zhcor1(iaz)
	  else
	    dz(i)=mhead(45)
	  endif
	elseif((real(mhead(34)/64.0)).lt.1.3)then
          if(zhcor2(iaz).lt.36.0.and.dm(i).gt.-105.0)then
            dz(i)=dz(i)+zhcor2(iaz)
	  else
	    dz(i)=mhead(45)
	  endif	
	endif 
100   continue	  	
       
      do 200 i=1,hdz(6)
        if((real(mhead(34)/64.0)).lt.0.9)then
          if((az.ge.0.0.and.az.lt.3.0).or.(az.ge.35.0.and.az.lt.36)
     +    .or.(az.ge.110.0.and.az.lt.112).or.(az.ge.252.and.az.lt.
     +    307.0).or.(az.ge.313.0.and.az.lt.317).and.(az.ge.322.0.and.
     +    az.lt.323).or.(az.ge.330.and.az.lt.331))then
            dr(i)=mhead(45)
	  endif    	  
	elseif((real(mhead(34)/64.0)).lt.1.3)then
          if((az.ge.254.0.and.az.lt.304).or.(az.ge.322.0.and.az.lt.
     +	  323.0))then
            dr(i)=mhead(45)   
	  endif
	endif 
200   continue	  	       
             
      return
      end
