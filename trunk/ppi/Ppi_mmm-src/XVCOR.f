c
c----------------------------------------------------------------------X
c
      SUBROUTINE XVCOR(DAT,IOUT,IIN1,IIN2,BDVAL,MNGATE,MXGATE,NANG,
     X                 C1,C2,RCOR,CTDBXV)
c
c	Simple routine to fix a stuck bit in CP2 xvert counts during
c	RAPS92.  May also be applied to CP2 in CaPE, but the info used
c	concerning the calibrations must be properly changed.
c
c	An attempt has been made to make this correction routine very
c	conservative.  This routine should not be called unless the radar
c	was running in true 8-parameter mode.  A correction is not applied
c	unless there is a single-gate jump in counts of 80 or more in xvert
c	*and* the jump occured while the counts were decreasing near 128
c	counts.  Additionally, the correction is applied only if the original
c	xvert counts translate into a received power that is *greater* than the
c	corresponding received power in the xhoriz (such a condition implies
c	that the resultant ldr would be greater than 1).
c
c	For convenience, and to avoid errors that may be caused by close-in
c	ground clutter, no attempt is made to correct data within the
c	first 20 gates near the radar.  (This is an option that may eventually
c	have to be made selectable by the user).
c
c	For RAPS92, the 24-jun-92 x-band calibration is referenced.  This CAL
c	is known to apply well over the period 17-jun thru 3-sep '92, and is
c	not too bad an approximation to the data prior to 17-jun.
c
c	For the 24-jun cal, we have the equations:
c		Phoriz = .376xhoriz - 115.18dBm
c		Pvert  = .191xvert  - 115.39dBm
c
c	Making some assumptions, over the linear portion of the cal curves
c	(this happens to be the most likely region affected by any stuck
c	bit in xvert), we have, if Phoriz = Pvert:
c
c		xhoriz = .509 xvert
c
c	Basically, any xvert counts are considered to be in error if
c	the xhoriz counts are not more than .509 times greater than
c	the xvert counts.
c				RARilling   Feb '92
c
      INCLUDE 'dim.inc'
      DIMENSION RCOR(MXR),CTDBXV(500),TMP(MXR),DAT(MXR,MXA,MXF)

        dat127=ctdbxv(127)
        do 10 j=1,nang
	i = mngate

        do 15 k=mngate,mxgate
           dat(k,j,iout)=dat(k,j,iin1)
 15        dat(k,j,iin1)=dat(k,j,iin1)-rcor(k)


	do 30 n=mngate,mxgate
	   i = i + 1
	   if( i .gt. mxgate ) go to 40

	   if(dat(i,j,iin1).lt.dat127 .or. dat(i,j,iin1).eq.bdval)
     +             go to 30
	   dg = dat(i,j,iin1) - dat(i-1,j,iin1)
	   if( dg .ge. 25. .and. dat(i-1,j,iin1) .ge. dat127 ) then
		if( (dat(i-1,j,iin1) - dat(i-2,j,iin1)) .gt. 0
     +            .and.(dat(i-1,j,iin1) - dat(i-3,j,iin1)) .gt.0)
     +             go to 30
 20             if(dat(i,j,iin1) .gt. dat127)then
                   do 25 i1=255,128,-1
                      dif=ctdbxv(i1)-dat(i,j,iin1)
                      if(dif.lt.0.)then
                         dat(i,j,iout)=ctdbxv(i1-127)+rcor(i)
                         go to 27
                      end if
 25                continue
 27                i = i + 1
	           if( i .le. mxgate) go to 20
	        endif
	   endif
   30	continue

   40	do 50 i=mngate,mxgate
           iz1=dat(i,j,iin1)
           iz2=dat(i,j,iout)
 50        dat(i,j,iin1)=dat(i,j,iin1)+rcor(i)

 10     continue
        return
c
c	Here's the intended logic of the above:
c
c	Do 30 until you run out of gates.  Start at gate 20.
c	If xvnew is less than 127, don't do anything (continue to next gate);
c		this condition should apply to much of the data.
c	Compute a change in xvnew; if greater than plus 80, and the previous
c		gate was greater than 127, take some action.
c
c		If the 2-point slope OR the 3-point slope just prior to the
c			80-count jump is generally decreasing, then go into
c			a loop to correct the data.  Continue the correction
c			until counts are again less than 127 (only do the
c			correction if the apparent xvnew power is greater
c			than the reported xhoriz power).
c	Increment the gate.

	end
