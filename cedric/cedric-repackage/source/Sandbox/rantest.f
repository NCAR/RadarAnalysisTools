
      program rantest

      eps=1.0e-6
      ir1=111

      sum=0.0
      do 100 i=1,1000
         val=ran(ir1)
         sum=sum+val
         write(*,*)'***val,ir1=',val,ir1
 100  continue
      fmean=sum/1000.0
      write(*,*)'***mean #1=',fmean

      sum=0.0
      write(*,*)'***should be same***'
      ir1=111
      do 200 i=1,1000
         val=ran(ir1)
         sum=sum+val
         write(*,*)'***val,ir1=',val,ir1
 200  continue
      fmean=sum/1000.0
      write(*,*)'***mean #2=',fmean
      
      sum=0.0
      write(*,*)'***should be different now***'
      ir1=7000
      do 300 i=1,1000
         val=ran(ir1)
         sum=sum+val
         write(*,*)'***val,ir1=',val,ir1
 300  continue
      fmean=sum/1000.0
      write(*,*)'***mean #3=',fmean
      end
