      program tst

      character*2 iname

      print *,'Iname=',iname
      if(iname.eq.'  ')then
         print *,'Iname is blank'
      else if(iname.eq.'\0\0')then
         print *,'Iname is null'
      else
         print *,'Iname is not blank'
      end if
      stop
      end
