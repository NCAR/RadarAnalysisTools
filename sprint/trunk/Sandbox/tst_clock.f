      program tst_clock

      character*8 tyme
      integer begsec,endsec,totsec,totmin,remsec

c      call clock(tyme)
      tyme = '10:34:27'
      read (tyme,11)ihr,imn,isc
 11   format(i2,1x,i2,1x,i2)
      begsec = isc + 60*imn + 3600*ihr
      print *,' '
      print *,'++++++++++++++++++++++++++++++++++++'
      print *,'Sprint execution started at ',tyme
      print *,'++++++++++++++++++++++++++++++++++++'
      print *,' '

c      call sleep(37)

c      call clock(tyme)
      tyme = '11:37:15'
      read (tyme,11)ihr,imn,isc
      endsec = isc + 60*imn + 3600*ihr
      totsec = endsec - begsec
      totmin = int(float(totsec)/60.0)
      remsec= totsec - 60*totmin
      print *,' '
      write(6,903)
 903  format(72('+'))
      write(6,905)tyme,totsec,totmin,remsec
 905  format('Sprint execution ended at ',a8, 
     X     ' taking ',i6,' seconds or MM:SS=',i3.2,':',i2.2)
      write(6,903)
      
      stop
      end

