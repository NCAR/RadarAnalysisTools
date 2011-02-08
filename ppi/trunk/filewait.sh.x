#! /bin/sh
# sh script to see if a file has been
# successfully transferred from the Mass Store.

# check for improper invocation
if (test "$#" != 1)
then
     echo "usage:" $0 " filename"
     exit
fi

# see if file transfer has started
while (test ! -f $1)
do
     date +"DATE: %m/%d/%y%nTIME: %H:%M:%S";ls -l $1
     sleep 60
done
  
# now check to see if file transfer has completed
string=`ls -l $1 | awk '{print $4}'`
sleep 60
while (test "$string" != `ls -l $1 | awk '{print $4}'`)
do
	string=`ls -l $1 | awk '{print $4}'`
        date +"DATE: %m/%d/%y%nTIME: %H:%M:%S";ls -l $1
	sleep 60
done

echo "file is present and size unchanged for 1 minute"
date +"DATE: %m/%d/%y%nTIME: %H:%M:%S";ls -l $1

	
