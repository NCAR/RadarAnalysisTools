#!/bin/sh

for i in `ls *.[fc]`
do
   nrnet msput $i l df=tr r flnm=/ANDERSNB/CEDRIC/WKSTN/SUN/$i rtpd=367
   sleep 30
done
