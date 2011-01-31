#
#   Side-by-side comparison of a file within two directories
#      Usage: sdif dir_one dir_two
#
set dir_one = /ash/ljmill/JaySprint
set dir_two = /ash/sherrie/Sprint
set files = (BEAMIN.f NEXSWP.f NEXVOL.f PROFIL.f RAYIN.f \
             RPNCAR.f SPRINT.INC TRPPPI.f VERSOUT.f dorade.c)
echo " "
echo "*----*---- Compare JaySprint vs. Sherrie's Sprint ----*----*"
echo " "
foreach file ($files)
   echo " "
   echo "*----*----*----*---- Compare $file ----*----*----*----*"
   sdiff $dir_one/$file $dir_two/$file
end
exit
