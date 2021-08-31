#!/bin/sh 

#---------------------------------------------------------
# This script was written by Tim Marchok.  It creates the
# "CARQ" TC analysis records cards" needed for an adeck file, 
# and it also creates the corresponding best track records 
# needed for the bdeck files.  We would only need to use 
# this script if we do not have access to the official adeck
# or bdeck files.  For example, we do not have the adeck or
# bdeck files for the western Pacific basin, but we do have
# a lot of tracks for the WP that we get from the run of
# our tracker using GFS, ensemble, UKMET and NAVGEM data.
# So we use this script to first create the adeck files
# from information contained in the tcvitals files.  Then
# you can use the insert_atx_tracks.sh script after this
# one in order to insert all of your tracks into the 
# proper location in these newly created adeck files.
# Finally, you can use the bdecks created from this script
# in order to run verifications for those other basins.
#
# Original writing:  Fall, 2003
# Modified by Guang Ping Lou, May 2009
#
export PS4=' + conv_tcvit_to_carq.sh line $LINENO: '

set -x 
stid=` echo $1 | tr '[a-z]' '[A-Z]'`
yyyy=$2

stnum=`   echo $stid | cut -c1-2`
baschar=` echo $stid | cut -c3-3`

case ${baschar} in
  L) BASIN=AL; basin="al";;
  W) BASIN=WP; basin="wp";;
  E) BASIN=EP; basin="ep";;
  C) BASIN=CP; basin="cp";;
  A) BASIN=NA; basin="na";;
  B) BASIN=BB; basin="bb";;
  S) BASIN=SI; basin="si";;
  P) BASIN=SP; basin="sp";;
  O) BASIN=SC; basin="sc";;
  T) BASIN=EC; basin="ec";;
  U) BASIN=AU; basin="au";;
  *) echo "!!! ERROR: BAD BASIN CHARACTER = --> $baschar <--"; exit 8;;
esac
 
sh ${USHtrkr}/tcvit.sh ${stid} ${yyyy} >${stid}.${yyyy}.tempvit

#-------------------------------------------------------
# Create the CARQ cards for the adeck....
#-------------------------------------------------------

while read stormrec
do
  
  echo "${stormrec}" | \

  awk '{

    latstr = $6
    lat    = substr(latstr,1,3)
    latns  = substr(latstr,4,4)

    lonstr = $7
    lon    = substr(lonstr,1,4)
    lonew  = substr(lonstr,5,5)

    hh   = substr($5,1,2)
    wspd = int( (($13 * 1.9427)/ 5.0) + 0.5) * 5

    printf ("%2s, %4s, %8s%2s, 01, CARQ,   0, %3d%s, %4d%s, %3s, %4d, XX,  34\n",BASIN,stnum,$4,hh,
            lat,latns,lon,lonew,wspd,$10)}

  ' BASIN=${BASIN} stnum=00${stnum}

done <${stid}.${yyyy}.tempvit >a${basin}00${stnum}${yyyy}.dat

#-------------------------------------------------------
# Create the best track data set....
#-------------------------------------------------------

while read stormrec
do
  
  echo "${stormrec}" | \

  awk '{
        
    latstr = $6
    lat    = substr(latstr,1,3)
    latns  = substr(latstr,4,4)

    lonstr = $7
    lon    = substr(lonstr,1,4)
    lonew  = substr(lonstr,5,5)

    hh   = substr($5,1,2)
    wspd = int( (($13 * 1.9427)/ 5.0) + 0.5) * 5
    
    printf ("%2s, %4s, %8s%2s,   , BEST,   0, %3d%s, %4d%s, %3s, %4d, XX,  34\n",BASIN,stnum,$4,hh,
            lat,latns,lon,lonew,wspd,$10)}
    
  ' BASIN=${BASIN} stnum=00${stnum}
    
done <${stid}.${yyyy}.tempvit >b${basin}00${stnum}${yyyy}.dat
  
