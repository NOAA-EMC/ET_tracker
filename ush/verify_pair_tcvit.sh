#!/bin/ksh -xf

# This script was written by Guang Ping Lou
# It seeks out named storm tcvitals and pairing them with model forcasts.

export PS4=' + verify_pair_tcvit.sh line $LINENO: '

date
ymdh=$1
regtype=$2

export archsyndir=${archsyndir:-prod/gfs/v16.2/syndat}
ndate=ndate
set -x

#cd ${TRKDATA}
  syyyy=` echo "$ymdh" | cut -c1-4`
  ym=` echo "$ymdh" | cut -c1-6`
  mm=` echo "$ymdh" | cut -c5-6`
  dd=` echo "$ymdh" | cut -c7-8`
 CYL=` echo ${ymdh} | cut -c9-10`

grep "${ym}${dd} ${CYL}" ${archsyndir}/syndat_tcvitals.${syyyy} | \
     grep -v TEST | grep -v INVEST |  \
     awk 'substr($0,6,1) !~ /[8-9]/ {print $0}'  >tcvitals
sort -k1,2 -u tcvitals |sort -k2,2 > tcvitals.1
nist=` cat tcvitals.1 | wc -l`
if [ ${nist} -ge 1 ]; then
cat tcvitals.1
 
while read atcfrec
do
  stid=` echo "$atcfrec" | cut -c6-8`
  sh ${USHtrkr}/conv_tcvit_to_carq.sh $stid ${syyyy}
done <tcvitals.1

ist=1
while read atcfrec
do
>adeck${ist}.temp
  stid=` echo "$atcfrec" | cut -c6-8`
stnum=`   echo "$atcfrec" | cut -c6-7`
baschar=` echo "$atcfrec" | cut -c8`

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

    for model in gfso ukx cmc emx ngx
    do
      grep "${BASIN},  ${stid}" ${savedir}/trak.${model}.atcf_gen.${regtype}.${syyyy} \
            |sort -u >>adeck${ist}.temp
    done

  stid="${BASIN}, 00${stnum}"
  stid1=` head -1 "adeck${ist}.temp" | cut -c1-8`
  sed "s/${stid1}/${stid}/" adeck${ist}.temp >adeck${ist}.temp1
awk 'match($0,"0 ,    0 ,   0,    0, XX") == 0 {print $0}' adeck${ist}.temp1 \
         >adeck${ist}.temp

     let ist=$ist+1
done <tcvitals.1

ls a*${syyyy}.dat >besttrack
ist=1
hr=06
while read atcfrec
do
/usr/bin/perl -pi -e 'substr($_,9,33)=" "' adeck${ist}.temp
   cat  adeck${ist}.temp | awk -FNEQ '{print $1}' > adeck${ist}.temp3

>adeck${ist}.temp2
  begdate=` head -1 "${atcfrec}" | cut -c11-20`
  enddate=` tail -1 "${atcfrec}" | cut -c11-20`
while [ ${begdate} -le ${enddate} ]
 do
      grep ${begdate} "${atcfrec}" >>adeck${ist}.temp2
      grep ${begdate} adeck${ist}.temp3 >>adeck${ist}.temp2
     begdate=` ${NDATE} ${hr} ${begdate}`
 done
cp adeck${ist}.temp2 ${optrack}/${atcfrec}
#cp adeck${ist}.temp2 ${savedir}/${atcfrec}
cp adeck${ist}.temp2 ${atcfrec}
cat ${atcfrec}

     let ist=$ist+1

done < besttrack
cp b*${syyyy}.dat ${optrack}
#cp b*${syyyy}.dat ${savedir}

# if [ ${nist} -ge 1 ] ends here
else
echo "no named storms"
fi
