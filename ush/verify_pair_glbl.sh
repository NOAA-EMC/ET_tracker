#!/bin/ksh -x

# This script was written by Guang Ping Lou
# It seeks out analysis and forecast cyclone tracks
# and arrange them in order. 
# 8/20/13 - Magee change ngps to nvgm
# 03/04/2015 - Guang Ping Lou added FNMOC ensemble
#             - add Dec. and Jan. switch storm ID recovery scripts

export PS4=' + verify_pair_glbl.sh line $LINENO: '

date
ymdh=$1
model=$2
regtype=$3

ndate=ndate
set -x

  syyyy=` echo "$ymdh" | cut -c1-4`
  ym=` echo "$ymdh" | cut -c1-6`
  mm=` echo "$ymdh" | cut -c5-6`
  dd=` echo "$ymdh" | cut -c7-8`
 CYL=` echo ${ymdh} | cut -c9-10`

if [ $cmodel = 'gfs' ]; then
   cmodel=gfso
fi
if [ $cmodel = 'ukmet' ]; then
   cmodel=ukx
fi
if [ $cmodel = 'ecmwf' ]; then
   cmodel=emx
fi
if [ $cmodel = 'nvgm' ]; then
   cmodel=ngx
fi
if [ $cmodel = 'nam' -o $cmodel = 'gfso' -o $cmodel = 'gefs' ]; then
  hrs=" 00 06 12 18 "
  hr=6
  hr12=12
elif [ $cmodel = 'sref' ]; then
  hrs=" 03 09 15 21 "
  hr=6
  hr12=12
else
  hrs=" 00 12 "
  hr=12
  hr12=24
fi

case ${cmodel} in
  gefs) aa=a;;
  eens) aa=e;;
  cens) aa=c;;
  fens) aa=f;;
     *) aa="";;
esac

if [ $model = 'xxxx' ]; then
bmodel=${cmodel}
modeldir=${TRKDATA}/xxxx
else
modeldir=${TRKDATA}/${model}
bmodel=${aa}${model}
fi
if [ ! -d ${modeldir} ];   then mkdir -p ${modeldir}; fi
cd ${modeldir}
trkdir=trackdir
if [ ! -d ${trkdir} ];   then mkdir -p ${trkdir}; fi
export analtrack=analdir
if [ ! -d ${analtrack} ];   then mkdir -p ${analtrack}; fi
optrackf=fcstdir
if [ ! -d ${optrackf} ];   then mkdir -p ${optrackf}; fi
optrackab=optrack
if [ ! -d ${optrackab} ];   then mkdir -p ${optrackab}; fi

trakfile=trak.anal_${bmodel}.atcf_gen.${regtype}.${ym}
trakfile1=trak.all_${bmodel}.atcf_gen.${regtype}.${ym}
savefile=trak.${bmodel}.atcf_gen.${regtype}.${syyyy}

eemodel=`echo ${bmodel} | tr "[a-z]" "[A-Z]"`
if [ $bmodel = 'ngx' -o $bmodel = 'ukx' -o $bmodel = 'emx' -o $bmodel = 'nam' -o $bmodel = 'cmc'  ]; then
emodel=" ${eemodel}"
zmodel="Z${eemodel}"
else
emodel="${eemodel}"
zmodel="Z${eemodel}"
fi

if [ $cmodel = 'nam' -o $cmodel = 'gfso' -o $cmodel = 'sref' ]; then
  dhr=3
  fhr=3
elif [ $cmodel = 'gefs' ]; then
  dhr=5
  fhr=6
else
  dhr=2
  fhr=2
fi

>${trakfile}
>${trakfile1}
>analysis.all.temp
  if [[ ${mm} -eq 01 && ${dd} -lt 10 ]]; then
  next_yyyy=` ${NDATE} -264 ${ymdh} | cut -c1-4`
   export savedir1=${COMGLTRK}/${next_yyyy}
  grep ${next_yyyy}12 ${savedir1}/${savefile} | grep "34, NEQ" >> ${trakfile1}
  fi
  hr1=240
while [ ${hr1} -ge 0 ]
do
  next_date=` ${NDATE} -${hr1} ${ymdh}`
  grep ${next_date} ${savedir}/${savefile} | grep "34, NEQ" >> ${trakfile1}
  let hr1=${hr1}-$hr
done
  grep ${ymdh} ${trakfile1} > trak.${bmodel}.atcf_gen.glbl.${ymdh}
  hr1=168
while [ ${hr1} -ge 0 ]
do
  next_date=` ${NDATE} -${hr1} ${ymdh}`
  grep ${next_date} ${trakfile1} | grep -i "${bmodel}, 000" \
              >> analysis.all.temp
  let hr1=${hr1}-${hr}
done

sort -u -o ${trakfile}.sorted analysis.all.temp
grep -v "ML" ${trakfile1} | \
     awk 'substr($0,6,1) !~ /[8-9]/ {print $0}'  \
     > TCV.all
grep -v "ML" ${trakfile}.sorted | \
     awk 'substr($0,6,1) !~ /[8-9]/ {print $0}'  \
     > TCV.all.1
grep "ML" ${trakfile}.sorted |sort -o ${trakfile}.1


sort -t, -k3,3 -u ${trakfile}.1 > storm_name
ist=1
while read atcfrec
do
  sname=` echo "$atcfrec" | cut -c11-40`
       grep "${sname}" ${trakfile1} | sort -u \
            |sort -t, -k4,4 >trackf
     lines=` grep "${begdate}, " trackf |wc -l`
     if [ $lines -ge $fhr ]; then
      mv trackf trackf.$ist
     fi
    if [ -f trackf.$ist ]; then
       grep "${sname}" analysis.all.temp   \
        | sort -u |sort -t, -k4,4 >tracka.$ist
         let ist=$ist+1
     fi
done < storm_name
         let storm_no=$ist-1

sort -t, -k1,2 -u TCV.all.1 > TCV_name
numtimes=` cat TCV_name | wc -l`
      if [ ${numtimes} -ge 1 ]; then
while read atcfrec
do
  sname=` echo "$atcfrec" | cut -c1-8`
       grep "${sname}" TCV.all.1 | sort -t, -k4,4 >tracka.$ist
       grep "${sname}" TCV.all | sort -t, -k4,4 \
                                  >trackf.$ist
         let ist=$ist+1
done < TCV_name
        let TCV_no=$ist-1
       else
        let TCV_no=${storm_no}

      fi

j=1
k=1
while [ $j -le ${storm_no} ]
do
  lines=` cat tracka.$j |wc -l`
  if [ $lines -ge $dhr ]; then

  sname=` head -1 "tracka.$j" | cut -c11-40`
  cat tracka.$j | sed "s/${sname}, //" | awk -FNEQ '{print $1}' > tempa
  cat trackf.$j | sed "s/${sname}, //" | awk -FNEQ '{print $1}' > tempf
/usr/bin/perl -pi -e 'substr($_,4,4)="    "' tempa 
/usr/bin/perl -pi -e 'substr($_,4,4)="    "' tempf 
    if [ $k -lt 10 ]; then
      kk="000${k}"
        elif [ $k -lt 100 ]; then
        kk="00${k}"
        elif [ $k -lt 1000 ]; then
        kk="0${k}"
     else
      kk="${k}"
    fi

    sed "s/     / ${kk}/" tempa |sort -u >${analtrack}/bml${syyyy}${mm}${dd}${CYL}.${bmodel}.${kk}
    sed "s/     / ${kk}/" tempf |sort -u >${optrackf}/aml${syyyy}${mm}${dd}${CYL}.${bmodel}.${kk}
    sed "s/${emodel}/CARQ/" ${analtrack}/bml${syyyy}${mm}${dd}${CYL}.${bmodel}.${kk} >bdeck
    cat ${optrackf}/aml${syyyy}${mm}${dd}${CYL}.${bmodel}.${kk} >> bdeck
    sed "s/${emodel}/${zmodel}/" bdeck | sort >bdeck1
    sed "s/${zmodel}/${emodel}/" bdeck1 |sort -t, -k3,6 -u >${optrackab}/ab${ymdh}_${kk}.${bmodel}
    sed "s/${emodel}/BEST/" ${analtrack}/bml${syyyy}${mm}${dd}${CYL}.${bmodel}.${kk} \
        | sort -t, -k3,6 -u > ${optrackab}/bb${ymdh}_${kk}.${bmodel}
     let k=$k+1

#file_size
    fi
         let j=$j+1
done

while [ $j -le ${ist} ]
do
  lines=` cat tracka.$j |wc -l`
  if [ $lines -ge $dhr ]; then

  cat tracka.$j | awk -FNEQ '{print $1}' > tempa
  cat trackf.$j | awk -FNEQ '{print $1}' > tempf
/usr/bin/perl -pi -e 'substr($_,0,40)="        "' tempa 
/usr/bin/perl -pi -e 'substr($_,0,40)="        "' tempf 
    if [ $k -lt 10 ]; then
      kk="000${k}"
        elif [ $k -lt 100 ]; then
        kk="00${k}"
        elif [ $k -lt 1000 ]; then
        kk="0${k}"
     else
      kk="${k}"
    fi

    sed "s/        /ML, ${kk}/" tempa >${analtrack}/bml${syyyy}${mm}${dd}${CYL}.${bmodel}.${kk}
    sed "s/        /ML, ${kk}/" tempf >${optrackf}/aml${syyyy}${mm}${dd}${CYL}.${bmodel}.${kk}
    sed "s/${emodel}/CARQ/" ${analtrack}/bml${syyyy}${mm}${dd}${CYL}.${bmodel}.${kk} >bdeck
    cat ${optrackf}/aml${syyyy}${mm}${dd}${CYL}.${bmodel}.${kk} >> bdeck
    sed "s/${emodel}/${zmodel}/" bdeck | sort >bdeck1
    sed "s/${zmodel}/${emodel}/" bdeck1 |sort -t, -k3,6 -u > ${optrackab}/ab${ymdh}_${kk}.${bmodel}
    sed "s/${emodel}/BEST/" ${analtrack}/bml${syyyy}${mm}${dd}${CYL}.${bmodel}.${kk} \
        |sort -t, -k3,6 -u > ${optrackab}/bb${ymdh}_${kk}.${bmodel}
     let k=$k+1

#file_size
    fi
         let j=$j+1
done

  grep ML trak.${bmodel}.atcf_gen.glbl.${ymdh} \
      | grep "XX,  34" |sort -u > trackf_${bmodel}_temp
    file_size=`ls -l trackf_${bmodel}_temp | awk '{print $5}'`
while [ ${file_size} -gt 0 ]
  do

  sname=` head -1 trackf_${bmodel}_temp | cut -c1-8`
  grep "${sname}" trackf_${bmodel}_temp > trackf_temp
  lines=` cat trackf_temp |wc -l`
  fcsthr=` head -1 trackf_temp | cut -c65-67`
  if [ $lines -ge $dhr ]; then
   if [ $cmodel = 'gefs' -o $cmodel = 'cens' -o $cmodel = 'eens' -o $cmodel = 'fens' ]; then
/usr/bin/perl -pi -e 'substr($_,9,33)=" "' trackf_temp 
    cat trackf_temp | awk -FNEQ '{print $1}' >> trackf_${bmodel}_${ymdh}_long
    if [ $fcsthr -le '072' ]; then
    cat trackf_temp | awk -FNEQ '{print $1}' >> trackf_${bmodel}_${ymdh}
    fi
   else
/usr/bin/perl -pi -e 'substr($_,9,33)=" "' trackf_temp 
    cat trackf_temp | awk -FNEQ '{print $1}' >> trackf_${bmodel}_${ymdh}
   fi
  fi
  grep -v "${sname}" trackf_${bmodel}_temp >trackf
   mv trackf trackf_${bmodel}_temp
file_size=`ls -l trackf_${bmodel}_temp | awk '{print $5}'`
done

  ist=${storm_no}
  while [ ${ist} -lt ${TCV_no} ]
   do
/usr/bin/perl -pi -e 'substr($_,9,33)=" "' trackf.$ist
   cat trackf.$ist | awk -FNEQ '{print $1}' >> trackf_${bmodel}_${ymdh}
   cat trackf.$ist | awk -FNEQ '{print $1}' >> trackf_${bmodel}_${ymdh}_long
   let  ist=$ist+1
  done
if [ -f trackf_${bmodel}_${ymdh} ]; then
cp trackf_${bmodel}_${ymdh} ${optrack}
fi
if [ -f trackf_${bmodel}_${ymdh}_long ]; then
cp trackf_${bmodel}_${ymdh}_long ${optrack}
fi
cp ${optrackab}/bb${ymdh}_*.${bmodel} ${optrack}
cp ${optrackab}/ab${ymdh}_*.${bmodel} ${optrack}
cp ${optrackf}/aml${ymdh}.${bmodel}.* ${optrack}
cp ${analtrack}/bml${ymdh}.${bmodel}.* ${optrack}
