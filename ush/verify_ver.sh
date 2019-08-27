#!/bin/sh -xf

# This script was written by Guang Ping Lou
# It seeks out analysis cyclone tracks
# and arrange them in order. 

export PS4=' + verify_ver.sh line $LINENO: '

set -x
date
ymdh=$1
cmodel=$2
regtype=$3

hostn=`hostname | cut -c1`
if [ ${hostn} = 'v' -o ${hostn} = 'm' ]; then
export ndate=/gpfs/dell1/nco/ops/nwprod/prod_util.v1.1.0/exec/ips/ndate
else
ndate=/nwprod/util/exec/ndate
fi

if [ ${cmodel} = 'sref' ]; then
   ymdh_synop=` $ndate -3 $ymdh`
 else
   ymdh_synop=$ymdh
fi

if [ ${cmodel} = 'gefs' ]; then
# Global ensemble
  pertstring=' ap01 ap02 ap03 ap04 ap05 ap06 ap07 ap08 ap09 ap10 ap11 ap12 ap13 ap14 ap15 ap16 ap17 ap18 ap19 ap20 ac00'
elif [ ${cmodel} = 'sref' ]; then
# Short range ensemble
  pertstring=' sac1 san1 sap1 sec1 sec2 sen1 sen2 sen3 sen4 sep1 sep2 sep3 sep4 snc1 snn1 snp1 src1 srn1 srp1 srn2 srp2'
elif [ ${cmodel} = 'eens' ]; then
# ECMWF ensemble
  pertstring=' ep01 ep02 ep03 ep04 ep05 ep06 ep07 ep08 ep09 ep10 ep11 ep12 ep13 ep14 ep15 ep16 ep17 ep18 ep19 ep20 ep21 ep22 ep23 ep24 ep25 en01 en02 en03 en04 en05 en06 en07 en08 en09 en10 en11 en12 en13 en14 en15 en16 en17 en18 en19 en20 en21 en22 en23 en24 en25'
elif [ ${cmodel} = 'cens' ]; then
# Canadian ensemble
  pertstring=' cc00 cp01 cp02 cp03 cp04 cp05 cp06 cp07 cp08 cp09 cp10 cp11 cp12 cp13 cp14 cp15 cp16'
else
  pertstring=${cmodel}
fi


cp ${PARMtrkr}/htcard.modelm.al htcard.model.al
fname=htcard.${cmodel}.al
emodel=`echo ${cmodel} | tr "[a-z]" "[A-Z]"`
if [ $cmodel = 'eens' -o $cmodel = 'cens' -o $cmodel = 'sref' -o $cmodel = 'gefs' ]; then
jst=1
for pert in ${pertstring} ; do
emodel2=`echo ${pert} | tr "[a-z]" "[A-Z]"`

nist=` ls ${optrack}/ab${ymdh}_*.${pert} |wc -l`
 if [ $nist -gt 0 ]; then
ist=1
while [ $ist -le ${nist} ]; do
  if [ $jst -lt 10 ]; then
    charnum1="000${jst}"
  elif [ $jst -lt 100 ]; then
    charnum1="00${jst}"
  elif [ $jst -lt 1000 ]; then
    charnum1="0${jst}"
  else
    charnum1="${jst}"
  fi

  if [ $ist -lt 10 ]; then
    charnum="000${ist}"
  elif [ $ist -lt 100 ]; then
    charnum="00${ist}"
  elif [ $ist -lt 1000 ]; then
    charnum="0${ist}"
  else
    charnum="${ist}"
  fi
    cp ${optrack}/ab${ymdh}_${charnum}.${pert} ab${ymdh}_${charnum1}
sed "s/ML, ${charnum}/ML, ${charnum1}/" ab${ymdh}_${charnum1} \
            | sed "s/${emodel2}/${emodel}/" > ab${ymdh}_${charnum1}.${cmodel}
sed "s/ML, ${charnum}/ML, ${charnum1}/" ab${ymdh}_${charnum1} \
            | sed "s/${emodel2}/${emodel}/" > ab${ymdh}_${charnum1}.${cmodel}
    cp ${optrack}/bb${ymdh}_${charnum}.${pert} bb${ymdh}_${charnum1}
sed "s/ML, ${charnum}/ML, ${charnum1}/" bb${ymdh}_${charnum1} > bb${ymdh}_${charnum1}.${cmodel}

    let ist=ist+1
    let jst=jst+1
done
fi
done
cp ab${ymdh}_*.$cmodel ${optrack}
cp bb${ymdh}_*.$cmodel ${optrack}
else
cp ${optrack}/ab${ymdh}_*.${cmodel} .
cp ${optrack}/bb${ymdh}_*.${cmodel} .

fi
ls ab${ymdh}_*.$cmodel >ablist.${cmodel}

if [ $cmodel = 'gfso' -o $cmodel = 'cens' -o $cmodel = 'sref' -o $cmodel = 'gefs' -o $cmodel = 'eens' ]; then
emodel=`echo ${cmodel} | tr "[a-z]" "[A-Z]"`
sed "s/RPLC/${emodel}/" htcard.model.al |sed "s/mod1/${emodel}/" >${fname}.temp
else
sed "s/RPLC/ ${emodel}/" htcard.model.al |sed "s/mod1/ ${emodel}/" >${fname}.temp
fi
sed "s/: techn/: 1/" ${fname}.temp >${fname}
sed "s/a//1" ablist.${cmodel} >>${fname}

${EXECtrkr}/nhcver_opr_glbl.x ${fname} .
rcc=$?
if [ $rcc -eq 0 ]; then
  cp ${fname}.out ${optrack}/${fname}_${ymdh}z.out
fi

#Now do the North America regional verification

for model in gfso ukx emx cmc ngx cens gefs eens
do
fname=htcard.${model}.na
if [ -f ${optrack}/${fname}_${ymdh_synop}z.out ]; then
 continue
else
 if [ -f ${optrack}/ab${ymdh_synop}_0001.${model} ]; then
emodel=`echo ${model} | tr "[a-z]" "[A-Z]"`
cp ${optrack}/ab${ymdh_synop}_*.${model} .
cp ${optrack}/bb${ymdh_synop}_*.${model} .
ls ab${ymdh_synop}_*.${model} > ablist1.${model}

>ablist.${model}
while read atcfrec
do
  templatN=` head -1 "$atcfrec" | cut -c41`
  templon=` head -1 "$atcfrec" | cut -c44-47 | sed 's/ //g'`
  templonE=` head -1 "$atcfrec" | cut -c48`
  templatNt=` tail -1 "$atcfrec" | cut -c41`
  templont=` tail -1 "$atcfrec" | cut -c44-47 | sed 's/ //g'`
  templonEt=` tail -1 "$atcfrec" | cut -c48`
   if [ ${templatN} == "N" ] && [ ${templonE} == "W" ] ||
            [ ${templatNt} == "N" ] && [ ${templonEt} == "W" ]; then
       if ((${templon} < 1700 )) && (( ${templon} > 200)) ||
            (( ${templont} < 1700 )) && (( ${templont} > 200)); then
        echo ${atcfrec} >> ablist.${model}
       fi
   fi
done < ablist1.${model}
#fi
if [ $model = 'gfso' -o $model = 'eens' -o $model = 'cens' -o $model = 'gefs' ]; then
emodel=`echo ${model} | tr "[a-z]" "[A-Z]"`
sed "s/RPLC/${emodel}/" htcard.model.al |sed "s/mod1/${emodel}/" >${fname}.temp
else
sed "s/RPLC/ ${emodel}/" htcard.model.al |sed "s/mod1/ ${emodel}/" >${fname}.temp
fi

sed "s/: techn/: 1/" ${fname}.temp >${fname}
sed "s/a//1" ablist.${model} >>${fname}

${EXECtrkr}/nhcver_opr_glbl.x ${fname} .
rcc=$?
if [ $rcc -eq 0 ]; then
  cp ${fname}.out ${optrack}/${fname}_${ymdh_synop}z.out
fi

fi
fi
done
#Now do the Tropical named storm verification
nist=` cat besttrack | wc -l`
if [ ${nist} -ge 1 ]; then

while read atcfrec
do
cp htcard.model.al htcard.temp
ist=1
  stmnm1=` echo  "${atcfrec}" | cut -c2-11`
  stmnm=` echo  "${atcfrec}" | cut -c2-15`
fname=htcard.${stmnm1}

    for modl in GFSO GEFS CENS EENS
    do
     mnum=` grep $modl $atcfrec |wc -l`
      if [ $mnum -ge 1 ]; then
       sed "s/mod${ist}/${modl}/" htcard.temp >${fname}
        mv ${fname} htcard.temp
        let ist=ist+1
      fi
    done
    for modl in NGX CMC EMX
    do
     mnum=` grep $modl $atcfrec |wc -l`
      if [ $mnum -ge 1 ]; then
       sed "s/mod${ist}/ ${modl}/" htcard.temp >${fname}
        mv ${fname} htcard.temp
        let ist=ist+1
      fi
    done
        let ist=ist-1
if [ $cmodel = 'gfso' -o $cmodel = 'cens' -o $cmodel = 'sref' -o $cmodel = 'gefs' -o $cmodel = 'eens' ]; then
sed "s/RPLC/${emodel}/" htcard.temp >${fname}.temp
else
sed "s/RPLC/ ${emodel}/" htcard.temp >${fname}.temp
fi
sed "s/: techn/: $ist/" ${fname}.temp >${fname}
echo "${stmnm}" >> ${fname}

${EXECtrkr}/nhcver_opr_glbl.x ${fname} .
done < besttrack

#tropical storm ends (nist)
fi

