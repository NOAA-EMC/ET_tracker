#!/bin/sh

#-----------------------------------------------------------------
# This script is used to cut apart the output files from TPC's
# verification program and grab the average track errors and the 
# errors relative to CLIPER and write all those values back out to
# another ascii file that will be used as input to a program that 
# converts the data to GrADS format.  The awk portion of the script
# puts missing values of -999 in for the 0, 60, 84 and 108h forecast 
# periods, and also puts in -999 if the 72h values are missing (in 
# case your input file is a 48h file).

# **** THIS VERSION CUTS APART THE TRACK FILES.  THERE IS ANOTHER
# **** VERSION, CALLED IVERCUT.SH, THAT CUTS APART THE INTENSITY 
# **** FILES.

set -x

homedir=${USHtrkr:-/meso/save/wx22gl/genesis_tracker_rfc/ush}
PARMtrkr=${PARMtrkr:-/meso/save/wx22gl/genesis_tracker_rfc/parm}

full_path_file=$1

ifile=` basename ${full_path_file}`
datdir=` dirname ${full_path_file}`

ifbasenum=` echo $ifile | awk -F. '{print NF}'`
let ifbasenum=ifbasenum-1
ifbase=` echo $ifile | cut -d. -f1-${ifbasenum}`

outfile="${ifbase}.dat"
gradsfile="${ifbase}.gr"
ctlfile="${ifbase}.ctl"

awk '

 BEGIN  {
    for (i=1; i<=9; i++) {
      for (j=1; j<=15; j++) {
        terr[j,i]  = -999.0
        rms[j,i]  = -999.0
        xbias[j,i]  = -999.0
        ybias[j,i]  = -999.0
        stnum[j,i]  = 0
      }
    }
  }
{
    if (match($0," average track errors") || match($0," AVERAGE TRACK ERRORS")) {
    m = 0
      getline
      while (1) {
        getline
        if (match($0,"#CASES")) {
          stnum[m,1]   = $1
          stnum[m,2]   = $2
          stnum[m,3]   = $3
          stnum[m,4]   = $4
          stnum[m,5]   = $5
          stnum[m,6]   = $6
          stnum[m,7]   = $7
          stnum[m,8]   = $8
          stnum[m,9]   = $9
         mm=m
          break
        }
        else {
         m++
          terr[m,1]   = $1
          terr[m,2]   = $2
          terr[m,3]   = $3
          terr[m,4]   = $4
          terr[m,5]   = $5
          terr[m,6]   = $6
          terr[m,7]   = $7
          terr[m,8]   = $8
          terr[m,9]   = $9
#          if (h00 == 0.0)  { h00 = -999.0 }
          if (terr[m,3] == 0.0)  { terr[m,3] = -999.0 }
          if (terr[m,4] == 0.0)  { terr[m,4] = -999.0 }
          if (terr[m,5] == 0.0)  { terr[m,5] = -999.0 }
          if (terr[m,6] == 0.0)  { terr[m,6] = -999.0 }
          if (terr[m,7] == 0.0)  { terr[m,7] = -999.0 }
          if (terr[m,8] == 0.0)  { terr[m,8] = -999.0 }
          if (terr[m,9] == 0.0)  { terr[m,9] = -999.0 }
        }
      }
    }

    if (match($0," ERROR STANDARD DEVIATION") || match($0," error standard deviation")) {
    m = 0
      getline
      while (1) {
        getline
        if (match($0,"#CASES")) {
          break
        }
        else {
         m++
          rms[m,1]   = $2
          rms[m,2]   = $3
          rms[m,3]   = $4
          rms[m,4]   = $5
          rms[m,5]   = $6
          rms[m,6]   = $7
          rms[m,7]   = $8
          rms[m,8]   = $9
#          if (h00 == 0.0)  { h00 = -999.0 }
          if (rms[m,2] == 0.0)  { rms[m,2] = -999.0 }
          if (rms[m,3] == 0.0)  { rms[m,3] = -999.0 }
          if (rms[m,4] == 0.0)  { rms[m,4] = -999.0 }
          if (rms[m,5] == 0.0)  { rms[m,5] = -999.0 }
          if (rms[m,6] == 0.0)  { rms[m,6] = -999.0 }
          if (rms[m,7] == 0.0)  { rms[m,7] = -999.0 }
          if (rms[m,8] == 0.0)  { rms[m,8] = -999.0 }
        }
      }
    }

    if (match($0," AVERAGE XBIAS") || match($0," average xbias")) {
    m = 0
      getline
      while (1) {
        getline
        if (match($0,"#CASES")) {
          break
        }
        else {
         m++
          xbias[m,1]   = $2
          xbias[m,2]   = $3
          xbias[m,3]   = $4
          xbias[m,4]   = $5
          xbias[m,5]   = $6
          xbias[m,6]   = $7
          xbias[m,7]   = $8
          xbias[m,8]   = $9
#          if (h00 == 0.0)  { h00 = -999.0 }
          if (xbias[m,2] == 0.0)  { xbias[m,2] = -999.0 }
          if (xbias[m,3] == 0.0)  { xbias[m,3] = -999.0 }
          if (xbias[m,4] == 0.0)  { xbias[m,4] = -999.0 }
          if (xbias[m,5] == 0.0)  { xbias[m,5] = -999.0 }
          if (xbias[m,6] == 0.0)  { xbias[m,6] = -999.0 }
          if (xbias[m,7] == 0.0)  { xbias[m,7] = -999.0 }
          if (xbias[m,8] == 0.0)  { xbias[m,8] = -999.0 }
        }
      }
    }

    if (match($0," AVERAGE YBIAS") || match($0," average ybias")) {
    m = 0
      getline
      while (1) {
        getline
        if (match($0,"#CASES")) {
          break
        }
        else {
         m++
          ybias[m,1]   = $2
          ybias[m,2]   = $3
          ybias[m,3]   = $4
          ybias[m,4]   = $5
          ybias[m,5]   = $6
          ybias[m,6]   = $7
          ybias[m,7]   = $8
          ybias[m,8]   = $9
#          if (h00 == 0.0)  { h00 = -999.0 }
          if (ybias[m,2] == 0.0)  { ybias[m,2] = -999.0 }
          if (ybias[m,3] == 0.0)  { ybias[m,3] = -999.0 }
          if (ybias[m,4] == 0.0)  { ybias[m,4] = -999.0 }
          if (ybias[m,5] == 0.0)  { ybias[m,5] = -999.0 }
          if (ybias[m,6] == 0.0)  { ybias[m,6] = -999.0 }
          if (ybias[m,7] == 0.0)  { ybias[m,7] = -999.0 }
          if (ybias[m,8] == 0.0)  { ybias[m,8] = -999.0 }
        }
      }
    }

    if (match($0," AVERAGE ALONG TRACK ERROR") ) {
    m = 0
      getline
      while (1) {
        getline
        if (match($0,"#CASES")) {
          break
        }
        else {
         m++
          mate[m,1]   = $2
          mate[m,2]   = $3
          mate[m,3]   = $4
          mate[m,4]   = $5
          mate[m,5]   = $6
          mate[m,6]   = $7
          mate[m,7]   = $8
          mate[m,8]   = $9
#          if (h00 == 0.0)  { h00 = -999.0 }
          if (mate[m,2] == 0.0)  { mate[m,2] = -999.0 }
          if (mate[m,3] == 0.0)  { mate[m,3] = -999.0 }
          if (mate[m,4] == 0.0)  { mate[m,4] = -999.0 }
          if (mate[m,5] == 0.0)  { mate[m,5] = -999.0 }
          if (mate[m,6] == 0.0)  { mate[m,6] = -999.0 }
          if (mate[m,7] == 0.0)  { mate[m,7] = -999.0 }
          if (mate[m,8] == 0.0)  { mate[m,8] = -999.0 }
        }
      }
    }

    if (match($0," AVERAGE CROSS TRACK ERROR") ) {
    m = 0
      getline
      while (1) {
        getline
        if (match($0,"#CASES")) {
          break
        }
        else {
         m++
          mcte[m,1]   = $2
          mcte[m,2]   = $3
          mcte[m,3]   = $4
          mcte[m,4]   = $5
          mcte[m,5]   = $6
          mcte[m,6]   = $7
          mcte[m,7]   = $8
          mcte[m,8]   = $9
#          if (h00 == 0.0)  { h00 = -999.0 }
          if (mcte[m,2] == 0.0)  { mcte[m,2] = -999.0 }
          if (mcte[m,3] == 0.0)  { mcte[m,3] = -999.0 }
          if (mcte[m,4] == 0.0)  { mcte[m,4] = -999.0 }
          if (mcte[m,5] == 0.0)  { mcte[m,5] = -999.0 }
          if (mcte[m,6] == 0.0)  { mcte[m,6] = -999.0 }
          if (mcte[m,7] == 0.0)  { mcte[m,7] = -999.0 }
          if (mcte[m,8] == 0.0)  { mcte[m,8] = -999.0 }
        }
      }
    }

    if (match($0," AVERAGE ABSOLUTE ALONG TRACK ERROR") ) {
    m = 0
      getline
      while (1) {
        getline
        if (match($0,"#CASES")) {
          break
        }
        else {
         m++
          amate[m,1]   = $2
          amate[m,2]   = $3
          amate[m,3]   = $4
          amate[m,4]   = $5
          amate[m,5]   = $6
          amate[m,6]   = $7
          amate[m,7]   = $8
          amate[m,8]   = $9
#          if (h00 == 0.0)  { h00 = -999.0 }
          if (amate[m,2] == 0.0)  { amate[m,2] = -999.0 }
          if (amate[m,3] == 0.0)  { amate[m,3] = -999.0 }
          if (amate[m,4] == 0.0)  { amate[m,4] = -999.0 }
          if (amate[m,5] == 0.0)  { amate[m,5] = -999.0 }
          if (amate[m,6] == 0.0)  { amate[m,6] = -999.0 }
          if (amate[m,7] == 0.0)  { amate[m,7] = -999.0 }
          if (amate[m,8] == 0.0)  { amate[m,8] = -999.0 }
        }
      }
    }

    if (match($0," AVERAGE ABSOLUTE CROSS TRACK ERROR") ) {
    m = 0
      getline
      while (1) {
        getline
        if (match($0,"#CASES")) {
          break
        }
        else {
         m++
          amcte[m,1]   = $2
          amcte[m,2]   = $3
          amcte[m,3]   = $4
          amcte[m,4]   = $5
          amcte[m,5]   = $6
          amcte[m,6]   = $7
          amcte[m,7]   = $8
          amcte[m,8]   = $9
#          if (h00 == 0.0)  { h00 = -999.0 }
          if (amcte[m,2] == 0.0)  { amcte[m,2] = -999.0 }
          if (amcte[m,3] == 0.0)  { amcte[m,3] = -999.0 }
          if (amcte[m,4] == 0.0)  { amcte[m,4] = -999.0 }
          if (amcte[m,5] == 0.0)  { amcte[m,5] = -999.0 }
          if (amcte[m,6] == 0.0)  { amcte[m,6] = -999.0 }
          if (amcte[m,7] == 0.0)  { amcte[m,7] = -999.0 }
          if (amcte[m,8] == 0.0)  { amcte[m,8] = -999.0 }
        }
      }
    }

 }
  END {

    for (k=1;k<=mm;k++) {
      printf (" %-4s    %6.1f %6.1f %6.1f %6.1f %6.1f -999.0 %6.1f -999.0 %6.1f -999.0 %6.1f\n",terr[k,1],terr[k,2],terr[k,3],terr[k,4],terr[k,5],terr[k,6],terr[k,7],terr[k,8],terr[k,9])
      printf (" %-4s    %6.1f %6.1f %6.1f %6.1f %6.1f -999.0 %6.1f -999.0 %6.1f -999.0 %6.1f\n",terr[k,1],rms[k,1],rms[k,2],rms[k,3],rms[k,4],rms[k,5],rms[k,6],rms[k,7],rms[k,8])
      printf (" %-4s    %6.1f %6.1f %6.1f %6.1f %6.1f -999.0 %6.1f -999.0 %6.1f -999.0 %6.1f\n",terr[k,1],xbias[k,1],xbias[k,2],xbias[k,3],xbias[k,4],xbias[k,5],xbias[k,6],xbias[k,7],xbias[k,8])
      printf (" %-4s    %6.1f %6.1f %6.1f %6.1f %6.1f -999.0 %6.1f -999.0 %6.1f -999.0 %6.1f\n",terr[k,1],ybias[k,1],ybias[k,2],ybias[k,3],ybias[k,4],ybias[k,5],ybias[k,6],ybias[k,7],ybias[k,8])
     printf (" %-4s    %6.1f %6.1f %6.1f %6.1f %6.1f -999.0 %6.1f -999.0 %6.1f -999.0 %6.1f\n",terr[k,1],mate[k,1],mate[k,2],mate[k,3],mate[k,4],mate[k,5],mate[k,6],mate[k,7],mate[k,8])
      printf (" %-4s    %6.1f %6.1f %6.1f %6.1f %6.1f -999.0 %6.1f -999.0 %6.1f -999.0 %6.1f\n",terr[k,1],mcte[k,1],mcte[k,2],mcte[k,3],mcte[k,4],mcte[k,5],mcte[k,6],mcte[k,7],mcte[k,8])
      printf (" %-4s    %6.1f %6.1f %6.1f %6.1f %6.1f -999.0 %6.1f -999.0 %6.1f -999.0 %6.1f\n",terr[k,1],amate[k,1],amate[k,2],amate[k,3],amate[k,4],amate[k,5],amate[k,6],amate[k,7],amate[k,8])
      printf (" %-4s    %6.1f %6.1f %6.1f %6.1f %6.1f -999.0 %6.1f -999.0 %6.1f -999.0 %6.1f\n",terr[k,1],amcte[k,1],amcte[k,2],amcte[k,3],amcte[k,4],amcte[k,5],amcte[k,6],amcte[k,7],amcte[k,8])
      printf (" %-4s    %6.1f %6.1f %6.1f %6.1f %6.1f -999.0 %6.1f -999.0 %6.1f -999.0 %6.1f\n",terr[k,1],stnum[mm,2],stnum[mm,3],stnum[mm,4],stnum[mm,5],stnum[mm,6],stnum[mm,7],stnum[mm,8],stnum[mm,9])

 }
  } ' ${full_path_file} > ${datdir}/${outfile}

${homedir}/wrtdat.sh ${datdir}/${outfile}

nmodel=0 
imodel=` grep CENS ${datdir}/${outfile} | wc -l`
if [ $imodel -ge 1 ]; then
 let nmodel=$nmodel+1 
fi
echo $nmodel
imodel=` grep GEFS ${datdir}/${outfile} | wc -l`
if [ $imodel -ge 1 ]; then
 let nmodel=$nmodel+1 
fi
imodel=` grep SREF ${datdir}/${outfile} | wc -l`
if [ $imodel -ge 1 ]; then
 let nmodel=$nmodel+1 
fi
imodel=` grep EENS ${datdir}/${outfile} | wc -l`
if [ $imodel -ge 1 ]; then
 let nmodel=$nmodel+1 
fi
imodel=` grep NAM ${datdir}/${outfile} | wc -l`
if [ $imodel -ge 1 ]; then
 let nmodel=$nmodel+1 
fi
imodel=` grep GFS ${datdir}/${outfile} | wc -l`
if [ $imodel -ge 1 ]; then
 let nmodel=$nmodel+1 
fi
imodel=` grep CMC ${datdir}/${outfile} | wc -l`
if [ $imodel -ge 1 ]; then
 let nmodel=$nmodel+1 
fi
imodel=` grep UKX ${datdir}/${outfile} | wc -l`
if [ $imodel -ge 1 ]; then
 let nmodel=$nmodel+1 
fi
imodel=` grep EMX ${datdir}/${outfile} | wc -l`
if [ $imodel -ge 1 ]; then
 let nmodel=$nmodel+1 
fi
imodel=` grep NGX ${datdir}/${outfile} | wc -l`
if [ $imodel -ge 1 ]; then
 let nmodel=$nmodel+1 
fi

#nmodel=` cat ${datdir}/${outfile} | wc -l`
#  ((nmodels=nmodel / 5))

sed -e "s/_FNAME/${gradsfile}/g" \
    -e "s/_NMODELS/${nmodel}/g" \
    ${PARMtrkr}/shell_opr.ctl >${datdir}/${ctlfile}
