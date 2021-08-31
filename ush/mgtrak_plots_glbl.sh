#!/bin/sh

# This script was written by Tim Marchok
# and modified by Guang Ping Lou
# 8/20/13 - Magee - change ngps to nvgm

  set +x
# get userid
user=`whoami`
echo 'user = ' ${user}

ymdh=$1
cmodel=$2
regtype=$3

export PS4=' + mgtrak_plots_glbl.sh line $LINENO: '

  case ${cmodel} in
    gfs) cmodel=gfso;;
   nvgm) cmodel=ngx;;
  ecmwf) cmodel=emx;;
  ukmet) cmodel=ukx;;
  esac

if [ $cmodel = 'nam' -o $cmodel = 'aeperts' -o $cmodel = 'gfso' -o \
     $cmodel = 'ukx' -o $cmodel = 'ngx' -o $cmodel = 'srperts' -o \
     $cmodel = 'emx' -o $cmodel = 'eeperts' -o $cmodel = 'cmc' -o \
     $cmodel = 'ceperts' -o $cmodel = 'anal_all' -o $cmodel = 'anal_wemx' -o \
     $cmodel = 'anal_emx' -o $cmodel = 'anal_ngx' -o $cmodel = 'anal_cmc' -o \
     $cmodel = 'anal_ukx' ]; then
  echo
else
  set +x
  echo " "
  echo "!!! ERROR: The model ID you entered is not currently handled by "
  echo "!!!        this script.  Model ID must be either gfso, aeperts, nam,"
  echo "!!!        ukx, ngx, srperts, emx, eeperts, cmc or ceperts."
  echo "!!! "
  echo "!!!        You entered a model ID of --->${cmodel}<---"
  echo " "
  exit 8
  set -x
fi

#------------------------------------------------------------------
# Put the dates of the most recent 4 cycles into a text
# file in the grads directory.  This text file is used by
# the grads script when creating the plots with the most
# recent 4 cycles on the plot.

cp ${PARMtrkr}/mgplot_glbl.gs       .
cp ${PARMtrkr}/mgplot_anal_glbl.gs   .
cp ${PARMtrkr}/mgtrak.grib     .
cp ${PARMtrkr}/mgtrak.ctl      .
cp ${PARMtrkr}/mgtrak.ix       .
cp ${PARMtrkr}/latlon_bounds*  .

datefile=dates.last4.${ymdh}

$NDATE -18 $ymdh  >${datefile}.temp
$NDATE -12 $ymdh >>${datefile}.temp
$NDATE -06 $ymdh >>${datefile}.temp
echo       $ymdh >>${datefile}.temp

mv ${datefile}.temp ${datefile}

#------------------------------------------------------------------
#

export grads=/usrx/local/grads/bin/gradsc

#--------------------------------------------------
# FOR MID-LATITUDE CYCLONES, U.S. / NORTH AMERICA 
#--------------------------------------------------

if [ ${regtype} = 'ecml' -o ${regtype} = 'glbl' ]; then

  set +x 
  echo "TIMING: mgplot_glbl.sh at A = `date`"
  set -x

  ${grads} -bcl "run mgplot_glbl.gs ${ymdh} east_noam       ${cmodel} ${regtype} mslpy single"

  set +x 
  echo "TIMING: mgplot_glbl.sh at B = `date`"
  set -x

  ${grads} -bcl "run mgplot_glbl.gs ${ymdh} east_coast      ${cmodel} ${regtype} mslpy single"

  set +x 
  echo "TIMING: mgplot_glbl.sh at C = `date`"
  set -x

  ${grads} -bcl "run mgplot_glbl.gs ${ymdh} nw_atlantic_nps ${cmodel} ${regtype} mslpy single"

  set +x 
  echo "TIMING: mgplot_glbl.sh at D = `date`"
  set -x

  ${grads} -bcl "run mgplot_glbl.gs ${ymdh} nw_atlantic_ll  ${cmodel} ${regtype} mslpy single"

  set +x 
  echo "TIMING: mgplot_glbl.sh at E = `date`"
  set -x

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} nhemi         ${cmodel}  ${regtype} mslpn single"
    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} shemi         ${cmodel}  ${regtype} mslpn single"
#--------------------------------------------------
# FOR Alaska CYCLONES, Alaska region
#--------------------------------------------------

  ${grads} -bcl "run mgplot_glbl.gs ${ymdh} alaska_nps       ${cmodel} ${regtype} mslpy single"

  set +x 
  echo "TIMING: mgplot_glbl.sh at E = `date`"
  set -x

  if [ $cmodel = 'aeperts' -o $cmodel = 'srperts' -o \
       $cmodel = 'ceperts' -o $cmodel = 'eeperts' ]; then

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} east_noam       ${cmodel} ${regtype}   mslpn 4cyc"

    set +x 
    echo "TIMING: mgplot_glbl.sh at F = `date`"
    set -x

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} east_coast      ${cmodel} ${regtype}   mslpn 4cyc"

    set +x 
    echo "TIMING: mgplot_glbl.sh at G = `date`"
    set -x

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} nw_atlantic_nps ${cmodel} ${regtype}   mslpn 4cyc"

    set +x 
    echo "TIMING: mgplot_glbl.sh at H = `date`"
    set -x

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} nw_atlantic_ll  ${cmodel} ${regtype}   mslpn 4cyc"

    set +x 
    echo "TIMING: mgplot_glbl.sh at I = `date`"
    set -x

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} alaska_nps ${cmodel} ${regtype}   mslpn 4cyc"

    set +x 
    echo "TIMING: mgplot_glbl.sh at H = `date`"
  else

    ${grads} -bcl "run mgplot_anal_glbl.gs ${ymdh} east_noam       anal_${cmodel}  ${regtype} mslpn analysis"
    ${grads} -bcl "run mgplot_anal_glbl.gs ${ymdh} east_coast      anal_${cmodel}  ${regtype} mslpn analysis"
    ${grads} -bcl "run mgplot_anal_glbl.gs ${ymdh} nw_atlantic_nps anal_${cmodel}  ${regtype} mslpn analysis"
    ${grads} -bcl "run mgplot_anal_glbl.gs ${ymdh} nw_atlantic_ll  anal_${cmodel}  ${regtype} mslpn analysis"
    ${grads} -bcl "run mgplot_anal_glbl.gs ${ymdh} alaska_nps      anal_${cmodel}  ${regtype} mslpn analysis"

    ${grads} -bcl "run mgplot_anal_glbl.gs ${ymdh} east_noam       anal_all  ${regtype} mslpn analysis"
    ${grads} -bcl "run mgplot_anal_glbl.gs ${ymdh} east_coast      anal_all  ${regtype} mslpn analysis"
    ${grads} -bcl "run mgplot_anal_glbl.gs ${ymdh} nw_atlantic_nps anal_all  ${regtype} mslpn analysis"
    ${grads} -bcl "run mgplot_anal_glbl.gs ${ymdh} nw_atlantic_ll  anal_all  ${regtype} mslpn analysis"
    ${grads} -bcl "run mgplot_anal_glbl.gs ${ymdh} alaska_nps      anal_all  ${regtype} mslpn analysis"
    ${grads} -bcl "run mgplot_anal_glbl.gs ${ymdh} nhemi           anal_all  ${regtype} mslpn analysis"
    ${grads} -bcl "run mgplot_anal_glbl.gs ${ymdh} shemi           anal_all  ${regtype} mslpn analysis"

    ${grads} -bcl "run mgplot_anal_glbl.gs ${ymdh} east_noam       anal_wemx  ${regtype} mslpn analysis"
    ${grads} -bcl "run mgplot_anal_glbl.gs ${ymdh} east_coast      anal_wemx  ${regtype} mslpn analysis"
    ${grads} -bcl "run mgplot_anal_glbl.gs ${ymdh} nw_atlantic_nps anal_wemx  ${regtype} mslpn analysis"
    ${grads} -bcl "run mgplot_anal_glbl.gs ${ymdh} nw_atlantic_ll  anal_wemx  ${regtype} mslpn analysis"
    ${grads} -bcl "run mgplot_anal_glbl.gs ${ymdh} alaska_nps      anal_wemx  ${regtype} mslpn analysis"
    ${grads} -bcl "run mgplot_anal_glbl.gs ${ymdh} nhemi           anal_wemx  ${regtype} mslpn analysis"
    ${grads} -bcl "run mgplot_anal_glbl.gs ${ymdh} shemi           anal_wemx  ${regtype} mslpn analysis"


    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} east_noam       all       ${regtype} mslpn single"

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} east_coast      all       ${regtype} mslpy single"

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} nw_atlantic_nps all       ${regtype} mslpn single"

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} nw_atlantic_ll  all       ${regtype} mslpy single"

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} alaska_nps      all       ${regtype} mslpy single"

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} east_noam       all_wemx  ${regtype} mslpn single"

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} east_coast      all_wemx  ${regtype} mslpy single"

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} nw_atlantic_nps all_wemx  ${regtype} mslpn single"

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} nw_atlantic_ll  all_wemx  ${regtype} mslpy single"

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} alaska_nps      all_wemx  ${regtype} mslpy single"

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} nhemi           all_wemx  ${regtype} mslpn single"
    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} shemi           all_wemx  ${regtype} mslpn single"

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} east_noam       ${cmodel} ${regtype} mslpn 4cyc"

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} east_coast      ${cmodel} ${regtype} mslpy 4cyc"

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} nw_atlantic_nps ${cmodel} ${regtype} mslpn 4cyc"

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} nw_atlantic_ll  ${cmodel} ${regtype} mslpy 4cyc"

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} alaska_nps      ${cmodel} ${regtype} mslpn 4cyc"

  fi

fi

#--------------------------------------------------
# FOR TROPICAL CYCLONES, ATLANTIC BASIN
#--------------------------------------------------

if [ ${regtype} = 'altg' ]; then

  set +x
  echo "TIMING: mgplot_glbl.sh at beginning of ALTG = `date`"
  set -x

  ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_atl_nps ${cmodel} ${regtype} mslpn single"

  ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_atl_ll  ${cmodel} ${regtype} mslpn single"

  if [ $cmodel = 'aeperts' -o $cmodel = 'srperts' -o \
       $cmodel = 'ceperts' -o $cmodel = 'eeperts' ]; then

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_atl_nps ${cmodel} ${regtype} mslpn 4cyc"

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_atl_ll  ${cmodel} ${regtype} mslpn 4cyc"

  else
    
    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_atl_nps all       ${regtype} mslpn single"

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_atl_ll  all       ${regtype} mslpn single"

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_atl_nps all_wemx  ${regtype} mslpn single"

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_atl_ll  all_wemx  ${regtype} mslpn single"

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_atl_nps ${cmodel} ${regtype} mslpn 4cyc"

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_atl_ll  ${cmodel} ${regtype} mslpn 4cyc"

  fi

fi

set +x
echo "TIMING: mgplot_glbl.sh at end of ALTG = `date`"
set -x


#--------------------------------------------------
# FOR TROPICAL CYCLONES, EASTERN PACIFIC BASIN
#--------------------------------------------------

if [ ${regtype} = 'eptg' ]; then

  set +x
  echo "TIMING: mgplot_glbl.sh at beginning of EPTG = `date`"
  set -x

  ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_epac_nps ${cmodel} ${regtype} mslpn single"

  ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_epac_ll  ${cmodel} ${regtype} mslpn single"

  if [ $cmodel = 'aeperts' -o $cmodel = 'srperts' -o \
       $cmodel = 'ceperts' -o $cmodel = 'eeperts' ]; then

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_epac_nps ${cmodel} ${regtype} mslpn 4cyc"

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_epac_ll  ${cmodel} ${regtype} mslpn 4cyc"

  else
                                                                                                   
    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_epac_nps all       ${regtype} mslpn single"

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_epac_ll  all       ${regtype} mslpn single"
                                                                                                   
    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_epac_nps all_wemx  ${regtype} mslpn single"

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_epac_ll  all_wemx  ${regtype} mslpn single"
                                                                                                   
    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_epac_nps ${cmodel} ${regtype} mslpn 4cyc"
                                                                                             
    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_epac_ll  ${cmodel} ${regtype} mslpn 4cyc"
                                                                                                
  fi

fi

set +x
echo "TIMING: mgplot_glbl.sh at end of EPTG = `date`"
set -x


#--------------------------------------------------
# FOR TROPICAL CYCLONES, WESTERN PACIFIC BASIN
#--------------------------------------------------

if [ ${regtype} = 'wptg' ]; then

  set +x
  echo "TIMING: mgplot_glbl.sh at beginning of WPTG = `date`"
  set -x

  ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_wpac_nps ${cmodel} ${regtype} mslpn single"

  ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_wpac_ll  ${cmodel} ${regtype} mslpn single"

  if [ $cmodel = 'aeperts' -o \
       $cmodel = 'ceperts' -o $cmodel = 'eeperts' ]; then

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_wpac_nps ${cmodel} ${regtype} mslpn 4cyc"

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_wpac_ll  ${cmodel} ${regtype} mslpn 4cyc"

  else
                                                                                                   
    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_wpac_nps all       ${regtype} mslpn single"

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_wpac_ll  all       ${regtype} mslpn single"
                                                                                                   
    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_wpac_nps all_wemx  ${regtype} mslpn single"

    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_wpac_ll  all_wemx  ${regtype} mslpn single"
                                                                                                   
    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_wpac_nps ${cmodel} ${regtype} mslpn 4cyc"
                                                                                             
    ${grads} -bcl "run mgplot_glbl.gs ${ymdh} tc_wpac_ll  ${cmodel} ${regtype} mslpn 4cyc"
                                                                                                
  fi

fi

set +x
echo "TIMING: mgplot_glbl.sh at end of WPTG = `date`"
set -x
