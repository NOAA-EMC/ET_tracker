#!/bin/bash
####################################################################
# Script Name: genesis_gettrk.sh
# Purpose of Script: to generate the genesis trackers for each forecast model
# Usage: genesis_gettrk.sh pert namelist gribfile ixfile
# Mod:  8/20/13 - Magee - change ngps to nvgm, nogaps to navgem.
# Modify: 2/24/2015 -Guang Ping Lou - added option for  grib2 data input, 
#                                     change forecast intervals,
#                                     change SREF memmbers,
#                                     add FENS memmbers, etc.
#
###############################################################################
set -x
export PS4='$SECONDS + genesis_gettrk_poe.sh line $LINENO: '

if [ $# -lt 1 ]
then
   echo "Invalid argument"
   echo "usage: genesis_gettrk.sh pert"
   err_exit
fi

export pert=$1

missing_file_cnt=0
total_file_cnt=0

PERTDATA=${TRKDATA}/${pert}
if [ ! -d ${PERTDATA} ]; then mkdir -p ${PERTDATA}; fi
cd ${PERTDATA}

#/nwprod/util/ush/setup.sh
#/gpfs/dell1/nco/ops/nwprod/mag.v3.17.4/ush/setup.sh

   export gfsvitdir=${gfsvitdir:-${COMROOT}/gfs/prod}
   export namvitdir=${namvitdir:-${COMROOT}/nam/prod}
   export archsyndir=${archsyndir:-${COMROOTp1}/arch/prod/syndat}

   export gltrkdir=${gltrkdir:-$COMGLTRK}
#   export gltrkdir=/meso/save/Guang.Ping.Lou/com/hur/prod/global
   export savedir=${gltrkdir}/${syyyy}


if [ ! -d ${savedir} ];   then mkdir -p ${savedir}; fi

   export TPCATdir=/tpcprd/atcf

   export homesyndir=${homesyndir:-/nwprod/util}
   export exectrkdir=${exectrkdir:-${EXECtrkr}}
#   export exectrkdir=/meso/save/Guang.Ping.Lou/genesis_tracker.v3.4.0/exec
   export ushtrkdir=${ushtrkdir:-${homesyndir}/ush}

      echo " shell is  " $shell
export ndate=ndate
export wgrib=wgrib
export gix=grbindex
export cgb=copygb
export wgrib2=wgrib2
export g2ix=grb2index
export cgb2=copygb2
export cnvgrib=cnvgrib

export gribver=${gribver:-2}

   cmodel=`echo ${cmodel} | tr "[A-Z]" "[a-z]"`

       if [ ${gribver} -eq 1 ]; then
wgrib_parmlist=" UGRD:850 UGRD:700 UGRD:500 VGRD:850 VGRD:700 VGRD:500 SurfaceU SurfaceV ABSV:850 ABSV:700 PRMSL:MSL HGT:900 HGT:850 HGT:800 HGT:750 HGT:700 HGT:650 HGT:600 HGT:550 HGT:500 HGT:450 HGT:400 HGT:350 HGT:300 "
       else
wgrib_parmlist=" UGRD:850 UGRD:700 UGRD:500 VGRD:850 VGRD:700 VGRD:500 SurfaceU SurfaceV ABSV:850 ABSV:700 PRMSL:mean HGT:900 HGT:850 HGT:800 HGT:750 HGT:700 HGT:650 HGT:600 HGT:550 HGT:500 HGT:450 HGT:400 HGT:350 HGT:300 "
       fi
wgrib_ec_hires_parmlist=" GH:850 GH:700 U:850 U:700 U:500 V:850 V:700 V:500 10U:sfc 10V:sfc MSL:sfc "
wgrib_phase_parmlist="HGT:900|HGT:850|HGT:800|HGT:750|HGT:700|HGT:650|HGT:600|HGT:550|HGT:500|HGT:450|HGT:400|HGT:350|HGT:300"
wgrib_ec_phase_parmlist=" GH:900|GH:850|GH:800|GH:750|GH:700|GH:650|GH:600|GH:550|GH:500|GH:450|GH:400|GH:350|GH:300 "

   export maxtime=65    # Max number of forecast time levels

#----------------------------------------------------------------#
#
#    --- Define data directories and data file names ---
#               
# Convert the input model to lowercase letters and check to see 
# if it's a valid model, and assign a model ID number to it.  
# This model ID number is passed into the Fortran program to 
# let the program know what set of forecast hours to use in the 
# ifhours array.  Also, set the directories for the operational 
# input GRIB data files and create templates for the file names.
# While only 1 of these sets of directories and file name 
# templates is used during a particular run of this script, 
# "gfsvitdir" is used every time, because that is the directory 
# that contains the error-checked TC vitals file that Steve Lord 
# produces, and so it is included after the case statement.
#
# NOTE: The varible PDY is now defined within the J-Jobs that
# call this script.  Therefore there is no reason to do this
# here.
#
# NOTE: The script that processes the ECMWF data defines PDY as
# the current day, and in this script we need PDY to be 
# yesterday's date (for the ecmwf ONLY).  So instead, the ecmwf
# script will pass the variable PDYm1 to this script, and in the
# case statement below we change that to PDY.
#
# NOTE: Do NOT try to standardize this script by changing all of 
# these various data directories' variable names to be all the 
# same, such as "datadir".  As you'll see in the data cutting 
# part of this script below, different methods are used to cut 
# apart different models, thus it is important to know the 
# difference between them....
#----------------------------------------------------------------#

   case ${cmodel} in 

      gfs) set +x                                         ;
         echo " "; echo " ++ operational GFS chosen"      ;
         echo " "                                         ;
         set -x                                           ;
         globaldir=$COMIN                                 ;
       if [ ${gribver} -eq 1 ]; then
         globalgfile=gfs.t${CYL}z.pgrbf
         globaligfile=gfs.t${CYL}z.pgrbif
         fcsthrs=' 00 06 12 18 24 30 36 42 48 54 
                   60 66 72 78 84 90 96 102 108 114
                   120 126 132 138 144
                   150 156 162 168 174 180  99  99  99  99
                   99  99  99  99  99  99  99  99  99  99
                   99  99  99  99  99  99  99  99  99  99
                   99  99  99  99  99  99  99  99  99  99' 
       else
         globalgfile=gfs.t${CYL}z.pgrb2.1p00.f
         globaligfile=gfs.t${CYL}z.pgrb2.1p00.f
         fcsthrs=' 000 006 012 018 024 030 036 042 048 054 
                   060 066 072 078 084 090 096 102 108 114
                   120 126 132 138 144
                   150 156 162 168 174 180  99  99  99  99
                   99  99  99  99  99  99  99  99  99  99
                   99  99  99  99  99  99  99  99  99  99
                   99  99  99  99  99  99  99  99  99  99'
       fi                                                 ;
         fcstlen=180                                      ;
         atcfnum=15                                       ;
         atcfname="gfso"                                  ;
         atcfout="gfso"                                    ;
         grmodel="gfso"                                   ;
         atcf_vit=${atcfout}                              ;
         mslpthresh=0.0015                                ;
         v850thresh=1.5000                                ;
         modtyp='global'                                  ;
# g2_jpdtn sets the variable that will be used as "JPDTN" for
# the call to getgb2, if gribver=2.  jpdtn=1 for ens data,
# jpdtn=0 for deterministic data.
         g2_jpdtn=0                                      ;
         model=1                                         ;;

      ukmet) set +x                                       ; 
         echo " "; echo " ++ operational UKMET chosen"    ;
         echo " "                                         ;
         set -x                                           ;
         ukmetdir=${COMIN}                                ;
         ukmetgfile=ukm.${PDY}${CYL}                      ;
         ukmetifile=                                      ;
         fcstlen=144                                      ;
         fcsthrs=' 00 06 12 18 24  30  36  42  48  54  60  66
                   72 78 84 90 96  102 108 114 120 126 132 138 144
                   99 99 99 99 99 99 99 99 99 99 99 99 99
                   99 99 99 99 99 99 99 99 99 99 99 99 99
                   99 99 99 99 99 99 99 99 99 99 99 99 99 99' ;
         atcfnum=17                                       ;
         atcfname="ukx "                                  ;
         atcfout="ukx"                                    ;
         grmodel="ukx"                                   ;
         atcf_vit=${atcfout}                              ;
         mslpthresh=0.0015                                ;
         v850thresh=1.5000                                ;
         modtyp='global'                                  ;
         g2_jpdtn=0                                       ;
         model=3                                         ;;

      ecmwf) set +x                                       ;
         echo " "; echo " ++ operational ECMWF chosen"    ;
         echo " "                                         ;
         set -x                                           ;
         ecmwfdir=$COMIN                                  ;
         ecmwfgfile=                                      ;
         ecmwfifile=                                      ;
         fcstlen=180                                      ;
         fcsthrs=' 00 06 12 18 24 30  36  42  48  54  60  66
                   72 78 84 90 96 102 108 114 120 126 132 138 
                   144 150 156 162 168 174 180 
                   99 99 99 99 99 99 99 99 99 99 99 99
                   99 99 99 99 99 99 99 99 99 99 99
                   99 99 99 99 99 99 99 99 99 99 99 ' ;
         atcfnum=19                                       ;
         atcfname="emx "                                  ;
         atcfout="emx"                                    ;
         grmodel="emx"                                   ;
         atcf_vit=${atcfout}                              ;
         mslpthresh=0.0015                                ;
         v850thresh=1.5000                                ;
         modtyp='global'                                  ;
         g2_jpdtn=0                                       ;
         model=4                                         ;;

      nam) set +x                                         ;
         echo " "; echo " ++ operational NAM chosen";
         echo " "                                         ;
         set -x                                           ;
         namdir=$COMIN                                    ;
       if [ ${gribver} -eq 1 ]; then
         namgfile=nam.t${CYL}z.awip32
         namifile=nam.t${CYL}z.awip32i 
       else
         namgfile=nam.t${CYL}z.awip32
         namifile=nam.t${CYL}z.awip32i
       fi
         fcstlen=84                                       ;
         fcsthrs=' 00 06 12 18 24 30 36 42 48 54 60 66 72 78
                   84 99 99 99 99 99 99 99 99 99 99 99 99 99
                   99 99 99 99 99 99 99 99 99 99 99 99 99 99
                   99 99 99 99 99 99 99 99 99 99 99 99 99 99
                   99 99 99 99 99 99 99 99 99';
         atcfnum=73                                       ;
         atcfname="nam "                                  ;
         atcfout="nam"                                    ;
         grmodel="nam"                                   ;
         atcf_vit=${atcfout}                              ;
         mslpthresh=0.0015                                ;
         v850thresh=1.5000                                ;
         export PHASEFLAG=y                                 ;
         export PHASE_SCHEME=cps                            ;
         modtyp='regional'                                ;
         g2_jpdtn=0                                       ;
         model=6                                         ;;

      nvgm) set +x                                        ;
         echo " "; echo " ++ operational NAVGEM chosen"   ;
         echo " "                                         ;
         set -x                                           ;
         nvgmdir=${COMIN}                                 ;
         nvgmgfile=US058GMET-OPSbd2.NAVGEM                ;
         nvgmifile=
         fcstlen=180                                      ;
         fcsthrs=' 000 006 012 018 024 030 036 042 048 054 060 066
                   072 084 096 108 120 132 144 156 168 180
                   99 99 99 99 99 99 99 99 99
                   99 99 99 99 99 99 99 99 99 99 99 99
                   99 99 99 99 99 99 99 99 99 99 99
                   99 99 99 99 99 99 99 99 99 99 99 ' ;
         atcfnum=29                                       ;
         atcfname="ngx "                                  ;
         atcfout="ngx"                                    ;
         grmodel="ngx"                                   ;
         atcf_vit=${atcfout}                              ;
         mslpthresh=0.0015                                ;
         v850thresh=1.5000                                ;
         export PHASEFLAG=y                                 ;
         export PHASE_SCHEME=cps                            ;
         modtyp='global'                                  ;
         g2_jpdtn=0                                       ;
         model=7                                         ;; 

      gdas) set +x                                        ;
         echo " "; echo " ++ operational GDAS (relocation) chosen";
         echo " "                                         ;
         set -x                                           ;
         gdasdir=$COMIN                                   ;
         gdasgfile=gdas1.t${CYL}z.pgrbf                   ;
         gdasifile=gdas1.t${CYL}z.pgrbif                  ;
         fcstlen=9                                        ;
         fcsthrs=' 00 03 06 09 99 99 99 99 99 99 99 99 99 99
                   99 99 99 99 99 99 99 99 99 99 99 99 99 99
                   99 99 99 99 99 99 99 99 99 99 99 99 99 99
                   99 99 99 99 99 99 99 99 99 99 99 99 99 99
                   99 99 99 99 99 99 99 99 99' ;
         atcfnum=72                                       ;
         atcfname="gdas"                                  ;
         atcfout="gdas"                                   ;
         grmodel="gdas"                                   ;
         atcf_vit=${atcfout}                              ;
         mslpthresh=0.0015                                ;
         v850thresh=1.5000                                ;
         export PHASEFLAG=y                               ;      
         export PHASE_SCHEME=cps                          ;
         modtyp='global'                                  ;
         g2_jpdtn=0                                       ;
         model=8                                         ;;

      gfdl) set +x                                        ;
         echo " "; echo " ++ operational GFDL chosen"     ;
         echo " "                                         ;
         set -x                                           ;
         gfdldir=$COMIN                                   ;
         gfdlgfile=${stormenv}.${PDY}${CYL}.grib3rd.f     ;
         gfdlifile=${stormenv}.${PDY}${CYL}.grib3rd.if    ;
         fcstlen=126                                      ;
         fcsthrs=' 00 06 12 18 24 30 36 42 48 54 60 66 72 78
                   84 90 96 102 108 114 120 126 99 99 99 99
                   99 99 99 99 99 99 99 99 99 99 99 99 99 99
                   99 99 99 99 99 99 99 99 99 99 99 99 99 99
                   99 99 99 99 99 99 99 99 99 99 99';
         stormid=${atcfid}                                ;
         atcfnum=81                                       ;
         atcfname="gfdt"                                  ;
         atcfout="gfdt"                                   ;
         grmodel="gfdt"                                   ;
         atcf_vit=${atcfout}                              ;
         mslpthresh=0.0015                                ;
         v850thresh=1.5000                                ;
         export PHASEFLAG=y                               ; 
         export PHASE_SCHEME=cps                          ;
         modtyp='regional'                                ;
         g2_jpdtn=0                                       ;
         model=9                                         ;;

      gefs) set +x                                         ;
         echo " "                                         ;
         echo " ++ operational ensemble member ${pert} chosen";
         pert=` echo ${pert} | tr '[A-Z]' '[a-z]'`        ;
         PERT=` echo ${pert} | tr '[a-z]' '[A-Z]'`        ;
         echo " "                                         ;
         set -x                                           ;
         ensdir=$COMIN                                    ;
       if [ ${gribver} -eq 1 ]; then
         ensgfile=ge${pert}.t${CYL}z.pgrbaf               ;
         ensifile=ge${pert}.t${CYL}z.pgrbaif              ;
        else
         ensgfile=ge${pert}.t${CYL}z.pgrb2af               ;
         ensifile=ge${pert}.t${CYL}z.pgrb2aif              ;
       fi
         fcstlen=384                                      ;
         fcsthrs=' 00 06 12 18 24 30 36 42 48 54 60 66 72 78
                   84 90 96 102 108 114 120 126 132 138 144
                   150 156 162 168 174 180 186 192 198 204 210
                   216 222 228 234 240 246 252 258 264 270 276
                   282 288 294 300 306 312 318 324 330 336 342
                   348 354 360 366 372 378 384';
         atcfnum=91                                       ;
         pert_posneg=` echo "${pert}" | cut -c1-1`        ;
         pert_num=`    echo "${pert}" | cut -c2-3`        ;
         atcfname="a${pert_posneg}${pert_num}"            ;
         atcfout="a${pert_posneg}${pert_num}"             ;
         grmodel="a${pert_posneg}${pert_num}"             ;
         atcf_vit="gfso"                                  ;
         mslpthresh=0.0015                                ;
         v850thresh=1.5000                                ;
         export PHASEFLAG=y                               ;
         export PHASE_SCHEME=cps                          ;
         modtyp='global'                                  ;
         g2_jpdtn=1                                       ;
         model=10                                        ;;

 ensr) set +x                                          ;
       echo " "; echo " ++ ensemble RELOCATION chosen for member ${pert}";
       echo " "                                         ;
       set -x                                           ;
       if [ ${ensclen} -eq 6 ]; then                      
         fcstlen=6                                       ;
         fcsthrs=' 00 06 99 99 99 99 99 99 99 99 99 99 99 99
                   99 99 99 99 99 99 99 99 99 99 99 99 99 99
                   99 99 99 99 99 99 99 99 99 99 99 99 99 99
                   99 99 99 99 99 99 99 99 99 99 99 99 99 99
                   99 99 99 99 99 99 99 99 99' ;
         atcfname="y${pert}"                             ;
         atcfout="y${pert}"                              ;
         grmodel="y${pert}"                              ;
       else
         fcstlen=24                                      ;
         fcsthrs=' 24 99 99 99 99 99 99 99 99 99 99 99 99 99
                   99 99 99 99 99 99 99 99 99 99 99 99 99 99
                   99 99 99 99 99 99 99 99 99 99 99 99 99 99
                   99 99 99 99 99 99 99 99 99 99 99 99 99 99
                   99 99 99 99 99 99 99 99 99' ;
         atcfname="z${pert}"                             ;
         atcfout="z${pert}"                              ;
         grmodel="z${pert}"                              ;
       fi
       atcf_vit=${atcfout}                              ;
       pert=` echo ${pert} | tr '[A-Z]' '[a-z]'`        ;
       PERT=` echo ${pert} | tr '[a-z]' '[A-Z]'`        ;
       atcfnum=72                                       ;
       ensrdir=${COMIN}                                 ;
#      ensrgfile=en${pert}.t${CYL}z.pgrbf               ;
       ensrgfile=en${pert}.t${CYL}z.reloc.pgrbf         ;
#      ensrifile=en${pert}.t${CYL}z.pgrbif              ;
       ensrifile=en${pert}.t${CYL}z.reloc.pgrbif        ;
       COM={ensrdir}                                    ;
       mslpthresh=0.0015                                ;
       v850thresh=1.5000                                ;
       modtyp='global'                                  ;
       model=11                                        ;;

  aear) set +x                                          ;
       echo " "; echo " ++ GFS ensemble control analysis (for relocation purposes) chosen";
       echo " "                                         ;
       set -x                                           ;
       fcstlen=0                                       ;
       fcsthrs=' 00 99 99 99 99 99 99 99 99 99 99 99 99 99
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99
                 99 99 99 99 99 99 99 99 99' ;
       atcfname="aear"                                  ;
       atcfout="aear"                                   ;
       grmodel="aear"                                   ;
       atcf_vit=${atcfout}                              ;
       aeardir=${COMIN}                                 ;
       aeargfile=en${pert}.t${CYL}z.reloc.pgrbf         ;
       aearifile=en${pert}.t${CYL}z.reloc.pgrbif        ;
       COM={aeardir}                                    ;
       mslpthresh=0.0020                                ;
       v850thresh=2.0000                                ;
       modtyp='global'                                  ;
       model=12                                        ;;

    sref) set +x                                        ;
       echo " "; echo " ++ operational SREF ensemble member ${pert} chosen"     ;
       echo " "                                         ;
       set -x                                           ;
       pert=` echo ${pert} | tr '[A-Z]' '[a-z]'`        ;
       PERT=` echo ${pert} | tr '[a-z]' '[A-Z]'`        ;
       srtype=` echo ${pert} | cut -c1-2`               ;
       srpertnum=` echo ${pert} | cut -c3-4`            ;
     if [ ${gribver} -eq 1 ]; then
       if [ ${srtype} = 'sa' ]; then
         # ARW members
         if [ ${srpertnum} = 'c1' ]; then
           srefgfile=sref_em.t${CYL}z.pgrb221.ctl.f
         else
           srefgfile=sref_em.t${CYL}z.pgrb221.${srpertnum}.f
         fi
       elif [ ${srtype} = 'sb' ]; then
         # NMB members
         if [ ${srpertnum} = 'c1' ]; then
           srefgfile=sref_nmb.t${CYL}z.pgrb221.ctl.f
         else
           srefgfile=sref_nmb.t${CYL}z.pgrb221.${srpertnum}.f
         fi
       elif [ ${srtype} = 'sn' ]; then
         # NMM members
         if [ ${srpertnum} = 'c1' ]; then
           srefgfile=sref_nmm.t${CYL}z.pgrb221.ctl.f
         else
           srefgfile=sref_nmm.t${CYL}z.pgrb221.${srpertnum}.f
         fi
       fi

      else

       if [ ${srtype} = 'sa' ]; then
         # ARW members
         if [ ${srpertnum} = 'c1' ]; then
           srefgfile=sref_arw.t${CYL}z.pgrb221.ctl.f
         else
           srefgfile=sref_arw.t${CYL}z.pgrb221.${srpertnum}.f
         fi
       elif [ ${srtype} = 'sb' ]; then
         # NMB members
         if [ ${srpertnum} = 'c1' ]; then
           srefgfile=sref_nmb.t${CYL}z.pgrb221.ctl.f
         else
           srefgfile=sref_nmb.t${CYL}z.pgrb221.${srpertnum}.f
         fi
       else
         set +x
         echo " "
         echo "!!! ERROR: SREF MEMBER NOT RECOGNIZED. "
         echo "!!!        USER INPUT SREF MEMBER = --->${pert}<---"
         echo " "
         set -x
         err_exit " FAILED ${jobid} - ERROR INTERPOLATING SREF DATA IN TRACKER SCRIPT - ABNORMAL EXIT"
         exit 8
       fi
     fi
       srefdir=$COMIN                                   ;
       fcstlen=84                                       ;
       fcsthrs=' 00 06 12 18 24 30 36 42 48 54 60 66 72 78
                 84 99 99 99 99 99 99 99 99 99 99 99 99 99
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99
                 99 99 99 99 99 99 99 99 99';
       atcfnum=92                                       ;
       atcfname="${pert}"                               ;
       atcfout="${pert}"                                ;
       atcf_vit="sbc1"                                  ;
       modtyp='regional'                                ;
       mslpthresh=0.0015                                ;
       v850thresh=1.5000                                ;
       g2_jpdtn=1                                       ;
       model=13                                        ;;

  eens) set +x                                           ;
       echo " "; echo " ++ ECMWF ensemble member ${pert} chosen"     ;
       pert=` echo ${pert} | tr '[A-Z]' '[a-z]'`        ;
       PERT=` echo ${pert} | tr '[a-z]' '[A-Z]'`        ;
       echo " "                                         ;
       set -x                                           ;
       ecedir=${ECEDIRIN}/$CYL/pgrba                          ;
       ecegfile=                                        ;
       eceifile=                                        ;
       fcstlen=240                                      ;
       fcsthrs=' 00 12 24 36 48 60 72 84 96 108 120 132 144
                 156 168 180 192 204 216 228 240 99 99 99 99
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99
                 99 99 99 99 99 99 99 99 99 99' ;
       atcfnum=91                                       ;
       pert_posneg=` echo "${pert}" | cut -c1-1`        ;
       pert_num=`    echo "${pert}" | cut -c2-3`        ;
       atcfname="e${pert_posneg}${pert_num}"            ;
       atcfout="e${pert_posneg}${pert_num}"             ;
       atcf_vit="emx"                                   ;
       mslpthresh=0.0015                                ;
       v850thresh=1.50000                                ;
       modtyp='global'                                  ;
       g2_jpdtn=1                                       ;
       model=21                                        ;;

  fens) set +x                                           ;
       echo " "; echo " ++ FNMOC ensemble member ${pert} chosen"     ;
       pert=` echo ${pert} | tr '[A-Z]' '[a-z]'`        ;
       PERT=` echo ${pert} | tr '[a-z]' '[A-Z]'`        ;
       echo " "                                         ;
       set -x                                           ;
       fensdir=${COMIN}                                 ;
       fcstlen=180                                      ;
       fcsthrs='  00  06  12  18  24  30  36  42  48  54  60  66  72  78
                  84  90  96 102 108 114 120 126 132 138 144
                 150 156 162 168 174 180  99  99  99  99  99
                  99  99  99  99  99  99  99  99  99  99  99
                  99  99  99  99  99  99  99  99  99  99  99
                  99  99  99  99  99  99  99';
       if [ ${gribver} -eq 2 ]; then
       fcsthrs='  000  006  012  018  024  030  036  042  048  
                  054  060  066  072  078  084  090  096  102
                  108  114  120  126  132  138  144  150  156
                  162  168  174  180  99   99   99   99   99
                  99  99  99  99  99  99  99  99  99  99  99
                  99  99  99  99  99  99  99  99  99  99  99
                  99  99  99  99  99  99  99';
       fi
       atcfnum=91                                       ;
       pert_posneg=` echo "${pert}" | cut -c1-1`        ;
       pert_num=`    echo "${pert}" | cut -c2-3`        ;
       atcfname="f${pert_posneg}${pert_num}"            ;
       atcfout="f${pert_posneg}${pert_num}"             ;
       atcf_vit="ngx"                                   ;
       mslpthresh=0.0015                                ;
       v850thresh=1.5000                                ;
       modtyp='global'                                  ;
       g2_jpdtn=1                                       ;
       model=23                                        ;;

   cmc) set +x                                        ;
         echo " "; echo " ++ operational Canadian global model chosen"   ;
         echo " "                                         ;
         set -x                                           ;
         cmcdir=$COMIN                                    ;
       if [ ${gribver} -eq 1 ]; then
         cmcgfile=glb_${CYL}_         
         cmcifile=nonexistant        
         else
         cmcgfile=CMC_glb_latlon.24x.24_${PDY}${CYL}_P
         cmcifile=nonexistant          
       fi
         if [ ! -d $COM ]; then mkdir -p $COM ; fi        ;
         fcstlen=180                                      ;
         fcsthrs=' 000 006 012 018 024 030 036 042 048 054
                   060 066 072 078 084 090 096 102 108 114 
                   120 126 132 138 144 150 156 162 168 174
                   180 99 99 99 99 99 99 99
                   99 99 99 99 99 99 99 99 99 99 99 99 99 99
                   99 99 99 99 99 99 99 99 99 99 99 99' ;
         atcfnum=39                                       ;
         atcfname="cmc "                                  ;
         atcfout="cmc"                                    ;
         grmodel="cmc"                                   ;
         atcf_vit=${atcfout}                              ;
         mslpthresh=0.0015                                ;
         v850thresh=1.5000                                ;
         export PHASEFLAG=y                               ;
         export PHASE_SCHEME=cps                          ;
         modtyp='global'                                  ;
         g2_jpdtn=0                                       ;
         model=15                                         ;;

  cens) set +x                                            ;
       echo " "; echo " ++ Canadian ensemble member ${pert} chosen";
       pert=` echo ${pert} | tr '[A-Z]' '[a-z]'`        ;
       PERT=` echo ${pert} | tr '[a-z]' '[A-Z]'`        ;
       echo " "                                         ;
       set -x                                           ;
       ccedir=${COMIN}                                  ;
       if [ ${gribver} -eq 1 ]; then
       ccegfile=cmc_ge${pert}.t${CYL}z.pgrbaf           ;
       cceifile=does_not_exist                          ;
       fcstlen=180                                      ;
       fcsthrs=' 00 06 12 18 24 30 36 42 48 54 60 66 72 78
                 84 90 96 102 108 114 120 126 132 138 144
                 150 156 162 168 174 180  99  99  99  99  99
                  99  99  99  99  99  99  99  99  99  99  99
                  99  99  99  99  99  99  99  99  99  99  99
                  99  99  99  99  99  99  99';
         else
         ccegfile=cmc_ge${pert}.t${CYL}z.pgrb2a.0p50.f    ;
         cceifile=nonexistant                             ;
         if [ ! -d $COM ]; then mkdir -p $COM ; fi        ;
         fcstlen=384                                      ;
         fcsthrs=' 000 006 012 018 024 030 036 042 048 054 060
                   066 072 078 084 090 096 102 108 114 120 126
                   132 138 144 150 156 162 168 174 180 186 192
                   198 204 210 216 222 228 234 240 246 252 258
                   264 270 276 282 288 294 300 306 312 318 324
                   330 336 342 348 354 360 366 372 378 384';
       fi
       atcfnum=91                                       ;
       pert_posneg=` echo "${pert}" | cut -c1-1`        ;
       pert_num=`    echo "${pert}" | cut -c2-3`        ;
       atcfname="c${pert_posneg}${pert_num}"            ;
       atcfout="c${pert_posneg}${pert_num}"             ;
       atcf_vit="cmc"                                   ;
       mslpthresh=0.0015                                ;
       v850thresh=1.5000                                ;
       export PHASEFLAG=y                               ;
       export PHASE_SCHEME=cps                          ;
       modtyp='global'                                  ;
       g2_jpdtn=1                                       ;
       model=16                                        ;;

  *) set +x                                            ;
         echo " "                                         ;
         echo " !!! Model selected is not recognized."    ;
         echo " Model= --> ${cmodel} <-- ... Please submit the script again...";
         echo " "                                         ;
         set -x                                           ;
         err_exit " FAILED ${jobid} -- UNKNOWN cmodel IN TRACKER SCRIPT -\
    ABNORMAL EXIT";;

   esac

#------------------------------------------------------------------------#
#
#      --------  TC Vitals processing   --------
#
# Check Steve Lord's operational tcvitals file to see if any 
# vitals records were processed for this time by his system.  
# If there were, then you'll find a file in /com/gfs/prod/gfs.yymmdd 
# with the vitals in it.  Also check the raw TC Vitals file in
# /com/arch/prod/syndat , since this may contain storms that Steve's 
# system ignored (Steve's system will ignore all storms that are 
# either over land or very close to land);  We still want to track 
# these inland storms, AS LONG AS THEY ARE NHC STORMS (don't 
# bother trying to track inland storms that are outside of NHC's 
# domain of responsibility -- we don't need that info).
# UPDATE 5/12/98 MARCHOK: The script is updated so that for the
#   global models, the gfs directory is checked for the error-
#   checked vitals file, while for the regional models, the 
#   nam directory is checked for that file.
#------------------------------------------------------------------------#

# First check to see if the vitals file is in gfsvitdir or not.  If 
# it's not, then run Hua-Lu's ftp script to get the file from one
# of the other machines.  If it's still not there, then no big 
# deal; this script will exit just a little further down once it
# realizes there are not any storms to process.

  if [ ${cmodel} = 'gfs' -o ${cmodel} = 'nam' -o ${cmodel} = 'gefs' ]; then
   d6ago_ymdh=` ${ndate} -6 ${PDY}${CYL}`
   elif [ ${cmodel} = 'sref' ]; then
   d6ago_ymdh=` ${ndate} -9 ${PDY}${CYL}`
   else
   d6ago_ymdh=` ${ndate} -12 ${PDY}${CYL}`
  fi
   d6ago_4ymd=` echo ${d6ago_ymdh} | cut -c1-8`
   d6ago_ymd=` echo ${d6ago_ymdh} | cut -c3-8`
   d6ago_hh=`  echo ${d6ago_ymdh} | cut -c9-10`
   d6ago_str="${d6ago_ymd} ${d6ago_hh}00"

  if [ ${cmodel} = 'gfs' -o ${cmodel} = 'nam' -o ${cmodel} = 'gefs' ]; then
   d6ahead_ymdh=` ${ndate} 6 ${PDY}${CYL}`
  elif [ ${cmodel} = 'sref' ]; then
   d6ahead_ymdh=` ${ndate} 9 ${PDY}${CYL}`
   else
   d6ahead_ymdh=` ${ndate} 12 ${PDY}${CYL}`
  fi
   d6ahead_4ymd=` echo ${d6ahead_ymdh} | cut -c1-8`
   d6ahead_ymd=` echo ${d6ahead_ymdh} | cut -c3-8`
   d6ahead_hh=`  echo ${d6ahead_ymdh} | cut -c9-10`
   d6ahead_str="${d6ahead_ymd} ${d6ahead_hh}00"

   if [ ${modtyp} = 'global' ]; then

      synvitdir=${gfsvitdir}/gfs.$PDY/${CYL}
      synvitfile=gfs.t${CYL}z.syndata.tcvitals.tm00
      synvit6ago_dir=${gfsvitdir}/gfs.${d6ago_4ymd}/${d6ago_hh}
      synvit6ago_file=gfs.t${d6ago_hh}z.syndata.tcvitals.tm00
      synvit6ahead_dir=${gfsvitdir}/gfs.${d6ahead_4ymd}/${d6ahead_hh}
      synvit6ahead_file=gfs.t${d6ahead_hh}z.syndata.tcvitals.tm00

   else
      synvitdir=${namvitdir}/nam.$PDY
      synvitfile=nam.t??z.syndata.tcvitals.tm00
      synvit6ago_dir=${namvitdir}/nam.${d6ago_4ymd}
      synvit6ago_file=nam.t${d6ago_hh}z.syndata.tcvitals.tm00
      synvit6ahead_dir=${namvitdir}/nam.${d6ahead_4ymd}
      synvit6ahead_file=nam.t${d6ahead_hh}z.syndata.tcvitals.tm00

   fi

   set +x
   echo " "
   echo "              -----------------------------"
   echo " "
   echo " Now sorting and updating the TC Vitals file.  Please wait...."
   echo " "
   set -x

set -xa

   dnow_str="${symd} ${CYL}00"
################################################
# In order to include HPC tcvitals in SREF tracking, 
# change timing 
  if [ ${cmodel} = 'sref' ]; then
   d6ago_ymdh=` ${ndate} -9 ${PDY}${CYL}`
   d6ago_4ymd=` echo ${d6ago_ymdh} | cut -c1-8`
   d6ago_ymd=` echo ${d6ago_ymdh} | cut -c3-8`
   d6ago_hh=`  echo ${d6ago_ymdh} | cut -c9-10`
   d6ago_str="${d6ago_ymd} ${d6ago_hh}00"

   d6ahead_ymdh=` ${ndate} 3 ${PDY}${CYL}`
   d6ahead_4ymd=` echo ${d6ahead_ymdh} | cut -c1-8`
   d6ahead_ymd=` echo ${d6ahead_ymdh} | cut -c3-8`
   d6ahead_hh=`  echo ${d6ahead_ymdh} | cut -c9-10`
   d6ahead_str="${d6ahead_ymd} ${d6ahead_hh}00"

   d3ago_ymdh=` ${ndate} -3 ${PDY}${CYL}`
   d3ago_ymd=` echo ${d3ago_ymdh} | cut -c3-8`
   d3ago_hh=`  echo ${d3ago_ymdh} | cut -c9-10`
   dnow_str="${d3ago_ymd} ${d3ago_hh}00"
  fi
################################################

      grep -h "${d6ago_str}" ${synvit6ago_dir}/${synvit6ago_file}        \
                  >${PERTDATA}/tmpsynvit.${atcfout}.${regtype}.${PDY}${CYL}
      grep -h "${dnow_str}"  ${synvitdir}/${synvitfile}                  \
                 >>${PERTDATA}/tmpsynvit.${atcfout}.${regtype}.${PDY}${CYL}
      grep -h "${d6ahead_str}" ${synvit6ahead_dir}/${synvit6ahead_file}  \
                 >>${PERTDATA}/tmpsynvit.${atcfout}.${regtype}.${PDY}${CYL}

# Take the vitals from Steve Lord's /com/gfs/prod tcvitals file,
# and cat them with the NHC-only vitals from the raw, original
# /com/arch/prod/synda_tcvitals file.  Do this because the nwprod
# tcvitals file is the original tcvitals file, and Steve runs a
# program that ignores the vitals for a storm that's over land or
# even just too close to land, and for tracking purposes for the
# US regional models, we need these locations.  Only include these
# "inland" storm vitals for NHC (we're not going to track inland 
# storms that are outside of NHC's domain of responsibility -- we 
# don't need that info).  
# UPDATE 5/12/98 MARCHOK: awk logic is added to screen NHC 
#   vitals such as "91L NAMELESS" or "89E NAMELESS", since TPC 
#   does not want tracks for such storms.

#update 04/30/2009 Guang-Ping Lou: Include all Hurricane centers and INVEST ones.
   grep "${d6ago_str}" ${archsyndir}/syndat_tcvitals.${CENT}${syy}   | \
      grep -v TEST | grep -v INVEST | awk 'substr($0,6,1) !~ /[8-9]/ {print $0}' \
      >${PERTDATA}/tmprawvit.${atcfout}.${regtype}.${PDY}${CYL}
   grep "${dnow_str}"  ${archsyndir}/syndat_tcvitals.${CENT}${syy}   | \
      grep -v TEST | grep -v INVEST | awk 'substr($0,6,1) !~ /[8-9]/ {print $0}' \
      >>${PERTDATA}/tmprawvit.${atcfout}.${regtype}.${PDY}${CYL}
   grep "${d6ahead_str}" ${archsyndir}/syndat_tcvitals.${CENT}${syy} | \
      grep -v TEST | grep -v INVEST | awk 'substr($0,6,1) !~ /[8-9]/ {print $0}' \
      >>${PERTDATA}/tmprawvit.${atcfout}.${regtype}.${PDY}${CYL}

# IMPORTANT:  When "cat-ing" these files, make sure that the vitals
# files from the "raw" TC vitals files are first in order and Steve's
# TC vitals files second.  This is because Steve's vitals file has
# been error-checked, so if we have a duplicate tc vitals record in
# these 2 files (very likely), program supvit.x below will
# only take the last vitals record listed for a particular storm in
# the vitals file (all previous duplicates are ignored, and Steve's
# error-checked vitals records are kept).

   cat ${PERTDATA}/tmprawvit.${atcfout}.${regtype}.${PDY}${CYL} > ${PERTDATA}/vitals.${atcfout}.${regtype}.${PDY}${CYL}
   cat ${PERTDATA}/tmpsynvit.${atcfout}.${regtype}.${PDY}${CYL} >> ${PERTDATA}/vitals.${atcfout}.${regtype}.${PDY}${CYL}

# If we are doing the processing for the GFDL model, then we want
# to further cut down on which vitals we allow into this run of the
# tracker.  The reason is that this program will be called from 
# each individual run for a storm, so the grib files will be 
# specific to each storm.  So if 4 storms are being run at a 
# particular cycle, then this script is run 4 separate times from
# within the GFDL_POST job.

   if [ ${cmodel} = 'gfdl' ]; then
      grep -i ${stormid} ${savedir}/${ATCFNAME}.vitals.${syy}${smm}${sdd}${shh} >${PERTDATA}/tmpvit
      mv ${PERTDATA}/tmpvit ${PERTDATA}/vitals.${atcfout}.${regtype}.${PDY}${CYL}
   fi

#--------------------------------------------------------------#
# Now run a fortran program that will read all the TC vitals
# records for the current dtg and the dtg from 6h ago, and
# sort out any duplicates.  If the program finds a storm that
# was included in the vitals file 6h ago but not for the current
# dtg, this program updates the 6h-old first guess position
# and puts these updated records as well as the records from
# the current dtg into a temporary vitals file.  It is this
# temporary vitals file that is then used as the input for the
# tracking program.
#--------------------------------------------------------------#

   ymdh6ago=` ${ndate} -6 ${PDY}${CYL}`
   syy6=`echo ${ymdh6ago} | cut -c3-4`
   smm6=`echo ${ymdh6ago} | cut -c5-6`
   sdd6=`echo ${ymdh6ago} | cut -c7-8`
   shh6=`echo ${ymdh6ago} | cut -c9-10`
   syyyy6=`echo ${ymdh6ago} | cut -c1-4`
   symd6=${syy6}${smm6}${sdd6}

   ymdh6ahead=` ${ndate} 6 ${PDY}${CYL}`
   syyp6=`echo ${ymdh6ahead} | cut -c3-4`
   smmp6=`echo ${ymdh6ahead} | cut -c5-6`
   sddp6=`echo ${ymdh6ahead} | cut -c7-8`
   shhp6=`echo ${ymdh6ahead} | cut -c9-10`
   syyyyp6=`echo ${ymdh6ahead} | cut -c1-4`
   symdp6=${syyp6}${smmp6}${sddp6}

   echo "&datenowin   dnow%yy=${syy}, dnow%mm=${smm},"       >${PERTDATA}/suv_input.${atcfout}.${PDY}${CYL}
   echo "             dnow%dd=${sdd}, dnow%hh=${CYL}/"      >>${PERTDATA}/suv_input.${atcfout}.${PDY}${CYL}
   echo "&date6agoin  d6ago%yy=${syy6}, d6ago%mm=${smm6},"  >>${PERTDATA}/suv_input.${atcfout}.${PDY}${CYL}
   echo "             d6ago%dd=${sdd6}, d6ago%hh=${shh6}/"  >>${PERTDATA}/suv_input.${atcfout}.${PDY}${CYL}
   echo "&date6aheadin  d6ahead%yy=${syyp6}, d6ahead%mm=${smmp6},"  >>${PERTDATA}/suv_input.${atcfout}.${PDY}${CYL}
   echo "               d6ahead%dd=${sddp6}, d6ahead%hh=${shhp6}/"  >>${PERTDATA}/suv_input.${atcfout}.${PDY}${CYL}

################################################
# In order to include HPC tcvitals in SREF tracking, 
# change timing 
  if [ ${cmodel} = 'sref' ]; then
   ymdh6ago=` ${ndate} -9 ${PDY}${CYL}`
   syy6=`echo ${ymdh6ago} | cut -c3-4`
   smm6=`echo ${ymdh6ago} | cut -c5-6`
   sdd6=`echo ${ymdh6ago} | cut -c7-8`
   shh6=`echo ${ymdh6ago} | cut -c9-10`
   syyyy6=`echo ${ymdh6ago} | cut -c1-4`
   symd6=${syy6}${smm6}${sdd6}

   ymdh6ahead=` ${ndate} 3 ${PDY}${CYL}`
   syyp6=`echo ${ymdh6ahead} | cut -c3-4`
   smmp6=`echo ${ymdh6ahead} | cut -c5-6`
   sddp6=`echo ${ymdh6ahead} | cut -c7-8`
   shhp6=`echo ${ymdh6ahead} | cut -c9-10`
   syyyyp6=`echo ${ymdh6ahead} | cut -c1-4`
   symdp6=${syyp6}${smmp6}${sddp6}

   ymdh3ago=` ${ndate} -3 ${PDY}${CYL}`
   syy3=`echo ${ymdh3ago} | cut -c3-4`
   smm3=`echo ${ymdh3ago} | cut -c5-6`
   sdd3=`echo ${ymdh3ago} | cut -c7-8`
   shh3=`echo ${ymdh3ago} | cut -c9-10`
   syyyy3=`echo ${ymdh3ago} | cut -c1-4`
   symd3=${syy3}${smm3}${sdd3}

   echo "&datenowin   dnow%yy=${syy3}, dnow%mm=${smm3},"       >${PERTDATA}/suv_input.${atcfout}.${PDY}${CYL}
   echo "             dnow%dd=${sdd3}, dnow%hh=${shh3}/"      >>${PERTDATA}/suv_input.${atcfout}.${PDY}${CYL}
   echo "&date6agoin  d6ago%yy=${syy6}, d6ago%mm=${smm6},"  >>${PERTDATA}/suv_input.${atcfout}.${PDY}${CYL}
   echo "             d6ago%dd=${sdd6}, d6ago%hh=${shh6}/"  >>${PERTDATA}/suv_input.${atcfout}.${PDY}${CYL}
   echo "&date6aheadin  d6ahead%yy=${syyp6}, d6ahead%mm=${smmp6},"  >>${PERTDATA}/suv_input.${atcfout}.${PDY}${CYL}
   echo "               d6ahead%dd=${sddp6}, d6ahead%hh=${shhp6}/"  >>${PERTDATA}/suv_input.${atcfout}.${PDY}${CYL}

  fi

   numvitrecs=`cat ${PERTDATA}/vitals.${atcfout}.${regtype}.${PDY}${CYL} | wc -l`
   if [ ${numvitrecs} -eq 0 ]; then
      if [ ${trkrtype} = 'tracker' ]; then
         set +x
         echo " "
         echo "!!! NOTE -- There are no vitals records for this time period."
         echo "!!! File ${PERTDATA}/vitals.${atcfout}.${PDY}${CYL} is empty."
         echo "!!! It could just be that there are no storms for the current"
         echo "!!! time.  Please check the dates and submit this job again...."
         echo " "
         set -x
         err_exit " FAILED ${jobid} - ERROR IN TRACKER SCRIPT - ABNORMAL EXIT" 
         exit 1
      fi
   fi

# - - - - - - - - - - - - -
# Before running the program to read, sort and update the vitals,
# first run the vitals through some awk logic, the purpose of 
# which is to convert all the 2-digit years into 4-digit years.
# Beginning 4/21/99, NHC and JTWC will begin sending the vitals
# with 4-digit years, however it is unknown when other global
# forecasting centers will begin using 4-digit years, thus we
# need the following logic to ensure that all the vitals going
# into supvit.f have uniform, 4-digit years in their records.
#
# 1/8/2000: sed code added by Tim Marchok due to the fact that 
#       some of the vitals were getting past the syndata/qctropcy
#       error-checking with a colon in them; the colon appeared
#       in the character immediately to the left of the date, which
#       was messing up the "(length($4) == 8)" statement logic.
# - - - - - - - - - - - - -

   sed -e "s/\:/ /g"  ${PERTDATA}/vitals.${atcfout}.${regtype}.${PDY}${CYL} > ${PERTDATA}/tempvit
   mv ${PERTDATA}/tempvit ${PERTDATA}/vitals.${atcfout}.${regtype}.${PDY}${CYL}

   awk '
   {
     yycheck = substr($0,20,2)
   if ((yycheck == 20 || yycheck == 19) && (length($4) == 8)) {
    printf ("%s\n",$0)
   }
   else {
      if (yycheck >= 0 && yycheck <= 50) {
        printf ("%s20%s\n",substr($0,1,19),substr($0,20))
        }
      else {
         printf ("%s19%s\n",substr($0,1,19),substr($0,20))
         }
   }
   } ' ${PERTDATA}/vitals.${atcfout}.${regtype}.${PDY}${CYL} >${PERTDATA}/vitals.${atcfout}.${regtype}.${PDY}${CYL}.y4

   mv ${PERTDATA}/vitals.${atcfout}.${regtype}.${PDY}${CYL}.y4 ${PERTDATA}/vitals.${atcfout}.${regtype}.${PDY}${CYL}

   if [ ${numvitrecs} -gt 0 ]; then

      export pgm=supvit
      . prep_step

      ln -s -f ${PERTDATA}/vitals.${atcfout}.${regtype}.${PDY}${CYL} fort.31
      ln -s -f ${PERTDATA}/vitals.upd.${atcfout}.${regtype}.${PDY}${CYL}        fort.51

      msg="$pgm start for $atcfout at ${CYL}z"
#      postmsg "$jlogfile" "$msg"

      ${exectrkdir}/supvit <${PERTDATA}/suv_input.${atcfout}.${PDY}${CYL}
      suvrcc=$?

      if [ ${suvrcc} -eq 0 ]; then
         msg="$pgm end for $atcfout at ${CYL}z completed normally"
#         postmsg "$jlogfile" "$msg"
      else
         set +x
         echo " "
         echo "!!! ERROR -- An error occurred while running supvit.x, "
         echo "!!! which is the program that updates the TC Vitals file."
         echo "!!! Return code from supvit.x = ${suvrcc}"
         echo "!!! model= ${atcfout}, forecast initial time = ${PDY}${CYL}"
         echo "!!! Exiting...."
         echo " "
         set -x
         err_exit " FAILED ${jobid} - ERROR RUNNING SUPVIT IN TRACKER SCRIPT- ABNORMAL EXIT"
      fi

   else

      touch ${PERTDATA}/vitals.upd.${atcfout}.${regtype}.${PDY}${CYL}

   fi

if [ ${USE_OPER_VITALS} = 'INIT_ONLY' ]; then

  if [ ${init_flag} = 'yes' ]; then
    set +x
    echo " "
    echo "NOTE: User has requested that operational historical TC vitals be used,"
    echo "      but only for the initial time, which we are currently at."
    echo " "
    set -x
  else
    set +x
    echo " "
    echo "NOTE: User has requested that operational historical TC vitals be used,"
    echo "      but only for the initial time, which we are now *PAST*."
    echo " "
    set -x
    echo " " >${PERTDATA}/vitals.upd.${atcfout}.${regtype}.${PDY}${CYL}
  fi

elif [ ${USE_OPER_VITALS} = 'NO' ]; then

  set +x
  echo " "
  echo "NOTE: User has requested that historical vitals not be used...."
  echo " "
  set -x
  echo " " >${PERTDATA}/vitals.upd.${atcfout}.${regtype}.${PDY}${CYL}

fi

#------------------------------------------------------------------#
# Now select all storms to be processed, that is, process every
# storm that's listed in the updated vitals file for the current
# forecast hour.  If there are no storms for the current time,
# then exit.
#------------------------------------------------------------------#

   numvitrecs=`cat ${PERTDATA}/vitals.upd.${atcfout}.${regtype}.${PDY}${CYL} | wc -l`
   if [ ${numvitrecs} -eq 0 ]; then
      touch ${PERTDATA}/vitals.upd.${atcfout}.${regtype}.${PDY}${CYL}
      if [ ${trkrtype} = 'tracker' ]; then
         set +x
         echo " "
         echo "!!! NOTE -- There are no vitals records for this time period "
         echo "!!! in the UPDATED vitals file."
         echo "!!! It could just be that there are no storms for the current"
         echo "!!! time.  Please check the dates and submit this job again...."
         echo " "
         set -x
         err_exit " FAILED ${jobid} - ERROR IN TRACKER SCRIPT - ABNORMAL EXIT"
         exit 1
      fi
   fi

   set +x
   echo " "
   echo " *--------------------------------*"
   echo " |        STORM SELECTION         |"
   echo " *--------------------------------*"
   echo " "
   set -x

   ict=1
   while [ $ict -le 15 ]
   do
      stormflag[${ict}]=3
      let ict=ict+1
   done

   dtg_current="${symd} ${CYL}00"
   stormmax=` grep "${dtg_current}" ${PERTDATA}/vitals.upd.${atcfout}.${regtype}.${PDY}${CYL} | wc -l`

   if [ ${stormmax} -gt 15 ]; then
      stormmax=15
   fi

   sct=1
   while [ ${sct} -le ${stormmax} ]
   do
      stormflag[${sct}]=1
      let sct=sct+1
   done

#---------------------------------------------------------------#
#
#    --------  "Genesis" Vitals processing   --------
#
# May 2006:  This entire genesis tracking system is being
# upgraded to more comprehensively track and categorize storms.
# One thing that has been missing from the tracking system is
# the ability to keep track of storms from one analysis cycle
# to the next.  That is, the current system has been very
# effective at tracking systems within a forecast, but we have
# no methods in place for keeping track of storms across
# difference initial times.  For example, if we are running
# the tracker on today's 00z GFS analysis, we will get a
# position for various storms at the analysis time.  But then
# if we go ahead and run again at 06z, we have no way of
# telling the tracker that we know about the 00z position of
# this storm.  We now address that problem by creating
# "genesis" vitals, that is, when a storm is found at an
# analysis time, we not only produce "atcfunix" output to
# detail the track & intensity of a found storm, but we also
# produce a vitals record that will be used for the next
# run of the tracker script.  These "genesis vitals" records
# will be of the format:
#
#  YYYYMMDDHH_AAAH_LLLLX_TYP
#
#    Where:
#
#      YYYYMMDDHH = Date the storm was FIRST identified
#                   by the tracker.
#             AAA = Abs(Latitude) * 10; integer value
#               H = 'N' for norther hem, 'S' for southern hem
#            LLLL = Abs(Longitude) * 10; integer value
#               X = 'E' for eastern hem, 'W' for western hem
#             TYP = Tropical cyclone storm id if this is a
#                   tropical cyclone (e.g., "12L", or "09W", etc).
#                   If this is one that the tracker instead "Found
#                   On the Fly (FOF)", we simply put those three
#                   "FOF" characters in there.

################################################
set -xa
#after  tcvitals, return to normal time
  if [ ${cmodel} = 'sref' ]; then
   d6ago_ymdh=` ${ndate} -6 ${PDY}${CYL}`
   d6ago_4ymd=` echo ${d6ago_ymdh} | cut -c1-8`
   d6ago_ymd=` echo ${d6ago_ymdh} | cut -c3-8`
   d6ago_hh=`  echo ${d6ago_ymdh} | cut -c9-10`
   d6ago_str="${d6ago_ymd} ${d6ago_hh}00"

   d6ahead_ymdh=` ${ndate} 6 ${PDY}${CYL}`
   d6ahead_4ymd=` echo ${d6ahead_ymdh} | cut -c1-8`
   d6ahead_ymd=` echo ${d6ahead_ymdh} | cut -c3-8`
   d6ahead_hh=`  echo ${d6ahead_ymdh} | cut -c9-10`
   d6ahead_str="${d6ahead_ymd} ${d6ahead_hh}00"

   dnow_str="${symd} ${CYL}00"

   ymdh6ago=` ${ndate} -6 ${PDY}${CYL}`
   syy6=`echo ${ymdh6ago} | cut -c3-4`
   smm6=`echo ${ymdh6ago} | cut -c5-6`
   sdd6=`echo ${ymdh6ago} | cut -c7-8`
   shh6=`echo ${ymdh6ago} | cut -c9-10`
   syyyy6=`echo ${ymdh6ago} | cut -c1-4`
   symd6=${syy6}${smm6}${sdd6}

   ymdh6ahead=` ${ndate} 6 ${PDY}${CYL}`
   syyp6=`echo ${ymdh6ahead} | cut -c3-4`
   smmp6=`echo ${ymdh6ahead} | cut -c5-6`
   sddp6=`echo ${ymdh6ahead} | cut -c7-8`
   shhp6=`echo ${ymdh6ahead} | cut -c9-10`
   syyyyp6=`echo ${ymdh6ahead} | cut -c1-4`
   symdp6=${syyp6}${smmp6}${sddp6}
  fi
################################################
#Because international centers make twice a day forecasts,
#the gen_vitals exist in 12hr intervals. G.P. Lou
  if [ ${cmodel} = 'cmc' -o ${cmodel} = 'nvgm' -o ${cmodel} = 'ukmet' -o ${cmodel} = 'ecmwf' -o ${cmodel} = 'cens' -o ${cmodel} = 'eens'  -o ${cmodel} = 'fens' ]; then
       m12=-12
       p12=12

   d6ago_ymdh=` ${ndate} $m12 ${PDY}${CYL}`
   d6ago_4ymd=` echo ${d6ago_ymdh} | cut -c1-8`
   d6ago_ymd=` echo ${d6ago_ymdh} | cut -c3-8`
   d6ago_hh=`  echo ${d6ago_ymdh} | cut -c9-10`
   d6ago_str="${d6ago_ymd} ${d6ago_hh}00"

   d6ahead_ymdh=` ${ndate} $p12 ${PDY}${CYL}`
   d6ahead_4ymd=` echo ${d6ahead_ymdh} | cut -c1-8`
   d6ahead_ymd=` echo ${d6ahead_ymdh} | cut -c3-8`
   d6ahead_hh=`  echo ${d6ahead_ymdh} | cut -c9-10`
   d6ahead_str="${d6ahead_ymd} ${d6ahead_hh}00"

   ymdh6ago=` ${ndate} $m12 ${PDY}${CYL}`
   syy6=`echo ${ymdh6ago} | cut -c3-4`
   smm6=`echo ${ymdh6ago} | cut -c5-6`
   sdd6=`echo ${ymdh6ago} | cut -c7-8`
   shh6=`echo ${ymdh6ago} | cut -c9-10`
   syyyy6=`echo ${ymdh6ago} | cut -c1-4`
   symd6=${syy6}${smm6}${sdd6}

   ymdh6ahead=` ${ndate} $p12 ${PDY}${CYL}`
   syyp6=`echo ${ymdh6ahead} | cut -c3-4`
   smmp6=`echo ${ymdh6ahead} | cut -c5-6`
   sddp6=`echo ${ymdh6ahead} | cut -c7-8`
   shhp6=`echo ${ymdh6ahead} | cut -c9-10`
   syyyyp6=`echo ${ymdh6ahead} | cut -c1-4`
   symdp6=${syyp6}${smmp6}${sddp6}
  fi
################################################


#/com/hur/prod/global/2015/genesis.vitals.sac1.tggb.2015
#genvitdir=/com/hur/prod/global/${syyyy6}
genvitdir=${gltrkdir}/${syyyy6}
genvitfile=${genvitdir}/genesis.vitals.${atcf_vit}.${regtype}.${syyyy6}

echo " "
echo " d6ago_str=    --->${d6ago_str}<---"
echo " dnow_str=     --->${dnow_str}<---"
echo " d6ahead_str=  --->${d6ahead_str}<---"
echo " "
echo " Listing and contents of ${genvitdir}/genesis.vitals.${atcfout}.${regtype}.${CENT}${syy} follow: "
echo " "

ls -la ${genvitfile}

grep "${d6ago_str}" ${genvitfile} >${PERTDATA}/genvitals.${atcfout}.${regtype}.${PDY}${CYL}
grep "${dnow_str}"  ${genvitfile} >>${PERTDATA}/genvitals.${atcfout}.${regtype}.${PDY}${CYL}
grep "${d6ahead_str}" ${genvitfile} >>${PERTDATA}/genvitals.${atcfout}.${regtype}.${PDY}${CYL}

echo "&datenowin   dnow%yy=${syyyy}, dnow%mm=${smm},"       >${PERTDATA}/sgv_input.${atcfout}.${PDY}${CYL}
echo "             dnow%dd=${sdd}, dnow%hh=${CYL}/"        >>${PERTDATA}/sgv_input.${atcfout}.${PDY}${CYL}
echo "&date6agoin  d6ago%yy=${syyyy6}, d6ago%mm=${smm6},"  >>${PERTDATA}/sgv_input.${atcfout}.${PDY}${CYL}
echo "             d6ago%dd=${sdd6}, d6ago%hh=${shh6}/"    >>${PERTDATA}/sgv_input.${atcfout}.${PDY}${CYL}
echo "&date6aheadin  d6ahead%yy=${syyyyp6}, d6ahead%mm=${smmp6}," >>${PERTDATA}/sgv_input.${atcfout}.${PDY}${CYL}
echo "               d6ahead%dd=${sddp6}, d6ahead%hh=${shhp6}/"  >>${PERTDATA}/sgv_input.${atcfout}.${PDY}${CYL}

num_gen_vits=`cat ${PERTDATA}/genvitals.${atcfout}.${regtype}.${PDY}${CYL} | wc -l`

rm -f fort.31
rm -f fort.51
   set +x
if [ ${num_gen_vits} -gt 0 ]
then

  export pgm=supvit_gen
  . prep_step

      ln -s -f ${PERTDATA}/genvitals.${atcfout}.${regtype}.${PDY}${CYL} fort.31
      ln -s -f ${PERTDATA}/genvitals.upd.${atcfout}.${regtype}.${PDY}${CYL}        fort.51
#  export XLFUNIT_31=${PERTDATA}/genvitals.${atcfout}.${regtype}.${PDY}${CYL}
#  export XLFUNIT_51=${PERTDATA}/genvitals.upd.${atcfout}.${regtype}.${PDY}${CYL}

  msg="$pgm start for $atcfout at ${CYL}z"
  postmsg "$jlogfile" "$msg"

  ${exectrkdir}/supvit_gen <${PERTDATA}/sgv_input.${atcfout}.${PDY}${CYL}
  sgvrcc=$?

  if [ ${sgvrcc} -eq 0 ]
  then
    msg="$pgm end for $atcfout at ${CYL}z completed normally"
    postmsg "$jlogfile" "$msg"
  else
    set +x
    echo " "
    echo "!!! ERROR -- An error occurred while running supvit_gen, "
    echo "!!! which is the program that updates the genesis vitals file."
    echo "!!! Return code from supvit_gen = ${sgvrcc}"
    echo "!!! model= ${atcfout}, forecast initial time = ${PDY}${CYL}"
    echo "!!! Exiting...."
    echo " "
    set -x
    err_exit " FAILED ${jobid} - ERROR RUNNING SUPVIT_GEN IN TRACKER SCRIPT- ABNORMAL EXIT"
    exit 8
  fi

else

  touch ${PERTDATA}/genvitals.upd.${atcfout}.${regtype}.${PDY}${CYL}

fi

rm -f fort.31
rm -f fort.51
set -x
#-----------------------------------------------------------------#
#
#         ------  CUT APART INPUT GRIB FILES  -------
#
# For the selected model, cut apart the GRIB input files in order
# to pull out only the variables that we need for the tracker.  
# Put these selected variables from all forecast hours into 1 big 
# GRIB file that we'll use as input for the tracker.
# 
# The wgrib utility (gwgrib) is used to cut out 
# the needed parms for the GFS, MRF, GDAS, UKMET and NAVGEM files.
# The utility gcopygb is used to interpolate the 
# NGM (polar stereographic) and Eta (Lambert Conformal) data from 
# their grids onto lat/lon grids.  Note that while the lat/lon 
# grid that I specify overlaps into areas that don't have any data 
# on the original grid, Mark Iredell wrote the copygb software so 
# that it will mask such "no-data" points with a bitmap (just be 
# sure to check the lbms in your fortran program after getgb).
#-----------------------------------------------------------------#

   set +x
   echo " "
   echo " -----------------------------------------"
   echo "   NOW CUTTING APART INPUT GRIB FILES TO "
   echo "   CREATE 1 BIG GRIB INPUT FILE "
   echo " -----------------------------------------"
   echo " "
   set -x


   regflag=`grep NHC ${PERTDATA}/vitals.upd.${atcfout}.${regtype}.${PDY}${CYL} | wc -l`

# ----------------------------------
#   Process Early NAM, if selected
# ----------------------------------

   if [ ${model} -eq 6 ]; then

      grid='255 0 381 161 80000 160000 128 0000 350000  500  500 0'
#      grid='255 0 301 141 70000 190000 128 0000 340000  500  500 0'
  grid2="0 6 0 0 0 0 0 0 381 161 0 0 80000000 160000000 48 0000000 350000000 500000 500000 0"

      if [ ${regflag} -eq 0 ]; then
         if [ ${trkrtype} = 'tracker' ]; then
            set +x
            echo " "
            echo " !!! NAM model has been selected, but there are no storms in"
            echo " !!! the TC Vitals file that can be processed.  That is, "
            echo " !!! there are no Vitals records from NHC.  The vitals "
            echo " !!! records that are in the updated vitals file must be from"
            echo " !!! another cyclone forecast center, and the Eta domain does"
            echo " !!! not extend to any region other than that covered by NHC."
            echo " !!! Exiting....."
            set -x
            err_exit " FAILED ${jobid} - ERROR IN TRACKER SCRIPT - ABNORMAL EXIT"
            exit 1
         fi
      fi

      if [ -s ${PERTDATA}/namlatlon.pgrb.${PDY}${CYL} ]; then 
         rm ${PERTDATA}/namlatlon.pgrb.${PDY}${CYL}
      fi

      for fhour in ${fcsthrs}
      do

         if [ ${fhour} -eq 99 ]; then
            continue
         fi

         total_file_cnt=$(($total_file_cnt+1))
       if [ ${gribver} -eq 1 ]; then
         if [ ! -s ${namdir}/${namgfile}${fhour}.tm00 ]; then
            set +x
            echo " "
            echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
            echo " !!! Early NAM File missing:                            "
            echo " !!! ${namdir}/${namgfile}${fhour}.tm00                 "
            echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
            echo " "
            echo " !!! Please re-run the job when NAM file is available ..... "
            echo " "
            missing_file_cnt=$(($missing_file_cnt+1))
            set -x
            err_exit " FAILED ${jobid} - MISSING NAM FILE IN TRACKER SCRIPT - ABNORMAL EXIT"
         fi

         if [ -s ${namdir}/${namifile}${fhour} ]; then
            x1=${namdir}/${namifile}${fhour}
         else
            if [ -s ${PERTDATA}/tmpnamixfile ]; then
               rm ${PERTDATA}/tmpnamixfile
            fi
            $gix ${namdir}/${namgfile}${fhour}.tm00 ${PERTDATA}/tmpnamixfile
            x1=${PERTDATA}/tmpnamixfile
         fi

         set +x
         echo " "
         echo " Extracting Early NAM GRIB data for forecast hour = $fhour"
         echo " "
         set -x
  
         g1=${namdir}/${namgfile}${fhour}.tm00
     
         $cgb -g"$grid" -k'4*-1 33 100 850' $g1 $x1 ${PERTDATA}/namllu850.grb.f${fhour};   rcc1=$?
         $cgb -g"$grid" -k'4*-1 33 100 700' $g1 $x1 ${PERTDATA}/namllu700.grb.f${fhour};   rcc2=$?
         $cgb -g"$grid" -k'4*-1 33 100 500' $g1 $x1 ${PERTDATA}/namllu500.grb.f${fhour};   rcc3=$?
         $cgb -g"$grid" -k'4*-1 33 105 10'  $g1 $x1 ${PERTDATA}/namllu10m.grb.f${fhour};   rcc4=$?
         $cgb -g"$grid" -k'4*-1 41 100 850' $g1 $x1 ${PERTDATA}/namllav850.grb.f${fhour};  rcc5=$?
         $cgb -g"$grid" -k'4*-1 41 100 700' $g1 $x1 ${PERTDATA}/namllav700.grb.f${fhour};  rcc6=$?
         $cgb -g"$grid" -k'4*-1  7 100 850' $g1 $x1 ${PERTDATA}/namllz850.grb.f${fhour};   rcc7=$?
         $cgb -g"$grid" -k'4*-1  7 100 700' $g1 $x1 ${PERTDATA}/namllz700.grb.f${fhour};   rcc8=$?
         $cgb -g"$grid" -k'4*-1  2 102   0' $g1 $x1 ${PERTDATA}/namllmslp.grb.f${fhour};   rcc9=$?

         if [ $rcc1 -eq 134 -o $rcc2 -eq 134 -o $rcc3 -eq 134 -o $rcc4 -eq 134 -o $rcc5 -eq 134 -o \
            $rcc6 -eq 134 -o $rcc7 -eq 134 -o $rcc8 -eq 134 -o $rcc9 -eq 134 ]
         then
            set +x
            echo " "
            echo "!!! ERROR using $cgb to interpolate nam data.  We will stop"
            echo "!!! execution because some variables may have been copied "
            echo "!!! okay, while some obviously have not, and that could lead"
            echo "!!! to unreliable results from the tracker.  Check to make"
            echo "!!! sure you've allocated enough memory for this job. "
            echo "!!! Exiting...."
            echo " "
            set -x
            err_exit " FAILED ${jobid} - ERROR INTERPOLATING NAM PERTDATA IN TRACKER SCRIPT - ABNORMAL EXIT"
         fi

         cat ${PERTDATA}/namllu850.grb.f${fhour} ${PERTDATA}/namllu700.grb.f${fhour} ${PERTDATA}/namllu500.grb.f${fhour} ${PERTDATA}/namllz850.grb.f${fhour} \
        ${PERTDATA}/namllz700.grb.f${fhour} ${PERTDATA}/namllmslp.grb.f${fhour} \
        ${PERTDATA}/namllav850.grb.f${fhour} ${PERTDATA}/namllav700.grb.f${fhour}  ${PERTDATA}/namllu10m.grb.f${fhour} \
        >>${PERTDATA}/namlatlon.pgrb.${PDY}${CYL}
  

   else ##${gribver} -eq 2

        if [  -s ${namdir}/${namgfile}${fhour}.tm00.grib2 ]
        then
          echo ${namdir}/${namgfile}${fhour}.tm00.grib2
          # Try to wgrib the primary file....
          $wgrib2 -s ${namdir}/${namgfile}${fhour}.tm00.grib2 > namifile.ix
          gfile=${namdir}/${namgfile}${fhour}.tm00.grib2
        else
            set +x
            echo " "
            echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
            echo " !!! ERROR: Both the primary and secondary global     !!!!!!!!!!!!!!"
            echo " !!! grib version 2 files are missing for:            !!!!!!!!!!!!!!"
            echo " !!! cmodel= ${cmodel}                                !!!!!!!!!!!!!!"
            echo " !!! symdh=  ${symdh}                                 !!!!!!!!!!!!!!"
            echo " !!! fhour=  ${fhour}                                 !!!!!!!!!!!!!!"
            echo " !!! Check for the existence of these files:          !!!!!!!!!!!!!!"
            echo " !!!    ${gfile}                                                    "
            echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
            echo " "
            set -x
            exit
        fi

        set +x
        echo "Timing: fhour= $fhour before wgrib loop at `date`"
        set -x

        for parm in ${wgrib_parmlist}
        do
          case ${parm} in
            "SurfaceU")
              grep "UGRD:10 m " namifile.ix | \
                              $wgrib2 -i $gfile -append -grib \
                              ${PERTDATA}/namgribfile.${PDY}${CYL}.f${fhour} ;;
            "SurfaceV")
              grep "VGRD:10 m " namifile.ix | \
                              $wgrib2 -i $gfile -append -grib \
                              ${PERTDATA}/namgribfile.${PDY}${CYL}.f${fhour} ;;
                     *)
              grep "${parm}" namifile.ix | $wgrib2 -i $gfile -append -grib \
                              ${PERTDATA}/namgribfile.${PDY}${CYL}.f${fhour} ;;
          esac
        done

        set +x
        echo "Timing: fhour= $fhour after wgrib loop at `date`"
        set -x

        nam_file=${PERTDATA}/namgribfile.${PDY}${CYL}.f${fhour}

          cat ${nam_file} >> ${PERTDATA}/namgribfile.${PDY}${CYL}

      fi
  
    done
  
    if [ ${gribver} -eq 1 ]; then
      $gix ${PERTDATA}/namlatlon.pgrb.${PDY}${CYL} ${PERTDATA}/namlatlon.pgrb.ix.${PDY}${CYL}
    else
      ${wgrib2} ${PERTDATA}/namgribfile.${PDY}${CYL} \
              -new_grid_vectors none       \
              -new_grid latlon "140:421:0.5"  "0:180:0.5"  \
              namlatlon.pgrb.${PDY}${CYL}
      $g2ix ${PERTDATA}/namlatlon.pgrb.${PDY}${CYL} ${PERTDATA}/namlatlon.pgrb.ix.${PDY}${CYL}
    fi

      gribfile=${PERTDATA}/namlatlon.pgrb.${PDY}${CYL}
      ixfile=${PERTDATA}/namlatlon.pgrb.ix.${PDY}${CYL}

   fi

# ----------------------------------
#   Process GFDL, if selected
# ----------------------------------

# The GFDL GRIB grid is already a lat/lon grid, however it uses a scanning 
# mode flag of 64, which means the data starts in the south and goes north.
# The tracker needs the data to start in the north and go south, so we will
# use copygb to flip the data in the grid.  The other thing that copygb 
# will do here is make the new file have wider boundaries around all 4 
# sides of the grid.  This is done because the GFDL grid is a regional grid,
# and we need to have that buffer of null, bitmapped data around a 
# regional grid.  The north-south extent of the domain is always the same,
# so we will hardwire the new latitude bounds in the "grid=" statement
# (adding an extra ~5.0 degrees both north and south), but we need to wgrib
# the 00h file to get the longitude bounds, and then modify them.  The 
# northern and southern boundaries are fixed in that "grid=" statement 
# because those boundaries do NOT change from run to run; the integration
# grid only changes in the east/west direction.

   if [ ${model} -eq 9 ]; then

      origwestlon=`$wgrib -V ${gfdldir}/${gfdlgfile}00 | grep long | head -1 | awk '{printf ("%d",$2*1000)}'`
      origeastlon=`$wgrib -V ${gfdldir}/${gfdlgfile}00 | grep long | head -1 | awk '{printf ("%d",$4*1000)}'`
      let newwestlon=origwestlon-5000
      let neweastlon=origeastlon+5000

      grid="255 0 255 255 69833 ${newwestlon} 128 -14833 ${neweastlon} 333  333 0"

      if [ ${regflag} -eq 0 ]; then
         if [ ${trkrtype} = 'tracker' ]; then
            set +x
            echo " "
            echo " !!! GFDL model has been selected, but there are no storms in"
            echo " !!! the TC Vitals file that can be processed.  That is, "
            echo " !!! there are no Vitals records from NHC.  The vitals "
            echo " !!! records that are in the updated vitals file must be from"
            echo " !!! another cyclone forecast center, and the GFDL domain "
            echo " !!! does not extend to any region other than that covered "
            echo " !!! by NHC.  Exiting....."
            set -x
            err_exit " FAILED ${jobid} - ERROR IN TRACKER SCRIPT - ABNORMAL EXIT"
            exit 0
         fi
      fi

      if [ -s ${PERTDATA}/gfdl.${stormenv}.pgrb.${PDY}${CYL} ]; then
         rm ${PERTDATA}/gfdl.${stormenv}.pgrb.${PDY}${CYL}
      fi

      for fhour in ${fcsthrs}
      do

         if [ ${fhour} -eq 99 ]; then
            continue
         fi

         total_file_cnt=$(($total_file_cnt+1))
         if [ ! -s ${gfdldir}/${gfdlgfile}${fhour} ]; then
            set +x
            echo " "
            echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
            echo " !!! GFDL File missing: ${gfdldir}/${gfdlgfile}${fhour}" 
            echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" 
            echo " "
            echo " !!! Please re-run the job when GFDL file is available ..... "
            echo " "
            missing_file_cnt=$(($missing_file_cnt+1))
            set -x 
            err_exit " FAILED ${jobid} - MISSING GFDL FILE IN TRACKER SCRIPT - ABNORMAL EXIT"
#            continue
         fi
         if [ -s ${gfdldir}/${gfdlifile}${fhour} ]; then
            x1=${gfdldir}/${gfdlifile}${fhour}
         else
            if [ -s ${PERTDATA}/tmpgfdlixfile ]; then
               rm ${PERTDATA}/tmpgfdlixfile
            fi
            $gix ${gfdldir}/${gfdlgfile}${fhour} ${PERTDATA}/tmpgfdlixfile
            x1=${PERTDATA}/tmpgfdlixfile
         fi

         set +x
         echo " "
         echo " Extracting GFDL GRIB data for forecast hour = $fhour"
         echo " "
         set -x

         g1=${gfdldir}/${gfdlgfile}${fhour}

         $cgb -g"$grid" -k'4*-1 33 100 850' $g1 $x1 ${PERTDATA}/gfdlu850.grb.f${fhour};   rcc1=$?
         $cgb -g"$grid" -k'4*-1 33 100 700' $g1 $x1 ${PERTDATA}/gfdlu700.grb.f${fhour};   rcc2=$?
         $cgb -g"$grid" -k'4*-1 33 100 500' $g1 $x1 ${PERTDATA}/gfdlu500.grb.f${fhour};   rcc3=$?
         $cgb -g"$grid" -k'4*-1 33 105  35' $g1 $x1 ${PERTDATA}/gfdlu35m.grb.f${fhour};   rcc4=$?
         $cgb -g"$grid" -k'4*-1  7 100 850' $g1 $x1 ${PERTDATA}/gfdlz850.grb.f${fhour};   rcc7=$?
         $cgb -g"$grid" -k'4*-1  7 100 700' $g1 $x1 ${PERTDATA}/gfdlz700.grb.f${fhour};   rcc8=$?
         $cgb -g"$grid" -k'4*-1  2 102   0' $g1 $x1 ${PERTDATA}/gfdlmslp.grb.f${fhour};   rcc9=$?

         if [ $rcc1 -eq 134 -o $rcc2 -eq 134 -o $rcc3 -eq 134 -o $rcc4 -eq 134 -o $rcc7 -eq 134 -o $rcc8 -eq 134 -o $rcc9 -eq 134 ]; then
            set +x
            echo " "
            echo "!!! ERROR using $cgb to interpolate gfdl data.  We will stop"
            echo "!!! execution because some variables may have been copied "
            echo "!!! okay, while some obviously have not, and that could lead"
            echo "!!! to unreliable results from the tracker.  Check to make"
            echo "!!! sure you've allocated enough memory for this job."
            echo "!!! Exiting...."
            echo " "
            set -x
            err_exit " FAILED ${jobid} - ERROR INTERPOLATING GFDL PERTDATA IN TRACKER SCRIPT - ABNORMAL EXIT"
         fi

         cat ${PERTDATA}/gfdlu850.grb.f${fhour} ${PERTDATA}/gfdlu700.grb.f${fhour}  ${PERTDATA}/gfdlu500.grb.f${fhour} ${PERTDATA}/gfdlz850.grb.f${fhour} \
         ${PERTDATA}/gfdlz700.grb.f${fhour} ${PERTDATA}/gfdlmslp.grb.f${fhour} \
         ${PERTDATA}/gfdlu35m.grb.f${fhour} \
         >>${PERTDATA}/gfdl.${stormenv}.pgrb.${PDY}${CYL}
 
      done

      $gix ${PERTDATA}/gfdl.${stormenv}.pgrb.${PDY}${CYL} ${PERTDATA}/gfdl.${stormenv}.pgrb.ix.${PDY}${CYL}
      gribfile=${PERTDATA}/gfdl.${stormenv}.pgrb.${PDY}${CYL}
      ixfile=${PERTDATA}/gfdl.${stormenv}.pgrb.ix.${PDY}${CYL}

   fi

# ---------------------------------------------------------
# Process Global GRIB data.  We will use just one IF block,
# whether this is GFS, GDAS or Parallel data, and whether 
# this is GRIB1 or GRIB2 data.
# ---------------------------------------------------------

if [ ${model} -eq 1 ]
then

    if [ -s ${PERTDATA}/globalgribfile.${PDY}${CYL} ]
    then
      rm ${PERTDATA}/globalgribfile.${PDY}${CYL}
    fi

    rm ${PERTDATA}/master.globalgribfile.${PDY}${CYL}.f*
    rm ${PERTDATA}/globalgribfile.${PDY}${CYL}.f*
    >${PERTDATA}/globalgribfile.${PDY}${CYL}

    set +x
    echo " "
    echo "Time before global wgrib loop is `date`"
    echo " "
    set -x
  
    for fhour in ${fcsthrs}
    do

      set +x
      echo "Timing: fhour= $fhour at `date`"
      set -x
  
      if [ ${fhour} -eq 99 ]
      then
        continue
      fi
  
      if [ ${gribver} -eq 1 ]; then

        if [ -s ${globaldir}/${globalgfile}${fhour} ]
        then
          # Try to wgrib the primary file....
          gfile=${globaldir}/${globalgfile}${fhour}
          $wgrib -s ${gfile} >globalgfile.ix
        else
            set +x
            echo " "
            echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
            echo " !!! ERROR: Both the primary and secondary global     !!!!!!!!!!!!!!"
            echo " !!! grib version 1 files are missing for:            !!!!!!!!!!!!!!"
            echo " !!! cmodel= ${cmodel}                                !!!!!!!!!!!!!!"
            echo " !!! symdh=  ${symdh}                                 !!!!!!!!!!!!!!"
            echo " !!! fhour=  ${fhour}                                 !!!!!!!!!!!!!!"
            echo " !!! Check for the existence of these files:          !!!!!!!!!!!!!!"
            echo " !!!    ${globaldir}/${globalgfile} "
            echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
            echo " "
            set -x
            exit
        fi

        set +x
        echo "Timing: fhour= $fhour before wgrib loop at `date`"
        set -x

        for parm in ${wgrib_parmlist}
        do
          case ${parm} in
            "SurfaceU")
              grep "UGRD:10 m " globalgfile.ix | \
                          $wgrib -s $gfile -i -grib -append \
                          -o ${PERTDATA}/master.globalgribfile.${PDY}${CYL}.f${fhour} ;;
            "SurfaceV")
              grep "VGRD:10 m " globalgfile.ix | \
                          $wgrib -s $gfile -i -grib -append \
                          -o ${PERTDATA}/master.globalgribfile.${PDY}${CYL}.f${fhour} ;;
                     *)
              grep "${parm}" globalgfile.ix | \
                          $wgrib -s $gfile -i -grib -append \
                          -o ${PERTDATA}/master.globalgribfile.${PDY}${CYL}.f${fhour} ;;
          esac
        done

        set +x
        echo "Timing: fhour= $fhour after wgrib loop at `date`"
        set -x
        global_master_file=${PERTDATA}/master.globalgribfile.${PDY}${CYL}.f${fhour}

          cat ${global_master_file} >> ${PERTDATA}/globalgribfile.${PDY}${CYL}

        set +x
        echo "Timing: fhour= $fhour after cgb loop at `date`"
        set -x

      else

        if [ -s ${globaldir}/${globalgfile}${fhour} ]
        then
          echo ${globaldir}/${globalgfile}${fhour}
          # Try to wgrib the primary file....
          $wgrib2 -s ${globaldir}/${globalgfile}${fhour} > globalgfile.ix
          gfile=${globaldir}/${globalgfile}${fhour}
        else
            set +x
            echo " "
            echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
            echo " !!! ERROR: Both the primary and secondary global     !!!!!!!!!!!!!!"
            echo " !!! grib version 2 files are missing for:            !!!!!!!!!!!!!!"
            echo " !!! cmodel= ${cmodel}                                !!!!!!!!!!!!!!"
            echo " !!! symdh=  ${symdh}                                 !!!!!!!!!!!!!!"
            echo " !!! fhour=  ${fhour}                                 !!!!!!!!!!!!!!"
            echo " !!! Check for the existence of these files:          !!!!!!!!!!!!!!"
            echo " !!!    ${globaldir}/${globalgfile}${fhour} "
            echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
            echo " "
            set -x
            exit
        fi

        set +x
        echo "Timing: fhour= $fhour before wgrib loop at `date`"
        set -x

        for parm in ${wgrib_parmlist}
        do
          case ${parm} in
            "SurfaceU")
              grep "UGRD:10 m " globalgfile.ix | \
                              $wgrib2 -i $gfile -append -grib \
                              ${PERTDATA}/master.globalgribfile.${PDY}${CYL}.f${fhour} ;;
            "SurfaceV")
              grep "VGRD:10 m " globalgfile.ix | \
                              $wgrib2 -i $gfile -append -grib \
                              ${PERTDATA}/master.globalgribfile.${PDY}${CYL}.f${fhour} ;;
                     *)
              grep "${parm}" globalgfile.ix | $wgrib2 -i $gfile -append -grib \
                              ${PERTDATA}/master.globalgribfile.${PDY}${CYL}.f${fhour} ;;
          esac
        done

        set +x
        echo "Timing: fhour= $fhour after wgrib loop at `date`"
        set -x

        global_master_file=${PERTDATA}/master.globalgribfile.${PDY}${CYL}.f${fhour}

          cat ${global_master_file} >> ${PERTDATA}/globalgribfile.${PDY}${CYL}

      fi
  
    done
  
    if [ ${gribver} -eq 1 ]; then
      $gix ${PERTDATA}/globalgribfile.${PDY}${CYL} ${PERTDATA}/globalixfile.${PDY}${CYL}
    else
      $g2ix ${PERTDATA}/globalgribfile.${PDY}${CYL} ${PERTDATA}/globalixfile.${PDY}${CYL}
    fi
      gribfile=${PERTDATA}/globalgribfile.${PDY}${CYL}
      ixfile=${PERTDATA}/globalixfile.${PDY}${CYL}

   fi

# --------------------------------------------
#   Process Ensemble perturbation, if selected
# --------------------------------------------

   if [ ${model} -eq 10 ]; then

    if [ ! -f ${PERTDATA}/ens${pert}gribfile.${PDY}${CYL} ]; then

      for fhour in ${fcsthrs}
      do

        echo "fcst hours: " $fhour
         if [ ${fhour} -eq 99 ]; then
            continue
         fi

         total_file_cnt=$(($total_file_cnt+1))

         if [ ! -s ${ensdir}/${ensgfile}${fhour} ]; then
            set +x
            echo " "
            echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! "
            echo " !!! ENSEMBLE ${PERT} File missing: "
            echo " !!!                          ${ensdir}/${ensgfile}${fhour}  "
            echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! "
            echo " "
            echo " !!! Please re-run the job when ENSEMBLE ${PERT} file is available ..... "
            echo " "
            missing_file_cnt=$(($missing_file_cnt+1))
            set -x
            err_exit " FAILED ${jobid} - MISSING ENSEMBLE ${PERT} FILE IN TRACKER SCRIPT - ABNORMAL EXIT"
#            continue
         fi

      if [ ${gribver} -eq 1 ]; then
         gfile=${ensdir}/${ensgfile}${fhour}
         $wgrib -s $gfile >ens.ix

         for parm in ${wgrib_parmlist}
         do
            case ${parm} in
            "SurfaceU")
            grep "UGRD:10 m " ens.ix | $wgrib -s $gfile -i -grib -append \
                                -o ${PERTDATA}/ens${pert}gribfile.${PDY}${CYL} ;;
            "SurfaceV")
            grep "VGRD:10 m " ens.ix | $wgrib -s $gfile -i -grib -append \
                                -o ${PERTDATA}/ens${pert}gribfile.${PDY}${CYL} ;;
                 *)
            grep "${parm}" ens.ix | $wgrib -s $gfile -i -grib -append \
                                -o ${PERTDATA}/ens${pert}gribfile.${PDY}${CYL} ;;
            esac

         done

    else

         gfile=${ensdir}/${ensgfile}${fhour}
         $wgrib2 -s $gfile >ens.ix

        for parm in ${wgrib_parmlist}
        do
          case ${parm} in
            "SurfaceU")
              grep "UGRD:10 m " ens.ix | \
                              $wgrib2 -i $gfile -append -grib \
                              ${PERTDATA}/ens${pert}gribfile.${PDY}${CYL} ;;
            "SurfaceV")
              grep "VGRD:10 m " ens.ix | \
                              $wgrib2 -i $gfile -append -grib \
                              ${PERTDATA}/ens${pert}gribfile.${PDY}${CYL} ;;
                     *)
              grep "${parm}" ens.ix | $wgrib2 -i $gfile -append -grib \
                              ${PERTDATA}/ens${pert}gribfile.${PDY}${CYL} ;;
          esac
        done
    fi

      done

    if [ ${gribver} -eq 1 ]; then
      $gix ${PERTDATA}/ens${pert}gribfile.${PDY}${CYL} ${PERTDATA}/ens${pert}ixfile.${PDY}${CYL}
     else
      $g2ix ${PERTDATA}/ens${pert}gribfile.${PDY}${CYL} ${PERTDATA}/ens${pert}ixfile.${PDY}${CYL}
    fi
      gribfile=${PERTDATA}/ens${pert}gribfile.${PDY}${CYL}
      ixfile=${PERTDATA}/ens${pert}ixfile.${PDY}${CYL}

   else  ## files are already available
      gribfile=${PERTDATA}/ens${pert}gribfile.${PDY}${CYL}
      ixfile=${PERTDATA}/ens${pert}ixfile.${PDY}${CYL}
   fi   ## files are already available end here

   fi

# -------------------------------------
#   Process GDAS, if selected
# -------------------------------------

   if [ ${model} -eq 8 ]; then

      if [ -s ${PERTDATA}/gdasgribfile.${PDY}${CYL} ]; then
         rm ${PERTDATA}/gdasgribfile.${PDY}${CYL}
      fi

      for fhour in ${fcsthrs}
      do

         if [ ${fhour} -eq 99 ]; then
            continue
         fi

         total_file_cnt=$(($total_file_cnt+1))
         if [ ! -s ${gdasdir}/${gdasgfile}${fhour} ]; then
            set +x
            echo " "
            echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
            echo " !!! GDAS File missing: ${gdasdir}/${gdasgfile}${fhour}"
            echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
            echo " "
            echo " !!! Please re-run the job when GDAS file is available ..... "
            echo " "
            missing_file_cnt=$(($missing_file_cnt+1))
            set -x
            err_exit " FAILED ${jobid} - MISSING GDAS FILE IN TRACKER SCRIPT - ABNORMAL EXIT"
#            continue
         fi

         gfile=${gdasdir}/${gdasgfile}${fhour}
         $wgrib -s $gfile >gdas.ix

         for parm in ${wgrib_parmlist}
         do
            case ${parm} in
            "SurfaceU")
            grep "UGRD:10 m " gdas.ix | $wgrib -s $gfile -i -grib -append \
                                  -o ${PERTDATA}/gdasgribfile.${PDY}${CYL} ;;
            "SurfaceV")
            grep "VGRD:10 m " gdas.ix | $wgrib -s $gfile -i -grib -append \
                                  -o ${PERTDATA}/gdasgribfile.${PDY}${CYL} ;;
                 *)
            grep "${parm}" gdas.ix | $wgrib -s $gfile -i -grib -append    \
                                  -o ${PERTDATA}/gdasgribfile.${PDY}${CYL} ;;
            esac
         done

      done

      $gix ${PERTDATA}/gdasgribfile.${PDY}${CYL} ${PERTDATA}/gdasixfile.${PDY}${CYL}
      gribfile=${PERTDATA}/gdasgribfile.${PDY}${CYL}
      ixfile=${PERTDATA}/gdasixfile.${PDY}${CYL}

   fi

# ------------------------------
#   Process UKMET, if selected
# ------------------------------
  
   if [ ${model} -eq 3 ]; then

      if [ -s ${PERTDATA}/ukmetgribfile.${PDY}${CYL} ]; then 
         rm ${PERTDATA}/ukmetgribfile.${PDY}${CYL}
      fi

      for fhour in ${fcsthrs}
      do

         if [ ${fhour} -eq 99 ]; then
            continue
         fi

         total_file_cnt=$(($total_file_cnt+1))
    if [ ${gribver} -eq 1 ]; then
      gfile=${ukmetdir}/pgbf${fhour}.${ukmetgfile}

         if [ ! -s ${ukmetdir}/pgbf${fhour}.${ukmetgfile} ]; then
            set +x
            echo " "
            echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
            echo " !!! UKMET File missing: ${ukmetdir}/${ukmetgfile}${fhour}"
            echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
            echo " "
            echo " !!! Please re-run the job when UKMET file is available ..... "
            echo " "
            missing_file_cnt=$(($missing_file_cnt+1))
            set -x
            err_exit " FAILED ${jobid} - MISSING UKMET FILE IN TRACKER SCRIPT - ABNORMAL EXIT"
#            continue
         fi
      $wgrib -s $gfile >ukmet.ix

         for parm in ${wgrib_parmlist}
         do

          case ${parm} in
            "SurfaceU")
              grep "UGRD:sfc" ukmet.ix | \
                $wgrib -s $gfile -i -grib -append \
                             -o ${PERTDATA}/ukmetgribfile.${PDY}${CYL} ;;
            "SurfaceV")
              grep "VGRD:sfc" ukmet.ix | \
                $wgrib -s $gfile -i -grib -append \
                             -o ${PERTDATA}/ukmetgribfile.${PDY}${CYL} ;;
            *)
            grep "${parm}" ukmet.ix | $wgrib -s $gfile -i -grib -append \
                             -o ${PERTDATA}/ukmetgribfile.${PDY}${CYL} ;;
            esac
         done

         if [ ${PHASEFLAG} = 'y' ]; then
           egrep "${wgrib_phase_parmlist}" ukmet.ix | grep mb | \
                                     $wgrib -s $gfile -i -grib -append \
                                  -o ${PERTDATA}/ukmetgribfile.${PDY}${CYL}
         fi
    else  ## gribver = 2
      gfile=${ukmetdir}/${ukmetgfile}${fhour}.grib2
         if [ ! -s $gfile ]; then
            set +x
            echo " "
            echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
            echo " !!! UKMET File missing: $gfile                         "
            echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
            echo " "
            echo " !!! Please re-run the job when UKMET file is available ..... "
            echo " "
            missing_file_cnt=$(($missing_file_cnt+1))
            set -x
            err_exit " FAILED ${jobid} - MISSING UKMET FILE IN TRACKER SCRIPT - ABNORMAL EXIT"
         fi
      $wgrib2 -s $gfile >ukmet.ix

         for parm in ${wgrib_parmlist}
         do
          case ${parm} in
            "SurfaceU")
              grep "UGRD:sfc" ukmet.ix | \
                $wgrib2 -i $gfile -append -grib \
                             -o ${PERTDATA}/ukmetgribfile.${PDY}${CYL} ;;
            "SurfaceV")
              grep "VGRD:sfc" ukmet.ix | \
                $wgrib2 -i $gfile -append -grib \
                             -o ${PERTDATA}/ukmetgribfile.${PDY}${CYL} ;;
            *)
            grep "${parm}" ukmet.ix | $wgrib2 -i $gfile -append -grib \
                              ${PERTDATA}/ukmetgribfile.${PDY}${CYL}
           esac
         done
     fi ## gribver ends

      done

    if [ ${gribver} -eq 1 ]; then
      $gix ${PERTDATA}/ukmetgribfile.${PDY}${CYL} ${PERTDATA}/ukmetixfile.${PDY}${CYL}
     else
      $g2ix ${PERTDATA}/ukmetgribfile.${PDY}${CYL} ${PERTDATA}/ukmetixfile.${PDY}${CYL}
    fi
      gribfile=${PERTDATA}/ukmetgribfile.${PDY}${CYL}
      ixfile=${PERTDATA}/ukmetixfile.${PDY}${CYL}

   fi

# ------------------------------
#   Process ECMWF, if selected
# ------------------------------

# As of Summer, 2005, ECMWF is now sending us high res (1-degree) data on
# a global grid with 12-hourly resolution out to 240h.  Previously, we
# only got their data on a low res (2.5-degree) grid, from 35N-35S, with
# 24-hourly resolution out to only 168h.

if [ ${model} -eq 4 ] ; then

    if [ -s ${PERTDATA}/ecgribfile.${PDY}${CYL} ] ; then
      rm ${PERTDATA}/ecgribfile.${PDY}${CYL}
    fi

    immddhh=`echo ${PDY}${CYL}| cut -c5-`
    ict=0

    for fhour in ${fcsthrs}
    do

      if [ ${fhour} -eq 99 ]
      then
        continue
      fi

      let fhr=ict*6
      echo "fhr= $fhr  fhour= $fhour"
      fmmddhh=` ${ndate} ${fhour} ${PDY}${CYL} | cut -c5- `
      ec_hires_orig=DCD${immddhh}00${fmmddhh}001
#      ec_hires_orig=ecens_DCD${immddhh}00${fmmddhh}001

      total_file_cnt=$(($total_file_cnt+1))
      if [ ! -s ${ecmwfdir}/${ec_hires_orig} ]
      then
        set +x
        echo " "
        echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        echo " !!! ECMWF File missing: ${ecmwfdir}/${ec_hires_orig}"
        echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        missing_file_cnt=$(($missing_file_cnt+1))
        echo " "
        echo " !!! Please re-run the job when ECMWF file is available ..... "
        echo " "
        set -x
        err_exit " FAILED ${jobid} - MISSING ECMWF FILE IN TRACKER SCRIPT - ABNORMAL EXIT"
#        continue
      fi

      ecfile=${ecmwfdir}/${ec_hires_orig}
      $wgrib -s $ecfile >ec.ix

      for parm in ${wgrib_ec_hires_parmlist}
      do
        grep "${parm}" ec.ix | $wgrib -s $ecfile -i -grib -append  -o ${PERTDATA}/ecgribfile.${PDY}${CYL}

      done

         if [ ${PHASEFLAG} = 'y' ]; then
           egrep "${wgrib_ec_phase_parmlist}" ec.ix | grep mb | \
                                     $wgrib -s $ecfile -i -grib -append \
                                  -o ${PERTDATA}/ecgribfile.${PDY}${CYL}
         fi

      let ict=ict+1

    done

  $gix ${PERTDATA}/ecgribfile.${PDY}${CYL} ${PERTDATA}/ecixfile.${PDY}${CYL}
  gribfile=${PERTDATA}/ecgribfile.${PDY}${CYL}
  ixfile=${PERTDATA}/ecixfile.${PDY}${CYL}

fi

# --------------------------------------------------
#   Process ECMWF Ensemble perturbation, if selected
# --------------------------------------------------

if [ ${model} -eq 21 ]
then

    if [ -s ${PERTDATA}/ece${pert}gribfile.${PDY}${CYL} ]
    then
      rm ${PERTDATA}/ece${pert}gribfile.${PDY}${CYL}
    fi

    if [ ${pert_posneg} = 'n' ]; then
      posneg=2
    elif [ ${pert_posneg} = 'p' ]; then
      posneg=3
    elif [ ${pert_posneg} = 'c' ]; then
      # low-res control
      posneg=1
    else
      set +x
      echo " "
      echo "!!! ERROR: ECMWF PERT ID NOT RECOGNIZED"
      echo "!!! pert_posneg= ${pert_posneg}"
      echo " "
      set -x
      err_exit " FAILED ${jobid} - ERROR IN TRACKER SCRIPT - ABNORMAL EXIT"
      exit 8
    fi

    pnum=${pert_num}
    if [ ${pnum} -lt 10 ]; then
      pnum=` echo $pnum | cut -c2-2`
    fi

    if [ ${pnum} -eq 0 ]; then
      # low-res control
      pnum=2
    fi

    pert_grep_str=" 0 0 0 1 ${posneg} ${pnum} 1 "

    glo=${PERTDATA}/ece.lores.cut.${PDY}${CYL}
    xlo=${PERTDATA}/ece.lores.cut.${PDY}${CYL}.i

    if [ -s ${glo} ]; then rm ${glo}; fi
    if [ -s ${xlo} ]; then rm ${xlo}; fi

    grid="255 0 360 181 90000 0000 128 -90000 -1000 1000 1000 0"

    # This next part simply uses wgrib to parse out
    # the member records for each variable from each
    # respective enspost file.

    for var in u500 v500 u850 v850 mslp
    do

      ecegfile=ensposte.t${CYL}z.${var}hr

      total_file_cnt=$(($total_file_cnt+1))
      if [ ! -s ${ecedir}/${ecegfile} ]
      then
        set +x
        echo " "
        echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        echo " !!! ECMWF ENSEMBLE POST File missing: ${ecedir}/${ecegfile}"
        echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        echo " "
        echo " !!! Please re-run the job when ECMWF file is available ..... "
        echo " "
        missing_file_cnt=$(($missing_file_cnt+1))
        set -x
        err_exit " FAILED ${jobid} - MISSING ECMWF FILE IN TRACKER SCRIPT - ABNORMAL EXIT"
#        continue
      fi

      ece_orig=${ecedir}/${ecegfile}

      $wgrib -PDS10 ${ece_orig} | grep "${pert_grep_str}" |  awk -F:TR= '{print $1}' | $wgrib -i ${ece_orig} -grib -append -o ${glo}

    done

    # ECMWF data are 2.5-degree data, so for consistency
    # with the NCEP ensemble data, we now use copygb to
    # interpolate down to 1-degree.  The -g3 in the copygb
    # statement is for grid 3, a 1x1 global grid (AVN).
### 20151127: The new eens is 1x1 resolution, so no need to cbg --Guang Ping Lou

##    ${gix} ${glo} ${xlo}
    gfile=${PERTDATA}/ece${pert}gribfile.${PDY}${CYL}
##    $cgb -g"${grid}" -a ${glo} ${xlo} ${gfile}
    mv ${glo} ${gfile}

    $gix ${PERTDATA}/ece${pert}gribfile.${PDY}${CYL} ${PERTDATA}/ece${pert}ixfile.${PDY}${CYL}

  gribfile=${PERTDATA}/ece${pert}gribfile.${PDY}${CYL}
  ixfile=${PERTDATA}/ece${pert}ixfile.${PDY}${CYL}

fi



# ------------------------------
#   Process NAVGEM, if selected
# ------------------------------

   if [ ${model} -eq 7 ]; then
         set -x

      if [ -s ${PERTDATA}/nvgmgribfile.${PDY}${CYL} ]; then
         rm ${PERTDATA}/nvgmgribfile.${PDY}${CYL}
      fi

      for fhour in ${fcsthrs}
      do

         if [ ${fhour} -eq 99 ]; then
            continue
         fi

      total_file_cnt=$(($total_file_cnt+1))
    if [ ${gribver} -eq 1 ]; then
      gfile=${nvgmdir}/${nvgmgfile}${fhour}
      $wgrib -s $gfile >nvgm.ix
      if [ ! -s ${gfile} ]; then
         set +x
         echo " "
         echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
         echo " !!! NAVGEM File missing: ${gfile}"
         echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
         echo " "
         echo " !!! Please re-run the job when NAVGEM file is available ..... "
         echo " "
         missing_file_cnt=$(($missing_file_cnt+1))
         #err_exit " FAILED ${jobid} - MISSING NAVGEM FILE IN TRACKER SCRIPT - ABNORMAL EXIT"
         exit 0
         set -x
      fi

         for parm in ${wgrib_parmlist}
         do
            case ${parm} in
            "SurfaceU")
            grep "UGRD:10 m " nvgm.ix |  \
            $wgrib -s $gfile -i -grib -append -o ${PERTDATA}/nvgmgribfile.${PDY}${CYL} ;;
            "SurfaceV")
            grep "VGRD:10 m " nvgm.ix | \
            $wgrib -s $gfile -i -grib -append -o ${PERTDATA}/nvgmgribfile.${PDY}${CYL} ;;
                 *)
            grep "${parm}" nvgm.ix |  \
            $wgrib -s $gfile -i -grib -append -o ${PERTDATA}/nvgmgribfile.${PDY}${CYL} ;;
            esac
         done

    else  ## gribver = 2

      gfile=${nvgmdir}/${nvgmgfile}${fhour}-${PDY}${CYL}-NOAA-halfdeg.gr2
      $wgrib2 -s $gfile >nvgm.ix
      if [ ! -s ${gfile} ]; then
         set +x
         echo " "
         echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
         echo " !!! NAVGEM File missing: ${gfile}"
         echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
         echo " "
         echo " !!! Please re-run the job when NAVGEM file is available ..... "
         echo " "
         missing_file_cnt=$(($missing_file_cnt+1))
         #err_exit " FAILED ${jobid} - MISSING NAVGEM FILE IN TRACKER SCRIPT - ABNORMAL EXIT"
         exit 0
         set -x
      fi

        for parm in ${wgrib_parmlist}
        do
          case ${parm} in
            "SurfaceU")
              grep "UGRD:10 m " nvgm.ix | \
                              $wgrib2 -i $gfile -append -grib \
                              ${PERTDATA}/nvgmgribfile.${PDY}${CYL} ;;
            "SurfaceV")
              grep "VGRD:10 m " nvgm.ix | \
                              $wgrib2 -i $gfile -append -grib \
                              ${PERTDATA}/nvgmgribfile.${PDY}${CYL} ;;
                     *)
              grep "${parm}" nvgm.ix | $wgrib2 -i $gfile -append -grib \
                              ${PERTDATA}/nvgmgribfile.${PDY}${CYL} ;;
          esac
        done

      fi  ## gribver ends
      
      done

    if [ ${gribver} -eq 1 ]; then
      $gix ${PERTDATA}/nvgmgribfile.${PDY}${CYL} ${PERTDATA}/nvgmixfile.${PDY}${CYL}
      ixfile=${PERTDATA}/nvgmixfile.${PDY}${CYL}
      gribfile=${PERTDATA}/nvgmgribfile.${PDY}${CYL}
     else
      grid2="0 6 0 0 0 0 0 0 360 181 0 0 90000000 000000000 48 -90000000 359000000 1000000 1000000 0"
      $g2ix ${PERTDATA}/nvgmgribfile.${PDY}${CYL} ${PERTDATA}/nvgmixfile.${PDY}${CYL}
      $cgb2 -g "$grid2" ${PERTDATA}/nvgmgribfile.${PDY}${CYL} ${PERTDATA}/nvgmixfile.${PDY}${CYL} \
                                     ${PERTDATA}/nvgmgribfileLL.${PDY}${CYL}
      ixfile=${PERTDATA}/nvgmixfile.${PDY}${CYL}
      gribfile=${PERTDATA}/nvgmgribfileLL.${PDY}${CYL}
    fi
   fi

# ------------------------------
#   Process FNMOC Ensemble, if selected
# ------------------------------

if [ ${model} -eq 23 ]
then

    if [ -s ${PERTDATA}/fensgribfile.${pert}.${PDY}${CYL} ]
    then
      rm ${PERTDATA}/fensgribfile.${pert}.${PDY}${CYL}
    fi

    >${PERTDATA}/fensgribfile.${pert}.${PDY}${CYL}

    for fhr in ${fcsthrs}
    do

      if [ ${fhr} -eq 99 ]
      then
        continue
      fi

           if [ ${gribver} -eq 1 ]; then
      fensfile=${fensdir}/fnmoc_ge${pert}.t${CYL}z.pgrbaf${fhr}

      if [ ! -s ${fensfile} ]
      then
        set +x
        echo " "
        echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        echo " !!! FNMOC Ens File missing: ${fensfile}"
        echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        echo " "
        set -x
        err_exit " FAILED ${jobid} - MISSING ENSEMBLE ${PERT} FILE IN TRACKER SCRIPT - ABNORMAL EXIT"
#        continue
      fi

      $wgrib -s $fensfile >fens.ix

      for parm in ${wgrib_parmlist}
      do
        case ${parm} in
          "SurfaceU")
            grep "UGRD:10 m " fens.ix | grep -v pv | $wgrib -s $fensfile -i -grib -append -o \
                                    ${PERTDATA}/fensgribfile.${pert}.${PDY}${CYL} ;;
          "SurfaceV")
            grep "VGRD:10 m " fens.ix | grep -v pv | $wgrib -s $fensfile -i -grib -append -o \
                                    ${PERTDATA}/fensgribfile.${pert}.${PDY}${CYL} ;;
                   *)
            grep "${parm}" fens.ix | grep -v pv | $wgrib -s $fensfile -i -grib -append -o \
                                    ${PERTDATA}/fensgribfile.${pert}.${PDY}${CYL} ;;
        esac
      done
    else

         fensfile=${fensdir}/ENSEMBLE.MET.fcst_et0${pert_num}.${fhr}.${PDY}${CYL}
         $wgrib2 -s $fensfile >fens.ix

        for parm in ${wgrib_parmlist}
        do
          case ${parm} in
            "SurfaceU")
              grep "UGRD:10 m " fens.ix | \
                              $wgrib2 -i $fensfile -append -grib \
                              ${PERTDATA}/fens${pert}gribfile.${PDY}${CYL} ;;
            "SurfaceV")
              grep "VGRD:10 m " fens.ix | \
                              $wgrib2 -i $fensfile -append -grib \
                              ${PERTDATA}/fens${pert}gribfile.${PDY}${CYL} ;;
                     *)
              grep "${parm}" fens.ix | $wgrib2 -i $fensfile -append -grib \
                              ${PERTDATA}/fens${pert}gribfile.${PDY}${CYL} ;;
          esac
        done
    fi

    done

    if [ ${gribver} -eq 1 ]; then
      gribfile=${PERTDATA}/fensgribfile.${pert}.${PDY}${CYL}
      ixfile=${PERTDATA}/fensixfile.${pert}.${PDY}${CYL}
      $gix $gribfile $ixfile
     else
      gribfile=${PERTDATA}/fens${pert}gribfile.${PDY}${CYL}
      ixfile=${PERTDATA}/fens${pert}ixfile.${PDY}${CYL}
      $g2ix $gribfile $ixfile
    fi

 fi

# ----------------------------------------------
#   *** ENSEMBLE RELOCATION ONLY FOLLOWS....***
#   Process Ensemble perturbation, if selected
# ----------------------------------------------

   if [ ${model} -eq 11 ]; then
    
      if [ -s ${PERTDATA}/ensr${pert}gribfile.${PDY}${CYL} ]; then
         rm ${PERTDATA}/ensr${pert}gribfile.${PDY}${CYL}
      fi
    
      for fhour in ${fcsthrs}
      do
    
         if [ ${fhour} -eq 99 ]; then
            continue
         fi
      
         total_file_cnt=$(($total_file_cnt+1))
         if [ ! -s ${ensrdir}/${ensrgfile}${fhtur} ]; then
            set +x
            echo " "
            echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! "
            echo " !!! ENSEMBLE RELOCATION ${PERT} File missing:               "
            echo " !!!                     ${ensrdir}/${ensrgfile}${fhour}     "
            echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! "
            echo " "
            echo " !!! Please re-run the job when ENSEMBLE RELOCATION ${PERT} file is available ..... "
            echo " "
            missing_file_cnt=$(($missing_file_cnt+1))
            set -x
            err_exit " FAILED ${jobid} - MISSING ENSEMBLE RELOCATION ${PERT} FILE IN TRACKER SCRIPT - ABNORMAL EXIT"
#            continue
         fi
      
         gfile=${ensrdir}/${ensrgfile}${fhour}
         $wgrib -s $gfile >ensr.ix

         for parm in ${wgrib_parmlist}
         do
            case ${parm} in
            "SurfaceU")
            grep "UGRD:10 m " ensr.ix | $wgrib -s $gfile -i -grib -append \
                               -o ${PERTDATA}/ensr${pert}gribfile.${PDY}${CYL} ;;
            "SurfaceV")
            grep "VGRD:10 m " ensr.ix | $wgrib -s $gfile -i -grib -append \
                               -o ${PERTDATA}/ensr${pert}gribfile.${PDY}${CYL} ;;
                 *)
            grep "${parm}" ensr.ix | $wgrib -s $gfile -i -grib -append \
                               -o ${PERTDATA}/ensr${pert}gribfile.${PDY}${CYL} ;;
            esac

         done

      done

      $gix ${PERTDATA}/ensr${pert}gribfile.${PDY}${CYL} ${PERTDATA}/ensr${pert}ixfile.${PDY}${CYL}
      gribfile=${PERTDATA}/ensr${pert}gribfile.${PDY}${CYL}
      ixfile=${PERTDATA}/ensr${pert}ixfile.${PDY}${CYL}

   fi

# ---------------------------------------------------
# * GFS ensemble control analysis (for relocation) *
# ---------------------------------------------------

   if [ ${model} -eq 12 ]; then
    
      if [ -s ${PERTDATA}/aeargribfile.${PDY}${CYL} ]; then
         rm ${PERTDATA}/aeargribfile.${PDY}${CYL}
      fi
    
      for fhour in ${fcsthrs}
      do
    
         if [ ${fhour} -eq 99 ]; then
            continue
         fi
     
         total_file_cnt=$(($total_file_cnt+1)) 
         if [ ! -s ${aeardir}/${aeargfile}${fhour} ]; then
            set +x
            echo " "
            echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! "
            echo " !!! GFS control analysis file missing:                      "
            echo " !!!                      ${aeardir}/${aeargfile}${fhour}    "
            echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! "
            echo " "
            echo " !!! Please re-run the job when GFS file is available ..... "
            echo " "
            missing_file_cnt=$(($missing_file_cnt+1))
            set -x
            err_exit " FAILED ${jobid} - MISSING GFS FILE IN TRACKER SCRIPT - ABNORMAL EXIT"
#            continue
         fi
      
         gfile=${aeardir}/${aeargfile}${fhour}
         $wgrib -s $gfile >aear.ix

         for parm in ${wgrib_parmlist}
         do
            case ${parm} in
            "SurfaceU")
            grep "UGRD:10 m " aear.ix | $wgrib -s $gfile -i -grib -append \
                                  -o ${PERTDATA}/aeargribfile.${PDY}${CYL} ;;
            "SurfaceV")
            grep "VGRD:10 m " aear.ix | $wgrib -s $gfile -i -grib -append \
                                  -o ${PERTDATA}/aeargribfile.${PDY}${CYL} ;;
                 *)
            grep "${parm}" aear.ix | $wgrib -s $gfile -i -grib -append \
                                  -o ${PERTDATA}/aeargribfile.${PDY}${CYL} ;;
            esac

         done

      done

      $gix ${PERTDATA}/aeargribfile.${PDY}${CYL} ${PERTDATA}/aearixfile.${PDY}${CYL}
      gribfile=${PERTDATA}/aeargribfile.${PDY}${CYL}
      ixfile=${PERTDATA}/aearixfile.${PDY}${CYL}

   fi

# --------------------------------------------------
#   Process SREF Ensemble perturbation, if selected
# --------------------------------------------------

   if [ ${model} -eq 13 ]; then

      grid='255 0 381 161 80000 160000 128 0000 350000  500  500 0'
      grid2="0 6 0 0 0 0 0 0 381 161 0 0 80000000 160000000 48 0000000 350000000 500000 500000 0"
## grid 221(?)      grid='255 0 381 161 80000 160000 128 0000 350000  500  500 0'
##      grid='255 0 301 141 70000 190000 128 0000 340000  500  500 0'
#      grid='255 0 221 101 60000 200000 128 10000 310000  500  500 0'

      if [ ${regflag} -eq 0 ]; then
         if [ ${trkrtype} = 'tracker' ]; then
            set +x
            echo " "
            echo " !!! SREF ensemble has been selected, but there are no storms"
            echo " !!! in the TC Vitals file that can be processed.  That is, "
            echo " !!! there are no Vitals records from NHC.  The vitals "
            echo " !!! records that are in the updated vitals file must be from"
            echo " !!! another cyclone forecast center, and the SREF domain "
            echo " !!! does not extend to any region other than that covered "
            echo " !!! by NHC.  Exiting....."
            set -x
            err_exit " FAILED ${jobid} - ERROR IN TRACKER SCRIPT - ABNORMAL EXIT" 
            exit 1
         fi
    fi

    if [ -s ${PERTDATA}/sref${pert}gribfile.${PDY}${CYL} ]; then
       rm ${PERTDATA}/sref${pert}gribfile.${PDY}${CYL}
    fi

    for fhour in ${fcsthrs}
    do

       if [ ${fhour} -eq 99 ]; then
          continue
       fi

       total_file_cnt=$(($total_file_cnt+1))
       if [ ${gribver} -eq 1 ]; then
       if [ ! -s ${srefdir}/${srefgfile}${fhour} ]; then
          set +x
          echo " "
          echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
          echo " !!! SREF File missing: ${srefdir}/${srefgfile}${fhour}!"
          echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
          echo " "
          echo " !!! Please re-run the job when SREF file is available ..... "
          echo " "
          missing_file_cnt=$(($missing_file_cnt+1))
          set -x
          err_exit " FAILED ${jobid} - MISSING SREF FILE IN TRACKER SCRIPT - ABNORMAL EXIT"
#          continue
       fi

       if [ -s ${PERTDATA}/tmpsrefixfile ]; then rm ${PERTDATA}/tmpsrefixfile; fi
          $gix ${srefdir}/${srefgfile}${fhour} ${PERTDATA}/tmpsrefixfile
          x1=${PERTDATA}/tmpsrefixfile

          set +x
          echo " "
          echo " Extracting SREF GRIB data for pert ${pert} "
          echo "                for forecast hour = $fhour"
          echo " "
          set -x

          g1=${srefdir}/${srefgfile}${fhour}

          $cgb -g"$grid" -k'4*-1 33 100 850' $g1 $x1 ${PERTDATA}/srefllu850.${pert}.grb.f${fhour};   rcc1=$?
          $cgb -g"$grid" -k'4*-1 33 100 700' $g1 $x1 ${PERTDATA}/srefllu700.${pert}.grb.f${fhour};   rcc2=$?
          $cgb -g"$grid" -k'4*-1 33 100 500' $g1 $x1 ${PERTDATA}/srefllu500.${pert}.grb.f${fhour};   rcc3=$?
          $cgb -g"$grid" -k'4*-1 33 105 10'  $g1 $x1 ${PERTDATA}/srefllu10m.${pert}.grb.f${fhour};   rcc4=$?
          $cgb -g"$grid" -k'4*-1 41 100 850' $g1 $x1 ${PERTDATA}/srefllav850.${pert}.grb.f${fhour};  rcc5=$?
          $cgb -g"$grid" -k'4*-1 41 100 700' $g1 $x1 ${PERTDATA}/srefllav700.${pert}.grb.f${fhour};  rcc6=$?
          $cgb -g"$grid" -k'4*-1  7 100 850' $g1 $x1 ${PERTDATA}/srefllz850.${pert}.grb.f${fhour};   rcc7=$?
          $cgb -g"$grid" -k'4*-1  7 100 700' $g1 $x1 ${PERTDATA}/srefllz700.${pert}.grb.f${fhour};   rcc8=$?
          $cgb -g"$grid" -k'4*-1  2 102   0' $g1 $x1 ${PERTDATA}/srefllmslp.${pert}.grb.f${fhour};   rcc9=$?

          if [ $rcc1 -eq 134 -o $rcc2 -eq 134 -o $rcc3 -eq 134 -o $rcc4 -eq 134 -o $rcc5 -eq 134 -o \
              $rcc6 -eq 134 -o $rcc7 -eq 134 -o $rcc8 -eq 134 -o $rcc9 -eq 134 ]
          then
             set +x
             echo " "
             echo "!!! ERROR using $cgb to interpolate sref data.  We will stop"
             echo "!!! execution because some variables may have been copied"
             echo "!!! okay, while some obviously have not, and that could lead"
             echo "!!! to unreliable results from the tracker.  Check to make"
             echo "!!! sure you've allocated enough memory for this job."
             echo "!!! Exiting.... "
             echo " "
             set -x
             err_exit " FAILED ${jobid} - ERROR INTERPOLATING SREF DATA IN TRACKER SCRIPT - ABNORMAL EXIT"
          fi

          cat ${PERTDATA}/srefllu850.${pert}.grb.f${fhour}   ${PERTDATA}/srefllu700.${pert}.grb.f${fhour} \
          ${PERTDATA}/srefllu500.${pert}.grb.f${fhour}   ${PERTDATA}/srefllz850.${pert}.grb.f${fhour} \
          ${PERTDATA}/srefllz700.${pert}.grb.f${fhour}   ${PERTDATA}/srefllmslp.${pert}.grb.f${fhour} \
          ${PERTDATA}/srefllav850.${pert}.grb.f${fhour}  ${PERTDATA}/srefllav700.${pert}.grb.f${fhour} \
          ${PERTDATA}/srefllu10m.${pert}.grb.f${fhour} \
          >>${PERTDATA}/sref${pert}gribfile.${PDY}${CYL}

   else ##${gribver} -eq 2
          gfile=${srefdir}/${srefgfile}${fhour}.grib2

       if [ -s $gfile ]; then
          echo $gfile
          # Try to wgrib the primary file....
          $wgrib2 -s $gfile > ifile.ix
        else

          set +x
          echo " "
          echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
          echo " !!! SREF File missing: ${srefdir}/${srefgfile}${fhour}.grib2!"
          echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
          echo " "
          echo " !!! Please re-run the job when SREF file is available ..... "
          echo " "
          missing_file_cnt=$(($missing_file_cnt+1))
          set -x
#          err_exit " FAILED ${jobid} - MISSING SREF FILE IN TRACKER SCRIPT - ABNORMAL EXIT"
          continue
       fi

        for parm in ${wgrib_parmlist}
        do
          case ${parm} in
            "SurfaceU")
              grep "UGRD:10 m " ifile.ix | \
                              $wgrib2 -i $gfile -append -grib \
                              ${PERTDATA}/sref${pert}gribfile.${PDY}${CYL}.f${fhour} ;;
            "SurfaceV")
              grep "VGRD:10 m " ifile.ix | \
                              $wgrib2 -i $gfile -append -grib \
                              ${PERTDATA}/sref${pert}gribfile.${PDY}${CYL}.f${fhour} ;;
                     *)
              grep "${parm}" ifile.ix | $wgrib2 -i $gfile -append -grib \
                              ${PERTDATA}/sref${pert}gribfile.${PDY}${CYL}.f${fhour} ;;
          esac
        done

          if [ $fhour -ne 99 ]; then
            set +x
            echo "+++ TIMING: AFTER  SREF COPYGB for fhour= $fhour  ---> `date`"
            set -x
          fi

        sref_file=${PERTDATA}/sref${pert}gribfile.${PDY}${CYL}.f${fhour}

          cat ${sref_file} >> ${PERTDATA}/${pert}gribfile.pgrb.${PDY}${CYL}

      fi

       done

    if [ ${gribver} -eq 1 ]; then
       $gix ${PERTDATA}/sref${pert}gribfile.${PDY}${CYL} ${PERTDATA}/sref${pert}ixfile.${PDY}${CYL}
       gribfile=${PERTDATA}/sref${pert}gribfile.${PDY}${CYL}
       ixfile=${PERTDATA}/sref${pert}ixfile.${PDY}${CYL}
    else
      ${wgrib2} ${pert}gribfile.pgrb.${PDY}${CYL} \
              -new_grid_vectors none       \
              -new_grid latlon "140:421:0.5"  "0:180:0.5"  \
              sref${pert}gribfile.${PDY}${CYL}
       $g2ix sref${pert}gribfile.${PDY}${CYL} ${PERTDATA}/sref${pert}ixfile.${PDY}${CYL}
       gribfile=${PERTDATA}/sref${pert}gribfile.${PDY}${CYL}
       ixfile=${PERTDATA}/sref${pert}ixfile.${PDY}${CYL}
    fi

   fi

# ------------------------------------------------------
#   Process Canadian (CMC) hi-res deterministic, if selected
# ------------------------------------------------------

   if [ ${model} -eq 15 ]; then

    if [ ! -f ${PERTDATA}/cmcgribfile.${PDY}${CYL} -o ! -f ${PERTDATA}/cmcgribfileLL.${PDY}${CYL} ]; then

    for fhour in ${fcsthrs}
    do

      if [ ${fhour} -eq 99 ]
      then
        continue
      fi

      if [ ${gribver} -eq 1 ]; then

      total_file_cnt=$(($total_file_cnt+1))
      if [ ! -s ${cmcdir}/${cmcgfile}${fhour} ]
      then
        set +x
        echo " "
        echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        echo " !!! CMC File missing: ${cmcdir}/${cmcgfile}${fhour}"
        echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        echo " "
        echo " !!! Due to missing CMC file, execution is ending...."
        echo " "
        echo " !!! Please re-run the job when CMC file is available ...."
        echo " "
        missing_file_cnt=$(($missing_file_cnt+1))
        set -x
        err_exit " FAILED ${jobid} - ERROR INTERPOLATING CMC DATA IN TRACKER SCRIPT - ABNORMAL EXIT"
        exit 1
      fi

      gfile=${cmcdir}/${cmcgfile}${fhour}
      $wgrib -s $gfile >cmc.ix

         if [ $fhour = '000' ]; then
            vtstring=":anl:"
         else
        if [ ${fhour} -gt 99 ]; then
          vtstring="${fhour}hr"
        fi
        if [ ${fhour} -lt 10 ]; then
          vthour=` echo $fhour | cut -c3-3`
          vtstring="${vthour}hr"
        fi
        if [ ${fhour} -ge 10 -a ${fhour} -lt 99 ]; then
          vthour=` echo $fhour | cut -c2-3`
          vtstring="${vthour}hr"
        fi
       fi
         for parm in ${wgrib_parmlist}
         do
        case ${parm} in
          "SurfaceU")
            grep "UGRD:10 m " cmc.ix | grep ${vtstring} |  $wgrib -s $gfile -i -grib -append -o ${PERTDATA}/cmcgribfile.${PDY}${CYL} ;;
          "SurfaceV")
            grep "VGRD:10 m " cmc.ix | grep ${vtstring} |  $wgrib -s $gfile -i -grib -append -o ${PERTDATA}/cmcgribfile.${PDY}${CYL} ;;
                   *)
            grep "${parm}" cmc.ix | grep ${vtstring} |  $wgrib -s $gfile -i -grib -append -o ${PERTDATA}/cmcgribfile.${PDY}${CYL} ;;
          esac
         done

         if [ ${PHASEFLAG} = 'y' ]; then
           egrep "${wgrib_phase_parmlist}" cmc.ix | grep mb | \
                                     $wgrib -s $gfile -i -grib -append \
                                  -o ${PERTDATA}/cmcgribfile.${PDY}${CYL}
         fi

      else  ##for grib2 below
      if [ -s ${cmcdir}/${cmcgfile}${fhour}_NCEP.grib2 ]; then
          echo ${cmcdir}/${cmcgfile}${fhour}_NCEP.grib2
          # Try to wgrib the primary file....
          $wgrib2 -s ${cmcdir}/${cmcgfile}${fhour}_NCEP.grib2 > cmc.ix
          gfile=${cmcdir}/${cmcgfile}${fhour}_NCEP.grib2 
        else
            set +x
            echo " "
            echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
            echo " !!! ERROR: Both the primary and secondary global     !!!!!!!!!!!!!!"
            echo " !!! grib version 2 files are missing for:            !!!!!!!!!!!!!!"
            echo " !!! cmodel= ${cmodel}                                !!!!!!!!!!!!!!"
            echo " !!! symdh=  ${symdh}                                 !!!!!!!!!!!!!!"
            echo " !!! fhour=  ${fhour}                                 !!!!!!!!!!!!!!"
            echo " !!! Check for the existence of these files:          !!!!!!!!!!!!!!"
            echo " !!!    ${globaldir}/${globalgfile}${fhour} "
            echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
            echo " "
            set -x
            exit
        fi

         if [ $fhour = '000' ]; then
            vtstring=":anl:"
         else
        if [ ${fhour} -gt 99 ]; then
          vtstring="${fhour}hr"
        fi
        if [ ${fhour} -lt 10 ]; then
          vthour=` echo $fhour | cut -c3-3`
          vtstring="${vthour}hr"
        fi
        if [ ${fhour} -ge 10 -a ${fhour} -lt 99 ]; then
          vthour=` echo $fhour | cut -c2-3`
          vtstring="${vthour}hr"
        fi
       fi
         for parm in ${wgrib_parmlist}
         do
        case ${parm} in
          "SurfaceU")
            grep "UGRD:10 m " cmc.ix  |  $wgrib2 -i $gfile -append -grib ${PERTDATA}/cmcgribfile.${PDY}${CYL} ;;
          "SurfaceV")
            grep "VGRD:10 m " cmc.ix  |  $wgrib2 -i $gfile -append -grib ${PERTDATA}/cmcgribfile.${PDY}${CYL} ;;
                   *)
            grep "${parm}" cmc.ix |   $wgrib2 -i $gfile -append -grib ${PERTDATA}/cmcgribfile.${PDY}${CYL} ;;
          esac
         done

         if [ ${PHASEFLAG} = 'y' ]; then
           egrep "${wgrib_phase_parmlist}" cmc.ix | grep mb | \
                                     $wgrib2 -i $gfile -append -grib \
                                  ${PERTDATA}/cmcgribfile.${PDY}${CYL}
         fi
       fi  #gribver ends here

      done

    if [ ${gribver} -eq 1 ]; then
      $gix ${PERTDATA}/cmcgribfile.${PDY}${CYL} ${PERTDATA}/cmcixfile.${PDY}${CYL}
      gribfile=${PERTDATA}/cmcgribfile.${PDY}${CYL}
      ixfile=${PERTDATA}/cmcixfile.${PDY}${CYL}

    else
      ${wgrib2} cmcgribfile.${PDY}${CYL}  \
               -new_grid_vectors none       \
               -new_grid_winds earth        \
               -new_grid latlon "0:360:1.0"  "-90:181:1.0"  \
                cmcgribfileLL.${PDY}${CYL}

      $g2ix ${PERTDATA}/cmcgribfileLL.${PDY}${CYL}  ${PERTDATA}/cmcixfileLL.${PDY}${CYL}
      ixfile=${PERTDATA}/cmcixfileLL.${PDY}${CYL}
      gribfile=${PERTDATA}/cmcgribfileLL.${PDY}${CYL}
    fi

   else  ## files are already available
    if [ ${gribver} -eq 1 ]; then
      gribfile=${PERTDATA}/cmcgribfile.${PDY}${CYL}
      ixfile=${PERTDATA}/cmcixfile.${PDY}${CYL}
      else
      gribfile=${PERTDATA}/cmcgribfileLL.${PDY}${CYL}
      ixfile=${PERTDATA}/cmcixfileLL.${PDY}${CYL}
    fi
   fi   ## files are already available end here

   fi

# ------------------------------------------------------
#   Process Canadian Ensemble perturbation, if selected
# ------------------------------------------------------

if [ ${model} -eq 16 ] ; then

    if [ ! -f ${PERTDATA}/cce${pert}LL.${PDY}${CYL} -o ! -f ${PERTDATA}/cce${pert}.${PDY}${CYL} ]; then

    for fhour in ${fcsthrs}
    do

      if [ ${fhour} -eq 99 ]
      then
        continue
      fi
      total_file_cnt=$(($total_file_cnt+1))

     if [ ${gribver} -eq 1 ]; then
      if [ ! -s ${ccedir}/${ccegfile}${fhour} ]
      then
        set +x
        echo " "
        echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        echo " !!! CANADIAN ENSEMBLE ${PERT} File missing: ${ccedir}/${ccegfile}${fhour}"
        echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        echo " "
        echo " !!! Please re-run the job when CANADIAN ENSEMBLE ${PERT} file is available ..... "
        echo " "
        missing_file_cnt=$(($missing_file_cnt+1))
        set -x
        err_exit " FAILED ${jobid} - MISSING CANADIAN ENSEMBLE ${PERT} FILE IN TRACKER SCRIPT - ABNORMAL EXIT"
      fi

      gfile=${ccedir}/${ccegfile}${fhour}
      $wgrib -s $gfile >ens.ix

      for parm in ${wgrib_parmlist}
      do
        case ${parm} in
          "SurfaceU")
            grep "UGRD:10 m " ens.ix | $wgrib -s $gfile -i -grib -append  -o ${PERTDATA}/cce${pert}.${PDY}${CYL} ;;
          "SurfaceV")
            grep "VGRD:10 m " ens.ix | $wgrib -s $gfile -i -grib -append  -o ${PERTDATA}/cce${pert}.${PDY}${CYL} ;;
                   *)
            grep "${parm}" ens.ix | $wgrib -s $gfile -i -grib -append  -o ${PERTDATA}/cce${pert}.${PDY}${CYL} ;;
        esac

      done

         if [ ${PHASEFLAG} = 'y' ]; then
           egrep "${wgrib_phase_parmlist}" ens.ix | grep mb | \
                                     $wgrib -s $gfile -i -grib -append \
                                  -o ${PERTDATA}/cce${pert}.${PDY}${CYL}
         fi

      else  ##for grib2 below
      if [ -s ${ccedir}/${ccegfile}${fhour} ]; then
         echo ${ccedir}/${ccegfile}${fhour}
          # Try to wgrib the primary file....
          $wgrib2 -s ${ccedir}/${ccegfile}${fhour} > ens.ix
          gfile=${ccedir}/${ccegfile}${fhour}
      else
        set +x
        echo " "
        echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        echo " !!! CANADIAN ENSEMBLE ${PERT} File missing: ${ccedir}/${ccegfile}${fhour}"
        echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        echo " "
        echo " !!! Please re-run the job when CANADIAN ENSEMBLE ${PERT} file is available ..... "
        echo " "
        missing_file_cnt=$(($missing_file_cnt+1))
        set -x
        err_exit " FAILED ${jobid} - MISSING CANADIAN ENSEMBLE ${PERT} FILE IN TRACKER SCRIPT - ABNORMAL EXIT"
      fi

         for parm in ${wgrib_parmlist}
         do
        case ${parm} in
          "SurfaceU")
            grep "UGRD:10 m " ens.ix  |  $wgrib2 -i $gfile -append -grib ${PERTDATA}/cce${pert}.${PDY}${CYL} ;;
          "SurfaceV")
            grep "VGRD:10 m " ens.ix  |  $wgrib2 -i $gfile -append -grib ${PERTDATA}/cce${pert}.${PDY}${CYL} ;;
                   *)
            grep "${parm}" ens.ix |   $wgrib2 -i $gfile -append -grib ${PERTDATA}/cce${pert}.${PDY}${CYL} ;;
          esac
         done

         if [ ${PHASEFLAG} = 'y' ]; then
        for parm in ${wgrib_phase_parmlist}
           do
            grep "${parm}" ens.ix | $wgrib2 -i $gfile -append -grib ${PERTDATA}/cce${pert}.${PDY}${CYL}
           done
         fi

       fi  #gribver ends here

    done

    if [ ${gribver} -eq 1 ]; then
      $gix ${PERTDATA}/cce${pert}.${PDY}${CYL} ${PERTDATA}/cce${pert}ixfile.${PDY}${CYL}
      gribfile=${PERTDATA}/cce${pert}.${PDY}${CYL}
      ixfile=${PERTDATA}/cce${pert}ixfile.${PDY}${CYL}

    else
      ${wgrib2} cce${pert}.${PDY}${CYL}  \
               -new_grid_vectors none       \
               -new_grid_winds earth        \
               -new_grid latlon "0:360:1.0"  "-90:181:1.0"  \
                cce${pert}LL.${PDY}${CYL}

      $g2ix ${PERTDATA}/cce${pert}LL.${PDY}${CYL}  ${PERTDATA}/cce${pert}ixfileLL.${PDY}${CYL}
      ixfile=${PERTDATA}/cce${pert}ixfileLL.${PDY}${CYL}
      gribfile=${PERTDATA}/cce${pert}LL.${PDY}${CYL}
    fi

   else  ## files are already available
    if [ ${gribver} -eq 1 ]; then
      gribfile=${PERTDATA}/cce${pert}.${PDY}${CYL}
      ixfile=${PERTDATA}/cce${pert}ixfile.${PDY}${CYL}
      else
      gribfile=${PERTDATA}/cce${pert}LL.${PDY}${CYL}
      ixfile=${PERTDATA}/cce${pert}ixfileLL.${PDY}${CYL}
    fi
   fi   ## files are already available end here
fi

# ----------------------------------------------
#------------------------------------------------------------------------#
#                         Now run the tracker                            #
#------------------------------------------------------------------------#

   ist=1
   while [ $ist -le 15 ]
   do
      if [ ${stormflag[${ist}]} -ne 1 ]; then
         set +x; echo "Storm number $ist NOT selected for processing"; set -x
      else
         set +x; echo "Storm number $ist IS selected for processing...."; set -x
      fi
      let ist=ist+1
   done

# Load the forecast hours for this particular model into an array 
# that will be passed into the executable via a namelist....

   last_fcst_hour=0
   ifh=1
   while [ $ifh -le ${maxtime} ]
   do
      fh[${ifh}]=` echo ${fcsthrs} | awk '{print $n}' n=$ifh`
      fhr=`        echo ${fcsthrs} | awk '{print $n}' n=$ifh`
      if [ ${fhr} -ne 99 ]
      then
        last_fcst_hour=${fhr}
      fi
      let ifh=ifh+1
   done

#if [ ! -s ${PERTDATA}/last_fcst_hour.${atcfout}.${PDY}${CYL} ]
#then
  echo ${last_fcst_hour} >${PERTDATA}/last_fcst_hour.${atcfout}.${PDY}${CYL}
#fi

   namelist=${PERTDATA}/input.${atcfout}.${PDY}${CYL}
   ATCFNAME=` echo "${atcfname}" | tr '[a-z]' '[A-Z]'`

#   contour_interval=200.0
contour_interval=100.0
write_vit=y

  echo "&datein inp%bcc=${scc},inp%byy=${syy},inp%bmm=${smm},"      >${namelist}
  echo "        inp%bdd=${sdd},inp%bhh=${shh},inp%model=${model}/" >>${namelist}
  echo "&fhlist itmphrs = ${fh[1]},${fh[2]},${fh[3]},"             >>${namelist}
  echo "      ${fh[4]},${fh[5]},${fh[6]},${fh[7]},"                >>${namelist}
  echo "      ${fh[8]},${fh[9]},${fh[10]},${fh[11]},"              >>${namelist}
  echo "      ${fh[12]},${fh[13]},${fh[14]},"                      >>${namelist}
  echo "      ${fh[15]},${fh[16]},${fh[17]},"                      >>${namelist}
  echo "      ${fh[18]},${fh[19]},${fh[20]},"                      >>${namelist}
  echo "      ${fh[21]},${fh[22]},${fh[23]},"                      >>${namelist}
  echo "      ${fh[24]},${fh[25]},${fh[26]},"                      >>${namelist}
  echo "      ${fh[27]},${fh[28]},${fh[29]},"                      >>${namelist}
  echo "      ${fh[30]},${fh[31]},${fh[32]},"                      >>${namelist}
  echo "      ${fh[33]},${fh[34]},${fh[35]},"                      >>${namelist}
  echo "      ${fh[36]},${fh[37]},${fh[38]},"                      >>${namelist}
  echo "      ${fh[39]},${fh[40]},${fh[41]},"                      >>${namelist}
  echo "      ${fh[42]},${fh[43]},${fh[44]},"                      >>${namelist}
  echo "      ${fh[45]},${fh[46]},${fh[47]},"                      >>${namelist}
  echo "      ${fh[48]},${fh[49]},${fh[50]},"                      >>${namelist}
  echo "      ${fh[51]},${fh[52]},${fh[53]},"                      >>${namelist}
  echo "      ${fh[54]},${fh[55]},${fh[56]},"                      >>${namelist}
  echo "      ${fh[57]},${fh[58]},${fh[59]},"                      >>${namelist}
  echo "      ${fh[60]},${fh[61]},${fh[62]},"                      >>${namelist}
  echo "      ${fh[63]},${fh[64]},${fh[65]}/"                      >>${namelist}
  echo "&atcfinfo atcfnum=${atcfnum},atcfname='${ATCFNAME}'/"      >>${namelist}
  echo "&trackerinfo trkrinfo%westbd=${trkrwbd},"                  >>${namelist}
  echo "      trkrinfo%eastbd=${trkrebd},"                         >>${namelist}
  echo "      trkrinfo%northbd=${trkrnbd},"                        >>${namelist}
  echo "      trkrinfo%southbd=${trkrsbd},"                        >>${namelist}
  echo "      trkrinfo%type='${trkrtype}',"                        >>${namelist}
  echo "      trkrinfo%mslpthresh=${mslpthresh},"                  >>${namelist}
  echo "      trkrinfo%v850thresh=${v850thresh},"                  >>${namelist}
  echo "      trkrinfo%gridtype='${modtyp}',"                      >>${namelist}
  echo "      trkrinfo%contint=${contour_interval},"               >>${namelist}
  echo "      trkrinfo%out_vit='${write_vit}',"                    >>${namelist}
  echo "      trkrinfo%gribver=${gribver},"                        >>${namelist}
  echo "      trkrinfo%g2_jpdtn=${g2_jpdtn}/"                      >>${namelist}
  echo "&phaseinfo phaseflag='${PHASEFLAG}',"                      >>${namelist}
  echo "           phasescheme='${PHASE_SCHEME}'/"                 >>${namelist}

  # Execute the program

  export pgm=gettrk_gen
  . prep_step

  ln -s -f ${gribfile}                                                  fort.11
  ln -s -f ${PERTDATA}/vitals.upd.${atcfout}.${regtype}.${PDY}${shh}    fort.12
  ln -s -f ${PERTDATA}/genvitals.upd.${atcfout}.${regtype}.${PDY}${shh} fort.13
  ln -s -f ${ixfile}                                                    fort.31
  ln -s -f ${PERTDATA}/trak.${atcfout}.radii.${regtype}.${PDY}${CYL}     fort.63
  ln -s -f ${PERTDATA}/trak.${atcfout}.atcfunix.${regtype}.${PDY}${CYL}  fort.64
  ln -s -f ${PERTDATA}/trak.${atcfout}.atcf_gen.${regtype}.${PDY}${CYL}     fort.66

  if [ ${atcfname} = 'aear' ]; then
     ln -s -f ${PERTDATA}/trak.${atcfout}.initvitl.${PDY}${CYL}          fort.65
  fi

  if [ ${trkrtype} = 'midlat' -o ${trkrtype} = 'tcgen' ]; then
     ln -s -f ${PERTDATA}/trkrmask.${atcfout}.${regtype}.${PDY}${CYL}    fort.72
  fi

  if [ ${write_vit} = 'y' ]
  then
    ln -s -f ${PERTDATA}/output_genvitals.${atcfout}.${regtype}.${PDY}${shh}        fort.67
  fi

  set +x
  echo " "
  echo " -----------------------------------------------"
  echo "           NOW EXECUTING TRACKER......"
  echo " -----------------------------------------------"
  echo " "
  set -x
  
  msg="$pgm start for $atcfout at ${CYL}z"
#  postmsg "$jlogfile" "$msg"
  
  #LB-----------------------------------------------------------------------
  #    The bullet proofing for nam and sref has been removed because the
  #    hooks in the postprocessors for each model's tracks will take the
  #    regions into account.
  #LB-----------------------------------------------------------------------
  
  set +x
  echo "+++ TIMING: BEFORE gettrk  ---> `date`"
  set -x
  if [ ${cmodel} = 'gefs' -o ${cmodel} = 'sref' ]; then
     $exectrkdir/gettrk_gen <${namelist} 2>&1 >${PERTDATA}/trkr.${regtype}.${cmodel}.${pert}.out
  else
     $exectrkdir/gettrk_gen <${namelist} 2>&1 >${PERTDATA}/trkr.${regtype}.${cmodel}.out
  fi
  ##export err=$?; err_chk
  gettrk_rcc=$?
  
  set +x
  echo "+++ TIMING: AFTER  gettrk  ---> `date`"
  set -x
  
  #--------------------------------------------------------------#
  # Now copy the output track files to different directories
  #--------------------------------------------------------------#
  
  set +x
  echo " "
  echo " -----------------------------------------------"
  echo "    NOW COPYING OUTPUT TRACK FILES TO COM  "
  echo " -----------------------------------------------"
  echo " "
  set -x
  
  if [ ${gettrk_rcc} -eq 0 ]; then
  
     if [ ${PARAFLAG} = 'YES' ]; then
        cp ${PERTDATA}/trak.${atcfout}.atcfunix.${regtype}.${PDY}${CYL} ../.
      else
        cp ${PERTDATA}/trak.${atcfout}.atcfunix.${regtype}.${PDY}${CYL} $COMOUT/.
     fi
        if [ ${write_vit} = 'y' ]
        then
           cat ${PERTDATA}/output_genvitals.${atcfout}.${regtype}.${PDY}${shh} \
            >>${savedir}/genesis.vitals.${atcfout}.${regtype}.${CENT}${syy}
        fi

     msg="$pgm end for $atcfout at ${CYL}z completed normally"
     postmsg "$jlogfile" "$msg"
  
  # Copy atcf files to NHC archives. We'll use Steve Lord's original script,
  # distatcf.sh, to do this, and that script requires the input atcf file to
  # have the name "attk126", so first copy the file to that name, then call
  # the distatcf.sh script.  After that's done, then copy the full 0-126-h
  # track into the /com/hur/prod/global track archive file.

     if [ ${SENDCOM} = 'YES' ]; then
  
        if [ ${PARAFLAG} = 'YES' ]; then
         echo " "
        else
              cp ${PERTDATA}/trak.${atcfout}.atcfunix.${regtype}.${PDY}${CYL} ${COMOUT}/${atcfout}.t${CYL}z.cyclone.trackatcfunix.${regtype}
              cp ${PERTDATA}/trak.${atcfout}.atcf_gen.${regtype}.${PDY}${CYL}  ${COMOUT}
              cp ${PERTDATA}/trak.${atcfout}.radii.${regtype}.${PDY}${CYL} ${COMOUT}/${atcfout}.t${CYL}z.cyclone.radii.${regtype}
              cp ${PERTDATA}/trak.${atcfout}.atcf_gen.${regtype}.${PDY}${CYL}  ${COMOMB}
              cp ${PERTDATA}/trak.${atcfout}.atcfunix.${regtype}.${PDY}${CYL} ${COMOMB}/${atcfout}.t${CYL}z.cyclone.trackatcfunix.${regtype}
              cp ${PERTDATA}/trak.${atcfout}.radii.${regtype}.${PDY}${CYL} ${COMOMB}/${atcfout}.t${CYL}z.cyclone.radii.${regtype}

##G.P. Lou
## Moved this part to "genesis_tracker_poe.sh" due to multiple CPU usage.
##              if [ $regtype = "glbl" ] ; then
##                cat ${COMOMB}/${atcfout}.t${CYL}z.cyclone.trackatcfunix.${regtype} >> ${COMOMB}/cyclone_${PDY}${CYL}.ens
##                 if [ $SENDDBN = "YES" ] ; then
##	            $DBNROOT/bin/dbn_alert MODEL OMBATCF $job ${COMOMB}/cyclone_${PDY}${CYL}.ens
##	         fi
##	      fi

           tmscrdir=${tmscrdir:-${COMROOTp1}/arch/${envir}} 
           tmtrakstat=${tmscrdir}/tracker.prod.status
           echo "${atcfout} trkr completed okay for ${PDY}${CYL}" >>${tmtrakstat}
  
  #   We need to parse apart the atcfunix file and distribute the forecasts to 
  # the necessary directories.  To do this, first sort the atcfunix records 
  # by forecast hour (k6), then sort again by ocean basin (k1), storm number (k2)
  # and then quadrant radii wind threshold (k12).  Once you've got that organized 
  # file, break the file up by putting all the forecast records for each storm 
  # into a separate file.  Then, for each file, find the corresponding atcfunix 
  # file in the /tpcprd directory and dump the atcfunix records for that storm 
  # in there.  NOTE: Only do this if the model run is NOT for the ensemble.
  # the reason is that we do NOT want to write out the individual member tracks
  #   to the atcfunix file.  We only want to write out the ensemble mean track
  # to the atcfunix file, and the mean track is calculated in a separate script.
  
         if [ $cmodel != 'gefs' ]; then

            if [ ${cmodel} = 'gfdl' ]; then
               auxfile=${COMOUT}/${stormenv}.${PDY}${CYL}.trackeratcfunix
            else
               auxfile=${PERTDATA}/trak.${atcfout}.atcfunix.${regtype}.${PDY}${CYL}
            fi

            sort -k6 ${auxfile} | sort -k1 -k2 -k12  >atcfunix.sorted

            old_string="XX, XX"

            ict=0
            while read unixrec
            do
               storm_string=` echo "${unixrec}" | cut -c1-6`
               if [ "${storm_string}" = "${old_string}" ]; then
                  echo "${unixrec}" >>atcfunix_file.${ict}
               else
                  let ict=ict+1
                  echo "${unixrec}"  >atcfunix_file.${ict}
                  old_string="${storm_string}"
               fi
            done <atcfunix.sorted

         fi

      fi

   fi

  else

     if [ ${PARAFLAG} = 'YES' ]; then
        echo " "
     else
        tmtrakstat=/com/arch/${envir}/tracker.prod.status
        echo "ERROR: ${atcfout} tracker FAILED for ${PDY}${CYL}" >>${tmtrakstat}
     fi

     set +x
     echo " "
     echo "!!! ERROR -- An error occurred while running genesis_gettrk, "
     echo "!!! which is the program that actually gets the track."
     echo "!!! Return code from genesis_gettrk = ${gettrk_rcc}"
     echo "!!! model= ${atcfout}, forecast initial time = ${PDY}${CYL}"
     echo "!!! Exiting...."
     echo " "
     set -x
     err_exit " FAILED ${jobid} - ERROR RUNNING GETTRK IN TRACKER SCRIPT- ABNORMAL EXIT"
  
  fi

  #   Run the script that filters out storms which don't live
  #   for at least 24h....
# Remove this stript to save all centers for verification purposes --G.P.Lou

##  ${ushtrkdir}/sort_tracks_ush.sh ${PDY}${CYL} ${atcfout} ${regtype} >${PERTDATA}/sort.${regtype}.${atcfout}.${PDY}${CYL}.out
   trakfile=trak.${atcfout}.atcfunix.${regtype}.${PDY}${CYL}
   >storms.${atcfout}.atcfunix.${regtype}.${PDY}${CYL}
   cat ${trakfile} | grep ", NEQ" | grep FOF | egrep "(ML, |TG, )" | sort -n >${trakfile}.sorted
   awk 'match($0,"0 ,    0 ,   0,    0, XX") == 0 {print $0}' ${trakfile}.sorted \
                  >>storms.${atcfout}.atcfunix.${regtype}.${PDY}${CYL}
   grep "TCV" ${trakfile} | sort -n >>storms.${atcfout}.atcfunix.${regtype}.${PDY}${CYL}

  cat ${PERTDATA}/storms.${atcfout}.atcfunix.${regtype}.${PDY}${CYL} >> ${savedir}/storms.${atcfout}.atcfunix.${regtype}.${syyyy}
  cat ${PERTDATA}/trak.${atcfout}.atcf_gen.${regtype}.${PDY}${CYL}  >>  ${savedir}/trak.${atcfout}.atcf_gen.${regtype}.${syyyy}

   if [ $missing_file_cnt -gt 0 ]; then
      echo "missing data" ${atcfout} ${PDY}${CYL} | mail -s "tracker status" guang.ping.lou@noaa.gov
      echo " WARNING: Missing data $missing_file_cnt files out of $total_file_cnt files total !!!!" 
#   else
#      
#      echo "No missing file out of $total_file_cnt files total !!!" 
       
   fi
   
  #  ----END GENESIS_GETTRK.SH.SH----------
