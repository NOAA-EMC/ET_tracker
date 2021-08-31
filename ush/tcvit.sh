#!/bin/sh

# Script written by Tim Marchok

if [ $# -lt 2 ]; then
  echo " "
  echo "  Script `basename $0` will get all the TC Vitals records for a "
  echo "  particular storm.  At the command line, you need to supply the"
  echo "  3-character storm identifier (eg, 02L, 05E, etc) and the year (yyyy)."
  echo " "
  echo "  Usage: `basename $0` stormid yyyy"
  echo " "
  exit 8
fi

stid=$1
yyyy=$2

basinchar=` echo $stid | cut -c3-3 | tr '[A-Z]' '[a-z]'`

if [ ${yyyy} -ge 2003 ]; then
  vitdir=${archsyndir}
  if [ $basinchar = 'l' -o $basinchar = 'e' -o $basinchar = 'c' ]; then
    grep -i " $stid" ${vitdir}/syndat_tcvitals.${yyyy}      | \
            awk '{ if (length($0) > 100) {print $0} }'      | \
            sort -k4,5 -u
  else
    grep -i " $stid" ${vitdir}/syndat_tcvitals.${yyyy}      | \
            sort -k4,5 -u
  fi
else
  vitdir=/nfsuser/g01/wx20tm/syndat
  if [ $basinchar = 'l' -o $basinchar = 'e' -o $basinchar = 'c' ]; then
    grep -i " $stid" ${vitdir}/syndat_tcvitals.${yyyy}      | \
            awk '{ if (length($0) > 100) {print $0} }'      | \
            sort -k4,5 -u
  else
    grep -i " $stid" ${vitdir}/syndat_tcvitals.${yyyy}      | \
            sort -k4,5 -u
#            sort +3 -n +4 -n -u
  fi
fi

