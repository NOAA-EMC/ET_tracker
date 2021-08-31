      program sort_and_update_vitals
c
c$$$  MAIN PROGRAM DOCUMENTATION BLOCK
c
c Main Program: SUPVIT_GEN   Sort and Update Genesis Vitals File
C   PRGMMR: MARCHOK          ORG: NP22        DATE: 2006-07-06
c
c ABSTRACT: This program searches through the genesis vitals file and 
c   reads the records for a particular dtg.  It contains logic to 
c   eliminate duplicate records and only keep the most recent one (see
c   further documentation below).  It also searches to see if a storm 
c   was included in the Vitals file 6 hours earlier but is missing from
c   the current Vitals records.  In this case, the program assumes
c   that the regional forecasting center was late in reporting the
c   current position, and it includes the 6h-old Vitals record with
c   the current Vitals records.  This program will also take the
c   position and heading from that 6h-old record and extrapolate the
c   information to get a current first guess estimate of the storm's
c   position.  By the way, if a storm was found 6 hours earlier, logic
c   is also included to eliminate any duplicate records of that storm
c   in those 6h-old records.  Finally, if it turns out that the reason
c   a 6h-old storm is no longer on the current records is that the
c   storm has dissipated, don't worry about including it to be passed
c   into the tracking program; the tracking program will not be able
c   to track it and that'll be the end of it.
c
c Program history log:
c   2006-07-06  Marchok - Original version, modified from the old 
c                         TC tracker version of supvit_main.f
c   2007-04-25  Marchok - Modified to allow for wrapping around the
c                         Greenwich Meridian
c
c Input files:
c   unit   31    Text file containing all vitals (including duplicates)
c                for current time and time from 6 hours ago.
c Output files:
c   unit   51    Text file containing sorted, updated vitals (without
c                any duplicates) valid at the current time only.
c
c Subprograms called:
c   read_nlists  Read input namelists for input dates
c   read_vitals_file Read TC vitals file to get initial storm positions
c   delete_dups  Delete duplicate TC vitals records from current time
c   delete_6ago  Delete records from 6h ago if current record exists
c   delete_6ago_dups  Delete duplicate records from 6h ago time
c   update_6ago_vits  Update position of storms from 6h ago positions
c   output       Output 1 record for each updated vitals record
c
c Attributes:
c   Language:  Fortran_90, no platform-specific extensions.
c
c$$$
c
c-------
c 
c
      USE gen_vitals; USE set_max_parms; USE inparms; USE date_checks
      USE trig_vals
c
      type (gencard) storm(maxstorm)
      type (datecard) dnow, d6ago, d6ahead

      logical(1)  okstorm(maxstorm)
c
      call w3tagb('SUPVIT_GEN',2006,0811,0058,'NP22   ')
c
      okstorm = .FALSE.
c
      pi = 4. * atan(1.)   ! pi, dtr and rtd were declared in module
      dtr = pi/180.0       ! trig_vals, but were not yet defined.
      rtd = 180.0/pi 
c
c     -----------------------------------------
c     Read namelists to get date information
c
      call read_nlists (dnow,d6ago,d6ahead)
c
c     -----------------------------------------------------------
c     Read in storm cards for current time and delete duplicates
c

      inowct = 0
      call read_vitals_file (storm,ymd_now,hhmm_now,inowct,okstorm)
 
      if (inowct > 0) then
        call delete_dups (storm,inowct,okstorm)
      else
        print *,' '
        print *,'!!! No storms on vitals card for current time.'
        print *,'!!! A check will be made for 6h-old vitals cards,'
        print *,'!!! and if any exist, the positions will be  updated'
        print *,'!!! (extrapolated) to get a first guess position for'
        print *,'!!! the current time.'
        print *,'!!! Current forecast time = ',ymd_now,hhmm_now
        print *,'!!! 6h-ago forecast time = ',ymd_6ago,hhmm_6ago
      endif
c
c     -----------------------------------------------------------
c     Read in storm cards for 6h ago and delete duplicates
c
      rewind (31)
      itempct = inowct
      call read_vitals_file (storm,ymd_6ago,hhmm_6ago,itempct,okstorm)
      i6agoct = itempct - inowct
 
      if (i6agoct > 0) then
        if (inowct > 0) then
          call delete_6ago (storm,inowct,i6agoct,okstorm)
        endif
        call delete_6ago_dups (storm,inowct,i6agoct,okstorm)
      endif
 
c     ----------------------------------------------------------------
c     Now update any vitals records left from 6h ago by extrapolating
c     their positions 6h ahead to the current time.
 
      if (i6agoct > 0) then
        call update_6ago_vits (storm,inowct,i6agoct,okstorm)
      endif


c     --------------------------------------------------------------
c     Read in storm cards for 6h ahead and delete duplicates.  This
c     is used for Qingfu's vortex relocation purposes.  If he is 
c     doing the analysis/relocation for, say, 12z, he looks at the
c     first guess files from the 06z cycle and tracks from there.
c     But suppose there is a storm whose first vitals card is 
c     issued at 12z; then we would have no vitals card at 06z for
c     the tracker to use.  So this next part reads the vitals from
c     the cycle 6h ahead and, if it finds any vitals that were not
c     included with the current time's vitals, then it extrapolates
c     those vitals from the next cycle *backwards* to the current
c     time.  By the way, itempct is input/output for the  read 
c     routine.  Going in, it contains the count of the number of 
c     records read in so far.  In that read routine, itempct is 
c     incremented for every valid record read for the input time.

      rewind (31)
      iprevct = inowct + i6agoct
      call read_vitals_file (storm,ymd_6ahead,hhmm_6ahead,itempct
     &                      ,okstorm)
      i6aheadct = itempct - iprevct

      print *,'before d6a if, i6aheadct = ',i6aheadct,' iprevct= '
     &       ,iprevct
      print *,'before d6a if, inowct = ',inowct,' i6agoct= ',i6agoct

      if (i6aheadct > 0) then
        if (iprevct > 0) then
          call delete_6ahead (storm,iprevct,i6aheadct,okstorm)
        endif
        call delete_6ahead_dups (storm,iprevct,i6aheadct,okstorm)
      endif

c     ----------------------------------------------------------------
c     Now update any vitals records not filtered out from 6h-ahead by
c     extrapolating their positions 6h-ahead *backwards* to the 
c     current time.
 
      if (i6aheadct > 0) then
        call update_6ahead_vits (storm,iprevct,i6aheadct,okstorm)
      endif

 
c     ---------------------------------------------------------
c     Now output all of the sorted, updated TC Vitals records
 
      itotalct = inowct + i6agoct + i6aheadct
      call output (storm,itotalct,okstorm)
c
      call w3tage('SUPVIT_GEN')
      stop
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine read_vitals_file (storm,ymd,hhmm,ict,okstorm)
c
c     ABSTRACT: This routine  reads in the TC Vitals file, and stores
c     into an array those records that match the input ymd and hhmm.
c
c     INPUT:
c
c     ict     Tells at what index in the storm array to begin reading
c             the input records into.  This is important because this
c             subroutine is called twice; the first time the data are
c             for the current time and are just started at ict = 0,
c             but the second time it's called we're getting the 6h ago
c             data, and they have to be added onto the end of the
c             array, so we need to know where the current time's data
c             ends so we know what index to start the 6h ago data.
c
      USE gen_vitals; USE set_max_parms
c
      type (gencard) storm(maxstorm), ts
c
      integer  ymd,hhmm
      logical(1)  okstorm(maxstorm)
c
      lucard = 31

      print *,' '
      print '(a29,i8.8,a8,i4.4)',' IN READ_VITALS_FILE: , ymd= ',ymd
     &      ,'  hhmm= ',hhmm
      print *,' '


      do while (.true.)
        read (lucard,21,END=801,ERR=891) ts
        write (6,23) ' ooo  READ:      ',444,ts
        if (ts%gv_obs_ymd == ymd .and. ts%gv_obs_hhmm == hhmm) then
          ict = ict + 1
          storm(ict) = ts
          okstorm(ict) = .TRUE.
          write (6,23) ' !!! MATCH, ict= ',ict,storm(ict)
        else
          print *,'!!! NO: ts%gv_obs_ymd= ',ts%gv_obs_ymd,' ymd= ',ymd
     &           ,' ts%gv_obs_hhmm= ',ts%gv_obs_hhmm,' hhmm= ',hhmm
        endif
      enddo
  801 continue

   21 format (a30,1x,i8,1x,i4,1x,i3,a1,1x,i4,a1,1x,i3,1x,i3,a85)
   23 format (a18,i4,2x,a30,1x,i8.8,1x,i4.4,1x,i3,a1,1x,i4
     &       ,a1,1x,i3,1x,i3,a85)

      iret = 0
      return

  891 print *,'!!! ERROR in program sort_and_update_vitals.  Error '
      print *,'!!! occurred in read_vitals_file while reading unit '
     &       ,lucard
      iret = 98

      return
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine output (storm,itotalct,okstorm)
c
      USE gen_vitals; USE set_max_parms; USE inparms
c
      type (gencard) storm(maxstorm)
      type (datecard) dnow, d6ago, d6ahead

      logical(1)  okstorm(maxstorm)
c
      lunvit = 51

      ist = 1
      do while (ist <= itotalct)

        if (okstorm(ist)) then
          if (storm(ist)%gv_stdir == -99 .or.
     &        storm(ist)%gv_stspd == -99) then
            write (lunvit,23,ERR=891) storm(ist)
          else
            write (lunvit,21,ERR=891) storm(ist)
          endif
        endif

        ist = ist + 1

      enddo

   21 format (a30,1x,i8.8,1x,i4.4,1x,i3.3,a1,1x,i4.4
     &       ,a1,1x,i3.3,1x,i3.3,a85)
   23 format (a30,1x,i8.8,1x,i4.4,1x,i3.3,a1,1x,i4.4
     &       ,a1,1x,i3,1x,i3,a85)

      iret = 0
      return

  891 print *,'!!! ERROR in program sort_and_update_vitals.  Error '
      print *,'!!! occurred in output while writing new vitals file '
      print *,'!!! to unit number',lunvit
      iret = 98

      return
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine update_6ago_vits (storm,inowct,i6agoct,okstorm)
c
c     ABSTRACT: This subroutine  updates the vitals from 6 hours ago.
c     It uses the heading and direction values listed in the vitals
c     record (see Module gen_vitals for specfics on where to find 
c     heading & direction in the vitals record) to get a new
c     position for the current time by extrapolating out 6 hours.
c
      USE gen_vitals; USE set_max_parms; USE inparms; USE date_checks
      USE trig_vals
c
      type (gencard) storm(maxstorm)
      type (datecard) dnow, d6ago

      logical(1)  okstorm(maxstorm)
c
      ist  = inowct + 1
      iend = inowct + i6agoct
      do while (ist <= iend)

        if (okstorm(ist) .and. storm(ist)%gv_obs_ymd == ymd_6ago .and.
     &                         storm(ist)%gv_obs_hhmm == hhmm_6ago) then

          rlat = float(storm(ist)%gv_obs_lat) / 10.
          rlon = float(storm(ist)%gv_obs_lon) / 10.
          rhdg = float(storm(ist)%gv_stdir)
          rspd = float(storm(ist)%gv_stspd) / 10.
 
c         ------------------------------------------
c         This first part updates the positions by simply
c         extrapolating the current motion along the current
c         heading at the current speed for 6 hours.  Be
c         careful with adding and subtracting these distances
c         in the different hemispheres (see the if statements).
c         Remember: In the storm message file, there are NO
c         negative signs to distinguish between hemispheres,
c         so a southern hemisphere latitude will be POSITIVE,
c         but will be distinguished by the 'S'.
          
          strmucomp = rspd * sin(dtr*rhdg)
          strmvcomp = rspd * cos(dtr*rhdg)
c
          vdistdeg = (strmvcomp * secphr * 6) / dtk
          if (storm(ist)%gv_obs_latns == 'N') then
            rnewlat = rlat + vdistdeg
          else
            rnewlat = rlat - vdistdeg
          endif
c
          avglat = 0.5 * (rlat + rnewlat)
          cosfac = cos(dtr * avglat)
          udistdeg = (strmucomp * secphr * 6) / (dtk * cosfac)
          if (storm(ist)%gv_obs_lonew == 'W') then
            rnewlon = rlon - udistdeg
          else
            rnewlon = rlon + udistdeg
          endif
 
c         ------------------------------------------
c         This part updates the E/W and N/S characters
c         in the event that a storm changes hemisphere.
c         If a storm does change hemisphere, say from 
c         W to E at 180, we need to also adjust the new 
c         longitude value from say 186W to 174E.  Have to 
c         include this code since storm messages contain 
c         longitudes on a 0-180 basis (E&W), NOT 0-360.
 
          if (storm(ist)%gv_obs_latns == 'N') then
            if (rnewlat < 0.) then
              storm(ist)%gv_obs_latns = 'S'
              rnewlat   = -1. * rnewlat
            endif
          else
            if (rnewlat < 0.) then
              storm(ist)%gv_obs_latns = 'N'
              rnewlat   = -1. * rnewlat
            endif
          endif
c
          if (storm(ist)%gv_obs_lonew == 'W') then
            if (rnewlon > 180.) then
              ! The storm moved westward across 180....
              storm(ist)%gv_obs_lonew = 'E'
              rnewlon = 180. -  abs(rnewlon - 180.)
            elseif (rnewlon <= 0.) then
              ! A storm in the Western Hemisphere moved eastward 
              ! across the Greenwich Meridian....
              storm(ist)%gv_obs_lonew = 'E'
              rnewlon = -1. * rnewlon
            endif
          else
            if (rnewlon > 180.) then
              ! A storm in the Eastern Hemisphere moved eastward 
              ! across 180 into the Western Hemisphere....
              storm(ist)%gv_obs_lonew = 'W'
              rnewlon = 180. - abs(rnewlon - 180.)
            elseif (rnewlon < 0.) then
              ! A storm in the Eastern Hemisphere moved westward 
              ! across the Greenwich Meridian....
              storm(ist)%gv_obs_lonew = 'W'
              rnewlon = -1. * rnewlon
            endif
          endif

          storm(ist)%gv_obs_lat = int ((rnewlat + 0.05) * 10.)
          storm(ist)%gv_obs_lon = int ((rnewlon + 0.05) * 10.)
          storm(ist)%gv_obs_ymd  = ymd_now
          storm(ist)%gv_obs_hhmm = hhmm_now

        endif

        ist = ist + 1
   
      enddo
c
      return
      end

c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine update_6ahead_vits (storm,iprevct,i6aheadct,okstorm)
c
c     ABSTRACT: This subroutine  updates the vitals from 6 hours ahead.
c     It uses the heading and direction values listed in the vitals
c     record (see Module gen_vitals for specfics on where to find 
c     heading & direction in the vitals record) to get a new
c     position for the current time by extrapolating *BACKWARDS*
c     6 hours to the current time.
c
      USE gen_vitals; USE set_max_parms; USE inparms; USE date_checks
      USE trig_vals
c
      type (gencard) storm(maxstorm)
      type (datecard) dnow, d6ago, d6ahead

      logical(1)  okstorm(maxstorm)
c
      ist  = iprevct + 1
      iend = iprevct + i6aheadct
      do while (ist <= iend)

        if (okstorm(ist) .and. storm(ist)%gv_obs_ymd == ymd_6ahead .and.
     &                   storm(ist)%gv_obs_hhmm == hhmm_6ahead) then

          rlat = float(storm(ist)%gv_obs_lat) / 10.
          rlon = float(storm(ist)%gv_obs_lon) / 10.
          rhdg = float(storm(ist)%gv_stdir)
          rspd = float(storm(ist)%gv_stspd) / 10.

c         IMPORTANT NOTE: Since we are extrapolating *BACKWARDS* in 
c         time in this routine, we have to take that value of the 
c         storm heading in rhdg and switch it by 180 degrees so that
c         we will be pointing back in the direction the storm came
c         from....

          if (rhdg >= 0. .and. rhdg <= 180.) then
            rhdg = rhdg + 180.
          else 
            rhdg = rhdg - 180.
          endif
 
c         ------------------------------------------
c         This first part updates the positions by simply
c         extrapolating the current motion along the REVERSE of
c         the current heading at the current speed for 6 hours. 
c         Be careful with adding and subtracting these distances
c         in the different hemispheres (see the if statements).
c         Remember: In the storm message file, there are NO
c         negative signs to distinguish between hemispheres,
c         so a southern hemisphere latitude will be POSITIVE,
c         but will be distinguished by the 'S'.
          
          strmucomp = rspd * sin(dtr*rhdg)
          strmvcomp = rspd * cos(dtr*rhdg)
c
          vdistdeg = (strmvcomp * secphr * 6) / dtk
          if (storm(ist)%gv_obs_latns == 'N') then
            rnewlat = rlat + vdistdeg
          else
            rnewlat = rlat - vdistdeg
          endif
c
          avglat = 0.5 * (rlat + rnewlat)
          cosfac = cos(dtr * avglat)
          udistdeg = (strmucomp * secphr * 6) / (dtk * cosfac)
          if (storm(ist)%gv_obs_lonew == 'W') then
            rnewlon = rlon - udistdeg
          else
            rnewlon = rlon + udistdeg
          endif
 
c         ------------------------------------------
c         This part updates the E/W and N/S characters
c         in the event that a storm changes hemisphere.
c         If a storm does change hemisphere, say from
c         W to E at 180, we need to also adjust the new
c         longitude value from say 186W to 174E.  Have to
c         include this code since storm messages contain
c         longitudes on a 0-180 basis (E&W), NOT 0-360.
 
          if (storm(ist)%gv_obs_latns == 'N') then
            if (rnewlat < 0.) then
              storm(ist)%gv_obs_latns = 'S'
              rnewlat   = -1. * rnewlat
            endif
          else
            if (rnewlat < 0.) then
              storm(ist)%gv_obs_latns = 'N'
              rnewlat   = -1. * rnewlat
            endif
          endif
c
          if (storm(ist)%gv_obs_lonew == 'W') then
            if (rnewlon > 180.) then
              ! The storm moved westward across 180....
              storm(ist)%gv_obs_lonew = 'E'
              rnewlon = 180. -  abs(rnewlon - 180.)
            elseif (rnewlon <= 0.) then
              ! A storm in the Western Hemisphere moved eastward
              ! across the Greenwich Meridian....
              storm(ist)%gv_obs_lonew = 'E'
              rnewlon = -1. * rnewlon
            endif
          else   
            if (rnewlon > 180.) then
              ! A storm in the Eastern Hemisphere moved eastward
              ! across 180 into the Western Hemisphere....
              storm(ist)%gv_obs_lonew = 'W'
              rnewlon = 180. - abs(rnewlon - 180.)
            elseif (rnewlon < 0.) then
              ! A storm in the Eastern Hemisphere moved westward
              ! across the Greenwich Meridian....
              storm(ist)%gv_obs_lonew = 'W'
              rnewlon = -1. * rnewlon
            endif
          endif

          storm(ist)%gv_obs_lat = int ((rnewlat + 0.05) * 10.)
          storm(ist)%gv_obs_lon = int ((rnewlon + 0.05) * 10.)
          storm(ist)%gv_obs_ymd  = ymd_now
          storm(ist)%gv_obs_hhmm = hhmm_now

        endif

        ist = ist + 1
   
      enddo
c
      return
      end

c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine delete_6ago_dups (storm,inowct,i6agoct,okstorm)
c
c     ABSTRACT: The purpose of this subroutine is to loop through the
c     list of storms for the dtg from 6h ago to eliminate any 
c     duplicates.  Be sure to sort based on storm identifier (e.g.,
c     13L) instead of storm name, since the name may change (e.g., 
c     from "THIRTEEN" to "IRIS") for an upgrade in intensity, but the
c     storm number identifier will remain the same.
c
c     ict     Total number of storm card entries for this dtg
c
      USE gen_vitals; USE set_max_parms
c 
      type (gencard) storm(maxstorm)
      logical(1) okstorm(maxstorm)
      character found_dup*1
c
      ist  = inowct + 1
      iend = inowct + i6agoct
      do while (ist < iend)

        isortnum  = ist + 1
        found_dup = 'n'
        if (okstorm(ist)) then

          do while (isortnum <= iend .and. found_dup == 'n')

            if (storm(ist)%gv_gen_id == storm(isortnum)%gv_gen_id)
     &      then
              found_dup = 'y'
            endif
            isortnum = isortnum + 1

          enddo

        endif

        if (found_dup == 'y') then
          okstorm(ist) = .FALSE.
        endif

        ist = ist + 1

      enddo

c     NOTE: The last member of the array to be checked is okay,
c     since all potential duplicates for this record were eliminated
c     in the previous sort while loop just completed, and, further,
c     the last member of this array is either already FALSE (from 
c     being checked off in delete_6ago), or it's TRUE because it
c     didn't get checked off in delete_6ago, so keep it.

      return
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine delete_6ago (storm,inowct,i6agoct,okstorm)
c
c     ABSTRACT: This subroutine compares the list of storm card 
c     entries from 6h ago to those from the current time to eliminate
c     any matching storms (i.e., if we've got a current record for a 
c     storm, we obviously don't need the 6h-old one).
c
      USE gen_vitals; USE set_max_parms
c
      type (gencard) storm(maxstorm)
c
      logical(1)   okstorm(maxstorm)
      character found_dup*1 
c
      ist  = inowct + 1
      iend = inowct + i6agoct
      do while (ist <= iend)

        isortnum = 1
        found_dup = 'n'
        do while (isortnum <= inowct .and. found_dup == 'n')

          if (storm(ist)%gv_gen_id == storm(isortnum)%gv_gen_id)
     &    then
            found_dup = 'y'
          endif
          isortnum = isortnum + 1

        enddo      

        if (found_dup == 'y') then
          okstorm(ist) = .FALSE.
        endif

        ist = ist + 1

      enddo

      return
      end

c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine delete_6ahead (storm,iprevct,i6aheadct,okstorm)
c
c     ABSTRACT: This subroutine compares the list of storm card
c     entries from 6h ahead to those from the current time and from 
c     6h ago to eliminate any matching storms (i.e., we only need the
c     record for 6h ahead if we don't have either a current time
c     record or a 6h-old record that we've  updated).
c
      USE gen_vitals; USE set_max_parms
c
      type (gencard) storm(maxstorm)
c
      logical(1)   okstorm(maxstorm)
      character found_dup*1
c
      ist  = iprevct + 1
      iend = iprevct + i6aheadct
      do while (ist <= iend)

        isortnum = 1
        found_dup = 'n'
        do while (isortnum <= iprevct .and. found_dup == 'n')

          if (storm(ist)%gv_gen_id == storm(isortnum)%gv_gen_id)
     &    then
            found_dup = 'y'
          endif
          isortnum = isortnum + 1

        enddo

        if (found_dup == 'y') then
          okstorm(ist) = .FALSE.
        endif

        ist = ist + 1

      enddo

      return
      end

c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine delete_6ahead_dups (storm,iprevct,i6aheadct,okstorm)
c
c     ABSTRACT: The purpose of this subroutine is to loop through the
c     list of storms for the dtg from 6h ahead to eliminate any
c     duplicates.  Be sure to sort based on storm identifier (e.g.,
c     13L) instead of storm name, since the name may change (e.g.,
c     from "THIRTEEN" to "IRIS") for an upgrade in intensity, but the
c     storm number identifier will remain the same.
c
c     ict     Total number of storm card entries for this dtg
c
      USE gen_vitals; USE set_max_parms
c
      type (gencard) storm(maxstorm)
      logical(1) okstorm(maxstorm)
      character found_dup*1
c
      ist  = iprevct + 1
      iend = iprevct + i6aheadct
      do while (ist < iend)

        isortnum  = ist + 1
        found_dup = 'n'
        if (okstorm(ist)) then

          do while (isortnum <= iend .and. found_dup == 'n')

            if (storm(ist)%gv_gen_id == storm(isortnum)%gv_gen_id)
     &      then
              found_dup = 'y'
            endif
            isortnum = isortnum + 1

          enddo

        endif

        if (found_dup == 'y') then
          okstorm(ist) = .FALSE.
        endif

        ist = ist + 1

      enddo

c     NOTE: The last member of the array to be checked is okay,
c     since all potential duplicates for this record were eliminated
c     in the previous sort while loop just completed, and, further,
c     the last member of this array is either already FALSE (from
c     being checked off in delete_6ahead), or it's TRUE because it
c     didn't get checked off in delete_6ahead, so keep it.

      return
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine delete_dups (storm,ict,okstorm)
c
c     ABSTRACT: The purpose of this subroutine is to loop through the
c     list of storms for the current dtg to eliminate any duplicates.
c     Be sure to sort based on storm identifier (e.g.,13L) instead of
c     storm name, since the name may change (e.g., from "THIRTEEN" to
c     "IRIS") for an upgrade in intensity, but the storm number 
c     identifier will remain the same.
c
c     ict     Total number of storm card entries for this dtg
c
      USE gen_vitals; USE set_max_parms
c  
      type (gencard) storm(maxstorm)
      logical(1) okstorm(maxstorm)
      character found_dup*1
c
      ist = 1
      do while (ist < ict)

        isortnum  = ist + 1
        found_dup = 'n'
        do while (isortnum <= ict .and. found_dup == 'n')

          if (storm(ist)%gv_gen_id == storm(isortnum)%gv_gen_id)
     &    then
            found_dup = 'y'
          endif
          isortnum = isortnum + 1

        enddo

        if (found_dup == 'y') then
          okstorm(ist) = .FALSE.
        endif

        ist = ist + 1

      enddo

c     Now set the last member of the array to be checked as okay, 
c     since all potential duplicates for this record were eliminated 
c     in the previous sort while loop just completed.

      okstorm(ict) = .TRUE.
c
      return
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine read_nlists (dnow,d6ago,d6ahead)
c
c     ABSTRACT:  Read in the namelists that contain the date for the
c     current time and the time from 6 hours ago.  It also converts
c     the input dates for the current time, the 6 hour-ago time and
c     the 6 hour-ahead time into a format that can be easily compared
c     against the dates in the TC Vitals file.
c
      USE inparms; USE date_checks
c
      type (datecard) dnow,d6ago,d6ahead
c
      namelist/datenowin/dnow
      namelist/date6agoin/d6ago
      namelist/date6aheadin/d6ahead
c
      read (5,NML=datenowin,END=801)
  801 continue
      read (5,NML=date6agoin,END=803)
  803 continue
      read (5,NML=date6aheadin,END=805)
  805 continue
c
      ymd_now     = dnow%yy * 10000 + dnow%mm * 100 + dnow%dd
      hhmm_now    = dnow%hh * 100
      ymd_6ago    = d6ago%yy * 10000 + d6ago%mm * 100 + d6ago%dd
      hhmm_6ago   = d6ago%hh * 100
      ymd_6ahead  = d6ahead%yy * 10000 + d6ahead%mm * 100 + d6ahead%dd
      hhmm_6ahead = d6ahead%hh * 100
c
      return
      end
c
c----------------------------------------------------------------------
c
c----------------------------------------------------------------------
      integer function char2int (charnum)
c
c     This function takes as input a character numeral and
c     returns the integer equivalent
c
      character*1 charnum,cx(10)
      data cx/'0','1','2','3','4','5','6','7','8','9'/
c
      do i=1,10
        if (charnum.eq.cx(i)) char2int = i-1
      enddo
c
      return
      end
c
c----------------------------------------------------------------------
c
c----------------------------------------------------------------------
      character function int2char (inum)
c
c     This function takes as input an integer and
c     returns the character numeral equivalent
c
      character*1 cx(10)
      data cx/'0','1','2','3','4','5','6','7','8','9'/
c
      do i=1,10
        ihold=i-1
        if (ihold.eq.inum) int2char = cx(i)
      enddo
c
      return
      end
