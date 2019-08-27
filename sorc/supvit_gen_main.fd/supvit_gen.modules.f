      module gen_vitals
        type gencard     ! Define a new type for a genesis vitals card
          sequence
          character*30  gv_gen_id      ! Full 30 chars of storm ID
          integer       gv_obs_ymd     ! Date of observation cycle
                                       ! (yyyymmdd)
          integer       gv_obs_hhmm    ! Time of observation cycle (UTC)
          integer       gv_obs_lat     ! Storm Lat (*10), always >0
          character*1   gv_obs_latns   ! 'N' or 'S'
          integer       gv_obs_lon     ! Storm Lon (*10), always >0
          character*1   gv_obs_lonew   ! 'E' or 'W'
          integer       gv_stdir       ! Storm motion vector (in degr)
          integer       gv_stspd       ! Spd of storm movement (m/s*10)
          character*85  tcv_chunk      ! Remainder of vitals record;
                                       ! will just be read & written
        end type gencard
      end module gen_vitals
c
      module inparms
        type datecard  ! Define a new type for the input namelist parms
          sequence
          integer       yy    ! Beginning yy of date to search for
          integer       mm    ! Beginning mm of date to search for
          integer       dd    ! Beginning dd of date to search for
          integer       hh    ! Beginning hh of date to search for
        end type datecard
      end module inparms
c
      module date_checks
        integer, save  :: ymd_now,hhmm_now,ymd_6ago,hhmm_6ago
     &                   ,ymd_6ahead,hhmm_6ahead
      end module date_checks
c
      module set_max_parms
        integer, parameter :: maxstorm=2000  ! max # of storms pgm can
                                            ! handle
      end module set_max_parms
c
      module trig_vals
        real, save :: pi, dtr, rtd
        real, save :: dtk = 111194.9     ! Dist (m) over 1 deg lat
                                         ! using erad=6371.0e+3
        real, save :: erad = 6371.0e+3   ! Earth's radius (m)
        real, save :: ecircum = 40030200  ! Earth's circumference
                                          ! (m) using erad=6371.e3
        real, save :: omega = 7.292e-5
        real, save :: secphr = 3600.
      end module trig_vals
c
c------------------------------------------------------
c
