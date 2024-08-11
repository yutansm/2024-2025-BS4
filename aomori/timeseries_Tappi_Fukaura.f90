program examining_2011
    implicit none
    integer,parameter::years = 15, months = 12, lines = 2, stations = 9, depth = 400
    integer,parameter::l = 1, st = 9
    real,dimension(years,months)::SSH_f,SSH_t
    integer::y,m

    call calibrated_fukauraSSH(SSH_f);call calibrated_tappiSSH(SSH_t)
    ! print*,minval(SSH_f),minval(SSH_t)
    call plots(1.,6.,13,'/LARGE0/gr10291/nishimori2/aomori/Errorbar_plots/timeseries_SSH_FandT.ps')
    call mod12_memori(12*15,0.2,20.,0.,0.)
    call plote
end program