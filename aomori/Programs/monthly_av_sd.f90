program monthly_av_sd_wo2012
    implicit none
    integer::y,m,l,st,d,n,station_quan
    real::salco,tempco,axis_temp_range,axis_sal_range,diff,r,g,b,rosso,verde,azul,xco,yco
    integer,parameter:: years = 15, months = 12, lines = 2, stations = 9,depth = 400,ini_st = 4,js = 1,obs_line =1
    real,parameter::temp_min = 0., temp_max = 25., axis_sal_min = 33.0, axis_sal_max = 34.5, standard_sal_450 = 34.07
    real,parameter::width = 1.5, height = 3.
    real,parameter::dx = width/real(stations-ini_st+1),dy = -height/real(depth)
    real,dimension(years,months,lines,stations,depth)::potemp_5,sal_5
    real,dimension(months,lines,stations,depth)::temp_avarray,temp_sdarray,sal_avarray,sal_sdarray
    real,dimension(stations,depth)::ps_temp_avarray,ps_temp_sdarray,ps_temp_dataarray,ps_sal_avarray,ps_sal_sdarray,ps_sal_dataarray
    integer,dimension(stations,depth)::temp_mask,sal_mask
    integer,dimension(months,lines,stations,depth)::temp_dataarray,sal_dataarray
    character(len=4),dimension(12)::month_names

    if(obs_line==1)then
        call plots(3.,15.5,13,'/LARGE0/gr10291/nishimori2/aomori/Monthly_av_and_sd/monthly_data1-6_NLine_HOSEI.ps')
        call symbol(1.5,3.,1.,'Monthly Data at N-Line stations 1 to 6', 0.,len('monthly data at N line stations 8 to 9'))
    else 
        call plots(2.5,15.5,13,'/LARGE0/gr10291/nishimori2/aomori/Monthly_av_and_sd/monthly_data_SLine_2012_excluded.ps')
        call symbol(1.,3.,1.,'Monthly Data at S-Line 2012 excluded', 0.,len('monthly data at S line 2012 excluded'))
    end if
    call newpen2(3)
    call create_potemp_5(potemp_5)
    call create_sal_5(sal_5)
    call month_str_array(month_names)
    axis_temp_range = temp_max - temp_min; axis_sal_range = axis_sal_max - axis_sal_min
    
    sal_5(1,12,2,5,1:depth)=0.;sal_5(1,12,2,6,1:depth)=0.;sal_5(1,12,2,7,1:depth)=0.!May 10 金サロ前
    sal_5(2,4,1,4,1:depth)=0.
    ! sal_5(4,10,1,4,:)=0.;sal_5(4,10,1,6,:)=0.;sal_5(4,10,1,7,:)=0.;sal_5(4,10,1,8,:)=0.;sal_5(4,10,1,5,:)=0.;sal_5(4,12,1,8,:)=0.;sal_5(4,12,1,9,:)=0.
    ! sal_5(4,12,1,4,:)=0.;sal_5(4,12,1,5,:)=0.;sal_5(4,12,1,6,:)=0.;sal_5(4,12,1,7,:)=0. !may 10 金サロ後
    sal_5(4,1:months,1:lines,1:stations,1:depth)=0.
    potemp_5(1:years,1:months,1:lines,1:stations,1:30)=0.
    sal_5(1:years,1:months,1:lines,1:stations,1:30)=0.
    station_quan = stations - ini_st +1

!データ処理
    do y = 1, years
        do m = 1,months
            do l = 1,lines
                do st = ini_st, stations
                    if (sal_5(y,m,l,st,depth)<30. .or. sal_5(y,m,l,st,depth)>35.) then
                        sal_5(y,m,l,st,1:depth) = 0.  !一列全部0 データが元から無い列も0だからこれに含まれる
                        potemp_5(y,m,l,st,1:depth) = 0.
                    else
                        diff = sal_5(y,m,l,st,depth) - standard_sal_450 
                    end if
                    do d = 1, depth
                        if(sal_5(y,m,l,st,d)/=0.) then
                        sal_5(y,m,l,st,d) = sal_5(y,m,l,st,d) - diff
                        else; end if
                    end do
                    !diff = 0.
                end do 
            end do
        end do
    end do

call avsd_dataquan(potemp_5,temp_avarray,temp_sdarray,temp_dataarray)
call avsd_dataquan(sal_5,sal_avarray,sal_sdarray,sal_dataarray)

do l = obs_line,obs_line
    do m = 1, months
        do st = ini_st,stations
            do d = 1,depth
                if (m ==1 .or. m == 7 .and. st == 9) then            !temp av for jan and jul
                    do n = 1,24
                        if (n<=10) then
                            r = (real(n))/10.*.9; g = (real(n))/10.*.9; b = 1.
                        else
                            r = 1.; g = .9-(real(n)-10.)/14.*.9 ;b = .9-(real(n)-10.)/14.*.9  
                        end if
                        if (real(n-1)< temp_avarray(m,l,st,d) .and. temp_avarray(m,l,st,d)<=real(n)) then
                            call betsqk(dx*real(stations-ini_st),real(d-1)*dy,dx*real(stations-ini_st+1),real(d)*dy,r,g,b)
                        else if(temp_avarray(m,l,st,d)<0.) then
                            call betsqk(dx*real(stations-ini_st),real(d-1)*dy,dx*real(stations-ini_st+1),real(d)*dy,0.,0.,1.)
                        else if(temp_avarray(m,l,st,d)==0.) then
                            call betsqk(dx*real(stations-ini_st),real(d-1)*dy,dx*real(stations-ini_st+1),real(d)*dy,1.,1.,1.)
                        else if(temp_avarray(m,l,st,d)>24.) then
                            call betsqk(dx*real(stations-ini_st),real(d-1)*dy,dx*real(stations-ini_st+1),real(d)*dy,1.,0.,0.)
                        end if
                    end do
                    
                    call plot(0.,-height*(1.3),-3)
                    do n = 1,20                                     !temp sd for jan and jul
                        if (n<=10) then
                            r = (real(n))/10.*.9; g = (real(n))/10.*.9; b = 1.
                        else 
                            r = 1.; g = .9-(real(n)-10.)/10.*.9; b =.9-(real(n)-10.)/10.*.9
                        end if
                        if (real(n-1)/4.<temp_sdarray(m,l,st,d) .and. temp_sdarray(m,l,st,d)<=real(n)/4.) then
                            call betsqk(dx*real(stations-ini_st),real(d-1)*dy,dx*real(stations-ini_st+1),real(d)*dy,r,g,b)
                        else if(temp_sdarray(m,l,st,d) == 0.) then
                            call betsqk(dx*real(stations-ini_st),real(d-1)*dy,dx*real(stations-ini_st+1),real(d)*dy,1.,1.,1.)
                        else if (temp_sdarray(m,l,st,d)>5.) then
                            call betsqk(dx*real(stations-ini_st),real(d-1)*dy,dx*real(stations-ini_st+1),real(d)*dy,1.,0.,0.)
                        end if
                    end do
                    
                    call plot(0.,-height*(1.3),-3)
                    do n=1,25                                       !sal av for jan and jul
                        if (n<=10) then
                            r = (real(n))/10.*.9; g = (real(n))/10.*.9; b = 1.
                        else
                            r = 1.; g = .9-(real(n)-10.)/15.*0.9; b = .9-(real(n)-10.)/15.*0.9
                        end if
                        if (33.8+real(n-1)/50.<sal_avarray(m,l,st,d) .and. sal_avarray(m,l,st,d)<=33.8+real(n)/50.) then
                            call betsqk(dx*real(stations-ini_st),real(d-1)*dy,dx*real(stations-ini_st+1),real(d)*dy,r,g,b)
                        else if(sal_avarray(m,l,st,d)<33.8 .and. sal_avarray(m,l,st,d)/=0.) then
                            call betsqk(dx*real(stations-ini_st),real(d-1)*dy,dx*real(stations-ini_st+1),real(d)*dy,0.,0.,1.)
                        else if(sal_avarray(m,l,st,d)>34.3) then
                            call betsqk(dx*real(stations-ini_st),real(d-1)*dy,dx*real(stations-ini_st+1),real(d)*dy,1.,0.,0.)
                        else if(sal_avarray(m,l,st,d) == 0.) then
                            call betsqk(dx*real(stations-ini_st),real(d-1)*dy,dx*real(stations-ini_st+1),real(d)*dy,1.,1.,1.)
                        end if
                    end do
                    
                    call plot(0.,-height*(1.3),-3)
                    do n=1,40,1                                     !sal sd for jan and jul
                        if (n<=20) then
                            r = (real(n))/20.*.9; g = (real(n))/20.*.9; b = 1.
                        else 
                            r = 1.; g = .9-(real(n)-20.)/20.*.9; b = .9-(real(n)-20.)/20.*.9
                        end if
                        if (real(n-1)/100.<sal_sdarray(m,l,st,d) .and. sal_sdarray(m,l,st,d)<=real(n)/100.) then
                            call betsqk(dx*real(stations-ini_st),real(d-1)*dy,dx*real(stations-ini_st+1),real(d)*dy,r,g,b)
                        else if(sal_sdarray(m,l,st,d) == 0.) then
                            call betsqk(dx*real(stations-ini_st),real(d-1)*dy,dx*real(stations-ini_st+1),real(d)*dy,1.,1.,1.)
                        else if(sal_sdarray(m,l,st,d) > 0.50) then
                            call betsqk(dx*real(stations-ini_st),real(d-1)*dy,dx*real(stations-ini_st+1),real(d)*dy,1.,0.,0.)
                        end if
                    end do
                            
                    call plot(0.,3.*height*(1.3),-3)
                end if                                 !end of betsqk for jan and jul
                ps_temp_avarray(st,d) = temp_avarray(m,l,st,d)
                ps_temp_sdarray(st,d) = temp_sdarray(m,l,st,d)
                ps_sal_avarray(st,d) = sal_avarray(m,l,st,d)
                ps_sal_sdarray(st,d) = sal_sdarray(m,l,st,d)
                if(temp_avarray(m,l,st,d)/=0.) then; temp_mask(st,d)=1;else;temp_mask(st,d)=0;end if
                if(sal_avarray(m,l,st,d)/=0.) then; sal_mask(st,d)=1;else;sal_mask(st,d)=0;end if
            end do !end of d
        end do !end of st
        call symbolc(width/2.,.5,.4,month_names(m),0.,len(month_names(m)))
        do n = 1,4
            call psframe(ini_st,stations,400,width,height,0.05)
            call plot(0.,-height*(1.3),-3) 
        end do;call plot(0.,4.*height*(1.3),-3)
        if(m == 1) then
            call symbolc(-.7,-height/2.,0.4,'Temp Av',90.,len('temp av'))
            else;end if
        
            call pscolork(dx,dy,ps_temp_avarray,temp_mask,ini_st,stations,js,depth,stations,depth,-100.,0.,0.,0.,1.)
            call pscolork(dx,dy,ps_temp_avarray,temp_mask,ini_st,stations,js,depth,stations,depth,24.,100.,1.,0.,0.)
        
            do n=1,24,1  !最初は，１月,S_line の鉛直図の作成 
            if (n<=10) then
                r = (real(n))/10.*.9
                g = (real(n))/10.*.9
                b = 1.     
            else 
                r = 1.
                g = .9-(real(n)-10.)/14.*.9
                b = .9-(real(n)-10.)/14.*.9
            end if
            call pscolorK(dx,dy,ps_temp_avarray,temp_mask,ini_st,stations,js,depth,stations,depth,real(n-1),real(n),r,g,b)
            end do   
        call newpen2(1)
        call rgbk(0.,0.,0.)
        call pscont3(dx,dy,ps_temp_avarray,temp_mask,ini_st,stations,js,depth,stations,depth,8,0.,3.)                                                                                       !水温の平均鉛直図一個完成


        !↓温度ｓｄ図      
        call plot(0.,-height*(1.3),-3) 
        if(m == 1) then
            call symbolc(-.7,-height/2.,0.4,'Temp sd',90.,len('temp sd'))
            else
            end if
            call pscolork(dx,dy,ps_temp_sdarray,temp_mask,ini_st,stations,js,depth,stations,depth,-100.,0.,0.,0.,1.)
            call pscolork(dx,dy,ps_temp_sdarray,temp_mask,ini_st,stations,js,depth,stations,depth,5.,100.,1.,0.,0.)
            do n=1,20,1  !最初は，１月,S_line の鉛直図の作成 
            if (n<=10) then
                r = (real(n))/10.*.9
                g = (real(n))/10.*.9
                b = 1.
                else 
                r = 1.
                g = .9-(real(n)-10.)/10.*.9
                b = .9-(real(n)-10.)/10.*.9
            end if
            call pscolorK(dx,dy,ps_temp_sdarray,temp_mask,ini_st,stations,js,depth,stations,depth,real(n-1)/4.,real(n)/4.,r,g,b)
        end do

        !↓塩分av図
        call plot(0.,-height*(1.3),-3)
        if(m == 1) then
            call symbolc(-.7,-height/2.,0.4,'Sal Av',90.,len('sal av'))
            else
            end if
            call pscolorK(dx,dy,ps_sal_avarray,sal_mask,ini_st,stations,js,depth,stations,depth,-100.,33.8,0.,0.,1.)
            call pscolorK(dx,dy,ps_sal_avarray,sal_mask,ini_st,stations,js,depth,stations,depth,34.3,100.,1.,0.,0.)

            do n=1,25  !最初は，１月,S_line の鉛直図の作成 
            if (n<=10) then
                r = (real(n))/10.*.9
                g = (real(n))/10.*.9
                b = 1.
            else
                r = 1.
                g = .9-(real(n)-10.)/15.*0.9
                b = .9-(real(n)-10.)/15.*0.9
            end if
            call pscolorK(dx,dy,ps_sal_avarray,sal_mask,ini_st,stations,js,depth,stations,depth,33.8+real(n-1)/50.,33.8+real(n)/50.,r,g,b)
            end do
        ! print*, sal_avarray(8,200)
            call newpen2(1)
            call rgbk(0.,0.,0.)
            call pscont3(dx,dy,ps_sal_avarray,sal_mask,ini_st,stations,js,depth,stations,depth,6,33.8,0.1)

        !↓塩分sd図
        call plot(0.,-height*(1.3),-3)
        if(m == 1) then
            call symbolc(-.7,-height/2.,0.4,'Sal sd',90.,len('sal sd'))
            else
            end if
            call pscolork(dx,dy,ps_sal_sdarray,sal_mask,ini_st,stations,js,depth,stations,depth,0.4,100.,1.,0.,0.)
                do n=1,40,1  !最初は，１月,S_line の鉛直図の作成 
                    if (n<=20) then
                        r = (real(n))/20.*.9
                        g = (real(n))/20.*.9
                        b = 1.
                    else 
                        r = 1.
                        g = .9-(real(n)-20.)/20.*.9
                        b = .9-(real(n)-20.)/20.*.9
                    end if
                    call pscolorK(dx,dy,ps_sal_sdarray,sal_mask,ini_st,stations,js,depth,stations,depth,real(n-1)/100.,real(n)/100.,r,g,b)
                end do

        call plot(width+0.4,3.*height*(1.3),-3)   
    end do !end of months
end do ! end of lines

call newpage

call newpen2(3)
call plot(0.,3.,-3)
call symbol(0.,0.5,0.3,'Temp Av (deg.C)',0.,len('Temp Av (deg.c)'))
    do n=0,25                       !temp av　凡例
        if(n==0) then
            rosso = 0.; verde = 0. ; azul =1.
        else if (1<=n .and. n<=10) then
            rosso = (real(n))/10.*.9 
            verde = (real(n))/10.*.9
            azul = 1.
        else if (11<=n .and. n<=24) then
            rosso = 1.
            verde = .9-(real(n)-10.)/14.*.9
            azul = .9-(real(n)-10.)/14.*.9
        else ! n==25
            rosso = 1.;verde = 0. ; azul = 0.
        end if
            call betsqk(0.,0.,0.6,0.4,rosso,verde,azul)
            call rgbk(0.,0.,0.)
            call plot(0.,0.,3)
            call plot(0.,-0.05,2)
            call plot(0.,0.,3)
            call plot(0.6,0.,2)
            call plot(0.6,0.,3)
            call plot(0.6,-0.05,2)
            if (1<=n .and. n<=24) then
                call numberc(0.6,-0.3,0.2,real(n),0.,1)
            else if (n ==0) then
                call symbolr(0.,-0.3,0.2,'<0.0',0.,4)
                call symbolc(0.6,-0.3,0.2,'0.0',0.,3)
            else if (n == 25) then
                call symbol(0.6,-0.3,0.2,'24.0<',0.,5)
            end if           
        call plot(0.6,0.,-3)
    end do

    call plot(-26.*0.6,-2.,-3)
    call symbol(0.,0.5,0.3,'Temp sd (deg.C)',0.,len('Temp sd (deg.c)'))
    do n=0,21                       !temp sd 凡例
        if(n==0) then
            rosso = 0.; verde = 0. ; azul =1.
        else if (1<=n .and. n<=10) then
            rosso = (real(n))/10.*.9 
            verde = (real(n))/10.*.9
            azul = 1.
        else if (11<=n .and. n<=20) then
            rosso = 1.
            verde = .9-(real(n)-10.)/10.*.9
            azul = .9-(real(n)-10.)/10.*.9
        else ! n==21
            rosso = 1.;verde = 0. ; azul = 0.
        end if
        call betsqk(0.,0.,15.6/22.,0.4,rosso,verde,azul)
            call rgbk(0.,0.,0.)
            call plot(0.,0.,3)
            call plot(0.,-0.05,2)
            call plot(0.,0.,3)
            call plot(15.6/22.,0.,2)
            call plot(15.6/22.,0.,3)
            call plot(15.6/22.,-0.05,2)
            if (1<=n .and. n<=20) then
                call numberc(15.6/22.,-0.3,0.2,real(n)/4.,0.,2)
            else if (n ==0) then
                ! call symbolc(0.,-0.3,0.2,'<0.0',0.,4)
                call symbolc(15.6/22.,-0.3,0.2,'0.0',0.,3)
            else if (n == 21) then
                call symbol(0.6,-0.3,0.2,'5.00<',0.,5)
            end if           
        call plot(15.6/22.,0.,-3)
    end do

    call plot(-15.6,-2.,-3)
    call symbol(0.,0.5,0.3,'Sal av (per mille)',0.,len('Sal av (per mille)'))
    do n=0,26                       !sal av 凡例
        if (n ==0) then
            rosso = 0.; verde = 0.; azul =1.
        else if (1<=n .and. n<=10) then
            rosso = (real(n))/10.*.9
            verde = (real(n))/10.*.9
            azul = 1.
        else if (11<=n .and. n<=25) then
            rosso = 1.
            verde = .9-(real(n)-10.)/15.*.9
            azul = .9-(real(n)-10.)/15.*.9
        else ! n =26
            rosso=1.;verde=0.;azul=0.
        end if
            call betsqk(0.,0.,15.6/27.,0.4,rosso,verde,azul)
            call rgbk(0.,0.,0.)
            call plot(0.,0.,3)
            call plot(0.,-0.05,2)
            call plot(0.,0.,3)
            call plot(15.6/27.,0.,2)
            call plot(15.6/27.,0.,3)
            call plot(15.6/27.,-0.05,2)
            if (1<=n .and. n<=25 .and. mod(n,2)/=0) then
                call numberc(15.6/27.,-0.3,0.2,33.8+real(n)/50.,0.,2)
            else if (n ==0) then
                call symbolr(0.,-0.3,0.2,'<33.80',0.,6)
                call symbolc(15.6/27.,-0.3,0.2,'33.80',0.,5)
            else if (n == 26) then
                call symbol(15.6/27.,-0.3,0.2,'34.30<',0.,6)
            end if    
            call plot(15.6/27.,0.,-3)       
        end do

        call plot(3.,0.,-3);call create_map(40,42,137,142,ini_st,stations,4.);call plot(-3.,0.,-3)
    call plot(-15.6,-2.,-3)

        call symbol(0.,0.5,0.3,'Sal sd (per mille)',0.,len('Sal sd (per mille)'))
    do n=0,41                       !sal sd 凡例
        if (n ==0) then
            rosso = 0.; verde = 0.; azul =1.
        else if (1<=n .and. n<=20) then
            rosso = (real(n))/20.*.9
            verde = (real(n))/20.*.9
            azul = 1.
        else if (21<=n .and. n<=40) then
            rosso = 1.
            verde = .9-(real(n)-20.)/20.*.9
            azul = .9-(real(n)-20.)/20.*.9
        else ! n =41
            rosso=1.;verde=0.;azul=0.
        end if
            call betsqk(0.,0.,15.6/42.,0.4,rosso,verde,azul)
            call rgbk(0.,0.,0.)
            call plot(0.,0.,3)
            call plot(0.,-0.05,2)
            call plot(0.,0.,3)
            call plot(15.6/42.,0.,2)
            call plot(15.6/42.,0.,3)
            call plot(15.6/42.,-0.05,2)
            if (1<=n .and. n<=40 .and. mod(n,2)==0) then
                call numberc(15.6/42.,-0.3,0.2,real(n)/100.,0.,2)
            else if (n ==0) then
                ! call symbolc(0.,-0.3,0.2,'<33.80',0.,6)
                call symbolc(15.6/42.,-0.3,0.2,'0.00',0.,4)
            else if (n == 41) then
                call symbol(15.6/42.,-0.3,0.2,'0.40<',0.,5)
            end if    
            call plot(15.6/42.,0.,-3)       
    end do
    call plot(-15.6,-2.5,-3)

    call symbol(0.,height/3.,0.5,'Data Quantity',0.,len('data quantity'))
    call symbolc(-.7,-height/2.,0.4,'Temp',90.,len('temp'))
    call newpen2(3)

    do m = 1,months
        do l = obs_line,obs_line
            do st = ini_st,stations
                do d = 1, depth
                    if (temp_dataarray(m,l,st,d) == 0) then
                        r = 1.
                        g = 1.
                        b = 1.
                    else if (temp_dataarray(m,l,st,d) <= 10) then
                        r = 1.
                        g = (real(temp_dataarray(m,l,st,d)))/10.
                        b = 0.
                    else 
                        r = 1.-(real(temp_dataarray(m,l,st,d))-10.)/5.
                        g = 1.
                        b = 0.
                    end if
                    xco = dx*real(st-(ini_st))
                    yco = dy*real(d-1)
                    call betsqk(xco,yco,xco+dx,yco+dy,r,g,b)
                                                  
                end do !end of d
            end do !end of st
                call symbolc(width/2.,.4,.4,month_names(m),0.,len(month_names(m)))
                call psframe(ini_st,stations,depth,width,height,0.05)
                call plot(real(station_quan)*dx+0.4,0.,-3)
        end do !end of lines
    end do !end of months
    call plot(-12.*(real(station_quan)*dx+.4),-height-1.,-3)

    call symbolc(-.7,-width/2.,0.4,'Sal',90.,len('sal'))
    do m = 1,months
        do l = obs_line,obs_line
            do st = ini_st,stations
                do d = 1, depth
                    if (sal_dataarray(m,l,st,d) == 0) then
                        r = 1.
                        g = 1.
                        b = 1.
                    else if (sal_dataarray(m,l,st,d) <= 10) then
                        r = 1.
                        g = (real(sal_dataarray(m,l,st,d)))/10.
                        b = 0.
                    else 
                        r = 1.-(real(sal_dataarray(m,l,st,d))-10.)/5.
                        g = 1.
                        b = 0.
                    end if
                    xco = dx*real(st-(ini_st))
                    yco = dy*real(d-1)
                    call betsqk(xco,yco,xco+dx,yco+dy,r,g,b)
                                                  
                end do !end of d
            end do !end of st
                call symbolc(width/2.,.4,.4,month_names(m),0.,len(month_names(m)))
                call psframe(ini_st,stations,depth,width,height,0.05)
                call plot(real(station_quan)*dx+0.4,0.,-3)
        end do !end of lines
    end do !end of months
    call plot(-12.*(real(station_quan)*dx+.4),-height-1.,-3)
    call symbol(0.,0.5,0.3,'Data Quantity',0.,len('Data Quantity'))
    do n = 0,15                             !data quantity legend
        if(n==0) then
            r = 1.
            g = 1.
            b = 1.
        else if (n<=10) then
            r = 1.
            g = (real(n)/10.)
            b = 0.
        else
            r = 1.-(real(n)-10.)/5.
            g = 1.
            b = 0.
        end if
        call betsqk(0.,0.,14.4/15.,0.4,r,g,b)
        call rgbk(0.,0.,0.)
        call plot(0.,0.,3)
        call plot(14.4/15.,0.,2)
        call plot(14.4/15./2.,0.,3)
        call plot(14.4/15./2.,-0.05,2)
        call numberc(14.4/15./2.,-0.3,0.2,real(n),0.,-1)
        
        call plot(14.4/15.,0.,-3)
    end do
call plote
end program