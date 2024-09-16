program TS_diagram_aomori
    implicit none
    integer::y,m,l,st,d
    real::salco,tempco,axis_temp_range,axis_sal_range,diff,r,g,b
    integer,parameter:: years = 15, months = 12, lines = 1, stations = 9,depth = 400
    real,parameter::temp_min = 0., temp_max = 25., axis_sal_min = 33.0, axis_sal_max = 34.5, standard_sal_450 = 34.07
    real,parameter::width =4.5, height = 4.5
    real,dimension(years,months,lines,stations,depth)::potemp_5,sal_5
    character(len=3),dimension(12)::month_names

    if(lines==1)then
        call plots(2.5,13.5,13,'/LARGE0/gr10291/nishimori2/aomori/TS_Diagrams/TS_diagram_monthly_NLine_2012_excluded.ps')
        call symbol(1.,5.5,1.,'Monthly TS Diagrams at N-Line 2012 excluded', 0.,len('monthly ts diagrams at N line 2012 excluded'))
    else 
        call plots(2.5,13.5,13,'/LARGE0/gr10291/nishimori2/aomori/TS_Diagrams/TS_diagram_monthly_SLine_2012_excluded.ps')
        call symbol(1.,5.5,1.,'Monthly TS Diagrams at S-Line 2012 excluded', 0.,len('monthly ts diagrams at S line 2012 excluded'))
    end if
    call newpen2(3)
    call create_potemp_5(years,months,lines,stations,depth,potemp_5)
    call create_sal_5(years,months,lines,stations,depth,sal_5)
    call month_str_array(month_names)
    axis_temp_range = temp_max - temp_min; axis_sal_range = axis_sal_max - axis_sal_min
    
    sal_5(1,12,2,5,:)=0.;sal_5(1,12,2,6,:)=0.;sal_5(1,12,2,7,:)=0.!May 10 金サロ前
    sal_5(2,4,1,4,:)=0.
    ! sal_5(4,10,1,4,:)=0.;sal_5(4,10,1,6,:)=0.;sal_5(4,10,1,7,:)=0.;sal_5(4,10,1,8,:)=0.;sal_5(4,10,1,5,:)=0.;sal_5(4,12,1,8,:)=0.;sal_5(4,12,1,9,:)=0.
    ! sal_5(4,12,1,4,:)=0.;sal_5(4,12,1,5,:)=0.;sal_5(4,12,1,6,:)=0.;sal_5(4,12,1,7,:)=0. !may 10 金サロ後
    sal_5(4,:,:,:,:)=0.


    do y = 1, years
        do m = 1,months
            do l = lines, lines
                do st = 4, stations
                    if (sal_5(y,m,l,st,depth)<30. .or. sal_5(y,m,l,st,depth)>35.) then
                        sal_5(y,m,l,st,:) = 0.  !一列全部0 データが元から無い列も0だからこれに含まれる
                        potemp_5(y,m,l,st,:) = 0.
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

    do m = 1,months
        call rgbk(0.,0.,0.)
        call TS_diagram(temp_min,temp_max,axis_sal_min,axis_sal_max,100,20.,0.5,width,height,0.,0.)
        call symbolc(width/2.,height+0.5,0.5,month_names(m),0.,3)
        do st = 4, stations
            do l = lines,lines
                do y = 1, years
                    do d = 1, depth
                        salco = width*(sal_5(y,m,l,st,d)-axis_sal_min)/axis_sal_range; tempco = height*(potemp_5(y,m,l,st,d)-temp_min)/axis_temp_range
                        if (st<=3) then; r = 0.6-0.6*real(st-1)/2.; g = 0.6-0.6*real(st-1)/2.; b = 1.-0.2*real(st)/2.
                            else if (st>=4 .and. st<=6) then; r = 0.6-0.6*real(st-4)/2.; g = 1.-0.2*real(st-4)/2.; b = 0.6-0.6*real(st-4)/2.
                            else if(st>=7 .and. st<=9) then; r = 1.-0.2*real(st-7)/2. ; g = 0.6-0.6*real(st-7)/2.; b = 0.6-0.6*real(st-7)/2.
                            else;end if  !色決定
                        if(sal_5(y,m,l,st,d)/=0. .and. potemp_5(y,m,l,st,d)/=0.) then;call rgbk(r,g,b)
                            if (st<=3) then;call gmark(salco,tempco,0.05,1)
                            else if (st>=4 .and. st<=6) then;call gmark(salco,tempco,0.1-real(st-1)/100.,6)
                            else if(st>=7 .and. st<=9) then;call gmark(salco,tempco,0.07-real(st-4)/100.,8)
                            else;end if
                        else; end if
                    end do
                end do
            end do
        end do
        if (mod(m,4)==0) then;call plot(-3*(width+1.5),-height-1.7,-3)
        else;call plot(width+1.5,0.,-3);end if
    end do

    call plot(0.,height+2.-1.,-3)

    do st = 4, stations
        if (st<=3) then; r = 0.6-0.6*real(st-1)/2.; g = 0.6-0.6*real(st-1)/2.; b = 1.-0.2*real(st)/2.
    else if (st>=4 .and. st<=6) then; r = 0.6-0.6*real(st-4)/2.; g = 1.-0.2*real(st-4)/2.; b = 0.6-0.6*real(st-4)/2.
    else if(st>=7 .and. st<=9) then; r = 1.-0.2*real(st-7)/2. ; g = 0.6-0.6*real(st-7)/2.; b = 0.6-0.6*real(st-7)/2.
    else;end if
        call rgbk(r,g,b)
        if (st<=3) then;call gmark(0.,0.,0.2,1);call numberc(0.,-0.3,0.3,real(10-st),0.,-1)
        else if (st>=4 .and. st<=6) then;call gmark(0.,0.,0.2,6);call numberc(0.,-0.3,0.3,real(10-st),0.,-1)
        else if(st>=7 .and. st<=9) then;call gmark(0.,0.,0.2,8);call numberc(0.,-0.3,0.3,real(10-st),0.,-1)
        else; end if
        call plot(1.,0.,-3)
    end do


call plote
end program