program TS_diagram_aomori
    implicit none
    integer::y,m,l,st,d
    real::salco,tempco,axis_temp_range,axis_sal_range,diff,r,g,b
    integer,parameter:: years = 15, months = 12, lines = 2, stations = 9,depth = 400
    real,parameter::temp_min = 0., temp_max = 25., axis_sal_min = 33.0, axis_sal_max = 34.5, standard_sal_450 = 34.07
    real,parameter::width =4., height = 4.
    real,dimension(years,months,lines,stations,depth)::potemp_5,sal_5
    character(len=3),dimension(12)::month_names
    
    if (lines == 1) then
        call plots(1.5,13.5,13,'TS_diagram_yearly_NLine_St1-6.ps')
        call symbol(1.,5.5,1.,'Yearly TS Diagrams at N-Line St1-6', 0.,len('yearly ts diagrams at n line St1-6'))
    else 
        call plots(1.5,13.5,13,'TS_diagram_yearly_SLine_St1-6.ps')
        call symbol(1.,5.5,1.,'Yearly TS Diagrams at S-Line St1-6', 0.,len('yearly ts diagrams at s line St1-6'))
    end if
    call newpen2(3)
    call create_potemp_5(years,months,lines,stations,depth,potemp_5)
    call create_sal_5(years,months,lines,stations,depth,sal_5)
    call month_str_array(month_names)
    axis_temp_range = temp_max - temp_min; axis_sal_range = axis_sal_max - axis_sal_min
    
    sal_5(1,12,2,5,:)=0.;sal_5(1,12,2,6,:)=0.;sal_5(1,12,2,7,:)=0.
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

    do y = 1,years
        call rgbk(0.,0.,0.)
        call TS_diagram(temp_min,temp_max,axis_sal_min,axis_sal_max,100,20.,0.5,width,height,0.,0.)
        call numberc(width/2.,height+0.5,0.3,real(y)+2008.,0.,-1)
        do m = 1,months
            do l = lines,lines
                do st = 4, stations
                    do d = 1, depth
                        salco = width*(sal_5(y,m,l,st,d)-axis_sal_min)/axis_sal_range; tempco = height*(potemp_5(y,m,l,st,d)-temp_min)/axis_temp_range
                        if (1<=m .and. m<=4) then; r = 0.; g = real(m)/4.; b = 1.- real(m)/4.
                            else if(5<=m .and. m<=8) then; r = real(m-4)/4.; g = 1.- real(m-4)/4.; b = 0.
                            else if (9<=m .and. m<=12) then; r = 1.- real(m-8)/4.; g = 0.; b = real(m-8)/4.
                            else;end if
                        if(sal_5(y,m,l,st,d)/=0. .and. potemp_5(y,m,l,st,d)/=0.) then;call rgbk(r,g,b)
                            if (st==1.or.st==2.or.st==3) then; call gmark(salco,tempco,0.05,1)
                            else if(st==4.or.st==5.or.st==6) then; call gmark(salco,tempco,0.05,6)
                            else if(st==7.or.st==8.or.st==9) then; call gmark(salco,tempco,0.05,8)
                            else;end if
                        end if
                    end do
                end do
            end do
        end do
        if (mod(y,5)==0) then;call plot(-4*(width+1.),-height-2.,-3)
        else;call plot(width+1.,0.,-3);end if
    end do

    call plot(0.,height+2.-1.5,-3)
    do m = 1,12
            if (1<=m .and. m<=4) then; r = 0.; g = real(m)/4.; b = 1.- real(m)/4.
        else if(5<=m .and. m<=8) then; r = real(m-4)/4.; g = 1.- real(m-4)/4.; b = 0.
        else if (9<=m .and. m<=12) then; r = 1.- real(m-8)/4.; g = 0.; b = real(m-8)/4.
        else;end if
            call betsqk(0.,0.,0.6,0.6,r,g,b)
            call plot(0.,0.,3);call plot(0.6,0.,2); call plot(0.3,0.,3); call plot(0.3,-0.05,2);call symbolc(0.3,-0.2,0.2,month_names(m),0.,3)
            call plot(0.6,0.,-3)
    end do
    call plot(1.,0.,-3)
    ! call gmark(0.,0.,0.2,1);call symbolc(0.,-.5,0.3,'stations 7-9',0.,len('stations 7-9'))
    ! call plot(2.,0.,-3)
    call gmark(0.,0.,0.2,6);call symbolc(0.,-.5,0.3,'stations 4-6',0.,len('stations 4-6'))
    call plot(2.,0.,-3)
    call gmark(0.,0.,0.2,8);call symbolc(0.,-.5,0.3,'stations 1-3',0.,len('stations 1-3'))

    call plot(3.,-.5,-3)
    call create_map(40,42,137,142,1,9,2.5,0.,0.)


call plote
end program