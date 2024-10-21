program tssigma201
    use constants
    use subroutines
    implicit none
    real,parameter::width = 1.8, height = 24.,width2 = 8.,height2 = 6.,width3 = 2.,height3=3.8,width4=3.,height4=6.
    integer,parameter::ini_st = 4, fin_st = 9, obs_depth = 50
    real,dimension(fin_st-ini_st+1,months*years)::psarray2=0.
    integer,dimension(fin_st-ini_st+1,months*years)::mask2
    real,dimension(years)::testy;real,dimension(fin_st-ini_st+1)::totalol,slol
    real,dimension(0:180)::plotx=0.,ploty=0.
    character(len=5),dimension(fin_st-ini_st+1,180)::interpolated = 'false'
    real::rnum,dxval,s,plot_s,totalmean; integer::count1
    character(len=1)::ststring
  
    call plots(2.5,0.5,9,'../Plots/TempSalSigma/Timeseries/sal50m.ps')
    call calibrated_data51(potemp_c5,sal_c5)
    call create_sigma_array(potemp_c5,sal_c5,sigma_c5)
    call geovel_array(51,geovel_5)
  
    ! do j = 1,obs_depth,50
        j = obs_depth; l = 1
        call create_box(width,height,3)
        call numberr(width/2.,height+0.4,0.5,real(j),0.,-1);call symbol(width/2.,height+0.4,0.5,'m',0.,len('m'))
        dx = width/real(fin_st-ini_st+1);dy = height/(real(months*years))
        call mod12_memori(months*years,0.15,-90,height,1,2)
        call st_memori(1,6,width,1,0.4,2)
        call floating_numbers(2023.,-1.,15,0.6,0.,dy*12.,0.,-1,-1.5,dy*5.)
        call symbolr(-0.3,-0.5,0.4,'St',0.,len('St'))
        call plot(0.,height,-3)
  
        do st = 1,fin_st-ini_st+1
            i = 1
            do y = 1, years
                do m = 1, months
                    !temp
                    ! psarray2(st,i) = potemp_c5(y,m,l,st+ini_st-1,j)
                    !sigma
                    ! psarray2(st,i) = sigma_c5(y,m,l,st+ini_st-1,j)
                    !sal
                    psarray2(st,i) = sal_c5(y,m,l,st+ini_st-1,j)
  
                        if(psarray2(st,i) == 0.) then;mask2(st,i) = 0;else;mask2(st,i) = 1;endif
                    i = i + 1
                end do 
            end do
        end do        
        ! print*,minval(psarray2),maxval(psarray2)
        
    call butler_psbet(psarray2,fin_st-ini_st+1,months*years,width,-height,0.,33.4,34.6,0.05,'b2r',24,bpt1=12,conti = 33.4, continc = 0.2,r= r1,g=g1,b=b1)
    call rgbk(0.,0.,0.)
  
    ! call colorscale(24,r1,g1,b1,33.4,34.6,2,0.4,2,10.,.3,lessthan=1,morethan=1,rangle=90.,x=width+1.5,y=height-7.)
        ! call plot(1.,0.,-3)
    
    call floating_lines(7.*width+6.*0.7+2.,0.,15,3,0.,-dy*12.,x = -2.,y=-dy/2.)
    call plot(width,-height,-3)
    do st = 1,6
            write(ststring,'(i1.1)')7-st
        call plot(0.7,0.,-3)
        call create_box(width,height,3)
        call mod12_memori(months*years,0.15,-90,height,1,2)
        call num_memori(33.4,34.6,12,6,0.3,1,-width,0,x=width)
        call symbolc(width/2.,height+0.4,0.5,'st'//trim(ststring),0.,len('st'//trim(ststring)))
        call plot(0.,height,-3)
        
        ! linear interpolation for years 2010 and onwards excluding 2012 of course
        do i = 12+1,180
            if(i>=12*3+1.and.i<=12*4+1)cycle
            if(psarray2(st,i)==0.) then
                if(psarray2(st,i-1)/=0. .and. psarray2(st,i+1)/=0.) then
                    psarray2(st,i) = (psarray2(st,i-1)+psarray2(st,i+1))/2.;interpolated(st,i) = 'TRUE'
                else if (psarray2(st,i-1)/=0. .and. psarray2(st,i+1)==0.) then
                    psarray2(st,i) = psarray2(st,i-1)+(psarray2(st,i+2)-psarray2(st,i-1))/3.
                    psarray2(st,i+1) = psarray2(st,i-1)+(psarray2(st,i+2)-psarray2(st,i-1))*2./3.
                    interpolated(st,i)='TRUE';interpolated(st,i+1) = 'TRUE'
                end if
            end if
        end do
        ! every single data point 
        do i = 1,180
            call gmark_ratio(psarray2(st,i),33.4,34.6,width,plotx(i))
            ! print*,psarray2(st,i),plotx(i)
            ploty(i) = -dy/2. - dy*real(i-1)
            if(interpolated(st,i)=='TRUE') then;call rgbk(1.,0.,0.);else;call rgbk(0.,0.,0.);end if
            call gmark(width-plotx(i),ploty(i),0.1,1)
            call rgbk(0.,0.,0.)     
            if(i>1) then
                if(psarray2(st,i-1)/=0. .and. psarray2(st,i)/=0.) then
                call plot(width-plotx(i-1),ploty(i-1),3);call plot(width-plotx(i),ploty(i),2)
                end if
            else;end if
                ! call rgbk(0.,0.,0.)  
        end do
        ! total mean
            call avsemloop_1D(psarray2(st,13:180),168,168,1,mean_1D=totalol(st),s_1D=slol(st))
            totalmean = totalol(st)
            s = slol(st)
            ! print*,totalmean,s
            call rgbk(0.,0.,1.)
            call newpen2(4)
            call gmark_ratio(totalmean,33.4,34.6,width,rnum)
            call gmark_ratio(s,0.,1.2,width,plot_s)
            ! print*,plot_s
            call plot(width-rnum,-dy/2.-12.*dy,3);call plot(width-rnum,-dy/2.-12.*dy*real(years),2)
            call rgbk(0.4,0.4,0.4);call newpen2(3);call newpen2(-2)
            call plot(width-rnum-plot_s,-12.*dy-dy/2.,3);call plot(width-rnum-plot_s,-dy/2.-12.*dy*real(years),2)
            call plot(width-rnum+plot_s,-12.*dy-dy/2.,3);call plot(width-rnum+plot_s,-dy/2.-12.*dy*real(years),2)
            call rgbk(0.,0.,0.)
        !   
        ! yearly means
            call avsemloop_1D(psarray2(st,1:180),180,12,15,mean_1D=testy)
            call newpen2(3);call newpen2(-6)
            do y = 1, years
                call gmark_ratio(testy(y),33.4,34.6,width,rnum)
                call plot(width-rnum,-dy/2.-12.*dy*real(y-1),3);call plot(width-rnum,-dy/2.-12.*dy*real(y),2)
            end do
    call plot(width,-height,-3)
end do

call newpage
call plot(1.,10.,-3)
call colorscale(24,r1,g1,b1,33.4,34.6,2,0.4,2,10.,.3,lessthan=1,morethan=1,rangle=90.)
call plot(5.,0.,-3)
call create_map(40,46,137,142,1,6,1,8.,0.4)

  
    call plote
  end program