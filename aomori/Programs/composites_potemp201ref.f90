program tssigma201
    use always
    implicit none
    real,parameter::width = 2., height = 24.,width2 = 10.5,height2 = 7.,width3 = 2.,height3=3.8,width4=3.,height4=6.,width5=2.1,width6 = 3.,height6 = 6.
    integer,parameter::ini_st = 4, fin_st = 9, obs_depth = 201
    real,dimension(years,months,lines,stations,depth)::geovel_5
    real,dimension(2,fin_st-ini_st+1,months*years)::psarray2=0.
    real,dimension(155)::N_st4,S_st4
    real,dimension(-6:6)::coeffs
    integer,dimension(fin_st-ini_st+1,months*years)::mask2
    real,dimension(1)::totalol,slol
    real,dimension(0:180)::plotx=0.,ploty=0.
    character(len=5),dimension(2,fin_st-ini_st+1,180)::interpolated = 'false'
    real::rnum,dxval,plot_s,totalmean,gmarksize; integer::count1,lag,h,z
    real,dimension(4,15,12,fin_st-ini_st+1,depth)::pstemp=0. !1=above,2=below,3=2018and2019, 4=below(excluding 2018and2019)
    real,dimension(4,15,12,fin_st-ini_st+1,depth)::pssal=0.
    real,dimension(4,15,12,fin_st-ini_st+1,depth)::psden=0.
    real,dimension(4,15,12,fin_st-ini_st,depth)::psvel=0.
    integer,dimension(12,12)::log=0
    real,dimension(:,:,:),allocatable::mmean
    real,dimension(:,:),allocatable::ymmeanabove,ymmeanbelow,ymmeandiffd,ymmeandiffv,ymmean1819,ymmeanex1819,ymmean1,ymmean2
    real,dimension(:,:),allocatable::ymsemabove,ymsembelow,ymsem1,ymsem2
    integer,dimension(:,:),allocatable::ymdataabove,ymdatabelow,ymdata1,ymdata2
    real,dimension(:),allocatable::rsal,gsal,bsal,r1,b1,g1,r,g,b
    integer,dimension(6,depth)::resultd
    real::dx,dy,s,a,c

  
    ! call plots(0.,0.,9,'../Plots/Favorites/composites_potemp201refX2.ps')
    call plots2(nnfile = 'composites_test',mode = 'portrait')
    call plotmove(0.5,0.95);call symbolc(0.,0.,0.8,'composite graphs of times with offshore warm water',0.)
    call plotmove(0.,0.)
    call plot(3.,3.,-3);call plotsave('first')
    call calibrated_data51(potemp_c5,sal_c5)
    call create_sigma_array(potemp_c5,sal_c5,sigma_c5)
    call geovel_array(51,geovel_5)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                            ! Station 4 Examination
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! do j = 1,obs_depth,50
            j = obs_depth; 
            call box(width,height,3)
            call numberr(width/2.,height+0.4,0.5,real(j),0.,-1);call symbol(width/2.,height+0.4,0.5,'m',0.)
            dx = width/real(fin_st-ini_st+1);dy = height/(real(months*years))
            call mod12_memori(months*years,symbol_size = 0.25,angle = -90,length = -height,gap = 2,num_freq=2,y=height)
            call st_memori(1,6,width,1,0.5,2)
            call floating_numbers(2023.,-1.,15,0.6,0.,dy*12.,0.,-1,-1.5,dy*6.)
            call symbolr(-0.3,-0.7,0.5,'St',0.)
            call plot(0.,height,-3)
        do l = 1,2
            do st = 1,fin_st-ini_st+1
                i = 1
                do y = 1, years
                    do m = 1, months
                        !temp
                        psarray2(l,st,i) = potemp_c5(y,m,l,st+ini_st-1,j)
                            if(psarray2(l,st,i) == 0.) then;mask2(st,i) = 0;else;mask2(st,i) = 1;endif
                        i = i + 1
                    end do 
                end do
            end do    
        end do    
        !temp of N line
        call butler_psbet(psarray2(1,1:fin_st-ini_st+1,1:180),width,-height,0.,0.,12.,1.,'b2r',12,bpt1=5,conti = 0., continc = 1.,r= r1,g=g1,b=b1,thicc=5)
        call colorscale(12,r1,g1,b1,0.,12.,5,0.5,1,width*2.,0.3,lt=1,gt=1,x=width+0.25,y=-height-1.5)
        call rgbk(0.,0.,0.)
    
    call plot(width+0.5,0.,-3)
        ! Station 4 examination
        st = 6-3 !wakarizuraine trust me bro
        call box(width5,-height,3)
        call memori(180,0.1,0,-height,-90.,gap=2,y=-height/2.)
        call num_memori(0.,12.,13,5,0.5,1,-width5,0,x=width5,y=-height)
        call symbolc(width5/2.,0.4,0.5,'st4',0.)
        call floating_lines(width+width5+2.*dx+0.5,0.,15,3,0.,-dy*12.,-(width+2.*dx+0.5),-dy/2.)
        
        do l = 1,2
            ! linear interpolation for years 2010 and onwards excluding 2012 of course
            do i = 12+1,180
                if(i>=12*3+1.and.i<=12*4+1)cycle
                if(psarray2(l,st,i)==0.) then
                    if(psarray2(l,st,i-1)/=0. .and. psarray2(l,st,i+1)/=0.) then
                        psarray2(l,st,i) = (psarray2(l,st,i-1)+psarray2(l,st,i+1))/2.;interpolated(l,st,i) = 'TRUE'
                    else if (psarray2(l,st,i-1)/=0. .and. psarray2(l,st,i+1)==0.) then
                        psarray2(l,st,i) = psarray2(l,st,i-1)+(psarray2(l,st,i+2)-psarray2(l,st,i-1))/3.
                        psarray2(l,st,i+1) = psarray2(l,st,i-1)+(psarray2(l,st,i+2)-psarray2(l,st,i-1))*2./3.
                        interpolated(l,st,i)='TRUE';interpolated(l,st,i+1) = 'TRUE'
                    end if
                end if
            end do
        end do
            psarray2(2,3,13) = psarray2(2,3,11)+(psarray2(2,3,14)-psarray2(2,3,11))*2./3.;interpolated(2,3,13) = 'TRUE'
        ! every single data point 
        do l = 1,2
                if(l==2)then;call rgbk(1.,0.,0.);gmarksize=0.1;else;call rgbk(0.,0.,0.);gmarksize=0.13;end if
            do i = 1,180
                call gmark_ratio(psarray2(l,st,i),0.,12.,width5,plotx(i))
                ! print*,psarray2(l,st,i),plotx(i)
                ploty(i) = -dy/2. - dy*real(i-1)
                if(psarray2(l,st,i)==0.)cycle
                if(interpolated(l,st,i)=='TRUE') then
                    call gmark(width5-plotx(i),ploty(i),gmarksize,4)
                else;call gmark(width5-plotx(i),ploty(i),gmarksize,1);end if      
                if(i>1) then
                    if(psarray2(l,st,i-1)/=0. .and. psarray2(l,st,i)/=0.) then
                        call newpen2(3)
                    call plot(width5-plotx(i-1),ploty(i-1),3);call plot(width5-plotx(i),ploty(i),2)
                    end if
                else;end if
                    ! call rgbk(0.,0.,0.)  
            end do
            ! total mean
            if(l==1)then
                call avsemloop_1D(psarray2(l,st,13:180),168,168,1,mean_1D=totalol(l),s_1D=slol(l))
                totalmean = totalol(l)
                s = slol(l)
                ! print*,totalmean,s
                if(l==1)call rgbk(0.,0.,0.)
                if(l==2)call rgbk(1.,0.,0.)
                call newpen2(4)
                call gmark_ratio(totalmean,0.,12.,width5,rnum)
                call gmark_ratio(s,0.,12.,width5,plot_s)
                call plot(width5-rnum,-dy/2.-12.*dy,3);call plot(width5-rnum,-dy/2.-12.*dy*real(years),2)
                call newpen2(4);call newpen2(-6)
                call plot(width5-rnum-plot_s,-12.*dy-dy/2.,3);call plot(width5-rnum-plot_s,-dy/2.-12.*dy*real(years),2)
                call plot(width5-rnum+plot_s,-12.*dy-dy/2.,3);call plot(width5-rnum+plot_s,-dy/2.-12.*dy*real(years),2)
                call newpen2(3)
            end if
            !   
            ! yearly means
                ! call avsemloop_1D(psarray2(l,st,1:180),180,12,15,mean_1D=testy)
                ! call newpen2(3);call newpen2(-6)
                ! do y = 1, years
                !     call gmark_ratio(testy(y),0.,12.,width5,rnum)
                !     call plot(width5-rnum,-dy/2.-12.*dy*real(y-1),3);call plot(width5-rnum,-dy/2.-12.*dy*real(y),2)
                ! end do
        end do
        call plot(width5+0.7,-height2,-3)

        ! USAGE OF PSARRAY2 ENDS HERE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                        ! evaluating times with and without offshore warm water 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
        totalmean = totalol(1);s = slol(1)
        print*,'totalmean=',totalol(1),'s=',slol(1)
        dxval = width2/13.
        !above
        do i = 1, 15
        do m=1,12
            if(potemp_c5(i,m,1,6,201)>=totalmean+s)then
                print*,'warm',i+2008,m,potemp_c5(i,m,1,6,201)
            pstemp(1,i,m,1:6,1:depth) = potemp_c5(i,m,1,4:9,1:depth)
            pssal(1,i,m,1:6,1:depth) = sal_c5(i,m,1,4:9,1:depth)
            psden(1,i,m,1:6,1:depth) = sigma_c5(i,m,1,4:9,1:depth)
            psvel(1,i,m,1:5,1:depth) = geovel_5(i,m,1,5:9,1:depth)
            else;pstemp(1,i,m,1:6,1:depth)=0.0
                pssal(1,i,m,1:6,1:depth)=0.0
                psden(1,i,m,1:6,1:depth)=0.0
                psvel(1,i,m,1:5,1:depth)=0.0
            end if
        end do
        end do
            
        !below 
        do i = 1,15
        do m = 1, 12
            if (potemp_c5(i,m,1,6,201)<totalmean)then
                print*,'cold',i+2008,m,potemp_c5(i,m,1,6,201)
            pstemp(2,i,m,1:6,1:depth) = potemp_c5(i,m,1,4:9,1:depth)
            pssal(2,i,m,1:6,1:depth) = sal_c5(i,m,1,4:9,1:depth)
            psden(2,i,m,1:6,1:depth) = sigma_c5(i,m,1,4:9,1:depth)
            psvel(2,i,m,1:5,1:depth) = geovel_5(i,m,1,5:9,1:depth)
            else;pstemp(2,i,m,1:6,1:depth)=0.0
                pssal(2,i,m,1:6,1:depth)=0.0
                psden(2,i,m,1:6,1:depth)=0.0
                psvel(2,i,m,1:5,1:depth)=0.0
            end if     
        end do
            ! print*,pstemp(4,i,1:months,st,201)
        end do

        ! 2018 and 2019
            pstemp(3,10,1:12,1:6,1:depth) = pstemp(2,10,1:12,1:6,1:depth)
            pstemp(3,11,1:12,1:6,1:depth) = pstemp(2,11,1:12,1:6,1:depth)
            pssal(3,10,1:12,1:6,1:depth) = pssal(2,10,1:12,1:6,1:depth)
            pssal(3,11,1:12,1:6,1:depth) = pssal(2,11,1:12,1:6,1:depth)
            psden(3,10,1:12,1:6,1:depth) = psden(2,10,1:12,1:6,1:depth)
            psden(3,11,1:12,1:6,1:depth) = psden(2,11,1:12,1:6,1:depth)
            psvel(3,10,1:12,1:5,1:depth) = psvel(2,10,1:12,1:5,1:depth)
            psvel(3,11,1:12,1:5,1:depth) = psvel(2,11,1:12,1:5,1:depth)
        !
        ! below excluding 20182019
            pstemp(4,1:15,1:12,1:6,1:depth) = pstemp(2,1:15,1:12,1:6,1:depth) - pstemp(3,1:15,1:12,1:6,1:depth)
            pssal(4,1:15,1:12,1:6,1:depth) = pssal(2,1:15,1:12,1:6,1:depth) - pssal(3,1:15,1:12,1:6,1:depth)
            psden(4,1:15,1:12,1:6,1:depth) = psden(2,1:15,1:12,1:6,1:depth) - psden(3,1:15,1:12,1:6,1:depth)
            psvel(4,1:15,1:12,1:5,1:depth) = psvel(2,1:15,1:12,1:5,1:depth) - psvel(3,1:15,1:12,1:5,1:depth)
        !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                            ! creating frequency tables for times with and without offshore warm water
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!        
        call plot(1.,-.5,-3)
        call rgbk(0.,0.,0.);call newpen2(3)
        call plot(0.,height2/2.,3);call plot(width2,height2/2.,2);call plot(0.,0.,3);call plot(0.,height2,2)
        call num_memori(0.,12.,13,5,0.5,1,-height2/2.,-90,y=height2/2.);call num_memori(0.,12.,13,5,0.5,1,height2/2.,-90,y=height2/2.)
        call floating_numbers(1.,1.,12,0.5,dxval,0.,0.,-1,x=dxval,y = -0.4);call floating_numbers(1.,1.,12,0.5,dxval,0.,0.,-1,x=dxval,y = height2+0.2)
        call plot(0.,height2/2.,-3);call gmark_ratio(totalmean,0.,12.,height2/2.,rnum);call gmark_ratio(s,0.,12.,height2/2.,plot_s)
        call newpen2(5);call rgbk(0.,0.,0.);call plot(0.,rnum,3);call plot(width2,rnum,2);call plot(0.,-rnum,3);call plot(width2,-rnum,2)
        call rgbk(0.,0.,0.);call newpen2(-6)
        call plot(0.,rnum-plot_s,3);call plot(width2,rnum-plot_s,2);call plot(0.,rnum+plot_s,3);call plot(width2,rnum+plot_s,2)
        call plot(0.,-rnum-plot_s,3);call plot(width2,-rnum-plot_s,2);call plot(0.,-rnum+plot_s,3);call plot(width2,-rnum+plot_s,2)
        call newpen2(3)
    !above
        h=0
        do i = 1,15
            do m = 1,12
                if(pstemp(1,i,m,3,201)==0.)cycle
                do z = 1,12
                    if(real(z-1)<=pstemp(1,i,m,3,201).and.pstemp(1,i,m,3,201)<real(z))then
                        dy = real(z-1)*height2/24.
                        count1=log(m,z)
                        dx = real(count1)*dxval/10.
                        ! print*,y+2008,m,(y-1)*12+m,h,count1
                        log(m,z)=log(m,z)+1
                        h = h+1;exit
                    else;end if
                end do
                call betsqk(dxval/2.+dxval*real(m-1)+dx,dy,dxval/2.+dxval*real(m-1)+dx+dxval/12.,dy+height2/25.,0.,0.,0.)
            end do
        end do
        call symbol(width2+0.2,height2/2.,0.6,'n',0.)
        call number(width2+0.1,height2/4.,0.6,real(sum(log)),0.,-1)
    !
    !below
        h = 1;log=0
        do i = 1,15
                do m = 1,12
                    if(pstemp(2,i,m,3,201)==0.)cycle
                    do z = 1,12
                        if(real(z-1)<=pstemp(2,i,m,3,201).and.pstemp(2,i,m,3,201)<real(z))then
                            dy = -1.*real(z-1)*height2/24.
                            count1=log(m,z)
                            dx = real(count1)*dxval/10.
                            ! print*,y+2008,m,(y-1)*12+m,h,count1
                            log(m,z)=log(m,z)+1
                            h = h+1;exit
                        else;end if
                    end do
                call betsqk(dxval/2.+dxval*real(m-1)+dx,dy,dxval/2.+dxval*real(m-1)+dx+dxval/12.,dy-height2/25.,0.,0.,0.)
            end do
        end do
        call number(width2+0.1,-height2/8.,0.6,real(sum(log)),0.,-1)
    ! 
    
        call floating_lines(height2,90.,13,2,dxval,0.,x=dxval/2.,y=-height2/2.);call floating_lines(width2,0.,5,2,0.,height2/24.*5.,y=height2/24.*2-height2/2.)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                            ! creating xz cross sectional plots for ps arrays + t tests
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!     

    !above standard deviation
        ! PS
            call plot(1.5,-height3-2.,-3);call plotsave('PS')
            call symbolc(width3/2.,0.3,0.5,'PT,S',0.);call symbolr(-1.5,-height3/2.,1.,'A',0.)
            call num_memori(0.,400.,40,10,0.4,-1,-height3,-90)
            call memori(6,0.125,0,width3,0.,y = -height3,x=width3/2.)
            call avsemdata_4D(pssal(1,1:15,1:months,1:6,1:depth),'dim2',mmean) !sal first 
            call avsemdata_3D(mmean(1:15,1:6,1:depth),'dim1',ymmeanabove)
            call butler_psk(ymmeanabove,width3,-height3,0.,33.95,34.30,0.05,'b2w2r',7,bpt1=4,r=rsal,g=gsal,b=bsal)!,conti=33.8,continc=0.05,r=rsal,g=gsal,b=bsal,thicc=2
            call colorscale(7,rsal,gsal,bsal,33.95,34.3,2,0.5,2,height3*2.-2.,0.3,lt=1,gt=1,rangle=90.,symbol_start = 2,x = 3.*(width3+0.5)+0.5,y=-height3-0.25)
            print*,maxval(ymmeanabove),minval(ymmeanabove),'above, sal'

            call avsemdata_4D(pstemp(1,1:15,1:months,1:6,1:depth),'dim2',mmean)
            call avsemdata_3D(mmean(1:15,1:6,1:depth),'dim1',ymmeanabove)
            call butler_cont(ymmeanabove,width3,-height3,0.,0.,1.,thicc=5)
            print*,maxval(ymmeanabove),minval(ymmeanabove),'above, temp'
        !density
            call plot(width3+.5,0.,-3)
            call symbolc(width3/2.,0.3,0.5,'Density',0.)
            call memori(40,0.1,10,-height3,-90.,y=-height3/2.)
            call memori(6,0.125,0,width3,0.,y = -height3,x=width3/2.)
            call avsemdata_4D(psden(1,1:15,1:months,1:6,1:depth),'dim2',mmean)
            call avsemdata_3D(mmean(1:15,1:6,1:depth),'dim1',ymmeanabove)
            call butler_cont(ymmeanabove,width3,-height3,0.,20.,0.2,thicc=5)
            print*,maxval(ymmeanabove),minval(ymmeanabove),'above, sigma'
        !velocity
            call plot(width3+.5,0.,-3)
            call symbolc(width3/2.,0.6,0.4,'Geostrophic;Velocity',0.)
            call memori(40,0.1,10,-height3,-90.,y=-height3/2.)
            call memori(6,0.125,0,width3,0.,y = -height3,x=width3/2.)
            call avsemdata_4D(psvel(1,1:15,1:months,1:5,1:depth),'dim2',mmean)
            call avsemdata_3D(mmean(1:15,1:5,1:depth),'dim1',mean_2D=ymmeanabove,sem_2D=ymsemabove,dataquan_2D=ymdataabove)
            call butler_psmask(ymmeanabove,width3,-height3,-40.,0.,r=0.8,g=0.8,b=0.8,gap =1)
            call butler_cont(ymmeanabove,width3,-height3,0.,-40.,2.,thicc=5,gap=1)
            print*,maxval(ymmeanabove),minval(ymmeanabove),'above, vel'
    call plotback('PS')
    call plot(0.,-height3-.5,-3)
    ! below standard deviation
        ! PS
            call symbolc(-2.,-height3/2.,1.,'B',0.)
            call num_memori(0.,400.,40,10,0.4,-1,-height3,-90)
            call st_memori(1,6,width3,1,0.4,0,y=-height3)
            call avsemdata_4D(pssal(2,1:15,1:months,1:6,1:depth),'dim2',mmean)
            call avsemdata_3D(mmean(1:15,1:6,1:depth),'dim1',ymmeanbelow)
            call butler_psk(ymmeanbelow,width3,-height3,0.,33.95,34.30,0.05,'b2w2r',7,bpt1=4,r=rsal,g=gsal,b=bsal)!,conti=33.8,continc=0.05,r=rsal,g=gsal,b=bsal,thicc=2
            print*,maxval(ymmeanbelow),minval(ymmeanbelow),'below, sal'

            call avsemdata_4D(pstemp(2,1:15,1:months,1:6,1:depth),'dim2',mmean)
            call avsemdata_3D(mmean(1:15,1:6,1:depth),'dim1',ymmeanbelow)
            call butler_cont(ymmeanbelow,width3,-height3,0.,0.,1.,thicc=5,r=0.,g=0.,b=0.)
            print*,maxval(ymmeanbelow),minval(ymmeanbelow),'below, temp'

        !density  
            call plot(width3+.5,0.,-3)
            call memori(40,0.1,10,-height3,-90.,y=-height3/2.)
            call st_memori(1,6,width3,1,0.4,0,y=-height3)
            call avsemdata_4D(psden(2,1:15,1:months,1:6,1:depth),'dim2',mmean)
            call avsemdata_3D(mmean(1:15,1:6,1:depth),'dim1',ymmeanbelow)
            call butler_cont(ymmeanbelow,width3,-height3,0.,20.,0.2,thicc=5)
            print*,maxval(ymmeanbelow),minval(ymmeanbelow),'below, sigma'
        !velocity
            call plot(width3+.5,0.,-3)
            call memori(40,0.1,10,-height3,-90.,y=-height3/2.)
            call st_memori(1,6,width3,1,0.4,0,y=-height3)        
            call avsemdata_4D(psvel(2,1:15,1:months,1:5,1:depth),'dim2',mmean)
            call avsemdata_3D(mmean(1:15,1:5,1:depth),'dim1',mean_2D=ymmeanbelow,sem_2D=ymsembelow,dataquan_2D=ymdatabelow)
            call butler_psmask(ymmeanbelow,width3,-height3,-40.,0.,r=0.8,g=0.8,b=0.8,gap =1)
            call butler_cont(ymmeanbelow,width3,-height3,0.,-40.,2.,thicc=5,gap=1)
            print*,maxval(ymmeanbelow),minval(ymmeanbelow),'below, vel'

    call plotback('PS');call plot(0.,2*(-height3-1.)-0.5,-3)
    !above sd - below sd and tests
        !density diff
            call symbolc(-2.,-height3/2.,1.,'A-B',0.)
            call avsemdata_4D(psden(1,1:15,1:months,1:6,1:depth),'dim2',mmean)
            call avsemdata_3D(mmean(1:15,1:6,1:depth),'dim1',mean_2D=ymmean1,s_2D=ymsem1,dataquan_2D=ymdata1) !above                     !sem array corrected to s array name mistake
            call avsemdata_4D(psden(2,1:15,1:months,1:6,1:depth),'dim2',mmean)
            call avsemdata_3D(mmean(1:15,1:6,1:depth),'dim1',mean_2D=ymmean2,s_2D=ymsem2,dataquan_2D=ymdata2) !below
            ymmeandiffd = ymmean1-ymmean2
            print*,minval(ymmeandiffd),maxval(ymmeandiffd),minloc(ymmeandiffd),maxloc(ymmeandiffd),'above-below,density'
            call symbolc(width3/2.,0.2,0.5,'Density',0.)
            call num_memori(0.,400.,40,10,0.4,-1,-height3,-90)
            call st_memori(1,6,width3,1,0.4,0,y=-height3)
            call butler_psmask(ymmeandiffd,width3,-height3,-2.,0.,r=0.8,g=0.8,b=0.8)
            call butler_cont(ymmeandiffd,width3,-height3,0.,-1.,0.2,5)
            do n = 1,6
                do i = 1,depth
                    resultd(n,i) = fwelcht(ymmean1(n,i),ymsem1(n,i),ymdata1(n,i),ymmean2(n,i),ymsem2(n,i),ymdata2(n,i))
                end do
            end do
        ! t test for density
        call plot(width3+0.5,0.,-3)
            call symbolc(width3/2.,0.6,0.4,'Density;T-test',0.)
            call memori(40,0.1,10,-height3,-90.,y=-height3/2.);call st_memori(1,6,width3,1,0.4,0,y=-height3)
            call butler_imask(resultd,width3,-height3,0)
            call butler_imask(resultd,width3,-height3,1,r=1.,g=0.5,b=0.5)
            call butler_imask(resultd,width3,-height3,-1,r=0.5,g=0.5,b=1.)
        call plot(width3+0.5,0.,-3)
        !geovel diff
            call avsemdata_4D(psvel(1,1:15,1:months,1:5,1:depth),'dim2',mmean)
            call avsemdata_3D(mmean(1:15,1:5,1:depth),'dim1',mean_2D=ymmean1,sem_2D=ymsem1,dataquan_2D=ymdata1) !above
            call avsemdata_4D(psvel(2,1:15,1:months,1:5,1:depth),'dim2',mmean)
            call avsemdata_3D(mmean(1:15,1:5,1:depth),'dim1',mean_2D=ymmean2,sem_2D=ymsem2,dataquan_2D=ymdata2) !below
            ymmeandiffv = ymmean1-ymmean2
            print*,minval(ymmeandiffv),maxval(ymmeandiffv),minloc(ymmeandiffv),maxloc(ymmeandiffv),'above-below,vel'
            call symbolc(width3/2.,0.6,0.4,'Geostrophic;Velocity',0.)
            call memori(40,0.1,10,-height3,-90.,y=-height3/2.)
            call st_memori(1,6,width3,1,0.4,0,y=-height3)
            call butler_psmask(ymmeandiffv,width3,-height3,-40.,0.,r=0.8,g=0.8,b=0.8,gap =1)
            call butler_cont(ymmeandiffv,width3,-height3,0.,-40.,2.,thicc=5,r=0.,g=0.,b=0.,gap=1)
        ! end first page

    ! 2018 2019 
    call newpage;call plotmove2(-1.+width3+5,24.);call plotsave('2nd page')
    ! PS
        call symbolc(width3/2.,0.3,0.5,'PT,S',0.);call symbolc(-2.,-height3/2.,1.,'C',0.)
        call num_memori(0.,400.,40,10,0.4,-1,-height3,-90)
        call memori(6,0.125,0,width3,0.,y = -height3,x=width3/2.)
        call avsemdata_4D(pssal(3,1:15,1:months,1:6,1:depth),'dim2',mmean)
        call avsemdata_3D(mmean(1:15,1:6,1:depth),'dim1',ymmean1819)
        call butler_psk(ymmean1819,width3,-height3,0.,33.95,34.30,0.05,'b2w2r',7,bpt1=4)
        print*,maxval(ymmean1819),minval(ymmean1819),'1819, sal'
        call avsemdata_4D(pstemp(3,1:15,1:months,1:6,1:depth),'dim2',mmean)
        call avsemdata_3D(mmean(1:15,1:6,1:depth),'dim1',ymmean1819)
        call butler_cont(ymmean1819,width3,-height3,0.,0.,1.,thicc=5,r=0.,g=0.,b=0.)
        print*,maxval(ymmean1819),minval(ymmean1819),'1819, temp'
    !density
        call plot(width3+.5,0.,-3)
        call symbolc(width3/2.,0.3,0.5,'Density',0.)
        call memori(40,0.1,10,-height3,-90.,y=-height3/2.)
        call memori(6,0.125,0,width3,0.,y = -height3,x=width3/2.)
        call avsemdata_4D(psden(3,1:15,1:months,1:6,1:depth),'dim2',mmean)
        call avsemdata_3D(mmean(1:15,1:6,1:depth),'dim1',ymmean1819)
        call butler_cont(ymmean1819,width3,-height3,0.,24.0,0.2,thicc=5)
        print*,maxval(ymmean1819),minval(ymmean1819),'1819, sigma'
        call plot(width3+.5,0.,-3)
    !velocity
        call symbolc(width3/2.,0.6,0.4,'Geostrophic;Velocity',0.)
        call memori(40,0.1,10,-height3,-90.,y=-height3/2.)
        call memori(6,0.125,0,width3,0.,y = -height3,x=width3/2.)
        call avsemdata_4D(psvel(3,1:15,1:months,1:5,1:depth),'dim2',mmean)
        call avsemdata_3D(mmean(1:15,1:5,1:depth),'dim1',mean_2D=ymmean1819,sem_2D=ymsemabove,dataquan_2D=ymdataabove)
        call butler_psmask(ymmean1819,width3,-height3,-40.,0.,r=0.8,g=0.8,b=0.8,gap =1)
        call butler_cont(ymmean1819,width3,-height3,0.,-40.,2.,thicc=5,gap=1)
        print*,maxval(ymmean1819),minval(ymmean1819),'1819, vel'

    call plotback('2nd page');call plot(0.,-height3-.5,-3)  
    ! ex 2018 2019 
    !PS
        call num_memori(0.,400.,40,10,0.4,-1,-height3,-90);call symbolc(-2.,-height3/2.,1.,'D',0.)
        call st_memori(1,6,width3,1,0.4,0,y=-height3)
        call avsemdata_4D(pssal(4,1:15,1:months,1:6,1:depth),'dim2',mmean)
        call avsemdata_3D(mmean(1:15,1:6,1:depth),'dim1',ymmeanex1819)
        call butler_psk(ymmeanex1819,width3,-height3,0.,33.95,34.30,0.05,'b2w2r',7,bpt1=4)!,conti=33.8,continc=0.05,r=rsal,g=gsal,b=bsal,thicc=2
        print*,maxval(ymmeanex1819),minval(ymmeanex1819),'ex1819, sal'
        call avsemdata_4D(pstemp(4,1:15,1:months,1:6,1:depth),'dim2',mmean)
        call avsemdata_3D(mmean(1:15,1:6,1:depth),'dim1',ymmeanex1819)
        call butler_cont(ymmeanex1819,width3,-height3,0.,0.,1.,thicc=5)
        print*,maxval(ymmeanex1819),minval(ymmeanex1819),'ex1819, temp'
    !density
        call plot(width3+.5,0.,-3)
        call memori(40,0.1,10,-height3,-90.,y=-height3/2.)
        call st_memori(1,6,width3,1,0.4,0,y=-height3)
        call avsemdata_4D(psden(4,1:15,1:months,1:6,1:depth),'dim2',mmean)
        call avsemdata_3D(mmean(1:15,1:6,1:depth),'dim1',ymmeanex1819)
        call butler_cont(ymmeanex1819,width3,-height3,0.,24.0,0.2,thicc=5)
        print*,maxval(ymmeanex1819),minval(ymmeanex1819),'ex1819, sigma'

    !velocity
        call plot(width3+.5,0.,-3)
        call memori(40,0.1,10,-height3,-90.,y=-height3/2.)
        call st_memori(1,6,width3,1,0.4,0,y=-height3)
        call avsemdata_4D(psvel(4,1:15,1:months,1:5,1:depth),'dim2',mmean)
        call avsemdata_3D(mmean(1:15,1:5,1:depth),'dim1',mean_2D=ymmeanex1819,sem_2D=ymsemabove,dataquan_2D=ymdataabove)
        call butler_psmask(ymmeanex1819,width3,-height3,-40.,0.,r=0.8,g=0.8,b=0.8,gap =1)
        call butler_cont(ymmeanex1819,width3,-height3,0.,-40.,2.,thicc=5,r=0.,g=0.,b=0.,gap=1)
        print*,maxval(ymmeanex1819),minval(ymmeanex1819),'ex1819, vel'
    !
    ! 1819 -ex1819 
    call plotback('2nd page');call plot(0.,2.*(-height3-1.)-1.,-3)

    !density diff
        call symbolc(-2.,-height3/2.,1.,'C-D',0.)
        call avsemdata_4D(psden(3,1:15,1:months,1:6,1:depth),'dim2',mmean)
        call avsemdata_3D(mmean(1:15,1:6,1:depth),'dim1',mean_2D=ymmean1,s_2D=ymsem1,dataquan_2D=ymdata1) 
        call avsemdata_4D(psden(4,1:15,1:months,1:6,1:depth),'dim2',mmean)
        call avsemdata_3D(mmean(1:15,1:6,1:depth),'dim1',mean_2D=ymmean2,s_2D=ymsem2,dataquan_2D=ymdata2) 
        ymmeandiffd = ymmean1-ymmean2
        print*,minval(ymmeandiffd),maxval(ymmeandiffd),minloc(ymmeandiffd),maxloc(ymmeandiffd),'1819-ex1819,density'
        call num_memori(0.,400.,40,10,0.4,-1,-height3,-90)
        call symbolc(width3/2.,0.3,0.5,'Density',0.)
        call st_memori(1,6,width3,1,0.4,0,y=-height3)
        call butler_psmask(ymmeandiffd,width3,-height3,-1.,0.,r=0.8,g=0.8,b=0.8)
        call butler_cont(ymmeandiffd,width3,-height3,0.,-1.,0.2,thicc=5)
        do n = 1,6
            do i = 1,depth
                resultd(n,i) = fwelcht(ymmean1(n,i),ymsem1(n,i),ymdata1(n,i),ymmean2(n,i),ymsem2(n,i),ymdata2(n,i))
            end do
        end do

    ! t test for density
    call plot(width3+.5,0.,-3)
        call memori(40,0.1,10,-height3,-90.,y=-height3/2.);call st_memori(1,6,width3,1,0.4,0,y=-height3)
        call symbolc(width3/2.,0.6,0.4,'Density;T-test',0.)
        call butler_imask(resultd,width3,-height3,0)   
        call butler_imask(resultd,width3,-height3,1,r=1.,g=0.5,b=0.5)
        call butler_imask(resultd,width3,-height3,-1,r=0.5,g=0.5,b=1.)

    !geovel diff
    call plot(width3+0.5,0.,-3)

    call avsemdata_4D(psvel(3,1:15,1:months,1:5,1:depth),'dim2',mmean)
    call avsemdata_3D(mmean(1:15,1:5,1:depth),'dim1',mean_2D=ymmean1,sem_2D=ymsem1,dataquan_2D=ymdata1) !above
    call avsemdata_4D(psvel(4,1:15,1:months,1:5,1:depth),'dim2',mmean)
    call avsemdata_3D(mmean(1:15,1:5,1:depth),'dim1',mean_2D=ymmean2,sem_2D=ymsem2,dataquan_2D=ymdata2) !below
    ymmeandiffv = ymmean1-ymmean2
    print*,minval(ymmeandiffv),maxval(ymmeandiffv),minloc(ymmeandiffv),maxloc(ymmeandiffv),'above-below,vel'
    call symbolc(width3/2.,0.6,0.4,'Geostrophic;Velocity',0.)
    call memori(40,0.1,10,-height3,-90.,y=-height3/2.)
    call st_memori(1,6,width3,1,0.4,0,y=-height3)
    call butler_psmask(ymmeandiffv,width3,-height3,-40.,0.,r=0.8,g=0.8,b=0.8,gap =1)
    call butler_cont(ymmeandiffv,width3,-height3,0.,-40.,2.,thicc=5,r=0.,g=0.,b=0.,gap=1)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                    ! Lag Correlation
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call newpage;call plotmove(0.05,0.8)
    ! correlation coefficients of N and S Lines includes lagged correlations STATION 4
    n = 0
    do i = 1, 180
        if(1<=i.and.i<=12)cycle
        if(psarray2(1,3,i)==0. .or. psarray2(2,3,i)==0.)cycle
        n = n+1
        ! ;print*,n,i
        N_st4(n) = psarray2(1,3,i);S_st4(n) = psarray2(2,3,i)
    end do
    call dozencolors(r,g,b)
    j = 0
    do lag = 6,-6,-1 ! plus means N is lead by S by lag months
        j = j + 1
        if(lag>0)then
            ! print*,1+lag,size(N_st4),1,size(S_st4)-lag
            call correcoeff(N_st4(1+lag:size(N_st4)),S_st4(1:size(S_st4)-lag),size(S_st4(1:size(S_st4)-lag)),coeffs(lag))
            call box(width6,width6,3)
            if(lag ==6)then;call num_memori(0.,12.,13,5,0.4,1,width6,-90);else;call memori(12,0.1,5,width6,0.,x = width6/2.);endif
            if(lag==6)then;call num_memori(0.,12.,13,5,0.4,1,width6,0);else;call memori(12,0.1,5,width6,-90.,y = width6/2.);endif
            call numberc(width6/2.,width6+0.5,0.6,real(lag),0.,-1)
            do n = 1, size(N_st4)-lag
                i = lag+n
                call gmark_ratio(N_st4(i),0.,12.,width6,a)
                call gmark_ratio(S_st4(n),0.,12.,width6,c)
                call gmark(a,c,0.1,1)
            end do
            call numberc(width6/2.,-1.5,0.6,coeffs(lag),0.,3)
            call plot(width6+.3,0.,-3)
        endif
        if(lag==0)then
            call plot(-6.*(width6+0.3),-width6-5.,-3);call plot(width6+0.3,0.,-3)
            ! print*,size(N_st4),size(S_st4)
            call correcoeff(N_st4,S_st4,size(N_st4),coeffs(lag))
            call box(width6,width6,3)
            call num_memori(0.,12.,13,5,0.4,1,width6,-90)
            call num_memori(0.,12.,13,5,0.4,1,width6,0)
            call numberc(width6/2.,width6+0.5,0.6,real(lag),0.,-1)
            do n = 1, size(N_st4)
                call gmark_ratio(N_st4(n),0.,12.,width6,a)
                call gmark_ratio(S_st4(n),0.,12.,width6,c)
                call gmark(a,c,0.1,1)
            end do
            call numberc(width6/2.,-1.5,0.6,coeffs(lag),0.,3)
            call plot(0.,-width6-5.,-3);call plot(-(width6+0.3),0.,-3)
        endif
        if(lag<0)then
            ! print*,1,size(N_st4)-abs(lag),1+abs(lag),size(S_st4)
            call correcoeff(N_st4(1:size(N_st4)-abs(lag)),S_st4(1+abs(lag):size(S_st4)),size(N_st4(1:size(N_st4)-abs(lag))),coeffs(lag))
            call box(width6,width6,3)
            if(lag ==-1)then;call num_memori(0.,12.,13,5,0.4,1,width6,-90);else;call memori(12,0.1,5,width6,0.,x = width6/2.);endif
            if(lag==-1)then;call num_memori(0.,12.,13,5,0.4,1,width6,0);else;call memori(12,0.1,5,width6,-90.,y = width6/2.);endif
            call numberc(width6/2.,width6+0.5,0.6,real(lag),0.,-1)
            do n = 1, size(N_st4)-abs(lag)
                i = abs(lag)+n
                call gmark_ratio(N_st4(n),0.,12.,width6,a)
                call gmark_ratio(S_st4(i),0.,12.,width6,c)
                call gmark(a,c,0.1,1)
            end do
            call numberc(width6/2.,-1.5,0.6,coeffs(lag),0.,3)
            call plot(width6+.3,0.,-3)

        endif
        ! print*,lag,coeffs(lag)
    end do

    call plot(-3.*(width6+0.3),width6+2.,-3)
    call box(height6,height6,3)
    call num_memori(-1.,1.,21,2,0.4,1,height6,-90);call num_memori(-1.,1.,21,2,0.4,1,height6,90,x = height6)
    call num_memori(6.,-6.,13,1,0.4,-1,height6,0);call memori(12,0.1,1,height6,-180.,x = height6/2.,y = height6)
    j = 0;i =0
    ploty=0.
    call dozencolors(r,g,b)
    do n = 6,-6,-1
        j = j +1
        ! if(n/=0)then;i=i+1;else;i = i;endif
        ! if(n==0)then;call rgbk(0.,0.,0.);else;call rgbk(r(i),g(i),b(i));endif
        call numberc(real(j-1)*height6/12.,height6+0.5,0.4,real(n),0.,-1)
        call gmark_ratio(coeffs(n),-1.,1.,height6,ploty(j))
        call gmark(real(j-1)*height6/12.,ploty(j),0.2,1)
        if(j/=1)then;call plot(real(j-2)*height6/12.,ploty(j-1),3);call plot(real(j-1)*height6/12.,ploty(j),2);endif
    end do

    call rgbk(0.3,0.3,0.3)
    call floating_lines(height6,0.,10,3,x_inc=0.,y_inc = height6/10.,dashy = -3)
    call floating_lines(height6,90.,12,3,x_inc=height6/12.,y_inc = 0.,dashy = -3)


        call plote
  end program