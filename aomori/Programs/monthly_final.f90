program oct22nd
    use always
    implicit none
    real,parameter::width = 2.,height = 4.
    real,dimension(:,:,:,:),allocatable::tempmean,salmean,sigmamean,geovelmean,sigma_sem

    call plots(0.,0.,13,'../Plots/Favorites/monthlyPS_final_centralizedcolors.ps')
    call plotmove(0.05,0.8);call plotsave('original')
    call plotmove(0.5,0.9)
    call symbolc(0.,0.,1.,'Monthly Data N-Line,stations 1 to 6',0.)
    call plotback('original')

    call calibrated_data51(potemp_c5,sal_c5)
    call create_sigma_array(potemp_c5,sal_c5,sigma_c5)
    call geovel_array(51,geovel_5)
    call avsemdata_5D(potemp_c5,years,months,lines,stations,depth,'dim1',mean_4D=tempmean)
    call avsemdata_5D(sal_c5,years,months,lines,stations,depth,'dim1',mean_4D=salmean)
    call avsemdata_5D(sigma_c5,years,months,lines,stations,depth,'dim1',mean_4D=sigmamean,sem_4D=sigma_sem)
    call avsemdata_5D(geovel_5,years,months,lines,stations,depth,'dim1',mean_4D=geovelmean)
    ! print*,minex0(D4=tempmean),maxval(tempmean),'tempmean'
    ! print*,minex0(D4=salmean),maxval(salmean),'salmean'
    ! print*,minex0(D4=sigmamean),maxval(sigmamean),'sigmamean'
    ! print*,minex0(D4=geovelmean),maxval(geovelmean),'geovelmean'
    print*,minex0(D3=sigma_sem(:,1,:,:)),maxval(sigma_sem(:,1,:,:)),'sigma_sem'
    print*,maxloc(sigma_sem(:,1,:,:)),'maxloc'

    ! do m = 1, months
    !     do st = 4,9
    !         do d = 1, depth
    !             if(sigma_sem(m,1,st,d)>1.)then;print*,m,st,d,sigma_sem(m,1,st,d);sigma_sem(m,1,st,d) = 0.;endif
    !         end do 
    !     end do
    ! end do
    ! print*,minex0(D3=sigma_sem(:,1,:,:)),maxval(sigma_sem(:,1,:,:)),'sigma_sem'
    ! print*,maxloc(sigma_sem(:,1,:,:)),'maxloc'
    ! print*,sigma_sem(10,1,5,30:35);print*,sigma_sem(10,1,6,30:35)
! Potemp and Sal

    do m = 1,months
        call symbolc(width/2.,0.4,0.6,month_names(m),0.)
        if(m==1)then;call num_memori(0.,400.,40,10,0.4,-1,-height,-90);call st_memori(1,6,width,1,0.4,y=-height,gap = 2)
        else if(m==7)then;call memori(40,0.1,10,height,-90.,y=-height/2.);call st_memori(1,6,width,1,0.4,y=-height,gap = 2)
        elseif(m==4.or.m==10)then;call memori(40,0.1,10,height,-90.,y=-height/2.);call st_memori(1,6,width,1,0.4,y=-height)
        else;call memori(40,0.1,10,height,-90.,y=-height/2.);call memori(6,0.12,0,width,0.,x=width/2.,y=-height)
        end if
            call butler_psk(salmean(m,1,4:9,1:depth),6,depth,width,-height,0.,33.95,34.3,0.05,'b2w2r',7,bpt1=4,r=r1,g=g1,b=b1,centralize = 4)
            call butler_cont(tempmean(m,1,4:9,1:depth),6,depth,width,-height,0.,0.,1.,thicc=5)
        call plot(width+0.3,0.,-3)
    end do
! potemp and sal ends

call plotback('original')
call plot(0.,-height-1.,-3);call plotsave('sigma')

! ! sigma and its standard deviation
    do m = 1, months
        if(m==1)then;call num_memori(0.,400.,40,10,0.4,-1,-height,-90);call st_memori(1,6,width,1,0.4,y=-height,gap = 2)
        else if(m==7)then;call memori(40,0.1,10,height,-90.,y=-height/2.);call st_memori(1,6,width,1,0.4,y=-height,gap = 2)
        elseif(m==4.or.m==10)then;call memori(40,0.1,10,height,-90.,y=-height/2.);call st_memori(1,6,width,1,0.4,y=-height)
        else;call memori(40,0.1,10,height,-90.,y=-height/2.);call memori(6,0.12,0,width,0.,x=width/2.,y=-height)
        end if
            call butler_psk(sigma_sem(m,1,4:9,1:depth),6,depth,width,-height,0.,0.,0.2,0.05,'wred',4,r=r2,g=g2,b=b2,centralize = 1)
            call butler_cont(sigmamean(m,1,4:9,1:depth),6,depth,width,-height,0.,20.,.2,thicc=5)
        call plot(width+0.3,0.,-3)
    end do
! ! sigma and its standard deviation ends

call plotback('sigma')
call plot(0.,-height-1.,-3);call plotsave('geovel')

! ! geovel 
    do m = 1, months
        if(m==1)then;call num_memori(0.,400.,40,10,0.4,-1,-height,-90);call st_memori(1,6,width,1,0.4,y=-height,gap = 2)
        else if(m==7)then;call memori(40,0.1,10,height,-90.,y=-height/2.);call st_memori(1,6,width,1,0.4,y=-height,gap = 2)
        elseif(m==4.or.m==10)then;call memori(40,0.1,10,height,-90.,y=-height/2.);call st_memori(1,6,width,1,0.4,y=-height)
        else;call memori(40,0.1,10,height,-90.,y=-height/2.);call memori(6,0.12,0,width,0.,x=width/2.,y=-height)
        end if
            call butler_psk(geovelmean(m,1,5:9,1:depth),5,depth,width,-height,0.,0.,30.,10.,'wred',3,gap =1,r=r3,g=g3,b=b3,centralize=1)
            call butler_psmask(geovelmean(m,1,5:9,1:depth),5,depth,width,-height,-40.,0.,r=0.8,g=0.8,b=0.8,gap =1)
            call butler_cont(geovelmean(m,1,5:9,1:depth),5,depth,width,-height,0.,-40.,2.,thicc=5,r=0.,g=0.,b=0.,gap=1)
        call plot(width+0.3,0.,-3)
    end do
! ! geovel 

call plotmove(0.25,0.06)
call colorscale(7,r1,g1,b1,33.95,34.3,2,0.4,2,3.*width,0.3,lt=1,gt=1,symbol_start=2)

call plotmove(0.5,0.06) 
call colorscale(4,r2,g2,b2,0.,.2,2,0.4,2,3.*width,0.3,gt=1)

call plotmove(0.75,0.06)
call colorscale(3,r3,g3,b3,0.,30.,1,0.5,-1,3.*width,0.3,gt=1)
print*,g3
call plote
end program

