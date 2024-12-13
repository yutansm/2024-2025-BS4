program hakidame
    use always 
    implicit none
    real,parameter::width1 = 4.,height1 = 8.,width2 = 3.,height2 = 6.
    real,dimension(15,12,1,5,400)::geovel_5
    real,dimension(:,:,:,:),allocatable::avpotemp_c5,avsal_c5,avsigma_c5,avgeovel_5,sem_potemp_c5,sem_sal_c5,sem_sigma_c5,sem_geovel_5
    integer,dimension(:,:,:,:),allocatable::data_potemp_c5,data_sal_c5,data_sigma_c5,data_geovel_5
    real,dimension(:,:),allocatable::geovel2d
    real,dimension(:),allocatable::r1,g1,b1
    integer,dimension(6,depth)::testmatrix
    real,dimension(6,depth)::diffp,diffsal,diffsig
    real,dimension(5,depth)::diffgeo
    integer::month1,month2

    call calibrated_data51(potemp_c5,sal_c5) ! 15*12*2*9*400
    do y = 1, 15
        do m = 1, 12
            call calc_geovel(geovel2d,delta_x,temp_2D = potemp_c5(y,m,1,4:9,:),sal_2D = sal_c5(y,m,1,4:9,:),lat = 41.)
            geovel_5(y,m,1,:,:) = geovel2d
            deallocate(geovel2d)
        end do
    end do
    call create_sigma_array(potemp_c5,sal_c5,sigma_c5)
    call avsemdata_5D(potemp_c5,'dim1',mean_4D=avpotemp_c5,s_4D=sem_potemp_c5,dataquan_4D=data_potemp_c5)! sd data inside sem array my mistake in naming
    call avsemdata_5D(sal_c5,'dim1',mean_4D=avsal_c5,s_4D=sem_sal_c5,dataquan_4D=data_sal_c5)
    call avsemdata_5D(sigma_c5,'dim1',mean_4D=avsigma_c5,s_4D=sem_sigma_c5,dataquan_4D=data_sigma_c5)
    call avsemdata_5D(geovel_5,'dim1',mean_4D=avgeovel_5,s_4D=sem_geovel_5,dataquan_4D=data_geovel_5)
    print*,minex0(D4=avpotemp_c5),maxval(avpotemp_c5),'avpotemp_c5'
    print*,minex0(D4=avsal_c5),maxval(avsal_c5),'avsal_c5'
    print*,minex0(D4=avsigma_c5),maxval(avsigma_c5),'avsigma_c5'
    print*,minex0(D4=avgeovel_5),maxval(avgeovel_5),'avgeovel_5'


    do m = 1,months
        ! if(m==1.or.m==7)cycle
        ! if(m==2)then;month1 = m;month2 = 12
        !     elseif(m==8)then;month1 = m;month2 = 6
        !     else;month1 = m;month2 = m-1
        ! end if
        ! if(m==1)then;month1 = 1;month2 = 12
        ! elseif(m==2)then;month1 = 2;month2 = 1
        ! elseif(m==7)then;month1 = 7;month2 = 6    ! all the forgotten ones
        ! elseif(m==8)then;month1 = 8;month2 = 7
        ! elseif(m==10)then;month1=10;month2=8
        ! elseif(m==12)then;month1 = 12;month2 = 10
        ! else;cycle
        ! end if
        if(m==8)then;month1 = 10;month2 = 8
        elseif(m==10)then;month1 = 12;month2 = 10
        else;cycle
        end if

        
    call plots(0.,0.,9,'../Plots/Favorites/t_bymonths2-onetailed/'//monthnames(month1)(1:3)//'-'//monthnames(month2)(1:3)//'.ps')
    call plotmove(0.5,0.95);call symbolc(0.,-0.5,1.,"Welch's T Test "//trim(monthnames(month1))//' - '//trim(monthnames(month2)),0.)

    call plot(-width1-2.,-2.,-3)
! potemp sal ps and contour
    call symbolc(width1/2.,0.3,0.8,monthnames(month1),0.)
    call num_memori(0.,400.,41,10,0.6,-1,-height1,-90);call st_memori(1,6,width1,1,0.7,y=-height1)
    call butler_psk(avsal_c5(month1,1,4:9,:),width1,-height1,0.,33.95,34.3,0.05,'b2w2r',7,bpt1=4,r=r1,g=g1,b=b1)
    call butler_cont(avpotemp_c5(month1,1,4:9,:),width1,-height1,0.,0.,1.,thicc=5)
    call newpen2(5);call plot(width1+1.,-height1/2.,3);call plot(width1+2.5,-height1/2.,2);call newpen2(3)
    call plot(width1+4.,0.,-3)
    call num_memori(0.,400.,41,10,0.6,-1,-height1,-90);call st_memori(1,6,width1,1,0.7,y=-height1)
    call butler_psk(avsal_c5(month2,1,4:9,:),width1,-height1,0.,33.95,34.3,0.05,'b2w2r',7,bpt1=4)
    call symbolc(width1/2.,0.3,0.8,monthnames(month2),0.)
    call butler_cont(avpotemp_c5(month2,1,4:9,:),width1,-height1,0.,0.,1.,thicc=5)
    call colorscale(7,r1,g1,b1,33.95,34.3,2,0.5,2,height1-1.,0.3,lt = 1,gt = 1,rangle=90.,x= width1+2.,y = -height1/2.,symbol_start = 2)

    if(month1==1.or.month1==2.or.month1==7 .or.month1 ==8)then 
        diffp(6,:) = avpotemp_c5(month1,1,9,:)-avpotemp_c5(month2,1,9,:);diffp(1:5,:) = 0.
        diffsal(6,:) = avsal_c5(month1,1,9,:)-avsal_c5(month2,1,9,:);diffsal(1:5,:) = 0.
        diffsig(6,:) = avsigma_c5(month1,1,9,:)-avsigma_c5(month2,1,9,:);diffsig(1:5,:) = 0.
        diffgeo = 0.
    else
        diffp = avpotemp_c5(month1,1,4:9,:)-avpotemp_c5(month2,1,4:9,:)
        diffsal = avsal_c5(month1,1,4:9,:)-avsal_c5(month2,1,4:9,:)
        diffsig = avsigma_c5(month1,1,4:9,:)-avsigma_c5(month2,1,4:9,:)
        diffgeo = avgeovel_5(month1,1,:,:)-avgeovel_5(month2,1,:,:)
    end if
    print*,monthnames(month1),'-',monthnames(month2),minex0(D2=diffp),maxval(diffp),'diffp'
    print*,monthnames(month1),'-',monthnames(month2),minex0(D2=diffsal),maxval(diffsal),'diffsal'
    print*,monthnames(month1),'-',monthnames(month2),minex0(D2=diffsig),maxval(diffsig),'diffsig'
    print*,monthnames(month1),'-',monthnames(month2),minex0(D2=diffgeo),maxval(diffgeo),'diffgeo'


! diff potemp 
    call plotmove2(4.,15.5);call plotsave('potemp')
    call symbolc(width2/2.,0.3,0.7,'PoTemp',0.)
    call symbolc(-2.,-height2/2.,0.8,'diff',0.)
    call num_memori(0.,400.,41,10,0.5,-1,-height2,-90)
    call st_memori(1,6,width2,1,0.5,y=-height2)
    call butler_psmask(diffp,width2,-height2,-100.,0.,r=0.8,g=0.8,b=0.8)
    call butler_cont(diffp,width2,-height2,0.,-100.,1.,thicc=5)

    call plot(width2+0.8,0.,-3)

!diff sal
    ! call plotmove2(10.5+2.,15.5);call plotsave('sal')
    call symbolc(width2/2.,0.3,0.7,'Sal',0.)
    call memori(40,0.11,10,height2,-90.,y=-height2/2.);call memori(6,0.2,0,width2,0.,x=width2/2.,y=-height2)
    call butler_psmask(diffsal,width2,-height2,-100.,0.,r=0.8,g=0.8,b=0.8)
    call butler_cont(diffsal,width2,-height2,0.,-100.,0.05,thicc=2)
    ! if(month1==8)print*,'aug-jul',diffsal

!diff sigma
    ! call plotmove2(3.,7.5);call plotsave('sigma')
    call plot(width2+0.8,0.,-3)
    call symbolc(width2/2.,0.7,0.6,'Sigma;Theta',0.)
    call memori(40,0.11,10,height2,-90.,y=-height2/2.);call memori(6,0.2,0,width2,0.,x=width2/2.,y=-height2)
    call butler_psmask(diffsig,width2,-height2,-100.,0.,r=0.8,g=0.8,b=0.8)
    call butler_cont(diffsig,width2,-height2,0.,-100.,0.2,thicc=5)

!diff geovel
    ! call plotmove2(10.5+2.,7.5);call plotsave('geovel')
    call plot(width2+0.8,0.,-3)
    call symbolc(width2/2.,0.8,0.6,'Geostrophic;Velocity',0.)
    call memori(40,0.11,10,height2,-90.,y=-height2/2.);call memori(6,0.2,0,width2,0.,x=width2/2.,y=-height2)
    call butler_psmask(diffgeo,width2,-height2,-100.,0.,r=0.8,g=0.8,b=0.8,gap=1)
    call butler_cont(diffgeo,width2,-height2,0.,-0.2,0.02,thicc=5,gap=5)



    call plotmove2(4.,7.5)
! potemp ttest  
    do st = 1,6
        do d = 1, depth
            if(month1 == 10)then ! want to see red parts
                testmatrix(st,d) = fwelcht_greater(avpotemp_c5(month1,1,st+3,d),sem_potemp_c5(month1,1,st+3,d),data_potemp_c5(month1,1,st+3,d),avpotemp_c5(month2,1,st+3,d),sem_potemp_c5(month2,1,st+3,d),data_potemp_c5(month2,1,st+3,d))
            elseif(month1 == 12) then ! want to see blue parts
                testmatrix(st,d) = fwelcht_smaller(avpotemp_c5(month1,1,st+3,d),sem_potemp_c5(month1,1,st+3,d),data_potemp_c5(month1,1,st+3,d),avpotemp_c5(month2,1,st+3,d),sem_potemp_c5(month2,1,st+3,d),data_potemp_c5(month2,1,st+3,d))
            end if
        end do
    end do
    call symbolc(-2.2,-height2/2.,0.7,"Welch;T-Test",0.)
    call num_memori(0.,400.,41,10,0.5,-1,-height2,-90)
    call st_memori(1,6,width2,1,0.5,y=-height2)
    call butler_imask(testmatrix,width2,-height2,0)
    call butler_imask(testmatrix,width2,-height2,1,r=1.,g=0.5,b=0.5)
    call butler_imask(testmatrix,width2,-height2,-1,r=0.5,g=0.5,b=1.)
    call butler_imask(testmatrix,width2,-height2,911,r=0.5,g=1.,b=0.5)


! sal ttest
    call plot(width2+0.8,0.,-3)
    do st = 1,6
        do d = 1, depth
            if(month1 == 10)then ! want to see blue parts
                testmatrix(st,d) = fwelcht_smaller(avsal_c5(month1,1,st+3,d),sem_sal_c5(month1,1,st+3,d),data_sal_c5(month1,1,st+3,d),avsal_c5(month2,1,st+3,d),sem_sal_c5(month2,1,st+3,d),data_sal_c5(month2,1,st+3,d))
            elseif(month1 == 12) then ! want to see blue parts
                testmatrix(st,d) = fwelcht_smaller(avsal_c5(month1,1,st+3,d),sem_sal_c5(month1,1,st+3,d),data_sal_c5(month1,1,st+3,d),avsal_c5(month2,1,st+3,d),sem_sal_c5(month2,1,st+3,d),data_sal_c5(month2,1,st+3,d))
            end if
        end do
    end do
    ! call symbolc(width2/2.,0.7,0.6,"Welch's;T-Test",0.)
    call memori(40,0.11,10,height2,-90.,y=-height2/2.);call memori(6,0.2,0,width2,0.,x=width2/2.,y=-height2)
    call butler_imask(testmatrix,width2,-height2,0)
    call butler_imask(testmatrix,width2,-height2,1,r=1.,g=0.5,b=0.5)
    call butler_imask(testmatrix,width2,-height2,-1,r=0.5,g=0.5,b=1.)
    call butler_imask(testmatrix,width2,-height2,911,r=0.5,g=1.,b=0.5)

! sigma ttest
    call plot(width2+0.8,0.,-3)
    do st = 1,6
        do d = 1, depth
            if(month1 == 10)then ! want to see blue parts
                testmatrix(st,d) = fwelcht_smaller(avsigma_c5(month1,1,st+3,d),sem_sigma_c5(month1,1,st+3,d),data_sigma_c5(month1,1,st+3,d),avsigma_c5(month2,1,st+3,d),sem_sigma_c5(month2,1,st+3,d),data_sigma_c5(month2,1,st+3,d))
            elseif(month1 == 12) then ! want to see red parts
                testmatrix(st,d) = fwelcht_greater(avsigma_c5(month1,1,st+3,d),sem_sigma_c5(month1,1,st+3,d),data_sigma_c5(month1,1,st+3,d),avsigma_c5(month2,1,st+3,d),sem_sigma_c5(month2,1,st+3,d),data_sigma_c5(month2,1,st+3,d))
            end if
        end do
    end do
    ! call symbolc(width2/2.,0.7,0.6,"Welch's;T-Test",0.)
    call memori(40,0.11,10,height2,-90.,y=-height2/2.);call memori(6,0.2,0,width2,0.,x=width2/2.,y=-height2)
    call butler_imask(testmatrix,width2,-height2,0)
    call butler_imask(testmatrix,width2,-height2,1,r=1.,g=0.5,b=0.5)
    call butler_imask(testmatrix,width2,-height2,-1,r=0.5,g=0.5,b=1.)
    call butler_imask(testmatrix,width2,-height2,911,r=0.5,g=1.,b=0.5)

! geovel ttest
    call plot(width2+0.8,0.,-3)
    do st = 1,5
        do d = 1, depth
            if(month1 == 10)then ! want to see red parts
                testmatrix(st,d) = fwelcht_greater(avgeovel_5(month1,1,st,d),sem_geovel_5(month1,1,st,d),data_geovel_5(month1,1,st,d),avgeovel_5(month2,1,st,d),sem_geovel_5(month2,1,st,d),data_geovel_5(month2,1,st,d))
            elseif(month1 == 12) then ! want to see red parts
                testmatrix(st,d) = fwelcht_greater(avgeovel_5(month1,1,st,d),sem_geovel_5(month1,1,st,d),data_geovel_5(month1,1,st,d),avgeovel_5(month2,1,st,d),sem_geovel_5(month2,1,st,d),data_geovel_5(month2,1,st,d))
            end if
        end do
    end do
    ! call symbolc(width2/2.,0.7,0.6,"Welch's;T-Test",0.)
    call memori(40,0.11,10,height2,-90.,y=-height2/2.);call memori(6,0.2,0,width2,0.,x=width2/2.,y=-height2)
    call butler_imask(testmatrix(1:5,:),width2,-height2,0,gap=1)
    call butler_imask(testmatrix(1:5,:),width2,-height2,1,r=1.,g=0.5,b=0.5,gap = 1)
    call butler_imask(testmatrix(1:5,:),width2,-height2,-1,r=0.5,g=0.5,b=1.,gap =1)
    call butler_imask(testmatrix(1:5,:),width2,-height2,911,r=0.5,g=1.,b=0.5,gap=1)
call plote
call plotomit
end do



end program