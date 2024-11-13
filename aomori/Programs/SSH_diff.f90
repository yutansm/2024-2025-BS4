program SSHdiff
    use always
    implicit none
    real,parameter::width = 7.,height = 6.,width2 = 26.,height2 = 4.,width3 = 26.,height3 = 8.
    real,dimension(15,12)::esa,oku,nez,awa,plotarray,ncalesa=-999.,ncaloku=-999.,ncalnez=-999.,ncalawa=-999.,rawesa,rawoku,rawnez,rawawa,esa_oku=0.,nez_awa=0.
    real,dimension(12)::avarray,semarray
    integer,dimension(12)::dataarray
    real,dimension(180)::ploty=0.,ploty2=0.,esa_oku1d,nez_awa1d
    real,dimension(:),allocatable::avesa,avoku,avnez,avawa,semesa,semoku,semnez,semawa,sdesa,sdoku,sdnez,sdawa,ymeane,ymeano,ymeann,ymeana,ymeanrawe,ymeanrawo,ymeanrawn,ymeanrawawa,ymeanesa_oku,ymeannez_awa,avesa_oku,avnez_awa
    integer,dimension(:),allocatable::esadata,okudata,nezdata,awadata
    real::dx,min,max,num1,num2,num3,num4,sesa_oku1d,snez_awa1d
    ! real::tmeandiff_eo,tsemdiff_eo,tmeandiff_na,tsemdiff_na,tsdiff_eo,tsdiff_na
    integer::month1,month2,result
    integer::k2
    character(len=30)::string

    !!! Pairs of SSAP of (esashi & okushiri) and (nezugaseki & awashima) are different only in August 2020 
    !!! esashi == from 北海道開発局, okushiri == from 国土地理院, nezugaseki == from 国土地理院, awashima == from 海上保安庁
    ! esashi = 4703, okushiri = 1702, nezugaseki = 1604, awashima = 3602
    call SSH_data(esa,ilabel = 4703,convert = 1, calibrate = 1)
    call SSH_data(oku,ilabel = 1702,convert = 1, calibrate = 1)
    call SSH_data(nez,ilabel = 1604,convert = 1, calibrate = 1)
    call SSH_data(awa,ilabel = 3602,convert = 1, calibrate = 1)
    call SSH_data(rawesa,ilabel = 4703,convert = 1)
    call SSH_data(rawoku,ilabel = 1702,convert = 1)
    call SSH_data(rawnez,ilabel = 1604,convert = 1)
    call SSH_data(rawawa,ilabel = 3602,convert = 1)
    ! print*,esa
    ! print*,ncalesa

    call avsemdata_2D(esa,'dim2',mean_1D = ymeane,rmask = -999.) ! 15 yearly means
    call avsemdata_2D(oku,'dim2',mean_1D = ymeano,rmask = -999.) ! 15 yearly means
    call avsemdata_2D(nez,'dim2',mean_1D = ymeann,rmask = -999.) ! 15 yearly means
    call avsemdata_2D(awa,'dim2',mean_1D = ymeana,rmask = -999.) ! 15 yearly means
    call avsemdata_2D(rawesa,'dim2',mean_1D = ymeanrawe,rmask = -999.) ! 15 yearly means
    call avsemdata_2D(rawoku,'dim2',mean_1D = ymeanrawo,rmask = -999.) ! 15 yearly means
    call avsemdata_2D(rawnez,'dim2',mean_1D = ymeanrawn,rmask = -999.) ! 15 yearly means
    call avsemdata_2D(rawawa,'dim2',mean_1D = ymeanrawawa,rmask = -999.) ! 15 yearly means
    ! print*,ymeane,'esashi'
    ! print*,ymeano,'okushiri'
    ! print*,ymeann,'nezugaseki'
    ! print*,ymeana,'awashima'
    do n = 1, 15
        do m = 1,12
                if(esa(n,m) == 0..or. esa(n,m) == -999.)then
            else;esa(n,m) = esa(n,m) - ymeane(n) ! esa array now contains deviations from yearly means      2023 has no data
            end if
                if(oku(n,m) == 0..or. oku(n,m) == -999.)then
            else;oku(n,m) = oku(n,m) - ymeano(n) ! oku array now contains deviations from yearly means
            end if
                if(nez(n,m) == 0..or. nez(n,m) == -999.)then
            else;nez(n,m) = nez(n,m) - ymeann(n) ! nez array now contains deviations from yearly means
            end if
                if(awa(n,m) == 0..or. awa(n,m) == -999.)then
            else;awa(n,m) = awa(n,m) - ymeana(n) ! awa array now contains deviations from yearly means
            end if
                if(rawesa(n,m) == 0..or. rawesa(n,m) == -999.)then
            else;ncalesa(n,m) = rawesa(n,m) - ymeanrawe(n) ! ncalesa array now contains deviations from yearly means
            end if
                if(rawoku(n,m) == 0..or. rawoku(n,m) == -999.)then
            else;ncaloku(n,m) = rawoku(n,m) - ymeanrawo(n) ! ncaloku array now contains deviations from yearly means
            end if
                if(rawnez(n,m) == 0..or. rawnez(n,m) == -999.)then
            else;ncalnez(n,m) = rawnez(n,m) - ymeanrawn(n) ! ncalnez array now contains deviations from yearly means
            end if
                if(rawawa(n,m) == 0..or. rawawa(n,m) == -999.)then
            else;ncalawa(n,m) = rawawa(n,m) - ymeanrawawa(n) ! ncalawa array now contains deviations from yearly means
            end if
        end do
    end do

    !esa - oku 差の時系列
    do n = 1, 15
        do m = 1,12
            if(esa(n,m) == 0..or. esa(n,m) == -999..or. oku(n,m) == 0..or. oku(n,m) == -999.)then
            else;esa_oku(n,m) = esa(n,m) - oku(n,m) ! esa_oku contains differences between esashi and okushiri from their respective yearly means
            end if
        end do
    end do

    !nez - awa
    do n = 1, 15
        do m = 1,12
            if(nez(n,m) == 0..or. nez(n,m) == -999..or. awa(n,m) == 0..or. awa(n,m) == -999.)then
            else;nez_awa(n,m) = nez(n,m) - awa(n,m) ! 
            end if
        end do
    end do
    
    ! print*,esa_oku(15,:);print*,nez_awa(15,:)

    call avsemdata_2D(esa,'dim1',mean_1D = avesa,sem_1D = semesa,s_1D = sdesa,rmask = -999.,dataquan_1D = esadata) ! 12 monthly means
    call avsemdata_2D(oku,'dim1',mean_1D = avoku,sem_1D = semoku,s_1D = sdoku,rmask = -999.,dataquan_1D = okudata) ! 12 monthly means
    call avsemdata_2D(nez,'dim1',mean_1D = avnez,sem_1D = semnez,s_1D = sdnez,rmask = -999.,dataquan_1D = nezdata) ! 12 monthly means
    call avsemdata_2D(awa,'dim1',mean_1D = avawa,sem_1D = semawa,s_1D = sdawa,rmask = -999.,dataquan_1D = awadata) ! 12 monthly means

    call avsemdata_2D(esa_oku,'dim2',mean_1D = ymeanesa_oku,rmask=-999.) ! 差の時系列の年平均
    call avsemdata_2D(nez_awa,'dim2',mean_1D = ymeannez_awa,rmask=-999.) 
    ! call avsemdata_1D(ymeanesa_oku,mean = num1,rmask = -999.)
    ! call avsemdata_1D(ymeannez_awa,mean = num2,rmask = -999.) !差の時系列の各年平均の平均は0にはならない。これは間違い

    do n = 1, 15
        do i = 1,12
            if(esa(n,i)==-999..or.esa(n,i)==0..or.oku(n,i)==-999..or.oku(n,i)==0)then
            else;esa_oku(n,i) = esa_oku(n,i) - ymeanesa_oku(n) ! 差の時系列の年平均からの差
            end if
        end do
    end do
    do n = 1, 15
        do i = 1,12
            if(nez(n,i)==-999..or.nez(n,i)==0..or.awa(n,i)==-999..or.awa(n,i)==0)then
            else;nez_awa(n,i) = nez_awa(n,i) - ymeannez_awa(n)
            end if
        end do
    end do

    esa_oku1d = reshape(esa_oku, [180]);nez_awa1d = reshape(nez_awa, [180])

    call avsemdata_1D(esa_oku1d,mean = num3,rmask = -999.,s = sesa_oku1d) ! 差の時系列の年平均からの差の和＝0 平均＝0 になるはず。江差奥尻は-7オーダ，鼠ヶ関粟島は-0.276 why
    call avsemdata_1D(nez_awa1d,mean = num4,rmask = -999.,s = snez_awa1d)
    print*,num3,num4,sesa_oku1d,snez_awa1d
    call avsemdata_2D(esa_oku,'dim1',mean_1D = avesa_oku) ! 12 monthly means
    call avsemdata_2D(nez_awa,'dim1',mean_1D = avnez_awa) ! 12 monthly means

    call plots2('../Plots/SSH/monthly_coast-offshoreXX.ps',h = 'Coastal and Offshore Comparisons of Sea Surface Height',oopt= 'otops')
    call plot(1.5,-1.,-3);call plotsave('first')

! monthly means and diffs
    ! esashi and okushiri
    do l = 1, 3
        call newpen2(3)
        if(l == 1)then
            min = 100.;max = -100.;avarray = avesa;semarray = semesa;dataarray = esadata
            call symbolc(width/2.,1.,0.6,'Esashi',0.)
        else if(l == 2)then
            min = 100.;max = -100.;avarray = avoku;semarray = semoku;dataarray = okudata
            call symbolc(width/2.,1.,0.6,'Okushiri',0.)
        else if(l == 3)then
            min = 2.;max = -2.;avarray = avesa_oku
            do i = 1, size(avarray)
                semarray(i) = sqrt((sdesa(i)**2.0 / real(esadata(i))) + (sdoku(i)**2.0 / real(okudata(i))))
            end do
            call symbolc(width/2.,0.9,0.6,'Esashi;-Okushiri',0.)
            call rgbk(0.4,0.4,0.4)
            ! call floating_lines(width,0.,5,3,y_inc=-height/6.,y=-height/6.,dashy = -4)
            call rgbk(0.,0.,0.)
        end if
        call box(width,-height,3);call mod12_memori(13,0.5,0,width,gap = 2,num_freq = 1,y = -height,dxval = dx)
        if(l/=3)call num_memori(min/10.,max/10.,20,5,0.6,-1,-height,-90)
        if(l==3)call num_memori(min,max,4,1,0.6,-1,-height,-90)
        call rgbk(0.4,0.4,0.4);call newpen2(3);call newpen2(-4);call plot(0.,-height/2.,3);call plot(width,-height/2.,2);call rgbk(0.,0.,0.)
        do n = 1, 13
            if(n==13)then;m = 1;else;m = n
            end if
            if(l/=3)then
                call gmark_ratio(avarray(m),min,max,-height,ploty(m)) !individual data points
                ploty2(m) = semarray(m)*height/(min-max)
            end if
            if(l == 3)then
                if(m == 1)then;month1 = 1;month2 = 12
                else;month1 = m;month2 = m-1
                end if
                    ! ploty2(m) = f_t95(fwelchdf(sdesa(m),esadata(m),sdoku(m),okudata(m)))*semarray(m)*height/200.
                    call gmark_ratio(avarray(m)/sesa_oku1d,min,max,-height,ploty(m)) ! diff from yearly mean of the diffs normalized 
                    ! print*,avarray(m)/sesa_oku1d,'ai'
                    ! ploty2(m) = sqrt((sdesa(m)**2.0 / esadata(m)) + (sdoku(m)**2.0 / okudata(m)))/tsdiff_eo*height/(min-max) ! biased pooled sem devided by the mean sem of the diffs
                    ! print*,sqrt((sdesa(m)**2.0 / esadata(m)) + (sdoku(m)**2.0 / okudata(m)))/sesa_oku1d,'oi'
                    ! call gmark(width+0.5,tmeandiff_eo)
                    ! result = fwelcht(avesa(month1),sdesa(month1),esadata(month1),avoku(month1),sdoku(month1),okudata(month1))
                    ! if(result==1)then;call symbolc(dx/2.+dx*real(n-1),ploty(m)+0.6,0.4,'U',0.)
                    ! else if(result==-1)then;call symbolc(dx/2.+dx*real(n-1),ploty(m)-1.2,0.4,'t,a=0.05;lower',0.)
                    ! end if
            else 
                ! if(m == 7)then
                !     write(string,*)dataarray(m)
                ! call symbolc(width/2.,0.4,0.5,'Data Quantity:all'//trim(adjustl(string)),0.)
                ! end if
            endif
            call newpen2(3)
            call gmark(dx/2.+dx*real(n-1),ploty(m),0.2,1)
            if(n>1)then
                if(n/=13)then
                    call plot(dx/2.+dx*real(n-2),ploty(m-1),3);call plot(dx/2.+dx*real(n-1),ploty(m),2)
                else;call plot(dx/2.+dx*real(n-2),ploty(12),3);call plot(dx/2.+dx*real(n-1),ploty(1),2)
                end if
            end if
            ! if(l==3)call newpen2(2)
            if(l/=3)then
                call plot(dx/2.+dx*real(n-1),ploty(m)-ploty2(m),3);call plot(dx/2.+dx*real(n-1),ploty(m)+ploty2(m),2) ! sem or 95 CI lines
            end if
        end do
        call plot(width+2.,0.,-3)
    end do

    ! nezugaseki and awashima

    call plotback('first');call plot(0.,-height-2.5,-3)
    do l = 1, 3
        if(l == 1)then
            min = 150.;max = -150.;avarray = avnez;semarray = semnez;dataarray = nezdata
            call symbolc(width/2.,1.,0.6,'Nezugaseki',0.)
        else if(l == 2)then
            min = 150.;max = -150.;avarray = avawa;semarray = semawa;dataarray = awadata
            call symbolc(width/2.,1.,0.6,'Awashima',0.)
        else if(l == 3)then
            min = 2.;max = -2.;avarray = avnez_awa
            do i = 1, size(avarray)
                semarray(i) = sqrt((sdnez(i)**2.0 / real(nezdata(i))) + (sdawa(i)**2.0 / real(awadata(i))))
            end do
            call symbolc(width/2.,0.9,0.6,'Nezugaseki;-Awashima',0.)
            call rgbk(0.4,0.4,0.4)
            ! call floating_lines(width,0.,5,3,y_inc=-height/6.,y=-height/6.,dashy = -4)
            call rgbk(0.,0.,0.)
        end if
        call box(width,-height,3);call mod12_memori(13,0.5,0,width,gap = 2,num_freq = 1,y = -height,dxval = dx)
        if(l==1.or.l==2)call num_memori(min/10.,max/10.,30,5,0.6,-1,-height,-90)
        if(l==3)call num_memori(min,max,4,1,0.6,-1,-height,-90)
        call rgbk(0.4,0.4,0.4);call newpen2(3);call newpen2(-4);call plot(0.,-height/2.,3);call plot(width,-height/2.,2);call rgbk(0.,0.,0.)

        do n = 1, 13
            if(n==13)then;m = 1;else;m = n
            end if
            if(l/=3)then
                call gmark_ratio(avarray(m),min,max,-height,ploty(m))
                ploty2(m) = semarray(m)*height/(min-max)
            end if
            if(l == 3)then
                if(m == 1)then;month1 = 1;month2 = 12
                else;month1 = m;month2 = m-1
                end if
                    ! ploty2(m) = f_t95(fwelchdf(sdnez(m),nezdata(m),sdawa(m),awadata(m)))*semarray(m)*height/200.
                    call gmark_ratio(avarray(m)/snez_awa1d,min,max,-height,ploty(m))
                    ! print*,avarray(m)/snez_awa1d,'ai2'
                    ploty2(m) = sqrt((sdnez(m)**2.0 / nezdata(m)) + (sdawa(m)**2.0 / awadata(m)))/snez_awa1d*height/(min-max)
                    ! print*,sqrt((sdnez(m)**2.0 / nezdata(m)) + (sdawa(m)**2.0 / awadata(m)))/snez_awa1d,'oi2'
                    ! result = fwelcht(avnez(month1),sdnez(month1),nezdata(month1),avawa(month1),sdawa(month1),awadata(month1))
                    ! if(result==1)then;call symbolc(dx/2.+dx*real(n-1),ploty(m)+0.6,0.4,'U',0.)
                    ! else if(result==-1)then;call symbolc(dx/2.+dx*real(n-1),ploty(m)-.7,0.4,'t,a=0.05;lower',0.)
                    ! end if
            else 
                ploty2(m) = semarray(m)*height/200.
                ! call numberc(dx/2.+dx*real(n-1),0.4,0.5,real(dataarray(m)),0.,-1)
            endif
            call newpen2(3)
            call gmark(dx/2.+dx*real(n-1),ploty(m),0.2,1)
            if(n>1)then
                if(n/=13)then
                    call plot(dx/2.+dx*real(n-2),ploty(m-1),3);call plot(dx/2.+dx*real(n-1),ploty(m),2)
                else;call plot(dx/2.+dx*real(n-2),ploty(12),3);call plot(dx/2.+dx*real(n-1),ploty(1),2)
                end if
                ! if(l==3)call newpen2(2)
            end if
            if(l/=3)then
                call plot(dx/2.+dx*real(n-1),ploty(m)-ploty2(m),3);call plot(dx/2.+dx*real(n-1),ploty(m)+ploty2(m),2)
            end if
        end do
        call plot(width+2.,0.,-3)
    end do

    

call newpage('SSH timeseries',x = 1.5,y = 0.5);call plotsave('2ndpage')
call rgbk(0.6,0.6,0.6)
call floating_lines(-21.,90.,15,4,x_inc = width3/15.,x = width3/15./12./2.,dashy = -3)
call rgbk(0.,0.,0.)
call floating_numbers(2009.,1.,15,0.4,width3/15.,0.,0.,-1,x = width3/15./12.*6.5,y = 0.2)

! time series of esashi and okushiri together
    do l = 1,4
        min = 200.;max = -200.
        call rgbk(0.,0.,0.)
        if(l==3)call plot(0.,-height3-.7,-3)
        if(mod(l,2)==1)then
            call box(width3,-height3,3)
            call mod12_memori(15*12,0.3,0,width3,gap = 2,num_freq = 6,num_st = 1,y = -height3,dxval = dx)
            call num_memori(min,max,20,5,0.5,1,-height3,-90)
            call rgbk(0.4,0.4,0.4);call newpen2(3);call newpen2(-4);call plot(0.,-height3/2.,3);call plot(width3,-height3/2.,2);call rgbk(0.,0.,0.)
        end if

        if(l == 1)then
            plotarray = esa
            call symbolc(-2.,-height3/2.,0.5,'Esa&Oku;Oku=Grey',90.)
        else if(l == 2)then
            plotarray = oku
        else if(l == 3)then
            plotarray = nez
            call symbolc(-2.,-height3/2.,0.5,'Nez&Awa;Awa=Grey',90.)
        else if(l == 4)then
            plotarray = awa
        end if
        j = 1
        do n = 1, 15
            do i = 1,12
                ! if(l>4)call rgbk(1.,0.,0.)
                ! if(l>8)call rgbk(0.,0.,0.)
                if(plotarray(n,i)==0..or.plotarray(n,i)==-999.)then
                    ! print*,l,n,i,plotarray(n,i)
                    if(j==180)then;ploty(j)=0.;exit
                    else;ploty(j)=0.;j = j+1;cycle
                    end if
                else;call gmark_ratio(plotarray(n,i),min,max,-height3,ploty(j))
                    ! print*, plotarray(n,i)
                end if
                ! print*,ploty(j)

                if(mod(l,2)==0)call rgbk(0.5,0.5,.5)
                call gmark(dx/2.+dx*real(j-1),ploty(j),0.15,1)
                if(j > 1)then
                    do k = 1, 5
                        if(ploty(j-k)/=0.and.ploty(j-k)/=-999.)then
                            k2 = k;exit
                        end if
                    end do
                    if(k2>1)then
                        call newpen2(3);call newpen2(-6)
                    else if(k2==1)then
                        call newpen2(3)
                    end if
                    call plot(dx/2.+dx*real(j-k2-1),ploty(j-k2),3)
                    call plot(dx/2.+dx*real(j-1),ploty(j),2)
                end if
                j = j + 1
            end do
        end do
    end do

! time series of diffs between pairs of stations
call newpage('Timeseries of SSH differences',x = 1.5,y = 0.5)
call rgbk(0.6,0.6,0.6)
call floating_lines(-21.,90.,15,4,x_inc = width3/15.,x = width3/15./12./2.,dashy = -3)
call rgbk(0.,0.,0.)
call floating_numbers(2009.,1.,15,0.4,width3/15.,0.,0.,-1,x = width3/15./12.*6.5,y = 0.2)

    do l = 1,2
        call rgbk(0.,0.,0.)
        if(l==2)call plot(0.,-height3-.7,-3)
        min = 100.;max = -100.
        call box(width3,-height3,3)
        call mod12_memori(15*12,0.3,0,width3,gap = 2,num_freq = 6,num_st = 1,y = -height3,dxval = dx)
        call num_memori(min,max,20,5,0.5,1,-height3,-90)
        call rgbk(0.4,0.4,0.4);call newpen2(3);call newpen2(-4);call plot(0.,-height3/2.,3);call plot(width3,-height3/2.,2);call rgbk(0.,0.,0.)

        if(l == 1)then
            plotarray = esa_oku
            call symbolc(-2.,-height3/2.,0.5,'Esa-Oku',90.)
        else 
            plotarray = nez_awa
            call symbolc(-2.,-height3/2.,0.5,'Nez-Awa',90.)
        end if
        j = 1
        do n = 1, 15
            do i = 1,12
                if(plotarray(n,i)==0..or.plotarray(n,i)==-999.)then
                    ! print*,l,n,i,plotarray(n,i)
                    if(j==180)then;ploty(j)=0.;exit
                    else;ploty(j)=0.;j = j+1;cycle
                    end if
                else
                    call gmark_ratio(plotarray(n,i),min,max,-height3,ploty(j))
                    call gmark(dx/2.+dx*real(j-1),ploty(j),0.15,1)
                    if(j > 1)then
                        do k = 1, 5
                            if(ploty(j-k)/=0..and.ploty(j-k)/=-999.)then
                                k2 = k;exit
                            end if
                        end do
                        if(k2>1)then
                            ! print*,l,n,i,k,plotarray(n,i)
                            call newpen2(3);call newpen2(-6)
                        else if(k2==1)then
                            call newpen2(3)
                        end if
                        call plot(dx/2.+dx*real(j-k2-1),ploty(j-k2),3)
                        call plot(dx/2.+dx*real(j-1),ploty(j),2)
                        ! print*,j-k2,ploty(j-k2),j,ploty(j)
                    end if
                end if
                j = j + 1
            end do
        end do
    end do
    


call newpage('SSH individual timeseries',x = 1.5,y = 0.5)
call plotback('2ndpage')
call rgbk(0.6,0.6,0.6)
call floating_lines(-21.,90.,15,4,x_inc = width2/15.,x = width2/15./12./2.,dashy = -3)
call rgbk(0.,0.,0.)
call floating_numbers(2009.,1.,15,0.4,width2/15.,0.,0.,-1,x = width2/15./12.*6.5,y = 0.2)
! time series of individual stations

    do l = 1,12
        if(l==5)call plotback('2ndpage')
        if(l==9)call newpage('From Respective Reference Points',x = 1.5,y = 0.5)
        min = 200.;max = -200.
        if(l == 1)then
            plotarray = esa
            call symbolc(-2.,-height2/2.,0.7,'Esashi',90.)
        else if(l == 2)then
            plotarray = oku
            call symbolc(-2.,-height2/2.,0.7,'Okushiri',90.)
        else if(l == 3)then
            plotarray = nez
            call symbolc(-2.,-height2/2.,0.7,'Nezugaseki',90.)
        else if(l == 4)then
            plotarray = awa
            call symbolc(-2.,-height2/2.,0.7,'Awashima',90.)
        else if(l == 5)then
            plotarray = ncalesa
        else if(l == 6)then
            plotarray = ncaloku
        else if(l == 7)then
            plotarray = ncalnez
        else if(l == 8)then
            plotarray = ncalawa
        else if(l == 9 )then
            plotarray = rawesa
        else if(l == 10)then
            plotarray = rawoku
        else if(l == 11)then
            plotarray = rawnez
        else if(l == 12)then
            plotarray = rawawa
        end if
        if(l>8)then;min = 2000.;max = 0.;end if
        if(l<5.or. l>8)then
            call box(width2,-height2,3)
            call mod12_memori(15*12,0.3,0,width2,gap = 2,num_freq = 6,num_st = 1,y = -height2,dxval = dx)
            call num_memori(min,max,20,5,0.5,1,-height2,-90)
        end if
        j = 1
        do n = 1, 15
            do i = 1,12
                if(l>4)call rgbk(1.,0.,0.)
                if(l>8)call rgbk(0.,0.,0.)
                if(plotarray(n,i)==0..or.plotarray(n,i)==-999.)then
                    ! print*,l,n,i,plotarray(n,i)
                    if(j==180)then;ploty(j)=0.;exit
                    else;ploty(j)=0.;j = j+1;cycle
                    end if
                else;call gmark_ratio(plotarray(n,i),min,max,-height2,ploty(j))
                    ! print*, plotarray(n,i)
                end if
                ! print*,ploty(j)

                call gmark(dx/2.+dx*real(j-1),ploty(j),0.15,1)
                if(j > 1)then
                    do k = 1, 5
                        if(ploty(j-k)/=0.and.ploty(j-k)/=-999.)then
                            k2 = k;exit
                        end if
                    end do
                    if(k>1)then
                        call newpen2(3);call newpen2(-6)
                    else if(k==1)then
                        call newpen2(3)
                    end if
                    call plot(dx/2.+dx*real(j-k2-1),ploty(j-k2),3)
                    call plot(dx/2.+dx*real(j-1),ploty(j),2)
                end if
                j = j + 1;call rgbk(0.,0.,0.)
            end do
        end do
        call plot(0.,-height2-.7,-3)
    end do

    call plote


end program