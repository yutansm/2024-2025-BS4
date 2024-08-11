program aomori_cooridinates
    ! ! implicit none
    integer,parameter::month=12,year_start=2008,year_max=15,station_y=2,station_x=9,depth_max=1000 !station_xは深層見れない時7
    ! !line_num=緯度別の観測、st_x=経度別の観測列、年、月、深さ、塩分または水温
    ! real,dimension(year_max,month,station_y,station_x,depth_max)::potemp_5,sal_5
    ! ! real,dimension(station_y,station_x,depth_max)::initial_tempsum
    ! ! real,dimension(year_max,month,station_y,station_x)::lon
    character::aa*9,filename*999,line_name*20,moon*9
    character(len=*), parameter::pass_CTD = 'edited_data'
    integer::i,j,k,y,m,line_num,a,b,c,year,mon
    ! integer,parameter::interval = 5

    ! real,dimension(month,station_y,station_x,depth_max)::av_array
    ! real,dimension(station_x,depth_max)::useful_array
    ! integer,dimension(station_x,depth_max)::mask
    ! real,parameter::xlength=1.5, ylength=3.
    ! real,parameter::dx = xlength/real(station_x), dy = -ylength/real(depth_max)
    ! integer::is=1,js=1,n,q,x,memori,num = 0
    ! real::calc_sum,first_sum,calc_av,g,r,blue,xpoints,ypoints,rosso,verde,azul

    intrinsic sin,cos,tan,asin,acos

    integer,parameter::imax=2080, jmax=2640, istart=(137-122)*80+1, iend=(142-122)*80, jstart=(40-24)*120+1, jend=(42-24)*120
    real,parameter::pi=2.*asin(1.),ratio=6357./6378./cos(41.*pi/180.), mapxlength=10.,mapylength=mapxlength*ratio*2./5.
    real,parameter::dx2=mapxlength/real(iend-istart+1.),dy2=mapylength/real(jend-jstart+1.)
    real,dimension(imax,jmax)::dep
    real,dimension(station_y,station_x)::lon
    integer,dimension(imax,jmax)::dep_m
    real::SlineYco = dy2*(40.6-40.)*120, NlineYco = dy2*(41.-40.)*120



    102 format(9(f9.4))

    do line_num = 1,2    !参照ファイルの決定
        if(line_num==1) then
            line_name ='S-line'
        else 
            line_name ='N-line'
        end if
        filename = trim(pass_CTD)//'/'//trim(line_name)//'/lon.csv'
        open(31,file=filename,status='old',action='read')
        read(31,102) (lon(line_num,i),i=1,station_x)  !経度lonを配列に入れる
        close(31)
    end do

    open(21,file='japan1km122-148_24-46.bin',form='unformatted',status='old') !配列にデータを入れる
    do j=jmax,1,-1
        read(21)(dep(i,j),i=1,imax)
        dep(i,j)=-dep(i,j)    
    end do
    close(21)
    
    call plots(5.,5.,13,'x_stations_coordinates.ps') !ファイル作成
    
    do i=1,imax !マスキング
        do j=1,jmax
            dep_m(i,j)=1
        end do
    end do    

    call newpen2(3)
    call rgbk(0.,0.,0.)
    call pscont3(dx2,dy2,dep,dep_m,istart,iend,jstart,jend,imax,jmax,1,0.,1) !地図作成

    call plot(0.,0.,3)  !枠作り
    call plot(mapxlength,0.,2)
    call plot(mapxlength,mapylength,2)
    call plot(0.,mapylength,2)
    call plot(0.,0.,2)

    do x=1,6  !目盛
        call plot(dx2*80*real(x-1),0,3)
        call plot(dx2*80*real(x-1),-0.1,2)
        call numberc(dx2*80*real(x-1),-ylength-0.3,0.2,real(x+136),0.,-1)
    end do

    do y=1,3  !目盛　　　　　後でもっと細かくする
        call plot(0.,dy2*120*real(y-1),3)
        call plot(-0.1,dy2*120*real(y-1),2)
        call numberc(-0.2,dy2*120*real(y-1),0.2,real(y+39),0.,-1)
    end do
! call numberc(5.,5.,1.,lon(1,1),0.,3)

    do line_num = 1,2
        do x = 1,station_x
            Xco = dx2*(lon(line_num,x)-137.)*80.    !x_stationそれぞれのｘ座標
            if (line_num == 1) then   ! 1 = S-line, 2 = N-line
                call gmark(Xco,SlineYco,0.1,1)
                call numberc(Xco,SlineYco+0.2,0.2,real(x),0.,-1)
            else if(line_num == 2) then
                call gmark(Xco,NlineYco,0.1,1)
                call numberc(Xco,NlineYco+0.2,0.2,real(x),0.,-1)
            else
            end if
        end do    
    end do

    call symbol(1.,SlineYco+0.4,0.3,'S-line',0.,6)
    call symbol(1.,NlineYco+0.4,0.3,'N-line',0.,6)

        

call plote
end program