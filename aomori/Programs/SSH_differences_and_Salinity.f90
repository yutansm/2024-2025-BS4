program gomibako
    use always 
    implicit none
    real,parameter::width = 25.,height = 3.,height2 = 4.5
    real,dimension(15,12)::esa,oku,esa_oku,sak,tob,sak_tob,mat,tap,tap_mat
    real,dimension(15*12,400)::salinityseries
    real,dimension(:),allocatable::r1,g1,b1
    real::dx

    call SSH_data(esa,slabel = '江差港',calibrate = .true.,diff_from_yearly_mean = .true.)
    call SSH_data(oku,slabel = '奥尻',calibrate = .true.,diff_from_yearly_mean = .true.)
    call SSH_data(sak,slabel = '酒田港',calibrate = .true.,diff_from_yearly_mean = .true.)
    call SSH_data(tob,slabel = '飛島',calibrate = .true.,diff_from_yearly_mean = .true.)
    call SSH_data(mat,ilabel = 4701,calibrate = .true.,diff_from_yearly_mean = .true.) ! matsumae , slabel cannot be used in conjunction with 'calibrate' since the name of the station differs in the ssh and ssap files oh no
    call SSH_data(tap,slabel = '竜飛',calibrate = .true.,diff_from_yearly_mean = .true.)
    call calibrated_data2(potemp_c5 = potemp_c5,sal_c5 = sal_c5,match_station_labels_and_array_indices = .true.)    

    esa_oku = esa - oku
    sak_tob = sak - tob
    tap_mat = tap - mat

    tap_mat(14,8) = 0.
    sak_tob(1,1:2) = 0.
    do i = 1, 400
        salinityseries(:,i) = reshape(transpose(sal_c5(:,:,1,1,i)),[15*12])
    end do

    call plots2(nnfile = 'SSH_differences_and_Salinity',oopt = 'otops',x = 1.5,y = -height,h = 'SSH differences and Salinity')
    call plotsave('first')

    call symbolc(width/2.,height+0.1,0.6,'ESA-OKU')
    call mod12_memori(180,width,symbol_size = 0.3,num_freq = 6,gap = 2,dxval = dx)
    call floating_lines(height,90.,15,3,dx*12.,x = dx*3./2.,dashy = -6,b = 1.) ! feb
    call floating_lines(height,90.,15,3,dx*12.,x = dx*19./2.,dashy = -6,b = 1.) ! oct
    call butler_linegraph(reshape(transpose(esa_oku),[15*12]),width,height,-200.,200.,memiter = 9,maskbelow = 0.,memscale = 0.1,mem = .true.,lthick = 4)
    
    call plotback('first');call plot(0.,-height-1.2,-3)

    call symbolc(width/2.,height+0.1,0.6,'TAP-MAT')
    call mod12_memori(180,width,symbol_size = 0.3,num_freq = 6,gap = 2,dxval = dx)
    call floating_lines(height,90.,15,3,dx*12.,x = dx*3./2.,dashy = -6,b = 1.) ! feb
    call floating_lines(height,90.,15,3,dx*12.,x = dx*19./2.,dashy = -6,b = 1.) ! oct
    call butler_linegraph(reshape(transpose(tap_mat),[15*12]),width,height,-100.,100.,memiter = 5,maskbelow = 0.,memscale = 0.1,mem = .true.,lthick = 4,LI = .true.)

    call plotback('first');call plot(0.,(-height-1.2)*2.,-3)

    call symbolc(width/2.,height+0.1,0.6,'SAK-TOB')
    call mod12_memori(180,width,symbol_size = 0.3,num_freq = 6,gap = 2,dxval = dx)
    call floating_lines(height,90.,15,3,dx*12.,x = dx*3./2.,dashy = -6,b = 1.) ! feb
    call floating_lines(height,90.,15,3,dx*12.,x = dx*19./2.,dashy = -6,b = 1.) ! oct
    call butler_linegraph(reshape(transpose(sak_tob),[15*12]),width,height,-150.,150.,memiter = 7,maskbelow = 0.,memscale = 0.1,mem = .true.,lthick = 4)

    call plotback('first');call plot(0.,(-height-1.2)*2.-1.5,-3)

    call symbolc(width/2.,0.2,0.6,'Salinity')
    call mod12_memori(180,width,symbol_size = 0.3,num_freq = 6,gap = 2,dxval = dx,y = -height2)
    call butler_psk(salinityseries,width,-height2,0.,33.8,34.3,0.05,'b2r',10,bpt1 = 5,r = r1,g = g1,b = b1,conti = 34.,continc = -0.1)
    call floating_lines(height2,90.,15,3,dx*12.,x = dx*3./2.,dashy = -6,b = 1.,y = -height2) ! feb
    call floating_lines(height2,90.,15,3,dx*12.,x = dx*19./2.,dashy = -6,b = 1.,y = -height2) ! oct

    call colorscale(10,r1,g1,b1,33.5,34.,2,0.6,1,height2,0.2,lt = 1, gt = 1,rangle = 90.,symbol_start = 2,x = -.5,y = -height2/2.,TorB = 'T')

    ! call plot(0.,-heigh)


    call plote

end program 
