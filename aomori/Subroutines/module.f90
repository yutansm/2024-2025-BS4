module psstat
    implicit none
    logical :: stoff, land, pageend
    integer :: ipage
    real :: xorig, yorig
    integer::ounit = 255
end module psstat

module mypsstat
    integer::savecount = 0,plots2count = 0
    real,parameter::precision = 1.5*10.**(-4)
    real::xn,yn
    character(len=20),dimension(100)::labels
    real,dimension(100)::label_x,label_y
end module mypsstat

module origin
    use psstat
    use mypsstat
    contains
    subroutine plots(x,y,mode,psfile)
        character psfile*(*)
        character*24 date
        character*20 head
        character(len=8) :: date_str, time_str
        integer :: values(8)
        xorig=x
        yorig=y
        ipage=0
        stoff=.false.
        land =.false.
        pageend =.true.
        ! if(lu.le.0) lu=8

        if(mod(mode/2,2).eq.1) stoff=.true.
        if(mod(mode/4,2).eq.1) land =.true.

        if(mod(mode,2).eq.0) then
            open(unit = ounit,file='xys.ps')
        else 
            open(unit = ounit,file=psfile)
        endif

        call date_and_time(values = values)
        write(date_str, '(i4.4, i2.2, i2.2)') values(1), values(2), values(3)
        write(time_str, '(i2.2, i2.2, i2.2)') values(5), values(6), values(7)
        date = trim(date_str) // 'T' // trim(time_str) // 'Z'
        head = 'Yuta Nishimori'

        ! write(ounit,'(a)') "%!PS-Adobe-2.1"
        write(ounit,'(a)') "%!PS-Adobe-3.0"
        write(ounit,'(a)') "%%Creator: PS-LibraryV2 97.08.08" 
        ! write(ounit,'(a)') "%%Creator: Yuta Nishimori" 

        write(ounit,'(a)') "%%Copyright: PS-LibraryV2  Fukuda & Saito & little bit of Yuta" 
        write(ounit,'(2a)') "%%CreationDate: ",date
        write(ounit,'(2a)') "%%Title: ",trim(head)
        
        write(ounit,'(a)') "%%Pages: (atend)"
        write(ounit,'(a)') "%%BoundingBox: 0 0 596 841 "
        write(ounit,'(a)') "%%EndComments: AS CHILDREN OF THE REPUBLIC"
        write(ounit,'(a)') "%%BeginPlolog"
        write(ounit,'(a)') "%%%PS-LibraryV2(Spring-Tulip) define start %%%"
        write(ounit,'(3a)') "/head ( ",trim(head)," ) def" 

        write(ounit,'(a)') "<< /PageSize [596 841] >> setpagedevice" !841 1191
        write(ounit,'(a)') "/np  { newpath } def"
        write(ounit,'(a)') "/mv  { moveto } def"
        write(ounit,'(a)') "/ln  { lineto } def"
        write(ounit,'(a)') "/sh  { show } def"
        write(ounit,'(a)') "/sl  { setlinewidth } def"
        write(ounit,'(a)') "/sn  { stroke newpath } def" 

        write(ounit,'(a)') "/fo  { /Helvetica-Bold  findfont } def"
        write(ounit,'(a)') "/kfo { /GothicBBB-Medium-H  findfont } def"
        ! write(ounit,'(a)') "/jfo { /Ryumin-Light findfont } def"
        ! write(ounit,'(a)') "/shs { /SourceHanSansJP-Regular findfont } def"
        ! write(ounit,'(a)') "/Helvetica-Bold /CIDFont findresource"
        ! write(ounit,'(a)') "/Identity-H /CMap findresource"
        ! write(ounit,'(a)') "composefont /Helvetica-Bold-Japanese def"
        ! write(ounit,'(a)') "/fo  { /Helvetica-Bold-Japanese findfont } def"

        write(ounit,'(a)') "/sf  { scalefont } def"
        write(ounit,'(a)') "/se  { setfont } def"
        write(ounit,'(a)') "/ro  { rotate } def"
        write(ounit,'(a)') "/sd  { setdash } def"
        write(ounit,'(a)') "/st  { stroke } def"
        write(ounit,'(a)') "/sp  { stroke showpage } def"
        write(ounit,'(a)') "/tl  { translate } def"
        write(ounit,'(a)') "/sc  { scale } def"
        write(ounit,'(a)') "/gs  { gsave } def"
        write(ounit,'(a)') "/gr  { grestore } def"
        write(ounit,'(a)') "/crg { setrgbcolor } def"
        write(ounit,'(a)') "/chs { sethsbcolor } def"
        write(ounit,'(a)') "/sg  { setgray } def"
        write(ounit,'(a)') "/av  { add 2 div } def"
        write(ounit,'(a)') "/ed  { exch def } def"
        write(ounit,'(a)') "/atp { arcto pop pop pop pop } def"

        write(ounit,'(a)') "/roundRect {"
        write(ounit,'(a)') "  np width 0.1 sub 0 mv"
        write(ounit,'(a)') "  width 0 width height 0.1 atp"
        write(ounit,'(a)') "  width height 0 height 0.1 atp"
        write(ounit,'(a)') "  0 height 0 0 0.1 atp"
        write(ounit,'(a)') "  0 0 width 0 0.1 atp closepath"
        write(ounit,'(a)') "} def"

        write(ounit,'(a)') "/Stamp {"
        write(ounit,'(a)') "  fo 0.3 sf se"
        write(ounit,'(a)') "  gs clippath pathbbox gr"
        write(ounit,'(a)') "  pop /urx ed /lly ed pop"
        write(ounit,'(a)') "  urx 0.2 sub lly 0.2 add"
        write(ounit,'(a)') "  gs"
        write(ounit,'(a)') "    tl" 
        write(ounit,'(a)') "    0.01 sl"
        write(ounit,'(a)') "    0 0 mv" 
        write(ounit,'(a)') "    3 string cvs dup false charpath" 
        write(ounit,'(a)') "    ( : Page ) dup false charpath" 
        write(ounit,'(a)') "    head dup false charpath" 
        write(ounit,'(a)') "    pathbbox" 
        write(ounit,'(a)') "    /y1 ed /x1 ed /y0 ed /x0 ed"
        write(ounit,'(a)') "    /width x1 x0 sub 0.2 add def"
        write(ounit,'(a)') "    /height y1 y0 sub 0.2 add def"
        write(ounit,'(a)') "    x1 neg 0 tl"
        write(ounit,'(a)') "    gs"
        write(ounit,'(a)') "      x0 0.1 sub y0 0.1 sub tl roundRect"
        write(ounit,'(a)') "      st"
        write(ounit,'(a)') "    gr"
        write(ounit,'(a)') "    0 0 mv"
        write(ounit,'(a)') "    sh sh sh"
        write(ounit,'(a)') "  gr"
        write(ounit,'(a)') "} def"

        write(ounit,'(a)') "/tocm { 72.0 2.54 div dup sc } def"
        write(ounit,'(a)') "/a4x 21.0 def"
        write(ounit,'(a)') "/a4y 29.7 def"
        ! write(ounit,'(a)') "/tocm { 72.0 2.54 div dup sc } def"
        ! write(ounit,'(a)') "/a4x 29.7 def"
        ! write(ounit,'(a)') "/a4y 42.0 def"

        write(ounit,'(a)') "/landscape {  90.0 ro 0.0 a4x neg tl } def" 

        write(ounit,'(a)') "/boxf " 
        write(ounit,'(a)') "{-0.5 -0.5 rmoveto"
        write(ounit,'(a)') "  0.0  1.0 rlineto" 
        write(ounit,'(a)') "  1.0  0.0 rlineto"
        write(ounit,'(a)') "  0.0 -1.0 rlineto"
        write(ounit,'(a)') " -1.0  0.0 rlineto"
        write(ounit,'(a)') "  closepath"
        write(ounit,'(a)') "  fill"
        write(ounit,'(a)') "} def"

        write(ounit,'(a)') "/box " 
        write(ounit,'(a)') "{-0.5 -0.5 rmoveto"
        write(ounit,'(a)') "  0.0  1.0 rlineto" 
        write(ounit,'(a)') "  1.0  0.0 rlineto"
        write(ounit,'(a)') "  0.0 -1.0 rlineto"
        write(ounit,'(a)') " -1.0  0.0 rlineto"
        write(ounit,'(a)') "  closepath   "
        write(ounit,'(a)') "} def     "

        write(ounit,'(a)') "/circle  { 0.0 360.0 arc st } def  "
        write(ounit,'(a)') "/circle2 { 0.0 360.0 arc st } def  "

        write(ounit,'(a)') "/plus"
        write(ounit,'(a)') "{ 0.0 -0.5 rmoveto"
        write(ounit,'(a)') "  0.0  1.0 rlineto"
        write(ounit,'(a)') " -0.5 -0.5 rmoveto"
        write(ounit,'(a)') "  1.0  0.0 rlineto"
        write(ounit,'(a)') "} def"

        write(ounit,'(a)') "/X"
        write(ounit,'(a)') "{   45.0   ro" 
        write(ounit,'(a)') "  0.0 -0.5 rmoveto"
        write(ounit,'(a)') "  0.0  1.0 rlineto"
        write(ounit,'(a)') " -0.5 -0.5 rmoveto"
        write(ounit,'(a)') "  1.0  0.0 rlineto"
        write(ounit,'(a)') "   -45.0   ro"
        write(ounit,'(a)') "} def"

        write(ounit,'(a)') "/star "
        write(ounit,'(a)') "{ 0.0    0.5   rmoveto"
        write(ounit,'(a)') "  0.105 -0.35  rlineto"
        write(ounit,'(a)') "  0.36   0.0   rlineto"
        write(ounit,'(a)') " -0.3   -0.21  rlineto"
        write(ounit,'(a)') "  0.1   -0.35  rlineto"
        write(ounit,'(a)') " -0.27   0.205 rlineto"
        write(ounit,'(a)') " -0.27  -0.205 rlineto"
        write(ounit,'(a)') "  0.1    0.35  rlineto"
        write(ounit,'(a)') " -0.3    0.21  rlineto"
        write(ounit,'(a)') "  0.36   0.0   rlineto"
        write(ounit,'(a)') "  closepath"
        write(ounit,'(a)') "} def"

        write(ounit,'(a)') "/dot { 0.0 360.0 arc fill st } def"
        write(ounit,'(a)') 
        write(ounit,'(a)') "/tranf    "
        write(ounit,'(a)') "{ 0.5 -0.288 rmoveto"
        write(ounit,'(a)') " -0.5  0.866 rlineto"
        write(ounit,'(a)') " -0.5 -0.866 rlineto"
        write(ounit,'(a)') "  1.0  0.0   rlineto"
        write(ounit,'(a)') "  closepath"
        write(ounit,'(a)') "  fill"
        write(ounit,'(a)') "} def"

        write(ounit,'(a)') "/tran     "
        write(ounit,'(a)') "{ 0.5 -0.288 rmoveto"
        write(ounit,'(a)') " -0.5  0.866 rlineto"
        write(ounit,'(a)') " -0.5 -0.866 rlineto"
        write(ounit,'(a)') "  1.0  0.0   rlineto"
        write(ounit,'(a)') "  closepath"
        write(ounit,'(a)') "} def"

        write(ounit,'(a)') "/PslNewPage {"
        write(ounit,'(a)') " save"
        write(ounit,'(a)') " tocm " 
        write(ounit,'(a)') " 2 setlinejoin"
        write(ounit,'(a)') " 0.00  sl"
        write(ounit,'(a)') " [ ] 0 sd"  
        write(ounit,'(a)') "} def"

        write(ounit,'(a)') "/PslEndPage {"
        write(ounit,'(a)') " st"
        write(ounit,'(a)') " showpage"
        write(ounit,'(a)') " restore"
        write(ounit,'(a)') "} def"
        write(ounit,'(a)') "%%EndPlolog"
        write(ounit,'(a)') "%%% PS-Library define end %%%"

        call newpage

        return
    end
    subroutine plots2(psfile,mode)
        character(len=*),intent(in),optional:: psfile,mode
        integer::intmode
        character(len=3)::countstr

        plots2count = plots2count + 1
        ! ounit = ounit - 1
        write(countstr,'(i3)') plots2count
        countstr = adjustl(countstr)
        if(present(mode))then
            if(mode == 'land'.or.mode == 'landscape')then
                intmode = 13
            else
                intmode = 9
            end if
        else
            intmode = 13
        end if
        if(present(psfile))then
            call plots(0.,0.,intmode,psfile)
        else;
            call plots(0.,0.,intmode,'/Users/yuta/LABWORK/2024-2025-BS4/aomori/nonames/'//trim(countstr)//'.ps')
        end if
        return
    end 
    subroutine newpage
        call endpage
        call inipage      

        ! call plot(0.,0.,-3)

        return
    end
    subroutine plote
        call endpage
        write(ounit,'(a)') "%%Trailer"
        write(ounit,'(a,i3)') "%%Pages: ",ipage
        write(ounit,'(a,i3)') "%%EOF"
        close(ounit)
        return
    end 
    subroutine plot(x1,y1,im)
        use ieee_arithmetic

        if(ieee_is_nan(x1))then;print*,'x1 is NaN';stop;endif
        if(ieee_is_nan(y1))then;print*,'y1 is NaN';stop;endif
        if(ieee_is_finite(x1).eqv. .false.)then;print*,'x1 is Inf';stop;endif
        if(ieee_is_finite(y1).eqv..false.)then;print*,'y1 is Inf';stop;endif

        ! if(land.eqv..true.)then
            if(x1+xn>29.7)then;print*,'definitely drawing out of paper ','xn=',xn,'x1=',x1;stop;endif
            if(y1+yn>29.7)then;print*,'definitely drawing out of paper ','yn=',yn,'y1=',y1;stop;endif
        ! else
        !     if(x1+xn>21.0)then;print*,'drawing out of paper ','xn=',xn,'x1=',x1;stop;endif
        !     if(y1+yn>29.7)then;print*,'drawing out of paper ','yn=',yn,'y1=',y1;stop;endif
        ! end if
        
        if(im.eq.3) then 
            write(ounit, '(a)') " sn"
            write(ounit, *) x1 
            ! write(ounit, '(2x)')  ! Two spaces
            write(ounit, *) y1  
            write(ounit, '(2x,a3)') " mv"  ! two spaces and move
        elseif(im.eq.2) then
            write(ounit, *) x1 
            ! write(ounit, '(2x)')  ! Two spaces
            write(ounit, *) y1  
            write(ounit, '(2x,a3)') " ln"  ! two spaces and move
        elseif(im.eq.-3) then
            xn = xn + x1; yn = yn + y1
            write(ounit, *) x1 
            ! write(ounit, '(2x)')  ! Two spaces
            write(ounit, *) y1  
            write(ounit, '(2x,a3)') " tl"  ! two spaces and move
            write(ounit, '(a)') " sn"
            write(ounit, '(a)') " 0.0 0.0 mv"
        endif
        return
    end
    subroutine plotsave(label)
        character(len=*), intent(in) :: label
        if(len_trim(label)>20)then;print*,'your label is too long';stop;endif
        if(savecount>100)then;print*,'you have too many labels';stop;endif
        write(ounit,*) '% begin plotsave'
        savecount = savecount +1
        labels(savecount) = trim(label)
        label_x(savecount) = xn;label_y(savecount) = yn
        ! print*,'label',savecount,'=',labels(savecount)
        write(ounit,*) '% end plotsave'
        return
    end 
    subroutine plotback(label)
        character(len=*), intent(in) :: label 
        do i = 1, savecount
            if(labels(i)==label)then
                call plot(label_x(i)-xn,label_y(i)-yn,-3)
                exit
            endif
            if(i == savecount .and. labels(i)/=label)then
                print*,'label not found'
                stop
            end if
        end do
    end 
    ! x and y are ratios
    subroutine plotmove(x,y)
        real,intent(in)::x,y
        if(x>1.)print*,'x /> 1, x is the ratio'
        if(y>1.)print*,'y /> 1, y is the ratio'
        if(land .eqv. .true.)then
            call plot(-xn,-yn,-3);call plot(29.7*x,21.*y,-3)
        else;call plot(-xn,-yn,-3);call plot(21.*x,29.7*y,-3)
        end if
        return
    end
    subroutine plotmove2(x,y)
        real,intent(in)::x,y
        if(land.eqv..true.)then  
            if(x>29.7)print*,'x > 29.7, x is the distance in cm from 0,0'
            if(y>21.0)print*,'y > 21.0, y is the distance in cm from 0,0'
        else
            if(x>21.0)print*,'x > 21.0, x is the distance in cm from 0,0'
            if(y>29.7)print*,'y > 29.7, y is the distance in cm from 0,0'
        end if
            call plot(-xn,-yn,-3);call plot(x,y,-3)
        return
    end
    ! y is the ratio
    subroutine header(head_str,symbol_size,rangle,y)
        implicit none
        character(len=*),intent(in)::head_str
        real,intent(in),optional::symbol_size,y,rangle
        real::xnn,ynn

        xnn = xn;ynn = yn
        write(ounit,*) '% begin header'
        if(land)then;
            if(present(y))then
                call plotmove(0.5,y)
            else 
                call plotmove(0.5,0.93)
            end if
        else
            if(present(y))then
                call plotmove(0.5,y)
            else 
                call plotmove(0.5,0.95)
            end if
        end if

        if(present(symbol_size))then
            if(present(rangle))then
                call symbolc(0.,0.,symbol_size,head_str,rangle)
            else 
                call symbolc(0.,0.,symbol_size,head_str,0.)
            end if
        else
            if(present(rangle))then
                call symbolc(0.,0.,1.,head_str,rangle)
            else
                call symbolc(0.,0.,1.,head_str,0.)
            end if
        end if
        call plotmove2(xnn,ynn)
    end
    !x and y are diffs (in cm) from static point
    subroutine otops(x,y)
        implicit none
        real,intent(in),optional::x,y

        if(land)then
            if(present(x))then
                if(present(y))then
                    call plotmove2(1.5+x,21.*0.93-1.5+y)
                else
                    call plotmove2(1.5+x,21.*0.93-1.5)
                end if
            else
                if(present(y))then
                    call plotmove2(1.5,21.*0.93-1.5+y)
                else
                    call plotmove2(1.5,21.*0.93-1.5)
                end if
            end if
        else
            if(present(x))then
                if(present(y))then
                    call plotmove2(1.5+x,29.7*0.95-1.5+y)
                else
                    call plotmove2(1.5+x,29.7*0.95-1.5)
                end if
            else 
                if(present(y))then
                    call plotmove2(1.5,29.7*0.95-1.5+y)
                else
                    call plotmove2(1.5,29.7*0.95-1.5)
                end if
            end if
        end if
    end
    subroutine ocenter(x,y)
        implicit none
        real,intent(in),optional::x,y

        if(land)then
            if(present(x))then
                if(present(y))then
                    call plotmove2(29.7*0.5+x,21.*0.5+y)
                else
                    call plotmove2(29.7*0.5+x,21.*0.5)
                end if
            else
                if(present(y))then
                    call plotmove2(29.7*0.5,21.*0.5+y)
                else
                    call plotmove2(29.7*0.5,21.*0.5)
                end if
            end if
        else
            if(present(x))then
                if(present(y))then
                    call plotmove2(21.*0.5+x,29.7*0.5+y)
                else
                    call plotmove2(21.*0.5+x,29.7*0.5)
                end if
            else
                if(present(y))then
                    call plotmove2(21.*0.5,29.7*0.5+y)
                else
                    call plotmove2(21.*0.5,29.7*0.5)
                end if
            end if
        end if
    end 
    subroutine obottoms(x,y)
        implicit none
        real,intent(in),optional::x,y

        if(land)then
            if(present(x))then
                if(present(y))then
                    call plotmove2(1.5+x,1.5+y)
                else
                    call plotmove2(1.5+x,1.5)
                end if
            else
                if(present(y))then
                    call plotmove2(1.5,1.5+y)
                else
                    call plotmove2(1.5,1.5)
                end if
            end if
        else
            if(present(x))then
                if(present(y))then
                    call plotmove2(1.5+x,1.5+y)
                else
                    call plotmove2(1.5+x,1.5)
                end if
            else 
                if(present(y))then
                    call plotmove2(1.5,1.5+y)
                else
                    call plotmove2(1.5,1.5)
                end if
            end if
        end if

    end
    subroutine inipage
            if(.not.pageend) return

            ipage=ipage+1
            write(ounit,'(a,2i3)') "%%Page:",ipage,ipage
            if(land) then 
                write(ounit,'(a)') "%%PageOrientation: Landscape"
            else
                write(ounit,'(a)') "%%PageOrientation: Portrait"
            endif
            write(ounit,'(a,2i2)') "%%BeginPageSetup:"
            write(ounit,'(a)') "PslNewPage"
            if(land) then 
                write(ounit,'(a)') " landscape"  
            endif
            write(ounit,'(a,2i2)') "%%EndPageSetup:"
            write(ounit,'(2f9.4,a)') xorig,yorig," tl" 
            write(ounit,'(a)') " np 0.0 0.0  mv" 
            pageend=.false.
            xn = xorig;yn = yorig
            return
    end      
    subroutine endpage     
        if(pageend) return
        write(ounit,'(a)') "PslEndPage"
        pageend=.true.
                    
        return
    end
    subroutine rgbK(red,gre,blu)     
        real red,gre,blu
        call plot(0.0,0.0,3)
        if(red.gt.1.0) stop 'argument error in rgb !!!'
        if(red.lt.0.0) stop 'argument error in rgb !!!'
        if(gre.gt.1.0) stop 'argument error in rgb !!!'
        if(gre.lt.0.0) stop 'argument error in rgb !!!'
        if(blu.gt.1.0) stop 'argument error in rgb !!!'
        if(blu.lt.0.0) stop 'argument error in rgb !!!'
        write(ounit,*) "% begin rgb " 
        write(ounit,'(3f9.4,a4)' ) red ,gre ,blu      ,' crg' 
        write(ounit,*) "% end rgb "
        return
    end
    subroutine betmlK(x,y,m,n,red,gre,blu) 
            dimension x(n),y(n)
            real red,gre,blu
            write(ounit,*) 'newpath'
            call rgbK(red,gre,blu) 
            ! call newpen2(1)
            call plot(x(1),y(1),3)
            do 10 i=2 , m 
            call plot(x(i),y(i),2)
        10      continue 
            call plot(x(1),y(1),2)
            write(ounit,*) 'closepath'
            write(ounit,*) 'fill'
            call color(0)
            return
    end
    subroutine color(ic)   
        integer    ic    
        real red,gre,blu
        write(ounit,*) "% begin color " ,ic
        red = 0.0
        gre = 0.0
        blu = 0.0
        if(ic.eq.1) red = 1.0 
        if(ic.eq.5) red = 1.0 
        if(ic.eq.6) red = 1.0 
        if(ic.eq.7) red = 1.0
        if(ic.eq.2) gre = 1.0
        if(ic.eq.4) gre = 1.0
        if(ic.eq.5) gre = 1.0
        if(ic.eq.7) gre = 1.0
        if(ic.eq.3) blu = 1.0 
        if(ic.eq.4) blu = 1.0 
        if(ic.eq.6) blu = 1.0 
        if(ic.eq.7) blu = 1.0
        write(ounit,'(3f9.4,a4)' ) red ,gre ,blu      ,' crg' 
        write(ounit,*) "% end color "
        return
    end
    subroutine newpen(ip)
        if((ip.ge.4).or.(ip.le.-4)) then
        write(6,*) '--< Attention >--'
        write(6,*) ' You have to re-write [newpen] --> [newpen2]'
        end if
        if((ip.ge.4).or.(ip.le.-4)) return
        write(ounit,*) "% begin newpen " ,ip
        write(ounit,*) "sn"
        if (ip.ge. 0) write(ounit,*) "[] 0 sd"
            if (ip.eq.1) write(ounit,*) ' 0.01  sl '
            if (ip.eq.2) write(ounit,*) ' 0.03  sl '
            if (ip.eq.3) write(ounit,*) ' 0.07  sl '
            if(ip.eq.-1) write(ounit,*) "[0.4 0.1] 0 sd"
            if(ip.eq.-2) write(ounit,*) "[0.2 0.1] 0 sd"
            if(ip.eq.-4) write(ounit,*) "[0.6 0.1] 0 sd"
            if(ip.eq.-3) write(ounit,*) "[0.1 0.2] 0 sd"
            if(ip.eq.-5) write(ounit,*) "[0.1 0.1] 0 sd"
        write(ounit,*) "% end newpen"
        return
    end
    subroutine newpen2(ip)
        write(ounit,*) "% begin newpen2 " ,ip
        write(ounit,*) "sn"
        if (ip.ge. 0) write(ounit,*) "[] 0 sd"
        if (ip.eq. 7) write(ounit,*) ' 0.10  sl '
        if (ip.eq. 6) write(ounit,*) ' 0.08  sl '
        if (ip.eq. 5) write(ounit,*) ' 0.06  sl '
        if (ip.eq. 4) write(ounit,*) ' 0.04  sl '
        if (ip.eq. 3) write(ounit,*) ' 0.02  sl '
        if (ip.eq. 2) write(ounit,*) ' 0.01  sl '
        if (ip.eq. 1) write(ounit,*) ' 0.00  sl '
        if (ip.eq.-1) write(ounit,*) "[0.6 0.1] 0 sd"
        if (ip.eq.-2) write(ounit,*) "[0.4 0.1] 0 sd"
        if (ip.eq.-3) write(ounit,*) "[0.2 0.1] 0 sd"
        if (ip.eq.-4) write(ounit,*) "[0.2 0.2] 0 sd"
        if (ip.eq.-5) write(ounit,*) "[0.1 0.1] 0 sd"
        if (ip.eq.-6) write(ounit,*) "[0.05 0.05] 0 sd"
        if (ip.eq.-7) write(ounit,*) "[0.02 0.02] 0 sd"
        write(ounit,*) "% end newpen2"
        return
    end
    subroutine gmark(xp,yp,hi,markty)
        character*8 marka(0:13)
        real xp,yp,hi,hi2
        integer markty
        data marka / "        " , "dot     ",&
                    "plus    " , "star    ","circle  ","X       ", &
                    "tranf   ",  "tran    ","boxf    ","box     ", &
                    "circle2 ",  "        ","        ","        " /
        ! c     mark type 

        write(ounit,*) "% begin gmark "
        if(markty.eq.1.or.markty.eq.4)then 
        write(ounit,*)" sn "
        write(ounit,*) " gs "
        hi2 = hi/2.0
        write(ounit,' (3f12.6) ' ) xp,yp,hi2
        write(ounit,*) marka (markty)
        write(ounit,*) " st "
        write(ounit,*) " gr "
        ! c     update   93/07/10
        call plot(xp,yp,3)
        else  
        write(ounit,*)" sn "
        call plot(xp,yp,3)
        write(ounit,*) " gs "
        write(ounit,'(2f10.5,2x,a4)') hi ,hi, " sc "
        write(ounit,*) marka (markty)
        write(ounit,*) " st "
        write(ounit,*) " gr "
        endif
        write(ounit,*) "% end gmark "
        return
    end
    subroutine arohd(x0,y0,x1,y1,al,aw,ic)

            if(x0.eq.x1.and.y0.eq.y1) return

            write(ounit,*) "% begin arohd"
            xo=x0
            yo=y0
            xd=x1
            yd=y1

            i=ic/10
            j=mod(ic,10)

            dx=xd-xo
            dy=yd-yo
            r0=sqrt(xo*xo+yo*yo)
            dr=sqrt(dx*dx+dy*dy)

            pi=acos(-1.)
            theta=atan2(dy,dx)/pi*180.
        
            write(ounit, '(a)') " sn"
            if(i.eq.0) then
                write(ounit, '(2(f9.4,2x),a3)')  xd,yd," mv"
            else
                write(ounit, '(2(f9.4,2x),a3)')  xo ,yo," mv"
                write(ounit, '(2(f9.4,2x),a3)')  xd,yd," ln"
            endif

        10   write(ounit, '(2(f9.4,2x),a3)')  xd,yd," tl"
            write(ounit, '(f9.4,a3)')  theta," ro"
            write(ounit, '(a)') " sn 0.0 0.0 mv" 

            if(j.eq.7) then
                write(ounit, '(2(f9.4,2x),a3)')  0., 0.," mv"      
                write(ounit, '(2(f9.4,2x),a3)') -al,-aw," ln"      
                write(ounit, '(2(f9.4,2x),a3)') -al, 0.," ln"      
                write(ounit, '(2(f9.4,2x),a3)')  0., 0.," ln"      
                write(ounit, *) " gs" 
                write(ounit,'(f9.4,a)') 0.0, " sg fill"
                write(ounit, *) " gr st"
            elseif(j.eq.6) then
                write(ounit, '(2(f9.4,2x),a3)')  0., 0.," mv"      
                write(ounit, '(2(f9.4,2x),a3)') -al, aw," ln"      
                write(ounit, '(2(f9.4,2x),a3)') -al,-aw," ln"      
                write(ounit, *) " gs" 
                write(ounit,'(f9.4,a)') 0.0, " sg fill"
                write(ounit, *) " gr st"
            elseif(j.eq.5) then
                write(ounit, '(2(f9.4,2x),a3)')  0., 0.," mv"      
                write(ounit, '(2(f9.4,2x),a3)') -al,-aw," ln"      
                write(ounit, '(2(f9.4,2x),a3)')  0., 0.," mv"      
                write(ounit, '(2(f9.4,2x),a3)') -al, 0.," ln"      
            elseif(j.eq.4) then
                write(ounit, '(2(f9.4,2x),a3)')  0., 0.," mv"      
                write(ounit, '(2(f9.4,2x),a3)') -al,-aw," ln"      
                write(ounit, '(2(f9.4,2x),a3)')  0., 0.," mv"      
                write(ounit, '(2(f9.4,2x),a3)') -al, aw," ln"      
                write(ounit, '(2(f9.4,2x),a3)')  0., 0.," mv"      
                write(ounit, '(2(f9.4,2x),a3)') -al, 0.," ln"      
            elseif(j.eq.3) then
                write(ounit, '(2(f9.4,2x),a3)')  0., 0.," mv"      
                write(ounit, '(2(f9.4,2x),a3)') -al,-aw," ln"      
                write(ounit, '(2(f9.4,2x),a3)') -al, 0.," ln"
                write(ounit, '(2(f9.4,2x),a3)')  0., 0.," ln"            
            elseif(j.eq.2) then
                write(ounit, '(2(f9.4,2x),a3)')  0., 0.," mv"      
                write(ounit, '(2(f9.4,2x),a3)') -al, aw," ln"      
                write(ounit, '(2(f9.4,2x),a3)') -al,-aw," ln"      
                write(ounit, '(2(f9.4,2x),a3)')  0., 0.," ln"      
                write(ounit, '(2(f9.4,2x),a3)') -al, 0.," ln"      
            else
                write(ounit, '(2(f9.4,2x),a3)')  0., 0.," mv"      
                write(ounit, '(2(f9.4,2x),a3)') -al, aw," ln"      
                write(ounit, '(2(f9.4,2x),a3)') -al,-aw," ln"      
                write(ounit, '(2(f9.4,2x),a3)')  0., 0.," ln"      
                write(ounit, *) " gs" 
                write(ounit,'(f9.4,a)') 1.0, " sg fill"
                write(ounit, *) " gr st"
            endif
        
            write(ounit, '(f9.4,a3)') -theta," ro"
            write(ounit, '(2(f9.4,2x),a3)') -xd,-yd," tl"
        
            if(i.ge.2) then
                xo=x1
                yo=y1
                xd=x0
                yd=y0
                i=1
                theta=theta+180
                go to 10
            endif
            write(ounit,*) "% end arohd"
            return
    end
    subroutine factor(fct)
        write(ounit, '(2f9.4,2x,a4)') fct,fct , " sc "
        return
    end 
    subroutine number(x,y,h,anu,ang,n)
        character isymb*16,form*16
        ! c
        write(ounit,*) "% begin number"
        zero= 0.0
        one = 1.0
        ten =10.0
        if(n.lt.0) then
            np=0
            nd=int(ten)**abs(n+1)
            tmp=anu/nd
            if(tmp.lt.-one) then
        ! c           nf=log10(-tmp)+1
                pnf=log10(-tmp)
                nf = int(pnf)+1
                np=np+1
            elseif(tmp.lt.zero) then
                nf=1
                np=np+1
            elseif(tmp.lt.one) then
                nf=1
                np=np
            else
        ! c           nf=log10(tmp)+1
                pnf=log10(tmp)
                nf = int(pnf)+1
            endif
            nw=nf+np
            write(form,'(a2,i2,a1)') '(i',nw,')'
            itmp=int(tmp)
            write(isymb,form) itmp
        else
            np=1
            tmp=anu
            if(tmp.lt.-one) then
        ! c           nf=log10(-tmp)+1
                pnf=log10(-tmp)
                nf = int(pnf)+1
                np=np+1
            elseif(tmp.lt.zero) then
                nf=1
                np=np+1
            elseif(tmp.lt.one) then
                nf=1
                np=np
            else
        ! c           nf=log10(tmp)+1
                pnf=log10(tmp)
                nf = int(pnf)+1
                np=np
            endif
            nw=nf+np+n
            write(form,'(a2,i2,a1,i2,a1)') '(f',nw,'.',n,')'
            write(isymb,form) tmp
        endif

        write(ounit,*) "fo"
        write(ounit,10) h
        10   format(f8.4," sf")
        write(ounit,*) "se"
        if(x.ge.9999.0.or.y.ge.9999.0)then
        else
            write(ounit,'(2f10.4,2x,a3)' ) x,y, " mv" 
        endif 
        write(ounit,'(f10.4,2x,a4)' ) ang , ' ro ' 
        write(ounit,*) "(",isymb(1:nw),") sh" 
        write(ounit,'(f9.4,2x,a4)' ) -ang , ' ro ' 
        write(ounit,*) "% end number"
        return
    end
    subroutine numberc(x,y,h,anu,ang,n)
        character isymb*16,form*16
        ! c
        write(ounit,*) "% begin numberc"
        zero= 0.0
        one = 1.0
        ten =10.0
        if(n.lt.0) then
            np=0
            nd=int(ten)**abs(n+1)
            tmp=anu/nd
            if(tmp.lt.-one) then
        ! c           nf=log10(-tmp)+1
                pnf=log10(-tmp)
                nf = int(pnf)+1
                np=np+1
            elseif(tmp.lt.zero) then
                nf=1
                np=np+1
            elseif(tmp.lt.one) then
                nf=1
                np=np
            else
        ! c           nf=log10(tmp)+1
                pnf=log10(tmp)
                nf = int(pnf)+1
            endif
            nw=nf+np
            write(form,'(a2,i2,a1)') '(i',nw,')'
            itmp=int(tmp)
            write(isymb,form) itmp
        else
            np=1
            tmp=anu
            if(tmp.lt.-one) then
        ! c           nf=log10(-tmp)+1
                pnf=log10(-tmp)
                nf = int(pnf)+1
                np=np+1
            elseif(tmp.lt.zero) then
                nf=1
                np=np+1
            elseif(tmp.lt.one) then
                nf=1
                np=np
            else
        ! c           nf=log10(tmp)+1
                pnf=log10(tmp)
                nf = int(pnf)+1
                np=np
            endif
            nw=nf+np+n
            write(form,'(a2,i2,a1,i2,a1)') '(f',nw,'.',n,')'
            write(isymb,form) tmp
        endif

        write(ounit,*) "fo"
        write(ounit,10) h
        10   format(f8.4," sf")
        write(ounit,*) "se"
        if(x.ge.9999.0.or.y.ge.9999.0)then
        else
            write(ounit,'(2f10.4,2x,a3)' ) x,y, " mv" 
        endif 
        write(ounit,'(f10.4,2x,a4)' ) ang , ' ro '
        write(ounit,*) "(",isymb(1:nw),") stringwidth pop " 
        write(ounit,*) 'neg 2 div 0 rmoveto '
        write(ounit,*) "(",isymb(1:nw),") sh" 
        write(ounit,'(f9.4,2x,a4)' ) -ang , ' ro ' 
        write(ounit,*) "% end numberc"
        return
    end
    subroutine numberr(x,y,h,anu,ang,n)
        character isymb*16,form*16
        ! c
        write(ounit,*) "% begin numberr"
        zero= 0.0
        one = 1.0
        ten =10.0
        if(n.lt.0) then
            np=0
            nd=int(ten)**abs(n+1)
            tmp=anu/nd
            if(tmp.lt.-one) then
        ! c           nf=log10(-tmp)+1
                pnf=log10(-tmp)
                nf = int(pnf)+1
                np=np+1
            elseif(tmp.lt.zero) then
                nf=1
                np=np+1
            elseif(tmp.lt.one) then
                nf=1
                np=np
            else
        ! c           nf=log10(tmp)+1
                pnf=log10(tmp)
                nf = int(pnf)+1
            endif
            nw=nf+np
            write(form,'(a2,i2,a1)') '(i',nw,')'
            itmp=int(tmp)
            write(isymb,form) itmp
        else
            np=1
            tmp=anu
            if(tmp.lt.-one) then
        ! c           nf=log10(-tmp)+1
                pnf=log10(-tmp)
                nf = int(pnf)+1
                np=np+1
            elseif(tmp.lt.zero) then
                nf=1
                np=np+1
            elseif(tmp.lt.one) then
                nf=1
                np=np
            else
        ! c           nf=log10(tmp)+1
                pnf=log10(tmp)
                nf = int(pnf)+1
                np=np
            endif
            nw=nf+np+n
            write(form,'(a2,i2,a1,i2,a1)') '(f',nw,'.',n,')'
            write(isymb,form) tmp
        endif

        write(ounit,*) "fo"
        write(ounit,10) h
        10   format(f8.4," sf")
        write(ounit,*) "se"
        if(x.ge.9999.0.or.y.ge.9999.0)then
        else
            write(ounit,'(2f10.4,2x,a3)' ) x,y, " mv" 
        endif 
        write(ounit,'(f10.4,2x,a4)' ) ang , ' ro ' 
        write(ounit,*) "(",isymb(1:nw),") stringwidth pop " 
        write(ounit,*) 'neg 0 rmoveto '
        write(ounit,*) "(",isymb(1:nw),") sh" 
        write(ounit,'(f9.4,2x,a4)' ) -ang , ' ro ' 
        write(ounit,*) "% end numberr"
        return
    end
    subroutine pscont3(dx,dy,a,ib,ips,ipe,jps,jpe &
                                    ,mx,my,icnu,contst,contint)
        dimension conta(1000),a(mx,my),ib(mx,my),x(5),y(5)
        !-------------------schematic of contour-----------------------
        !---> x direction is (i)
        ! c   |  ---      a(i,j)-----x(1),y(1)-----a(i+1,j)
        ! c   |   |         |                         |
        ! c   |   |         |                         |
        ! c   |   |         |                         |
        ! c   y   dy    x(2),y(2)                 x(4),y(4)
        ! c       |         |                         |
        ! c   i   |         |                         |
        ! c   s   |         |                         |
        ! c      ---     a(i,j+1)----x(3),y(3)-----a(i+1,j+1)
        ! c  (j)            |<-----------dx---------->|
        ! c
        ! c       | 
        ! c       |<-yycop
        ! c       |____xxcop_
        ! c
        ! c********correct of x and y axis**********
        !-----------------------
            pxm=-dx*float(ips-1)
            pym=-dy*float(jps-1)
        !-----------------------
            xxcop= 0.5*dx + pxm
            yycop= 0.5*dy + pym
        ! c***********************
        !------ The positions of contour line are 1~4x,y 
        ! c            The contour lines are drawn by these position
        !---------------------------------------
        !------setting the value of contour line
        do 200 i=1,icnu
        conta(i)=contst+float(i-1)*contint
        200  continue

            x5=0.
            y5=0.
            x4=0.
            y4=0.
        ! c
            xdx1=0.
            xdx2=0.
            xdx3=0.
            xdx4=0.
            ydy1=0.
            ydy2=0.
            ydy3=0.
            ydy4=0.
        !---
            do kk=1,4
            x(kk)=0.
            y(kk)=0.
            enddo
        ! c<<<<<<<<<<<<<<<<<<<<<<<<start>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        !------------cont.        
        do 300 k=1,icnu
        !------------y axis        
        do 280 j=jps,jpe-1
        !------------x axis        
        do 270 i=ips,ipe-1
            if ((ib(i,j).eq.0).or.(ib(i+1,j).eq.0) &
                                .or.(ib(i,j+1).eq.0).or.(ib(i+1,j+1).eq.0)) &
                go to 270
        ! !------------compare value of conta. to value of a(i,j)
            if ((a(i,j).eq.conta(k)).and.(a(i+1,j).eq.conta(k)) &
            .and.(a(i,j+1).eq.conta(k)).and.(a(i+1,j+1).eq.conta(k))) then
            go to 270
            end if
            if ((a(i,j).lt.conta(k)).and.(a(i+1,j).lt.conta(k)) &
            .and.(a(i,j+1).lt.conta(k)).and.(a(i+1,j+1).lt.conta(k))) then

            go to 270
            end if
        ! c
            if (a(i,j).eq.conta(k)) then
            a( i , j )=a( i , j )+0.001*contint
            end if
        ! c
            if (a(i+1,j).eq.conta(k)) then
            a(i+1, j )=a(i+1, j )+0.001*contint
            end if
        ! c
            if (a(i,j+1).eq.conta(k)) then
            a( i ,j+1)=a( i ,j+1)+0.001*contint
            end if
            if (a(i+1,j+1).eq.conta(k)) then
            a(i+1,j+1)=a(i+1,j+1)+0.001*contint
            end if
        ! c
        ! c==========search position (1)============
            dex1=a(i,j)-conta(k)
            dex2=a(i+1,j)-conta(k)
            dex3=dex1*dex2
            if (dex3.le.0.) then
            go to 211
            else
            go to 212
            end if
        !-----------
        211     dex4=a(i,j)-a(i+1,j)
            if (dex4.eq.0.) then
            go to 212
            end if

        ! !----------judgment of land and sea
            jnd1=ib(i,j)*ib(i+1,j)
            if (jnd1 .eq. 0) then
            go to 212
            end if
        
        ! c*********position (1)
            x(1)=dx*float(i-1)+abs(dex1)*dx/abs(dex4)+xxcop
            y(1)=dy*float((j-1))+yycop

        !         write(*,*) 'x(1),y(1)',x(1),y(1)
        ! c==========search position (2)================
        212     dex1=a(i,j)-conta(k)
            dex2=a(i,j+1)-conta(k)
            dex3=dex1*dex2
            if (dex3.le.0.) then
            go to 213
            else
            go to 214
            end if
        !-----------
        213     dex4=a(i,j)-a(i,j+1)
            if (dex4.eq.0.) then
            go to 214
            else
            end if

        ! !----------judgment of land and sea
            jnd2=ib(i,j)*ib(i,j+1)
            if (jnd2 .eq. 0) then
            go to 214
            end if
        ! c
        ! c**********position (2)
                x(2)=dx*float(i-1)+xxcop
                y(2)=dy*float(j-1)+abs(dex1)*dy/abs(dex4) &
                    +yycop 

        ! !         write(*,*) 'x(2),y(2)',x(2),y(2)
        ! c==========search position (3)================
        214    dex1=a(i,j+1)-conta(k)
            dex2=a(i+1,j+1)-conta(k)
            dex3=dex1*dex2
            if (dex3.le.0.) then
            go to 215
            else
            go to 216
            end if
        !-----------
        215    dex4=a(i,j+1)-a(i+1,j+1)
            if (dex4.eq.0.) then
            go to 216
            end if
        ! c
        ! !----------judgment of land and sea
            jnd3=ib(i,j+1)*ib(i+1,j+1)
            if (jnd3 .eq. 0) then
            go to 216
            end if
        ! c
        ! c*********position (3)
            x(3)=dx*float(i-1)+abs(dex1)*dx/abs(dex4)+xxcop
            y(3)=dy*float((j))+yycop

        !         write(*,*) 'x(3),y(3)',x(3),y(3)
        ! c==========search position (4)================
        216     dex1=a(i+1,j)-conta(k)
            dex2=a(i+1,j+1)-conta(k)
            dex3=dex1*dex2
            if (dex3.le.0.) then
            go to 217
            else
            go to 240
            end if
        !-----------
        217     dex4=a(i+1,j)-a(i+1,j+1)
            if (dex4.eq.0.) then
            go to 230
            end if
        ! c
        ! !----------judgment of land and sea
            jnd4=ib(i+1,j)*ib(i+1,j+1)
            if (jnd4 .eq. 0) then
            go to 240
            end if
        ! c
        ! c**********position (4)
                x(4)=dx*float(i)+xxcop
                y(4)=dy*float(j-1)+abs(dex1)*dy/abs(dex4) &
                    +yycop
        !         write(*,*) 'x(4),y(4)',x(4),y(4)
        ! c#########################################################
        ! c===========drawing contour line==========================
        ! !-----------case of four positions 
        230    if ((x(1).ne.0.).and.(x(2).ne.0.).and.(x(3).ne.0.).and. &
                (x(4).ne.0.)) then

            go to 235
            else
            go to 240
            end if
        ! c
        235     xlong1=(x(1)-x(2))**2+(y(1)-y(2))**2
            xlong2=(x(1)-x(4))**2+(y(1)-y(4))**2
                if (xlong1.le.xlong2) then
                go to 236
                else
                goto 237
                end if
        ! c
        236   call plot(x(1),y(1),3)
            call plot(x(2),y(2),2)
            call plot(x(3),y(3),3)
            call plot(x(4),y(4),2)
        
            

        !       write(*,*) 1,i,j,x(1),y(1)
        !       write(*,*) 1,i,j,x(2),y(2)
        !       write(*,*) 1,i,j,x(3),y(3)
        !       write(*,*) 1,i,j,x(4),y(4)
        ! C       stop 

            go to 250
        
        237   call plot(x(1),y(1),3)
            call plot(x(4),y(4),2)
            call plot(x(2),y(2),3)
            call plot(x(3),y(3),2)

        !       write(*,*) 2,i,j,x(1),y(1)
        !       write(*,*) 2,i,j,x(2),y(2)
        !       write(*,*) 2,i,j,x(3),y(3)
        !       write(*,*) 2,i,j,x(4),y(4)
        ! C       stop 

            go to 250
        ! c
        ! !-----------else case /  
        240   do 245 kk=1,4
        !        if(kk.eq.1) write(*,*)0,(x(jm),y(jm),jm=1,4) 
                    if (x(kk).eq.0.) then
        !               write(*,*) 'x(kk)',kk,x(kk)
                    go to 245
                    end if
                    x4=x(kk)
                    y4=y(kk)
        !       write(*,*) 'U',kk,x4,y4

                    if (x5.eq.0.) then
                    go to 238
                    end if

            call plot(x4,y4,3)
            call plot(x5,y5,2)

        !        write(*,*) 3,i,j,x4,y4
        !        write(*,*) 3,i,j,x5,y5


        238          x5=x4
                    y5=y4
        !       write(*,*) 'L',kk,x4,y4,x5,y5
                    x(kk)=0.
        245    continue
        250     x5=0.
            y5=0.
            x4=0.
            y4=0.

            xdx1=0.
            xdx2=0.
            xdx3=0.
            xdx4=0.
            ydy1=0.
            ydy2=0.
            ydy3=0.
            ydy4=0.
        !---
            do 260 kk=1,4
            x(kk)=0.
            y(kk)=0.
        260    continue
        !---

        270    continue
        280    continue
        300    continue
        ! c<<<<<<<<<<<<<<<<<<<<<<<<end>>>>>>>>>>>>>>>>>>>>>>>>>>>>
            return
    end
    subroutine pscolorK(px,py,ss,is,ist,ied,jst,jed,ix,iy,cst,ced,rr,gg,bb)
        ! ***********************************************************************
        ! *     cst <==> ced   : Range of painting 
        ! *    (RR,gg,bb)      : RGB color   
            Parameter(nx=20)
                dimension ss(ix,iy),is(ix,iy)
                dimension x(nx),y(nx)


        ! *###################################################################
        ! *################### ZERO STEP  ####################################
        ! *###################################################################

            do i=ist,ied-1 
                lp = 0
            do j=jst,jed-1  

        ! **************************************************************
                IF(is( i , j ).ne.0.and.is(i+1, j ).ne.0 .and.is( i ,j+1).ne.0.and.is(i+1,j+1).ne.0) THEN
        ! **************************************************************

                        s1 = ss( i , j )
                        s2 = ss(i+1, j )
                        s3 = ss( i ,j+1)
                        s4 = ss(i+1,j+1)

        ! *#(1-1)#*-----------
                if(((s1.ge.cst).and.(s1.le.ced)).and.&
                   ((s2.ge.cst).and.(s2.le.ced)).and.&
                   ((s3.ge.cst).and.(s3.le.ced)).and.&
                   ((s4.ge.cst).and.(s4.le.ced))&
                               .and.lp.eq.0.and.j.ne.jed-1) then 
                        lp = 1
                        xx = px*(i-ist) + px/2.0
                        yy = py*(j-jst) + py/2.0
                    xlef = xx
                    ylef = yy

                            is( i , j ) = 2  
                            is(i+1, j ) = 2  
                            is( i ,j+1) = 2  
                            is(i+1,j+1) = 2  

        ! *#(1-2)#*-----------
                    elseif((s1.ge.cst.and.s1.le.ced.and.&
                          s2.ge.cst.and.s2.le.ced.and.&
                          s3.ge.cst.and.s3.le.ced.and.&
                          s4.ge.cst.and.s4.le.ced)&   
                                   .and.lp.eq.1.and.j.ne.jed-1) then 

                            is( i , j ) = 2  
                            is(i+1, j ) = 2  
                            is( i ,j+1) = 2  
                            is(i+1,j+1) = 2  


        ! *#(1-3)#*-----------
                    elseif((s1.lt.cst .or. s1.gt.ced  .or.&
                          s2.lt.cst .or. s2.gt.ced  .or.&
                          s3.lt.cst .or. s3.gt.ced  .or.&
                          s4.lt.cst .or. s4.gt.ced)    .and.lp.eq.1) then
                        lp = 0
                        xx = px*(i+1 - ist) + px/2.0
                        yy = py*( j  - jst) + py/2.0
                    xrig = xx
                    yrig = yy
                        call betsqK(xlef,ylef,xrig,yrig,rr,gg,bb)


        ! *#(1-4)#*-----------
                    elseif((s1.ge.cst.and.s1.le.ced.and.&
                          s2.ge.cst.and.s2.le.ced.and.&
                          s3.ge.cst.and.s3.le.ced.and.&
                          s4.ge.cst.and.s4.le.ced) &
                                   .and.lp.eq.1.and.j.eq.jed-1) then 

                            is( i , j ) = 2  
                            is(i+1, j ) = 2  
                            is( i ,j+1) = 2  
                            is(i+1,j+1) = 2  

                        xx = px*(i+1 - ist) + px/2.0
                        yy = py*(j+1 - jst) + py/2.0
                    xrig = xx
                    yrig = yy
                        call betsqK(xlef,ylef,xrig,yrig,rr,gg,bb)
                        


        ! *#(1-5)#*-----------
                    elseif((s1.ge.cst.and.s1.le.ced.and.&
                          s2.ge.cst.and.s2.le.ced.and.&
                          s3.ge.cst.and.s3.le.ced.and.&
                          s4.ge.cst.and.s4.le.ced)&   
                                   .and.lp.eq.0.and.j.eq.jed-1) then 
                            is( i , j ) = 2  
                            is(i+1, j ) = 2  
                            is( i ,j+1) = 2  
                            is(i+1,j+1) = 2  

                        xx = px*(i - ist) + px/2.0
                        yy = py*(j - jst) + py/2.0
                    xlef = xx
                    ylef = yy
                    xrig = xx + px
                    yrig = yy + py
                        call betsqK(xlef,ylef,xrig,yrig,rr,gg,bb)

                endif
        ! *****************
            ELSE
        ! *****************
                    if(lp.eq.1) then 
                        lp = 0
                        xx = px*(i+1 - ist) + px/2.0
                        yy = py*( j  - jst) + py/2.0
                    xrig = xx
                    yrig = yy
                        call betsqK(xlef,ylef,xrig,yrig,rr,gg,bb)
                    endif 

        ! ********************
            ENDIF
        ! ********************
            enddo
            enddo


        ! *###################################################################
        ! *################### FIRST STEP ####################################
        ! *###################################################################
            do 20 j=jst,jed-1 
            do 10 i=ist,ied-1  
            if(is(i  ,j  ).eq.0) goto 10 
            if(is(i+1,j  ).eq.0) goto 10 
            if(is(i  ,j+1).eq.0) goto 10 
            if(is(i+1,j+1).eq.0) goto 10 

        ! ************************************************************
            if(is( i , j ).eq.2 .and. is(i+1, j ).eq.2  .and.&
                is( i ,j+1).eq.2 .and. is(i+1,j+1).eq.2 ) goto 10 
        ! ************************************************************

                s1 = ss( i , j )
                s2 = ss(i+1, j )
                s3 = ss( i ,j+1)
                s4 = ss(i+1,j+1)
                    xlef = real( i -ist)*px + px/2.0 
                    ylef = real( j -jst)*py + py/2.0
                    xrig = real(i+1-ist)*px + px/2.0 
                    yrig = real(j+1-jst)*py + py/2.0
        ! *       ------------------------------------------------------------
                if(((s1.ge.cst).and.(s1.le.ced)).and.&
                   ((s2.ge.cst).and.(s2.le.ced)).and.&
                   ((s3.ge.cst).and.(s3.le.ced)).and.&
                   ((s4.ge.cst).and.(s4.le.ced))) then 
                    x(1) = xlef
                    x(2) = xlef 
                    x(3) = xrig
                    x(4) = xrig 
                    y(1) = ylef
                    y(2) = yrig 
                    y(3) = yrig 
                    y(4) = ylef
                call betmlK(x,y,4,nx,rr,gg,bb)
        ! *       ------------------------------------------------------------
                elseif(((s1.ge.cst).and.(s1.le.ced)).and.&
                      ((s2.ge.cst).and.(s2.le.ced)).and.&
                      ((s3.ge.cst).and.(s3.le.ced)).and.&
                      ((s4.lt.cst).or.(s4.gt.ced))) then 
        ! *((((1))))
                dxa = abs(s4-s3)
                if(s4.gt.ced) then 
                ddx = abs(s3 - ced)
                else
                ddx = abs(s3 - cst)
                endif 
                x(3) = xlef + (ddx/dxa)*px 
        ! *((((2)))) 
                dxa = abs(s4-s2)        
                if(s4.gt.ced) then 
                ddx = abs(s2 - ced)
                else
                ddx = abs(s2 - cst)
                endif 
                y(4) = ylef + (ddx/dxa)*py 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = xrig
                    x(5) = xrig
                    y(1) = ylef
                    y(2) = yrig
                    y(3) = yrig 
                    y(4) = y(4)
                    y(5) = ylef  
                call betmlK(x,y,5,nx,rr,gg,bb)
        ! *       -------------------------------------------------------------
                elseif(((s1.ge.cst).and.(s1.le.ced)).and.&
                      ((s2.ge.cst).and.(s2.le.ced)).and.&
                      ((s3.lt.cst).or.(s3.gt.ced)).and.&
                      ((s4.ge.cst).and.(s4.le.ced))) then 
        ! *((((1)))) 
                dxa = abs(s3-s1)
                if(s3.gt.ced) then 
                ddx = abs(s1 - ced)
                else
                ddx = abs(s1 - cst)
                endif 
                y(2) = ylef + (ddx/dxa)*py 
        ! *((((2)))) 
                dxa = abs(s3-s4)
                if(s3.gt.ced) then 
                ddx = abs(s3 - ced)
                else
                ddx = abs(s3 - cst)
                endif 
                x(3) = xlef + (ddx/dxa)*px 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = xrig
                    x(5) = xrig
                    y(1) = ylef 
                    y(2) = y(2)
                    y(3) = yrig
                    y(4) = yrig
                    y(5) = ylef 
                call betmlK(x,y,5,nx,rr,gg,bb)
        ! *       -------------------------------------------------------------
                elseif(((s1.ge.cst).and.(s1.le.ced)).and.&
                      ((s2.lt.cst).or.(s2.gt.ced)).and.&
                      ((s3.ge.cst).and.(s3.le.ced)).and.&
                      ((s4.ge.cst).and.(s4.le.ced))) then 
        ! *((((1)))) 
                dxa = abs(s2-s1)
                if(s2.gt.ced) then 
                ddx = abs(s1 - ced)
                else
                ddx = abs(s1 - cst)
                endif 
                x(5) = xlef + (ddx/dxa)*px 
        ! *((((2)))) 
                dxa = abs(s2-s4)
                if(s2.gt.ced) then 
                ddx = abs(s2 - ced)
                else
                ddx = abs(s2 - cst)
                endif 
                y(4) = ylef + (ddx/dxa)*py 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = xrig 
                    x(4) = xrig 
                    x(5) = x(5)
                    y(1) = ylef
                    y(2) = yrig
                    y(3) = yrig
                    y(4) = y(4)
                    y(5) = ylef 
                call betmlK(x,y,5,nx,rr,gg,bb)
        ! *       -------------------------------------------------------------
                elseif(((s1.lt.cst).or.(s1.gt.ced)).and.&
                       ((s2.ge.cst).and.(s2.le.ced)).and.&
                       ((s3.ge.cst).and.(s3.le.ced)).and.&
                       ((s4.ge.cst).and.(s4.le.ced))) then 
        ! *((((1)))) 
                dxa = abs(s3-s1)
                if(s1.gt.ced) then 
                ddx = abs(s1 - ced)
                else
                ddx = abs(s1 - cst)
                endif 
                y(1) = ylef  + (ddx/dxa)*py 
        ! *((((2)))) 
                dxa = abs(s2-s1)
                if(s1.gt.ced) then 
                ddx = abs(s1 - ced)
                else
                ddx = abs(s1 - cst)
                endif 
                x(5) = xlef + (ddx/dxa)*px  
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = xrig
                    x(4) = xrig 
                    x(5) = x(5)
                    y(1) = y(1)
                    y(2) = yrig
                    y(3) = yrig
                    y(4) = ylef
                    y(5) = ylef 
                call betmlK(x,y,5,nx,rr,gg,bb)
        ! *       -------------------------------------------------------------
                elseif(((s1.ge.cst).and.(s1.le.ced)).and.&
                       ((s2.ge.cst).and.(s2.le.ced)).and.&
                       ((s3.lt.cst).or.(s3.gt.ced)).and.&
                       ((s4.lt.cst).or.(s4.gt.ced))) then 
                if((s3.gt.ced).and.(s4.lt.cst)) then 
                dxa = abs(s3-s1)
                ddx = abs(s1 - ced)
                y(2) = ylef + (ddx/dxa)*py 
                dxa = abs(s3-s4)
                ddx = abs(s3-ced)
                x(3) = xlef + (ddx/dxa)*px 
                ddx = abs(s3-cst)
                x(4) = xlef + (ddx/dxa)*px
                dxa = abs(s4-s2)
                ddx = abs(s2-cst)
                y(5) = ylef + (ddx/dxa)*py
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = x(4)
                    x(5) = xrig
                    x(6) = xrig 
                    y(1) = ylef 
                    y(2) = y(2)
                    y(3) = yrig
                    y(4) = yrig 
                    y(5) = y(5)
                    y(6) = ylef 
                call betmlK(x,y,6,nx,rr,gg,bb)
                else if((s3.lt.cst).and.(s4.gt.ced)) then 
                dxa = abs(s3-s1)
                ddx = abs(s1 - cst)
                y(2)= ylef + (ddx/dxa)*py 
                dxa = abs(s3-s4)
                ddx = abs(s3-cst)
                x(3) = xlef + (ddx/dxa)*px 
                ddx = abs(s3-ced)
                x(4) = xlef + (ddx/dxa)*px
                dxa = abs(s4-s2)
                ddx = abs(s2-ced)
                y(5) = ylef + (ddx/dxa)*py
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = x(4)
                    x(5) = xrig
                    x(6) = xrig 
                    y(1) = ylef 
                    y(2) = y(2)
                    y(3) = yrig
                    y(4) = yrig 
                    y(5) = y(5)
                    y(6) = ylef 
                call betmlK(x,y,6,nx,rr,gg,bb)
                else if((s3.gt.ced).and.(s4.gt.ced)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                y(3) = ylef + (ddx/dxa)*py 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = xrig
                    x(4) = xrig 
                    y(1) = ylef
                    y(2) = y(2)
                    y(3) = y(3)
                    y(4) = ylef 
                call betmlK(x,y,4,nx,rr,gg,bb)
                else if((s3.lt.cst).and.(s4.lt.cst)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                y(3) = ylef + (ddx/dxa)*py 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = xrig
                    x(4) = xrig 
                    y(1) = ylef
                    y(2) = y(2)
                    y(3) = y(3)
                    y(4) = ylef 
                call betmlK(x,y,4,nx,rr,gg,bb)
                endif  
        ! *       -------------------------------------------------------------
                elseif(((s1.ge.cst).and.(s1.le.ced)).and.&
                       ((s2.lt.cst).or.(s2.gt.ced)).and.&
                       ((s3.lt.cst).or.(s3.gt.ced)).and.&
                       ((s4.ge.cst).and.(s4.le.ced))) then 
                if((s3.gt.ced).and.(s2.lt.cst)) then 
                    dxa = abs(s3-s1)
                    ddx = abs(s1-ced)
                y(2) = ylef + (ddx/dxa)*py
                    dxa = abs(s4-s3) 
                    ddx = abs(s3-ced) 
                x(3) = xlef + (ddx/dxa)*px
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                x(6) = xlef + (ddx/dxa)*px
                    dxa = abs(s4-s2)
                    ddx = abs(s2-cst)
                y(5) = ylef + (ddx/dxa)*py
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = xrig
                    x(5) = xrig 
                    x(6) = x(6)
                    y(1) = ylef
                    y(2) = y(2)
                    y(3) = yrig
                    y(4) = yrig
                    y(5) = y(5)
                    y(6) = ylef 
                call betmlK(x,y,6,nx,rr,gg,bb)
                elseif((s3.lt.cst).and.(s2.gt.ced)) then 
                    dxa = abs(s3-s1)
                    ddx = abs(s1-cst)
                y(2) = ylef + (ddx/dxa)*py
                    dxa = abs(s4-s3) 
                    ddx = abs(s3-cst) 
                x(3) = xlef + (ddx/dxa)*px
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                x(6) = xlef + (ddx/dxa)*px
                    dxa = abs(s4-s2)
                    ddx = abs(s2-ced)
                y(5) = ylef + (ddx/dxa)*py
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = xrig
                    x(5) = xrig 
                    x(6) = x(6)
                    y(1) = ylef
                    y(2) = y(2)
                    y(3) = yrig
                    y(4) = yrig
                    y(5) = y(5)
                    y(6) = ylef 
                call betmlK(x,y,6,nx,rr,gg,bb)
                elseif((s3.lt.cst).and.(s2.lt.cst)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x01 = xlef + (ddx/dxa)*px
                    y01 = ylef  
                    dxa = abs(s3-s1)
                    ddx = abs(s1-cst)
                    x02 = xlef
                    y02 = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x03 = xlef + (ddx/dxa)*px 
                    y03 = yrig 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-cst)
                    x04 = xrig 
                    y04 = ylef + (ddx/dxa)*py
                    eng1 = (x01-x02)**2 + (y01-y02)**2 
                    eng2 = (x01-x04)**2 + (y01-y04)**2 
        ! ***POST***
                if(eng2.lt.eng1) then 
                    x(1) = xlef 
                    x(2) = x02
                    x(3) = x03  
                    x(4) = xrig 
                    x(5) = x04 
                    x(6) = x01 
                    y(1) = ylef
                    y(2) = y02 
                    y(3) = y03
                    y(4) = yrig 
                    y(5) = y04
                    y(6) = y01
                call betmlK(x,y,6,nx,rr,gg,bb)
                else 
                    x(1) = xlef
                    x(2) = x02
                    x(3) = x01
                    y(1) = ylef
                    y(2) = y02
                    y(3) = y01
                call betmlK(x,y,3,nx,rr,gg,bb)
                    x(1) = x03 
                    x(2) = xrig 
                    x(3) = x04
                    y(1) = y03
                    y(2) = yrig
                    y(3) = y04 
                call betmlK(x,y,3,nx,rr,gg,bb)
                endif
                elseif((s3.gt.ced).and.(s2.gt.ced)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x01 = xlef + (ddx/dxa)*px
                    y01 = ylef  
                    dxa = abs(s3-s1)
                    ddx = abs(s1-ced)
                    x02 = xlef
                    y02 = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x03 = xlef + (ddx/dxa)*px 
                    y03 = yrig 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-ced)
                    x04 = xrig 
                    y04 = ylef + (ddx/dxa)*py
                    eng1 = (x01-x02)**2 + (y01-y02)**2 
                    eng2 = (x01-x04)**2 + (y01-y04)**2 
        ! ***POST***
                if(eng2.lt.eng1) then 
                    x(1) = xlef 
                    x(2) = x02
                    x(3) = x03  
                    x(4) = xrig 
                    x(5) = x04 
                    x(6) = x01 
                    y(1) = ylef
                    y(2) = y02 
                    y(3) = y03
                    y(4) = yrig 
                    y(5) = y04
                    y(6) = y01
                call betmlK(x,y,6,nx,rr,gg,bb)
                else 
                    x(1) = xlef
                    x(2) = x02
                    x(3) = x01
                    y(1) = ylef
                    y(2) = y02
                    y(3) = y01
                call betmlK(x,y,3,nx,rr,gg,bb)
                    x(1) = x03 
                    x(2) = xrig 
                    x(3) = x04
                    y(1) = y03
                    y(2) = yrig
                    y(3) = y04 
                call betmlK(x,y,3,nx,rr,gg,bb)
                endif
                endif 
        ! *       -------------------------------------------------------------
                elseif(((s1.lt.cst).or.(s1.gt.ced)).and.&
                       ((s2.ge.cst).and.(s2.le.ced)).and.&
                       ((s3.lt.cst).or.(s3.gt.ced)).and.&
                       ((s4.ge.cst).and.(s4.le.ced))) then 
                if((s1.lt.cst).and.(s3.gt.ced)) then 
                dxa = abs(s1-s3)
                ddx = abs(s1-cst)
                y(1) = ylef + (ddx/dxa)*py
                ddx = abs(s1-ced)
                y(2) = ylef + (ddx/dxa)*py
                dxa = abs(s3-s4)
                ddx = abs(s3-ced)
                x(3) = xlef + (ddx/dxa)*px  
                dxa = abs(s1-s2)
                ddx = abs(s1-cst)
                x(6) = xlef + (ddx/dxa)*px 
                    x(1) = xlef 
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = xrig
                    x(5) = xrig 
                    x(6) = x(6)
                    y(1) = y(1)
                    y(2) = y(2)
                    y(3) = yrig
                    y(4) = yrig
                    y(5) = ylef
                    y(6) = ylef 
                call betmlK(x,y,6,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s3.lt.cst)) then 
                dxa = abs(s1-s3)
                ddx = abs(s1-ced)
                y(1) = ylef + (ddx/dxa)*py
                ddx = abs(s1-cst)
                y(2) = ylef + (ddx/dxa)*py
                dxa = abs(s3-s4)
                ddx = abs(s3-cst)
                x(3) = xlef + (ddx/dxa)*px  
                dxa = abs(s1-s2)
                ddx = abs(s1-ced)
                x(6) = xlef + (ddx/dxa)*px 
                    x(1) = xlef 
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = xrig
                    x(5) = xrig 
                    x(6) = x(6)
                    y(1) = y(1)
                    y(2) = y(2)
                    y(3) = yrig
                    y(4) = yrig
                    y(5) = ylef
                    y(6) = ylef 
                call betmlK(x,y,6,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s3.lt.cst)) then 
                dxa = abs(s1-s2)
                ddx = abs(s1-cst)
                x(1) = xlef + (ddx/dxa)*px 
                dxa = abs(s3-s4)
                ddx = abs(s3-cst)
                x(2) = xlef + (ddx/dxa)*px 
                    x(1) = x(1)
                    x(2) = x(2)        
                    x(3) = xrig        
                    x(4) = xrig        
                    y(1) = ylef
                    y(2) = yrig        
                    y(3) = yrig        
                    y(4) = ylef         
                call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s3.gt.ced)) then 
                dxa = abs(s1-s2)
                ddx = abs(s1-ced)
                x(1) = xlef + (ddx/dxa)*px 
                dxa = abs(s3-s4)
                ddx = abs(s3-ced)
                x(2) = xlef + (ddx/dxa)*px 
                    x(1) = x(1)
                    x(2) = x(2)        
                    x(3) = xrig        
                    x(4) = xrig        
                    y(1) = ylef
                    y(2) = yrig        
                    y(3) = yrig        
                    y(4) = ylef         
                call betmlK(x,y,4,nx,rr,gg,bb)
                endif 
        ! *       -------------------------------------------------------------
                elseif(((s1.lt.cst).or.(s1.gt.ced)).and.&
                       ((s2.lt.cst).or.(s2.gt.ced)).and.&
                       ((s3.ge.cst).and.(s3.le.ced)).and.&
                       ((s4.ge.cst).and.(s4.le.ced))) then 
                if((s1.gt.ced).and.(s2.lt.cst)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                y(1) = ylef + (ddx/dxa)*py 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                y(4) = ylef + (ddx/dxa)*py
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                x(5) = xlef + (ddx/dxa)*px 
                    ddx = abs(s1-ced)
                x(6) = xlef + (ddx/dxa)*px 
                    x(1) = xlef 
                    x(2) = xlef 
                    x(3) = xrig 
                    x(4) = xrig 
                    x(5) = x(5) 
                    x(6) = x(6) 
                    y(1) = y(1) 
                    y(2) = yrig 
                    y(3) = yrig 
                    y(4) = y(4) 
                    y(5) = ylef 
                    y(6) = ylef 
                call betmlK(x,y,6,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.gt.ced)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                y(1) = ylef + (ddx/dxa)*py 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                y(4) = ylef + (ddx/dxa)*py
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                x(5) = xlef + (ddx/dxa)*px 
                    ddx = abs(s1-cst)
                x(6) = xlef + (ddx/dxa)*px 
                    x(1) = xlef 
                    x(2) = xlef 
                    x(3) = xrig 
                    x(4) = xrig 
                    x(5) = x(5) 
                    x(6) = x(6) 
                    y(1) = y(1) 
                    y(2) = yrig 
                    y(3) = yrig 
                    y(4) = y(4) 
                    y(5) = ylef 
                    y(6) = ylef 
                call betmlK(x,y,6,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.lt.cst)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                y(1) = ylef + (ddx/dxa)*py 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                y(4) = ylef + (ddx/dxa)*py 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = xrig 
                    x(4) = xrig 
                    y(1) = y(1) 
                    y(2) = yrig
                    y(3) = yrig
                    y(4) = y(4) 
                call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s2.gt.ced)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                y(1) = ylef + (ddx/dxa)*py 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                y(4) = ylef + (ddx/dxa)*py 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = xrig 
                    x(4) = xrig 
                    y(1) = y(1) 
                    y(2) = yrig
                    y(3) = yrig
                    y(4) = y(4)
                call betmlK(x,y,4,nx,rr,gg,bb)
                endif 
        ! *       -------------------------------------------------------------
                elseif(((s1.lt.cst).or.(s1.gt.ced)).and.&
                       ((s2.ge.cst).and.(s2.le.ced)).and.&
                       ((s3.ge.cst).and.(s3.le.ced)).and.&
                       ((s4.lt.cst).or.(s4.gt.ced))) then 
                if((s1.lt.cst).and.(s4.gt.ced)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                    y(1) = ylef + (ddx/dxa)*py
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x(3) = xlef + (ddx/dxa)*px 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                    y(4) = ylef + (ddx/dxa)*py 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x(6) = xlef + (ddx/dxa)*px 
                    x(1) = xlef 
                    x(2) = xlef
                    x(3) = x(3) 
                    x(4) = xrig 
                    x(5) = xrig  
                    x(6) = x(6) 
                    y(1) = y(1) 
                    y(2) = yrig 
                    y(3) = yrig 
                    y(4) = y(4) 
                    y(5) = ylef 
                    y(6) = ylef  
                call betmlK(x,y,6,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s4.lt.cst)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                    y(1) = ylef + (ddx/dxa)*py
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x(3) = xlef + (ddx/dxa)*px 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                    y(4) = ylef + (ddx/dxa)*py 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x(6) = xlef + (ddx/dxa)*px 
                    x(1) = xlef 
                    x(2) = xlef
                    x(3) = x(3) 
                    x(4) = xrig 
                    x(5) = xrig  
                    x(6) = x(6) 
                    y(1) = y(1) 
                    y(2) = yrig 
                    y(3) = yrig 
                    y(4) = y(4) 
                    y(5) = ylef 
                    y(6) = ylef  
                call betmlK(x,y,6,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s4.lt.cst)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst) 
                    x01 = xlef + (ddx/dxa)*px 
                    y01 = ylef 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                    x02 = xlef 
                    y02 = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x03 = xlef + (ddx/dxa)*px
                    y03 = yrig 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                    x04 = xrig 
                    y04 = ylef + (ddx/dxa)*py 
                    eng1 = (x01-x02)**2 + (y01-y02)**2 
                    eng2 = (x01-x04)**2 + (y01-y04)**2 
        ! ***POST***
                if(eng2.lt.eng1) then 
                    x(1) = x02  
                    x(2) = xlef 
                    x(3) = x03  
                    y(1) = y02 
                    y(2) = yrig 
                    y(3) = y03   
                    call betmlK(x,y,3,nx,rr,gg,bb)
                    x(1) = x01  
                    x(2) = x04 
                    x(3) = xrig  
                    y(1) = y01 
                    y(2) = y04 
                    y(3) = ylef
                    call betmlK(x,y,3,nx,rr,gg,bb)
                    else 
                    x(1) = x02  
                    x(2) = xlef 
                    x(3) = x03 
                    x(4) = x04 
                    x(5) = xrig 
                    x(6) = x01  
                    y(1) = y02 
                    y(2) = yrig 
                    y(3) = y03 
                    y(4) = y04 
                    y(5) = ylef 
                    y(6) = y01 
                    call betmlK(x,y,6,nx,rr,gg,bb)
                    endif 
                elseif((s1.gt.ced).and.(s4.gt.ced)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced) 
                    x01 = xlef + (ddx/dxa)*px 
                    y01 = ylef 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                    x02 = xlef 
                    y02 = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x03 = xlef + (ddx/dxa)*px
                    y03 = yrig 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                    x04 = xrig 
                    y04 = ylef + (ddx/dxa)*py 
                    eng1 = (x01-x02)**2 + (y01-y02)**2 
                    eng2 = (x01-x04)**2 + (y01-y04)**2 
        ! ***POST***
                if(eng2.lt.eng1) then 
                    x(1) = x02  
                    x(2) = xlef 
                    x(3) = x03  
                    y(1) = y02 
                    y(2) = yrig 
                    y(3) = y03   
                    call betmlK(x,y,3,nx,rr,gg,bb)
                    x(1) = x01  
                    x(2) = x04 
                    x(3) = xrig  
                    y(1) = y01 
                    y(2) = y04 
                    y(3) = ylef
                    call betmlK(x,y,3,nx,rr,gg,bb)
                    else 
                    x(1) = x02  
                    x(2) = xlef 
                    x(3) = x03 
                    x(4) = x04 
                    x(5) = xrig 
                    x(6) = x01  
                    y(1) = y02 
                    y(2) = yrig 
                    y(3) = y03 
                    y(4) = y04 
                    y(5) = ylef 
                    y(6) = y01 
                    call betmlK(x,y,6,nx,rr,gg,bb)
                    endif 
                endif  
        ! *       -------------------------------------------------------------
                elseif(((s1.ge.cst).and.(s1.le.ced)).and.&
                       ((s2.lt.cst).or.(s2.gt.ced)).and.&
                       ((s3.ge.cst).and.(s3.le.ced)).and.&
                       ((s4.lt.cst).or.(s4.gt.ced))) then 
                if((s2.lt.cst).and.(s4.gt.ced)) then 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x(3) = xlef + (ddx/dxa)*px
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                    y(4) = ylef + (ddx/dxa)*py 
                    ddx = abs(s2-cst)
                    y(5) = ylef + (ddx/dxa)*py
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x(6) = xlef + (ddx/dxa)*px 
                    x(1) = xlef 
                    x(2) = xlef 
                    x(3) = x(3) 
                    x(4) = xrig 
                    x(5) = xrig 
                    x(6) = x(6) 
                    y(1) = ylef 
                    y(2) = yrig 
                    y(3) = yrig 
                    y(4) = y(4) 
                    y(5) = y(5)  
                    y(6) = ylef  
                call betmlK(x,y,6,nx,rr,gg,bb)
                elseif((s2.gt.ced).and.(s4.lt.cst)) then 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x(3) = xlef + (ddx/dxa)*px
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                    y(4) = ylef + (ddx/dxa)*py 
                    ddx = abs(s2-ced)
                    y(5) = ylef + (ddx/dxa)*py
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x(6) = xlef + (ddx/dxa)*px 
                    x(1) = xlef 
                    x(2) = xlef 
                    x(3) = x(3) 
                    x(4) = xrig 
                    x(5) = xrig 
                    x(6) = x(6) 
                    y(1) = ylef 
                    y(2) = yrig 
                    y(3) = yrig 
                    y(4) = y(4) 
                    y(5) = y(5)  
                    y(6) = ylef  
                call betmlK(x,y,6,nx,rr,gg,bb)
                elseif((s2.lt.cst).and.(s4.lt.cst)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x(4) = xlef + (ddx/dxa)*px 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x(3) = xlef + (ddx/dxa)*px
                        x(1) = xlef
                        x(2) = xlef
                        x(3) = x(3)
                        x(4) = x(4)
                        y(1) = ylef
                        y(2) = yrig
                        y(3) = yrig
                        y(4) = ylef 
                call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s2.gt.ced).and.(s4.gt.ced)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x(4) = xlef + (ddx/dxa)*px 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x(3) = xlef + (ddx/dxa)*px
                        x(1) = xlef  
                        x(2) = xlef
                        x(3) = x(3)
                        x(4) = x(4)
                        y(1) = ylef
                        y(2) = yrig
                        y(3) = yrig
                        y(4) = ylef 
                call betmlK(x,y,4,nx,rr,gg,bb)
                endif 
        ! *       -------------------------------------------------------------
                elseif(((s1.ge.cst).and.(s1.le.ced)).and.&
                       ((s2.lt.cst).or.(s2.gt.ced)).and.&
                       ((s3.lt.cst).or.(s3.gt.ced)).and.&
                       ((s4.lt.cst).or.(s4.gt.ced))) then 
                if((s2.lt.cst).and.(s3.gt.ced).and.(s4.gt.ced)) then  
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                y(3) = ylef + (ddx/dxa)*py 
                    ddx = abs(s2-cst)
                y(4) = ylef + (ddx/dxa)*py  
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                x(5) = xlef + (ddx/dxa)*px 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = xrig 
                    x(4) = xrig 
                    x(5) = x(5)
                    y(1) = ylef 
                    y(2) = y(2)
                    y(3) = y(3)
                    y(4) = y(4)
                    y(5) = ylef  
                call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s2.lt.cst).and.(s3.gt.ced).and.(s4.lt.cst)) then  
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                x(3) = xlef + (ddx/dxa)*px 
                    ddx = abs(s3-cst)
                x(4) = xlef + (ddx/dxa)*px 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                x(5) = xlef + (ddx/dxa)*px 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3) 
                    x(4) = x(4) 
                    x(5) = x(5)
                    y(1) = ylef 
                    y(2) = y(2)
                    y(3) = yrig
                    y(4) = yrig
                    y(5) = ylef  
                call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s2.lt.cst).and.(s3.lt.cst).and.(s4.lt.cst)) then  
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                x(3) = xlef + (ddx/dxa)*px 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3) 
                    y(1) = ylef
                    y(2) = y(2)
                    y(3) = ylef 
                call betmlK(x,y,3,nx,rr,gg,bb)
                elseif((s2.gt.ced).and.(s3.gt.ced).and.(s4.gt.ced)) then  
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                x(3) = xlef + (ddx/dxa)*px 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3) 
                    y(1) = ylef
                    y(2) = y(2)
                    y(3) = ylef 
                call betmlK(x,y,3,nx,rr,gg,bb)
                elseif((s2.gt.ced).and.(s3.lt.cst).and.(s4.lt.cst)) then  
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                y(3) = ylef + (ddx/dxa)*py 
                    ddx = abs(s2-ced)
                y(4) = ylef + (ddx/dxa)*py 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                x(5) = xlef + (ddx/dxa)*px 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = xrig
                    x(4) = xrig 
                    x(5) = x(5)
                    y(1) = ylef
                    y(2) = y(2)
                    y(3) = y(3) 
                    y(4) = y(4) 
                    y(5) = ylef  
                call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s2.gt.ced).and.(s3.lt.cst).and.(s4.gt.ced)) then  
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                x(3) = xlef + (ddx/dxa)*px 
                    ddx = abs(s3-ced)
                x(4) = xlef + (ddx/dxa)*px 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                x(5) = xlef + (ddx/dxa)*px 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = x(4) 
                    x(5) = x(5)
                    y(1) = ylef
                    y(2) = y(2)
                    y(3) = yrig 
                    y(4) = yrig 
                    y(5) = ylef   
                call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s2.gt.ced).and.(s3.gt.cst).and.(s4.lt.cst)) then  
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x01 = xlef + (ddx/dxa)*px
                    y01 = ylef  
                    dxa = abs(s3-s1)
                    ddx = abs(s1-ced)
                    x02 = xlef
                    y02 = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x03 = xlef + (ddx/dxa)*px 
                    y03 = yrig 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-ced)
                    x04 = xrig 
                    y04 = ylef + (ddx/dxa)*py
                    eng1 = (x01-x02)**2 + (y01-y02)**2 
                    eng2 = (x01-x04)**2 + (y01-y04)**2 

                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x(4) = xlef + (ddx/dxa)*px 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-cst)
                    y(5) = ylef + (ddx/dxa)*py
        ! ***POST***
                if(eng2.lt.eng1) then 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x03
                    x(4) = x(4)
                    x(5) = xrig
                    x(6) = xrig
                    x(7) = x01
                    y(1) = ylef
                    y(2) = y02
                    y(3) = yrig
                    y(4) = yrig 
                    y(5) = y(5)
                    y(6) = y04
                    y(7) = ylef 
                    call betmlK(x,y,7,nx,rr,gg,bb)
                else
                    x(1) = xlef
                    x(2) = xlef 
                    x(3) = x01  
                    y(1) = ylef
                    y(2) = y02
                    y(3) = ylef  
                    call betmlK(x,y,3,nx,rr,gg,bb)
                    x(1) = x03
                    x(2) = x(4)
                    x(3) = xrig
                    x(4) = xrig 
                    y(1) = yrig
                    y(2) = yrig
                    y(3) = y(5)
                    y(4) = y04 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                endif
                elseif((s2.lt.cst).and.(s3.lt.cst).and.(s4.gt.ced)) then  
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x01 = xlef + (ddx/dxa)*px
                    y01 = ylef  
                    dxa = abs(s3-s1)
                    ddx = abs(s1-cst)
                    x02 = xlef
                    y02 = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x03 = xlef + (ddx/dxa)*px 
                    y03 = yrig 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-cst)
                    x04 = xrig 
                    y04 = ylef + (ddx/dxa)*py
                    eng1 = (x01-x02)**2 + (y01-y02)**2 
                    eng2 = (x01-x04)**2 + (y01-y04)**2 

                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x(4) = xlef + (ddx/dxa)*px 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-ced)
                    y(5) = ylef + (ddx/dxa)*py
        ! ***POST***
                if(eng2.lt.eng1) then 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x03
                    x(4) = x(4)
                    x(5) = xrig
                    x(6) = xrig
                    x(7) = x01
                    y(1) = ylef
                    y(2) = y02
                    y(3) = yrig
                    y(4) = yrig 
                    y(5) = y(5)
                    y(6) = y04
                    y(7) = ylef 
                    call betmlK(x,y,7,nx,rr,gg,bb)
                else
                    x(1) = xlef
                    x(2) = xlef 
                    x(3) = x01  
                    y(1) = ylef
                    y(2) = y02
                    y(3) = ylef  
                    call betmlK(x,y,3,nx,rr,gg,bb)
                    x(1) = x03
                    x(2) = x(4)
                    x(3) = xrig
                    x(4) = xrig 
                    y(1) = yrig
                    y(2) = yrig
                    y(3) = y(5)
                    y(4) = y04 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                endif
                endif 
        ! *       -------------------------------------------------------------
                elseif(((s1.lt.cst).or.(s1.gt.ced)).and.&
                       ((s2.ge.cst).and.(s2.le.ced)).and.&
                       ((s3.lt.cst).or.(s3.gt.ced)).and.&
                       ((s4.lt.cst).or.(s4.gt.ced))) then 
                if((s4.lt.cst).and.(s1.gt.ced).and.(s3.gt.ced)) then  
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                x(1) = xlef + (ddx/dxa)*px 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                x(2) = xlef + (ddx/dxa)*px 
                    ddx = abs(s3-cst)
                x(3) = xlef + (ddx/dxa)*px  
                    dxa = abs(s4-s2)
                    ddx = abs(s2-cst)
                y(4) = ylef + (ddx/dxa)*py 
                    x(1) = x(1)
                    x(2) = x(2)
                    x(3) = x(3)
                    x(4) = xrig 
                    x(5) = xrig
                    y(1) = ylef
                    y(2) = yrig
                    y(3) = yrig
                    y(4) = y(4)
                    y(5) = ylef 
                    call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s4.lt.cst).and.(s1.gt.ced).and.(s3.lt.cst)) then  
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                x(1) = xlef + (ddx/dxa)*px 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                y(2) = ylef + (ddx/dxa)*py 
                    ddx = abs(s1-cst)
                y(3) = ylef + (ddx/dxa)*py 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                y(4) = ylef + (ddx/dxa)*py 
                    x(1) = x(1)
                    x(2) = xlef
                    x(3) = xlef
                    x(4) = xrig 
                    x(5) = xrig
                    y(1) = ylef
                    y(2) = y(2)
                    y(3) = y(3)
                    y(4) = y(4)
                    y(5) = ylef 
                    call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s4.lt.cst).and.(s1.lt.cst).and.(s3.lt.cst)) then  
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                x(1) = xlef + (ddx/dxa)*px 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                y(2) = ylef + (ddx/dxa)*py 
                    x(1) = x(1)
                    x(2) = xrig
                    x(3) = xrig
                    y(1) = ylef
                    y(2) = y(2)
                    y(3) = ylef 
                    call betmlK(x,y,3,nx,rr,gg,bb)
                elseif((s4.gt.ced).and.(s1.gt.ced).and.(s3.gt.ced)) then  
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                x(1) = xlef + (ddx/dxa)*px 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                y(2) = ylef + (ddx/dxa)*py 
                    x(1) = x(1)
                    x(2) = xrig
                    x(3) = xrig
                    y(1) = ylef
                    y(2) = y(2)
                    y(3) = ylef 
                    call betmlK(x,y,3,nx,rr,gg,bb)
                elseif((s4.gt.ced).and.(s1.lt.cst).and.(s3.lt.cst)) then  
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                x(1) = xlef + (ddx/dxa)*px 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                x(2) = xlef + (ddx/dxa)*px 
                    ddx = abs(s3-ced)
                x(3) = xlef + (ddx/dxa)*px 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                y(4) = ylef + (ddx/dxa)*py 
                    x(1) = x(1)
                    x(2) = x(2)
                    x(3) = x(3)
                    x(4) = xrig
                    x(5) = xrig 
                    y(1) = ylef
                    y(2) = yrig
                    y(3) = yrig 
                    y(4) = y(4)
                    y(5) = ylef 
                    call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s4.gt.ced).and.(s1.lt.cst).and.(s3.gt.ced)) then  
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                x(1) = xlef + (ddx/dxa)*px 
                    dxa = abs(s3-s1)
                    ddx = abs(s1-cst)
                y(2) = ylef + (ddx/dxa)*py 
                    ddx = abs(s1-ced)
                y(3) = ylef + (ddx/dxa)*py 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-ced)
                y(4) = ylef + (ddx/dxa)*py 
                    x(1) = x(1)
                    x(2) = xlef
                    x(3) = xlef
                    x(4) = xrig
                    x(5) = xrig 
                    y(1) = ylef
                    y(2) = y(2)
                    y(3) = y(3) 
                    y(4) = y(4)
                    y(5) = ylef 
                    call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s4.gt.ced).and.(s1.gt.ced).and.(s3.lt.cst)) then  
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x01 = xlef + (ddx/dxa)*px
                    y01 = ylef  
                    dxa = abs(s3-s1)
                    ddx = abs(s1-ced)
                    x02 = xlef
                    y02 = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x03 = xlef + (ddx/dxa)*px 
                    y03 = yrig 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-ced)
                    x04 = xrig 
                    y04 = ylef + (ddx/dxa)*py
                    eng1 = (x01-x02)**2 + (y01-y02)**2 
                    eng2 = (x01-x04)**2 + (y01-y04)**2 

                    dxa = abs(s3-s1)
                    ddx = abs(s1-cst)
                    y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x(3) = xlef + (ddx/dxa)*px
        ! ***POST***
                if(eng2.lt.eng1) then 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = x03
                    y(1) = y02
                    y(2) = y(2)
                    y(3) = yrig
                    y(4) = yrig 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                    x(1) = x01 
                    x(2) = xrig
                    x(3) = xrig
                    y(1) = ylef
                    y(2) = y04
                    y(3) = ylef 
                    call betmlK(x,y,3,nx,rr,gg,bb)
                else 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = x03
                    x(5) = xrig
                    x(6) = xrig 
                    x(7) = x01
                    y(1) = y02
                    y(2) = y(2)
                    y(3) = yrig
                    y(4) = yrig
                    y(5) = y04
                    y(6) = ylef
                    y(7) = ylef 
                    call betmlK(x,y,7,nx,rr,gg,bb)
                endif 
                elseif((s4.lt.cst).and.(s1.lt.cst).and.(s3.gt.ced)) then  
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x01 = xlef + (ddx/dxa)*px
                    y01 = ylef  
                    dxa = abs(s3-s1)
                    ddx = abs(s1-cst)
                    x02 = xlef
                    y02 = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x03 = xlef + (ddx/dxa)*px 
                    y03 = yrig 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-cst)
                    x04 = xrig 
                    y04 = ylef + (ddx/dxa)*py
                    eng1 = (x01-x02)**2 + (y01-y02)**2 
                    eng2 = (x01-x04)**2 + (y01-y04)**2 

                    dxa = abs(s3-s1)
                    ddx = abs(s1-ced)
                    y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x(3) = xlef + (ddx/dxa)*px
        ! ***POST***
                if(eng2.lt.eng1) then 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = x03
                    y(1) = y02
                    y(2) = y(2)
                    y(3) = yrig
                    y(4) = yrig 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                    x(1) = x01 
                    x(2) = xrig
                    x(3) = xrig
                    y(1) = ylef
                    y(2) = y04
                    y(3) = ylef 
                    call betmlK(x,y,3,nx,rr,gg,bb)
                else 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = x03
                    x(5) = xrig
                    x(6) = xrig 
                    x(7) = x01
                    y(1) = y02
                    y(2) = y(2)
                    y(3) = yrig
                    y(4) = yrig
                    y(5) = y04
                    y(6) = ylef
                    y(7) = ylef 
                    call betmlK(x,y,7,nx,rr,gg,bb)
                endif 
                endif 
        ! *       -------------------------------------------------------------
                elseif(((s1.lt.cst).or.(s1.gt.ced)).and.&
                       ((s2.lt.cst).or.(s2.gt.ced)).and.&
                       ((s3.ge.cst).and.(s3.le.ced)).and.&
                       ((s4.lt.cst).or.(s4.gt.ced))) then 
                if((s1.lt.cst).and.(s2.gt.ced).and.(s4.gt.ced)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                x(1) = xlef + (ddx/dxa)*px 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                x(4) = xlef + (ddx/dxa)*px 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                x(5) = xlef + (ddx/dxa)*px 
                    x(1) = x(1)
                    x(2) = xlef
                    x(3) = xlef 
                    x(4) = x(4)
                    x(5) = x(5)
                    y(1) = ylef 
                    y(2) = y(2) 
                    y(3) = yrig
                    y(4) = yrig 
                    y(5) = ylef 
                    call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.lt.cst).and.(s4.gt.ced)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                y(1) = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                x(3) = xlef + (ddx/dxa)*px 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                y(4) = ylef + (ddx/dxa)*py 
                    ddx = abs(s2-cst)
                y(5) = ylef + (ddx/dxa)*py 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3) 
                    x(4) = xrig
                    x(5) = xrig
                    y(1) = y(1) 
                    y(2) = yrig 
                    y(3) = yrig
                    y(4) = y(4) 
                    y(5) = y(5) 
                    call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.lt.cst).and.(s4.lt.cst)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                y(1) = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                x(3) = xlef + (ddx/dxa)*px 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    y(1) = y(1)
                    y(2) = yrig
                    y(3) = yrig 
                    call betmlK(x,y,3,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s2.gt.ced).and.(s4.gt.ced)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                y(1) = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                x(3) = xlef + (ddx/dxa)*px 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    y(1) = y(1)
                    y(2) = yrig
                    y(3) = yrig 
                    call betmlK(x,y,3,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s2.lt.cst).and.(s4.lt.cst)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                y(1) = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                x(3) = xlef + (ddx/dxa)*px 
                    dxa = abs(s2-s1)
                    ddx = abs(s1-cst)
                x(4) = xlef + (ddx/dxa)*px 
                    ddx = abs(s1-ced)
                x(5) = xlef + (ddx/dxa)*px 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = x(4)
                    x(5) = x(5)
                    y(1) = y(1)
                    y(2) = yrig
                    y(3) = yrig
                    y(4) = ylef
                    y(5) = ylef
                    call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s2.gt.ced).and.(s4.lt.cst)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                y(1) = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                x(3) = xlef + (ddx/dxa)*px 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                y(4) = ylef + (ddx/dxa)*py 
                    ddx = abs(s2-ced)
                y(5) = ylef + (ddx/dxa)*py 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = xrig
                    x(5) = xrig
                    y(1) = y(1)
                    y(2) = yrig
                    y(3) = yrig
                    y(4) = y(4)
                    y(5) = y(5)
                    call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s2.lt.cst).and.(s4.gt.ced)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x01 = xlef + (ddx/dxa)*px
                    y01 = ylef  
                    dxa = abs(s3-s1)
                    ddx = abs(s1-ced)
                    x02 = xlef
                    y02 = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x03 = xlef + (ddx/dxa)*px 
                    y03 = yrig 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-ced)
                    x04 = xrig 
                    y04 = ylef + (ddx/dxa)*py
                    eng1 = (x01-x02)**2 + (y01-y02)**2 
                    eng2 = (x01-x04)**2 + (y01-y04)**2 

                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                    y(5) = ylef + (ddx/dxa)*py 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x(6) = xlef + (ddx/dxa)*px
        ! ***POST***
                if(eng2.lt.eng1) then 
                    x(1) = xlef 
                    x(2) = xlef 
                    x(3) = x03 
                    y(1) = y02 
                    y(2) = yrig 
                    y(3) = yrig 
                    call betmlK(x,y,3,nx,rr,gg,bb)
                    x(1) = x01
                    x(2) = xrig
                    x(3) = xrig
                    x(4) = x(6)
                    y(1) = ylef
                    y(2) = y04
                    y(3) = y(5)
                    y(4) = ylef 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                else
                    x(1) = xlef
                    x(2) = xlef 
                    x(3) = x03 
                    x(4) = xrig
                    x(5) = xrig
                    x(6) = x(6)
                    x(7) = x01
                    y(1) = y02
                    y(2) = yrig
                    y(3) = yrig
                    y(4) = y04
                    y(5) = y(5)
                    y(6) = ylef
                    y(7) = ylef 
                    call betmlK(x,y,7,nx,rr,gg,bb)
                endif 
                elseif((s1.lt.cst).and.(s2.gt.ced).and.(s4.lt.cst)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x01 = xlef + (ddx/dxa)*px
                    y01 = ylef  
                    dxa = abs(s3-s1)
                    ddx = abs(s1-cst)
                    x02 = xlef
                    y02 = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x03 = xlef + (ddx/dxa)*px 
                    y03 = yrig 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-cst)
                    x04 = xrig 
                    y04 = ylef + (ddx/dxa)*py
                    eng1 = (x01-x02)**2 + (y01-y02)**2 
                    eng2 = (x01-x04)**2 + (y01-y04)**2 

                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                    y(5) = ylef + (ddx/dxa)*py 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x(6) = xlef + (ddx/dxa)*px
        ! ***POST***
                if(eng2.lt.eng1) then 
                    x(1) = xlef 
                    x(2) = xlef 
                    x(3) = x03 
                    y(1) = y02 
                    y(2) = yrig 
                    y(3) = yrig 
                    call betmlK(x,y,3,nx,rr,gg,bb)
                    x(1) = x01
                    x(2) = xrig
                    x(3) = xrig
                    x(4) = x(6)
                    y(1) = ylef
                    y(2) = y04
                    y(3) = y(5)
                    y(4) = ylef 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                else
                    x(1) = xlef
                    x(2) = xlef 
                    x(3) = x03 
                    x(4) = xrig
                    x(5) = xrig
                    x(6) = x(6)
                    x(7) = x01
                    y(1) = y02
                    y(2) = yrig
                    y(3) = yrig
                    y(4) = y04
                    y(5) = y(5)
                    y(6) = ylef
                    y(7) = ylef 
                    call betmlK(x,y,7,nx,rr,gg,bb)
                endif 
                endif 
        ! *       -------------------------------------------------------------
                elseif(((s1.lt.cst).or.(s1.gt.ced)).and.&
                       ((s2.lt.cst).or.(s2.gt.ced)).and.&
                       ((s3.lt.cst).or.(s3.gt.ced)).and.&
                       ((s4.ge.cst).and.(s4.le.ced))) then 
                if((s1.gt.ced).and.(s2.gt.ced).and.(s3.lt.cst)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                    y(1) = ylef + (ddx/dxa)*py 
                    ddx = abs(s1-cst)
                    y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x(3) = xlef + (ddx/dxa)*px
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                    y(5) = ylef + (ddx/dxa)*py 
                    x(1) = xlef
                    x(2) = xlef 
                    x(3) = x(3)
                    x(4) = xrig
                    x(5) = xrig
                    y(1) = y(1)
                    y(2) = y(2)
                    y(3) = yrig
                    y(4) = yrig 
                    y(5) = y(5)
                    call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.gt.ced).and.(s3.lt.cst)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x(1) = xlef + (ddx/dxa)*px
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x(2) = xlef + (ddx/dxa)*px
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                    y(4) = ylef + (ddx/dxa)*py 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x(5) = xlef + (ddx/dxa)*px
                    x(1) = x(1)
                    x(2) = x(2) 
                    x(3) = xrig
                    x(4) = xrig
                    x(5) = x(5)
                    y(1) = ylef
                    y(2) = yrig
                    y(3) = yrig
                    y(4) = y(4) 
                    y(5) = ylef 
                    call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.lt.cst).and.(s3.lt.cst)) then 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x(1) = xlef + (ddx/dxa)*px
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                    y(3) = ylef + (ddx/dxa)*py 
                    x(1) = x(1)
                    x(2) = xrig 
                    x(3) = xrig
                    y(1) = yrig
                    y(2) = yrig
                    y(3) = y(3)
                    call betmlK(x,y,3,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s2.gt.ced).and.(s3.gt.ced)) then 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x(1) = xlef + (ddx/dxa)*px
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                    y(3) = ylef + (ddx/dxa)*py 
                    x(1) = x(1)
                    x(2) = xrig 
                    x(3) = xrig
                    y(1) = yrig
                    y(2) = yrig
                    y(3) = y(3)
                    call betmlK(x,y,3,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.lt.cst).and.(s3.gt.ced)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                    y(1) = ylef + (ddx/dxa)*py 
                    ddx = abs(s1-ced)
                    y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x(3) = xlef + (ddx/dxa)*px
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                    y(5) = ylef + (ddx/dxa)*py 
                    x(1) = xlef
                    x(2) = xlef 
                    x(3) = x(3)
                    x(4) = xrig
                    x(5) = xrig
                    y(1) = y(1)
                    y(2) = y(2)
                    y(3) = yrig
                    y(4) = yrig
                    y(5) = y(5)
                    call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s2.lt.cst).and.(s3.gt.ced)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                x(1) = xlef + (ddx/dxa)*px
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                x(2) = xlef + (ddx/dxa)*px
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                y(4) = ylef + (ddx/dxa)*py 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                x(5) = xlef + (ddx/dxa)*px
                    x(1) = x(1)
                    x(2) = x(2) 
                    x(3) = xrig
                    x(4) = xrig
                    x(5) = x(5)
                    y(1) = ylef 
                    y(2) = yrig
                    y(3) = yrig
                    y(4) = y(4)
                    y(5) = ylef
                    call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.gt.ced).and.(s3.gt.ced)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x01 = xlef + (ddx/dxa)*px
                    y01 = ylef  
                    dxa = abs(s3-s1)
                    ddx = abs(s1-ced)
                    x02 = xlef
                    y02 = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x03 = xlef + (ddx/dxa)*px 
                    y03 = yrig 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-ced)
                    x04 = xrig 
                    y04 = ylef + (ddx/dxa)*py
                    eng1 = (x01-x02)**2 + (y01-y02)**2 
                    eng2 = (x01-x04)**2 + (y01-y04)**2 

                    dxa = abs(s3-s1)
                    ddx = abs(s1-cst)
                    y05 = ylef + (ddx/dxa)*py 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x06 = xlef + (ddx/dxa)*px
        ! ***POST***
                if(eng1.le.eng2) then 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x01
                    x(4) = x06
                    y(1) = y05
                    y(2) = y02
                    y(3) = ylef
                    y(4) = ylef 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                    x(1) = x03
                    x(2) = xrig
                    x(3) = xrig 
                    y(1) = yrig
                    y(2) = yrig
                    y(3) = y04 
                    call betmlK(x,y,3,nx,rr,gg,bb)
                    else
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x03
                    x(4) = xrig
                    x(5) = xrig
                    x(6) = x01
                    x(7) = x06
                    y(1) = y05
                    y(2) = y02
                    y(3) = yrig
                    y(4) = yrig
                    y(5) = y04
                    y(6) = ylef
                    y(7) = ylef
                    call betmlK(x,y,7,nx,rr,gg,bb)
                    endif 
                elseif((s1.gt.ced).and.(s2.lt.cst).and.(s3.lt.cst)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x01 = xlef + (ddx/dxa)*px
                    y01 = ylef  
                    dxa = abs(s3-s1)
                    ddx = abs(s1-cst)
                    x02 = xlef
                    y02 = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x03 = xlef + (ddx/dxa)*px 
                    y03 = yrig 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-cst)
                    x04 = xrig 
                    y04 = ylef + (ddx/dxa)*py
                    eng1 = (x01-x02)**2 + (y01-y02)**2 
                    eng2 = (x01-x04)**2 + (y01-y04)**2 

                    dxa = abs(s3-s1)
                    ddx = abs(s1-ced)
                    y05 = ylef + (ddx/dxa)*py 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x06 = xlef + (ddx/dxa)*px
        ! ***POST***
                if(eng1.le.eng2) then 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x01
                    x(4) = x06
                    y(1) = y05
                    y(2) = y02
                    y(3) = ylef
                    y(4) = ylef 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                    x(1) = x03
                    x(2) = xrig
                    x(3) = xrig 
                    y(1) = yrig
                    y(2) = yrig
                    y(3) = y04 
                    call betmlK(x,y,3,nx,rr,gg,bb)
                    else
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x03
                    x(4) = xrig
                    x(5) = xrig
                    x(6) = x01
                    x(7) = x06
                    y(1) = y05
                    y(2) = y02
                    y(3) = yrig
                    y(4) = yrig
                    y(5) = y04
                    y(6) = ylef
                    y(7) = ylef
                    call betmlK(x,y,7,nx,rr,gg,bb)
                    endif 
                endif
                elseif(((s1.lt.cst).or.(s1.gt.ced)).and.&
                       ((s2.lt.cst).or.(s2.gt.ced)).and.&
                       ((s3.lt.cst).or.(s3.gt.ced)).and.&
                       ((s4.lt.cst).or.(s4.gt.ced))) then 
                if((s1.gt.ced).and.(s2.gt.ced).and.&
                   (s3.lt.cst).and.(s4.gt.ced)) then 
                    dxa = abs(s3-s1)
                    ddx = abs(s1-ced)
                    y(1) = ylef + (ddx/dxa)*py 
                    ddx = abs(s1-cst)
                    y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x(3) = xlef + (ddx/dxa)*px
                    ddx = abs(s3-ced)
                    x(4) = xlef + (ddx/dxa)*px
                        x(1) = xlef
                        x(2) = xlef 
                        x(3) = x(3)
                        x(4) = x(4)
                        y(1) = y(1)
                        y(2) = y(2)
                        y(3) = yrig
                        y(4) = yrig 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s2.gt.ced).and.&
                      (s3.gt.ced).and.(s4.lt.cst)) then 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x(1) = xlef + (ddx/dxa)*px
                    ddx = abs(s3-cst)
                    x(2) = xlef + (ddx/dxa)*px
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                    y(3) = ylef + (ddx/dxa)*py 
                    ddx = abs(s2-ced)
                    y(4) = ylef + (ddx/dxa)*py 
                        x(1) = x(1)
                        x(2) = x(2) 
                        x(3) = xrig
                        x(4) = xrig 
                        y(1) = yrig
                        y(2) = yrig 
                        y(3) = y(3)
                        y(4) = y(4) 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.gt.ced).and.&
                       (s3.gt.ced).and.(s4.gt.ced)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                    y(1) = ylef + (ddx/dxa)*py 
                    ddx = abs(s1-ced)
                    y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x(3) = xlef + (ddx/dxa)*px
                    ddx = abs(s1-cst)
                    x(4) = xlef + (ddx/dxa)*px
                        x(1) = xlef
                        x(2) = xlef 
                        x(3) = x(3)
                        x(4) = x(4)
                        y(1) = y(1)
                        y(2) = y(2)
                        y(3) = ylef
                        y(4) = ylef 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s2.lt.cst).and.&
                       (s3.gt.ced).and.(s4.gt.ced)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x(1) = xlef + (ddx/dxa)*px
                    ddx = abs(s1-cst)
                    x(2) = xlef + (ddx/dxa)*px
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                    y(3) = ylef + (ddx/dxa)*py 
                    ddx = abs(s2-ced)
                    y(4) = ylef + (ddx/dxa)*py 
                        x(1) = x(1)
                        x(2) = x(2) 
                        x(3) = xrig
                        x(4) = xrig 
                        y(1) = ylef
                        y(2) = ylef 
                        y(3) = y(3)
                        y(4) = y(4) 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.lt.cst).and.&
                       (s3.gt.ced).and.(s4.gt.ced)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                    y(1) = ylef + (ddx/dxa)*py 
                    ddx = abs(s1-ced)
                    y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                    y(3) = ylef + (ddx/dxa)*py 
                    ddx = abs(s2-cst)
                    y(4) = ylef + (ddx/dxa)*py 
                        x(1) = xlef
                        x(2) = xlef 
                        x(3) = xrig
                        x(4) = xrig 
                        y(1) = y(1)
                        y(2) = y(2)
                        y(3) = y(3)
                        y(4) = y(4) 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s2.lt.cst).and.&
                       (s3.gt.ced).and.(s4.lt.cst)) then 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x(1) = xlef + (ddx/dxa)*px
                    ddx = abs(s3-cst)
                    x(2) = xlef + (ddx/dxa)*px
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x(3) = xlef + (ddx/dxa)*px
                    ddx = abs(s1-ced)
                    x(4) = xlef + (ddx/dxa)*px
                        x(1) = x(1)
                        x(2) = x(2) 
                        x(3) = x(3)
                        x(4) = x(4) 
                        y(1) = yrig
                        y(2) = yrig 
                        y(3) = ylef
                        y(4) = ylef  
                    call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s2.gt.ced).and.&
                       (s3.lt.cst).and.(s4.lt.cst)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                    y(1) = ylef + (ddx/dxa)*py 
                    ddx = abs(s1-cst)
                    y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                    y(3) = ylef + (ddx/dxa)*py 
                    ddx = abs(s2-ced)
                    y(4) = ylef + (ddx/dxa)*py 
                        x(1) = xlef
                        x(2) = xlef 
                        x(3) = xrig
                        x(4) = xrig 
                        y(1) = y(1)
                        y(2) = y(2)
                        y(3) = y(3)
                        y(4) = y(4) 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.gt.ced).and.&
                       (s3.lt.cst).and.(s4.gt.ced)) then 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x(1) = xlef + (ddx/dxa)*px
                    ddx = abs(s3-ced)
                    x(2) = xlef + (ddx/dxa)*px
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x(3) = xlef + (ddx/dxa)*px
                    ddx = abs(s1-cst)
                    x(4) = xlef + (ddx/dxa)*px
                        x(1) = x(1)
                        x(2) = x(2) 
                        x(3) = x(3)
                        x(4) = x(4) 
                        y(1) = yrig
                        y(2) = yrig 
                        y(3) = ylef
                        y(4) = ylef  
                    call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.gt.ced).and.&
                       (s3.lt.cst).and.(s4.lt.cst)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x(1) = xlef + (ddx/dxa)*px
                    ddx = abs(s1-ced)
                    x(2) = xlef + (ddx/dxa)*px
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                    y(3) = ylef + (ddx/dxa)*py 
                    ddx = abs(s2-cst)
                    y(4) = ylef + (ddx/dxa)*py 
                        x(1) = x(1)
                        x(2) = x(2) 
                        x(3) = xrig
                        x(4) = xrig
                        y(1) = ylef
                        y(2) = ylef 
                        y(3) = y(3)
                        y(4) = y(4)   
                    call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s2.lt.cst).and.&
                       (s3.lt.cst).and.(s4.lt.cst)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                    y(1) = ylef + (ddx/dxa)*py 
                    ddx = abs(s1-cst)
                    y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x(3) = xlef + (ddx/dxa)*px
                    ddx = abs(s1-ced)
                    x(4) = xlef + (ddx/dxa)*px
                        x(1) = xlef
                        x(2) = xlef 
                        x(3) = x(3)
                        x(4) = x(4)
                        y(1) = y(1)
                        y(2) = y(2) 
                        y(3) = ylef
                        y(4) = ylef    
                    call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.lt.cst).and.&
                      (s3.lt.cst).and.(s4.gt.ced)) then 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x(1) = xlef + (ddx/dxa)*px
                    ddx = abs(s3-ced)
                    x(2) = xlef + (ddx/dxa)*px
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                    y(3) = ylef + (ddx/dxa)*py 
                    ddx = abs(s2-cst)
                    y(4) = ylef + (ddx/dxa)*py 
                        x(1) = x(1)
                        x(2) = x(2) 
                        x(3) = xrig
                        x(4) = xrig
                        y(1) = yrig
                        y(2) = yrig 
                        y(3) = y(3)
                        y(4) = y(4)    
                    call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.lt.cst).and.&
                       (s3.gt.ced).and.(s4.lt.cst)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                    y(1) = ylef + (ddx/dxa)*py 
                    ddx = abs(s1-ced)
                    y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x(3) = xlef + (ddx/dxa)*px
                    ddx = abs(s3-cst)
                    x(4) = xlef + (ddx/dxa)*px
                        x(1) = xlef
                        x(2) = xlef 
                        x(3) = x(3)
                        x(4) = x(4)
                        y(1) = y(1)
                        y(2) = y(2) 
                        y(3) = yrig
                        y(4) = yrig     
                    call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.gt.ced).and.&
                       (s3.gt.ced).and.(s4.lt.cst)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x01u = xlef + (ddx/dxa)*px
                    y01u = ylef  
                    dxa = abs(s3-s1)
                    ddx = abs(s1-ced)
                    x02u = xlef
                    y02u = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x03u = xlef + (ddx/dxa)*px 
                    y03u = yrig 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-ced)
                    x04u = xrig 
                    y04u = ylef + (ddx/dxa)*py
                    engu1 = (x01u-x02u)**2 + (y01u-y02u)**2 
                    engu2 = (x01u-x04u)**2 + (y01u-y04u)**2 

                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x01d = xlef + (ddx/dxa)*px
                    y01d = ylef  
                    dxa = abs(s3-s1)
                    ddx = abs(s1-cst)
                    x02d = xlef
                    y02d = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x03d = xlef + (ddx/dxa)*px 
                    y03d = yrig 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-cst)
                    x04d = xrig 
                    y04d = ylef + (ddx/dxa)*py
                    engd1 = (x01d-x02d)**2 + (y01d-y02d)**2 
                    engd2 = (x01d-x04d)**2 + (y01d-y04d)**2 
                    
        ! cc             engu3 = (x01u-x03u)**2 + (y01u-y03u)**2 
        ! cc             engu4 = (x02u-x03u)**2 + (y02u-y03u)**2 
        ! cc             engu5 = (x02u-x04u)**2 + (y02u-y04u)**2 
        ! cc             engu6 = (x03u-x04u)**2 + (y03u-y04u)**2 

        ! cc             engd3 = (x01d-x03d)**2 + (y01d-y03d)**2 
        ! cc             engd4 = (x02d-x03d)**2 + (y02d-y03d)**2 
        ! cc             engd5 = (x02d-x04d)**2 + (y02d-y04d)**2 
        ! cc             engd6 = (x03d-x04d)**2 + (y03d-y04d)**2 

        ! cc            write(*,*) 'u' 
        ! cc            write(*,*) engu1,engu2,engu3 
        ! cc            write(*,*) engu4,engu5,engu6 
        ! cc            write(*,*) 'd'
        ! cc            write(*,*) engd1,engd2,engd3 
        ! cc            write(*,*) engd4,engd5,engd6 
        ! ***POST***
                if((engu1.le.engu2).and.(engd1.le.engd2)) then  
                        x(1) = xlef
                        x(2) = xlef
                        x(3) = x01u
                        x(4) = x01d
                        y(1) = y02d
                        y(2) = y02u
                        y(3) = ylef
                        y(4) = ylef 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                        x(1) = x03u
                        x(2) = x03d
                        x(3) = xrig
                        x(4) = xrig
                        y(1) = yrig
                        y(2) = yrig
                        y(3) = y04d
                        y(4) = y04u  
                    call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((engu1.le.engu2).and.(engd1.gt.engd2)) then  
                    write(*,*) 'subroutine contor-or.f 2148'
                elseif((engu1.gt.engu2).and.(engd1.le.engd2)) then  
                        x(1) = xlef
                        x(2) = xlef
                        x(3) = x03u
                        x(4) = x03d
                        x(5) = xrig
                        x(6) = xrig
                        x(7) = x01u
                        x(8) = x01d 
                        y(1) = y02d
                        y(2) = y02u
                        y(3) = yrig
                        y(4) = yrig
                        y(5) = y04d
                        y(6) = y04u
                        y(7) = ylef 
                        y(8) = ylef 
                    call betmlK(x,y,8,nx,rr,gg,bb)
                elseif((engu1.gt.engu2).and.(engd1.gt.engd2)) then  
                        x(1) = xlef
                        x(2) = xlef
                        x(3) = x03u
                        x(4) = x03d
                        y(1) = y02d
                        y(2) = y02u
                        y(3) = yrig
                        y(4) = yrig  
                    call betmlK(x,y,4,nx,rr,gg,bb)
                        x(1) = xrig
                        x(2) = xrig
                        x(3) = x01u
                        x(4) = x01d
                        y(1) = y04d
                        y(2) = y04u
                        y(3) = ylef
                        y(4) = ylef   
                    call betmlK(x,y,4,nx,rr,gg,bb)
                endif 
                elseif((s1.gt.ced).and.(s2.lt.cst).and.&
                       (s3.lt.cst).and.(s4.gt.ced)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x01u = xlef + (ddx/dxa)*px
                    y01u = ylef  
                    dxa = abs(s3-s1)
                    ddx = abs(s1-ced)
                    x02u = xlef
                    y02u = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x03u = xlef + (ddx/dxa)*px 
                    y03u = yrig 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-ced)
                    x04u = xrig 
                    y04u = ylef + (ddx/dxa)*py
                    engu1 = (x01u-x02u)**2 + (y01u-y02u)**2 
                    engu2 = (x01u-x04u)**2 + (y01u-y04u)**2 

                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x01d = xlef + (ddx/dxa)*px
                    y01d = ylef  
                    dxa = abs(s3-s1)
                    ddx = abs(s1-cst)
                    x02d = xlef
                    y02d = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x03d = xlef + (ddx/dxa)*px 
                    y03d = yrig 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-cst)
                    x04d = xrig 
                    y04d = ylef + (ddx/dxa)*py
                    engd1 = (x01d-x02d)**2 + (y01d-y02d)**2 
                    engd2 = (x01d-x04d)**2 + (y01d-y04d)**2 

        ! ***POST***
                if((engu1.le.engu2).and.(engd1.le.engd2)) then  
                    x(1) = xlef
                    x(2) = xlef 
                    x(3) = x01d
                    x(4) = x01u
                    y(1) = y02u
                    y(2) = y02d
                    y(3) = ylef
                    y(4) = ylef  
                    call betmlK(x,y,4,nx,rr,gg,bb)
                    x(1) = x03d
                    x(2) = x03u 
                    x(3) = xrig
                    x(4) = xrig
                    y(1) = yrig
                    y(2) = yrig 
                    y(3) = y04u
                    y(4) = y04d   
                    call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((engu1.le.engu2).and.(engd1.gt.engd2)) then  
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x03d
                    x(4) = x03u
                    x(5) = xrig
                    x(6) = xrig
                    x(7) = x01d
                    x(8) = x01u
                    y(1) = y02u
                    y(2) = y02d
                    y(3) = yrig
                    y(4) = yrig
                    y(5) = y04u
                    y(6) = y04d
                    y(7) = ylef
                    y(8) = ylef 
                    call betmlK(x,y,8,nx,rr,gg,bb)
                elseif((engu1.gt.engu2).and.(engd1.le.engd2)) then  
                    write(*,*) 'subroutine contor-or.f 2148'
                elseif((engu1.gt.engu2).and.(engd1.gt.engd2)) then  
                    x(1) = xlef
                    x(2) = xlef 
                    x(3) = x03d
                    x(4) = x03u
                    y(1) = y02u
                    y(2) = y02d 
                    y(3) = yrig
                    y(4) = yrig   
                    call betmlK(x,y,4,nx,rr,gg,bb)
                    x(1) = x01u
                    x(2) = xrig 
                    x(3) = xrig
                    x(4) = x01d
                    y(1) = ylef
                    y(2) = y04u 
                    y(3) = y04d
                    y(4) = ylef    
                    call betmlK(x,y,4,nx,rr,gg,bb)
                    endif
                    endif
                endif
        10    continue 
        20    continue
            call color(0)

            do i=ist , ied 
            do j=jst , jed 
                if(is(i,j).eq.2) is(i,j) = 1 
            enddo 
            enddo 

            return    
        ! c
        ! c     Programed by Hiroshi Kuroda (2003/02/22)
        ! c
        ! ***********************************************************************
    end 
    subroutine pscmaskK(px,py,imask,is,ie,js,je,ix,iy,r,g,b)
        ! ***********************************************************************
                dimension imask(ix,iy)

            do 20 i=is,ie
                lp = 0 
            do 10 j=js,je 
                if(imask(i,j).eq.0.and.lp.eq.0) then
                    lp = 1 
                    xx = px*(i-is)
                    yy = py*(j-js)
                    xlef = xx 
                    ylef = yy 
                elseif(imask(i,j).eq.1.and.lp.eq.1) then 
                    lp = 0 
                    xx = px*(i-is)
                    yy = py*(j-js)
                    xrig = xx + px 
                    yrig = yy  
                    call betsqK(xlef,ylef,xrig,yrig,r,g,b)  
                elseif(imask(i,j).eq.0 .and. lp.eq.1 .and. j.eq.je) then 
                    xx = px*(i-is)
                    yy = py*(j-js)
                    xrig = xx + px 
                    yrig = yy + py 
                    call betsqK(xlef,ylef,xrig,yrig,r,g,b)  
                elseif(imask(i,j).eq.0 .and. lp.eq.0 .and. j.eq.je) then 
                    xx = px*(i-is)
                    yy = py*(j-js)
                    xlef = xx 
                    ylef = yy 
                    xrig = xx + px 
                    yrig = yy + py 
                    call betsqK(xlef,ylef,xrig,yrig,r,g,b)  
                endif
        10   continue
        20   continue
            return
    end 
    subroutine betsqK(xlef,ylef,xrig,yrig,rr,gg,bb)
        ! ***********************************************************************
        ! c     xlef ==> Hidari Shita No x Zahyou
        ! c     ylef ==> Hidari Shita No y Zahyou
        ! c     xrig ==> Migi Ue No x Zahyou
        ! c     yrig ==> Migi Ue No y Zahyou
            call rgbK(rr,gg,bb)
            call plot(xlef,ylef,3)
            call plot(xrig,ylef,2)
            call plot(xrig,yrig,2)
            call plot(xlef,yrig,2)
            call plot(xlef,ylef,2)
            write(ounit,*) 'closepath'
            write(ounit,*) 'fill'
            call color(0)
            return
    end
    subroutine symbol(x, y, h, isymb, ang, n)
        implicit none
        character(len=*), intent(in) :: isymb
        integer, intent(in), optional :: n
        real, intent(in) :: x, y, h, ang
        character(len=256) :: segment
        integer :: i, iend, istart,count = 0,lol
    
        if (present(n)) lol = 420 ! no need to specify length anymore
        if (len_trim(isymb) > 256) print*, 'ARE YOU WRITING A BOOK??'
        write(ounit,*) "% begin symbol"
        write(ounit,*) "fo"
        write(ounit, 10) h
        10  format(f8.4, " sf")
        write(ounit,*) "se"
    
        call plot(x,y,-3)
        write(ounit, '(f9.4, 2x, a4)') ang, ' ro '
        istart = 1;count=0
        ! print*,len_trim(isymb)
        do i = 1, len_trim(isymb)
            if (isymb(i:i) == ';' .or. i == len_trim(isymb)) then
                if (isymb(i:i) == ';') then
                    iend = i - 1
                else
                    iend = i
                end if
                segment = isymb(istart:iend)
                write(ounit, '(3a)') "(", trim(segment), ") sh"
                if (isymb(i:i) == ';')then;call plot(0.,-h,-3);count = count+1;end if
                istart = iend +2
            end if
        end do
        call plot(0.,h*real(count),-3)
        write(ounit, '(f9.4, 2x, a4)') -ang, ' ro '
        call plot(-x,-y,-3)
        write(ounit,*) "% end symbol"
        return
    end 
    subroutine symbolc(x,y,h,isymb,ang,n)
        implicit none
        character(len=*), intent(in) :: isymb
        integer, intent(in), optional :: n
        real, intent(in) :: x, y, h, ang
        character(len=256) :: segment
        integer :: i, iend, istart,count = 0,lol
        if (present(n)) lol = 420 ! no need to specify length anymore
        if (len_trim(isymb) > 256) print*, 'ARE YOU WRITING A BOOK??'
        write(ounit,*) "% begin symbol"
        ! do i = 1, len_trim(adjustl((isymb)))
        !     if(iachar(isymb(i:i))<=127 )then
                write(ounit,*) "fo"
            ! else
                ! write(ounit,*) "jfo"
        !     end if
        ! end do
            ! print*,len_trim(isymb),isymb
        write(ounit, 10) h
        10  format(f8.4, " sf")
        write(ounit,*) "se"
    
        call plot(x,y,-3)
        write(ounit, '(f9.4, 2x, a4)') ang, ' ro '
        istart = 1;count=0
        ! print*,len_trim(isymb)
        do i = 1, len_trim(isymb)
            if (isymb(i:i) == ';' .or. i == len_trim(isymb)) then
                if (isymb(i:i) == ';') then
                    iend = i - 1
                else
                    iend = i
                end if
                segment = isymb(istart:iend)
                write(ounit,*) "(",trim(segment),") stringwidth pop " 
                write(ounit,*) 'neg 2 div 0 rmoveto '
                write(ounit, '(3a)') "(", trim(segment), ") sh"
                if (isymb(i:i) == ';')then;call plot(0.,-h,-3);count = count+1
                endif
                istart = iend +2
            end if
        end do
        call plot(0.,h*real(count),-3)
        write(ounit, '(f9.4, 2x, a4)') -ang, ' ro '
        call plot(-x,-y,-3)
        write(ounit,*) "% end symbolc"
        return
    end     
    subroutine symbolr(x,y,h,isymb,ang,n)
        implicit none
        character(len=*), intent(in) :: isymb
        integer, intent(in), optional :: n
        real, intent(in) :: x, y, h, ang
        character(len=256) :: segment
        integer :: i, iend, istart,count = 0,lol
        if (present(n)) lol = 420 ! no need to specify length anymore
        if (len_trim(isymb) > 256) print*, 'ARE YOU WRITING A BOOK??'
        write(ounit,*) "% begin symbol"
        write(ounit,*) "fo"
        write(ounit, 10) h
        10  format(f8.4, " sf")
        write(ounit,*) "se"

        call plot(x,y,-3)
        write(ounit, '(f9.4, 2x, a4)') ang, ' ro '
        istart = 1;count=0
        ! print*,len_trim(isymb)
        do i = 1, len_trim(isymb)
            if (isymb(i:i) == ';' .or. i == len_trim(isymb)) then
                if (isymb(i:i) == ';') then
                    iend = i - 1
                else
                    iend = i
                end if
                segment = isymb(istart:iend)
                write(ounit,*) "(",trim(segment),") stringwidth pop " 
                write(ounit,*) 'neg 0 rmoveto '
                write(ounit, '(3a)') "(", trim(segment), ") sh"
                if (isymb(i:i) == ';')then;call plot(0.,-h,-3);count = count+1;endif
                istart = iend +2
            end if
        end do
        call plot(0.,h*real(count),-3)
        write(ounit, '(f9.4, 2x, a4)') -ang, ' ro '
        call plot(-x,-y,-3)
        write(ounit,*) "% end symbolr"
        return
    end
end module origin

module oldsubs
    use origin
    implicit none
    contains
    ! creating temp_5,sal_5 using original data
    subroutine temsal(temp_5,sal_5)
        implicit none
        integer,parameter::years = 15, months = 12,lines = 2, stations = 9,depth = 1000
        real,dimension(years,months,lines,stations,depth),intent(out)::temp_5,sal_5
        character::yyyy*9,tempfile*999,salfile*999,line*20,mm*9
        integer::y,m,l,st,d

        102 format(9(f9.4))
        do m = 1, months
            write(mm,'(i2.2)')m
            do l = 1,lines
                if(l==1) then;line = 'N-Line'
                else;line = 'S-Line';end if
                    do y = 1, years
                        write(yyyy,'(i4.4)')y+2008
                        tempfile = '../Data/Yuta_unedited/'//'/'//trim(line)//'/'//trim(mm)//'/01tem'//trim(yyyy)//'.csv'
                        salfile = '../Data/Yuta_unedited/'//'/'//trim(line)//'/'//trim(mm)//'/01sal'//trim(yyyy)//'.csv'
                        open(11,file = tempfile,status = 'old', action = 'read')
                        do d = 1, depth
                            read(11,102)(temp_5(y,m,l,st,d),st = 1,stations)
                        end do
                        close(11)
                        open(77,file = salfile,status ='old',action = 'read')
                        do d = 1,depth
                            read(77,102)(sal_5(y,m,l,st,d),st = 1,stations)
                        end do
                        close(77)
                    end do
            end do
        end do
    end subroutine
    ! creating potemp and sal from 51db median filtered data
    subroutine potempsal_51(potemp_5,sal_5)
        implicit none
        integer,parameter::years = 15, months = 12,lines = 2, stations = 9,depth = 400
        real,dimension(years,months,lines,stations,depth),intent(out)::potemp_5,sal_5
        character::yyyy*9,tempfile*999,salfile*999,line*20,mm*9
        integer::y,m,l,st,d

        102 format(9(f9.4))
        do m = 1, months
            write(mm,'(i2.2)')m
            do l = 1,lines
                if(l==1) then;line = 'N-Line'
                else;line = 'S-Line';end if
                    do y = 1, years
                        write(yyyy,'(i4.4)')y+2008
                        tempfile = '../Data/51_Median'//'/'//trim(line)//'/'//trim(mm)//'/'//'51potemp'//trim(yyyy)//'.csv'  
                        salfile = '../Data/51_Median'//'/'//trim(line)//'/'//trim(mm)//'/51sal'//trim(yyyy)//'.csv'
                        open(11,file = tempfile,status = 'old', action = 'read')
                        do d = 1, depth
                            read(11,102)(potemp_5(y,m,l,st,d),st = 1,stations)
                        end do
                        close(11)
                        open(77,file = salfile,status ='old',action = 'read')
                        do d = 1,depth
                            read(77,102)(sal_5(y,m,l,st,d),st = 1,stations)
                        end do
                        close(77)
                    end do
            end do
        end do
        
    end subroutine
    ! creating potemp and sal from 25db median filtered data
    subroutine potempsal_25(potemp_5,sal_5)
        implicit none
        integer,parameter::years = 15, months = 12,lines = 2, stations = 9,depth = 400
        real,dimension(years,months,lines,stations,depth),intent(out)::potemp_5,sal_5
        character::yyyy*9,tempfile*999,salfile*999,line*20,mm*9
        integer::y,m,l,st,d

        102 format(9(f9.4))
        do m = 1, months
            write(mm,'(i2.2)')m
            do l = 1,lines
                if(l==1) then;line = 'N-Line'
                else;line = 'S-Line';end if
                    do y = 1, years
                        write(yyyy,'(i4.4)')y+2008
                        tempfile = '../Data/25_Median'//'/'//trim(line)//'/'//trim(mm)//'/25potemp'//trim(yyyy)//'.csv'
                        salfile = '../Data/25_Median'//'/'//trim(line)//'/'//trim(mm)//'/25sal'//trim(yyyy)//'.csv'
                        open(11,file = tempfile,status = 'old', action = 'read')
                        do d = 1, depth
                            read(11,102)(potemp_5(y,m,l,st,d),st = 1,stations)
                        end do
                        close(11)
                        open(77,file = salfile,status ='old',action = 'read')
                        do d = 1,depth
                            read(77,102)(sal_5(y,m,l,st,d),st = 1,stations)
                        end do
                        close(77)
                    end do
            end do
        end do
        
    end subroutine
                                                !ABOVE DATA IS NOT REALLY FOR USE!

                                                ! SUBROUTINES FOR DATA OBTAINMENT !

    ! from a rectangle numeric file to a 2D array
    subroutine csv2array(filename,format,column_quan,row_quan,twoD_array)
        implicit none
        real,dimension(column_quan,row_quan),intent(out)::twoD_array
        integer,intent(in)::column_quan,row_quan
        character,intent(in)::filename*999,format*99
        integer::c,r,ios

        open(11,file = filename,status = 'old',action='read')
        do r = 1, row_quan
            read(11,format,iostat=ios)(twoD_array(c,r),c = 1, column_quan)
            if(ios /= 0) then
                exit
            else;end if
        end do
        close(11)
    end subroutine
    ! calibrated data for potempsal_51
    subroutine calibrated_data51(potemp_c5,sal_c5)
        implicit none
        integer,parameter:: years = 15, months = 12, lines = 2, stations = 9, depth = 400
        real,dimension(:,:,:,:,:),allocatable::potemp_5,sal_5
        ! real,dimension(years,months,lines,stations,depth)::potemp_5,sal_5
        real,dimension(years,months,lines,stations,depth),intent(out)::potemp_c5,sal_c5
        real,parameter::standard_sal_400 = 34.07
        real::diff,initial_num
        integer::y,m,l,st,d,initial_depth

        allocate(potemp_5(years,months,lines,stations,depth))
        allocate(sal_5(years,months,lines,stations,depth))
        potemp_5 = 0.;sal_5 = 0.
        call potempsal_51(potemp_5,sal_5)
        ! print*,sal_5(1,8,1,7,1:depth)
        sal_5(1,12,2,5,1:depth)=0.;sal_5(1,12,2,6,1:depth)=0.;sal_5(1,12,2,7,1:depth)=0.!May 10 金サロ前 Sline dgf
        sal_5(2,4,1,4,1:depth)=0. ! 2010 april Nline station 4 erased
        sal_5(4,1:months,1:lines,1:stations,1:depth)=0. !2012 data all erased

        potemp_5(1,12,2,5,1:depth)=0.;potemp_5(1,12,2,6,1:depth)=0.;potemp_5(1,12,2,7,1:depth)=0.!May 10 金サロ前 Sline dgf
        potemp_5(2,4,1,4,1:depth)=0. ! 2010 april Nline station 4 erased
        potemp_5(4,1:months,1:lines,1:stations,1:depth)=0. !2012 data all erased

        !データが最初に存在する深さの数値を表面まで持ってくる。
        do y = 1, years 
            do m = 1, months
                do l = 1, lines
                    do st = 1,stations
                        if (potemp_5(y,m,l,st,50)/=0.) then
                            do d = 1,50
                                if (potemp_5(y,m,l,st,d) /= 0.) then
                                    initial_depth = d
                                    initial_num = potemp_5(y,m,l,st,d)
                                    exit
                                else;end if
                            end do
                        potemp_5(y,m,l,st,1:initial_depth-1) = initial_num
                        else !(if potemp_5(y,m,l,st,50)==0.)
                        end if
                        if (sal_5(y,m,l,st,50)/=0.) then
                            do d = 1,50
                                if (sal_5(y,m,l,st,d) /= 0.) then
                                    initial_depth = d
                                    initial_num = sal_5(y,m,l,st,d)
                                    exit
                                else;end if
                            end do
                        sal_5(y,m,l,st,1:initial_depth-1) = initial_num
                        else !(if sal_5(y,m,l,st,50)==0.)
                        end if
                    end do
                end do
            end do
        end do

        !底の塩分を全ての月において34.07にしてしまう
        !2023 august station 9 はデータが348までしかないので以下のプログラムにより欠測になる
        do y = 1, years
            do m = 1,months
                do l = 1,lines
                    do st = 1, stations
                        if (sal_5(y,m,l,st,depth)<30. .or. sal_5(y,m,l,st,depth)>35.) then
                            sal_5(y,m,l,st,1:depth) = 0.  !一列全部0 データが元から無い列も0だからこれに含まれる
                            potemp_5(y,m,l,st,1:depth) = 0.
                        else
                            diff = sal_5(y,m,l,st,depth) - standard_sal_400 
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

        potemp_c5(1:years,1:months,1:lines,1:stations,1:depth) = potemp_5(1:years,1:months,1:lines,1:stations,1:depth)
        sal_c5(1:years,1:months,1:lines,1:stations,1:depth) = sal_5(1:years,1:months,1:lines,1:stations,1:depth)
        ! print*,sal_c5(1,8,1,7,1:depth)
        deallocate(potemp_5);deallocate(sal_5)
        
    end subroutine
    ! calibrated_data for potempsal_25
    subroutine calibrated_data25(potemp_c5,sal_c5)
        implicit none
        integer,parameter:: years = 15, months = 12, lines = 2, stations = 9, depth = 400
        real,dimension(:,:,:,:,:),allocatable::potemp_5,sal_5
        real,dimension(years,months,lines,stations,depth),intent(out)::potemp_c5,sal_c5
        real,parameter::standard_sal_400 = 34.07
        real::diff,initial_num
        integer::y,m,l,st,d,initial_depth

        allocate(potemp_5(years,months,lines,stations,depth))
        allocate(sal_5(years,months,lines,stations,depth))
        potemp_5 = 0.;sal_5 = 0.
        call potempsal_25(potemp_5,sal_5)
        sal_5(1,12,2,5,1:depth)=0.;sal_5(1,12,2,6,1:depth)=0.;sal_5(1,12,2,7,1:depth)=0.!May 10 金サロ前 Sline dgf
        sal_5(2,4,1,4,1:depth)=0. ! 2010 april Nline station 4 erased
        sal_5(4,1:months,1:lines,1:stations,1:depth)=0. 

        potemp_5(1,12,2,5,1:depth)=0.;potemp_5(1,12,2,6,1:depth)=0.;potemp_5(1,12,2,7,1:depth)=0.!May 10 金サロ前 Sline dgf
        potemp_5(2,4,1,4,1:depth)=0. ! 2010 april Nline station 4 erased
        potemp_5(4,1:months,1:lines,1:stations,1:depth)=0. !2012 data all erased

        sal_5(5,5,2,6,272:279) = 34.0304    !途中で0が続く変な時があるSline 前後の値と同じにした

        !データが最初に存在する深さの数値を表面まで持ってくる。
        do y = 1, years 
            do m = 1, months
                do l = 1, lines
                    do st = 1,stations
                        if (potemp_5(y,m,l,st,50)/=0.) then
                            do d = 1,50
                                if (potemp_5(y,m,l,st,d) /= 0.) then
                                    initial_depth = d
                                    initial_num = potemp_5(y,m,l,st,d)
                                    exit
                                else;end if
                            end do
                        potemp_5(y,m,l,st,1:initial_depth-1) = initial_num
                        else !(if potemp_5(y,m,l,st,50)==0.)
                        end if
                        if (sal_5(y,m,l,st,50)/=0.) then
                            do d = 1,50
                                if (sal_5(y,m,l,st,d) /= 0.) then
                                    initial_depth = d
                                    initial_num = sal_5(y,m,l,st,d)
                                    exit
                                else;end if
                            end do
                        sal_5(y,m,l,st,1:initial_depth-1) = initial_num
                        else !(if sal_5(y,m,l,st,50)==0.)
                        end if
                    end do
                end do
            end do
        end do

        !底の塩分を全ての月において34.07としてしまう。
        !2023 august station 9 はデータが348までしかないので以下のプログラムにより欠測になる
        do y = 1, years
            do m = 1,months
                do l = 1,lines
                    do st = 1, stations
                        if (sal_5(y,m,l,st,depth)<30. .or. sal_5(y,m,l,st,depth)>35.) then
                            sal_5(y,m,l,st,1:depth) = 0.  !一列全部0 データが元から無い列も0だからこれに含まれる
                            potemp_5(y,m,l,st,1:depth) = 0.
                        else
                            diff = sal_5(y,m,l,st,depth) - standard_sal_400 
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

        potemp_c5(1:years,1:months,1:lines,1:stations,1:depth) = potemp_5(1:years,1:months,1:lines,1:stations,1:depth)
        sal_c5(1:years,1:months,1:lines,1:stations,1:depth) = sal_5(1:years,1:months,1:lines,1:stations,1:depth)
        deallocate(potemp_5);deallocate(sal_5)
        
    end subroutine
    ! put geostrophic velocity into an array for 25 or 51db median filtered data
    subroutine geovel_array(medianfiltertype,geovel_5) 
        implicit none
        integer,parameter::years = 15, months = 12, lines = 2, stations = 9, depth = 400
        real,dimension(years,months,lines,stations,depth),intent(out)::geovel_5
        integer,intent(in)::medianfiltertype !25 or 51
        integer::y,m,l,st,d
        character::mm*9,aaaa*9,filename*999,line*9

        102 format(9(f9.4))
        if (medianfiltertype == 25) then
            do l = 1,lines
                if(l==1) then;line = 'N-Line';else; line='S-Line';end if
                do m = 1, months
                    write(mm,'(i2.2)')m
                    do y = 1, years
                        write(aaaa,'(i4.4)')y+2008
                        filename = '../Data/Geostrophy/'//trim(line)//'/'//trim(mm)//'/Velocity_25median'//trim(aaaa)//'.csv'
                        open(77,file = filename,status = 'old',action = 'read')
                        do d = 1,depth
                            read(77,102)(geovel_5(y,m,l,st,d),st = 1,stations)
                        end do
                    end do
                end do
            end do
        else if(medianfiltertype == 51) then
            do l = 1,lines
                if(l==1) then;line = 'N-Line';else; line='S-Line';end if
                do m = 1, months
                    write(mm,'(i2.2)')m
                    do y = 1, years
                        write(aaaa,'(i4.4)')y+2008
                        filename = '../Data/Geostrophy/'//trim(line)//'/'//trim(mm)//'/Velocity_51median'//trim(aaaa)//'.csv'
                        open(77,file = filename,status = 'old',action = 'read')
                        do d = 1,depth
                            read(77,102)(geovel_5(y,m,l,st,d),st = 1,stations)
                        end do
                    end do
                end do
            end do
        else;print*,'select either 25 or 51 for type of median filter'
        end if

    end subroutine
    ! monthly geostrophic transport only through Station1-6(4-9)
    subroutine geotransport(medianfiltertype,geotrans)
        implicit none
        integer,parameter::years = 15, months = 12, lines = 2
        real,dimension(years,months,lines),intent(out)::geotrans
        integer,intent(in)::medianfiltertype
        integer::y,m,l
        character::yyyy*9,filename*999,mm*9,line*9

        
        102 format(f9.4)
        if (medianfiltertype == 25) then
            do l = 1, lines
                if(l==1) then;line = 'N-Line';else; line='S-Line';end if
                do m = 1, months
                    write(mm,'(i2.2)')m
                    do y = 1, years
                        write(yyyy,'(i4.4)')y+2008
                        filename = '../Data/Geostrophy/'//trim(line)//'/'//trim(mm)//'/Transport_25Median'//trim(yyyy)//'.csv'
                        open(21,file = filename,status = 'old',action = 'read')
                            read(21,102)geotrans(y,m,l)
                        close(21)
                    end do
                end do
            end do
        else if(medianfiltertype == 51) then
            do l = 1, lines
                if(l==1) then;line = 'N-Line';else; line='S-Line';end if
                do m = 1, months
                    write(mm,'(i2.2)')m
                    do y = 1, years
                        write(yyyy,'(i4.4)')y+2008
                        filename = '../Data/Geostrophy/'//trim(line)//'/'//trim(mm)//'/Transport_51Median'//trim(yyyy)//'.csv'
                        open(31,file = filename,status = 'old',action = 'read')
                            read(31,102)geotrans(y,m,l)
                        close(31)
                    end do
                end do
            end do
        else;print*,'select either 25 or 51 for type of median filter'
        end if

    end subroutine
    ! gives an array of monthly mean sea surface height and pressure in Fukaura 2009-2023
    subroutine fukauraSSH(SSH_array)
        implicit none
        integer,parameter::years = 15, months = 12
        real,dimension(years,months),intent(out)::SSH_array
        integer::y,m,ios,yyyy,mm,nodata
        real::SSH,SSP
        character::filename*999

        ! 102 format((i4),(i2.2),(f9.4))
        ! 103 format((i4),(i2.2),(f9.4),(i2),(f9.4))
        filename = '../Data/SSH/Fukaura_Tides.csv'
        open(92,file = filename, status = 'old',action = 'read')
        do y = 1,years
            do m = 1,12
                read(92,*,iostat = ios)yyyy,mm,SSH,nodata,SSP
                ! print*,yyyy,mm,SSH,nodata,SSP
                SSH_array(y,m) = SSH
            end do
        end do
        close(92)
        ! calibratedの方から逆補正，SSPの影響を足す
        ! SSH_array(2,11) = 1677.7 - (10163.-10130.)
    end subroutine
    subroutine calibrated_fukauraSSH(calibrated_SSH_array)
        implicit none
        integer,parameter::years = 15, months = 12
        real,dimension(years,months),intent(out)::calibrated_SSH_array
        real,dimension(years,months)::SSH_array=0.,SSP_array=0.
        integer::y,m,ios,yyyy,mm,nodata
        real::SSH,SSP
        character::filename*999

        ! 102 format((i4),(i2.2),(f9.4))
        ! 103 format((i4),(i2.2),(f9.4),(i2),(f9.4))
        filename = '../Data/SSH/Fukaura_Tides.csv'
        open(92,file = filename, status = 'old',action = 'read')
        do y = 1,years
            do m = 1,12
                read(92,*,iostat = ios)yyyy,mm,SSH,nodata,SSP
                ! print*,yyyy,mm,SSH,nodata,SSP
                SSH_array(y,m) = SSH;SSP_array(y,m) = SSP
            end do
        end do
        close(92)
        do y = 1,years
            do m = 1,months
                if(SSH_array(y,m)/=0.) then
                    calibrated_SSH_array(y,m) = SSH_array(y,m) + (SSP_array(y,m)-10130)
                else;calibrated_SSH_array(y,m) = 0.
                end if
            end do
        end do
        ! calibrated_SSH_array(2,11) = (sum(calibrated_SSH_array(1:15,11))/real(years-1))

    end subroutine
    ! SSH and SSP at Tappi
    subroutine tappiSSH(SSH_array)
        implicit none
        integer,parameter::years = 15, months = 12
        real,dimension(years,months),intent(out)::SSH_array
        integer::y,m,ios,yyyy,mm,nodata
        real::SSH,SSP
        character::filename*999

        ! 102 format((i4),(i2.2),(f9.4))
        ! 103 format((i4),(i2.2),(f9.4),(i2),(f9.4))
        filename = '../Data/SSH/Tappi_Tides.csv'
        open(92,file = filename, status = 'old',action = 'read')
        do y = 1,years
            do m = 1,12
                read(92,*,iostat = ios)yyyy,mm,SSH,nodata,SSP
                ! print*,yyyy,mm,SSH,nodata,SSP
                SSH_array(y,m) = SSH
            end do
        end do
        close(92)
        ! calibratedの方から逆補正，SSPの影響を足す
        ! SSH_array(11,2) = 983.5 - (10177.-10130.)
    end subroutine
    subroutine calibrated_tappiSSH(calibrated_SSH_array)
        implicit none
        integer,parameter::years = 15, months = 12
        real,dimension(years,months),intent(out)::calibrated_SSH_array
        real,dimension(years,months)::SSH_array=0.,SSP_array=0.
        integer::y,m,ios,yyyy,mm,nodata
        real::SSH,SSP
        character::filename*999

        ! 102 format((i4),(i2.2),(f9.4))
        ! 103 format((i4),(i2.2),(f9.4),(i2),(f9.4))
        filename = '../Data/SSH/Tappi_Tides.csv'
        open(92,file = filename, status = 'old',action = 'read')
        do y = 1,years
            do m = 1,12
                read(92,*,iostat = ios)yyyy,mm,SSH,nodata,SSP
                ! print*,yyyy,mm,SSH,nodata,SSP
                SSH_array(y,m) = SSH;SSP_array(y,m) = SSP
            end do
        end do
        close(92)
        do y = 1,years
            do m = 1,months
                if(SSH_array(y,m)/=0.) then
                    calibrated_SSH_array(y,m) = SSH_array(y,m) + (SSP_array(y,m)-10130)
                else;calibrated_SSH_array(y,m) = 0.
                end if
            end do
        end do
        ! calibrated_SSH_array(11,2) = sum(calibrated_SSH_array(1:15,2))/real(years-2)
    end subroutine
    subroutine matsumaeSSH(SSH_array)
        implicit none
        integer,parameter::years = 15, months = 12
        real,dimension(years,months),intent(out)::SSH_array
        integer::y,m,ios,yyyy,mm,nodata
        real::SSH,SSP
        character::filename*999

        ! 102 format((i4),(i2.2),(f9.4))
        ! 103 format((i4),(i2.2),(f9.4),(i2),(f9.4))
        filename = '../Data/SSH/Matsumae_Tides.csv'
        open(92,file = filename, status = 'old',action = 'read')
        do y = 1,years
            do m = 1,12
                read(92,*,iostat = ios)yyyy,mm,SSH,nodata,SSP
                ! print*,yyyy,mm,SSH,nodata,SSP
                SSH_array(y,m) = SSH
            end do
        end do
        close(92)
    end subroutine
    subroutine calibrated_matsumaeSSH(calibrated_SSH_array)
        implicit none
        integer,parameter::years = 15, months = 12
        real,dimension(years,months),intent(out)::calibrated_SSH_array
        real,dimension(years,months)::SSH_array=0.,SSP_array=0.
        integer::y,m,ios,yyyy,mm,nodata
        real::SSH,SSP
        character::filename*999

        ! 102 format((i4),(i2.2),(f9.4))
        ! 103 format((i4),(i2.2),(f9.4),(i2),(f9.4))
        filename = '../Data/SSH/Matsumae_Tides.csv'
        open(92,file = filename, status = 'old',action = 'read')
        do y = 1,years
            do m = 1,12
                read(92,*,iostat = ios)yyyy,mm,SSH,nodata,SSP
                ! print*,ios,yyyy,mm,SSH,nodata,SSP
                SSH_array(y,m) = SSH;SSP_array(y,m) = SSP
            end do
        end do
        close(92)
        do y = 1,years
            do m = 1,months
                if(SSH_array(y,m)/=0.) then
                    calibrated_SSH_array(y,m) = SSH_array(y,m) + (SSP_array(y,m)-10130)
                else;calibrated_SSH_array(y,m) = 0.
                end if
            end do
        end do

    end subroutine
                                                ! SUBROUTINES FOR DATA OBTAINMENT !

                                                ! SUBROUTINES FOR DATA MANIPULATION !

    ! creating sigma array from potemp_5 and sal_5
    subroutine create_sigma_array(temp_array,sal_array,sigma_array)
        implicit none
        integer,parameter::years = 15, months = 12, lines = 2, stations = 9, depth = 400
        real,dimension(years,months,lines,stations,depth),intent(in)::temp_array,sal_array
        real,dimension(years,months,lines,stations,depth),intent(out)::sigma_array
        real::sigma
        integer::y,m,l,st,d

        do y = 1,years
            do m = 1, months
                do l = 1, lines
                    do st = 1, stations
                        do d = 1, depth
                            call sigma_T_S(sigma,temp_array(y,m,l,st,d),sal_array(y,m,l,st,d))
                            sigma_array(y,m,l,st,d) = sigma
                        end do
                    end do
                end do
            end do
        end do
    end subroutine
    ! creating an array of dynamic height from sigma array. 
    subroutine create_DH_array(sigma_array,DH_array)
        implicit none
        integer,parameter:: years = 15, months = 12, lines = 2, stations = 9, depth = 400
        real,dimension(years,months,lines,stations,depth),intent(in)::sigma_array
        real,dimension(years,months,lines,stations,depth),intent(out)::DH_array
        integer::y,m,l,st,d
        double precision::hiyou
        double precision::zero;double precision::one;double precision::four;double precision::ten;double precision::thousand
        double precision::firstsum=0.0d0;double precision::DH=0.0d0;double precision::g
        zero = 0.0d0;one = 1.0d0;four = 4.0d0;ten = 10.0d0;thousand = 1000.0d0; g = 9.8d0

        do y = 1, years
            do m = 1, months
                do l = 1, lines
                    do st = 1,stations
                        do d = 1, depth
                            if(sigma_array(y,m,l,st,d)/=0.)then
                                hiyou = one/((thousand+sigma_array(y,m,l,st,d))) !m^3/kg
                            else;hiyou = zero
                            end if
                            DH = firstsum + hiyou*ten**(four)/g
                            firstsum = DH
                            DH_array(y,m,l,st,d) = real(DH)
                        end do
                        firstsum = zero
                    end do
                end do
            end do
        end do

    end subroutine
    ! for calculating potential density as sigma
    subroutine sigma_T_S(sigma,potemp,sal)

        real,intent(in)::potemp,sal
        real,intent(out)::sigma
        !密度計算
                !!!Keisan Parameter
            double precision::a0,a1,a2,a3,a4,a5
            double precision::b0,b1,b2,b3,b4
            double precision::c0,c1,c2,c3
            double precision::d0,d1
            double precision::e0,e1,e2,e3,e4
            double precision::f0,f1,f2,f3
            double precision::g0,g1,g2
            double precision::h0,h1,h2,h3
            double precision::i0,i1,i2
            double precision::j0
            double precision::k0,k1,k2
            double precision::m0,m1,m2
            double precision::S,t,pp,rho,rhow,KK,Kt,AAAA,BB,Kw,Aw,Bw,rhoafter
            ! double precision::q,Gamma,Theta,xk
        
        
        
        !密度パラメータ
            a0=3.5803E-5
            a1=8.5258E-6
            a2=-6.8360E-8
            a3=6.6228E-10
            b0=1.8932E-6
            b1=-4.2393E-8
            c0=1.8741E-8
            c1=-6.7795E-10
            c2=8.733E-12
            c3=-5.4481E-14
            d0=-1.1351E-10
            d1=2.7759E-12
            e0=-4.6206E-13
            e1=1.8676E-14
            e2=-2.1687E-16
        
            
            a0=999.842594
            a1=6.793952E-2
            a2=-9.095290E-3
            a3=1.001685E-4
            a4=-1.120083E-6
            a5=6.536332E-9
            b0=8.24493E-1
            b1=-4.0899E-3
            b2=7.6438E-5
            b3=-8.2467E-7
            b4=5.3875E-9
            c0=-5.72466E-3
            c1=1.0227E-4
            c2=-1.6546E-6
            d0=4.8314E-4
            e0=19652.21
            e1=148.4206
            e2=-2.327105
            e3=1.360477E-2
            e4=-5.155288E-5
            f0=54.6746
            f1=-0.603459
            f2=1.09987E-2
            f3=-6.1670E-5
            g0=7.944E-2
            g1=1.6483E-2
            g2=-5.3009E-4
            h0=3.239908
            h1=1.43713E-3
            h2=1.16092E-4
            h3=-5.77905E-7
            i0=2.2838E-3
            i1=-1.0981E-5
            i2=-1.6078E-6
            j0=1.91075E-4
            k0=8.50935E-5
            k1=-6.12293E-6
            k2=5.2787E-8
            m0=-9.9348E-7
            m1=2.0816E-8
            m2=9.1697E-10
        !パラメータ終わり
        
        !密度計算
                t=potemp;pp=0.;S=sal
                rhow=a0+a1*t+a2*t**2.+a3*t**3.+a4*t**4.+a5*t**5.
                rho=rhow+(b0+b1*t+b2*t**2.+b3*t**3.+b4*t**4.)*S+(c0+c1*t+c2*t**2.)*S**(3./2.)+d0*S**2 
                Kw=e0+e1*t+e2*t**2.+e3*t**3.+e4*t**4.
                Aw=h0+h1*t+h2*t**2.+h3*t**3.
                Bw=k0+k1*t+k2*t**2.
                AAAA=Aw+(i0+i1*t+i2*t**2.)*S+j0*S**(3./2.)
                BB=Bw+(m0+m1*t+m2*t**2.)*S
                Kt=Kw+(f0+f1*t+f2*t**2.+f3*t**3.)*S+(g0+g1*t+g2*t**2.)*S**(3./2.)
                KK=Kt+AAAA*pp+BB*pp**2.
                rhoafter=rho/(1.-pp/KK)
                if (S==0) then
                sigma=0.
                else
                sigma=real(rhoafter)-1000.
                end if 
        !密度計算終わり
        
        
    end subroutine
    ! for calculating potential temperature
    subroutine potemp_T_S_depth(potemp,tem,sal,depth)
        real,intent(in)::tem,sal,depth
        real,intent(out)::potemp
        !密度計算
        !!!Keisan Parameter
        double precision::a0,a1,a2,a3
        double precision::b0,b1
        double precision::c0,c1,c2,c3
        double precision::d0,d1
        double precision::e0,e1,e2
        ! double precision::f0,f1,f2,f3
        ! double precision::g0,g1,g2
        ! double precision::h0,h1,h2,h3
        ! double precision::i0,i1,i2
        ! double precision::j0
        ! double precision::k0,k1,k2
        ! double precision::m0,m1,m2
        double precision::d,S,t,p
        double precision::q,Gamma,Theta,xk
            a0=3.5803E-5
            a1=8.5258E-6
            a2=-6.8360E-8
            a3=6.6228E-10
            b0=1.8932E-6
            b1=-4.2393E-8
            c0=1.8741E-8
            c1=-6.7795E-10
            c2=8.733E-12
            c3=-5.4481E-14
            d0=-1.1351E-10
            d1=2.7759E-12
            e0=-4.6206E-13
            e1=1.8676E-14
            e2=-2.1687E-16

            !ポテンシャル水温計算
            S=sal
            t=tem
            d=0.-depth
            Gamma=a0+a1*t+a2*(t**2.)+a3*(t**3.)+(S-35.)*(b0+b1*t)+depth*(c0+c1*t+c2*(t**2.)+c3*(t**3.))+depth*(S-35.)*(d0+d1*t)+(depth**2.)*(e0+e1*t+e2*(t**2.))
            xk=d*Gamma
            t=t+0.5*xk
            q=xk
            p=depth+0.5*d
            Gamma=a0+a1*t+a2*(t**2.)+a3*(t**3.)+(S-35.)*(b0+b1*t)+p*(c0+c1*t+c2*(t**2.)+c3*(t**3.))+p*(S-35.)*(d0+d1*t)+(p**2.)*(e0+e1*t+e2*(t**2.))
            xk=d*Gamma
            t=t+0.29289322*(xk-q)
            q=0.58578644*xk+0.121320344*q
            Gamma=a0+a1*t+a2*(t**2.)+a3*(t**3.)+(S-35.)*(b0+b1*t)+p*(c0+c1*t+c2*(t**2.)+c3*(t**3.))+p*(S-35.)*(d0+d1*t)+(p**2.)*(e0+e1*t+e2*(t**2.))
            xk=d*Gamma
            t=t+1.707106781*(xk-q)
            q=3.414213562*xk-4.121320344*q
            p=p+0.5*d
            Gamma=a0+a1*t+a2*(t**2.)+a3*(t**3.)+(S-35.)*(b0+b1*t)+p*(c0+c1*t+c2*(t**2.)+c3*(t**3.))+p*(S-35.)*(d0+d1*t)+(p**2.)*(e0+e1*t+e2*(t**2.))
            xk=d*Gamma
            Theta=(t+(xk-2.0*q)/6.)
            if(S==0) then
            potemp=0.
            else
            potemp=real(Theta)
            end if  
        
        
    end subroutine
    ! for calbrating SSH using SSP units must be in mm 
    subroutine calibrate_SSH(SSH,SSP,calibratedSSH)
        implicit none
        real,intent(in)::SSH,SSP
        real,intent(out)::calibratedSSH

        if(SSH/=0.) then
            calibratedSSH = SSH + (SSP-10130)
        else;end if 
    end subroutine 

                                                ! SUBROUTINES FOR DATA MANIPULATION !

                                                ! SUBROUTINES FOR GRAPHS AND SHIT !

    subroutine rotate(angle)
        implicit none
        real,intent(in)::angle
        write(ounit,*) "% begin rotate"
        write(ounit,'(f10.4,2x,a8)' ) angle , ' ro ' 
        write(ounit,*) "% end rotate"

    end subroutine
    ! creating ps frame
    subroutine psframe(ini_st,fin_st,depth,width,height,memori_size)
        integer,intent(in)::ini_st,fin_st,depth
        real,intent(in)::width,height,memori_size
        real::dx,dy
        integer::x_memori,y_memori,st_quan,depth_quan
        integer,parameter::increment = 50

        dx = width/real(fin_st-ini_st+1); dy = height/real(depth)
        st_quan = fin_st - ini_st + 1; depth_quan = depth/increment
        call newpen2(3)
        call plot(0.,0.,3);call plot(0.,-height,2);call plot(width,-height,2);call plot(width,0.,2);call plot(0.,0.,2)
        call newpen2(3)
        do x_memori = 1,st_quan
            call plot(dx*real(x_memori)-dx/2.,-height,3);call plot(dx*real(x_memori)-dx/2.,-height-memori_size,2)
            call numberc(dx*real(x_memori)-dx/2.,-height-memori_size*4.,memori_size*3.,real(st_quan-x_memori+1),0.,-1)
        end do
        call newpen2(3)
        do y_memori = 0,depth_quan
            call plot(0.,-dy*real(increment)*real(y_memori),3);call plot(-memori_size,-dy*real(increment)*real(y_memori),2)
            call numberc(-memori_size*4.,-dy*real(increment)*real(y_memori),memori_size*3.,real(y_memori*increment),0.,-1)
        end do
    end subroutine
    ! creating map   line_opt == 1 means NLine, line_opt == 2 means SLine, line_opt == 3 means both
    subroutine create_map(ini_lat,fin_lat,ini_long,fin_long,ini_st,fin_st,line_opt,width,symbol_size)
        implicit none
        integer,intent(in)::ini_lat,fin_lat,ini_long,fin_long,ini_st,fin_st,line_opt
        real,intent(in):: width,symbol_size
        intrinsic sin,cos,tan,asin,acos
        integer,parameter::imax = 2080,jmax = 2640,station_x = 9, station_y = 2
        real,dimension(:,:),allocatable::dep
        integer,dimension(:,:),allocatable::dep_m
        integer::i,j,is,ie,js,je,n,line_num
        real::dx,dy,height,ratio,pi,xco,NLineYco,SLineYco
        character::line_name*10,filename*999
        real,dimension(station_y,station_x)::lon

        allocate(dep(imax,jmax));allocate(dep_m(imax,jmax))
        dep = 0.;dep_m = 0
        open(21,file='../Data/japan1km122-148_24-46.bin',form='unformatted',status='old')
        do j=jmax,1,-1
            if(j>jmax) then; exit;end if
            read(21)(dep(i,j),i=1,min(imax,2080))
        end do
        close(21)

        dep(1:imax,1:jmax) = -dep(1:imax,1:jmax)
        ! mask sea level, meaning height = 1
        do i=1,imax
            do j=1,jmax
                dep_m(i,j)=1
            end do
        end do

        if (ini_lat<24 .or. ini_lat>46 .or. fin_lat<24 .or. fin_lat>46 .or. ini_long<122 .or. ini_long>148 .or. fin_long<122 .or. fin_long>148 .or.ini_lat>fin_lat .or. ini_long>fin_long) then
            print*, 'Your map coordinates must be within 24-46N and 122-148E'
        else
            js = (ini_lat-24)*120+1
            je = (fin_lat-24)*120
            is = (ini_long-122)*80+1
            ie = (fin_long-122)*80
            pi = 2.*asin(1.)
            ratio = 6357./6378./cos((ini_lat+fin_lat)/2.*pi/180.)
            height = width*ratio*real(fin_lat-ini_lat)/real(fin_long-ini_long)
            dx = width/real(ie-is)
            dy = height/real(je-js)

            ! call plot(x,y,-3)
            if (symbol_size<=0.2) then;call newpen2(2);else if(symbol_size>=0.2 .and. symbol_size<=0.4) then;call newpen2(3);else;call newpen2(4);end if
            call rgbk(0.,0.,0.)
            call pscont3(dx,dy,dep,dep_m,is,ie,js,je,imax,jmax,1,0.,10.)
        end if
        call rgbk(0.,0.,0.)
        call plot(0.,0.,3);call plot(width,0.,2);call plot(width,height,2);call plot(0.,height,2);call plot(0.,0.,2)
        do n = 0,fin_long-ini_long
            call plot(dx*80.*real(n),0.,3);call plot(dx*80.*real(n),-symbol_size*0.5,2);call numberc(dx*80.*real(n),-symbol_size*1.4,symbol_size,real(n+ini_long),0.,-1)
            if(n/=fin_long-ini_long) then
                do i = 1, 9
                    call plot(dx*80.*(real(n)+real(i)/10.),0.,3);call plot(dx*80.*(real(n)+real(i)/10.),-symbol_size*0.25,2)
                end do
            end if
        end do
        do n = 0,fin_lat-ini_lat
            call plot(0.,dy*120.*real(n),3);call plot(-symbol_size*0.5,dy*120.*real(n),2);call numberc(-symbol_size*1.2,dy*120.*real(n),symbol_size,real(n+ini_lat),0.,-1)
            if(n/=fin_lat - ini_lat) then
                do i = 1,9
                    call plot(0.,dy*120.*(real(n)+real(i)/10.),3);call plot(-symbol_size*0.25,dy*120.*(real(n)+real(i)/10.),2)
                end do
            end if
        end do
        call symbolc(width/2.,-symbol_size*2.5,symbol_size*0.8,'Long (E)',0.,len('Long (e)'))
        call symbolc(-symbol_size*2.5,height/2.,symbol_size*0.8,'Lat (N)',90.,len('Lat (n)'))
        if(ini_long<=137 .and.fin_long>=140 .and. ini_lat<=40 .and. fin_lat>=41) then
            do line_num = 1,station_y;if(line_num == 1) then; line_name = 'N-Line';else;line_name = 'S-Line';end if
                filename = '../Data/Coordinates/'//trim(line_name)//'/lon.csv'
                NLineYco = dy*(41.-real(ini_lat))*120.;SLineYco = dy*(40.6-real(ini_lat))*120.
                open(32,file=filename,status = 'old',action = 'read')
                read(32,'(9(f9.4))')(lon(line_num,i),i = 1,station_x)
                close(32)
                do n = ini_st,fin_st; xco = dx*(lon(line_num,10-n)-real(ini_long))*80.
                    if (line_num ==1 .and. line_opt/=2) then
                        if(n==1.or.n==2.or.n==3)then;call gmark(xco,NLineYco,symbol_size*0.4,1);call numberc(xco,NLineYco+symbol_size*0.6,symbol_size*0.8,real(n),0.,-1)
                        else if(n==4.or.n==5.or.n==6)then;call gmark(xco,NLineYco,symbol_size*0.4,6);call numberc(xco,NLineYco+symbol_size*0.6,symbol_size*0.8,real(n),0.,-1)
                        else if(n==7.or.n==8.or.n==9)then;call gmark(xco,NLineYco,symbol_size*0.4,8);call numberc(xco,NLineYco+symbol_size*0.6,symbol_size*0.8,real(n),0.,-1)
                        else;end if
                    else if(line_num ==2 .and. line_opt /= 1) then
                        if(n==1.or.n==2.or.n==3)then;call gmark(xco,SLineYco,symbol_size*0.4,1);call numberc(xco,SLineYco-symbol_size*1.2,symbol_size*0.8,real(n),0.,-1)
                        else if(n==4.or.n==5.or.n==6)then;call gmark(xco,SLineYco,symbol_size*0.4,6);call numberc(xco,SLineYco-symbol_size*1.2,symbol_size*0.8,real(n),0.,-1)
                        else if(n==7.or.n==8.or.n==9)then;call gmark(xco,SLineYco,symbol_size*0.4,8);call numberc(xco,SLineYco-symbol_size*1.2,symbol_size*0.8,real(n),0.,-1)
                        else;end if
                    else;end if
                end do
            end do
            if(line_opt ==1 ) then
                call symbol(dx*(lon(1,1)-real(ini_long))*80.,NLineYco+symbol_size*1.5,symbol_size,'N-Line',0.,6)
            else if(line_opt==2) then
                call symbol(dx*(lon(2,1)-real(ini_long))*80.,SLineYco-symbol_size*2.3,symbol_size,'S-Line',0.,6)
            else;call symbol(dx*(lon(1,1)-real(ini_long))*80.,NLineYco+symbol_size*1.5,symbol_size,'N-Line',0.,6)
                call symbol(dx*(lon(2,1)-real(ini_long))*80.,SLineYco-symbol_size*2.3,symbol_size,'S-Line',0.,6)
            end if
        else
            print*,'stations are just outside of your map, like a perfect flower that is just beyond your reach...(mj)'
        end if
        deallocate(dep);deallocate(dep_m)

        ! call plot(-x,-y,-3)
            
    end subroutine
    ! only creates a frame unfortunately
    subroutine TS_diagram(temp_min,temp_max,sal_min,sal_max,sal_memoriterations,sal_symbolfreq,symbol_size,width,height)
        implicit none
        integer,parameter::ds_total = 101, dt_total =101
        integer,parameter::xs = 1,xe = ds_total,ys = 1,ye = dt_total
        real,parameter:: cont_inc = 0.2
        real,intent(in)::sal_min,sal_max,width,height,symbol_size
        ! integer,intent(in)::arraysize
        integer,intent(in)::temp_min,temp_max,sal_memoriterations,sal_symbolfreq
        ! real,dimension(arraysize),intent(in)::temp_array,sal_array
        real::sal,temp,sal_range,whatyouneed,dx,dy,real_sigma,y,sigma_print_y,min_sigma,max_sigma,first_sigma,sal_memoridiff,sal_numdiff
        integer::n,s,t,temp_range,cont_quantity
        integer,dimension(ds_total,dt_total)::mask=0
        real,dimension(ds_total,dt_total)::sigma_array=0.
        dx = width/real(ds_total-1); dy = height/real(dt_total-1)
        temp_range = temp_max - temp_min; sal_range = sal_max - sal_min
        do s=1,ds_total
            do t=1,dt_total
                mask(s,t)=1
            end do
        end do  !just for the use of Kuroda's subroutine

        
        call newpen2(3)
        call plot(0.,0.,3);call plot(width,0.,2);call plot(width,height,2);call plot(0.,height,2);call plot(0.,0.,2)
        call symbolc(width/2.,-2.2*symbol_size,symbol_size,'Salinity',0.,len('salinity'))
        call symbolc(-2.5*symbol_size,height/2.,symbol_size,'Temperature',90.,len('temperature'))
        call newpen2(3)

        do n = 0,sal_memoriterations
            call newpen2(4)
            sal_memoridiff = width/real(sal_memoriterations);sal_numdiff = (sal_max-sal_min)/real(sal_memoriterations)
            if (mod(n,sal_symbolfreq)==0) then
                call plot(real(n)*sal_memoridiff,0.,3);call plot(real(n)*sal_memoridiff,-0.2,2)
                call numberc(real(n)*sal_memoridiff,-1.2*symbol_size,symbol_size,sal_min+real(n)*sal_numdiff,0.,2)
            else
                call plot(real(n)*sal_memoridiff,0.,3);call plot(real(n)*sal_memoridiff,-0.1,2)
            end if
        end do
        do n = 0,temp_range
            call newpen2(4)
            if(mod(n,5)==0) then
                call plot(0.,real(n)*real(height)/real(temp_range),3);call plot(-0.2,real(n)*real(height)/real(temp_range),2)
                call numberr(-0.3,real(n)*real(height)/real(temp_range),symbol_size*0.9,real(temp_min+temp_range)*real(n)/real(temp_range),0.,1)
            else
                call plot(0.,real(n)*real(height)/real(temp_range),3);call plot(-0.1,real(n)*real(height)/real(temp_range),2)  
            end if
        end do

        do s = 1,ds_total
            do t = 1,dt_total
            temp = temp_min+temp_range*real(t)/real(dt_total); sal = sal_min+sal_range*real(s)/real(ds_total)
                call sigma_T_S(whatyouneed,temp,sal)
                sigma_array(s,t) = whatyouneed
            end do
        end do
        min_sigma = minval(sigma_array) 
        max_sigma = maxval(sigma_array)  
        ! print*,min_sigma,max_sigma
        y = 0.
        call newpen2(3)
        cont_quantity = int(max_sigma-min_sigma)*5+10
        first_sigma = int(min_sigma)
        call plot(-dx/2.,-dy/2.,-3)
        call symbol(width+symbol_size*0.2,height+symbol_size*0.4,symbol_size*0.8,'sigma:',0.,len('sigma:'))
        do n = 0,cont_quantity
            real_sigma = first_sigma+real(n)*cont_inc
            if(int(real_sigma)==real_sigma .and. real_sigma>=min_sigma .and. real_sigma<=max_sigma) then
                call newpen2(5);call rgbk(0.,0.,0.);call pscont3(dx,dy,sigma_array,mask,xs,xe,ys,ye,ds_total,dt_total,1,real_sigma,0.)
                sigma_print_y = height-0.8*symbol_size-y
                call numberc(width+symbol_size,sigma_print_y,0.8*symbol_size,real_sigma,0.,1)
                y = y +0.8*symbol_size
            else
                call newpen2(2);call rgbk(0.,0.,0.);call pscont3(dx,dy,sigma_array,mask,xs,xe,ys,ye,ds_total,dt_total,1,real_sigma,0.)
            end if
        end do
        y = 0.
        call plot(dx/2.,dy/2.,-3)

        ! do n = 1, arraysize
        !     if(temp_array(n)/=0. .and. sal_array(n)/=0.)then
        !         plot_x = (sal_array(n)-sal_min)*width/real(sal_range)
        !         plot_y = (temp_array(n)-real(temp_min))*height/real(temp_range)
        !         call gmark(plot_x,plot_y,0.05,1)
        !     else;end if
        ! end do

    end subroutine
    ! the output is the ratio of the input value to the max value relative to the length of the box or the axis
    subroutine gmark_ratio(input_value,min,max,length,output)
        implicit none
        real,intent(in)::input_value,min,max,length
        real,intent(out)::output

        output = (input_value-min)*length/(max-min)

    end subroutine
    ! month names array
    subroutine month_str_array(month_names)
        implicit none
        character(len=4),dimension(12),intent(out)::month_names

        month_names = (/'Jan.','Feb.','Mar.','Apr.','May ','Jun.','Jul.','Aug.','Sep.','Oct.','Nov.','Dec.'/)
    end subroutine
    ! betanuri version of ps color, only works for 1D arrays 
    subroutine betcolork(starting_x,dx,dy,oneD_array,array_size,mask,ini_value,fin_value,r,g,b)
        implicit none
        integer,intent(in)::array_size
        real,dimension(array_size),intent(in)::oneD_array
        integer,dimension(array_size),intent(in)::mask
        real,intent(in)::starting_x,ini_value,fin_value,r,g,b,dx,dy
        integer::n

        do n = 1, array_size
            ! print*,oneD_array(n)
            if(mask(n)/=0) then
                if(ini_value<=oneD_array(n) .and. oneD_array(n)<=fin_value) then
                    call betsqk(starting_x,real(n-1)*dy,starting_x+dx,real(n)*dy,r,g,b)
                else;end if
            else;end if
        end do
        ! print*,"ongongo"

    end subroutine
    subroutine betcolork2(dx,dy,twoD_array,mask,ix,ex,iy,ey,x_size,y_size,ini_value,fin_value,r,g,b)
        implicit none
        integer,intent(in)::ix,ex,iy,ey,x_size,y_size
        real,dimension(x_size,y_size),intent(in)::twoD_array
        integer,dimension(x_size,y_size),intent(in)::mask
        real,intent(in)::ini_value,fin_value,r,g,b,dx,dy
        integer::n,m

        do n = ix, ex
            do m = iy,ey
                ! if(mask(n,m)/=0) then
                    if(mask(n,m)/=0.and.ini_value<=twoD_array(n,m) .and. twoD_array(n,m)<=fin_value) then
                        call betsqk(real(n-1)*dx,real(m-1)*dy,real(n)*dx,real(m)*dy,r,g,b)
                    else;end if
                ! else;end if
            end do
        end do

    end subroutine
    ! betcolork but for integers such as data quantity
    subroutine betcolorI(dx,dy,twoD_array,mask,ix,ex,iy,ey,x_size,y_size,someint,r,g,b)
        implicit none
        integer,intent(in)::ix,ex,iy,ey,x_size,y_size,someint
        integer,dimension(x_size,y_size),intent(in)::twoD_array,mask
        real,intent(in)::r,g,b,dx,dy
        integer::n,m

        do n = ix, ex
            do m = iy,ey
                ! if(mask(n,m)/=0) then
                    if(mask(n,m)/=0.and.twoD_array(n,m)==someint) then
                        call betsqk(real(n-1)*dx,real(m-1)*dy,real(n)*dx,real(m)*dy,r,g,b)
                    else;end if
                ! else;end if
            end do
        end do

    end subroutine
    ! creates colorscale for data quantity min and max are integers. array size for colors is (iterations)
    subroutine colorscale_data(iterations,r,g,b,min,max,symbol_freq,symbol_size,length,width,angle)
        implicit none
        integer,intent(in)::iterations,min,max,symbol_freq,angle
        real,intent(in)::symbol_size,length,width
        real,dimension(iterations),intent(in)::r,g,b 
        integer::n,quotient
        real::memori_diff
        
        if(mod(max-min+1,iterations)/=0) then
            print*,'Something Wrong with the Number of Colors in subroutine (colorscale_data)'
        else;quotient = (max-min+1)/iterations
        end if

            memori_diff = length/real(iterations)
            if(angle == 0) then
                call newpen2(2)
                do n = 1,iterations
                    call betsqk(real(n-1)*memori_diff,0.,real(n)*memori_diff,width,r(n),g(n),b(n))
                end do
                call plot(0.,0.,3);call plot(length,0.,2);call plot(0.,width,3);call plot(length,width,2)
                do n = 0, iterations
                    call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,width,2)
                end do
                do n = 0,iterations-1
                    call plot(real(n)*memori_diff+memori_diff/2.,0.,3);call plot(real(n)*memori_diff+memori_diff/2.,-0.1,2)
                    if(mod(n,symbol_freq)==0) then
                        call numberc(real(n)*memori_diff+memori_diff/2.,-1.4*symbol_size,symbol_size,real(min)+real(n)*real(quotient),0.,-1)
                    else;end if
                end do
            else !(angle == 90)
                call newpen2(2)
                do n = 1, iterations
                    call betsqk(0.,real(n-1)*memori_diff,-width,real(n)*memori_diff,r(n),g(n),b(n))
                end do
                call plot(0.,0.,3);call plot(0.,length,2);call plot(-width,0.,3);call plot(-width,length,2)
                do n = 0, iterations
                    call plot(0.,real(n)*memori_diff,3);call plot(-width,real(n)*memori_diff,2)
                end do
                do n = 0,iterations-1
                    call plot(0.,real(n)*memori_diff+memori_diff/2.,3);call plot(0.1,real(n)*memori_diff+memori_diff/2.,2)
                    if(mod(n,symbol_freq)==0) then
                        call number(1.4*symbol_size,real(n)*memori_diff+memori_diff/2.,symbol_size,real(min)+real(n)*real(quotient),0.,-1)
                    else;end if
                end do
            end if

    end subroutine
    ! t value for t distribution 95 percent confidence interval df<=30
    subroutine t95_value(t95) !this is and array of 31 values, dimension is(0:30)
        implicit none
        real,dimension(0:30),intent(out)::t95

        t95(1) = 12.706 ; t95(11) = 2.2010 ; t95(21) = 2.0796
        t95(2) = 4.3026 ; t95(12) = 2.1788 ; t95(22) = 2.0739
        t95(3) = 3.1824 ; t95(13) = 2.1604 ; t95(23) = 2.0687
        t95(4) = 2.7765 ; t95(14) = 2.1448 ; t95(24) = 2.0639
        t95(5) = 2.5706 ; t95(15) = 2.1315 ; t95(25) = 2.0595
        t95(6) = 2.4469 ; t95(16) = 2.1191 ; t95(26) = 2.0555
        t95(7) = 2.3646 ; t95(17) = 2.1098 ; t95(27) = 2.0518
        t95(8) = 2.3060 ; t95(18) = 2.1009 ; t95(28) = 2.0484
        t95(9) = 2.2621 ; t95(19) = 2.0930 ; t95(29) = 2.0452
        t95(10) = 2.2281 ;t95(20) = 2.0860 ; t95(30) = 2.0423

        t95(0) = 0. !just for the sake of programs
    end subroutine
end module oldsubs

module subroutines 
    use oldsubs
    implicit none
    contains

    ! DATA
        ! SSH DATA put st label and get array of 15 years and 12 months.   -999 means no data or insufficient data
        subroutine SSH_data(SSH2D,ilabel,slabel,convert)
            implicit none
            type :: labeled_array
                integer, dimension(:,:),allocatable:: num_labels
                character(len=30), dimension(:,:),allocatable:: str_labels
                real, dimension(:,:,:),allocatable:: values
            end type labeled_array

            type(labeled_array) :: localssh
            integer,parameter::num_rows=150,num_years=15,num_months=12
            integer,intent(in),optional::ilabel
            character(len=*),intent(in),optional::slabel
            integer,intent(in),optional::convert
            real,dimension(num_years,num_months),intent(out)::SSH2D
            integer::n,i,ios,convint=0
            character::yyyy*4,row1*999,convstr*30

            allocate(localssh%num_labels(num_years,num_rows))
            allocate(localssh%str_labels(num_years,num_rows))
            allocate(localssh%values(num_years,num_rows,num_months)) ! allocating arrays just to use heap memory
            localssh%num_labels = 0;localssh%str_labels = ' ';localssh%values = 0.
            convstr = ''
            do n = 1, num_years
                write(yyyy,'(i4)') n+2008
                open(unit=20, file='../Data/SSH/data/SSH'//trim(yyyy)//'.csv', status='old', action='read',iostat=ios)
                if(ios/=0) then
                    print*,'Error opening file';stop
                end if
                do i = 1, num_rows
                    read(20,'(A)',iostat = ios)row1
                    if(ios == 0)then
                        call parse_csv_row(row1, localssh%num_labels(n,i), localssh%str_labels(n,i), localssh%values(n,i,:))
                    else if(i<100.and.ios/=0)then
                        print*,'Error reading file';stop
                    else;exit
                    end if
                end do
            end do
            
            if(.not.present(ilabel).and..not.present(slabel))print*,'Provide The Station Label or Index'
            ! print*,localssh%num_labels(1,1:20)
            ! print*,localssh%str_labels(1,1:10)

            if(present(ilabel).and..not.present(slabel))then
                do n = 1, num_years
                    do i = 1, num_rows
                        if(localssh%num_labels(n,i)==ilabel)then
                            SSH2D(n,:) = localssh%values(n,i,:)
                            if(present(convert))then
                                if(n==1)convstr = localssh%str_labels(n,i)
                                if(n/=1.and.convstr/=localssh%str_labels(n,i))then
                                    print*,'inconsistant station labels at',convstr,'and',localssh%str_labels(n,i)
                                end if
                            end if
                            exit
                        end if
                        if(i==num_rows)print*,'Station Index',ilabel,'not found for Year',n+2008
                    end do
                end do
                if(present(convert))print*,ilabel,'-->',convstr
            else if(.not.present(ilabel).and.present(slabel))then
                do n = 1, num_years
                    do i = 1, num_rows
                        if(trim(localssh%str_labels(n,i))==slabel)then
                            SSH2D(n,:) = localssh%values(n,i,:)
                            if(present(convert))then
                                if(n==1)convint = localssh%num_labels(n,i)
                                if(n/=1.and.convint/=localssh%num_labels(n,i))then
                                    print*,'inconsistant station labels at',convint,'and',localssh%num_labels(n,i)
                                end if
                            end if
                            exit
                        end if
                        if(i==num_rows)print*,'Station Label',slabel,'not found for Year',n+2008
                    end do
                end do
                if(present(convert))print*,slabel,'-->',convint
            else;print*,'Provide Either Station Label or Index but not both'
            end if

            contains
            subroutine parse_csv_row(row, num_label, str_label, values)
                implicit none
                character(len=*), intent(in) :: row
                integer, intent(out) :: num_label
                character(len=30), intent(out) :: str_label
                real, dimension(:), intent(out) :: values
                integer :: pos, start_pos, end_pos,k
            
                ! Initialize positions
                start_pos = 1
                pos = 0
            
                ! Extract num_label
                end_pos = index(row(start_pos:), ',')
                read(row(start_pos:start_pos+end_pos-2), '(I4)', iostat=ios) num_label
                if (ios /= 0) then
                    print *, 'Error parsing num_label'
                    stop
                end if
                start_pos = start_pos + end_pos
            
                ! Extract str_label
                end_pos = index(row(start_pos:), ',')
                str_label = adjustl(row(start_pos:start_pos+end_pos-2))
                start_pos = start_pos + end_pos
            
                ! Extract values
                do k = 1, size(values) ! is 12
                    end_pos = index(row(start_pos:), ',')
                    if (end_pos == 0) end_pos = start_pos ! the last iteration
                    read(row(start_pos:start_pos+end_pos-2), '(F7.1)', iostat=ios) values(k)
                    if (ios /= 0) then
                        print *, 'Error parsing values'
                        stop
                    end if
                    start_pos = start_pos + end_pos
                end do
            end subroutine parse_csv_row

        end subroutine
    ! COLORGRAD
        ! r,g,b individual color gradient
        subroutine colorgrad(rgb,iterations,r,g,b)
            implicit none
            integer,intent(in)::iterations
            real,dimension(:),allocatable,intent(out)::r,g,b
            integer::n
            character(len=*),intent(in)::rgb
            real::tops

            allocate(r(0:iterations+1),g(0:iterations+1),b(0:iterations+1))
            if(rgb=='red'.or.rgb=='wred')then
                r(0) = 1.; g(0) = 1.; b(0) = 1.
                r(1) = 1.; g(1) = 0.9; b(1) = 0.9
                tops=0.9
                if(rgb=='wred')then
                    r(1) = 1.; g(1) = 1.; b(1) = 1.
                    tops = 1.
                end if
                do n = 2, iterations
                    r(n) = 1.
                    g(n) = tops-(real(n-1)/real(iterations-1))*tops
                    b(n) = tops-(real(n-1)/real(iterations-1))*tops
                end do
                r(iterations+1) = 0.6 ; g(iterations+1) = 0. ; b(iterations+1) = 0.
            else if(rgb=='green'.or.rgb=='wgreen')then
                r(0) = 1.; g(0) = 1.; b(0) = 1.
                r(1) = 0.9; g(1) = 1.; b(1) = 0.9
                tops = 0.9
                if(rgb=='wgreen')then
                    r(1) = 1.; g(1) = 1.; b(1) = 1.
                    tops = 1.
                end if
                do n = 2, iterations
                    r(n) = tops-(real(n-1)/real(iterations-1))*tops
                    g(n) = 1.
                    b(n) = tops-(real(n-1)/real(iterations-1))*tops
                end do
                r(iterations+1) = 0. ; g(iterations+1) = 0.6 ; b(iterations+1) = 0.
            else if(rgb=='blue'.or.rgb=='wblue')then
                r(0) = 1.; g(0) = 1.; b(0) = 1.
                r(1) = 0.9; g(1) = 0.9; b(1) = 1.
                tops = 0.9
                if(rgb=='wblue')then
                    r(1) = 1.; g(1) = 1.; b(1) = 1.
                    tops=1.
                end if
                do n = 2, iterations
                    r(n) = tops-(real(n-1)/real(iterations-1))*tops
                    g(n) = tops-(real(n-1)/real(iterations-1))*tops
                    b(n) = 1.
                end do
                r(iterations+1) = 0. ; g(iterations+1) = 0. ; b(iterations+1) = 0.6
            else;print*,'Choose From red, green, blue'
            end if
        end subroutine
        ! blue to red
        subroutine b2r_colorgrad(iterations,midpoint,r,g,b)
            implicit none
            integer,intent(in)::iterations,midpoint
            real,dimension(:),allocatable,intent(out)::r,g,b
            integer::n
            
            allocate(r(0:iterations+1),g(0:iterations+1),b(0:iterations+1))
            do n = 1, iterations
                if(midpoint==1.and.n==1)then;r(1)=0.;g(1)=0.;b(1)=1.;cycle;endif
                if (n <= midpoint) then 
                    r(n) = 0.+(real(n-1)/real(midpoint-1))*0.90
                    g(n) = 0.+(real(n-1)/real(midpoint-1))*0.90
                    b(n) = 1.
                else
                    r(n) = 1.
                    g(n) = 0.90-(real(n-midpoint-1)/real(iterations-midpoint-1))*0.90
                    b(n) = 0.90-(real(n-midpoint-1)/real(iterations-midpoint-1))*0.90
                end if
            end do

            r(0) = 0.; g(0) = 0.; b(0) = 0.6
            r(iterations+1) = 0.6 ; g(iterations+1) = 0. ; b(iterations+1) = 0.

        end subroutine
        subroutine b2w2r_colorgrad(iterations,midpoint,r,g,b)
            implicit none
            integer,intent(in)::iterations,midpoint
            real,dimension(:),allocatable,intent(out)::r,g,b
            integer::n
            
            allocate(r(0:iterations+1),g(0:iterations+1),b(0:iterations+1))
            do n = 1, iterations
                if(midpoint==1)then;print*,'midpoint of 0 is invalid for b2w2r colorgrad';stop;endif
                if (n < midpoint) then 
                    r(n) = 0.+(real(n-1)/real(midpoint-2))*0.85
                    g(n) = 0.+(real(n-1)/real(midpoint-2))*0.85
                    b(n) = 1.
                else if(n==midpoint)then
                    r(n)=1.;g(n)=1.;b(n)=1.
                else
                    r(n) = 1.
                    g(n) = 0.85-(real(n-midpoint-1)/real(iterations-midpoint-1))*0.85
                    b(n) = 0.85-(real(n-midpoint-1)/real(iterations-midpoint-1))*0.85
                end if
            end do

            r(0) = 0.; g(0) = 0.; b(0) = 0.6
            r(iterations+1) = 0.6 ; g(iterations+1) = 0. ; b(iterations+1) = 0.

        end subroutine
        ! blue to grey to red
        subroutine b2gy2r_colorgrad(iterations,midpoint,r,g,b)
            implicit none
            integer,intent(in)::iterations,midpoint
            real,dimension(:),allocatable,intent(out)::r,g,b
            integer::n
            
            allocate(r(0:iterations+1),g(0:iterations+1),b(0:iterations+1))
            do n = 1, midpoint-1
                    r(n) = 0.+(real(n-1)/real(midpoint-2))*0.8
                    g(n) = 0.+(real(n-1)/real(midpoint-2))*0.8
                    b(n) = 1.
            end do
            do n = midpoint+1,iterations
                    r(n) = 1.
                    g(n) = 0.8-(real(n-midpoint-1)/real(iterations-midpoint-1))*0.8
                    b(n) = 0.8-(real(n-midpoint-1)/real(iterations-midpoint-1))*0.8
            end do
            r(midpoint) = 0.85;g(midpoint) = 0.85;b(midpoint) = 0.85



            r(0) = 0.; g(0) = 0.; b(0) = 0.6
            r(iterations+1) = 0.6 ; g(iterations+1) = 0. ; b(iterations+1) = 0.
        end subroutine
        ! red to green
        subroutine r2g_colorgrad(iterations,midpoint,r,g,b)
            implicit none
            integer,intent(in)::iterations,midpoint
            real,dimension(:),allocatable,intent(out)::r,g,b
            integer::n 

            allocate(r(0:iterations+1),g(0:iterations+1),b(0:iterations+1))
            do n = 1,iterations
                if(n<=midpoint) then
                    r(n) = 1.
                    g(n) = real(n-1)/real(midpoint-1)
                    b(n) = 0.
                else
                    r(n) = 1.-real(n-midpoint)/real(iterations-midpoint)
                    g(n) = 1.
                    b(n) = 0.
                end if
            end do
            ! r(0) = 0.; g(0) = 0.; b(0) = 0.
        end subroutine
        ! black to red, then gradient from red to green. 
        subroutine bk2r2g_colorgrad(iterations,midpoint,r,g,b)
            implicit none
            integer,intent(in)::iterations,midpoint
            real,dimension(:),allocatable,intent(out)::r,g,b 
            integer::n 

            allocate(r(0:iterations+1),g(0:iterations+1),b(0:iterations+1))
            r(1) = 0.; g(1) = 0.; b(1) = 0.
            do n = 2,iterations
                if(n<=midpoint) then
                    r(n) = 1.
                    g(n) = real(n-2)/real(midpoint-2)
                    b(n) = 0.
                else 
                    r(n) = 1.-real(n-midpoint)/real(iterations-midpoint)
                    g(n) = 1.
                    b(n) = 0.
                end if
            end do

        end subroutine
        ! b<breakpoint1>g<breakpoint2>y<breakpoint3>r. bluecyan==dark->light, yellowred==light->dark
        subroutine b2cy2y2r_colorgrad(iterations,breakpoint1,breakpoint2,breakpoint3,r,g,b)
            implicit none
            integer,intent(in)::iterations,breakpoint1,breakpoint2,breakpoint3
            real,dimension(:),allocatable,intent(out)::r,g,b
            integer::n

            allocate(r(0:iterations+1),g(0:iterations+1),b(0:iterations+1))
            r(0)=0.;g(0)=0.;b(0)=0.9
            ! from dark blue to light blue
            do n = 1, breakpoint1
                r(n) = 0.+(real(n-1)/real(breakpoint1-1))*0.8
                g(n) = 0.+(real(n-1)/real(breakpoint1-1))*0.8
                b(n) = 1.
            end do
            ! from dark cyan to light cyan
            do n = 1,breakpoint2-breakpoint1
                r(n+breakpoint1) = (real(n-1)/real(breakpoint2-breakpoint1-1))*0.9
                g(n+breakpoint1) = 1.
                b(n+breakpoint1) = 1.
            end do

            ! from light yellow to dark yellow
            do n = 1, breakpoint3-breakpoint2
                r(n+breakpoint2) = 1.
                g(n+breakpoint2) = 1.
                b(n+breakpoint2) = 0.9-(real(n-1)/real(breakpoint3-breakpoint2-1))*0.9
            end do
            ! from light red to dark red
            do n = 1, iterations-breakpoint3
                r(n+breakpoint3) = 1.
                g(n+breakpoint3) = 0.9-(real(n-1)/real(iterations-breakpoint3-1))*0.9
                b(n+breakpoint3) = 0.9-(real(n-1)/real(iterations-breakpoint3-1))*0.9
            end do
            r(iterations+1) = 0.9;g(iterations+1) = 0.;b(iterations+1) = 0.
        end subroutine
        ! b,zeropoint,g->y->r. blue==dark->light, greenyellowred==light->dark
        subroutine b2g2y2r_colorgrad(iterations,breakpoint1,breakpoint2,breakpoint3,r,g,b)
            implicit none
            integer,intent(in)::iterations,breakpoint1,breakpoint2,breakpoint3
            real,dimension(:),allocatable,intent(out)::r,g,b
            integer::n

            allocate(r(0:iterations+1),g(0:iterations+1),b(0:iterations+1))
            r(0)=0.;g(0)=0.;b(0)=0.9
            ! from dark blue to light blue
            do n = 1, breakpoint1
                r(n) = 0.+(real(n-1)/real(breakpoint1-1))*0.8
                g(n) = 0.+(real(n-1)/real(breakpoint1-1))*0.8
                b(n) = 1.
            end do
            ! from dark green to light green
            do n = 1,breakpoint2-breakpoint1
                r(n+breakpoint1) = 0.9-(real(n-1)/real(breakpoint2-breakpoint1-1))*0.9
                g(n+breakpoint1) = 1.
                b(n+breakpoint1) = 0.9-(real(n-1)/real(breakpoint2-breakpoint1-1))*0.9
            end do

            ! from light yellow to dark yellow
            do n = 1, breakpoint3-breakpoint2
                r(n+breakpoint2) = 1.
                g(n+breakpoint2) = 1.
                b(n+breakpoint2) = 0.9-(real(n-1)/real(breakpoint3-breakpoint2-1))*0.9
            end do
            ! from light red to dark red
            do n = 1, iterations-breakpoint3
                r(n+breakpoint3) = 1.
                g(n+breakpoint3) = 0.9-(real(n-1)/real(iterations-breakpoint3-1))*0.9
                b(n+breakpoint3) = 0.9-(real(n-1)/real(iterations-breakpoint3-1))*0.9
            end do
            r(iterations+1) = 0.9;g(iterations+1) = 0.;b(iterations+1) = 0.
        end subroutine
        subroutine dozencolors(r,g,b)
            real, dimension(:),allocatable,intent(out) :: r, g, b
            allocate (r(12), g(12), b(12))
            r = (/ 1.0, 0.0, 0.0, .8, 0.0, 1.0, 1.0, 0.75, 1.0, 0.5, 0.0, 0.65 /)
            g = (/ 0.0, 1.0, 0.0, .8, 1.0, 0.0, 0.65, 1.0, 0.41, 0.0, 0.5, 0.16 /)
            b = (/ 0.0, 0.0, 1.0, 0.0, 1.0, .8, 0.0, 0.0, 0.71, 0.5, 0.5, 0.16 /)
        end subroutine
        ! END COLORGRAD
        subroutine rgb_to_cmyk(r, g, b, c, m, y, k)
            implicit none
            real, intent(in) :: r, g, b
            real, intent(out) :: c, m, y, k
            real :: r_norm, g_norm, b_norm
        
            r_norm = r / 255.0
            g_norm = g / 255.0
            b_norm = b / 255.0
        
            k = 1.0 - max(r_norm, g_norm, b_norm)
            if (k < 1.0) then
                c = (1.0 - r_norm - k) / (1.0 - k)
                m = (1.0 - g_norm - k) / (1.0 - k)
                y = (1.0 - b_norm - k) / (1.0 - k)
            else
                c = 0.0
                m = 0.0
                y = 0.0
            end if
        end subroutine rgb_to_cmyk
        subroutine cmyk_to_rgb(c, m, y, k, r, g, b)
            implicit none
            real, intent(in) :: c, m, y, k
            real, intent(out) :: r, g, b
        
            r = 255.0 * (1.0 - c) * (1.0 - k)
            g = 255.0 * (1.0 - m) * (1.0 - k)
            b = 255.0 * (1.0 - y) * (1.0 - k)
        end subroutine cmyk_to_rgb
        subroutine centeralize_colors(iterations,midpoint,r,g,b)
            implicit none
            integer,intent(in)::iterations,midpoint
            real,dimension(0:),intent(inout)::r,g,b
            integer::n

            if(lbound(r,1).ne.0 .or. ubound(r,1).ne.(iterations+1))then
                print*,'Iterations and the array size do not match in SUBROUTINE (centeralize_colors)'
            end if
            if(midpoint>3)then
                do n = 2, iterations-1
                    if(n<midpoint-1)then
                        r(n) = r(n) + (r(midpoint)-r(n))/1.7**(real(midpoint-n))
                        g(n) = g(n) + (g(midpoint)-g(n))/1.7**(real(midpoint-n))
                        b(n) = b(n) + (b(midpoint)-b(n))/1.7**(real(midpoint-n))
                    else if(n>midpoint+1)then
                        r(n) = r(n) + (r(midpoint)-r(n))/1.7**(real(n-midpoint))
                        g(n) = g(n) + (g(midpoint)-g(n))/1.7**(real(n-midpoint))
                        b(n) = b(n) + (b(midpoint)-b(n))/1.7**(real(n-midpoint))
                    end if
                end do
            else if(midpoint<3)then ! colors in midpoint dont change
                if(midpoint+1>iterations-1)then
                    print*,'no change in subroutine centralize_colors',midpoint+1,'>',iterations
                    return
                end if 
                if(iterations<3)then
                    do n = midpoint+1,iterations
                        r(n) = r(n) + (r(midpoint)-r(n))/1.7**(real(n-midpoint))
                        g(n) = g(n) + (g(midpoint)-g(n))/1.7**(real(n-midpoint))
                        b(n) = b(n) + (b(midpoint)-b(n))/1.7**(real(n-midpoint))
                    end do
                    print*, 'colors shifted to the left relative of ',midpoint,'(subroutine centeralize_colors)'
                else 
                    do n = midpoint+1,iterations-1
                        r(n) = r(n) + (r(midpoint)-r(n))/1.7**(real(n-midpoint))
                        g(n) = g(n) + (g(midpoint)-g(n))/1.7**(real(n-midpoint))
                        b(n) = b(n) + (b(midpoint)-b(n))/1.7**(real(n-midpoint))
                    end do
                    print*, 'colors shifted to the left relative of ',midpoint,'(subroutine centeralize_colors)'
                end if
            end if
        end subroutine
    ! END COLORGRAD 

    ! PLOTS
        subroutine box(width,height,thickness,x,y)
            implicit none
            real,intent(in)::width,height
            real,intent(in),optional::x,y
            integer,intent(in)::thickness
            if(present(x).and.present(y))call plot(x,y,-3)
            if(present(x).and. .not.present(y))call plot(x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,y,-3)
            call newpen2(thickness)
            call plot(0.,0.,3);call plot(width,0.,2);call plot(width,height,2);call plot(0.,height,2);call plot(0.,0.,2)
            if(present(x).and.present(y))call plot(-x,-y,-3)
            if(present(x).and. .not.present(y))call plot(-x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,-y,-3)
        end subroutine    
        subroutine floating_numbers(ini_num,num_inc,iterations,symbol_size,x_inc,y_inc,rangle,float_quantity,x,y)
            real,intent(in)::symbol_size,x_inc,y_inc,rangle,ini_num,num_inc
            real,intent(in),optional::x,y
            integer,intent(in)::iterations,float_quantity
            integer::n
            real::printnum

            if(present(x).and.present(y))call plot(x,y,-3)
            if(present(x).and. .not.present(y))call plot(x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,y,-3)
            do n = 1, iterations
                printnum = ini_num + num_inc*real(n-1)
                call numberc(real(n-1)*x_inc,real(n-1)*y_inc,symbol_size,printnum,rangle,float_quantity)
            end do
            if(present(x).and.present(y))call plot(-x,-y,-3)
            if(present(x).and. .not.present(y))call plot(-x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,-y,-3)
        end subroutine
        subroutine floating_lines(length,rangle,iterations,line_thickness,x_inc,y_inc,x,y,dashy)
            real,intent(in)::length,x_inc,y_inc,rangle
            real,intent(in),optional::x,y
            integer,intent(in),optional::dashy
            integer,intent(in)::iterations,line_thickness
            integer::n

            if(present(x).and.present(y))call plot(x,y,-3)
            if(present(x).and. .not.present(y))call plot(x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,y,-3)

            call newpen2(line_thickness)
            if(present(dashy))call newpen2(dashy)
            write(ounit,*) "% begin floating_lines"
            do n = 1, iterations
                write(ounit,'(f10.4,2x,a4)' ) rangle , ' ro ' 
                call plot(0.,0.,3);call plot(length,0.,2)
                write(ounit,'(f9.4,2x,a4)' ) -rangle , ' ro ' 
                call plot(x_inc,y_inc,-3)
            end do
            write(ounit,*) "% end floating_lines"
            call plot(-real(iterations)*x_inc,-real(iterations)*y_inc,-3)

            if(present(x).and.present(y))call plot(-x,-y,-3)
            if(present(x).and. .not.present(y))call plot(-x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,-y,-3)
            return
        end subroutine
        ! defaults for optional arguments are, rangle=0.,symbol_start=0,TorB='B',lt=0,gt=0,symbol_start=0,x=0.,y=0.
        subroutine colorscale(iterations,r,g,b,ini_num,fin_num,symbol_freq,symbol_size,float_quantity,length,width,lt,gt,rangle,TorB,symbol_start,x,y)
            implicit none
            intrinsic::sin,cos
            integer,intent(in)::iterations,symbol_freq,float_quantity
            real,intent(in)::ini_num,fin_num,symbol_size,length,width
            integer,intent(in),optional::lt,gt,symbol_start
            character(len=*),intent(in),optional::TorB
            real,intent(in),optional::rangle,x,y
            real,dimension(0:),intent(in)::r,g,b
            integer::n,intquan
            real::memori_diff,num_diff
            real,dimension(3)::lefty_x=0.,lefty_y=0.,righty_x=0.,righty_y=0.
            character(len=20)::min,max,format1,format2

            if(lbound(r,1).ne.0 .or. ubound(r,1).ne.(iterations+1))then
                print*,'Something wrong with the color array'
            end if
            do n = 1,10
                if(ini_num/(10.**real(n))>=1.)then;cycle
                else;intquan=n;exit;end if
            end do
            if(float_quantity>=0)then
                write(format1,'(A,I0,A,I0,A)') "(f",intquan+float_quantity+3,'.',float_quantity,")"
                write(min,format1)ini_num
            else;write(format1,'(A,I0,A)') "(i",intquan+2,")"
                write(min,format1)int(ini_num)
            end if
            ! write(min,format1)ini_num
            do n = 1,10
                if(fin_num/(10.**real(n))>=1.)then;cycle
                else;intquan=n;exit;endif
            end do
            if(float_quantity>=0)then
                write(format2,'(A,I0,A,I0,A)') "(f",intquan+float_quantity+3,'.',float_quantity,")"
                write(max,format2)fin_num
            else;write(format2,'(A,I0,A)') "(i",intquan+2,")"
                write(max,format2)int(fin_num)
            end if
            ! write(max,format2)fin_num
            ! print*,format1,format2,max,min
            memori_diff = length/real(iterations+2); num_diff = (fin_num-ini_num)/real(iterations)

            lefty_x(1) = 0.;lefty_x(2) = -memori_diff; lefty_x(3) = 0.
            lefty_y(1) = 0.;lefty_y(2) = width/2. ;lefty_y(3) = width
            righty_x(1) = length-memori_diff*2. ;righty_x(2) = length-memori_diff;righty_x(3) = length-memori_diff*2.
            righty_y(1) = 0. ;righty_y(2) = width/2. ;righty_y(3) = width
            if(present(x).and.present(y))call plot(x,y,-3)
            if(present(x).and. .not.present(y))call plot(x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,y,-3)
            write(ounit,*) "% begin colorscale"

                if(present(rangle)) then
                    write(ounit,'(f10.4,2x,a4)' ) rangle , ' ro '
                end if
                call plot(-length/2.+memori_diff,-width/2.,-3)
                if(symbol_size<0.3)then;call newpen2(3)
                else if(symbol_size>=0.3.and.symbol_size<0.6)then;call newpen2(4)
                else;call newpen2(5)
                end if
                do n = 1, iterations
                    call betsqk(real(n-1)*memori_diff,0.,real(n)*memori_diff,width,r(n),g(n),b(n))
                end do
                call plot(0.,0.,3);call plot(length-2.*memori_diff,0.,2);call plot(0.,width,3);call plot(length-2.*(memori_diff),width,2);call plot(0.,0.,3)
                if(present(lt))then
                    call betmlk(lefty_x,lefty_y,3,3,r(0),g(0),b(0));call plot(0.,0.,3)
                    call plot(lefty_x(1),lefty_y(1),3)
                    call plot(lefty_x(2),lefty_y(2),2);call plot(lefty_x(3),lefty_y(3),2)
                end if
                if(present(gt))then
                    call plot(0.,0.,3)
                    call betmlk(righty_x,righty_y,3,3,r(iterations+1),g(iterations+1),b(iterations+1));call plot(0.,0.,3)
                    call plot(righty_x(1),righty_y(1),3);call plot(righty_x(2),righty_y(2),2);call plot(righty_x(3),righty_y(3),2)
                end if
                do n = 0, iterations
                    call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,width,2)
                    if(present(TorB).and.TorB=='T')then
                        call plot(real(n)*memori_diff,width,3);call plot(real(n)*memori_diff,width+symbol_size/8.,2)
                    else
                        call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,-symbol_size/8.,2)
                    end if
                    if(present(symbol_start))then
                        if(n>=symbol_start-1.and.mod(n-symbol_start-1,symbol_freq)==0 ) then
                            if(present(TorB).and.TorB=='T')then
                                call plot(real(n)*memori_diff,width,3);call plot(real(n)*memori_diff,width+symbol_size/4.,2)
                                if(present(rangle)) then
                                    if(rangle/=0.)then;call numberc(real(n)*memori_diff-symbol_size*0.3,width+1.5*symbol_size,symbol_size,ini_num+num_diff*real(n),-rangle,float_quantity)
                                    else;call numberc(real(n)*memori_diff,width+.5*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                                    end if
                                else;call numberc(real(n)*memori_diff,width+.5*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                                end if
                            else ! TorB=='B' or nada
                                call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,-symbol_size/4.,2)
                                if(present(rangle)) then
                                    if(rangle/=0.)then;call numberc(real(n)*memori_diff-symbol_size*0.3,-1.6*symbol_size,symbol_size,ini_num+num_diff*real(n),-rangle,float_quantity)
                                    else;call numberc(real(n)*memori_diff,-1.3*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                                    end if
                                elseif(.not.present(rangle))then;call numberc(real(n)*memori_diff,-1.3*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                                end if
                            end if
                        end if
                    else if(.not.present(symbol_start))then
                        if(mod(n,symbol_freq)==0 ) then
                            if(present(TorB).and.TorB=='T')then
                                call plot(real(n)*memori_diff,width,3);call plot(real(n)*memori_diff,width+symbol_size/4.,2)
                                if(present(rangle)) then
                                    if(rangle/=0.)then;call numberc(real(n)*memori_diff-symbol_size*0.3,width+1.5*symbol_size,symbol_size,ini_num+num_diff*real(n),-rangle,float_quantity)
                                    else;call numberc(real(n)*memori_diff,width+.5*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                                    end if
                                else;call numberc(real(n)*memori_diff,width+.5*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                                end if
                            else ! TorB=='B' or nada
                                call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,-symbol_size/4.,2)
                                if(present(rangle)) then
                                    if(rangle/=0.)then;call numberc(real(n)*memori_diff-symbol_size*0.3,-1.6*symbol_size,symbol_size,ini_num+num_diff*real(n),-rangle,float_quantity)
                                    else;call numberc(real(n)*memori_diff,-1.3*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                                    end if
                                else;call numberc(real(n)*memori_diff,-1.3*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                                end if
                            end if
                        end if
                    end if
                end do
                ! print*,'bpt3'
            if(present(lt))then
                if(present(TorB).and.TorB=='T')then
                    if(present(rangle))then
                        if(rangle/=0.)then;call symbolc(-1.4*symbol_size,+1.8*symbol_size,symbol_size*0.7,'<'//trim(min),-rangle,len('<'//trim(min)))
                        else;call symbolc(-2.*symbol_size,+1.*symbol_size,symbol_size*0.7,'<'//trim(min),0.,len('<'//trim(min)))
                        end if
                    else;call symbolc(-2.*symbol_size,+1.*symbol_size,symbol_size*0.7,'<'//trim(min),0.,len('<'//trim(min)))
                    end if
                else ! TorB=='B' or nada
                    if(present(rangle))then
                        if(rangle/=0.)then;call symbolc(-1.2*symbol_size,-1.2*symbol_size,symbol_size*0.7,'<'//trim(min),-rangle,len('<'//trim(min)))
                        else;call symbolc(-2.*symbol_size,-0.5*symbol_size,symbol_size*0.7,'<'//trim(min),0.,len('<'//trim(min)))
                        end if
                    else;call symbolc(-2.*symbol_size,-0.5*symbol_size,symbol_size*0.7,'<'//trim(min),0.,len('<'//trim(min)))
                    end if
                end if
            end if
            if (present(gt))then
                if(present(TorB).and.TorB=='T')then
                    if(present(rangle))then
                        if(rangle/=0.)then;call symbolc(length-2.*memori_diff+1.*symbol_size,+1.8*symbol_size,symbol_size*0.7,trim(max)//'<',-rangle,len('>'//trim(max)))
                        else;call symbolc(length-2.*memori_diff+1.5*symbol_size,+1.*symbol_size,symbol_size*0.7,trim(max)//'<',0.,len('>'//trim(max)))
                        end if
                    else;call symbolc(length-2.*memori_diff+1.5*symbol_size,+1.*symbol_size,symbol_size*0.7,trim(max)//'<',0.,len('>'//trim(max)))
                    end if
                else ! TorB=='B' or nada
                    if(present(rangle))then
                        if(rangle/=0.)then;call symbolc(length-2.*memori_diff+1.*symbol_size,-1.2*symbol_size,symbol_size*0.7,trim(max)//' <',-rangle,len('>'//trim(max)))
                        else;call symbolc(length-2.*memori_diff+1.5*symbol_size,-0.5*symbol_size,symbol_size*0.7,trim(max)//'<',0.,len('>'//trim(max)))
                        end if
                    else;call symbolc(length-2.*memori_diff+1.5*symbol_size,-0.5*symbol_size,symbol_size*0.7,trim(max)//'<',0.,len('>'//trim(max)))
                    end if
                end if
            end if
            call plot(length/2.-memori_diff,width/2.,-3)
            if(present(rangle)) then
                write(ounit,'(f9.4,2x,a4)' ) -rangle , ' ro '
            end if
            if(present(x).and.present(y))call plot(-x,-y,-3)
            if(present(x).and. .not.present(y))call plot(-x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,-y,-3)

            
        end subroutine
        ! MEMORI
        subroutine memori(iterations,memori_size,bimemori_freq,length,rangle,x,y,gap)
            implicit none
            real,intent(in)::memori_size,length,rangle
            integer,intent(in)::iterations,bimemori_freq
            integer,intent(in),optional::gap
            real,intent(in),optional::x,y
            real::dx,gappy
            integer::n
        
            if(present(x).and.present(y))call plot(x,y,-3)
            if(present(x).and. .not.present(y))call plot(x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,y,-3)
            write(ounit,'(f10.4,2x,a4)' ) rangle , ' ro ' 
            call plot(-length/2.,0.,-3)
            if(memori_size<=0.05)then;call newpen2(2)
            else if(memori_size>0.05.and.memori_size<=0.12)then;call newpen2(3)
            else if(memori_size>0.12.and.memori_size<=0.2)then;call newpen2(4)
            else;call newpen2(5);end if
            
            if(present(gap))then
                if(gap == 2) then
                    dx = length/real(iterations);gappy = dx/2.
                else if(gap == 1) then
                    dx = length/real(iterations+1);gappy = dx
                end if
            else;dx = length/real(iterations-1);gappy = 0.
            end if
            do n = 1, iterations
                ! if(gappy/=0.and.n==iterations)cycle
                if(bimemori_freq==0)then;call plot(gappy+real(n-1)*dx,0.,3);call plot(gappy+real(n-1)*dx,-memori_size,2)
                elseif(bimemori_freq/=0) then
                    if(mod(n,bimemori_freq)==0 .or.n==1)then
                    call plot(gappy+real(n-1)*dx,0.,3);call plot(gappy+real(n-1)*dx,-memori_size*2.,2)
                    else;call plot(gappy+real(n-1)*dx,0.,3);call plot(gappy+real(n-1)*dx,-memori_size,2)
                    end if
                end if
            end do
            call plot(length/2.,0.,-3)
            write(ounit,'(f9.4,2x,a4)' ) -rangle , ' ro ' 

            if(present(x).and.present(y))call plot(-x,-y,-3)
            if(present(x).and. .not.present(y))call plot(-x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,-y,-3)

        end subroutine
        subroutine num_memori(ini_num,fin_num,iterations,symbol_freq,symbol_size,float_quantity,length,angle,lt,gt,x,y)
            implicit none
            real,intent(in)::ini_num,fin_num,symbol_size,length
            integer,intent(in)::iterations,symbol_freq,angle,float_quantity
            integer,intent(in),optional::lt,gt
            real,intent(in),optional::x,y
            real::memori_diff,num_diff
            integer::n
        
            if(present(x).and.present(y))call plot(x,y,-3)
            if(present(x).and. .not.present(y))call plot(x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,y,-3)
            if(symbol_size<=0.2)then;call newpen2(2)
            else if(symbol_size>0.2.and.symbol_size<=0.5)then;call newpen2(3)
            else if(symbol_size>0.5.and.symbol_size<=0.8)then;call newpen2(4)
            else;call newpen2(5);end if

            if(.not.present(lt).and..not.present(gt)) then
                ! call newpen2(3)
                memori_diff = length/real(iterations); num_diff = (fin_num-ini_num)/real(iterations)
                if(angle == 0) then
                    do n = 0, iterations
                        if(mod(n,symbol_freq)==0) then
                            call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,-0.4*symbol_size,2)
                            call numberc(real(n)*memori_diff,-1.2*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                        else; call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,-0.2*symbol_size,2)
                        end if
                    end do
                else if(angle == 90) then
                    do n = 0, iterations
                        if(mod(n,symbol_freq)==0) then
                            call plot(0.,real(n)*memori_diff,3);call plot(0.4*symbol_size,real(n)*memori_diff,2)
                            call number(0.5*symbol_size,real(n)*memori_diff-symbol_size*0.3,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                        else;call plot(0.,real(n)*memori_diff,3);call plot(0.2*symbol_size,real(n)*memori_diff,2)
                        end if
                    end do
                else if (angle == -90) then
                    do n = 0, iterations
                        if(mod(n,symbol_freq)==0) then
                            call plot(0.,real(n)*memori_diff,3);call plot(-0.4*symbol_size,real(n)*memori_diff,2)
                            call numberr(-0.4*symbol_size,real(n)*memori_diff-symbol_size*0.3,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                        else;call plot(0.,real(n)*memori_diff,3);call plot(-0.2*symbol_size,real(n)*memori_diff,2)
                        end if
                    end do
                else;end if
            else;end if
        
            if(present(lt).and.present(gt)) then
                ! call newpen2(3)
                memori_diff = length/real(iterations); num_diff = (fin_num-ini_num)/real(iterations)
                if(angle == 0) then
                    do n = 0, iterations
                        if(mod(n,symbol_freq)==0) then;call numberc(real(n)*memori_diff,-1.2*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                        call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,-0.1,2)
                        else;call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,-0.05,2)
                        end if
                    end do
                    call symbolr(-memori_diff*1.6,-2.0*symbol_size,symbol_size,'<',0.,1);call number(-memori_diff*1.6,-2.0*symbol_size,symbol_size,ini_num,0.,float_quantity)
                    call symbol(length+memori_diff*1.6,-2.0*symbol_size,symbol_size,'<',0.,1);call numberr(length+memori_diff*1.6,-2.0*symbol_size,symbol_size,fin_num,0.,float_quantity)
                else !(angle == 90)
                    do n = 0, iterations
                        if(mod(n,symbol_freq)==0) then;call number(0.5*symbol_size,real(n)*memori_diff,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                        call plot(0.,real(n)*memori_diff,3);call plot(0.01,real(n)*memori_diff,2)
                        else;call plot(0.,real(n)*memori_diff,3);call plot(0.05,real(n)*memori_diff,2)
                        end if
                    end do
                    call symbolr(1.4*symbol_size,-memori_diff*1.1,symbol_size,'<',0.,1);call number(1.4*symbol_size,-memori_diff*1.1,symbol_size,ini_num,0.,float_quantity)
                    call symbolr(1.4*symbol_size,length+memori_diff*1.1,symbol_size,'>',0.,1);call number(1.4*symbol_size,length+memori_diff*1.1,symbol_size,fin_num,0.,float_quantity)
                end if
            else;end if 
        
            if(present(gt) .and. .not.present(lt)) then
            ! call newpen2(3)
                memori_diff = length/real(iterations); num_diff = (fin_num-ini_num)/real(iterations)
                if(angle == 0) then
                    do n = 0, iterations
                        if(mod(n,symbol_freq)==0) then;call numberc(n*memori_diff,-1.2*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                        call plot(n*memori_diff,0.,3);call plot(n*memori_diff,-0.1,2)
                        else;call plot(n*memori_diff,0.,3);call plot(n*memori_diff,-0.05,2)
                        end if
                    end do
                    call symbol(length+memori_diff*1.6,-2.0*symbol_size,symbol_size,'<',0.,1);call numberr(length+memori_diff*1.6,-2.0*symbol_size,symbol_size,fin_num,0.,float_quantity)
                else !(angle == 90)
                    do n = 0, iterations
                        if(mod(n,symbol_freq)==0) then;call number(0.5*symbol_size,real(n)*memori_diff,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                        call plot(0.,n*memori_diff,3);call plot(0.01,n*memori_diff,2)
                        else;call plot(0.,n*memori_diff,3);call plot(0.05,n*memori_diff,2)
                        end if
                    end do
                    call symbolr(1.4*symbol_size,length+memori_diff*1.1,symbol_size,'>',0.,1);call number(1.4*symbol_size,length+memori_diff*1.1,symbol_size,fin_num,0.,float_quantity)
                end if
            else;end if
        
            if(.not.present(gt) .and. present(lt)) then
                ! call newpen2(3)
                    memori_diff = length/real(iterations); num_diff = (fin_num-ini_num)/real(iterations)
                    if(angle == 0) then
                        do n = 0, iterations
                            if(mod(n,symbol_freq)==0) then;call numberc(n*memori_diff,-1.2*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                            call plot(n*memori_diff,0.,3);call plot(n*memori_diff,-0.1,2)
                            else;call plot(n*memori_diff,0.,3);call plot(n*memori_diff,-0.05,2)
                            end if
                        end do
                        call symbolr(-memori_diff*1.6,-2.0*symbol_size,symbol_size,'<',0.,1);call number(-memori_diff*1.6,-2.0*symbol_size,symbol_size,ini_num,0.,float_quantity)
                    else !(angle == 90)
                        do n = 0, iterations
                            if(mod(n,symbol_freq)==0) then;call number(0.5*symbol_size,real(n)*memori_diff,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                            call plot(0.,n*memori_diff,3);call plot(0.1,n*memori_diff,2)
                            else;call plot(0.,n*memori_diff,3);call plot(0.05,n*memori_diff,2)
                            end if
                        end do
                        call symbolr(1.4*symbol_size,-memori_diff*1.1,symbol_size,'<',0.,1);call number(1.4*symbol_size,-memori_diff*1.1,symbol_size,ini_num,0.,float_quantity)
                    end if
            else;end if   

            if(present(x).and.present(y))call plot(-x,-y,-3)
            if(present(x).and. .not.present(y))call plot(-x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,-y,-3)
        
        end subroutine
        subroutine st_memori(ini_st,fin_st,width,top_bottom,symbol_size,gap,x,y)
            implicit none
            integer,intent(in)::ini_st,fin_st,top_bottom
            real,intent(in)::width,symbol_size
            real,intent(in),optional::x,y
            integer,intent(in),optional::gap
            real::dx,gappy
            integer::n
            if(present(x).and.present(y))call plot(x,y,-3)
            if(present(x).and. .not.present(y))call plot(x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,y,-3)
            if(symbol_size<=0.2)then;call newpen2(2)
            else if(symbol_size>0.2.and.symbol_size<=0.4)then;call newpen2(3)
            else if(symbol_size>0.2.and.symbol_size<=0.4)then;call newpen2(4)
            else;call newpen2(5);end if
            if(present(gap))then
                if(gap == 2) then
                dx = width/real(fin_st-ini_st+1);gappy = dx/2.
                else if(gap == 1) then
                    dx = width/real(fin_st-ini_st+2);gappy = dx
                else if(gap == 0) then
                    dx = width/real(fin_st-ini_st);gappy = 0.
                end if
            else;dx = width/real(fin_st-ini_st);gappy = 0.
            end if
            if(top_bottom == 1) then
                do n = 1,fin_st-ini_st+1
                    call plot(gappy+real(n-1)*dx,0.,3);call plot(gappy+real(n-1)*dx,-symbol_size*0.3,2)
                    call numberc(gappy+real(n-1)*dx,-symbol_size*1.3,symbol_size,real(fin_st-n+1),0.,-1)
                end do
            else if(top_bottom == 0) then
                do n = 1,fin_st-ini_st+1
                    call plot(gappy+real(n-1)*dx,0.,3);call plot(gappy+real(n-1)*dx,symbol_size*0.3,2)
                    call numberc(gappy+real(n-1)*dx,symbol_size*0.8,symbol_size,real(fin_st-n+1),0.,-1)
                end do
            end if 
            if(present(x).and.present(y))call plot(-x,-y,-3)
            if(present(x).and. .not.present(y))call plot(-x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,-y,-3)
        end subroutine
        ! gap bw memori and box. gap=0:no gap, gap=1:dx, gap=2:dx/2. get dxval if needed
        subroutine mod12_memori(iterations,symbol_size,angle,length,gap,num_freq,num_st,x,y,dxval)
            implicit none
            real,intent(in)::symbol_size,length
            real,intent(in),optional::x,y
            integer,intent(in),optional::num_freq,num_st
            real,intent(out),optional::dxval
            integer,intent(in)::iterations,angle,gap
            real::dx,gappy
            integer::n,m,printm

            if(present(x).and.present(y))call plot(x,y,-3)
            if(present(x).and. .not.present(y))call plot(x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,y,-3)
            if(symbol_size<=0.2)then;call newpen2(2)
            else if(symbol_size>0.2.and.symbol_size<=0.5)then;call newpen2(3)
            else if(symbol_size>0.5.and.symbol_size<=0.8)then;call newpen2(4)
            else;call newpen2(5);end if

            if(gap == 2) then
                    dx = length/real(iterations);gappy = dx/2.
                else if(gap == 1) then
                    dx = length/real(iterations+1);gappy = dx
                else if(gap == 0) then
                    dx = length/real(iterations-1);gappy = 0.
            end if
            if(present(dxval)) then;dxval = dx;else;end if
            if(angle==0) then
                call plot(0.,0.,3);call plot(length,0.,2)
                do n = 1,iterations
                    if (mod(n,12)/=0) then;m = mod(n,12)
                    else if(mod(n,12)==0) then;m = 12
                    else;end if
                    call plot(gappy+real(n-1)*dx,0.,3);call plot(gappy+real(n-1)*dx,-0.4*symbol_size,2)
                    ! if(inc_dec == 1) then;printm = 13-m;else;printm = m;end if
                    printm = m
                    if(present(num_freq))then
                        if(present(num_st))then
                            if(n>=num_st.and.mod(n,num_freq)==0)then
                                call numberc(gappy+real(n-1)*dx,-1.2*symbol_size,symbol_size,real(printm),0.,-1)
                            end if
                        else;if(mod(n,num_freq)==0)call numberc(gappy+real(n-1)*dx,-1.2*symbol_size,symbol_size,real(printm),0.,-1)
                        end if
                    else;call numberc(gappy+real(n-1)*dx,-1.2*symbol_size,symbol_size,real(printm),0.,-1)
                    end if
                end do
            else if(angle == -90) then
                call plot(0.,0.,3);call plot(0.,length,2)
                do n = 1,iterations
                    if (mod(n,12)/=0) then;m = mod(n,12)
                    else if(mod(n,12)==0) then;m = 12
                    else;end if
                    call plot(0.,gappy+real(n-1)*dx,3);call plot(-0.4*symbol_size,gappy+real(n-1)*dx,2)
                    ! if(inc_dec == 1) then;printm = 13-m;else;printm = m;end if
                    printm = m
                    if(present(num_freq))then
                        if(present(num_st))then
                            if(n>=num_st.and.mod(n,num_freq)==0)then
                                call numberc(-1.*symbol_size,gappy+real(n-1)*dx-symbol_size*0.3,symbol_size,real(printm),0.,-1)
                            end if
                        else;if(mod(n,num_freq)==0)call numberc(-1.*symbol_size,gappy+real(n-1)*dx-symbol_size*0.3,symbol_size,real(printm),0.,-1)
                        end if
                    else;call numberc(-1.*symbol_size,gappy+real(n-1)*dx-symbol_size*0.3,symbol_size,real(printm),0.,-1)
                    end if
                end do
            end if

            if(present(x).and.present(y))call plot(-x,-y,-3)
            if(present(x).and. .not.present(y))call plot(-x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,-y,-3)        

        end subroutine
    ! END PLOTS
    ! BASIC STATISTICS
        ! calculates mean, s, and sem per LOOP of a 1D array make sure LOOP*ITERATIONS < ARRAYSIZE, size of arrays is ITERATIONS
        subroutine avsemloop_1D(array_1D,dim1,loop,iterations,mean_1D,s_1D,sem_1D,dataquan_1D)
            implicit none
            integer,intent(in)::dim1,loop,iterations
            real,intent(in)::array_1D(:)
            real,dimension(iterations),intent(out),optional::mean_1D,s_1D,sem_1D
            integer,dimension(iterations),intent(out),optional::dataquan_1D
            integer::l,i,count=0
            real::smean, s,sem,sum0=0.,sum1=0.

            if(dim1 < loop*iterations) then;print*,'Array size < loop*iterations';stop;end if
            if(size(array_1D)/=dim1) then;print*,'Array size /= dim1';stop;end if
            do i = 1, iterations
                count = 0;sum0=0.;sum1 = 0.
                do l = 1+loop*(i-1),loop*i
                    if(array_1D(l)/=0.) then
                        count = count +1;sum0 = sum0 + array_1D(l)
                    end if
                end do
                if(count <=1) then
                    smean = 0.;s = 0.;sem = 0.
                else
                    smean = sum0 / real(count)
                    do l = 1+loop*(i-1),loop*i
                        if(array_1D(l)/=0.) then
                            sum1 = sum1 + (array_1D(l) - smean)**2.
                        end if
                    end do
                    s = sqrt(sum1 / real(count-1))
                    sem = s / sqrt(real(count))
                end if
                if(present(dataquan_1D)) dataquan_1D(i) = count
                if(present(mean_1D)) mean_1D(i) = smean
                if(present(s_1D)) s_1D(i) = s
                if(present(sem_1D)) sem_1D(i) = sem
            end do
        end subroutine
        ! calulates mean, s, and sem of numbers intermittently in a 1D array. JUMP is the number of elements to skip. array(jump)
        subroutine avsemjump_1D(array_1D,dim1,jump,loops,mean_1D,s_1D,sem_1D,dataquan_1D)
            implicit none
            integer,intent(in)::dim1,jump,loops
            real,intent(in)::array_1D(:)
            real,dimension(jump),intent(out),optional::mean_1D,s_1D,sem_1D
            integer,dimension(jump),intent(out),optional::dataquan_1D
            integer::l,i,count=0
            real::smean, s,sem,sum0=0.,sum1=0.

            if(dim1 < jump*loops) then;print*,'Array size < jump*iterations';stop;end if
            if(size(array_1D)/=dim1) then;print*,'Array size /= dim1';stop;end if
            do i = 1, jump
                count = 0;sum0=0.;sum1 = 0.
                do l = i,i+jump*(loops-1),jump
                    if(array_1D(l)/=0.) then
                        count = count +1;sum0 = sum0 + array_1D(l)
                    end if
                end do
                if(count <=1) then
                    smean = 0.;s = 0.;sem = 0.
                else
                    smean = sum0 / real(count)
                    do l = i,jump*(loops-1),jump
                        if(array_1D(l)/=0.) then
                            sum1 = sum1 + (array_1D(l) - smean)**2.
                        end if
                    end do
                    s = sqrt(sum1 / real(count-1))
                    sem = s / sqrt(real(count))
                end if
                if(present(dataquan_1D)) dataquan_1D(i) = count
                if(present(mean_1D)) mean_1D(i) = smean
                if(present(s_1D)) s_1D(i) = s
                if(present(sem_1D)) sem_1D(i) = sem
            end do
        end subroutine
        ! welch's t for diff in (mean1-mean2).a = 0.05 on both sides. 0==nodif,1==larger,-1==smaller,911==error
        subroutine welchttest(mean1,s1,dataquan1,mean2,s2,dataquan2,result)
            implicit none
            real,intent(in)::mean1,s1,mean2,s2
            integer,intent(in)::dataquan1,dataquan2
            integer,intent(out)::result
            real,dimension(0:30)::t_95=0.
            real::diff_mean,n1,n2,df,sem,bottomCI,topCI

            if(mean1 /=0. .and. mean2/=0. .and. dataquan1/=0 .and. dataquan2/=0) then
                diff_mean = mean1 - mean2
                n1 = real(dataquan1);n2 = real(dataquan2)
                sem = sqrt((s1**2./n1)+(s2**2./n2)) 
                df = (((s1**2.)/n1)+((s2**2.)/n2))**2./(((s1**2./n1)**2./(n1-1))+((s2**2./n2)**2./(n2-1)))
                call t95_value(t_95)
                bottomCI = diff_mean - t_95(int(df))*sem ; topCI = diff_mean + t_95(int(df))*sem
                ! print*,diff_mean,bottomCI,topCI,int(df)
                if(bottomCI>0.) then
                    result = 1 !larger
                else if(topCI<0.) then
                    result = -1 !smaller
                else;result = 0 !no difference
                end if
            else;result = 911 !error
            end if

        end subroutine
        ! arrays have to be the same size
        subroutine correcoeff(array_1D,array_1D2,dim,r)
            implicit none
            integer,intent(in)::dim
            real,intent(in)::array_1D(:),array_1D2(:)
            real,intent(out)::r
            real::mean1,mean2,s1,s2,sum0=0.,sum1=0.,sum2=0.,covariance
            integer::n,count=0

            if(size(array_1D)/=dim)then; print*,'dim=',dim;print*,'array1 size=',size(array_1D);stop;end if
            if(size(array_1D2)/=dim)then; print*,'dim=',dim;print*,'array2 size=',size(array_1D2);stop;end if
            do n = 1, dim
                if(array_1D(n)/=0. .and. array_1D2(n)/=0.) then
                    count = count + 1
                    sum1 = sum1 + array_1D(n)
                    sum2 = sum2 + array_1D2(n)
                end if
            end do

            if(count <=1) then
                mean1 = 0.;mean2 = 0.;r = 0.
            else
                mean1 = sum1 / real(count);mean2 = sum2 / real(count)
                sum0 = 0.;sum1 = 0.;sum2 = 0.
                do n = 1, dim
                    if(array_1D(n)/=0. .and. array_1D2(n)/=0.) then
                        sum0 = sum0 + (array_1D(n) - mean1)*(array_1D2(n) - mean2) ! product of diffs from each means 
                        sum1 = sum1 + (array_1D(n) - mean1)**2. ! sum of squares of diffs from mean1
                        sum2 = sum2 + (array_1D2(n) - mean2)**2. ! sum of squares of diffs from mean2
                    end if
                end do
                s1 = sqrt(sum1 / real(count-1))
                s2 = sqrt(sum2 / real(count-1))
                covariance = sum0 / real(count-1)
                if(s1 == 0. .or. s2 == 0.) then
                    r = 0.
                else
                r = covariance / (s1*s2)
                end if
            end if


        end subroutine
    
        ! avsdsemdataquan better series
        ! gives an array of mean arrays    DO NOT USE INSIDE A LOOP ALLOCATION IS TRICKY
        subroutine avsemdata_2D(array_2D,dim1,dim2,dec_dim,mean_1D,s_1D,sem_1D,dataquan_1D)
            implicit none
            integer,intent(in)::dim1,dim2
            character(len=*),intent(in)::dec_dim
            real,intent(in)::array_2D(:,:)
            real,dimension(:),allocatable,intent(out),optional::mean_1D,s_1D,sem_1D
            integer,dimension(:),allocatable,intent(out),optional::dataquan_1D
            integer::n,i,count=0
            real::smean, s, sem, sum0=0.,sum1=0.

            if(size(array_2D,1)<dim1 .or. size(array_2D,2)<dim2) then 
                print*,'Array size < dim1 or dim2';stop
            end if
            if (dec_dim == 'dim1') then
                if (present(mean_1D)) allocate(mean_1D(dim2))
                if (present(s_1D)) allocate(s_1D(dim2))
                if (present(sem_1D)) allocate(sem_1D(dim2))
                if (present(dataquan_1D)) allocate(dataquan_1D(dim2))
                do n = 1, dim2
                    count = 0;sum0=0.;sum1 = 0.
                    do i = 1, dim1
                        if (array_2D(i, n) /= 0.0)then;count = count + 1;sum0 = sum0 + array_2D(i,n);end if
                    end do
                    if (count <=1 ) then
                        smean = 0.;s = 0.;sem = 0.
                    else
                        smean = sum0 / real(count)
                        do i = 1, dim1
                            if(array_2D(i,n)/=0.) then
                                sum1 = sum1 + (array_2D(i,n) - smean)**2.
                            end if
                        end do
                        s = sqrt(sum1 / real(count-1))
                        sem = s / sqrt(real(count))
                    end if
                        if (present(dataquan_1D)) dataquan_1D(n) = count
                        if (present(mean_1D)) mean_1D(n) = smean
                        if (present(s_1D)) s_1D(n) = s
                        if (present(sem_1D)) sem_1D(n) = sem
                end do
            else if (dec_dim == 'dim2') then
                if (present(mean_1D)) allocate(mean_1D(dim1))
                if (present(s_1D)) allocate(s_1D(dim1))
                if (present(sem_1D)) allocate(sem_1D(dim1))
                if (present(dataquan_1D)) allocate(dataquan_1D(dim1))
                do n = 1, dim1
                    count = 0;sum0=0.;sum1 = 0.
                    do i = 1, dim2
                        if (array_2D(n, i) /= 0.0)then;count = count + 1;sum0 = sum0 + array_2D(n,i);end if
                        ! print*,n,i,count,sum0
                    end do
                    if (count <=1 ) then
                        smean = 0.;s = 0.;sem = 0.
                    else
                        smean = sum0 / real(count)
                        do i = 1, dim2
                            if(array_2D(n,i)/=0.) then
                                sum1 = sum1 + (array_2D(n,i) - smean)**2.
                            end if
                        end do
                        s = sqrt(sum1/ real(count-1))
                        sem = s / sqrt(real(count))
                    end if
                        if (present(dataquan_1D)) dataquan_1D(n) = count
                        if (present(mean_1D)) mean_1D(n) = smean
                        if (present(s_1D)) s_1D(n) = s
                        if (present(sem_1D)) sem_1D(n) = sem
                end do
            else
                print *, 'Choose a dimension to take mean of (descard)'
                stop
            end if


            ! if(present(mean_1D)) deallocate(mean_1D)
            ! if(present(s_1D)) deallocate(s_1D)
            ! if(present(sem_1D)) deallocate(sem_1D)
            ! if(present(dataquan_1D)) deallocate(dataquan_1D)

        end subroutine

        subroutine avsemdata_3D(array_3D,dim1,dim2,dim3,dec_dim,mean_2D,s_2D,sem_2D,dataquan_2D)
            implicit none
            integer,intent(in)::dim1,dim2,dim3
            character(len=*),intent(in)::dec_dim
            real,intent(in)::array_3D(:,:,:)
            real,dimension(:,:),allocatable,intent(out),optional::mean_2D,s_2D,sem_2D
            integer,dimension(:,:),allocatable,intent(out),optional::dataquan_2D
            integer::l1,l2,l3,count=0,loop1,loop2,loop3
            real::smean, s, sem, sum0=0.,sum1=0.

            if(size(array_3D,1)<dim1 .or. size(array_3D,2)<dim2 .or. size(array_3D,3)<dim3) then 
                print*,'Array size < dim1 or dim2 or dim3';stop
            end if
            ! loop determination
                if (dec_dim == 'dim1') then
                    loop1 = dim2
                    loop2 = dim3
                    loop3 = dim1
                    if(present(mean_2D)) allocate(mean_2D(dim2,dim3))
                    if(present(s_2D)) allocate(s_2D(dim2,dim3))
                    if(present(sem_2D)) allocate(sem_2D(dim2,dim3))
                    if(present(dataquan_2D)) allocate(dataquan_2D(dim2,dim3))
                elseif (dec_dim == 'dim2') then
                    loop1 = dim1
                    loop2 = dim3
                    loop3 = dim2
                    if(present(mean_2D)) allocate(mean_2D(dim1,dim3))
                    if(present(s_2D)) allocate(s_2D(dim1,dim3))
                    if(present(sem_2D)) allocate(sem_2D(dim1,dim3))
                    if(present(dataquan_2D)) allocate(dataquan_2D(dim1,dim3))
                elseif (dec_dim == 'dim3') then
                    loop1 = dim1
                    loop2 = dim2
                    loop3 = dim3
                    if(present(mean_2D)) allocate(mean_2D(dim1,dim2))
                    if(present(s_2D)) allocate(s_2D(dim1,dim2))
                    if(present(sem_2D)) allocate(sem_2D(dim1,dim2))
                    if(present(dataquan_2D)) allocate(dataquan_2D(dim1,dim2))
                else
                    print*, 'Invalid dec_dim value'
                    stop
                end if
            ! loop determination end
            do l1 = 1, loop1
                do l2 = 1, loop2
                    count = 0; sum0 = 0.;sum1 = 0.
                    do l3 = 1, loop3 !dec_dim loop
                        if(dec_dim=='dim1') then
                            if(array_3D(l3,l1,l2)/=0.) then
                                count = count + 1
                                sum0 = sum0 + array_3D(l3,l1,l2)
                            end if
                        else if(dec_dim=='dim2') then
                            if(array_3D(l1,l3,l2)/=0.) then
                                count = count + 1
                                sum0 = sum0 + array_3D(l1,l3,l2)
                            end if
                        else if(dec_dim=='dim3') then
                            if(array_3D(l1,l2,l3)/=0.) then
                                count = count + 1
                                sum0 = sum0 + array_3D(l1,l2,l3)
                            end if
                        end if
                    end do
                        if(count<=1) then;smean= 0.;s=0.;sem=0.
                        else;smean = sum0/real(count)
                            do l3 = 1, loop3
                                if(dec_dim=='dim1') then
                                    if(array_3D(l3,l1,l2)/=0.) then
                                        sum1 = sum1 + (array_3D(l3,l1,l2) - smean)**2.
                                    end if
                                else if(dec_dim=='dim2') then
                                    if(array_3D(l1,l3,l2)/=0.) then
                                        sum1 = sum1 + (array_3D(l1,l3,l2) - smean)**2.
                                    end if
                                else if(dec_dim=='dim3') then
                                    if(array_3D(l1,l2,l3)/=0.) then
                                        sum1 = sum1 + (array_3D(l1,l2,l3) - smean)**2.
                                    end if
                                end if
                            end do
                            s = sqrt(sum1/real(count-1))
                            sem = s / sqrt(real(count))
                        end if
                        if(present(mean_2D)) mean_2D(l1,l2) = smean
                        if(present(s_2D)) s_2D(l1,l2) = s
                        if(present(sem_2D)) sem_2D(l1,l2) = sem
                        if(present(dataquan_2D)) dataquan_2D(l1,l2) = count
                end do
            end do
            ! end do



        end subroutine

        subroutine avsemdata_4D(array_4D,dim1,dim2,dim3,dim4,dec_dim,mean_3D,s_3D,sem_3D,dataquan_3D)
            implicit none
            integer,intent(in)::dim1,dim2,dim3,dim4
            character(len=*),intent(in)::dec_dim
            real,intent(in)::array_4D(:,:,:,:)
            real,dimension(:,:,:),allocatable,intent(out),optional::mean_3D,s_3D,sem_3D
            integer,dimension(:,:,:),allocatable,intent(out),optional::dataquan_3D
            integer::l1,l2,l3,l4,count=0,loop1,loop2,loop3,loop4
            real::smean, s, sem, sum0=0.,sum1=0.

            if(size(array_4D,1)<dim1 .or. size(array_4D,2)<dim2 .or. size(array_4D,3)<dim3 .or. size(array_4D,4)<dim4) then 
                print*,'Array size < dim1 or dim2 or dim3 or dim4';stop
            end if
            ! loop determination
                if (dec_dim == 'dim1') then
                    loop1 = dim2
                    loop2 = dim3
                    loop3 = dim4
                    loop4 = dim1
                    if(present(mean_3D)) allocate(mean_3D(dim2,dim3,dim4))
                    if(present(s_3D)) allocate(s_3D(dim2,dim3,dim4))
                    if(present(sem_3D)) allocate(sem_3D(dim2,dim3,dim4))
                    if(present(dataquan_3D)) allocate(dataquan_3D(dim2,dim3,dim4))
                elseif (dec_dim == 'dim2') then
                    loop1 = dim1
                    loop2 = dim3
                    loop3 = dim4
                    loop4 = dim2
                    if(present(mean_3D)) allocate(mean_3D(dim1,dim3,dim4))
                    if(present(s_3D)) allocate(s_3D(dim1,dim3,dim4))
                    if(present(sem_3D)) allocate(sem_3D(dim1,dim3,dim4))
                    if(present(dataquan_3D)) allocate(dataquan_3D(dim1,dim3,dim4))
                elseif (dec_dim == 'dim3') then
                    loop1 = dim1
                    loop2 = dim2
                    loop3 = dim4
                    loop4 = dim3
                    if(present(mean_3D)) allocate(mean_3D(dim1,dim2,dim4))
                    if(present(s_3D)) allocate(s_3D(dim1,dim2,dim4))
                    if(present(sem_3D)) allocate(sem_3D(dim1,dim2,dim4))
                    if(present(dataquan_3D)) allocate(dataquan_3D(dim1,dim2,dim4))
                elseif (dec_dim == 'dim4') then
                    loop1 = dim1
                    loop2 = dim2
                    loop3 = dim3
                    loop4 = dim4
                    if(present(mean_3D)) allocate(mean_3D(dim1,dim2,dim3))
                    if(present(s_3D)) allocate(s_3D(dim1,dim2,dim3))
                    if(present(sem_3D)) allocate(sem_3D(dim1,dim2,dim3))
                    if(present(dataquan_3D)) allocate(dataquan_3D(dim1,dim2,dim3))
                else;print*, 'Invalid dec_dim value';stop
                end if
            ! loop determination end
            do l1 = 1, loop1
                do l2 = 1, loop2
                    do l3 = 1, loop3
                        count = 0; sum0 = 0.;sum1 = 0.
                        do l4 = 1, loop4 !dec_dim loop
                            if(dec_dim=='dim1') then
                                if(array_4D(l4,l1,l2,l3)/=0.) then
                                    count = count + 1
                                    sum0 = sum0 + array_4D(l4,l1,l2,l3)
                                end if
                            else if(dec_dim=='dim2') then
                                if(array_4D(l1,l4,l2,l3)/=0.) then
                                    count = count + 1
                                    sum0 = sum0 + array_4D(l1,l4,l2,l3)
                                end if
                            else if(dec_dim=='dim3') then
                                if(array_4D(l1,l2,l4,l3)/=0.) then
                                    count = count + 1
                                    sum0 = sum0 + array_4D(l1,l2,l4,l3)
                                end if
                            else if(dec_dim=='dim4') then
                                if(array_4D(l1,l2,l3,l4)/=0.) then
                                    count = count + 1
                                    sum0 = sum0 + array_4D(l1,l2,l3,l4)
                                end if
                            end if
                        end do
                            if(count<=1) then;smean= 0.;s=0.;sem=0.
                            else;smean = sum0/real(count)
                                do l4 = 1, loop4
                                    if(dec_dim=='dim1') then
                                        if(array_4D(l4,l1,l2,l3)/=0.) then
                                            sum1 = sum1 + (array_4D(l4,l1,l2,l3) - smean)**2.
                                        end if
                                    else if(dec_dim=='dim2') then
                                        if(array_4D(l1,l4,l2,l3)/=0.) then
                                            sum1 = sum1 + (array_4D(l1,l4,l2,l3) - smean)**2.
                                        end if
                                    else if(dec_dim=='dim3') then
                                        if(array_4D(l1,l2,l4,l3)/=0.) then
                                            sum1 = sum1 + (array_4D(l1,l2,l4,l3) - smean)**2.
                                        end if
                                    else if(dec_dim=='dim4') then
                                        if(array_4D(l1,l2,l3,l4)/=0.) then
                                            sum1 = sum1 + (array_4D(l1,l2,l3,l4) - smean)**2.
                                        end if
                                    end if
                                end do
                                s = sqrt(sum1/real(count-1))
                                sem = s / sqrt(real(count))
                            end if  
                            if(present(mean_3D)) mean_3D(l1,l2,l3) = smean
                            if(present(s_3D)) s_3D(l1,l2,l3) = s
                            if(present(sem_3D)) sem_3D(l1,l2,l3) = sem
                            if(present(dataquan_3D)) dataquan_3D(l1,l2,l3) = count                   
                    end do
                end do
            end do

        end subroutine

        subroutine avsemdata_5D(array_5D,dim1,dim2,dim3,dim4,dim5,dec_dim,mean_4D,s_4D,sem_4D,dataquan_4D)
            implicit none
            integer,intent(in)::dim1,dim2,dim3,dim4,dim5
            character(len=*),intent(in)::dec_dim
            real,intent(in)::array_5D(:,:,:,:,:)
            real,dimension(:,:,:,:),allocatable,intent(out),optional::mean_4D,s_4D,sem_4D
            integer,dimension(:,:,:,:),allocatable,intent(out),optional::dataquan_4D
            integer::l1,l2,l3,l4,l5,count=0,loop1,loop2,loop3,loop4,loop5
            real::smean, s, sem, sum0=0.,sum1=0.

            if(size(array_5D,1)<dim1 .or. size(array_5D,2)<dim2 .or. size(array_5D,3)<dim3 .or. size(array_5D,4)<dim4 .or. size(array_5D,5)<dim5) then 
                print*,'Array size < dim1 or dim2 or dim3 or dim4 or dim5';stop
            end if
            ! loop determination
                if (dec_dim == 'dim1') then
                    loop1 = dim2
                    loop2 = dim3
                    loop3 = dim4
                    loop4 = dim5
                    loop5 = dim1
                    if(present(mean_4D)) allocate(mean_4D(dim2,dim3,dim4,dim5))
                    if(present(s_4D)) allocate(s_4D(dim2,dim3,dim4,dim5))
                    if(present(sem_4D)) allocate(sem_4D(dim2,dim3,dim4,dim5))
                    if(present(dataquan_4D)) allocate(dataquan_4D(dim2,dim3,dim4,dim5))
                elseif (dec_dim == 'dim2') then
                    loop1 = dim1
                    loop2 = dim3
                    loop3 = dim4
                    loop4 = dim5
                    loop5 = dim2
                    if(present(mean_4D)) allocate(mean_4D(dim1,dim3,dim4,dim5))
                    if(present(s_4D)) allocate(s_4D(dim1,dim3,dim4,dim5))
                    if(present(sem_4D)) allocate(sem_4D(dim1,dim3,dim4,dim5))
                    if(present(dataquan_4D)) allocate(dataquan_4D(dim1,dim3,dim4,dim5))
                elseif (dec_dim == 'dim3') then
                    loop1 = dim1
                    loop2 = dim2
                    loop3 = dim4
                    loop4 = dim5
                    loop5 = dim3
                    if(present(mean_4D)) allocate(mean_4D(dim1,dim2,dim4,dim5))
                    if(present(s_4D)) allocate(s_4D(dim1,dim2,dim4,dim5))
                    if(present(sem_4D)) allocate(sem_4D(dim1,dim2,dim4,dim5))
                    if(present(dataquan_4D)) allocate(dataquan_4D(dim1,dim2,dim4,dim5))
                elseif (dec_dim == 'dim4') then
                    loop1 = dim1
                    loop2 = dim2
                    loop3 = dim3
                    loop4 = dim5
                    loop5 = dim4
                    if(present(mean_4D)) allocate(mean_4D(dim1,dim2,dim3,dim5))
                    if(present(s_4D)) allocate(s_4D(dim1,dim2,dim3,dim5))
                    if(present(sem_4D)) allocate(sem_4D(dim1,dim2,dim3,dim5))
                    if(present(dataquan_4D)) allocate(dataquan_4D(dim1,dim2,dim3,dim5))
                elseif (dec_dim == 'dim5') then
                    loop1 = dim1
                    loop2 = dim2
                    loop3 = dim3
                    loop4 = dim4
                    loop5 = dim5
                    if(present(mean_4D)) allocate(mean_4D(dim1,dim2,dim3,dim4))
                    if(present(s_4D)) allocate(s_4D(dim1,dim2,dim3,dim4))
                    if(present(sem_4D)) allocate(sem_4D(dim1,dim2,dim3,dim4))
                    if(present(dataquan_4D)) allocate(dataquan_4D(dim1,dim2,dim3,dim4))
                else;print*, 'Invalid dec_dim value';stop
                end if

            ! loop determination end
            do l1 = 1, loop1
                do l2 = 1, loop2
                    do l3 = 1, loop3
                        do l4 = 1, loop4
                            count = 0; sum0 = 0.;sum1 = 0.
                            do l5 = 1, loop5 !dec_dim loop
                                if(dec_dim=='dim1') then
                                    if(array_5D(l5,l1,l2,l3,l4)/=0.) then
                                        count = count + 1
                                        sum0 = sum0 + array_5D(l5,l1,l2,l3,l4)
                                    end if
                                else if(dec_dim=='dim2') then
                                    if(array_5D(l1,l5,l2,l3,l4)/=0.) then
                                        count = count + 1
                                        sum0 = sum0 + array_5D(l1,l5,l2,l3,l4)
                                    end if
                                else if(dec_dim=='dim3') then
                                    if(array_5D(l1,l2,l5,l3,l4)/=0.) then
                                        count = count + 1
                                        sum0 = sum0 + array_5D(l1,l2,l5,l3,l4)
                                    end if
                                else if(dec_dim=='dim4') then
                                    if(array_5D(l1,l2,l3,l5,l4)/=0.) then
                                        count = count + 1
                                        sum0 = sum0 + array_5D(l1,l2,l3,l5,l4)
                                    end if
                                else if(dec_dim=='dim5') then
                                    if(array_5D(l1,l2,l3,l4,l5)/=0.) then
                                        count = count + 1
                                        sum0 = sum0 + array_5D(l1,l2,l3,l4,l5)
                                    end if
                                end if
                            end do
                                if(count<=1) then;smean= 0.;s=0.;sem=0.
                                else;smean = sum0/real(count)
                                    do l5 = 1, loop5
                                        if(dec_dim=='dim1') then
                                            if(array_5D(l5,l1,l2,l3,l4)/=0.) then
                                                sum1 = sum1 + (array_5D(l5,l1,l2,l3,l4) - smean)**2.
                                            end if
                                        else if(dec_dim=='dim2') then
                                            if(array_5D(l1,l5,l2,l3,l4)/=0.) then
                                                sum1 = sum1 + (array_5D(l1,l5,l2,l3,l4) - smean)**2.
                                            end if
                                        else if(dec_dim=='dim3') then
                                            if(array_5D(l1,l2,l5,l3,l4)/=0.) then
                                                sum1 = sum1 + (array_5D(l1,l2,l5,l3,l4) - smean)**2.
                                            end if
                                        else if(dec_dim=='dim4') then
                                            if(array_5D(l1,l2,l3,l5,l4)/=0.) then
                                                sum1 = sum1 + (array_5D(l1,l2,l3,l5,l4) - smean)**2.
                                            end if
                                        else if(dec_dim=='dim5') then
                                            if(array_5D(l1,l2,l3,l4,l5)/=0.) then
                                                sum1 = sum1 + (array_5D(l1,l2,l3,l4,l5) - smean)**2.
                                            end if
                                        end if
                                    end do
                                    s = sqrt(sum1/real(count-1))
                                    sem = s / sqrt(real(count))
                                end if
                                if(present(mean_4D)) mean_4D(l1,l2,l3,l4) = smean
                                if(present(s_4D)) s_4D(l1,l2,l3,l4) = s
                                if(present(sem_4D)) sem_4D(l1,l2,l3,l4) = sem
                                if(present(dataquan_4D)) dataquan_4D(l1,l2,l3,l4) = count
                        end do 
                    end do            
                end do
            end do
        end subroutine
    !END BASIC STATISTICS 

    ! ps boys are good bois 
        ! if present, gap is dx/2. ;centralization of colors is done relative to integer centralize
        subroutine butler_psk(array_2D,dim1,dim2,width,height,maskval,ival,fval,inc,colorscheme,iterations,bpt1,bpt2,bpt3,conti,continc,thicc,r,g,b,gap,centralize)
            implicit none
            integer,intent(in)::dim1,dim2,iterations
            real,intent(in)::maskval,ival,fval,inc,width,height
            integer,intent(in),optional::bpt1,bpt2,bpt3,gap,thicc,centralize
            real,intent(in),optional::conti,continc
            real,intent(in)::array_2D(:,:)
            integer,dimension(size(array_2D,1),size(array_2D,2))::mask
            character(len=*),intent(in)::colorscheme
            real,dimension(:),allocatable::r1,g1,b1
            real,dimension(:,:),allocatable::another
            integer,dimension(:,:),allocatable::anothermask
            real,dimension(:),allocatable,intent(out),optional::r,g,b
            real::dx,dy
            integer::i,j,n,contquan,zerocolumns,nonzerocol
            write(ounit,*)'%begin butler_psk'
            if(size(array_2D,1)<dim1)then;print*,'Array size < dim1 ';stop;endif
            if(size(array_2D,2)<dim2)then;print*,'Array size < dim2 ';stop;endif

            if((abs(ival+inc*real(iterations)-fval))>=precision)then;print*,'your f value =',fval,'calculated f value =',ival+inc*real(iterations),abs(ival+inc*real(iterations)-fval),'(butler_psk)';end if
            call box(width,height,3)

            select case(colorscheme)
            case('red');call colorgrad('red',iterations,r1,g1,b1)
            case('wred');call colorgrad('wred',iterations,r1,g1,b1)
            case('green');call colorgrad('green',iterations,r1,g1,b1)
            case('wgreen');call colorgrad('wgreen',iterations,r1,g1,b1)
            case('blue');call colorgrad('blue',iterations,r1,g1,b1)
            case('wblue');call colorgrad('wblue',iterations,r1,g1,b1)
            case('b2r');if(present(bpt1))then;call b2r_colorgrad(iterations,bpt1,r1,g1,b1);else;print*,'bpt1 is required';stop;end if
            case('b2w2r');if(present(bpt1))then;call b2w2r_colorgrad(iterations,bpt1,r1,g1,b1);else;print*,'bpt1 is required';stop;end if
            case('b2gy2r');if(present(bpt1))then;call b2gy2r_colorgrad(iterations,bpt1,r1,g1,b1);else;print*,'bpt1 is required';stop;end if
            case('r2g');if(present(bpt1))then;call r2g_colorgrad(iterations,bpt1,r1,g1,b1);else;print*,'bpt1 is required';stop;end if
            case('bk2r2g');if(present(bpt1))then;call bk2r2g_colorgrad(iterations,bpt1,r1,g1,b1);else;print*,'bpt1 is required';stop;end if
            case('b2cy2y2r');if(present(bpt2).and.present(bpt3)) then;call b2cy2y2r_colorgrad(iterations,bpt1,bpt2,bpt3,r1,g1,b1);else;print*,'bpt2 and bpt3 are required';stop;end if
            case('b2g2y2r');if(present(bpt2).and.present(bpt3)) then;call b2g2y2r_colorgrad(iterations,bpt1,bpt2,bpt3,r1,g1,b1);else;print*,'bpt2 and bpt3 are required';stop;end if
            case default;print*,'Invalid colorscheme';stop
            end select

            if(present(centralize))then
                call centeralize_colors(iterations,centralize,r1,g1,b1)
            endif

            if(present(r).and.present(g).and.present(b)) then
                allocate(r(0:iterations+1),g(0:iterations+1),b(0:iterations+1));r = r1;g = g1;b = b1
            ! else;print*,'color arrays are not allocated'
            end if

            do i = 1, dim1
                do j = 1, dim2
                    if(array_2D(i,j)==maskval) then;mask(i,j)=0
                    else;mask(i,j)=1
                    end if
                end do
            end do
            zerocolumns = 0
            do i = 1, dim1
                if(all(mask(i,1:dim2)==0).eqv..true.)then
                    zerocolumns = zerocolumns + 1
                else;nonzerocol = i
                end if
            end do
            if(nonzerocol==0)then;print*,'zero matrix';return;endif

            if(dim1-zerocolumns>1)then
                if(.not.present(gap))then
                    dx = width/real(dim1-1);dy = height/real(dim2-1);call plot(-dx/2.,-dy/2.,-3)
                else;dx = width/real(dim1);dy = height/real(dim2);call plot(0.,-dy/2.,-3)
                end if
            else
                if(.not.present(gap))then
                    dx = width/real(dim1);dy = height/real(dim2)
                else;dx = width/real(dim1)*real(dim1-1)/real(dim1);dy = height/real(dim2);call plot(width/real(dim1)/2.,0.,-3)
                end if
            end if


            if(dim1-zerocolumns>1)then
                call pscolork(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,ival-10.**(10.),ival,r1(0),g1(0),b1(0))
                call pscolork(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,fval,fval+10.**(10.),r1(iterations+1),g1(iterations+1),b1(iterations+1))
                do n = 1, iterations
                    call pscolork(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,ival+real(n-1)*inc,ival+real(n)*inc,r1(n),g1(n),b1(n))
                end do
            else
                call betcolork2(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,ival-10.**(10.),ival,r1(0),g1(0),b1(0))
                call betcolork2(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,fval,fval+10.**(10.),r1(iterations+1),g1(iterations+1),b1(iterations+1))
                do n = 1, iterations
                    call betcolork2(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,ival+real(n-1)*inc,ival+real(n)*inc,r1(n),g1(n),b1(n))
                end do
            end if

            if(present(conti).and.present(continc))then
                contquan = int((maxval(array_2D)-conti)/continc+1)
                    if(dim1-zerocolumns>1)then
                        do n = 0, contquan
                            if(abs(width)<=2.)then;call newpen2(2);elseif(abs(width)>2..and.abs(width)<=4.)then;call newpen2(3);else;call newpen2(4);end if
                            if(present(thicc))then
                                if(mod(n,thicc)==0)then
                                    if(abs(width)<=2.)then;call newpen2(4);elseif(abs(width)>2..and.abs(width)<=4.)then;call newpen2(5);else;call newpen2(6);end if
                                end if
                            end if
                            call pscont3(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,1,conti+continc*real(n),0.)
                        end do
                    else
                        if(dim1-zerocolumns==1)then
                            ! if(present(gap))print*,'Having GAP for an array with one column'
                            print*,'has only one nonzero column=',nonzerocol
                            allocate(another(dim1+1,dim2));allocate(anothermask(dim1+1,dim2))
                            another(1:dim1,1:dim2) = array_2D;another(nonzerocol+1,1:dim2) = another(nonzerocol,1:dim2)
                            anothermask(1:dim1,1:dim2)=mask;anothermask(nonzerocol+1,1:dim2) = anothermask(nonzerocol,1:dim2)
                            call plot(-dx/2.,0.,-3)
                            do n = 0,contquan
                                if(abs(width)<=2.)then;call newpen2(2);elseif(abs(width)>2..and.abs(width)<=4.)then;call newpen2(3);else;call newpen2(4);end if
                                if(present(thicc))then
                                    if(mod(n,thicc)==0)then
                                        if(abs(width)<=2.)then;call newpen2(4);elseif(abs(width)>2..and.abs(width)<=4.)then;call newpen2(5);else;call newpen2(6);end if
                                    end if
                                end if
                                call pscont3(dx,dy,another,anothermask,1,dim1+1,1,dim2,dim1+1,dim2,1,conti+continc*real(n),0.)
                            end do
                            deallocate(another);deallocate(anothermask)
                            call plot(dx/2.,0.,-3)
                        end if
                    end if
            end if
            

            deallocate(r1,g1,b1)
            if(dim1-zerocolumns>1)then
                if(.not.present(gap))then
                    call plot(dx/2.,dy/2.,-3)
                else;call plot(0.,dy/2.,-3)
                end if
            else
                if(.not.present(gap))then
                    ! call plot(-dx/2.,0.,-3)
                else;call plot(-width/real(dim1)/2.,0.,-3)
                end if
            end if
            write(ounit,*)'%end butler_psk'

        end subroutine
        ! can draw contours on 1 column arrays
        subroutine butler_psbet(array_2D,dim1,dim2,width,height,maskval,ival,fval,inc,colorscheme,iterations,bpt1,bpt2,bpt3,conti,continc,thicc,r,g,b,centralize)
            implicit none
            integer,intent(in)::dim1,dim2,iterations
            real,intent(in)::maskval,ival,fval,inc,width,height
            integer,intent(in),optional::bpt1,bpt2,bpt3,thicc,centralize
            real,intent(in),optional::conti,continc
            real,intent(in)::array_2D(:,:)
            integer,dimension(size(array_2D,1),size(array_2D,2))::mask
            real,dimension(:,:),allocatable::another
            character(len=*),intent(in)::colorscheme
            real,dimension(:),allocatable::r1,g1,b1
            real,dimension(:),allocatable,intent(out),optional::r,g,b
            real::dx,dy
            integer::i,j,n,contquan,zerocolumns=0,nonzerocol=0

            if(size(array_2D,1)<dim1 .or. size(array_2D,2)<dim2) then 
                print*,'Array size < dim1 or dim2';stop
            end if
            if((abs(ival+inc*real(iterations)-fval))>=precision)then;print*,'your f value =',fval,'calculated f value =',ival+inc*real(iterations),abs(ival+inc*real(iterations)-fval),'(butler_psbet)';end if
            dx = width/real(dim1);dy = height/real(dim2)
            call box(width,height,3)
            select case(colorscheme)
            case('red');call colorgrad('red',iterations,r1,g1,b1)
            case('wred');call colorgrad('wred',iterations,r1,g1,b1)
            case('green');call colorgrad('green',iterations,r1,g1,b1)
            case('wgreen');call colorgrad('wgreen',iterations,r1,g1,b1)
            case('blue');call colorgrad('blue',iterations,r1,g1,b1)
            case('wblue');call colorgrad('wblue',iterations,r1,g1,b1)
            case('b2r');if(present(bpt1))then;call b2r_colorgrad(iterations,bpt1,r1,g1,b1);else;print*,'bpt1 is required';stop;end if
            case('b2w2r');if(present(bpt1))then;call b2w2r_colorgrad(iterations,bpt1,r1,g1,b1);else;print*,'bpt1 is required';stop;end if
            case('b2gy2r');if(present(bpt1))then;call b2gy2r_colorgrad(iterations,bpt1,r1,g1,b1);else;print*,'bpt1 is required';stop;end if
            case('r2g');if(present(bpt1))then;call r2g_colorgrad(iterations,bpt1,r1,g1,b1);else;print*,'bpt1 is required';stop;end if
            case('bk2r2g');if(present(bpt1))then;call bk2r2g_colorgrad(iterations,bpt1,r1,g1,b1);else;print*,'bpt1 is required';stop;end if
            case('b2cy2y2r');if(present(bpt2).and.present(bpt3)) then;call b2cy2y2r_colorgrad(iterations,bpt1,bpt2,bpt3,r1,g1,b1);else;print*,'bpt2 and bpt3 are required';stop;end if
            case('b2g2y2r');if(present(bpt2).and.present(bpt3)) then;call b2g2y2r_colorgrad(iterations,bpt1,bpt2,bpt3,r1,g1,b1);else;print*,'bpt2 and bpt3 are required';stop;end if
            case default;print*,'Invalid colorscheme';stop
            end select

            if(present(centralize))then
                call centeralize_colors(iterations,centralize,r1,g1,b1)
            endif
            
            if(present(r).and.present(g).and.present(b)) then
                allocate(r(0:iterations+1),g(0:iterations+1),b(0:iterations+1));r = r1;g = g1;b = b1
            ! else;print*,'color arrays are not allocated'
            end if

            do i = 1, dim1
                do j = 1, dim2
                    if(array_2D(i,j)==maskval) then;mask(i,j)=0
                    else;mask(i,j)=1
                    end if
                end do
            end do
            zerocolumns = 0
            do i = 1, dim1
                if(all(mask(i,1:dim2)==0).eqv..true.)then
                    zerocolumns = zerocolumns + 1
                else;nonzerocol = i
                end if
            end do
            if(nonzerocol==0)then;print*,'zero matrix';endif
            ! print*,'psbet',zerocolumns,nonzerocol
            call betcolork2(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,ival-10.**(10.),ival,r1(0),g1(0),b1(0))
            call betcolork2(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,fval,fval+10.**(10.),r1(iterations+1),g1(iterations+1),b1(iterations+1))
            do n = 1, iterations
                call betcolork2(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,ival+real(n-1)*inc,ival+real(n)*inc,r1(n),g1(n),b1(n))
            end do

            contquan = int((maxval(array_2D)-conti)/continc+1)
            if(present(conti).and.present(continc)) then
                if(dim1-zerocolumns>1)then
                    do n = 0, contquan
                        if(abs(width)<=2.)then;call newpen2(2);elseif(abs(width)>2..and.abs(width)<=4.)then;call newpen2(3);else;call newpen2(4);end if
                        if(present(thicc))then
                            if(mod(n,thicc)==0)then
                                if(abs(width)<=2.)then;call newpen2(4);elseif(abs(width)>2..and.abs(width)<=4.)then;call newpen2(5);else;call newpen2(6);end if
                            end if
                        end if
                        call pscont3(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,1,conti+continc*real(n),0.)
                    end do
                else if(dim1-zerocolumns==1)then
                    print*,'psbet nonzero column=',nonzerocol
                    allocate(another(dim1,dim2));another = array_2D;another(nonzerocol-1,1:dim2) = another(nonzerocol,1:dim2)
                    mask(nonzerocol-1,1:dim2) = mask(nonzerocol,1:dim2)
                    call plot(dx/2.,0.,-3)
                    do n = 0,contquan
                        if(abs(width)<=2.)then;call newpen2(2);elseif(abs(width)>2..and.abs(width)<=4.)then;call newpen2(3);else;call newpen2(4);end if
                        if(present(thicc))then
                            if(mod(n,thicc)==0)then
                                if(abs(width)<=2.)then;call newpen2(4);elseif(abs(width)>2..and.abs(width)<=4.)then;call newpen2(5);else;call newpen2(6);end if
                            end if
                        end if
                        call pscont3(dx,dy,another,mask,1,dim1,1,dim2,dim1,dim2,1,conti+continc*real(n),0.)
                    end do
                    call plot(-dx/2.,0.,-3)
                    deallocate(another)
                end if
            end if
            ! else;print*,'no contour'

            deallocate(r1,g1,b1)
        end subroutine
        ! can draw contours on 1 column arrays 
        subroutine butler_cont(array_2D,dim1,dim2,width,height,maskval,conti,continc,thicc,r,g,b,gap)
            implicit none
            integer,intent(in)::dim1,dim2
            real,intent(in)::maskval,conti,continc,width,height
            real,intent(in)::array_2D(:,:)
            integer,dimension(size(array_2D,1),size(array_2D,2))::mask
            real,dimension(:,:),allocatable::another
            integer,dimension(:,:),allocatable::anothermask
            real,intent(in),optional::r,g,b
            integer,intent(in),optional::thicc,gap
            real::dx,dy
            integer::i,j,n,contquan,zerocolumns=0,nonzerocol=0

            write(ounit,*)'%begin butler_cont'
            if(size(array_2D,1)<dim1 .or. size(array_2D,2)<dim2) then 
                print*,'Array size < dim1 or dim2';stop
            end if
            call box(width,height,3)
            do i = 1,dim1
                do j = 1, dim2
                    if(array_2D(i,j)== maskval) then;mask(i,j)=0
                    else;mask(i,j)=1
                    end if
                end do
            end do

            zerocolumns = 0
            do i = 1, dim1
                if(all(mask(i,1:dim2)==0).eqv..true.)then
                    zerocolumns = zerocolumns + 1
                else;nonzerocol = i
                end if
            end do

            if(nonzerocol==0)then;print*,'zero matrix';endif

            if(dim1-zerocolumns>1)then ! normal case
                if(.not.present(gap))then
                    dx = width/real(dim1-1);dy = height/real(dim2-1);call plot(-dx/2.,-dy/2.,-3)
                else;dx = width/real(dim1);dy = height/real(dim2);call plot(0.,-dy/2.,-3)
                end if
            else ! 1 column array
                if(.not.present(gap))then
                    dx = width/real(dim1);dy = height/real(dim2)
                else;dx = width/real(dim1)*real(dim1-1)/real(dim1);dy = height/real(dim2);call plot(width/real(dim1)/2.,0.,-3)
                end if
            end if

            contquan = int((maxval(array_2D)-conti)/continc+1)
            if(present(r).and.present(g).and.present(b))then;call rgbk(r,g,b);else;call rgbk(0.,0.,0.);end if
            if(dim1-zerocolumns>1)then
                do n = 0, contquan
                    if(abs(width)<=2.5)then;call newpen2(2);elseif(abs(width)>2.5.and.abs(width)<=4.)then;call newpen2(3);else;call newpen2(4);end if
                    if(present(thicc))then
                        if(mod(n,thicc)==0)then
                            if(abs(width)<=2.5)then;call newpen2(4);elseif(abs(width)>2.5.and.abs(width)<=4.)then;call newpen2(5);else;call newpen2(6);end if
                        end if
                    end if
                    call pscont3(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,1,conti+continc*real(n),0.)
                end do
            else
                if(dim1-zerocolumns==1)then
                    print*,'has only one nonzero column=',nonzerocol
                    allocate(another(dim1+1,dim2));allocate(anothermask(dim1+1,dim2))
                    another(1:dim1,1:dim2) = array_2D;another(nonzerocol+1,1:dim2) = another(nonzerocol,1:dim2)
                    anothermask(1:dim1,1:dim2)=mask;anothermask(nonzerocol+1,1:dim2) = anothermask(nonzerocol,1:dim2)
                    call plot(-dx/2.,0.,-3)
                    do n = 0,contquan
                        if(abs(width)<=2.5)then;call newpen2(2);elseif(abs(width)>2.5.and.abs(width)<=4.)then;call newpen2(3);else;call newpen2(4);end if
                        if(present(thicc))then
                            if(mod(n,thicc)==0)then
                                if(abs(width)<=2.5)then;call newpen2(4);elseif(abs(width)>2.5.and.abs(width)<=4.)then;call newpen2(5);else;call newpen2(6);end if
                            end if
                        end if
                        call pscont3(dx,dy,another,anothermask,1,dim1+1,1,dim2,dim1+1,dim2,1,conti+continc*real(n),0.)
                    end do
                    deallocate(another);deallocate(anothermask)
                    call plot(dx/2.,0.,-3)
                end if
            end if

            call rgbk(0.,0.,0.)
            if(dim1-zerocolumns>1)then
                if(.not.present(gap))then
                    call plot(dx/2.,dy/2.,-3)
                else;call plot(0.,dy/2.,-3)
                end if
            else
                if(.not.present(gap))then
                    ! call plot(-dx/2.,0.,-3)
                else;call plot(-width/real(dim1)/2.,0.,-3)
                end if
            end if
            write(ounit,*)'%end butler_cont'
            
        end subroutine
        ! paints areas within range with color given, other regions will not be painted. betcolork2
        subroutine butler_mask(array_2D,dim1,dim2,width,height,mask_ini,mask_fin,r,g,b,gap)
            implicit none
            integer,intent(in)::dim1,dim2
            integer,intent(in),optional::gap
            real,intent(in),optional::r,g,b
            real,intent(in)::width,height
            real,intent(in)::array_2D(:,:)
            real,intent(in)::mask_ini,mask_fin
            integer,dimension(size(array_2D,1),size(array_2D,2))::mask
            real::dx,dy,r1,g1,b1
            integer::i,j
            
            write(16,*)"% begin butler_mask"

            if(size(array_2D,1)<dim1 .or. size(array_2D,2)<dim2) then 
                print*,'Array size < dim1 or dim2';stop
            end if
            call box(width,height,3)
            if(.not.present(gap))then
                dx = width/real(dim1);dy = height/real(dim2)
            else;dx = width/real(dim1)*real(dim1-1)/real(dim1);dy = height/real(dim2);call plot(width/real(dim1)/2.,0.,-3)
            end if
            do i = 1, dim1
                do j = 1, dim2
                    if(mask_ini<=array_2D(i,j).and.array_2D(i,j)<=mask_fin) then;mask(i,j)=1
                    else;mask(i,j)=0
                    end if

                end do
            end do
            ! print*,mask

            if(present(r).and.present(g).and.present(b))then;r1=r;g1=g;b1=b;else;r1=0.;g1=0.;b1=0.;end if
            call betcolork2(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,-10.**(10.),10.**(10.),r1,g1,b1)
            if(present(gap))then;call plot(-width/real(dim1)/2.,0.,-3);else;end if
            write(16,*)"% end butler_mask"

        end subroutine
        ! for integer arrays
        subroutine butler_imask(array_2D,dim1,dim2,width,height,integer_in,r,g,b,gap)
            implicit none
            integer,intent(in)::dim1,dim2,integer_in
            integer,intent(in),optional::gap
            real,intent(in),optional::r,g,b
            real,intent(in)::width,height
            integer,intent(in)::array_2D(:,:)
            integer,dimension(size(array_2D,1),size(array_2D,2))::mask
            real::dx,dy,r1,g1,b1
            integer::i,j
            
            write(16,*)"% begin butler_mask"

            if(size(array_2D,1)<dim1 .or. size(array_2D,2)<dim2) then 
                print*,'Array size < dim1 or dim2';stop
            end if
            call box(width,height,3)
            if(.not.present(gap))then
                dx = width/real(dim1);dy = height/real(dim2)
            else;dx = width/real(dim1)*real(dim1-1)/real(dim1);dy = height/real(dim2);call plot(width/real(dim1)/2.,0.,-3)
            end if
            do i = 1, dim1
                do j = 1, dim2
                    if(array_2D(i,j)==integer_in) then;mask(i,j)=1
                    else;mask(i,j)=0
                    end if

                end do
            end do
            ! print*,mask

            if(present(r).and.present(g).and.present(b))then;r1=r;g1=g;b1=b;else;r1=0.;g1=0.;b1=0.;end if
            call betcolorI(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,integer_in,r1,g1,b1)
            if(present(gap))then;call plot(-width/real(dim1)/2.,0.,-3);else;end if
            write(16,*)"% end butler_mask"

        end subroutine
        ! paints areas within range with color given, other regions will not be painted. pscolork
        subroutine butler_psmask(array_2D,dim1,dim2,width,height,mask_ini,mask_fin,r,g,b,gap)
            implicit none
            integer,intent(in)::dim1,dim2
            integer,intent(in),optional::gap
            real,intent(in),optional::r,g,b
            real,intent(in)::width,height
            real,intent(in)::array_2D(:,:)
            real,intent(in)::mask_ini,mask_fin
            integer,dimension(size(array_2D,1),size(array_2D,2))::mask
            real,dimension(:,:),allocatable::another
            integer,dimension(:,:),allocatable::anothermask
            real::dx,dy,r1,g1,b1
            integer::i,j,zerocolumns,nonzerocol
            
            write(16,*)"% begin butler_psmask"
            call newpen2(3)
            if(size(array_2D,1)<dim1 .or. size(array_2D,2)<dim2) then 
                print*,'Array size < dim1 or dim2';stop
            end if
            call box(width,height,3)

            do i = 1, dim1
                do j = 1, dim2
                    if(array_2D(i,j)/=0..and.array_2D(i,j)/=0.) then;mask(i,j)=1
                    else;mask(i,j)=0
                    end if

                end do
            end do

            zerocolumns = 0
            do i = 1, dim1
                if(all(mask(i,1:dim2)==0).eqv..true.)then
                    zerocolumns = zerocolumns + 1
                else;nonzerocol = i
                end if
            end do
            if(nonzerocol==0)then;print*,'zero matrix';return;endif

            if(dim1-zerocolumns>1)then
                if(.not.present(gap))then
                    dx = width/real(dim1-1);dy = height/real(dim2-1);call plot(-dx/2.,-dy/2.,-3)
                else;dx = width/real(dim1);dy = height/real(dim2);call plot(0.,-dy/2.,-3)
                end if
            else
                if(.not.present(gap))then
                    dx = width/real(dim1);dy = height/real(dim2)
                else;dx = width/real(dim1)*real(dim1-1)/real(dim1);dy = height/real(dim2);call plot(width/real(dim1)/2.,0.,-3)
                end if
            end if


            if(present(r).and.present(g).and.present(b))then;r1=r;g1=g;b1=b;else;r1=0.;g1=0.;b1=0.;end if
            if(dim1-zerocolumns>1)then
                call pscolork(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,mask_ini,mask_fin,r1,g1,b1)
            else 
                allocate(another(dim1+1,dim2));allocate(anothermask(dim1+1,dim2))
                another(1:dim1,1:dim2) = array_2D;another(nonzerocol+1,1:dim2) = another(nonzerocol,1:dim2)
                anothermask(1:dim1,1:dim2)=mask;anothermask(nonzerocol+1,1:dim2) = anothermask(nonzerocol,1:dim2)
                call plot(-dx/2.,0.,-3)
                call pscolork(dx,dy,another,anothermask,1,dim1+1,1,dim2,dim1+1,dim2,mask_ini,mask_fin,r1,g1,b1)
                deallocate(another);deallocate(anothermask)
                call plot(dx/2.,0.,-3)
            end if


            if(dim1-zerocolumns>1)then
                if(.not.present(gap))then
                    call plot(dx/2.,dy/2.,-3)
                else;call plot(0.,dy/2.,-3)
                end if
            else
                if(.not.present(gap))then
                    ! call plot(-dx/2.,0.,-3)
                else;call plot(-width/real(dim1)/2.,0.,-3)
                end if
            end if
            write(16,*)"% end butler_psmask"
        end subroutine
    ! ps bois
    ! RANDOM 
        
end module subroutines

module functions
    implicit none
    contains
    function minex0(D1, D2, D3, D4, D5, D6) result(min_val)
        implicit none
        real, dimension(:), intent(in), optional :: D1
        real, dimension(:,:), intent(in), optional :: D2
        real, dimension(:,:,:), intent(in), optional :: D3
        real, dimension(:,:,:,:), intent(in), optional :: D4
        real, dimension(:,:,:,:,:), intent(in), optional :: D5
        real, dimension(:,:,:,:,:,:), intent(in), optional :: D6
        real, dimension(:), allocatable :: array1
        real, dimension(:,:), allocatable :: array2
        real, dimension(:,:,:), allocatable :: array3
        real, dimension(:,:,:,:), allocatable :: array4
        real, dimension(:,:,:,:,:), allocatable :: array5
        real, dimension(:,:,:,:,:,:), allocatable :: array6
        real :: min_val
        integer :: n, l, m, o, p, q
    
        if (present(D1)) then
            allocate(array1(size(D1)))
            do n = 1, size(D1)
                if (D1(n) /= 0.0) then
                    array1(n) = D1(n)
                else
                    array1(n) = 10.0**10.0
                end if
            end do
            min_val = minval(array1)
            deallocate(array1)
    
        else if (present(D2)) then
            allocate(array2(size(D2, 1), size(D2, 2)))
            do n = 1, size(D2, 1)
                do l = 1, size(D2, 2)
                    if (D2(n, l) /= 0.0) then
                        array2(n, l) = D2(n, l)
                    else
                        array2(n, l) = 10.0**10.0
                    end if
                end do
            end do
            min_val = minval(array2)
            deallocate(array2)
    
        else if (present(D3)) then
            allocate(array3(size(D3, 1), size(D3, 2), size(D3, 3)))
            do n = 1, size(D3, 1)
                do l = 1, size(D3, 2)
                    do m = 1, size(D3, 3)
                        if (D3(n, l, m) /= 0.0) then
                            array3(n, l, m) = D3(n, l, m)
                        else
                            array3(n, l, m) = 10.0**10.0
                        end if
                    end do
                end do
            end do
            min_val = minval(array3)
            deallocate(array3)
    
        else if (present(D4)) then
            allocate(array4(size(D4, 1), size(D4, 2), size(D4, 3), size(D4, 4)))
            do n = 1, size(D4, 1)
                do l = 1, size(D4, 2)
                    do m = 1, size(D4, 3)
                        do o = 1, size(D4, 4)
                            if (D4(n, l, m, o) /= 0.0) then
                                array4(n, l, m, o) = D4(n, l, m, o)
                            else
                                array4(n, l, m, o) = 10.0**10.0
                            end if
                        end do
                    end do
                end do
            end do
            min_val = minval(array4)
            deallocate(array4)
    
        else if (present(D5)) then
            allocate(array5(size(D5, 1), size(D5, 2), size(D5, 3), size(D5, 4), size(D5, 5)))
            do n = 1, size(D5, 1)
                do l = 1, size(D5, 2)
                    do m = 1, size(D5, 3)
                        do o = 1, size(D5, 4)
                            do p = 1, size(D5, 5)
                                if (D5(n, l, m, o, p) /= 0.0) then
                                    array5(n, l, m, o, p) = D5(n, l, m, o, p)
                                else
                                    array5(n, l, m, o, p) = 10.0**10.0
                                end if
                            end do
                        end do
                    end do
                end do
            end do
            min_val = minval(array5)
            deallocate(array5)
    
        else if (present(D6)) then
            allocate(array6(size(D6, 1), size(D6, 2), size(D6, 3), size(D6, 4), size(D6, 5), size(D6, 6)))
            do n = 1, size(D6, 1)
                do l = 1, size(D6, 2)
                    do m = 1, size(D6, 3)
                        do o = 1, size(D6, 4)
                            do p = 1, size(D6, 5)
                                do q = 1, size(D6, 6)
                                    if (D6(n, l, m, o, p, q) /= 0.0) then
                                        array6(n, l, m, o, p, q) = D6(n, l, m, o, p, q)
                                    else
                                        array6(n, l, m, o, p, q) = 10.0**10.0
                                    end if
                                end do
                            end do
                        end do
                    end do
                end do
            end do
            min_val = minval(array6)
            deallocate(array6)
    
        else
            print *, 'no, or invalid input'
            stop
        end if
    end function minex0

    function f_t95(df) result(t95coeff)
        implicit none
        integer,intent(in)::df
        real::t95coeff
        real,dimension(0:30)::t95

        t95(1) = 12.706 ; t95(11) = 2.2010 ; t95(21) = 2.0796
        t95(2) = 4.3026 ; t95(12) = 2.1788 ; t95(22) = 2.0739
        t95(3) = 3.1824 ; t95(13) = 2.1604 ; t95(23) = 2.0687
        t95(4) = 2.7765 ; t95(14) = 2.1448 ; t95(24) = 2.0639
        t95(5) = 2.5706 ; t95(15) = 2.1315 ; t95(25) = 2.0595
        t95(6) = 2.4469 ; t95(16) = 2.1191 ; t95(26) = 2.0555
        t95(7) = 2.3646 ; t95(17) = 2.1098 ; t95(27) = 2.0518
        t95(8) = 2.3060 ; t95(18) = 2.1009 ; t95(28) = 2.0484
        t95(9) = 2.2621 ; t95(19) = 2.0930 ; t95(29) = 2.0452
        t95(10) = 2.2281 ;t95(20) = 2.0860 ; t95(30) = 2.0423

        t95(0) = 0. !just for the sake of programs

        if(df>=0 .and. df<=30) then
            t95coeff = t95(df)
        else
            t95coeff = 911; print*, 'df out of range'
        end if
    end function f_t95
    function f_t90(df) result(t90coeff)
        implicit none
        integer,intent(in)::df
        real::t90coeff
        real,dimension(0:30)::t90

        t90(1) = 6.3138 ; t90(11) = 1.7959 ; t90(21) = 1.7210
        t90(2) = 2.9200 ; t90(12) = 1.7823 ; t90(22) = 1.7171
        t90(3) = 2.3534 ; t90(13) = 1.7709 ; t90(23) = 1.7139
        t90(4) = 2.1318 ; t90(14) = 1.7613 ; t90(24) = 1.7109
        t90(5) = 2.0150 ; t90(15) = 1.7531 ; t90(25) = 1.7081
        t90(6) = 1.9432 ; t90(16) = 1.7459 ; t90(26) = 1.7056
        t90(7) = 1.8946 ; t90(17) = 1.7396 ; t90(27) = 1.7033
        t90(8) = 1.8595 ; t90(18) = 1.7341 ; t90(28) = 1.7011
        t90(9) = 1.8331 ; t90(19) = 1.7291 ; t90(29) = 1.6991
        t90(10) = 1.8125 ; t90(20) = 1.7250 ; t90(30) = 1.6973

        t90(0) = 0. !just for the sake of programs

        if(df>=0 .and. df<=30) then
            t90coeff = t90(df)
        else
            t90coeff = 911; print*, 'df out of range'
        end if
    end function f_t90

    function fwelcht(mean1, s1, dataquan1, mean2, s2, dataquan2) result(result)
        implicit none
        real, intent(in) :: mean1, s1, mean2, s2
        integer, intent(in) :: dataquan1, dataquan2
        integer :: result
        ! real, dimension(0:30) :: t_95 = 0.0
        real :: diff_mean, n1, n2, df, sem, bottomCI, topCI
    
        if (mean1 /= 0.0 .and. mean2 /= 0.0 .and. dataquan1 /= 0 .and. dataquan2 /= 0) then
            diff_mean = mean1 - mean2
            n1 = real(dataquan1)
            n2 = real(dataquan2)
            sem = sqrt((s1**2.0 / n1) + (s2**2.0 / n2))
            df = (((s1**2.0) / n1) + ((s2**2.0) / n2))**2.0 / (((s1**2.0 / n1)**2.0 / (n1 - 1)) + ((s2**2.0 / n2)**2.0 / (n2 - 1)))
            ! call t95_value(t_95)
            bottomCI = diff_mean - f_t95(int(df)) * sem
            topCI = diff_mean + f_t95(int(df)) * sem
            ! print*, diff_mean, bottomCI, topCI, int(df)
            if (bottomCI > 0.0) then
                result = 1 ! larger
            else if (topCI < 0.0) then
                result = -1 ! smaller
            else
                result = 0 ! no difference in the desired level
            end if
        else
            result = 911 ! error
        end if
    
    end function fwelcht
    function fwelcht90(mean1, s1, dataquan1, mean2, s2, dataquan2) result(result)
        implicit none
        real, intent(in) :: mean1, s1, mean2, s2
        integer, intent(in) :: dataquan1, dataquan2
        integer :: result
        ! real, dimension(0:30) :: t_90 = 0.0
        real :: diff_mean, n1, n2, df, sem, bottomCI, topCI

        if (mean1 /= 0.0 .and. mean2 /= 0.0 .and. dataquan1 /= 0 .and. dataquan2 /= 0) then
            diff_mean = mean1 - mean2
            n1 = real(dataquan1)
            n2 = real(dataquan2)
            sem = sqrt((s1**2.0 / n1) + (s2**2.0 / n2))
            df = (((s1**2.0) / n1) + ((s2**2.0) / n2))**2.0 / (((s1**2.0 / n1)**2.0 / (n1 - 1)) + ((s2**2.0 / n2)**2.0 / (n2 - 1)))
            ! call t90_value(t_90)
            bottomCI = diff_mean - f_t90(int(df)) * sem
            topCI = diff_mean + f_t90(int(df)) * sem
            ! print*, diff_mean, bottomCI, topCI, int(df)
            if (bottomCI > 0.0) then
                result = 1 ! larger
            else if (topCI < 0.0) then
                result = -1 ! smaller
            else
                result = 0 ! no difference in the desired level
            end if
        else
            result = 911 ! error
        end if
    end function fwelcht90

end module functions

module constants
    implicit none
    integer, parameter :: years = 15, months = 12, lines = 2, stations = 9, depth = 400
    real,dimension(years,months,lines,stations,depth):: temp_5=0.,potemp_5=0.,sal_5=0.,sigma_5=0.,potemp_c5=0.,sal_c5=0.,sigma_c5=0.,geovel_5=0.
    real,dimension(:),allocatable::r,g,b,r1,g1,b1,r2,g2,b2,r3,g3,b3,r4,g4,b4,r5,g5,b5,r6,g6,b6
    character(len=4),dimension(12)::month_names = (/'Jan.','Feb.','Mar.','Apr.','May ','Jun.','Jul.','Aug.','Sep.','Oct.','Nov.','Dec.'/)
    integer::y,m,l,st,d,i,j,k,n,x,z,h
    real::dx,dy,a,c,e,f,q,s
end module constants

module always
    use subroutines
    use constants
    use functions
end module

module test
    use constants
    contains
    subroutine printl()
        print*,l
        l = l +1
    end subroutine
end module
