! plots (init-x, init-y, fileno,psfilename ) 

! USES MODULES INSTEAD OF COMMON BLOCKS


      subroutine plots(x,y,mode,psfile)
        use psstat
        character psfile*(*)
        character*24 date
        character*20 head
        character(len=8) :: date_str, time_str
        integer :: values(8)
        ! logical stoff,land,pageend
        ! common /psstat/ipage,stoff,land,xorig,yorig,pageend
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
  
        write(ounit,'(a)') "%!PS-Adobe-2.1"
        write(ounit,'(a)') "%%Creator: PS-LibraryV2 97.08.08" 
        ! write(ounit,'(a)') "%%Creator: Yuta Nishimori" 
  
        write(ounit,'(a)') "%%Copyright: PS-LibraryV2  Fukuda & Saito" 
        write(ounit,'(2a)') "%%CreationDate: ",date
        write(ounit,'(2a)') "%%Title: ",trim(head)
        
        write(ounit,'(a)') "%%Pages: (atend)"
        write(ounit,'(a)') "%%BoundingBox: 0 0 612 792 "
        write(ounit,'(a)') "%%EndComments: AS CHILDREN OF THE REPUBLIC"
        write(ounit,'(a)') "%%BeginPlolog"
        write(ounit,'(a)') "%%%PS-LibraryV2(Spring-Tulip) define start %%%"
        write(ounit,'(3a)') "/head ( ",trim(head)," ) def" 
  
        write(ounit,'(a)') "/np  { newpath } def"
        write(ounit,'(a)') "/mv  { moveto } def"
        write(ounit,'(a)') "/ln  { lineto } def"
        write(ounit,'(a)') "/sh  { show } def"
        write(ounit,'(a)') "/sl  { setlinewidth } def"
        write(ounit,'(a)') "/sn  { stroke newpath } def" 
        write(ounit,'(a)') "/fo  { /Helvetica-Bold  findfont } def"
        write(ounit,'(a)') "/kfo { /GothicBBB-Medium-H  findfont } def"
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
  
  ! newpage      c
              subroutine newpage
              use psstat
            !   logical stoff,land
            !   common /psstat/ipage,stoff,land,xorig,yorig,pageend
        
              call endpage
              call inipage      
        
              call plot(0.7,0.7,-3)
           
              return
              end
  ! plote   
  
        subroutine plote
            use psstat
        ! logical stoff,land,pageend
        ! common /psstat/ipage,stoff,land,xorig,yorig,pageend
        call endpage
        write(ounit,'(a)') "%%Trailer"
        write(ounit,'(a,i3)') "%%Pages: ",ipage
        write(ounit,'(a,i3)') "%%EOF"
        close(ounit)
        return
        end
  
  
  ! plot(x,y,ip)             c
        subroutine plot(x1,y1,im)
            use psstat
            use qbase
        ! common /qbase/qcxp,qcyp,ip
        ! real qcxp,qcyp
        ! integer ip
        ip = im
        qcxp = x1
        qcyp = y1
        if(im.eq.3) then 
           write(ounit, '(a)') " sn"
           write(ounit, '(f9.4,2x,f9.4,2x,a3)') x1,y1," mv"
        elseif(im.eq.2) then
           write(ounit, '(f9.4,2x,f9.4,2x,a3)') x1,y1," ln"
        endif
        if(im.eq.-3) then
           write(ounit, '(f9.4,2x,f9.4,2x,a3)') x1,y1," tl"
           write(ounit, '(a)') " sn"
           write(ounit, '(a)') " 0.0 0.0 mv"
        endif
        return
        end
  
  ! call inipage       c
              subroutine inipage
                use psstat
            !   logical stoff,land,pageend
            !   common /psstat/ipage,stoff,land,xorig,yorig,pageend
        
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
        
              return
              end
        
  ! call endpage       c
  subroutine endpage
    use psstat
    !  logical stoff,land,pageend
    !  common /psstat/ipage,stoff,land,xorig,yorig,pageend
                 
     if(pageend) return
     write(ounit,'(a)') "PslEndPage"
     pageend=.true.
                 
     return
  end
  subroutine rgbK(red,gre,blu)     
   use psstat         
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
   use psstat
         dimension x(n),y(n)
         real red,gre,blu
         write(ounit,*) 'newpath'
         call rgbK(red,gre,blu) 
         call newpen2(1)
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
   use psstat           
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
   use psstat
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
   use psstat
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
   use psstat
    use qbase
    !  common /qbase/qcxp,qcyp,ip
    !  real qcxp,qcyp
    !  integer ip
     character*8 marka(0:13)
     real xp,yp,hi,hi2
     integer markty
     data marka / "        " , "dot     ",&
                  "plus    " , "star    ","circle  ","X       ", &
                  "tranf   ",  "tran    ","boxf    ","box     ", &
                  "circle2 ",  "        ","        ","        " /
     ! c     mark type 
     qcxp = xp
     qcyp = yp
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
   use psstat
  
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
   use psstat
     write(ounit, '(2f9.4,2x,a4)') fct,fct , " sc "
     return
  end 
  subroutine number(x,y,h,anu,ang,n)
   use psstat
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
   use psstat
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
   use psstat
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
  
  subroutine betsqK(xlef,ylef,xrig,yrig,rr,gg,bb)
   use psstat
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
  
  subroutine symbol(x,y,h,isymb,ang,n)
   use psstat
     ! c     character isymb*256,ica*256,ich(256)*
           character isymb*(*)
     ! c     equivalence (ica,ich)
     ! c     ica=isymb 
           write(ounit,*) "% begin symbol"
           write(ounit,*) "fo"
           write(ounit,10) h
      10   format(f8.4," sf")
           write(ounit,*) "se"
           if(x.ge.9999.0.or.y.ge.9999.0)then
           else
              write(ounit,'(2f10.4,2x,a3)' ) x,y, " mv" 
           endif 
           write(ounit,'(f9.4,2x,a4)' ) ang , ' ro '  
           write(ounit,'(3a)') "(",isymb(1:n),") sh" 
           write(ounit,'(f9.4,2x,a4)' ) -ang , ' ro ' 
           write(ounit,*) "% end symbol"
           return
  end
  
  subroutine symbolc(x,y,h,isymb,ang,n)
   use psstat
     ! c     character isymb*256,ica*256,ich(256)*
           character isymb*(*)
     ! c     equivalence (ica,ich)
     ! c     ica=isymb 
           write(ounit,*) "% begin symbolc"
           write(ounit,*) "fo"
           write(ounit,10) h
      10   format(f8.4," sf")
           write(ounit,*) "se"
           if(x.ge.9999.0.or.y.ge.9999.0)then
           else
              write(ounit,'(2f10.4,2x,a3)' ) x,y, " mv" 
           endif 
           write(ounit,'(f9.4,2x,a4)' ) ang , ' ro '  
           write(ounit,*) "(",isymb(1:n),") stringwidth pop " 
           write(ounit,*) 'neg 2 div 0 rmoveto '
           write(ounit,'(3a)') "(",isymb(1:n),") sh" 
           write(ounit,'(f9.4,2x,a4)' ) -ang , ' ro ' 
           write(ounit,*) "% end symbolc"
           return
  end
           
  subroutine symbolr(x,y,h,isymb,ang,n)
   use psstat
     ! c     character isymb*256,ica*256,ich(256)*
        character isymb*(*)
     ! c     equivalence (ica,ich)
     ! c     ica=isymb 
        write(ounit,*) "% begin symbolr"
        write(ounit,*) "fo"
        write(ounit,10) h
   10   format(f8.4," sf")
        write(ounit,*) "se"
        if(x.ge.9999.0.or.y.ge.9999.0)then
        else
           write(ounit,'(2f10.4,2x,a3)' ) x,y, " mv" 
        endif 
        write(ounit,'(f9.4,2x,a4)' ) ang , ' ro '  
        write(ounit,*) "(",isymb(1:n),") stringwidth pop " 
        write(ounit,*) 'neg 0 rmoveto '
        write(ounit,'(3a)') "(",isymb(1:n),") sh" 
        write(ounit,'(f9.4,2x,a4)' ) -ang , ' ro ' 
        write(ounit,*) "% end symbolr"
        return
  end
