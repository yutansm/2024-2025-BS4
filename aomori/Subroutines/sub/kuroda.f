c2345678
***********************************************************************
      subroutine pscolorK(px,py,ss,is,ist,ied,jst,jed,ix,iy,
     &                                     cst,ced,rr,gg,bb)
***********************************************************************
*     cst <==> ced   : Range of painting 
*    (RR,gg,bb)      : RGB color   
      Parameter(nx=20)
        dimension ss(ix,iy),is(ix,iy)
        dimension x(nx),y(nx)


*###################################################################
*################### ZERO STEP  ####################################
*###################################################################

      do i=ist,ied-1 
        lp = 0
      do j=jst,jed-1  

**************************************************************
        IF(is( i , j ).ne.0.and.is(i+1, j ).ne.0 .and.
     &     is( i ,j+1).ne.0.and.is(i+1,j+1).ne.0      ) THEN
**************************************************************

                   s1 = ss( i , j )
                   s2 = ss(i+1, j )
                   s3 = ss( i ,j+1)
                   s4 = ss(i+1,j+1)

*#(1-1)#*-----------
          if(((s1.ge.cst).and.(s1.le.ced)).and.
     &       ((s2.ge.cst).and.(s2.le.ced)).and.
     &       ((s3.ge.cst).and.(s3.le.ced)).and.
     &       ((s4.ge.cst).and.(s4.le.ced))
     &                   .and.lp.eq.0.and.j.ne.jed-1) then 
                lp = 1
                xx = px*(i-ist) + px/2.0
                yy = py*(j-jst) + py/2.0
              xlef = xx
              ylef = yy

                      is( i , j ) = 2  
                      is(i+1, j ) = 2  
                      is( i ,j+1) = 2  
                      is(i+1,j+1) = 2  

*#(1-2)#*-----------
            elseif((s1.ge.cst.and.s1.le.ced.and.
     &              s2.ge.cst.and.s2.le.ced.and.
     &              s3.ge.cst.and.s3.le.ced.and.
     &              s4.ge.cst.and.s4.le.ced)   
     &                       .and.lp.eq.1.and.j.ne.jed-1) then 

                      is( i , j ) = 2  
                      is(i+1, j ) = 2  
                      is( i ,j+1) = 2  
                      is(i+1,j+1) = 2  


*#(1-3)#*-----------
            elseif((s1.lt.cst .or. s1.gt.ced  .or.
     &              s2.lt.cst .or. s2.gt.ced  .or.
     &              s3.lt.cst .or. s3.gt.ced  .or.
     &              s4.lt.cst .or. s4.gt.ced)    .and.lp.eq.1) then
                lp = 0
                xx = px*(i+1 - ist) + px/2.0
                yy = py*( j  - jst) + py/2.0
              xrig = xx
              yrig = yy
                   call betsqK(xlef,ylef,xrig,yrig,rr,gg,bb)


*#(1-4)#*-----------
            elseif((s1.ge.cst.and.s1.le.ced.and.
     &              s2.ge.cst.and.s2.le.ced.and.
     &              s3.ge.cst.and.s3.le.ced.and.
     &              s4.ge.cst.and.s4.le.ced) 
     &                       .and.lp.eq.1.and.j.eq.jed-1) then 

                      is( i , j ) = 2  
                      is(i+1, j ) = 2  
                      is( i ,j+1) = 2  
                      is(i+1,j+1) = 2  

                xx = px*(i+1 - ist) + px/2.0
                yy = py*(j+1 - jst) + py/2.0
              xrig = xx
              yrig = yy
                   call betsqK(xlef,ylef,xrig,yrig,rr,gg,bb)
                  


*#(1-5)#*-----------
            elseif((s1.ge.cst.and.s1.le.ced.and.
     &              s2.ge.cst.and.s2.le.ced.and.
     &              s3.ge.cst.and.s3.le.ced.and.
     &              s4.ge.cst.and.s4.le.ced)   
     &                       .and.lp.eq.0.and.j.eq.jed-1) then 
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
*****************
      ELSE
*****************
            if(lp.eq.1) then 
                lp = 0
                xx = px*(i+1 - ist) + px/2.0
                yy = py*( j  - jst) + py/2.0
              xrig = xx
              yrig = yy
                call betsqK(xlef,ylef,xrig,yrig,rr,gg,bb)
            endif 

********************
      ENDIF
********************
      enddo
      enddo


*###################################################################
*################### FIRST STEP ####################################
*###################################################################
      do 20 j=jst,jed-1 
      do 10 i=ist,ied-1  
       if(is(i  ,j  ).eq.0) goto 10 
       if(is(i+1,j  ).eq.0) goto 10 
       if(is(i  ,j+1).eq.0) goto 10 
       if(is(i+1,j+1).eq.0) goto 10 

************************************************************
       if(is( i , j ).eq.2 .and. is(i+1, j ).eq.2  .and.
     &    is( i ,j+1).eq.2 .and. is(i+1,j+1).eq.2 ) goto 10 
************************************************************

          s1 = ss( i , j )
          s2 = ss(i+1, j )
          s3 = ss( i ,j+1)
          s4 = ss(i+1,j+1)
            xlef = real( i -ist)*px + px/2.0 
            ylef = real( j -jst)*py + py/2.0
            xrig = real(i+1-ist)*px + px/2.0 
            yrig = real(j+1-jst)*py + py/2.0
*       ------------------------------------------------------------
          if(((s1.ge.cst).and.(s1.le.ced)).and.
     &       ((s2.ge.cst).and.(s2.le.ced)).and.
     &       ((s3.ge.cst).and.(s3.le.ced)).and.
     &       ((s4.ge.cst).and.(s4.le.ced))) then 
               x(1) = xlef
               x(2) = xlef 
               x(3) = xrig
               x(4) = xrig 
               y(1) = ylef
               y(2) = yrig 
               y(3) = yrig 
               y(4) = ylef
           call betmlK(x,y,4,nx,rr,gg,bb)
*       ------------------------------------------------------------
         elseif(((s1.ge.cst).and.(s1.le.ced)).and.
     &          ((s2.ge.cst).and.(s2.le.ced)).and.
     &          ((s3.ge.cst).and.(s3.le.ced)).and.
     &          ((s4.lt.cst).or .(s4.gt.ced))) then 
*((((1))))
         dxa = abs(s4-s3)
          if(s4.gt.ced) then 
           ddx = abs(s3 - ced)
          else
           ddx = abs(s3 - cst)
          endif 
         x(3) = xlef + (ddx/dxa)*px 
*((((2)))) 
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
*       -------------------------------------------------------------
         elseif(((s1.ge.cst).and.(s1.le.ced)).and.
     &          ((s2.ge.cst).and.(s2.le.ced)).and.
     &          ((s3.lt.cst).or .(s3.gt.ced)).and.
     &          ((s4.ge.cst).and.(s4.le.ced))) then 
*((((1)))) 
        dxa = abs(s3-s1)
         if(s3.gt.ced) then 
           ddx = abs(s1 - ced)
         else
           ddx = abs(s1 - cst)
         endif 
        y(2) = ylef + (ddx/dxa)*py 
*((((2)))) 
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
*       -------------------------------------------------------------
         elseif(((s1.ge.cst).and.(s1.le.ced)).and.
     &          ((s2.lt.cst).or .(s2.gt.ced)).and.
     &          ((s3.ge.cst).and.(s3.le.ced)).and.
     &          ((s4.ge.cst).and.(s4.le.ced))) then 
*((((1)))) 
         dxa = abs(s2-s1)
          if(s2.gt.ced) then 
           ddx = abs(s1 - ced)
          else
           ddx = abs(s1 - cst)
          endif 
         x(5) = xlef + (ddx/dxa)*px 
*((((2)))) 
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
*       -------------------------------------------------------------
          elseif(((s1.lt.cst).or .(s1.gt.ced)).and.
     &           ((s2.ge.cst).and.(s2.le.ced)).and.
     &           ((s3.ge.cst).and.(s3.le.ced)).and.
     &           ((s4.ge.cst).and.(s4.le.ced))) then 
*((((1)))) 
         dxa = abs(s3-s1)
          if(s1.gt.ced) then 
           ddx = abs(s1 - ced)
          else
           ddx = abs(s1 - cst)
          endif 
         y(1) = ylef  + (ddx/dxa)*py 
*((((2)))) 
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
*       -------------------------------------------------------------
         elseif(((s1.ge.cst).and.(s1.le.ced)).and.
     &           ((s2.ge.cst).and.(s2.le.ced)).and.
     &           ((s3.lt.cst).or .(s3.gt.ced)).and.
     &           ((s4.lt.cst).or .(s4.gt.ced))) then 
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
*       -------------------------------------------------------------
         elseif(((s1.ge.cst).and.(s1.le.ced)).and.
     &           ((s2.lt.cst).or .(s2.gt.ced)).and.
     &           ((s3.lt.cst).or .(s3.gt.ced)).and.
     &           ((s4.ge.cst).and.(s4.le.ced))) then 
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
***POST***
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
***POST***
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
*       -------------------------------------------------------------
          elseif(((s1.lt.cst).or .(s1.gt.ced)).and.
     &           ((s2.ge.cst).and.(s2.le.ced)).and.
     &           ((s3.lt.cst).or .(s3.gt.ced)).and.
     &           ((s4.ge.cst).and.(s4.le.ced))) then 
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
*       -------------------------------------------------------------
          elseif(((s1.lt.cst).or .(s1.gt.ced)).and.
     &           ((s2.lt.cst).or .(s2.gt.ced)).and.
     &           ((s3.ge.cst).and.(s3.le.ced)).and.
     &           ((s4.ge.cst).and.(s4.le.ced))) then 
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
*       -------------------------------------------------------------
          elseif(((s1.lt.cst).or .(s1.gt.ced)).and.
     &           ((s2.ge.cst).and.(s2.le.ced)).and.
     &           ((s3.ge.cst).and.(s3.le.ced)).and.
     &           ((s4.lt.cst).or .(s4.gt.ced))) then 
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
***POST***
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
***POST***
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
*       -------------------------------------------------------------
          elseif(((s1.ge.cst).and.(s1.le.ced)).and.
     &           ((s2.lt.cst).or .(s2.gt.ced)).and.
     &           ((s3.ge.cst).and.(s3.le.ced)).and.
     &           ((s4.lt.cst).or .(s4.gt.ced))) then 
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
*       -------------------------------------------------------------
          elseif(((s1.ge.cst).and.(s1.le.ced)).and.
     &           ((s2.lt.cst).or .(s2.gt.ced)).and.
     &           ((s3.lt.cst).or .(s3.gt.ced)).and.
     &           ((s4.lt.cst).or .(s4.gt.ced))) then 
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
***POST***
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
***POST***
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
*       -------------------------------------------------------------
          elseif(((s1.lt.cst).or .(s1.gt.ced)).and.
     &           ((s2.ge.cst).and.(s2.le.ced)).and.
     &           ((s3.lt.cst).or .(s3.gt.ced)).and.
     &           ((s4.lt.cst).or .(s4.gt.ced))) then 
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
***POST***
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
***POST***
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
*       -------------------------------------------------------------
          elseif(((s1.lt.cst).or .(s1.gt.ced)).and.
     &           ((s2.lt.cst).or .(s2.gt.ced)).and.
     &           ((s3.ge.cst).and.(s3.le.ced)).and.
     &           ((s4.lt.cst).or .(s4.gt.ced))) then 
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
***POST***
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
***POST***
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
*       -------------------------------------------------------------
          elseif(((s1.lt.cst).or .(s1.gt.ced)).and.
     &           ((s2.lt.cst).or .(s2.gt.ced)).and.
     &           ((s3.lt.cst).or .(s3.gt.ced)).and.
     &           ((s4.ge.cst).and.(s4.le.ced))) then 
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
***POST***
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
***POST***
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
          elseif(((s1.lt.cst).or .(s1.gt.ced)).and.
     &           ((s2.lt.cst).or .(s2.gt.ced)).and.
     &           ((s3.lt.cst).or .(s3.gt.ced)).and.
     &           ((s4.lt.cst).or .(s4.gt.ced))) then 
          if((s1.gt.ced).and.(s2.gt.ced).and.
     &       (s3.lt.cst).and.(s4.gt.ced)) then 
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
          elseif((s1.gt.ced).and.(s2.gt.ced).and.
     &           (s3.gt.ced).and.(s4.lt.cst)) then 
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
          elseif((s1.lt.cst).and.(s2.gt.ced).and.
     &           (s3.gt.ced).and.(s4.gt.ced)) then 
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
          elseif((s1.gt.ced).and.(s2.lt.cst).and.
     &           (s3.gt.ced).and.(s4.gt.ced)) then 
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
          elseif((s1.lt.cst).and.(s2.lt.cst).and.
     &           (s3.gt.ced).and.(s4.gt.ced)) then 
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
          elseif((s1.gt.ced).and.(s2.lt.cst).and.
     &           (s3.gt.ced).and.(s4.lt.cst)) then 
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
          elseif((s1.gt.ced).and.(s2.gt.ced).and.
     &           (s3.lt.cst).and.(s4.lt.cst)) then 
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
          elseif((s1.lt.cst).and.(s2.gt.ced).and.
     &           (s3.lt.cst).and.(s4.gt.ced)) then 
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
          elseif((s1.lt.cst).and.(s2.gt.ced).and.
     &           (s3.lt.cst).and.(s4.lt.cst)) then 
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
          elseif((s1.gt.ced).and.(s2.lt.cst).and.
     &           (s3.lt.cst).and.(s4.lt.cst)) then 
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
          elseif((s1.lt.cst).and.(s2.lt.cst).and.
     &           (s3.lt.cst).and.(s4.gt.ced)) then 
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
          elseif((s1.lt.cst).and.(s2.lt.cst).and.
     &           (s3.gt.ced).and.(s4.lt.cst)) then 
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
          elseif((s1.lt.cst).and.(s2.gt.ced).and.
     &           (s3.gt.ced).and.(s4.lt.cst)) then 
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
            
cc             engu3 = (x01u-x03u)**2 + (y01u-y03u)**2 
cc             engu4 = (x02u-x03u)**2 + (y02u-y03u)**2 
cc             engu5 = (x02u-x04u)**2 + (y02u-y04u)**2 
cc             engu6 = (x03u-x04u)**2 + (y03u-y04u)**2 

cc             engd3 = (x01d-x03d)**2 + (y01d-y03d)**2 
cc             engd4 = (x02d-x03d)**2 + (y02d-y03d)**2 
cc             engd5 = (x02d-x04d)**2 + (y02d-y04d)**2 
cc             engd6 = (x03d-x04d)**2 + (y03d-y04d)**2 

cc            write(*,*) 'u' 
cc            write(*,*) engu1,engu2,engu3 
cc            write(*,*) engu4,engu5,engu6 
cc            write(*,*) 'd'
cc            write(*,*) engd1,engd2,engd3 
cc            write(*,*) engd4,engd5,engd6 
***POST***
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
          elseif((s1.gt.ced).and.(s2.lt.cst).and.
     &           (s3.lt.cst).and.(s4.gt.ced)) then 
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

***POST***
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
       end 



     
c
c     Programed by Hiroshi Kuroda (2003/02/22)
c
***********************************************************************
      subroutine pscmaskK(px,py,imask,is,ie,js,je,ix,iy,r,g,b)
***********************************************************************
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


