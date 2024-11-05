9/17 tuesday

    call plots が予測不能なことを度々行う。
    
    call plots
    call calibrated data
    call plote                  の順番で行うと難なく行くが，
    
    call calibrated data
    call plots
    call plote                  の順番で行うとsigsevが起きる。
    
    
    ちなみにcalibrated data 以外の，csvから配列にデータ入れる系サブルーチンでは上記の問題が起きない。
    calibrated data はそのサブルーチン内で他のサブルーチンをcallしているのが悪さをしてる？わからん

9/25 wednesday

    betcolork subroutine　のr,g,bを入れる部分に配列の要素を入れるとSIGSEGVが起きる。
    
    eg.)
    do n = 1,100
       call betcolork(-dx,dx,dy,psarray,months*years,mask,-20.+real(2*(n-1)),-20.+real(2*n),r(n),g(n),b(n))
    end do
    
    をするとsigsev
    
    do n = 1,100
       r = r(n),g = g1(n),b = b1(n)
       call betcolork(-dx,dx,dy,psarray,months*years,mask,-20.+real(2*(n-1)),-20.+real(2*n),r,g,b)
    end do
    
    にするとうまくいく。これもなぜかはよくわからん。メモリーの使い方がifortと違うんだろう。おそらく。
