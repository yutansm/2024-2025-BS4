subroutine sigma_potemp_sal_cal(sigma,potemp,sal)
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
    double precision::q,Gamma,Theta,xk



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
        sigma=rhoafter-1000.
        end if 
!密度計算終わり


end subroutine
