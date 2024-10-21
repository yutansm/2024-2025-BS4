program correlationcoefficient
    use always
    implicit none
    call plots(10.,15.,9,'/Users/yuta/LABWORK/2024-2025-BS4/aomori/Plots/TempSalSigma/Timeseries/test.ps')
    call gmark(0.,0.,0.1,1)
    call symbolc(2.5/2.,0.3,0.4,'PT,S',0.)
    call rgbk(1.,0.,0.);call gmark(0.,0.,0.1,1)
    call plote
end program