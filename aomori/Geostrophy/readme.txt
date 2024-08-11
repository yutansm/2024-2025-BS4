データの存在する列をnとすると，それはstation (n)とstation(n-1)の間のものを指す。
データはステーション1-6(プログラム上では4-9)までしか計算していない
用いたデータは25_Median と51_Median にあるデータをcalibrate subroutineに通したもの
velocity の次元はcm/s, transport の次元はsveldrups(=10^6 m^3/s)
ラベルは51db もしくは25db median filtered のものを表す。
transport はst 1-6(4-9)のモノで，1-6全てのvelocityが0でないときのみ計算している
計算はともにdouble precision 


eg)

    1             2            3            4           5           6             7           8             9                   列

0.000   0.000   0.000   0.000   20.00  34.00   -14.00   50.00   2.000	  値

st0-1     st1-2    st2-3    st3-4    st4-5    st5-6     st6-7     st7-8    st8-9	  の値(st0-1は形式的なもの)