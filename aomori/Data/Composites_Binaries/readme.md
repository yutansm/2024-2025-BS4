these binary files contain monthly means of times when warm water was coastally trapped and times when it wasn't. the files are big_endian direct access files

the *_trapped.bin files contain coastal trapped mean profiles 
the *.bifur.bin files contain bifuricating mean profiles
the dimensions are; (st1:6,1:400) ,except for V (1:5(between stations),1:400) be wary that the station indices match the array indices
PT = potential temperature; S = salinity; D = density [sigma-theta]; V = geostrophic velocity [m/s]

the criterion is the following;

temperatures of station 4 at depth 250m were taken as reference temps

if the reference temp > (mean + sd)temp of st4 250m of the entire time series data,
then that month is interpreted as when warm water (tsushima origin) pertruded offshore, ie bifuricating warm current

On the contrary, when the reference temp < (mean) of ... then that month is interpreted as when warm water was coastally trapped. the criterion for coastal-trapped-ness is weaker since no reference temp was below (mean-sd). 

Although this is not a perfect way to distinguish the two different phenomena, it is still quite good and interesting.

The reason why we take st4 250m as the criterion is, upon looking at the hydrographic data, the times when warm waters seemed to bifuricate offshore, the bottom core tended to exist around station4 250m . About the depth, 250m is around the depth where permanent thermocline exists so unless the warm waters bifuricate to st4, the temperature there is almost static regardless of seasonal changes.

The bifuricating binary only exist for total mean since only 16 profiles exists in total. So no monthly mean. the conf_bifur is not monthly, it is totalmean just printed 12 times.
