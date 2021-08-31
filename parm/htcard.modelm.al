Input file for verification program, nrl_truth 
---------------------------------------------- 
process track, intensity or wind radii  : track
process in the units                    : metric
print individual storm averages         : .true. 
print all storm errors processed        : .true. 
print all lat/lon values processed      : .true. 
no errors at or above  initial vmax     : 250 
no errors at or below  initial vmax     :   0 
no errors at or above this verify vmax  : 250 
no errors at or below this verify vmax  :   0 
no errors at or north of this initl lat :  90N 
no errors at or south of this initl lat :  90S 
no errors at or east of this initl lon  :   0W 
no errors at or west of this initl lon  :   0E 
no errors before this initial mn/dy/hr  : 010100 
no errors after  this initial mn/dy/hr  : 123118 
process errors for initial 00 hr        : .true. 
process errors for initial 06 hr        : .true. 
process errors for initial 12 hr        : .true. 
process errors for initial 18 hr        : .true. 
compute spatial verification            : .false.
name of model to spatially verify       : RPLC
name of spatial verif reference model   : RPLC
radius of influence (km) for spatial    : 900
# of development levels (max=20)        :  6 
list of development levels to process   : listed on next line 
XX
# of techniques to process (max=12)     : techn
list of technique names to process      : listed on next line 
mod1 mod2 mod3 mod4 mod5 mod6 mod7 mod8 mod9 mod10 mod11 mod12
list of storm IDs to be processed       : listed below 
