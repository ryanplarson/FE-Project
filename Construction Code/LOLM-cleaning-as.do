* Locked out of the labor market
* 8.23.17: preliminary data quality check - AS

*For Aaron desktop
global path "C:\Users\asojourn\Google Drive\FE 2017\"

* Aaron laptop
global path "C:\Users\asojourn\GDrive\FE 2017\"


clear
clear all

* Import
import delimited using "$path\FE_prelim.csv"
drop v1

sum, d
codebook

* Clean SSDI Rate to make numeric and replace missing with 0
* Need to always have year effects in
rename ssdirate orig_ssdirate
gen i_Obs_ssdirate = 1
replace  i_Obs_ssdirate = 0 if orig_ssdirate == "NA"
replace orig_ssdirate = "0" if orig_ssdirate == "NA"
destring orig_ssdirate, gen(ssdirate)
tab year i_Obs_ssdirate

rename p1y4cfwrate orig_p1y4cfwrate
gen i_Obs_p1y4cfwrate = 1
replace  i_Obs_p1y4cfwrate = 0 if orig_p1y4cfwrate == "NA"
replace orig_p1y4cfwrate = ""  if orig_p1y4cfwrate == "NA"
destring orig_p1y4cfwrate, gen(p1y4cfwrate)
tab year i_Obs_p1y4cfwrate

global Ps "ssdirate effectivewage tanfmu z_labor_unemployment_compensatio"
global Xs "popshare1625 popshare2635 popshare3645 popshare4655  popshare5665 t_1unemprate t_2unemprate t_3unemprate"
xtset statefip year
sum p1y1notemployedrate p1y2unemployedrate p1y3idlerate p1y4cfwrate pctexfel $Ps $Xs year
sum ssdirate if i_Obs_ssdirate==1

* TBA Need a different one for p1y4cfwrate
bysort statefip: egen Tpop_s = total(p1pop)

foreach y in p1y1notemployedrate p1y2unemployedrate p1y3idlerate p1y4cfwrate { 
	xtreg `y' i.year         pctexfel [aw=Tpop_s], fe vce(cluster statefip)
	xtreg `y' i.year $Xs     pctexfel [aw=Tpop_s], fe vce(cluster statefip)
	xtreg `y' i.year $Xs $Ps pctexfel [aw=Tpop_s], fe vce(cluster statefip)
	}

	