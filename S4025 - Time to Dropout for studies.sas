dm log 'clear' output;
dm 'odsresults; clear';

%let outfile = "P:\Xiaomin Lu\Liver\Liver Fibrosis\FXR PBC-PSC\GS9674_Liver_Fibrosis_428-4025\For New PSC Study\01Feb2019\Time to Dropouts.xlsx";
options nosource nonotes;


** 428-4025;
%let path = G:\projects\p428\s4284025\dmc_mar2019\draft1;
libname raw "&path\rawdata";
libname adam "&path\adamdata";
/*%include "P:\Xiaomin Lu\Liver\Liver Fibrosis\FXR PBC-PSC\GS9674_Liver_Fibrosis_428-4025\Data validation\DMC Mar2019\S4025 - ADSL.sas";*/

data TimeToD_4025; format Study $20.; set adam.adsl;
	if find(SubjID,'S') = 0; 
/*	if ComT01FL = 'N' or ComT02FL = 'N' or ComSFL = 'N' then Drop = 1;*/
	if ComSFL = 'N' then Drop = 1;
	else Drop = 0;
	TimeToD = (TrtEDT - TrtSDT + 1)/7;
	if SafOLFL = 'Y' then TimeToD = (TrtEDT - Tr02SDT + Tr01EDT - TrtSDT + 1)/7;

	Study = "FXR 428-4025";
	keep SubjID TimeToD Drop Study DcsReas;
run;


** 427-4024;
%let path = G:\projects\p427\s4274024\dmc_mar2019\draft1;
libname raw "&path\rawdata";
libname adam "&path\adamdata";
options nosource nonotes;

data TimeToD_4024; format Study $20.; set adam.adsl;
	if find(SubjID,'S') = 0; 
/*	if ComT01FL = 'N' or ComT02FL = 'N' or ComSFL = 'N' then Drop = 1;*/
	if ComSFL = 'N' then Drop = 1;
	else Drop = 0;
	TimeToD = (TrtEDT - TrtSDT + 1)/7;
*	if SafOLFL = 'Y' then TimeToD = (TrtEDT - Tr02SDT + Tr01EDT - TrtSDT + 1)/7;

	Study = "FXR 427-4024";
	keep SubjID TimeToD Drop Study DcsReas;
run;


** 321-0102;
libname super "G:\projects\p321\s3210102\eff_explore\draft1\adata\21may2018";
data TimeToD_0102; format Study $20.; set super.adexp1;
	if find(SubjID,'S') = 0; 
	if COMSTFL = 'N' then Drop = 1;
	else Drop = 0;
	TimeToD = (TrtEDT - TrtSDT + 1)/7;

	Study = "SIM 321-0102";
	keep SubjID TimeToD Drop Study DiscST;
	rename DiscST = DcsReas;
run;


** 402-1852;
libname adam "G:\projects\p402\s4021852\final\version1\adamdata";
data TimeToD_1852; format Study $20.; set adam.adsl;
	if find(SubjID,'S') = 0; 
	if ComSFL = 'N' then Drop = 1;
	else Drop = 0;
	TimeToD = (TrtEDT - TrtSDT + 1)/7;

	Study = "FXR 402-1852";
	keep SubjID TimeToD Drop Study DcsReas;
run;

data cmd_study; set TimeToD_4025 TimeToD_4024 TimeToD_0102 TimeToD_1852; run;


ods output LifetableEstimates=lifetable (keep = Study LowerTime UpperTime Failed Censored Survival Failure StdErr);
proc lifetest data=cmd_study method=lt intervals = (0 to 96 by 4) plots=none;
	time TimeToD*Drop(0);
	strata Study;
run;
proc export data = lifetable outfile = &outfile dbms = xlsx replace; 
	sheet = "LifeTable";
run;


ods graphics on;
proc lifetest data=cmd_study plots = s (atrisk=0 to 96 by 12) notable;
	time TimeToD*Drop(0);
	strata Study;
run;
ods graphics off;


options source notes;
