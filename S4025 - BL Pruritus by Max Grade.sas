dm log 'clear' output;
dm 'odsresults; clear';

%let path = G:\projects\p428\s4284025\cdp;

libname raw "&path\rawdata";
libname lucdisc "G:\global\CDISC\SDTM\lookup";

%include "P:\Xiaomin Lu\Liver\Liver Fibrosis\FXR PBC-PSC\GS9674_Liver_Fibrosis_428-4025\Data validation\DMC Mar2019\S4025 - ADAE.sas";

%let outfile = "P:\Xiaomin Lu\Liver\Liver Fibrosis\FXR PBC-PSC\GS9674_Liver_Fibrosis_428-4025\For New PSC Study\Current Data\output_PRUT 07-16-2019.xlsx";

options nosource nonotes;

data dat1; format trtgrp $20.; set adae;
	if upcase(AEDECOD) = upcase("Pruritus") or  upcase(AEDECOD) = upcase("Pruritus generalised");
/*	if AStDt <= TrtSDT and (AEnDt >= TrtSDT or AEnDt = .);*/
	keep SubjID AEDECOD AtoxGrN;
run;

proc sort data=dat1; by SubjID descending AtoxGrN; run;
data prut; set dat1;
	by SubjID descending AtoxGrN;
	if first.SubjID;
run;

data dat2; format trtgrp $20.; set adsl;
	Armc = cat('Grp', Trt01PN);
	if Trt01PN = 3 then Armc = 'Grp4';
	if Armc = 'Grp1' then trtgrp = '100 mg';
	if Armc = 'Grp2' then trtgrp = '30 mg';
	if Armc = 'Grp4' then trtgrp = 'Placebo';
	if trtgrp ne '';
run;

proc sort data=dat2; by SubjID; run;
data drop_prut; merge dat2 prut; by SubjID;
	if ComSFL = 'N' then Drop = 1;
	else Drop = 0;
	if AtoxGrN = . then AtoxGrN = 0;
	if AtoxGrN >= 2 then Prut_grd = "Prut G2-3";
	else Prut_grd = "Prut G0-1";
run;

proc freq data=drop_prut; tables drop*(AtoxGrN Prut_grd) / nopercent nocol fisher; run;


options source notes;

