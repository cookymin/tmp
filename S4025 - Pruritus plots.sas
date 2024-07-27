dm log 'clear' output;
dm 'odsresults; clear';



%let path = G:\projects\p428\s4284025\cdp;

libname raw "&path\rawdata";
libname lucdisc "G:\global\CDISC\SDTM\lookup";

%include "P:\Xiaomin Lu\Liver\Liver Fibrosis\FXR PBC-PSC\GS9674_Liver_Fibrosis_428-4025\Data validation\Current data\S4025 - ADAE.sas";

%let outpath = P:\Xiaomin Lu\Liver\Liver Fibrosis\FXR PBC-PSC\GS9674_Liver_Fibrosis_428-4025\For New PSC Study\Current Data;

options nosource nonotes;

data dat_AE; format Armc $10.; set adae;
	if Trt01PN = 1 then Armc = '100 mg';
	if Trt01PN = 2 then Armc = '30 mg';
	if Trt01PN = 3 then Armc = 'Placebo';

	dur_BD = Tr01EDT - Tr01SDT + 31;
	dur_OL = Tr02EDT - Tr02SDT + 31;
	dur_LT = TrtEDT - TrtSDT + 31;
	if SubjID = '48448' then dur_LT = Tr01EDT - TrtSDT + 31;

	if upcase(AEDECOD) = upcase("Pruritus") and TrtEmFL = 'Y';

	DStAE_LT = AStDt - TrtSDT + 1;
	DEnAE_LT = AEnDt - TrtSDT + 1;
	if AEnDt = . then DEnAE_LT = TrtEDT - TrtSDT + 31;
	if DEnAE_LT = DStAE_LT then DEnAE_LT = DStAE_LT + 1;
	Status_LT = 1;
	Day_OLE = Tr02SDT - TrtSDT + 1;

	if TrtEmFL_BD = 'Y' then do;
		DStAE_BD = AStDt - Tr01SDT + 1;
		DEnAE_BD = AEnDt - Tr01SDT + 1;
		if AEnDt = . then DEnAE_BD = Tr01EDT - Tr01SDT + 31;
		if DEnAE_BD = DStAE_BD then DEnAE_BD = DStAE_BD + 1;
		Status_BD = 1;
	end; 
	if TrtEmFL_OL = 'Y' then do;
		DStAE_OL = AStDt - Tr02SDT + 1;
		DEnAE_OL = AEnDt - Tr02SDT + 1;
		if AEnDt = . then DEnAE_OL = Tr02EDT - Tr02SDT + 31;
		if DEnAE_OL = DStAE_OL then DEnAE_OL = DStAE_OL + 1;
		Status_OL = 1;
	end; 

	time_BD = DStAE_BD;
	if Status_BD ne 1 then do;
		Status_BD = 0;
		time_BD = dur_BD;
	end;

	time_OL = DStAE_OL;
	if Status_OL ne 1 then do;
		Status_OL = 0;
		time_OL = dur_OL;
	end;

	time_LT = DStAE_LT;
	if Status_LT ne 1 then do;
		Status_LT = 0;
		time_LT = dur_LT;
	end;
	AtoxGr = cat("Grade ", AtoxGrN);
	keep SubjID Armc dur_BD dur_OL dur_LT TrtEmFL_BD TrtEmFL_OL AStDt AEnDt AtoxGrN AtoxGr AacnOth AEterm AESer Aongo AErel AEacn AEacn1 
		DStAE_BD DEnAE_BD Status_BD DStAE_OL DEnAE_OL Status_OL DStAE_LT DEnAE_LT Status_LT Day_OLE AEacn_std AEacn1_std 
		time_BD Status_BD time_OL Status_OL time_LT Status_LT ComT01FL ComT02FL ComSFL Tr01SDT Tr01EDT Tr02SDT Tr02EDT;
run;


%macro EAIR(ph =, outdat =);
	proc sort data=dat_AE out=tmp; by SubjID time_&ph; run;
	data Surv; set tmp; by SubjID time_&ph; 
		if first.SubjID; 
		Survtime = time_&ph/365.25; Status = Status_&ph;
		keep SubjID Armc Survtime Status;
	run;
	data Surv_&ph; merge Surv adsl; by SubjID; run;
	data Surv_&ph; set Surv_&ph;
		if Status = . then do;
			Status = 0;
			if "&ph" = "BD" then Survtime = (Tr01EDT - Tr01SDT + 31)/365.25;
			if "&ph" = "OL" then Survtime = (Tr02EDT - Tr02SDT + 31)/365.25;
			if "&ph" = "LT" then Survtime = (TrtEDT - TrtSDT + 31)/365.25;
		end;
		if Trt01PN = 1 then Armc = '100 mg';
		if Trt01PN = 2 then Armc = '30 mg';
		if Trt01PN = 3 then Armc = 'Placebo';
	run;
	ods output Summary = tmp_summry (keep = Armc Survtime_N Status_Sum Survtime_Sum);
	proc means data=Surv_&ph sum N; var Status Survtime; class Armc; run;
	ods output Summary = tmp_summry_o (keep = Survtime_N Status_Sum Survtime_Sum);
	proc means data=Surv_&ph sum N; var Status Survtime; run;
	data one; format Armc $20.;
		set tmp_summry tmp_summry_o;
		if Armc = '' then Armc = "Overall";
		Phase = "&ph";
		EAIR = Status_Sum/Survtime_Sum;
		Rate = Status_Sum/Survtime_N;
	run;
	data &outdat; set &outdat one; run;
%mend;

ods exclude all;
ods noresults;

data outEAIR; if _N_ < 1; run;
%EAIR(ph = BD, outdat = outEAIR);
%EAIR(ph = OL, outdat = outEAIR);
%EAIR(ph = LT, outdat = outEAIR);

ods exclude none;
ods results; 

title "EAIR of Pruritus";
data outEAIR; 
	retain Phase Armc Survtime_N Status_sum Survtime_sum Rate EAIR;
	set outEAIR; 
	rename Survtime_N = N Status_sum = n_Prut Survtime_sum = Exposure Rate = Rate_unadj;
run;
proc export data = outEAIR outfile = "&outpath\EAIR of Pruritus.xls" dbms = xls  replace; run;



** Medical History of Subjects with Pruritus;
data MH; set raw.MH; 
	keep SubjID MHTerm MHSTDat_YY MHEnDat_YY MHongo;
run;
proc sort data=MH; by SubjID; run;
proc sort data=dat_AE out=subj_prut (keep = subjID Armc) nodupkey; by SubjID; run;
data subj_prut; set subj_prut; prutfl = 'Y'; run;
data Prut_MH; merge MH subj_prut; by SubjID; if prutfl = 'Y'; drop prutfl; run; 

data Prut_MH; 
	retain SubjID Armc MHTerm MHSTDat_YY MHEnDat_YY MHongo;
	set Prut_MH; by SubjID;
	if not first.SubjID then do; SubjID = ''; Armc = ''; end;
run;
proc export data = Prut_MH outfile = "&outpath\Prut_MH.csv" dbms = CSV  replace; run;



** data for Pruritus plot;
data prut_swimmer_BD; set dat_AE; 
	if TrtEmFL_BD = 'Y';
	if ComT01FL = 'N' then Drop_BD = '#'; 
	if ComSFL = 'N' and ComT01FL = 'Y' and ComT02FL = '' then Drop = '$';
	if AEacn_std = 6 then AE_disc = "Y";

	keep SubjID Armc DStAE_BD DEnAE_BD AtoxGrN AtoxGr ComT01FL ComT02FL ComSFL dur_BD dur_OL dur_LT Drop_BD Drop AE_disc time_BD Status_BD;
run;

proc export data = prut_swimmer_BD outfile = "&outpath\dat_prut_swimmer_BD.csv" dbms = CSV  replace; run;


data prut_swimmer_OL; set dat_AE; 
	if TrtEmFL_OL = 'Y';
	if ComT02FL = 'N' then Drop_OL = '#';
	if AEacn1_std = 6 then AE_disc = "Y";
	keep SubjID Armc DStAE_OL DEnAE_OL AtoxGrN AtoxGr ComT01FL ComT02FL ComSFL dur_BD dur_OL dur_LT Drop_OL AE_disc Status_OL time_OL;
run;
proc export data = prut_swimmer_OL outfile = "&outpath\dat_prut_swimmer_OL.csv" dbms = CSV  replace; run;


data prut_swimmer_LT; set dat_AE; 
	if ComT01FL = 'N' or ComT02FL = 'N' then Drop_LT = '#'; 
	if ComSFL = 'N' and ComT01FL = 'Y' and ComT02FL = '' then Drop = '$';
	if AEacn_std = 6 or AEacn1_std = 6 then AE_disc = "Y";

	keep SubjID Armc DStAE_LT DEnAE_LT AtoxGrN AtoxGr ComT01FL ComT02FL ComSFL dur_BD dur_OL dur_LT Drop_LT Day_OLE Drop AE_disc Status_LT time_LT;
run;

proc export data = prut_swimmer_LT outfile = "&outpath\dat_prut_swimmer_LT.csv" dbms = CSV  replace; run;



options source notes;


