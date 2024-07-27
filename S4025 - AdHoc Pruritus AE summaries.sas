dm log 'clear' output;
dm 'odsresults; clear';

%let path = G:\projects\p428\s4284025\cdp;

libname raw "&path\rawdata";
libname lucdisc "G:\global\CDISC\SDTM\lookup";

%include "P:\Xiaomin Lu\Liver\Liver Fibrosis\FXR PBC-PSC\GS9674_Liver_Fibrosis_428-4025\Data validation\DMC Mar2019\S4025 - ADAE.sas";

%let outfile = "P:\Xiaomin Lu\Liver\Liver Fibrosis\FXR PBC-PSC\GS9674_Liver_Fibrosis_428-4025\For New PSC Study\Current Data\AdHoc-Itching.xlsx";
options nosource nonotes;

data dat1; set adae;
	Armc = cat('Grp', Trt01PN);
	if Trt01PN = 3 then Armc = 'Grp4';
	if PrutFL = 'Y';
run;

data Pooled; set dat1; if Armc in ('Grp1' 'Grp2'); Armc = 'Grp3'; run;
data all; set dat1; Armc = 'Grp5'; run;
data anlydat; set dat1 pooled all; run;

%macro table1 (dataset =, cond =, MaxGr = , varname =, outdat =);
	data _sub; set &dataset; if &cond; run;
	%if &MaxGr > 0 %then %do;
		proc sort data=_sub; by &TrtEMFL SubjID descending AtoxGrN; run;
		data _sub; set _sub;
			by &TrtEMFL SubjID descending AtoxGrN;
			retain MaxAEtoxGr; 
			if first.SubjID then MaxAEtoxGr = AtoxGrN;
			if &TrtEMFL ne 'Y' then MaxAEtoxGr = .;
			if MaxAEtoxGr = &MaxGr;
		run;
	%end;
	proc sort data=_sub ; by SubjID Armc; run;
	data _sub1; set _sub; by SubjID Armc; if last.Armc; run;

	proc freq data=_sub1; table Armc / out=tmp_freq; run;
	data tmp_freq; set tmp_freq; 
/*		count_pct = cat(trim(left(put(Count,8.))),' ( ',trim(left(put(Percent,8.1))),'%)'); */
		count_pct = strip(put(Count,8.)); 
		keep Armc count_pct;
	run;

	proc transpose data=tmp_freq out=outtmp;
		ID Armc;
		Var count_pct;
	run;
	data one; format Variable $50.; set outtmp; Variable = "&varname"; drop _name_; run;
	data &outdat; set &outdat one; run;
%mend;

%let dataset = anlydat; %let TrtEMFL = TrtEMFL_BD; %let outdat = TEAE_BD; 
data &outdat; if _N_ < 1 ;run;

ods exclude all;
ods noresults;
%table1(dataset = &dataset, cond = &TrtEMFL = 'Y' and AtoxGrN >= 2, MaxGr = , varname = Itching related TEAE with Grade 2 or Higher, outdat = &outdat);
ods exclude none;
ods results; 

proc export data = TEAE_BD outfile = &outfile dbms = xlsx  replace; 
	sheet = 'TEAE_BD';
run;



%let dataset = anlydat; %let TrtEMFL = TrtEMFL_OL; %let outdat = TEAE_OL; 
data &outdat; if _N_ < 1 ;run;

ods exclude all;
ods noresults;
%table1(dataset = &dataset, cond = &TrtEMFL = 'Y' and AtoxGrN >= 2, MaxGr = , varname = Itching related TEAE with Grade 2 or Higher, outdat = &outdat);
ods exclude none;
ods results; 


proc export data = TEAE_OL outfile = &outfile dbms = xlsx  replace; 
	sheet = 'TEAE_OL';
run;




********************************************************
*                     TEAE PT                      *
********************************************************;

%macro PT (dataset =, cond =, ph =, outdat =);
	data _sub; set &dataset; if &cond; run;
	proc sort data=_sub ; by SubjID Armc; run;
	data _sub1; set _sub; by SubjID Armc; if last.Armc; run;

	proc freq data=_sub1; table Armc / out=tmp_freq; run;
	data tmp_freq; set tmp_freq; 
		count_pct = strip(put(Count,8.)); 
		keep Armc count_pct;
	run;

	proc transpose data=tmp_freq out=one (drop = _name_);
		ID Armc;
		Var count_pct;
	run;

	data &outdat._&ph; set one; run;

	proc sort data=_sub out=_sub1PT nodupkey; by SubjID AEDeCod Armc; run;

	ods output CrossTabFreqs = tmpPT;
	proc freq data=_sub1PT; table AEDeCod*Armc / nopercent norow; run;
	data tmpPT; set tmpPT; 
		count_pct = right(put(Frequency,8.)); 
		if AEDeCod ne '' and Armc ne '';
		keep AEDeCod Armc count_pct;
	run;
	proc transpose data=tmpPT out=outPT (drop = _name_);
		by AEDeCod;
		ID Armc;
		Var count_pct;
	run;
	data outPT; set name outPT; run;
	proc sort data=outPT; by descending Grp1; run;
	%if "&ph" = "OL" %then %do;
		proc sort data=outPT; by descending Grp5 AEDeCod; run;
	%end;
	data &outdat._&ph; set &outdat._&ph outPT; run;
%mend;
data name; input AEBodSys $200. AEDeCod $200.; cards; ; 
run;


data nonprut; set anlydat; if AEDeCod ne 'Pruritus' then AEDeCod = "NonPrut"; run; 
data coded; set anlydat nonprut;
	if AEBodSys = '' or AEDeCod = '' then do; 
		AEBodSys = '_NOT CODED_';
		AEDeCod = AEterm;
	end; 
run;

ods exclude all;
ods noresults;

%PT(dataset = coded, cond = TrtEMFL_BD = 'Y', 					ph = BD, outdat = PT);
%PT(dataset = coded, cond = TrtEMFL_BD = 'Y' and AtoxGrN >= 2, 	ph = BD, outdat = PT_g2);

%PT(dataset = coded, cond = TrtEMFL_OL = 'Y', 					ph = OL, outdat = PT);
%PT(dataset = coded, cond = TrtEMFL_OL = 'Y' and AtoxGrN >= 2, 	ph = OL, outdat = PT_g2);

%PT(dataset = coded, cond = TrtEMFL = 'Y', 						ph = LT, outdat = PT);
%PT(dataset = coded, cond = TrtEMFL = 'Y' and AtoxGrN >= 2, 	ph = LT, outdat = PT_g2);

ods exclude none;
ods results; 

%macro prnt_tab(ph =);
	title "Pruritus TEAE PT - &ph";
	proc export data = PT_&ph outfile = &outfile dbms = xlsx  replace; 
		sheet = "PT_&ph";
	run;

	title "Pruritus TEAE PT grade 2 or higher - &ph";
	proc export data = PT_g2_&ph outfile = &outfile dbms = xlsx  replace; 
		sheet = "PT_g2_&ph";
	run;

%mend;

%prnt_tab(ph = BD);
%prnt_tab(ph = OL);
%prnt_tab(ph = LT);





proc sort data=dat1; by AtoxGrN TrtEMFL_BD TrtEMFL_OL SubjID AStDt AEnDt; run;
title 'Listing - Pruritus TEAE';
data dat1; 
	retain SubjID Armc AtoxGrN TrtEMFL_BD TrtEMFL_OL AEDeCod AETerm TrtEMFL AStDt AEnDt Tr01SDT Tr01EDT Tr02SDT Tr02EDT AEser AtoxGrN AErel AEacn_std AEacn1_std AacnOth;
	set dat1; 
	keep SubjID Armc AtoxGrN TrtEMFL_BD TrtEMFL_OL AEDeCod AETerm TrtEMFL AStDt AEnDt Tr01SDT Tr01EDT Tr02SDT Tr02EDT AEser AtoxGrN AErel AEacn_std AEacn1_std AacnOth;
run;
proc export data = dat1 outfile = &outfile dbms = xlsx  replace; 
	sheet = 'Listing_TEAE';
run;


options source notes;

