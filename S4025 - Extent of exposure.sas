dm log 'clear' output;
dm 'odsresults; clear';

%let path = G:\projects\p428\s4284025\dmc_mar2019\draft1;

libname raw "&path\rawdata";
libname lucdisc "G:\global\CDISC\SDTM\lookup";

%include "P:\Xiaomin Lu\Liver\Liver Fibrosis\FXR PBC-PSC\GS9674_Liver_Fibrosis_428-4025\Data validation\DMC Mar2019\S4025 - ADSL.sas";

%let outfile = "P:\Xiaomin Lu\Liver\Liver Fibrosis\FXR PBC-PSC\GS9674_Liver_Fibrosis_428-4025\For New PSC Study\01Feb2019\Exposure Disc.xlsx";
options nosource nonotes;

data adsl_r; set adsl; 
	if find(SubjID,'S') = 0; 
	Armc = cat('Grp', Trt01PN);
	if Trt01PN = 3 then Armc = 'Grp4';
	if ComSFL = 'N';
run;
data all; set adsl_r; 
	Arm = "Overall"; Armc = 'Grp5';
run;
data Pooled; set adsl_r; 
	if Armc in ('Grp1' 'Grp2');
	Arm = "Pooled"; Armc = 'Grp3';
run;

data anlydat; 
/*	format ExpCatBD1 $20. ExpCatBD4 $20. ExpCatBD8 $20. ExpCatBD12 $20. */
/*		ExpCatOL1 $20. ExpCatOL4 $20. ExpCatOL12 $20. ExpCatOL24 $20. ExpCatOL36 $20. ExpCatOL48 $20. ExpCatOL60 $20. ExpCatOL72 $20. ExpCatOL84 $20.*/
/*		ExpCat1 $20. ExpCat4 $20. ExpCat8 $20. ExpCat12 $20. ExpCat24 $20. ExpCat36 $20. ExpCat48 $20. ExpCat60 $20. ExpCat72 $20. ExpCat84 $20.  ExpCat96 $20.;*/
	set adsl_r pooled all; 
/*	if Tr01DurW > 0 then ExpCatBD1 = '>=  1 Day';*/
/*	if Tr01DurW >= 4 then ExpCatBD4 = '>=  4 weeks';*/
/*	if Tr01DurW >= 8 then ExpCatBD8 = '>=  8 weeks';*/
/*	if Tr01DurW >= 12 then ExpCatBD12 = '>=  12 weeks';*/
/**/
/*	if Tr02DurW > 0 then ExpCatOL1 = '>=  1 Day';*/
/*	if Tr02DurW >= 4 then ExpCatOL4 = '>=  4 weeks';*/
/*	if Tr02DurW >= 12 then ExpCatOL12 = '>=  12 weeks';*/
/*	if Tr02DurW >= 24 then ExpCatOL24 = '>=  24 weeks';*/
/*	if Tr02DurW >= 36 then ExpCatOL36 = '>=  36 weeks';*/
/*	if Tr02DurW >= 48 then ExpCatOL48 = '>=  48 weeks';*/
/*	if Tr02DurW >= 60 then ExpCatOL60 = '>=  60 weeks';*/
/*	if Tr02DurW >= 72 then ExpCatOL72 = '>=  72 weeks';*/
/*	if Tr02DurW >= 84 then ExpCatOL84 = '>=  84 weeks';*/
/**/
/*	TrtDurW = Tr01DurW + Tr02DurW;*/
/*	if Tr02DurW = . then TrtDurW = Tr01DurW;*/
/*	if TrtDurW > 0 then ExpCat1 = '>=  1 Day';*/
/*	if TrtDurW >= 4 then ExpCat4 = '>=  4 weeks';*/
/*	if TrtDurW >= 8 then ExpCat8 = '>=  8 weeks';*/
/*	if TrtDurW >= 12 then ExpCat12 = '>=  12 weeks';*/
/*	if TrtDurW >= 24 then ExpCat24 = '>=  24 weeks';*/
/*	if TrtDurW >= 36 then ExpCat36 = '>=  36 weeks';*/
/*	if TrtDurW >= 48 then ExpCat48 = '>=  48 weeks';*/
/*	if TrtDurW >= 60 then ExpCat60 = '>=  60 weeks';*/
/*	if TrtDurW >= 72 then ExpCat72 = '>=  72 weeks';*/
/*	if TrtDurW >= 84 then ExpCat84 = '>=  84 weeks';*/
/*	if TrtDurW >= 96 then ExpCat96 = '>=  96 weeks';*/
run;

%macro Tableone (dataset =, durc =, d1 =, d2 =, outdat =, cont =);
	data dat1; 
		set &dataset; 
		%if "&ph" = "BD" %then %do; 
			Dur = Tr01DurW;
			if SAFOLFL ne "Y";
		%end;
		if "&ph" = "OL" then Dur = Tr02DurW;
		if "&ph" = "LT" then do;
			Dur = Tr01DurW + Tr02DurW;
			if Tr02DurW = . then Dur = Tr01DurW;
		end;
	run;
	data subdat; format Exp $20.;
		set &dataset; 
		%if "&ph" = "BD" %then %do; 
			Dur = Tr01DurW;
			if SAFOLFL ne "Y";
		%end;
		if "&ph" = "OL" then Dur = Tr02DurW;
		if "&ph" = "LT" then do;
			Dur = Tr01DurW + Tr02DurW;
			if Tr02DurW = . then Dur = Tr01DurW;
		end;
		%if &durc = 1 %then %do; 
			if dur > 0; Exp = ">= 1 Day";
		%end;
		%if &durc > 1 %then %do; 
			if dur >= &durc; Exp = ">= &durc Weeks";
		%end;
	run;
	%if &cont = 1 %then %do;
		%let var = Dur;
		ods output Summary = tmp_summry;
		proc means data=dat1 N mean std median q1 q3 min max; class armc; var &var; run;
		data tmp;
			format N $20. mean_SD $20. median $20. q1_3 $20. min_max $20.;
			set tmp_summry;
			N = strip(put(&var._N,8.));
			mean_SD = cat(strip(put(&var._mean,8.&d1)),' (',strip(put(&var._stddev,8.%eval(&d1 + 1))),')');
			median = strip(put(&var._median,8.&d1));
			q1_3 = cat(strip(put(&var._Q1,8.&d1)),', ',strip(put(&var._Q3,8.&d1)));
			min_max = cat(strip(put(&var._Min,8.&d1)),', ',strip(put(&var._Max,8.&d1)));

			keep Armc N mean_SD median q1_3 min_max;
		run;
		
		proc transpose data=tmp out=outtmp;
			ID Armc;
			Var N mean_SD median q1_3 min_max;
		run;

		data one; format Variable $20. _name_ $20.; 
			set outtmp; 
			if _N_ = 1 then Variable = "&var"; 
			rename _name_ = Level;
		run;
	%end;
	%else %do;
		data one; if _N_ < 1; run;
		%let dsid = %sysfunc(open(subdat));
		%let nobs = %sysfunc(attrn(&dsid.,NOBS));
		%let rc = %sysfunc(close(&dsid.));
		%if &nobs > 0 %then %do;
			%let var = Exp;
			ods output CrossTabFreqs = tmp_freq;
			proc freq data=subdat; table &var*Armc / nopercent norow; run;
			data tmp_freq; set tmp_freq; 
	/*			count_pct = cat(trim(left(put(Frequency,8.))),' ( ',trim(left(put(ColPercent,8.1))),'%)'); */
				count_pct = strip(put(Frequency,8.)); 
				if &var ne '' and Armc ne '';
			run;
			proc sort data=tmp_freq; by &var; run;

			proc transpose data=tmp_freq out=outfreq (drop = _name_);
				by &var;
				ID Armc;
				Var count_pct;
			run;

			data one; format Variable $20. &var $20. ; 
				set outfreq; 
				rename &var = Level; 
				if _N_ = 1 then Variable = "&var";
			run;
		%end;
	%end;
	data &outdat; set &outdat one; run;
%mend;

** Demographaic;
data outExp; if _N_ < 1; run;
%let outdat = outExp;
%let dataset = anlydat;

ods exclude all;
ods noresults;
%let ph = BD; data ph; format Ph $10.; Ph = "&ph Phase"; run;
data &outdat; set &outdat ph; run;
%Tableone(dataset = &dataset, durc = , 	d1 = 1, d2 = 2, outdat = &outdat, cont = 1);
%Tableone(dataset = &dataset, durc = 1,	d1 = , d2 = , outdat = &outdat, cont = 0);
%Tableone(dataset = &dataset, durc = 4, 	d1 = , d2 = , outdat = &outdat, cont = 0);
%Tableone(dataset = &dataset, durc = 8, 	d1 = , d2 = , outdat = &outdat, cont = 0);
%Tableone(dataset = &dataset, durc = 12, d1 = , d2 = , outdat = &outdat, cont = 0);

%let ph = OL; data ph; format ph $10.; Ph = "&ph Phase"; run;
data &outdat; set &outdat ph; run;
%Tableone(dataset = &dataset, durc = , 	d1 = 1, d2 = 2, outdat = &outdat, cont = 1);
%Tableone(dataset = &dataset, durc = 1, d1 = , d2 = , outdat = &outdat, cont = 0);
%Tableone(dataset = &dataset, durc = 4, d1 = , d2 = , outdat = &outdat, cont = 0);
%Tableone(dataset = &dataset, durc = 12, d1 = , d2 = , outdat = &outdat, cont = 0);
%Tableone(dataset = &dataset, durc = 24, d1 = , d2 = , outdat = &outdat, cont = 0);
%Tableone(dataset = &dataset, durc = 36, d1 = , d2 = , outdat = &outdat, cont = 0);
%Tableone(dataset = &dataset, durc = 48, d1 = , d2 = , outdat = &outdat, cont = 0);
%Tableone(dataset = &dataset, durc = 60, d1 = , d2 = , outdat = &outdat, cont = 0);
%Tableone(dataset = &dataset, durc = 72, d1 = , d2 = , outdat = &outdat, cont = 0);
%Tableone(dataset = &dataset, durc = 84, d1 = , d2 = , outdat = &outdat, cont = 0);

%let ph = LT; data ph; format ph $10.; Ph = "&ph Phase"; run;
data &outdat; set &outdat ph; run;
%Tableone(dataset = &dataset, durc = , 	d1 = 1, d2 = 2, outdat = &outdat, cont = 1);
%Tableone(dataset = &dataset, durc = 1, d1 = , d2 = , outdat = &outdat, cont = 0);
%Tableone(dataset = &dataset, durc = 4, d1 = , d2 = , outdat = &outdat, cont = 0);
%Tableone(dataset = &dataset, durc = 12, d1 = , d2 = , outdat = &outdat, cont = 0);
%Tableone(dataset = &dataset, durc = 24, d1 = , d2 = , outdat = &outdat, cont = 0);
%Tableone(dataset = &dataset, durc = 36, d1 = , d2 = , outdat = &outdat, cont = 0);
%Tableone(dataset = &dataset, durc = 48, d1 = , d2 = , outdat = &outdat, cont = 0);
%Tableone(dataset = &dataset, durc = 60, d1 = , d2 = , outdat = &outdat, cont = 0);
%Tableone(dataset = &dataset, durc = 72, d1 = , d2 = , outdat = &outdat, cont = 0);
%Tableone(dataset = &dataset, durc = 84, d1 = , d2 = , outdat = &outdat, cont = 0);
%Tableone(dataset = &dataset, durc = 96, d1 = , d2 = , outdat = &outdat, cont = 0);


ods exclude none;
ods results;

title "Extent of Exposure"; 
proc export data = outExp outfile = &outfile dbms = xlsx replace; 
	sheet = "Exposure";
run;


options source notes;

