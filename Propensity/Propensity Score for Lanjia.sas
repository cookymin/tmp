dm log 'clear' output;
dm 'odsresults; clear';

%let outpath = P:\Xiaomin Lu\Liver\Journal Club\NASH\Propensity;
%let outfile = "&outpath\Summary - Propensity Score Matching 01-29-2020.xlsx";
libname outlib "&outpath";

options nosource nonotes;


********************************************************
**                     PS Matching                    **
********************************************************;

%macro PS(dataset =, resp =, model =, grp =, var_clas =, var_num =);
	data subdat; set &dataset;
		keep USUBJID STUDYID &grp &var_clas &var_num &resp;
		if &resp ne '';
	run;
	ods output StdDiff = tmp_diff  PSInfo = tmp_PSinfo;
	proc psmatch data=subdat region=treated;
		class &grp &var_clas;
		psmodel &grp (treated='CILO/FIR') = &var_clas &var_num;
		match method=greedy(k=1) exact=(&var_clas);
		assess lps var=(&var_clas &var_num) / weight=none;
		output out(obs=match)=&resp lps=_Lps matchid=_MatchID;
	run;
	data outlib.&resp._&model; set &resp; run;

	data tmp_diff; format Response $20. Model $10.;
		set tmp_diff; 
		Response = "&resp";
		Model = "Model " || strip(&model);
	run;
	data tmp_PSinfo; format Response $20. Model $10.;
		set tmp_PSinfo; 
		Response = "&resp";
		Model = "Model " || strip(&model);
	run;

	data diff; set diff tmp_diff; run;
	data PSinfo; set PSinfo tmp_PSinfo; run;
%mend;

data diff; set _null_; run;
data PSinfo; set _null_; run;

ods exclude all;
ods noresults;

%let model = 1;
%PS(dataset = datPS, resp = PE48OC, model = &model, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL FIBSCAN_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASLI_BL NASHB_BL COLLAGEN_BL ALPHASMA_BL BILI_BL);

ods exclude none;
ods results; 

proc export data = diff outfile = &outfile dbms = xlsx replace; 
	sheet = "Mean_diff";
run;

proc export data = PSinfo outfile = &outfile dbms = xlsx replace; 
	sheet = "PS_Summary";
run;




********************************************************
**                         IPW                        **
********************************************************;


/*%macro WT_IPW(dataset =, resp =, model =, var_clas =, var_num =);*/
/*	proc psmatch data=&dataset region=allobs;*/
/*		class ARM &var_clas;*/
/*		psmodel ARM (treated='CILO/FIR') = &var_clas &var_num;*/
/*		assess lps var=(&var_clas &var_num) / weight=ATEWGT;*/
/*		output out(obs=all)=outtmp ATEWGT = WT_&Resp._&model;*/
/*	run;*/
/*	data &dataset; set outtmp;*/
/*		if WT_&Resp._&model > 10 then WT_&Resp._&model = 10;*/
/*		drop _PS_ _ATTWGT_;*/
/*	run;*/
/*%mend;*/
%macro WT_IPW(dataset =, resp =, model =, var_clas =, var_num =);
	proc logistic data=&dataset; 
		class &var_clas;
		model ARM (event='CILO/FIR') = &var_clas &var_num;
		output out=outtmp p = PS;
	run;
	data &dataset; set outtmp; 
		WT_&Resp._&model = (ARM = "CILO/FIR")/PS + (ARM = "Placebo")/(1-PS);
		if WT_&Resp._&model > 10 then WT_&Resp._&model = 10;
		drop PS _LEVEL_; 
	run;
%mend;


ods exclude all;
ods noresults;

%let model = 1;
%WT_IPW(dataset = datPS, resp = PE48OC, model = &model, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL FIBSCAN_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASLI_BL NASHB_BL COLLAGEN_BL ALPHASMA_BL BILI_BL);

ods exclude none;
ods results; 

data outlib.PS_WT; set datPS; run;


options source notes;
