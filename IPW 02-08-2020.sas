dm log 'clear' output;
dm 'odsresults; clear';

libname atlas "\\gsasdata\biometrics\projects\p454\s4544378\eff_explore\final_unblinded\adata\" access = readonly;
libname stellar3 "\\gsasdata\biometrics\projects\p384\s3841943\eff_explore\draft1\adata" access = readonly;
libname stellar4 "\\gsasdata\biometrics\projects\p384\s3841944\eff_explore\draft1\adata" access = readonly;

%let outpath = P:\Xiaomin Lu\Liver\Journal Club\NASH\Propensity\IPW;
libname outlib "&outpath";

options nosource nonotes;

data S4378; set atlas.adexp;
	keep STUDYID USUBJID ARM RANDFL SAFFL FASFL STRAT1NM STRAT1V STRAT1VN STRAT2NM STRAT2V STRAT2VN AGE BMI_BL
		ELFSCORE_BL HYLURAC_BL PIIINP_BL TIMP1_BL FIBSCAN_BL FIBSG_BL COLLAGEN_BL ALPHASMA_BL AST_BL ALT_BL GGT_BL BILI_BL PLAT_BL CRP_BL A2MACG_BL FINSULIN_BL 
		CY18M30_BL CY18M65_BL NASLI_BL NASHB_BL STEATC_BL NAS_BL PE48OC NR48OC NAS48OC HS48OC LI48OC HB48OC;
	if ARM = "GS-0976 + GS-9674" then ARM = "CILO/FIR";
	if ARM in ("CILO/FIR", "Placebo");
run;

data S1943; set stellar3.adexp;
	keep STUDYID USUBJID ARM RANDFL SAFFL FASFL STRAT1NM STRAT1V STRAT1VN AGE BBMI
		ELFSCORE_BL HYLURAC_BL PIIINP_BL TIMP1_BL FIBSCAN_BL FIBSG_BL COLLAGEN_BL ALPHASMA_BL AST_BL ALT_BL GGT_BL BILI_BL PLAT_BL CRP_BL A2MACG_BL FINSULIN_BL 
		CY18M30_BL CY18M65_BL NASLI_BL NASHB_BL STEATC_BL NAS_BL PE48OC NR48OC NAS48OC HS48OC LI48OC HB48OC;
	rename BBMI = BMI_BL;
	if ARM = "one PTM SEL 6 mg tablet + one PTM SEL 18 mg tablet administered orally";
	ARM = "Placebo";
run;

data S1944; set stellar4.adexp;
	keep STUDYID USUBJID ARM RANDFL SAFFL FASFL STRAT1NM STRAT1V STRAT1VN AGE BBMI
		ELFSCORE_BL HYLURAC_BL PIIINP_BL TIMP1_BL FIBSCAN_BL FIBSG_BL COLLAGEN_BL ALPHASMA_BL AST_BL ALT_BL GGT_BL BILI_BL PLAT_BL CRP_BL A2MACG_BL FINSULIN_BL 
		CY18M30_BL CY18M65_BL NASLI_BL NASHB_BL STEATC_BL NAS_BL PE48OC NR48OC NAS48OC HS48OC LI48OC HB48OC;
	rename BBMI = BMI_BL;
	if ARM = "one PTM SEL 6 mg tablet + one PTM SEL 18 mg tablet administered orally";
	ARM = "Placebo";
run;



data datPS; set S4378 S1943 S1944; run;
data datPS; set datPS; 
	if STUDYID in ("GS-US-384-1943" "GS-US-384-1944") then do;
		STRAT2NM = "Cirrhosis Status";
		if FIBSG_BL = 4 then do; STRAT2VN = 1; STRAT2V = "Cirrhosis (F4)"; end;
		if FIBSG_BL < 4 then do; STRAT2VN = 2; STRAT2V = "Non-cirrhosis (Not F4)"; end;
	end;
run;

data outlib.datPS; set datPS; run;
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

%WT_IPW(dataset = datPS, resp = NR48OC, model = &model, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL FIBSCAN_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASLI_BL NASHB_BL);

%WT_IPW(dataset = datPS, resp = NAS48OC, model = &model, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL FIBSCAN_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASLI_BL NASHB_BL STEATC_BL COLLAGEN_BL BMI_BL);

%WT_IPW(dataset = datPS, resp = HS48OC, model = &model, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL FIBSCAN_BL AST_BL GGT_BL PLAT_BL CY18M30_BL STEATC_BL BMI_BL);

%WT_IPW(dataset = datPS, resp = LI48OC, model = &model, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL FIBSCAN_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASLI_BL CRP_BL);

%WT_IPW(dataset = datPS, resp = HB48OC, model = &model, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL FIBSCAN_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASHB_BL CRP_BL A2MACG_BL FINSULIN_BL);


%let model = 2;
%WT_IPW(dataset = datPS, resp = PE48OC, model = &model, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASLI_BL NASHB_BL COLLAGEN_BL ALPHASMA_BL BILI_BL);

%WT_IPW(dataset = datPS, resp = NR48OC, model = &model, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASLI_BL NASHB_BL);

%WT_IPW(dataset = datPS, resp = NAS48OC, model = &model, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASLI_BL NASHB_BL STEATC_BL COLLAGEN_BL BMI_BL);

%WT_IPW(dataset = datPS, resp = HS48OC, model = &model, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL AST_BL GGT_BL PLAT_BL CY18M30_BL STEATC_BL BMI_BL);

%WT_IPW(dataset = datPS, resp = LI48OC, model = &model, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASLI_BL CRP_BL);

%WT_IPW(dataset = datPS, resp = HB48OC, model = &model, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASHB_BL CRP_BL A2MACG_BL FINSULIN_BL);



%WT_IPW(dataset = datPS, resp = PE48OC, model = 3, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL FIBSCAN_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASLI_BL NASHB_BL BILI_BL);

%WT_IPW(dataset = datPS, resp = PE48OC, model = 4, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASLI_BL NASHB_BL BILI_BL);


%WT_IPW(dataset = datPS, resp = NAS48OC, model = 3, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL FIBSCAN_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASLI_BL NASHB_BL STEATC_BL BMI_BL);

%WT_IPW(dataset = datPS, resp = NAS48OC, model = 4, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASLI_BL NASHB_BL STEATC_BL BMI_BL);


%WT_IPW(dataset = datPS, resp = HB48OC, model = 3, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL FIBSCAN_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASHB_BL CRP_BL);

%WT_IPW(dataset = datPS, resp = HB48OC, model = 4, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASHB_BL CRP_BL);


ods exclude none;
ods results; 

data outlib.PS_WT; set datPS; run;

options source notes;

