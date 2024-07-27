dm log 'clear' output;
dm 'odsresults; clear';

libname atlas "\\gsasdata\biometrics\projects\p454\s4544378\eff_explore\final_unblinded\adata\" access = readonly;
/*libname stellar3 "\\gsasdata\biometrics\projects\p384\s3841943\final\eff_explore\adata" access = readonly;*/
/*libname stellar4 "\\gsasdata\biometrics\projects\p384\s3841944\final\eff_explore\adata" access = readonly;*/
libname stellar3 "\\gsasdata\biometrics\projects\p384\s3841943\eff_explore\draft1\adata" access = readonly;
libname stellar4 "\\gsasdata\biometrics\projects\p384\s3841944\eff_explore\draft1\adata" access = readonly;

%let outpath = P:\Xiaomin Lu\Liver\Journal Club\NASH\Propensity;
%let outfile = "&outpath\Summary - Propensity Score Matching (Continuous) 02-06-2020.xlsx";
libname outlib "&outpath";

options nosource nonotes;

data S4378; set atlas.adexp;
	keep STUDYID USUBJID ARM RANDFL SAFFL FASFL STRAT1NM STRAT1V STRAT1VN STRAT2NM STRAT2V STRAT2VN AGE BMI_BL WEIGHT_BL
		ELFSCORE_BL HYLURAC_BL PIIINP_BL TIMP1_BL FIBSCAN_BL FIBSG_BL COLLAGEN_BL ALPHASMA_BL AST_BL ALT_BL GGT_BL BILI_BL PLAT_BL CRP_BL A2MACG_BL FINSULIN_BL FBILEAC_BL 
		GFR_BL CY18M30_BL CY18M65_BL NASLI_BL NASHB_BL STEATC_BL NAS_BL PE48OC NR48OC NAS48OC HS48OC LI48OC HB48OC 
		ALT_W48 FBILEAC_W48 ELFSCORE_W48 FIBSCAN_W48 CY18M30_W48 WEIGHT_W48 FINSULIN_W48 GFR_W48
		ALT_CHG_W48 FBILEAC_CHG_W48 ELFSCORE_CHG_W48 FIBSCAN_CHG_W48 CY18M30_CHG_W48 WEIGHT_CHG_W48 FINSULIN_CHG_W48 GFR_CHG_W48
		ALT_PCHG_W48 FBILEAC_PCHG_W48 ELFSCORE_PCHG_W48 FIBSCAN_PCHG_W48 CY18M30_PCHG_W48 WEIGHT_PCHG_W48 FINSULIN_PCHG_W48 GFR_PCHG_W48;
	if ARM = "GS-0976 + GS-9674" then ARM = "CILO/FIR";
	if ARM in ("CILO/FIR", "Placebo");
run;

data S1943; set stellar3.adexp;
	keep STUDYID USUBJID ARM RANDFL SAFFL FASFL STRAT1NM STRAT1V STRAT1VN STRAT2NM STRAT2V STRAT2VN AGE BBMI WEIGHT_BL
		ELFSCORE_BL HYLURAC_BL PIIINP_BL TIMP1_BL FIBSCAN_BL FIBSG_BL COLLAGEN_BL ALPHASMA_BL AST_BL ALT_BL GGT_BL BILI_BL PLAT_BL CRP_BL A2MACG_BL FINSULIN_BL FBILEAC_BL 
		GFR_BL CY18M30_BL CY18M65_BL NASLI_BL NASHB_BL STEATC_BL NAS_BL PE48OC NR48OC NAS48OC HS48OC LI48OC HB48OC 
		ALT_W48 FBILEAC_W48 ELFSCORE_W48 FIBSCAN_W48 CY18M30_W48 WEIGHT_W48 FINSULIN_W48 GFR_W48
		ALT_CHG_W48 FBILEAC_CHG_W48 ELFSCORE_CHG_W48 FIBSCAN_CHG_W48 CY18M30_CHG_W48 WEIGHT_CHG_W48 FINSULIN_CHG_W48 GFR_CHG_W48
		ALT_PCHG_W48 FBILEAC_PCHG_W48 ELFSCORE_PCHG_W48 FIBSCAN_PCHG_W48 CY18M30_PCHG_W48 WEIGHT_PCHG_W48 FINSULIN_PCHG_W48 GFR_PCHG_W48;
	rename BBMI = BMI_BL;
	if ARM = "one PTM SEL 6 mg tablet + one PTM SEL 18 mg tablet administered orally";
	ARM = "Placebo";
run;

data S1944; set stellar4.adexp;
	keep STUDYID USUBJID ARM RANDFL SAFFL FASFL STRAT1NM STRAT1V STRAT1VN STRAT2NM STRAT2V STRAT2VN AGE BBMI WEIGHT_BL
		ELFSCORE_BL HYLURAC_BL PIIINP_BL TIMP1_BL FIBSCAN_BL FIBSG_BL COLLAGEN_BL ALPHASMA_BL AST_BL ALT_BL GGT_BL BILI_BL PLAT_BL CRP_BL A2MACG_BL FINSULIN_BL FBILEAC_BL 
		GFR_BL CY18M30_BL CY18M65_BL NASLI_BL NASHB_BL STEATC_BL NAS_BL PE48OC NR48OC NAS48OC HS48OC LI48OC HB48OC 
		ALT_W48 FBILEAC_W48 ELFSCORE_W48 FIBSCAN_W48 CY18M30_W48 WEIGHT_W48 FINSULIN_W48 GFR_W48
		ALT_CHG_W48 FBILEAC_CHG_W48 ELFSCORE_CHG_W48 FIBSCAN_CHG_W48 CY18M30_CHG_W48 WEIGHT_CHG_W48 FINSULIN_CHG_W48 GFR_CHG_W48
		ALT_PCHG_W48 FBILEAC_PCHG_W48 ELFSCORE_PCHG_W48 FIBSCAN_PCHG_W48 CY18M30_PCHG_W48 WEIGHT_PCHG_W48 FINSULIN_PCHG_W48 GFR_PCHG_W48;
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

%macro PS(dataset =, resp =, model =, grp =, var_clas =, var_num =);
	data subdat; set &dataset;
		keep USUBJID STUDYID &grp &var_clas &var_num &resp._W48 &resp._CHG_W48 &resp._pCHG_W48;
		if &resp._CHG_W48 ne '';
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
%PS(dataset = datPS, resp = ALT, model = &model, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL FIBSCAN_BL AST_BL GGT_BL PLAT_BL CY18M30_BL ALT_BL);

%PS(dataset = datPS, resp = FBILEAC, model = &model, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL FIBSCAN_BL AST_BL GGT_BL PLAT_BL CY18M30_BL FBILEAC_BL);

%PS(dataset = datPS, resp = ELFSCORE, model = &model, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL FIBSCAN_BL AST_BL GGT_BL PLAT_BL CY18M30_BL);

%PS(dataset = datPS, resp = FIBSCAN, model = &model, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL FIBSCAN_BL AST_BL GGT_BL PLAT_BL CY18M30_BL);

%PS(dataset = datPS, resp = CY18M30, model = &model, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL FIBSCAN_BL AST_BL GGT_BL PLAT_BL CY18M30_BL);

%PS(dataset = datPS, resp = WEIGHT, model = &model, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL FIBSCAN_BL AST_BL GGT_BL PLAT_BL CY18M30_BL WEIGHT_BL);

%PS(dataset = datPS, resp = FINSULIN, model = &model, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL FIBSCAN_BL AST_BL GGT_BL PLAT_BL CY18M30_BL FINSULIN_BL);

%PS(dataset = datPS, resp = GFR, model = &model, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL FIBSCAN_BL AST_BL GGT_BL PLAT_BL CY18M30_BL FINSULIN_BL GFR_BL);


%let model = 2;
%PS(dataset = datPS, resp = ALT, model = &model, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL AST_BL GGT_BL PLAT_BL CY18M30_BL ALT_BL);

%PS(dataset = datPS, resp = FBILEAC, model = &model, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL AST_BL GGT_BL PLAT_BL CY18M30_BL FBILEAC_BL);

%PS(dataset = datPS, resp = ELFSCORE, model = &model, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL AST_BL GGT_BL PLAT_BL CY18M30_BL);

%PS(dataset = datPS, resp = CY18M30, model = &model, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL AST_BL GGT_BL PLAT_BL CY18M30_BL);

%PS(dataset = datPS, resp = WEIGHT, model = &model, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL AST_BL GGT_BL PLAT_BL CY18M30_BL WEIGHT_BL);

%PS(dataset = datPS, resp = FINSULIN, model = &model, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL AST_BL GGT_BL PLAT_BL CY18M30_BL FINSULIN_BL);

%PS(dataset = datPS, resp = GFR, model = &model, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL  AST_BL GGT_BL PLAT_BL CY18M30_BL FINSULIN_BL GFR_BL);


ods exclude none;
ods results; 

proc export data = diff outfile = &outfile dbms = xlsx replace; 
	sheet = "Mean_diff";
run;

proc export data = PSinfo outfile = &outfile dbms = xlsx replace; 
	sheet = "PS_Summary";
run;


options source notes;



options source notes;

/*proc means data=datPS; var AGE BMI_BL*/
/*		ELFSCORE_BL HYLURAC_BL PIIINP_BL TIMP1_BL FIBSCAN_BL FIBSG_BL COLLAGEN_BL ALPHASMA_BL AST_BL ALT_BL GGT_BL BILI_BL PLAT_BL CRP_BL A2MACG_BL FINSULIN_BL */
/*		CY18M30_BL CY18M65_BL NASLI_BL NASHB_BL STEATC_BL NAS_BL;*/
/*run;*/

/*proc freq data=datPS; table  ARM*STUDYID SAFFL FASFL STRAT1NM STRAT1V*STRAT1VN STRAT2NM STRAT2V*STRAT2VN STRAT1V*STRAT2V ARM*STRAT1V*STRAT2V ARM*PE48OC; run;*/

