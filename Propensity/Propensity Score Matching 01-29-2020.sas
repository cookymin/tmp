dm log 'clear' output;
dm 'odsresults; clear';

libname atlas "\\gsasdata\biometrics\projects\p454\s4544378\eff_explore\final_unblinded\adata\" access = readonly;
libname stellar3 "\\gsasdata\biometrics\projects\p384\s3841943\final\eff_explore\adata" access = readonly;
libname stellar4 "\\gsasdata\biometrics\projects\p384\s3841944\final\eff_explore\adata" access = readonly;

%let outpath = P:\Xiaomin Lu\Liver\Journal Club\NASH\Propensity;
%let outfile = "&outpath\Summary - Propensity Score Matching 01-29-2020.xlsx";
libname outlib "&outpath";

options nosource nonotes;

data S4378; set atlas.adexp;
	keep STUDYID USUBJID ARM RANDFL SAFFL FASFL STRAT1NM STRAT1V STRAT1VN STRAT2NM STRAT2V STRAT2VN AGE BMI_BL
		ELFSCORE_BL HYLURAC_BL PIIINP_BL TIMP1_BL FIBSCAN_BL FIBSG_BL COLLAGEN_BL ALPHASMA_BL AST_BL ALT_BL GGT_BL BILI_BL PLAT_BL CRP_BL A2MACG_BL FINSULIN_BL 
		CY18M30_BL CY18M65_BL NASLI_BL NASHB_BL STEATC_BL NAS_BL PE48OC NR48OC NAS48OC HS48OC LI48OC HB48OC;
	if ARM = "GS-0976 + GS-9674" then ARM = "CILO/FIR";
	if ARM in ("CILO/FIR", "Placebo");
*	if PE48OC ne "";
run;

data S1943; set stellar3.adexp;
	keep STUDYID USUBJID ARM RANDFL SAFFL FASFL STRAT1NM STRAT1V STRAT1VN AGE BBMI
		ELFSCORE_BL HYLURAC_BL PIIINP_BL TIMP1_BL FIBSCAN_BL FIBSG_BL COLLAGEN_BL ALPHASMA_BL AST_BL ALT_BL GGT_BL BILI_BL PLAT_BL CRP_BL A2MACG_BL FINSULIN_BL 
		CY18M30_BL CY18M65_BL NASLI_BL NASHB_BL STEATC_BL NAS_BL PE48OC NR48OC NAS_CHG_W48 STEATC_CHG_W48 NASLI_CHG_W48 NASHB_CHG_W48;
	rename BBMI = BMI_BL;
	if ARM = "one PTM SEL 6 mg tablet + one PTM SEL 18 mg tablet administered orally";
	ARM = "Placebo";
run;

data S1944; set stellar4.adexp;
	keep STUDYID USUBJID ARM RANDFL SAFFL FASFL STRAT1NM STRAT1V STRAT1VN AGE BBMI
		ELFSCORE_BL HYLURAC_BL PIIINP_BL TIMP1_BL FIBSCAN_BL FIBSG_BL COLLAGEN_BL ALPHASMA_BL AST_BL ALT_BL GGT_BL BILI_BL PLAT_BL CRP_BL A2MACG_BL FINSULIN_BL 
		CY18M30_BL CY18M65_BL NASLI_BL NASHB_BL STEATC_BL NAS_BL PE48OC NR48OC NAS_CHG_W48 STEATC_CHG_W48 NASLI_CHG_W48 NASHB_CHG_W48;
	rename BBMI = BMI_BL;
	if ARM = "one PTM SEL 6 mg tablet + one PTM SEL 18 mg tablet administered orally";
	ARM = "Placebo";
run;

data datPS;	set S4378 S1943 S1944; run;
data datPS; set datPS; 
	if STUDYID in ("GS-US-384-1943" "GS-US-384-1944") then do;
		STRAT2NM = "Cirrhosis Status";
		if FIBSG_BL = 4 then do; STRAT2VN = 1; STRAT2V = "Cirrhosis (F4)"; end;
		if FIBSG_BL < 4 then do; STRAT2VN = 2; STRAT2V = "Non-cirrhosis (Not F4)"; end;

		**** NAS Response *****;
		if NAS_CHG_W48 ne . and NAS_BL >= 2 then do;
		    if NAS_CHG_W48 < -1 then NAS48OC = 'Y';
		    else NAS48OC = 'N';
		end;

		***** steatosis improvement ******;
		if STEATC_CHG_W48 ne . and STEATC_BL >= 1 then do;
			if STEATC_CHG_W48 < 0 then HS48OC = 'Y';
		    else HS48OC = 'N';
		end;

		***** inflammation improvement ******;
		if NASLI_CHG_W48 ne . and NASLI_BL >= 1 then do;
		 	if NASLI_CHG_W48 < 0 then LI48OC = 'Y';
		    else LI48OC = 'N';
		end;

		***** ballooning improvement ******;
		if NASHB_CHG_W48 ne . and NASHB_BL >= 1 then do;
		    if NASHB_CHG_W48 < 0 then HB48OC = 'Y';
		    else HB48OC = 'N';
		end;
	end;
run;
data outlib.datPS; set datPS; run;

/*%let resp = PE48OC; %let grp = ARM; %let var_clas = STRAT1V STRAT2V; %let var_num = AGE BMI_BL;*/

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

%PS(dataset = datPS, resp = NR48OC, model = &model, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL FIBSCAN_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASLI_BL NASHB_BL);

%PS(dataset = datPS, resp = NAS48OC, model = &model, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL FIBSCAN_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASLI_BL NASHB_BL STEATC_BL COLLAGEN_BL BMI_BL);

%PS(dataset = datPS, resp = HS48OC, model = &model, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL FIBSCAN_BL AST_BL GGT_BL PLAT_BL CY18M30_BL STEATC_BL BMI_BL);

%PS(dataset = datPS, resp = LI48OC, model = &model, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL FIBSCAN_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASLI_BL CRP_BL);

%PS(dataset = datPS, resp = HB48OC, model = &model, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL FIBSCAN_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASHB_BL CRP_BL A2MACG_BL FINSULIN_BL);


%let model = 2;
%PS(dataset = datPS, resp = PE48OC, model = &model, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASLI_BL NASHB_BL COLLAGEN_BL ALPHASMA_BL BILI_BL);

%PS(dataset = datPS, resp = NR48OC, model = &model, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASLI_BL NASHB_BL);

%PS(dataset = datPS, resp = NAS48OC, model = &model, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASLI_BL NASHB_BL STEATC_BL COLLAGEN_BL BMI_BL);

%PS(dataset = datPS, resp = HS48OC, model = &model, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL AST_BL GGT_BL PLAT_BL CY18M30_BL STEATC_BL BMI_BL);

%PS(dataset = datPS, resp = LI48OC, model = &model, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASLI_BL CRP_BL);

%PS(dataset = datPS, resp = HB48OC, model = &model, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASHB_BL CRP_BL A2MACG_BL FINSULIN_BL);




%PS(dataset = datPS, resp = PE48OC, model = 3, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL FIBSCAN_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASLI_BL NASHB_BL BILI_BL);

%PS(dataset = datPS, resp = PE48OC, model = 4, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASLI_BL NASHB_BL BILI_BL);


%PS(dataset = datPS, resp = NAS48OC, model = 3, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL FIBSCAN_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASLI_BL NASHB_BL STEATC_BL BMI_BL);

%PS(dataset = datPS, resp = NAS48OC, model = 4, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASLI_BL NASHB_BL STEATC_BL BMI_BL);


%PS(dataset = datPS, resp = HB48OC, model = 3, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL FIBSCAN_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASHB_BL CRP_BL);

%PS(dataset = datPS, resp = HB48OC, model = 4, grp = ARM, var_clas = STRAT1V STRAT2V, 
	var_num = AGE ELFSCORE_BL AST_BL GGT_BL PLAT_BL CY18M30_BL NASHB_BL CRP_BL);



ods exclude none;
ods results; 

proc export data = diff outfile = &outfile dbms = xlsx replace; 
	sheet = "Mean_diff";
run;

proc export data = PSinfo outfile = &outfile dbms = xlsx replace; 
	sheet = "PS_Summary";
run;



options source notes;

/*proc means data=datPS; var AGE BMI_BL*/
/*		ELFSCORE_BL HYLURAC_BL PIIINP_BL TIMP1_BL FIBSCAN_BL FIBSG_BL COLLAGEN_BL ALPHASMA_BL AST_BL ALT_BL GGT_BL BILI_BL PLAT_BL CRP_BL A2MACG_BL FINSULIN_BL */
/*		CY18M30_BL CY18M65_BL NASLI_BL NASHB_BL STEATC_BL NAS_BL;*/
/*run;*/

/*proc freq data=datPS; table  ARM*STUDYID SAFFL FASFL STRAT1NM STRAT1V*STRAT1VN STRAT2NM STRAT2V*STRAT2VN STRAT1V*STRAT2V ARM*STRAT1V*STRAT2V ARM*PE48OC; run;*/

