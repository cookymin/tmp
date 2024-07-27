/*dm log 'clear' output;*/
/*dm 'odsresults; clear';*/

%let path = G:\projects\p428\s4284025\cdp;

libname raw "&path\rawdata";
%let outfile = "P:\Xiaomin Lu\Liver\Liver Fibrosis\FXR PBC-PSC\GS9674_Liver_Fibrosis_428-4025\For New PSC Study\Current Data\FXR 428-4025 Dose Reduction as of 12-05-2018.xlsx";

options nosource nonotes;
**************************************************************
*                  Study/Drug Completion                     *
**************************************************************;
data BD_comp; length SubjID $30.; set raw.sdrgcomp; 
	ComT01FL = SDRGYN_STD;
	keep Subjid ComT01FL;
run;

data OL_comp; length SubjID $30.; set raw.sdrgcomp1; 
	ComT02FL = SDRGYN_STD;
	keep Subjid ComT02FL SDRGREAS;
	rename SDRGREAS = DCT02RS;
run;

* Study completion;
data study_comp; length SubjID $30.; set raw.studcomp; 
	ComSFL = COMPYN_STD;
	keep Subjid ComSFL COMPREAS;
	rename COMPREAS = DCSREAS;
run;
proc sort data=BD_comp; by SubjID; run;
proc sort data=OL_comp; by SubjID; run;
proc sort data=study_comp; by SubjID; run;
data disc; merge BD_comp OL_comp study_comp; by SubjID; run;
proc sort data=disc out=unidisc nodupkey; by SubjID; run;
data disc; set disc; 
	if ComT01FL = 'N' or ComT02FL = 'N' or ComSFL = 'N';
run; 

data expo; format SubjID $30. Arm $10.; set raw.Ixrsdrug;  
	if TrtGrpc = 1 then do; Arm = "30 mg"; dose_assg=30; end;
  	if TrtGrpc = 2 then do; Arm = "100 mg"; dose_assg=100; end;
	if TrtGrpc ne 3;
	dose_recv = input(scan(DDDosage, 1, " "),8.); 
	if find(DDadescr, "Open Label") = 1 then dose_assg=100;
	keep SubjID Arm Visit VisitDTN TrtGrpc DaDescr DDadescr DDDosage dose_assg dose_recv; 
run;
proc sort data=expo; by SubjID VisitDTN; run;

data expo; merge expo BD_comp OL_comp Study_comp; by SubjID;
run;

data BD_OL_expo; set expo;
	if find(DDadescr, "Open Label") = 0 and find(DaDescr, "Placebo to Match") = 0;
	drop ComT02FL DCT02RS;
run;

data OL_expo; set expo;
	if find(DDadescr, "Open Label") = 1;
	drop ComT01FL DCT01RS;
run;

title "Dose Reduction in BD phase";
proc print data=BD_OL_expo; where dose_assg > dose_recv; run;

proc sort data=OL_expo out = OLSubj(keep = SubjID) nodupkey; where dose_assg > dose_recv; by SubjID; run;
data OL_dosered; 
	merge OL_expo (in = a) OLSubj (in = b);
	by SubjID;
	if b;
run;

title "Dose Reduction in OL phase - Subject";
proc print data=OLSubj; run;

title "Dose Reduction in OL phase - Subject and visit";
proc print data=OL_dosered; run;

/*proc export data = work.OL_dosered outfile = &outfile dbms = Excel replace; */
/*	sheet = "Dose_red_OL";*/
/*run;*/
proc export data=OL_dosered dbms=xlsx
	outfile = &outfile
	replace;
run;



proc sort data=raw.AE 
	out = tmpAE (keep = SubjID Folder AEterm AEser_std AESTDat AESTDat_raw AESTDat_YY AESTDat_MM AESTDat_DD AESTDat AEENDat AEENDat_raw AEENDat_YY AEENDat_MM AEENDat_DD 
			AEongo AErel AErel_std AErel1 AErel1_std AErel2 AErel2_std AErelPrc_std AEacn AEacn_std AEacn1 AEacn1_std AEacnOth1 AEacnOth2 AEacnOth4 
			AEacnOth5 AEtoxGr AEtoxGr_std AESDth_std AESLife_std AESHosp_std AEHSTDat AEHSTDat_YY AEHSTDat_MM AEHENDat AEHENDat_YY AEHENDat_MM
			AESDisab_std AESCong_std AESMie_std AEAltCau AEDiseas AEPreCon AEInter AECM AECMspec AEOthSp AEout AEout_std
			Diction Dic_ver MdrLLTC MdrLLT MdrPTC MdrPT MdrHLTC MdrHLT MdrHLGTC MdrHLGT MdrSOCC MdrSOC); 
	by SubjID;  
run;

data AE; format SubjID $30.; 
	merge tmpAE (in = a) OLSubj (in = b);
	by SubjID;
	if a and b;
	if AEacn_std = 4 or AEacn1_std = 4;
run;

options source notes;

