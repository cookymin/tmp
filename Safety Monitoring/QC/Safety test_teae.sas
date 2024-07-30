libname safty "P:\Safety Signal Detection";

proc sort data=safty.adsl_ex(where=(trt01a ^= "")) nodupkey out=adsl;
   by usubjid;
run;
proc sort data=safty.adae_ex(where=(trtemfl = 'Y')) out=adae;
   by usubjid;
run;

/*Check if AE onset prior to 1st dosing and continue over dosing period, and have grade change, is separated to 2 AEs
data one;
   set adae(where=(astdt<trtsdt));
   ind=1;
run;
proc sort data=one;
   by usubjid aebodsys aedecod;
run;
proc sort data=adae;
   by usubjid aebodsys aedecod;
run;
data adae2;
   set adae;
   ind=0;
run;
data two2;
   merge one(in=a keep=usubjid aebodsys aedecod ind)
         adae2(in=b);
   by usubjid aebodsys aedecod;
   if a;
run;*/

data combine(where=(trt01a not in ("Not Treated","Screen Failure")));
   merge adsl adae;
   by usubjid;
run;
/*number of subjects in each treatment group*/
proc sort data=combine nodupkey out=trt;
  by usubjid;
run;
proc freq data=trt;
   table trt01a;
run;
proc sort data=combine out=pterm(keep=aebodsys aedecod where=(aedecod ne "")) nodupkey;
   by aebodsys aedecod;
run;
proc sort data=pterm;
   by aedecod;
run;

proc sql noprint;
   select count(distinct aedecod)
      into :n
      from pterm;
   select distinct aedecod
      into :pt1 - :pt%left(&n)
      from pterm;
quit;

%macro signal;
%do i=1 %to &n;
  data temp;
     set combine;
	 if aedecod="&&pt&i" then do;
	   if astdt>=trtsdt then ind=1;
       else ind=0;
       output temp;
	 end;
  run;
  proc sort data=temp;
     by usubjid ind;
  run;
  data temp_sub;/*subjects who has all AEs occurring prior to first dose*/  
     set temp;
	 by usubjid ind;
	 if last.usubjid and ind=0 then output;
  run;
  proc sql noprint;
     select count(usubjid) into: sub1 from temp_sub where temp_sub.trt01a="Treatment";
     select count(usubjid) into: sub2 from temp_sub where temp_sub.trt01a="Placebo";
  quit;
  /*if there is any subject whose AEs are ALL prior to 1st dose then exposure and logrank are not calculated*/
  %if &sub1 = 0 %then %do;
  proc sort data=temp(where=(trt01a="Treatment")) out=temp_t;
     by usubjid astdt;
  run;
   data pt_t;
   set temp_t(where=(ind=1));
   by usubjid;
   if first.usubjid then output;
  run;
/*For subjects with the specific AE, duration is first AE start date to the 1st dosing date*/
  data pt2_t;
   set pt_t;
   dur = (astdt - trtsdt + 1)/365.25;
   cnsr = 0;
  run;
/*For subjects without the specific AE, duration is the 1st dosing date to the last dose plus 30*/
  data ptno_t;
   merge pt2_t(in=a) combine(in=b where=(trt01a="Treatment"));
   by usubjid;
   if a=0 and b=1;
   dur = (trtedt + 30 - trtsdt + 1)/365.25; 
   cnsr = 1;
  run;
  proc sort data=ptno_t nodupkey out=ptno2_t;
   by usubjid dur;
  run;
  data pt_both_t;
   set pt2_t ptno2_t;
  run;
  proc sort data=pt_both_t;
   by trt01a;
  run;
  data pt_both2_t;
   retain total;
   set pt_both_t;
   by trt01a;
   if first.trt01a then total=dur;
   else total=total+dur;
   if last.trt01a then output;
  run;
  data exadj_t&i;
    length pt $200.;
    set pt_both2_t(keep=trt01a total);
    pt = "&&pt&i";
  run;
%end;
 %else %if &sub1 > 0 %then %do;
  data exadj_t&i;
    length pt $200.;
    trt01a="Treatment";
    pt = "&&pt&i";
	total = .;
  run;
 %end;
 %if &sub2 = 0 %then %do;
  proc sort data=temp(where=(trt01a="Placebo")) out=temp_p;
     by usubjid astdt;
  run;
   data pt_p;
   set temp_p(where=(ind=1));
   by usubjid;
   if first.usubjid then output;
  run;
/*For subjects with the specific AE, duration is first AE start date to the 1st dosing date*/
  data pt2_p;
   set pt_p;
   dur = (astdt - trtsdt + 1)/365.25;
   cnsr = 0;
  run;
/*For subjects without the specific AE, duration is the 1st dosing date to the last dose plus 30*/
  data ptno_p;
   merge pt2_p(in=a) combine(in=b where=(trt01a="Placebo"));
   by usubjid;
   if a=0 and b=1;
   dur = (trtedt + 30 - trtsdt + 1)/365.25; 
   cnsr = 1;
  run;
  proc sort data=ptno_p nodupkey out=ptno2_p;
   by usubjid dur;
  run;
  data pt_both_p;
   set pt2_p ptno2_p;
  run;
  proc sort data=pt_both_p;
   by trt01a;
  run;
  data pt_both2_p;
   retain total;
   set pt_both_p;
   by trt01a;
   if first.trt01a then total=dur;
   else total=total+dur;
   if last.trt01a then output;
  run;
  data exadj_p&i;
    length pt $200.;
    set pt_both2_p(keep=trt01a total);
    pt = "&&pt&i";
  run;
%end;
 %else %if &sub2 > 0 %then %do;
  data exadj_p&i;
    length pt $200.;
    trt01a="Placebo";
    pt = "&&pt&i";
	total = .;
  run;
 %end;
 data exadj&i;
    set exadj_t&i exadj_p&i;
 run;
/*Log rank test and */
 %if &sub1=0 and &sub2=0 %then %do;
 data pt_both;
    set pt_both_t pt_both_p;
 run;
  ods output hazardratios=hazard;
  proc phreg data=pt_both;
    class trt01a(descending);
    model dur*cnsr(1) = trt01a;
	hazardratio trt01a;
  run;
  ods output;
  data hr&i;
   length pt $200.;
   set hazard;
   pt = "&&pt&i";
   Rename WaldLower = HR_Lower WaldUpper = HR_Upper;
   drop Description;
  run;
  ods output homtests=log HomStats=stat;
  proc lifetest data=pt_both;
   time dur*cnsr(1);
   strata trt01a/test=logrank;
  run;
  ods output close;
  proc sql noprint;
     select logrank into: rank from stat where stat.trt01a="Treatment";
  quit;
  data logrank&i;
   length pt $200.;
   set log; 
   pt = "&&pt&i";
   if &rank < 0 then logrank = 1-ProbChiSq / 2;
   else if &rank>0 then logrank = ProbChiSq / 2;
   keep pt logrank;
  run;
 %end;
  
 %else %if &sub1 > 0 or &sub2 > 0 %then %do;
  data logrank&i;
   length pt $200.;
   pt = "&&pt&i";
   logrank = .;
  run;
  data hr&i;
   length pt $200.;
   pt = "&&pt&i";
   hazardratio = .;
   HR_Lower = .;
   HR_Upper = .;
  run;
 %end;
  %if &i=1 %then %do;
 	data exadj;
	   set exadj&i;
    run;
	data logrank;
	   set logrank&i;
	run;
	data hr;
	   set hr&i;
	run;
  %end;
  %else %do;
	proc append base=exadj data=exadj&i;
    run;
	proc append base=logrank data=logrank&i;
	run;
	proc append base=hr data=hr&i;
	run;
  %end;
%end;
%mend signal;

%signal;

proc format;
   value $trt(DEFAULT=50)
         'Treatment'='trt1'
		 'Placebo'='trt2';
run;

%macro freq(stat=);
%do i=1 %to &n;
  data temp1;
     set combine;
     if aedecod="&&pt&i" then cnsr = 0;
	 else cnsr=1;
  run;
  proc sort data=temp1;
     by usubjid cnsr;
  run;
  data temp2;
     format trt01a $trt.;
     set temp1;
	 by usubjid cnsr;
	 if first.usubjid then output;
   run;
  proc sort data=temp2 nodupkey;
   by usubjid cnsr;
  run;
  ods output FishersExact=Fisher CrossTabFreqs=count RiskDiffCol1=risk;
  proc freq data=temp2 order=formatted;
   tables trt01a*cnsr/fisher cmh riskdiff;
  run;
  ods output close;
  data risk;
   set risk;
   if row = "Difference";
   rename LowerCL = RD_Lower UpperCL = RD_Upper;
  run;
  data fish&i;
     length pt $200.;
     set fisher(where=(Name1 = "&stat"));
	 pt = "&&pt&i";
   run;
  data freq&i;
     length pt $200.;
     set count(where=(cnsr ^=1 and _TYPE_ in ("11","10")));
	 pt = "&&pt&i";
	 keep pt trt01a frequency;
   run;
   data rd&i;
      length pt $200.;
	  set risk(where=(row="Difference"));
	  pt = "&&pt&i";
	  keep pt Risk RD_Lower RD_Upper;
	run;
  %if &i=1 %then %do;
 	data fish;
	   set fish&i;
    run;
	data freq;
	   set freq&i;
	run;
	data rd;
	   set rd&i;
	run;
  %end;
  %else %do;
	proc append base=fish data=fish&i;
    run;
	proc append base=freq data=freq&i;
	run;
	proc append base=rd data=rd&i;
	run;
  %end;
%end;
%mend freq;
%freq(stat=XPR_FISH);

proc sort data=exadj;
   by pt descending trt01a;
run;
proc transpose data=exadj out=exadj2;
   by pt;
run;
data exadj3;
   set exadj2;
   rename col1 = ex_trt col2 = ex_plc;
   drop _name_;
run; 
proc transpose data=freq out=freq2;
   by pt;
run;
data freq3;
   set freq2;
   rename col1 = n_trt col2 = t_trt col3 = n_plc col4 = t_plc;
   pct_trt = col1/col2;
   pct_plc = col3/col4;
   drop _name_ _label_;
run;
data adjust;
   merge exadj3(in=a) freq3(in=b);
   by pt;
   if a and b;
   eair_plc = n_plc/ex_plc;
   eair_trt = n_trt/ex_trt;
   plc_low = cinv(0.025,n_plc*2)/(2*ex_plc);
   plc_upp = cinv(0.975,(n_plc+1)*2)/(2*ex_plc); 
   trt_low = cinv(0.025,n_trt*2)/(2*ex_trt);
   trt_upp = cinv(0.975,(n_trt+1)*2)/(2*ex_trt); 
run;

/*Odds ratio/relative risk, zero cell will use 0.5 correction on every cell*/
proc sort data=combine nodupkey out=combine2;
   by usubjid aedecod;
run;
ods output crosstabfreqs=count;
proc freq data=combine2;
   tables aedecod*trt01a;
run;
ods output close;

data zero;
   set count(where=(trt01a ^= "" and aedecod ^= ""));
   if frequency = 0;
run;
proc sort data=zero;
   by aedecod;
run;
proc sort data=count;
   by aedecod trt01a;
run;
data count2;
   merge count(in=a where=(trt01a ^= "" and aedecod ^= ""))
         zero(in=b keep=aedecod);
   by aedecod;
   if b=1 then do;
      resp=frequency+0.5;
	  if trt01a = "Treatment" then noresp = 110 - resp + 1;
	  else if trt01a = "Placebo" then noresp = 108 - resp + 1;
   end;
   else if b=0 then do;
      resp = frequency;
	  if trt01a = "Treatment" then noresp = 110 - resp;
	  else if trt01a = "Placebo" then noresp = 108 - resp;
   end;
run;
proc transpose data=count2(keep=aedecod trt01a resp noresp) out=count3;
   by aedecod trt01a;
run;

data count4;
   format trt01a $trt.;
   set count3;
   if _name_='resp' then response=0;
   else response=1;
run;
ods output CrossTabFreqs=freq_ OddsRatioCLs=or RelativeRiskCLs=rr;
proc freq data=count4 order=formatted;
   tables trt01a*response/OR(cl=(WALD)) RELRISK(cl=WALD);
   weight col1;
   by aedecod;
run;
ods output close;
data or;
   set or;
   rename aedecod = pt LowerCL = OR_LowCL UpperCL = OR_UpperCL;
   drop table type;
run;
data rr;
   set rr;
   rename aedecod = pt LowerCL = RR_LowCL UpperCL = RR_UpperCL;
   drop table type column;
run;
data all;
   merge adjust(in=a)
         logrank(in=b)
		 hr(in=c)
		 fish(in=d)
		 rd(in=e)
         or(in=f)
         rr(in=g);
   by pt;
   if a and b and c and d and e and f and g;
run;
PROC EXPORT DATA= WORK.all 
            OUTFILE= "P:\Safety Signal Detection\Stat_teae.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
PROC IMPORT OUT= WORK.all 
            DATAFILE= "P:\Safety Signal Detection\Stat_teae.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
PROC IMPORT OUT= WORK.result 
            DATAFILE= "P:\Safety Signal Detection\Output_20171110_TEAE.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
proc sort data=result;
   by pt;
run;

proc compare CRITERION=0.00015 method=absolute
             data=all(drop = Table Name1 Label1 cValue1 )
             compare=result(drop= soc EAIR_test_p_value Landmark_KM_test_p_value
			                      DFDR_Fisher_exact_test_p_value DFDR_EAIR_test_p_value DFDR_Logrank_test_p_value DFDR_Landmark_KM_test_p_value
                            rename=(No__of_Subjects_w_Events__Treatm = n_trt 
                                    No__of_Subjects_w_Events__Contro = n_plc
                                    Total_No__of_Subjects__Treatment = t_trt 
                                    Total_No__of_Subjects__Control_ = t_plc
									Incidence_Rate__Treatment_ = pct_trt
									Incidence_Rate__Control_ = pct_plc
									EAIR__Treatment_ = eair_trt
									EAIR__Treatment__lower = trt_low
									EAIR__Treatment__upper = trt_upp
									EAIR__Control_ = eair_plc
									EAIR__Control__lower = plc_low
									EAIR__Control__upper = plc_upp
                                    Drug_Exposure_at_Risk__Treatment = ex_trt 
                                    Drug_Exposure_at_Risk__Control_ = ex_plc
									Hazard_ratio = HazardRatio 
                                    Hazard_ratio_lower = HR_Lower 
                                    Hazard_ratio_upper = HR_Upper
                                    Fisher_exact_test_p_value = nValue1
                                    Risk_difference = risk
                                    Risk_difference_lower = RD_Lower
                                    Risk_difference_upper = rd_Upper
                                    Logrank_test_p_value=logrank
									Odds_ratio=OddsRatio
                                    Odds_ratio_lower=OR_LowCL
                                    Odds_ratio_upper=OR_UpperCL
									Relative_risk=RelativeRisk
                                    Relative_risk_lower=RR_LowCL
                                    Relative_risk_upper=RR_UpperCL));
   id pt;
run;

