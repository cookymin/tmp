dm log 'clear' output;
dm 'odsresults; clear';

%let outpath = \\gsasdata\biometrics\projects\p454\s4544378\final\version1\statprog\Xiaomin Lu;
libname outlib "&outpath";
%let outpath1 = P:\Xiaomin Lu\Liver\Journal Club\NASH\Propensity;
libname outlib1 "&outpath1";
%macro resp(resp = );
	title "&resp._1";
	proc sort data=outlib.&resp._1 out=dat1; by USUBJID; run;
	proc sort data=outlib1.&resp._1 out=dat2; by USUBJID; run;
	proc compare base=dat1 compare=dat2; id USUBJID; run;
	proc freq data=dat1; table arm*&resp/nopercent nocol; run;

	title "&resp._2";
	proc sort data=outlib.&resp._2 out=dat1; by USUBJID; run;
	proc sort data=outlib1.&resp._2 out=dat2; by USUBJID; run;
	proc compare base=dat1 compare=dat2; id USUBJID; run;
	proc freq data=dat1; table arm*&resp/nopercent nocol; run;
%mend;
%macro resp1(resp = );
	title "&resp._3";
	proc sort data=outlib.&resp._3 out=dat1; by USUBJID; run;
	proc sort data=outlib1.&resp._3 out=dat2; by USUBJID; run;
	proc compare base=dat1 compare=dat2; id USUBJID; run;
	proc freq data=dat1; table arm*&resp/nopercent nocol; run;

	title "&resp._4";
	proc sort data=outlib.&resp._4 out=dat1; by USUBJID; run;
	proc sort data=outlib1.&resp._4 out=dat2; by USUBJID; run;
	proc compare base=dat1 compare=dat2; id USUBJID; run;
	proc freq data=dat1; table arm*&resp/nopercent nocol; run;
%mend;

%macro resp3(resp = );
	title "&resp._1";
	proc sort data=outlib.&resp._1 out=dat1; by USUBJID; run;
	proc sort data=outlib1.&resp._1 out=dat2; by USUBJID; run;
	proc compare base=dat1 compare=dat2; id USUBJID; run;
	proc glm data=dat1;
		class arm (ref = "Placebo") STRAT1V STRAT2V;
		model &resp._CHG_W48 = arm STRAT1V STRAT2V &resp._BL / solution;
		lsmeans arm/ pdiff cl;
	quit;

	title "&resp._2";
	proc sort data=outlib.&resp._2 out=dat1; by USUBJID; run;
	proc sort data=outlib1.&resp._2 out=dat2; by USUBJID; run;
	proc compare base=dat1 compare=dat2; id USUBJID; run;
	proc glm data=dat1;
		class arm (ref = "Placebo") STRAT1V STRAT2V;
		model &resp._CHG_W48 = arm STRAT1V STRAT2V &resp._BL / solution;
		lsmeans arm/ pdiff cl;
	quit;
%mend;

/*%resp(resp = PE48OC);*/
/*%resp(resp = NR48OC);*/
/*%resp(resp = NAS48OC);*/
/*%resp(resp = HS48OC);*/
/*%resp(resp = LI48OC);*/
/*%resp(resp = HB48OC);*/
/*%resp1(resp = PE48OC);*/
/*%resp1(resp = NAS48OC);*/
/*%resp1(resp = HB48OC);*/

%resp3(resp = ALT);
%resp3(resp = FBILEAC);
%resp3(resp = ELFSCORE );
%resp3(resp = FIBSCAN );
%resp3(resp = CY18M30);
%resp3(resp = WEIGHT);
%resp3(resp = FINSULIN);
%resp3(resp = GFR);


%let resp = ALT;
	proc means data=S4378 N mean std min q1 median q3 max; class arm; var &resp._W48 &resp._CHG_W48 &resp._PCHG_W48; run;

/*ods output parameterestimates=xpval lsmeancl= cl lsmeandiffcl=diff;*/
	proc glm data=outlib.&resp._1;
		class arm (ref = "Placebo") STRAT1V;
		model &resp._CHG_W48 = arm STRAT1V &resp._BL / solution;
		lsmeans arm/ pdiff cl;
	run;

