/*** I use two ways to access SAS, hence I use to libname statements***/
libname thesis "/folders/myfolders/thesis";

libname thesis 'h:/sas/Thesis';


/* The following is code to model the functional mobility (6MWT)*/

/******************************************************************************************************
********************** 					Y5(6MWT)					          ***********************
*****************************************************************************************************/

/* within-time mean and SD*/
proc means data = thesis.dataset2 n mean std;
	class timepoint;
	var X6mwt;
	output out=sumstat_6 mean=mean std=std stderr=stderr n=n;
run;

/* # of observations per timepoint - No count the observations that are missing the response*/
proc tabulate data=thesis.dataset2;
	title 'Observed times by time';
	class id timepoint;
	var X6mwt;
	table X6mwt*n*f=5., timepoint/rts=15;
run;

/*Number of Subjects by Number of Observations*/
proc freq data=thesis.dataset2 NOPRINT;
	where X6mwt~=.;
	tables ID /
	out=COUNTS(KEEP=ID COUNT RENAME=(COUNT=NOBS));  
   run;
proc tabulate data=counts;
	class nobs;
	var id;
	title "Number of Subjects by Number of Observations";
	table id*n*f=2., nobs="Number of Observations";
run;

/*stats for response per timepoint*/
proc univariate data= thesis.dataset2;
 	class timepoint;
 	VAR X6mwt;
 	histogram X6mwt/normal;  
 	title 'Univariate look at Data';
run;
   
   
   
/****************** Descriptive graphs for 6MWT ******************/
/*** Individual profiles ***/
proc sgplot data=thesis.dataset2 /*(where=(id in (34, 4, 23)))*/;
series x=timepoint y=X6mwt/group=id lineattrs=(color=black pattern=solid);
yaxis LABEL = "6MWT(m)";
run;

/*** Mean and SD ***/
proc sgplot data = thesis.dataset2;
title 'Empirical Summary Plot';	
vline timepoint / response = X6mwt stat = mean limitstat = stddev limits = both;
yaxis LABEL = "6MWT(m)";
run;
/***same as before***/
goptions reset = all;
proc gplot data = thesis.dataset2;
plot X6mwt*timepoint=id/ haxis=axis1 vaxis=axis2 nolegend;
plot2 X6mwt*timepoint/ noframe noaxis;
symbol v= none repeat=42 i=stdmj color = gray height = 2 l=2;
symbol2 v= none color = black i =stdmj width = 2;
axis1 label=("Time points");
axis2 label=(angle=90 "6MWD (meters)") value=(h=1) offset=(0.1)cm minor=none order=(5 to 28);
/*title "Max(Quadriceps) (SE) vs timepoint";*/
run;

goptions reset = all;
proc gplot data = thesis.dataset2;
where timepoint<8;
plot X6mwt*timepoint=id/ haxis=axis1 vaxis=axis2 nolegend;
plot2 X6mwt*timepoint/ noaxis;
symbol v= none repeat=42 i=j color = gray height = 2 l=2;
symbol2 v= none color = black i =stdj width = 2;
axis1 label=("Time points");
axis2 label=(angle=90 "6MWD (meters)") value=(h=1) offset=(0.1)cm minor=none;
/*title "Max(Quadriceps) (SE) vs timepoint";*/
run;

/*** Mean and SE ***/
proc sgplot data = thesis.dataset2;
title 'Empirical Summary Plot';	
vline timepoint / response = X6mwt stat = mean limitstat = stderr limits = both;
yaxis LABEL = "6MWT(m)";
run;

/*** the same as before***/
goptions reset = all;
proc gplot data = thesis.dataset2;
where timepoint<8;
plot X6mwt*timepoint=id/ haxis=axis1 vaxis=axis2 nolegend;
plot2 X6mwt*timepoint/ noaxis;
symbol v= none repeat=42 i= std1mj color = gray height = 2 l=2;
symbol2 v= none color = black i =stdmj width = 2;
axis1 label=("Time points");
axis2 label=(angle=90 "6MWD (meters)") value=(h=1) offset=(0.1)cm minor=none;
run;

/*** Mean and SE by Gender ***/
proc sgplot data = thesis.dataset2;
title 'Empirical Summary Plot, By Gender';	
vline timepoint / response = X6mwt group = gender stat = mean limitstat = stderr limits = both;
yaxis LABEL = "6MWT(m)";
run;

/*** Evolution by treatment ***/
proc sgplot data = thesis.dataset2;
title '6MWT By Treatment';	
vline timepoint / response = X6mwt group = treatment stat = mean;
yaxis LABEL = "6MWT(m)";
run;

/* Empirical standard deviations over time*/
proc gplot data=sumstat_6; /*sumstat from proc means before*/
plot std*timepoint/vaxis=axis1;
symbol1 i=spline v=circle color=black height=2 r=1;
title 'SD over time';
axis1 minor=none label=(angle=90 "SD of 6MWT(m)");
run;


/**** Correlation Structure ****/
/*sort before transposing*/
proc sort data=thesis.dataset2;
	by id;
run;
proc transpose data=thesis.dataset2 out=wide5 name=column_that_was_transposed prefix=time;
	by id;
	id timepoint;	
	var pinch_max;
run;

data wide5;
set wide5;
rename time0=time00 time1=time005 time2=time011
time3=time022 time4=time029 time5=time052 time6=time078 time7=time104 time8=time130;
run;
proc print data=wide5;run;
/* Means, Covariances, and Correlations */
/*an interesting post: https://groups.google.com/forum/#!topic/comp.soft-sys.sas/DALAsmjSskw*/
ods graphics on; 
proc corr data=wide5 plots=matrix /*NOPROB*/ /*NOSIMPLE*/  /*fisher*/ outp=CorrOutp;
	var time00--time130;
	ods select pearsoncorr; 
	ods output pearsoncorr=pearsoncorr_6; 
run;
ods graphics off;

/*** CORRELOGRAM ***/
/*Weiss lab 2 */
/*CREATE DATASET WITH ONLY THE UPPER TRIANGULAR VALUES*/
data tri (drop=i obsno variable);
	set pearsoncorr_6 (drop=ptime00--ptime130);
	obsno=_N_; *_N_ specifies the observation numbers;
	time=input(substr(variable,5,3),best.); 
	*This is used to create a time variable...
	We want the number after "time" so we go to the 5th position of the string and extract 
	the last 2 positions (the number). Then using input, we convert the number (which is a character string) 
	into a number.; *best refers to the format for the number.;
	array w{*}time00--time130;
	do i=1 to 9;
		if i<=obsno then w{i}=.;  
		end;
		*Array and do loop sets all values on and below 
		the diagonal to missing;
run;
*CREATE THE DATASET IN LONG FORM;
data trilong (keep = time wt);
	set tri;
	array w{*} time00--time130;
	do i=1 to 9;
		wt=w{i};
		output;
		end;
run;
*DELETE MISSING OBSERVATIONS;
data trilong; 
	set trilong;
	if wt=. then delete;
run;
*CREATE LAG VARIABLE;
data trilag;
	set trilong;
	by time;
	if first.time then lags=0;
	else lags+1;
run;

/*PLOT CORRELOGRAM*/
proc gplot data=trilag;
	plot wt*lags=time/vaxis=axis1 haxis=axis2 nolegend;
	title 'Correlogram';
	symbol i=join l=1 v=circle color=black r=10;
	axis1 minor=none label=(angle=90 'Correlation');
	axis2 minor=none label=('Lag(Weeks)');
run;


/*************** Step 1 ****************/
/*for OLS residuals and variogram*/
proc mixed data=thesis.piecewise;
	class gender treatment timepoint;
	model X6mwt = treatment*timepoint gender*timepoint length*timepoint age*timepoint/noint solution OUTPM=out_6 residual;
	where timepoint < 8;
run;

/*OLS residual profiles - from the model for above*/
goptions reset=all;
proc gplot data=out_6;
plot resid*timepoint=id/haxis=axis1 vaxis=axis2 vref=0 lvref=4 wvref=2 cvref= black nolegend;
symbol i=join c=gray r=42 mode=include;
axis1 minor=none label=('Time points');
axis2 minor=none label=(angle=90 'OLS Residuals') order=(-230 to 200);
/*title h=2 'OLS residual profiles - 6MWD';*/
run;


/*Semi-Variogram*/

/* Calculation of the variogram - dataset from proc mixed above (OLS residuals and variogram)*/
proc variogram data=out_6 outpair=out;
	coordinates xc=time yc=id;
	compute robust novariogram;
	var resid;
run;
data variogram;set out;
if y1=y2;vario=(v1-v2)**2/2; run;
data variance;set out;
if y1<y2; vario=(v1-v2)**2/2; run;
/* Calculation of the total variance (=4036.79) */
proc means data=variance mean;
	var vario;
run;
/* Loess smoothing of the variogram */
proc loess data=variogram;
	ods output scoreresults=out;
	model vario=distance;
	score data=variogram;
run;
proc sort data=out;by distance;run;
goptions reset=all; /*ftext=swiss device=psepsf gsfname=fig1
gsfmode=replace rotate=landscape;*/
proc gplot data=out;
plot vario*distance=1 p_vario*distance=2/ overlay haxis=axis1 vaxis=axis2 vref=4036.79 lvref=3;
symbol1 c=red v=dot h=0.2 mode=include;
symbol2 c=black i=join w=2 mode=include;
axis1 label=(h=2 "Time lag") minor=none;
axis2 label=(h=2 A=90 "v(u)") minor=none order=(0 to 9000);
/*title h=3 "Empirical Semi-variogram (6MWT)";*/
run;


/****************** COVARIANCE PATTERN MODEL On the RESIDUALS ******************/
/*** In order to choose appropriate covariance structure***/
/***************** Step 2 *****************/
/*Model A*/
proc mixed data = out_6 method = ml;
	class id gender treatment timepoint; 
	model resid = / noint s;
	repeated timepoint/ type =cs subject = id;
	where timepoint<8;
run;
/*Model B */
proc mixed data = out_6 method = ml;
	class id gender treatment timepoint; 
	model resid = / noint s;
	repeated timepoint/ type =csh subject = id;
	where timepoint<8;
run;
/*Model C */
proc mixed data = out_6 method = ml;
	class id gender treatment timepoint; 
	model resid = / noint s;
	repeated timepoint/ type =sp(gau)(time_sc) subject = id;
	where timepoint<8;
run;
/*Model D */
proc mixed data = out_6 method = ml;
	class id gender treatment timepoint; 
	model resid = / noint s;
	repeated timepoint/ type =sp(exp)(time_sc) subject = id;
	where timepoint<8;
run;


/****************** Estimation of the model on the RESPONSE ******************/
/***************** STEP 3 *****************/ 

/*Model 1*/
proc mixed data = thesis.piecewise method = ml;
	class id gender treatment timepoint; 
	model  X6mwt = treatment*timepoint gender*timepoint length*timepoint age*timepoint/ noint s ddfm=KR;
	repeated timepoint/ type =csh subject = id;
	where timepoint<8;
run;


/****************** Model Reduction ******************/
/*Model 2: no age*timepoint*/
proc mixed data = thesis.piecewise method = ml;
	class id gender treatment timepoint; 
	model X6mwt = treatment*timepoint gender*timepoint length*timepoint/ noint s ddfm=KR;
	repeated timepoint/ type =csh subject = id;
	where timepoint<8;
run;

/*Model 3: no gender*timepoint*/
proc mixed data = thesis.piecewise method = ml;
	class id gender treatment timepoint; 
	model X6mwt = treatment*timepoint length*timepoint/ noint s ddfm=KR;
	repeated timepoint/ type =csh subject = id;
	where timepoint<8;
run;

/*Model 4: no length*timepoint*/
proc mixed data =thesis.piecewise method = ml;
	class id gender treatment timepoint; 
	model X6mwt = treatment*timepoint/ noint s ddfm=KR;
	repeated timepoint/ type =csh subject = id;
	where timepoint<8;
run;

/*Model 5: no treatment*timepoint*/
proc mixed data = thesis.piecewise method = ml;
	class id gender treatment timepoint; 
	model X6mwt = length*timepoint/ noint s ddfm=KR;
	repeated timepoint/ type =csh subject = id;
	where timepoint<8;
run;


/* Model 3 has been chosen*/
proc mixed data = thesis.piecewise method = reml;
	class id gender treatment timepoint; 
	model X6mwt = treatment*timepoint length*timepoint/ noint s ddfm=KR;
	repeated timepoint/ type =csh subject = id;
	where timepoint<8;
run;


/****************** Model Checking ******************/
				/* using variance function*/
	
/*OLS residulas*/				
proc mixed data = thesis.piecewise method = ml;
	class id treatment timepoint;
	model X6mwt = treatment*timepoint length*timepoint/ noint s OUTPM=out_resid;
	where timepoint < 8;
run;

proc sort data=out_resid;
	by timepoint;
run;

/*calculate variance of residuals per timepoint*/
proc means data=out_resid var;
	by timepoint;
	var resid;
	output out=obs_var var=var;
run;

data obs_var;
	set obs_var;
	group = "obs";
run;

/* fit final mixed model - save cov parameters*/
proc mixed data = thesis.piecewise method = reml;
	class id treatment timepoint;
	model X6mwt = treatment*timepoint length*timepoint/ noint s ddfm=KR;
	repeated timepoint / type = csh subject = id;
	where timepoint < 8;
	ods output CovParms=covparms;
run;

data fitted_var;
	set covparms;
	var = estimate;
	group = "fitted";
	timepoint + 1;
  	by subject;
  	if first.subject then timepoint = 0;
run;

data variance_function;
	set fitted_var obs_var;
	if timepoint = 8 then delete;
	keep timepoint var group;
run;

goptions reset = all;
proc gplot data = variance_function;
plot var*timepoint=group /overlay haxis=axis1 vaxis=axis2 nolegend autovref;
symbol1 v=circle c=black i=j l=3 w=1.5;
symbol2 v=circle c=black i=j w=1.5;
axis1 label=(a=0 h=2 'Timepoint') minor=none;
axis2 label = (a=90 h=2 'Var(6MWD) (meters^2)') minor=none;;
run; 
quit;

proc sgplot data = variance_function;
title 'Observed - fitted variance function';	
vline timepoint / response = var group = group stat = mean limitstat = stderr limits = both;
yaxis LABEL = "Variance";
run;


/****************** Model Checking ******************/
				/* using variogram - not valid for my final model*/

/*** Observed and Fitted Variogram***/
/* OLS model*/
proc mixed data=thesis.piecewise method=reml;
	class gender treatment timepoint;
	model X6mwt = treatment*timepoint length*timepoint/noint solution OUTPM=out_model4 residual;
	where timepoint < 8;
run;
/* Calculation of the variogram - data set from OLS proc mixed previously */
proc variogram data=out_model4 outpair=out;
	coordinates xc=time yc=id;
	compute robust novariogram;
	var resid;
run;
data variogram;set out;
if y1=y2;vario=(v1-v2)**2/2; run;
data variance;set out;
if y1<y2; vario=(v1-v2)**2/2; run;
/* Calculation of the total variance (=4999.2) */
proc means data=variance mean;
	var vario;
run;
/* Loess smoothing of the variogram */
proc loess data=variogram;
	ods output scoreresults=out;
	model vario=distance;
	score data=variogram;
run;
proc sort data=out; by distance obs;run;
data out;
set out;
vario = 3565.12;
run;
goptions reset=all;
proc gplot data=out;
plot p_vario*distance/ haxis=axis1 vaxis=axis2 autovref chref=(black black) vzero vref=4999.2 legend=legend1;
plot2 vario*distance/ vaxis=axis2 haxis=axis1 vref=5883 lvref=3 legend=legend1;
symbol1 c=black i=join h=0.2 w=1 mode=include;
symbol2 c=black i=join h=0.2 w=1 l=3 mode=include;
axis1 label=(h=2 "Time lag") value=(h=1.5) minor=none;
axis2 label=(h=2 A=90 "v(u)") value=(h=1.5) minor=none order=(0 to 9000);
title h=3 "Fitted and Observed Semi-variogram";
legend1 down=2 frame label=(h=1.5 "Variogram") value=(h=1.5 "Emprirical" "Fitted") mode=protect position=(inside top right);
run;


/*Model 3 has been selected*/
/***the sample means and the estimated mean over time***/
ods output LSMeans=means5;
proc mixed data = thesis.piecewise method = reml;
	class id treatment timepoint;
	model X6mwt = treatment*timepoint length*timepoint/ noint s ddfm=KR outpredm=pred_6MWT_6;
	repeated timepoint / type = csh subject = id;
	where timepoint < 8;
	lsmeans treatment*timepoint;
	contrast "all vs VHR at T6"
	treatment*timepoint 1 0 0  /*AR1*/
						0 0   /*AR2B*/
						0 0 0  /*AR2T*/
						-1 0 0  /*VHR*/
						0 0 0 /*VLR*/
						0 0 0 0 0, /*common*/
	treatment*timepoint	0 0 0 
						1 0  
						0 0 0 
						-1 0 0
						0 0 0
						0 0 0 0 0,
	treatment*timepoint	0 0 0 
						0 0  
						1 0 0 
						-1 0 0 
						0 0 0 
						0 0 0 0 0,
	treatment*timepoint 0 0 0 
						0 0  
						0 0 0 
						-1 0 0 
						1 0 0 
						0 0 0 0/E;
						
	contrast "AR1+AR2B vs VLR+AR2T at T6, T7, T8"
	treatment*timepoint 1 0 0 /*AR1*/
						1 0      /*AR2B*/
						-1 0 0   /*AR2T*/
						0 0 0     /*VHR*/
						-1 0 0 /*VLR*/
						0 0 0 0 0,  /*common*/
	treatment*timepoint 0 1 0 
						0 0 
						0 -1 0
						0 0 0 
						0 -1 0 
						0 0 0 0 0,
	treatment*timepoint 0 0 1 
						0 1 
						0 0 -1 
						0 0 0  
						0 0 -1 
						0 0 0 0 0/E;
run;


/* http://support.sas.com/documentation/cdl/en/statug/63962/HTML/default/viewer.htm#statug_mixed_sect025.htm#statug.mixed.mixedinference*/

proc mixed data = thesis.piecewise method = reml;
	class id treatment timepoint;
	model X6mwt = treatment*timepoint length*timepoint/ noint s ddfm=KR;
	repeated timepoint / type = csh subject = id;
	where timepoint < 8;
	
	estimate "diff AR1 vs VHR at T6" 
	treatment*timepoint 1 0 0  /*AR1*/
						0 0   /*AR2B*/
						0 0 0  /*AR2T*/
						-1 0 0  /*VHR*/
						0 0 0 /*VLR*/
						0 0 0 0 0/E CL; /*common*/
	estimate "diff AR2B vs VHR at T6" 
	treatment*timepoint	0 0 0 
						1 0  
						0 0 0 
						-1 0 0
						0 0 0
						0 0 0 0 0/E CL;
	estimate "diff AR2T vs VHR at T6" 
	treatment*timepoint	0 0 0 
						0 0  
						1 0 0 
						-1 0 0 
						0 0 0 
						0 0 0 0 0/E CL;
	estimate "diff VLR vs VHR at T6" 
	treatment*timepoint 0 0 0 
						0 0  
						0 0 0 
						-1 0 0 
						1 0 0 
						0 0 0 0/E CL;
						
	estimate "AR1+AR2B vs VLR+AR2T at T6"
	treatment*timepoint 1 0 0 /*AR1*/
						1 0      /*AR2B*/
						-1 0 0   /*AR2T*/
						0 0 0     /*VHR*/
						-1 0 0 /*VLR*/
						0 0 0 0 0/ E CL;  /*common*/
	estimate "AR1+AR2B vs VLR+AR2T at T7"
	treatment*timepoint 0 1 0 
						0 0 
						0 -0.5 0
						0 0 0 
						0 -0.5 0 
						0 0 0 0 0/E Cl;
	estimate "AR1+AR2B vs VLR+AR2T at T8"				
	treatment*timepoint 0 0 1
						0 1 
						0 0 -1
						0 0 0  
						0 0 -1
						0 0 0 0 0/CL E;				
run;
	
proc sql;
 create table summary_means as
  select timepoint,
         mean(X6mwt) as sample_mean,
         mean(pred) as pred_mean
  from pred_6MWT_6
  group by timepoint;
quit;
goptions reset = all;
 symbol1 value=circle i=j c=black w=1.5 l=3;
 symbol2 value=circle i=j c=black w=1.5;
 axis1 label=(a=0 h=2 'Timepoint') minor=none;
 axis2 label = (a=90 h=2 '6MWD (meters)') minor=none order=(330 to 510 by 20) ;
proc gplot data = summary_means;
plot (sample_mean pred_mean)*timepoint /overlay haxis=axis1 vaxis=axis2 autovref;
run; 
quit;



/*estimate 6MWD for median height*/
proc mixed data = thesis.piecewise method = reml;
	class id treatment timepoint;
	model X6mwt = treatment*timepoint length*timepoint/ noint s ddfm=KR;
	repeated timepoint / type = csh subject = id;
	where timepoint < 8;
	/*lsmeans treatment*timepoint / at length=113.5;*/
	/*lsmeans treatment*timepoint / at length=142.6;*/
	lsmeans treatment*timepoint / at length=121.6 cl;
	ods output lsmeans=lsm;
run;

data lsm_2;
	set  lsm;
	output;
	if treatment="comm" then output;  
	if treatment="comm" then output;
	if treatment="comm" then output;
	if treatment="comm" then output;
run;

data lsm_3;
	set lsm_2;
	IDNew=_n_; 
	if idnew in (15,20,25,30,35) then treatment="AR1";
	if idnew in (16,21,26,31,36) then treatment="AR2B";
	if idnew in (17,22,27,32,37) then treatment="AR2T";
	if idnew in (18,23,28,33,38) then treatment="VLR";
	if idnew in (19,24,29,34,39) then treatment="VHR";
	run;

proc sort data=lsm_3;
	by timepoint;
run;

goptions reset=all;
proc gplot data = lsm_3;
plot estimate*timepoint=treatment/legend=legend1 haxis=axis1 vaxis=axis2 autovref chref=(black black) skipmiss;
/*plot2 (upper lower)*timepoint=treatment/overlay;*/
symbol1 c=black i=j l=2 v=star h=2;
symbol2 c=black i=j l=2 v=diamond  h=2; 
symbol3 c=black i=j l=2 v=square   h=2;
symbol4 c=black i=j l=2 v=traingle h=2;
symbol5 c=black i=j l=2 v=circle   h=2;
legend1 label=(h=1.5 "risk-group") value=(h=1.5);
axis2 label=(h=2 A=90 "6MWD(meters)") minor=none order=(330 to 530 by 20);
axis1 label=(h=2 "Timepoint") minor=none;
run;




/*Plotting LS means*/
/*d.f. from http://www.hsph.harvard.edu/fitzmaur/ala/lectures.pdf */
proc print data=means5;
run;
/*Creating Graphs of the Means*/
/*http://www.ats.ucla.edu/stat/sas/seminars/sas_repeatedmeasures/ */ 
goptions reset=all;
symbol1 c=blue v=star h=.8 i=j;
symbol2 c=red v=dot h=.8 i=j;
symbol3 c=green v=square h=.8 i=j;
symbol4 c=black v=square h=.8 i=j;
symbol5 c=purple v=square h=.8 i=j;
symbol5 c=brown v=square h=.8 i=j;
title "LSmeans";
proc gplot data=means5;
  plot estimate*timepoint=treatment;
run; 
quit;


/*power calculations - code from Little book*/
proc mixed data = thesis.piecewise method = reml;
	class id treatment timepoint;
	model X6mwt = treatment*timepoint length*timepoint/ noint s ddfm=KR;
	repeated timepoint / type = csh subject = id;
	where timepoint < 8;
	ods output tests3=t3;
	run;
	
data f_power;
	set t3;
	Noncen = NumDF*FValue;
	Alpha = 0.05;
	FCrit = finv(1-Alpha,NumDF,DenDF,0);
	Power = 1 - probf(FCrit,NumDF,DenDF,Noncen);
run;

proc print data=f_power;
run;

/*code from course notes*/
proc mixed data = thesis.piecewise method = reml;
	class id treatment timepoint;
	model X6mwt = treatment*timepoint length*timepoint/ noint s ddfm=KR;
	repeated timepoint / type = csh subject = id;
	where timepoint < 8;
	contrast "AR1+AR2B vs VLR+AR2T at T6, T7, T8"
	treatment*timepoint 1 0 0 /*AR1*/
						1 0      /*AR2B*/
						-1 0 0   /*AR2T*/
						0 0 0     /*VHR*/
						-1 0 0 /*VLR*/
						0 0 0 0 0,  /*common*/
	treatment*timepoint 0 1 0 
						0 0 
						0 -1 0
						0 0 0 
						0 -1 0 
						0 0 0 0 0,
	treatment*timepoint 0 0 1 
						0 1 
						0 0 -1 
						0 0 0  
						0 0 -1 
						0 0 0 0 0/E;
	ods output contrasts=c;
						run;
data power;
	set c;
	alpha=0.05;
	ncparm=numdf*fvalue;
	fc=finv(1-alpha,numdf,dendf,0);
	power=1-probf(fc,numdf,dendf,ncparm);
run;
proc print;run;
