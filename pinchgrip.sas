/*** I use two ways to access SAS, hence I use to libname statements***/
libname thesis "/folders/myfolders/thesis";

libname thesis 'h:/sas/Thesis';


/* The following is code to model the pinchgrip strength (univarietly)*/


/******************************************************************************************************
********************** 					Y4(Pinchgrip)					        ***********************
*****************************************************************************************************/

/* within-time mean and SD*/
proc means data = thesis.dataset2 n mean std;
	class timepoint;
	var pinch_max;
	output out=sumstat_p mean=mean std=std stderr=stderr n=n;
run;

/* # of observations per timepoint - No count the observations that are missing the response*/
proc tabulate data=thesis.dataset2;
	title 'Observed times by time';
	class id timepoint;
	var pinch_max;
	table pinch_max*n*f=5., timepoint/rts=15;
run;

/*Number of Subjects by Number of Observations*/
proc freq data=thesis.dataset2 NOPRINT;
	where pinch_max~=.;
	tables ID /
	out=COUNTS(KEEP=ID COUNT RENAME=(COUNT=NOBS));  
   run;
proc tabulate data=counts;
	class nobs;
	var id;
	title "Number of Subjects by Number of Observations";
	table id*n*f=2., nobs="Number of Observations";
run;


/****************** Descriptive graphs for pinch_max ******************/
/*** Individual profiles ***/
proc sgplot data=thesis.dataset2 /*(where=(id in (34, 4, 23)))*/;
series x=timepoint y=pinch_max/group=id lineattrs=(color=black pattern=solid);
yaxis LABEL = "Max(Pinchgrip)(kg)";
run;

/*** Mean and SD ***/
proc sgplot data = thesis.dataset2;
title 'Empirical Summary Plot';	
vline timepoint / response = pinch_max stat = mean limitstat = stddev limits = both;
yaxis LABEL = "Max(Pinchgrip)(kg)";
run;

/*** Mean and SE ***/
proc sgplot data = thesis.dataset2;
title "Max(Pinchgrip)(SE) vs Timepoint";	
vline timepoint / response = pinch_max stat = mean limitstat = stderr limits = both;
yaxis LABEL = "Max(Pinchgrip)(kg)";
run;

goptions reset = all;
proc gplot data = thesis.dataset2;
where timepoint < 8;
plot pinch_max*timepoint=id/ haxis=axis1 vaxis=axis2 nolegend;
plot2 pinch_max*timepoint/ noaxis;
symbol v = none repeat = 42 i = std1mj color = gray height = 2 l = 2;
symbol2 v= none color = black i = stdmj width = 2;
axis1 label=("Time point") minor = none;
axis2 label=(angle=90 "Pinch grip (kg)") value=(h=1) offset=(0.1)cm;
run;

/*** Mean and SE by Gender ***/
proc sgplot data = thesis.dataset2;
title 'Empirical Summary Plot, By Gender';	
vline timepoint / response =pinch_max group = gender stat = mean limitstat = stderr limits = both;
yaxis LABEL = "Max(Pinchgrip)(kg)";
run;

/*** Evolution by treatment ***/
proc sgplot data = thesis.dataset2;
title 'Empirical Summary Plot, By Treatment';	
vline timepoint / response =pinch_max group = treatment stat = mean;
yaxis LABEL = "Max(Pinchgrip)(kg)";
run;

/* Empirical standard deviations over time*/
proc gplot data=sumstat_p; /*sumstat from proc means before*/
plot std*timepoint/vaxis=axis1;
symbol1 i=none v=circle color=black height=2 r=1 ;
title 'SD over time';
axis1 minor=none label=(angle=90 'SD of Max(Pinchgrip)(kg)');
run;


/**** Correlation Structure ****/
/*sort before transposing*/
proc sort data=thesis.dataset2;
	by id;
run;
proc transpose data=thesis.dataset2 out=wide4 name=column_that_was_transposed prefix=time;
	by id;
	id timepoint;	
	var pinch_max;
run;

data wide4;
set wide4;
rename time0=time00 time1=time005 time2=time011
time3=time022 time4=time029 time5=time052 time6=time078 time7=time104 time8=time130;
run;
proc print data=wide4;run;
/* Means, Covariances, and Correlations */
/*an interesting post: https://groups.google.com/forum/#!topic/comp.soft-sys.sas/DALAsmjSskw*/
ods graphics on; 
proc corr data=wide4 plots=matrix /*NOPROB*/ /*NOSIMPLE*/  /*fisher*/ outp=CorrOutp;
	var time00--time130;
	ods select pearsoncorr; 
	ods output pearsoncorr=pearsoncorr_p; 
run;
ods graphics off;

/*** CORRELOGRAM ***/
/*Weiss lab 2 */
/*CREATE DATASET WITH ONLY THE UPPER TRIANGULAR VALUES*/
data tri (drop=i obsno variable);
	set pearsoncorr_p (drop=ptime00--ptime130);
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

/*for OLS residuals and variogram*/
proc mixed data=piecewise;
	class gender treatment timepoint;
	model pinch_max = treatment*timepoint gender*timepoint weight*timepoint age*timepoint/noint solution OUTPM=out_p residual;
	where timepoint<8;
run;

/*OLS residual profiles - from the model for above*/
goptions reset=all;
proc gplot data=out_p;
plot resid*timepoint=id/haxis=axis1 vaxis=axis2 nolegend;
symbol i= join c=gray r=42 mode=include;
axis1 minor=none label=('Timepoint');
axis2 minor=none label=('OLS Residuals');
title h=2 'OLS residual profiles -pinchgrip';
run;

/*Semi-Variogram*/

/* Calculation of the variogram - dataset from proc mixed above (OLS residuals and variogram)*/
proc variogram data=out_p outpair=out;
	coordinates xc=time yc=id;
	compute robust novariogram;
	var resid;
run;
data variogram;set out;
if y1=y2;vario=(v1-v2)**2/2; run;
data variance;set out;
if y1<y2; vario=(v1-v2)**2/2; run;
/* Calculation of the total variance (=0.5897341) */
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
plot vario*distance=1 p_vario*distance=2/ overlay haxis=axis1 vaxis=axis2 vref=0.5897341 lvref=3;
symbol1 c=red v=dot h=0.2 mode=include;
symbol2 c=black i=join w=2 mode=include;
axis1 label=(h=2 "Time lag") value=(h=1.5) minor=none;
axis2 label=(h=2 A=90 "v(u)") value=(h=1.5) minor=none order=(0 to 2.5);
title h=3 "Empirical Semi-variogram (Y4)";
run;



/****************** COVARIANCE PATTERN MODEL On the RESIDUALS ******************/
/***************** Step 2 *****************/
/*ods output clear;
ods listing close;
ods output fitstatistics(match_all persist=proc)=modstat;*/

/*Model A*/
proc mixed data = out_p method = ml;
	class id gender treatment timepoint; 
	model resid = / noint s;
	repeated timepoint/ type =cs subject = id;
	where timepoint <8;
run;
/*Model B */
proc mixed data = out_p method = ml maxit=100;
	class id gender treatment timepoint; 
	model resid = / noint s;
	repeated timepoint/ type =csh subject = id;
	where timepoint<8;
run;
/*Model C */
proc mixed data = out_p method = ml;
	class id gender treatment timepoint; 
	model resid = / noint s;
	repeated timepoint/ type =sp(gau)(time_sc) local subject = id;
	where timepoint<8;
run;
/*Model D */
proc mixed data = out_p method = ml maxit=100;
	class id gender treatment timepoint; 
	model resid = / noint s;
	repeated timepoint/ type =sp(exp)(time_sc) local subject = id;
	where timepoint<8;
run;

/*ods listing;*/


