/*** I use two ways to access SAS, hence I use to libname statements***/

libname thesis "/folders/myfolders/thesis";

libname thesis 'h:/sas/Thesis';


/* The following is code to model the quadriceps strength (univariatly)*/

/******************************************************************************************************
********************** 					Y1(Quadriceps)					  *****************************
******************************************************************************************************/

/******************* Decriptive - Summarize Data *******************/
/* within-time mean and SD*/
proc means data = thesis.dataset2 n mean std;
	class timepoint;
	var quad_max;
	output out=sumstat mean=mean std=std stderr=stderr n=n;
run;
/* within-subject mean and SD*/
proc means data = thesis.dataset2 n mean std;
	class id;
	var quad_max tibialis_max;
run;

/*Histograms*/
proc univariate data=thesis.dataset2 noprint;
	histogram;
	var hb TUG _6mwt pinch pinch_max quad quad_max tibialis tibialis_max;
run;
/* # of observations per timepoint - No count the observations that are missing the response*/
proc tabulate data=thesis.dataset2;
	title 'Observed times by time';
	class id timepoint;
	var quad_max;
	table quad_max*n*f=5., timepoint/rts=15;
run;

/*missing values per timepoint*/
proc freq data=thesis.dataset2 ;
tables missing*timepoint/nopercent norow;
run;

proc print data=thesis.dataset2;
var id missing X6mwt quad_max tibialis_max hand_max pinch_max;
where timepoint=7;
run;

/*Number of Subjects by Number of Observations*/
proc freq data=thesis.dataset2 NOPRINT;
	where quad_max~=.;
	tables ID /
	out=COUNTS(KEEP=ID COUNT RENAME=(COUNT=NOBS));  
   run;
proc tabulate data=counts;
	class nobs;
	var id;
	title "Number of Subjects by Number of Observations";
	table id*n*f=2., nobs="Number of Observations";
run;



/****************** Descriptive graphs for quad_max ******************/
/*** Individual profiles ***/
proc sort data=thesis.dataset2;
by id;
goptions reset=all ftext=swiss gsfname=fig1
gsfmode=replace rotate=landscape i=join;
proc gplot data=thesis.dataset2;
plot quad_max*timepoint=id / haxis=axis1 vaxis=axis2 nolegend;
axis1 minor=none label=("Time points");
axis2 minor=none label=(angle=90 "Max(Quadriceps)(kg)");
title h=3 'Individual profiles';
run;
quit;
/* same using sgplot*/
proc sgplot data=thesis.dataset2 /*(where=(id in (34, 4, 23)))*/;
series x=timepoint y=quad_max/group=id lineattrs=(color=black pattern=solid);
yaxis LABEL = 'Max(Quad)(kg)';
run;

/*** Mean with individual profiles and SD ***/
goptions reset = all;
proc gplot data = thesis.dataset2;
plot quad_max*timepoint=id / haxis=axis1 vaxis=axis2 nolegend;
plot2 quad_max*timepoint / noframe noaxis;
symbol v= none repeat=42 i= stdmj color = gray height = 2;
symbol2 v= none color = blue i =stdj width = 2;
axis1 label=("Time points");
axis2 label=(angle=90 "Max(Quadriceps)(kg)") value=(h=1) offset=(0.1)cm minor=none;
run;

/**** Mean with individual profiles and SE ***/
goptions reset = all;
proc gplot data = thesis.dataset2;
where timepoint < 8;
plot quad_max*timepoint=id/ haxis=axis1 vaxis=axis2 nolegend;
plot2 quad_max*timepoint/ noframe noaxis;
symbol v = none repeat = 42 i = std1mj color = gray height = 2 l = 2;
symbol2 v= none color = black i = stdmj width = 2;
axis1 label=("Time point") minor = none;
axis2 label=(angle=90 "Quadriceps femoris (kg)") value=(h=1) offset=(0.1)cm order = 0 to 30 by 5;
run;

/*** Means by gender ***/
goptions reset = all;
proc gplot data=thesis.dataset2;/*change the dataset*/
plot quad_max*timepoint=gender/ haxis=axis1 vaxis=axis2 noframe legend=legend1;
symbol3 i=stdlmj r=1 w=2 l=1 c=blue;
symbol4 i=stdlmj r=1 w=2 l=1 c=red;
axis1 label=("Time points");
axis2 label=("Max(Quadriceps)(kg)") value=(h=1) offset=(0.1)cm minor=none;
legend1 label=(h=1"Gender") value=(h=1  "Females"  "Males") order=(1 2);
run;
/* same as before with sgplot*/
proc sgplot data = thesis.dataset2;
title 'Empirical Summary Plot, By Gender';	
vline timepoint / response = quad_max group = gender stat = mean limitstat = stderr limits = both;
yaxis LABEL = "Max(Quadriceps)(kg)";
run;

/****quad_max over time ****/
proc sgplot data=thesis.dataset2;
	vbox quad_max/ category=timepoint;
run;
/*Histogram of Baseline Values With Kernal and Normal Densities*/
/* http://www.mwsug.org/proceedings/2015/AA/MWSUG-2015-AA-09.pdf */
/* do it by timepoint !!!!!!*/
proc sgplot data=thesis.dataset2; 
	histogram quad_max; 
	density quad_max; 
	density quad_max / type=kernel;
	keylegend / location=inside position=topright; 
run;
/*Histograms: response per timepoint*/
proc univariate data=thesis.dataset2;
 	class timepoint;
 	VAR quad_max;
 	histogram quad_max;  
 	title 'Univariate look at Data';
run;

/**** pattern over time for males and females ****/
proc sgpanel data=thesis.dataset2;
	panelby gender;
	vbox quad_max/ category=timepoint;
run;

/*** Evolution by treatment ***/
proc sgplot data = thesis.dataset2;
title 'Empirical Summary Plot, By Treatment';	
vline timepoint / response =quad_max group = treatment stat = mean;
yaxis LABEL = "Max(Quadriceps)(kg)";
run;

/*http://www2.sas.com/proceedings/forum2007/091-2007.pdf*/
/*not completely correct!!!!!!*/
goptions reset = all;
proc gplot data = thesis.dataset2;
plot quad_max*timepoint=id /haxis=axis1 vaxis=axis2 nolegend;
plot2 quad_max*timepoint=treatment/ noaxis noframe ;
symbol v = none repeat = 41 i = sm50s color = darkgray height = 2;
symbol2 v = none i = sm50s color = black width = 4 line = 2;
symbol3 v = none i = sm50s color = yellow width = 4 line = 5;
symbol4 v = none i = sm50s color = red width = 4 line = 2;
symbol5 v = none i = sm50s color = blue width = 4 line = 2;
axis1 label=("Time points");
axis2 label=("Max(Quadriceps)(kg)") value=(h=1) offset=(0.1)cm minor=none;
run;
/*evolution by gender and treatment*/
data interact;
set thesis.dataset2;
if treatment="AR1" then treatmentnew=1;
else if treatment="AR2" then treatmentnew=2;
else if treatment="VLR" then treatmentnew=3;
else if treatment="VHR" then treatmentnew=4;
if gender=0 then gendernew=5;
else gendernew=6;
interact2=gendernew*treatmentnew;
run;
proc print data=interact;run;
proc format;
value treatgender 
	6 = "AR1-male" 
	5 = "AR1-female"
	12 = "AR2-male" 
	10 = "AR2-female" 
	24 = "VHR-male"
	20 = "VHR-female"
	18 = "VLR-male"
	15="VLR-female";
run;
goptions reset = all;
proc gplot data = interact;
plot quad_max*timepoint=interact2 /haxis=axis1 vaxis=axis2;
symbol1 v = none i = sm50s color = darkgray height = 2;
symbol2 v = none i = sm50s color = BLUE width = 4 line = 2;
symbol3 v = none i = sm50s color = CYAN width = 4 line = 6;
symbol4 v = none i = sm50s color = OLIVE width = 4 line = 2;
symbol5 v = none i = sm50s color = LIME width = 4 line = 6;
symbol6 v = none i = sm50s color = black width = 4 line = 2;
symbol7 v = none i = sm50s color = MAGENTA width = 4 line = 6;
symbol8 v = none i = sm50s color = BIOY width = 4 line = 2;
symbol9 v = none i = sm50s color = DAOY width = 4 line = 6;
axis1 label=("Time points") value=(h=1.5);
axis2 label=(A=90 "Max(Quadriceps)(kg)") value=(h=1.2) offset=(0.1)cm minor=none;
format interact2 treatgender.; run;
run;

/* Empirical standard deviations over time*/
proc gplot data=sumstat; /*sumstat from proc means before*/
	plot std*timepoint/vaxis=axis1;
	symbol1 i=none v=circle color=black height=2 r=1 ;
	title 'SD over time';
	axis1 minor=none label=(angle=90 'SD of Max(Quad)(kg)');
run;

/**** Correlation Structure ****/
/*sort before transposing*/
proc sort data=thesis.dataset2;
	by id;
run;
proc transpose data=thesis.dataset2 out=wide1 name=column_that_was_transposed prefix=time;
	by id;
	id timepoint;	
	var quad_max;
run;

data wide1;
set wide1;
rename time0=time00 time1=time005 time2=time011
time3=time022 time4=time029 time5=time052 time6=time078 time7=time104 time8=time130;
run;
proc print data=wide1;run;
/* Means, Covariances, and Correlations */
/*an interesting post: https://groups.google.com/forum/#!topic/comp.soft-sys.sas/DALAsmjSskw*/
ods graphics on; 
proc corr data=wide1 plots=matrix /*NOPROB*/ /*NOSIMPLE*/  /*fisher*/ outp=CorrOutp;
	var time00--time130;
	ods select pearsoncorr; 
	ods output pearsoncorr=pearsoncorr; 
run;
ods graphics off;

/*CORRELOGRAM*/
/*Weiss lab 2 */
/*CREATE DATASET WITH ONLY THE UPPER TRIANGULAR VALUES*/
data tri (drop=i obsno variable);
	set pearsoncorr (drop=ptime00--ptime130);
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


/*deleting unessessary info*/
proc sql;
	delete from CorrOutp
	where _TYPE_ IN ("MEAN","N","STD");
proc print data=CorrOutp;run;
/*plotting correlations*/
goptions reset=all;
proc gplot data=CorrOutp;
plot time1*_NAME_ time2*_NAME_ time3*_NAME_ time4*_NAME_ time5*_NAME_ time6*_NAME_ time7*_NAME_ 
time8*_NAME_ time9*_NAME_ /overlay haxis=axis1 legend;
axis1 label=("Time points");
symbol i=join;
run;

/*Correlogram-  see Weiss p86*/
/*%corrgram(data=CorrOutp,var=time1-time9, type=corr, fill=N N N); */


/** Semi-Variogram **/
/* Calculation of residuals */
proc mixed data=thesis.piecewise method=ml;
	class gender treatment timepoint;
	model quad_max = treatment*timepoint gender*timepoint weight*timepoint age*timepoint/noint solution OUTPM=quad_ols residual;
	where timepoint<8;
run;
data quad_ols;
	set  quad_ols;
	if resid=. then delete;
run;
/* Calculation of the variogram */
proc variogram data=quad_ols outpair=out;
	coordinates xc=time yc=id;
	compute robust novariogram;
	var resid;
run;
data variogram;set out;
if y1=y2;vario=(v1-v2)**2/2; run;
data variance;set out;
if y1<y2; vario=(v1-v2)**2/2; run;
/* Calculation of the total variance (=5.6562994) */
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
plot vario*distance=1 p_vario*distance=2/ overlay haxis=axis1 vaxis=axis2 vref=5.6562994 lvref=3;
symbol1 c=red v=dot h=0.2 mode=include;
symbol2 c=black i=join w=2 mode=include;
axis1 label=(h=2 "Time lag") value=(h=1.5) minor=none;
axis2 label=(h=2 A=90 "v(u)") value=(h=1.5) minor=none order=(0 to 15);
title h=3 "Empirical Semi-variogram";
run;

/*OLS residual profiles - from the model for the variogram*/
goptions reset=all;
proc gplot data=quad_ols;
plot resid*timepoint=id/haxis=axis1 vaxis=axis2 nolegend;
symbol i= join c=gray r=42 mode=include;
axis1 minor=none label=('Timepoint');
axis2 minor=none label=('OLS Residuals');
title h=2 'OLS residual profiles';
run;

/*sample autocorrelation plot*/
/*file:///Users/solon/Downloads/Longitudinal_Data_Analysis_with_Discrete_and_Cont%20(1).pdf*/
data out_autoc;
set out;
autocorr=1-(p_vario/4.9999017);
run;
goptions reset=all;
proc gplot data=out_autoc;
plot autocorr*distance / vaxis=axis1 haxis=axis2;
symbol v=none i=sm60s;
label time_interval='Time Interval';
title 'Autocorrelation Plot';
run;
quit;


/****************** COVARIANCE PATTERN MODEL On the RESIDUALS ******************/
/***************** Step 2 *****************/
ods output clear;
ods listing close;
ods output fitstatistics(match_all persist=proc)=modstat;

/*Model A*/
proc mixed data = quad_ols method = ml;
	class id gender treatment timepoint; 
	model resid = / noint s;
	repeated timepoint/ type =cs subject = id;
	where timepoint <8;
run;
/*Model B */
proc mixed data = quad_ols method = ml maxit=100;
	class id gender treatment timepoint; 
	model resid = / noint s;
	repeated timepoint/ type =csh subject = id;
	where timepoint<8;
run;
/*Model C */
proc mixed data = quad_ols method = ml;
	class id gender treatment timepoint; 
	model resid = / noint s;
	repeated timepoint/ type =sp(gau)(time_sc) local subject = id;
	/*where timepoint<8;*/
run;
/*Model D */
proc mixed data = quad_ols method = ml;
	class id gender treatment timepoint; 
	model resid = / noint s;
	repeated timepoint/ type =sp(exp)(time_sc) local subject = id;
	/*where timepoint<8;*/
run;

ods listing;


data model_fit;
length model $ 7 type $ 5;
set modstat (in=cs)
modstat1 (in=gau)
modstat2 (in=exp);
if substr(descr,1,1) in ('A','B','C');
if substr(descr,1,7)='2LogLik' then type="LogLik";
if substr(descr,1,3)='AIC' then type='AIC';
if substr(descr,1,4)='AICC' then type='AICC';
if substr(descr,1,3)='BIC' then type='BIC';
if cs then model='CS';
if exp then model='SpExp';
if gau then model='SpGau';
run;

goptions reset=all;
proc gplot data=model_fit;
plot value*model=type;
symbol1 value=star color=blue;
symbol2 value=circle color=red;
symbol3 value=dot color=green;
title 'Model Fit Statistics by Covariance Structure';
run;
quit;



/* not useful part for this project*/

/*http://www.ats.ucla.edu/stat/sas/faq/SAS_variogram_fit.htm*/
/*file:///Users/solon/Downloads/Longitudinal_Data_Analysis_with_Discrete_and_Cont%20(1).pdf*/
/*http://www.biostat.umn.edu/~xianghua/8452/note/02EDA.pdf*/

/* Fitted and Observed Variogram*/ 
proc mixed data = thesis.dataset2 method = ml /*empirical*/;
	class id gender treatment timepoint; 
	model quad_max = treatment*timepoint gender*timepoint bmi*timepoint age_diag*timepoint/ noint s ;
	repeated timepoint/ type =sp(exp)(time_sc) local subject = id;
run;

/*use the dataset for the empirical variogram*/
data out_g; 
set out; 
	vari = p_vario; type = 'data'; 
	output; 
	vari = 2.3067*(1-exp(-distance/9.6600)); type = 'exponential'; 
	output; 
	vari = 2.7090*(1-exp((-distance*distance)/(3.6622*3.6622))); type = 'gaussian'; 
	output;
  	vari = 3.5010; type='cs';
	output;
run;

goptions reset = all;
proc gplot data=out_g; 
  plot vari*distance=type; 
symbol1 i=join l=1 c=blue line=20; 
symbol2 i=join l=1 c=black; 
symbol3 i=join l=1 c=green line=21; 
symbol4 i=join l=1 c=red line=23;
axis1 minor=none label=(c=black 'Distance'); 
axis2 order=(0 to 9 by 1) minor=none label=(angle=90 rotate=0 c=black 'Variogram');
run;

