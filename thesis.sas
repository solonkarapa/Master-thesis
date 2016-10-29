libname thesis "/folders/myfolders/thesis";

proc print data=thesis.dataset2(obs=20);run;

proc contents data=thesis.dataset2;run;

libname thesis 'h:/sas/Thesis';


/*create time variable (weeks)*/
data thesis.dataset2;
set thesis.dataset2;
if timepoint=0 then time=0;
else if timepoint=1 then time=5;
else if timepoint=2 then time=11;
else if timepoint=3 then time=22;
else if timepoint=4 then time=29;
else if timepoint=5 then time=52;
else if timepoint=6 then time=78;
else if timepoint=7 then time=104;
else if timepoint=8 then time=130;
time_sc = time/10;
missing = Missing__YES__NO__;
/*date = input(date, ANYDTDTE10.);*/
/*age = YRDIF(birth,date,"AGE");*/
run;


data thesis.piecewise;
	set thesis.dataset2;
	if timepoint<5 then treatment="common";
	if timepoint<5 then time2=0;
		else time2=timepoint;
	if timepoint=>5 then time1=0;
		else time1=timepoint; 
run;


/*vis the data
proc contents data=thesis.dataset;run;
proc print data=thesis.dataset;run;*/

/*drop useless variables
data thesis.dataset2; 
set thesis.dataset;
drop Naam voornaam anemie var35;
age_diag = YRDIF(birth,diagnosis,"AGE");
timepoint=timepoint-1;
timepointclss = timepoint;
bmi = weight/((length/100)*(length/100));
run;*/

proc print data=thesis.dataset2;run;

/*check for missing values*/
proc means data=thesis.dataset2 nmiss;
	var date;
	by id timepoint;
run;

/*missing values-incomplete, also look at joint_model.sas*/
proc freq data=thesis.dataset2; 
tables Missing__YES__NO__*timepoint;
run;

/*demographics: Inspect the time fixed covariates*/
proc sort data=thesis.dataset2; 
	by id;
run;
data datademog (keep=bmi gender treatment id age_diag age);
	set thesis.dataset2;
	by id;
	if first.id;
run;

proc freq data=datademog;
	table gender treatment/norow nocol;
run;

/*age at diagnosis*/
proc univariate data=datademog;
	/*class gender;*/
	var age_diag;
	/*histogram age_diag;*/
	output out=want1 min=age_min max=age_max;
run;

/*age*/
proc univariate data=datademog;
	/*class gender;*/
	var age;
	/*histogram age_diag;*/
	output out=want2 min=age_min max=age_max mean=age_mean;
run;

/*length*/
proc means data=thesis.dataset2 q1 q3 median;
var length weight age;
run;

/*options pagesize=100 linesize=150;*/
proc tabulate data=datademog missing;
class gender treatment;
var age_diag;
table ((gender all='Both Male and Female')),(treatment all='All Treatments')*(age_diag*((n nmiss='.')*f=3. (mean std)*f=5.2))/
rts=25;
title 'Data Summary Table';
run;

proc means data=thesis.dataset2 min max ;
var age_diag;
run;

/*Counting the Number of Visits for Each Patient*/
data delete9th; 
set thesis.dataset2;
if timepoint=8 then delete;
if quad_max=. then timepoint=.;
run;

proc freq data=delete9th NOPRINT; 
tables id / OUT = COUNT(KEEP=id COUNT RENAME=(COUNT = N_VISITS));
run;

PROC SQL;
CREATE TABLE COUNT_VISITS AS
SELECT id,
COUNT(id) AS N_VISITS FROM thesis.dataset2 GROUP BY id; 
QUIT;

proc means data=COUNT median mean n ;
var N_VISITS;
run;

/*Age over time*/
goptions reset=all ftext=swiss gsfname=fig1
gsfmode=replace rotate=landscape i=join;
proc gplot data=thesis.dataset2;
plot age*timepoint=id / haxis=axis1 vaxis=axis2 nolegend;
axis1 minor=none label=("Time points");
axis2 minor=none label=("Age (years)");
run;
quit;



/*BMI over time*/
goptions reset = all;
proc gplot data = thesis.dataset2;
plot bmi*timepoint=id / haxis=axis1 vaxis=axis2 noframe nolegend;
plot2 bmi*timepoint / noframe noaxis;
symbol v= none repeat=42 i= stdmj color = gray height = 2;
symbol2 v= none color = blue i =std1mj width = 2;
axis1 label=("Time points");
axis2 label=("BMI (kg/m2)") value=(h=1) offset=(0.1)cm minor=none;
run;


/*Height over time*/
goptions reset=all ftext=swiss gsfname=fig1
gsfmode=replace rotate=landscape i=join;
proc gplot data=thesis.dataset2;
plot length*timepoint=id / haxis=axis1 vaxis=axis2 nolegend;
axis1 minor=none label=("Time points");
axis2 minor=none label=("Heigth (cm)");
run;
quit;

/*Weight over time*/
goptions reset=all ftext=swiss gsfname=fig1
gsfmode=replace rotate=landscape i=join;
proc gplot data=thesis.dataset2;
plot weight*timepoint=id / haxis=axis1 vaxis=axis2 nolegend;
axis1 minor=none label=("Time points");
axis2 minor=none label=("Weight (kg)");
run;
quit;


/************ piecewise model ************/
data piecewise;
set quatrovariate;
if timepoint<5 then treatment="common";
run;

ods output LSMeans=meansLS;
proc mixed data=piecewise CL method=ml covtest;
	class id VAR timepoint gender treatment;
	model Y=VAR*timepoint*treatment VAR*timepoint*bmi VAR*timepoint*age_diag VAR*timepoint*gender/noint s ddfm=KR;
	repeated VAR timepoint/type = UN@CS subject=id r;
	lsmeans VAR*treatment*timepoint;
run;
goptions reset=all;
symbol1 c=blue v=star h=.8 i=j;
symbol2 c=red v=dot h=.8 i=j;
symbol3 c=green v=square h=.8 i=j;
symbol4 c=black v=square h=.8 i=j;
symbol5 c=purple v=square h=.8 i=j;
title "LSmeans";
proc gplot data=meansLS;
	by VAR;
	plot estimate*timepoint=treatment;
run; 
quit;

proc sgplot data = meansLS;
/*by VAR;*/
title 'Empirical Summary Plot, By Treatment';	
vline timepoint / response =estimate group = treatment stat = mean;
yaxis LABEL = "Response";
run;






















	



