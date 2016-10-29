libname thesis "/folders/myfolders/thesis";

libname thesis 'h:/sas/Thesis';


/******** Modeling Jointly Longitudinal Data ********/

/*A few references:
http://arxiv.org/pdf/0705.0568.pdf
https://faculty.biostat.ucla.edu/robweiss/biostat236 - lab8
http://support.sas.com/documentation/cdl/en/statug/63962/HTML/default/viewer.htm#statug_mixed_sect020.htm
http://www2.sas.com/proceedings/sugi31/187-31.pdf*/

/*Create an appropriate data set*/
data var0;
	set thesis.piecewise;
	VAR=0;
	Y=quad_max;
	keep id Y VAR timepoint gender age weight treatment missing;
data var1;
	set thesis.piecewise;
	VAR=1;
	Y=tibialis_max;
keep id Y VAR timepoint gender age weight treatment missing;

data biv;
	set var0 var1;
run;

data var2;
	set thesis.piecewise;
	VAR=2;
	Y=hand_max;
	keep id Y VAR timepoint gender age weight treatment missing;
data var3;
	set thesis.piecewise;
	VAR=3;
	Y=pinch_max;
keep id Y VAR timepoint gender age weight treatment missing;
data thesis.quatrovariate;
	set var0 var1 var2 var3;
run;

/*missing*/
proc print data=thesis.quatrovariate;
	var missing timepoint VAR Y id;
	where missing="yes";
run;

data missing;
	set thesis.quatrovariate;
	keep missing timepoint VAR Y id;
	where missing="yes";
run;

proc freq data=missing;
table id timepoint/ OUT = COUNT(KEEP=id COUNT RENAME=(COUNT = N_Mis));
run; 


data thesis.random_model;
set thesis.quatrovariate; 
	if VAR=0 then INT1=1;
	else INT1=0;
	if VAR=1 then INT2=1;
	else INT2=0;
	if VAR=2 then INT3=1;
	else INT3=0;
	if VAR=3 then INT4=1;
	else INT4=0;
run;
proc print data=thesis.random_model;run;


/****** Marginal Bivariate model*******/
/*not used for this project*/
proc mixed data=biv CL method=reml covtest;
	class id VAR timepoint gender treatment;
	model Y=VAR*timepoint*treatment VAR*timepoint*gender VAR*timepoint*weight VAR*timepoint*age/noint s ddfm=KR;
	repeated VAR timepoint /type = UN@CS subject=id r;
run;
/* It has an unstructured covariance matrix for the two responses at one time. Then across time there is an additional 
correlation parameter which is required to be the same for the two outcomes*/



/*******Quatrovariate model*******/

 /*Kronecker product covariance*/ 
/* Model 1*/
proc mixed data=thesis.quatrovariate CL method=ml covtest;
	class id VAR timepoint gender treatment;
	model Y=VAR*timepoint*treatment VAR*timepoint*gender VAR*timepoint*weight VAR*timepoint*age/noint s ddfm=KR outpredm=pred_4out;
	repeated VAR timepoint/type = UN@CS subject=id r;
	where timepoint<8;
run;

/*Model checking*/
proc sql;
 create table summary_means as
  select timepoint, VAR,  
         mean(Y) as sample_mean,
         mean(pred) as pred_mean
  from pred_4out
  group by timepoint, VAR;
quit;
proc sort data=summary_means; by VAR treatment; run;
goptions reset = all;
 symbol1 value=circle color=black;
 symbol2 value=none interpol = join repeat = 1;
 axis1 label=(a=0 'Timepoint') minor=none;
 axis2 label = (a=90 'Response') minor=none ;
proc gplot data = summary_means;
by VAR;
plot (sample_mean pred_mean)*timepoint /overlay haxis=axis1 vaxis=axis2;
run; 
quit;


/*Random coefficient*/
/*MODEL R1a*/
proc mixed data=thesis.random_model CL method=reml covtest maxiter=100;
	class id VAR timepoint gender treatment;
	model Y=VAR*timepoint*treatment VAR*timepoint*gender VAR*timepoint*weight VAR*timepoint*age /noint s ddfm=KR;
	random INT1 INT2 INT3 INT4/ type=un subject=id g gcorr;
	repeated timepoint*VAR /type = simple subject=id group=VAR r;
	where timepoint < 8;
run;

/*MODEL R1b*/
/*the same as before, different specification of the random statement*/
proc mixed data=thesis.random_model CL method=reml covtest;
	class id VAR timepoint gender treatment;
	model Y=VAR*timepoint*treatment VAR*timepoint*gender VAR*timepoint*weight VAR*timepoint*age /noint s ddfm=KR;
	random VAR/ type=un subject=id g gcorr v;
	repeated timepoint*VAR /type = simple subject=id group=VAR r;
	where timepoint < 8;
run;
	

/*MODEL R2a.1*/
proc mixed data=thesis.random_model CL method=reml covtest maxiter=100;
	class id VAR timepoint gender treatment;
	model Y=VAR*timepoint*treatment VAR*timepoint*gender VAR*timepoint*weight VAR*timepoint*age /noint s ddfm=KR;
	random INT1 INT2 INT3 INT4/ type=un subject=id g gcorr;
	repeated VAR /type = UN subject=id*timepoint r;
	where timepoint < 8;
run;

/*MODEL R2b.1*/
proc mixed data=thesis.random_model CL method=ml covtest maxiter=100;
	class id VAR timepoint gender treatment;
	model Y=VAR*timepoint*treatment VAR*timepoint*gender VAR*timepoint*weight VAR*timepoint*age /noint s ddfm=KR;
	random VAR/ type=un subject=id g gcorr;
	repeated VAR /type = UN subject=id*timepoint r rcorr;
	where timepoint < 8;
run;



/*MODEL R2b.2: no VAR interaction with gender*/
proc mixed data=thesis.random_model CL method=ml covtest;
	class id VAR timepoint gender treatment;
	model Y=VAR*timepoint*treatment timepoint*gender VAR*timepoint*weight VAR*timepoint*age /noint s ddfm=KR;
	random VAR/ type=un subject=id g gcorr;
	repeated VAR /type = UN subject=id*timepoint r rcorr;
	where timepoint < 8;
run;

/*MODEL R2b.3: no timepoint*gender*/
proc mixed data=thesis.random_model CL method=ml covtest;
	class id VAR timepoint gender treatment;
	model Y=VAR*timepoint*treatment VAR*timepoint*weight VAR*timepoint*age /noint s ddfm=KR;
	random VAR/ type=un subject=id g gcorr;
	repeated VAR /type = UN subject=id*timepoint r rcorr;
	where timepoint < 8;
run;

/*MODEL R2b.4: no VAR and timepoint*weight interaction*/
proc mixed data=thesis.random_model CL method=ml covtest;
	class id VAR timepoint gender treatment;
	model Y=VAR*timepoint*treatment timepoint*weight VAR*timepoint*age /noint s ddfm=KR;
	random VAR/ type=un subject=id g gcorr ;
	repeated VAR /type = UN subject=id*timepoint r rcorr;
	where timepoint < 8;
run;

/*MODEL R2b.3.err= R2b.3 without correlated errors*/
proc mixed data=thesis.random_model CL method=reml covtest asycov;
	class id VAR timepoint gender treatment;
	model Y=VAR*timepoint*treatment VAR*timepoint*weight VAR*timepoint*age /noint s ddfm=KR;
	random VAR/ type=un subject=id g gcorr v;
	repeated timepoint*VAR /type = simple subject=id group=VAR r;
	where timepoint < 8;
	ods output AsyCov=asycov;
run;

/*FINAL MODEL R2b.3: fit under REML*/
proc mixed data=thesis.random_model CL method=reml covtest asycov;
	class id VAR timepoint gender treatment;
	model Y=VAR*timepoint*treatment VAR*timepoint*weight VAR*timepoint*age /noint s ddfm=KR;
	random VAR/ type=un subject=id g gcorr v;
	repeated VAR /type = UN subject=id*timepoint r rcorr;
	where timepoint < 8;
	
	/*ods output AsyCov=asycov;*/
	/*ods output CovParms=covparms;*/
	
	contrast "AR1+AR2B vs VLR+AR2T at T6, T7, T8"
	VAR*treatment*timepoint 0 0 0 0 0 1 1 -1 0 -1,  					/*T6 for Y1*/
	VAR*treatment*timepoint	0 0 0 0 0 0 0 0 0 0 1 -1 0 -1, 				/*T7 for Y1*/
	VAR*treatment*timepoint	0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 -1 0 -1,	/*T8 for Y1*/
	
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0   
							0 0 0 0 0 1 1 -1 0 -1, 						/*T6 for Y2*/
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  
							0 0 0 0 0 0 0 0 0 0 1 -1 0 -1, 				/*T7 for Y2*/
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 -1 0 -1, 	/*T8 for Y2*/
							
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
							0 0 0 0 0 1 1 -1 0 -1, 						/*T6 for Y3*/
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
							0 0 0 0 0 0 0 0 0 0 1 -1 0 -1, 				/*T7 for Y3*/
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 -1 0 -1,		/*T8 for Y3*/
	
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
						    0 0 0 0 0 1 1 -1 0 -1,		/*T6 f4r Y3*/
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
						    0 0 0 0 0 0 0 0 0 0 1 -1 0 -1,				/*T7 for Y4*/
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 -1 0 -1/E; 	/*T8 for Y4*/
										
	contrast "all vs VHR at T6"
	VAR*treatment*timepoint 0 0 0 0 0 1 0 0 -1 0, /*for Y1*/
	VAR*treatment*timepoint	0 0 0 0 0 0 1 0 -1 0,
	VAR*treatment*timepoint	0 0 0 0 0 0 0 1 -1 0,
	VAR*treatment*timepoint	0 0 0 0 0 0 0 0 -1 1,
	
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 -1 0,/*for Y2*/
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 -1 0,					
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0,						
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 1, 
	
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 /*for Y3*/
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 -1 0,						
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 -1 0,
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0,
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 1,
							
	VAR*treatment*timepoint	0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 /*for Y4*/
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 -1 0,
	VAR*treatment*timepoint	0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 -1 0,
	VAR*treatment*timepoint	0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0,
	VAR*treatment*timepoint	0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 1/E;
	run;	


/*http://www.ats.ucla.edu/stat/sas/faq/estimate_statement.htm*/
proc mixed data=thesis.random_model CL method=reml;
	class id VAR timepoint gender treatment;
	model Y=VAR*timepoint*treatment VAR*timepoint*weight VAR*timepoint*age /noint s ddfm=KR;
	random VAR/ type=un subject=id;
	repeated VAR /type = UN subject=id*timepoint;
	where timepoint < 8;
	
	estimate "AR1 vs VHR at T6 for Y1"
	VAR*treatment*timepoint 0 0 0 0 0 1 0 0 -1 0/CL E; /*for Y1*/
	estimate "AR2B vs VHR at T6 for Y1"
	VAR*treatment*timepoint	0 0 0 0 0 0 1 0 -1 0/CL E;
	estimate "AR2T vs VHR at T6 for Y1"
	VAR*treatment*timepoint	0 0 0 0 0 0 0 1 -1 0/CL E;
	estimate "VLR vs VHR at T6 for Y1"
	VAR*treatment*timepoint	0 0 0 0 0 0 0 0 -1 1/E CL; 

	estimate "AR1 vs VHR at T6 for Y2"
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 -1 0/CL E;/*for Y2*/
	estimate "AR2B vs VHR at T6 for Y2"
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 -1 0/CL E;	
	estimate "AR2T vs VHR at T6 for Y2"			
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0/CL E;
	estimate "VLR vs VHR at T6 for Y2"					
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 1/CL E;
	
	estimate "AR1 vs VHR at T6 for Y3"
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 -1 0/CL E;	/*for Y3*/
	estimate "AR2B vs VHR at T6 for Y3"					
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 -1 0/CL E;
	estimate "AR2T vs VHR at T6 for Y3"
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0/CL E;
	estimate "VLR vs VHR at T6 for Y3"
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 1/CL E;
	
	estimate "AR1 vs VHR at T6 for Y4"				
	VAR*treatment*timepoint	0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 -1 0/CL E;/*for Y4*/
	estimate "AR2B vs VHR at T6 for Y4"
	VAR*treatment*timepoint	0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 -1 0/CL E;
	estimate "AR2T vs VHR at T6 for Y4"
	VAR*treatment*timepoint	0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0/CL E;
	estimate "VLR vs VHR at T6 for Y4"
	VAR*treatment*timepoint	0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 1/CL E;
	run;
	
	proc mixed data=thesis.random_model CL method=reml;
	class id VAR timepoint gender treatment;
	model Y=VAR*timepoint*treatment VAR*timepoint*weight VAR*timepoint*age /noint s ddfm=KR;
	random VAR/ type=un subject=id;
	repeated VAR /type = UN subject=id*timepoint;
	where timepoint < 8;
	
	estimate "AR1+AR2B vs VLR+AR2T at T6 for Y1" 	/*T6 for Y1*/
	VAR*treatment*timepoint 0 0 0 0 0 1 1 -1 0 -1/ CL E; 
	estimate "AR1+AR2B vs VLR+AR2T at T7 for Y1"        
	VAR*treatment*timepoint	0 0 0 0 0 0 0 0 0 0 1 -0.5 0 -0.5/CL E; 	
	estimate "AR1+AR2B vs VLR+AR2T at T8 for Y1"
	VAR*treatment*timepoint	0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 -1 0 -1/ CL E; 
					
	estimate "AR1+AR2B vs VLR+AR2T at T6 for Y2" 
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0   
							0 0 0 0 0 1 1 -1 0 -1/ CL E;  	
	estimate "AR1+AR2B vs VLR+AR2T at T7 for Y2" 				
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  
							0 0 0 0 0 0 0 0 0 0 1 -0.5 0 -0.5/CL E; 	
	estimate "AR1+AR2B vs VLR+AR2T at T8 for Y2" 			
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 -1 0 -1/CL E;  
							
	estimate "AR1+AR2B vs VLR+AR2T at T6 for Y3" 											
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
							0 0 0 0 0 1 1 -1 0 -1/CL E;  
	estimate "AR1+AR2B vs VLR+AR2T at T7 for Y3" 						
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
							0 0 0 0 0 0 0 0 0 0 1 -0.5 0 -0.5/CL E;  	
	estimate "AR1+AR2B vs VLR+AR2T at T8 for Y3" 			
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 -1 0 -1/CL E; 		
	
	estimate "AR1+AR2B vs VLR+AR2T at T6 for Y4"
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
						    0 0 0 0 0 1 1 -1 0 -1/CL E; 
	estimate "AR1+AR2B vs VLR+AR2T at T6 for Y4_2"
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
						    0 0 0 0 0 1 1 -1 0 -1/CL E;
	estimate "AR1+AR2B vs VLR+AR2T at T6 for Y4_3"
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
						    0 0 0 0 0 0.5 0.5 -0.5 0 -0.5/CL E;
	estimate "AR1+AR2B vs VLR+AR2T at T7 for Y4"
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
						    0 0 0 0 0 0 0 0 0 0 1 -0.5 0 -0.5/CL E; 				
	estimate "AR1+AR2B vs VLR+AR2T at T8 for Y4"
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 -1 0 -1/CL E; 
	run;
	


/*estimate muscle strength for quantiles*/
proc mixed data=thesis.random_model CL method=reml;
	class id VAR timepoint gender treatment;
	model Y=VAR*timepoint*treatment VAR*timepoint*weight VAR*timepoint*age /noint s ddfm=KR;
	random VAR/ type=un subject=id;
	repeated VAR /type = UN subject=id*timepoint;
	where timepoint < 8;
	lsmeans VAR*timepoint*treatment/at weight=19.8 at age=5.3369863 cl; /*25 percentile*/
	lsmeans VAR*timepoint*treatment/at weight=33.4 at age=10.67 cl; /*75percentile*/
	lsmeans VAR*timepoint*treatment/at weight=24.2 at age=6.498 cl; /*median*/
	ods output LSMeans=means_joint;
	run;

proc sort data=means_joint;
by VAR;
run;
/*Creating Graphs of the Means*/
/*http://www.ats.ucla.edu/stat/sas/seminars/sas_repeatedmeasures/ */ 
goptions reset=all;
symbol1 c=blue v=star h=.8 i=j;
symbol2 c=red v=dot h=.8 i=j;
symbol3 c=green v=square h=.8 i=j;
symbol4 c=black v=square h=.8 i=j;
symbol5 c=purple v=square h=.8 i=j;
symbol6 c=brown v=square h=.8 i=j;
title "LSmeans";
proc gplot data=means_joint;
	where VAR=1;
plot estimate*timepoint=treatment;
run; 
quit;
	
/*data step muscle strength for quantiles*/
data interact1; 
set means_joint;
if treatment="AR1" then treatmentnew=100;
else if treatment="AR2B" then treatmentnew=110;
else if treatment="AR2T" then treatmentnew=120;
else if treatment="VLR" then treatmentnew=130;
else if treatment="VHR" then treatmentnew=140;
else if treatment="comm" then treatmentnew=150;
if weight=19.8 then perc=100;
else if weight=24.2 then perc=1000;
else if weight=33.4 then perc=10000;
interact=treatmentnew*perc;
run;

proc format; 
value treatweight

	10000 = "AR1-25%"
 	100000 = "AR1-50%"
	1000000 = "AR1-75%"

	11000 = "AR2B-25%"
	110000 = "AR2B-50%"
	1100000 = "AR2B-75%"

	12000 = "AR2T-25%"
	120000 = "AR2T-50%"
	1200000 = "AR2T-75%"

	13000 = "VLR-25%"
	130000 = "VLR-50%"
	1300000 = "VLR-75%"

	14000 = "VHR-25%"
	140000 = "VHR-50%"
	1400000 = "VHR-75%"

	15000 = "comm-25%"
	150000 = "comm-50%"
	1500000 = "comm-75%";
run;

/*plot muscle strength for quantiles*/
goptions reset = all;
proc gplot data = interact1;
where VAR=0;
plot estimate*timepoint=interact/ haxis=axis1 vaxis=axis2 legend=legend1;
symbol1 cv=blue v=star h=2 i=j;
symbol2 cv=blue v=diamond h=2 i=j ;
symbol3 cv=blue v=square  h=2 i=j ;
symbol4 cv=blue v=circle h=2 i=j ;
symbol5 cv=blue v=triangle h=2 i=j ;
symbol6 cv=blue 					h=.8 i=j l=2;
symbol7 c=red v=star h=2 i=j;
symbol8 c=red v=diamond h=2 i=j;
symbol9 c=red v=square h=2 i=j;
symbol10 c=red v=circle h=2 i=j;
symbol11 c=red v=triangle h=2 i=j;
symbol12 c=red  				h=2 i=j l=2;
symbol13 c=green v=star h=2 i=j;
symbol14 c=green v=diamond h=2 i=j;
symbol15 c=green v=square h=2 i=j;
symbol16 c=green v=circle h=2 i=j;
symbol17 c=green v=traingle h=2 i=j;
symbol18 c=green 				h=2 i=j l=2;
format interact treatweight.;
axis1 label=(h=1 "Timepoint") minor=none order=(5 to 7 by 1);
axis2 label=(angle=90 "Quadriceps (kilos)") value=(h=1) offset=(0.1)cm minor=none;
title "Quadriceps per timepoint per risk-group";
run;


/*estimate muscle strength for median age and weight*/
proc mixed data=thesis.random_model CL method=reml;
	class id VAR timepoint gender treatment;
	model Y=VAR*timepoint*treatment VAR*timepoint*weight VAR*timepoint*age /noint s ddfm=KR;
	random VAR/ type=un subject=id;
	repeated VAR /type = UN subject=id*timepoint;
	where timepoint < 8;
	lsmeans VAR*timepoint*treatment/at weight=24.2 at age=6.498 cl; /*median*/
	ods output LSMeans=means_joint2;
	run;

data means_joint2_1;
	set means_joint2;
	output;
	if VAR=0 AND treatment="comm" then output; 
	if VAR=0 AND treatment="comm" then output;
	if VAR=0 AND treatment="comm" then output;
	if VAR=0 AND treatment="comm" then output; 
	if VAR=1 AND treatment="comm" then output; 
	if VAR=1 AND treatment="comm" then output;
	if VAR=1 AND treatment="comm" then output;
	if VAR=1 AND treatment="comm" then output;
	if VAR=2 AND treatment="comm" then output; 
	if VAR=2 AND treatment="comm" then output;
	if VAR=2 AND treatment="comm" then output;
	if VAR=2 AND treatment="comm" then output;
	if VAR=3 AND treatment="comm" then output; 
	if VAR=3 AND treatment="comm" then output;
	if VAR=3 AND treatment="comm" then output;
	if VAR=3 AND treatment="comm" then output;
	run;

data means_joint2_2;
	set means_joint2_1;
	IDNew=_n_;
	if idnew in (1,6,11,16,21, 40,45,50,55,60, 79,84,89,94,99, 118,123,128,133,138) then treatment="AR1";
	if idnew in (2,7,12,17,22, 41,46,51,56,61, 80,85,90,95,100, 119,124,129,134,139) then treatment="AR2B";
	if idnew in (3,8,13,18,23, 42,47,52,57,62, 81,86,91,96,101, 120,125,130,135,140) then treatment="AR2T";
	if idnew in (4,9,14,19,24, 43,48,53,58,63, 82,87,92,97,102, 121,126,131,136,141) then treatment="VLR";
	if idnew in (5,10,15,20,25,44,49,54,59,64, 83,88,93,98,103, 122,127,132,137,142) then treatment="VHR";
run;


goptions reset = all;
proc gplot data=means_joint2_2;
plot estimate*timepoint=treatment/legend=legend1 haxis=axis1 vaxis=axis2 autovref chref=(black black) skipmiss;
/*plot2 (upper lower)*timepoint=treatment/overlay;*/
where VAR=3;
symbol1 c=black i=j l=2 v=star h=2;
symbol2 c=black i=j l=2 v=diamond  h=2; 
symbol3 c=black i=j l=2 v=square   h=2;
symbol4 c=black i=j l=2 v=traingle h=2;
symbol5 c=black i=j l=2 v=circle   h=2;
legend1 label=(h=1.5 "risk-group") value=(h=1.5);
axis2 label=(h=2 A=90 "Pinch grip (kilos)") minor=none order=(1.8 to 3.9 by 0.20);
axis1 label=(h=2 "Timepoint") minor=none;
run;


/*data step muscle strength for median age and weight*/
data interact2;
set means_joint2_2;
if treatment="AR1" then treatmentnew=100;
else if treatment="AR2B" then treatmentnew=110;
else if treatment="AR2T" then treatmentnew=120;
else if treatment="VLR" then treatmentnew=130;
else if treatment="VHR" then treatmentnew=140;
else if treatment="comm" then treatmentnew=150;
else if treatment="com1" then treatmentnew=150;
else if treatment="com2" then treatmentnew=160;
if VAR=0 then VARnew=220;
else if VAR=1 then VARnew=2200;
else if VAR=2 then VARnew=22000;
else if VAR=3 then VARnew=220000;
interact2=treatmentnew*VARnew;
run;
proc format;
value treatVAR 
	22000 = "AR1-Q" 
	220000 = "AR1-T"
	2200000= "AR1-HG"
	22000000 = "AR1-PG"

	24200 = "AR2B-Q" 
	242000 = "AR2B-T"
	2420000 = "AR2B-HG"
	24200000 = "AR2B-PG"
 
	26400 = "AR2T-Q" 
	264000 = "AR2T-T"
	2640000 = "AR2T-HG"
	26400000 = "AR2T-PG"

	28600 = "VLR-Q"
	286000 = "VLR-T"
	2860000 = "VLR-HG"
	28600000 = "VLR-PG"

	30800 = "VHR-Q"
	308000 = "VHR-T"
	3080000 = "VHR-HG"
	30800000 = "VHR-PG"

	33000 = "comm-Q"
	330000 = "comm-T"
	3300000 = "comm-HG"
	33000000 = "comm-PG";
run;

proc sort data=interact2;
by timepoint;
run;
/*plot muscle strength for median age and weight*/
goptions reset = all;
proc gplot data = interact2;
plot estimate*timepoint=interact2/ haxis=axis1 vaxis=axis2 legend=legend1;
symbol1 cv=blue v=star h=2 i=j;
symbol2 cv=blue v=diamond h=2 i=j ;
symbol3 cv=blue v=square  h=2 i=j ;
symbol4 cv=blue v=circle h=2 i=j ;
symbol5 cv=blue v=triangle h=2 i=j ;
symbol6 cv=blue 					h=.8 i=j l=2;
symbol7 c=red v=star h=2 i=j;
symbol8 c=red v=diamond h=2 i=j;
symbol9 c=red v=square h=2 i=j;
symbol10 c=red v=circle h=2 i=j;
symbol11 c=red v=triangle h=2 i=j;
symbol12 c=red  				h=2 i=j l=2;
symbol13 c=black v=star h=2 i=j;
symbol14 c=black v=diamond h=2 i=j;
symbol15 c=black v=square h=2 i=j;
symbol16 c=black v=circle h=2 i=j;
symbol17 c=black v=traingle h=2 i=j;
symbol18 c=black  				h=2 i=j l=2;
symbol19 c=green v=star h=2 i=j;
symbol20 c=green v=diamond h=2 i=j;
symbol21 c=green v=square h=2 i=j;
symbol22 c=green v=circle h=2 i=j;
symbol23 c=green v=traingle h=2 i=j;
symbol24 c=green  				h=2 i=j l=2;
format interact2 treatVAR.; 
axis1 label=(h=1 "Timepoint") minor=none /*order=(5 to 7 by 1)*/;
axis2 label=(angle=90 "Muscle strength (kilos)") value=(h=1) offset=(0.1)cm minor=none;
title "Muslce strength per timepoint per risk-group";
run;



data prova;
	set  means_joint2;
	where VAR=0;
	keep timepoint treatment estimate;  
	run;
data prova1;
	set  prova;
	output;
	if treatment="comm" then output;  
	if treatment="comm" then output;
	if treatment="comm" then output;
	if treatment="comm" then output;
run;
data prova2;
	set prova1;
	IDNew=_n_; 
	if idnew in (1,6,11,16,21) then treatment="AR1";
	if idnew in (2,7,12,17,22) then treatment="AR2B";
	if idnew in (3,8,13,18,23) then treatment="AR2T";
	if idnew in (4,9,14,19,24) then treatment="VLR";
	if idnew in (5,10,15,20,25) then treatment="VHR";
	run;


data prova3;
set prova; 
output;
if timepoint=4 and treatment="comm" then output;
if timepoint=4 and treatment="comm" then output;
if timepoint=4 and treatment="comm" then output;
if timepoint=4 and treatment="comm" then output;
run; 
	
data prova4;
	set prova3;
	IDNew=_n_;
if  idnew=5 then treatment="AR1";
if  idnew=6 then treatment="AR2B";
if  idnew=7 then treatment="AR2T";
if  idnew=8 then treatment="VLR";
if  idnew=9 then treatment="VHR";
if timepoint<4 then timepoint1=timepoint;
else timepoint2=timepoint;
run;

data prova5; 
set prova4;
if treatment="comm" then newtreatment=1;
else if treatment="AR1" then newtreatment=10;
else if treatment="AR2B" then newtreatment=100;
else if treatment="AR2T" then newtreatment=1000;
else if treatment="VLR" then newtreatment=10000;
else if treatment="VHR" then newtreatment=100000;
interact=newtreatment*timepoint;
run;

goptions reset = all;
	proc gplot data=means_joint2_2;
	plot estimate*timepoint=treatment;
	symbol1 cv=blue v=star h=2 i=j;
	symbol2 cv=red v=diamond h=2 i=j ;
	symbol3 cv=green v=square  h=2 i=j ;
	symbol4 cv=black v=circle h=2 i=j ;
run;	


proc sgpanel DATA=means_joint2;
 PANELBY VAR;
 Series X = timepoint Y = estimate/ group=treatment;
 TITLE 'Number of Participants by Total Medals Won for Each Country';
run;




/****************** Model Checking ******************/
				/* using correlations*/

/*obtain OLS residuals*/
proc mixed data=thesis.random_model CL method=reml;
	class id VAR timepoint gender treatment;
	model Y=VAR*timepoint*treatment VAR*timepoint*weight VAR*timepoint*age /noint s ddfm=KR outp=predresid;
	/*random VAR/ type=un subject=id;*/
	/*repeated VAR /type = UN subject=id*timepoint;*/
	where timepoint < 8;
	run;

/*change format of dataset*/
data dataset0;
	set predresid;
	if VAR=0 then quad=resid;
	else if VAR=1 or VAR=2 or VAR=3 then delete;
	keep id timepoint VAR quad;
data dataset1;
	set predresid;
	if VAR=1 then tibialis=resid;
	else if VAR=0 or VAR=2 or VAR=3 then delete;
	keep id timepoint VAR tibialis;
data dataset2;
	set predresid;
	if VAR=2 then hand=resid;
	else if VAR=0 or VAR=1 or VAR=3 then delete;
	keep id timepoint VAR hand;
data dataset3;
	set predresid;
	if VAR=3 then pinch=resid; 
	else if VAR=0 or VAR=1 or VAR=2 then delete;
	keep id timepoint VAR pinch;
data same_timepoint;
	merge dataset0 dataset1 dataset2 dataset3;
 	by id timepoint;
 	keep id timepoint quad tibialis hand pinch;
run;

proc sort data=same_timepoint;
	by timepoint;
run;

/*corr quadriceps - tibialis*/
proc corr data=same_timepoint /*NOPROB*/ /*NOSIMPLE*/ /*fisher*/ outp=CorrOutp;
	by timepoint;
	var tibialis pinch;
	ods select pearsoncorr; 
	ods output pearsoncorr=corr_quad_tib; 
run;

data corr_quad_tib;
	set corr_quad_tib;
	if Variable = pinch then delete;
	if tibialis = 1 then delete;
	upperCL = 0.75;
	lowerCL = 0.25;
	r = 0.56;
	keep timepoint tibialis upperCL lowerCL r;
run;

proc means data=corr_quad_tib mean;
var tibialis;
run;


goptions reset=all;
proc gplot data=corr_quad_tib;
	plot (quad r upperCL lowerCL quad)*timepoint/vaxis=axis1 haxis=axis2 nolegend overlay;
	title 'Observed (dots) and implied (dashed line) correlation (quad-tibialis)';
	symbol1 i=join l=1 v=circle color=black;
	symbol2 i=join l=2  color=black;                                                                                           
	symbol3 i=join l=3  color=black;
	symbol4 i=join l=3  color=black;
	axis1 minor=none label=(angle=90 'Correlation');
	axis2 minor=none label=('Timepoints'); 
run;


/*corr quadriceps - pinch grip*/
proc corr data=same_timepoint /*NOPROB*/ /*NOSIMPLE*/ /*fisher*/ outp=CorrOutp;
	by timepoint;
	var quad pinch;
	ods select pearsoncorr; 
	ods output pearsoncorr=corr_quad_pinch; 
run;

data corr_quad_pinch;
	set corr_quad_pinch;
	if Variable = quad then delete;
	if quad = 1 then delete;
	upperCL = 0.68;
	lowerCL = -0.20;
	r = 0.30;
	keep timepoint quad upperCL lowerCL r;
run;

goptions reset=all;
proc gplot data=corr_quad_pinch;
	plot (quad r upperCL lowerCL quad)*timepoint/vaxis=axis1 haxis=axis2 nolegend overlay;
	title 'Observed (dots) and implied (dashed line) correlation (quad-pinch)';
	symbol1 i=join l=1 v=circle color=black;
	symbol2 i=join l=2  color=black;                                                                                           
	symbol3 i=join l=3  color=black;
	symbol4 i=join l=3  color=black;
	axis1 minor=none label=(angle=90 'Correlation');
	axis2 minor=none label=('Timepoints'); 
run;



/*shift response by 1 timepoint*/
data diff_timepoints;
	merge same_timepoint same_timepoint (firstobs=85 rename=(tibialis=tibialis_new hand=hand_new pinch=pinch_new));
run;/*43,85,126,164,201,234,257 */

proc sort data=diff_timepoints;
	by timepoint;
run;

/*corr quadriceps - tibialis*/
proc corr data=diff_timepoints /*NOPROB*/ /*NOSIMPLE*/ /*fisher*/ outp=CorrOutp;
	by timepoint;
	var quad tibialis;
	ods select pearsoncorr; 
	ods output pearsoncorr=corr_quad_tib; 
run;

data corr_quad_tib;
	set corr_quad_tib;
	if Variable = quad then delete;
	if quad = 1 then delete;
	upperCL = 0.46;
	lowerCL = 0.10;
	r = 0.29;
	keep timepoint quad upperCL lowerCL r;
run;

proc means data=corr_quad_tib mean;
	var quad;
run;

goptions reset=all;
proc gplot data=corr_quad_tib;
	plot (quad r upperCL lowerCL quad)*timepoint/vaxis=axis1 haxis=axis2 nolegend overlay;
	title 'Observed (dots) and implied (dashed line) correlation (quad-tibialis)';
	symbol1 i=join l=1 v=circle color=black;
	symbol2 i=join l=2  color=black;                                                                                           
	symbol3 i=join l=3  color=black;
	symbol4 i=join l=3  color=black;
	axis1 minor=none label=(angle=90 'Correlation');
	axis2 minor=none label=('Timepoints'); 
run;


/*corr quadriceps - pinch grip*/
proc corr data=diff_timepoints /*NOPROB*/ /*NOSIMPLE*/ /*fisher*/ outp=CorrOutp;
	by timepoint;
	var quad pinch;
	ods select pearsoncorr; 
	ods output pearsoncorr=corr_quad_pinch; 
run;

data corr_quad_pinch;
	set corr_quad_pinch;
	if Variable=quad then delete;
	if quad=1 then delete;
	upperCL = 0.60;
	lowerCL = -0.24;
	r = 0.22;
	keep timepoint quad upperCL lowerCL r;
run;
goptions reset=all;
proc gplot data=corr_quad_pinch;
	plot (quad r upperCL lowerCL quad)*timepoint/vaxis=axis1 haxis=axis2 nolegend overlay;
	title 'Observed (dots) and implied (dashed line) correlation (quad-pinch)';
	symbol1 i=join l=1 v=circle color=black;
	symbol2 i=join l=2  color=black;                                                                                           
	symbol3 i=join l=3  color=black;
	symbol4 i=join l=3  color=black;
	axis1 minor=none label=(angle=90 'Correlation');
	axis2 minor=none label=('Timepoints'); 
run;


/*power calculations*/
proc mixed data=thesis.random_model CL method=reml;
	class id VAR timepoint gender treatment;
	model Y=VAR*timepoint*treatment VAR*timepoint*weight VAR*timepoint*age /noint s ddfm=KR;
	random VAR/ type=un subject=id;
	repeated VAR /type = UN subject=id*timepoint;
	where timepoint < 8;
	ods output contrasts=c;
	
	contrast "all vs VHR at T6"
	VAR*treatment*timepoint 0 0 0 0 0 1 0 0 -1 0, /*for Y1*/
	VAR*treatment*timepoint	0 0 0 0 0 0 1 0 -1 0,
	VAR*treatment*timepoint	0 0 0 0 0 0 0 1 -1 0,
	VAR*treatment*timepoint	0 0 0 0 0 0 0 0 -1 1,
	
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 -1 0,/*for Y2*/
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 -1 0,					
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0,						
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 1, 
	
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 /*for Y3*/
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 -1 0,						
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 -1 0,
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0,
	VAR*treatment*timepoint 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 1,
							
	VAR*treatment*timepoint	0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 /*for Y4*/
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 -1 0,
	VAR*treatment*timepoint	0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 -1 0,
	VAR*treatment*timepoint	0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0,
	VAR*treatment*timepoint	0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 1/E;
	run;	

data f_power;
	set c;
	Noncen = NumDF*FValue;
	Alpha = 0.05;
	FCrit = finv(1-Alpha,NumDF,DenDF,0);
	Power = 1 - probf(FCrit,NumDF,DenDF,Noncen);
run;
/*intepretation: the probability of observing a weight*VAR*timepoint F-test p-value less than α = 0.05 
is approximately 0.9885*/
proc print data=f_power;
run;	

