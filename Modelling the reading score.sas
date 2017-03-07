/*############################################*/
/*Read in the data, start with the full model*/
Data ecls_reading;
	infile "J:\SAS\Longitudinal Data Analysis\Final Project\data_4_var_reading_full.csv" firstobs=2 dlm=",";
	input id $ region urban gender family_type income parent_ed non_english full_time reading time;
run;
/*sort by id and time;*/
proc sort data = ecls_reading;
	by id descending time;
run;



/*###############################################*/
/*The model for reading score, parametric curve (time as continuous)*/
Data ecls_reading2;
set ecls_reading;
t=time;
time=time-4.25;
time_sq=time*time;
run;
/*Model 1:linear trend model*/
proc Mixed data = ecls_reading2 method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parent_ed(ref="0") non_english(ref="0") full_time(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time region time*region urban time*urban gender time*gender parent_ed time*parent_ed non_english time*non_english full_time time*full_time /s chisq;
	Repeated t /type=un subject=id;
Run;
/*-2 Log Likelihood=647471.4*/
/*AIC=647557.4*/
/*Model 2: quadratic trend model, even including interaction between time_sq and each predictors*/
proc Mixed data = ecls_reading2 method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parent_ed(ref="0") non_english(ref="0") full_time(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time time_sq region time*region time_sq*region urban time*urban time_sq*urban gender time*gender time_sq*gender parent_ed time*parent_ed time_sq*parent_ed non_english time*non_english time_sq*non_english full_time time*full_time time_sq*full_time/s chisq;
	Repeated t /type=un subject=id;
Run;
/*-2 Log Likelihood=631038.9*/
/*AIC=631146.9*/
/*Model 3: quadratic trend model, without any interaction for time_sq*/
proc Mixed data = ecls_reading2 method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parent_ed(ref="0") non_english(ref="0") full_time(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time time_sq region time*region urban time*urban gender time*gender parent_ed time*parent_ed non_english time*non_english full_time time*full_time /s chisq residual outp=reading_resid;
	Repeated t /type=un subject=id;
Run;
/*-2 Log Likelihood=631656.7*/
/*AIC=631744.7*/
/*Model 4: quadratic trend model, without any interaction for time_sq, and remove time*non_english*/
proc Mixed data = ecls_reading2 method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parent_ed(ref="0") non_english(ref="0") full_time(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time time_sq region time*region urban time*urban gender time*gender parent_ed time*parent_ed non_english full_time time*full_time /s chisq residual outp=reading_resid;
	Repeated t /type=un subject=id;
Run;
/*-2 Log Likelihood=631657.4*/
/*AIC=631743.4*/
/*LRT (compare model 4 and 3): p-value=0.70*/



/*########################################*/
/*stepwise backward variable selection*/
/*######################################*/
/*reduced model with 5 predictors*/
/*remove full_time*/
proc Mixed data = ecls_reading2 method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parent_ed(ref="0") non_english(ref="0") full_time(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time time_sq region time*region urban time*urban gender time*gender parent_ed time*parent_ed non_english time*non_english/s chisq residual outp=reading_resid;
	Repeated t /type=un subject=id;
Run;
/*-2 Log Likelihood=631708.8*/
/*remove urbanicity*/
proc Mixed data = ecls_reading2 method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parent_ed(ref="0") non_english(ref="0") full_time(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time time_sq region time*region gender time*gender parent_ed time*parent_ed non_english time*non_english full_time time*full_time /s chisq residual outp=reading_resid;
	Repeated t /type=un subject=id;
Run;
/*-2 Log Likelihood=631753.5*/
/*remove region*/
proc Mixed data = ecls_reading2 method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parent_ed(ref="0") non_english(ref="0") full_time(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time time_sq urban time*urban gender time*gender parent_ed time*parent_ed non_english time*non_english full_time time*full_time /s chisq residual outp=reading_resid;
	Repeated t /type=un subject=id;
Run;
/*-2 Log Likelihood=631733.0*/
/*remove gender*/
proc Mixed data = ecls_reading2 method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parent_ed(ref="0") non_english(ref="0") full_time(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time time_sq region time*region urban time*urban parent_ed time*parent_ed non_english time*non_english full_time time*full_time /s chisq residual outp=reading_resid;
	Repeated t /type=un subject=id;
Run;
/*-2 Log Likelihood=631766.1*/
/*remove parent_ed*/
proc Mixed data = ecls_reading2 method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parent_ed(ref="0") non_english(ref="0") full_time(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time time_sq region time*region urban time*urban gender time*gender non_english time*non_english full_time time*full_time /s chisq residual outp=reading_resid;
	Repeated t /type=un subject=id;
Run;
/*stopped because of infinite likelihood*/
/*remove non_english*/
proc Mixed data = ecls_reading2 method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parent_ed(ref="0") non_english(ref="0") full_time(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time time_sq region time*region urban time*urban gender time*gender parent_ed time*parent_ed full_time time*full_time /s chisq residual outp=reading_resid;
	Repeated t /type=un subject=id;
Run;
/*-2 Log Likelihood=631809.6*/
/*so first predictor removed is full_time*/

/*######################################*/
/*reduced model with 4 predictors*/
/*remove region*/
proc Mixed data = ecls_reading2 method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parent_ed(ref="0") non_english(ref="0") full_time(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time time_sq urban time*urban gender time*gender parent_ed time*parent_ed non_english time*non_english/s chisq residual outp=reading_resid;
	Repeated t /type=un subject=id;
Run;
/*-2 Log Likelihood=631789.7*/
/*remove urban*/
proc Mixed data = ecls_reading2 method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parent_ed(ref="0") non_english(ref="0") full_time(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time time_sq region time*region gender time*gender parent_ed time*parent_ed non_english time*non_english/s chisq residual outp=reading_resid;
	Repeated t /type=un subject=id;
Run;
/*-2 Log Likelihood=631793.1*/
/*remove gender*/
proc Mixed data = ecls_reading2 method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parent_ed(ref="0") non_english(ref="0") full_time(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time time_sq region time*region urban time*urban parent_ed time*parent_ed non_english time*non_english/s chisq residual outp=reading_resid;
	Repeated t /type=un subject=id;
Run;
/*-2 Log Likelihood=631819.0*/
/*remove parent_ed*/
proc Mixed data = ecls_reading2 method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parent_ed(ref="0") non_english(ref="0") full_time(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time time_sq region time*region urban time*urban gender time*gender non_english time*non_english/s chisq residual outp=reading_resid;
	Repeated t /type=un subject=id;
Run;
/*-2 Log Likelihood=633969.7*/
/*remove non_english*/
proc Mixed data = ecls_reading2 method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parent_ed(ref="0") non_english(ref="0") full_time(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time time_sq region time*region urban time*urban gender time*gender parent_ed time*parent_ed/s chisq residual outp=reading_resid;
	Repeated t /type=un subject=id;
Run;
/*-2 Log Likelihood=631859.9*/
/*So the second predictor removed is region*/

/*######################################*/
/*reduced model with 3 predictors*/
/*remove urban*/
proc Mixed data = ecls_reading2 method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parent_ed(ref="0") non_english(ref="0") full_time(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time time_sq gender time*gender parent_ed time*parent_ed non_english time*non_english/s chisq residual outp=reading_resid;
	Repeated t /type=un subject=id;
Run;
/*-2 Log Likelihood=631882.4*/
/*remove gender*/
proc Mixed data = ecls_reading2 method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parent_ed(ref="0") non_english(ref="0") full_time(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time time_sq urban time*urban parent_ed time*parent_ed non_english time*non_english/s chisq residual outp=reading_resid;
	Repeated t /type=un subject=id;
Run;
/*-2 Log Likelihood=631902.8*/
/*remove parent_ed*/
proc Mixed data = ecls_reading2 method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parent_ed(ref="0") non_english(ref="0") full_time(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time time_sq region time*region gender time*gender non_english time*non_english/s chisq residual outp=reading_resid;
	Repeated t /type=un subject=id;
Run;
/*stopped because of infinite likelihood*/
/*remove non_english*/
proc Mixed data = ecls_reading2 method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parent_ed(ref="0") non_english(ref="0") full_time(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time time_sq region time*region gender time*gender parent_ed time*parent_ed/s chisq residual outp=reading_resid;
	Repeated t /type=un subject=id;
Run;
/*-2 Log Likelihood=631930.4*/
/*so the third predictor removed is urban*/

/*######################################*/
/*reduced model with 2 predictors*/
/*remove gender*/
proc Mixed data = ecls_reading2 method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parent_ed(ref="0") non_english(ref="0") full_time(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time time_sq parent_ed time*parent_ed non_english time*non_english/s chisq residual outp=reading_resid;
	Repeated t /type=un subject=id;
Run;
/*-2 Log Likelihood=631994.8*/
/*remove parent_ed*/
proc Mixed data = ecls_reading2 method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parent_ed(ref="0") non_english(ref="0") full_time(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time time_sq gender time*gender non_english time*non_english/s chisq residual outp=reading_resid;
	Repeated t /type=un subject=id;
Run;
/*-2 Log Likelihood=634322.7*/
/*remove non_english*/
proc Mixed data = ecls_reading2 method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parent_ed(ref="0") non_english(ref="0") full_time(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time time_sq gender time*gender parent_ed time*parent_ed /s chisq residual outp=reading_resid;
	Repeated t /type=un subject=id;
Run;
/*-2 Log Likelihood=632030.8*/
/*so the fourth predictor removed is gender*/

/*######################################*/
/*reduced model with 1 predictors*/
/*remove parent_ed*/
proc Mixed data = ecls_reading2 method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parent_ed(ref="0") non_english(ref="0") full_time(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time time_sq non_english time*non_english/s chisq residual outp=reading_resid;
	Repeated t /type=un subject=id;
Run;
/*-2 Log Likelihood=634408.9*/
/*remove non_english*/
proc Mixed data = ecls_reading2 method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parent_ed(ref="0") non_english(ref="0") full_time(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time time_sq parent_ed time*parent_ed /s chisq residual outp=reading_resid;
	Repeated t /type=un subject=id;
Run;
/*-2 Log Likelihood=632142.2*/
/*so the fifth predictor removed is non_english, the last one left in the model is parent_ed*/



/*#####################################################*/
/*residual analyses and diagnostics for the final model*/
proc Mixed data = ecls_reading2 method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parent_ed(ref="0") non_english(ref="0") full_time(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time time_sq region time*region urban time*urban gender time*gender parent_ed time*parent_ed non_english full_time time*full_time /s chisq residual outp=reading_resid;
	Repeated t /type=un subject=id;
Run;
/*Histogram*/
 proc sgplot data=reading_resid;
  title "Histogram of Residual Distribution";
  histogram resid;
 run;
/*Q-Q plot*/
proc univariate data=reading_resid noprint;
   title "Q-Q Plot";
   qqplot resid / normal(mu=est sigma=est color=red l=2);
run;
/*Scatterplot of residuals vs. predicted values, together with loess smoothed curve*/
 proc loess data=reading_resid; 
 model resid=pred / smooth=0.5 degree=2; 
 ods output OutputStatistics=Results; 
 run;
symbol1 color=black value=dot height=0.5;   
symbol2 color=green i=join value=none;
   /* macro used in subsequent examples */ 
   %let opts=vaxis=axis1 hm=3 vm=3 overlay; 
   axis1 label=(angle=90 rotate=0); 
 proc gplot data=Results;  
 title1 'Residual vs. Predicted Values, with LOESS smoothing'; 
 plot DepVar*pred pred2*pred /&opts; 
 run;



 /*####################################################################################################*/
 /*Try mixed-effects model, assuming random intercept; based on the final parametric curve model*/
proc Mixed data = ecls_reading2 method=ML noclprint=10 covtest;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parent_ed(ref="0") non_english(ref="0") full_time(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time time_sq region time*region urban time*urban gender time*gender parent_ed time*parent_ed non_english full_time time*full_time /s chisq covb;
	Random intercept / subject=id type=un g gcorr;
Run;
/*-2 Log Likelihood=668874.0*/
/*AIC=668922.0*/
 /*Try mixed-effects model, assuming both random intercept and slope (time); based on the final parametric curve model*/
proc Mixed data = ecls_reading2 method=ML noclprint=10 covtest;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parent_ed(ref="0") non_english(ref="0") full_time(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time time_sq region time*region urban time*urban gender time*gender parent_ed time*parent_ed non_english full_time time*full_time /s chisq covb;
	Random intercept time/ subject=id type=un g gcorr;
Run;
/*ERROR: Integer overflow on computing amount of memory required. A request to allocate the memory cannot be honored.*/
