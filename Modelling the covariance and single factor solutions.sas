/*Read in data and center for quadratic time variable*/
Data erf;
	infile "J:\SAS\Longitudinal Data Analysis\Final Project\data_4_var_reading_full.csv" firstobs=2 dlm=",";
	input id $ region urban gender family income parentEd english fullTime reading t;
run;
Data erf;
	set erf;
	time = t - 4.25;
	tsq = time*time;
Run;



/*find the best covariance*/
proc Mixed data = erf method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parentEd(ref="0") english(ref="0") fullTime(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time tsq region time*region urban time*urban gender time*gender parentEd time*parentEd english time*english fullTime time*fullTime/s chisq;
	Repeated t /type=un subject=id;
Run;
/*Fit Statistics 
-2 Log Likelihood 651380.3 
AIC (Smaller is Better) 651468.3 
AICC (Smaller is Better) 651468.4 
BIC (Smaller is Better) 651813.1 



Null Model Likelihood Ratio Test 
DF Chi-Square Pr > ChiSq 
20 70169.71 <.0001 
*/
proc Mixed data = erf method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parentEd(ref="0") english(ref="0") fullTime(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time tsq region time*region urban time*urban gender time*gender parentEd time*parentEd english time*english fullTime time*fullTime/s chisq;
	Repeated t /type=cs subject=id;
Run;
/*Fit Statistics 
-2 Log Likelihood 690278.0 
AIC (Smaller is Better) 690328.0 
AICC (Smaller is Better) 690328.0 
BIC (Smaller is Better) 690523.8 



Null Model Likelihood Ratio Test 
DF Chi-Square Pr > ChiSq 
1 31272.09 <.0001 
*/
proc Mixed data = erf method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parentEd(ref="0") english(ref="0") fullTime(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time tsq region time*region urban time*urban gender time*gender parentEd time*parentEd english time*english fullTime time*fullTime/s chisq;
	Repeated t /type=ar(1) subject=id;
Run;
/*Fit Statistics 
-2 Log Likelihood 689357.0 
AIC (Smaller is Better) 689407.0 
AICC (Smaller is Better) 689407.0 
BIC (Smaller is Better) 689602.8 



Null Model Likelihood Ratio Test 
DF Chi-Square Pr > ChiSq 
1 32193.08 <.0001 
*/
proc Mixed data = erf method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parentEd(ref="0") english(ref="0") fullTime(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time tsq region time*region urban time*urban gender time*gender parentEd time*parentEd english time*english fullTime time*fullTime/s chisq;
	Repeated t /type=TOEP subject=id;
Run;
/*Fit Statistics 
-2 Log Likelihood 674854.8 
AIC (Smaller is Better) 674912.8 
AICC (Smaller is Better) 674912.8 
BIC (Smaller is Better) 675140.0 



Null Model Likelihood Ratio Test 
DF Chi-Square Pr > ChiSq 
5 46695.29 <.0001 
*/
proc Mixed data = erf method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parentEd(ref="0") english(ref="0") fullTime(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time tsq region time*region urban time*urban gender time*gender parentEd time*parentEd english time*english fullTime time*fullTime/s chisq;
	Repeated t /type=TOEPH subject=id;
Run;
/*Fit Statistics 
-2 Log Likelihood 656502.4 
AIC (Smaller is Better) 656570.4 
AICC (Smaller is Better) 656570.4 
BIC (Smaller is Better) 656836.8 



Null Model Likelihood Ratio Test 
DF Chi-Square Pr > ChiSq 
10 65047.63 <.0001 
*/
proc Mixed data = erf method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parentEd(ref="0") english(ref="0") fullTime(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time tsq region time*region urban time*urban gender time*gender parentEd time*parentEd english time*english fullTime time*fullTime/s chisq;
	Repeated t /type=CSH subject=id;
Run;
/*Fit Statistics 
-2 Log Likelihood 664438.2 
AIC (Smaller is Better) 664498.2 
AICC (Smaller is Better) 664498.3 
BIC (Smaller is Better) 664733.3 



Null Model Likelihood Ratio Test 
DF Chi-Square Pr > ChiSq 
6 57111.80 <.0001 
*/
proc Mixed data = erf method=ML;
	Class id region(ref="1") urban(ref="3") gender(ref="0") parentEd(ref="0") english(ref="0") fullTime(ref="0") t(ref="0");
	/*rural is the reference level for urban; high school is the reference level for parent_ed; speaking english at home is the reference level for non_english*/
	Model reading = time tsq region time*region urban time*urban gender time*gender parentEd time*parentEd english time*english fullTime time*fullTime/s chisq;
	Repeated t /type=VC subject=id;
Run;
/*Fit Statistics 
-2 Log Likelihood 721550.0 
AIC (Smaller is Better) 721598.0 
AICC (Smaller is Better) 721598.1 
BIC (Smaller is Better) 721786.1 



Null Model Likelihood Ratio Test 
DF Chi-Square Pr > ChiSq 
0 0.00 1.0000 
*/



/*#########################*/
/*Exploratory Data Analysis*/
/*Single variable solutions for creating Graphs 1-6 in the report*/
Proc Mixed data = erf method = ML;
	Class id t(ref="0") gender(ref="0");
	Model reading = time tsq gender time*gender /s chisq;
	Repeated t /type=un subject = id;
Run;
Proc Mixed data = erf method = ML;
	Class id t(ref="0") region(ref="1");
	Model reading = time tsq region time*region /s chisq;
	Repeated t /type=un subject = id;
Run;
Proc Mixed data = erf method = ML;
	Class id t(ref="0") urban(ref="3");
	Model reading = time tsq urban time*urban /s chisq;
	Repeated t /type=un subject = id;
Run;
Proc Mixed data = erf method = ML;
	Class id t(ref="0") parentEd(ref="0");
	Model reading = time tsq parentEd time*parentEd /s chisq;
	Repeated t /type=un subject = id;
Run;
Proc Mixed data = erf method = ML;
	Class id t(ref="0") english(ref="0");
	Model reading = time tsq english time*english /s chisq;
	Repeated t /type=un subject = id;
Run;
Proc Mixed data = erf method = ML;
	Class id t(ref="0") fullTime(ref="0");
	Model reading = time tsq fullTime time*fullTime /s chisq;
	Repeated t /type=un subject = id;
Run;
/*The coefficients from each model can be used to create Graph 1-6,
which show mean reading score vs. time for different sub-groups of students*/
