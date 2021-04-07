/* SAS Lab 2 - Stress Echo - Survival Analysis */

/* Set Working directory */

LIBNAME LAB '\\apporto.com\dfs\depaul\Users\sbesser1_depaul\Documents\Week 1_SAS_Lab';


/* View what is available in the Library */

PROC CONTENTS DATA=lab._ALL_ NODS;
RUN;

/* Read in Dataset */

PROC IMPORT DATAFILE="\\apporto.com\dfs\depaul\Users\sbesser1_depaul\Documents\Week 1_SAS_Lab\stress_echo_time.csv" 
    OUT=LAB.stress_echo
    DBMS=csv
    REPLACE;
    GETNAMES=YES;
RUN;

/* Check that File was Read in Correctly */

PROC PRINT DATA=LAB.stress_echo; RUN;

/* Check Structure of the File and Variables */
/* ORDER can order the variables in a variety of ways, varnum orders by variable number */

PROC CONTENTS DATA=LAB.stress_echo ORDER=varnum; RUN;

/* Variables Defined

Explanation of Data Measurement Abbreviations in the Data File

bhr	basal heart rate
basebp	basal blood pressure
basedp	basal double product (= bhr x basebp)
pkhr	peak heart rate
sbp	systolic blood pressure
dp	double product (= pkhr x sbp)
dose	dose of dobutamine given
maxhr	maximum heart rate
%mphr(b)	% of maximum predicted heart rate achieved
mbp	maximum blood pressure
dpmaxdo	double product on maximum dobutamine dose
dobdose	dobutamine dose at which maximum double product occured
age	age
gender	gender
baseef	baseline cardiac ejection fraction (a measure of the heart's pumping efficiency)
dobef	ejection fraction on dobutamine
chestpain	1 means experienced chest pain
posecg	signs of heart attack on ecg (1 = yes)
equivecg	ecg is equivocal (1 = yes)
restwma	cardiologist sees wall motion anamoly on echocardiogram (1 = yes)
posse	stress echocardiogram was positive (1 = yes)
newmi	new myocardial infarction, or heart attack (1 = yes)
newptca	recent angioplasty (1 = yes)
newcabg	recent bypass surgery (1 = yes)
death	died (1 = yes)
hxofht	history of hypertension (1 = yes)
hxofdm	history of diabetes (1 = yes)
hxofcig	history of smoking (1 = yes)
hxofmi	history of heart attack (1 = yes)
hxofptca	history of angioplasty (1 = yes)
hxofcabg	history of bypass surgery (1 = yes)
any event	Outcome variable, defined as "death or newmi or newptca or newcabg". if any of these variables is positive (= 1) then "any event" is also postive (= 1).

*/

/* ROC Curves -  Determining Optimal Cutoff Point */
ODS GRAPHIC ON;
PROC LOGISTIC DATA = LAB.stress_echo;
 MODEL any_event (EVENT='1')=baseEF/OUTROC=ROCDATA;
 ROC; ROCCONTRAST;
RUN;
ODS GRAPHIC OFF;

DATA ROCDATA;
	SET ROCDATA;

	/* Note:  Remember to change the cutoff formula for appropriate intercept and slope from the logistic regression above */
    /* cutoff = (logit+Intercept)/slope; */
	/* Choose cutoff with maximum Youden */

	logit=log(_prob_/(1-_prob_));
	cutoff=(logit+1.1058)/-0.0515;
	prob= _prob_;
	Sensitivity = _SENSIT_;
	Specificity = 1-_1MSPEC_;
	Youden= _SENSIT_+ (1-_1MSPEC_)-1;
RUN;

PROC SORT DATA=ROCDATA DESCENDING; 
	BY Youden;
RUN;

PROC PRINT DATA=ROCDATA; RUN;

/* What is the optimal ROC Cutoff Point */

*************************************************************;
/* Kaplan-Meier Curve Analysis */

/* time_var = time to event variable (i.e. Time to Any Cardiovascular Event)
   censor_var = censored variable (i.e. Any Cardiovascular Event)
   strata_var = Strata Variable (i.e. Gender, Cutoff-Variable) */

PROC LIFETEST DATA=LAB.stress_echo PLOTS=survival(atrisk=0 to 365 by 60);
  TIME time_to_any_event*any_event(0);
  STRATA gender;
RUN;
*************************************************************;
/* Cox-Proportional Hazards Regression */

/*Note:  0 should be used for any_event, so the 0s can be excluded as non-censored events (i.e. the events did not occur) */

PROC PHREG DATA = LAB.stress_echo;
	CLASS gender;
	MODEL time_to_any_event*any_event(0)= gender / RISKLIMITS TIES=efron;
RUN;
