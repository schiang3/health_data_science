/* #Week 1 SAS Lab - Stress Echo */

/* Set Working directory */

LIBNAME LAB '\\apporto.com\dfs\depaul\Users\sbesser1_depaul\Documents\Week 1_SAS_Lab';


/* View what is available in the Library */

PROC CONTENTS DATA=lab._ALL_ NODS;
RUN;

/* Read in Dataset */

PROC IMPORT DATAFILE="\\apporto.com\dfs\depaul\Users\sbesser1_depaul\Documents\Week 1_SAS_Lab\stressEcho.csv" 
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

/* Show Levels of Gender, hxofCig, ecg Variable */

PROC FREQ DATA=LAB.stress_echo;
	TABLES Gender hxofCig ecg;
RUN;


/* Check for Normality of Age and Plot Age, as well as Percentiles */

PROC UNIVARIATE DATA=LAB.stress_echo NORMAL PLOT;
	VAR AGE;
RUN;


/* Create an Age Categorical Variable using Tertiles */

PROC RANK DATA = LAB.stress_echo OUT=LAB.stress_echo2 GROUPS = 3;
	VAR age;
	RANKS age_tert;
RUN;

/* Uses the cutoff points 63 and 73 */

PROC CONTENTS DATA=LAB.stress_echo2 ORDER=varnum; RUN;

PROC FREQ DATA=LAB.stress_echo2;
	TABLES age age_tert;
RUN;


PROC FORMAT;
	VALUE age_tert

		0 = "26-63"
		1 = "63-73"
		2 = "73-93";

RUN;

/* Need to update dataset using DATA and SET commands */
/* DATA would be the Updated dataset and SET is using the OLD dataset */

DATA LAB.stress_echo2;
	SET LAB.stress_echo2;

	FORMAT age_tert age_tert.;

	LABEL age_tert =  "Age Tertiles";


/* Need to change Gender, hxofCig, and ecg variables to read as numeric variables instead of categorical variables */

	/* Gender */
	IF Gender="female" THEN gender_num=0;
    IF Gender="male" THEN gender_num=1;

	/* hxofCig */
	IF hxofCig="non-smoker" THEN hxofCig_num=0;
    IF hxofCig="moderate" THEN hxofCig_num=1;
	IF hxofCig="heavy" THEN hxofCig_num=2;

	/* ecg */
	IF ecg="normal" THEN ecg_num=0;
    IF ecg="equivocal" THEN ecg_num=1;
	IF ecg="MI" THEN ecg_num=2;

RUN;

/* Check Variables were created */

PROC CONTENTS DATA=LAB.stress_echo2 ORDER=varnum; RUN;

PROC FREQ DATA=LAB.stress_echo2;
	TABLES gender_num hxofCig_num ecg_num;
RUN;

/* Check for Normality of Continuous Variables, as well as Percentiles */

PROC UNIVARIATE DATA=LAB.stress_echo NORMAL PLOT;
	VAR basebp	basedp	pkhr sbp dp	dose maxhr pctMphr mbp dpmaxdo dobdose	age	baseEF	dobEF;
RUN;


/* Check Descriptive Statistics of Continuous Variables */

PROC MEANS DATA=LAB.stress_echo2 N NMISS MEAN STD MIN Q1 MEDIAN Q3 MAX SKEWNESS KURTOSIS MAXDEC=2;
	VAR basebp	basedp	pkhr sbp dp	dose maxhr pctMphr mbp dpmaxdo dobdose	age	baseEF	dobEF;
RUN;
*********************************************************************************************;
/* Research Question: Is there a relationship between hxofCig and hxofMI? */

PROC FREQ DATA=LAB.stress_echo2;
	TABLES hxofCig*hxofMI / CHISQ;
RUN;

*********************************************************************************************;
/* Research Question:  Is there a difference between gender for Baseline Blood Pressure? */

PROC SORT DATA=Lab.stress_echo2;
	BY gender;
RUN;

PROC UNIVARIATE DATA=LAB.stress_echo2 NORMAL PLOT CIPCTLDF;
	BY gender;
	VAR basebp;
	HISTOGRAM basebp / NORMAL;
	QQPLOT / NORMAL (MU=est SIGMA=est);
RUN;

/* Check Normality Visually Looking at Boxplots */

PROC SGPLOT DATA=LAB.stress_echo2;
	TITLE "Boxplots of Baseline BP by Gender";
	VBOX baseBP / Category=Gender;
RUN;

/* What doe the normality tests tell us? */

/* What is the appropriate test to use? */


/* Independent (Student) T-test */
PROC TTEST DATA=LAB.stress_echo2;
	CLASS Gender;
	VAR baseBP;
RUN;

/* Mann-Whitney U (Wilcoxon) test - Nonparametric T-Test */

PROC NPAR1WAY DATA=LAB.stress_echo2 WILCOXON;
	CLASS Gender;
	VAR baseBP;
RUN;
*************************************************************************************************;
/* Research Question:  Is there a difference between ages for Baseline EF (Ejection Fraction)? */

/* Test for Normality and Check Normality Visually with Histogram with Normal Curve */

/* Make sure to sort the data first */

PROC SORT DATA=Lab.stress_echo2;
	BY age_tert;
RUN;

PROC UNIVARIATE DATA=LAB.stress_echo2 NORMAL PLOT CIPCTLDF;
	BY age_tert;
	VAR baseEF;
	HISTOGRAM baseEF / NORMAL;
	QQPLOT / NORMAL (MU=est SIGMA=est);
RUN;

/* Check Normality Visually Looking at Boxplots */

PROC SGPLOT DATA=LAB.stress_echo2;
	TITLE "Boxplots of Baseline EF by Age Tertiles";
	VBOX baseEF / Category=age_tert;
RUN;

/* What do the tests of Normality tell us? */

/* What test is appropriate to use and why? */

/* One-Way ANOVA */
PROC ANOVA DATA=LAB.stress_echo2;
	CLASS age_tert;
	MODEL baseEF = age_tert;
	MEANS age_tert / SNK;
RUN;

/* Kruskal-Wallis test - Nonparametric ANOVA */
PROC NPAR1WAY DATA=LAB.stress_echo2 WILCOXON DSCF;
	CLASS age_tert;
	VAR baseEF;
RUN;
*****************************************************************************;

/* Research Question:  Is there a difference between baselineEF and dobEF? */

/* Paired T-Test */

PROC TTEST DATA=LAB.stress_echo2;
	PAIRED baseEF * dobEF;
RUN;

/* Paired T-Test - Nonparametric (Sign Rank test) */

DATA LAB.stress_echo2;
	SET LAB.stress_echo2;

	diff_EF = dobEF - baseEF;
RUN;

PROC UNIVARIATE DATA=LAB.stress_echo2;
	VAR diff_EF;
RUN;

/* What does the Sign Rank test tell you? */

******************************************************************************;

/* Research Question: Is there a change in diagnosis between baseline and end diagnosis for MI? */

/* McNemar Test */

PROC FREQ DATA = LAB.stress_echo2;
  TABLES hxofMI*newMI /agree;
RUN;

******************************************************************************;
/* Research Question:  Is there an association between Baseline BP and Baseline EF? */

/* Check for Normality */

PROC UNIVARIATE DATA=LAB.stress_echo2 NORMAL PLOT CIPCTLDF;
	VAR basebp baseEF;
	HISTOGRAM basebp / NORMAL;
	QQPLOT / NORMAL (MU=est SIGMA=est);
RUN;

/* What is the approriate correlation to use? */

PROC CORR DATA=LAB.stress_echo2 PEARSON SPEARMAN KENDALL;
	VAR basebp;
	WITH baseEF;
RUN;
***********************************************************************************;

/* Research Question: What explains a patient's baseline systolic blood pressure? */

/* Check Spearman Correlations */

PROC CORR DATA=LAB.stress_echo2 SPEARMAN;
	VAR basebp bhr basedp pctMphr age gender_num baseEF	chestpain restwma posSE hxofHT hxofDM hxofCig_num hxofMI hxofPTCA hxofCABG;
RUN;

/* Check for Normality */

PROC UNIVARIATE DATA=LAB.stress_echo2 NORMAL PLOT;
	VAR basebp bhr pctMphr age baseEF;
RUN;

/* Are there issues with normality? If so, how would you treat it? */

/* Using Stepwise Multiple Linear Regression */

PROC REG DATA=LAB.stress_echo2;
	MODEL basebp = bhr pctMphr age gender_num baseEF chestpain restwma posSE hxofHT hxofDM hxofCig_num hxofMI hxofPTCA hxofCABG  / 
    SELECTION = STEPWISE;
RUN;

/* Using Manual Multiple Linear Regression */

PROC REG DATA=LAB.stress_echo2;
	MODEL basebp = bhr pctMphr age gender_num baseEF chestpain restwma posSE hxofHT hxofDM hxofCig_num hxofMI hxofPTCA hxofCABG;
RUN;

/* What are issues with the model? */
**********************************************************;
/* Research Question: What explains whether a patient will have one of the composite endpts:  death, MI, PCTA, CABG? */


/* Check Tables for endpoint frequencies */

PROC FREQ DATA=LAB.stress_echo2;
	TABLES any_event;
RUN;

/* Check Spearman Correlations */

PROC CORR DATA=LAB.stress_echo2 SPEARMAN;
	VAR any_event bhr basebp basedp pctMphr age	gender_num	baseEF	chestpain restwma posSE hxofHT hxofDM hxofCig_num hxofMI hxofPTCA hxofCABG;
RUN;

/* Are there variables that are highly correlated to each other?  If so, what are they? */

/* Using Automatic Method */

PROC LOGISTIC DATA=LAB.stress_echo2 DESCENDING;
	MODEL any_event = bhr basebp pctMphr age gender_num	baseEF	chestpain restwma posSE hxofHT hxofDM hxofCig_num hxofMI hxofPTCA hxofCABG / 
	SELECTION = FORWARD
	CTABLE PPROB=(0 to 1 by .1)
	LACKFIT
	RISKLIMITS;
RUN;

/* Using Manual Method */
PROC LOGISTIC DATA=LAB.stress_echo2 DESCENDING;
	MODEL any_event = bhr basebp pctMphr age gender_num	baseEF	chestpain restwma posSE hxofHT hxofDM hxofCig_num hxofMI hxofPTCA hxofCABG / 
	CTABLE PPROB=(0 to 1 by .1)
	LACKFIT
	RISKLIMITS;
RUN;
