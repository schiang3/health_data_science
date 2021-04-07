
/* Set Working directory */
LIBNAME HK '\\apporto.com\dfs\depaul\Users\schiang4_depaul\Documents\HK';


/* View what is available in the Library */
PROC CONTENTS DATA=HK._ALL_ NODS;
RUN;

/* Read in Dataset  DBMS MEANS WHAT DATA FILE WE READ*/
PROC IMPORT DATAFILE="\\apporto.com\dfs\depaul\Users\schiang4_depaul\Documents\HK\frmgham2.csv" 
    OUT=HK.frmgham2
    DBMS=csv 
    REPLACE;
    GETNAMES=YES;
RUN;

/* Check that File was Read in Correctly */
PROC PRINT DATA=HK.frmgham2; RUN;

PROC CONTENTS DATA=HK.frmgham2 ORDER=varnum; RUN;


PROC FREQ DATA=HK.frmgham2;
	TABLES SEX DIABETES PREVCHD PREVSTRK;
RUN;


/* Check for Normality of TOTCHOL and Plot TOTCHOL, as well as Percentiles */
PROC UNIVARIATE DATA=HK.frmgham2 NORMAL PLOT;
	VAR TOTCHOL;
RUN;

PROC UNIVARIATE DATA=HK.frmgham2 NORMAL PLOT;
	VAR GLUCOSE;
RUN;

DATA HK.frmgham2;
	SET HK.frmgham2;
	/* Gender */
	IF SEX="2" THEN gender_num=0;
    IF SEX="1" THEN gender_num=1;
RUN;

PROC CONTENTS DATA=HK.frmgham2 ORDER=varnum; RUN;

PROC FREQ DATA=HK.frmgham2;
	TABLES SEX DIABETES PREVCHD PREVSTRK gender_num;
RUN;

PROC CONTENTS DATA=HK.frmgham2 ORDER=varnum; RUN;

/* Check for Normality of Continuous Variables, as well as Percentiles */
PROC UNIVARIATE DATA=HK.frmgham2 NORMAL PLOT;
	VAR TOTCHOL GLUCOSE SYSBP;
RUN;

/* Check Descriptive Statistics of Continuous Variables */
PROC MEANS DATA=HK.frmgham2 N NMISS MEAN STD MIN Q1 MEDIAN Q3 MAX SKEWNESS KURTOSIS MAXDEC=2;
	VAR TOTCHOL GLUCOSE SYSBP;
RUN;

*********************************************************************************************;
/* Research Question: Is there a difference in cholesterol levels between male and female patients?*/
PROC SORT DATA=HK.frmgham2;
	BY SEX;
RUN;

PROC UNIVARIATE DATA=HK.frmgham2 NORMAL PLOT CIPCTLDF;
	BY SEX;
	VAR TOTCHOL;
	HISTOGRAM TOTCHOL / NORMAL;
	QQPLOT / NORMAL (MU=est SIGMA=est);
RUN;

/* Check Normality Visually Looking at Boxplots */
PROC SGPLOT DATA=HK.frmgham2;
	TITLE "Boxplots of TOTCHOL  by SEX";
	VBOX TOTCHOL / Category=SEX;
RUN;

/* Independent (Student) T-test */
PROC TTEST DATA=HK.frmgham2;
	CLASS SEX;
	VAR TOTCHOL;
RUN;

/* Mann-Whitney U (Wilcoxon) test - Nonparametric T-Test */
PROC NPAR1WAY DATA=HK.frmgham2 WILCOXON;
	CLASS SEX;
	VAR TOTCHOL;
RUN;

/* Kruskal-Wallis test - Nonparametric ANOVA */
PROC NPAR1WAY DATA=HK.frmgham2 WILCOXON DSCF;
	CLASS SEX;
	VAR TOTCHOL;
RUN;
*********************************************************************************************;
/* Research Question: Is there a relationship between Glucose levels and Diabetes? */
PROC SORT DATA=HK.frmgham2;
	BY DIABETES;
RUN;

PROC UNIVARIATE DATA=HK.frmgham2 NORMAL PLOT CIPCTLDF;
	BY DIABETES;
	VAR GLUCOSE;
	HISTOGRAM GLUCOSE / NORMAL;
	QQPLOT / NORMAL (MU=est SIGMA=est);
RUN;

/* Check Normality Visually Looking at Boxplots */
PROC SGPLOT DATA=HK.frmgham2;
	TITLE "Boxplots of GLUCOSE  by DIABETES";
	VBOX GLUCOSE / Category=DIABETES;
RUN;

/* Independent (Student) T-test */
PROC TTEST DATA=HK.frmgham2;
	CLASS DIABETES;
	VAR GLUCOSE;
RUN;

/* Mann-Whitney U (Wilcoxon) test - Nonparametric T-Test */
PROC NPAR1WAY DATA=HK.frmgham2 WILCOXON;
	CLASS DIABETES;
	VAR GLUCOSE;
RUN;

/* One-Way ANOVA */
PROC ANOVA DATA=HK.frmgham2;
	CLASS DIABETES;
	MODEL GLUCOSE = DIABETES;
	MEANS DIABETES / SNK;
RUN;

/* Kruskal-Wallis test - Nonparametric ANOVA */
PROC NPAR1WAY DATA=HK.frmgham2 WILCOXON DSCF;
	CLASS DIABETES;
	VAR GLUCOSE;
RUN;

PROC CORR DATA=HK.frmgham2 PEARSON;
	VAR DIABETES GLUCOSE;
	RUN;
*************************************************************************************************;
/* ROC Curves -  Determining Optimal Cutoff Point */
ODS GRAPHIC ON;
PROC LOGISTIC DATA =HK.frmgham2;
MODEL PREVCHD (EVENT='1')=TIMEAP TIMECHD PREVMI PREVAP  ANYCHD SEX  AGE SYSBP BPMEDS TOTCHOL STROKE  CURSMOKE GLUCOSE BMI HEARTRTE /OUTROC=ROCDATA0;
 ROC; ROCCONTRAST;
RUN;
ODS GRAPHIC OFF;
ODS GRAPHIC ON;
PROC LOGISTIC DATA =HK.frmgham2;
 MODEL STROKE  (EVENT='1')=SYSBP  /OUTROC=ROCDATA;
 ROC; ROCCONTRAST;
RUN;
ODS GRAPHIC OFF;
DATA ROCDATA;
	SET ROCDATA;
    /* cutoff = (logit+Intercept)/slope; */
	/* Choose cutoff with maximum Youden */
	logit=log(_prob_/(1-_prob_));
	cutoff=(logit+6.0456)/0.0263;
	prob= _prob_;
	Sensitivity = _SENSIT_;
	Specificity = 1-_1MSPEC_;
	Youden= _SENSIT_+ (1-_1MSPEC_)-1;
RUN;
PROC SORT DATA=ROCDATA ; 
	BY Youden DESCENDING;
RUN;
PROC PRINT DATA=ROCDATA; RUN;

ODS GRAPHIC ON;
PROC LOGISTIC DATA =HK.frmgham2;
 MODEL PREVCHD  (EVENT='1')=SYSBP  /OUTROC=ROCDATA2;
 ROC; ROCCONTRAST;
RUN;
ODS GRAPHIC OFF;
DATA ROCDATA2;
	SET ROCDATA2;
    /* cutoff = (logit+Intercept)/slope; */
	/* Choose cutoff with maximum Youden */
	logit=log(_prob_/(1-_prob_));
	cutoff=(logit+5.2789)/0.0194;
	prob= _prob_;
	Sensitivity = _SENSIT_;
	Specificity = 1-_1MSPEC_;
	Youden= _SENSIT_+ (1-_1MSPEC_)-1;
RUN;

PROC SORT DATA=ROCDATA2 ; 
	BY Youden DESCENDING;
RUN;

PROC PRINT DATA=ROCDATA2; RUN;


ODS GRAPHIC ON;
PROC LOGISTIC DATA =HK.frmgham2;
 MODEL PREVSTRK  (EVENT='1')=SYSBP  /OUTROC=ROCDATA3;
 ROC; ROCCONTRAST;
RUN;
ODS GRAPHIC OFF;
DATA ROCDATA3;
	SET ROCDATA3;
	logit=log(_prob_/(1-_prob_));
	cutoff=(logit+8.4480)/0.0284;
	prob= _prob_;
	Sensitivity = _SENSIT_;
	Specificity = 1-_1MSPEC_;
	Youden= _SENSIT_+ (1-_1MSPEC_)-1;
RUN;

PROC SORT DATA=ROCDATA3 ; 
	BY Youden DESCENDING;
RUN;

PROC PRINT DATA=ROCDATA3; RUN;

DATA HK.frmgham2;
	SET HK.frmgham2;
	/* SYSBP */
	IF SYSBP<=140.143 THEN sysbp_group=0;
    IF SYSBP>140.143 THEN sysbp_group=1;
RUN;

ODS GRAPHIC ON;
PROC LIFETEST DATA=HK.frmgham2  ;
  TIME TIME*STROKE(0);
RUN;

PROC LIFETEST DATA=HK.frmgham2 ;
  TIME TIME*STROKE(0);
 STRATA sysbp_group;
RUN;

