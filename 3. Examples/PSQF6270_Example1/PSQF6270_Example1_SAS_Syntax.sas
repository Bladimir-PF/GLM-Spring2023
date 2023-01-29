* Output options: all can be turned on/off by adding or removing the NO;
* page number, date, centering, or page breaks, page length and width;
  OPTIONS NOnumber NOdate NOcenter FormDlim=' ' PageSize=MAX LineSize=120;
* Eliminate SAS default titles and names of tables in output (TRACE ON to show);
  TITLE; ODS TRACE OFF; *ODS LISTING; ODS GRAPHICS;

***********************************************************************************;
*******             BEGIN MANIPULATION FOR HOFFMAN CHAPTER 2 DATA           *******;
*******                  CHANGE "filesave" TO YOUR FOLDER PATH              *******;
***********************************************************************************;

* Define global variable for file location to be replaced in code below;
* \\Client\ precedes path in Virtual Desktop outside H drive;
  %LET filesave= C:\Dropbox\23_PSQF6270\PSQF6270_Example1;
* Location for SAS files for these models (uses macro variable filesave);
  LIBNAME filesave "&filesave.";

* Import chapter 2 SAS data into work library as Example1;
DATA work.Example1; SET filesave.SAS_Chapter2;
* Center quantitative predictors;
  age85 = age-85;
  grip9 = grip-9;
* Create dummy-coded binary indicator predictors for dementia groups;
  demNF=.; demNC=.; * Create two new empty variables;
  IF demgroup=1 THEN DO; demNF=0; demNC=0; END; * Recode if demgroup=none;
  IF demgroup=2 THEN DO; demNF=1; demNC=0; END; * Recode if demgroup=future;
  IF demgroup=3 THEN DO; demNF=0; demNC=1; END; * Recode if demgroup=current;
* Label all variables -– note semi-colon is only at the end of ALL labels;
  LABEL   
  age85=     "age85: Age in Years (0=85)"
  grip9=     "grip9: Grip Strength in Pounds (0=9)"
  sexMW=     "sexMW: Sex (0=M, 1=W)"
  demNF=     "demNF: Dementia Predictor for None=0 vs Future=1"
  demNC=     "demNC: Dementia Predictor for None=0 vs Current=1"
  cognition= "cognition: Cognition Outcome"
  demgroup=  "demgroup: Dementia Group 1N 2F 3C";
* Filter to only cases complete on all variables to be used below;
  IF NMISS(cognition,age,grip,sexmw,demgroup)>0 THEN DELETE;
RUN;

* Open output directory to save results to;
ODS RTF FILE="&filesave.\PSQF6270_Example1_SAS_Output.rtf" 
        STYLE=HTMLBlue STARTPAGE=NO BODYTITLE;


***********************************************************************************;
*******                       BEGIN DESCRIPTIVE STATISTICS                  *******;
***********************************************************************************;

TITLE "SAS Descriptive Statistics";
PROC MEANS DATA=work.Example1 NOLABELS NONOBS NDEC=3 MEAN STDDEV VAR MIN MAX; 
     VAR cognition age grip sexMW;  * For quantitative variables;
RUN;
PROC FREQ DATA=work.Example1;  * For categorical variables;
     TABLE sexMW*demgroup / NOROW NOCOL; * Remove row and column totals;
RUN; TITLE;


***********************************************************************************;
*******                   BEGIN GLM WITH MAIN EFFECTS ONLY                  *******;
***********************************************************************************;

****** CREATING PREDICTED OUTCOMES USING SEPARATE STATEMENTS *******;

TITLE1 "SAS Eq 2.8: Main-Effects-Only GLM Predicting Cognition";
TITLE2 "Demonstrating how to get predicted outcomes using ESTIMATE statements";
PROC GLM DATA=work.Example1 NAMELEN=100;
     MODEL cognition = age85 grip9 sexMW demNF demNC / ALPHA=.05 CLPARM SOLUTION SS3;
     * ESTIMATE creates a single linear combination of fixed effects;
     ESTIMATE "Mean Diff: Future vs Current" demNF -1 demNC 1; * Beta5-Beta4;
     * CONTRAST lumps together fixed effects for joint tests -- indicate DFnum by commas;
     CONTRAST "DFnum=5 F-test for Model R2"  age85 1, grip9 1, sexMW 1, demNF 1, demNC 1;
     CONTRAST "DFnum=2 F-test for Demgroup"  demNF 1, demNC 1; * Omnibus group main effect;
     * Pred cognition outcomes holding sexMW=men, demNF=none, and demNC=none;
     ESTIMATE "Yhat for Age=80 Grip=6"  intercept 1 age85 -5 grip9 -3 sexMW 0 demNF 0 demNC 0;
     ESTIMATE "Yhat for Age=80 Grip=9"  intercept 1 age85 -5 grip9  0 sexMW 0 demNF 0 demNC 0;
     ESTIMATE "Yhat for Age=80 Grip=12" intercept 1 age85 -5 grip9  3 sexMW 0 demNF 0 demNC 0;
     ESTIMATE "Yhat for Age=85 Grip=6"  intercept 1 age85  0 grip9 -3 sexMW 0 demNF 0 demNC 0;
     ESTIMATE "Yhat for Age=85 Grip=9"  intercept 1 age85  0 grip9  0 sexMW 0 demNF 0 demNC 0;
     ESTIMATE "Yhat for Age=85 Grip=12" intercept 1 age85  0 grip9  3 sexMW 0 demNF 0 demNC 0;
     ESTIMATE "Yhat for Age=90 Grip=6"  intercept 1 age85  5 grip9 -3 sexMW 0 demNF 0 demNC 0;
     ESTIMATE "Yhat for Age=90 Grip=9"  intercept 1 age85  5 grip9  0 sexMW 0 demNF 0 demNC 0;
     ESTIMATE "Yhat for Age=90 Grip=12" intercept 1 age85  5 grip9  3 sexMW 0 demNF 0 demNC 0;
     ODS OUTPUT Estimates=work.EstMainEffects; * Save ESTIMATEs to dataset for plotting;
RUN; QUIT; TITLE1; TITLE2;

* Labeling saved ESTIMATES for use in plot;
* INDEX finds value in parentheses for that column;
DATA work.EstMainEffects; SET work.EstMainEffects;
     WHERE INDEX(Parameter,"Yhat")>0; * Only for predicted values;
     IF INDEX(Parameter,"Age=80")>0  THEN age=80;
     IF INDEX(Parameter,"Age=85")>0  THEN age=85;
     IF INDEX(Parameter,"Age=90")>0  THEN age=90;
     IF INDEX(Parameter,"Grip=6")>0  THEN grip=6;
     IF INDEX(Parameter,"Grip=9")>0  THEN grip=9;
     IF INDEX(Parameter,"Grip=12")>0 THEN grip=12;
RUN;

* Plot ESTIMATES -- grip as X by age;
PROC SGPLOT DATA=work.EstMainEffects;
     SERIES x=grip y=Estimate / GROUP=age; 
     XAXIS LABEL="Grip Strength" VALUES=(6 TO 12 BY 1);
     YAXIS LABEL="Predicted Cognition" VALUES=(15 TO 45 BY 5);
RUN; QUIT;


****** CREATING PREDICTED OUTCOMES USING FAKE PEOPLE INSTEAD *******;

DATA work.FakePeople; * Create new blank dataset;
* INPUT: list variables in order of entry, transformations happen to entered data;
  INPUT PersonID age grip sexMW demgroup; 
* Center quantitative predictors; 
  age85=age-85; grip9=grip-9;
* Create dummy-coded binary indicator predictors for dementia groups;
  demNF=.; demNC=.; * Create two new empty variables;
  IF demgroup=1 THEN DO; demNF=0; demNC=0; END; * Recode if demgroup=none;
  IF demgroup=2 THEN DO; demNF=1; demNC=0; END; * Recode if demgroup=future;
  IF demgroup=3 THEN DO; demNF=0; demNC=1; END; * Recode if demgroup=current;
* Enter data -- each row is a fake person for which to create a predicted outcome;
  DATALINES; 
-99 80  6  0  1
-99 80  9  0  1
-99 80 12  0  1
-99 85  6  0  1
-99 85  9  0  1
-99 85 12  0  1
-99 90  6  0  1
-99 90  9  0  1
-99 90 12  0  1 
; RUN;

* Add fake people to real data;
DATA work.Example1; SET work.FakePeople work.Example1; RUN;

TITLE1 "SAS Eq 2.8: Main-Effects-Only GLM Predicting Cognition";
TITLE2 "Using dataset with fake people to get predicted outcomes as saved variable";
PROC GLM DATA=work.Example1 NAMELEN=100;
     MODEL cognition = age85 grip9 sexMW demNF demNC / ALPHA=.05 CLPARM SOLUTION SS3;
     * We are ignoring the effects we would normally request for dementia for now;
     * Request columns of predicted outcome and SE for all cases for plotting;
       OUTPUT OUT=work.PredMain PREDICTED=Yhat STDP=SEyhat;
RUN; QUIT; TITLE1; TITLE2;

* Plot saved predicted values for fake people -- grip as X by age;
PROC SGPLOT DATA=work.PredMain;
     WHERE PersonID=-99; * Only for fake people;
     SERIES x=grip y=Yhat / GROUP=age;
     XAXIS LABEL="Grip Strength" VALUES=(6 TO 12 BY 1);
     YAXIS LABEL="Predicted Cognition" VALUES=(15 TO 45 BY 5);
RUN; QUIT;


***********************************************************************************;
*******                 BEGIN GLM WITH AGE*GRIP INTERACTION                 *******;
***********************************************************************************;

TITLE1 "SAS Eq 2.9: GLM Adding Age by Grip Strength Interaction";
TITLE2 "Using dataset with fake people to get predicted outcomes as saved variable";
* Estimate model on data with fake people to make predictions;
PROC GLM DATA=work.Example1 NAMELEN=100;
  MODEL cognition = age85 grip9 sexMW demNF demNC age85*grip9 / ALPHA=.05 CLPARM SOLUTION SS3;
  ESTIMATE "Mean Diff: Future vs Current" demNF -1 demNC 1; * Beta5-Beta4;
  CONTRAST "DFnum=2 F-test for Demgroup"  demNF 1, demNC 1; * Omnibus group main effect;
  CONTRAST "DFnum=3 F-test for age, grip, age*grip"  age85 1, grip9 1, age85*grip9 1;
  * Request columns of predicted outcome and SE for all cases;
  OUTPUT OUT=work.PredAgeGrip PREDICTED=Yhat STDP=SEyhat;
  * Simple slopes for age per grip, grip per age;
  ESTIMATE "Age Slope at Grip =  6"  age85 1 age85*grip9 -3;
  ESTIMATE "Age Slope at Grip =  9"  age85 1 age85*grip9  0;
  ESTIMATE "Age Slope at Grip = 12"  age85 1 age85*grip9  3;
  ESTIMATE "Grip Slope at Age = 80"  grip9 1 age85*grip9 -5;
  ESTIMATE "Grip Slope at Age = 85"  grip9 1 age85*grip9  0;
  ESTIMATE "Grip Slope at Age = 90"  grip9 1 age85*grip9  5; 
  * If you are NOT using fake people, you have to write these to create predicted outcomes;
  * Pred cognition outcomes holding sexMW=men, demNF=none, and demNC=none;
  ESTIMATE "Yhat for Age=80 Grip=6"  intercept 1 age85 -5 grip9 -3 age85*grip9  15;
  ESTIMATE "Yhat for Age=80 Grip=9"  intercept 1 age85 -5 grip9  0 age85*grip9   0;
  ESTIMATE "Yhat for Age=80 Grip=12" intercept 1 age85 -5 grip9  3 age85*grip9 -15;
  ESTIMATE "Yhat for Age=85 Grip=6"  intercept 1 age85  0 grip9 -3 age85*grip9   0;
  ESTIMATE "Yhat for Age=85 Grip=9"  intercept 1 age85  0 grip9  0 age85*grip9   0;
  ESTIMATE "Yhat for Age=85 Grip=12" intercept 1 age85  0 grip9  3 age85*grip9   0;
  ESTIMATE "Yhat for Age=90 Grip=6"  intercept 1 age85  5 grip9 -3 age85*grip9 -15;
  ESTIMATE "Yhat for Age=90 Grip=9"  intercept 1 age85  5 grip9  0 age85*grip9   0;
  ESTIMATE "Yhat for Age=90 Grip=12" intercept 1 age85  5 grip9  3 age85*grip9  15;
RUN; QUIT; TITLE1; TITLE2;

* Plot saved predicted values for fake people -- age as X;
PROC SGPLOT DATA=work.PredAgeGrip;
     WHERE PersonID=-99; * Only for fake people;
     SERIES x=age y=Yhat / GROUP=grip;
     XAXIS LABEL="Years of Age" VALUES=(80 TO 90 BY 1);
     YAXIS LABEL="Predicted Cognition" VALUES=(15 TO 45 BY 5);
RUN; QUIT;

* Plot saved predicted values for fake people -- grip as X;
PROC SGPLOT DATA=work.PredAgeGrip;
     WHERE PersonID=-99; * Only for fake people;
     SERIES x=grip y=Yhat / GROUP=age;
     XAXIS LABEL="Pounds of Grip Strength" VALUES=(6 TO 12 BY 1);
     YAXIS LABEL="Predicted Cognition" VALUES=(15 TO 45 BY 5);
RUN; QUIT;

TITLE1 "SAS Eq 2.9: GLM Adding Age by Grip Interaction in MIXED to Get COVB";
* Estimate model on data with fake people to make predictions;
PROC MIXED DATA=work.Example1 COVTEST NOCLPRINT NAMELEN=100 METHOD=REML;
     MODEL cognition = age85 grip9 sexMW demNF demNC age85*grip9
                        / SOLUTION DDFM=BW COVB;
  * Saving info for regions to datasets: fixed effects and COVB;
  ODS OUTPUT SolutionF=FixAgeGrip COVB=CovBAgeGrip; 
RUN; TITLE1; 

********************** CHANGE NOTHING IN THIS SECTION, JUST RUN IT **************;
* To use Regions Macro, enter;
* FixData =   Name of ODS SolutionF table that stores fixed effects for model;
* CovBData =  Name of ODS CovB table that stores XTX inv matrix for model;
* Pred =      Case-sensitive name of predictor effect regions are for; 
* Mod =       Case-sensitive name of moderator effect (for region values);
* ModCenter = Centering point of moderator predictor;
* Interact =  Case-sensitive name of interaction effect; 
* Order =     Order of entry of interaction in MODEL statement;
%MACRO Regions(FixData=,CovBData=,Pred=,Mod=,ModCenter=,Interact=,Order=);
DATA _NULL_; SET &FixData.; WHERE Effect="&Pred.";
     CALL SYMPUT('Bpred', Estimate); 
     CALL SYMPUT('SEpred', StdErr); RUN; 
DATA _NULL_; SET &FixData.; WHERE Effect="&Interact.";
     CALL SYMPUT('Binter', Estimate); 
     CALL SYMPUT('SEinter', StdErr); RUN; 
%LET order=%EVAL(&order.+1);
DATA _NULL_; SET &CovBData.; 
     WHERE INDEX(Effect,"&Pred.")>0 AND INDEX(Effect,"*")=0;
     CALL SYMPUT('CovPredInt', ROUND(Col&order.,.0001)); RUN;    
%PUT Bpred=&Bpred. SEpred=&SEpred. Binter=&Binter. 
     SEinter=&SEinter. CovPredInt=&CovPredInt.;
DATA Regions;
     A=(1.96*1.96)*(&SEinter.*&SEinter.)-(&Binter.*&Binter.);
     B=2*((1.96*1.96)*&CovPredInt.-(&Bpred.*&Binter.));
     C=(1.96*1.96*&SEpred.*&SEpred.)-(&Bpred.*&Bpred.);
     CenteredLower=((-1*B)+SQRT((B*B)-4*A*C))/(2*A); 
        CALL SYMPUT('cenlower',ROUND(CenteredLower,.001));
     CenteredUpper=((-1*B)-SQRT((B*B)-4*A*C))/(2*A); 
        CALL SYMPUT('cenupper',ROUND(CenteredUpper,.001));
     UncenteredLower=CenteredLower+&ModCenter.; 
        CALL SYMPUT('uncenlower',ROUND(UncenteredLower,.001));
     UncenteredUpper=CenteredUpper+&ModCenter.; 
        CALL SYMPUT('uncenupper',ROUND(UncenteredUpper,.001));
RUN;
TITLE7 "Regions of significance for &interact. interaction:";
TITLE8 "The &pred. slope will be significant at centered values of &mod. BELOW the lower bound"; 
TITLE9 "and ABOVE the upper bound, which translate to these uncentered lower and upper bounds."; 
PROC PRINT DATA=Regions NOOBS; VAR CenteredLower--UncenteredUpper; RUN; TITLE7; TITLE8; TITLE9;
%MEND Regions;
*********************************************************************************************

* Call macro for regions of significance for main effects of interaction;
%Regions(FixData=FixAgeGrip, CovBData=CovBAgeGrip, Pred=age85, Mod=grip9,
         ModCenter=9, Interact=age85*grip9, Order=6);
%Regions(FixData=FixAgeGrip, CovBData=CovBAgeGrip, Pred=grip9, Mod=age85,
         ModCenter=85, Interact=age85*grip9, Order=6);


***********************************************************************************;
*******                BEGIN GLM WITH SEX*DEMGROUP INTERACTION              *******;
***********************************************************************************;

* Demonstrating how to get predicted outcomes using "fake people";
* Each row is a fake person for which to create a predicted outcome;
DATA work.FakePeople2; 
* INPUT: list variables in order of entry, transformations happen to entered data;
  INPUT PersonID age grip sexMW demgroup; 
* Center quantitative predictors; 
  age85=age-85; grip9=grip-9;
* Create dummy-coded binary indicator predictors for dementia groups;
  demNF=.; demNC=.; * Create two new empty variables;
  IF demgroup=1 THEN DO; demNF=0; demNC=0; END; * Recode if demgroup=none;
  IF demgroup=2 THEN DO; demNF=1; demNC=0; END; * Recode if demgroup=future;
  IF demgroup=3 THEN DO; demNF=0; demNC=1; END; * Recode if demgroup=current;
* Enter data -- each row is a fake person for which to create a predicted outcome;
DATALINES; 
-98 85  9  0  1 
-98 85  9  0  2
-98 85  9  0  3
-98 85  9  1  1
-98 85  9  1  2
-98 85  9  1  3
; RUN;
* Merge with real data;
DATA work.Example1; SET work.FakePeople2 work.Example1; RUN;

TITLE1 "SAS Eq 2.13: GLM Adding Sex by Dementia Group Interaction";
TITLE2 "Dummy-Coded Predictors for Sex (0=Men) and Demgroup (0=None)";
PROC GLM DATA=work.Example1 NAMELEN=100;
MODEL cognition = age85 grip9 age85*grip9 sexMW demNF demNC
                  sexMW*demNF sexMW*demNC / ALPHA=.05 CLPARM SOLUTION SS3;
CONTRAST "Omnibus DF=2 F-test for Sex*Demgroup Interaction"  sexMW*demNF 1, sexMW*demNC 1;
* In CONTRASTs below, linear combinations are created within a comma set (still 1 DF each);
CONTRAST "Omnibus DF=2 F-test for Dementia Simple Main Effect for Men"   
          demNF 1 sexMW*demNF 0, demNC 1 sexMW*demNC 0;
CONTRAST "Omnibus DF=2 F-test for Dementia Simple Main Effect for Women" 
          demNF 1 sexMW*demNF 1, demNC 1 sexMW*demNC 1;
* Request columns of predicted outcome and SE for all cases for plotting;
OUTPUT OUT=work.PredSexDem PREDICTED=Yhat STDP=SEyhat;
* Predicted cognition outcomes --adjusted cell means-- holding age=85 and grip=9;
ESTIMATE "Yhat for Men   None"       intercept 1 sexMW 0 demNF  0 demNC 0 sexMW*demNF 0 sexMW*demNC 0;
ESTIMATE "Yhat for Women None"       intercept 1 sexMW 1 demNF  0 demNC 0 sexMW*demNF 0 sexMW*demNC 0;
ESTIMATE "Yhat for Men   Future"     intercept 1 sexMW 0 demNF  1 demNC 0 sexMW*demNF 0 sexMW*demNC 0;
ESTIMATE "Yhat for Women Future"     intercept 1 sexMW 1 demNF  1 demNC 0 sexMW*demNF 1 sexMW*demNC 0;
ESTIMATE "Yhat for Men   Current"    intercept 1 sexMW 0 demNF  0 demNC 1 sexMW*demNF 0 sexMW*demNC 0;
ESTIMATE "Yhat for Women Current"    intercept 1 sexMW 1 demNF  0 demNC 1 sexMW*demNF 0 sexMW*demNC 1;
* DF=1 simple slopes for sex per demgroup;
ESTIMATE "Sex Diff for No Dementia"              sexMW 1 demNF  0 demNC 0 sexMW*demNF  0 sexMW*demNC 0;
ESTIMATE "Sex Diff for Future Dementia"          sexMW 1 demNF  0 demNC 0 sexMW*demNF  1 sexMW*demNC 0;
ESTIMATE "Sex Diff for Current Dementia"         sexMW 1 demNF  0 demNC 0 sexMW*demNF  0 sexMW*demNC 1;
* DF=1 simple slopes for demgroup per sex;
ESTIMATE "None-Future Diff for Men"              sexMW 0 demNF  1 demNC 0 sexMW*demNF  0 sexMW*demNC 0;
ESTIMATE "None-Future Diff for Women"            sexMW 0 demNF  1 demNC 0 sexMW*demNF  1 sexMW*demNC 0;
ESTIMATE "None-Current Diff for Men"             sexMW 0 demNF  0 demNC 1 sexMW*demNF  0 sexMW*demNC 0;
ESTIMATE "None-Current Diff for Women"           sexMW 0 demNF  0 demNC 1 sexMW*demNF  0 sexMW*demNC 1;
ESTIMATE "Future-Current Diff for Men"           sexMW 0 demNF -1 demNC 1 sexMW*demNF  0 sexMW*demNC 0;
ESTIMATE "Future-Current Diff for Women"         sexMW 0 demNF -1 demNC 1 sexMW*demNF -1 sexMW*demNC 1;
* DF=1 differences in simple slopes = interactions;
ESTIMATE "A: Sex Effect differ btw None and Future?"                      sexMW*demNF  1 sexMW*demNC 0;
ESTIMATE "A: None-Future Effect differ btw Men and Women?"                sexMW*demNF  1 sexMW*demNC 0;
ESTIMATE "B: Sex Effect differ btw None and Current?"                     sexMW*demNF  0 sexMW*demNC 1;
ESTIMATE "B: None-Current Effect differ btw Men and Women?"               sexMW*demNF  0 sexMW*demNC 1;
ESTIMATE "C: Sex Effect differ btw Future and Current?"                   sexMW*demNF -1 sexMW*demNC 1;
ESTIMATE "C: Future-Current Effect differ btw Men and Women?"             sexMW*demNF -1 sexMW*demNC 1;
RUN; QUIT; TITLE1; TITLE2;

* Plot saved predicted values for fake people -- dementia as X;
PROC SGPLOT DATA=work.PredSexDem;
     WHERE PersonID=-98; * Only for new fake people;
     SERIES x=demgroup y=Yhat / GROUP=sexMW;
     XAXIS LABEL="Dementia Group" VALUES=(1 TO 3 BY 1);
     YAXIS LABEL="Predicted Cognition" VALUES=(0 TO 35 BY 5);
RUN; QUIT;

* Plot saved predicted values for fake people -- sex as X;
PROC SGPLOT DATA=work.PredSexDem;
     WHERE PersonID=-98; * Only for new fake people;
     SERIES x=sexMW y=Yhat / GROUP=demgroup;
     XAXIS LABEL="Sex" VALUES=(0 TO 1 BY 1);
     YAXIS LABEL="Predicted Cognition" VALUES=(0 TO 35 BY 5);
RUN; QUIT;


* Close output;
ODS RTF CLOSE;
