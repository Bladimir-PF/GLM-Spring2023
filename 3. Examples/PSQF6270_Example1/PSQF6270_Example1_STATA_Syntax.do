// Prevent "more" messages from appearing
   set more off
// Control line length
   set linesize 150

///////////////////////////////////////////////////////////////////////////////////////
////////               BEGIN MANIPULATION FOR HOFFMAN CHAPTER 2 DATA            ///////
////////                  CHANGE "filesave" TO YOUR FOLDER PATH                 ///////
///////////////////////////////////////////////////////////////////////////////////////

// Define global variable for file location to be replaced in code below
// \\Client\ precedes path in Virtual Desktop outside H drive
   global filesave "C:\Dropbox\23_PSQF6270\PSQF6270_Example1"

// Open chapter 2 STATA dataset and clear away any existing data
   use "$filesave\STATA_Chapter2.dta", clear // Has converted all variables to lower-case
   
// Center quantitative predictors
   gen age85 = age-85
   gen grip9 = grip-9

// Create dummy-coded binary indicator predictors for dementia groups
   gen demnf=.   // Create two new empty variables
   gen demnc=.
// Recode if demgroup = none   
   replace demnf=0 if demgroup==1
   replace demnc=0 if demgroup==1
// Recode if demgroup = future 
   replace demnf=1 if demgroup==2
   replace demnc=0 if demgroup==2
// Recode if demgroup = current
   replace demnf=0 if demgroup==3
   replace demnc=1 if demgroup==3
   
// Label all variables
   label variable age85     "age85: Age in Years (0=85)"
   label variable grip9     "grip9: Grip Strength in Pounds (0=9)"
   label variable sexmw     "sexmw: Sex (0=Men, 1=Women)"
   label variable demnf     "demnf: Dementia Predictor for None=0 vs Future=1"
   label variable demnc     "demnc: Dementia Predictor for None=0 vs Current=1"
   label variable cognition "cognition: Cognition Outcome"
   label variable demgroup  "demgroup: Dementia Group 1N 2F 3C"

// Filter to only cases complete on all variables to be used below
   egen nmiss=rowmiss(cognition age grip sexmw demgroup)
   drop if nmiss>0

// Save results to separate file
   log using $filesave\PSQF6270_Example1_STATA_Output.log, replace
   
   
///////////////////////////////////////////////////////////////////////////////////////
////////                     BEGIN DESCRIPTIVE STATISTICS                        //////
///////////////////////////////////////////////////////////////////////////////////////
   
display "STATA Descriptive Statistics"
format cognition age grip sexmw demnf demnc %4.3f      // to control precision
summarize cognition age grip sexmw demnf demnc, format // for quantitative variables
summarize cognition age grip sexmw demnf demnc, detail // detail to get variance
tabulate sexmw demgroup                                // for categorical variables


///////////////////////////////////////////////////////////////////////////////////////
////////                   BEGIN GLM WITH MAIN EFFECTS ONLY                      //////
///////////////////////////////////////////////////////////////////////////////////////

display "STATA Eq 2.8: Main-Effects-Only GLM Predicting Cognition"
regress cognition c.age85 c.grip9 c.sexmw c.demnf c.demnc, level(95)
// LINCOM creates a single linear combination of fixed effects
   lincom c.demnf*-1 + c.demnc*1 // Mean Diff: Future vs. Current = B5-B4
// TEST lumps together fixed effects for joint tests -- indicate DFnum by ()  
   test (c.age85=0)(c.grip9=0)(c.sexmw=0)(c.demnf=0)(c.demnc=0) // DFnum=5 F-test for Model R2   
   test (c.demnf=0)(c.demnc=0)   // DFnum=2 Omnibus F-test for Demgroup   
// Pred cognition outcomes holding sexmw=men, demnf=none, and demnc=none
// predictor=(from(by)to), c.=quantitative predictor, vsquish compresses output empty lines
   margins, at(c.age85=(-5(5)5) c.grip9=(-3(3)3) c.sexmw=0 c.demnf=0 c.demnc=0) vsquish
// Get and save plot of predicted outcomes
   marginsplot, xdimension(grip9) name(predicted_means, replace) 
   graph export "$filesave\STATA plots\STATA Main-Effect-Only GLM Plot.png", replace
   
   
///////////////////////////////////////////////////////////////////////////////////////
////////                   BEGIN GLM WITH AGE*GRIP INTERACTION                   //////
///////////////////////////////////////////////////////////////////////////////////////

display "STATA Eq 2.9: GLM with Age by Grip Interaction"
regress cognition c.age85 c.grip9 c.sexmw c.demnf c.demnc c.age85#c.grip9, level(95)
  lincom c.demnf*-1 + c.demnc*1 // Mean Diff: Future vs. Current = Beta5-Beta4
  test (c.demnf=0)(c.demnc=0)   // DFnum=2 Omnibus F-test for main effect of demgroup
  test (c.age85=0)(c.grip9=0)(c.age85#c.grip9=0) // DFnum=3 F-test for age, grip, age*grip
// Simple slopes for age per grip, grip per age
   lincom c.age85*1 + c.age85#c.grip9*-3  // Age Slope at Grip =  6
   lincom c.age85*1 + c.age85#c.grip9*0   // Age Slope at Grip =  9
   lincom c.age85*1 + c.age85#c.grip9*3   // Age Slope at Grip = 12
   lincom c.grip9*1 + c.age85#c.grip9*-5  // Grip Slope at Age = 80
   lincom c.grip9*1 + c.age85#c.grip9*0   // Grip Slope at Age = 85
   lincom c.grip9*1 + c.age85#c.grip9*5   // Grip Slope at Age = 90
// dydx in margins provides simple slopes for that variable by (from(by)to) moderator
   margins, at(c.grip9=(-3(3)3)) dydx(c.age85) vsquish // Age Slope per Grip
   margins, at(c.age85=(-5(5)5)) dydx(c.grip9) vsquish // Grip Slope per Age
// predictor=(from(by)to), c.=quantitative predictor, vsquish compresses output empty lines 
   margins, at(c.age85=(-5(5)5) c.grip9=(-3(3)3) c.sexmw=0 c.demnf=0 c.demnc=0) vsquish
   marginsplot, xdimension(age85)   // Get and save plot for pred outcomes by age 
   graph export "$filesave\STATA plots\STATA Grip by Age=x GLM Plot.png", replace
   marginsplot, xdimension(grip9)   // Get and save plot for pred outcomes by grip 
   graph export "$filesave\STATA plots\STATA Age by Grip=x GLM Plot.png", replace
   
display "STATA Eq 2.9: GLM with Age by Grip Interaction adding VCE for Regions"
regress cognition c.age85 c.grip9 c.sexmw c.demnf c.demnc c.age85#c.grip9, level(95)
  estat vce  // Asymptotic covariance matrix of fixed effects for regions

  
///////////////////////////////////////////////////////////////////////////////////////
////////                   BEGIN GLM WITH SEX*DEMGROUP INTERACTION               //////
///////////////////////////////////////////////////////////////////////////////////////   

display "STATA Eq 2.13: Adding Sex by Dementia Group Interaction"
display "Dummy-Coded Predictors for Sex (0=Men) and Demgroup (0=None)"
regress cognition c.age85 c.grip9 c.sexmw c.demnf c.demnc c.age85#c.grip9  ///
                  c.sexmw#c.demnf c.sexmw#c.demnc, level(95)
// Omnibus DF=2 F-Test for Dementia*Sex Interaction              
   test (c.sexmw#c.demnf=0) (c.sexmw#c.demnc=0) 
// In TESTs below, linear combinations are created within parentheses (still 1 DF each)
   // Omnibus DF=2 F-test for Dementia Simple Main Effect for Men
      test (c.demnf*1 + c.sexmw#c.demnf*0 =0)(c.demnc*1 + c.sexmw#c.demnc*0=0)                    
   // Omnibus DF=2 F-test for Dementia Simple Main Effect for Men
      test (c.demnf*1 + c.sexmw#c.demnf*1 =0)(c.demnc*1 + c.sexmw#c.demnc*1=0)  
// Predicted cognition outcomes --adjusted cell means-- holding age=85 and grip=9
   margins, at(c.age85=0 c.grip9=0 c.sexmw=(0(1)1) c.demnf=0 c.demnc=0) // yhats for None
   margins, at(c.age85=0 c.grip9=0 c.sexmw=(0(1)1) c.demnf=1 c.demnc=0) // yhats for Future
   margins, at(c.age85=0 c.grip9=0 c.sexmw=(0(1)1) c.demnf=0 c.demnc=1) // yhats for Current
// DF=1 simple slopes for sex per demgroup
   lincom c.sexmw*1 + c.sexmw#c.demnf*0 + c.sexmw#demnc*0  // Sex Diff for No Dementia
   lincom c.sexmw*1 + c.sexmw#c.demnf*1 + c.sexmw#demnc*0  // Sex Diff for Future Dementia
   lincom c.sexmw*1 + c.sexmw#c.demnf*0 + c.sexmw#demnc*1  // Sex Diff for Current Dementia
// DF=1 simple slopes for demgroup per sex
   lincom c.demnf*1  + c.demnc*0 + c.sexmw#c.demnf*0  + c.sexmw#c.demnc*0 // None-Future Diff for Men
   lincom c.demnf*1  + c.demnc*0 + c.sexmw#c.demnf*1  + c.sexmw#c.demnc*0 // None-Future Diff for Women
   lincom c.demnf*0  + c.demnc*1 + c.sexmw#c.demnf*0  + c.sexmw#c.demnc*0 // None-Current Diff for Men
   lincom c.demnf*0  + c.demnc*1 + c.sexmw#c.demnf*0  + c.sexmw#c.demnc*1 // None-Current Diff for Women 
   lincom c.demnf*-1 + c.demnc*1 + c.sexmw#c.demnf*0  + c.sexmw#c.demnc*0 // Future-Current Diff for Men
   lincom c.demnf*-1 + c.demnc*1 + c.sexmw#c.demnf*-1 + c.sexmw#c.demnc*1 // Future-Current Diff for Women
// DF=1 differences in simple slopes = interactions
   lincom c.sexmw#c.demnf*1  + c.sexmw#c.demnc*0  // A: Sex Effect differ btw None and Future?
   lincom c.sexmw#c.demnf*1  + c.sexmw#c.demnc*0  // A: None-Future Effect differ btw Men and Women?
   lincom c.sexmw#c.demnf*0  + c.sexmw#c.demnc*1  // B: Sex Effect differ btw None and Current?
   lincom c.sexmw#c.demnf*0  + c.sexmw#c.demnc*1  // B: None-Current Effect differ btw Men and Women?
   lincom c.sexmw#c.demnf*-1 + c.sexmw#c.demnc*1  // C: Sex Effect differ btw Future and Current?
   lincom c.sexmw#c.demnf*-1 + c.sexmw#c.demnc*1  // C: Future-Current Effect differ btw Men and Women?
   
// To make pictures, need to represent demgroup as program-categorical predictor instead 
display "STATA Eq 2.13: Adding Sex by Dementia Group Interaction"
display "Program-Categorical Predictor for Demgroup Instead"
regress cognition c.age85 c.grip9 c.sexmw i.demgroup c.age85#c.grip9 c.sexmw#i.demgroup, level(95)
// Get predicted cognition outcomes --adjusted cell means-- holding age=85 and grip=9
   margins i.demgroup, at(c.age85=0 c.grip9=0 c.sexmw=(0(1)1))   
   marginsplot, xdimension(demgroup)  // Get and save plot for pred outcomes by demgroup 
   graph export "$filesave\STATA plots\STATA Sex by Demgroup=x GLM Plot.png", replace
   marginsplot, xdimension(sexmw)  // Get and save plot for pred outcomes by sexmw 
   graph export "$filesave\STATA plots\STATA Demgroup by Sex=x GLM Plot.png", replace

// Close log
log close

