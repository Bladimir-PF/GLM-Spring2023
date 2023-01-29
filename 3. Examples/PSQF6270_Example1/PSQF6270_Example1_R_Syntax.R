########################### PSQF 6270 Example 1 using R #####################################

# Set width of output and number of significant digits printed,
# number of digits before using scientific notation, shut off significance stars
options(width=120, digits=8, scipen=9, show.signif.stars=FALSE)

#####  Check to see if packages are downloaded, install if not, then load  #####

if (!require("readxl")) install.packages("readxl")
library(readxl) # To import xls or xlsx data as table

if (!require("expss")) install.packages("expss")
library(expss) # To add variable and value labels, sorting

if (!require("TeachingDemos")) install.packages("TeachingDemos")
library(TeachingDemos) # To create text output files

if (!require("psych")) install.packages("psych")
library(psych) # To add descriptive summary functions

if (!require("multcomp")) install.packages("multcomp")
library(multcomp) # To do glht linear combinations in GLMs

if (!require("prediction")) install.packages("prediction")
library(prediction) # To get predicted values like Stata does

if (!require("reghelper")) install.packages("reghelper")
library(reghelper) # To do efficient simple slopes in GLMs

if (!require("interactions")) install.packages("interactions")
library(interactions) # To do regions of significance in GLMs

#############################################################################################
#####              BEGIN DATA MANIPULATION OF HOFFMAN CHAPTER 2 DATA                    #####
#####                   CHANGE "filesave" TO YOUR FOLDER PATH                           #####
#############################################################################################

# Define variables for working directory and data name
filesave = "C:\\Dropbox/23_PSQF6270/PSQF6270_Example1/"
filename = "Excel_Chapter2.xlsx"
setwd(dir=filesave)

# Import chapter 2 Excel data as Example1
Example1 = read_excel(path=paste0(filesave,filename))
# Convert to data frame to use for analysis
Example1 = as.data.frame(Example1)
# Sort data by PersonID
Example1 = sort_asc(data=Example1,PersonID)

# Center quantitative predictors
Example1$age85 = Example1$age-85
Example1$grip9 = Example1$grip-9

# Create dummy-coded binary indicator predictors for dementia groups
Example1$demNF = NA; Example1$demNC = NA  # Create two new empty variables
# Recode if demgroup=none
Example1$demNF[which(Example1$demgroup==1)]=0 
Example1$demNC[which(Example1$demgroup==1)]=0
# Recode if demgroup=future
Example1$demNF[which(Example1$demgroup==2)]=1 
Example1$demNC[which(Example1$demgroup==2)]=0
# Recode if demgroup=current
Example1$demNF[which(Example1$demgroup==3)]=0 
Example1$demNC[which(Example1$demgroup==3)]=1

# Label all variables as comments only (not actually added to data)
#age85=     "age85: Age in Years (0=85)"
#grip9=     "grip9: Grip Strength in Pounds (0=9)"
#sexMW=     "sexMW: Sex (0=M, 1=W)"
#demNF=     "demNF: Dementia Predictor for None=0 vs Future=1"
#demNC=     "demNC: Dementia Predictor for None=0 vs Current=1"
#cognition= "cognition: Cognition Outcome"
#demgroup=  "demgroup: Dementia Group 1N 2F 3C"

# Filter to only cases complete on all variables to be used below
Example1 = Example1[complete.cases(Example1[ , 1:6]),]

# Open external text file to save results to (turn off to view in console)
txtStart(file=paste0(filesave,"PSQF6270_Example1_R_Output.txt"))


#############################################################################################
#####                       BEGIN DESCRIPTIVE STATISTICS                                #####
#############################################################################################

print("R Descriptive Statistics")
describe(x=Example1[ , c("cognition","age","grip","sexMW")]) # for quantitative variables
table(x=Example1$sexMW,Example1$demgroup,useNA="ifany")      # for categorical variables

txtComment(" ") # insert blank space in output  


#############################################################################################
#####                       BEGIN GLM WITH MAIN EFFECTS ONLY                            #####
#############################################################################################

## Btw, to get confidence intervals: confint(Model, level=.95) 

print("R Eq 2.8: Main-Effects-Only GLM Predicting Cognition")
ModelMain = lm(data=Example1, formula=cognition~1+age85+grip9+sexMW+demNF+demNC)
summary(ModelMain); anova(ModelMain) # anova to get residual variance

print("Get missing demgroup difference: Future vs Current = Beta5-Beta4")
summary(glht(model=ModelMain, linfct=rbind(c(0,0,0,0,-1,1))),test=adjusted("none"))

print("Get DFnum=5 F-test of Model R2 only for demonstration purposes")
MainFR2 = glht(model=ModelMain, linfct=c("age85=0","grip9=0","sexMW=0","demNF=0","demNC=0"))
summary(MainFR2, test=Ftest()) # ask for joint hypothesis test instead of separate

print("Get DFnum=2 F-test for demgroup") # Omnibus group main effect
mainFdem = glht(model=ModelMain, linfct=c("demNF=0","demNC=0"))
summary(mainFdem, test=Ftest()) # ask for joint hypothesis test instead of separate

print("How to get predicted outcomes using prediction package")
print("Pred cognition outcomes holding sexMW=men, demNF=none, and demNC=none")
print("Provides predicted outcomes from min,max,by=increment of predictors")
PredMain = summary(prediction(model=ModelMain, type="response",
           at=list(sexMW=0, demNF=0, demNC=0, age85=seq(-5,5,by=5), grip9=seq(-3,3,by=3))))
PredMain # print predicted outcomes
# Threw a warning that predictions went out of bounds!

# Create data frame with uncentered version of predictors for plotting
PredMain = data.frame(PredMain) # first remove () from variable names
PredMain = data.frame(PredMain, grip=PredMain$at.grip9.+9, age=PredMain$at.age85.+85) 
PredMain = sort_asc(data=PredMain,age) # sort data by age for plot

# Save plot: open file, make plot, close file
png(file = "R Main-Effects-Only GLM Plot.png")  # open file
plot(y=PredMain$Prediction, x=PredMain$grip, type="n", ylim=c(15,45), xlim=c(6,12),
     xlab="Grip Strength",ylab="Predicted Cognition")
lines(x=PredMain$grip[1:3], y=PredMain$Prediction[1:3], type="l", col="blue1")
lines(x=PredMain$grip[4:6], y=PredMain$Prediction[4:6], type="l", col="red1")
lines(x=PredMain$grip[7:9], y=PredMain$Prediction[7:9], type="l", col="green1")
legend(x=6, y=45, legend=c("Age 80","Age 85","Age 90"), col=1:3, lty=1) #lty=linetype
dev.off()  # close file

###################################################################################
print("How to get predicted outcomes and CIs using glht statements instead")
PredMain = glht(model=ModelMain, linfct=rbind(
  "Yhat for Age=80 Grip=6"  = c(1,-5,-3, 0,0,0),  # in order of fixed effects
  "Yhat for Age=80 Grip=9"  = c(1,-5, 0, 0,0,0),
  "Yhat for Age=80 Grip=12" = c(1,-5, 3, 0,0,0),
  "Yhat for Age=85 Grip=6"  = c(1, 0,-3, 0,0,0),
  "Yhat for Age=85 Grip=9"  = c(1, 0, 0, 0,0,0),
  "Yhat for Age=85 Grip=12" = c(1, 0, 3, 0,0,0),
  "Yhat for Age=90 Grip=6"  = c(1, 5,-3, 0,0,0),
  "Yhat for Age=90 Grip=9"  = c(1, 5, 0, 0,0,0),
  "Yhat for Age=90 Grip=12" = c(1, 5, 3, 0,0,0)))
summary(PredMain,test=adjusted("none"))
confint(PredMain, level=.95, calpha = univariate_calpha()) # unadjusted CIs
###################################################################################

txtComment(" ") # insert blank space in output  


#############################################################################################
#####                     BEGIN GLM WITH AGE*GRIP INTERACTION                           #####
#############################################################################################

print("R Eq 2.9: GLM Adding Age by Grip Strength Interaction")
ModelAgeGrip = lm(data=Example1, formula=cognition~1+age85+grip9+sexMW+demNF+demNC +age85:grip9)
summary(ModelAgeGrip); anova(ModelAgeGrip) # anova to get residual variance

print("Get missing demgroup difference: Future vs Current = Beta5-Beta4")
summary(glht(model=ModelAgeGrip, linfct=rbind(c(0,0,0,0,-1,1,0))),test=adjusted("none"))

print("Get DFnum=2 F-test for demgroup") # Omnibus group main effect
AgeGripFdem = glht(model=ModelAgeGrip,linfct=c("demNF=0","demNC=0"))
summary(AgeGripFdem, test=Ftest()) # ask for joint hypothesis test instead of separate

print("Get DFnum=3 F-test for age, grip, and age*grip")
AgeGripF = glht(model=ModelAgeGrip, linfct=c("age85=0","grip9=0","age85:grip9=0"))
summary(AgeGripF, test=Ftest()) # ask for joint hypothesis test instead of separate

print("Simple slopes for age per grip, grip per age")
summary(glht(model=ModelAgeGrip, linfct=rbind(
            "Age Slope at Grip = 6"  = c(0,1,0,0,0,0,-3),  # in order of fixed effects
            "Age Slope at Grip = 9"  = c(0,1,0,0,0,0, 0),
            "Age Slope at Grip = 12" = c(0,1,0,0,0,0, 3),
            "Grip Slope at Age = 80" = c(0,0,1,0,0,0,-5),
            "Grip Slope at Age = 85" = c(0,0,1,0,0,0, 0),
            "Grip Slope at Age = 90" = c(0,0,1,0,0,0, 5))),test=adjusted("none"))
print("Simple slopes over range of moderator values using reghelper package")
simple_slopes(model=ModelAgeGrip, levels=list(age85=c(-5,0,5,'sstest'),grip9=c(-3,0,3,'sstest')))

print("Regions of significance using interactions package") # plots broke my computer!
johnson_neyman(model=ModelAgeGrip, pred="age85", modx="grip9", digits=3, plot=FALSE) 
johnson_neyman(model=ModelAgeGrip, pred="grip9", modx="age85", digits=3, plot=FALSE)

print("Pred cognition outcomes holding sexMW=men, demNF=none, and demNC=none")
print("Provides predicted outcomes from min,max,by=increment of predictors")
PredAgeGrip = summary(prediction(model=ModelAgeGrip, type="response",
              at=list(sexMW=0, demNF=0, demNC=0, age85=seq(-5,5,by=5), grip9=seq(-3,3,by=3))))
PredAgeGrip # print predicted outcomes, get warning that predictions go out of bounds

# Create data frame with uncentered version of predictors for plotting
PredAgeGrip = data.frame(PredAgeGrip) # first remove () from variable names
PredAgeGrip = data.frame(PredAgeGrip, grip=PredAgeGrip$at.grip9.+9, age=PredAgeGrip$at.age85.+85) 

# Make and save plots
png(file = "R Grip by Age=x GLM Plot.png")  # open file
plot(y=PredAgeGrip$Prediction, x=PredAgeGrip$age, type="n", ylim=c(15,45), xlim=c(80,90),
     xlab="Years of Age", ylab="Predicted Cognition")
PredAgeGrip = sort_asc(data=PredAgeGrip,grip) # 3 rows per grip
lines(x=PredAgeGrip$age[1:3], y=PredAgeGrip$Prediction[1:3], type="l", col="blue1")
lines(x=PredAgeGrip$age[4:6], y=PredAgeGrip$Prediction[4:6], type="l", col="red1")
lines(x=PredAgeGrip$age[7:9], y=PredAgeGrip$Prediction[7:9], type="l", col="green1")
legend(x=80, y=45, legend = c("Grip=6", "Grip=9", "Grip=12"), col=1:3, lty=1) #lty=linetype
dev.off()  # close file

png(file = "R Age by Grip=x GLM Plot.png")  # open file
plot(y=PredAgeGrip$Prediction, x=PredAgeGrip$grip, type="n", ylim=c(15,45), xlim=c(6,12),
     xlab="Pounds of Grip Strength", ylab="Predicted Cognition")
PredAgeGrip = sort_asc(data=PredAgeGrip,age) # 3 rows per age now
lines(x=PredAgeGrip$grip[1:3], y=PredAgeGrip$Prediction[1:3], type="l", col="blue1")
lines(x=PredAgeGrip$grip[4:6], y=PredAgeGrip$Prediction[4:6], type="l", col="red1")
lines(x=PredAgeGrip$grip[7:9], y=PredAgeGrip$Prediction[7:9], type="l", col="green1")
legend(x=6, y=45, legend = c("Age=80", "Age=85", "Age=90"), col=1:3, lty=1) #lty=linetype
dev.off()  # close file

##################################################################################
# Here is another way of using GLHT create predicted outcomes
print("Pred cognition outcomes holding sexMW=men, demNF=none, and demNC=none")
summary(glht(model=ModelAgeGrip, linfct=rbind(
  "Yhat for Age=80 Grip=6"  = c(1,-5,-3, 0,0,0, 15),  # in order of fixed effects
  "Yhat for Age=80 Grip=9"  = c(1,-5, 0, 0,0,0,  0),
  "Yhat for Age=80 Grip=12" = c(1,-5, 3, 0,0,0,-15),
  "Yhat for Age=85 Grip=6"  = c(1, 0,-3, 0,0,0,  0),
  "Yhat for Age=85 Grip=9"  = c(1, 0, 0, 0,0,0,  0),
  "Yhat for Age=85 Grip=12" = c(1, 0, 3, 0,0,0,  0),
  "Yhat for Age=90 Grip=6"  = c(1, 5,-3, 0,0,0, -15),
  "Yhat for Age=90 Grip=9"  = c(1, 5, 0, 0,0,0,  0),
  "Yhat for Age=90 Grip=12" = c(1, 5, 3, 0,0,0,  15))),test=adjusted("none"))
##################################################################################

txtComment(" ") # insert blank space in output  


#############################################################################################
#####                   BEGIN GLM WITH SEX*DEMGROUP INTERACTION                         #####
#############################################################################################

print("R Eq 2.13: GLM Adding Sex by Dementia Group Interaction")
print("Dummy-Coded Predictors for Sex (0=Men) and Demgroup (0=None)")
ModelSexDem = lm(data=Example1, formula=cognition~1+age85+grip9+sexMW+demNF+demNC
                                         +age85:grip9 +sexMW:demNF +sexMW:demNC)
summary(ModelSexDem); anova(ModelSexDem) # anova to get residual variance

print("Omnibus DFnum=2 F-test for Sex*Demgroup Interaction")
SexDemFint = glht(model=ModelSexDem, linfct=c("sexMW:demNF=0","sexMW:demNC=0"))
summary(SexDemFint, test=Ftest()) # ask for joint hypothesis test instead of separate

print("Omnibus DF=2 F-test for Dementia Simple Main Effect for Men")
DemforM = glht(model=ModelSexDem, linfct=rbind(c(0,0,0,0,1,0,0,0,0),c(0,0,0,0,0,1,0,0,0)))
summary(DemforM, test=Ftest()) # ask for joint hypothesis test instead of separate

print("Omnibus DF=2 F-test for Dementia Simple Main Effect for Women")
DemforW = glht(model=ModelSexDem, linfct=rbind(c(0,0,0,0,1,0,0,1,0),c(0,0,0,0,0,1,0,0,1)))
summary(DemforW, test=Ftest()) # ask for joint hypothesis test instead of separate

print("Pred cognition outcomes --adjusted cell means-- holding age=85 and grip=9")
print("Will need to ignore impossible combinations of demNF and demNC for min:max")
PredSexDem = summary(prediction(model=ModelSexDem, type="response",
                     at=list(sexMW=0:1, demNF=0:1, demNC=0:1, age85=0, grip9=0)))
PredSexDem # print predicted outcomes

#######################################################################################
# Here is another way of using GLHT create predicted outcomes
print("GLHT pred cognition outcomes --adjusted cell means-- holding age=85 and grip=9")
summary(glht(model=ModelSexDem, linfct=rbind(
  "Yhat for Men   None"    = c(1,0,0,0,0,0,0,0,0),  # in order of fixed effects
  "Yhat for Women None"    = c(1,0,0,1,0,0,0,0,0),
  "Yhat for Men Future"    = c(1,0,0,0,1,0,0,0,0),
  "Yhat for Women Future"  = c(1,0,0,1,1,0,0,1,0),
  "Yhat for Men Current"   = c(1,0,0,0,0,1,0,0,0),
  "Yhat for Women Current" = c(1,0,0,1,0,1,0,0,1))),test=adjusted("none"))
#######################################################################################

print("DF=1 simple slopes for sex per demgroup, demgroup per sex, and interactions")
summary(glht(model=ModelSexDem, linfct=rbind(
  "Sex Diff for No Dementia"      = c(0,0,0,1, 0,0,0, 0,0),  # in order of fixed effects
  "Sex Diff for Future Dementia"  = c(0,0,0,1, 0,0,0, 1,0),
  "Sex Diff for Current Dementia" = c(0,0,0,1, 0,0,0, 0,1),
  "None-Future Diff for Men"      = c(0,0,0,0, 1,0,0, 0,0),
  "None-Future Diff for Women"    = c(0,0,0,0, 1,0,0, 1,0),
  "None-Current Diff for Men"     = c(0,0,0,0, 0,1,0, 0,0),
  "None-Current Diff for Women"   = c(0,0,0,0, 0,1,0, 0,1),
  "Future-Current Diff for Men"   = c(0,0,0,0,-1,1,0, 0,0),
  "Future-Current Diff for Women" = c(0,0,0,0,-1,1,0,-1,1),
  "A: Sex effect differ btw None and Future?"          = c(0,0,0,0,0,0,0, 1,0),
  "A: None-Future effect differ btw Men and Women?"    = c(0,0,0,0,0,0,0, 1,0),
  "B: Sex effect differ btw None and Current?"         = c(0,0,0,0,0,0,0, 0,1),
  "B: None-Current effect differ btw Men and Women?"   = c(0,0,0,0,0,0,0, 0,1),
  "C: Sex effect differ btw Future and Current?"       = c(0,0,0,0,0,0,0,-1,1),
  "C: Future-Current effect differ btw Men and Women?" = c(0,0,0,0,0,0,0,-1,1))),test=adjusted("none"))

print("Create data frame for plotting and remove unneeded rows")
PredSexDem = data.frame(PredSexDem) # first remove () from variable names
PredSexDem$sum = PredSexDem$at.demNF.+PredSexDem$at.demNC. # sum dummy codes
PredSexDem = subset(x=PredSexDem, PredSexDem$sum<2) # keep if sum<2
# Make demgroup combined variable for plot
PredSexDem$demgroup=NA # Make new empty variable to be recoded
PredSexDem$demgroup[which(PredSexDem$at.demNF.==0 & PredSexDem$at.demNC.==0)]=1
PredSexDem$demgroup[which(PredSexDem$at.demNF.==1 & PredSexDem$at.demNC.==0)]=2
PredSexDem$demgroup[which(PredSexDem$at.demNF.==0 & PredSexDem$at.demNC.==1)]=3

# Make and save plots
png(file = "R Sex by Demgroup=x GLM Plot.png")  # open file
plot(y=PredSexDem$Prediction, x=PredSexDem$demgroup, type="n", ylim=c(0,35), xlim=c(1,3),
     xlab="Dementia Group (1=None, 2=Future, 3=Current)", ylab="Predicted Cognition")
PredSexDem = sort_asc(data=PredSexDem,at.sexMW.) # 3 rows per sexMW now
lines(x=PredSexDem$demgroup[1:3], y=PredSexDem$Prediction[1:3], type="l", col="blue1")
lines(x=PredSexDem$demgroup[4:6], y=PredSexDem$Prediction[4:6], type="l", col="red1")
legend(x=1, y=35, legend = c("Sex=Men", "Sex=Women"), col=1:2, lty=1) #lty=linetype
dev.off()  # close file

png(file = "R Demgroup by Sex=x GLM Plot.png")  # open file
plot(y=PredSexDem$Prediction, x=PredSexDem$at.sexMW., type="n", ylim=c(0,35), xlim=c(0,1),
     xlab="Sex (0=Men, 1=Women)", ylab="Predicted Cognition")
PredSexDem = sort_asc(data=PredSexDem,demgroup) # 2 rows per demgroup now
lines(x=PredSexDem$at.sexMW.[1:2], y=PredSexDem$Prediction[1:2], type="l", col="blue1")
lines(x=PredSexDem$at.sexMW.[3:4], y=PredSexDem$Prediction[3:4], type="l", col="red1")
lines(x=PredSexDem$at.sexMW.[5:6], y=PredSexDem$Prediction[5:6], type="l", col="green1")
legend(x=0, y=36, legend = c("Demgroup=None", "Demgroup=Future", "Demgroup=Current"), 
       col=1:3, lty=1) #lty=linetype
dev.off()  # close file


# Close output text file
txtStop()


