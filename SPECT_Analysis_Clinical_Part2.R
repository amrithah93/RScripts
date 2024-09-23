setwd("/Users/amrithah/Desktop")
attach(Loadings_Parameters) #attach file with all loading parameters 
#next, more clean up
Loadings_Parameters <-data.frame(Loadings_Parameters) #unlist the whole file so that analysis is easier
#read specific rows and columns to make a specific two variables for HC and SCZ from the loading means worksheet

attach(healthy_8_31_23_symptom_checklist)
attach(schiz_8_31_23_dx_classes_symptoms)

Group_Controls <- Group
Group_Patients <- group

library(readxl) #load readxl package
#create variables for controls and patients from loading parameters
HC_Loading_Parameters<-read_excel("/Users/amrithah/Desktop/Loadings_Parameters.xlsx", sheet = "HC_Loading_Parameters") #healthy control loading parameters
#remove extra column in the beginning that somehow showed up
HC_Loading_Parameters <- HC_Loading_Parameters[ -c(1,1) ] #removed!
SCZ_Loading_Parameters<-read_excel("/Users/amrithah/Desktop/Loadings_Parameters.xlsx", sheet = "SZ_Loading_Parameters" ) #scz loading parameters

#transpose variables for regression set up as the dependent variable with the loading parameters
HC_transp <-t(HC_Loading_Parameters) #healthy control transpose
SZ_transp <-t(SCZ_Loading_Parameters) #now the patients
#mean of 53 components for HC and SCZ

#now load mean parameter values as variables
HC_mean<-read_excel("/Users/amrithah/Desktop/Loadings_Means_Values.xlsx", sheet = "Mean Loading Parameters Plot", "B1:B54") #average loading parameters for each control subject
SZ_mean<-read_excel("/Users/amrithah/Desktop/Loadings_Means_Values.xlsx", sheet = "Mean Loading Parameters Plot", "C1:C54") #average loading parameters for each patient subject
################################
t.test(HC_mean,SCZ_mean) #do t-test comparing the two groups initially

###setting up regression models for clinical variables##
#use tidyr to clean up data
library(tidyr)
attach(schiz_8_31_23_dx_classes_symptoms) #attach scz clinical dataset from David
attach(healthy_8_31_23_symptom_checklist) #attach healthy control dataset from David
HC_clinical<-read_excel("/Users/amrithah/Desktop/healthy_8_31_23_symptom_checklist.xls") # load clinical variables
SZ_clinical<-read_excel("/Users/amrithah/Desktop/schiz_8_31_23_dx_classes_symptoms.xls") # load clinical variables
Patient_Group <-group #coding for patient group 

#Lots of missing data in both datasets so...know which ones have complete cases

#define demographic variables
hc_age <-read_excel("/Users/amrithah/Desktop/healthy_8_31_23_symptom_checklist.xls", sheet = "healthy clinical", "E1:E77") # load age
hc_sex<-read_excel("/Users/amrithah/Desktop/healthy_8_31_23_symptom_checklist.xls", sheet = "healthy clinical", "D1:D77") # load sex variable


#same for patients
sz_age <-read_excel("/Users/amrithah/Desktop/schiz_8_31_23_dx_classes_symptoms.xls", sheet = "sz clinical", "D1:D138") #load age
sz_sex <-read_excel("/Users/amrithah/Desktop/schiz_8_31_23_dx_classes_symptoms.xls", sheet = "sz clinical", "F1:F138") #load sex variable


#now, remove any missing stuff
sz_age <-na.omit(sz_age) #remove any missing data from age
hc_age <-na.omit(hc_age) #remove any missing data from age
#unlist variables for readability
sz_age <-unlist(sz_age)
hc_age <-unlist(hc_age)
sz_sex<-unlist(sz_sex)
hc_sex<-unlist(hc_sex)

#first set of regression models starting with patient dataå
#first set of linear models: let's start with looking at age, sex, BSC_item 68 x against patients loading matrices
Y1 <- SZ_transp #list all loading parameters for patients
Y1 <-data.matrix(Y1)
model1 <- aov(Y1 ~ sz_age + sz_sex+ BSC_68_Experiencing_periods_of_troubled_breathing_or_feeling_smothered + BSC_100_Having_dark_thoughts_ones_that_may_involve_suicidal_or_homicidal_thoughts, data=SZ_clinical, na.action=na.omit)
summary.aov(model1) #view model output

#second model - looking at more social cognition variables now 
Y2 <-SZ_transp
#clean out all NAs in dataset
Y2 <-na.omit(SZ_transp) #loading parameters for patients
Y2 <-data.matrix(Y2)
model2 <- aov(Y2 ~ sz_age + sz_sex + LDS_64_I_am_teased_by_others + BSC_75_Avoiding_conflict + GSC_A_104_Being_in_an_inappropriate_mood_for_a_given_situation_laughing_at_sad_events + `GSC_A_31_Fearing_going_crazy_or_doing_something_out-of-control`, data=SZ_clinical, na.action=na.omit)
summary.aov(model2)

#relationship between age and loading parameters and GSC scores
Y3<-SZ_transp
model3<-aov(Y3 ~ sz_age + sz_sex + GSC_A_Score, data=SZ_clinical, na.action = na.omit)
summary.aov(model3)

##lot of missing data, so check with Vince on what option he prefers
#now do the same with clinical variables

#same model but in controls
Y4 <- HC_transp #list all loading parameters for patients
Y4 <-data.matrix(Y4)
model4 <- aov(Y4 ~ hc_age + hc_sex + BSC_75_Avoiding_conflict + GSC_A_104_Being_in_an_inappropriate_mood_for_a_given_situation_laughing_at_sad_events + `GSC_A_31_Fearing_going_crazy_or_doing_something_out-of-control`, data=HC_clinical, na.action=na.omit)
summary.aov(model4) #view model output

Y5 <- HC_transp #list all loading parameters for patients
Y5 <-data.matrix(Y5)
model5 <- aov(Y5 ~ hc_age + hc_sex+ BSC_68_Experiencing_periods_of_troubled_breathing_or_feeling_smothered + BSC_100_Having_dark_thoughts_ones_that_may_involve_suicidal_or_homicidal_thoughts, data=HC_clinical, na.action=na.omit)
summary.aov(model5) #view model output

###now look specifically at positive and negative symptoms in patient group, and cluster models as such ####

#looking at perceptual abnormalities and relationships with clustered symptoms **USE THIS MODEL IN THE PAPER AS THE FIRST MODEL**
Y6 <- SZ_transp
Y6 <-data.matrix(Y6)
GSC_A_98_Hearing_voices_or_sounds_that_are_not_real <-na.omit(GSC_A_98_Hearing_voices_or_sounds_that_are_not_real)
model6 <-aov(Y6 ~ sz_age + sz_sex + GSC_A_98_Hearing_voices_or_sounds_that_are_not_real + LDS_76_I_have_an_unusual_sensitivity_to_certain_smells + LDS_77_I_have_an_unusual_sensitivity_to_light, data=SZ_clinical, na.action=na.omit)
summary.aov(model6)

#looking at clustered symptoms
#looking at hallucinations/delusions symptoms 
Y7 <- SZ_transp
Y7 <-data.matrix(Y7)
model7 <-aov(Y7 ~sz_age + sz_sex + GSC_A_98_Hearing_voices_or_sounds_that_are_not_real + LDS_76_I_have_an_unusual_sensitivity_to_certain_smells + LDS_77_I_have_an_unusual_sensitivity_to_light + age:LDS_77_I_have_an_unusual_sensitivity_to_light + age:GSC_A_98_Hearing_voices_or_sounds_that_are_not_real + age:LDS_76_I_have_an_unusual_sensitivity_to_certain_smells, data=SZ_clinical, na.action=na.omit)
summary.aov(model7)

#looking at simpler model **USE THIS MODEL IN THE PAPER AS THE SECOND MODEL*** using the SZ_mean FNC
 #correlations corrected for patients
Y8 <-SZ_transp #Loading parameters for patients 
Y8 <-data.matrix(Y8)
#rename variables for brevity
model8 <-aov(Y8 ~age + sex + GSC_A_98_Hearing_voices_or_sounds_that_are_not_real + GSC_A_99_Experiencing_periods_of_time_where_your_thoughts_or_speech_were_disjointed_or_didnt_make_sense_to_you_or_others, data=schiz_8_31_23_dx_classes_symptoms, na.action=na.omit)
summary.aov(model8)
results_model8 <-summary.aov(model8)
capture.output(results_model8, file = "results_model8_regression_summary.txt")

#calculate eta squared using lsr package
library('lsr') #load lsr package
etaSquared(model8)

#concatenate p vals for FDR analysis
pval_response1_model8<-results_model8[[" Response 1"]][["Pr(>F)"]]
pval_response2_model8<-results_model8[[" Response 2"]][["Pr(>F)"]]
pval_response3_model8<-results_model8[[" Response 3"]][["Pr(>F)"]]
pval_response4_model8<-results_model8[[" Response 4"]][["Pr(>F)"]]
pval_response5_model8<-results_model8[[" Response 5"]][["Pr(>F)"]]
pval_response6_model8<-results_model8[[" Response 6"]][["Pr(>F)"]]
pval_response7_model8<-results_model8[[" Response 7"]][["Pr(>F)"]]
pval_response8_model8<-results_model8[[" Response 8"]][["Pr(>F)"]]
pval_response9_model8<-results_model8[[" Response 9"]][["Pr(>F)"]]
pval_response10_model8<-results_model8[[" Response 10"]][["Pr(>F)"]]
pval_response11_model8<-results_model8[[" Response 11"]][["Pr(>F)"]]
pval_response12_model8<-results_model8[[" Response 12"]][["Pr(>F)"]]
pval_response13_model8<-results_model8[[" Response 13"]][["Pr(>F)"]]
pval_response14_model8<-results_model8[[" Response 14"]][["Pr(>F)"]]
pval_response15_model8<-results_model8[[" Response 15"]][["Pr(>F)"]]
pval_response16_model8<-results_model8[[" Response 16"]][["Pr(>F)"]]
pval_response17_model8<-results_model8[[" Response 17"]][["Pr(>F)"]]
pval_response18_model8<-results_model8[[" Response 18"]][["Pr(>F)"]]
pval_response19_model8<-results_model8[[" Response 19"]][["Pr(>F)"]]
pval_response20_model8<-results_model8[[" Response 20"]][["Pr(>F)"]]
pval_response21_model8<-results_model8[[" Response 21"]][["Pr(>F)"]]
pval_response22_model8<-results_model8[[" Response 22"]][["Pr(>F)"]]
pval_response23_model8<-results_model8[[" Response 23"]][["Pr(>F)"]]
pval_response24_model8<-results_model8[[" Response 24"]][["Pr(>F)"]]
pval_response25_model8<-results_model8[[" Response 25"]][["Pr(>F)"]]
pval_response26_model8<-results_model8[[" Response 26"]][["Pr(>F)"]]
pval_response27_model8<-results_model8[[" Response 27"]][["Pr(>F)"]]
pval_response28_model8<-results_model8[[" Response 28"]][["Pr(>F)"]]
pval_response29_model8<-results_model8[[" Response 29"]][["Pr(>F)"]]
pval_response30_model8<-results_model8[[" Response 30"]][["Pr(>F)"]]
pval_response31_model8<-results_model8[[" Response 31"]][["Pr(>F)"]]
pval_response32_model8<-results_model8[[" Response 32"]][["Pr(>F)"]]
pval_response33_model8<-results_model8[[" Response 33"]][["Pr(>F)"]]
pval_response34_model8<-results_model8[[" Response 34"]][["Pr(>F)"]]
pval_response35_model8<-results_model8[[" Response 35"]][["Pr(>F)"]]
pval_response36_model8<-results_model8[[" Response 36"]][["Pr(>F)"]]
pval_response37_model8<-results_model8[[" Response 37"]][["Pr(>F)"]]
pval_response38_model8<-results_model8[[" Response 38"]][["Pr(>F)"]]
pval_response39_model8<-results_model8[[" Response 39"]][["Pr(>F)"]]
pval_response40_model8<-results_model8[[" Response 40"]][["Pr(>F)"]]
pval_response41_model8<-results_model8[[" Response 41"]][["Pr(>F)"]]
pval_response42_model8<-results_model8[[" Response 42"]][["Pr(>F)"]]
pval_response43_model8<-results_model8[[" Response 43"]][["Pr(>F)"]]
pval_response44_model8<-results_model8[[" Response 44"]][["Pr(>F)"]]
pval_response45_model8<-results_model8[[" Response 45"]][["Pr(>F)"]]
pval_response46_model8<-results_model8[[" Response 46"]][["Pr(>F)"]]
pval_response47_model8<-results_model8[[" Response 47"]][["Pr(>F)"]]
pval_response48_model8<-results_model8[[" Response 48"]][["Pr(>F)"]]
pval_response49_model8<-results_model8[[" Response 49"]][["Pr(>F)"]]
pval_response50_model8<-results_model8[[" Response 50"]][["Pr(>F)"]]
pval_response51_model8<-results_model8[[" Response 51"]][["Pr(>F)"]]
pval_response52_model8<-results_model8[[" Response 52"]][["Pr(>F)"]]
pval_response53_model8<-results_model8[[" Response 53"]][["Pr(>F)"]]

capture.output(pval_response1_model8, pval_response2_model8, pval_response3_model8, pval_response4_model8, pval_response5_model8, pval_response6_model8, pval_response7_model8, pval_response8_model8, pval_response9_model8,pval_response10_model8, pval_response11_model8, pval_response12_model8, pval_response13_model8, pval_response14_model8, pval_response15_model8, pval_response16_model8, pval_response17_model8, pval_response18_model8, pval_response19_model8, pval_response20_model8, pval_response21_model8, pval_response22_model8, pval_response23_model8, pval_response24_model8, pval_response25_model8, pval_response26_model8, pval_response27_model8, pval_response28_model8, pval_response29_model8, pval_response30_model8, pval_response31_model8, pval_response32_model8, pval_response33_model8, pval_response34_model8, pval_response35_model8, pval_response36_model8, pval_response37_model8, pval_response38_model8, pval_response39_model8, pval_response40_model8, pval_response41_model8, pval_response42_model8, pval_response43_model8, pval_response44_model8, pval_response45_model8, pval_response46_model8, pval_response47_model8, pval_response48_model8, pval_response49_model8, pval_response50_model8, pval_response51_model8, pval_response52_model8, pval_response53_model8, file = "regression results_model8_hallucinations.txt") #output regression results into a text file
regression_pvals_hallucinations <-regression.results_model8_hallucinations

Y9 <-Y6 #Loading parameters for patients 
Y9 <-data.matrix(Y9)
#rename variables for brevity
model9 <-aov(Y9 ~sz_age + sz_sex + GSC_A_98_Hearing_voices_or_sounds_that_are_not_real + GSC_A_99_Experiencing_periods_of_time_where_your_thoughts_or_speech_were_disjointed_or_didnt_make_sense_to_you_or_others, data=SZ_clinical, na.action=na.omit)
results_model9 <-summary.aov(model9)
#use capture output to export anova results
capture_a <- summary.aov(model9)
capture.output(capture_a, file = "anova results.txt")


#test models
Y_test <- aov(SZ_transp ~ age + GSC_A_98_Hearing_voices_or_sounds_that_are_not_real, data=SZ_clinical, na.action=na.omit) #test models and see if it works
etaSquared(Y_test) #worked

### run a similar model in healthy controls and see what changes come up
Y9 <- HC_transp
Y9 <-data.matrix(Y9)
#rename variables for brevity
model9 <-aov(Y9 ~ hc_age + hc_sex + GSC_A_98_Hearing_voices_or_sounds_that_are_not_real + GSC_A_99_Experiencing_periods_of_time_where_your_thoughts_or_speech_were_disjointed_or_didnt_make_sense_to_you_or_others, data = HC_clinical, na.action=na.omit)
summary.aov(model9)

#run GLM to check David's questions
attach(regression_data)
Y10 <- loadings
Y10 <-data.matrix(Y10)
#rename variables for brevity
model10 <-aov(Y10 ~ age + sex_coded + Diagnosis, data = regression_data, na.action=na.omit)
summary.aov(model10)
model10_coefficients <-model10[["coefficients"]]
results_model10_regression <- tidy(model10)
capture_regression <- summary.aov(model10)
capture.output(capture_regression, file = "regression results.txt") #output regression results into a text file
capture.output(model10_coefficients, file = "regression_coefficients.txt") #get coefficients for interpretation

#use broom to tidy up the regressions and export into excel for the sz model
library(broom)
library(xlsx)
results_model8 = summary.aov(model8)
results_model8_clean = tidy(results_model8)
write.xlsx(results_model8,"results_model8")

#same for healthy controls
library(broom)
results_model9_clean <-unlist(results_model9_clean)
results_model9_clean <-summary.aov(model9) #generate model 9 summary
write.xls(results_model9_clean, "results_model9_clean.xlsx") #write initial results to .csv file

library(broom)
results_model10_clean <-summary.aov(model10) #generate model 9 summary
write.xlsx(results_model10_clean, "results_model10_clean.xlsx") #write initial results to .csv file

#compute cross correlations of variables to check if they are highly related to each other
#remove NAs
GSC_A_98_Hearing_voices_or_sounds_that_are_not_real <-na.omit(GSC_A_98_Hearing_voices_or_sounds_that_are_not_real)
LDS_76_I_have_an_unusual_sensitivity_to_certain_smells <-na.omit(LDS_76_I_have_an_unusual_sensitivity_to_certain_smells)
LDS_77_I_have_an_unusual_sensitivity_to_light <-na.omit(LDS_77_I_have_an_unusual_sensitivity_to_light)
#now run the cross correlations on the pairs and plot for each pair and plot them as well with Pearson correlations

library("ggpubr") #use this package to plot the variables with each other 
library("ggcorrplot") #use this to visualize correlation matrix
library("corrplot")

ggscatter(schiz_8_31_23_dx_classes_symptoms, x = "GSC_A_98_Hearing_voices_or_sounds_that_are_not_real", y = "LDS_76_I_have_an_unusual_sensitivity_to_certain_smells", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "GSC_A_98", ylab = "LDS_76")

#make a correlation matrix of all three variables and set up variables
correlations_matrix <- cbind(GSC_A_98_Hearing_voices_or_sounds_that_are_not_real, LDS_76_I_have_an_unusual_sensitivity_to_certain_smells, LDS_77_I_have_an_unusual_sensitivity_to_light)
correlations_matrix<-na.omit(correlations_matrix) #remove NA's 
correlations_matrix <- cor(correlations_matrix, method = c("pearson"))
correlations_matrix <-as.table(correlations_matrix)
correlations_matrix

ggcorrplot(cor(correlations_matrix)) #correlation matrix 1; make sure to zoom into figure to see values
ggcorrplot(correlations_matrix,
           hc.order = TRUE,
           type = "full",
           lab = TRUE)

##
correlations_matrix2 <- cbind(GSC_A_98_Hearing_voices_or_sounds_that_are_not_real, GSC_A_Score)
correlations_matrix2<-na.omit(correlations_matrix2) #remove NA's 
correlations_matrix2 <- cor(correlations_matrix2, method = c("pearson"))
correlations_matrix2 <-as.table(correlations_matrix2)
correlations_matrix2

#now plot this in a correlation matrix
ggcorrplot(cor(correlations_matrix2)) #correlation matrix 1; make sure to zoom into figure to see values
ggcorrplot(correlations_matrix2,
           hc.order = TRUE,
           type = "full",
           lab = TRUE)

#####

correlations_matrix3 <- cbind(GSC_A_98_Hearing_voices_or_sounds_that_are_not_real, GSC_A_Score, GSC_A_100_Feeling_socially_isolated_or_withdrawn, GSC_A_105_Having_a_marked_lack_of_initiative, GSC_A_102_Behaving_peculiarly, `GSC_A_11_Having_periods_of_a_very_high_self-esteem_or_grandiose_thinking`)
correlations_matrix3<-na.omit(correlations_matrix3) #remove NA's 
correlations_matrix3 <- cor(correlations_matrix3, method = c("pearson"))
correlations_matrix3 <-as.table(correlations_matrix3)
correlations_matrix3

#now plot this in a correlation matrix
ggcorrplot(cor(correlations_matrix3)) #correlation matrix 1; make sure to zoom into figure to see values
ggcorrplot(correlations_matrix3,
           hc.order = TRUE,
           type = "full",
           lab = TRUE)
