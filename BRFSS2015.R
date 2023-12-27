#Diana Bowden
#Data Analytics in R(DTSC-650-80)
#Final Project: Correlation of Exercise, Weight and Marital Status on prediction of Dementia.

#load required packages and files
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lm.beta))
BRFSS2015<-read_csv('BRFSS2015.csv')

#Section Q1
#filter on HLTHPLN1 data where coverage is yes ("1"); total up number of those with coverage.
Q1<-BRFSS2015%>%
  filter(HLTHPLN1== "1")%>%
  summarise(n=n())

#Section Q2 
#Filter state and mental health parameters I want vs. don't want; replace the 88 (none) code with 0 per instructions;
# take mean; did not pipe; used the df$variable function instead, which can be easier when variable name is complex
q2<-BRFSS2015 %>%
  filter(BRFSS2015$`_STATE` == "42", BRFSS2015$MENTHLTH < 89 & BRFSS2015$MENTHLTH !=77)
BRFSS2015$MENTHLTH[BRFSS2015$MENTHLTH == 88] <- 0
Q2<-round(mean(BRFSS2015$MENTHLTH),2)

#Section Q3
#select arthritis and weight columns before filtering on values needed. Group by values needed and summarise with mean,
#sd while also converting values to lbs; rounded final result.
q3<-BRFSS2015 %>%
  select(HAVARTH3, WTKG3) %>%
  filter(HAVARTH3== "1" | HAVARTH3 == "2", WTKG3 != 99999)%>%
  group_by(HAVARTH3)%>%
  summarise(mean_weight = mean(WTKG3*.0220462, na.rm = TRUE), sd_weight = sd(WTKG3 *.0220462, na.rm = TRUE))%>%
  select(mean_weight, sd_weight)
Q3<-round((q3),2)

#Section Q4
#Use quantile function to get lower and upper outliers; assign outliers to variable; determine % data left for Q4.
exercise_lower<-quantile(BRFSS2015$PA1MIN_, .003, na.rm=TRUE)
exercise_upper<-quantile(BRFSS2015$PA1MIN_, .997, na.rm=TRUE)
exercise_out<-which(BRFSS2015$PA1MIN_ > exercise_upper| BRFSS2015$PA1MIN_ < exercise_lower)
Q4<-round((nrow(BRFSS2015) - length(exercise_out))/nrow(BRFSS2015)*100,2)

#Section Q5
#Exercise values expressed in minutes, per codebook report; means reasonable; max values seem very high. 
#Perhaps physical activity variable could be work-related too. Without more info,  will leave values the way they are.
exercise_noout <-BRFSS2015[-exercise_out,]%>%
  select(MARITAL, PA1MIN_)%>%
  filter(MARITAL!=9)
exercise_noout$MARITAL <- as.factor(exercise_noout$MARITAL)
exercise_noout_orblanks<-na.omit(exercise_noout)%>%
  group_by(MARITAL)%>%
  summarise(mean = round(mean(PA1MIN_),2), sd = round(sd(PA1MIN_),2), max= round(max(PA1MIN_),2), min = round(min(PA1MIN_),2))
Q5<-exercise_noout_orblanks

#section Q6
#outliers removed per specs. Data still shows outliers; however, no further adjustments made unless instructed.
Q6<-ggplot(data = exercise_noout, mapping = aes(x=MARITAL, y = PA1MIN_)) + 
  geom_boxplot() +
  labs(x = "Marital_Status", y = "Total_Exercise")

#section Q7
#Please note I re-coded / re-named the cleaned data for regression. Selected columns and filtered out values not needed.
#Created levels using as.factor() function for Marital status and eliminated na's to get clean, filtered data.
#Ran regression predicting exercise based on Marital status.
exercise_noout<-BRFSS2015[-exercise_out,]%>%
  select(MARITAL, PA1MIN_)%>%
  filter(MARITAL!=9)
exercise_noout$MARITAL <- as.factor(exercise_noout$MARITAL)
clean_project_data<-na.omit(exercise_noout)
clean_project_data

q7<-lm(clean_project_data$PA1MIN_ ~ clean_project_data$MARITAL)
Q7<-summary(q7)

#Section Q8
#Ran ANOVA using aov() comparing exercise across marital status.used TukeyHsD() to asess significance of diff between
#pairs of group means.
modq8<-aov(PA1MIN_~ MARITAL, data = clean_project_data)
Q8<-TukeyHSD(modq8, conf.level = .95)

#Section Q9
#Ran AIC on q9 and on q7 - determined q9 had higher AIC and slightly higher adjusted R2. 
#Did not remove na's because of repeated errors and unequal dataframes, so left data as is.

q9_data<-BRFSS2015%>%
  select(MARITAL, PA1MIN_, '_FRUTSUM')%>%
  filter(MARITAL!=9)
q9_data$MARITAL <- as.factor(q9_data$MARITAL)

q9_model<-lm(q9_data$PA1MIN_ ~ q9_data$MARITAL + q9_data$'_FRUTSUM')

#Compare IAcs between q7 and q9 and determine which one has lowest IAC; q7 had lower IAC, so assigned to Q9.
#made decision that q7 is better model because of lower AIC; adjusted R2 was not that much different.
AIC(q9_model)
AIC(q7)
Q9<-AIC(q7)

#Sections Q10-Q14
#General -Project explores relationship between cog. decline, vigorous physical activity, hours worked/week and weight.

#Section Q10 Variables: filter out variables that represent refused, don't know, blank etc.
#Q10, Variable 1: Cognitive Decline: values are categorical:  1 = yes, 2 = No; keep these values; Remove values
#of 7 = don't know and 9 = Refused
#Q10, Variable 2: Vigorous Physical Activity: values are  1-999 total minutes/week. Remove values of Blank / NA's
#Q10, Variable 3: Hours Worked per Week: Values are 1-96; 98 = 0 (need to replace 98 with 0 value); Remove all values
# with 97 =don't know, 99= refused and Blank(NA's)
#Q10, Variable 4: Weight(lbs): values are 50-999; removed all other values of refused, don't know or answers in kg.
#Q10 #Decimals implied?? No, I do not see any decimals implied in the variables I have selected.
#Q10 - Final code for filtered data based on parameters is below. Will remove na's within the functions as needed.

filtered_data<-BRFSS2015%>%
  select(CIMEMLOS, PA1VIGM_, WEIGHT2, SCNTWRK1)%>%
  filter(PA1VIGM_ <980 &  CIMEMLOS <3 & WEIGHT2 <999 & SCNTWRK1<97)
filtered_data

#Section Q11
#Q11 General: Ran ggplot histograms; removed outliers for work and weight (see comments below for specifics)
#Q11, Variable 1: There are only 2 responses allowed (1 or 2), so no outliers will need to be removed
#Q11, Variable 2: Vig.Exercise: values are 1-999; based on histogram visual, using quantile +-2sd to remove outliers.
#Q11(cont) - For 0 exercise, the value in R is 5.397605e-79; when I round at end, this will be "0", so ok for now.
#Q11, Variable 3: Hrs Worked/wk: Values are 1-96; 98 = 0; based on histogram, using quantile +-sd to remove outliers.
#Q11, Variable 4: Weight(lbs): values are 0-999; after looking at values, I do not want to exclude any outliers 
#since I believe they are pertinent to results. Not removing outliers here.

#Q11 Summary - only exclude outliers for exercise and hours worked per week.
vig_ex_lower<-quantile(filtered_data$PA1VIGM_, .003, na.rm=TRUE)
vig_ex_upper<-quantile(filtered_data$PA1VIGM_, .997, na.rm=TRUE)
vig_ex_out<-which(filtered_data$PA1VIGM_ > vig_ex_upper | filtered_data$PA1VIGM_<vig_ex_lower)
vig_ex_out

hrs_work_lower <-quantile(filtered_data$SCNTWRK1, .003, na.rm = TRUE)
hrs_work_upper <-quantile(filtered_data$SCNTWRK1, .997, na.rm = TRUE)
hrs_work_out <-which(filtered_data$SCNTWRK1 > hrs_work_upper | filtered_data$SCNTWRK1 < hrs_work_lower)
hrs_work_out


#Section Q12
#Q12a - Cognitive Decline - Categorical variable with 2 responses; no outliers removed but filtered out unessential data.

cog_dec_hist <-ggplot(data_noblanks, aes(x = data_noblanks$CIMEMLOS))+
  geom_histogram(bins = 30)
cog_dec_hist

#Q12b - Weight(lbs) - removed 777 & 999 values and ran histogram; keeping all values in this variable.
weight_hist<-ggplot(filtered_data, aes(WEIGHT2)) +
  geom_histogram(bins = 30)
weight_hist

#Q12c - Total Hours Work / week - data normally distributed; will remove data >97 per opening comments and 
#also will apply quartile function for outliers +-2sd.
total_work <-ggplot(BRFSS2015, aes(BRFSS2015$SCNTWRK1)) +
  geom_histogram(bins = 30)
total_work

#Q12d - Total Vigorous Physical Activity - I decided to apply quantile function for outliers +-2sd.
vigorex_hist <- ggplot(BRFSS2015, aes(x = BRFSS2015$PA1VIGM_)) +
  geom_histogram(bins = 200)
vigorex_hist

#12-clean data no outliers - removed outliers from vig exercise and work and created filtered data without outliers.
filt_data_noexouts <-filtered_data[-vig_ex_out,]
filt_data_noouts<-filt_data_noexouts[-hrs_work_out,]
filt_data_noouts

#Section Q13 
#Before running descriptive statistics, convert Cognitive Decline observations to levels/factors as we did in Q7.
#assigning clean data to Final_data.

Final_data<-filt_data_noouts%>%
  filt_data_noouts$CIMEMLOS <- as.factor(filt_data_noouts$CIMEMLOS)

#Section Q13
#Assumed Descriptive Statistics to be same as Q5: mean, sd, min, max for each variable, grouped by cognitive decline.
#I reformatted this so it was easier for you to grade but hope it stayed intact! rounded each statistic to 2.
q13<-Final_data%>%
  group_by(cognitive_decline = CIMEMLOS)%>%
  summarise(mean_exercise = round(mean(PA1VIGM_),2), mean_weight = round(mean(WEIGHT2),2), mean_work = round(mean(SCNTWRK1),2), 
  sd_exercise = round(sd(PA1VIGM_),2), sd_weight = round(sd(WEIGHT2),2),   sd_work=round(sd(SCNTWRK1),2), 
  min_exercise = round(min(PA1VIGM_),2), min_weight = round(min(WEIGHT2),2),   min_work=round(min(SCNTWRK1),2), 
  max_exercise = round(max(PA1VIGM_),2), max_weight = round(max(WEIGHT2),2), max_work = round(max(SCNTWRK1),2))

#Section Q14
#Selected Logistic Regression to predict whether or not cognitive decline is predicted (categorical outcomes), based on:
# work, vigorous exercise and weight. For cog decline, "1" = yes, "2" = No

#Step 1: run model with no predictors; AIC = 4131
q14null<-glm(CIMEMLOS ~ 1, family = binomial(), data = Final_data)

#Step2: run model with 3 predictors; note weight and work significant; vigorous exercise not significant; AIC=4116
q14_mod1<-glm(CIMEMLOS ~ WEIGHT2 + PA1VIGM_ + SCNTWRK1, family = binomial(), data = Final_data)

#Step 3: run model with 2 predictors: weight and work.AIc = 4114.5; both weight and work are significant.
q14_mod2<-glm(CIMEMLOS ~ WEIGHT2 + SCNTWRK1, family = binomial(), data = Final_data)

#Step 4: run model with just weight predictor. AIC = 4125.9; still significant predictor.
q14_mod3<-glm(CIMEMLOS ~ WEIGHT2, family = binomial(), data = Final_data)

#Step 5: run model with just work predictor; AIC = 4123; still significant predictor.

#Final Step: compare models and AIC values to determine best model. best model is q14_mod2 (2 predictors)
#weight and work.AIC is lowest in this model and significant result obtained. Renaming mod2 as Q14 as best model fit.
Q14<-q14_mod2





