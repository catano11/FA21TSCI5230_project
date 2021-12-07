#' ## Model  Summary

#' Call for any packages and data needed to run script
packageList <- c("knitr","Publish","ggplot2","rio","GGally","dplyr","pander","readr","synthpop", "rmarkdown", "readxl", "tibble", "broom")
for(package in packageList){
  if(!require(package,character.only = TRUE)){
    install.packages(package);require(package,character.only = TRUE);}
};

if(!file.exists('data/dtrain.Rds')){source('Analysis.R',local=TRUE,verbose=FALSE)};
 dtrain <- load('data/dtrain.Rds');
 
#' Fitting data to appropriate statistic linear model with our predictors
fit <- lm(NOREADMISSION ~ ACS_PER_CAPITA_INCOME + ACS_PCT_POSTHS_ED + ACS_MEDIAN_HOME_VALUE + ACS_PCT_MEDICAID_ANY + ACS_PCT_DISABLE + SDI_SCORE, data = dtrain);
# summary(fit);
#' Provide a clean version of the summary - both produce the same results
# summary(fit) %>% tidy;
# fit %>% tidy;
#' Show a quick look at all of the summary statistics
# glance(fit);

#' Create a 'null' model that does not specify predictors
fit0 <- lm(NOREADMISSION ~ 1, data = dtrain);
# summary(fit0);
# summary(fit0) %>% tidy;
# fit0 %>% tidy;
# glance(fit0);

# When using lm repeatedly you can use '.~' to keep the original outcome and '~.' to keep the original predictors
#'
#' Create an all encompassing model for your predictors
# Include some interaction terms with interaction model: 'OUTCOME ~ PRED1*PRED2*PRED3 ...'
# For all 2 way interactions : '~ (A+B+C)^2', for all 3 way interactions: '~ (A+B+C)^3'
# Additive model for note: 'OUTCOME ~ PRED1 + PRED2 + PRED3 + ...'
fit1 <- lm(NOREADMISSION ~ (ACS_PER_CAPITA_INCOME + ACS_PCT_POSTHS_ED + ACS_MEDIAN_HOME_VALUE + ACS_PCT_MEDICAID_ANY + ACS_PCT_DISABLE + SDI_SCORE)^2, data = dtrain);

# If you forget the arguments for a function you can check it with args()
# i.e. args(step)
#' Create a BIC stepwise regression
# BIC is when k is the log of the number of rows of the data file
# AIC is always set at 2
nrow(dtrain)
fitBIC <- step(fit, scope = list(lower = fit0, upper = fit1), scale = 0, direction = "both", k = log(32707));
# summary(fitBIC) %>% tidy;

#' Compare the models
# anova(fit0, fit1);
# plot(fit1);
# plot(data$________, predict(fit1,dat1test)-dat1test$________);
# plot(dat1train$________, predict(fit1)-dat1train$________)

#Save enviroment of the script for use in R markdown file
save.image(file='data/Analysis.Rdata')
#load('data/Analysis.Rdata')
saveRDS(fit, file="data/analysis_fit.RDS")
saveRDS(fitBIC, file="data/analysis_fitBIC.RDS")
