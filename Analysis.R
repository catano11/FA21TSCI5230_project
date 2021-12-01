#' ## Model  Summary

#' Fitting data to appropriate statistic linear model with our predictors
# fit <- lm(_______ ~ ACS_PER_CAPITA_INCOME + ACS_PCT_POSTHS_ED + ACS_MEDIAN_HOME_VALUE + ACS_PCT_MEDICAID_ANY + ACS_PCT_DISABLE + sdi_score, data = ________);
# summary(fit);
#' Provide a clean version of the summary - both produce the same results
# summary(fit) %>% tidy;
# fit %>% tidy;
#' Show a quick look at all of the summary statistics
# glance(fit);

#' Create a 'null' model that does not specify predictors
# fit0 <- lm(________ ~ 1, data = ________);
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
# fit1 <- lm(________ ~ (ACS_PER_CAPITA_INCOME + ACS_PCT_POSTHS_ED + ACS_MEDIAN_HOME_VALUE + ACS_PCT_MEDICAID_ANY + ACS_PCT_DISABLE + sdi_score)^2, data = ________);

# If you forget the arguments for a function you can check it with args()
# i.e. args(step)
#' Create a BIC stepwise regression
# fitBIC <- step(fit, scope = list(lower = fit0, upper = fit1), scale = 0, direction = "both", k = log(_));
# summary(fitBIC) %>% tidy;

#' Compare the models
# anova(fit0, fit1);
# plot(fit1)
# plot(data$________, predict(fit1,dat1test)-dat1test$________)
# plot(dat1train$________, predict(fit1)-dat1train$________)

#' [residual plots go here]'

#' [plot of predicted vs observed values goes here]'