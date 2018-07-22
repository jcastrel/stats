#run simple linear regression between discounting measures and D2 BPnd ROIs
# libraries
library("plyr")
.pardefault <- par()
dev.off()

# load data
data = read.csv(file="./study01_data.csv",header=TRUE, sep=",")
#get regions
varlist <- names(data)[13:29]

#####################################
#Proportion of Smaller-Sooner Options
#run linear models with proportion of smaller-sooner options
models_ss <- lapply(varlist, function(x) {
    lm(substitute(chosesnr ~ i, list(i = as.name(x))), data = data)
})
#look at models - optional
#models_ss
## apply summary to each model stored in the list, models
lapply(models_ss, summary)
#plots - optional
#par(mfrow = c(4, 4))
#lapply(models_ss, plot)
#####################################

#####################################
#Time Ln(k+1)
#run linear models with Time Ln(k+1)
models_time_ln_k <- lapply(varlist, function(x) {
    lm(substitute(data$time_ln_k+1 ~ i, list(i = as.name(x))), data = data)
})
#look at models - optional
#models_time_ln_k
## apply summary to each model stored in the list, models
lapply(models_time_ln_k, summary)
#plots - optional
#par(mfrow = c(4, 4))
#lapply(models_time_ln_k, plot)
#####################################

#####################################
#Proportion of High Probability Options
#run linear models with proportion of high probability options
models_shiprob <- lapply(varlist, function(x) {
    lm(substitute(chosehiprob ~ i, list(i = as.name(x))), data = data)
})
#look at models - optional
#models_shiprob
## apply summary to each model stored in the list, models
lapply(models_shiprob, summary)
#plots - optional
#par(mfrow = c(4, 4))
#lapply(models_shiprob, plot)
#####################################

#####################################
#Time Ln(k+1)
#run linear models with Probability Ln(k+1)
models_prob_ln_k <- lapply(varlist, function(x) {
    lm(substitute(data$prob_ln_k+1 ~ i, list(i = as.name(x))), data = data)
})
#look at models - optional
#models
## apply summary to each model stored in the list, models
lapply(models_prob_ln_k, summary)
#plots - optional
#par(mfrow = c(4, 4))
#lapply(models_prob_ln_k, plot)
#####################################

#####################################
#Proportion of Low Effort Options
#run linear models with proportion of low effort options
models_sloeff <- lapply(varlist, function(x) {
    try(lm(substitute(choseloeffort ~ i, list(i = as.name(x))), data = data))
})
#look at models - optional
#models_sloeff
## apply summary to each model stored in the list, models
lapply(models_sloeff, summary)
#plots - optional
#par(mfrow = c(4, 4))
#lapply(models_sloeff, plot)
#####################################

#####################################
#Effort Ln(k+1)
#run linear models with Probability Ln(k+1)
models_effort_ln_k <- lapply(varlist, function(x) {
    lm(substitute(data$effort_ln_k+1 ~ i, list(i = as.name(x))), data = data)
})
#look at models - optional
#models
## apply summary to each model stored in the list, models
lapply(models_effort_ln_k, summary)
#plots - optional
#par(mfrow = c(4, 4))
#lapply(models_effort_ln_k, plot)
#####################################