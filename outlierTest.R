#bivariate outlier test for correlation between accumbens D2 and mPFC PE
library(aplpack)
library(car)
library(outliers)

data <- read.csv(file="./data.csv",header=TRUE, sep=",")

x <-data$accumbens_bpnd
y <-data$mPFC_PE
bagplot(cbind(x, y),pch=16,cex=2)


#cooks_distance
mod <- lm(y ~ x, data=data)
cooksd <- cooks.distance(mod)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance", xlim=c(0,30))  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""),col="red")  # add labels

#bivariate outlier test
outlierTest(lm(y~x))

#univariate outlier test
outlier(x, opposite = TRUE, logical = FALSE)
outlier(y, opposite = TRUE, logical = FALSE)


#bivariate heterskedasticity
library(gvlma)
gvmodel <- gvlma(mod) 
summary(mod)