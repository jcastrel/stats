#Run canonical correlation analysis on discounting rates & D2 BPND

library(ggplot2)
library(GGally)
library(CCA)
library(vegan)
library(CCP)


data <- read.csv(file="./study01_data.csv", header=TRUE, sep = ",")

#striatum
d2r <- data[24:26]

#striatum and midbrain
#d2r <- data[c(14,24:26)]


#other subcortical rois (amygdala, hippocampus, acc, thalamus, insula)
#d2r <- data[28:32]


#time, prob, effort - proportion
#discount <- data[c(33,36,39)]

#time & prob - proportion
#discount <- data[c(33,36)]


#time, prob, effort - proportion
discount <- data[c(35,38,41)]

#time & prob - proportion
#discount <- data[c(35,38)]





ggpairs(d2r)
ggpairs(discount)


matcor(d2r, discount)


cc1 <- cc(d2r, discount)
# display the canonical correlations
cc1$cor


cc2 <- comput(d2r, discount, cc1)




# tests of canonical dimensions
ev <- (1 - cc1$cor^2)

n <- dim(d2r)[1]
p <- length(d2r)
q <- length(discount)
k <- min(p, q)
m <- n - 3/2 - (p + q)/2

w <- rev(cumprod(rev(ev)))

# initialize
d1 <- d2 <- f <- vector("numeric", k)

for (i in 1:k) {
    s <- sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
    si <- 1/s
    d1[i] <- p * q
    d2[i] <- m * s - p * q/2 + 1
    r <- (1 - w[i]^si)/w[i]^si
    f[i] <- r * d2[i]/d1[i]
    p <- p - 1
    q <- q - 1
}

pv <- pf(f, d1, d2, lower.tail = FALSE)
(dmat <- cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv))





s1 <- diag(sqrt(diag(cov(d2r))))
s1 %*% cc1$xcoef


s2 <- diag(sqrt(diag(cov(discount))))
s2 %*% cc1$ycoef




#PLOTTING
res.cc = cc(d2r,discount)
plot(res.cc$cor,type="b")
plt.cc(res.cc)
plt.cc(res.cc,d1=1,d2=3,type="b",var.label=TRUE)



#MORE PLOTTING
cc3 <- cca(d2r, discount)
plot(cc3, scaling = 1)


correl <- matcor(d2r, discount)
img.matcor(correl, type = 2)


##NEW FUNCTIONS

p <- length(d2r)
q <- length(discount)
k <- min(p, q)

p.asym(cc1$cor, k, p, q, tstat = "Wilks")

