### META ANALYSIS

# load libraries
library("foreign")
library("robumeta")
library("metafor")
library("dplyr")

# open data
data <- read.csv(file="./study02_data_sorted_whole_striatum.csv",header=TRUE, sep=",")

data <- escalc(measure="ZCOR", ri=r_adj, ni=N_integer, data=data, slab=paste(author, year, sep=", ")) 
#View(data)

# Mixed-effects analysis
res <- rma(yi, vi, mods = ~ factor(DA_status_dummy) ,data=data) 
res 
#show group coeficient estimates as Pearson correlation coefficients
predict(res, digits=3, transf=transf.ztor)
confint(res)

# Funnel plot
dev.off()
funnel(res, atransf=transf.ztor, xlab = "Correlation coefficient", digits=c(2,2), at=c(-1.832,-1.472,-0.973,-0.549,0,0.549,0.973), hlines="gray83")
axis(side=4, at=c(0,0.25,0.5,0.75,1))
dev.off()
funnel(res, atransf=transf.ztor,  digits=c(2,2),xlab = "Correlation coefficient")
funnel(res,  digits=c(2,2),xlab = "Correlation coefficient")
funnel(res,  level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), refline=0)
dev.copy2eps(file="meta_funnelplot_metafor_DA_staus_term.eps",width=7)


# Forestplot
dev.off()
par(mar=c(5,4,0,2))
forest(res, xlim=c(-2.6,2.6),alim=c(-1,1), transf=transf.ztor, 
       at=c( -1.0,-0.8,-0.6,-0.4,-.2,0,.2,.4,.6,.8,1.0),digits=c(2,1), cex=.7, pch=18,
       ilab = data$N_integer, ilab.xpos = 1.2,
       ylim=c(-1, 22))
text(-2.6, 21, "Author, Year", pos=4, cex=.7, font=2)
text( 2.6, 21, "Correlation [95% CI]", pos=2, cex=.7, font=2)
text( 1.2,21, "N", cex=0.7, font=2)
text(-2.6,22, "DA Function", cex=1, font=2, pos=4)
#text(-1.6, 34, pos=4, font=2, cex=0.7,c("Striatum"))
dev.print(postscript,"meta_forestplot_metafor_DA_status_term.eps", width = 7, horizontal=FALSE)