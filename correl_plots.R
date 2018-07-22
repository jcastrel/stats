library("ggpubr")

#load data
data = read.csv(file="./study01_data.csv",header=TRUE, sep=",")

#output

setEPS()
postscript("panel_d2_correlations_colors.eps")
#png(filename = "panel_d2_correlations_colors.png", width=7, height=7, units="in", res=300)
#pdf("panel_d2_correlations.pdf", width=7, height=7)

#panel correlations
par(mfrow=c(3,3), tcl=-0.5, mai=c(0.6, 0.3, 0.3, 0.3), cex.lab=1.5)

#midbrain
midbrain = plot(data$midbrain, data$chosesnr, xlim=range(0,3), ylim=range(0, 1.1), pch=19, col='#7dd1b4', bty="n", 
	xlab = "Midbrain", ylab = "Discounting", font.x=c(10, "plain"),
	font.y=c(10, "plain"), font.legend=c(10, "plain"), font.tickslab = c(10, "plain"))
points(data$midbrain, data$chosehiprob, col='#da9ac0', pch=19)
points(data$midbrain, data$choseloeffort, col='#b7c856', pch=19)
abline(midbrain_time <- lm(data$chosesnr ~ data$midbrain, data = data), col='#7dd1b4')
abline(midbrain_prob <- lm(data$chosehiprob ~ data$midbrain, data = data), col='#da9ac0')
abline(midbrain_effort <- lm(data$choseloeffort ~ data$midbrain, data = data), col='#b7c856')

#accumbens_PVC
accumbens_PVC = plot(data$accumbens_PVC, data$chosesnr,  xlim=range(10,60), ylim=range(0, 1.1), pch=19, col='#7dd1b4', bty="n", 
	xlab = "Ventral Striatum", ylab = "Discounting", font.x=c(10, "plain"),
	font.y=c(10, "plain"), font.legend=c(10, "plain"), font.tickslab = c(10, "plain"))
points(data$accumbens_PVC, data$chosehiprob, col='#da9ac0', pch=19)
points(data$accumbens_PVC, data$choseloeffort, col='#b7c856', pch=19)
abline(accumbens_PVC_time <- lm(data$chosesnr ~ data$accumbens_PVC, data = data), col='#7dd1b4')
abline(accumbens_PVC_prob <- lm(data$chosehiprob ~ data$accumbens_PVC, data = data), col='#da9ac0')
abline(accumbens_PVC_effort <- lm(data$choseloeffort ~ data$accumbens_PVC, data = data), col='#b7c856')

#caudate_PVC
caudate_PVC = plot(data$caudate_PVC, data$chosesnr, xlim=range(0,50), ylim=range(0, 1.1), pch=19, col='#7dd1b4', bty="n",
	xlab = "Caudate", ylab = "Discounting", font.x=c(10, "plain"),
	font.y=c(10, "plain"), font.legend=c(10, "plain"), font.tickslab = c(10, "plain"))
points(data$caudate_PVC, data$chosehiprob, col='#da9ac0', pch=19)
points(data$caudate_PVC, data$choseloeffort, col='#b7c856', pch=19)
abline(caudate_PVC_time <- lm(data$chosesnr ~ data$caudate_PVC, data = data), col='#7dd1b4')
abline(caudate_PVC_prob <- lm(data$chosehiprob ~ data$caudate_PVC, data = data), col='#da9ac0')
abline(caudate_PVC_effort <- lm(data$choseloeffort ~ data$caudate_PVC, data = data), col='#b7c856')

#putamen_PVC
putamen_PVC = plot(data$putamen_PVC, data$chosesnr, xlim=range(10,50), ylim=range(0, 1.1), pch=19, col='#7dd1b4', bty="n",
	xlab = "Putamen", ylab = "Discounting", font.x=c(10, "plain"),
	font.y=c(10, "plain"), font.legend=c(10, "plain"), font.tickslab = c(10, "plain"))
points(data$putamen_PVC, data$chosehiprob, col='#da9ac0', pch=19)
points(data$putamen_PVC, data$choseloeffort, col='#b7c856', pch=19)
abline(putamen_PVC_time <- lm(data$chosesnr ~ data$putamen_PVC, data = data), col='#7dd1b4')
abline(putamen_PVC_prob <- lm(data$chosehiprob ~ data$putamen_PVC, data = data), col='#da9ac0')
abline(putamen_PVC_effort <- lm(data$choseloeffort ~ data$putamen_PVC, data = data), col='#b7c856')

#acc
acc_PVC = plot(data$acc_PVC, data$chosesnr, xlim=range(0,3), ylim=range(0, 1.1), pch=19, col='#7dd1b4', bty="n",
	xlab = "ACC", ylab = "Discounting", font.x=c(10, "plain"),
	font.y=c(10, "plain"), font.legend=c(10, "plain"), font.tickslab = c(10, "plain"))
points(data$acc_PVC, data$chosehiprob, col='#da9ac0', pch=19)
points(data$acc_PVC, data$choseloeffort, col='#b7c856', pch=19)
abline(acc_time <- lm(data$chosesnr ~ data$acc_PVC, data = data), col='#7dd1b4')
abline(acc_prob <- lm(data$chosehiprob ~ data$acc_PVC, data = data), col='#da9ac0')
abline(acc_effort <- lm(data$choseloeffort ~ data$acc_PVC, data = data), col='#b7c856')

#thalamus_PVC
thalamus_PVC = plot(data$thalamus_PVC, data$chosesnr, xlim=range(0,6), ylim=range(0, 1.1), pch=19, col='#7dd1b4', bty="n",
	xlab = "Thalamus", ylab = "Discounting", font.x=c(10, "plain"),
	font.y=c(10, "plain"), font.legend=c(10, "plain"), font.tickslab = c(10, "plain"))
points(data$thalamus_PVC, data$chosehiprob, col='#da9ac0', pch=19)
points(data$thalamus_PVC, data$choseloeffort, col='#b7c856', pch=19)
abline(thalamus_PVC_time <- lm(data$chosesnr ~ data$thalamus_PVC, data = data), col='#7dd1b4')
abline(thalamus_PVC_prob <- lm(data$chosehiprob ~ data$thalamus_PVC, data = data), col='#da9ac0')
abline(thalamus_PVC_effort <- lm(data$choseloeffort ~ data$thalamus_PVC, data = data), col='#b7c856')

#amygdala_PVC
amygdala_PVC = plot(data$amygdala_PVC, data$chosesnr, xlim=range(0,6), ylim=range(0, 1.1), pch=19, col='#7dd1b4', bty="n",
	xlab = "Amygdala", ylab = "Discounting", font.x=c(10, "plain"),
	font.y=c(10, "plain"), font.legend=c(10, "plain"), font.tickslab = c(10, "plain"))
points(data$amygdala_PVC, data$chosehiprob, col='#da9ac0', pch=19)
points(data$amygdala_PVC, data$choseloeffort, col='#b7c856', pch=19)
abline(amygdala_PVC_time <- lm(data$chosesnr ~ data$amygdala_PVC, data = data), col='#7dd1b4')
abline(amygdala_PVC_prob <- lm(data$chosehiprob ~ data$amygdala_PVC, data = data), col='#da9ac0')
abline(amygdala_PVC_effort <- lm(data$choseloeffort ~ data$amygdala_PVC, data = data), col='#b7c856')

#hippocampus_PVC
hippocampus_PVC = plot(data$hippocampus_PVC, data$chosesnr, xlim=range(0,4), ylim=range(0, 1.1), pch=19, col='#7dd1b4', bty="n",
	xlab = "Hippocampus", ylab = "Discounting", font.x=c(10, "plain"),
	font.y=c(10, "plain"), font.legend=c(10, "plain"), font.tickslab = c(10, "plain"))
points(data$hippocampus_PVC, data$chosehiprob, col='#da9ac0', pch=19)
points(data$hippocampus_PVC, data$choseloeffort, col='#b7c856', pch=19)
abline(hippocampus_PVC_time <- lm(data$chosesnr ~ data$hippocampus_PVC, data = data), col='#7dd1b4')
abline(hippocampus_PVC_prob <- lm(data$chosehiprob ~ data$hippocampus_PVC, data = data), col='#da9ac0')
abline(hippocampus_PVC_effort <- lm(data$choseloeffort ~ data$hippocampus_PVC, data = data), col='#b7c856')

#insula_PVC
insula_PVC = plot(data$insula_PVC, data$chosesnr, xlim=range(0,6), ylim=range(0, 1.1), pch=19, col='#7dd1b4', bty="n",
	xlab = "Insula", ylab = "Discounting", font.x=c(10, "plain"),
	font.y=c(10, "plain"), font.legend=c(10, "plain"), font.tickslab = c(10, "plain"))
points(data$insula_PVC, data$chosehiprob, col='#da9ac0', pch=19)
points(data$insula_PVC, data$choseloeffort, col='#b7c856', pch=19)
abline(insula_PVC_time <- lm(data$chosesnr ~ data$insula_PVC, data = data), col='#7dd1b4')
abline(insula_PVC_prob <- lm(data$chosehiprob ~ data$insula_PVC, data = data), col='#da9ac0')
abline(insula_PVC_effort <- lm(data$choseloeffort ~ data$insula_PVC, data = data), col='#b7c856')

dev.off()