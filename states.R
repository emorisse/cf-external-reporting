require(lme4)
require(arm)
pool <- lm(cpu_usage_rate_average ~ mem_usage_absolute_average, data=res)
nopool <- lm(cpu_usage_rate_average ~ mem_usage_absolute_average + factor(resource_name) -1, data=res)
M0 <- lmer(cpu_usage_rate_average ~ mem_usage_absolute_average + ( 1 + mem_usage_absolute_average | resource_name ), data=res)
a.hat.M1 <- coef(M0)$resource_name[,1]
b.hat.M1 <- coef(M0)$resource_name[,2]
names(a.hat.M1) <- rownames(coef(M0)$resource_name)
names(b.hat.M1) <- rownames(coef(M0)$resource_name)
#x.jitter <- res$mem_usage_absolute_average + runif(length(res$mem_usage_absolute_average), -0.05, 0.05)
par(mfrow=c(9,6), mar=rep(.1,4))
for (j in sort(unique(res$resource_name))) {
	#plot(x.jitter[res$resource_name==j], res$cpu_usage_rate_average[res$resource_name==j],
	plot(res$mem_usage_absolute_average[res$resource_name==j], res$cpu_usage_rate_average[res$resource_name==j],
		xlim=c(min(res$mem_usage_absolute_average)-.05, max(res$mem_usage_absolute_average)+0.05),
		ylim=range(res$cpu_usage_rate_average), main="", cex=0.5, axes=F
	)
	if ( FALSE ) {
		net <- log(res$net_usage_rate_average[res$resource_name == j]+1)
		net <- net/max(net) * 100
		points(net, res$cpu_usage_rate_average[res$resource_name==j],
			xlim=c(min(res$mem_usage_absolute_average)-.05, max(res$mem_usage_absolute_average)+0.05),
			ylim=range(res$cpu_usage_rate_average), main="", cex=0.5, col="green", axes=F
		)
		disk <- log(res$disk_usage_rate_average[res$resource_name == j]+1)
		disk <- disk/max(disk) * 100
		points(disk, res$cpu_usage_rate_average[res$resource_name==j],
			xlim=c(min(res$mem_usage_absolute_average)-.05, max(res$mem_usage_absolute_average)+0.05),
			ylim=range(res$cpu_usage_rate_average), main="", cex=0.5, col="blue"
		)
	}
	title(main=j, line=-2, cex.main=1.0)
	curve(coef(pool)[1] + coef(pool)[2]*x, lty=2, col="dark gray", add=T)
	curve(coef(nopool)[paste("factor(resource_name)", j, sep="")] + coef(nopool)[1]*x, lty=2, col="blue", add=T)
	curve(a.hat.M1[j] + b.hat.M1[j]*x, lwd=1, col="black", add=T)
}
