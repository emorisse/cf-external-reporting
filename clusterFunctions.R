plot2DClusterAssociation <- function(res) {
	# create our data set for clustering
	rm(mydata)
	mydata <- data.frame(cpu=res$cpu_usage_rate_average, mem=res$mem_usage_absolute_average, net=res$net_usage_rate_average, disk=res$disk_usage_rate_average)
	
	# get rid of any NA's 
	mydata <- na.omit(mydata)
	
	# scale the data by the standard deviation of each set
	mydata <- as.data.frame(scale(mydata))
	
	# get the number of clusters
	require(fpc)
	pam <- pamk(mydata, usepam=F)
	
	# assign data by cluster
	fit <- kmeans(mydata, pam$nc)
	rm(pam)
	
	# draw the graph
	require(cluster)
	xlab <- paste( "sd(", colnames(mydata)[1], ")", sep="");
	ylab <- paste( "sd(", colnames(mydata)[2], ")", sep="");
	clusplot(mydata, fit$cluster,
			color=TRUE, shade=TRUE, labels=0, lines=0, cex=0.1, col.p="grey",
			main="Server Usage Groupings", xlab=xlab, ylab=ylab, sub=NA)
	# add lines through 0,0 (average cpu and mem utilization) for visual reference
	abline(0,0, lw=0.5, col="grey")
	abline(0,10000000000, lw=0.5, col="grey")
}

plotCPUMemCutoffs <- function( mydata, fit ) {
	require(arm)
	names(mydata) <- c("cpu", "mem")
	mydata$cluster <- fit$cluster
	fit.1 <- bayespolr(factor(cluster) ~ mem, data=mydata)
	sigma <- 1/fit.1$coefficients
	c1.5 <- fit.1$zeta[1]/fit.1$coefficients
	c2.5 <- fit.1$zeta[2]/fit.1$coefficients
	plot(mydata$mem,mydata$cluster, xlim=c(0,100), ylim=c(1,3), xlab="cpu usage", ylab="cluster")
	lines(rep(c1.5,2), c(1,2))
	lines(rep(c2.5,2), c(2,3))
	expected <- function(x, c1.5, c2.5, sigma){
		p1.5 <- invlogit((x-c1.5)/sigma)
		p2.5 <- invlogit((x-c2.5)/sigma)
		return((1*(1-p1.5)+2*(p1.5-p2.5)+3*p2.5))
	}
	curve(expected(x, c1.5, c2.5, sigma), add=T)
}

reorderClustersByCPU <- function ( mydata, field = "cpu" ) {
	formula <- paste(field, "~ cluster")
	cpu.order <- aggregate(as.formula(formula), FUN=median, data=mydata)
	shift <- max(mydata$cluster) + 10;
	cpu.order <- order(cpu.order[field])
	for (i in 1:length(cpu.order) ) {
		mydata$cluster[mydata$cluster == cpu.order[i]] <- i + shift
	}
	mydata$cluster <- mydata$cluster - shift
	return(mydata)
}


plotClusters <- function(mydata, sortby="cpu") {
	require(fpc)
	pam <- pamk(mydata, usepam=F)
	mydata <- as.data.frame(mydata)
	fit <- kmeans(mydata, pam$nc)
	mydata$cluster <- fit$cluster
	mydata <- reorderClustersByCPU(mydata, sortby)
	xmin <- floor(min(mydata$cpu, mydata$mem, mydata$net, mydata$disk))
	xmax <- ceiling(max(mydata$cpu, mydata$mem, mydata$net, mydata$disk))
	plot(mydata$cpu, jitter(mydata$cluster,factor=0.25)+0.225, cex=0.001, col='blue', xlim=c(xmin,xmax), ylim=c(0,pam$nc+1), ylab="cluster", xlab="% utilization", main=paste("clusters sorted by", sortby))
	points(mydata$mem, jitter(mydata$cluster,factor=0.25)+0.075, cex=0.001, col='red')
	points(mydata$net, jitter(mydata$cluster,factor=0.25)-0.075, cex=0.001, col='gray')
	points(mydata$disk, jitter(mydata$cluster,factor=0.25)-0.225, cex=0.001, col='green')
	legend("bottomright", title="resource", c("cpu","mem","net","disk"), fill=c("blue", "red", "gray", "green"), horiz=F)
	return(mydata)
}

plotClustersX <- function(mydata, sortby="cpu", update=FALSE, factor = 1, legend=TRUE, ...) {
	require(fpc)
	pam <- pamk(mydata, usepam=F)
	mydata <- as.data.frame(mydata)
	fit <- kmeans(mydata, pam$nc)
	mydata$cluster <- fit$cluster
	mydata <- reorderClustersByCPU(mydata, sortby)
	xmin <- floor(min(mydata$cpu, mydata$mem, mydata$net, mydata$disk))*factor
	xeax <- ceiling(max(mydata$cpu, mydata$mem, mydata$net, mydata$disk))*factor
	points(jitter(mydata$cluster,factor=0.25)+0.225, mydata$cpu*factor, cex=0.001, col='blue', ylim=c(xmin,xmax), xlim=c(0,pam$nc+1), xlab="cluster", ylab="% utilization", ...)
	points(jitter(mydata$cluster,factor=0.25)+0.075, mydata$mem*factor, cex=0.001, col='red')
	points(jitter(mydata$cluster,factor=0.25)-0.075, mydata$net*factor, cex=0.001, col='gray')
	points(jitter(mydata$cluster,factor=0.25)-0.225, mydata$disk*factor, cex=0.001, col='green')
	if ( legend ) legend("topleft", title="resource", rev(c("cpu","mem","net","disk")), fill=rev(c("blue", "red", "gray", "green")), horiz=T)
	if ( update ) return(mydata)
}

plotClustersXcentered <- function(mydata, sortby="cpu", update=FALSE, factor = 1, legend=TRUE, ...) {
	require(fpc)
	pam <- pamk(mydata, usepam=F)
	mydata <- as.data.frame(mydata)
	fit <- kmeans(mydata, pam$nc)
	mydata$cluster <- fit$cluster
	mydata <- reorderClustersByCPU(mydata, sortby)
	xmin <- floor(min(mydata$cpu, mydata$mem, mydata$net, mydata$disk))*factor
	xeax <- ceiling(max(mydata$cpu, mydata$mem, mydata$net, mydata$disk))*factor
	points(jitter(mydata$cluster+.5,factor=0.25)+0.225, mydata$cpu*factor, cex=0.001, col='blue', ylim=c(xmin,xmax), xlim=c(0,pam$nc+1), xlab="cluster", ylab="% utilization", ...)
	points(jitter(mydata$cluster+.5,factor=0.25)+0.075, mydata$mem*factor, cex=0.001, col='red')
	points(jitter(mydata$cluster+.5,factor=0.25)-0.075, mydata$net*factor, cex=0.001, col='gray')
	points(jitter(mydata$cluster+0.5,factor=0.25)-0.225, mydata$disk*factor, cex=0.001, col='green')
	if ( legend ) legend("topleft", title="resource", rev(c("cpu","mem","net","disk")), fill=rev(c("blue", "red", "gray", "green")), horiz=T)
	if ( update ) return(mydata)
}

plotClustersSpark <- function(mydata, sortby="cpu", update=FALSE) {
	require(fpc)
	pam <- pamk(mydata, usepam=F)
	mydata <- as.data.frame(mydata)
	fit <- kmeans(mydata, pam$nc)
	mydata$cluster <- fit$cluster
	mydata <- reorderClustersByCPU(mydata, sortby)
	xmin <- floor(min(mydata$cpu, mydata$mem, mydata$net, mydata$disk))
	xmax <- ceiling(max(mydata$cpu, mydata$mem, mydata$net, mydata$disk))
	par(mfrow=c(max(mydata$cluster),1))
	par(mar=c(1,0,0,0))
	for (c in sort(unique(mydata$cluster))) {
		plot(mydata$cpu[mydata$cluster==c], jitter(mydata$cluster[mydata$cluster==c],factor=0.25)+0.225, cex=0.001, col='blue', xlim=c(xmin,xmax), ylim=c(c-0.5,c+0.5), ylab="cluster", xlab="% utilization")
		points(mydata$mem[mydata$cluster==c], jitter(mydata$cluster[mydata$cluster==c],factor=0.25)+0.075, cex=0.001, col='red')
		points(mydata$net[mydata$cluster==c], jitter(mydata$cluster[mydata$cluster==c],factor=0.25)-0.075, cex=0.001, col='gray')
		points(mydata$disk[mydata$cluster==c], jitter(mydata$cluster[mydata$cluster==c],factor=0.25)-0.225, cex=0.001, col='green')
	}
	#legend("bottomright", title="resource", c("cpu","mem","net","disk"), fill=c("blue", "red", "gray", "green"), horiz=F)
	if ( update ) return(mydata)
}

plot4graph <- function(res) {
	alldata <- data.frame(cpu=res$cpu_usage_rate_average, mem=res$mem_usage_absolute_average, net=res$net_usage_rate_average, disk=res$disk_usage_rate_average, resource=res$resource_name)
	alldata <- na.omit(alldata)
	alldata$net <- alldata$net/max(alldata$net) * 100
	alldata$disk <- alldata$disk/max(alldata$disk) * 100
	#mydata <- scale(mydata)
	mydata <- alldata[,1:4]
	plotClusters(mydata, "mem")
}

transitions <- function(mydata) {
	plot <- 0
	colors <- rainbow(length(unique(mydata$resource)))
	mycolor <- 1
	ymax = max(mydata$cluster)
	ymin = min(mydata$cluster)
	for (r in sort(unique(mydata$resource)) ) {
		if ( plot == 0 ) { plot(jitter(mydata$cluster[mydata$resource==r]), col=colors[mycolor], ylim=c(ymin,ymax), cex=0.01) }
		else { points(jitter(mydata$cluster[mydata$resource==r]), col=colors[mycolor], cex=0.01) }
		plot <- 1
		mycolor <- mycolor + 1
	}
}

transitionhists <- function(mydata) {
	plot <- 0
	colors <- rainbow(length(unique(mydata$resource)))
	mycolor <- 1
	ymax = max(mydata$cluster)
	ymin = min(mydata$cluster)
	resources <- length(unique(mydata$resource))
	par(mfrow=c(floor(sqrt(resources)), ceiling(sqrt(resources))))
	par(mar=c(1,0,0,0))
	for (r in sort(unique(mydata$resource)) ) {
		hist(mydata$cluster[mydata$resource==r], main="", xlab="", ylab="", sub="", axes=F, labels=as.character(1:ymax), breaks=0:ymax, col.lab="gray")
      title(main=r, col.main="red", line=-2, cex.main=0.7)
	}
}
