thresholds <- function(
	data,
	frequency,
	threshold = 95,
   field="cpu_usage_rate_average"
) {
	# other recommended field is "mem_usage_absolute_average"
	resources <- sort(unique(data$resource_name))
	qq <- vector("list",length(resources))
	names <- c("field",80,95)
	qq <- as.data.frame(matrix(data=NA,nrow=length(resources),ncol=3,dimnames=list(resources,names)))
	for (resource in resources) {
		#print(resource)
		rrows <- nrow(data[data$resource_name ==resource,])
		if ( rrows > (frequency * 2) ) {
			myts <- ts(data[data$resource_name ==resource,][[field]], frequency=frequency)
			fitR <- stl(myts, s.window="periodic", robust=TRUE)
			fc <- forecast(fitR)
			one <- max(fc$upper[,1])
			two <- max(fc$upper[,2])
			onelevel <- paste("inside ",fc$level[1], "% confidence range", sep="")
			twolevel <- paste("inside ",fc$level[2], "% confidence range", sep="")
			warning <- ""
			if ( two > threshold ) {
				if ( one > threshold ) {
					warning <- paste(resource,"likely to surpass threshold of",threshold,onelevel)
				} else {
					warning <- paste(resource,"likely to surpass threshold of",threshold,twolevel)
				}
				print(warning)
			}
			qq[resource,] <- c(field, one, two)
		}
		else {
			qq[resource,] <- c(field, NA, NA)
		}
	}
	#names(qq) <- c("resource","field",one,two)
	return(qq)
}

allSparks <- function(
	data,
	max=36,
	frequency=180,
	bg = FALSE,
	forecast = FALSE,
	field="cpu_usage_rate_average"
) {
	resources <- sort(unique(data$resource_name))
	qq <- vector("list",length(resources))
	if ( frequency > 1 ) {
		for (resource in resources) {
			print(resource)
			rrows <- nrow(data[data$resource_name ==resource,])
			if ( rrows > (frequency * 2) ) {
				myts <- ts(data[data$resource_name ==resource,][[field]], frequency=frequency)
				fitR <- stl(myts, s.window="periodic", robust=TRUE)
				qq[[resource]] <- fitR$time.series[,"trend"]
			} else {
				#qq[[resource]] <- data[data$resource_name ==resource,]$cpu_usage_rate_average
				qq[[resource]] <- rep(-1, rrows)
			}
		}
	}
	else {
		for (resource in resources) {
			qq[[resource]] <- data[data$resource_name ==resource,][[field]]
		}
	}
	
	#columns <- 4
	gcols <- ceiling(min(sqrt(max)/2,sqrt(length(resources))/2))
	grows <- ceiling(min(max,length(resources))/gcols)

	par(mfrow=c(grows,gcols), #sets number of rows in space to number of cols in data frame x
	mar=c(1,0,0,0), #sets margin size for the figures
	oma=c(4,5,4,4)) #sets outer margin

	gCount <- 1;
	for (resource in resources){ # setup for statement to loops over all elements in a list or vector
		x <- as.data.frame(qq[resource])
		if ( max(x) == -1 ) { print(paste("skipping",resource,"for too few data points")); next; }
		if ( max(x) == min(x) ) { print(paste("skipping",resource,"for max == min")); next; }
		if ( gCount > max ) { break; }
		gCount <- gCount + 1
		x <- cbind(x,seq(1,nrow(x),1))
		#x <- cbind(x,data[data$resource_name ==resource,][["derived_memory_available"]])
		#names(x) <- c("y","x","mem")
		#print(head(x))
		names(x) <- c("y","x")
		if ( forecast == TRUE ) {
     		myts <- ts(data[data$resource_name ==resource,][[field]], frequency=frequency)
			plot(forecast(myts), col="grey",ylim=c(0,100),main="",axes=F)
			#lines(x=x$x,y=x$y,lwd=0.5,col="black",ylim=c(0,100))
		} else if ( bg == TRUE ) {
			plot(x=data[data$resource_name ==resource,][[field]],col="grey",type="l",lwd=0.5,axes=F,ylim=c(0,100))
			lines(x=x$x,y=x$y,lwd=0.5,col="black",ylim=c(0,100))
  			ymin<-min(x$y); tmin<-which.min(x$y);ymax<-max(x$y);tmax<-which.max(x$y);
  			points(x=c(tmin,tmax),y=c(ymin,ymax),pch=19,col=c("red","blue"),cex=1) # add coloured points at max and min
		} else { # no BG plots, no forecasting
	  		plot(y=x$y,x=x$x,
				col="grey",lwd=0.5, #make the line grey and thin
				axes=F,ylab="",xlab="",main="",type="l",ylim=c(0,100)); #suppress axes lines, set as line plot
	  		ymin<-min(x$y); tmin<-which.min(x$y);ymax<-max(x$y);tmax<-which.max(x$y);
	  		points(x=c(tmin,tmax),y=c(ymin,ymax),pch=19,col=c("red","blue"),cex=1) # add coloured points at max and min
		}
		title(main=resource, col.main="dark grey", line=-2, cex.main=1.0)
	  axis(2,yaxp=c(0,100,2),col="grey",tcl=0,labels=FALSE)  #y-axis: put a 2nd white axis line over the 1st y-axis to make it invisible
	  axis(1,xaxp=c(min(x$x),max(x$x),2),col="grey",tcl=0,labels=FALSE)  #y-axis: put a 2nd white axis line over the 1st y-axis to make it invisible
	  #ymin<-0; tmin<-which.min(x$y);ymax<-100;tmax<-which.max(x$y); # see the code from Jason below for what these do 
	}
  	#axis(1,pos=c(-5)) # places horizontal axis at the bottom of it all. 
}

getMetricRollups <- function(con) {
	
	tables <- c( 
	"metric_rollups_01", 
	"metric_rollups_02", 
	"metric_rollups_03", 
	"metric_rollups_04", 
	"metric_rollups_05", 
	"metric_rollups_06", 
	"metric_rollups_07", 
	"metric_rollups_08", 
	"metric_rollups_09", 
	"metric_rollups_10", 
	"metric_rollups_11", 
	"metric_rollups_12"
	) 
	
	select <- paste("select * from metric_rollups")
	print(select)
	res <- dbGetQuery(con, select)
	for (table in tables) {
		select <- paste("select * from", table)
		print(select)
		t <- dbGetQuery(con, select)
		if (nrow(t) > 0 ) {
			#res <- data.frame(res,t[!is.na(t$cpu_usage_rate_average),])
			res <- rbind(res,t[!is.na(t$cpu_usage_rate_average),])
		}
	}
	rm(t)
	
	res <- res[order(res$timestamp),]

	return(res)
}

CFDBOpen <- function(
	dbname = "vmdb_production",
	host = NA,
	port = NA,
	password = NA,
	user = NA
) {
	require(DBI)
	require(RPostgreSQL)

	drv <- dbDriver("PostgreSQL")
	
	if ( !exists("dbname") ) { print("enter dbname (default: tempdb):"); dbname<-readLines(file("stdin"),1) }
	if ( !exists("user") ) { print("enter username (default: current user name):");  user<-readLines(file("stdin"),1) }
	if ( !exists("password") ) { print("enter password (default NA):"); password<-readLines(file("stdin"),1) }
	if ( !exists("host") ) { print("enter host (default: localhost):"); host<-readLines(file("stdin"),1) }
	
	con <- dbConnect(drv,
		dbname=dbname,
		user=user,
		password=password,
		host=host,
		port=port
	)
	return(con)
}

getMetrics <- function(con) {
	
	tables <- c( 
	"metrics_01", 
	"metrics_02", 
	"metrics_03", 
	"metrics_04", 
	"metrics_05", 
	"metrics_06", 
	"metrics_07", 
	"metrics_08", 
	"metrics_09", 
	"metrics_10", 
	"metrics_11", 
	"metrics_12", 
	"metrics_13", 
	"metrics_14", 
	"metrics_15", 
	"metrics_16", 
	"metrics_17", 
	"metrics_18", 
	"metrics_19", 
	"metrics_20", 
	"metrics_21", 
	"metrics_22", 
	"metrics_23" 
	) 
	
	select <- paste("select * from metrics")
	print(select)
	res <- dbGetQuery(con, select)
	for (table in tables) {
		select <- paste("select * from", table)
		print(select)
		t <- dbGetQuery(con, select)
		if (nrow(t) > 0 ) {
			#res <- data.frame(res,t[!is.na(t$cpu_usage_rate_average),])
			res <- rbind(res,t[!is.na(t$cpu_usage_rate_average),])
		}
	}
	rm(t)
	
	res <- res[order(res$timestamp),]

	return(res)
}

pngStats <- function(data, prefix="", factors=c(1,24,24*7,24*7*28),bg=FALSE) {
	for (f in factors) {
		title <- paste(prefix, f, "intervals.png", sep="");
		png(title, height=800, width=600);
		allSparks(data=res,freq=f,bg=bg)
		dev.off();
	}
}
