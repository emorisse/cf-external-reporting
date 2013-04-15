source("stats.R")
con <- CFDBOpen(
	host="fixme",
	user="fixme",
	password="fixme",
	database="vmdb_production"
)

# if you have production data, these are going to take a while
rollups <- getMetricRollups(con)
shorts <- getMetrics(con)

# the next two function calls get the field value within the the 80% and 95% 
# confidence intervals.  Confidence intervals are the "strength" of likelihood # a vaule with fall within a given range.  The 80% confidence interval is 
# the set of values expected to fall within the range 80% of the time.  It
# is a smaller range than the 95% interval, and should be considered
# more likely. E.g. if are going to hit your memory threshold within the 80% 
# interval, look to address those limits before those that only fall within
# the 95% interval.

# get chances of memory overload for 1 or minutes
# shorts has polling freq of 20 seconds,
# so freq of 3 in thresholds = 1 minute averages
mem <- thresholds(data=shorts,freq=3,field="mem_usage_absolute_average",threshold=95)

# get chances of cpu pegged for 8 hours or more on average
# rollups have period of 1 hour, so freq of 24 = 1 day average
cpu <- thesholds(data=rollups,freq=8,field="cpu_usage_rate_average",threshold=95)

# aside on fields: these are column names from the CF 2.0 db,
# cpu_usage_rate_average is the default field and does not need to be specified

# draw sparkline graphs of the trends
# graphs for the systems are shown for the first X systems (up to "max") with
# sufficient data to perform the analysis (# of data points > freqency * 2)
# and that have a range of data, e.g. min < max.
# red point = min, blue point = max
allSparks(data=rollups,max=36,freq=24,field="mem_usage_absolute_average")
