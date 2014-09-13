library(quantmod)
require(lubridate)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)

In2012 <- year(sampleTimes)==2012
samplesIn2012 <- sampleTimes[In2012]
print(length(samplesIn2012))

OnTuesIn2012 <- wday(samplesIn2012)==2
MondaysIn2012 <- samplesIn2012[OnTuesIn2012]
print(length(MondaysIn2012))