library(RCurl)
res <- getURL("https://api.bridgedataoutput.com/api/v2/zgecon/marketreport?access_token=d24ff6a75af15f08c6a26b5428e6db9e&metricTypeKey=zhvi&cutTypeKey=uc_sfrcondo&regionTypeID=7&regionID.in=99561,99580", .opts=list(followlocation = TRUE))
cat(res)
