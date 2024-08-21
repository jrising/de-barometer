tsdf <- read.csv("data/timeseries.csv")
tsdf$units <- "billion 2022$"
tsdf$units[tsdf$Variable == 'Total Employement'] <- 'thousands'

plot2label <- list(#'gdp'="Coastal Value Added [billion 2022$]",
    'inc'="Coastal Personal Income [billion 2022$]",
    'emp'="Coastal Employment [thousands]",
    'out'="Coastal Industry Output [billion 2022$]")

tbltwo2implan <- rbind(data.frame(group="Food Services and Drinking Places", implan=c(509, 510, 511)),
                       data.frame(group="Real Estate Rental and Management Establishments", implan=c(447, 448)),
                       data.frame(group="Hospitals, Nursing Homes, and Medical Care Facilities", implan=c(490, 491, 492)),
                       data.frame(group="State and Local Government other than Education", implan=529:534),
                       data.frame(group="Offices of Physicians, Dentists, and other Health Practitioners", implan=483:486),
                       data.frame(group="Retail Stores: Food and Beverage", implan=406),
                       data.frame(group="Retail Stores: Clothing and Clothing Accessories", implan=409),
                       data.frame(group="Employment Placement Services, Including Temp. Workers", implan=472),
                       data.frame(group="Business, Professional, Labor, Political, Civic, Social, and Homeowners Associations", implan=c(473, 495, 522, 523, 524)),
                       data.frame(group="Services to Buildings and Dwellings: janitorial, pest control, etc.", implan=c(60, 61, 476, 477, 479, 516)))

get.ts <- function(sec, lab) {
    if (is.na(sec) || sec == 'Full Economy')
        subset(tsdf, is.na(sector) & label == lab & !(year == 2018 & group == 'Extrapolated')) %>% arrange(year)
    else {
        multts <- subset(tsdf, !is.na(sector) & (sector %in% tbltwo2implan$implan[tbltwo2implan$group == sec]) & label == lab & !(year == 2018 & group == 'Extrapolated'))
        multts %>% group_by(label, year, units) %>% summarize(scaled=sum(scaled)) %>% arrange(year)
    }
}
