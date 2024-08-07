## setwd("~/research/coastal/barometer/rshiny")

library(dplyr)
library(readxl)

load.data <- function(suffix) {
    df.bridge <- read_excel("data/Bridge_2017NaicsToImplan546.xlsx")
    df.bridge2 <- rbind(df.bridge, tibble(Implan546Index=529:531, Implan546Description="State Government other than Education",
                                          `2017NaicsCode`=999200, NaicsDescription="State Government, excluding schools and hospitals (OES Designation)",
                                          CewEmpRatio=1, CewWagesRatio=1, CewAvgRatio=1),
                        tibble(Implan546Index=532:534, Implan546Description="Local Government other than Education",
                               `2017NaicsCode`=999300, NaicsDescription="Local Government, excluding schools and hospitals (OES Designation)",
                               CewEmpRatio=1, CewWagesRatio=1, CewAvgRatio=1))

    grouping <- rbind(data.frame(group="Food Services and Drinking Places", implan=c(509, 510, 511)),
                      data.frame(group="Real Estate Rental and Management", implan=c(447, 448)),
                      data.frame(group="Hospitals, Nursing Homes, and Medical Care Facilities", implan=c(490, 491, 492)),
                      data.frame(group="State and Local Government other than Education", implan=529:534),
                      data.frame(group="Offices of Physicians, Dentists, and other Health Practitioners", implan=483:486),
                      data.frame(group="Retail Stores: Food and Beverage", implan=406),
                      data.frame(group="Retail Stores: Clothing and Clothing Accessories", implan=409),
                      data.frame(group="Employment Placement Services, Including Temp. Workers", implan=472),
                      data.frame(group="Business, Professional, Labor, Political, Civic, Social, and Homeowners Associations", implan=c(473, 495, 522, 523, 524)),
                      data.frame(group="Services to Buildings and Dwellings: janitorial, pest control, etc.", implan=c(60, 61, 476, 477, 479, 516)))

    econinfo.cbp <- read.csv(paste0("data/businesses", suffix, ".csv"))
    econinfo.cbp$naics <- gsub("[/-]", '0', econinfo.cbp$naics)
    econinfo.cbp <- econinfo.cbp[!duplicated(econinfo.cbp), ]

    econinfo.oews <- read.csv(paste0("data/wages", suffix, ".csv"))
    econinfo <- data.frame(naics=unique(c(econinfo.cbp$naics, econinfo.oews$NAICS[!grepl("-", econinfo.oews$NAICS)]))) %>%
        left_join(econinfo.cbp) %>% left_join(subset(econinfo.oews, !grepl("-", NAICS)), by=c('naics'='NAICS'))

    df.bridge3 <- df.bridge2 %>% left_join(grouping, by=c('Implan546Index'='implan'))
    df.bridge4 <- df.bridge3 %>% filter(!is.na(group)) %>% group_by(`2017NaicsCode`, group) %>%
        summarize(Description=Implan546Description[1])
    ## Checked and there are no cases with both a sector and its parent
    df.bridge4$`2017NaicsCode` <- as.character(df.bridge4$`2017NaicsCode`)
    econinfo2 <- econinfo %>% left_join(df.bridge4, by=c('naics'='2017NaicsCode'))
    econinfo2$group[econinfo2$naics == '000000'] <- "Full Economy"

    econinfo3 <- econinfo2 %>% filter(!is.na(group)) %>% group_by(group) %>% summarize(A_MEAN=weighted.mean(A_MEAN, ifelse(!is.na(emp.total), emp.total, 10), na.rm=T), est.total=sum(est.total, na.rm=T), emp.total=sum(emp.total, na.rm=T))
    econinfo3$est.total[econinfo3$group == "State and Local Government other than Education"] <- NA
    econinfo3$emp.total[econinfo3$group == "State and Local Government other than Education"] <- NA

    econinfo3
}

baseline3 <- load.data("-2022")

results <- read.csv("data/tables34.csv")
results2 <- results %>% left_join(baseline3, by=c('Sector'='group'))

## All of these show great estimates just based on establishments
mod.jobs <- lm(Direct.Jobs ~ est.total + emp.total, data=results2)
mod.income <- lm(Direct.Income ~ est.total + emp.total, data=results2)
mod.added <- lm(Direct.Added ~ est.total + emp.total, data=results2)
mod.output <- lm(Direct.Output ~ est.total + emp.total, data=results2)

mod.jobstot <- lm(Total.Jobs ~ est.total + emp.total, data=results2)
mod.incometot <- lm(Total.Income ~ est.total + emp.total, data=results2)
mod.addedtot <- lm(Total.Added ~ est.total + emp.total, data=results2)
mod.outputtot <- lm(Total.Output ~ est.total + emp.total, data=results2)

if (F) {
    ## All these show miserable estimates
    summary(lm(Direct.Jobs ~ A_MEAN, data=results2))
    summary(lm(Direct.Income ~ A_MEAN, data=results2))
    summary(lm(Direct.Added ~ A_MEAN, data=results2))
    summary(lm(Direct.Output ~ A_MEAN, data=results2))
}

econinfo3 <- load.data("")
newres2 <- results %>% left_join(econinfo3, by=c('Sector'='group'))

pred.value <- function(mod, newres2) {
    preds <- predict(mod, newres2)
    preds[-mod$na.action] <- preds[-mod$na.action] + mod$resid
    preds
}

newres2$Direct.Jobs <- pred.value(mod.jobs, newres2)
newres2$Direct.Income <- pred.value(mod.income, newres2)
newres2$Direct.Added <- pred.value(mod.added, newres2)
newres2$Direct.Output <- pred.value(mod.output, newres2)

newres2$Total.Jobs <- pred.value(mod.jobstot, newres2)
newres2$Total.Income <- pred.value(mod.incometot, newres2)
newres2$Total.Added <- pred.value(mod.addedtot, newres2)
newres2$Total.Output <- pred.value(mod.outputtot, newres2)
