## setwd("~/research/coastal/barometer/rshiny")

library(dplyr)
library(readxl)

df <- read.csv("data/businesses.csv")
df$naics <- gsub("[/-]+", "", df$naics)

df.desc <- read_excel("data/2022_NAICS_Descriptions.xlsx")

## Replace 31-33, 44-45, and 48-49 with multiple rows
df.desc.3133 <- rbind(subset(df.desc, Code == '31-33'), subset(df.desc, Code == '31-33'), subset(df.desc, Code == '31-33'))
df.desc.3133$Code <- as.character(31:33)
df.desc.4445 <- rbind(subset(df.desc, Code == '44-45'), subset(df.desc, Code == '44-45'))
df.desc.4445$Code <- as.character(44:45)
df.desc.4849 <- rbind(subset(df.desc, Code == '48-49'), subset(df.desc, Code == '48-49'))
df.desc.4849$Code <- as.character(48:49)
df.desc <- rbind(subset(df.desc, !(Code %in% c('31-33', '44-45', '48-49'))),
                 df.desc.3133, df.desc.4445, df.desc.4849)
df.desc$Title[df.desc$Title == "Public AdministrationT"] <- "Government"

df.desc$Parent.Code <- sapply(df.desc$Code, function(ss) {
    substring(ss, 1, nchar(ss)-1)
})
df.desc2 <- df.desc %>% left_join(df.desc[, c('Code', 'Title')], by=c('Parent.Code'='Code'), suffix=c('', '.Parent'))

df.desc2$Display.Description <- sapply(df.desc2$Description, function(ss) ifelse(substring(ss, 1, 2) == "* ", substring(ss, 3, nchar(ss)), ss))

df2 <- df %>% left_join(df.desc, by=c('naics'='Code'))

df.out <- rbind(data.frame(Code=df.desc$Code[nchar(df.desc$Code) == 2], Sector=df.desc$Title[nchar(df.desc$Code) == 2], Parent='All',
                           Employment=0, Establishments=0),
                data.frame(Code=df.desc$Code[nchar(df.desc$Code) > 2], Sector=df.desc$Title[nchar(df.desc$Code) > 2],
                           Parent=df.desc2$Title.Parent[nchar(df.desc$Code) > 2], Employment=0, Establishments=0),
                data.frame(Code=df2$naics, Sector=paste0("[", df2$naics, "]"),
                           Parent=df2$Title, Employment=df2$emp.total, Establishments=df2$est.total))
df.out$Sector <- sapply(df.out$Sector, function(ss) ifelse(substring(ss, nchar(ss)) == "T", substring(ss, 1, nchar(ss)-1), ss))
df.out$Parent <- sapply(df.out$Parent, function(ss) ifelse(substring(ss, nchar(ss)) == "T", substring(ss, 1, nchar(ss)-1), ss))

df.out2 <- df.out %>% group_by(Sector) %>% summarize(count=length(Code), Parent=Parent[1], Employment=sum(Employment),
                                                     Establishments=sum(Establishments)) %>%
    filter(Establishments == 0 | Employment > 0) # For plot by employment

## Drop the extra Food Services and Drinking Places
df.out2 <- subset(df.out2, Sector != Parent)

## Drop parents with only 1 child
changed <- T
while (changed) {
    changed <- F
    allparents <- unique(df.out2$Parent[df.out2$Employment > 0])
    for (parent in allparents) {
        if (sum(df.out2$Parent == parent) == 1) {
            ## Change "frogs -> amph. -> animals" to "frogs -> animals"
            df.out2$Parent[df.out2$Parent == parent] <- df.out2$Parent[df.out2$Sector == parent]
            df.out2 <- subset(df.out2, Sector != parent)
            changed <- T
        }
    }
}

rows <- c("['Sector', 'Parent', 'Establishments', 'Employment Ratio'], ['All', null, 0, 0], ['Business, Professional, Labor, Political, Civic, Social, and Homeowners Associations', 'All', 0, 0], ['Services to Buildings and Dwellings: janitorial, pest control, etc.', 'All', 0, 0], ['State and Local Government other than Education', 'Government', 0, 0], ", paste0('["', df.out2$Sector, '", "', df.out2$Parent, '", ', df.out2$Establishments, ', ', df.out2$Employment, '],'))
boxdata <- rows[!duplicated(rows)]

keys <- c(paste0("I", df2$naics, ': "', df2$Title, '", '))
boxnames <- keys
