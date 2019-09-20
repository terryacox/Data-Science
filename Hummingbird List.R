library(chron)

# tct <- count.fields("~/Birds/Official/Hummingbird List.txt", sep=",", quote="")
# which(tct != 4)

hummer.df <- read.csv("~/Birds/Official/Hummingbird List.txt", header=TRUE, as.is=TRUE)

seen.df <- hummer.df[hummer.df$Location != "",]
str(seen.df)


attach(seen.df)
table(years(Date))
table(Location)

# s.year <- as.integer(as.character(years(Date)))
# seen.df[which(s.year==2006),]
# seen.df[which(Location=="Guam"),]
# s.name <- seen.df[grep("hemispingus",CommonName,ignore.case=T),]
write.table(seen.df,"~/Birds/seenHummers.txt", sep=",", row.names=FALSE, quote=FALSE)

detach(seen.df)
