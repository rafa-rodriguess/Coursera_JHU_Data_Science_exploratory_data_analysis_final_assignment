options(warn=-1)
downloadZipFile <- function(url, zipdest){
        #Download Zip File
        if (!file.exists(zipdest)) {
                download.file(url,zipdest)
                unzip(zipdest)
        }
}

downloadZipFile("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                "exdata_data_NEI_data.zip")

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


# Have total emissions from PM2.5 decreased in the United States 
# from 1999 to 2008? Using the base plotting system, make a plot 
# showing the total PM2.5 emission from all sources for each of 
# the years 1999, 2002, 2005, and 2008.

#View(NEI)
#View(SCC)

mysub <- subset(NEI, is.na(Emissions)==F, select=c(Emissions, year))
mysub$year <- factor(mysub$year)
mysub.summary <- tapply(mysub$Emissions,mysub$year, sum)

png("plot1.png" , width=1040, height=480)
b <- barplot(mysub.summary, main="Question 01 - Emissions by Year", xlab="Year", col = rainbow(20), axes=F,  ylim = c(0, max(mysub.summary) + mean(mysub.summary)))
text(x=b, y=mysub.summary, labels=paste(as.character(round(mysub.summary/1000000, digits = 2)),"M"),pos=3)
print(b)
dev.off()   






