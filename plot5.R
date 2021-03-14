options(warn=-1)
library(lattice)


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

#How have emissions from motor vehicle sources changed from 
#1999–2008 in Baltimore City?

# View(NEI)
# View(SCC)


subSCC <- SCC[grep("[M,m]obile", SCC$EI.Sector),1]
subNEI <- subset(NEI, SCC %in% subSCC)
mysub <- subset(subNEI, fips == "24510",select=c(year, Emissions))
mysub$year <- factor(mysub$year)

mysub.summary <- tapply(mysub$Emissions,mysub$year, sum)
png("plot5.png", width=1040, height=480)
b <- barplot(mysub.summary, main="Question 05 - Baltimore City Emissions motor vehicle by Year", xlab="Year", col = rainbow(20), axes=F,  ylim = c(0, max(mysub.summary) + mean(mysub.summary)))
text(x=b, y=mysub.summary, labels=paste(as.character(round(mysub.summary/1000, digits = 2)),"K"),pos=3)
print(b)
dev.off()
