options(warn=-1)
library(lattice)
library(cdlTools)

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

# Across the United States, how have emissions from coal combustion-related 
# sources changed from 1999–2008?

# View(NEI)
# View(SCC)


subSCC <- SCC[grep("[c,C]oal", SCC$EI.Sector),1]
subNEI <- subset(NEI, SCC %in% subSCC)
subNEI["State.Name"] <- fips(as.numeric(substr(subNEI$fips,1,2)), to="Name")
gNEI <- subset(subNEI, is.na(State.Name)==F,select=c(State.Name, year, Emissions))
df <- aggregate(gNEI$Emissions, by=list(gNEI$State.Name ,gNEI$year), FUN=sum, na.rm=TRUE)
df <- transform(df, Group.2 = factor(Group.2 ), Group.1 = factor(Group.1 ))

png("plot4.png" , width=1040, height=480)
b<-xyplot(x ~ Group.2 | Group.1,data = df, panel = function(x, y, ... ) {
             panel.xyplot(x, y, ... ) 
             panel.lmline(x, y, col = 2) 
        })
print(b)
dev.off()