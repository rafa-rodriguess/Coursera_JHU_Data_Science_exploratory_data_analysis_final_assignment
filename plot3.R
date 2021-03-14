options(warn=-1)
library(ggplot2)
library(hrbrthemes)
library(viridis)

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


# Of the four types of sources indicated by the 
# type (point, nonpoint, onroad, nonroad) 
# variable, which of these four sources have seen 
# decreases in emissions from 1999–2008 for Baltimore City?
# Which have seen increases in emissions from 
# 1999–2008? Use the ggplot2 plotting system to make
# a plot answer this question.

#View(NEI)
#View(SCC)



mysub <- subset(NEI, is.na(Emissions)==F & fips == "24510", select=c(year, type, Emissions))
mysub$year <- factor(mysub$year)
mysub$type <- factor(mysub$type)


df <- aggregate(mysub$Emissions, by=list(mysub$type ,mysub$year), FUN=sum, na.rm=TRUE)

png("plot3.png" , width=1040, height=480)

b<-ggplot(df, aes(x=Group.2, y=x, group=Group.1, color=Group.1)) + 
        geom_line() +
        geom_text(label=round(df$x, digit=2), nudge_y = 70, 
                  check_overlap = T) +
        scale_color_viridis(discrete = TRUE) +
        ggtitle("Question 03 - Baltimore City, Maryland Emissions by Year and Type") +
        theme_ipsum() +
        ylab("Emissions") +
        xlab("Year") +
        labs(colour = "Type")+
        guides(color = guide_legend(override.aes = list(size = 4) ) )
print(b)
dev.off()   