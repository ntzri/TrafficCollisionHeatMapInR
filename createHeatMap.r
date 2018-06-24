library(ggplot2)
library(ggmap)
library(dplyr)
library(lubridate)
library(tidyr)
losangeles_map <- get_map(location="Hollywood", zoom=11)
heatmapFunction <- function(){
    count = 0
    for(year in 2010:2018){
        fromdate = paste0(year, "-", month, "-01")
        todate = paste0(year, "-", month+1, "-01")
        if(month==12){
            todate = paste0(year+1, "-01-01")
        }
        for(month in 1:12){
            count = count+1
            imageFilename = paste0(count, "_", month(as.Date(fromdate), label=TRUE, abbr=FALSE), year(as.Date(fromdate)))
            imagelabel = paste0(month(as.Date(fromdate), label=TRUE, abbr=FALSE), " ", year(as.Date(fromdate)))
            collisionsdata <- collisions %>% filter(`Date Occurred` >= fromdate, `Date Occurred` < todate) %>% extract(Location, c("Latitude", "Longitude"), "\\(([^,]+), ([^)]+)\\)")
chart <- ggmap(ggmap=losangeles_map)
            chart <- chart+geom_density2d(data=collisionsdata, aes(x = as.numeric(Longitude), y = as.numeric(Latitude)), size = .2)
            chart <- chart+stat_density2d(data=collisionsdata, aes(x=as.numeric(Longitude), y=as.numeric(Latitude), fill=..level.., alpha=..level..),geom="polygon", size=1, bins=22)
            chart <- chart+scale_alpha(range = c(.01, 0.2),guide=FALSE)
            chart <- chart+scale_fill_gradient(low = "blue", high = "orange")
            chart <- chart+theme_void()
            chart <- chart+geom_label(aes(x=-118.2, y=34.2), cex=10, label.padding=unit(.5, "lines"), label=imagelabel ,color="black", fill="white")
            png(filename=paste0("forgif/",imageFilename,".png"), width=665, height=500)
            plot(chart)
            dev.off()
        }
    }
}
