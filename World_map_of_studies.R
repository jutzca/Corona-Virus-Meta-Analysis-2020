####Create map of the countries, where the studies have been performed
##Created by C.Jutzeler, April 1st, 2020


#clear working space
rm(list = ls())

#Install packages if required
# if (!require(RColorBrewer)) install.packages("RColorBrewer")
# if (!require(maptools)) install.packages("maptools")
# if (!require(ggplot2)) install.packages("ggplot2")
# if (!require(mapproj)) install.packages("mapproj")
# if (!require(gpclib)) install.packages("gpclib", type="source")


library(RColorBrewer)
library(maptools)
library(ggplot2)
library(mapproj)
library(gpclib)

#Turn on the license
gpclibPermit()

#Load the world data
data(wrld_simpl)

#List countries and number of studies that have been performed
countries_of_interest = read.table(text="
                 country value
                 'Canada' 30
                  'China' 31
                  'France' 32
                  'Germany' 33
                  'Italy' 34
                  'Japan' 35
                 'Viet Nam' 36
                 'Scottland' 37
                 'Singapore' 38
                 'Korea, Republic of' 39
                'United States' 40", header=TRUE)


#Prepare color palette  
pal <- colorRampPalette(brewer.pal(11, 'Reds'))(length(countries_of_interest$value))
pal <- pal[with(countries_of_interest, findInterval(value, sort(unique(value))))]
  
col <- rep(grey(0.8), length(wrld_simpl@data$NAME))
col[match(countries_of_interest$country, wrld_simpl@data$NAME)] <- pal
  
plot(wrld_simpl, col = col)
  


# align colors to countries   
countries_of_interest$brk <- cut(countries_of_interest$value, 
                 breaks=c(0, sort(countries_of_interest$value)), 
                 labels=as.character(countries_of_interest[order(countries_of_interest$value),]$country),
                 include.lowest=TRUE)
  
# this lets us use the contry name vs 3-letter ISO
wrld_simpl@data$id <- wrld_simpl@data$NAME
  
wrld <- fortify(wrld_simpl, region="id")
wrld <- subset(wrld, id != "Antarctica") # we don't rly need Antarctica
  
corona_world_map <- ggplot()
  
# setup base map
corona_world_map <- corona_world_map + geom_map(data=wrld, map=wrld, aes(map_id=id, x=long, y=lat), fill="white", color="#7f7f7f", size=0.25)
  
# add our colored regions
corona_world_map <- corona_world_map + geom_map(data=countries_of_interest, map=wrld, aes(map_id=country, fill=brk),  color="white", size=0.25)
  
  # this sets the scale and, hence, the legend
  corona_world_map <- corona_world_map + scale_fill_manual(values=colorRampPalette(brewer.pal(9, 'Reds'))(length(countries_of_interest$value)), 
                               name="Country")
  
  # this gives us proper coords. mercator proj is default
  corona_world_map <- corona_world_map + coord_map()
  corona_world_map <- corona_world_map + labs(x="", y="")
  corona_world_map <- corona_world_map + theme(plot.background = element_rect(fill = "transparent", colour = NA),
                   panel.border = element_blank(),
                   panel.background = element_rect(fill = "transparent", colour = NA),
                   panel.grid = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank(),
                   legend.position = "right")
  corona_world_map
  
