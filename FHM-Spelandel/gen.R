require(gdata)

# Read source data into data.frame

df.xls <- read.xls('http://www.folkhalsomyndigheten.se/documents/statistik-uppfoljning/enkater-undersokningar/nationella-folkhalsoenkaten/resultat-2015/spelvanor-regionala-resultat-2015.xls', sheet=3, header=FALSE)

df <- data.frame()
for (year.pos in seq(4, 318, by=35)) {
  df.this <- df.xls[(year.pos+3):(year.pos+23), c(1, 10)]
  names(df.this) <- c('region', 'p')
  
  df.this$region <- iconv(df.this$region, 'Latin1', 'UTF-8')
  df.this$p <- as.numeric(as.character(df.this$p))/100
  df.this$year <- df.xls[year.pos, 1]
  
  df <- rbind(df, df.this)
}


# Read GIS data
library(rgdal)
sweden <- readOGR('region-shapefile', layer="l") # Fetched, unpacked and renamed from http://www.arcgis.com/home/item.html?id=912b806e3b864b5f83596575a2f7cb01
sweden@data$id = rownames(sweden@data)
sweden.points <- fortify(sweden)



# Join GIS and source data
library(plyr)
regions <- c('Stockholm', 'Uppsala', 'Södermanland', 'Östergötland', 'Jönköping', 'Kronoberg', 'Kalmar', 'Gotland', 'Blekinge', 'Skåne', 'Halland', 'Västra Götaland', 'Värmland', 'Örebro', 'Västmanland', 'Dalarna', 'Gävleborg', 'Västernorrland', 'Jämtland', 'Västerbotten', 'Norrbotten')
df$id <- match(df$region, regions) - 1
sweden.df <- join(join(sweden.points, sweden@data, by='id'), df, by='id')



# Plot
library(ggplot2)
library(scales)
library(RColorBrewer)

ggplot(subset(sweden.df, year %in% c("2004-2007", "2006-2009", "2008-2011", "2010-2013", "2012-2015")), aes(long, lat, group=id, fill=p)) +
  geom_polygon(color="black", size=0.05) +
  scale_fill_gradientn(name="\nHar någon gång under de 12 senaste månaderna\nköpt lotter eller satsat pengar på spel", colours=rev(brewer.pal(7,"PuOr")), labels=percent) +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=NULL) +
  coord_equal() +
  labs(x="", y="", title="") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()) +
  facet_wrap( ~ year, ncol=5) +
  theme(strip.text.x = element_text(hjust=0.8, size=14, vjust=-100),
        legend.text = element_text(size=10),
        legend.title = element_text(size=14),
        legend.position="bottom") +
  guides(fill = guide_colorbar(reverse=TRUE, title.position="bottom", title.hjust=0.5, barwidth=unit(15, "cm"), barheight=unit(3, "mm")))

