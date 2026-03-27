## Combine + synthesize coastal OBIS data
## Date created: 23 Mar 2026
## Date updated: 26 Mar 2026


# code modified from: https://iobis.github.io/notebook-diversity-indicators/


library(arrow)
library(readr)
library(dplyr)
library(dggridR) # gridding map data
library(gsl) # diversity metrics
library(sf) # mapping
library(rnaturalearth) # mapping
library(rnaturalearthdata) # mapping
library(viridis)
library(ggplot2)
library(rgdal)


test <- open_dataset("OBIS_animals/",format = "csv") %>%
  select(decimalLongitude, decimalLatitude, date_year) %>%
  group_by(decimalLongitude, decimalLatitude) %>%
  collect() %>%
  summarize(min_year = min(date_year),max_year = max(date_year))


occ <- open_dataset("OBIS_animals/",format = "csv") %>%
  select(decimalLongitude, decimalLatitude, species) %>%
  group_by(decimalLongitude, decimalLatitude, species) %>%
  collect() %>%
  summarize(records = n())

# Create an ISEA discrete global grid using the dggridR package
dggs <- dgconstruct(projection = "ISEA", topology = "HEXAGON", res = 8)

#inf <- dginfo(dggs) #lists all possible resolutions

# assign cell numbers to the occurrence data -- replaced dgtransform with dgGEO_to_SEQNUM
occ$cell <- dgGEO_to_SEQNUM(dggs, occ$decimalLongitude,occ$decimalLatitude)$seqnum
test$cell <- dgGEO_to_SEQNUM(dggs,test$decimalLongitude,test$decimalLatitude)$seqnum


# calculate the number of records, species richness, Simpson index, 
#   Shannon index, Hurlbert index (n = 50), and Hill numbers for each cell

calc <- function(df, esn = 50) {
  t1 <- df %>%
    group_by(cell, species) %>%
    summarize(ni = sum(records))
  t2 <- t1 %>%
    group_by(cell) %>%
    mutate(n = sum(ni))
  t3 <- t2 %>%
    group_by(cell, species) %>%
    mutate(
      hi = -(ni/n*log(ni/n)),
      si = (ni/n)^2,
      qi = ni/n,
      esi = case_when(
        n-ni >= esn ~ 1-exp(lngamma(n-ni+1)+lngamma(n-esn+1)-lngamma(n-ni-esn+1)-lngamma(n+1)),
        n >= esn ~ 1
      )
    )
  t4 <- t3 %>%
    group_by(cell) %>%
    summarize(
      n = sum(ni),
      sp = n(),
      shannon = sum(hi), 
      simpson = sum(si),
      maxp = max(qi),
      es = sum(esi)
    )
  result <- t4 %>%
    mutate(
      hill_1 = exp(shannon), # Hill-Shannon index
      hill_2 = 1/simpson, # Hill-Simpson
      hill_inf = 1/maxp
    )
  return(result)
}

# Perform the calculation on species level data
#psystime = function(e){system.time(eval(e))}

occ_noNA = subset(occ, occ$species != "NA")


metrics <- occ_noNA %>%
  calc(50)

test <- test %>%
  group_by(cell) %>%
  summarize(min_year = min(min_year),max_year = max(max_year))

# add cell geometries to the metrics table
grid <- dgearthgrid(dggs) #, frame = FALSE, wrapcells = FALSE)
grid_sf <- grid %>%
  st_as_sf() %>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=230"))
grid_sf$cell <- names(grid)

metrics <- merge(grid_sf, metrics, by.x = "seqnum", by.y = "cell")# %>%
 # filter(st_intersects(geometry, 
  #                     st_as_sfc("SRID=4326;POLYGON((-180 85,180 85,180 -85,-180 -85,-180 85))"), 
   #                    sparse = FALSE))

test2 <- merge(test, grid_sf, by.x = "cell", by.y = "seqnum") # %>%
 # filter(st_intersects(geometry, 
 #                      st_as_sfc("SRID=4326;POLYGON((-180 85,180 85,180 -85,-180 -85,-180 85))"), 
 #                      sparse = FALSE))

test3 = subset(test2, test2$min_year < 1900)

world <- ne_countries(scale = "medium", returnclass = "sf")

# create a bounding box for the Robinson projection
robinson <- CRS("+proj=robin +over")
bb <- sf::st_union(sf::st_make_grid(
  st_bbox(c(xmin = -180,
            xmax = 180,
            ymax = 90,
            ymin = -90), crs = st_crs(4326)),
  n = 100))
bb_robinson <- st_transform(bb, as.character(robinson))

ggplot() +
  geom_sf(data = test3, aes_string(fill = "min_year", geometry = "geometry"), lwd = 0,col=NA) +
  scale_fill_viridis(option = "inferno", na.value = "white", 
                     name = "min year") +
                     #breaks=c(1800,1900,1950,1975,1990,2000)) +
  geom_sf(data = world, fill = "#dddddd", color = NA) +
  theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), panel.background = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()) + xlab("") + ylab("") +
  coord_sf()

ggplot() +
  geom_sf(data = metrics, aes_string(fill = "shannon", geometry = "geometry"),lwd = 0,col=NA) +
  scale_fill_viridis(option = "inferno", begin = 0.1,#trans = "log10",
                     na.value = "white", name = "Shannon index") +
  geom_sf(data = world, fill = "black", color = NA) +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(color = "black",
                                  #linewidth = 0.005,
                                  linetype = 1),
        #panel.border = element_rect(colour = "black", fill=NA, linewidth=5),
        axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.ticks = element_blank(), axis.title.x = element_blank(), 
        axis.title.y = element_blank()) + xlab("") + ylab("") +
 # coord_sf()
 coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")


