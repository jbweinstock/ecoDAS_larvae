## Combine + synthesize coastal OBIS data
## Date created: 23 Mar 2026
## Date updated: 30 Apr 2026


# code modified from: https://iobis.github.io/notebook-diversity-indicators/


library(arrow)
library(readr)
library(dplyr)
library(dggridR) # gridding map data
library(gsl) # diversity metrics
library(sf) # mapping
library(sfExtras) # polygon centroids
library(rnaturalearth) # mapping
library(rnaturalearthdata) # mapping
library(viridis)
library(scico)
library(ggplot2)
#library(rgdal)
library(worrms) # access WoRMS


# test <- open_dataset("OBIS_animals/",format = "csv") %>%
#   select(decimalLongitude, decimalLatitude, date_year) %>%
#   group_by(decimalLongitude, decimalLatitude) %>%
#   collect() %>%
#   summarize(min_year = min(date_year),max_year = max(date_year))


occ <- open_dataset("OBIS_animals/",format = "csv") %>%
  select(decimalLongitude, decimalLatitude, genus, species,basisOfRecord,
         occurrenceStatus,class,phylum,order,suborder,superfamily,organismQuantity) %>%
  collect() %>%
  filter_out(basisOfRecord == "FossilSpecimen" | 
               species == "NA" |
               class == "Copepoda" | #remove copepods (holoplankton)
               class == "Ostracoda" |  #remove ostracods (holoplankton)
               class == "Branchiopoda" | # (holoplankton)
               class == "Cephalopoda" |  #remove cephalopods
               genus == "Janthina" | #remove holoplanktonic snails
               order == "Mysida" | #remove mysids (holoplankton)
               order == "Euphausiacea" | #remove krill (holoplankton)
               suborder == "Hyperiidea" | #remove holoplanktonic amphipods
               order == "Pteropoda" |  #remove pteropods (holoplankton)
               order == "Salpida" |  #remove salps (holoplankton)
               order == "Siphonophorae" | #remove siphonophores (holoplankton)
               class == "Scyphozoa" |  #remove jellyfish
               class == "Cubozoa" |  #remove box jellyfish 
               phylum == "Ctenophora" |  #remove comb jellyfish
               class == "Appendicularia" |  #remove larvaceans (holoplankton)
               phylum == "Chaetognatha" |  #remove chaetognaths (holoplankton)
               phylum == "Chordata" |  #remove fish
               superfamily == "Pterotracheoidea" | # removes heteropods (holoplankton)
               is.na(species) |
               occurrenceStatus == "Ausent" | 
               occurrenceStatus == "Absent" | 
               occurrenceStatus == "absent" | 
               occurrenceStatus == "absence" | 
               occurrenceStatus == "Absence" |
               occurrenceStatus == "not observed" |
               occurrenceStatus == "Dead") %>%
  group_by(decimalLongitude, decimalLatitude, species) %>%
  collect() %>%
  summarize(records = n())

occ$decimalLongitude_shifted = occ$decimalLongitude + 180

spp_num = occ[!duplicated(occ$species),] # 58766 benthic spp

Chaudhary_df = read.csv("occurence_records.csv")
Chaudhary_df_spp = Chaudhary_df[!duplicated(Chaudhary_df$ValidName),]
Chaudhary_df_spp_small = as.data.frame(Chaudhary_df_spp$ValidName)
colnames(Chaudhary_df_spp_small) = c("ValidName")
Chaudhary_df_spp_small$ourclass = Chaudhary_df_spp$ourclass

test = merge(spp_num, Chaudhary_df_spp_small,
             by.x = "species", by.y = "ValidName", all.x=TRUE)

test_spp = test[!duplicated(test$species),]

# test_spp$status = NA
# for(i in 1:length(test_spp$status)){
#   tryCatch({
#     test_spp$status[i] = wm_records_name(test_spp$species[i],fuzzy=FALSE)$status
#   },
#   error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# }
#
#write.csv(test_spp,file="OBIS_animals/species_status.csv")

spp_status = read.csv("species_status.csv")

test_spp = merge(test_spp, spp_status,
                 by = "species", all.x=TRUE)
test_spp = test_spp[,-(2:3)]; test_spp = test_spp[,-(4:7)]; test_spp = test_spp[,-4]

spp_stat_NA = subset(test_spp, is.na(test_spp$status)==TRUE)

totals = test_spp %>%
  group_by(ourclass) %>%
  tally()

# for(i in 1:length(spp_stat_NA$status)){
#   tryCatch({
#     spp_stat_NA$status[i] = wm_records_name(spp_stat_NA$species[i],fuzzy=FALSE)$status
#   },
#   error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# }

test_sppp = merge(test_spp, spp_stat_NA,
                 by = "species", all.x=TRUE)

test_sppp$status = NA
test_sppp$status = ifelse(is.na(test_sppp$status.x) == TRUE,
                          yes = test_sppp$status.y,
                          no = test_sppp$status.x)

totals_new = test_sppp %>%
  group_by(status) %>%
  tally()

#write.csv(test_sppp, file = "species_status_v2.csv")

unaccepted_spp = subset(test_sppp, test_sppp$status != "accepted" & is.na(test_sppp$status) == FALSE)
NA_spp = subset(test_sppp, is.na(test_sppp$status) == TRUE)
#write.csv(NA_spp, file = "species_status_manual_v2.csv")

unaccepted_spp$accepted_name = NA
for(i in 1:length(unaccepted_spp$accepted_name)){
  worms = wm_records_name(unaccepted_spp$species[i],fuzzy=FALSE)
  
  if(length(worms$status) == 1){
    if(worms$status == unaccepted_spp$status[i] & 
       worms$scientificname == unaccepted_spp$species[i]){
      unaccepted_spp$accepted_name[i] = worms$valid_name
      }
    }
  if(length(worms$status) > 1){
    unaccepted_spp$accepted_name[i] = NA
  }
}
#write.csv(unaccepted_spp, file = "species_status_manual_unacc.csv")


# Create an ISEA discrete global grid using the dggridR package
dggs <- dgconstruct(projection = "ISEA", topology = "HEXAGON", res = 7)

#inf <- dginfo(dggs) #lists all possible resolutions

# assign cell numbers to the occurrence data -- replaced dgtransform with dgGEO_to_SEQNUM
occ$cell <- dgGEO_to_SEQNUM(dggs, occ$decimalLongitude,occ$decimalLatitude)$seqnum


# calculate the number of records, species richness, Simpson index, 
#   Shannon index, Hurlbert index (n = 50), and Hill numbers for each cell

calc <- function(df, esn = 50) {  #defaults to ES50
  t1 <- df %>%
    group_by(cell, species) %>%   #add up number of records for each species through time
    summarize(ni = sum(records))
  t2 <- t1 %>%
    group_by(cell) %>%   #calculate total number species and records / cell
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

#occ_noNA = subset(occ, occ$species != "NA")


metrics <- occ %>%
  calc(50)


# add cell geometries to the metrics table
grid <- dgearthgrid(dggs) #, frame = FALSE, wrapcells = FALSE)
grid_sf <- grid %>%
  st_as_sf() %>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=230"))
grid_sf$cell <- names(grid)

metrics <- merge(grid_sf, metrics, by.x = "seqnum", by.y = "cell") #%>%
#  filter(st_intersects(geometry, 
#                       st_as_sfc("SRID=4326;POLYGON((-180 85,180 85,180 -85,-180 -85,-180 85))"), 
#                       sparse = FALSE))

metrics$centroid_lon = NA
metrics$centroid_lat = NA
for(i in 1:length(metrics$seqnum)){
  #centroid = st_centroid(metrics[[12]][[i]])
  
  centroid = metrics$geometry[i] %>% 
    st_cast('MULTIPOLYGON') %>%
    st_cast('POLYGON') %>%
    as('Spatial') %>%
    centroid()
  
  metrics$centroid_lon[i] = centroid[1]
  metrics$centroid_lat[i] = centroid[2]
  
  if(metrics$centroid_lon[i] > 175 ){
    centroidd = metrics$geometry[i] %>% 
      st_cast('POLYGON') %>%
      st_shift_longitude() %>% 
      rmapshaper::ms_dissolve() %>% 
      as('Spatial') %>%
      centroid() 
 
    metrics$centroid_lon[i] = centroidd[1]
    metrics$centroid_lat[i] = centroidd[2]
  }
  if(metrics$centroid_lon[i] > 180){
    metrics$centroid_lon[i] = (360 - metrics$centroid_lon[i]) * (-1)
  }
  if(metrics$centroid_lon[i] <= 180){
    metrics$centroid_lon[i] = metrics$centroid_lon[i]
  }
}


ecoregions = read_sf("Ecoregions/01_Data/WCMC-036-MEOW-PPOW-2007-2012.shp") %>%
  sf::st_transform(4326) # because WGS84 is a good default
coastal_ecoregions = subset(ecoregions, ecoregions$TYPE == "MEOW")
coastal_ecoregions_small = coastal_ecoregions[,2]

points <- data.frame(x = metrics$centroid_lon,
                     y = metrics$centroid_lat) %>% 
  sf::st_as_sf(coords = c("x","y"), crs=4326) # transform to sf object & WGS84 CRS

sf_use_s2(FALSE) #throws error otherwise
metrics$MEOW = NA
metrics$MEOW_near = NA
metrics$MEOW = st_join(points, coastal_ecoregions_small,join=st_within)$REALM
metrics$MEOW_near = st_join(points, coastal_ecoregions_small,join=st_nearest_feature)$REALM


totals_new = metrics %>%
  group_by(MEOW_near) %>%
  tally()

metrics_csv = as.data.frame(metrics$n)
colnames(metrics_csv) = c("n")
metrics_csv$sp = metrics$sp
metrics_csv$ES50 = metrics$es
metrics_csv$shannon = metrics$shannon
metrics_csv$simpson = metrics$simpson
metrics_csv$maxp = metrics$maxp
metrics_csv$hill_1 = metrics$hill_1
metrics_csv$hill_2 = metrics$hill_2
metrics_csv$hill_inf = metrics$hill_inf
metrics_csv$centroid_lon = metrics$centroid_lon
metrics_csv$centroid_lat = metrics$centroid_lat
#write.csv(metrics_csv,"biodiversity_metrics_draft.csv")

#test2 <- merge(test, grid_sf, by.x = "cell", by.y = "seqnum") # %>%
 # filter(st_intersects(geometry, 
 #                      st_as_sfc("SRID=4326;POLYGON((-180 85,180 85,180 -85,-180 -85,-180 85))"), 
 #                      sparse = FALSE))

#test3 = subset(test2, test2$min_year < 1900)



## plotting ecoregion data to make sure it looks OK

sf_use_s2(TRUE)
world <- ne_countries(scale = "medium", returnclass = "sf")

# define a long & slim polygon that overlaps the meridian line & set its CRS to match that of world
polygon <- st_polygon(x = list(rbind(c(-0.0001, 90),
                                     c(0, 90),
                                     c(0, -90),
                                     c(-0.0001, -90),
                                     c(-0.0001, 90)))) %>%
  st_sfc() %>%
  st_set_crs(4326)

# modify world dataset to remove overlapping portions with world's polygons
world2 <- world %>% st_difference(polygon)

# perform transformation on modified version of world dataset
world_robinson <- st_transform(world2, 
                               crs = '+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')


ggplot() +
  geom_sf(data = world, fill = "#dddddd", color = "#666666", lwd = 0.1) +
  geom_sf(data = coastal_ecoregions,
          aes_string(fill = "REALM", color = "REALM", geometry = "geometry"), 
          lwd = 0.04) +
  theme_bw() + xlab("") + ylab("") +
  coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
#  coord_sf()

ggplot() +
  geom_sf(data = metrics, 
          aes_string(fill = "MEOW_near",geometry = "geometry"),
          lwd = 0.04,alpha=0.5) +
  geom_sf(data = world, fill = "#dddddd", color = "#666666", lwd = 0.1) +
  theme_bw() + xlab("") + ylab("") + 
  coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
#  coord_sf()

ggplot(metrics, aes(x=centroid_lat,y=es))+
  geom_point(aes(col=MEOW_near)) + geom_smooth() + theme_bw()


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
  geom_sf(data = world, fill = "#dddddd", color = NA) +
  geom_sf(data = metrics, aes_string(col = "shannon", geometry = "geometry"),fill=NA,lwd = 1) +
#  scale_fill_viridis_b(option = "inferno", begin = 0.1,#trans = "log10",
#                       na.value = "white", name = "ES50") +
  scale_colour_viridis_b(option = "inferno", begin = 0.1,#trans = "log10",
                         na.value = NA, name = "ES50") +
  geom_point(data = metrics, aes(x=centroid_lon,y=centroid_lat)) +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.background = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank()) + 
  xlab("") + ylab("") +
  coord_sf()

ggplot() +
  geom_sf(data = metrics, aes_string(fill = "es", col="es", geometry = "geometry"),lwd = 0.04) +
  scale_fill_viridis_b(option = "inferno", begin = 0.1,#trans = "log10",
                     na.value = "white", name = "ES50") +
  scale_colour_viridis_b(option = "inferno", begin = 0.1,#trans = "log10",
                     na.value = "white", name = "ES50") +
  geom_sf(data = world, fill = "#dddddd", color = "#666666", lwd = 0.1) +
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

plot(x=log(metrics$n), y=metrics$es)

ggplot(data = metrics, aes(x=centroid_lat,y=es,col=centroid_lon)) + 
  geom_point() + geom_smooth() + theme_bw() + 
  scale_color_scico(palette = "vikO")




ggplot() +
  geom_sf(data = metrics, aes_string(fill = "n", color = "n", geometry = "geometry"), lwd = 0.04) +
  scale_color_viridis(option = "inferno", na.value = "white", name = "Number of records", trans = "log10") +
  scale_fill_viridis(option = "inferno", na.value = "white", name = "Number of records", trans = "log10") +
  geom_sf(data = world, fill = "#dddddd", color = "#666666", lwd = 0.1) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(), 
    panel.background = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("") + ylab("") +
  coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" )




ggplot(data = metrics, aes_string(x = "n")) + 
  geom_histogram() + 
  scale_x_log10(breaks = c(50,200,1000,10000)) + 
  geom_vline(xintercept = 50) + 
  theme_bw() + 
  ggtitle("Distribution of records")



