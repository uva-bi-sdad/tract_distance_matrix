library(gmapsdistance)
library(sf)
library(tigris)
library(mapboxapi)
library(hereR)
library(osrm)
library(data.table)

gmapsdistance::set.api.key(Sys.getenv("GOOGLE_MAPS_API_KEY"))
hereR::set_key("iefIVbBb8Y2R574zTGyYhpY0FrYPuc1R4GrBqj1oQ2w")
mapboxapi::mb_access_token("pk.eyJ1IjoiZGFkczJidXN5IiwiYSI6ImNsd2t2czVvNjA5MWgycm83ajZ4cHc5bWEifQ.NebhQtWQRjT6SeZ5OY9r7Q")


va_tracts_2020 <- tigris::tracts(state = "VA", year = 2020, cb = T)

va_tracts_centroids_2020 <- sf::st_centroid(va_tracts_2020)

va_tracts_centroids_2020$latlons <- paste0(st_coordinates(va_tracts_centroids_2020$geometry)[, "Y"], "+", st_coordinates(va_tracts_centroids_2020$geometry)[, "X"])


gridll <- expand.grid(va_tracts_centroids_2020$latlons, va_tracts_centroids_2020$latlons)

gridll2 <- gridll[!duplicated(lapply(as.data.frame(t(gridll), stringsAsFactors=FALSE), sort)),]

gridll2 <- gridll2[gridll2$Var1!=gridll2$Var2,]

write.csv(gridll2, "data/va_unique_lonlat_combos.csv", row.names = F)



# Google
od <- read.csv("data/va_unique_lonlat_combos.csv")

x <-  58001
y <-  59000
for (j in 1:10) {
  if (exists("dist_fnl")) rm(dist_fnl)
  for (i in x:y) {
    distances <- gmapsdistance::gmapsdistance(origin = od[i,1], destination = od[i,2], shape = "long", combinations = "pairwise")
    ret <- data.table(origin = od[i,1], dest = od[i,2], dist = distances$Distance, time = distances$Time)
    if (!exists("dist_fnl")) dist_fnl <- ret else dist_fnl <- rbindlist(list(dist_fnl, ret))
  }
  
  write.csv(dist_fnl, paste0("data/tract_distances_", x,"_", y,".csv"), row.names = F)
  
  x <-  x + 1000
  y <-  y + 1000
  
  gc()
}



# distances <- gmapsdistance::gmapsdistance(origin = od[1:10,1], destination = od[1:10,2], shape = "long", combinations = "pairwise")






tracts <- va_tracts_centroids_2020[, c("GEOID", "latlons")]
m1 <- merge(d, tracts, by.x = "or", by.y = "latlons")
m2 <- merge(m1, tracts, by.x = "de", by.y = "latlons")

m3 <- m2[, c("GEOID.x", "GEOID.y", "Distance")]
colnames(m3) <- c("Tract_1", "Tract_2", "Distance_m")
m3

write_csv(m3, "data/tract_distances_1_100.csv")




# mapbox
mroute <- mapboxapi::mb_directions(input_data = stf[1:2,],
                                   profile = "driving", access_token = "pk.eyJ1IjoiZGFkczJidXN5IiwiYSI6ImNsd2t1bXBpMjA4eXIya3BiMGRncGdodnAifQ.RdVdnPSi3oBszzl9hpXUNQ")

mmatrix <- mb_matrix(origins = stf[1:4,], output = "distance", access_token = "pk.eyJ1IjoiZGFkczJidXN5IiwiYSI6ImNsd2t1bXBpMjA4eXIya3BiMGRncGdodnAifQ.RdVdnPSi3oBszzl9hpXUNQ")

summary(mmatrix)
mapboxapi::

# here
sf <- st_as_sf(address_closest_to_tract_centroid, )
stf <- st_as_sf(address_closest_to_tract_centroid, coords = c("X5", "X4"), crs = 4326)

hroute <- hereR::route(origin = stf[1:10, ],
                       destination =  stf[1:10, ],
                       transport_mode = "car") %>% 
  st_zm() # the Z (height) dimension is troublesome...
summary(hroute)

address_closest_to_tract_centroid[1,]


# osrm

summary(osroute)

x <-  118001
y <-  119000
for (j in 1:120) {
  if (exists("dist_fnl")) rm(dist_fnl)
  for (i in x:y) {
    o1 <- as.double(gsub("(.*)\\+(.*)", "\\2", od[i,1]))
    o2 <- as.double(gsub("(.*)\\+(.*)", "\\1", od[i,1]))
    o <- sf::st_as_sf(data.frame(lon=o1, lat=o2), coords = c("lon", "lat"), crs = st_crs(4326))
    
    d1 <- as.double(gsub("(.*)\\+(.*)", "\\2", od[i,2]))
    d2 <- as.double(gsub("(.*)\\+(.*)", "\\1", od[i,2]))
    d <- sf::st_as_sf(data.frame(lon=d1, lat=d2), coords = c("lon", "lat"), crs = st_crs(4326))
    
    osroute <- osrm::osrmRoute(src = o, dst = d, osrm.profile = "car")
    
    
    ret <- data.table(origin = od[i,1], dest = od[i,2], dist = round(osroute$distance,0)*1000, time = round(osroute$duration,0)*60)
    if (!exists("dist_fnl")) dist_fnl <- ret else dist_fnl <- rbindlist(list(dist_fnl, ret))
    
    Sys.sleep(runif(n=1, min=1, max=1.3))
  }
  write.csv(dist_fnl, paste0("data/tract_distances_", x,"_", y,".csv"), row.names = F)
  
  x <-  x + 1000
  y <-  y + 1000
  
  gc()
}





# x <- 1
# y <- 100
# i <- 1
unq_dst <- data.table::as.data.table(unique(od[,2]))
for (i in 4:2185) {
  o1 <- as.double(gsub("(.*)\\+(.*)", "\\2", od[i,1]))
  o2 <- as.double(gsub("(.*)\\+(.*)", "\\1", od[i,1]))
  o <- sf::st_as_sf(data.frame(lon=o1, lat=o2), coords = c("lon", "lat"), crs = st_crs(4326))
  
  print(od[i,1])
  
  chunckNum <- ceiling(nrow(unq_dst)/100)
  chunkSize <- 100
  # xo <- (0*chunkSize+1)
  if (exists("timedist_fnl")) rm(timedist_fnl)
  for (j in 1:chunckNum) {
    x <- ((j-1)*chunkSize+1)
    y <- min(nrow(unq_dst),(j*chunkSize))
    
    d1 <- as.double(gsub("(.*)\\+(.*)", "\\2", unq_dst[x:y][[1]]))
    d2 <- as.double(gsub("(.*)\\+(.*)", "\\1", unq_dst[x:y][[1]]))
    d <- sf::st_as_sf(data.frame(lon=d1, lat=d2), coords = c("lon", "lat"), crs = st_crs(4326))
    
    timedist <- osrm::osrmTable(o, d, measure = c("distance", "duration"))
    
    ret <- data.table(origin = od[i,1], dest = unq_dst[x:y,1])
    ret$dist <- round(timedist$distances,0)*1000
    ret$time <- round(timedist$durations,0)*60
    ret <- ret[dist != 0]
    
    if (!exists("timedist_fnl")) timedist_fnl <- ret else timedist_fnl <- rbindlist(list(timedist_fnl, ret))
    
    # x <- x + 100
    # y <- y + 100
    
    Sys.sleep(runif(n=1, min=1, max=1.3))
  }
  # print(od[i,1])
  data.table::fwrite(timedist_fnl, paste0("data/osrm_table/tract_distances_", round(o2, 4), "+", round(o1, 4),".csv"))
}

# Combine osrmtable outputs
if (exists("osrmtablecombined")) rm(osrmtablecombined)
tract_files <- list.files("data/osrm_table/", pattern = "tract_distances", full.names = T)
for (f in tract_files) {
  dt <- data.table::fread(f)
  if (!exists("osrmtablecombined")) osrmtablecombined <- dt else osrmtablecombined <- data.table::rbindlist(list(osrmtablecombined, dt))
}
data.table::fwrite(osrmtablecombined, "data/osrm_table/tract_distances_combined.csv")

# Merge to add tract ids
tract_ids_lat_lons <- data.table::setDT((va_tracts_centroids_2020[, c("GEOID", "latlons")]))
tract_ids_lat_lons$geometry <- NULL
m1 <- merge(tract_ids_lat_lons, osrmtablecombined, by.x = "latlons", by.y = "origin", all.x = T)
m2 <- merge(m1, tract_ids_lat_lons, by.x = "dest.V1", by.y = "latlons", all.x = T)
setorder(m2, latlons)

m_fnl <- m2[, .(tract_orig = GEOID.x, tract_dest = GEOID.y, tract_orig_coords = latlons, tract_dest_coords = dest.V1, dist_meters = dist/1000, time_mins = time/60)]
data.table::fwrite(m_fnl, "data/osrm_table/tract2tract_dist_time.csv")

# Split to 5 files
m_fnl_1 <- m_fnl[1:1000000,]
m_fnl_2 <- m_fnl[1000001:2000000,]
m_fnl_3 <- m_fnl[2000001:3000000,]
m_fnl_4 <- m_fnl[3000001:4000000,]
m_fnl_5 <- m_fnl[4000001:nrow(m_fnl)]

data.table::fwrite(m_fnl_1, "data/osrm_table/tract2tract_dist_time_1.csv")
data.table::fwrite(m_fnl_2, "data/osrm_table/tract2tract_dist_time_2.csv")
data.table::fwrite(m_fnl_3, "data/osrm_table/tract2tract_dist_time_3.csv")
data.table::fwrite(m_fnl_4, "data/osrm_table/tract2tract_dist_time_4.csv")
data.table::fwrite(m_fnl_5, "data/osrm_table/tract2tract_dist_time_5.csv")

zip::zip("data/tract2tract_dist_time_2020/tract2tract_dist_time_1.zip", "data/osrm_table/tract2tract_dist_time_1.csv")
zip::zip("data/tract2tract_dist_time_2020/tract2tract_dist_time_2.zip", "data/osrm_table/tract2tract_dist_time_2.csv")
zip::zip("data/tract2tract_dist_time_2020/tract2tract_dist_time_3.zip", "data/osrm_table/tract2tract_dist_time_3.csv")
zip::zip("data/tract2tract_dist_time_2020/tract2tract_dist_time_4.zip", "data/osrm_table/tract2tract_dist_time_4.csv")
zip::zip("data/tract2tract_dist_time_2020/tract2tract_dist_time_5.zip", "data/osrm_table/tract2tract_dist_time_5.csv")

