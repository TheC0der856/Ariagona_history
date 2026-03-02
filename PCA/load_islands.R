# find island
get_island <- function(name) {
  opq(name) %>%
    add_osm_feature(key = "place", value = "island") %>%
    osmdata_sf() %>%
    (\(x) st_cast(x$osm_multipolygons, "POLYGON"))() %>%
    (\(x) {
      x$area <- st_area(x)
      x[which.max(x$area), ]
    })() %>%
    st_transform(32628)
}
el_hierro_no_rocks  <- get_island("El Hierro, Canary Islands, Spain")
la_gomera_no_rocks  <- get_island("La Gomera, Canary Islands, Spain")
tenerife_no_rocks   <- get_island("Tenerife, Canary Islands, Spain")

# save islands
saveRDS(el_hierro_no_rocks,"E:/Innsbruck/Innsbruck2/stairwayplot/data/el_hierro_no_rocks.rds")
saveRDS(la_gomera_no_rocks,"E:/Innsbruck/Innsbruck2/stairwayplot/data/la_gomera_no_rocks.rds")
saveRDS(tenerife_no_rocks,"E:/Innsbruck/Innsbruck2/stairwayplot/data/tenerife_no_rocks.rds")