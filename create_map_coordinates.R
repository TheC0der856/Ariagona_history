library(sf)
library(vcfR)
library(adegenet)
library(ggplot2)
library(osmdata)
library(dplyr)
library(ggspatial)


# create map

# load coordinates
Ariagona <- read.csv("E:/Innsbruck/Innsbruck2/stairwayplot/data/Ariagona_margaritae.csv")
Ariagona <- Ariagona[, c("WGS84_X", "WGS84_Y", "Specimen_ID", "Place_Name")]
Ariagona$WGS84_X <- as.numeric(gsub(",", ".", Ariagona$WGS84_X))
Ariagona$WGS84_Y <- as.numeric(gsub(",", ".", Ariagona$WGS84_Y))

# genetic data
source("E:/Innsbruck/Innsbruck2/stairwayplot/prepare_data.R")
pop_t <- read.table(
  text = readLines("E:/Innsbruck/Innsbruck2/stairwayplot/new_popmaps/popmap_t_new_groups.txt"),
  sep = "\t",  header = FALSE,  stringsAsFactors = FALSE
) 
colnames(pop_t) <- c("ID", "Group")
pop_g <- read.table(
  text = readLines("E:/Innsbruck/Innsbruck2/stairwayplot/new_popmaps/popmap_g_new_groups.txt"),
  sep = "\t",  header = FALSE,  stringsAsFactors = FALSE
) 
colnames(pop_g) <- c("ID", "Group")
pop_h <- read.table(
  text = readLines("E:/Innsbruck/Innsbruck2/stairwayplot/new_popmaps/popmap_h_new_groups.txt"),
  sep = "\t",  header = FALSE,  stringsAsFactors = FALSE
) 
colnames(pop_h) <- c("ID", "Group")

pop(genlight_t) <- pop_t$Group[match(indNames(genlight_t), pop_t$ID)]
pop_h_filtered <- pop_h[match(indNames(genlight_h), pop_h$ID), ]
genlight_h <- genlight_h[!is.na(pop_h_filtered$Group)]
pop(genlight_h) <- as.factor(na.omit(pop_h_filtered$Group))
pop(genlight_g) <- pop_g$Group[match(indNames(genlight_g), pop_g$ID)]
genlight_h_filt <- genlight_h[, glNA(genlight_h) < 0.2]
 

# get coordinates as sf
coordinates_sf <- function(data, genlight_obj, pop_df) {
  data %>%
    dplyr::filter(Specimen_ID %in% genlight_obj@ind.names) %>%
    dplyr::left_join(pop_df, by = c("Specimen_ID" = "ID")) %>%
    st_as_sf(coords = c("WGS84_X", "WGS84_Y"),
             crs = 4326,
             remove = FALSE) %>%
    st_transform(32628)
}
Ariagona_h <- coordinates_sf(Ariagona, genlight_h_filt, pop_h)
Ariagona_g <- coordinates_sf(Ariagona, genlight_g, pop_g)
Ariagona_t <- coordinates_sf(Ariagona, genlight_t, pop_t)
pop_tgh <- tibble(ID = indNames(OGgenlight), Group = pop(OGgenlight))
Ariagona_tgh <- coordinates_sf(Ariagona, OGgenlight, pop_tgh)



# load island
el_hierro_no_rocks <- readRDS("E:/Innsbruck/Innsbruck2/stairwayplot/data/el_hierro_no_rocks.rds")
la_gomera_no_rocks <- readRDS("E:/Innsbruck/Innsbruck2/stairwayplot/data/la_gomera_no_rocks.rds")
tenerife_no_rocks <- readRDS("E:/Innsbruck/Innsbruck2/stairwayplot/data/tenerife_no_rocks.rds")

islands_all <- bind_rows(
  el_hierro_no_rocks %>% mutate(Island = "El Hierro"),
  la_gomera_no_rocks %>% mutate(Island = "La Gomera"),
  tenerife_no_rocks %>% mutate(Island = "Tenerife")
)

# functions: make plot
make_square_extent <- function(sf_obj, buffer_frac = 0.05, y_shift_frac = 0) {
  bb <- st_bbox(sf_obj)
  width  <- bb["xmax"] - bb["xmin"]
  height <- bb["ymax"] - bb["ymin"]
  side   <- max(width, height)
  xmid <- (bb["xmin"] + bb["xmax"]) / 2
  ymid <- (bb["ymin"] + bb["ymax"]) / 2
  half_side <- (side / 2) + side * buffer_frac
  ymid <- ymid + side * y_shift_frac
  list(
    xlim = c(xmid - half_side, xmid + half_side),
    ylim = c(ymid - half_side, ymid + half_side)
  )
}

base_map <- function(island, 
                     points, 
                     fill_values, 
                     width_hint = 0.5,
                     buffer = 0.05, 
                     y_shift = 0, 
                     breaks_width = 0.1, 
                     arrow_height = 0.6,
                     arrow_width  = 0.4, 
                     bar_x = 0.7,
                     bar_y = 0.3, 
                     pointsize = 4) {
  ext <- make_square_extent(island, buffer, y_shift)
  ggplot() +
    geom_sf(data = island, fill = "grey90", color = "black") +
    geom_sf(data = points, 
            aes(fill = Group),
            size = pointsize, 
            shape = 21, 
            show.legend = FALSE) +
    annotation_north_arrow(
      location = "topleft",
      which_north = "grid",
      style = north_arrow_orienteering(
        fill = c("black", "black"),
        line_col = "black",
        text_col = NA),
      height = unit(arrow_height, "cm"),
      width = unit(arrow_width, "cm")
    ) +
    annotation_scale(
      location = "tl",
      pad_x = unit(bar_x, "cm"),
      pad_y = unit(bar_y, "cm"),
      height = unit(0.05, "cm"),
      width_hint = width_hint,
      text_cex = 1,
      line_col = "black",
      text_col = "black",
      style = "bar",
      bar_cols = c("black", "black")
    ) +
    coord_sf(
      crs = 32628,
      datum = st_crs(4326),
      xlim = ext$xlim,
      ylim = ext$ylim,
      expand = FALSE
    ) +
    scale_x_continuous(breaks = scales::breaks_width(breaks_width)) +
    scale_y_continuous(breaks = scales::breaks_width(breaks_width)) +
    scale_fill_manual(values = fill_values) +
    theme_classic() +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      axis.text = element_text(size = 13),
      axis.text.y = element_text(angle = 90, hjust = 0.5),
      panel.grid.major = element_blank(),
      #panel.grid.major = element_line(color = "grey60", linetype = "dashed", linewidth = 0.2),
      panel.grid.minor = element_blank()
    )
}

# make plot
map_g <- base_map(
  island = la_gomera_no_rocks,
  points = Ariagona_g,
  fill_values = c(
    "north" = "darkolivegreen1",
    "south" = "darkolivegreen3",
    "majona" = "darkolivegreen" 
  ),
  width_hint = 0.5,
  buffer = 0.1,
  y_shift = 0.03
)
map_h <- base_map(
  island = el_hierro_no_rocks,
  points = Ariagona_h,
  fill_values = c("east" = "gold3",
                  "west" = "gold1"),
  buffer = 0.1
)
map_t <- base_map(
  island = tenerife_no_rocks,
  points = Ariagona_t,
  fill_values = c(
    "west" = "darkslategray1",
    "agua" = "darkslategray3",
    "little_east" = "darkslategray4",
    "most_east" = "darkslategray"
  ),
  width_hint = 0.15,
  breaks_width = 0.4
)
map_tgh <- base_map(
  island = islands_all, 
  points = Ariagona_tgh,
  fill_values = c(
    t = "darkslategray4",
    g = "darkolivegreen3",
    h = "gold3"
  ),
  breaks_width = 1,
  pointsize = 2
)

# show plot
# map_g
# map_h
# map_t