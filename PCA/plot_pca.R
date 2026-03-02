# load packages 
library(adegenet)
#library(adegraphics)
library(vcfR)
#library(pegas)
#library(StAMPP)
library(ggplot2) # plot
#library(grid)    # plot
# read in function for faster calculation of PCA 
source("D:/Innsbruck/Innsbruck2/stairwayplot/gl_PCAfast.R")   

# prepare data
source("D:/Innsbruck/Innsbruck2/stairwayplot/prepare_data.R")

# add pop info
pop_t <- read.table(
  text = readLines("D:/Innsbruck/Innsbruck2/stairwayplot/new_popmaps/popmap_t_new_groups.txt"),
  sep = "\t",  header = FALSE,  stringsAsFactors = FALSE
) 
colnames(pop_t) <- c("ID", "Group")

pop_g <- read.table(
  text = readLines("D:/Innsbruck/Innsbruck2/stairwayplot/new_popmaps/popmap_g_new_groups.txt"),
  sep = "\t",  header = FALSE,  stringsAsFactors = FALSE
) 
colnames(pop_g) <- c("ID", "Group")

pop_h <- read.table(
  text = readLines("D:/Innsbruck/Innsbruck2/stairwayplot/new_popmaps/popmap_h_new_groups.txt"),
  sep = "\t",  header = FALSE,  stringsAsFactors = FALSE
) 
colnames(pop_h) <- c("ID", "Group")



pop(genlight_t) <- pop_t$Group[match(indNames(genlight_t), pop_t$ID)]

# filter?
pop_h_filtered <- pop_h[match(indNames(genlight_h), pop_h$ID), ]
genlight_h <- genlight_h[!is.na(pop_h_filtered$Group)]
pop(genlight_h) <- as.factor(na.omit(pop_h_filtered$Group))
genlight_h_filt <- genlight_h[, glNA(genlight_h) < 0.2]

pop(genlight_g) <- pop_g$Group[match(indNames(genlight_g), pop_g$ID)]



############### run PCA
run_pca_df <- function(dataset, nf = 300) {
  pca <- glPcaFast(dataset, nf = nf)
  df <- data.frame(
    PC1 = pca$scores[,1],
    PC2 = pca$scores[,2],
    pop = pop(dataset)
  )
  list(pca = pca, df = df)
}

res_g <- run_pca_df(genlight_g)
res_t <- run_pca_df(genlight_t)
res_h <- run_pca_df(genlight_h_filt)
res_tgh <- run_pca_df(OGgenlight)


############## create plot
plot_pca <- function(df, pca, cols, breaks = 1, pointsize = 3) {
  square_limits <- function(x, y, factor = 1.2) {
    xr <- range(x)
    yr <- range(y)
    span <- max(diff(xr), diff(yr)) * factor
    x_mid <- mean(xr)
    y_mid <- mean(yr)
    list(
      x = c(x_mid - span/2, x_mid + span/2),
      y = c(y_mid - span/2, y_mid + span/2)
    )
  }
  lims <- square_limits(df$PC1, df$PC2)
  make_breaks <- function(lim, by) {
    lower <- floor(lim[1] / by) * by
    upper <- ceiling(lim[2] / by) * by
    seq(lower, upper, by = by)
  }
  x_breaks <- make_breaks(lims$x, breaks)
  y_breaks <- make_breaks(lims$y, breaks)
  ggplot(df, aes(PC1, PC2, color = pop)) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    geom_point(size = pointsize, alpha = 0.6) +
    scale_color_manual(values = cols) +
    labs(
      x = paste0(round(pca$eig[1] / sum(pca$eig) * 100, 1), " %"),
      y = paste0(round(pca$eig[2] / sum(pca$eig) * 100, 1), " %")
    ) +
    scale_x_continuous(
      breaks = x_breaks,
      limits = lims$x,
      sec.axis = dup_axis(name = NULL, labels = NULL)
    ) +
    scale_y_continuous(
      breaks = y_breaks,
      limits = lims$y,
      sec.axis = dup_axis(name = NULL, labels = NULL)
    ) +
    coord_fixed() +
    theme_classic(base_size = 12) +
    theme(
      legend.position = "none",
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      axis.ticks.length = unit(7, "pt"),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      plot.margin = unit(c(1, 1, 1, 1), "lines")
    )
}

g_pca <- plot_pca(
  res_g$df,
  res_g$pca,
  cols = c(
    "north" = "darkolivegreen1" ,
    "south" = "darkolivegreen3",
    "majona" = "darkolivegreen"
  ), 
  breaks = 4
)
t_pca <- plot_pca(
  res_t$df,
  res_t$pca,
  cols = c(
    "west" = "darkslategray1",
    "agua" = "darkslategray3",
    "little_east" = "darkslategray4",
    "most_east" = "darkslategray"), 
  breaks = 5
)
h_pca <- plot_pca(
  res_h$df,
  res_h$pca,
  cols = c(
    "east" = "gold3", 
    "west" = "gold1"
  )
)
tgh_pca <- plot_pca(
  res_tgh$df,
  res_tgh$pca,
  cols = c(    t = "darkslategray4",
               g = "darkolivegreen3",
               h = "gold3"), 
  breaks = 10,
  pointsize = 1.5
)

# view plots
# g_pca
# t_pca
# h_pca
