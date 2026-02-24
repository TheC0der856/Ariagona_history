# combine plots
library(patchwork)

sleeping <- (tgh_pca | map_tgh | t_pca | map_t ) /
  (g_pca | map_g | h_pca | map_h) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(face = "bold", size = 14)  
  )
standing <-  (tgh_pca | map_tgh ) /
(t_pca | map_t ) /
(g_pca | map_g ) /
(h_pca | map_h) +
plot_layout(heights = c(1, 1, 1, 1)) +
plot_annotation(tag_levels = "A") &
theme(
    plot.tag = element_text(face = "bold", size = 14),
    plot.margin = margin(0, 10, 0, 10)
)

# save
ggsave(
  filename = "E:/Innsbruck/Innsbruck2/stairwayplot/standing.pdf",
  plot = standing,
  width = 7,      
  height = 14,  
  units = "in",
  device = "pdf"
)
ggsave(
  filename = "E:/Innsbruck/Innsbruck2/stairwayplot/sleeping.pdf",
  plot = sleeping,
  width = 14,      
  height = 7,  
  units = "in",
  device = "pdf"
)