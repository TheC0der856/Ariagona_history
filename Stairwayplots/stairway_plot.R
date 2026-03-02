# Stairwayplot my way
library(ggplot2)
#library(ggthemes)
#library(ggpubr)
library(envalysis)
library(patchwork)

setwd("C:/Users/Gronefeld/Desktop/history_of_genflow/Stairwayplots/")

popmap <- read.csv("popmap_g_h_t_united_MASTER.txt",sep = "\t", header = F)
pops <- unique(popmap$V2)


stairway_plOtter <- function(species, color){
  in.file <- list.files(path = paste("./",species,"/",species, sep = ""), pattern = ".final.summary")
  in.data <- read.table(paste("./",species,"/",species,"/",in.file[1], sep = ""), header = TRUE)
  #x_breaks <- c(0,0.01,0.1,0.4,1,2,4,10,20,40,100,200)
  x_breaks <- c(0.01, 0.1, 1, 10, 100)
  #y_breaks <- c(0,0.06,0.1,0.2,0.4,0.6,1,2,4,6,10,20,40,100,200)
  y_breaks <- c(0,
                #0.06,
                0.1,0.2,0.4,
                #0.6,
                1,2,4,#6,
                10,20,40,100,200, 400)
  plot <- ggplot(in.data, aes(x=year)) +
    scale_x_continuous(trans = "log1p", 
                       breaks = x_breaks*1000, 
                       labels = x_breaks,
                       limits = c(1,200000), 
                       expand = c(0.05,0.0)) +
    xlab("") +
    scale_y_continuous(trans = "log1p",
                       breaks = y_breaks*1000,
                       expand = c(0.05, 0.05),
                       labels = y_breaks) +
    ylab("") +
    geom_ribbon(aes(ymin = Ne_2.5., ymax = Ne_97.5.), fill = color, alpha = 0.2) +
    geom_line(data=in.data, aes(y=Ne_median), colour = color, linetype='solid', size = 3) +
    geom_line(data=in.data, aes(y=Ne_2.5.), colour = color, linetype='solid', size = .25) +
    geom_line(data=in.data, aes(y=Ne_97.5.), colour = color, linetype='solid', size = .25) +
    theme_publish() +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 22),
          axis.text.y = element_text(size = 22))
  assign(paste(species,"final",sep="_"), plot, envir = .GlobalEnv)
}


P_t_most_east <- lapply(pops[7], stairway_plOtter, color = "darkslategray")[[1]]
P_t_little_east <- lapply(pops[9], stairway_plOtter, color = "darkslategray4")[[1]]
P_t_agua <- lapply(pops[8], stairway_plOtter, color = "darkslategray3")[[1]]
P_t_west <- lapply(pops[6], stairway_plOtter, color = "darkslategray1")[[1]]

P_g_majona <- lapply(pops[3], stairway_plOtter, color = "darkolivegreen")[[1]]
P_g_south <- lapply(pops[2], stairway_plOtter, color = "darkolivegreen3")[[1]]
P_g_north <- lapply(pops[1], stairway_plOtter, color = "darkolivegreen1")[[1]]


P_h_east <- lapply(pops[5], stairway_plOtter, color = "gold3")[[1]]
P_h_west <- lapply(pops[4], stairway_plOtter, color = "gold1")[[1]]


# Combine all plots into a single page
all_plots <- list(P_t_most_east, P_t_little_east, P_t_agua, P_t_west,
                  P_g_majona, P_g_south, P_g_north,
                  P_h_east, P_h_west )# have to photoshop and move down, switch H & I




Stairwayplot <- (
  all_plots[[1]] + all_plots[[2]] + all_plots[[3]] + all_plots[[4]] +
    all_plots[[5]] + all_plots[[6]] + all_plots[[7]] + plot_spacer() +
    all_plots[[8]] + all_plots[[9]] + plot_spacer() + plot_spacer()
) +
  plot_layout(ncol = 4) +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(
      family = "Arial",
      face = "bold",
      size = 30,
      color = "black"
    ),
    plot.tag.position = c(0.02, 0.98),
    plot.margin = margin(2, 2, 2, 2, "mm")
  )

# save as pdf
pdf("Fig2.pdf", width = 19, height = 13)
print(Stairwayplot) # Print the combined plot to the PDF
dev.off()