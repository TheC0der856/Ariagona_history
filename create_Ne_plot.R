# load libraries
library(ggplot2)
library(dplyr)
library(readr)

# convert data
df_plot <- df %>%
  filter(CritValue == 0.05) %>%
  mutate(
    Ne = parse_number(as.character(Ne)),
    low_para = parse_number(as.character(low_para)),
    high_para = parse_number(as.character(high_para)),
    Population = case_when(
      PopDecoded == "Hierro_west2023" ~ "H2",
      PopDecoded == "Hierro_east2010" ~ "H1",
      PopDecoded == "Gomera_center2010" ~ "G2",
      PopDecoded == "Gomera_north2023" ~ "G3",
      PopDecoded == "Gomera_east2015" ~ "G1",
      PopDecoded == "Tenerife_eAnaga_Lagunetas2023" ~ "T1",
      PopDecoded == "Tenerife_wAnaga2024" ~ "T2",
      PopDecoded == "Tenerife_west2023" ~ "T4",
      TRUE ~ PopDecoded
    ),
    Population = factor(
      Population,
      levels = c("T1", "T2", "T4", "G1", "G2", "G3", "H1", "H2")
    )
  )
pop_colors <- c(
  "T1" = "darkslategray4",
  "T2" = "darkslategray",
  "T4" = "darkslategray1",
  "G1" = "darkolivegreen",
  "G2" = "darkolivegreen3",
  "G3" = "darkolivegreen1",
  "H1" = "gold3",
  "H2" = "gold1"
)

# plot
ggplot(df_plot, aes(x = Population, y = Ne)) +
  geom_errorbar(
    data = df_plot %>%
      filter(
        !is.na(low_para),
        !is.na(high_para),
        is.finite(low_para),
        is.finite(high_para)
      ),
    aes(
      ymin = low_para,
      ymax = high_para,
      color = Population
    ),
    width = 0.2,
    linewidth = 0.8
  ) +
  geom_point(aes(color = Population), size = 7) +
  scale_color_manual(values = pop_colors) +
  scale_x_discrete(
    limits = c("T1", "T2", "T4", "G1", "G2", "G3", "H1", "H2"),
    drop = FALSE
  ) +
  geom_hline(
    yintercept = c(50, 500),
    color = "grey75",
    linewidth = 0.6,
    linetype = "solid"
  ) +
  labs(
    x = "Population",
    y = expression(italic(N[e])),
   
  ) +
  theme_classic(base_size = 15) +
  theme (
    legend.position = "none", 
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 26)
  )

