## Twitter
##  
library(tidyverse)
library(ggthemes)
library(grid)

worldcup_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week11_fifa_audience.csv") %>%
  select(-X1)
worldcup_raw


worldcup_raw %>% 
  add_count(country) %>% 
  group_by(confederation) %>% 
  summarize_if(is.numeric, sum) %>% 
  ungroup() %>% 
  mutate(
    total_members = sum(n),
    perc_members = (n / total_members) * 100,
    perc_members = round(perc_members, 1) 
  ) %>% 
  select(-n, -total_members) %>% 
  gather(key = shar_var, value = value, -confederation) ->
  worldcup_confed_sum

# reorder variables as factors for the plot
worldcup_confed_sum %>% 
  mutate(
    # reorder the shar_var
    shar_var = shar_var %>% 
      as_factor() %>% 
      fct_relevel("perc_members", "population_share",
                  "tv_audience_share", "gdp_weighted_share"),
    # reorder the confederation
    confederation = confederation %>% 
      as_factor() %>% 
      fct_relevel("OFC", "CAF", "CONMEBOL", "CONCACAF", "AFC", "UEFA")
  ) ->
  worldcup_confed_sum

# have % lable ONLY for UEFA
worldcup_confed_sum %>% 
  mutate(val2 = if_else(confederation == "UEFA", paste0(value, "%"), paste0(value))) ->
  worldcup_confed_sum
worldcup_confed_sum

perc_labs <- str_to_upper(c("fifa members", "global \npopulation", "wolrd cup tv \n audience",
                          "gdp-weighted \nTV audience"))
perc_labs %>% cat()

confed_labs <- c("OFC (Oceania)", "CAF (Africa)", "CONMEBOL (S. America)", 
                 "CONCACAF (N. America)", "AFC (Asia)", "UEFA (Europe)")

# set rmd chunk: fig.height = 6, fig.width = 10 VERY IMPORTANT
worldcup_confed_sum %>% 
  ggplot(aes(x = shar_var, y = confederation, fill = value)) +
  geom_tile(color = "lightgrey") +
  scale_fill_gradient(low = "#e5f5e0", high = "#008800") + # mid = "#a1d99b"
  geom_text(aes(label = val2, fontface = "bold"), size = 5) +
  theme_fivethirtyeight() +
  theme(
    legend.position = "none",
    text = element_text(face = "bold", colour = "black"),
    axis.title = element_text(),  # add axis.title back into plot ...
    axis.title.x.top = element_text(margin = margin(b = 10)),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(hjust = 0, size = 12),
    plot.margin = rep(grid::unit(0.75, "cm"), 4)
  ) +
  # set the x axis in the top area of the plot
  scale_x_discrete(position = "top", expand = c(0, 0),
                   labels = perc_labs) +
  labs(x = "IN 2010, SHARE OF ...", y = "") +
  scale_y_discrete(expand = c(0, 0),
                   labels = confed_labs) +
  # annotation_custom(grob = textGrob(
  #   label = expression(bold("CONFEDERATION")), # use bold() inside expression() to get BOLD text
  #   gp = gpar(cex = 1.0, fontface = "bold", hjust = 0,  # also need to specify the fontface as well
  #             ymin = 6.66, ymax = 6.66,
  #             xmin = -0.23, xmax = -0.23)
  # )) +
  annotation_custom(grob = linesGrob(),
                    xmin = 0.5, xmax = 4.5, ymin = 7, ymax = 7) + # top bar
  annotation_custom(grob = linesGrob(gp = gpar(lwd = 3)),
                    xmin = -0.64, xmax = 4.5, ymin = 6.5, ymax = 6.5) -> # x-axis bar
  p1
p1

pt <- ggplot_gtable(ggplot_build(p1))
pt
pt$layout$clip[pt$layout$name == "panel"] <- "off"


grid.newpage()
grid.draw(pt)
