#> -------------------------------------------
#> POWER ANALYSES IN MOTOR BEHAVIOUR PROJECT
#> -- McKay, Corson, Vinh, Jeyarajan, Tandon, Brooks,
#> Hubley, and Carter
#>
#> Figures
#>
#> Authors:
#>   Brad McKay
#>   Mike Carter
#>
#> Last update: October 18 2022
#>
#> Website: https://www.cartermaclab.org
#> -------------------------------------------

#> SCRIPT SETUP ----
#>
#> Load required libraries and system fonts
source("scripts/wrangle.R")
loadfonts()

#> Create figure 1 in manuscript
#> PRISMA flowchart
fig1 <- PRISMAstatement::prisma(
  found = 704,
  found_other = 0,
  no_dupes = 704,
  screened = 704,
  screen_exclusions = 0,
  full_text = 704,
  full_text_exclusions = 98,
  qualitative = 606,
  quantitative = 606,
  width = 1600,
  height = 1600,
  font_size = 12
)
fig1


#> Create figure 2 in manuscript
#> Waffle plot of proportion of studies finding support
fig2_tib <- tibble::tibble(
  category = c(
    "Power-Analysis & Support",
    "Power-Analysis & No Support",
    "No Power-Analysis & Support",
    "No Power-Analysis & No Support"
  ),
  n = c(65, 20, 469, 82)
)

fig2 <- waffle::waffle(
  fig2_tib,
  size = .5,
  rows = 12,
  colors = c("#12a4d9", "#89d2ec", "#666666" ,"#322e2f"),
  legend_pos = "bottom"
) +
  theme(
    text = element_text(family = "Roboto Condensed",
                        size = 15)
  )
fig2


#> Create figure 3 in manuscript
#> Waffle plot of proportion of studies finding support
#> based on effect size justification
fig3_tib <- tibble::tibble(
  support = c("Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No"),
  justification = c(rep("Previous study", 2),
                    rep("Benchmarks", 2),
                    rep("Pilot", 2),
                    rep("No justification", 2)
                  ),
  n = c(25, 12,
        13, 6,
        9, 0,
        18, 2)
)

fig3 <- ggplot2::ggplot(
  fig3_tib, aes(fill = support, values = n)) +
  geom_waffle(color = "#666666",
              size = .25,
              n_rows = 3,
              flip = TRUE) +
  facet_wrap(~forcats::fct_relevel(
    justification, "Previous study", "Benchmarks",
    "Pilot", "No justification"),
    nrow = 1,
    strip.position = "bottom") +
  scale_x_discrete(name = "Effect Size Justification") +
  scale_y_continuous(name = "Count",
                     labels = function(x) x * 3, #> must equal n_rows value
                     expand = c(0, 0)) +
  scale_fill_manual(name = "Primary hypothesis supported?",
                    values = c("#322e2f","#12a4d9")) +
  coord_equal() +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid = element_blank(),
        axis.ticks.y = element_line(),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(size = 12),
        legend.position = "top",
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12)
  ) +
  guides(fill = guide_legend(reverse = TRUE))
fig3
