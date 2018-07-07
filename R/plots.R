# Plotting ----------------------------------------------------------------

# produce ggplot-like colors
# input: single number of number of desired colors
# output: vector of color codes
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

# increase font size of titles for powerpoint
powerpoint_theme <- function () {
  ggplot2::theme (
    plot.title= ggplot2::element_text(size=16),
    axis.title.x = ggplot2::element_text(size=16),
    axis.title.y = ggplot2::element_text(size=16),
    legend.title = ggplot2::element_text(size=12),
    legend.text = ggplot2::element_text(size=12),
    strip.text.x = ggplot2::element_text(size=16),
    strip.text.y = ggplot2::element_text(size=16)
  )
}

# make blank filler plot
blankplot <- ggplot2::ggplot() +
  ggplot2::geom_blank(ggplot2::aes(1,1)) +
  cowplot::theme_nothing()

# clean legend format
clean_legend_theme <- function () {
  ggplot2::theme (
    legend.title = ggplot2::element_blank(),
    legend.background = ggplot2::element_rect(colour = "transparent", fill = "transparent", size = 0.5),
    legend.key = ggplot2::element_rect(colour = "white", fill = "white", size = 0.5) )
}

# standard theme
standard_theme <- function () {
  ggplot2::theme_bw() + ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(colour="black", vjust=0.5, size=12),
    axis.text.y = ggplot2::element_text(colour="black", vjust=0.5, size=12),
    plot.title= ggplot2::element_text(size=12),
    axis.title.x = ggplot2::element_text(size=12),
    axis.title.y = ggplot2::element_text(size=12),
    legend.title = ggplot2::element_text(size=12),
    legend.text = ggplot2::element_text(size=12),
    strip.text.x = ggplot2::element_text(size=12),
    strip.text.y = ggplot2::element_text(size=12),
    strip.background = ggplot2::element_rect(colour="white", fill = "white"))
}

blank_x_theme <- function() {
  ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_blank(),
                 axis.ticks.x = ggplot2::element_blank())
}

blank_y_theme <- function () {
  ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank())
}
