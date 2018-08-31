# Plotting ----------------------------------------------------------------

#' gg_color_hue
#'
#' Produce ggplot-like colors
#'
#' Shamelessly copied from \url{https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette}
#'
#' @param n The number of desired colors
#'
#' @return Vector of color codes
#' @examples
#' gg_color_hue(5)
#'
#' @export
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  grDevices::hcl(h=hues, l=65, c=100)[1:n]
}


#' powerpoint_theme
#'
#' Increase font size of titles for powerpoint
#'
#' @return ggplot object with larger font sizes
#' @author Joel H Nitta, \email{joelnitta@@gmail.com}
#'
#' @examples
#' p1 <- ggplot(iris, aes(Sepal.Length, Petal.Length)) +
#'  geom_point(aes(color = Species))
#' p1 + powerpoint_theme()
#'
#' @export
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

#' clean_legend_theme
#'
#' Clean up ggplot legends
#'
#' Removes the legend title and default boxes around ggplot legend entries.
#'
#' @return A ggplot object with legend entry boxes removed
#' @author Joel H Nitta, \email{joelnitta@@gmail.com}
#'
#' @examples
#' library(ggplot2)
#' p1 <- ggplot(iris, aes(Sepal.Length, Petal.Length)) +
#'   geom_point(aes(color = Species))
#' p1
#' p1 + clean_legend_theme()
#'
#' @export
clean_legend_theme <- function () {
  ggplot2::theme (
    legend.title = ggplot2::element_blank(),
    legend.background = ggplot2::element_rect(colour = "transparent", fill = "transparent", size = 0.5),
    legend.key = ggplot2::element_rect(colour = "white", fill = "white", size = 0.5) )
}

#' standard_theme
#'
#' A simple ggplot theme
#'
#' A simple ggplot theme with uniform font sizes and no extra gridlines.
#'
#' @param font_size Font size to use throughout
#' @return A ggplot object
#' @author Joel H Nitta, \email{joelnitta@@gmail.com}
#'
#' @examples
#' library(ggplot2)
#' p1 <- ggplot(iris, aes(Sepal.Length, Petal.Length)) +
#'   geom_point(aes(color = Species))
#' p1
#' p1 + standard_theme()
#'
#' @export
standard_theme <- function (font_size = 12) {
  ggplot2::theme_bw() + ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(colour="black", vjust=0.5, size=font_size),
    axis.text.y = ggplot2::element_text(colour="black", vjust=0.5, size=font_size),
    plot.title= ggplot2::element_text(size=font_size),
    axis.title.x = ggplot2::element_text(size=font_size),
    axis.title.y = ggplot2::element_text(size=font_size),
    legend.title = ggplot2::element_text(size=font_size),
    legend.text = ggplot2::element_text(size=font_size),
    strip.text.x = ggplot2::element_text(size=font_size),
    strip.text.y = ggplot2::element_text(size=font_size),
    strip.background = ggplot2::element_rect(colour="white", fill = "white"))
}

#' blank_x_theme
#'
#' Remove x-axis text and ticks
#'
#' @return A ggplot object
#' @author Joel H Nitta, \email{joelnitta@@gmail.com}
#'
#' @examples
#' library(ggplot2)
#' p1 <- ggplot(iris, aes(Sepal.Length, Petal.Length)) +
#'   geom_point(aes(color = Species))
#' p1
#' p1 + blank_x_theme()
#'
#' @export
blank_x_theme <- function() {
  ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_blank(),
                 axis.ticks.x = ggplot2::element_blank())
}

#' blank_y_theme
#'
#' Remove y-axis text and ticks
#'
#' @return A ggplot object
#' @author Joel H Nitta, \email{joelnitta@@gmail.com}
#'
#' @examples
#' library(ggplot2)
#' p1 <- ggplot(iris, aes(Sepal.Length, Petal.Length)) +
#'   geom_point(aes(color = Species))
#' p1
#' p1 + blank_y_theme()
#'
#' @export
blank_y_theme <- function () {
  ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank())
}
