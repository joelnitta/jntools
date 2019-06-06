# Plotting ----------------------------------------------------------------

#' Preview color palette
#'
#' @param colors A vector of colors
#'
#' @return Plot of the colors with their names if provided
#' @export
#'
#' @examples
#' preview_pal(c("red", "green"))
preview_pal <- function(colors) {

  labels <- if(is.null(names(colors))) {colors} else {names(colors)}

  pie(
    rep(1, length(colors)),
    labels = labels,
    col = colors)
}

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
#' library(ggplot2)
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

#' Get tips of a phylogenetic tree in their plotted order
#'
#' After re-rooting a tree, the order of tips when the tree
#' is plotted no longer match the order of $tip.label. Use
#' this function to get tips in the order they are plotted.
#'
#' Note that this is not compatible with ggtree! The same tree
#' plotted with ggtree and ape may have slightly different
#' tip order.
#'
#' To get tip order of a ggtree plot, do something like this:
#' data(bird.orders)
#' p <- ggtree::ggtree(bird.orders)
#' p[["data"]] # extract 'label' and 'y' columns
#'
#' @param tree Phylognetic tree (list of class "phylo")
#'
#' @return Vector
#'
#' @examples
#' library(ape)
#' data(bird.orders)
#' plot(bird.orders, no.margin = TRUE)
#' bird.orders$tip.label # matches plot
#' bird.orders_reroot <- root(bird.orders, 5)
#' plot(bird.orders_reroot)
#' # Tip order still the same even though the tree
#' # has been re-rooted!
#' all.equal(bird.orders_reroot$tip.label, bird.orders$tip.label)
#' get_tips_in_ape_plot_order(bird.orders_reroot) # matches order in plot
#' @references https://stackoverflow.com/questions/34364660/how-to-get-correct-order-of-tip-labels-in-ape-after-calling-ladderize-function
#' @export
get_tips_in_ape_plot_order <- function (tree) {
  assertthat::assert_that(inherits(tree, "phylo"))
  # First filter out internal nodes
  # from the the second column of the edge matrix
  is_tip <- tree$edge[,2] <= length(tree$tip.label)
  ordered_tips <- tree$edge[is_tip, 2]
  # Use this vector to extract the tips in the right order
  tree$tip.label[ordered_tips]
}
