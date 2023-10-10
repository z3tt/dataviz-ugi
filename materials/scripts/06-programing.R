#------------------------------------------------------------------------------#
#                                                                              #
#                    The Art & Science of Data Visualization                   #
#                Engaging and Reproducible Graphics with ggplot2               #
#                                                                              #
#                      Working with Layout and Compositions                    #
#                                                                              #
#                              Dr. Cedric Scherer                              #
#                       RTG-UGI Workshop // October 2023                       #
#                                                                              #
#------------------------------------------------------------------------------#


## -----------------------------------------------------------------------------
library(ggplot2)
library(dplyr)

bikes <- readr::read_csv(
  here::here("data", "london-bikes-custom.csv"),
  col_types = "Dcfffilllddddc"
)

bikes$season <- forcats::fct_inorder(bikes$season)

theme_set(theme_light(base_size = 14, base_family = "Asap SemiCondensed"))

theme_update(
  panel.grid.minor = element_blank(),
  plot.title = element_text(face = "bold"),
  plot.title.position = "plot"
)


## -----------------------------------------------------------------------------
smooth <- TRUE

ggplot(bikes, aes(x = temp, y = humidity)) +
  { if(smooth) geom_smooth(color = "red") } +
  geom_point(alpha = .5)


## -----------------------------------------------------------------------------
smooth <- FALSE

ggplot(bikes, aes(x = temp, y = humidity)) +
  { if(smooth) geom_smooth(color = "red") } +
  geom_point(alpha = .5)


## -----------------------------------------------------------------------------
draw_scatter <- function(smooth = TRUE) {
  ggplot(bikes, aes(x = temp, y = humidity)) +
    { if(smooth) geom_smooth(color = "red") } +
    geom_point(alpha = .5)
}


## -----------------------------------------------------------------------------
draw_scatter()


## -----------------------------------------------------------------------------
draw_scatter(smooth = FALSE)


## -----------------------------------------------------------------------------
geom_scatterfit <- function(pointsize = 1, pointalpha = 1, 
                            method = "lm", linecolor = "red", ...) {
  list(
    geom_point(size = pointsize, alpha = pointalpha, ...),
    geom_smooth(method = method, color = linecolor, ...)
  )
}


## -----------------------------------------------------------------------------
ggplot(bikes,
       aes(x = humidity, y = count)) +
  geom_scatterfit()


## -----------------------------------------------------------------------------
ggplot(bikes,
       aes(x = humidity, y = count)) +
  geom_scatterfit(
    color = "#28A87D", 
    linewidth = 3
  )


## -----------------------------------------------------------------------------
ggplot(diamonds, 
       aes(x = carat, y = price)) +
  geom_scatterfit(
    pointsize = .5, 
    pointalpha = .1,
    method = "gam",
    linecolor = "#EFAC00"
  )


## -----------------------------------------------------------------------------
scales_log <- function(sides = "xy") {
  list(
    if(stringr::str_detect(sides, "x")) {
      scale_x_log10(
        breaks = c(10^(1:100)), labels = scales::label_log()
      )
    },
    if(stringr::str_detect(sides, "y")) {
      scale_y_log10(
        breaks = c(10^(1:100)), labels = scales::label_log()
      )
    }
  )
}


## -----------------------------------------------------------------------------
ggplot(diamonds, 
       aes(x = carat, y = price)) +
  geom_scatterfit(
    pointsize = .5, 
    pointalpha = .1,
    method = "gam",
    linecolor = "#EFAC00"
  ) +
  scales_log(sides = "y")


## -----------------------------------------------------------------------------
trends_monthly <- function(grp = "January") {
  bikes |> 
    dplyr::mutate(month = lubridate::month(date, label = TRUE, abbr = FALSE)) |> 
    dplyr::filter(month %in% grp) |> 
    ggplot(aes(x = temp, y = count, color = day_night)) +
    geom_point(alpha = .2, show.legend = FALSE) +
    geom_smooth(se = FALSE) +
    scale_color_manual(values = c("#FFA200", "#757bc7")) +
    labs(title = grp, x = "Temperature", y = "Bike shares", color = NULL)
}


## -----------------------------------------------------------------------------
trends_monthly("July")


## -----------------------------------------------------------------------------
trends_monthly <- function(grp = "January") {
  bikes |> 
    dplyr::mutate(month = lubridate::month(date, label = TRUE, abbr = FALSE)) |> 
    dplyr::filter(month %in% grp) |> 
    ggplot(aes(x = temp, y = count, color = day_night)) +
    geom_point(alpha = .2, show.legend = FALSE) +
    geom_smooth(se = FALSE) +
    # keep axis ranges consistent
    scale_x_continuous(limits = range(bikes$temp)) +
    scale_y_continuous(limits = range(bikes$count)) +
    scale_color_manual(values = c("#FFA200", "#757bc7")) +
    labs(title = grp, x = "Temperature", y = "Bike shares", color = NULL)
}


## -----------------------------------------------------------------------------
trends_monthly("July")


## -----------------------------------------------------------------------------
plots <- purrr::map(month.name[1:12], trends_monthly) # also: ~ trends_monthly(.x)

plots[[9]]


## -----------------------------------------------------------------------------
patchwork::wrap_plots(plots)


## -----------------------------------------------------------------------------
plot_density <- function(data, var, grp = "") {
  ggplot(data, aes(x = !!sym(var))) +
    geom_density(aes(fill = !!sym(grp)), position = "identity",
                 color = "grey30", alpha = .3) +
    coord_cartesian(expand = FALSE, clip = "off") +
    scale_y_continuous(labels = scales::label_number()) +
    scale_fill_brewer(palette = "Dark2", name = NULL) +
    theme(legend.position = "top")
}


## -----------------------------------------------------------------------------
plot_density(
  bikes, "count"
)


## -----------------------------------------------------------------------------
plots <- purrr::map(
  c("count", "temp", "humidity", "wind_speed"), 
  ~ plot_density(data = bikes, var = .x, grp = "day_night")
)


## -----------------------------------------------------------------------------
patchwork::wrap_plots(plots, nrow = 1)


## -----------------------------------------------------------------------------
plots <- purrr::map(
  names(dplyr::select(midwest, where(is.numeric))),
  ~plot_density(data = midwest, var = .x)
)


## -----------------------------------------------------------------------------
patchwork::wrap_plots(plots)


## -----------------------------------------------------------------------------
## EXERCISE

##   -  Create a function that plots the famous Gapminder chart, highlighting 
##      one of the continents.
##       -  Extend the following code in to annotate a continent your choice of 
##          with {ggforce}.
##       -  Turn the code into a function with the utility to annotate any 
##          continent.
##       -  Bonus: Create a second function to highlight a country.

library(gapminder)

(gm2007 <- filter(gapminder, year == 2007))

ggplot(gm2007, aes(x = gdpPercap, y = lifeExp)) +
  geom_point( 
    aes(size = pop), alpha = .5
  ) +
  scale_x_log10(
    breaks = c(500, 2000, 8000, 32000),
    labels = scales::label_dollar(accuracy = 1)
  ) +
  scale_size(
    range = c(1, 12), name = "Population:", 
    breaks = c(10, 100, 1000)*1000000, 
    labels = scales::label_comma(scale = 1 / 10^6, suffix = "M")
  ) +
  labs(x = "GDP per capita", y = "Life expectancy") +
  theme_minimal(base_family = "Asap SemiCondensed") +
  theme(panel.grid.minor = element_blank())
