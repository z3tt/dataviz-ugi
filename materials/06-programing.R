#------------------------------------------------------------------------------#
#                                                                              #
#                  Engaging & Reproducible Data Visualization                  #
#                  From Theory to Implementation with ggplot2                  #
#                                                                              #
#              Workflows: Functional Programing & Corporate Themes             #
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
theme_grey


## -----------------------------------------------------------------------------
theme_minimal


## -----------------------------------------------------------------------------
theme_asap <- function(base_size = 13, base_family = "Asap SemiCondensed",
                       base_line_size = base_size/22, base_rect_size = base_size/22) {
  
  theme_minimal(base_size = base_size, base_family = base_family,
                base_line_size = base_line_size, base_rect_size = base_rect_size)  +
    theme(
      # add your theme changes here
    )
}


## -----------------------------------------------------------------------------
theme_asap <- function(base_size = 13, base_family = "Asap SemiCondensed", 
                       base_line_size = base_size/22, base_rect_size = base_size/22) {
  
  theme_minimal(base_size = base_size, base_family = base_family, 
                base_line_size = base_line_size, base_rect_size = base_rect_size) %replace%
    theme(
      plot.title = element_text(size = rel(1.3), margin = margin(b = base_size/2),
                                family = "Asap SemiCondensed Extrabold", hjust = 0),
      plot.title.position = "plot",
      plot.caption = element_text(color = "grey30", margin = margin(t = base_size),
                                  size = rel(0.8), hjust = 1, vjust = 1),
      plot.caption.position = "plot",
      axis.title.x = element_text(hjust = 0, vjust = 0, margin = margin(t = base_size/3)),
      axis.title.y = element_text(hjust = 1, vjust = 0, angle = 90, margin = margin(r = base_size/3)),
      panel.background = element_rect(fill = "white", color = "grey20"), 
      panel.border = element_rect(fill = NA, color = "grey20"), 
      plot.background = element_rect(fill = "grey85", color = NA), 
      legend.justification = "top",
      strip.text = element_text(size = rel(1.05), margin = margin(base_size/2, 0, base_size/2, 0)),
      panel.grid.minor = element_blank(), 
      complete = TRUE
    )
}


## -----------------------------------------------------------------------------
g <- 
  ggplot(bikes, aes(x = temp, y = count, color = day_night)) +
  geom_point(alpha = .3, size = 2) +
  scale_color_manual(values = c(day = "#FFA200", night = "#757BC7")) +
  theme_minimal(base_size = 14, base_family = "Asap SemiCondensed") +
  theme(panel.grid.minor = element_blank())


## -----------------------------------------------------------------------------
theme_asap <- function(base_size = 13, base_family = "Asap SemiCondensed", 
                            base_line_size = base_size/22, base_rect_size = base_size/22) {
  
  theme_minimal(base_size = base_size, base_family = base_family, 
                base_line_size = base_line_size, base_rect_size = base_rect_size) + 
    theme( 
      plot.title = element_text(size = rel(1.3), hjust = 0,
                                family = "Asap SemiCondensed Extrabold"),
      plot.title.position = "plot",
      plot.caption = element_text(color = "grey30", margin = margin(t = base_size)),
      plot.caption.position = "plot",
      axis.title.x = element_text(hjust = 0, margin = margin(t = base_size/3)),
      axis.title.y = element_text(hjust = 1, margin = margin(r = base_size/3)),
      panel.background = element_rect(fill = "white", color = "grey20"), 
      panel.border = element_rect(fill = NA, color = "grey20"), 
      plot.background = element_rect(fill = "grey85", color = NA), 
      legend.justification = "top",
      strip.text = element_text(size = rel(1.05), margin = margin(base_size/2, 0, base_size/2, 0)),
      panel.grid.minor = element_blank()
    )
}


## -----------------------------------------------------------------------------
g + 
  theme_asap()


## -----------------------------------------------------------------------------
g + 
  theme_asap() +
  theme(
    legend.position = "top",
    plot.background = element_rect(
      fill = NA, color = NA
    )
  )


## -----------------------------------------------------------------------------
g + 
  theme_asap(
    base_size = 9,
    base_family = "Hepta Slab"
  )


## -----------------------------------------------------------------------------
g + 
  theme_asap(
    base_size = 9,
    base_family = "Hepta Slab"
  ) +
  theme(
    plot.title = element_text(
      family = "Hepta Slab"
    )
  )


## -----------------------------------------------------------------------------
theme_asap_title <- function(base_size = 13, base_family = "Asap SemiCondensed", 
                             title_family = "Asap SemiCondensed Extrabold",
                             base_line_size = base_size/22, base_rect_size = base_size/22) {
  
  if (title_family == "Asap SemiCondensed Extrabold") {
    register_variant(name = "Asap SemiCondensed Extrabold",
                     family = "Asap SemiCondensed",
                     weight = "ultrabold")
  }
  
  theme_minimal(base_size = base_size, base_family = base_family, 
                base_line_size = base_line_size, base_rect_size = base_rect_size) + 
    theme(
      plot.title = element_text(size = rel(1.3), hjust = 0, family = title_family),
      plot.title.position = "plot",
      plot.caption = element_text(color = "grey30", margin = margin(t = base_size)),
      plot.caption.position = "plot",
      axis.title.x = element_text(hjust = 0, margin = margin(t = base_size/3)),
      axis.title.y = element_text(hjust = 1, margin = margin(r = base_size/3)),
      panel.background = element_rect(fill = "white", color = "grey20"), 
      panel.border = element_rect(fill = NA, color = "grey20"), 
      plot.background = element_rect(fill = "grey85", color = NA), 
      legend.justification = "top",
      strip.text = element_text(size = rel(1.05), margin = margin(base_size/2, 0, base_size/2, 0)),
      panel.grid.minor = element_blank()
    )
}


## -----------------------------------------------------------------------------
g +
  theme_asap_title(
    base_size = 9,
    base_family = "Hepta Slab",
    title_family = "Hepta Slab"
  )


## -----------------------------------------------------------------------------
theme_fonts <- function(base_size = 12, base_line_size = base_size/22, 
                        base_rect_size = base_size/22) {
  
  unavailable <- vector("character")
  
  if (sum(grepl("Hepta Slab", systemfonts::system_fonts()$family)) > 0) {
    systemfonts::register_variant(
      name = "Hepta Slab Extrabold",
      family = "Hepta Slab",
      weight = "ultrabold"
    )
    title_family <- "Hepta Slab Extrabold"
  } else {
    title_family <- ""
    unavailable <- c(unavailable, "Hepta Slab")
  }
  
  if (sum(grepl("Spline Sans", systemfonts::system_fonts()$family)) > 0) {
    base_family <- "Spline Sans"
  } else {
    base_family <- ""
    unavailable <- c(unavailable, "Spline Sans")
  }
  
  if (length(unavailable) > 0) {
    unavailable <- data.frame(
      name = unavailable, 
      url = paste0("https://fonts.google.com/specimen/", sub(" ", "+", unavailable))
    )
    message(paste(
      "Using system default typefaces.", 
      "For proper use, please install the following typeface(s):",
      paste0("  - ", unavailable$name, ": ", unavailable$url, collapse = "\n"),
      "Then restart your R session.",
      sep = "\n"
    ))
  }
  
  theme_asap(base_size = base_size, base_family = base_family, 
             base_line_size = base_line_size, base_rect_size = base_rect_size) + 
    theme(
      plot.title = element_text(size = rel(1.3), hjust = 0, family = title_family)
    )
}


## -----------------------------------------------------------------------------
g + theme_fonts()


## -----------------------------------------------------------------------------
theme_asap_grid <- function(base_size = 13, base_family = "Asap SemiCondensed", grid = "xy", 
                            base_line_size = base_size/22, base_rect_size = base_size/22) {
  
  out <- 
    theme_minimal(base_size = base_size, base_family = base_family, 
                  base_line_size = base_line_size, base_rect_size = base_rect_size) + 
    theme(
      plot.title = element_text(size = rel(1.3), margin = margin(b = base_size/2),
                                family = "Asap SemiCondensed Extrabold", hjust = 0), 
      plot.title.position = "plot",
      plot.caption = element_text(color = "grey30", margin = margin(t = base_size)),
      plot.caption.position = "plot",
      axis.title.x = element_text(hjust = 0, margin = margin(t = base_size/3)),
      axis.title.y = element_text(hjust = 1, margin = margin(r = base_size/3)),
      panel.background = element_rect(fill = "white", color = "grey20"), 
      panel.border = element_rect(fill = NA, color = "grey20"), 
      plot.background = element_rect(fill = "grey85", color = NA), 
      legend.justification = "top",
      strip.text = element_text(size = rel(1.05), margin = margin(base_size/2, 0, base_size/2, 0)),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.ticks = element_line(color = "grey20"),
      axis.ticks.length = unit(base_size/2, "pt")
    )
  
  if (stringr::str_detect(grid, "x|X")) {
    out <- out + theme(panel.grid.major.x = element_line(color = "grey87"),
                       axis.ticks.x = element_blank(),
                       axis.ticks.length.x = unit(base_line_size, "pt"))
  }
  if (stringr::str_detect(grid, "y|Y")) {
    out <- out + theme(panel.grid.major.y = element_line(color = "grey87"),
                       axis.ticks.y = element_blank(),
                       axis.ticks.length.y = unit(base_line_size, "pt"))
  }
  
  return(out)
}


## -----------------------------------------------------------------------------
g + theme_asap_grid()


## -----------------------------------------------------------------------------
g + 
  theme_asap_grid(
    grid = "y"
  )


## -----------------------------------------------------------------------------
g + theme_asap_grid(
  grid = "none"
)


## -----------------------------------------------------------------------------
g + 
  theme_asap_grid(
    grid = "all"
  )


## -----------------------------------------------------------------------------
theme_asap_grid <- function(base_size = 13, base_family = "Asap SemiCondensed", grid = "xy", 
                            base_line_size = base_size/22, base_rect_size = base_size/22) {
  
  if(!stringr::str_detect(grid, "none|x|X|y|Y")) stop('grid must be a character: "none" or any combination of "X", "Y", "x" and "y".')
  
  out <- 
    theme_minimal(base_size = base_size, base_family = base_family, 
                  base_line_size = base_line_size, base_rect_size = base_rect_size) + 
    theme(
      plot.title = element_text(size = rel(1.3), margin = margin(b = base_size/2),
                                family = "Asap SemiCondensed Extrabold", hjust = 0), 
      plot.title.position = "plot",
      plot.caption = element_text(color = "grey30", margin = margin(t = base_size)),
      plot.caption.position = "plot",
      axis.title.x = element_text(hjust = 0, margin = margin(t = base_size/3)),
      axis.title.y = element_text(hjust = 1, margin = margin(r = base_size/3)),
      panel.background = element_rect(fill = "white", color = "grey20"), 
      panel.border = element_rect(fill = NA, color = "grey20"), 
      plot.background = element_rect(fill = "grey85", color = NA), 
      legend.justification = "top",
      strip.text = element_text(size = rel(1.05), margin = margin(base_size/2, 0, base_size/2, 0)),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.ticks = element_line(color = "grey20"),
      axis.ticks.length = unit(base_size/2, "pt")
    )
  
  if (stringr::str_detect(grid, "x|X")) {
    out <- out + theme(panel.grid.major.x = element_line(color = "grey87"),
                       axis.ticks.x = element_blank(),
                       axis.ticks.length.x = unit(base_line_size, "pt"))
  }
  if (stringr::str_detect(grid, "y|Y")) {
    out <- out + theme(panel.grid.major.y = element_line(color = "grey87"),
                       axis.ticks.y = element_blank(),
                       axis.ticks.length.y = unit(base_line_size, "pt"))
  }
  
  return(out)
}


## -----------------------------------------------------------------------------
g +
  theme_asap_grid(
    grid = "all"
  )



## -----------------------------------------------------------------------------
## EXERCISE

## Exercise 1

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


## Exercise 2

##   -  Create a corporate or funny custom theme.
##       -  Make use of an existing complete theme to get started.
##       -  Pick a non-default font (or multiple).
##       -  Optional: Try working with font variants.
##       -  Optional: Add other helpful arguments to your `theme_*` function.
##   -  Showcase your theme using some example graphics.**
##       -  Save the plots to disk and share them with the group.
##       -  Did you add some additional arguments? 
##          Feel free to share your thoughts on "why" and "how".

