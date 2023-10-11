#------------------------------------------------------------------------------#
#                                                                              #
#                  Engaging & Reproducible Data Visualization                  #
#                  From Theory to Implementation with ggplot2                  #
#                                                                              #
#                      Working with Labels and Annotations                     #
#                                                                              #
#                              Dr. Cedric Scherer                              #
#                       RTG-UGI Workshop // October 2023                       #
#                                                                              #
#------------------------------------------------------------------------------#


## -----------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(stringr)

bikes <- readr::read_csv(
  here::here("data", "london-bikes-custom.csv"),
  col_types = "Dcfffilllddddc"
)

theme_set(theme_light(base_size = 14, base_family = "Asap SemiCondensed"))

theme_update(
  panel.grid.minor = element_blank(),
  legend.position = "top"
)


## -----------------------------------------------------------------------------
g <- ggplot(
    bikes,
    aes(x = temp, y = count,
        color = season)
  ) +
  geom_point(
    alpha = .5
  ) +
  labs(
    x = "Temperature (°C)",
    y = "Reported bike shares",
    title = "TfL bike sharing trends",
    subtitle = "Reported bike rents versus Temperature in London",
    caption = "Data: TfL",
    color = "Season:",
    tag = "1."
  )

g


## -----------------------------------------------------------------------------
g + theme(
  plot.title = element_text(face = "bold"),
  plot.title.position = "plot"
)


## -----------------------------------------------------------------------------
g + theme(
  plot.title = element_text(face = "bold"),
  plot.title.position = "plot",
  axis.text = element_text(
    color = "#28a87d"
  )
)


## -----------------------------------------------------------------------------
g + theme(
  plot.title = element_text(face = "bold"),
  plot.title.position = "plot",
  axis.text = element_text(
    color = "#28a87d",
    family = "Tabular",
    face = "italic",
    hjust = 1,
    vjust = 0,
    angle = 45,
    lineheight = 1.3, ## no effect here
    margin = margin(10, 0, 20, 0)
  )
)


## -----------------------------------------------------------------------------
g + theme(
  plot.title = element_text(face = "bold"),
  plot.title.position = "plot",
  axis.text = element_text(
    color = "#28a87d",
    family = "Tabular",
    face = "italic",
    colour = NULL,
    size = NULL,
    hjust = 1,
    vjust = 0,
    angle = 45,
    lineheight = 1.3, ## no effect here
    margin = margin(10, 0, 20, 0) ## no effect here
  ),
  axis.text.x = element_text(
    margin = margin(10, 0, 20, 0) ## trbl
  )
)


## -----------------------------------------------------------------------------
g + theme(
  plot.title = element_text(face = "bold"),
  plot.title.position = "plot",
  axis.text = element_text(
    color = "#28a87d",
    family = "Tabular",
    face = "italic",
    colour = NULL,
    size = NULL,
    hjust = 1,
    vjust = 0,
    angle = 45,
    lineheight = 1.3, ## no effect here
    margin = margin(10, 0, 20, 0) ## no effect here
  ),
  plot.tag = element_text(
    margin = margin(0, 12, -8, 0) ## trbl
  )
)


## -----------------------------------------------------------------------------
g + theme(
  plot.title = element_text(face = "bold"),
  plot.title.position = "plot",
  axis.text = element_text(
    color = "#28a87d",
    family = "Tabular",
    face = "italic",
    colour = NULL,
    size = NULL,
    hjust = 1,
    vjust = 0,
    angle = 45,
    lineheight = 1.3, ## no effect here
    margin = margin(10, 0, 20, 0), ## no effect here
    debug = TRUE
  ),
  plot.tag = element_text(
    margin = margin(0, 12, -8, 0), ## trbl
    debug = TRUE
  )
)


## -----------------------------------------------------------------------------
g <- g + labs(tag = NULL, title = NULL, 
              subtitle = NULL)

g +
  scale_y_continuous(
    breaks = 0:4*15000
  )


## -----------------------------------------------------------------------------
g +
  scale_y_continuous(
    breaks = 0:4*15000,
    labels = scales::comma_format()
  )


## -----------------------------------------------------------------------------
g +
  scale_y_continuous(
    breaks = 0:4*15000,
    labels = scales::comma_format(
      suffix = " bikes"
    ),
    name = NULL
  )


## -----------------------------------------------------------------------------
g +
  scale_y_continuous(
    breaks = 0:4*15000,
    labels = scales::comma_format(
      suffix = "\nbikes shared"
    ),
    name = NULL
  ) +
  theme(
    axis.text.y = element_text(
      hjust = .5
    )
  )


## -----------------------------------------------------------------------------
g +
  scale_y_continuous(
    breaks = 0:4*15000,
    labels = scales::comma_format(
      scale = .001
    ),
    name = "Reported bike shares in thousands"
  )


## -----------------------------------------------------------------------------
g +
  scale_y_continuous(
    breaks = 0:4*15000,
    labels = function(y) y / 1000,
    name = "Reported bike shares in thousands",
  )


## -----------------------------------------------------------------------------
g +
  scale_x_continuous(
    labels = function(y) paste0(y, "°C"),
    name = "Temperature"
  )


## -----------------------------------------------------------------------------
ggplot(
    bikes,
    aes(x = season, y = count)
  ) +
  geom_boxplot() +
  scale_x_discrete()


## -----------------------------------------------------------------------------
ggplot(
    bikes,
    aes(x = season, y = count)
  ) +
  geom_boxplot() +
  scale_x_discrete(
    name = NULL,
    labels = stringr::str_to_title
  )


## -----------------------------------------------------------------------------
g +
  scale_color_discrete(
    name = NULL,
    labels = stringr::str_to_title
  )


## -----------------------------------------------------------------------------
# install.packages("ggtext")

g +
  ggtitle("**TfL bike sharing trends by _season_**")


## -----------------------------------------------------------------------------
# install.packages("ggtext")

g +
  ggtitle("**TfL bike sharing trends by _season_**") +
  theme(
    plot.title = ggtext::element_markdown()
  )


## -----------------------------------------------------------------------------
# install.packages("ggtext")

g +
  ggtitle("<b style='font-size:25pt'>TfL</b> bike sharing trends by <i style='color:#28a87d;'>season</i>") +
  theme(
    plot.title = ggtext::element_markdown()
  )


## -----------------------------------------------------------------------------
ggplot(
    bikes,
    aes(x = weather_type,
        y = count)
  ) +
  geom_boxplot()


## -----------------------------------------------------------------------------
ggplot(
    bikes,
    aes(x = stringr::str_wrap(weather_type, 6),
        y = count)
  ) +
  geom_boxplot()


## -----------------------------------------------------------------------------
g +
  ggtitle("TfL bike sharing trends in 2015 and 2016 by season for day and night periods") +
  theme(
    plot.title = element_text(size = 20),
    plot.title.position = "plot"
  )


## -----------------------------------------------------------------------------
g +
  ggtitle("TfL bike sharing trends in 2015 and 2016 by season for day and night periods") +
  theme(
    plot.title =
      ggtext::element_textbox_simple(size = 20),
    plot.title.position = "plot"
  )


## -----------------------------------------------------------------------------
g +
  ggtitle("TfL bike sharing trends in 2015 and 2016 by season for day and night periods") +
  theme(
    plot.title = ggtext::element_textbox_simple(
      margin = margin(t = 12, b = 12),
      lineheight = .9
    ),
    plot.title.position = "plot"
  )


## -----------------------------------------------------------------------------
g +
  ggtitle("TfL bike sharing trends in 2015 and 2016 by season for day and night periods") +
  theme(
    plot.title = ggtext::element_textbox_simple(
      margin = margin(t = 12, b = 12),
      fill = "grey90",
      lineheight = .9
    ),
    plot.title.position = "plot"
  )


## -----------------------------------------------------------------------------
g +
  ggtitle("TfL bike sharing trends in 2015 and 2016 by season for day and night periods") +
  theme(
    plot.title = ggtext::element_textbox_simple(
      margin = margin(t = 12, b = 12),
      padding = margin(rep(12, 4)),
      fill = "grey90",
      box.colour = "grey30",
      linetype = "13",
      r = unit(9, "pt"),
      halign = .5,
      face = "bold",
      lineheight = .9
    ),
    plot.title.position = "plot"
  )


## -----------------------------------------------------------------------------
ga <- 
  ggplot(bikes, 
         aes(x = temp, y = count)) +
  geom_point(
    aes(color = count > 40000),
    size = 2
  ) +
  scale_color_manual(
    values = c("grey", "firebrick"),
    guide = "none"
  )

ga


## -----------------------------------------------------------------------------
ga +
  annotate(
    geom = "text",
    x = 18,
    y = 48000,
    label = "What happened here?"
  )


## -----------------------------------------------------------------------------
ga +
  annotate(
    geom = "text",
    x = 18,
    y = 48000,
    label = "What happened here?",
    color = "firebrick",
    size = 6,
    family = "Asap SemiCondensed",
    fontface = "bold",
    lineheight =  .8
  )


## -----------------------------------------------------------------------------
ga +
  annotate(
    geom = "text",
    x = c(18, max(bikes$temp)),
    y = c(48000, 1000),
    label = c("What happened here?", "Powered by TfL"),
    color = c("firebrick", "black"),
    size = c(6, 3),
    family = c("Asap SemiCondensed", "Hepta Slab"),
    fontface = c("bold", "plain"),
    hjust = c(.5, 1)
  )


## -----------------------------------------------------------------------------
## ggannotate::ggannotate(g)


## -----------------------------------------------------------------------------
ga + 
  annotate(
    geom = "text",
    x = 19.5,
    y = 42000,
    label = "What happened here?",
    family = "Asap SemiCondensed",
    size = 6,
    vjust = 1.3
  ) +
  annotate(
    geom = "rect",
    xmin = 17, 
    xmax = 22,
    ymin = 42000, 
    ymax = 54000,
    color = "firebrick", 
    fill = NA
  )


## -----------------------------------------------------------------------------
ga +
  annotate(
    geom = "text",
    x = 10,
    y = 38000,
    label = "The\nhighest\ncount",
    family = "Asap SemiCondensed",
    size = 6,
    lineheight =  .8
  ) +
  annotate(
    geom = "segment",
    x = 13, 
    xend = 18.2,
    y = 38000, 
    yend = 51870
  )


## -----------------------------------------------------------------------------
ga +
  annotate(
    geom = "text",
    x = 10,
    y = 38000,
    label = "The\nhighest\ncount",
    family = "Asap SemiCondensed",
    size = 6,
    lineheight =  .8
  ) +
  annotate(
    geom = "curve",
    x = 13, 
    xend = 18.2,
    y = 38000, 
    yend = 51870
  )


## -----------------------------------------------------------------------------
ga +
  annotate(
    geom = "text",
    x = 10,
    y = 38000,
    label = "The\nhighest\ncount",
    family = "Asap SemiCondensed",
    size = 6,
    lineheight =  .8
  ) +
  annotate(
    geom = "curve",
    x = 13, 
    xend = 18.2,
    y = 38000, 
    yend = 51870,
    curvature = .25,
    arrow = arrow()
  )


## -----------------------------------------------------------------------------
ga +
  annotate(
    geom = "text",
    x = 10,
    y = 38000,
    label = "The\nhighest\ncount",
    family = "Asap SemiCondensed",
    size = 6,
    lineheight =  .8
  ) +
  annotate(
    geom = "curve",
    x = 13, 
    xend = 18.2,
    y = 38000, 
    yend = 51870,
    curvature = .25,
    arrow = arrow(
      length = unit(10, "pt"),
      type = "closed",
      ends = "both"
    )
  )


## -----------------------------------------------------------------------------
ga +
  annotate(
    geom = "text",
    x = 10,
    y = 38000,
    label = "The\nhighest\ncount",
    family = "Asap SemiCondensed",
    size = 6,
    lineheight =  .8
  ) +
  annotate(
    geom = "curve",
    x = 13, 
    xend = 18.2,
    y = 38000, 
    yend = 51870,
    curvature = .8,
    angle = 130,
    arrow = arrow(
      length = unit(10, "pt"),
      type = "closed"
    )
  )


## -----------------------------------------------------------------------------
gh <- 
  ggplot(
    data = filter(bikes, temp >= 27),
    aes(x = date, y = temp)
  ) +
  geom_point(
    data = bikes,
    color = "grey65", alpha = .3
  ) +
  geom_point(size = 2.5)

gh


## -----------------------------------------------------------------------------
gh +
  geom_text(
    aes(label = format(date, "%m/%d")),
    nudge_x = 10,
    hjust = 0
  )


## -----------------------------------------------------------------------------
gh +
  geom_label(
    aes(label = format(date, "%m/%d")),
    nudge_x = .3,
    hjust = 0
  )


## -----------------------------------------------------------------------------
set.seed(20230918)

gh +
  ggrepel::geom_text_repel(
    aes(label = format(date, "%m/%d"))
  )


## -----------------------------------------------------------------------------
set.seed(20230918)

gh + 
  ggrepel::geom_text_repel(
    aes(label = format(date, "%m/%d")),
    family = "Spline Sans Mono",
    size = 4.5,
    fontface = "bold"
  )


## -----------------------------------------------------------------------------
gh +
  ggrepel::geom_text_repel(
    aes(label = format(date, "%m/%d")),
    family = "Spline Sans Mono",
    # space between points + labels
    box.padding = .8,
    # always draw segments
    min.segment.length = 0
  )


## -----------------------------------------------------------------------------
gh +
  ggrepel::geom_text_repel(
    aes(label = format(date, "%y/%m/%d")),
    family = "Spline Sans Mono",
    # force to the right
    xlim = c(NA, as.Date("2015-06-01")), 
    hjust = 1
  )


## -----------------------------------------------------------------------------
gh +
  ggrepel::geom_text_repel(
    aes(label = format(date, "%y/%m/%d")),
    family = "Spline Sans Mono",
    xlim = c(NA, as.Date("2015-06-01")),
    # style segment
    segment.curvature = .01,
    arrow = arrow(length = unit(.02, "npc"), type = "closed")
  )


## -----------------------------------------------------------------------------
gh +
  ggrepel::geom_text_repel(
    aes(label = format(date, "%y/%m/%d")),
    family = "Spline Sans Mono",
    xlim = c(NA, as.Date("2015-06-01")),
    # style segment
    segment.curvature = .001,
    segment.inflect = TRUE
  )


## -----------------------------------------------------------------------------
g +
  ggforce::geom_mark_rect(
    aes(label = "Outliers?",
        filter = count > 40000)
  )


## -----------------------------------------------------------------------------
g +
  ggforce::geom_mark_rect(
    aes(label = "Outliers?",
        filter = count > 40000),
    color = "black",
    label.family = "Asap SemiCondensed"
  )


## -----------------------------------------------------------------------------
g +
  ggforce::geom_mark_rect(
    aes(label = "Outliers?",
        filter = count > 40000),
    description = "What happened on\nthese two days?",
    color = "black",
    label.family = "Asap SemiCondensed"
  )


## -----------------------------------------------------------------------------
g +
  ggforce::geom_mark_rect(
    aes(label = "Outliers?",
        filter = count > 40000),
    description = "What happened on\nthese two days?",
    color = "black",
    label.family = "Asap SemiCondensed",
    expand = unit(8, "pt"),
    radius = unit(12, "pt"),
    con.cap = unit(0, "pt"),
    label.buffer = unit(15, "pt"),
    con.type = "straight",
    label.fill = "transparent"
  )


## -----------------------------------------------------------------------------
g +
  ggforce::geom_mark_circle(
    aes(label = "Outliers?",
        filter = count > 40000),
    description = "What happened on\nthese two days?",
    color = "black",
    label.family = "Asap SemiCondensed",
    expand = unit(8, "pt"),
    con.cap = unit(0, "pt"),
    label.buffer = unit(15, "pt"),
    con.type = "straight",
    label.fill = "transparent"
  )


## -----------------------------------------------------------------------------
g +
  ggforce::geom_mark_hull(
    aes(label = "Outliers?",
        filter = count > 40000),
    description = "What happened on\nthese two days?",
    color = "black",
    label.family = "Asap SemiCondensed",
    expand = unit(8, "pt"),
    con.cap = unit(0, "pt"),
    label.buffer = unit(15, "pt"),
    con.type = "straight",
    label.fill = "transparent"
  )


## -----------------------------------------------------------------------------
bikes |>
  filter(year == "2016") |>
  group_by(month, day_night) |> 
  summarize(count = sum(count)) |> 
  ggplot(aes(x = month, y = count, 
             color = day_night,
             group = day_night)) +
  geom_line(linewidth = 1) +
  coord_cartesian(expand = FALSE) +
  scale_y_continuous(
    labels = scales::label_comma(
      scale = 1/10^3, suffix = "K"
    ),
    limits = c(0, 850000)
  ) +
  scale_color_manual(
    values = c("#FFA200", "#757BC7"),
    name = NULL
  )


## -----------------------------------------------------------------------------
bikes |>
  filter(year == "2016") |>
  group_by(month, day_night) |> 
  summarize(count = sum(count)) |> 
  ggplot(aes(x = month, y = count, 
             color = day_night,
             group = day_night)) +
  geomtextpath::geom_textline(
    aes(label = day_night),
    linewidth = 1,
    vjust = -.5, 
    family = "Asap SemiCondensed",
    fontface = "bold"
  ) +
  coord_cartesian(expand = FALSE) +
  scale_y_continuous(
    labels = scales::label_comma(
      scale = 1/10^3, suffix = "K"
    ),
    limits = c(0, 850000)
  ) +
  scale_color_manual(
    values = c("#FFA200", "#757BC7"),
    guide = "none"
  )


## -----------------------------------------------------------------------------
bikes |>
  filter(year == "2016") |>
  group_by(month, day_night) |> 
  summarize(count = sum(count)) |> 
  mutate(day_night = if_else(
    day_night == "day", 
    "Day period (6am-6pm)", 
    "Night period (6pm-6am)"
  )) |> 
  ggplot(aes(x = month, y = count, 
             color = day_night,
             group = day_night)) +
  geomtextpath::geom_textline(
    aes(label = day_night),
    linewidth = 1,
    vjust = -.5, 
    hjust = .01,
    family = "Asap SemiCondensed",
    fontface = "bold"
  ) +
  coord_cartesian(expand = FALSE) +
  scale_y_continuous(
    labels = scales::label_comma(
      scale = 1/10^3, suffix = "K"
    ),
    limits = c(0, 850000)
  ) +
  scale_color_manual(
    values = c("#FFA200", "#757BC7"),
    guide = "none"
  )


## -----------------------------------------------------------------------------
bikes |>
  filter(year == "2016") |>
  ggplot(aes(x = month, y = count, 
             color = day_night,
             group = day_night)) +
  stat_summary(
    geom = "line", fun = sum,
    linewidth = 1
  ) +
  geomtextpath::geom_textline(
    aes(label = day_night), 
    stat = "summary" # fails
  ) +
  coord_cartesian(expand = FALSE) +
  scale_y_continuous(
    labels = scales::label_comma(
      scale = 1/10^3, suffix = "K"
    ),
    limits = c(0, 850000)
  ) +
  scale_color_manual(
    values = c("#FFA200", "#757BC7"),
    name = NULL
  )


## -----------------------------------------------------------------------------
url <- "https://www.gs.tum.de/fileadmin/_processed_/0/7/csm_UGI_Logo_frei_ba00944e1c.png"
img <- magick::image_read(url)

img


## -----------------------------------------------------------------------------
ggplot(bikes, aes(date, temp)) +
  annotation_custom(
    grid::rasterGrob(
      image = img
    )
  ) +
  geom_point(color = "#308DD1")


## -----------------------------------------------------------------------------
ggplot(bikes, aes(date, temp)) +
  annotation_custom(
    grid::rasterGrob(
      image = img,
      x = .5,
      y = .75,
      width = .3
    )
  ) +
  geom_point(color = "#308DD1") +
  ylim(NA, 35)


## -----------------------------------------------------------------------------
ggplot(bikes, aes(date, temp)) +
  annotation_custom(
    grid::rasterGrob(
      image = img,
      x = .9,
      y = 1.2,
      width = .3
    )
  ) +
  geom_point(color = "#308DD1") +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(120, 10, 10, 10)
  )



## EXERCISES

## Exercise 1
##
##   -  Take a look at the following visualization.
##       -  For each group of text labels, note how one would add and modify them.
##       -  Discuss how to automate the placement of the labels in- and outside 
##          of the bars.


## Exercise 2
##
##   -  {ggtext} also comes with some new geom's. Explore those and other options 
##      on the package webpage: https://wilkelab.org/ggtext.
##   -  {ggforce} comes with a variety of interesting geometries and utilities. 
##      Take a look at this wonderful toolbox: https://ggforce.data-imaginist.com. 
##      Share potentially helpful features with the group.

