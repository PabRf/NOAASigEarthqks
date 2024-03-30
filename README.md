
# NOAASigEarthqks

<!-- badges: start -->
[![R-CMD-check](https://github.com/PabRf/NOAASigEarthqks/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PabRf/NOAASigEarthqks/actions/workflows/R-CMD-check.yaml)
[![Build Status](https://app.travis-ci.com/PabRf/NOAASigEarthqks.svg?token=KMwuCq2izC8DxPixs6uj&branch=main)](https://app.travis-ci.com/PabRf/NOAASigEarthqks)
<!-- badges: end -->

The goal of NOAASigEarthqks is to provide tools for processing and visualizing the NOAA data so users may extract useful information embedded within.

## Installation

You can install the development version of NOAASigEarthqkDb like so:

``` r
install.packages("devtools")
install.packages("PabRf/NOAASigEarthqks")
```

## Example

These are basic examples which shows you how to use the functions:

``` r
library(NOAASigEarthqkDb)

eq_location_clean("earthquakes_-2150to2024.csv")

eq_clean_data("earthquakes_data_clean1.csv")

eq_filter_data("earthquakes_data_clean2.csv", xmin = as.Date("1950-01-01", "%Y-%m-%d"), xmax = as.Date("1955-01-01", "%Y-%m-%d"), c("Date", "xend", "y", "yend", "Mag", "Deaths", "Country"))

ggplot() + geom_timeline(data=earthquakes_data_final, aes(Date, y=0.2, size=Mag, colour=Deaths), alpha=0.3)

ggplot() + geom_timeline(data=earthquakes_data_final, aes(Date, Country, size=Mag, colour=Deaths), alpha=0.3)

ggplot() + geom_timeline_label(data=earthquakes_data_final, aes(Date, y=0.2, xend=xend, yend=yend, size=Mag, colour=Deaths, label=Country), alpha=0.3) +
    labs(size = "Richter Scale", colour = ("Deaths")) +
    ylim(c(0,1)) + geom_hline(aes(yintercept = 0.2)) +
    annotate("text", x=earthquakes_data_final$Date, y=0.42, label=earthquakes_data_final$Country, vjust=0,
             hjust=-0, size=2, angle=45) +
    theme(plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white", colour = "white", linewidth = 0.5, linetype = "solid"),
          panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "white"),
          axis.title.y = element_text(colour = "white"), axis.text.y = element_text(colour="white"), axis.ticks.y = element_line(colour = "white"),
          axis.line.x = element_line(colour = "black"),
          plot.title = element_text(colour = "white", hjust = 0.5)) +
    theme(legend.position="bottom") +
    theme(axis.text.x=element_text(angle=60, hjust=1)) +
    guides(size = guide_legend(order = 1), colour = guide_colorbar(order = 2), alpha="none")

ggplot() + geom_timeline_label(data=earthquakes_data_final, aes(Date, Country, xend=xend, yend=yend, size=Mag, colour=Deaths, label=Country), alpha=0.3) +
    labs(size = "Richter Scale", colour = ("Deaths")) +
    geom_hline(aes(yintercept = earthquakes_data_final$Country)) +
    annotate("text", x=earthquakes_data_final$Date, y=earthquakes_data_final$Country, label=earthquakes_data_final$Country, vjust=0, hjust=-0.2, size=2, angle=45) +
    theme(plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white", colour = "white", linewidth = 0.5, linetype = "solid"),
          panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "white"),
          axis.title.y = element_text(colour = "white"), axis.text.y = element_text(colour="white"),
          axis.ticks.y = element_line(colour = "white"), axis.line.x = element_line(colour = "black"),
          plot.title = element_text(colour = "white", hjust = 0.5)) +
    theme(legend.position="bottom") +
    theme(axis.text.x=element_text(angle=60, hjust=1)) +
    guides(size = guide_legend(order = 1), colour = guide_colorbar(order = 2), alpha="none")

library(magrittr)
dplyr::filter(earthquakes_data_clean2, Country == "MEXICO" & lubridate::year(Date) >= 2000) %>%
  eq_map(annot_col= ~Date, -120, -60, 0, 40)

dplyr::filter(earthquakes_data_clean2, Country == "MEXICO" & lubridate::year(Date) >= 2000) %>%
  dplyr::mutate(popup_text = eq_create_label(.)) %>%
  eq_map(annot_col= ~popup_text, -120, -60, 0, 40)

  
```

