test_that("Data", {
  data = readr::read_csv("earthquakes_-2150to2024.csv", col_names = T, progress = FALSE)
  expect_equal(data$`Location Name`[[1]], "JORDAN:  BAB-A-DARAA,AL-KARAK")
})

test_that("eq_location_clean", {
  data = readr::read_csv("earthquakes_-2150to2024.csv", col_names = T, progress = FALSE)
  data = eq_location_clean("earthquakes_-2150to2024.csv")
  expect_equal(data$`Location Name`[[1]], "Bab-A-Daraa,Al-Karak")
})

test_that("eq_clean_data", {
  data = readr::read_csv("earthquakes_data_clean1.csv", col_names = T, progress = FALSE)
  data = eq_clean_data("earthquakes_data_clean1.csv")
  expect_equal(data$`Location Name`[[1]], "Nisa")
})

test_that("eq_filter_data", {
  data = readr::read_csv("earthquakes_data_clean2.csv", col_names = T, progress = FALSE)
  data = eq_filter_data("earthquakes_data_clean2.csv", xmin = as.Date("1950-01-01", "%Y-%m-%d"),
                        xmax = as.Date("1955-01-01", "%Y-%m-%d"), c("Date", "xend", "y", "yend", "Mag", "Deaths", "Country"))
  expect_equal(nrow(data), 36)
})

test_that("GeomTimeline", {
  expect_equal(attr(GeomTimeline, "dim"), NULL)
})

test_that("geom_timeline", {
  data = readr::read_csv("earthquakes_data_final.csv", col_names = T, progress = FALSE)
  data = ggplot2::ggplot() + geom_timeline(data=data, ggplot2::aes(Date, y=0.2, size=Mag, colour=Deaths), alpha=0.3)
  expect_equal(data$labels$x, "Date")
})

test_that("GeomTimelineLabel", {
  expect_equal(attr(GeomTimeline, "dim"), NULL)
})

test_that("geom_timeline_label", {
  data = readr::read_csv("earthquakes_data_final.csv", col_names = T, progress = FALSE)
  data = ggplot2::ggplot() + geom_timeline_label(data=data, ggplot2::aes(Date, y=0.2, xend=xend, yend=yend, size=Mag,
                                                                         colour=Deaths,label=Country), alpha=0.3)
  expect_equal(data$labels$colour, "Deaths")
})

test_that("eq_map", {
  data = readr::read_csv("earthquakes_data_clean2.csv", col_names = T, progress = FALSE)
  data = dplyr::filter(data, Country == "MEXICO" & lubridate::year(Date) >= 2000)
  library(magrittr)
  data = eq_map(data, annot_col="Date", -120, -60, 0, 40)
  expect_equal(data$x$fitBounds[[2]], -120)
})

test_that("eq_create_label", {
  data = readr::read_csv("earthquakes_data_clean2.csv", col_names = T, progress = FALSE)
  library(magrittr)
  data = dplyr::filter(data, Country == "MEXICO" & lubridate::year(Date) >= 2000) %>%
    dplyr::mutate(popup_text = eq_create_label(.))
  expect_equal(colnames(data[,38]), "popup_text")
})
