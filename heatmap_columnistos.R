library(tidyverse)
library(lubridate)
library(magrittr)
library(plotly)
library(scales)
library(htmlwidgets)
library(htmltools)

Sys.setlocale("LC_TIME", "English")

articulos_fecha <- read_csv("https://raw.githubusercontent.com/columnistos/dump/master/ar/articulos.csv")

median_week <- function(month_i, data_wm) {
  median_week <-
    data_wm %>%
    dplyr::filter(month_name == month_i) %>%
    pull(week_num) %>%
    unique() %>%
    as.integer() %>%
    median() %>%
    round() %>%
    as.integer()

  median_week
}

compute_hover <- function(matrix_heatmap_ij, date_ij, gender) {
  Sys.setlocale("LC_TIME", "Spanish")
  hover <- ""
  if (matrix_heatmap_ij == 0) {
    if (gender == "M") {
      hover <- "Ningun varón opinó el"
    } else {
      hover <- "Ninguna mujer opinó el"
    }
  } else if (matrix_heatmap_ij == 1) {
    if (gender == "M") {
      hover <- "1 varón opinó el"
    } else {
      hover <- "1 mujer opinó el"
    }
  } else {
    if (gender == "M") {
      hover <- paste0(matrix_heatmap_ij, " varones opinaron el")
    } else {
      hover <- paste0(matrix_heatmap_ij, " mujeres opinaron el")
    }
  }
  return(paste0(hover, " ", day(date_ij), " de ", month(date_ij, label = TRUE, abb = FALSE, locale = Sys.getlocale("LC_TIME"))))
}

data_mujeres <- articulos_fecha %>%
  mutate(
    fecha = date(added),
    anio = year(added),
    mujer_bool = (gender == "F")
  ) %>%
  filter(
    mujer_bool == TRUE,
    fecha >= as_date("2017-12-31"),
    fecha <= as_date("2018-12-29")
  ) %>%
  select(fecha)

data_varones <- articulos_fecha %>%
  mutate(
    fecha = date(added),
    anio = year(added),
    mujer_bool = (gender == "M")
  ) %>%
  filter(
    mujer_bool == TRUE,
    fecha >= as_date("2017-12-31"),
    fecha <= as_date("2018-12-29")
  ) %>%
  select(fecha)

datos <- data_mujeres
genero <- "F"

all_dates_period <-
  tibble(gen_date = as_date(as_date("2017-12-31"):as_date("2018-12-29"))) %>%
  mutate(
    month_name = month(gen_date, TRUE),
    week_num = epiweek(gen_date), # to  start on Sun
    week_day = wday(gen_date, TRUE)
  )

base_to_heatmap <- all_dates_period %>%
  select(-month_name, -gen_date)

data_calendar <- datos %>%
  select(fecha) %>%
  mutate(
    month_name = month(fecha, TRUE),
    week_num = epiweek(fecha), # to  start on Sun
    week_day = wday(fecha, TRUE)
  ) %>%
  arrange(fecha) %>%
  group_by(week_num, week_day) %>%
  summarize(
    month_name = first(month_name),
    n = dplyr::n(),
    or_date = first(fecha)
  )

data_to_heatmap <- data_calendar %>%
  select(-month_name, -or_date)

to_heatmap <-
  base_to_heatmap %>%
  left_join(data_to_heatmap, by = c(
    "week_num" = "week_num",
    "week_day" = "week_day"
  )) %>%
  spread(key = week_num, value = n) %>%
  as.data.frame() %>%
  column_to_rownames("week_day") %>%
  replace(., is.na(.), 0)

# axis labels ####

median_weeks <- purrr::map_int(month.abb, median_week, data_wm = all_dates_period)

all_dates_period$month_name
all_dates_period$week_day

setNames(median_weeks, month.abb)

row_days <- rownames(to_heatmap)
lab_row_days <- rev(rownames(to_heatmap))
lab_row_days[c(1, 3, 5, 7)] <- rep(" ", 4)


data_wm_worep <-
  all_dates_period %>%
  select(week_num, month_name) %>%
  distinct() %>%
  group_by(week_num) %>%
  summarize(month_name = last(month_name))

column_months <- as.character(data_wm_worep$month_name)
lab_column_months <- as.character(data_wm_worep$month_name)
lab_column_months[!data_wm_worep$week_num %in% median_weeks] <- " "

matrix_heatmap <- as.matrix(to_heatmap)
matrix_heatmap <- matrix_heatmap[ nrow(matrix_heatmap):1, ]

# hover ####

get_date_weeknum_daynum_year <- function(dataset, weeknum, weekday) {
  fechas <- dataset %>%
    dplyr::filter(
      week_num == weeknum,
      week_day == weekday
    ) %$%
    gen_date
  return(fechas)
}

week_numbers <- unique(all_dates_period$week_num)
matrix_dates <- matrix(rep(NA, 7 * ncol(to_heatmap)), nrow = 7)
for (i in 1:7) {
  for (j in 1:ncol(to_heatmap)) {
    matrix_dates[i, j] <- get_date_weeknum_daynum_year(
      all_dates_period,
      week_numbers[j],
      row_days[i]
    )
  }
}

matrix_dates <- matrix_dates[nrow(matrix_dates):1, ]

dates_vec <- (lubridate::as_date(as.vector(matrix_dates), origin = lubridate::origin))
hover_heatmap <- matrix(purrr::map2_chr(
  as.vector(matrix_heatmap),
  dates_vec,
  ~ compute_hover(.x, .y, genero)
),
nrow = 7
)

# github colors:
# grey 0 #ebedf0
# light green #c6e48b
# green #7bc96f
# super green #239a3b
# super super green #196127

mycols <- c("#ebedf0", "#c6e48b", "#7bc96f", "#239a3b", "#196127")


vals <- unique(scales::rescale(c(matrix_heatmap)))
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric(mycols, domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)

interactive_plot_2 <- plot_ly(
  z = matrix_heatmap,
  type = "heatmap",
  colorscale = colz, xgap = 0.6, ygap = 0.6,
  showscale = FALSE, hoverinfo = "text",
  text = hover_heatmap, zmin = 0, zmax = 20
) %>%
  layout(
    yaxis = list(
      ticktext = lab_row_days, ticks = "",
      tickmode = "array",
      tickfont = list(color = "86888A"),
      tickvals = 0:6,
      zeroline = FALSE
    ),
    xaxis = list(
      side = "top", ticktext = lab_column_months, ticks = "",
      tickmode = "array", tickangle = 0,
      tickfont = list(color = "86888A"),
      tickvals = 0:51,
      zeroline = FALSE
    )
  ) %>%
  config(displayModeBar = FALSE)

interactive_plot_1$sizingPolicy$padding <- "0"
interactive_plot_2$sizingPolicy$padding <- "0"

lista_plots <- list(interactive_plot_2, interactive_plot_1)

htmlwidgets::saveWidget(
  interactive_plot, "index.html",
  libdir = "lib",
  title = "Iris Flowers Interactive",
  selfcontained = FALSE
)

htmltools::save_html(lista_plots, "prueba3.html")
