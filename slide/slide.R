## ---- start
please_give_me_an_answer <- function() rlang::abort("Go find out yourself!")
please_give_me_an_answer()

## ---- data
library(tidyverse)
library(janitor)
myopia <- readr::read_csv(
  "../data/myopia.csv",
  col_types = "__ld______ddddd___"
) %>%
  janitor::clean_names() %>%
  dplyr::mutate(across(
    where(is.logical),
    function(x) case_when(x ~ "Yes", !x ~ "No")
  ))
myopia

## ---- data-def
knitr::kable(
  tibble(
    "Variable Name" = names(myopia),
    Unit = c("boolean", "years", rep("hours per week", 5)),
    Description = c(
      "Myopia within the first five years of follow up",
      "Age at first visit",
      "Time spent engaging in sports/outdoor activities",
      "Time spent reading for pleasure",
      "Time spent playing video/computer games or working on the computer",
      "Time spent reading or studying for school assignments",
      "Time spent watching television"
    )
  ),
  format = "html",
  align = c("l", "c", "l")
)

## ---- prelim
library(GGally)
myopia %>%
  GGally::ggpairs(
    ggplot2::aes(col = myopic),
    columns = 2:7,
    upper = list(continuous = wrap("cor", size = 2))
  )

## ---- minimum
summary(myopia[-1])

## ---- overfix
myopia %>%
  dplyr::mutate(
    log_age = log(age),
    across(-(1:2), log1p, .names = "log1p_{col}"),
  ) %>%
  dplyr::select(-(2:7)) %>%
  GGally::ggpairs(
    ggplot2::aes(col = myopic),
    columns = 2:7,
    upper = list(continuous = wrap("cor", size = 2))
  ) +
  ggplot2::theme(strip.text.y = element_text(size = 4)) +
  ggplot2::labs(
    title = "Transformed Data",
    subtitle = expression(
      "By" ~ x %->% log(1 + x) ~
      "for all" ~ x ~
      "except for" ~
      age %->% log(age)
    )
  )

## ---- data-trans
(myopia_2 <- myopia %>%
  dplyr::mutate(
    across(4:6, log1p, .names = "log1p_{col}"),
    across(c(3, 7), sqrt, .names = "sqrt_{col}")
  ) %>%
  dplyr::select(-(3:7))) %>%
  GGally::ggpairs(
    ggplot2::aes(col = myopic),
    columns = 2:7,
    upper = list(continuous = wrap("cor", size = 2))
  ) +
  ggplot2::theme(strip.text.y = element_text(size = 4)) +
  ggplot2::labs(
    title = "Transformed Data",
    subtitle = expression(
      "By" ~ x %->% log(1 + x) ~
      "for readhr, comphr, studyhr and" ~
      x %->% sqrt(x) ~
      "for sporthr, tvhr"
    )
  )

## ---- effect
library(colorspace)
quantile_f <- function(p) {
  purrr::map(
    p,
    function(p) {
      eval(parse(
        text = paste0("function(x) quantile(x, probs = ", p, ", na.rm = TRUE)")
      ))
    }
  ) %>% purrr::set_names(p)
}
myopia_2 %>%
  tidyr::pivot_longer(-1, "var") %>%
  dplyr::group_by(myopic, var) %>%
  dplyr::summarise(across(value, quantile_f(seq(0, 1, .001)))) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_longer(
    dplyr::starts_with(paste0("value", "_")),
    names_to = ".quantile",
    values_to = ".value"
  ) %>%
  dplyr::mutate(.quantile = 100 * as.numeric(
    gsub(paste0("value", "_"), "", .quantile)
  )) %>%
  ggplot(aes(myopic, .value, col = .quantile, group = .quantile)) +
  geom_point() +
  geom_line() +
  ggplot2::facet_wrap(~var, scales = "free_y") +
  ggplot2::labs(col = "Quantile", x = "Is Myopic", y = "") +
  colorspace::scale_colour_continuous_diverging(
    mid = 50, c1 = 120
  )

## ---- eov
myopia_2[-1] %>%
  array_branch(2) %>%
  map_dbl(var) %>%
  as.matrix() %>%
  round(2)

## ---- vif
cor(myopia_2[-1]) %>%
  solve() %>%
  diag() %>%
  as.matrix() %>%
  round(2)
