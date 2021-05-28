## ---- start
please_give_me_an_answer <- function(x) {
  rlang::abort("Go find out yourself!")
}
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

