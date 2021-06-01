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

## ---- expl
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

## ---- prelim
library(MASS)
myopia_lda <- lda(myopic ~ ., data = myopia_2, prior = c(.76, .24))
myopia_qda <- qda(myopic ~ ., data = myopia_2, prior = c(.76, .24))

## ---- loo-lda
get_cm <- function(data, y, .f, prior) {
  y <- substitute(y)
  table(
    actual = data[[deparse(y)]],
    predicted = purrr::map(
      seq_len(nrow(data)),
      function(i) {
        fit <- .f(
          formula(paste(deparse(y), "~ .")),
          data = data[-i, ],
          prior = prior
        )
        as.character(predict(fit, data[i, ])[["class"]])
      }
    ) %>%
      purrr::set_names(seq_len(nrow(data))) %>%
      dplyr::bind_rows() %>%
      t()
  )
}
get_cm(
  myopia_2, myopic, lda,
  prior = c(.76, .24)
)

## ---- loo-qda
get_cm(
  myopia_2, myopic, qda,
  prior = c(.76, .24)
)

## ---- odds
lik_null <- mean(myopia_2[["myopic"]] == "No")
lik_null / (1 - lik_null) * .76 / .24

## ---- optim-lda
roc <- function(data, y, .f, p) {
  y <- substitute(y)
  tn_and_tp <- purrr::map(
    p,
    function(x) {
      fit <- .f(
        formula(paste(deparse(y), "~ .")),
        data = data,
        prior = c(1 - x, x)
      )
      cm <- table(
        data[[deparse(y)]],
        predict(fit)[["class"]]
      )
      diag(cm) / rowSums(cm)
    }
  ) %>%
    purrr::set_names(seq_len(length(p))) %>%
    dplyr::bind_rows()
  i_max <- which.max(rowSums(tn_and_tp))
  roc <- tn_and_tp %>%
    ggplot(aes(x = No, y = Yes)) +
    geom_line(col = "steelblue", lwd = 1) +
    ggplot2::scale_x_reverse() +
    geom_abline(slope = 1, intercept = 1) +
    geom_point(
      data = tn_and_tp[i_max, ],
      size = 2,
      col = "red"
    ) +
    geom_text(
      data = tn_and_tp[i_max, ],
      label = paste("p =", p[i_max]),
      position = position_nudge(x = .02, y = -.02),
      col = "red",
      hjust = 0
    ) +
    ggplot2::labs(
      title = paste(
        "ROC Curve of",
        toupper(deparse(substitute(.f)))
      ),
      x = "Specificity",
      y = "Sensitivity"
    ) +
    ggplot2::theme_bw()
  list(
    roc = roc,
    sensitivity = as.numeric(tn_and_tp[i_max, 2]),
    specificity = as.numeric(tn_and_tp[i_max, 1]),
    auc = with(tn_and_tp, DescTools::AUC(No, Yes)),
    p_optim = p[i_max]
  )
}
(roc_lda <- roc(myopia_2, myopic, lda, p = seq(.001, .999, .001)))[-1]

## ---- optim-qda
(roc_qda <- roc(myopia_2, myopic, qda, p = seq(.001, .999, .001)))[-1]

## ---- roc-lda
roc_lda[["roc"]]

## ---- roc-qda
roc_qda[["roc"]]

## ---- score
dplyr::bind_cols(
  predict(myopia_lda)[["x"]],
  myopic = myopia_2[["myopic"]]
) %>%
  ggplot(aes(x = LD1, y = myopic)) +
  geom_boxplot(outlier.alpha = 0) +
  geom_jitter(shape = 1, height = .02) +
  ggplot2::theme(plot.margin = unit(rep(0, 4), "in")) +
  ggplot2::labs(
    title = "Linear Discriminant Score Plot",
    y = "Is Myopic"
  )

## ---- loadings
cor(dplyr::bind_cols(
  as_tibble(predict(myopia_lda)[["x"]]),
  myopia_2[, 2:7]
))[-1, 1, drop = FALSE] %>%
  structure(class = "loadings")
