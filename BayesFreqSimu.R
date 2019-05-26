library(rvest)
library(readr)
library(tidyverse)
library(ggplot2)
library(ggstatsplot)
library(BayesFactor)

#reference
#https://ibecav.netlify.com/post/comparing-frequentist-bayesian-and-simulation-methods-and-conclusions/

#### IMPORTING DATA ====


male_100_html <- read_html("http://www.alltime-athletics.com/m_100ok.htm")
male_100_pres <- male_100_html %>%
  html_nodes(xpath = "//pre")
male_100_htext <- male_100_pres %>%
  html_text()
male_100_htext <- male_100_htext[[1]]

male_100 <- readr::read_fwf(male_100_htext, skip = 1, n_max = 3178,
                            col_types = cols(.default = col_character()),
                            col_positions = fwf_positions(
                              c(1, 16, 27, 35, 66, 74, 86, 93, 123),
                              c(15, 26, 34, 65, 73, 85, 92, 122, 132)
                            ))

male_100 <- male_100 %>%
  select(X2, X4) %>% 
  transmute(timing = X2, runner = X4) %>%
  mutate(timing = gsub("A", "", timing),
         timing = as.numeric(timing)) %>%
  filter(runner %in% c("Usain Bolt", "Asafa Powell", "Yohan Blake",
                       "Justin Gatlin", "Maurice Greene", "Tyson Gay")) %>%
  mutate_if(is.character, as.factor) %>%
  droplevels
male_100

## Takes the runner factor and reorders it according to the median by runner.
male_100$runner <- forcats::fct_reorder(male_100$runner, male_100$timing)

#### USING THE GGSTATSPLOT PACKAGE ====

## https://stackoverflow.com/questions/10581440/error-in-grid-calll-textbounds-as-graphicsannotxlabel-xx-xy-polygon

ggbetweenstats(data = male_100, 
               x = runner, 
               y = timing,
               type = "p",
               var.equal = FALSE,
               pairwise.comparisons = TRUE,
               partial = FALSE,
               effsize.type = "biased",
               point.jitter.height = 0, 
               title = "Parametric (Mean) testing assuming unequal variances",
               ggplot.component = ggplot2::scale_y_continuous(breaks = seq(9.6, 10.4, .2), 
                                                              limits = (c(9.6,10.4))),
               messages = TRUE
)

ggbetweenstats(data = male_100, 
               x = runner, 
               y = timing,
               type = "np",
               var.equal = FALSE,
               pairwise.comparisons = TRUE,
               partial = FALSE,
               effsize.type = "biased",
               point.jitter.height = 0, 
               title = "Non-Parametric (Rank) testing",
               ggplot.component = ggplot2::scale_y_continuous(breaks = seq(9.6, 10.4, .2), 
                                                              limits = (c(9.6,10.4))),
               messages = TRUE
)

ggbetweenstats(data = male_100, 
               x = runner, 
               y = timing,
               type = "bf",
               var.equal = FALSE,
               pairwise.comparisons = TRUE,
               partial = FALSE,
               effsize.type = "biased",
               point.jitter.height = 0, 
               title = "Bayesian testing",
               messages = TRUE
)

### See exp(2.9). The odds are better than 18:1 that the runners are not equally fast.

anovaBF(timing ~ runner, data = as.data.frame(male_100), rscaleFixed = .707)

# Okay that’s better so to Bayesian thinking the odds are 19:1 against the fact that they all run about the same speed, 
#or 19:1 they run at different speeds.

#### FUNCTION FOR PAIRWISE COMPARISON ====

compare_runners_bf <- function(df, runner1, runner2) {
  ds <- df %>%
    filter(runner %in% c(runner1, runner2)) %>%
    droplevels %>%
    as.data.frame
  zzz <- ttestBF(formula = timing ~ runner, data = ds)
  yyy <- extractBF(zzz)
  xxx <-
    paste0(
      "The evidence provided by the data corresponds to odds of ",
      round(yyy$bf, 0),
      ":1 that ",
      runner1,
      " is faster than ",
      runner2
    )
  return(xxx)
}

#### RUNNING TEST ====

compare_runners_bf(male_100, "Usain Bolt", "Asafa Powell")
compare_runners_bf(male_100, "Usain Bolt", "Tyson Gay")
compare_runners_bf(male_100, "Usain Bolt", "Justin Gatlin")
compare_runners_bf(male_100, "Usain Bolt", "Yohan Blake")
compare_runners_bf(male_100, "Usain Bolt", "Maurice Greene")





