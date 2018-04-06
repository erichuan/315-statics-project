happy15 <- read.csv("~/Desktop/36-315/world-happiness-report/2015.csv")
happy16 <- read.csv("~/Desktop/36-315/world-happiness-report/2016.csv")
happy17 <- read.csv("~/Desktop/36-315/world-happiness-report/2017.csv")

names(happy15) <- c("Country","Region","happy_rank", "happy_score", "se", "gdp_per_cap",
          "family", "life_expect", "freedom", "trust_govt", "generosity",
          "dystopian_res")

names(happy16) <- c("Country", "Region", "happy_rank", "happy_score", "lower_CI",
                    "higher_CI", "gdp_per_cap", "family", "life_expect", "freedom",
                    "trust_govt", "generosity", "dystopian_res")

names(happy17) <- c("Country","Region","happy_rank", "happy_score", "se", "gdp_per_cap",
                    "family", "life_expect", "freedom", "trust_govt", "generosity",
                    "dystopian_res")

library(ggplot2)
library(tidyverse)
library(MASS)
library(GGally)
library(forcats)
library(dendextend)
library(reshape2)

# Select continuous variables
cont_cols <- which(names(happy17) %in% 
                     c("happy_rank", "happy_score", "lower_CI",
                       "higher_CI", "gdp_per_cap", "family", "life_expect", "freedom",
                       "trust_govt", "generosity", "dystopian_res"))
happy17_cont <- happy17[, cont_cols]

# Dendrogram
branch_pal <- RColorBrewer::brewer.pal(n = 11, name = "Dark2")
correlation_matrix <- cor(happy17_cont)
dist_matrix <- 1 - abs(correlation_matrix)
dist_matrix <- as.dist(dist_matrix)

dend <- dist_matrix %>% 
  hclust %>% 
  as.dendrogram %>%
  set("branches_k_col", branch_pal, k = 8) %>%
    ggplot(horiz = TRUE, theme = NULL) +
    theme(axis.text.y = element_blank()) +
    labs(title = "Dendrogram of happiness factors", 
         subtitle = "Source: World Happiness Report (Kaggle)", 
         x = "Continuous Variables", y = "Distance")
dend

