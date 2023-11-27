##### DAG of relationships among measured variables
##### Author: Katherine Lauck
##### Last updated: 24 January 2022

#  set theme of all DAGs to `theme_dag()`
library(ggdag)
library(ggplot2)
library(tidyverse)
theme_set(theme_dag())

growth <- dagify(fitness ~ nestgrowth,
                 nestgrowth ~ foodprovis + neststress,
                 foodprovis ~ habitat + surftemp,
                 neststress ~ surftemp + foodprovis,
                 surftemp ~ climate + habitat,
                 labels = c("fitness" = "fitness",
                            "nestgrowth" = "nestling growth\n& survival",
                            "foodprovis" = "food provisioning",
                            "neststress" = "nestling stress",
                            "adultstress" = "adult stress",
                            "foodsupply" = "food supply",
                            "surftemp" = "surface temperature",
                            "habitat" = "land cover",
                            "climate" = "climate"),
                 exposure = c("surftemp","habitat"),
                 outcome = "nestgrowth")

ggdag_status(growth, text = FALSE, use_labels = 'label', layout = "nicely") + geom_dag_edges()
ggsave("figures/dag.png")

