## ----fig setup, echo = FALSE, warning = FALSE, message = FALSE-----------
knitr::opts_chunk$set(fig.width = 6, fig.height = 6)

## ----setup, echo = TRUE, warning = FALSE, message = FALSE----------------
library(dplyr); library(tidyr); library(readr)

library(provRmd)

prov_setup() ### Initialize provenance tracking

## ----git_prov example, echo = TRUE, message = FALSE, warning = FALSE-----
iris_df <- iris %>%
  mutate(Petal.Area = Petal.Length * Petal.Width) ### yeah, they're not square petals... whatever!

write_csv(iris_df, 'iris_table1.csv')

iris_df <- iris_df %>%
  group_by(Species) %>%
  mutate(Aspect.Ratio = Petal.Length / Petal.Width,
         Skinny.Petal = Aspect.Ratio > mean(Aspect.Ratio))

write_csv(iris_df, 'iris_table2.csv')

## ----git_prov source example, echo = TRUE, message = FALSE, warning = FALSE----
source(system.file('vignette_data/source_example.R', package = 'provRmd'))

## ----script_prov example, echo = TRUE, message = FALSE, warning = FALSE----
script_out_df <- script_prov() 

## ----plot_prov, echo = TRUE, message = FALSE, warning = FALSE------------
plot_obj <- plot_prov(df = .script_track, plot_dir = 'TB')

DiagrammeR::render_graph(plot_obj)

## ----broken_link_example1, echo = TRUE, message = FALSE, warning = FALSE----
prov_setup() ### to clear prior provenance tracking
x <- read_csv('iris_table1.csv')
y <- x %>%
  mutate(new_col = 'words')
write_csv(y, 'table_y.csv')

## ----broken_link_example2, echo = TRUE, message = FALSE, warning = FALSE----
z <- y %>%
  mutate(new_col2 = 'more words')
write_csv(z, 'table_z.csv')

