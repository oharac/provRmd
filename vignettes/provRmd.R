## ----fig setup, echo = FALSE, warning = FALSE, message = FALSE-----------
knitr::opts_chunk$set(fig.width = 7, fig.height = 7)

## ----setup, echo = TRUE, warning = FALSE, message = FALSE----------------
library(dplyr); library(tidyr); library(readr)

library(provRmd)

prov_setup()

## ----git_prov example, echo = TRUE, message = FALSE, warning = FALSE-----
prov_setup()

spp_df <- read_csv('spp_table1.csv')

spp_df <- spp_df %>%
  mutate(risk = ifelse(status > .16, 'high', 'low'))

write_csv(spp_df, 'spp_table2.csv')

names(.prov_track)

knitr::kable(.prov_track[, 1:5])

## ----git_prov source example, echo = TRUE, message = FALSE, warning = FALSE----

source('source_example.R')

knitr::kable(.prov_track[, 1:5])

## ----script_prov example, echo = TRUE, message = FALSE, warning = FALSE----

script_out_df <- script_prov() 

names(script_out_df)

## ----view script_track, echo = TRUE, message = FALSE, warning = FALSE----
names(.script_track)

knitr::kable(.script_track[, 4:9] %>%
               arrange(sequence) %>%
               head())

## ----plot_prov, echo = TRUE, message = FALSE, warning = FALSE------------
plot_obj <- plot_prov(df = .script_track, plot_dir = 'TB')

DiagrammeR::render_graph(plot_obj)

## ----broken_link_example1, echo = TRUE, message = FALSE, warning = FALSE----
prov_setup() ### to clear prior provenance tracking
x <- read_csv('spp_table1.csv')
y <- x %>%
  mutate(new_col = 'words')
write_csv(y, 'table_y.csv')

## ----broken_link_example2, echo = TRUE, message = FALSE, warning = FALSE----
z <- y %>%
  mutate(new_col2 = 'more words')
write_csv(z, 'table_z.csv')

