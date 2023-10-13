Project proposal
================
Rat Bastards

``` r
library(tidyverse)
library(broom)
```

## 1. Introduction

Our data are a collection of phenological observations on vegetation
from Hurricane Island. These data were gathered in the 2023 spring-fall
field season to track the phenophases of a broad range of plant species.
The data were collected by one of our team members who visited
individual plants each week throughout the season. Photos were taken of
each plant on a weekly basis to identify phenophase development. The
phenophases tracked were based on the categories described by the
National Phenology Network. The primary goal of this research is to
create and visualize a baseline phenological record of the vegetation on
Hurricane Island which can be used in future analysis. Additionally, we
plan to analyze how the phenophases develop in response to varying
climatic factors throughout the season. We also intend to visualize
comparisons between differing species categories such as
fruit-bearing/non-fruit-bearing, native/introduced, etc to see how their
development varies. The cases are each plant species and its disposition
on the calendar day on which it was observed. The variables are date of
data collection, species information, and phenological observations
made. Individual species have differing phenological observations made
depending on varying morphology. (Eg. A plant that fruits versus one
that does not).

## 2. Data

``` r
library(readr)

hurricane_raw_data <- read_csv("../data/hurricane_plants.csv")
```

    ## New names:
    ## Rows: 1320 Columns: 27
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (9): date, life_form, species, fallen_leaf_presence, notes, initial_eme... dbl
    ## (12): breaking_leaf_buds_count, percent_unfolded_leaves, percent_full_si... lgl
    ## (6): breaking_needle_bud_count, young_needle_count, pollen_cone_count, ...
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `...15` -> `...21`

``` r
# reorder variables to logical order

hurricane_plants <- relocate(hurricane_raw_data, 
       date, 
       life_form, 
       species, 
       initial_emergence,
       breaking_leaf_buds_count,
       leaf_presence,
       percent_unfolded_leaves,
       unfolded_leaves_count,
       percent_full_size_leaf,
       percent_leaves_colorful,
       fallen_leaf_presence,
       breaking_needle_bud_count,
       young_needle_count,
       percent_stalk_growth,
       percent_fiddlehead_unrolled,
       buds_and_flowers_count,
       percent_open_flowers,
       pollen_amount,
       pollen_cone_count,
       percent_open_pollen_cones,
       fruit_count,
       unripe_seed_cone_count, 
       percent_ripe_fruits,
       ripe_seed_cone_count,
       dropped_fruit_count,
       notes
       )

# Remove empty rows, where life_form is NA 
hurricane_plants <- hurricane_plants %>% 
  filter(!is.na(life_form))
# drop_na(life_form) - another option for the same function


# write_csv(hurricane_plants, file = "hurricane_plants_reordered.csv")

glimpse(hurricane_plants)
```

    ## Rows: 376
    ## Columns: 27
    ## $ date                        <chr> "4/14/2023", "4/26/2023", "4/29/2023", "5/…
    ## $ life_form                   <chr> "tree", "tree", "tree", "tree", "tree", "t…
    ## $ species                     <chr> "Malus sp.", "Malus sp.", "Malus sp.", "Ma…
    ## $ initial_emergence           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ breaking_leaf_buds_count    <dbl> 0, 10, 10, 1000, 10000, 10000, 10000, 1000…
    ## $ leaf_presence               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ percent_unfolded_leaves     <dbl> 0.00, 0.00, 0.00, 0.00, 0.75, 1.00, 1.00, …
    ## $ unfolded_leaves_count       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ percent_full_size_leaf      <dbl> 0.00, 0.00, 0.00, 0.00, 0.75, 1.00, 1.00, …
    ## $ percent_leaves_colorful     <dbl> 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.…
    ## $ fallen_leaf_presence        <chr> "absent", "absent", "absent", "absent", "a…
    ## $ breaking_needle_bud_count   <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ young_needle_count          <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ percent_stalk_growth        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ percent_fiddlehead_unrolled <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ buds_and_flowers_count      <dbl> 0, 0, 0, 0, 1000, 1000, 0, 0, 0, 0, 0, 0, …
    ## $ percent_open_flowers        <dbl> 0.00, 0.00, 0.00, 0.00, 0.05, 1.00, 0.00, …
    ## $ pollen_amount               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ pollen_cone_count           <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ percent_open_pollen_cones   <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ fruit_count                 <dbl> 0, 0, 0, 0, 0, 0, 1000, 1000, 1000, 1000, …
    ## $ unripe_seed_cone_count      <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ percent_ripe_fruits         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ ripe_seed_cone_count        <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ dropped_fruit_count         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 25, 25…
    ## $ notes                       <chr> NA, NA, NA, "other apple trees, perhaps wi…
    ## $ ...21                       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…

## 3. Ethics review

## 4. Data analysis plan

We will visualize date, breaking leaves/buds/needles, flowering, and
fruiting phenophases of comparable species. We will also visualize these
phenophases based on records of climatic variables from this field
season. The climatic variables will need to be sourced from the
Worldclim database. We plan on utilizing violin plots, lollipop plots,
ridge plots, as well as potential animations and maps.

``` r
# graph the amount of buds or flowers of each plant? facet by life form?
# graph the percent leaves unfolded 
# summary statistics
```
