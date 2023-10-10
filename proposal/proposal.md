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

hurricane_data <- read_csv("../data/hurricane_plants.csv")
```

    ## New names:
    ## • `...15` -> `...30`

    ## Warning: One or more parsing issues, call `problems()` on your data frame for details,
    ## e.g.:
    ##   dat <- vroom(...)
    ##   problems(dat)

    ## Rows: 1320 Columns: 39
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (15): date, life form, species, fallen leaves (present/absent), notes, i...
    ## dbl (15): # breaking leaf buds, % unfolded leaves, % full size are most leav...
    ## lgl  (9): # breaking needle buds, # young needles, # pollen cones, % open po...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# reorder variables to logical order

hurricane_plants <- relocate(hurricane_data, 
       date, 
       `life form`, 
       species, 
       `initial growth (presnt/absent)`,
       `emergence (present/absent)`,
       `# breaking leaf buds`,
       `leaves present/absent?`,
       `unfolded leaves (present/absent)`,
       `unfolded leaves? (present/absent)`,
       `fully unfolded leaves? (present/absent)`,
       `fully unfolded leaves (present or absent)`,
       `% unfolded leaves`,
       `% full size are most leaves?`,
       `# young leaves`,
       `# leaves unfolded`,
       `young leaves (present/absent)`,
       `leaves present/absent`,
       `% full-sized leaves`,
       `% leaves colorful (not including dried leaves retained)`,
       `% leaves color change`,
       `fallen leaves (present/absent)`,
       `# leaves unfolded`, 
       `# breaking needle buds`,
       `# young needles`,
       `% stalk full growth`,
       `% fiddlehead unrolled`,
       `# flowers or buds`,
       `% open flowers`,
       `pollen (little, some, lots)`,
       `# pollen cones`,
       `% open pollen cones`,
       `pollen release (little, some, lots)`,
       `# fruits`,
       `# unripe seed cones`,
       `% ripe fruits`,
       `# ripe seed cones`,
       `# dropped/removed fruits`,
       `# cone or seed drop`,
       notes
       )

glimpse(hurricane_plants)
```

    ## Rows: 1,320
    ## Columns: 39
    ## $ date                                                      <chr> "4/14/2023",…
    ## $ `life form`                                               <chr> "tree", "tre…
    ## $ species                                                   <chr> "Malus sp.",…
    ## $ `initial growth (presnt/absent)`                          <chr> NA, NA, NA, …
    ## $ `emergence (present/absent)`                              <chr> NA, NA, NA, …
    ## $ `# breaking leaf buds`                                    <dbl> 0, 10, 10, 1…
    ## $ `leaves present/absent?`                                  <chr> NA, NA, NA, …
    ## $ `unfolded leaves (present/absent)`                        <chr> NA, NA, NA, …
    ## $ `unfolded leaves? (present/absent)`                       <chr> NA, NA, NA, …
    ## $ `fully unfolded leaves? (present/absent)`                 <chr> NA, NA, NA, …
    ## $ `fully unfolded leaves (present or absent)`               <lgl> NA, NA, NA, …
    ## $ `% unfolded leaves`                                       <dbl> 0.00, 0.00, …
    ## $ `% full size are most leaves?`                            <dbl> 0.00, 0.00, …
    ## $ `# young leaves`                                          <dbl> NA, NA, NA, …
    ## $ `# leaves unfolded`                                       <dbl> NA, NA, NA, …
    ## $ `young leaves (present/absent)`                           <chr> NA, NA, NA, …
    ## $ `leaves present/absent`                                   <chr> NA, NA, NA, …
    ## $ `% full-sized leaves`                                     <dbl> NA, NA, NA, …
    ## $ `% leaves colorful (not including dried leaves retained)` <dbl> 0.0, 0.0, 0.…
    ## $ `% leaves color change`                                   <dbl> NA, NA, NA, …
    ## $ `fallen leaves (present/absent)`                          <chr> "absent", "a…
    ## $ `# breaking needle buds`                                  <lgl> NA, NA, NA, …
    ## $ `# young needles`                                         <lgl> NA, NA, NA, …
    ## $ `% stalk full growth`                                     <dbl> NA, NA, NA, …
    ## $ `% fiddlehead unrolled`                                   <dbl> NA, NA, NA, …
    ## $ `# flowers or buds`                                       <dbl> 0, 0, 0, 0, …
    ## $ `% open flowers`                                          <dbl> 0.00, 0.00, …
    ## $ `pollen (little, some, lots)`                             <chr> NA, NA, NA, …
    ## $ `# pollen cones`                                          <lgl> NA, NA, NA, …
    ## $ `% open pollen cones`                                     <lgl> NA, NA, NA, …
    ## $ `pollen release (little, some, lots)`                     <lgl> NA, NA, NA, …
    ## $ `# fruits`                                                <dbl> 0, 0, 0, 0, …
    ## $ `# unripe seed cones`                                     <lgl> NA, NA, NA, …
    ## $ `% ripe fruits`                                           <dbl> 0, 0, 0, 0, …
    ## $ `# ripe seed cones`                                       <lgl> NA, NA, NA, …
    ## $ `# dropped/removed fruits`                                <dbl> 0, 0, 0, 0, …
    ## $ `# cone or seed drop`                                     <lgl> NA, NA, NA, …
    ## $ notes                                                     <chr> NA, NA, NA, …
    ## $ ...30                                                     <chr> NA, NA, NA, …

## 3. Ethics review

## 4. Data analysis plan

We will visualize date, breaking leaves/buds/needles, flowering, and
fruiting phenophases of comparable species. We will also visualize these
phenophases based on records of climatic variables from this field
season. The climatic variables will need to be sourced from the
Worldclim database. We plan on utilizing violin plots, lollipop plots,
ridge plots, as well as potential animations and maps.
