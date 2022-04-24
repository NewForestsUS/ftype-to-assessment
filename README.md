# Forest Type to Assessment Area Crosswalk

This document describes the steps for generating a forest type to
assessment area crosswalk using CAR supersections, FIA forest types, ARB
species-to-forest, and Carbon plan assessment area and common practice.
The resulting dataset includes a many-to-many mapping of forest types to
assessment areas and common practice values. This crosswalk is later
applied to forest type and supersection raster data to generate raster
datasets of assessment area and common practice values.

### Data loading

First we read in the supersections vector data for the CONUS supplied by
the Carbon Action Reserve (CAR):

``` r
kable(head(supersect_sf))
```

|        AREA | PERIMETER |    ACRES | SSection                 | SS_Name2 | ss_code | geometry                     |
|------------:|----------:|---------:|:-------------------------|:---------|:--------|:-----------------------------|
| 30741682875 | 1404657.1 |  7596137 | Okanogan Highland        | NA       | 60      | MULTIPOLYGON (((-1194515 47… |
| 28793819858 | 1201969.9 |  7114983 | Northwest Cascades       | NA       | 58      | MULTIPOLYGON (((-1651109 27… |
| 17147017863 | 3335379.1 |  4237070 | Puget Trough             | NA       | 67      | MULTIPOLYGON (((-1700044 40… |
| 21883664760 |  790723.2 |  5407398 | Northern Rocky Mountains | NA       | 57      | MULTIPOLYGON (((-1103217 38… |
| 41675060546 | 1252913.5 | 10297621 | Northern Great Plains    | NA       | 56      | MULTIPOLYGON (((-869464.3 3… |
| 69771019667 | 2634088.8 | 17240259 | Columbia Basin           | NA       | 22      | MULTIPOLYGON (((-1418794 24… |

Next we read in a mapping of FIA Forest Types to common names:

``` r
kable(head(fortypcmn_dt))
```

| ftype_name                           | ftype_code |
|:-------------------------------------|:-----------|
| Jack pine                            | 101        |
| Red pine                             | 102        |
| Eastern white pine                   | 103        |
| Eastern white pine / eastern hemlock | 104        |
| Eastern hemlock                      | 105        |
| Balsam fir                           | 121        |

As well as species-to-forest type codes from ARB:

``` r
kable(head(fortypcds_dt))
```

| ftype_list                                                                          | aa_code |
|:------------------------------------------------------------------------------------|:--------|
| 104, 105, 506, 511, 512, 519                                                        | 3       |
| 507, 508, 513, 517, 520, 601, 602, 608, 700, 701, 702, 703, 704, 705, 706, 708, 991 | 4       |
| 171, 184, 400, 401, 402, 404, 405, 406, 409, 999                                    | 5       |
| 100, 102, 103, 121, 122, 123, 125, 126, 201, 381, 383, 384, 385, 900, 901, 903      | 6       |
| 516, 707, 800, 801, 802, 805, 809                                                   | 7       |
| 500, 501, 502, 503, 504, 505, 510, 514, 515                                         | 8       |

Lastly we read in the assessment area & common practice data from Carbon
Plan Paper (TODO: Add Link):

``` r
kable(head(aa_dt))
```

| supersection                           | assessment_area                                     | aa_code | ss_code | species                                                                                                                                                                                                                                                                                                                                                                                                           | site_class | common_practice |
|:---------------------------------------|:----------------------------------------------------|--------:|--------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:-----------|----------------:|
| Adirondacks & Green Mountains          | Adirondacks & Green Mountains Northeast Conifers    |       1 |       1 | Aspen, balsam fir, black spruce, eastern hemlock, eastern white pine, northern red oak, white ash, gray birch, northern white-cedar, Norway spruce, paper birch, red pine, red spruce, Scotch pine, tamarack, white, red, jack pine, white spruce                                                                                                                                                                 | high       |           97.98 |
| Adirondacks & Green Mountains          | Adirondacks & Green Mountains Northeast Conifers    |       1 |       1 | Aspen, balsam fir, black spruce, eastern hemlock, eastern white pine, northern red oak, white ash, gray birch, northern white-cedar, Norway spruce, paper birch, red pine, red spruce, Scotch pine, tamarack, white, red, jack pine, white spruce                                                                                                                                                                 | low        |           89.60 |
| Adirondacks & Green Mountains          | Adirondacks & Green Mountains Northern Hardwood     |       2 |       1 | Balsam poplar, black ash, american elm, red maple, black cherry, cherry, white ash, yellow poplar, maple, basswood, maple, beech, birch group, mixed upland hardwoods, northern red aok, post oak, blackjack oak, lowland, oak, upland, scarlet oak, silver maple, sugar maple, yellow birch, sugarberry, hackberry, elm, green ash, sweetbay, swamp tupelo, sycamore, pecan, white oak, red oak, hickory, willow | high       |          101.56 |
| Adirondacks & Green Mountains          | Adirondacks & Green Mountains Northern Hardwood     |       2 |       1 | Balsam poplar, black ash, american elm, red maple, black cherry, cherry, white ash, yellow poplar, maple, basswood, maple, beech, birch group, mixed upland hardwoods, northern red aok, post oak, blackjack oak, lowland, oak, upland, scarlet oak, silver maple, sugar maple, yellow birch, sugarberry, hackberry, elm, green ash, sweetbay, swamp tupelo, sycamore, pecan, white oak, red oak, hickory, willow | low        |          102.60 |
| Allegheny & North Cumberland Mountains | Allegheny & North Cumberland Mountains Cove Forests |       3 |       2 | Black walnut, eastern hemlock, eastern white pine, red maple, red oak, yellow poplar, white oak                                                                                                                                                                                                                                                                                                                   | high       |          114.61 |
| Allegheny & North Cumberland Mountains | Allegheny & North Cumberland Mountains Cove Forests |       3 |       2 | Black walnut, eastern hemlock, eastern white pine, red maple, red oak, yellow poplar, white oak                                                                                                                                                                                                                                                                                                                   | low        |           97.06 |

### Data Processing and Formatting

This next section shows the steps to wrangle all of these inputs into a
crosswalk table that links all of the forest types and supersections to
their corresponding assessment areas and common practice values.

Update assessment area names and format into a dataframe:

``` r
# Update assessment area name
aa_dt$assessment_area <- lapply(aa_dt$assessment_area, function(x){
  ss_name <- aa_dt[assessment_area == x,]$supersection[[1]]
  aa_name <- aa_dt[assessment_area == x,]$assessment_area[[1]]
  ss_words <- strsplit(ss_name, " ") %>% unlist()
  aa_words <- strsplit(aa_name, " ") %>% unlist()

  aa_only <- aa_words[!(aa_words %in% ss_words)]
  paste0(aa_only, collapse = " ")
}) %>% unlist()

aa_uni_dt <- aa_dt[, list(
  ss_name= min(supersection),
  aa_name = min(assessment_area),
  species = unique(species),
  cp_lo = min(common_practice),
  cp_hi = max(common_practice), 
  ss_code = as.character(unique(ss_code)),
  aa_code = as.character(unique(aa_code))
), by=c("assessment_area", "species", "supersection")]

# WHAT ABOUT SITES WITH 'site_class'=='all'???
sc_all <- c("155", "156", "157", "158", "159", "160", "161")
aa_uni_dt <- aa_uni_dt[!(aa_uni_dt$aa_code %in% sc_all),]
```

``` r
kable(head(aa_uni_dt))
```

| assessment_area     | species                                                                                                                                                                                                                                                                                                                                                                                                           | supersection                           | ss_name                                | aa_name             | species                                                                                                                                                                                                                                                                                                                                                                                                           |  cp_lo |  cp_hi | ss_code | aa_code |
|:--------------------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:---------------------------------------|:---------------------------------------|:--------------------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------:|-------:|:--------|:--------|
| Northeast Conifers  | Aspen, balsam fir, black spruce, eastern hemlock, eastern white pine, northern red oak, white ash, gray birch, northern white-cedar, Norway spruce, paper birch, red pine, red spruce, Scotch pine, tamarack, white, red, jack pine, white spruce                                                                                                                                                                 | Adirondacks & Green Mountains          | Adirondacks & Green Mountains          | Northeast Conifers  | Aspen, balsam fir, black spruce, eastern hemlock, eastern white pine, northern red oak, white ash, gray birch, northern white-cedar, Norway spruce, paper birch, red pine, red spruce, Scotch pine, tamarack, white, red, jack pine, white spruce                                                                                                                                                                 |  89.60 |  97.98 | 1       | 1       |
| Northern Hardwood   | Balsam poplar, black ash, american elm, red maple, black cherry, cherry, white ash, yellow poplar, maple, basswood, maple, beech, birch group, mixed upland hardwoods, northern red aok, post oak, blackjack oak, lowland, oak, upland, scarlet oak, silver maple, sugar maple, yellow birch, sugarberry, hackberry, elm, green ash, sweetbay, swamp tupelo, sycamore, pecan, white oak, red oak, hickory, willow | Adirondacks & Green Mountains          | Adirondacks & Green Mountains          | Northern Hardwood   | Balsam poplar, black ash, american elm, red maple, black cherry, cherry, white ash, yellow poplar, maple, basswood, maple, beech, birch group, mixed upland hardwoods, northern red aok, post oak, blackjack oak, lowland, oak, upland, scarlet oak, silver maple, sugar maple, yellow birch, sugarberry, hackberry, elm, green ash, sweetbay, swamp tupelo, sycamore, pecan, white oak, red oak, hickory, willow | 101.56 | 102.60 | 1       | 2       |
| Cove Forests        | Black walnut, eastern hemlock, eastern white pine, red maple, red oak, yellow poplar, white oak                                                                                                                                                                                                                                                                                                                   | Allegheny & North Cumberland Mountains | Allegheny & North Cumberland Mountains | Cove Forests        | Black walnut, eastern hemlock, eastern white pine, red maple, red oak, yellow poplar, white oak                                                                                                                                                                                                                                                                                                                   |  97.06 | 114.61 | 2       | 3       |
| Mixed Hardwoods     | Black ash , American elm , red maple, black locust, cottonwood, elm, ash, birch,sycamore, sassafras, persimmon, pecan, willow                                                                                                                                                                                                                                                                                     | Allegheny & North Cumberland Mountains | Allegheny & North Cumberland Mountains | Mixed Hardwoods     | Black ash , American elm , red maple, black locust, cottonwood, elm, ash, birch,sycamore, sassafras, persimmon, pecan, willow                                                                                                                                                                                                                                                                                     |  62.31 |  80.47 | 2       | 4       |
| Mixed Pine-Hardwood | Eastern redcedar, eastern white pine, northern red oak, white ash, Virginia pine, southern red oak                                                                                                                                                                                                                                                                                                                | Allegheny & North Cumberland Mountains | Allegheny & North Cumberland Mountains | Mixed Pine-Hardwood | Eastern redcedar, eastern white pine, northern red oak, white ash, Virginia pine, southern red oak                                                                                                                                                                                                                                                                                                                |  54.94 |  54.94 | 2       | 5       |
| Northern Conifer    | Aspen, balsam fir, black spruce, eastern white pine, Norway spruce, red pine, red spruce, Scotch pine                                                                                                                                                                                                                                                                                                             | Allegheny & North Cumberland Mountains | Allegheny & North Cumberland Mountains | Northern Conifer    | Aspen, balsam fir, black spruce, eastern white pine, Norway spruce, red pine, red spruce, Scotch pine                                                                                                                                                                                                                                                                                                             |  53.65 |  53.65 | 2       | 6       |

Join forest type lists to assessment area table:

``` r
# Join the forest type list to assessment data table
aa_ftype_dt <- aa_uni_dt[fortypcds_dt, on = 'aa_code', c("ss_name", "ss_code", "ftype_list", "aa_name", "species", "aa_code", "cp_lo", "cp_hi")]

# Break out the ftypes into individual rows
aa_ftype_expanded_dt <- lapply(aa_ftype_dt$aa_code, function(x)  {
  
  dt <- aa_ftype_dt[aa_ftype_dt$aa_code == x, ]
  
  dt_ftype_list <- dt$ftype_list[[1]]

  dt_expanded <- lapply(dt_ftype_list, function(y){
    dt_exp_y <- dt
    dt_exp_y$ftype_code <- as.character(y)
    dt_exp_y
  }) %>% rbindlist()

  dt_expanded
}) %>% rbindlist()
```

Assessment areas with forest type list, species, and common practice:

``` r
kable(head(aa_ftype_dt))
```

| ss_name                                | ss_code | ftype_list                                                                          | aa_name             | species                                                                                                                               | aa_code |  cp_lo |  cp_hi |
|:---------------------------------------|:--------|:------------------------------------------------------------------------------------|:--------------------|:--------------------------------------------------------------------------------------------------------------------------------------|:--------|-------:|-------:|
| Allegheny & North Cumberland Mountains | 2       | 104, 105, 506, 511, 512, 519                                                        | Cove Forests        | Black walnut, eastern hemlock, eastern white pine, red maple, red oak, yellow poplar, white oak                                       | 3       |  97.06 | 114.61 |
| Allegheny & North Cumberland Mountains | 2       | 507, 508, 513, 517, 520, 601, 602, 608, 700, 701, 702, 703, 704, 705, 706, 708, 991 | Mixed Hardwoods     | Black ash , American elm , red maple, black locust, cottonwood, elm, ash, birch,sycamore, sassafras, persimmon, pecan, willow         | 4       |  62.31 |  80.47 |
| Allegheny & North Cumberland Mountains | 2       | 171, 184, 400, 401, 402, 404, 405, 406, 409, 999                                    | Mixed Pine-Hardwood | Eastern redcedar, eastern white pine, northern red oak, white ash, Virginia pine, southern red oak                                    | 5       |  54.94 |  54.94 |
| Allegheny & North Cumberland Mountains | 2       | 100, 102, 103, 121, 122, 123, 125, 126, 201, 381, 383, 384, 385, 900, 901, 903      | Northern Conifer    | Aspen, balsam fir, black spruce, eastern white pine, Norway spruce, red pine, red spruce, Scotch pine                                 | 6       |  53.65 |  53.65 |
| Allegheny & North Cumberland Mountains | 2       | 516, 707, 800, 801, 802, 805, 809                                                   | Northern Hardwoods  | Black cherry, white ash, yellow-poplar, hard maple, basswood, maple, beech, birch group, red maple, upland, sugar maple, yellow birch | 7       |  95.89 | 104.55 |
| Allegheny & North Cumberland Mountains | 2       | 500, 501, 502, 503, 504, 505, 510, 514, 515                                         | Oak-Hickory         | Chestnut oak, black oak, scarlet oak, northern red oak, post oak, blackjack oak, white oak, hickory                                   | 8       | 117.89 | 119.27 |

``` r
kable(head(aa_ftype_expanded_dt))
```

| ss_name                                | ss_code | ftype_list                   | aa_name      | species                                                                                         | aa_code | cp_lo |  cp_hi | ftype_code |
|:---------------------------------------|:--------|:-----------------------------|:-------------|:------------------------------------------------------------------------------------------------|:--------|------:|-------:|:-----------|
| Allegheny & North Cumberland Mountains | 2       | 104, 105, 506, 511, 512, 519 | Cove Forests | Black walnut, eastern hemlock, eastern white pine, red maple, red oak, yellow poplar, white oak | 3       | 97.06 | 114.61 | 104        |
| Allegheny & North Cumberland Mountains | 2       | 104, 105, 506, 511, 512, 519 | Cove Forests | Black walnut, eastern hemlock, eastern white pine, red maple, red oak, yellow poplar, white oak | 3       | 97.06 | 114.61 | 105        |
| Allegheny & North Cumberland Mountains | 2       | 104, 105, 506, 511, 512, 519 | Cove Forests | Black walnut, eastern hemlock, eastern white pine, red maple, red oak, yellow poplar, white oak | 3       | 97.06 | 114.61 | 506        |
| Allegheny & North Cumberland Mountains | 2       | 104, 105, 506, 511, 512, 519 | Cove Forests | Black walnut, eastern hemlock, eastern white pine, red maple, red oak, yellow poplar, white oak | 3       | 97.06 | 114.61 | 511        |
| Allegheny & North Cumberland Mountains | 2       | 104, 105, 506, 511, 512, 519 | Cove Forests | Black walnut, eastern hemlock, eastern white pine, red maple, red oak, yellow poplar, white oak | 3       | 97.06 | 114.61 | 512        |
| Allegheny & North Cumberland Mountains | 2       | 104, 105, 506, 511, 512, 519 | Cove Forests | Black walnut, eastern hemlock, eastern white pine, red maple, red oak, yellow poplar, white oak | 3       | 97.06 | 114.61 | 519        |

Update some forest type codes from FIA DB documentation (TODO: Include
link to documentation)

``` r
# Join the forest type common name to the data table
aa_ftype_expanded_dt <- fortypcmn_dt[aa_ftype_expanded_dt, on='ftype_code']
aa_ftype_expanded_dt$ftype_name <- as.character(aa_ftype_expanded_dt$ftype_name)

# Forest type 932 has been replaced with 933 in the most recent FIADB documentation,
# and 933 is fairly common in the CONUS ftype raster
aa_ftype_expanded_dt[ftype_code==932]$ftype_code <- 933

# Trim columns
cols2keep <- c("ftype_code", "ftype_name", "aa_code", "aa_name", "ss_code", "ss_name", "cp_lo", "cp_hi", "species")
aa_crosswalk_dt <- aa_ftype_expanded_dt[,cols2keep, with=FALSE]

# AT THIS POINT, the crosswalk is missing classes 203, 934 and 935 -- hardcode them
# Notes: 
# - There are only 3 pixels with class 203 (Big Cone Douglas Fir) which only occur in San Rafael and Los Padres forests in SoCal
# - 934 and 935 are both oak variety that occur in Sierra foothills
ftype_missing_dt <- fortypcmn_dt[ftype_code %in% c(203, 934, 935)]
ftype_missing_dt$aa_code <- c("294", "281", "281")# Calling Bigcone mixed conifer, and the live oaks Mixed Oak Woodland ( same as 'Canyon live oak / interior live oak)
aa_missing_dt <- aa_crosswalk_dt[aa_code %in% c(294, 281), !(names(aa_crosswalk_dt) %in% c("ftype_code", "ftype_name")), with=FALSE] %>% distinct()
ftype_addons_dt <- ftype_missing_dt[aa_missing_dt, on="aa_code"]
```

Join the updated reformatted fores types:

Final crosswalk table

``` r
kable(head(aa_crosswalk_dt))
```

| ftype_code | ftype_name                                   | aa_code | aa_name                                             | ss_code | ss_name                                | cp_lo |  cp_hi | species                                                                                         |
|:-----------|:---------------------------------------------|:--------|:----------------------------------------------------|:--------|:---------------------------------------|------:|-------:|:------------------------------------------------------------------------------------------------|
| 104        | Eastern white pine / eastern hemlock         | 3       | Allegheny & North Cumberland Mountains Cove Forests | 2       | Allegheny & North Cumberland Mountains | 97.06 | 114.61 | Black walnut, eastern hemlock, eastern white pine, red maple, red oak, yellow poplar, white oak |
| 105        | Eastern hemlock                              | 3       | Allegheny & North Cumberland Mountains Cove Forests | 2       | Allegheny & North Cumberland Mountains | 97.06 | 114.61 | Black walnut, eastern hemlock, eastern white pine, red maple, red oak, yellow poplar, white oak |
| 506        | Yellow-poplar / white oak / northern red oak | 3       | Allegheny & North Cumberland Mountains Cove Forests | 2       | Allegheny & North Cumberland Mountains | 97.06 | 114.61 | Black walnut, eastern hemlock, eastern white pine, red maple, red oak, yellow poplar, white oak |
| 511        | Yellow-poplar                                | 3       | Allegheny & North Cumberland Mountains Cove Forests | 2       | Allegheny & North Cumberland Mountains | 97.06 | 114.61 | Black walnut, eastern hemlock, eastern white pine, red maple, red oak, yellow poplar, white oak |
| 512        | Black walnut                                 | 3       | Allegheny & North Cumberland Mountains Cove Forests | 2       | Allegheny & North Cumberland Mountains | 97.06 | 114.61 | Black walnut, eastern hemlock, eastern white pine, red maple, red oak, yellow poplar, white oak |
| 519        | Red maple / oak                              | 3       | Allegheny & North Cumberland Mountains Cove Forests | 2       | Allegheny & North Cumberland Mountains | 97.06 | 114.61 | Black walnut, eastern hemlock, eastern white pine, red maple, red oak, yellow poplar, white oak |
