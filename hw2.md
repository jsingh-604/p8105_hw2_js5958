hw2
================
2022-10-02

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(readxl)
library(dplyr)
```

### Problem 1

Below we import and clean data from
`NYC_Transit_Subway_Entrance_And_Exit_Data.csv`. The process begins with
data import, updates variable names, and selects the columns that will
be used in later parts fo this problem. We update `entry` from `yes` /
`no` to a logical variable. As part of data import, we specify that
`Route` columns 8-11 should be character for consistency with 1-7.

``` r
trans_ent = 
  read_csv(
    "/Users/jag/Downloads/NYC_Transit_Subway_Entrance_And_Exit_Data.csv",
    col_types = cols(Route8 = "c", Route9 = "c", Route10 = "c", Route11 = "c")) %>% 
  janitor::clean_names() %>% 
  select(
    line, station_name, station_latitude, station_longitude, 
    starts_with("route"), entry, exit_only, vending, entrance_type, 
    ada) %>% 
  mutate(entry = ifelse(entry == "YES", TRUE, FALSE))
```

As it stands, these data are not “tidy”: route number should be a
variable, as should route. That is, to obtain a tidy dataset we would
need to convert `route` variables from wide to long format. This will be
useful when focusing on specific routes, but may not be necessary when
considering questions that focus on station-level variables.

The following code chunk selects station name and line, and then uses
`distinct()` to obtain all unique combinations. As a result, the number
of rows in this dataset is the number of unique stations.

``` r
trans_ent %>% 
  select(station_name, line) %>% 
  distinct
```

    ## # A tibble: 465 × 2
    ##    station_name             line    
    ##    <chr>                    <chr>   
    ##  1 25th St                  4 Avenue
    ##  2 36th St                  4 Avenue
    ##  3 45th St                  4 Avenue
    ##  4 53rd St                  4 Avenue
    ##  5 59th St                  4 Avenue
    ##  6 77th St                  4 Avenue
    ##  7 86th St                  4 Avenue
    ##  8 95th St                  4 Avenue
    ##  9 9th St                   4 Avenue
    ## 10 Atlantic Av-Barclays Ctr 4 Avenue
    ## # … with 455 more rows

The next code chunk is similar, but filters according to ADA compliance
as an initial step. This produces a dataframe in which the number of
rows is the number of ADA compliant stations.

``` r
trans_ent %>% 
  filter(ada == TRUE) %>% 
  select(station_name, line) %>% 
  distinct
```

    ## # A tibble: 84 × 2
    ##    station_name                   line           
    ##    <chr>                          <chr>          
    ##  1 Atlantic Av-Barclays Ctr       4 Avenue       
    ##  2 DeKalb Av                      4 Avenue       
    ##  3 Pacific St                     4 Avenue       
    ##  4 Grand Central                  42nd St Shuttle
    ##  5 34th St                        6 Avenue       
    ##  6 47-50th Sts Rockefeller Center 6 Avenue       
    ##  7 Church Av                      6 Avenue       
    ##  8 21st St                        63rd Street    
    ##  9 Lexington Av                   63rd Street    
    ## 10 Roosevelt Island               63rd Street    
    ## # … with 74 more rows

To compute the proportion of station entrances / exits without vending
allow entrance, we first exclude station entrances that do not allow
vending. Then, we focus on the `entry` variable – this logical, so
taking the mean will produce the desired proportion (recall that R will
coerce logical to numeric in cases like this).

``` r
trans_ent %>% 
  filter(vending == "NO") %>% 
  pull(entry) %>% 
  mean
```

    ## [1] 0.3770492

Lastly, we write a code chunk to identify stations that serve the A
train, and to assess how many of these are ADA compliant. As a first
step, we tidy the data as alluded to previously; that is, we convert
`route` from wide to long format. After this step, we can use tools from
previous parts of the question (filtering to focus on the A train, and
on ADA compliance; selecting and using `distinct` to obtain dataframes
with the required stations in rows).

``` r
trans_ent %>% 
  pivot_longer(
    route1:route11,
    names_to = "route_num",
    values_to = "route") %>% 
  filter(route == "A") %>% 
  select(station_name, line) %>% 
  distinct
```

    ## # A tibble: 60 × 2
    ##    station_name                  line           
    ##    <chr>                         <chr>          
    ##  1 Times Square                  42nd St Shuttle
    ##  2 125th St                      8 Avenue       
    ##  3 145th St                      8 Avenue       
    ##  4 14th St                       8 Avenue       
    ##  5 168th St - Washington Heights 8 Avenue       
    ##  6 175th St                      8 Avenue       
    ##  7 181st St                      8 Avenue       
    ##  8 190th St                      8 Avenue       
    ##  9 34th St                       8 Avenue       
    ## 10 42nd St                       8 Avenue       
    ## # … with 50 more rows

``` r
trans_ent %>% 
  pivot_longer(
    route1:route11,
    names_to = "route_num",
    values_to = "route") %>% 
  filter(route == "A", ada == TRUE) %>% 
  select(station_name, line) %>% 
  distinct
```

    ## # A tibble: 17 × 2
    ##    station_name                  line            
    ##    <chr>                         <chr>           
    ##  1 14th St                       8 Avenue        
    ##  2 168th St - Washington Heights 8 Avenue        
    ##  3 175th St                      8 Avenue        
    ##  4 34th St                       8 Avenue        
    ##  5 42nd St                       8 Avenue        
    ##  6 59th St                       8 Avenue        
    ##  7 Inwood - 207th St             8 Avenue        
    ##  8 West 4th St                   8 Avenue        
    ##  9 World Trade Center            8 Avenue        
    ## 10 Times Square-42nd St          Broadway        
    ## 11 59th St-Columbus Circle       Broadway-7th Ave
    ## 12 Times Square                  Broadway-7th Ave
    ## 13 8th Av                        Canarsie        
    ## 14 Franklin Av                   Franklin        
    ## 15 Euclid Av                     Fulton          
    ## 16 Franklin Av                   Fulton          
    ## 17 Howard Beach                  Rockaway

# Problem 2

Reading the excel file and cleaning th data

``` r
df_trash <- read_excel("/Users/jag/Downloads/Trash-Wheel-Collection-Totals-7-2020-2.xlsx",
    sheet = "Mr. Trash Wheel",
    range = "A2:N408",
    )

#### cleaning the data
df_trash <-
    df_trash %>%
    janitor::clean_names() %>%
    drop_na() %>%
    mutate(type_trash = "Mr.Trash")
```

Rounding the number of sports balls to the nearest integer and converts
the result to an integer variable:

``` r
df_trash <-
  df_trash %>%
  filter(!str_detect(month, "Total")) %>%

  mutate(sports_balls = as.integer(round(sports_balls, digits = 0)))

df_trash
```

    ## # A tibble: 345 × 15
    ##    dumpster month  year date                weight_tons volume…¹ plast…² polys…³
    ##       <dbl> <chr> <dbl> <dttm>                    <dbl>    <dbl>   <dbl>   <dbl>
    ##  1        1 May    2014 2014-05-16 00:00:00        4.31       18    1450    1820
    ##  2        2 May    2014 2014-05-16 00:00:00        2.74       13    1120    1030
    ##  3        3 May    2014 2014-05-16 00:00:00        3.45       15    2450    3100
    ##  4        4 May    2014 2014-05-17 00:00:00        3.1        15    2380    2730
    ##  5        5 May    2014 2014-05-17 00:00:00        4.06       18     980     870
    ##  6        6 May    2014 2014-05-20 00:00:00        2.71       13    1430    2140
    ##  7        7 May    2014 2014-05-21 00:00:00        1.91        8     910    1090
    ##  8        8 May    2014 2014-05-28 00:00:00        3.7        16    3580    4310
    ##  9        9 June   2014 2014-06-05 00:00:00        2.52       14    2400    2790
    ## 10       10 June   2014 2014-06-11 00:00:00        3.76       18    1340    1730
    ## # … with 335 more rows, 7 more variables: cigarette_butts <dbl>,
    ## #   glass_bottles <dbl>, grocery_bags <dbl>, chip_bags <dbl>,
    ## #   sports_balls <int>, homes_powered <dbl>, type_trash <chr>, and abbreviated
    ## #   variable names ¹​volume_cubic_yards, ²​plastic_bottles, ³​polystyrene

Reading professor trash wheel:

``` r
pf_trash <- read_excel("/Users/jag/Downloads/Trash-Wheel-Collection-Totals-7-2020-2.xlsx",
    sheet = "Professor Trash Wheel",
    range = "A2:N408") %>%
    drop_na()
#### cleaning the data
pf_trash <-
    pf_trash %>%
    janitor::clean_names() %>%
    mutate(type_trash = "Pr.Trash")
```

``` r
merged_df <- 
  rbind(df_trash, pf_trash)
```

``` r
merged_df %>%
  filter(type_trash == "Pr.Trash") %>%
  select(weight_tons) %>%
  sum()
```

    ## [1] 135.5

``` r
merged_df %>%
  group_by(type_trash) %>%
  summarise(sum(weight_tons))
```

    ## # A tibble: 2 × 2
    ##   type_trash `sum(weight_tons)`
    ##   <chr>                   <dbl>
    ## 1 Mr.Trash                1125.
    ## 2 Pr.Trash                 135.

``` r
sum(merged_df[merged_df$type_trash=="Pr.Trash", "weight_tons"])
```

    ## [1] 135.5

``` r
sum(merged_df[merged_df$type_trash=="Mr.Trash", "sports_balls"])
```

    ## [1] 4074

# Problem 3

Reading pols-months from FiveThirtyEight data.

``` r
pol_df <- 
  read_csv("/Users/jag/Downloads/fivethirtyeight_datasets/pols-month.csv") %>%
  janitor::clean_names()
```

    ## Rows: 822 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (8): prez_gop, gov_gop, sen_gop, rep_gop, prez_dem, gov_dem, sen_dem, r...
    ## date (1): mon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#Use separate() to break up the variable mon into integer variables year, month, and day
pol_df <- pol_df%>%
  separate(col = mon, into = c("year","month", "day")) %>%
  mutate(across(.cols = c(year, month, day), as.integer)) %>%
  mutate(month = month.name[month]) 

#president variable taking values gop and dem
pol_df <- pol_df%>%
  mutate(president = case_when(prez_dem == 1 ~ "dem", 
                               prez_gop == 1 ~ "gop",
                              prez_gop == 2 ~ "gop")) 

#president variable taking values gop and dem
pol_df <- pol_df%>%
  select(everything(), -day, -prez_dem, -prez_gop)
```

Reading the snp data

``` r
snp_df <- 
  read_csv("/Users/jag/Downloads/fivethirtyeight_datasets/snp.csv") %>%
  janitor::clean_names()
```

    ## Rows: 787 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): date
    ## dbl (1): close
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
snp_df <- snp_df %>%
  separate(date, sep = "/", into = c("month", "day", "year")) %>%
  mutate(across(.cols = c(month, day, year), as.integer)) %>%
  mutate(month = month.name[month])  %>%
  mutate(year = ifelse(year > 21, 
                       1900 + year, 
                       2000 + year)) %>%
  select(year, month, close)
```

Import and cleaning the uneployment data

``` r
#### First we will clean the column names ####
ue_df <- read_csv("/Users/jag/Downloads/fivethirtyeight_datasets/unemployment.csv") %>%
  janitor::clean_names() 
```

    ## Rows: 68 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (13): Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
####### the months into a column so that we can merge
ue_df <- ue_df %>%
  pivot_longer(
    jan:dec,
    names_to = "month",
    values_to = "percentage") 
```

Correcting the year values from the acronyms to full month names:

``` r
ue_df <-ue_df %>%
  mutate(across(.col = c(year), as.integer)) %>%
  mutate(month = recode(month, "jan" = "January", "feb" = "February", "mar" = "March", "apr" = "April", "may" = "May", "jun" = "June",                                "jul" = "July", "aug" = "August", "sep" = "September", "oct" = "October", "nov" = "November", "dec" =                                  "December"))
```

``` r
pol_snp_df <- left_join(pol_df, snp_df, by = c("year", "month"))
net_df <-left_join(pol_snp_df, ue_df, by = c("year", "month"))
```
