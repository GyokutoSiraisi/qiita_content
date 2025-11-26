
# データの作成 ------------------------------------------------------------------

set.seed(42)

## ベースDID ----
df_base <-
  tibble(
    expand_grid(id   = 1:100,
                time = 1:20),
    treated = if_else(id <= 50, 1, 0),
    y = 50 + 
      (time * 2) +
      (treated * 5) +
      ((treated & time >= 10) * 10) + 
      rnorm(2000, mean = 0, sd = 5)
  )

## ベースDID＋共変量 ----
df_base_cov <-
  tibble(
    expand_grid(id   = 1:100,
                time = 1:20),
    treated = if_else(id <= 50, 1, 0),
    x = if_else(treated == 1, 
                time * 0.5 + rnorm(2000, 0, 1),
                rnorm(2000, 0, 1)),
    y = 50 + 
      (time * 1) +
      (treated * 5) +
      (3 * x) +
      ((treated & time >= 10) * 10) + 
      rnorm(2000, mean = 0, sd = 5)
  )

## Staggered DiD ----
df_stagg <-
  tibble(
    expand_grid(id   = 1:120,
                time = 1:20),
    treated      = if_else(id <= 60, 1, 0),
    year_treated = case_when(id <= 20  ~ 9,
                             id <= 40  ~ 12,
                             id <= 60 ~ 15,
                             id <= 120 ~ 10000),
    time_to_treated = if_else(treated == 1, time - year_treated, -1000),
    y = 50 + 
      (time * 2) +                
      (treated * 5) +
      ((treated & time_to_treated >= 0) * 10) +        
      rnorm(2400, mean = 0, sd = 5)
  )

## Staggered DID＋共変量 ----
df_stagg_cov <-
  tibble(
    expand_grid(id   = 1:120,
                time = 1:20),
    treated      = if_else(id <= 60, 1, 0),
    year_treated = case_when(id <= 20  ~ 9,
                             id <= 40  ~ 12,
                             id <= 60 ~ 15,
                             id <= 120 ~ 10000),
    time_to_treated = if_else(treated == 1, time - year_treated, -1000),
    x = if_else(treated == 1,
                (time * 0.4) + rnorm(2400, 0, 1),
                rnorm(2400, 0, 1)),
    y = 50 + 
      (time * 1) +             
      (3 * x) +
      (treated * 5) +
      ((treated & time_to_treated >= 0) * 10) +         
      rnorm(2400, mean = 0, sd = 5),
  )

## Staggered DID＋共変量（） ----

df_stagg_cov_2 <-
  tibble(
    expand_grid(id   = 1:120,
                time = 1:20),
    treated      = if_else(id <= 60, 1, 0),
    year_treated = case_when(id <= 20  ~ 9,
                             id <= 40  ~ 12,
                             id <= 60 ~ 15,
                             id <= 120 ~ 10000),
    time_to_treated = if_else(treated == 1, time - year_treated, -1000),
    x = if_else(treated == 1,
                rnorm(2400, mean = 2, sd = 1), 
                rnorm(2400, mean = 0, sd = 1)),
    y = 50 + 
      (time * 0.5) +             
      (time * x * 0.8) +
      (x * 2) +
      ((treated & time_to_treated >= 0) * 10) +         
      rnorm(2400, mean = 0, sd = 5),
  )











