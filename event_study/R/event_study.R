
# 準備 ----------------------------------------------------------------------

## パッケージの読み込み ----
pacman::p_load(tidyverse,
               magrittr,
               arrow,
               panelView,
               fixest,
               did)

## データの読み込み ----



## ベースDID ----
df_base
panelview()

res <- fixest::feols(y ~ i(time, treated, 9) | id + time,
                     data = data,
                     cluster = "id")
fixest::etable(res)
fixest::iplot(res)

## ベースDID＋共変量 ----

df_base_cov
res_cov <- fixest::feols(y ~ i(time, treated, 9)+x | id + time,
                         data = df_base_cov)
fixest::etable(res_cov)
fixest::iplot(res_cov)




## Staggered DiD ----


data(base_stagg)
res_2 <- fixest::feols(y ~ sunab(year_treated, time) | id + time,
                     data = df_2)
fixest::etable(res_2)
fixest::iplot(res_2)


## Staggered DID＋共変量 ----
res_2 <- fixest::feols(y ~ sunab(year_treated, time)+x | id + time,
                       data = df_stagg_cov)
fixest::etable(res_2)
fixest::iplot(res_2)


df_stagg_cov

## Staggered DiD ----
data(mpdta)

out <- att_gt(
  yname = "y",
  gname = "year_treated",
  idname = "id",
  tname = "time",
  xformla = ~1,
  data = df_stagg,
  est_method = "reg",
  base_period = "universal",
  control_group = "nevertreated"
)
ggdid(out)
es <- aggte(out, type = "dynamic")
ggdid(es)


## Staggered DID＋共変量 ----
out <- att_gt(
  yname = "y",
  gname = "year_treated",
  idname = "id",
  tname = "time",
  xformla = ~ 1,
  data = df_stagg_cov,
  est_method = "reg",
  base_period = "universal",
  control_group = "nevertreated"
)
ggdid(out)
es <- aggte(out, type = "dynamic")
ggdid(es)
summary(es)


## Staggered DID＋共変量（） ----
out <- att_gt(
  yname = "y",
  gname = "year_treated",
  idname = "id",
  tname = "time",
  xformla = ~ 1,
  data = df_conditional,
  est_method = "reg",
  base_period = "universal",
  control_group = "nevertreated"
)
ggdid(out)
es <- aggte(out, type = "dynamic")
ggdid(es)
summary(es)


out <- att_gt(
  yname = "y",
  gname = "year_treated",
  idname = "id",
  tname = "time",
  xformla = ~ 1+x,
  data = df_conditional,
  est_method = "reg",
  base_period = "universal",
  control_group = "nevertreated"
)
ggdid(out)
es <- aggte(out, type = "dynamic")
ggdid(es)
summary(es)

