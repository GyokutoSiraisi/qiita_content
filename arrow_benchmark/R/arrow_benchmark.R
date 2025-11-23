

# 準備 ----------------------------------------------------------------------

## パッケージの読み込み ----
pacman::p_load(tidyverse,
               magrittr,
               arrow,
               here,
               see,
               patchwork,
               gt,
               gtExtras,
               microbenchmark,
               bench)

## データの読み込み ----

# mini
Mini_1 <- arrow::read_csv_arrow("data/winequality-red.csv")
Mini_2 <- arrow::read_csv_arrow("data/SSDSE-B-2025.csv",
                                   read_options = arrow::CsvReadOptions$create(encoding = "Shift_JIS",
                                                                               skip_rows = 1))
Mini_3 <- arrow::read_csv_arrow("data/SSDSE-A-2025.csv",
                                read_options = arrow::CsvReadOptions$create(encoding = "Shift_JIS",
                                                                            skip_rows = 1))
# Middle
Middle <- arrow::read_csv_arrow("data/LAPD_data.csv")

# Large
# curl::multi_download(
#   "https://r4ds.s3.us-west-2.amazonaws.com/seattle-library-checkouts.csv",
#   "data/seattle_library_data.csv",
#   resume = TRUE
# )


# データの可視化 -----------------------------------------------------------------

data_dis <- tibble(name　　　= c("軽量データ１", "軽量データ２", "軽量データ３",
                                 "中規模データ", "大規模データ"),
                   row       = c(sapply(list(Mini_1, Mini_2, Mini_3, Middle), nrow), 41389465),
                   col       = c(sapply(list(Mini_1, Mini_2, Mini_3, Middle), ncol), 12),
                   file_size = fs::file_size(c("data/winequality-red.csv", "data/SSDSE-B-2025.csv",
                                               "data/SSDSE-A-2025.csv", "data/LAPD_data.csv",
                                               "data/seattle_library_data.csv")),
                   file_byte = as.numeric(file_size))

## 表の出力
gt_dis <-
  data_dis %>%
  dplyr::mutate(file_byte = file_byte/1024^2) %>% 
  gt() %>%
  opt_table_font(font = "Meiryo") %>% 
  cols_label(name      = "",
             row 　　　= "行数",
             col 　　　= "列数",
             file_size = "サイズ",
             file_byte = "（MB）") %>% 
  tab_header(title = "使用するデータの情報") %>% 
  cols_align(align = "right",
             columns = file_size) %>%
  fmt_number(columns = file_byte,
             decimals = 2,
             use_seps = FALSE) %>% 
  cols_width(name      ~ px(150),
             col       ~ px(60),
             everything() ~ px(100)) %T>% 
  print() 
gtsave(gt_dis, "output/file_info.png")

## 図の出力
windowsFonts("Meiryo" = windowsFont("Meiryo"))

p_1 <- ggplot(slice(data_dis, -1, -2), aes(x = fct_reorder(name, file_size), y = file_size)) +
  geom_col(fill = "#56B4E9", alpha = 0.9) +
  scale_y_continuous(limits = c(0, 1e10),
                     expand = c(0, 0),
                     breaks = seq(from = 0, to = 8e9, by = 2e9),
                     labels = seq(from = 0, to = 8, by = 2),
                     name   = "ファイルサイズ（GB）") +
  scale_x_discrete(name = NULL,
                   expand = c(0, 0.5)) +
  coord_flip(clip = "off") +
  theme(
    text = element_text(family = "Meiryo"),
    axis.title.x = element_text(size = 8),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major.x = element_line(colour = "lightgray", linewidth = 0.5))

p_2 <- ggplot(slice(data_dis, -c(1:2,5)), aes(x = fct_reorder(name, file_size), y = file_size)) +
  geom_col(fill = "#56B4E9", alpha = 0.9, width = 0.6) +
  scale_y_continuous(limits = c(0, 3e8),
                     expand = c(0, 0),
                     breaks = c(0, 1e8, 2e8),
                     labels = c(0, 1, 2),
                     name   = "ファイルサイズ（MB）") +
  scale_x_discrete(name = NULL,
                   expand = c(0, 0.5)) +
  coord_flip(clip = "off") +
  theme(
    text = element_text(family = "Meiryo"),
    axis.title.x = element_text(size = 8),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major.x = element_line(colour = "lightgray", linewidth = 0.5))

p_3 <- ggplot(slice(data_dis, -c(4:5)), aes(x = fct_reorder(name, file_size), y = file_byte/1024)) +
  geom_col(fill = "#56B4E9", alpha = 0.9, width = 0.6) +
  scale_y_continuous(limits = c(0, 1000),
                     expand = c(0, 0),
                     breaks = seq(0, 800, 200),
                     labels = seq(0, 800, 200),
                     name   = "ファイルサイズ（KB）") +
  scale_x_discrete(name = NULL,
                   expand = c(0, 0.5)) +
  coord_flip(clip = "off") +
  theme(
    text = element_text(family = "Meiryo"),
    axis.title.x = element_text(size = 8),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major.x = element_line(colour = "lightgray", linewidth = 0.5))

p_1/p_2/p_3

ggsave("output/file_size.png", plot = (p_1/p_2/p_3), dpi = 300)


# 検証 ----------------------------------------------------------------------

## ミニデータ ----
bench_mini_1 <- microbenchmark(
  "readr\n （軽量データ1）" = {readr::read_csv("data/winequality-red.csv", show_col_types = FALSE)},
  "arrow\n （軽量データ1）" = {arrow::read_csv_arrow("data/winequality-red.csv")},
  "readr\n （軽量データ2）" = {readr::read_csv("data/SSDSE-B-2025.csv",
                                        locale = locale(encoding = "shift-jis"),
                                        show_col_types = FALSE,
                                        skip = 1)},
  "arrow\n （軽量データ2）" = {arrow::read_csv_arrow("data/SSDSE-B-2025.csv",
                                              read_options = arrow::CsvReadOptions$create(encoding = "Shift_JIS",
                                                                                          skip_rows = 1))},
  "readr\n （軽量データ3）" = {readr::read_csv("data/SSDSE-A-2025.csv",
                                        locale = locale(encoding = "shift-jis"),
                                        show_col_types = FALSE,
                                        skip = 2)},
  "arrow\n （軽量データ3）" = {arrow::read_csv_arrow("data/SSDSE-A-2025.csv",
                                              read_options = arrow::CsvReadOptions$create(encoding = "Shift_JIS",
                                                                                          skip_rows = 1))}
  )
bench_mini_2 <- bench::mark(
  "readr\n （軽量データ1）" = {readr::read_csv("data/winequality-red.csv", show_col_types = FALSE)},
  "arrow\n （軽量データ1）" = {arrow::read_csv_arrow("data/winequality-red.csv")},
  "readr\n （軽量データ2）" = {readr::read_csv("data/SSDSE-B-2025.csv",
                                        locale = locale(encoding = "shift-jis"),
                                        show_col_types = FALSE,
                                        skip = 1)},
  "arrow\n （軽量データ2）" = {arrow::read_csv_arrow("data/SSDSE-B-2025.csv",
                                              read_options = arrow::CsvReadOptions$create(encoding = "Shift_JIS",
                                                                                          skip_rows = 1))},
  "readr\n （軽量データ3）" = {readr::read_csv("data/SSDSE-A-2025.csv",
                                        locale = locale(encoding = "shift-jis"),
                                        show_col_types = FALSE,
                                        skip = 2)},
  "arrow\n （軽量データ3）" = {arrow::read_csv_arrow("data/SSDSE-A-2025.csv",
                                              read_options = arrow::CsvReadOptions$create(encoding = "Shift_JIS",
                                                                                          skip_rows = 1))},
  check = FALSE
  )

## マイクロベンチマークのプロット
bench_mini_1 %<>% mutate(package = str_extract(expr, "^[a-z]+"))
p_mini <- 
  autoplot(bench_mini_1) + 
  aes(fill = package) +
  labs(title = "") + 
  see::scale_fill_okabeito() + 
  scale_y_log10(name="実行時間（ミリ秒）（反復100回）")+
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )
p_mini

ggsave("output/bench_mini.png", plot = p_mini,
       dpi = 300, width = 700*3, height = 500*3, units = "px")
# *3 はPreview時のdpiとのズレを直すため

## ベンチのプロット
gt_bench_1 <-
  bench_mini_2 %>% 
  dplyr::mutate(proc = str_extract(as.character(expression), "^[a-z]+"),
                data = str_extract(as.character(expression), "軽量データ\\d+")) %>% 
  dplyr::select(proc, data, 2, 3, 4, 5) %>%
  group_by(data) %>% 
  gt() %>% 
  opt_table_font(font = "Meiryo") %>% 
  cols_label(proc      = "",
             min 　　　= "実行時間（最小値）",
             median    = "（中央値）",
             `itr/sec` = "1秒あたり実行回数",
             mem_alloc = "使用メモリ") %>% 
  tab_header(title = "軽量データのベンチマーク") %>% 
  cols_align(align = "right",
             columns = everything()) %>%
  fmt_number(columns  = `itr/sec`,
             decimals = 2,
             use_seps = FALSE) %>% 
  cols_width(proc         ~ px(170),
             min          ~ px(160),
             `itr/sec`    ~ px(150),
             everything() ~ px(110)) %T>% 
  print()

gtsave(gt_bench_1, "output/bench_info_mini.png")


## ミドルデータ ----
bench_middle_1 <- microbenchmark(
  "readr\n （中規模データ）" = {readr::read_csv("data/LAPD_data.csv", show_col_types = FALSE)},
  "arrow\n （中規模データ）" = {arrow::read_csv_arrow("data/LAPD_data.csv")}
)
bench_middle_2 <- bench::mark(
  "readr\n （中規模データ）" = {readr::read_csv("data/LAPD_data.csv", show_col_types = FALSE)},
  "arrow\n （中規模データ）" = {arrow::read_csv_arrow("data/LAPD_data.csv")},
  check = FALSE
)

## マイクロベンチマークのプロット
bench_middle_1 %<>% mutate(package = str_extract(expr, "^[a-z]+"))
p_middle <- 
  autoplot(bench_middle_1) + 
  aes(fill = package) +
  labs(title = "") + 
  see::scale_fill_okabeito() +   scale_y_log10(name="実行時間（秒）（反復100回）")+
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
    )
p_middle

ggsave("output/bench_middle.png", plot =p_middle,
       dpi = 300, width = 700*3, height = 500*3, units = "px")

## ベンチのプロット
gt_bench_2 <-
  bench_middle_2 %>% 
  dplyr::mutate(proc = str_extract(as.character(expression), "^[a-z]+")) %>% 
  dplyr::select(proc, 2, 3, 4, 5) %>%
  gt() %>% 
  opt_table_font(font = "Meiryo") %>% 
  cols_label(proc      = "",
             min 　　　= "実行時間（最小値）",
             median    = "（中央値）",
             `itr/sec` = "1秒あたり実行回数",
             mem_alloc = "使用メモリ") %>% 
  tab_header(title = "中規模データのベンチマーク") %>% 
  cols_align(align = "right",
             columns = everything()) %>%
  fmt_number(columns  = `itr/sec`,
             decimals = 2,
             use_seps = FALSE) %>% 
  cols_width(proc         ~ px(170),
             min          ~ px(160),
             `itr/sec`    ~ px(150),
             everything() ~ px(110)) %T>% 
  print() 

gtsave(gt_bench_2, "output/bench_info_middle.png")


## 作業環境
sessionInfo()
