# 探索六都房價與人口流動、物價、利率等關係，並產出圖表與 CSV

# 如果第一次執行，而且尚未安裝套件，需先執行一次
# install.packages(c(
#     "tidyverse",     # 整合 dplyr、ggplot2、readr 等常用工具
#     "readr",         # 讀取 CSV
#     "lubridate",     # 日期處理
#     "forecast",      # ARIMA 時序預測
#     "janitor",       # 欄位清理（clean_names 等）
#     "car",           # VIF 共線性檢查與 Durbin–Watson 檢定
#     "broom",         # augment(), glance(), tidy 模型輸出表格
#     "ggcorrplot",    # 熱圖（相關係數）
#     "ggpubr",        # ggarrange() 多圖排版
#     "lmtest",        # bptest(), coeftest() 等假設檢定
#     "sandwich",      # Robust SE（vcovHC）
#     "nlme",          # GLS 模型（corAR1）
#     "plm"            # panel 資料固定效果回歸
# ))

# 載入所需套件
library(tidyverse)    # 資料處理與繪圖
library(readr)        # 讀取 CSV
library(lubridate)    # 處理日期
library(forecast)     # 時間序列預測
library(janitor)      # 欄位清理
library(purrr)        # remove_outlier 會用到 reduce
library(car)

options(ggsave.bg = "white")

# ChatGPT, respond to “我要怎麼寫一個可以重複使用的 outlier 過濾函式？” on May 22, 2025
# 通用極端值過濾器：method = "iqr" 或 "z"，coef 為倍率
remove_outlier <- function(data, cols, method = c("iqr", "z"), coef = 1.5) {
    method <- match.arg(method)
    purrr::reduce(
        cols,
        function(df, v) {
            x <- df[[v]]
            if (method == "iqr") {
                q   <- stats::quantile(x, probs = c(.25, .75), na.rm = TRUE)
                iqr <- diff(q)
                lo  <- q[1] - coef * iqr
                hi  <- q[2] + coef * iqr
            } else {                    # z-score
                m  <- mean(x, na.rm = TRUE)
                sd <- stats::sd(x,  na.rm = TRUE)
                lo <- m - coef * sd
                hi <- m + coef * sd
            }
            dplyr::filter(df, dplyr::between(.data[[v]], lo, hi))
        },
        .init = data
    )
}

# ChatGPT, respond to “為什麼我的 ggplot2 圖表背景不是白的？要怎麼設定白底” on May 20, 2025
# 全域切換為白底佈景，避免圖表內容難以理解
theme_set(theme_bw())

# 讀取原始資料並整理欄位
df_raw <- read_csv("六都房價整合表_UTF8.csv")

# ChatGPT, respond to “read_excel() 讀進來的欄位名稱含有換行符號，要怎麼清理？” on May 20, 2025
# 去除欄名中的換行符號，方便後續 rename()
names(df_raw) <- names(df_raw) %>%
    str_replace_all("[\r\n]", "")

# ChatGPT, respond to “mutate() 中使用 ymd(paste0()) 是什麼意思？為什麼可以用這樣的方式轉日期？” on May 20, 2025
# 重新命名並製作新欄位   ← **修正❶：遷入 / 遷出 欄位名稱**
df <- df_raw %>%
    rename(
        城市       = city,
        年         = year,
        月         = month,
        貸款利率   = `平均購屋貸款利率_(年息百分比率)`,
        年所得     = `每戶可支配年所得_`,
        CPI        = `CPI_`,
        失業率     = `失業率`,
        房價_平均  = `住宅大樓單價_平均`,
        房價_中位  = `住宅大樓單價_中位`,
        遷入       = `遷入總人數_(人)`,
        遷出       = `遷出總人數_(人)`
    ) %>%
    mutate(
        across(
            c(貸款利率, 年所得, CPI, 失業率, 房價_平均, 房價_中位, 遷入, 遷出),
            ~ parse_number(as.character(.x),
                           locale = locale(decimal_mark = ".", grouping_mark = ","))
        )
    ) %>%
    mutate(
        城市 = str_trim(城市),             # 去掉前後空白
        日期 = ymd(paste0(年, "-", 月, "-01")),
        人口淨流入 = 遷入 - 遷出
    ) %>%
    filter(!is.na(城市) & nzchar(城市)) %>% 
    relocate(日期, .after = 月)

# ChatGPT, respond to “unique() 是做什麼用的？為什麼要用它來取得城市列表？” on May 20, 2025
cities <- df$城市 |> unique() |> na.omit()

if (!dir.exists("output")) dir.create("output")

# 逐城市繪圖：❶–❹
for (ct in cities) {
    city_dir <- file.path("output", ct)
    if (!dir.exists(city_dir)) dir.create(city_dir)
    d <- df %>% filter(城市 == ct)

    # ❶ 房價(中位) & 人口淨流入
    g1 <- ggplot(d, aes(x = 日期)) +
        geom_line(aes(y = 房價_中位, colour = "房價(中位)"), size = 1) +
        geom_line(aes(y = 人口淨流入 / 100, colour = "人口淨流入 ÷100"), size = 1) +
        scale_color_manual(values = c("房價(中位)" = "steelblue", "人口淨流入 ÷100" = "firebrick")) +
        scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
        labs(title = paste0(ct, " 房價與人口變動"), x = NULL, y = NULL, colour = NULL) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggsave(file.path(city_dir, "01_房價_vs_人口.png"), g1, width = 8, height = 4)

    # ❷ CPI & 房價(平均)
    g2 <- ggplot(d, aes(x = 日期)) +
        geom_line(aes(y = CPI, colour = "CPI"), size = 1) +
        geom_line(aes(y = 房價_平均, colour = "房價(平均)"), size = 1) +
        scale_color_manual(values = c("CPI" = "orange", "房價(平均)" = "steelblue")) +
        scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
        labs(title = paste0(ct, " CPI 與房價"), x = NULL, y = NULL, colour = NULL) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggsave(file.path(city_dir, "02_CPI_vs_房價.png"), g2, width = 8, height = 4)

    # ❸ 貸款利率 & 房價(平均)
    g3 <- ggplot(d, aes(x = 日期)) +
        geom_line(aes(y = 貸款利率, colour = "利率"), size = 1) +
        geom_line(aes(y = 房價_平均, colour = "房價(平均)"), size = 1) +
        scale_color_manual(values = c("利率" = "purple", "房價(平均)" = "steelblue")) +
        scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
        labs(title = paste0(ct, " 房價與利率"), x = NULL, y = NULL, colour = NULL) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggsave(file.path(city_dir, "03_利率_vs_房價.png"), g3, width = 8, height = 4)

    # ❹ 時間序列預測：未來 60 期（5 年）   ← **修正❷：起始點以首筆資料為準**
    d <- d %>% arrange(年, 月)                                # 保證時間遞增
    ts_data <- ts(d$房價_平均,
                  start = c(d$年[1], d$月[1]),
                  frequency = 12)
    fit <- auto.arima(ts_data)
    fc  <- forecast(fit, h = 60)
    g4 <- autoplot(fc) +
        labs(title = paste0(ct, " 未來 5 年房價(平均)預測"),
             x = "時間", y = "房價(平均)") +
        geom_vline(xintercept = time(ts_data)[length(ts_data)],
                   linetype = "dashed", colour = "red")
    ggsave(file.path(city_dir, "04_房價預測.png"), g4, width = 8, height = 4)
}

# 全域分析：❺–❿

## ❺ 人口淨流入 vs 房價(平均)
g5 <- ggplot(df, aes(x = 人口淨流入, y = 房價_平均, colour = 城市)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, colour = "black") +
    labs(title = "❺ 人口淨流入 vs 房價（六都）", x = "人口淨流入", y = "房價(平均)")
ggsave("output/05_人口_vs_房價.png", g5, width = 6, height = 4)

## ❻ 失業率 vs 房價(平均)
# ChatGPT, respond to “為什麼要用 parse_number() 處理失業率？原本不是數字嗎？” on May 21, 2025
df <- df %>% mutate(失業率_num = as.numeric(失業率))

df_g6 <- df %>%
    remove_outlier(c("失業率_num", "房價_平均"), method = "iqr", coef = 1.5)
g6 <- ggplot(df_g6, aes(x = 失業率_num, y = 房價_平均, colour = 城市)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, colour = "black") +
    labs(title = "❻ 失業率 vs 房價（六都）", x = "失業率 (%)", y = "房價(平均)")
ggsave("output/06_失業率_vs_房價.png", g6, width = 6, height = 4)

## ❼ 變化率分析：房價 vs 利率
# ChatGPT, respond to “為什麼要用 lag() 來計算變化率？不直接相減不行嗎？” on May 21, 2025
# ChatGPT, respond to “為什麼要用 drop_na()？我直接畫圖不行嗎？” on May 21, 2025
df_change <- df %>%
    arrange(城市, 日期) %>%
    group_by(城市) %>%
    mutate(
        房價變化率 = (房價_平均 / lag(房價_平均) - 1) * 100,
        利率變化率 = (貸款利率 / lag(貸款利率) - 1) * 100
    ) %>%
    drop_na()

df_change_cln <- df_change %>%
    remove_outlier(c("房價變化率", "利率變化率"), method = "z", coef = 2.5)
g7 <- ggplot(df_change_cln, aes(x = 利率變化率, y = 房價變化率, colour = 城市)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, colour = "black") +
    labs(title = "❼ 房價變化率 vs 利率變化率", x = "利率變化率 (%)", y = "房價變化率 (%)")
ggsave("output/07_變化率_vs_變化率.png", g7, width = 6, height = 4)

## ❽ 六都平均房價時序
g8 <- df %>%
    group_by(日期) %>%
    summarize(平均房價 = mean(房價_平均, na.rm = TRUE)) %>%
    ggplot(aes(x = 日期, y = 平均房價)) +
    geom_line(colour = "steelblue", size = 1) +
    labs(title = "❽ 六都房價平均時序趨勢", x = NULL, y = "平均房價(平均)")
ggsave("output/08_平均房價時序.png", g8, width = 8, height = 4)

## ❾ 變化率 vs 淨遷徙變化率
df_change2 <- df_change %>%
    group_by(城市) %>%
    mutate(淨流入變化率 = (人口淨流入 / lag(人口淨流入) - 1) * 100) %>%
    ungroup() %>%
    drop_na(房價變化率, 淨流入變化率)

df_change2_cln <- df_change2 %>%
    remove_outlier(c("房價變化率", "淨流入變化率"), method = "z", coef = 2.5)
g9 <- ggplot(df_change2_cln,
             aes(x = 淨流入變化率, y = 房價變化率, colour = 城市)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, colour = "black") +
    labs(title = "❾ 房價變化率 vs 淨遷徙變化率", x = "淨流入變化率 (%)", y = "房價變化率 (%)")
ggsave("output/09_變化率_vs_淨流入變化率.png", g9, width = 6, height = 4)

## ❿ 預測誤差比較：各都 RMSE
# ChatGPT, respond to “fitted() 是做什麼的？為什麼可以用來算 RMSE？” on May 21, 2025
# ChatGPT, respond to “怎麼用 mean + sd 判斷異常值？這樣準確嗎？” on May 21, 2025
errors <- map_df(cities, function(ct) {
    dct <- df %>% filter(城市 == ct)
    ts_ct <- ts(dct$房價_平均, start = c(min(dct$年), min(dct$月)), frequency = 12)
    fit_ct <- auto.arima(ts_ct)
    rmse_ct <- sqrt(mean((fitted(fit_ct) - ts_ct)^2, na.rm = TRUE))
    tibble(城市 = ct, RMSE = rmse_ct)
})
thr <- mean(errors$RMSE) + sd(errors$RMSE)
errors <- errors %>% mutate(異常 = RMSE > thr)
g10 <- ggplot(errors, aes(x = reorder(城市, RMSE), y = RMSE, fill = 異常)) +
    geom_col() +
    geom_hline(yintercept = thr, linetype = "dashed", colour = "darkred") +
    annotate("text", x = Inf, y = thr, hjust = 1.05, vjust = -0.4,
             colour = "darkred", label = sprintf("Threshold = %.2f", thr)) +
    coord_flip() +
    scale_fill_manual(values = c("FALSE" = "gray70", "TRUE" = "red")) +
    labs(title = "❿ 各城市預測 RMSE (標記異常)", x = NULL, y = "RMSE", fill = "異常")
ggsave("output/10_預測誤差異常.png", g10, width = 6, height = 4)

message("各縣市與全域分析已完成，並輸出至 output/ ")

# 房價排名 & 殘差異常

## 六都整體「平均房價」排名
# ChatGPT, respond to “summarise() 裡的 .groups = 'drop' 是什麼意思？” on May 21, 2025
avg_price <- df %>%
    group_by(城市) %>%
    summarise(平均房價 = mean(房價_平均, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(平均房價)) %>%
    mutate(排名 = row_number())

write_csv(avg_price, "output/房價排名.csv")

# ChatGPT, respond to “coord_flip() 有什麼作用？為什麼 bar chart 要翻轉？” on May 21, 2025
g11 <- ggplot(avg_price, aes(x = reorder(城市, 平均房價), y = 平均房價)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(title = "六都整體平均房價排名", x = NULL, y = "平均房價（萬/坪）")
ggsave("output/11_房價排名.png", g11, width = 6, height = 4)

## 各都 ARIMA 殘差異常偵測
# ChatGPT, respond to “residuals() 是什麼？它代表什麼意思？怎麼用來判斷異常？” on May 21, 2025
resid_tbl <- map_df(cities, function(ct) {
    dct <- df %>% filter(城市 == ct)
    ts_ct <- ts(dct$房價_平均, start = c(min(dct$年), min(dct$月)), frequency = 12)
    fit   <- auto.arima(ts_ct)
    tibble(
        城市 = ct,
        日期 = seq.Date(from = min(dct$日期), by = "month", length.out = length(residuals(fit))),
        殘差 = as.numeric(residuals(fit))
    )
})

# ChatGPT, respond to “為什麼用 2 倍標準差來當作異常的判斷依據？” on May 21, 2025
resid_flag <- resid_tbl %>%
    group_by(城市) %>%
    mutate(
        mean_res = mean(殘差),
        sd_res   = sd(殘差),
        異常     = abs(殘差 - mean_res) > 2 * sd_res
    ) %>%
    ungroup() %>%
    select(城市, 日期, 殘差, 異常)

write_csv(resid_flag, "output/殘差異常清單.csv")

# ChatGPT, respond to “sum(異常) 是怎麼算的？為什麼這樣可以統計異常次數？” on May 21, 2025
anom_count <- resid_flag %>%
    group_by(城市) %>%
    summarise(異常次數 = sum(異常), .groups = "drop")

# ChatGPT, respond to “scale_fill_gradient() 要怎麼設定顏色？為什麼用灰到紅的漸層？” on May 21, 2025
g12 <- ggplot(anom_count, aes(x = reorder(城市, 異常次數), y = 異常次數, fill = 異常次數)) +
    geom_col() +
    coord_flip() +
    scale_fill_gradient(low = "gray70", high = "red") +
    labs(title = "各都 ARIMA 殘差異常次數", x = NULL, y = "異常次數")
ggsave("output/12_殘差異常次數.png", g12, width = 6, height = 4)

message("房價排名 及 殘差異常 已輸出至 output/")

# ⑬ 六都房價排名變化（時間序列整合圖）
# ChatGPT, respond to “為什麼要用 rank() + scale_y_reverse() 來畫排名變化？” on May 22, 2025
df_rank <- df %>%
    group_by(日期) %>%
    mutate(房價排名 = rank(-房價_平均)) %>%
    ungroup()

# A. 點圖版本（更清晰）
g13_point <- ggplot(df_rank, aes(x = 日期, y = 房價排名, colour = 城市)) +
    geom_point(size = 1.2, alpha = 0.9, shape = 16) +
    scale_y_reverse(breaks = 1:length(cities)) +
    scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
    labs(title = "六都房價排名變化（每坪平均價）- 點圖版",
         x = "月份", y = "房價排名 (1 最高)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("output/六都房價排名變化_點圖.png", g13_point, width = 8, height = 4, bg = "white")

# B. 折線圖版本（保留趨勢）
g13_line <- ggplot(df_rank, aes(x = 日期, y = 房價排名, colour = 城市)) +
    geom_line(linewidth = 0.6, alpha = 0.9) +  # ← 比之前細
    scale_y_reverse(breaks = 1:length(cities)) +
    scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
    labs(title = "六都房價排名變化（每坪平均價）- 折線圖版",
         x = "月份", y = "房價排名 (1 最高)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("output/六都房價排名變化_折線圖.png", g13_line, width = 8, height = 4, bg = "white")



# ⑭ 殘差最大前 10 筆資料（多元線性迴歸）
# ChatGPT, respond to “為什麼只挑前 10 筆殘差？這樣足夠嗎？” on May 22, 2025
lm_model <- lm(房價_平均 ~ 人口淨流入 + CPI + 貸款利率 + 年所得 + 失業率, data = df)

df_residuals <- df %>%
    mutate(
        預測房價 = predict(lm_model, .),
        殘差     = abs(房價_平均 - 預測房價)
    ) %>%
    arrange(desc(殘差)) %>%
    slice_head(n = 10)
write_csv(df_residuals, "output/殘差最大前10筆資料.csv")

# ⑮ 人口下降但房價上升案例偵測
# ChatGPT, respond to “為什麼要 lag() 房價再比較？直接看當期漲跌不行嗎？” on May 22, 2025
df_conflict <- df %>%
    arrange(城市, 日期) %>%
    group_by(城市) %>%
    mutate(前期房價 = lag(房價_平均)) %>%
    ungroup() %>%
    filter(人口淨流入 < 0 & 房價_平均 > 前期房價) %>%
    select(城市, 日期, 人口淨流入, 房價_平均, 前期房價)
write_csv(df_conflict, "output/人口下降房價上升_偵測表.csv")

message("（排名變化圖、最大殘差 CSV、人口下降房價上升偵測）均完成，已輸出至 output/")

# ChatGPT, respond to "如何檢查多元線性迴歸共線性與模型解釋力？" on May 23, 2025
# ChatGPT, respond to "如何產生更完整的線性模型診斷與共線性檢查圖？" on May 23, 2025
# ChatGPT, respond to “Breusch-Pagan、Robust SE、固定效果、GLS / Panel 要怎麼寫？” on May 23, 2025
suppressPackageStartupMessages({
    library(lmtest)       # bptest(), coeftest()
    library(sandwich)     # vcovHC()
    library(nlme)         # gls()
    library(plm)          # panel 回歸
    library(broom)        # augment(), glance()
    library(ggcorrplot)   # 熱圖
    library(ggpubr)       # ggarrange()
})

# ChatGPT, respond to “要怎麼一次把建模會用到的欄位都去掉 NA？” on May 23, 2025
model_vars <- c("房價_平均", "人口淨流入", "CPI", "年所得", "貸款利率",
                "失業率", "日期", "城市", "月")   # 後面模型都會用到
df_mod <- df %>% drop_na(all_of(model_vars))      # 給所有模型用
df <- df_mod


# 建立原模型
lm_full <- lm(房價_平均 ~ 人口淨流入 + CPI + 年所得 + 貸款利率 + 失業率, data = df)
summary(lm_full)
vif(lm_full)

# 多重共線性：VIF 與相關係數熱圖
vif_vals <- vif(lm_full)
vif_tbl <- tibble(
    變數 = names(vif_vals),
    VIF  = vif_vals
) %>%
    mutate(等級 = case_when(
        VIF < 5  ~ "低",
        VIF < 10 ~ "中",
        TRUE     ~ "高"
    ))
write_csv(vif_tbl, "output/VIF_結果.csv")

corr_mat <- df %>%
    select(人口淨流入, CPI, 年所得, 貸款利率, 失業率) %>%
    cor(use = "pairwise.complete.obs")
g_corr <- ggcorrplot(corr_mat,
                     type = "lower",
                     lab = TRUE,
                     tl.cex = 9,
                     outline.col = "#555555",
                     colors = c("blue", "white", "red")) +  # 手動指定漸層（不會出現黑紫）
          labs(title = "主要解釋變數相關係數熱圖") +
          theme_bw()                            # 強制使用白底主題

ggsave("output/相關係數熱圖.png",
       g_corr,
       width = 5,
       height = 4,
       bg = "white")                            # 保險起見強制白底

# 殘差診斷圖
diag_df <- augment(lm_full) %>%
    mutate(
        觀測順序   = row_number(),
        標準化殘差 = .std.resid
    )

g_res_fit <- ggplot(diag_df, aes(x = .fitted, y = .resid)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
    labs(title = "殘差 vs. 預測值", x = "預測值", y = "殘差")

g_qq <- ggplot(diag_df, aes(sample = 標準化殘差)) +
    stat_qq(alpha = 0.6) +
    stat_qq_line(colour = "red") +
    labs(title = "殘差常態 Q–Q 圖", x = "理論分位數", y = "樣本分位數")

g_res_time <- ggplot(diag_df, aes(x = 觀測順序, y = .resid)) +
    geom_line(colour = "steelblue") +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
    labs(title = "殘差 vs. 順序（檢查自相關）", x = "觀測順序", y = "殘差")

ggarrange(g_res_fit, g_qq, g_res_time,
          ncol = 3, nrow = 1, labels = c("(a)", "(b)", "(c)"))
ggsave("output/殘差診斷圖_三合一.png", width = 10, height = 4)

# Durbin–Watson 檢定
dw_full <- car::durbinWatsonTest(lm_full)
write_lines(capture.output(dw_full), "output/Durbin_Watson.txt")

# 改良模型：對數轉換與交互作用
df2 <- df %>%
    mutate(
        log_人口淨流入 = log10(abs(人口淨流入) + 1),
        log_CPI        = log10(CPI),
        交互_人口x利率  = log_人口淨流入 * 貸款利率
    )

lm_opt <- lm(房價_平均 ~ log_人口淨流入 + log_CPI + 年所得 +
                           貸款利率 + 失業率 + 交互_人口x利率,
             data = df2)
summary(lm_opt)
vif(lm_opt)

# 模型比較
comparison <- tibble(
    模型      = c("原模型", "優化模型"),
    Adj_R2    = c(summary(lm_full)$adj.r.squared,
                  summary(lm_opt)$adj.r.squared),
    AIC       = c(AIC(lm_full), AIC(lm_opt))
)
write_csv(comparison, "output/迴歸模型比較.csv")

# 異質性、自相關修正與固定效果 / GLS / Panel 回歸

# Breusch-Pagan 檢定
bp_full <- bptest(lm_full)
write_lines(capture.output(bp_full), "output/Breusch_Pagan.txt")

# Robust SE (HC1)
robust_full <- coeftest(lm_full, vcov = vcovHC(lm_full, type = "HC1"))
write_lines(capture.output(robust_full), "output/lm_full_RobustSE.txt")

# 城市 + 月固定效果
lm_fe <- lm(
    房價_平均 ~ 人口淨流入 + CPI + 年所得 + 貸款利率 + 失業率 +
               factor(城市) + factor(月),
    data = df
)
write_lines(capture.output(summary(lm_fe)), "output/lm_FE_summary.txt")

# GLS (AR1) 修正自相關
# 產生每城市連續時間序號
df_gls <- df %>%
    arrange(城市, 日期) %>%                     # 先排序
    group_by(城市) %>%
    mutate(時間序號 = row_number()) %>%          # 每城市從 1,2,3… 編號
    ungroup()

# GLS (AR1) 修正自相關：以『時間序號』做 covariate
gls_ar1 <- nlme::gls(
    房價_平均 ~ 人口淨流入 + CPI + 年所得 + 貸款利率 + 失業率,
    data = df_gls,
    correlation = nlme::corAR1(form = ~ 時間序號 | 城市)
)
write_lines(capture.output(summary(gls_ar1)), "output/gls_AR1_summary.txt")

# Panel two-way 固定效果
# ChatGPT, respond to “plm duplicate couples 要怎麼排除？” on May 23, 2025
df_panel <- df %>%
    arrange(城市, 日期) %>%
    distinct(城市, 日期, .keep_all = TRUE)   # ← 避免 id-time 重覆
plm_tw <- plm(
    房價_平均 ~ 人口淨流入 + CPI + 年所得 + 貸款利率 + 失業率,
    data   = df_panel,
    index  = c("城市", "日期"),
    model  = "within",
    effect = "twoways"
)
plm_robust <- coeftest(plm_tw, vcov = plm::vcovHC(plm_tw, type = "HC1"))
write_lines(capture.output(plm_robust), "output/plm_TWOWAY_RobustSE.txt")

# 進階模型比較 (Adj.R² / AIC)

# ChatGPT, respond to "如何輸出 two-way FE 的城市與時間固定效果係數？" on May 23, 2025
# 城市固定效果（individual FE）
fe_city <- fixef(plm_tw, effect = "individual")
write_csv(
    tibble(城市 = names(fe_city), FE_係數 = as.numeric(fe_city)),
    "output/FE_city_coeff.csv"
)

# 時間固定效果（time FE）
fe_time <- fixef(plm_tw, effect = "time")
write_csv(
    tibble(時間 = names(fe_time), FE_係數 = as.numeric(fe_time)),
    "output/FE_time_coeff.csv"
)

# ChatGPT, respond to "fixed effect 整體 F-test 要怎麼寫？" on May 23, 2025
# FE 整體檢定（H0: 無固定效果）
fe_Ftest <- plm::pFtest(plm_tw, lm_full)
write_lines(capture.output(fe_Ftest), "output/FE_Ftest.txt")

# ChatGPT, respond to “gls 沒有 adj.r.squared，要怎麼放進比較表？” on May 23, 2025
comparison_ext <- tibble(
    模型   = c("lm_full", "lm_FE", "gls_AR1"),
    Adj_R2 = c(summary(lm_full)$adj.r.squared,
               summary(lm_fe)$adj.r.squared,
               NA_real_),                         # gls 沒有 → 填 NA
    AIC    = c(AIC(lm_full), AIC(lm_fe), AIC(gls_ar1))
)
write_csv(comparison_ext, "output/模型比較_含FE_GLS.csv")

# FE 模型殘差診斷
diag_fe <- augment(lm_fe) %>%
    mutate(
        觀測順序   = row_number(),
        標準化殘差 = .std.resid
    )

g_fe <- ggplot(diag_fe, aes(x = .fitted, y = .resid)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
    labs(title = "FE 模型：殘差 vs. 預測值", x = "預測值", y = "殘差")
ggsave("output/FE_Residual_vs_Fitted.png", g_fe, width = 6, height = 4)

message("進階異質性檢定、Robust SE、固定效果、GLS 及 panel 估計完成，所有結果已輸出至 output/")

# ChatGPT, respond to “如何比較各城市『人口淨流入 → 房價』斜率是否不同？” on May 23, 2025
# 建立含 interaction 的模型：人口淨流入 * factor(城市)
lm_city_int <- lm(
    房價_平均 ~ 人口淨流入 * factor(城市) + CPI + 年所得 + 貸款利率 + 失業率,
    data = df
)
summary(lm_city_int)
write_lines(capture.output(summary(lm_city_int)),
            "output/lm_interaction_城市x人口.txt")

# ChatGPT, respond to “怎麼檢定各城市斜率是否全相同？” on May 23, 2025
# Wald test：H0 = 所有「人口淨流入 × 城市」交互項係數 = 0
int_terms <- grep("^人口淨流入:factor\\(城市\\)", names(coef(lm_city_int)), value = TRUE)

# car::linearHypothesis 可以一次檢定多顆係數 = 0
wald_test <- car::linearHypothesis(lm_city_int, int_terms)  # ← 這行才對
write_lines(capture.output(wald_test),
            "output/WaldTest_城市斜率差.txt")

# ChatGPT, respond to “想畫各城市斜率對比圖要怎麼做？” on May 23, 2025
# 視覺化：各城市人口淨流入斜率（信賴區間）
int_df <- broom::tidy(lm_city_int) %>%
    filter(term %in% c("人口淨流入", int_terms)) %>%
    mutate(
        城市 = str_replace(term, "人口淨流入:factor\\(城市\\)", ""),
        城市 = if_else(城市 == "", "Baseline", 城市),          # Baseline city 斜率
        斜率 = estimate,
        上界 = estimate + 1.96 * std.error,
        下界 = estimate - 1.96 * std.error
    )

g_slope <- ggplot(int_df, aes(x = reorder(城市, 斜率), y = 斜率)) +
    geom_point() +
    geom_errorbar(aes(ymin = 下界, ymax = 上界), width = 0.2) +
    coord_flip() +
    labs(title = "各城市『人口淨流入 → 房價』斜率比較",
         x = NULL, y = "斜率 (萬/坪‧人)") +
    theme_bw()
ggsave("output/城市斜率對比.png", g_slope, width = 6, height = 4, bg = "white")