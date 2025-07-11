# 六都房價與變數分析 R Script 說明

本專案使用 R 語言進行台灣六大都市（六都）房價分析，探討其與人口變動、利率、CPI、失業率與年所得等變數的關聯性，並透過視覺化與模型建構進行預測與診斷。本 README 逐段說明主程式中每個區塊的功能與目的。

---

## 📦 套件安裝與載入

* `tidyverse`、`readr`、`lubridate`：資料處理與匯入
* `forecast`：ARIMA 模型建構與預測
* `janitor`、`purrr`：資料欄位清理與過濾
* `car`、`broom`、`ggcorrplot` 等：回歸診斷、共線性檢查與視覺化

## 🧼 資料預處理

1. 載入 `六都房價整合表_UTF8.csv` 資料
2. 清理欄位名稱，將換行符號與逗號格式數字標準化
3. 建立欄位：

   * `日期`：將年月合併成日期物件
   * `人口淨流入`：遷入人數 - 遷出人數

## 🧪 極端值處理

自定函數 `remove_outlier()` 可使用 IQR 或 Z-score 去除離群值，並應用於關聯圖繪製與變化率分析中。

## 📈 城市別繪圖（❶–❹）

對每一城市分別輸出四張圖：

1. 房價(中位數) 與 人口淨流入 變化
2. CPI 與 房價(平均) 變化
3. 貸款利率 與 房價(平均) 變化
4. 使用 `auto.arima()` 模型預測未來 60 期（5 年）房價

## 🌐 六都全域分析（❺–❿）

5. 人口淨流入 vs 房價(平均)：繪製散佈圖與線性回歸線
6. 失業率 vs 房價(平均)：過濾離群後建模分析
7. 房價變化率 vs 利率變化率：lag 計算後視覺化分析
8. 六都平均房價時序圖：看整體趨勢
9. 房價變化率 vs 淨遷徙變化率：評估人口動態對價格波動之影響
10. 各都房價預測誤差（RMSE）與異常標記

## 🥇 排名與異常分析

* 計算六都平均房價與排名
* 使用 ARIMA 殘差標準差偵測異常觀測點，並計算異常次數
* 視覺化城市房價排名的時序變化（點圖與折線圖）
* 找出殘差最大前 10 筆資料（多元線性模型）
* 偵測人口減少但房價上升的情況（矛盾樣本）

## 🧮 模型建構與診斷

* 建立多元線性模型：房價 \~ 人口淨流入 + CPI + 所得 + 利率 + 失業率
* 執行 VIF 共線性檢查與相關係數熱圖
* 殘差診斷圖：預測 vs 殘差、Q-Q plot、自相關
* Durbin-Watson 與 Breusch-Pagan 檢定
* 加入 log 變數與交互作用後建立優化模型，並與原模型比較 Adj R2 與 AIC

## 🧪 進階回歸模型

* 固定效果模型（城市、月份）：`lm()`
* GLS 模型修正自相關：使用 `gls()` + `corAR1`
* Panel 固定效果回歸：使用 `plm()` 執行 two-way FE
* 模型比較與係數輸出

## 📏 城市人口影響斜率檢定

* 加入交互項目 `人口淨流入 * 城市`，建構城市分別回歸斜率模型
* 使用 Wald test 檢定各城市斜率是否相同
* 視覺化各城市「人口 → 房價」斜率差異與信賴區間

## 📂 輸出內容

所有圖片與分析結果將輸出至 `output/` 資料夾，包含：

* 分析圖（PNG）
* 模型與資料表格（CSV）
* 模型摘要與診斷報告（txt）

---

## ✅ 適合對象

* 想快速理解六都房價趨勢與人口結構關聯
* 欲進行資料視覺化與迴歸建模分析的學生或研究者

## ▶️ 如何使用

```r
source("your_script_name.R")
```

