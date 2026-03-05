# 加载需要的包
library(tidyverse)
library(autoReg)
library(haven)
library(openxlsx)

# 1. 设置文件路径
data_path <- "D:/RDirectory/zhengdabei/data/子女_老年人.RData"
output_xlsx <- "D:/RDirectory/zhengdabei/output/子女_老年人_统计汇总.xlsx"

# 2. 计算字符串宽度：中文×2 + 非中文×1.2
calc_char_width <- function(str) {
  if (is.na(str)) {
    return(0)
  }
  # 统计中文和非中文数量，计算总宽度
  (str_count(str, "[\\u4e00-\\u9fa5]") * 2) +
    (str_count(str, "[^\\u4e00-\\u9fa5]") * 1.2)
}

# 3. 转换labelled变量为因子
convert_labelled <- function(df) {
  as.data.frame(lapply(df, \(x) {
    if (inherits(x, "haven_labelled") & !inherits(x, "POSIXt")) {
      haven::as_factor(x)
    } else {
      x
    }
  }))
}

# 4. 加载数据，创建Excel工作簿
load(data_path)
wb <- createWorkbook()
data_list <- list(子女版 = df_C, 老年人版 = df_E)

# 5. 批量处理每个数据集
for (ds in names(data_list)) {
  cat("⏳ 正在处理：", ds, "...\n")

  # 转换变量并生成统计结果
  stats <- data_list[[ds]] %>% convert_labelled() %>% autoReg::gaze()

  # 写入Excel工作表
  addWorksheet(wb, ds)
  writeData(wb, ds, stats, rowNames = FALSE)

  # 计算列宽：取每列最大单元格宽度，保底10
  col_widths <- stats %>%
    # 合并表头和数据
    rbind(colnames(stats)) %>%
    as.data.frame() %>%
    # 逐列计算最大宽度
    summarise(across(everything(), \(col) {
      max(sapply(col, calc_char_width), na.rm = TRUE)
    })) %>%
    unlist() %>%
    # 列宽最小为10
    pmax(10)

  # 设置Excel列宽
  setColWidths(wb, ds, cols = 1:length(col_widths), widths = col_widths)

  cat("✅ 已处理：", ds, "\n")
}

# 6. 保存Excel文件
saveWorkbook(wb, output_xlsx, overwrite = TRUE)
cat("\n📁 文件保存至：", output_xlsx, "\n")
