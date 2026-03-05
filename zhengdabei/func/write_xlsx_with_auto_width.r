write_xlsx_with_auto_width <- function(df, file_path) {
  # 1. 检查依赖
  pkgs <- c("openxlsx", "stringr", "dplyr")
  if (any(!sapply(pkgs, requireNamespace, quietly = TRUE))) {
    stop(
      "请安装缺失包：",
      paste(
        pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)],
        collapse = ", "
      )
    )
  }

  # 2. 宽度计算函数 (中文×2 + 其他×1.2)
  calc_w <- function(x) {
    x <- as.character(x)
    if (is.na(x) || x == "NA" || nchar(x) == 0) {
      return(0)
    }
    stringr::str_count(x, "[\\u4e00-\\u9fa5]") *
      2 +
      stringr::str_count(x, "[^\\u4e00-\\u9fa5]") * 1.2
  }

  # 3. 创建工作簿并写入数据 (保留原始类型)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  options(
    "openxlsx.dateFormat" = "yyyy-mm-dd",
    "openxlsx.datetimeFormat" = "yyyy-mm-dd hh:mm:ss"
  )
  openxlsx::writeData(wb, 1, df, colNames = TRUE)

  # 4. 设置 POSIXct 列格式
  posix_cols <- which(sapply(df, inherits, "POSIXct"))
  if (length(posix_cols)) {
    style <- openxlsx::createStyle(numFmt = "yyyy-mm-dd hh:mm:ss")
    openxlsx::addStyle(
      wb,
      1,
      style,
      rows = 2:(nrow(df) + 1),
      cols = posix_cols,
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  # 5. 计算并设置列宽 (临时转字符以统一计算)
  df_char <- dplyr::mutate(df, dplyr::across(everything(), as.character))
  header <- setNames(
    as.data.frame(t(colnames(df)), stringsAsFactors = FALSE),
    colnames(df)
  )
  widths <- sapply(rbind(header, df_char), function(col) {
    max(sapply(col, calc_w), na.rm = TRUE)
  })
  openxlsx::setColWidths(
    wb,
    1,
    cols = seq_along(widths),
    widths = pmax(ceiling(widths), 10)
  )

  # 6. 保存
  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
  cat("✅ 自动调整列宽:", file_path, "\n")
}
