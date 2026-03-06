# 加载包
library(tidyverse)
library(ggcorrplot)
library(psych)

# 加载数据
data_E_path <- "data/df_E_desc.RData"
data_C_path <- "data/df_C_desc.RData"
load(data_E_path)
load(data_C_path)

# 创建输出目录
if (!dir.exists("output")) {
  dir.create("output")
}

# ==================== E版 热力图 ====================
# 安装场景
p1 <- df_E_desc |>
  select(starts_with("安装场景_")) |>
  polychoric() |>
  pluck("rho") |>
  ggcorrplot(
    lab = T,
    lab_size = 3.5,
    colors = c("#6D9EC1", "white", "#E46726"),
    title = "E安装场景 相关系数热力图"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

ggsave("output/E_安装场景_相关热力图.png", p1, width = 8, height = 6, dpi = 300)

# 补贴期望
p2 <- df_E_desc |>
  select(starts_with("补贴期望_")) |>
  polychoric() |>
  pluck("rho") |>
  ggcorrplot(
    lab = T,
    lab_size = 3.5,
    colors = c("#6D9EC1", "white", "#E46726"),
    title = "E补贴期望 相关系数热力图"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

ggsave("output/E_补贴期望_相关热力图.png", p2, width = 8, height = 6, dpi = 300)

# ==================== C版 热力图 ====================
# 安装场景
p3 <- df_C_desc |>
  select(starts_with("安装场景_")) |>
  polychoric() |>
  pluck("rho") |>
  ggcorrplot(
    lab = T,
    lab_size = 3.5,
    colors = c("#6D9EC1", "white", "#E46726"),
    title = "C安装场景 相关系数热力图"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

ggsave("output/C_安装场景_相关热力图.png", p3, width = 8, height = 6, dpi = 300)

# 服务期望
p4 <- df_C_desc |>
  select(starts_with("服务期望_")) |>
  polychoric() |>
  pluck("rho") |>
  ggcorrplot(
    lab = T,
    lab_size = 3.5,
    colors = c("#6D9EC1", "white", "#E46726"),
    title = "C服务期望 相关系数热力图"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

ggsave("output/C_服务期望_相关热力图.png", p4, width = 8, height = 6, dpi = 300)

cat("✅ 已导出: output/{E,C}_{安装场景,补贴/服务期望}_相关热力图.png (共4张)\n")
