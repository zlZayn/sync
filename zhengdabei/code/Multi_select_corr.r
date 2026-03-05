# 加载包
library(tidyverse)
library(ggcorrplot)
library(psych)

# 加载数据
data_E_path <- "data/df_E_desc.RData"
data_C_path <- "data/df_C_desc.RData"
load(data_E_path)
load(data_C_path)

# ==================== E版 热力图 ====================
# 安装场景
df_E_desc |>
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

# 补贴期望
df_E_desc |>
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

# ==================== C版 热力图 ====================
# 安装场景
df_C_desc |>
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

# 服务期望
df_C_desc |>
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
