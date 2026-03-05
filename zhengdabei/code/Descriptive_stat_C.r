# ======================= 1. 加载包与路径设置 =======================
library(tidyverse)
library(patchwork)
library(ggforce)
library(gt)

# 文件路径
data_path <- "data/df_all_dbl.RData"
output_path <- "output/"
func_path <- "func/write_xlsx_with_auto_width.r"

# 加载数据与自定义函数
load(data_path)
source(func_path)


# ======================= 2. 数据清洗：变量重编码 + 字段整理 =======================
# 单选转分类标签，多选保留0/1，统一变量名
df_C_desc <- df_C_all_dbl %>%
  # 单选变量 → 文字标签
  mutate(
    性别 = if_else(性别_男_1_1_C == 1, "男", "女"),
    年龄 = case_when(
      `年龄_18-24岁_1_2_1_C` == 1 ~ "18-24岁",
      `年龄_25-34岁_1_2_2_C` == 1 ~ "25-34岁",
      `年龄_35-44岁_1_2_3_C` == 1 ~ "35-44岁",
      `年龄_45-54岁_1_2_4_C` == 1 ~ "45-54岁",
      年龄_55岁及以上_1_2_5_C == 1 ~ "55岁及以上"
    ),
    居住关系 = case_when(
      与老人居住关系_同住_1_3_1_C == 1 ~ "与老人同住",
      与老人居住关系_同城不同住_1_3_2_C == 1 ~ "同城不同住",
      与老人居住关系_异地居住_1_3_3_C == 1 ~ "异地居住"
    ),
    老人年龄 = case_when(
      `老人年龄_60-64岁_1_4_1_C` == 1 ~ "60-64岁",
      `老人年龄_65-74岁_1_4_2_C` == 1 ~ "65-74岁",
      `老人年龄_75-84岁_1_4_3_C` == 1 ~ "75-84岁",
      老人年龄_85岁及以上_1_4_4_C == 1 ~ "85岁及以上"
    ),
    老人健康状况 = case_when(
      老人健康状况_失能程度_1_5_C == 1 ~ "完全自理",
      老人健康状况_失能程度_1_5_C == 2 ~ "半失能",
      老人健康状况_失能程度_1_5_C == 3 ~ "失能"
    ),
    行政区 = case_when(
      居住行政区_和平区_1_6_1_C == 1 ~ "和平区",
      居住行政区_南开区_1_6_2_C == 1 ~ "南开区",
      居住行政区_河北区_1_6_3_C == 1 ~ "河北区",
      居住行政区_河东区_1_6_4_C == 1 ~ "河东区",
      居住行政区_河西区_1_6_5_C == 1 ~ "河西区",
      居住行政区_红桥区_1_6_6_C == 1 ~ "红桥区"
    ),
    为老人装过扶手 = if_else(为老人装过扶手_是_1_7_C == 1, "是", "否"),
    扶手偏好类型 = case_when(
      扶手偏好类型_基础不锈钢_2_2_1_C == 1 ~ "基础不锈钢款",
      扶手偏好类型_可折叠_2_2_2_C == 1 ~ "可折叠款",
      扶手偏好类型_防滑_2_2_3_C == 1 ~ "防滑款",
      扶手偏好类型_智能预警_2_2_4_C == 1 ~ "智能预警款",
      扶手偏好类型_其他_2_2_5_C == 1 ~ "其他"
    ),
    可接受价格 = case_when(
      可接受价格_100元以内_2_3_1_C == 1 ~ "100元以内",
      `可接受价格_100-300元_2_3_2_C` == 1 ~ "100-300元",
      `可接受价格_300-500元_2_3_3_C` == 1 ~ "300-500元",
      可接受价格_500元以上_2_3_4_C == 1 ~ "500元以上",
      可接受价格_无所谓_2_3_5_C == 1 ~ "无所谓"
    )
  ) %>%

  # 多选/量表变量 → 重命名
  rename(
    # 安装场景
    安装场景_卫生间 = 扶手安装场景_卫生间_2_1_1_C,
    安装场景_床边 = 扶手安装场景_床边_2_1_2_C,
    安装场景_楼道 = 扶手安装场景_楼道_2_1_3_C,
    安装场景_阳台 = 扶手安装场景_阳台_2_1_4_C,
    安装场景_其他 = 扶手安装场景_其他_2_1_5_C,
    # 服务期望
    服务期望_免费安装 = 服务期望_免费安装_2_4_1_C,
    服务期望_24小时售后 = 服务期望_24小时售后_2_4_2_C,
    服务期望_定期维护 = 服务期望_定期维护_2_4_3_C,
    服务期望_上门教学 = 服务期望_上门教学_2_4_4_C,
    服务期望_其他 = 服务期望_其他_2_4_5_C,
    # 量表题项
    照护价值_1 = 照护价值_3_1_1_C,
    照护价值_2 = 照护价值_3_1_2_C,
    照护价值_3 = 照护价值_3_1_3_C,
    照护价值_4 = 照护价值_3_1_4_C,
    代际责任_1 = 代际责任_3_2_1_C,
    代际责任_2 = 代际责任_3_2_2_C,
    代际责任_3 = 代际责任_3_2_3_C,
    代际责任_4 = 代际责任_3_2_4_C,
    决策便利_1 = 决策便利_3_3_1_C,
    决策便利_2 = 决策便利_3_3_2_C,
    决策便利_3 = 决策便利_3_3_3_C,
    决策便利_4 = 决策便利_3_3_4_C,
    资源投入_1 = 资源投入_3_4_1_C,
    资源投入_2 = 资源投入_3_4_2_C,
    资源投入_3 = 资源投入_3_4_3_C,
    资源投入_4 = 资源投入_3_4_4_C,
    认知偏差_1 = 认知偏差_3_5_1_C,
    认知偏差_2 = 认知偏差_3_5_2_C,
    认知偏差_3 = 认知偏差_3_5_3_C,
    认知偏差_4 = 认知偏差_3_5_4_C,
    购买意愿_1 = 购买意愿_3_6_1_C,
    购买意愿_2 = 购买意愿_3_6_2_C,
    购买意愿_3 = 购买意愿_3_6_3_C
  ) %>%

  # 保留最终分析字段
  select(
    序号_C,
    性别,
    年龄,
    居住关系,
    老人年龄,
    老人健康状况,
    行政区,
    为老人装过扶手,
    安装场景_卫生间:安装场景_其他,
    扶手偏好类型,
    可接受价格,
    服务期望_免费安装:服务期望_其他,
    照护价值_1:购买意愿_3
  )

# 查看数据结构
glimpse(df_C_desc)

save(df_C_desc, file = "data/df_C_desc.RData")


# ======================= 3. 绘图：环形饼图（全分类变量） =======================
# ---------- 模块1：人口基本信息 ----------
p1 <- df_C_desc %>%
  count(性别, sort = T) %>%
  ggplot() +
  geom_arc_bar(
    stat = 'pie',
    aes(x0 = 0, y0 = 0, r0 = 0.5, r = 1, amount = n, fill = 性别)
  ) +
  coord_fixed() +
  theme_void() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.title = element_text(face = "bold", size = 12))

p2 <- df_C_desc %>%
  count(年龄, sort = T) %>%
  ggplot() +
  geom_arc_bar(
    stat = 'pie',
    aes(x0 = 0, y0 = 0, r0 = 0.5, r = 1, amount = n, fill = 年龄)
  ) +
  coord_fixed() +
  theme_void() +
  scale_fill_brewer(palette = "Oranges") +
  theme(legend.title = element_text(face = "bold", size = 12))

p3 <- df_C_desc %>%
  count(行政区, sort = T) %>%
  ggplot() +
  geom_arc_bar(
    stat = 'pie',
    aes(x0 = 0, y0 = 0, r0 = 0.5, r = 1, amount = n, fill = 行政区)
  ) +
  coord_fixed() +
  theme_void() +
  scale_fill_brewer(palette = "RdBu") +
  theme(legend.title = element_text(face = "bold", size = 12))

# ---------- 模块2：老人与居住特征 ----------
p4 <- df_C_desc %>%
  count(居住关系, sort = T) %>%
  ggplot() +
  geom_arc_bar(
    stat = 'pie',
    aes(x0 = 0, y0 = 0, r0 = 0.5, r = 1, amount = n, fill = 居住关系)
  ) +
  coord_fixed() +
  theme_void() +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.title = element_text(face = "bold", size = 12))

p5 <- df_C_desc %>%
  count(老人年龄, sort = T) %>%
  ggplot() +
  geom_arc_bar(
    stat = 'pie',
    aes(x0 = 0, y0 = 0, r0 = 0.5, r = 1, amount = n, fill = 老人年龄)
  ) +
  coord_fixed() +
  theme_void() +
  scale_fill_brewer(palette = "Blues") +
  theme(legend.title = element_text(face = "bold", size = 12))

p6 <- df_C_desc %>%
  count(老人健康状况, sort = T) %>%
  ggplot() +
  geom_arc_bar(
    stat = 'pie',
    aes(x0 = 0, y0 = 0, r0 = 0.5, r = 1, amount = n, fill = 老人健康状况)
  ) +
  coord_fixed() +
  theme_void() +
  scale_fill_brewer(palette = "Greens") +
  theme(legend.title = element_text(face = "bold", size = 12))

# ---------- 模块3：扶手安装与服务 ----------
p7 <- df_C_desc %>%
  count(为老人装过扶手, sort = T) %>%
  ggplot() +
  geom_arc_bar(
    stat = 'pie',
    aes(x0 = 0, y0 = 0, r0 = 0.5, r = 1, amount = n, fill = 为老人装过扶手)
  ) +
  coord_fixed() +
  theme_void() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.title = element_text(face = "bold", size = 12))

p8 <- df_C_desc %>%
  count(扶手偏好类型, sort = T) %>%
  ggplot() +
  geom_arc_bar(
    stat = 'pie',
    aes(x0 = 0, y0 = 0, r0 = 0.5, r = 1, amount = n, fill = 扶手偏好类型)
  ) +
  coord_fixed() +
  theme_void() +
  scale_fill_brewer(palette = "PRGn") +
  theme(legend.title = element_text(face = "bold", size = 12))

p9 <- df_C_desc %>%
  count(可接受价格, sort = T) %>%
  ggplot() +
  geom_arc_bar(
    stat = 'pie',
    aes(x0 = 0, y0 = 0, r0 = 0.5, r = 1, amount = n, fill = 可接受价格)
  ) +
  coord_fixed() +
  theme_void() +
  scale_fill_brewer(palette = "Purples") +
  theme(legend.title = element_text(face = "bold", size = 12))

# 拼接所有饼图
patch_1_C <- p1 +
  p2 +
  p3 +
  p4 +
  p5 +
  p6 +
  p7 +
  p8 +
  p9 +
  plot_layout(ncol = 3)
patch_1_C


# ======================= 4. 绘图：量表维度均值条形图 =======================
# 照护价值
p12 <- df_C_desc %>%
  summarise(across(照护价值_1:照护价值_4, mean)) %>%
  pivot_longer(everything()) %>%
  rename(照护价值 = name, 平均值 = value) %>%
  arrange(平均值) %>%
  mutate(照护价值 = factor(照护价值, levels = 照护价值)) %>%
  ggplot(aes(x = 平均值, y = 照护价值, fill = 照护价值)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8, linewidth = 0.3) +
  xlim(0, 5) +
  scale_fill_brewer(palette = "Blues") +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor.x = element_blank(),
    legend.position = "none"
  )

# 代际责任
p13 <- df_C_desc %>%
  summarise(across(代际责任_1:代际责任_4, mean)) %>%
  pivot_longer(everything()) %>%
  rename(代际责任 = name, 平均值 = value) %>%
  arrange(平均值) %>%
  mutate(代际责任 = factor(代际责任, levels = 代际责任)) %>%
  ggplot(aes(x = 平均值, y = 代际责任, fill = 代际责任)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8, linewidth = 0.3) +
  xlim(0, 5) +
  scale_fill_brewer(palette = "Greens") +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor.x = element_blank(),
    legend.position = "none"
  )

# 决策便利
p14 <- df_C_desc %>%
  summarise(across(决策便利_1:决策便利_4, mean)) %>%
  pivot_longer(everything()) %>%
  rename(决策便利 = name, 平均值 = value) %>%
  arrange(平均值) %>%
  mutate(决策便利 = factor(决策便利, levels = 决策便利)) %>%
  ggplot(aes(x = 平均值, y = 决策便利, fill = 决策便利)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8, linewidth = 0.3) +
  xlim(0, 5) +
  scale_fill_brewer(palette = "Oranges") +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor.x = element_blank(),
    legend.position = "none"
  )

# 资源投入
p15 <- df_C_desc %>%
  summarise(across(资源投入_1:资源投入_4, mean)) %>%
  pivot_longer(everything()) %>%
  rename(资源投入 = name, 平均值 = value) %>%
  arrange(平均值) %>%
  mutate(资源投入 = factor(资源投入, levels = 资源投入)) %>%
  ggplot(aes(x = 平均值, y = 资源投入, fill = 资源投入)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8, linewidth = 0.3) +
  xlim(0, 5) +
  scale_fill_brewer(palette = "Purples") +
  theme_bw() +
  theme(
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor.x = element_blank(),
    legend.position = "none"
  )

# 认知偏差
p16 <- df_C_desc %>%
  summarise(across(认知偏差_1:认知偏差_4, mean)) %>%
  pivot_longer(everything()) %>%
  rename(认知偏差 = name, 平均值 = value) %>%
  arrange(平均值) %>%
  mutate(认知偏差 = factor(认知偏差, levels = 认知偏差)) %>%
  ggplot(aes(x = 平均值, y = 认知偏差, fill = 认知偏差)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8, linewidth = 0.3) +
  xlim(0, 5) +
  scale_fill_brewer(palette = "Reds") +
  theme_bw() +
  theme(
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor.x = element_blank(),
    legend.position = "none"
  )

# 购买意愿
p17 <- df_C_desc %>%
  summarise(across(购买意愿_1:购买意愿_3, mean)) %>%
  pivot_longer(everything()) %>%
  rename(购买意愿 = name, 平均值 = value) %>%
  arrange(平均值) %>%
  mutate(购买意愿 = factor(购买意愿, levels = 购买意愿)) %>%
  ggplot(aes(x = 平均值, y = 购买意愿, fill = 购买意愿)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8, linewidth = 0.3) +
  xlim(0, 5) +
  scale_fill_brewer(palette = "BuGn") +
  theme_bw() +
  theme(
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor.x = element_blank(),
    legend.position = "none"
  )

# 拼接量表图
patch_2_C <- p12 +
  p13 +
  p14 +
  p15 +
  p16 +
  p17 +
  plot_layout(ncol = 3)
patch_2_C

# ======================= 多选题选择率 水平条形图 =======================
MS_rate_C <- df_C_desc |>
  select(starts_with("安装场景_"), starts_with("服务期望_")) |>
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "特征", values_to = "选择率") |>
  mutate(
    维度 = case_when(
      str_detect(特征, "安装场景_") ~ "安装场景",
      str_detect(特征, "服务期望_") ~ "服务期望"
    ),
    特征 = str_remove(特征, "^(安装场景_|服务期望_)")
  ) |>
  group_by(维度) |>
  arrange(维度, desc(选择率)) |>
  ungroup() |>
  mutate(特征 = fct_inorder(特征)) |>

  ggplot(aes(x = 选择率, y = reorder(特征, 选择率), fill = 特征)) +
  geom_bar(
    stat = "identity",
    color = "black",
    alpha = 0.8,
    linewidth = 0.3
  ) +
  geom_text(
    aes(label = scales::percent(选择率, accuracy = 1)),
    hjust = -0.1,
    size = 3.5
  ) +
  facet_wrap(vars(维度), scales = "free_y", ncol = 1) +
  scale_fill_brewer(palette = "YlGnBu") +
  scale_x_continuous(
    labels = scales::percent,
    limits = c(0, 1),
    breaks = seq(0, 1, 0.1)
  ) +
  labs(
    x = "选择比例",
    y = "选项"
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid.major.x = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "white"),
    legend.position = "none"
  )
MS_rate_C

# ======================= 5. 图片导出 =======================
# 导出分类变量环形图
ggsave(
  filename = paste0(output_path, "C_01_子女群体人口统计学与核心偏好分布.png"),
  plot = patch_1_C,
  width = 12,
  height = 12,
  dpi = 300
)

# 导出量表均值图
ggsave(
  filename = paste0(output_path, "C_02_子女量表维度均值图.png"),
  plot = patch_2_C,
  width = 10,
  height = 5,
  dpi = 300
)

ggsave(
  filename = paste0(output_path, "C_03_多选题选择率.png"),
  plot = MS_rate_C,
  width = 8,
  height = 6,
  dpi = 300
)

cat("✅ 子女全部图已导出至：", output_path, "\n")

#=================================================================================
#=================================================================================
#=================================================================================

# 1. 分类变量统计
df_C_desc |>
  select(where(is.character)) |>
  names() |>
  map_dfr(
    ~ {
      var <- .x
      vals <- df_C_desc[[var]]
      tbl <- table(vals)
      prop <- scales::percent(as.numeric(tbl) / sum(tbl), accuracy = 1)
      tibble(
        变量名 = var,
        类别及占比 = str_c(names(tbl), "(", prop, ")", collapse = "、")
      )
    }
  ) |>
  gt() |>
  tab_header(title = "分类变量描述统计", subtitle = "子女调研数据") |>
  tab_style(cell_text(weight = "bold"), cells_column_labels()) |>
  gtsave(paste0(output_path, "子女_分类变量统计.rtf"))

# 2. 数值变量统计
df_C_desc |>
  select(where(is.numeric), -序号_C) |>
  names() |>
  map_dfr(
    ~ {
      var <- .x
      vals <- df_C_desc[[var]]
      tibble(
        变量名 = var,
        `均值 ± 标准差` = str_c(
          round(mean(vals, na.rm = T), 3),
          " ± ",
          round(sd(vals, na.rm = T), 3)
        ),
        取值范围 = str_c(min(vals, na.rm = T), " ～ ", max(vals, na.rm = T))
      )
    }
  ) |>
  gt() |>
  tab_header(title = "数值变量描述统计", subtitle = "子女调研数据") |>
  tab_style(cell_text(weight = "bold"), cells_column_labels()) |>
  gtsave(paste0(output_path, "子女_数值变量统计.rtf"))

message("✅ 子女全部表已导出至：", output_path, "\n")
