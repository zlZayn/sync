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
df_E_desc <- df_E_all_dbl %>%
  # 单选变量 → 文字标签
  mutate(
    性别 = if_else(性别_男_1_1_E == 1, "男", "女"),
    年龄 = case_when(
      `年龄_60-64岁_1_2_1_E` == 1 ~ "60-64岁",
      `年龄_65-74岁_1_2_2_E` == 1 ~ "65-74岁",
      `年龄_75-84岁_1_2_3_E` == 1 ~ "75-84岁",
      年龄_85岁及以上_1_2_4_E == 1 ~ "85岁及以上"
    ),
    健康状况 = case_when(
      健康状况_失能程度_1_3_E == 1 ~ "完全自理",
      健康状况_失能程度_1_3_E == 2 ~ "半失能",
      健康状况_失能程度_1_3_E == 3 ~ "失能"
    ),
    行政区 = case_when(
      居住行政区_和平区_1_4_1_E == 1 ~ "和平区",
      居住行政区_南开区_1_4_2_E == 1 ~ "南开区",
      居住行政区_河北区_1_4_3_E == 1 ~ "河北区",
      居住行政区_河东区_1_4_4_E == 1 ~ "河东区",
      居住行政区_河西区_1_4_5_E == 1 ~ "河西区",
      居住行政区_红桥区_1_4_6_E == 1 ~ "红桥区"
    ),
    养老居住场景 = case_when(
      养老居住场景_居家独居_1_5_1_E == 1 ~ "居家独居",
      养老居住场景_居家同住_1_5_2_E == 1 ~ "居家同住",
      养老居住场景_社区养老_1_5_3_E == 1 ~ "社区养老",
      养老居住场景_养老机构_1_5_4_E == 1 ~ "养老机构"
    ),
    居家跌倒经历 = if_else(居家跌倒经历_有_1_6_E == 1, "是", "否"),
    已装扶手 = if_else(已装扶手_是_1_7_E == 1, "是", "否"),
    扶手偏好类型 = case_when(
      扶手偏好类型_基础不锈钢_2_2_1_E == 1 ~ "基础不锈钢款",
      扶手偏好类型_可折叠_2_2_2_E == 1 ~ "可折叠款",
      扶手偏好类型_防滑_2_2_3_E == 1 ~ "防滑款",
      扶手偏好类型_智能预警_2_2_4_E == 1 ~ "智能预警款",
      扶手偏好类型_其他_2_2_5_E == 1 ~ "其他"
    ),
    可接受价格 = case_when(
      可接受价格_100元以内_2_3_1_E == 1 ~ "100元以内",
      `可接受价格_100-300元_2_3_2_E` == 1 ~ "100-300元",
      `可接受价格_300-500元_2_3_3_E` == 1 ~ "300-500元",
      可接受价格_500元以上_2_3_4_E == 1 ~ "500元以上",
      可接受价格_无所谓_2_3_5_E == 1 ~ "无所谓"
    )
  ) %>%

  # 多选/量表变量 → 重命名
  rename(
    # 安装场景
    安装场景_卫生间 = 扶手安装场景_卫生间_2_1_1_E,
    安装场景_床边 = 扶手安装场景_床边_2_1_2_E,
    安装场景_楼道 = 扶手安装场景_楼道_2_1_3_E,
    安装场景_阳台 = 扶手安装场景_阳台_2_1_4_E,
    安装场景_其他 = 扶手安装场景_其他_2_1_5_E,

    # 补贴期望
    补贴期望_提高力度 = 补贴期望_提高力度_2_4_1_E,
    补贴期望_简化流程 = 补贴期望_简化流程_2_4_2_E,
    补贴期望_差异化补贴 = 补贴期望_差异化补贴_2_4_3_E,
    补贴期望_社区代办 = 补贴期望_社区代办_2_4_4_E,
    补贴期望_其他 = 补贴期望_其他_2_4_5_E,

    # 量表题项
    风险收益_1 = 风险收益_3_1_1_E,
    风险收益_2 = 风险收益_3_1_2_E,
    风险收益_3 = 风险收益_3_1_3_E,
    风险收益_4 = 风险收益_3_1_4_E,
    社会支持_1 = 社会支持_3_2_1_E,
    社会支持_2 = 社会支持_3_2_2_E,
    社会支持_3 = 社会支持_3_2_3_E,
    社会支持_4 = 社会支持_3_2_4_E,
    政策适配_1 = 政策适配_3_3_1_E,
    政策适配_2 = 政策适配_3_3_2_E,
    政策适配_3 = 政策适配_3_3_3_E,
    政策适配_4 = 政策适配_3_3_4_E,
    生活依赖_1 = 生活依赖_3_4_1_E,
    生活依赖_2 = 生活依赖_3_4_2_E,
    生活依赖_3 = 生活依赖_3_4_3_E,
    生活依赖_4 = 生活依赖_3_4_4_E,
    使用意愿_1 = 使用意愿_3_5_1_E,
    使用意愿_2 = 使用意愿_3_5_2_E,
    使用意愿_3 = 使用意愿_3_5_3_E
  ) %>%

  # 保留最终分析字段
  select(
    序号_E,
    性别,
    年龄,
    健康状况,
    行政区,
    养老居住场景,
    居家跌倒经历,
    已装扶手,
    安装场景_卫生间:安装场景_其他,
    扶手偏好类型,
    可接受价格,
    补贴期望_提高力度:补贴期望_其他,
    风险收益_1:使用意愿_3
  )

# 查看数据结构
glimpse(df_E_desc)

save(df_E_desc, file = "data/df_E_desc.RData")

# ======================= 3. 绘图：环形饼图（全分类变量） =======================
# ---------- 模块1：人口基本信息 ----------
p1 <- df_E_desc %>%
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

p2 <- df_E_desc %>%
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

p3 <- df_E_desc %>%
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

# ---------- 模块2：老人居住与健康 ----------
p4 <- df_E_desc %>%
  count(养老居住场景, sort = T) %>%
  ggplot() +
  geom_arc_bar(
    stat = 'pie',
    aes(x0 = 0, y0 = 0, r0 = 0.5, r = 1, amount = n, fill = 养老居住场景)
  ) +
  coord_fixed() +
  theme_void() +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.title = element_text(face = "bold", size = 12))

p5 <- df_E_desc %>%
  count(健康状况, sort = T) %>%
  ggplot() +
  geom_arc_bar(
    stat = 'pie',
    aes(x0 = 0, y0 = 0, r0 = 0.5, r = 1, amount = n, fill = 健康状况)
  ) +
  coord_fixed() +
  theme_void() +
  scale_fill_brewer(palette = "Greens") +
  theme(legend.title = element_text(face = "bold", size = 12))

p6 <- df_E_desc %>%
  count(居家跌倒经历, sort = T) %>%
  ggplot() +
  geom_arc_bar(
    stat = 'pie',
    aes(x0 = 0, y0 = 0, r0 = 0.5, r = 1, amount = n, fill = 居家跌倒经历)
  ) +
  coord_fixed() +
  theme_void() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.title = element_text(face = "bold", size = 12))

# ---------- 模块3：扶手安装与服务 ----------
p7 <- df_E_desc %>%
  count(已装扶手, sort = T) %>%
  ggplot() +
  geom_arc_bar(
    stat = 'pie',
    aes(x0 = 0, y0 = 0, r0 = 0.5, r = 1, amount = n, fill = 已装扶手)
  ) +
  coord_fixed() +
  theme_void() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.title = element_text(face = "bold", size = 12))

p8 <- df_E_desc %>%
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

p9 <- df_E_desc %>%
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
patch_1_E <- p1 +
  p2 +
  p3 +
  p4 +
  p5 +
  p6 +
  p7 +
  p8 +
  p9 +
  plot_layout(ncol = 3)
patch_1_E


# ======================= 4. 绘图：量表维度均值条形图 =======================
# 风险收益
p12 <- df_E_desc %>%
  summarise(across(风险收益_1:风险收益_4, mean)) %>%
  pivot_longer(everything()) %>%
  rename(风险收益 = name, 平均值 = value) %>%
  arrange(平均值) %>%
  mutate(风险收益 = factor(风险收益, levels = 风险收益)) %>%
  ggplot(aes(x = 平均值, y = 风险收益, fill = 风险收益)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8, linewidth = 0.3) +
  xlim(0, 5) +
  scale_fill_brewer(palette = "Blues", type = "seq") +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor.x = element_blank(),
    legend.position = "none"
  )

# 社会支持
p13 <- df_E_desc %>%
  summarise(across(社会支持_1:社会支持_4, mean)) %>%
  pivot_longer(everything()) %>%
  rename(社会支持 = name, 平均值 = value) %>%
  arrange(平均值) %>%
  mutate(社会支持 = factor(社会支持, levels = 社会支持)) %>%
  ggplot(aes(x = 平均值, y = 社会支持, fill = 社会支持)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8, linewidth = 0.3) +
  xlim(0, 5) +
  scale_fill_brewer(palette = "Greens", type = "seq") +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor.x = element_blank(),
    legend.position = "none"
  )

# 政策适配
p14 <- df_E_desc %>%
  summarise(across(政策适配_1:政策适配_4, mean)) %>%
  pivot_longer(everything()) %>%
  rename(政策适配 = name, 平均值 = value) %>%
  arrange(平均值) %>%
  mutate(政策适配 = factor(政策适配, levels = 政策适配)) %>%
  ggplot(aes(x = 平均值, y = 政策适配, fill = 政策适配)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8, linewidth = 0.3) +
  xlim(0, 5) +
  scale_fill_brewer(palette = "Oranges", type = "seq") +
  theme_bw() +
  theme(
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor.x = element_blank(),
    legend.position = "none"
  )

# 生活依赖
p15 <- df_E_desc %>%
  summarise(across(生活依赖_1:生活依赖_4, mean)) %>%
  pivot_longer(everything()) %>%
  rename(生活依赖 = name, 平均值 = value) %>%
  arrange(平均值) %>%
  mutate(生活依赖 = factor(生活依赖, levels = 生活依赖)) %>%
  ggplot(aes(x = 平均值, y = 生活依赖, fill = 生活依赖)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8, linewidth = 0.3) +
  xlim(0, 5) +
  scale_fill_brewer(palette = "Purples", type = "seq") +
  theme_bw() +
  theme(
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor.x = element_blank(),
    legend.position = "none"
  )

# 使用意愿
p16 <- df_E_desc %>%
  summarise(across(使用意愿_1:使用意愿_3, mean)) %>%
  pivot_longer(everything()) %>%
  rename(使用意愿 = name, 平均值 = value) %>%
  arrange(平均值) %>%
  mutate(使用意愿 = factor(使用意愿, levels = 使用意愿)) %>%
  ggplot(aes(x = 平均值, y = 使用意愿, fill = 使用意愿)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8, linewidth = 0.3) +
  xlim(0, 5) +
  scale_fill_brewer(palette = "BuGn", type = "seq") +
  theme_bw() +
  theme(
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor.x = element_blank(),
    legend.position = "none"
  )

# 拼接量表图
patch4_E <- p12 +
  p13 +
  p14 +
  p15 +
  p16 +
  plot_layout(ncol = 3, widths = c(1, 1, 1))
patch4_E

# ======================= 多选题选择率 水平条形图 =======================
MS_rate_E <- df_E_desc |>
  select(starts_with("安装场景_"), starts_with("补贴期望_")) |>
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "特征", values_to = "选择率") |>
  mutate(
    维度 = case_when(
      str_detect(特征, "安装场景_") ~ "安装场景",
      str_detect(特征, "补贴期望_") ~ "补贴期望"
    ),
    特征 = str_remove(特征, "^(安装场景_|补贴期望_)")
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
MS_rate_E

# ======================= 5. 图片导出 =======================
# 导出老年人分类变量环形图
ggsave(
  filename = paste0(output_path, "E_01_老年人群体人口统计学与核心偏好分布.png"),
  plot = patch_1_E,
  width = 12,
  height = 12,
  dpi = 300
)

# 导出老年人量表均值图
ggsave(
  filename = paste0(output_path, "E_02_老年人量表维度均值图.png"),
  plot = patch4_E,
  width = 10,
  height = 5,
  dpi = 300
)

ggsave(
  filename = paste0(output_path, "E_03_多选题选择率.png"),
  plot = MS_rate_E,
  width = 8,
  height = 6,
  dpi = 300
)

cat("✅ 老年人全部图已导出至：", output_path, "\n")

#=================================================================================
#=================================================================================
#=================================================================================

# 1. 分类变量统计
df_E_desc |>
  select(where(is.character)) |>
  names() |>
  map_dfr(
    ~ {
      var <- .x
      vals <- df_E_desc[[var]]
      tbl <- table(vals)
      prop <- scales::percent(as.numeric(tbl) / sum(tbl), accuracy = 1)
      tibble(
        变量名 = var,
        类别及占比 = str_c(names(tbl), "(", prop, ")", collapse = "、")
      )
    }
  ) |>
  gt() |>
  tab_header(title = "分类变量描述统计", subtitle = "老年人调研数据") |>
  tab_style(cell_text(weight = "bold"), cells_column_labels()) |>
  gtsave(paste0(output_path, "老年人_分类变量统计.rtf"))

# 2. 数值变量统计
df_E_desc |>
  select(where(is.numeric), -序号_E) |>
  names() |>
  map_dfr(
    ~ {
      var <- .x
      vals <- df_E_desc[[var]]
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
  tab_header(title = "数值变量描述统计", subtitle = "老年人调研数据") |>
  tab_style(cell_text(weight = "bold"), cells_column_labels()) |>
  gtsave(paste0(output_path, "老年人_数值变量统计.rtf"))

message("✅ 老年人全部表已导出至：", output_path, "\n")
