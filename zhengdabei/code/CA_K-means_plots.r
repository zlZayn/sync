# ==============================================================
# 防跌倒扶手调研 K-means 聚类分析 · 完整出图脚本（整理注释版）
# 说明：代码内容完全未修改，仅优化注释、排版、格式、空行与可读性
# ==============================================================

# ---------------------- 1. 加载依赖包 ----------------------
library(tidyverse)
library(cluster)
library(factoextra)
library(gt)
library(patchwork)

# ---------------------- 2. 设置文件路径 ----------------------
data_path <- "data/df_all_dbl.RData"
output_path <- "output/"
func_path <- "func/write_xlsx_with_auto_width.r"

# ---------------------- 3. 创建输出文件夹 ----------------------
if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
}

# ---------------------- 4. 加载数据与自定义函数 ----------------------
load(data_path)
source(func_path)

# ==============================================================
# 一、子女群体 K-means 聚类分析
# ==============================================================

# ---------------------- 变量筛选与标准化 ----------------------
cluster_vars_C_raw <- df_C_all_dbl |> select(-序号_C)
cluster_vars_C_clean <- cluster_vars_C_raw |>
  mutate(across(everything(), as.numeric))
df_C_scaled <- as_tibble(scale(cluster_vars_C_clean))

# ---------------------- 轮廓系数法确定最佳K值 ----------------------
set.seed(42)
silhouette_scores_C <- map_dbl(
  2:16,
  ~ mean(silhouette(
    kmeans(df_C_scaled, centers = ., nstart = 25)$cluster,
    dist(df_C_scaled)
  )[, "sil_width"])
)
best_k_C <- which.max(silhouette_scores_C) + 1

# ---------------------- 绘制K值选择图 ----------------------
p_k_C <- tibble(k = 2:16, silhouette = silhouette_scores_C) |>
  ggplot(aes(x = k, y = silhouette)) +
  geom_line(linewidth = 1.5, color = "#707070ff") +
  geom_point(size = 4, color = "#d67570ff") +
  scale_x_continuous(breaks = seq(2, 16, 1)) +
  labs(
    x = "聚类数量 k",
    y = "平均轮廓系数",
    caption = paste0("注：子女群体基于轮廓系数法确定最佳聚类数，K = ", best_k_C)
  ) +
  theme_classic() +
  theme(
    legend.direction = "horizontal",
    legend.title = element_blank(),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    plot.caption = element_text(size = 10, hjust = 0, face = "plain")
  ) +
  geom_vline(
    xintercept = best_k_C,
    linetype = "dashed",
    linewidth = 1.0,
    color = "#b08a32ff",
    alpha = 0.7
  )

# ---------------------- 执行K-means聚类 ----------------------
km_C <- kmeans(df_C_scaled, centers = best_k_C, nstart = 25)
df_C_cluster_tag <- tibble(
  序号_C = df_C_all_dbl$序号_C,
  聚类标签_C = as.factor(km_C$cluster)
)

# ==============================================================
# 二、老年人群体 K-means 聚类分析
# ==============================================================

# ---------------------- 变量筛选与标准化 ----------------------
cluster_vars_E_raw <- df_E_all_dbl |> select(-序号_E)
cluster_vars_E_clean <- cluster_vars_E_raw |>
  mutate(across(everything(), as.numeric))
df_E_scaled <- as_tibble(scale(cluster_vars_E_clean))

# ---------------------- 轮廓系数法确定最佳K值 ----------------------
set.seed(42)
silhouette_scores_E <- map_dbl(
  2:16,
  ~ mean(silhouette(
    kmeans(df_E_scaled, centers = ., nstart = 25)$cluster,
    dist(df_E_scaled)
  )[, "sil_width"])
)
best_k_E <- which.max(silhouette_scores_E) + 1

# ---------------------- 绘制K值选择图 ----------------------
p_k_E <- tibble(k = 2:16, silhouette = silhouette_scores_E) |>
  ggplot(aes(x = k, y = silhouette)) +
  geom_line(linewidth = 1.5, color = "#707070ff") +
  geom_point(size = 4, color = "#d67570ff") +
  scale_x_continuous(breaks = seq(2, 16, 1)) +
  labs(
    x = "聚类数量 k",
    y = "平均轮廓系数",
    caption = paste0(
      "注：老年人群体基于轮廓系数法确定最佳聚类数，K = ",
      best_k_E
    )
  ) +
  theme_classic() +
  theme(
    legend.direction = "horizontal",
    legend.title = element_blank(),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    plot.caption = element_text(size = 10, hjust = 0, face = "plain")
  ) +
  geom_vline(
    xintercept = best_k_E,
    linetype = "dashed",
    linewidth = 1.0,
    color = "#b08a32ff",
    alpha = 0.7
  )

# ---------------------- 执行K-means聚类 ----------------------
km_E <- kmeans(df_E_scaled, centers = best_k_E, nstart = 25)
df_E_cluster_tag <- tibble(
  序号_E = df_E_all_dbl$序号_E,
  聚类标签_E = as.factor(km_E$cluster)
)

# ==============================================================
# 三、聚类结果可视化（PCA图 + 轮廓系数图）
# ==============================================================

# ---------------------- 子女PCA聚类可视化 ----------------------
p1 <- fviz_cluster(
  km_C,
  data = df_C_scaled,
  ellipse.type = "norm",
  palette = c("#a08d65ff", "#92ab75ff")
) +
  labs(
    title = NULL,
    x = "主成分 1",
    y = "主成分 2",
    caption = "注：子女群体 K-means 聚类可视化（PCA 降维展示）"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank(),
    plot.caption = element_text(size = 10, hjust = 0)
  )

# ---------------------- 老人PCA聚类可视化 ----------------------
p2 <- fviz_cluster(
  km_E,
  data = df_E_scaled,
  ellipse.type = "norm",
  palette = c("#a08d65ff", "#92ab75ff")
) +
  labs(
    title = NULL,
    x = "主成分 1",
    y = "主成分 2",
    caption = "注：老年人群体 K-means 聚类可视化（PCA 降维展示）"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank(),
    plot.caption = element_text(size = 10, hjust = 0)
  )

# ---------------------- 子女轮廓系数图 ----------------------
sil_C <- silhouette(km_C$cluster, dist(df_C_scaled))
sil_C_df <- as.data.frame(sil_C) %>%
  group_by(cluster) %>%
  arrange(desc(sil_width), .by_group = TRUE) %>%
  ungroup() %>%
  mutate(id = row_number())
avg_sil_C <- round(mean(sil_C_df$sil_width), 3)

p3 <- ggplot(sil_C_df, aes(x = id, y = sil_width, fill = factor(cluster))) +
  geom_col(alpha = 0.85, width = 1) +
  scale_fill_manual(values = c("#a08d65ff", "#92ab75ff")) +
  geom_hline(
    yintercept = avg_sil_C,
    color = "#205f70ff",
    linetype = "dashed",
    linewidth = 1,
    alpha = 0.7
  ) +
  scale_y_continuous(limits = c(-0.1, 0.3), breaks = seq(-1, 1, 0.1)) +
  labs(
    x = "样本排序",
    y = "轮廓系数",
    caption = paste0("注：子女群体轮廓系数分布，平均轮廓系数 = ", avg_sil_C)
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank(),
    plot.caption = element_text(size = 10, hjust = 0)
  )

# ---------------------- 老人轮廓系数图 ----------------------
sil_E <- silhouette(km_E$cluster, dist(df_E_scaled))
sil_E_df <- as.data.frame(sil_E) %>%
  group_by(cluster) %>%
  arrange(desc(sil_width), .by_group = TRUE) %>%
  ungroup() %>%
  mutate(id = row_number())
avg_sil_E <- round(mean(sil_E_df$sil_width), 3)

p4 <- ggplot(sil_E_df, aes(x = id, y = sil_width, fill = factor(cluster))) +
  geom_col(alpha = 0.85, width = 1) +
  scale_fill_manual(values = c("#a08d65ff", "#92ab75ff")) +
  geom_hline(
    yintercept = avg_sil_E,
    color = "#205f70ff",
    linetype = "dashed",
    linewidth = 1,
    alpha = 0.7
  ) +
  scale_y_continuous(limits = c(-0.1, 0.3), breaks = seq(-1, 1, 0.1)) +
  labs(
    x = "样本排序",
    y = "轮廓系数",
    caption = paste0("注：老年人群体轮廓系数分布，平均轮廓系数 = ", avg_sil_E)
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank(),
    plot.caption = element_text(size = 10, hjust = 0)
  )

# ---------------------- 组合聚类可视化图表 ----------------------
plot_child <- p1 + p3 + plot_layout(ncol = 2, widths = c(3, 2))
plot_elder <- p2 + p4 + plot_layout(ncol = 2, widths = c(3, 2))

# ==============================================================
# 四、导出聚类图表与标签结果
# ==============================================================

# 导出最佳K值图
ggsave(
  filename = paste0(output_path, "01_子女最佳K值.png"),
  plot = p_k_C,
  width = 10,
  height = 5,
  dpi = 300,
  type = "cairo-png"
)

ggsave(
  filename = paste0(output_path, "02_老人最佳K值.png"),
  plot = p_k_E,
  width = 10,
  height = 5,
  dpi = 300,
  type = "cairo-png"
)

# 导出聚类组合图
ggsave(
  filename = paste0(output_path, "03_子女聚类组合图.png"),
  plot = plot_child,
  width = 14,
  height = 7,
  dpi = 300,
  type = "cairo-png"
)

ggsave(
  filename = paste0(output_path, "04_老人聚类组合图.png"),
  plot = plot_elder,
  width = 14,
  height = 7,
  dpi = 300,
  type = "cairo-png"
)

# 导出聚类标签结果
write_csv(df_C_cluster_tag, paste0(output_path, "子女_K-means聚类标签.csv"))
write_csv(df_E_cluster_tag, paste0(output_path, "老年人_K-means聚类标签.csv"))

# ==============================================================
# 五、变量名称映射表（原始变量 → 自定义特征名）
# ==============================================================

# ---------------------- 子女群体变量映射 ----------------------
name_map_C <- tibble::tribble(
  ~原始变量名                   , ~自定义特征名            ,
  "性别_男_1_1_C"             , "男性占比"             ,
  "年龄_18-24岁_1_2_1_C"      , "年龄在18至24岁占比"      ,
  "年龄_25-34岁_1_2_2_C"      , "年龄在25至34岁占比"      ,
  "年龄_35-44岁_1_2_3_C"      , "年龄在35至44岁占比"      ,
  "年龄_45-54岁_1_2_4_C"      , "年龄在45至54岁占比"      ,
  "年龄_55岁及以上_1_2_5_C"      , "年龄在55岁及以上占比"      ,
  "与老人居住关系_同住_1_3_1_C"     , "与老人同住占比"          ,
  "与老人居住关系_同城不同住_1_3_2_C"  , "与老人同城不同住占比"       ,
  "与老人居住关系_异地居住_1_3_3_C"   , "与老人异地居住占比"        ,
  "老人年龄_60-64岁_1_4_1_C"    , "老人在60至64岁占比"      ,
  "老人年龄_65-74岁_1_4_2_C"    , "老人在65至74岁占比"      ,
  "老人年龄_75-84岁_1_4_3_C"    , "老人在75至84岁占比"      ,
  "老人年龄_85岁及以上_1_4_4_C"    , "老人在85岁及以上占比"      ,
  "老人健康状况_失能程度_1_5_C"      , "老人失能程度均值"         ,
  "居住行政区_和平区_1_6_1_C"      , "和平区占比"            ,
  "居住行政区_南开区_1_6_2_C"      , "南开区占比"            ,
  "居住行政区_河北区_1_6_3_C"      , "河北区占比"            ,
  "居住行政区_河东区_1_6_4_C"      , "河东区占比"            ,
  "居住行政区_河西区_1_6_5_C"      , "河西区占比"            ,
  "居住行政区_红桥区_1_6_6_C"      , "红桥区占比"            ,
  "为老人装过扶手_是_1_7_C"        , "为老人装过扶手占比"        ,
  "扶手安装场景_卫生间_2_1_1_C"     , "卫生间安装偏好占比"        ,
  "扶手安装场景_床边_2_1_2_C"      , "床边安装偏好占比"         ,
  "扶手安装场景_楼道_2_1_3_C"      , "楼道安装偏好占比"         ,
  "扶手安装场景_阳台_2_1_4_C"      , "阳台安装偏好占比"         ,
  "扶手安装场景_其他_2_1_5_C"      , "其他安装场景占比"         ,
  "扶手偏好类型_基础不锈钢_2_2_1_C"   , "基础不锈钢偏好占比"        ,
  "扶手偏好类型_可折叠_2_2_2_C"     , "可折叠扶手偏好占比"        ,
  "扶手偏好类型_防滑_2_2_3_C"      , "防滑扶手偏好占比"         ,
  "扶手偏好类型_智能预警_2_2_4_C"    , "智能预警扶手偏好占比"       ,
  "扶手偏好类型_其他_2_2_5_C"      , "其他类型偏好占比"         ,
  "可接受价格_100元以内_2_3_1_C"   , "可接受价格在100元以内占比"   ,
  "可接受价格_100-300元_2_3_2_C" , "可接受价格在100至300元占比" ,
  "可接受价格_300-500元_2_3_3_C" , "可接受价格在300至500元占比" ,
  "可接受价格_500元以上_2_3_4_C"   , "可接受价格在500元以上占比"   ,
  "可接受价格_无所谓_2_3_5_C"      , "可接受价格无所谓占比"       ,
  "服务期望_免费安装_2_4_1_C"      , "期望免费安装占比"         ,
  "服务期望_24小时售后_2_4_2_C"    , "期望24小时售后占比"       ,
  "服务期望_定期维护_2_4_3_C"      , "期望定期维护占比"         ,
  "服务期望_上门教学_2_4_4_C"      , "期望上门教学占比"         ,
  "服务期望_其他_2_4_5_C"        , "期望其他服务占比"         ,
  "照护价值_3_1_1_C"           , "安全提升认可度"          ,
  "照护价值_3_1_2_C"           , "照护减负认可度"          ,
  "照护价值_3_1_3_C"           , "性价比认可度"           ,
  "照护价值_3_1_4_C"           , "产品质量认可度"          ,
  "代际责任_3_2_1_C"           , "安全责任感知度"          ,
  "代际责任_3_2_2_C"           , "跌倒风险担忧度"          ,
  "代际责任_3_2_3_C"           , "关爱行为表达度"          ,
  "代际责任_3_2_4_C"           , "亲友影响感知度"          ,
  "决策便利_3_3_1_C"           , "购买渠道便利度"          ,
  "决策便利_3_3_2_C"           , "安装预约便利度"          ,
  "决策便利_3_3_3_C"           , "补贴流程便利度"          ,
  "决策便利_3_3_4_C"           , "售后服务便利度"          ,
  "资源投入_3_4_1_C"           , "费用承担意愿度"          ,
  "资源投入_3_4_2_C"           , "精力投入意愿度"          ,
  "资源投入_3_4_3_C"           , "操作教学胜任度"          ,
  "资源投入_3_4_4_C"           , "维护协助意愿度"          ,
  "认知偏差_3_5_1_C"           , "类型偏好差异度"          ,
  "认知偏差_3_5_2_C"           , "需求认知差异度"          ,
  "认知偏差_3_5_3_C"           , "价格接受差异度"          ,
  "认知偏差_3_5_4_C"           , "政策了解差异度"          ,
  "购买意愿_3_6_1_C"           , "主动购买意愿度"          ,
  "购买意愿_3_6_2_C"           , "按需购买意愿度"          ,
  "购买意愿_3_6_3_C"           , "推荐他人意愿度"          ,
  "问卷总分_C"                 , "问卷总分均值"
)

# ---------------------- 老年人群体变量映射 ----------------------
name_map_E <- tibble::tribble(
  ~原始变量名                   , ~自定义特征名            ,
  "性别_男_1_1_E"             , "男性占比"             ,
  "年龄_60-64岁_1_2_1_E"      , "年龄在60至64岁占比"      ,
  "年龄_65-74岁_1_2_2_E"      , "年龄在65至74岁占比"      ,
  "年龄_75-84岁_1_2_3_E"      , "年龄在75至84岁占比"      ,
  "年龄_85岁及以上_1_2_4_E"      , "年龄在85岁及以上占比"      ,
  "健康状况_失能程度_1_3_E"        , "失能程度均值"           ,
  "居住行政区_和平区_1_4_1_E"      , "和平区占比"            ,
  "居住行政区_南开区_1_4_2_E"      , "南开区占比"            ,
  "居住行政区_河北区_1_4_3_E"      , "河北区占比"            ,
  "居住行政区_河东区_1_4_4_E"      , "河东区占比"            ,
  "居住行政区_河西区_1_4_5_E"      , "河西区占比"            ,
  "居住行政区_红桥区_1_4_6_E"      , "红桥区占比"            ,
  "养老居住场景_居家独居_1_5_1_E"    , "居家独居率"            ,
  "养老居住场景_居家同住_1_5_2_E"    , "居家与子女同住率"         ,
  "居家跌倒经历_有_1_6_E"         , "居家跌倒经历率"          ,
  "已装扶手_是_1_7_E"           , "已装扶手率"            ,
  "可接受价格_100元以内_2_3_1_E"   , "可接受价格在100元以内占比"   ,
  "可接受价格_100-300元_2_3_2_E" , "可接受价格在100至300元占比" ,
  "可接受价格_300-500元_2_3_3_E" , "可接受价格在300至500元占比" ,
  "可接受价格_500元以上_2_3_4_E"   , "可接受价格在500元以上占比"   ,
  "可接受价格_无所谓_2_3_5_E"      , "可接受价格无所谓占比"       ,
  "扶手安装场景_卫生间_2_1_1_E"     , "卫生间安装偏好率"         ,
  "扶手安装场景_床边_2_1_2_E"      , "床边安装偏好率"          ,
  "扶手安装场景_楼道_2_1_3_E"      , "楼道安装偏好率"          ,
  "扶手安装场景_阳台_2_1_4_E"      , "阳台安装偏好率"          ,
  "扶手安装场景_其他_2_1_5_E"      , "其他安装偏好率"          ,
  "扶手偏好类型_基础不锈钢_2_2_1_E"   , "基础不锈钢款偏好率"        ,
  "扶手偏好类型_可折叠_2_2_2_E"     , "可折叠款偏好率"          ,
  "扶手偏好类型_防滑_2_2_3_E"      , "防滑款偏好率"           ,
  "扶手偏好类型_智能预警_2_2_4_E"    , "智能预警款偏好率"         ,
  "扶手偏好类型_其他_2_2_5_E"      , "其他类型偏好率"          ,
  "补贴期望_提高力度_2_4_1_E"      , "提高补贴力度期望率"        ,
  "补贴期望_简化流程_2_4_2_E"      , "简化申请流程期望率"        ,
  "补贴期望_差异化补贴_2_4_3_E"     , "差异化补贴期望率"         ,
  "补贴期望_社区代办_2_4_4_E"      , "社区代办申请期望率"        ,
  "补贴期望_其他_2_4_5_E"        , "其他补贴期望率"          ,
  "风险收益_3_1_1_E"           , "防跌倒效果认可度"         ,
  "风险收益_3_1_2_E"           , "操作简易认可度"          ,
  "风险收益_3_1_3_E"           , "安全收益感知度"          ,
  "风险收益_3_1_4_E"           , "安装风险担忧度"          ,
  "社会支持_3_2_1_E"           , "家人支持感知度"          ,
  "社会支持_3_2_2_E"           , "社区宣传感知度"          ,
  "社会支持_3_2_3_E"           , "亲友推荐感知度"          ,
  "社会支持_3_2_4_E"           , "安装协助感知度"          ,
  "政策适配_3_3_1_E"           , "补贴政策合理性"          ,
  "政策适配_3_3_2_E"           , "补贴金额满意度"          ,
  "政策适配_3_3_3_E"           , "申请流程便利度"          ,
  "政策适配_3_3_4_E"           , "政策宣传到位度"          ,
  "生活依赖_3_4_1_E"           , "行动辅助依赖度"          ,
  "生活依赖_3_4_2_E"           , "起居辅助依赖度"          ,
  "生活依赖_3_4_3_E"           , "跌倒后安全依赖度"         ,
  "生活依赖_3_4_4_E"           , "独处时安全依赖度"         ,
  "使用意愿_3_5_1_E"           , "持续使用意愿度"          ,
  "使用意愿_3_5_2_E"           , "主动申请意愿度"          ,
  "使用意愿_3_5_3_E"           , "推荐他人意愿度"          ,
  "问卷总分_E"                 , "问卷总分均值"
)

# ==============================================================
# 六、聚类群体特征剖面计算
# ==============================================================

# ---------------------- 子女群体各聚类特征均值 ----------------------
df_C_profile_indpd <- df_C_all_dbl |>
  left_join(df_C_cluster_tag, by = "序号_C") |>
  group_by(聚类标签_C) |>
  summarise(
    样本量 = n(),
    问卷总分均值 = mean(问卷总分_C),
    # 人口学特征
    男性占比 = mean(性别_男_1_1_C),
    # 子女年龄分布
    年龄在18至24岁占比 = mean(`年龄_18-24岁_1_2_1_C`),
    年龄在25至34岁占比 = mean(`年龄_25-34岁_1_2_2_C`),
    年龄在35至44岁占比 = mean(`年龄_35-44岁_1_2_3_C`),
    年龄在45至54岁占比 = mean(`年龄_45-54岁_1_2_4_C`),
    年龄在55岁及以上占比 = mean(年龄_55岁及以上_1_2_5_C),
    # 居住关系
    与老人同住占比 = mean(与老人居住关系_同住_1_3_1_C),
    与老人同城不同住占比 = mean(与老人居住关系_同城不同住_1_3_2_C),
    与老人异地居住占比 = mean(与老人居住关系_异地居住_1_3_3_C),
    # 老人年龄
    老人在60至64岁占比 = mean(`老人年龄_60-64岁_1_4_1_C`),
    老人在65至74岁占比 = mean(`老人年龄_65-74岁_1_4_2_C`),
    老人在75至84岁占比 = mean(`老人年龄_75-84岁_1_4_3_C`),
    老人在85岁及以上占比 = mean(老人年龄_85岁及以上_1_4_4_C),
    # 老人健康状况
    老人失能程度均值 = mean(老人健康状况_失能程度_1_5_C),
    # 居住区域
    和平区占比 = mean(居住行政区_和平区_1_6_1_C),
    南开区占比 = mean(居住行政区_南开区_1_6_2_C),
    河北区占比 = mean(居住行政区_河北区_1_6_3_C),
    河东区占比 = mean(居住行政区_河东区_1_6_4_C),
    河西区占比 = mean(居住行政区_河西区_1_6_5_C),
    红桥区占比 = mean(居住行政区_红桥区_1_6_6_C),
    # 扶手安装情况
    为老人装过扶手占比 = mean(为老人装过扶手_是_1_7_C),
    # 安装场景偏好
    卫生间安装偏好占比 = mean(扶手安装场景_卫生间_2_1_1_C),
    床边安装偏好占比 = mean(扶手安装场景_床边_2_1_2_C),
    楼道安装偏好占比 = mean(扶手安装场景_楼道_2_1_3_C),
    阳台安装偏好占比 = mean(扶手安装场景_阳台_2_1_4_C),
    其他安装场景占比 = mean(扶手安装场景_其他_2_1_5_C),
    # 扶手类型偏好
    基础不锈钢偏好占比 = mean(扶手偏好类型_基础不锈钢_2_2_1_C),
    可折叠扶手偏好占比 = mean(扶手偏好类型_可折叠_2_2_2_C),
    防滑扶手偏好占比 = mean(扶手偏好类型_防滑_2_2_3_C),
    智能预警扶手偏好占比 = mean(扶手偏好类型_智能预警_2_2_4_C),
    其他类型偏好占比 = mean(扶手偏好类型_其他_2_2_5_C),
    # 可接受价格
    可接受价格在100元以内占比 = mean(可接受价格_100元以内_2_3_1_C),
    可接受价格在100至300元占比 = mean(`可接受价格_100-300元_2_3_2_C`),
    可接受价格在300至500元占比 = mean(`可接受价格_300-500元_2_3_3_C`),
    可接受价格在500元以上占比 = mean(可接受价格_500元以上_2_3_4_C),
    可接受价格无所谓占比 = mean(可接受价格_无所谓_2_3_5_C),
    # 服务期望
    期望免费安装占比 = mean(服务期望_免费安装_2_4_1_C),
    期望24小时售后占比 = mean(服务期望_24小时售后_2_4_2_C),
    期望定期维护占比 = mean(服务期望_定期维护_2_4_3_C),
    期望上门教学占比 = mean(服务期望_上门教学_2_4_4_C),
    期望其他服务占比 = mean(服务期望_其他_2_4_5_C),
    # 核心潜变量
    安全提升认可度 = mean(照护价值_3_1_1_C),
    照护减负认可度 = mean(照护价值_3_1_2_C),
    性价比认可度 = mean(照护价值_3_1_3_C),
    产品质量认可度 = mean(照护价值_3_1_4_C),
    安全责任感知度 = mean(代际责任_3_2_1_C),
    跌倒风险担忧度 = mean(代际责任_3_2_2_C),
    关爱行为表达度 = mean(代际责任_3_2_3_C),
    亲友影响感知度 = mean(代际责任_3_2_4_C),
    购买渠道便利度 = mean(决策便利_3_3_1_C),
    安装预约便利度 = mean(决策便利_3_3_2_C),
    补贴流程便利度 = mean(决策便利_3_3_3_C),
    售后服务便利度 = mean(决策便利_3_3_4_C),
    费用承担意愿度 = mean(资源投入_3_4_1_C),
    精力投入意愿度 = mean(资源投入_3_4_2_C),
    操作教学胜任度 = mean(资源投入_3_4_3_C),
    维护协助意愿度 = mean(资源投入_3_4_4_C),
    类型偏好差异度 = mean(认知偏差_3_5_1_C),
    需求认知差异度 = mean(认知偏差_3_5_2_C),
    价格接受差异度 = mean(认知偏差_3_5_3_C),
    政策了解差异度 = mean(认知偏差_3_5_4_C),
    主动购买意愿度 = mean(购买意愿_3_6_1_C),
    按需购买意愿度 = mean(购买意愿_3_6_2_C),
    推荐他人意愿度 = mean(购买意愿_3_6_3_C),
    .groups = "drop"
  )

# ---------------------- 老年人群体各聚类特征均值 ----------------------
df_E_profile_indpd <- df_E_all_dbl |>
  left_join(df_E_cluster_tag, by = "序号_E") |>
  group_by(聚类标签_E) |>
  summarise(
    样本量 = n(),
    问卷总分均值 = mean(问卷总分_E),
    # 基础信息
    男性占比 = mean(性别_男_1_1_E),
    # 年龄分类
    年龄在60至64岁占比 = mean(`年龄_60-64岁_1_2_1_E`),
    年龄在65至74岁占比 = mean(`年龄_65-74岁_1_2_2_E`),
    年龄在75至84岁占比 = mean(`年龄_75-84岁_1_2_3_E`),
    年龄在85岁及以上占比 = mean(年龄_85岁及以上_1_2_4_E),
    失能程度均值 = mean(健康状况_失能程度_1_3_E),
    # 居住区域
    和平区占比 = mean(居住行政区_和平区_1_4_1_E),
    南开区占比 = mean(居住行政区_南开区_1_4_2_E),
    河北区占比 = mean(居住行政区_河北区_1_4_3_E),
    河东区占比 = mean(居住行政区_河东区_1_4_4_E),
    河西区占比 = mean(居住行政区_河西区_1_4_5_E),
    红桥区占比 = mean(居住行政区_红桥区_1_4_6_E),
    居家独居率 = mean(养老居住场景_居家独居_1_5_1_E),
    居家与子女同住率 = mean(养老居住场景_居家同住_1_5_2_E),
    居家跌倒经历率 = mean(居家跌倒经历_有_1_6_E),
    已装扶手率 = mean(已装扶手_是_1_7_E),
    # 可接受价格
    可接受价格在100元以内占比 = mean(可接受价格_100元以内_2_3_1_E),
    可接受价格在100至300元占比 = mean(`可接受价格_100-300元_2_3_2_E`),
    可接受价格在300至500元占比 = mean(`可接受价格_300-500元_2_3_3_E`),
    可接受价格在500元以上占比 = mean(可接受价格_500元以上_2_3_4_E),
    可接受价格无所谓占比 = mean(可接受价格_无所谓_2_3_5_E),
    # 安装场景偏好率
    卫生间安装偏好率 = mean(扶手安装场景_卫生间_2_1_1_E),
    床边安装偏好率 = mean(扶手安装场景_床边_2_1_2_E),
    楼道安装偏好率 = mean(扶手安装场景_楼道_2_1_3_E),
    阳台安装偏好率 = mean(扶手安装场景_阳台_2_1_4_E),
    其他安装偏好率 = mean(扶手安装场景_其他_2_1_5_E),
    # 扶手类型偏好率
    基础不锈钢款偏好率 = mean(扶手偏好类型_基础不锈钢_2_2_1_E),
    可折叠款偏好率 = mean(扶手偏好类型_可折叠_2_2_2_E),
    防滑款偏好率 = mean(扶手偏好类型_防滑_2_2_3_E),
    智能预警款偏好率 = mean(扶手偏好类型_智能预警_2_2_4_E),
    其他类型偏好率 = mean(扶手偏好类型_其他_2_2_5_E),
    # 补贴期望率
    提高补贴力度期望率 = mean(补贴期望_提高力度_2_4_1_E),
    简化申请流程期望率 = mean(补贴期望_简化流程_2_4_2_E),
    差异化补贴期望率 = mean(补贴期望_差异化补贴_2_4_3_E),
    社区代办申请期望率 = mean(补贴期望_社区代办_2_4_4_E),
    其他补贴期望率 = mean(补贴期望_其他_2_4_5_E),
    # 核心潜变量
    防跌倒效果认可度 = mean(风险收益_3_1_1_E),
    操作简易认可度 = mean(风险收益_3_1_2_E),
    安全收益感知度 = mean(风险收益_3_1_3_E),
    安装风险担忧度 = mean(风险收益_3_1_4_E),
    家人支持感知度 = mean(社会支持_3_2_1_E),
    社区宣传感知度 = mean(社会支持_3_2_2_E),
    亲友推荐感知度 = mean(社会支持_3_2_3_E),
    安装协助感知度 = mean(社会支持_3_2_4_E),
    补贴政策合理性 = mean(政策适配_3_3_1_E),
    补贴金额满意度 = mean(政策适配_3_3_2_E),
    申请流程便利度 = mean(政策适配_3_3_3_E),
    政策宣传到位度 = mean(政策适配_3_3_4_E),
    行动辅助依赖度 = mean(生活依赖_3_4_1_E),
    起居辅助依赖度 = mean(生活依赖_3_4_2_E),
    跌倒后安全依赖度 = mean(生活依赖_3_4_3_E),
    独处时安全依赖度 = mean(生活依赖_3_4_4_E),
    持续使用意愿度 = mean(使用意愿_3_5_1_E),
    主动申请意愿度 = mean(使用意愿_3_5_2_E),
    推荐他人意愿度 = mean(使用意愿_3_5_3_E),
    .groups = "drop"
  )

# ==============================================================
# 七、特征标准化与差异分析（0~1标准化）
# ==============================================================

# ---------------------- 子女群体：全局极值计算 + 标准化 ----------------------
vars_to_scale_C <- name_map_C$原始变量名

global_minmax_C <- df_C_all_dbl %>%
  select(all_of(vars_to_scale_C)) %>%
  summarise(across(
    everything(),
    list(min = ~ min(., na.rm = TRUE), max = ~ max(., na.rm = TRUE))
  )) %>%
  unlist() %>%
  as.data.frame() %>%
  rownames_to_column("name") %>%
  rename(value = 2) %>%
  mutate(
    类型 = str_extract(name, "min|max"),
    原始变量名 = str_remove(name, "_min$|_max$")
  ) %>%
  select(原始变量名, 类型, value) %>%
  pivot_wider(names_from = 类型, values_from = value) %>%
  left_join(name_map_C, by = "原始变量名") %>%
  select(特征 = 自定义特征名, min, max)

df_C_profile_scaled <- df_C_profile_indpd %>%
  pivot_longer(-聚类标签_C, names_to = "特征", values_to = "原始均值") %>%
  filter(特征 != "样本量") %>%
  left_join(global_minmax_C, by = "特征") %>%
  mutate(
    标准化值 = case_when(
      max == min ~ 0,
      TRUE ~ (原始均值 - min) / (max - min)
    )
  ) %>%
  select(聚类标签_C, 特征, 标准化值) %>%
  pivot_wider(
    id_cols = 特征,
    names_from = 聚类标签_C,
    values_from = 标准化值,
    names_prefix = "群体"
  ) %>%
  mutate(差异 = abs(群体1 - 群体2)) %>%
  arrange(desc(差异))

# ---------------------- 老年人群体：全局极值计算 + 标准化 ----------------------
vars_to_scale_E <- name_map_E$原始变量名

global_minmax_E <- df_E_all_dbl %>%
  select(all_of(vars_to_scale_E)) %>%
  summarise(across(
    everything(),
    list(min = ~ min(., na.rm = TRUE), max = ~ max(., na.rm = TRUE))
  )) %>%
  unlist() %>%
  as.data.frame() %>%
  rownames_to_column("name") %>%
  rename(value = 2) %>%
  mutate(
    类型 = str_extract(name, "min|max"),
    原始变量名 = str_remove(name, "_min$|_max$")
  ) %>%
  select(原始变量名, 类型, value) %>%
  pivot_wider(names_from = 类型, values_from = value) %>%
  left_join(name_map_E, by = "原始变量名") %>%
  select(特征 = 自定义特征名, min, max)

df_E_profile_scaled <- df_E_profile_indpd %>%
  pivot_longer(-聚类标签_E, names_to = "特征", values_to = "原始均值") %>%
  filter(特征 != "样本量") %>%
  left_join(global_minmax_E, by = "特征") %>%
  mutate(
    标准化值 = case_when(
      max == min ~ 0,
      TRUE ~ (原始均值 - min) / (max - min)
    )
  ) %>%
  select(聚类标签_E, 特征, 标准化值) %>%
  pivot_wider(
    id_cols = 特征,
    names_from = 聚类标签_E,
    values_from = 标准化值,
    names_prefix = "群体"
  ) %>%
  mutate(差异 = abs(群体1 - 群体2)) %>%
  arrange(desc(差异))

# ==============================================================
# 八、特征差异可视化绘图函数
# ==============================================================

# 统一风格绘图：顶部高区分度特征条形图
plot_top_features <- function(df, title) {
  df |>
    filter(特征 != '样本量', 差异 > 0.15) |>
    ggplot(aes(x = reorder(特征, 差异), y = 差异, fill = 差异)) + # 按差异上色
    geom_bar(
      stat = "identity",
      color = "black",
      alpha = 0.9,
      linewidth = 0.3
    ) +
    coord_flip() +
    labs(
      title = title,
      x = "特征",
      y = "两聚类间差异绝对值"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      axis.title.x = element_text(face = "bold", size = 12),
      axis.title.y = element_text(face = "bold", size = 12),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      panel.grid.major.x = element_line(color = "gray85", linewidth = 0.3),
      panel.grid.minor.x = element_blank(),
      strip.text = element_text(face = "bold", size = 11),
      strip.background = element_rect(fill = "white"),
      legend.position = "none" # 渐变不需要图例
    ) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.05)) +
    # ========== 学术连续渐变色盘：越大越深 ==========
    scale_fill_distiller(palette = "YlOrRd", direction = 1)
}

# 绘制整体高区分度图
p1 <- plot_top_features(
  df_E_profile_scaled,
  "老年人两聚类的各特征差异(TOP)"
)

p2 <- plot_top_features(
  df_C_profile_scaled,
  "子女两聚类的各特征差异(TOP)"
)

# ==============================================================
# 九、核心维度分面数据整理
# ==============================================================

# 老年人群体维度分面
E_facet_df <- df_E_profile_scaled |>
  left_join(name_map_E, by = c("特征" = "自定义特征名")) |>
  filter(str_detect(
    原始变量名,
    "风险收益|社会支持|政策适配|生活依赖|使用意愿"
  )) |>
  mutate(
    维度 = case_when(
      str_detect(原始变量名, "风险收益") ~ "风险收益",
      str_detect(原始变量名, "社会支持") ~ "社会支持",
      str_detect(原始变量名, "政策适配") ~ "政策适配",
      str_detect(原始变量名, "生活依赖") ~ "生活依赖",
      str_detect(原始变量名, "使用意愿") ~ "使用意愿",
      TRUE ~ NA
    )
  ) |>
  select(-原始变量名) |>
  rename(高需求积极型 = 群体1, 低需求保守型 = 群体2)

# 子女群体维度分面
C_facet_df <- df_C_profile_scaled |>
  left_join(name_map_C, by = c("特征" = "自定义特征名")) |>
  filter(str_detect(
    原始变量名,
    "照护价值|代际责任|决策便利|资源投入|认知偏差|购买意愿"
  )) |>
  mutate(
    维度 = case_when(
      str_detect(原始变量名, "照护价值") ~ "照护价值",
      str_detect(原始变量名, "代际责任") ~ "代际责任",
      str_detect(原始变量名, "决策便利") ~ "决策便利",
      str_detect(原始变量名, "资源投入") ~ "资源投入",
      str_detect(原始变量名, "认知偏差") ~ "认知偏差",
      str_detect(原始变量名, "购买意愿") ~ "购买意愿",
      TRUE ~ NA
    )
  ) |>
  select(-原始变量名) |>
  rename(高卷入积极型 = 群体1, 低卷入保守型 = 群体2)

# ==============================================================
# 十、导出维度差异表
# ==============================================================
write_xlsx_with_auto_width(
  E_facet_df,
  paste0(output_path, "老年人_各核心维度特征差异.xlsx")
)

write_xlsx_with_auto_width(
  C_facet_df,
  paste0(output_path, "子女_各核心维度特征差异.xlsx")
)

cat("✅ 两个维度差异表已自动导出至：", output_path, "\n")

# ==============================================================
# 十一、维度分面差异图绘制
# ==============================================================

# 统一风格：维度分面差异条形图函数
plot_facet_dimension <- function(df, title) {
  ggplot(df, aes(x = 差异, y = reorder(特征, 差异), fill = 差异)) +
    geom_bar(
      stat = "identity",
      color = "black",
      alpha = 0.8,
      linewidth = 0.3
    ) +
    facet_wrap(vars(维度), scales = "free_y", ncol = 2) +
    scale_x_continuous(breaks = seq(0, 1, 0.1)) +
    # RdPu 渐变色：数值越大，颜色越深（论文最常用）
    scale_fill_distiller(palette = "YlOrBr", direction = 1) +
    labs(
      title = title,
      x = "两聚类间差异绝对值",
      y = "特征"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
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
}

# 绘制分面差异图
p3 <- plot_facet_dimension(E_facet_df, "老年人两聚类的各核心维度特征差异")
p4 <- plot_facet_dimension(C_facet_df, "子女两聚类的各核心维度特征差异")

# 组合最终差异图
diffplot <- (p1 + p3 + plot_spacer() + plot_spacer() + p2 + p4) +
  plot_layout(ncol = 2, widths = c(1, 2), heights = c(10, 1, 10))

# 导出特征差异组合图
ggsave(
  filename = paste0(output_path, "05_特征差异组合图.png"),
  plot = diffplot,
  width = 15,
  height = 10,
  dpi = 300
)

cat("✅ 所有图片已保存至：", output_path, "\n")
