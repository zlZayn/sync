# 加载必要的包
library(tidyverse)

# 模拟数据：创建包含风险收益_1到风险收益_4的数据集
# 假设数据是5点量表（1-5）
set.seed(123) # 确保结果可重复
df_E_desc <- data.frame(
  风险收益_1 = sample(1:5, 100, replace = TRUE),
  风险收益_2 = sample(1:5, 100, replace = TRUE),
  风险收益_3 = sample(1:5, 100, replace = TRUE),
  风险收益_4 = sample(1:5, 100, replace = TRUE)
)

# 复刻绘图代码
p12 <- df_E_desc %>%
  # 原句：summarise(across(风险收益_1:风险收益_4, mean))
  # 修改：同时计算均值mean和标准差sd
  summarise(across(
    风险收益_1:风险收益_4,
    list(mean = ~ mean(.x, na.rm = T), sd = ~ sd(.x, na.rm = T))
  )) %>%
  # 原句：pivot_longer(everything()) %>% rename(风险收益=name, 平均值=value)
  # 修改：拆分出题项、mean、sd三列
  pivot_longer(
    everything(),
    names_to = c("题项", ".value"),
    names_sep = "_(?=mean|sd)"
  ) %>%
  arrange(mean) %>%
  mutate(题项 = factor(题项, levels = 题项)) %>%

  ggplot(aes(x = mean, y = 题项)) +
  geom_bar(
    stat = "identity",
    aes(fill = 题项),
    color = "black",
    alpha = 0.8,
    linewidth = 0.3
  ) +
  # 原句：无
  # 修改：新增水平误差棒，显示±标准差
  geom_errorbarh(
    aes(xmin = mean - sd, xmax = mean + sd),
    height = 0.3,
    color = "black",
    linewidth = 0.3
  ) +
  # 原句：xlim(0, 5)
  # 修改：扩大X轴范围，防止误差棒超出，并添加明确的刻度
  scale_x_continuous(breaks = seq(0, 5, 1), limits = c(0, 5.5)) +
  scale_fill_brewer(palette = "Blues", type = "seq") +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor.x = element_blank(),
    legend.position = "none"
  )

# 显示图形
print(p12)
