fixed_scale <- function(x, feature_name, threshold_table) {
  # 1. 检查特征名是否在阈值表中，不在则直接返回原值
  if (!feature_name %in% names(threshold_table)) {
    return(x)
  }

  # 2. 提取该特征的理论阈值
  min_val <- threshold_table[[feature_name]][1]
  max_val <- threshold_table[[feature_name]][2]

  # 3. 处理分母为0的情况（最大值=最小值时，全部设为0）
  if (max_val - min_val == 0) {
    return(rep(0, length(x)))
  }

  # 4. 执行0-1放缩
  scaled_x <- (x - min_val) / (max_val - min_val)

  return(scaled_x)
}
