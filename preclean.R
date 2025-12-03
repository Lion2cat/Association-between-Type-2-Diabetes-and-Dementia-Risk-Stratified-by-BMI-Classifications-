library(dplyr)
library(ggplot2)
library(gridExtra)

# 确保数据对象名为 df
if(exists("d")) df <- d 

# 定义一个检查函数：同时画 直方图(看分布形态) 和 箱线图(看离群点)
check_continuous <- function(data, var_name, bin_width = 1) {
  # 1. 直方图 + 密度曲线
  p1 <- ggplot(data, aes_string(x = var_name)) +
    geom_histogram(aes(y = ..density..), binwidth = bin_width, fill = "#3498DB", color = "white", alpha=0.7) +
    geom_density(color = "#E74C3C", size = 1) +
    labs(title = paste("Distribution of", var_name), subtitle = "Check for Normality & Skewness") +
    theme_minimal()
  
  # 2. 箱线图 (标记极端值)
  p2 <- ggplot(data, aes_string(y = var_name)) +
    geom_boxplot(fill = "#F1C40F", outlier.colour = "red", outlier.shape = 1, outlier.size = 3) +
    labs(title = paste("Outliers of", var_name), subtitle = "Red points are potential outliers") +
    theme_minimal()
  
  grid.arrange(p1, p2, ncol = 2)
}

# --- 执行检查 ---
# 1. 检查 BMI (重点！)
check_continuous(df, "BMI", bin_width = 1)
# 打印具体极值数量，辅助决策
cat("BMI > 60 (超级肥胖) 人数:", sum(df$BMI > 60), "\n")
cat("BMI < 15 (重度消瘦) 人数:", sum(df$BMI < 15), "\n")

# 2. 检查 Age
check_continuous(df, "age", bin_width = 1)


# 定义分类变量列表
cat_vars <- c("sex", "education", "smoking", "diabetes", "age_group")

for (v in cat_vars) {
  cat("\n----------------------------------\n")
  cat("Checking Variable:", v, "\n")
  print(table(df[[v]], useNA = "ifany")) # 显示 NA 值
  
  # 画条形图
  print(ggplot(df, aes_string(x = v)) +
          geom_bar(fill = "steelblue") +
          geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
          labs(title = paste("Frequency of", v)) +
          theme_minimal())
}


cat("\n--- Dementia (Outcome) ---\n")
print(table(df$dementia))

df_clean <- df %>%
  filter(BMI > 10 & BMI < 100) %>%  # 保留 12-74，剔除极端错误
  mutate(dementia = as.factor(dementia)) # 转为因子

cat("\n清洗后样本量:", nrow(df_clean), "\n")

