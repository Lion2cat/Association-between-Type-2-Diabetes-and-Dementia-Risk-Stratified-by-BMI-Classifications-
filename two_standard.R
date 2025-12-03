library(dplyr)
library(ggplot2)
library(tidyr)

# ==============================================================================
# Step 1: 定义两种标准
# ==============================================================================
# 确保数据已加载
if(!exists("df")) load("mini_project_dementia_diabetes.Rdata")

df_standards <- df %>%
  mutate(
    # 1. 国际标准 (WHO)
    # Normal: 18.5-25, Overweight: 25-30, Obese: >30
    Group_WHO = case_when(
      BMI < 18.5 ~ "Underweight",
      BMI >= 18.5 & BMI < 25 ~ "Normal",
      BMI >= 25 & BMI < 30 ~ "Overweight",
      BMI >= 30 ~ "Obese"
    ),
    
    # 2. 亚洲标准 (WGOC - Working Group on Obesity in China)
    # Normal: 18.5-24, Overweight: 24-28, Obese: >28
    Group_Asian = case_when(
      BMI < 18.5 ~ "Underweight",
      BMI >= 18.5 & BMI < 24 ~ "Normal",
      BMI >= 24 & BMI < 28 ~ "Overweight",
      BMI >= 28 ~ "Obese"
    )
  )

# ==============================================================================
# Step 2: 转换数据格式以便绘图
# ==============================================================================
# 统计两种标准下各组的人数
plot_data <- df_standards %>%
  select(Group_WHO, Group_Asian) %>%
  pivot_longer(cols = c(Group_WHO, Group_Asian), names_to = "Standard", values_to = "Category") %>%
  group_by(Standard, Category) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  mutate(
    Standard = ifelse(Standard == "Group_WHO", "International (WHO)", "Asian (WGOC)"),
    Category = factor(Category, levels = c("Underweight", "Normal", "Overweight", "Obese")),
    # 计算百分比用于标签
    Percent = Count / 400000
  )

# ==============================================================================
# Step 3: 绘制对比图
# ==============================================================================
ggplot(plot_data, aes(x = Standard, y = Count, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  
  # 添加数字标签
  geom_text(aes(label = paste0(round(Percent*100, 1), "%")), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 4, fontface = "bold") +
  
  # 配色 (瘦=蓝, 正常=绿, 超重=黄, 肥胖=红)
  scale_fill_manual(values = c("Underweight"="#3498DB", "Normal"="#2ECC71", "Overweight"="#F1C40F", "Obese"="#E74C3C")) +
  
  labs(title = "Comparison of BMI Classification Standards",
       subtitle = "Left: WHO Standard vs. Right: Asian Standard (WGOC)",
       y = "Number of Participants", x = "") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "top")
