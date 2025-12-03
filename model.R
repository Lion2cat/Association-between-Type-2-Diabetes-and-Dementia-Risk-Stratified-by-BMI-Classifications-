# ==============================================================================
# 0. 环境准备 (Setup)
# ==============================================================================
packages <- c("dplyr", "ggplot2", "broom", "readr", "gridExtra", "grid", "gtable")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(dplyr)
library(ggplot2)
library(broom)
library(gridExtra)
library(grid)
library(gtable)

# ==============================================================================
# 1. 数据加载与清洗 (Data Loading & Cleaning)
# ==============================================================================
# 尝试加载默认文件，如果不存在则弹出选择框
if(file.exists("mini_project_dementia_diabetes.Rdata")) {
  load("mini_project_dementia_diabetes.Rdata")
  message(">>> 已加载默认数据文件。")
} else {
  message(">>> 请选择您的 .Rdata 文件...")
  load(file.choose())
}

# 统一变量名
if(exists("d")) df <- d
if(!exists("df")) stop("错误：未找到数据对象 df 或 d")

final_data <- df %>%
  filter(BMI > 10 & BMI < 100) %>%
  filter(age >= 5 & age <= 100) %>%
  na.omit() %>%
  mutate(
    dementia = factor(dementia, levels = c(0, 1), labels = c("No", "Yes")),
    diabetes = factor(diabetes, levels = c("no diabetes", "diabetes")),
    sex = factor(sex, levels = c("female", "male")),
    education = factor(education, levels = c("low education", "high education")),
    smoking = factor(smoking, levels = c("no smoking", "current smoking")),
    
    # --- 标准 A: WHO ---
    Group_WHO = case_when(
      BMI < 18.5 ~ "Underweight",
      BMI >= 18.5 & BMI < 25 ~ "Normal",
      BMI >= 25 & BMI < 30 ~ "Overweight",
      BMI >= 30 ~ "Obese"
    ),
    Group_WHO = factor(Group_WHO, levels = c("Normal", "Underweight", "Overweight", "Obese")),
    
    # --- 标准 B: Asian ---
    Group_Asian = case_when(
      BMI < 18.5 ~ "Underweight",
      BMI >= 18.5 & BMI < 24 ~ "Normal",
      BMI >= 24 & BMI < 28 ~ "Overweight",
      BMI >= 28 ~ "Obese"
    ),
    Group_Asian = factor(Group_Asian, levels = c("Normal", "Underweight", "Overweight", "Obese"))
  )

cat(">>> 数据准备完成。样本量:", nrow(final_data), "\n")

# ==============================================================================
# 2. 定义分析函数 (Helper Functions)
# ==============================================================================
# 自动化分层分析函数
run_stratified <- function(data, group_col) {
  levels_vec <- levels(data[[group_col]])
  results_list <- list()
  for(g in levels_vec) {
    sub_data <- data[data[[group_col]] == g, ]
    tryCatch({
      m <- glm(dementia ~ diabetes + age + sex + education + smoking, 
               data = sub_data, family = binomial)
      res <- tidy(m, exponentiate = TRUE, conf.int = TRUE) %>%
        filter(grepl("diabetes", term)) %>%
        mutate(
          Group = g,
          OR = round(estimate, 2),
          CI_Low = round(conf.low, 2),
          CI_High = round(conf.high, 2),
          P_Raw = p.value,
          P_Str = format.pval(p.value, eps = 0.001, digits = 3)
        ) %>% select(Group, OR, CI_Low, CI_High, P_Str)
      results_list[[g]] <- res
    }, error = function(e) message(paste("Skip:", g)))
  }
  return(do.call(rbind, results_list))
}

# 学术表格绘制函数
draw_academic_table <- function(data, title_text) {
  # 1. 定义三线表主题 (字体左对齐，增加内边距)
  academic_theme <- ttheme_minimal(
    base_size = 12,
    core = list(
      fg_params = list(hjust = 0, x = 0.05, fontfamily = "sans"),
      bg_params = list(fill = "white", col = NA),
      padding = unit(c(5, 5), "mm")
    ),
    colhead = list(
      fg_params = list(hjust = 0, x = 0.05, fontface = "bold", fontfamily = "sans"),
      bg_params = list(fill = "white", col = NA),
      padding = unit(c(5, 5), "mm")
    )
  )
  
  # 2. 生成基础表格对象
  g <- tableGrob(data, rows = NULL, theme = academic_theme)
  
  # 3. 添加标准三线 (加粗顶线和底线)
  # 顶线 (粗)
  g <- gtable_add_grob(g,
                       grobs = segmentsGrob(x0 = unit(0, "npc"), y0 = unit(1, "npc"), 
                                            x1 = unit(1, "npc"), y1 = unit(1, "npc"),
                                            gp = gpar(lwd = 3.0)),
                       t = 1, b = 1, l = 1, r = ncol(g))
  
  # 表头下线 (细)
  g <- gtable_add_grob(g,
                       grobs = segmentsGrob(x0 = unit(0, "npc"), y0 = unit(0, "npc"), 
                                            x1 = unit(1, "npc"), y1 = unit(0, "npc"),
                                            gp = gpar(lwd = 1.0)),
                       t = 1, b = 1, l = 1, r = ncol(g))
  
  # 底线 (粗)
  g <- gtable_add_grob(g,
                       grobs = segmentsGrob(x0 = unit(0, "npc"), y0 = unit(0, "npc"), 
                                            x1 = unit(1, "npc"), y1 = unit(0, "npc"),
                                            gp = gpar(lwd = 3.0)),
                       t = nrow(g), b = nrow(g), l = 1, r = ncol(g))
  
  # 创建标题文本对象
  title_grob <- textGrob(
    label = title_text,
    x = unit(0, "npc"), y = unit(0.3, "npc"), # 略微调整垂直位置
    hjust = 0, vjust = 0,
    gp = gpar(fontsize = 11, fontface = "bold", fontfamily = "sans")
  )
  
  # 在表格顶部插入一行 (高度包含标题和间距)
  g <- gtable_add_rows(g, heights = unit(1.2, "cm"), pos = 0)
  
  # 将标题放入这新的一行，并让它跨越所有列
  g <- gtable_add_grob(g, title_grob, t = 1, b = 1, l = 1, r = ncol(g))
  
  grid.newpage()
  grid.draw(g)
}

# ==============================================================================
# 3. 核心计算 (Running Models)
# ==============================================================================
cat("\n>>> 正在运行模型计算...\n")

# A. 亚洲标准计算
m_inter_asian <- glm(dementia ~ diabetes * Group_Asian + age + sex + education + smoking, 
                     data = final_data, family = binomial)
strat_res_asian <- run_stratified(final_data, "Group_Asian")

# B. WHO标准计算
m_inter_who <- glm(dementia ~ diabetes * Group_WHO + age + sex + education + smoking, 
                   data = final_data, family = binomial)
strat_res_who <- run_stratified(final_data, "Group_WHO")


# ==============================================================================
# 4. 生成 PPT 图表 - 亚洲标准 (Asian Criteria)
# ==============================================================================

# ------------------------------------------------------------------------------
# Slide 3: 交互作用测试结果表
# ------------------------------------------------------------------------------
tab_inter_asian <- tidy(m_inter_asian, exponentiate = FALSE) %>%
  filter(grepl(":", term)) %>%
  mutate(
    Label = gsub("diabetesdiabetes:Group_Asian", "Diabetes   ", term),
    Label = gsub("Group_Asian", "", Label),
    `P Value` = format.pval(p.value, eps = 0.001, digits = 3)
  ) %>%
  select(`Interaction Variable` = Label, `P Value`)

# 绘制
draw_academic_table(tab_inter_asian, "Table 2: Interaction Analysis (WGOC)")
cat(">>>(交互项表) 已绘制\n")

# ------------------------------------------------------------------------------
# Slide 4: 分层分析结果表
# ------------------------------------------------------------------------------
tab_strat_asian <- strat_res_asian %>%
  mutate(
    `OR (95% CI)` = sprintf("%.2f (%.2f - %.2f)", OR, CI_Low, CI_High),
    `P Value` = P_Str
  ) %>%
  arrange(factor(Group, levels = c("Underweight", "Normal", "Overweight", "Obese"))) %>%
  select(Subgroup = Group, `OR (95% CI)`, `P Value`)

# 绘制
draw_academic_table(tab_strat_asian, "Table 3: Stratified Association (WGOC)")
cat(">>> (分层表) 已绘制\n")

# ------------------------------------------------------------------------------
# Slide 5: 亚洲标准森林图 (Forest Plot)
# ------------------------------------------------------------------------------
plot_data_asian <- strat_res_asian
plot_data_asian$Group <- factor(plot_data_asian$Group, levels = c("Obese", "Overweight", "Normal", "Underweight"))

p_slide5 <- ggplot(plot_data_asian, aes(x = Group, y = OR, ymin = CI_Low, ymax = CI_High)) +
  geom_hline(yintercept = 1, linetype = "solid", color = "gray30") +
  geom_hline(yintercept = plot_data_asian$OR[plot_data_asian$Group == "Normal"], 
             linetype = "dashed", color = "#E41A1C", alpha = 0.6) +
  geom_errorbar(width = 0.25, color = "#377EB8", size = 1) +
  geom_point(size = 6, shape = 21, fill = "white", color = "#377EB8", stroke = 2) +
  geom_text(aes(label = sprintf("OR = %.2f", OR)), vjust = -1.2, fontface = "bold", size = 4.5) +
  scale_y_continuous(limits = c(0, 16)) +
  labs(
    title = "Dementia Risk by BMI Group (WGOC)",
    subtitle = "T2D Patients Risk Profile (Ref: Non-Diabetes)",
    x = NULL, y = "Odds Ratio (95% CI)"
  ) +
  theme_minimal(base_size = 14) +
  coord_flip() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.grid.major.y = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )

print(p_slide5)


# ==============================================================================
# 5.  WHO 标准补充分析
# ==============================================================================

# ------------------------------------------------------------------------------
# 1: WHO 交互作用表 (Table)
# ------------------------------------------------------------------------------
tab_inter_who <- tidy(m_inter_who, exponentiate = FALSE) %>%
  filter(grepl(":", term)) %>%
  mutate(
    Label = gsub("diabetesdiabetes:Group_WHO", "Diabetes   ", term),
    Label = gsub("Group_WHO", "", Label),
    `P Value` = format.pval(p.value, eps = 0.001, digits = 3)
  ) %>%
  select(`Interaction Variable` = Label, `P Value`)

draw_academic_table(tab_inter_who, "Table 4: Interaction Analysis (WHO)")

# ------------------------------------------------------------------------------
# 2: WHO 分层分析表 (Table)
# ------------------------------------------------------------------------------
tab_strat_who <- strat_res_who %>%
  mutate(
    `OR (95% CI)` = sprintf("%.2f (%.2f - %.2f)", OR, CI_Low, CI_High),
    `P Value` = P_Str
  ) %>%
  arrange(factor(Group, levels = c("Underweight", "Normal", "Overweight", "Obese"))) %>%
  select(Subgroup = Group, `OR (95% CI)`, `P Value`)

draw_academic_table(tab_strat_who, "Table 5: Stratified Association (WHO )")
cat(">>> Tables (WHO) 已绘制\n")

# ------------------------------------------------------------------------------
# 3: WHO 森林图 (Forest Plot)
# ------------------------------------------------------------------------------
plot_data_who <- strat_res_who
plot_data_who$Group <- factor(plot_data_who$Group, levels = c("Obese", "Overweight", "Normal", "Underweight"))

p_supp_forest <- ggplot(plot_data_who, aes(x = Group, y = OR, ymin = CI_Low, ymax = CI_High)) +
  geom_hline(yintercept = 1, linetype = "solid", color = "gray30") +
  geom_hline(yintercept = plot_data_who$OR[plot_data_who$Group == "Normal"], 
             linetype = "dashed", color = "#E41A1C", alpha = 0.6) +
  geom_errorbar(width = 0.25, color = "#4DAF4A", size = 1) +
  geom_point(size = 6, shape = 21, fill = "white", color = "#4DAF4A", stroke = 2) +
  geom_text(aes(label = sprintf("OR = %.2f", OR)), vjust = -1.2, fontface = "bold", size = 4.5) +
  scale_y_continuous(limits = c(0, 16)) +
  labs(
    title = "Dementia Risk by BMI Group (WHO Criteria)",
    x = NULL, y = "Odds Ratio (95% CI)"
  ) +
  theme_minimal(base_size = 14) +
  coord_flip() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.grid.major.y = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )

print(p_supp_forest)

cat("\n>>> 所有图表和表格生成完毕！\n请在 RStudio 的 Plots 窗口中使用 'Previous Plot' 箭头查看所有内容。\n")



cat("\n================ [交互作用 P值] ================\n")
print(tab_inter_who)  # 查看 P Value 栏

cat("\n================ [交互作用 P值] ================\n")
print(tab_inter_asian)  # 查看 P Value 栏

cat("\n================ [WHO 标准分层 OR 值] ================\n")
print(tab_strat_who)    # 用于对比验证 

cat("\n================ [亚洲标准分层 OR 值] ================\n")
print(tab_strat_asian)  # 查看 OR (95% CI) 和 P Value

cat("\n================ [备用 Slide: WHO 标准分层 OR 值] ================\n")
print(tab_strat_who)    # 用于对比验证 


