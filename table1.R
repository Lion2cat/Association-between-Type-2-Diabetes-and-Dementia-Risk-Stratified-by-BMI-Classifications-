library(table1)
library(dplyr)

# 加载数据
load("mini_project_dementia_diabetes.Rdata")
if(exists("d")) { df <- d }

label(df$age)       <- "Age (Years)"
label(df$sex)       <- "Sex"
label(df$BMI)       <- "BMI (kg/m²)"
label(df$education) <- "Education Level"
label(df$smoking)   <- "Smoking Status"
label(df$diabetes)  <- "Diabetes Status"
label(df$dementia)  <- "Dementia (Outcome)"


df_table <- df %>%
  mutate(
    # 结局变量转为因子，显示 Yes/No 而不是 1/0
    dementia = factor(dementia, levels = c(0, 1), labels = c("No", "Yes")),
    
    # 确保性别首字母大写 (美观)
    sex = factor(sex, levels = c("female", "male"), labels = c("Female", "Male")),
    
    # 确保吸烟状态首字母大写
    smoking = factor(smoking, levels = c("no smoking", "current smoking"), 
                     labels = c("No smoking", "Current smoking")),
    
    # 确保教育程度显示清晰
    education = factor(education, levels = c("low education", "high education"), 
                       labels = c("Low Education", "High Education"))
  )

table1(~ age + sex + BMI + education + smoking + diabetes | dementia, 
       data = df_table,
       overall = c(left = "Total Population"), # 加上总人群列
       caption = "Table 1: Baseline Characteristics of the Study Population (N=400,000)")

