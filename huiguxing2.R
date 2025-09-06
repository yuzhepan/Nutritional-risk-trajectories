################### 1.加载包 ###################
library(ggplot2)
library(dplyr)
library(readr)
library(Cairo)
library(showtext)
library(purrr)
library(broom)
library(tidyr)
library(ggmap)
library(maps)
library(readr)
library(readxl)
library(countrycode)
library(ggsci)
library(factoextra)
library(magrittr)
library(data.table)
library(apc)
library(readxl)
library(Hmisc)
library(patchwork)
library(forcats)
library(MASS)
library(lme4)
library(car)
library(glmnet)
library(RColorBrewer)
library(lmerTest)
library(performance)
library(lmtest) 
library(broom)
library(stringr)
library(rjson)
library(jsonlite)
library(geojsonsf)
library(sf)
library(showtext)
library(stringr)
library(purrr)
library(lubridate)
library(mice)
library(survival)
library(rms)
library(mfp)
library(mstate)
library(msm)
library(splines)
library(brms)
library(cmdstanr)
library(bayesplot)
library(rlang)
library(lcmm)
library(brms)
library(loo)
library(posterior)
library(bayesplot)
library(dplyr)
library(ggplot2)
library(stringr)
library(survminer)

library(conflicted)
conflict_prefer("select", "dplyr")  # 明确指定选择 dplyr 中的 `select` 函数
conflict_prefer("filter", "dplyr")  # 明确指定选择 dplyr 中的 `filter` 函数 
conflicts_prefer(dplyr::first)
conflict_prefer("ar", "brms") 
conflicts_prefer(patchwork::area)  # 如果你需要 patchwork 的布局函数
conflicts_prefer(posterior::rhat)
conflicted::conflicts_prefer(posterior::sd)
conflicted::conflicts_prefer(purrr::map)
conflicted::conflicts_prefer(lubridate::year)

############ 一、聚焦 ###################
############ 保存结果
save.image("贝叶斯诊断结果.RData")
####### 加载模型
load("贝叶斯诊断结果.RData")

######  1.1 放弃层级先验，转而在营养风险模块实施分析 ########
#  去除两个大模型
rm(fit_linear_prior, fit_spline_profession_time)

######对营养风险模块数据进行MCMC
# 设置路径
set_cmdstan_path("F:/R/cmdstan/cmdstan-2.36.0") 
###### 编译
###### rebuild_cmdstan()
# 检查
cmdstanr::cmdstan_path()
cmdstanr::cmdstan_version()

## 提取其他量表
unique(data_huli_combined$护理记录类型)
# 选择营养的量表
data_liangbiao_1 <- data_huli_combined %>%
  filter(护理记录类型 %in% c("营养风险筛查评估表", "NEW营养风险筛查评估表", 
                       "普通患者营养评定表（SGA）", "营养风险筛查表(成人)", 
                       "老年患者营养评定表(MNA)"))
unique(data_liangbiao_1$护理记录类型)
# 确认总评分为numeric
data_liangbiao_1$总评分 <- as.numeric(data_liangbiao_1$总评分)
# 剔除第2，4，8，9列
data_liangbiao_1 <- data_liangbiao_1[,-c(2,4,8,9,12)]
# 重命名
colnames(data_liangbiao_1)[2] <- "就诊编号"

#### 1.1.1 营养风险筛查评估表 #####
data_pinggu1 <- data_liangbiao_1 %>%
  filter(护理记录类型 == "营养风险筛查评估表")
# 确认有多少项目名称
length(unique(data_pinggu1$项目名称)) # 有 40个项目名称
unique(data_pinggu1$项目名称) # 显示所有项目名称
# 转置数据框
pivot_pinggu1 <- data_pinggu1 %>%
  group_by(患者编号, 就诊编号,护理记录类型,记录日期) %>%
  summarise(平均总分 = mean(总评分, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = 护理记录类型, values_from = 平均总分)
# 确认测量结果列为字符形式
data_pinggu1$测量结果 <- as.character(data_pinggu1$测量结果)
# 确认项目名称NA的个数
sum(is.na(data_pinggu1$项目名称)) # 53
# 去除项目名称为NA的行
data_pinggu1 <- data_pinggu1 %>%
  filter(!is.na(项目名称))
# 保留项目名称为体重(kg),身高(M),身高指数,体质指数(BMI)的行
data_pinggu1 <- data_pinggu1 %>%
  filter(项目名称 %in% c("体重(Kg)", "身高(M)", "身高指数", "体质指数(BMI)"))
# 去除第3列和第5列
data_pinggu1 <- data_pinggu1[,-c(3,5)]
# 转置项目名称的数据框
pivot_pinggu2 <- data_pinggu1 %>%
  group_by(患者编号, 就诊编号, 记录日期, 项目名称) %>%
  slice(1) %>%  # 保留每组中的第一条记录
  ungroup() %>%
  pivot_wider(names_from = 项目名称, values_from = 测量结果)
# 合并，这样我们就得到了营养风险筛查评估表的数据
data_pinggu_combined_1 <- merge(pivot_pinggu1, pivot_pinggu2, by = c("患者编号", "就诊编号", "记录日期"), all = TRUE)
# 检查患者数
length(unique(data_pinggu_combined_1$患者编号)) # 共有 388 个患者
# 检查缺失值
colSums(is.na(data_pinggu_combined_1)) #少许缺失

# 将记录日期转为Date形式
data_pinggu_combined_1$记录日期 <- as.Date(data_pinggu_combined_1$记录日期)

# 重命名第4，5，6，7，8列列名为总分、BMI、体重、身高、身高指数
colnames(data_pinggu_combined_1)[4:8] <- c("总分", "BMI", "体重", "身高", "身高指数")

# 确认各列为numeric（关键修复：确保数据类型正确）
data_pinggu_combined_1 <- data_pinggu_combined_1 %>%
  mutate(
    BMI = as.numeric(BMI),
    体重 = as.numeric(体重),
    身高 = as.numeric(身高),
    身高指数 = as.numeric(身高指数)
  )

# 若某患者BMI、体重、身高、身高指数存在缺失，或者值大于100或者为0，则用相同患者编号且记录日期最近的相应列的值填进去
# 对于BMI、体重、身高、身高指数缺失或异常值的处理
# 将BMI、体重、身高、身高指数中的异常值（>100 或 =0）设为NA
data_pinggu_combined_1 <- data_pinggu_combined_1 %>%
  mutate(
    BMI = ifelse( BMI == 0, NA, BMI),
    体重 = ifelse(体重 == 0, NA, 体重),
    身高 = ifelse(身高 == 0, NA, 身高),
    身高指数 = ifelse(身高指数 == 0, NA, 身高指数)
  )
data_pinggu_combined_1 <- data_pinggu_combined_1 %>%
  arrange(患者编号, desc(记录日期)) %>%  # 按患者编号分组，并按日期降序
  group_by(患者编号)  # 分组以便填充
# 对BMI、体重、身高、身高指数进行填充（优先用最近的记录）
data_pinggu_combined_1 <- data_pinggu_combined_1 %>%
  fill(BMI, 体重, 身高, 身高指数, .direction = "downup") %>%  # 先向下填充，再向上填充
  ungroup()  # 取消分组
# 去除BMI为NA的行
data_pinggu_combined_1 <- data_pinggu_combined_1 %>%
  filter(!is.na(BMI))

# 若身高的值大于100，则对该值取首数字后加小数点
data_pinggu_combined_1 <- data_pinggu_combined_1 %>%
  mutate(
    身高 = ifelse(身高 > 100, 
                as.numeric(paste0(substr(as.character(身高), 1, 1), ".", substr(as.character(身高), 2, nchar(as.character(身高))))),
                身高)
  )
# 若身高指数大于1000，则对该值取首数字后加小数点
data_pinggu_combined_1 <- data_pinggu_combined_1 %>%
  mutate(
    身高指数 = ifelse(身高指数 > 1000,
                  as.numeric(paste0(substr(as.character(身高指数), 1, 1), ".", substr(as.character(身高指数), 2, nchar(as.character(身高指数))))),
                  身高指数)
  )
# 检查处理后的数据情况
cat("最终数据处理结果:\n")
cat("总记录数:", nrow(data_pinggu_combined_1), "\n")
cat("缺失值情况:\n")
print(colSums(is.na(data_pinggu_combined_1)))
# 检查各列的基本统计信息
cat("\n各列基本统计信息:\n")
print(summary(data_pinggu_combined_1[, c("BMI", "体重", "身高", "身高指数")]))

#### 与人口学变量合并
# 转化为数值类型
data_pinggu_combined_1 <- data_pinggu_combined_1 %>%
  mutate(
    总分 = as.numeric(as.character(总分)))

# 处理数据
data_pinggu_combined_1 <- data_geren2 %>%
  left_join(data_geren1, by = "患者编号") %>%
  mutate(
    score = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在营养评估表中的记录
      df <- data_pinggu_combined_1[data_pinggu_combined_1$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次评估
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次评估
      df <- df[which.max(df$记录日期), ]
      return(df$总分)  
    }),
    
    # 对应的记录日期
    score_date = map_chr(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在营养评估表中的记录
      df <- data_pinggu_combined_1[data_pinggu_combined_1$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_character_)
      
      # 找到入院日期之前/最近的一次评估
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_character_)
      
      # 最近一次评估
      df <- df[which.max(df$记录日期), ]
      return(as.character(df$记录日期))  # 转换为字符型保存
    }) %>% as.Date()  # 最后转换回Date类型
  )
summary(data_pinggu_combined_1$score)
# 去除评分为NA的行
data_pinggu_combined_1 <- data_pinggu_combined_1 %>%
  filter(!is.na(score))
# 检测模块的患者数
length(unique(data_pinggu_combined_1$患者编号)) ##有380个患者


#### 1.1.2 NEW营养风险筛查评估表 #####
data_pinggu2 <- data_liangbiao_1 %>%
  filter(护理记录类型 == "NEW营养风险筛查评估表")

# 确认有多少项目名称
length(unique(data_pinggu2$项目名称)) # 查看项目名称数量
unique(data_pinggu2$项目名称) # 显示所有项目名称

# 转置数据框
pivot_pinggu1_new <- data_pinggu2 %>%
  group_by(患者编号, 就诊编号, 护理记录类型, 记录日期) %>%
  summarise(平均总分 = mean(总评分, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = 护理记录类型, values_from = 平均总分)
# 确认测量结果列为字符形式
data_pinggu2$测量结果 <- as.character(data_pinggu2$测量结果)
# 确认项目名称NA的个数
sum(is.na(data_pinggu2$项目名称)) # 查看项目名称为NA的数量
# 去除项目名称为NA的行
data_pinggu2 <- data_pinggu2 %>%
  filter(!is.na(项目名称))

# 保留项目名称为体重(kg),身高(cm),身高指数,体质指数(BMI)的行
data_pinggu2 <- data_pinggu2 %>%
  filter(项目名称 %in% c("体重(Kg)", "身高(cm)", "身高指数", "体质指数(BMI)"))
# 去除第3列和第5列
data_pinggu2 <- data_pinggu2[,-c(3,5)]
# 转置项目名称的数据框
pivot_pinggu2_new <- data_pinggu2 %>%
  group_by(患者编号, 就诊编号, 记录日期, 项目名称) %>%
  slice(1) %>%  # 保留每组中的第一条记录
  ungroup() %>%
  pivot_wider(names_from = 项目名称, values_from = 测量结果)
# 合并
data_pinggu_combined_2 <- merge(pivot_pinggu1_new, pivot_pinggu2_new, by = c("患者编号", "就诊编号", "记录日期"), all = TRUE)

# 检查患者数
length(unique(data_pinggu_combined_2$患者编号)) 
# 检查缺失值
colSums(is.na(data_pinggu_combined_2)) #少许缺失

# 将记录日期转为Date形式
data_pinggu_combined_2$记录日期 <- as.Date(data_pinggu_combined_2$记录日期)
# 重命名第4，5，6，7，8列列名为总分、BMI、体重、身高、身高指数
colnames(data_pinggu_combined_2)[4:8] <- c("总分", "BMI", "体重", "身高", "身高指数")

# 确认各列为numeric（关键修复：确保数据类型正确）
data_pinggu_combined_2 <- data_pinggu_combined_2 %>%
  mutate(
    BMI = as.numeric(BMI),
    体重 = as.numeric(体重),
    身高 = as.numeric(身高),
    身高指数 = as.numeric(身高指数)
  )

# 若某患者BMI、体重、身高、身高指数存在缺失，或者值大于100或者为0，则用相同患者编号且记录日期最近的相应列的值填进去
# 对于BMI、体重、身高、身高指数缺失或异常值的处理
# 将BMI、体重、身高、身高指数中的异常值（>100 或 =0）设为NA
data_pinggu_combined_2 <- data_pinggu_combined_2 %>%
  mutate(
    BMI = ifelse(BMI == 0, NA, BMI),
    体重 = ifelse(体重 == 0, NA, 体重),
    身高 = ifelse(身高 == 0, NA, 身高),
    身高指数 = ifelse(身高指数 == 0, NA, 身高指数)
  )
data_pinggu_combined_2 <- data_pinggu_combined_2 %>%
  arrange(患者编号, desc(记录日期)) %>%  # 按患者编号分组，并按日期降序
  group_by(患者编号)  # 分组以便填充
# 对BMI、体重、身高、身高指数进行填充（优先用最近的记录）
data_pinggu_combined_2 <- data_pinggu_combined_2 %>%
  fill(BMI, 体重, 身高, 身高指数, .direction = "downup") %>%  # 先向下填充，再向上填充
  ungroup()  # 取消分组

# 去除BMI为NA的行
data_pinggu_combined_2 <- data_pinggu_combined_2 %>%
  filter(!is.na(BMI))
# 若身高的值大于100，则对该值取首数字后加小数点
data_pinggu_combined_2 <- data_pinggu_combined_2 %>%
  mutate(
    身高 = ifelse(身高 > 100, 
                as.numeric(paste0(substr(as.character(身高), 1, 1), ".", substr(as.character(身高), 2, nchar(as.character(身高))))),
                身高)
  )
# 若身高指数大于1000，则对该值取首数字后加小数点
data_pinggu_combined_2 <- data_pinggu_combined_2 %>%
  mutate(
    身高指数 = ifelse(身高指数 > 1000,
                  as.numeric(paste0(substr(as.character(身高指数), 1, 1), ".", substr(as.character(身高指数), 2, nchar(as.character(身高指数))))),
                  身高指数)
  )

# 检查处理后的数据情况
cat("NEW营养风险筛查评估表处理结果:\n")
cat("总记录数:", nrow(data_pinggu_combined_2), "\n")
cat("缺失值情况:\n")
print(colSums(is.na(data_pinggu_combined_2)))

#### 合并人口学变量
data_pinggu_combined_2 <- data_pinggu_combined_2 %>%
  mutate(
    总分 = as.numeric(as.character(总分)))

# 处理数据
data_pinggu_combined_2 <- data_geren2 %>%
  left_join(data_geren1, by = "患者编号") %>%
  mutate(
    score = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在NEW营养风险筛查评估表中的记录
      df <- data_pinggu_combined_2[data_pinggu_combined_2$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次评估
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次评估
      df <- df[which.max(df$记录日期), ]
      return(df$总分)  
    }),
    
    # 对应的记录日期
    score_date = map_chr(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在NEW营养风险筛查评估表中的记录
      df <- data_pinggu_combined_2[data_pinggu_combined_2$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_character_)
      
      # 找到入院日期之前/最近的一次评估
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_character_)
      
      # 最近一次评估
      df <- df[which.max(df$记录日期), ]
      return(as.character(df$记录日期))  # 转换为字符型保存
    }) %>% as.Date()  # 最后转换回Date类型
  )
summary(data_pinggu_combined_2$score)
# 去除评分为NA的行
data_pinggu_combined_2 <- data_pinggu_combined_2 %>%
  filter(!is.na(score))
# 检测模块的患者数
length(unique(data_pinggu_combined_2$患者编号))  ## 1170个患者

####### 1.1.3 普通患者营养评定表（SGA） #######
data_pinggu3 <- data_liangbiao_1 %>%
  filter(护理记录类型 == "普通患者营养评定表（SGA）")
# 确认有多少项目名称
length(unique(data_pinggu3$项目名称)) # 查看项目名称数量
unique(data_pinggu3$项目名称) # 显示所有项目名称
# 转置数据框
pivot_pinggu1_sga <- data_pinggu3 %>%
  group_by(患者编号, 就诊编号, 护理记录类型, 记录日期) %>%
  summarise(平均总分 = mean(总评分, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = 护理记录类型, values_from = 平均总分)
# 合并
data_pinggu_combined_3 <- pivot_pinggu1_sga
# 检查患者数
length(unique(data_pinggu_combined_3$患者编号)) 
# 检查缺失值
colSums(is.na(data_pinggu_combined_3)) #少许缺失
# 将记录日期转为Date形式
data_pinggu_combined_3$记录日期 <- as.Date(data_pinggu_combined_3$记录日期)
# 重命名第4列列名为总分
colnames(data_pinggu_combined_3)[4] <- "总分"
# 确认为factor
data_pinggu_combined_3 <- data_pinggu_combined_3 %>%
  mutate(
    总分 = as.factor(总分))
table(data_pinggu_combined_3$总分)

### 与人口学变量合并
# 处理数据
data_pinggu_combined_3 <- data_geren2 %>%
  left_join(data_geren1, by = "患者编号") %>%
  mutate(
    score = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在普通患者营养评定表（SGA）中的记录
      df <- data_pinggu_combined_3[data_pinggu_combined_3$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次评估
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次评估
      df <- df[which.max(df$记录日期), ]
      return(df$总分)  
    }),
    
    # 对应的记录日期
    score_date = map_chr(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在普通患者营养评定表（SGA）中的记录
      df <- data_pinggu_combined_3[data_pinggu_combined_3$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_character_)
      
      # 找到入院日期之前/最近的一次评估
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_character_)
      
      # 最近一次评估
      df <- df[which.max(df$记录日期), ]
      return(as.character(df$记录日期))  # 转换为字符型保存
    }) %>% as.Date()  # 最后转换回Date类型
  )
summary(data_pinggu_combined_3$score)
# 去除评分为NA的行
data_pinggu_combined_3 <- data_pinggu_combined_3 %>%
  filter(!is.na(score))
# 检测模块的患者数
length(unique(data_pinggu_combined_3$患者编号))

####### 1.1.4 营养风险筛查表(成人) #######
data_pinggu4 <- data_liangbiao_1 %>%
  filter(护理记录类型 == "营养风险筛查表(成人)")
# 确认有多少项目名称
length(unique(data_pinggu4$项目名称)) # 查看项目名称数量
unique(data_pinggu4$项目名称) # 显示所有项目名称
# 转置数据框
pivot_pinggu1_adult <- data_pinggu4 %>%
  group_by(患者编号, 就诊编号, 护理记录类型, 记录日期) %>%
  summarise(平均总分 = mean(总评分, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = 护理记录类型, values_from = 平均总分)

# 确认测量结果列为字符形式
data_pinggu4$测量结果 <- as.character(data_pinggu4$测量结果)
# 确认项目名称NA的个数
sum(is.na(data_pinggu4$项目名称)) # 查看项目名称为NA的数量

# 去除项目名称为NA的行
data_pinggu4 <- data_pinggu4 %>%
  filter(!is.na(项目名称))
# 保留项目名称为体重(kg),身高(m),身高指数,体质指数(BMI)的行
data_pinggu4 <- data_pinggu4 %>%
  filter(项目名称 %in% c("体重(Kg)", "身高(m)", "身高指数", "体质指数(BMI)"))

# 去除第3列和第5列
data_pinggu4 <- data_pinggu4[,-c(3,5)]
# 转置项目名称的数据框
pivot_pinggu2_adult <- data_pinggu4 %>%
  group_by(患者编号, 就诊编号, 记录日期, 项目名称) %>%
  slice(1) %>%  # 保留每组中的第一条记录
  ungroup() %>%
  pivot_wider(names_from = 项目名称, values_from = 测量结果)

# 合并
data_pinggu_combined_4 <- merge(pivot_pinggu1_adult, pivot_pinggu2_adult, by = c("患者编号", "就诊编号", "记录日期"), all = TRUE)

# 检查患者数
length(unique(data_pinggu_combined_4$患者编号)) 
# 检查缺失值
colSums(is.na(data_pinggu_combined_4)) #少许缺失
# 将记录日期转为Date形式
data_pinggu_combined_4$记录日期 <- as.Date(data_pinggu_combined_4$记录日期)

# 重命名第4，5，6，7，8列列名为总分、BMI、体重、身高、身高指数
colnames(data_pinggu_combined_4)[4:8] <- c("总分", "体重", "身高", "身高指数","BMI")
# 确认各列为numeric（关键修复：确保数据类型正确）
data_pinggu_combined_4 <- data_pinggu_combined_4 %>%
  mutate(
    BMI = as.numeric(BMI),
    体重 = as.numeric(体重),
    身高 = as.numeric(身高),
    身高指数 = as.numeric(身高指数)
  )

# 若某患者BMI、体重、身高、身高指数存在缺失，或者值大于100或者为0，则用相同患者编号且记录日期最近的相应列的值填进去
# 对于BMI、体重、身高、身高指数缺失或异常值的处理
# 将BMI、体重、身高、身高指数中的异常值（>100 或 =0）设为NA
data_pinggu_combined_4 <- data_pinggu_combined_4 %>%
  mutate(
    BMI = ifelse(BMI == 0, NA, BMI),
    体重 = ifelse(体重 == 0, NA, 体重),
    身高 = ifelse(身高 == 0, NA, 身高),
    身高指数 = ifelse(身高指数 == 0, NA, 身高指数)
  )

data_pinggu_combined_4 <- data_pinggu_combined_4 %>%
  arrange(患者编号, desc(记录日期)) %>%  # 按患者编号分组，并按日期降序
  group_by(患者编号)  # 分组以便填充

# 对BMI、体重、身高、身高指数进行填充（优先用最近的记录）
data_pinggu_combined_4 <- data_pinggu_combined_4 %>%
  fill(BMI, 体重, 身高, 身高指数, .direction = "downup") %>%  # 先向下填充，再向上填充
  ungroup()  # 取消分组

# 去除BMI为NA的行
data_pinggu_combined_4 <- data_pinggu_combined_4 %>%
  filter(!is.na(BMI))

# 若身高的值大于100，则对该值取首数字后加小数点
data_pinggu_combined_4 <- data_pinggu_combined_4 %>%
  mutate(
    身高 = ifelse(身高 > 100, 
                as.numeric(paste0(substr(as.character(身高), 1, 1), ".", substr(as.character(身高), 2, nchar(as.character(身高))))),
                身高)
  )

# 若身高指数大于1000，则对该值取首数字后加小数点
data_pinggu_combined_4 <- data_pinggu_combined_4 %>%
  mutate(
    身高指数 = ifelse(身高指数 > 1000,
                  as.numeric(paste0(substr(as.character(身高指数), 1, 1), ".", substr(as.character(身高指数), 2, nchar(as.character(身高指数))))),
                  身高指数)
  )

# 检查处理后的数据情况
cat("营养风险筛查表(成人)处理结果:\n")
cat("总记录数:", nrow(data_pinggu_combined_4), "\n")
cat("缺失值情况:\n")
print(colSums(is.na(data_pinggu_combined_4)))

#### 合并人口学变量
# 转化为数值类型
data_pinggu_combined_4 <- data_pinggu_combined_4 %>%
  mutate(
    总分 = as.numeric(as.character(总分)))
# 处理数据
data_pinggu_combined_4 <- data_geren2 %>%
  left_join(data_geren1, by = "患者编号") %>%
  mutate(
    score = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在营养风险筛查表(成人)中的记录
      df <- data_pinggu_combined_4[data_pinggu_combined_4$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次评估
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次评估
      df <- df[which.max(df$记录日期), ]
      return(df$总分)  
    }),
    
    # 对应的记录日期
    score_date = map_chr(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在营养风险筛查表(成人)中的记录
      df <- data_pinggu_combined_4[data_pinggu_combined_4$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_character_)
      
      # 找到入院日期之前/最近的一次评估
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_character_)
      
      # 最近一次评估
      df <- df[which.max(df$记录日期), ]
      return(as.character(df$记录日期))  # 转换为字符型保存
    }) %>% as.Date()  # 最后转换回Date类型
  )
summary(data_pinggu_combined_4$score)
# 去除评分为NA的行
data_pinggu_combined_4 <- data_pinggu_combined_4 %>%
  filter(!is.na(score))
# 检测模块的患者数
length(unique(data_pinggu_combined_4$患者编号))


####### 1.1.5 老年患者营养评定表(MNA) #######
data_pinggu5 <- data_liangbiao_1 %>%
  filter(护理记录类型 == "老年患者营养评定表(MNA)")
# 确认有多少项目名称
length(unique(data_pinggu5$项目名称)) # 查看项目名称数量
unique(data_pinggu5$项目名称) # 显示所有项目名称
# 转置数据框
pivot_pinggu1_mna <- data_pinggu5 %>%
  group_by(患者编号, 就诊编号, 护理记录类型, 记录日期) %>%
  summarise(平均总分 = mean(总评分, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = 护理记录类型, values_from = 平均总分)

# 合并
data_pinggu_combined_5 <- pivot_pinggu1_mna
# 检查患者数
length(unique(data_pinggu_combined_5$患者编号)) 
# 检查缺失值
colSums(is.na(data_pinggu_combined_5)) #少许缺失
# 将记录日期转为Date形式
data_pinggu_combined_5$记录日期 <- as.Date(data_pinggu_combined_5$记录日期)
# 重命名第4列列名为总分
colnames(data_pinggu_combined_5)[4] <- "总分"
# 确认各列为numeric（关键修复：确保数据类型正确）
data_pinggu_combined_5 <- data_pinggu_combined_5 %>%
  mutate(
    总分 = as.numeric(总分))
table(data_pinggu_combined_5$总分)

### 合并人口学变量
# 转化为数值类型
data_pinggu_combined_5 <- data_pinggu_combined_5 %>%
  mutate(
    总分 = as.numeric(as.character(总分)))
# 处理数据
data_pinggu_combined_5 <- data_geren2 %>%
  left_join(data_geren1, by = "患者编号") %>%
  mutate(
    score = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在老年患者营养评定表(MNA)中的记录
      df <- data_pinggu_combined_5[data_pinggu_combined_5$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次评估
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次评估
      df <- df[which.max(df$记录日期), ]
      return(df$总分)  
    }),
    
    # 对应的记录日期
    score_date = map_chr(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在老年患者营养评定表(MNA)中的记录
      df <- data_pinggu_combined_5[data_pinggu_combined_5$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_character_)
      
      # 找到入院日期之前/最近的一次评估
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_character_)
      
      # 最近一次评估
      df <- df[which.max(df$记录日期), ]
      return(as.character(df$记录日期))  # 转换为字符型保存
    }) %>% as.Date()  # 最后转换回Date类型
  )
summary(data_pinggu_combined_5$score)
# 去除评分为NA的行
data_pinggu_combined_5 <- data_pinggu_combined_5 %>%
  filter(!is.na(score))
# 检测日常功能模块的患者数
length(unique(data_pinggu_combined_5$患者编号))

#### 统计患者数
# 统计data_pinggu1,data_pinggu2,data_pinggu3,data_pinggu4,data_pinggu5共同的患者数
# 提取每个数据框中的患者编号
patients_1 <- unique(data_pinggu1$患者编号)
patients_2 <- unique(data_pinggu2$患者编号)
patients_3 <- unique(data_pinggu3$患者编号)
patients_4 <- unique(data_pinggu4$患者编号)
patients_5 <- unique(data_pinggu5$患者编号)
# 找出共同的患者编号
common_patients <- intersect(intersect(intersect(intersect(patients_1, patients_2), patients_3), patients_4), patients_5)
# 输出结果
cat("各数据框患者数:\n")
cat("data_pinggu1 (营养风险筛查评估表):", length(patients_1), "人\n")
cat("data_pinggu2 (NEW营养风险筛查评估表):", length(patients_2), "人\n")
cat("data_pinggu3 (普通患者营养评定表SGA):", length(patients_3), "人\n")
cat("data_pinggu4 (营养风险筛查表(成人)):", length(patients_4), "人\n")
cat("data_pinggu5 (老年患者营养评定表MNA):", length(patients_5), "人\n")
cat("\n")
cat("五个数据框共同的患者数:", length(common_patients), "人\n")
# 查看共同患者的编号（可选）
# cat("共同患者编号:\n")
# print(common_patients)

##### 构造测量层
# 有数据data_pinggu_combined_1，data_pinggu_combined_2，data_pinggu_combined_3，data_pinggu_combined_4，data_pinggu_combined_5
table(data_pinggu_combined_1$score)
table(data_pinggu_combined_2$score)
table(data_pinggu_combined_3$score)
table(data_pinggu_combined_4$score)
table(data_pinggu_combined_5$score)

library(tidyverse)
library(reshape2)
# 五个量表的患者编号
patients_list <- list(
  `营养风险筛查评估表` = unique(data_pinggu_combined_1$患者编号),
  `NEW营养风险筛查评估表` = unique(data_pinggu_combined_2$患者编号),
  `普通患者营养评定表SGA` = unique(data_pinggu_combined_3$患者编号),
  `营养风险筛查表(成人)` = unique(data_pinggu_combined_4$患者编号),
  `老年患者营养评定表MNA` = unique(data_pinggu_combined_5$患者编号)
)

# 计算交集矩阵
n <- length(patients_list)
intersect_matrix <- matrix(0, nrow = n, ncol = n, dimnames = list(names(patients_list), names(patients_list)))

for (i in 1:n) {
  for (j in 1:n) {
    intersect_matrix[i, j] <- length(intersect(patients_list[[i]], patients_list[[j]]))
  }
}

# 转为数据框
intersect_df <- reshape2::melt(intersect_matrix)
colnames(intersect_df) <- c("量表1", "量表2", "交集人数")

# 画热力图
p <- ggplot(intersect_df, aes(x = 量表1, y = 量表2, fill = 交集人数)) +
  geom_tile(color = "white") +
  geom_text(aes(label = 交集人数), color = "black", size = 5) +
  scale_fill_gradient(low = "#f0f9e8", high = "#084081") +
  theme_minimal(base_size = 14) +
  labs(title = "五个营养量表患者交集矩阵", fill = "交集人数") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p
ggsave("热图.pdf", plot = p, width = 15, height = 10)

# 另一种更详细的方法：查看两两之间的重复情况
# 1和2之间
common_1_2 <- df1_key %>%
  select(患者编号, score_date) %>%
  inner_join(df2_key %>% select(患者编号, score_date), by = c("患者编号", "score_date"))

# 1和4之间
common_1_4 <- df1_key %>%
  select(患者编号, score_date) %>%
  inner_join(df4_key %>% select(患者编号, score_date))

# 2和4之间
common_2_4 <- df2_key %>%
  select(患者编号, score_date) %>%
  inner_join(df4_key %>% select(患者编号, score_date))

print(paste("data_pinggu_combined_1 和 data_pinggu_combined_2 共有", nrow(common_1_2), "条相同的患者编号和score_date记录"))
print(paste("data_pinggu_combined_1 和 data_pinggu_combined_4 共有", nrow(common_1_4), "条相同的患者编号和score_date记录"))
print(paste("data_pinggu_combined_2 和 data_pinggu_combined_4 共有", nrow(common_2_4), "条相同的患者编号和score_date记录"))

# 查看1和2之间的重复记录详情
common_1_2_details <- data_pinggu_combined_1 %>%
  inner_join(common_1_2, by = c("患者编号", "score_date")) %>%
  mutate(source = "data_pinggu_combined_1") %>%
  bind_rows(
    data_pinggu_combined_2 %>%
      inner_join(common_1_2, by = c("患者编号", "score_date")) %>%
      mutate(source = "data_pinggu_combined_2")
  ) %>%
  arrange(患者编号, score_date)

# 查看前几条记录
head(common_1_2_details)

# 同样处理2和4之间的重复记录
common_2_4_details <- data_pinggu_combined_2 %>%
  inner_join(common_2_4, by = c("患者编号", "score_date")) %>%
  mutate(source = "data_pinggu_combined_2") %>%
  bind_rows(
    data_pinggu_combined_4 %>%
      inner_join(common_2_4, by = c("患者编号", "score_date")) %>%
      mutate(source = "data_pinggu_combined_4")
  ) %>%
  arrange(患者编号, score_date)
 
###### 1.1.6 进行数据前处理 ######
# 创建统一的编码映射表
# 疾病分类编码映射
disease_mapping <- c(
  "多发性骨髓瘤" = 1,
  "非霍奇金淋巴瘤" = 2,
  "骨髓增生异常综合征" = 3,
  "霍奇金淋巴瘤" = 4,
  "急性淋巴细胞白血病" = 5,
  "急性髓系白血病" = 6,
  "急性早幼粒细胞白血病" = 7,
  "慢性淋巴细胞白血病" = 8,
  "慢性髓系白血病" = 9,
  "其他白血病" = 10,
  "其他疾病" = 11,
  "其他良性或未确诊" = 12
)

# 婚姻状况编码映射
marital_mapping <- c(
  "离婚" = 1,
  "其他" = 2,
  "丧偶" = 3,
  "未婚" = 4,
  "已婚" = 5
)

# 职业编码映射
profession_mapping <- c(
  "个体经营者" = 1,
  "工人" = 2,
  "国家公务员" = 3,
  "农民" = 4,
  "其他" = 5,
  "企业管理人员" = 6,
  "退(离)休人员" = 7,
  "无业人员" = 8,
  "现役军人" = 9,
  "学生" = 10,
  "专业技术人员" = 11,
  "自由职业者" = 12
)

# 创建应用统一编码的函数
apply_standard_mapping <- function(data) {
  data <- data %>%
    mutate(
      disease_code = ifelse(!is.na(疾病分类) & 疾病分类 %in% names(disease_mapping),
                            disease_mapping[as.character(疾病分类)],
                            NA_integer_),
      marital_code = ifelse(!is.na(婚姻状况) & 婚姻状况 %in% names(marital_mapping),
                            marital_mapping[as.character(婚姻状况)],
                            NA_integer_),
      profession_code = ifelse(!is.na(职业) & 职业 %in% names(profession_mapping),
                               profession_mapping[as.character(职业)],
                               NA_integer_)
    )
  return(data)
}

# 创建通用数据处理函数
process_medical_data <- function(data) {
  # 将诊断归转情况中的NA都变为"其他"
  data <- data %>%
    mutate(
      诊断归转情况 = ifelse(is.na(诊断归转情况), "其他", 诊断归转情况)
    )
  
  # 添加一列time，每个患者的第一个入院日期的行记为0，下一次入院日期的行的time则用该入院日期的date减去第一次入院日期，以此类推，得到随访天数
  data <- data %>%
    group_by(患者编号) %>%
    mutate(
      first_admission = min(入院日期, na.rm = TRUE),
      time = as.numeric(入院日期 - first_admission)
    ) %>%
    ungroup() %>%
    dplyr::select(-first_admission)
  
  # 调整疾病分类，如果一个患者有多个疾病分类，优先取不是"其他白血病"和"未分类"的那一类
  # 保持原有行数不变，只统一每个患者的疾病分类
  # 首先为每个患者确定优先级最高的疾病分类
  disease_priority <- data %>%
    group_by(患者编号) %>%
    mutate(
      disease_priority = case_when(
        疾病分类 != "其他白血病" & 疾病分类 != "未分类" ~ 3,
        疾病分类 != "其他白血病" & 疾病分类 == "未分类" ~ 2,
        疾病分类 == "其他白血病" & 疾病分类 != "未分类" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    arrange(患者编号, desc(disease_priority)) %>%
    dplyr::slice(1) %>%
    ungroup() %>%
    dplyr::select(患者编号, 疾病分类, disease_priority)
  
  # 将统一后的疾病分类合并回原数据
  data_1 <- data %>%
    dplyr::select(-疾病分类) %>%
    left_join(disease_priority %>% dplyr::select(患者编号, 疾病分类), by = "患者编号")
  
  # 将疾病类型中未分类都改为其他疾病
  data_1 <- data_1 %>%
    mutate(
      疾病分类 = ifelse(疾病分类 == "未分类", "其他疾病", 疾病分类)
    )
  
  # 删除诊断类型列（如果存在）
  if("诊断类型" %in% names(data_1)) {
    data_1 <- data_1 %>%
      dplyr::select(-"诊断类型")
  }
  
  # 对相同行进行去重
  data_1 <- data_1 %>%
    distinct()
  
  # 对诊断归转情况进行去重，同患者编号、同入院日期下如果同时存在"其他"和非"其他"项，保留非"其他"项，若都为其他，则保留为其他
  data_1 <- data_1 %>%
    group_by(患者编号, 入院日期) %>%
    mutate(
      has_non_other = any(诊断归转情况 != "其他", na.rm = TRUE)
    ) %>%
    filter(
      (has_non_other & 诊断归转情况 != "其他") | 
        (!has_non_other & 诊断归转情况 == "其他") |
        is.na(诊断归转情况)
    ) %>%
    dplyr::select(-has_non_other) %>%
    ungroup()
  
  # 继续去重，若同时存在好转和治愈，保留治愈，若同时存在死亡和非死亡项，保留死亡
  data_1 <- data_1 %>%
    group_by(患者编号, 入院日期) %>%
    mutate(
      # 定义优先级：死亡 > 治愈 > 好转 > 未愈 > 其他
      priority = case_when(
        诊断归转情况 == "死亡" ~ 5,
        诊断归转情况 == "治愈" ~ 3,
        诊断归转情况 == "好转" ~ 2,
        诊断归转情况 == "未愈" ~ 4,
        诊断归转情况 == "其他" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    arrange(患者编号, 入院日期, desc(priority)) %>%
    dplyr::slice(1) %>%
    dplyr::select(-priority) %>%
    ungroup()
  
  data_1 <- data_1 %>%
    mutate(
      职业 = ifelse(is.na(职业), "其他", 职业))
  
  return(data_1)
}

# 使用标准编码处理各模块数据
# 营养风险筛查评估表数据
df1 <- process_medical_data(data_pinggu_combined_1)
df1 <- apply_standard_mapping(df1)

df1  <- df1  %>%
  transmute(
    y = score,
    patient = as.character(患者编号),
    time = as.numeric(time),
    age = 年龄,
    sex = ifelse(性别 == "男性", 1, 0),
    disease = disease_code,
    Marital = marital_code,
    Profession = profession_code,
    module = "NRS_old"
  )
# NEW营养风险筛查评估表（锚定量表）
df2 <- process_medical_data(data_pinggu_combined_2)
df2 <- apply_standard_mapping(df2)

df2  <- df2  %>%
  transmute(
    y = score,
    patient = as.character(患者编号),
    time = as.numeric(time),
    age = 年龄,
    sex = ifelse(性别 == "男性", 1, 0),
    disease = disease_code,
    Marital = marital_code,
    Profession = profession_code,
    module = "NRS_new"
  )
# SGA量表
df3 <- process_medical_data(data_pinggu_combined_3)
df3 <- apply_standard_mapping(df3)

df3  <- df3  %>%
  transmute(
    y = score,
    patient = as.character(患者编号),
    time = as.numeric(time),
    age = 年龄,
    sex = ifelse(性别 == "男性", 1, 0),
    disease = disease_code,
    Marital = marital_code,
    Profession = profession_code,
    module = "NRS_SGA"
  )

# 营养风险筛查表(成人)
df4 <- process_medical_data(data_pinggu_combined_4)
df4 <- apply_standard_mapping(df4)

df4  <- df4  %>%
  transmute(
    y = score,
    patient = as.character(患者编号),
    time = as.numeric(time),
    age = 年龄,
    sex = ifelse(性别 == "男性", 1, 0),
    disease = disease_code,
    Marital = marital_code,
    Profession = profession_code,
    module = "NRS_adult"
  )
# 老年量表MNA
df5 <- process_medical_data(data_pinggu_combined_5)
df5 <- apply_standard_mapping(df5)

df5  <- df5  %>%
  transmute(
    y = score,
    patient = as.character(患者编号),
    time = as.numeric(time),
    age = 年龄,
    sex = ifelse(性别 == "男性", 1, 0),
    disease = disease_code,
    Marital = marital_code,
    Profession = profession_code,
    module = "NRS_MNA")
#  由于MNA量表值越大表示营养状况越好


########### 比较相同的行
# 假设 df1, df2, df4 已经定义好了，包含 patient, time, y 列
# 比较 df1 和 df2
common_1_2 <- df1 %>%
  inner_join(df2, by = c("patient", "time","age","sex","disease"), suffix = c("_1", "_2"))

# 计算相同patient和time的行数，以及y值也相同的行数
count_1_2 <- nrow(common_1_2)
same_y_1_2 <- sum(common_1_2$y_1 == common_1_2$y_2, na.rm = TRUE)

print("=== df1 和 df2 比较 ===")
print(paste("相同 patient 和 time 的行数:", count_1_2))
print(paste("其中 y 值也相同的行数:", same_y_1_2))
print(paste("其中 y 值不同的行数:", count_1_2 - same_y_1_2))

# 比较 df2 和 df4
common_2_4 <- df2 %>%
  inner_join(df4, by = c("patient", "time","age","sex","disease"), suffix = c("_2", "_4"))

# 计算相同patient和time的行数，以及y值也相同的行数
count_2_4 <- nrow(common_2_4)
same_y_2_4 <- sum(common_2_4$y_2 == common_2_4$y_4, na.rm = TRUE)

print("=== df2 和 df4 比较 ===")
print(paste("相同 patient 和 time 的行数:", count_2_4))
print(paste("其中 y 值也相同的行数:", same_y_2_4))
print(paste("其中 y 值不同的行数:", count_2_4 - same_y_2_4))

# 比较 df1 和 df4
common_1_4 <- df1 %>%
  inner_join(df4, by = c("patient", "time","age","sex","disease"), suffix = c("_1", "_4"))

# 计算相同patient和time的行数，以及y值也相同的行数
count_1_4 <- nrow(common_1_4)
same_y_1_4 <- sum(common_1_4$y_1 == common_1_4$y_4, na.rm = TRUE)

print("=== df1 和 df4 比较 ===")
print(paste("相同 patient 和 time 的行数:", count_1_4))
print(paste("其中 y 值也相同的行数:", same_y_1_4))
print(paste("其中 y 值不同的行数:", count_1_4 - same_y_1_4))

table(df1$y)
table(df2$y)
table(df4$y)
table(df5$y)

############ 保存结果
save.image("营养分析.RData")
####### 加载模型   
load("营养分析.RData")

####### 1.2 贝叶斯分层模型 ######
# max+min 反向
df5 <- df5 %>% mutate(y = max(y) + min(y) - y)

# 合并（此步仍先不放 SGA ）
meas_long <- bind_rows(df1, df2, df4, df5) %>%
  mutate(
    patient  = factor(patient),
    module   = factor(module, levels = c("NRS_new","NRS_old","NRS_adult","NRS_MNA")),
    time     = as.numeric(time),
    log_time = log1p(time),
    
    # 基线协变量：建议作为“首次入院/首次评估”的值；此处直接用你已整理在行里的基线
    sex        = factor(sex),
    age        = as.numeric(age),
    disease    = factor(disease),
    Marital    = factor(Marital),
    Profession = factor(Profession),
    age_std    = as.numeric(scale(age))
  ) %>%
  mutate(log_time_std = as.numeric(scale(log_time, center = TRUE, scale = TRUE)))

#  查看Profession各分类的数量
table(meas_long$Profession)
#  查看disease各分类的数量
table(meas_long$disease)
#  查看Marital各分类数量
table(meas_long$Marital)
#  查看患者数
length(unique(meas_long$patient))
#  查看观测数
nrow(meas_long)

# 时间样条（你现在用 df=3，就在公式里也用到三列）
B_time <- ns(meas_long$log_time_std, df = 3)
colnames(B_time) <- paste0("ns_time_", 1:ncol(B_time))
meas_long <- bind_cols(meas_long, as.data.frame(B_time))

# === 1) 非线性公式（测量层 + 纵向层） ===
bf_meas <- brms::bf(
  # 观测层：量表校准（b_m = exp(logb_m) > 0）
  y ~ alpha + exp(logb) * theta,
  
  # 量表特异参数：截距/斜率（在 log 尺度）
  alpha ~ 0 + module,
  logb  ~ 0 + module,
  
  # 纵向潜变量：theta(t) = 时间样条 + 基线协变量 + 患者随机截距
  theta ~ ns_time_1 + ns_time_2 + ns_time_3 +
    age_std + sex + disease + Marital + Profession +
    (1 | patient),
  
  # 分布参数：各量表有不同残差方差
  sigma ~ 0 + module,
  
  nl = TRUE
)

# === 2) 先验 ===
priors_joint <- c(
  # 量表校准先验
  set_prior("normal(0, 2)"      , class="b", nlpar="alpha"),
  set_prior("normal(0, 0.5)"    , class="b", nlpar="logb"),
  
  # theta 的固定效应与随机截距（y 已标准化则这些尺度合适；未标准化可放宽）
  set_prior("normal(0, 1)"      , class="b",  nlpar="theta"),
  set_prior("student_t(3,0,0.7)", class="sd", nlpar="theta"),
  
  # sigma 的系数先验（注意：因为写了 sigma ~ 0 + module，要给“系数”设先验）
  set_prior("student_t(3,0,1)"  , class="b", dpar="sigma"),
  
  # Student-t 自由度
  set_prior("gamma(2,0.1)"      , class="nu")
)

# === 3) 先用 get_prior 拿到精确的 coef 名字，再锚定 NEW ===
pri_skel <- brms::get_prior(bf_meas, data = meas_long, family = student())
print(pri_skel[pri_skel$nlpar %in% c("alpha","logb"), c("class","nlpar","coef")])
# 假设打印结果里是 coef="moduleNRS_new"（以你的输出为准！）
priors_anchor <- c(
  set_prior("constant(0)", class="b", nlpar="alpha", coef="moduleNRS_new"),
  set_prior("constant(0)", class="b", nlpar="logb",  coef="moduleNRS_new")  # exp(0)=1
)

# === 4) 拟合 ===
fit_joint <- brm(
  bf_meas, data = meas_long, family = student(),
  prior   = c(priors_joint, priors_anchor),
  chains = 4, iter = 2000, warmup = 1000, cores = 4,
  control = list(adapt_delta = 0.95, max_treedepth = 15),
  backend = "cmdstanr", seed = 2025
)
print(fit_joint)


# 1) 采样诊断
diag <- check_sampling(fit_joint)  # 你的函数
print(diag)  # 目标：rhat_max < 1.01，n_divergent = 0，ESS_tail 提升
# 2) 后验预测检查（按 module 看残差/拟合）
pp_check(fit_joint, type="dens_overlay", ndraws=100)
pp_check(fit_joint, type="error_hist", ndraws=100)
# 3) PSIS-LOO（必要时 moment matching）
loo_joint <- loo(fit_joint, moment_match = TRUE)
print(loo::pareto_k_table(loo_joint))


####### 1.3 更快速的贝叶斯分层模型 ######
######## 尝试做一版更快速的分析
meas_long_tuned <- bind_rows(df1, df2, df4, df5) %>%
  mutate(
    patient  = factor(patient),
    module   = factor(module, levels = c("NRS_new","NRS_old","NRS_adult","NRS_MNA")),
    time     = as.numeric(time),
    log_time = log1p(time),
    
    # 基线协变量：建议作为“首次入院/首次评估”的值；此处直接用你已整理在行里的基线
    sex        = factor(sex),
    age        = as.numeric(age),
    disease    = factor(disease),
    Marital    = factor(Marital),
    Profession = factor(Profession),
    age_std    = as.numeric(scale(age))
  ) %>%
  mutate(log_time_std = as.numeric(scale(log_time, center = TRUE, scale = TRUE)))
# 重新生成样条（先 df=2）
B_time <- ns(meas_long_tuned$log_time_std, df = 2)
colnames(B_time) <- paste0("ns_time_", 1:ncol(B_time))
meas_long_tuned <- bind_cols(meas_long_tuned, as.data.frame(B_time))
#  2) 非线性联合模型（简化版：sigma ~ 1；Gaussian；df=2） 
bf_meas_fast <- brms::bf(
  # 观测层：量表校准（b_m = exp(logb_m) > 0）
  y ~ alpha + exp(logb) * theta,
  
  # 量表特异参数
  alpha ~ 0 + module,
  logb  ~ 0 + module,
  
  # 潜在营养轨迹：时间样条 + 基线协变量 + 患者随机截距
  theta ~ ns_time_1 + ns_time_2 +
    age_std + sex + disease + Marital + Profession +
    (1 | patient),
  # 残差：先用全局单一 sigma（后续再升级）
  sigma ~ 1,
  nl = TRUE
)

# 设置先验
priors_fast <- c(
  # 量表校准先验
  set_prior("normal(0, 2)"      , class="b", nlpar="alpha"),
  set_prior("normal(0, 0.5)"    , class="b", nlpar="logb"),
  
  # theta 固定效应与随机截距
  set_prior("normal(0, 1)"      , class="b",  nlpar="theta"),
  set_prior("student_t(3,0,0.7)", class="sd", nlpar="theta"),
  
  # 全局 sigma 只有截距：给 dpar = "sigma" 的截距设先验
  set_prior("student_t(3,0,1)"  , class="Intercept", dpar="sigma")
)

# 锚定 NEW（你之前的两行保持不变）
priors_anchor <- c(
  set_prior("constant(0)", class="b", nlpar="alpha", coef="moduleNRS_new"),
  set_prior("constant(0)", class="b", nlpar="logb",  coef="moduleNRS_new")
)

fit_fast <- brm(
  bf_meas_fast, data = meas_long_tuned, family = gaussian(),
  prior   = c(priors_fast, priors_anchor),
  chains  = 4, iter = 4000, warmup = 2000,
  cores   = 4, threads = threading(4),
  control = list(adapt_delta = 0.95, max_treedepth = 10),
  backend = "cmdstanr", seed = 2025
)
print(fit_fast )

# 1) 采样诊断
# 需要：posterior, brms
check_sampling <- function(fit) {
  # 抽样 → draws_array
  dr <- tryCatch({
    d <- posterior::as_draws(fit)
    if (!posterior::is_draws_array(d)) posterior::as_draws_array(d) else d
  }, error = function(e) posterior::as_draws_array(brms::as_mcmc.list(fit)))
  
  # 逐参数诊断，再取极值
  summ <- posterior::summarise_draws(dr, "rhat", "ess_bulk", "ess_tail")
  rhat_max     <- suppressWarnings(max(summ$rhat,            na.rm = TRUE))
  ess_bulk_min <- suppressWarnings(min(summ$ess_bulk,        na.rm = TRUE))
  ess_tail_min <- suppressWarnings(min(summ$ess_tail,        na.rm = TRUE))
  
  # NUTS 诊断（兼容 cmdstanr/rstan；拿不到就 NA）
  np <- tryCatch(brms::nuts_params(fit), error = function(e) NULL)
  
  n_div <- if (!is.null(np) && "Parameter" %in% names(np)) {
    sum(np$Parameter == "divergent__" & np$Value == 1, na.rm = TRUE)
  } else NA_integer_
  
  max_treedepth_obs <- if (!is.null(np) && "Parameter" %in% names(np) &&
                           any(np$Parameter == "treedepth__")) {
    suppressWarnings(max(np$Value[np$Parameter == "treedepth__"], na.rm = TRUE))
  } else NA_real_
  
  data.frame(
    model             = deparse(substitute(fit)),
    rhat_max          = rhat_max,
    ess_bulk_min      = ess_bulk_min,
    ess_tail_min      = ess_tail_min,
    n_divergent       = n_div,
    max_treedepth_obs = max_treedepth_obs
  )
}
diag_fast <- check_sampling(fit_fast)  # 你的函数
print(diag_fast)  # 目标：rhat_max < 1.01，n_divergent = 0，ESS_tail 提升

# 2) 后验预测检查（按 module 看残差/拟合）
# 均值的PPC
pp_mean <- pp_check(fit_fast, type = "stat", stat = "mean")
pp_mean <- pp_mean +
  scale_fill_manual(values = c("#bdd7e7")) +
  scale_color_manual(values = c("#08519c")) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    plot.title = element_text(size = 16, hjust = 0.5)
  ) +
  labs(title = "PPC for the mean (observed vs replicated)")
pp_mean

# 方差的PPC
pp_var <- pp_check(fit_fast, type = "stat", stat = "sd")
pp_var <- pp_var +
  scale_fill_manual(values = c("#bdd7e7")) +
  scale_color_manual(values = c("#08519c")) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    plot.title = element_text(size = 16, hjust = 0.5)
  ) +
  labs(title = "PPC for the standard deviation (observed vs replicated)")
pp_var
# 导出PDF
ggsave("pp_mean.pdf", plot = pp_mean, width = 15, height = 10)
ggsave("pp_var.pdf", plot = pp_var, width = 15, height = 10)

# 计算bayes_R2
bayes_R2(fit_fast)

# 3) PSIS-LOO（必要时 moment matching）
loo_fast <- brms::loo(fit_fast, moment_match = FALSE)
print(loo::pareto_k_table(loo_fast))


### 将模型结果导出为excel
library(writexl)  # 如未安装：install.packages("writexl")
library(broom.mixed)
# 提取固定效应
fixed_effects <- tidy(fit_fast, effects = "fixed")
# 提取随机效应
random_effects <- tidy(fit_fast, effects = "ran_pars")
# 创建列表
results_list <- list(
  "Fixed_Effects" = fixed_effects,
  "Random_Effects" = random_effects
)
# 保存到 Excel
write_xlsx(results_list, "fit_fast_tidy_results.xlsx")


######### 2.1 提取每条观测的 θ（潜在营养状态） #########
# 1) 由于 as.numeric(scale(...)) 丢了属性，这里用原始 log_time 的均值/SD 作为“冻结”的标准化参数
mu_log <- mean(meas_long_tuned$log_time)
sd_log <- sd(meas_long_tuned$log_time)

# 2) 复用你训练时的样条 knots / 边界（直接从你已生成的 B_time 抓）
ns_knots <- attr(B_time, "knots")
ns_bks   <- attr(B_time, "Boundary.knots")

# 3) 用于“新时间点”的样条生成器（保持与训练同一标准化、同一 knots）
make_basis <- function(time_days) {
  log_time     <- log1p(time_days)
  log_time_std <- as.numeric((log_time - mu_log) / sd_log)
  B <- ns(log_time_std, knots = ns_knots, Boundary.knots = ns_bks)
  out <- as.data.frame(B)
  names(out) <- paste0("ns_time_", seq_len(ncol(out)))
  cbind(log_time_std = log_time_std, out)
}

# 构造与训练时一致的新数据（直接用现成列）
newdata_obs <- meas_long_tuned %>%
  arrange(patient, time, desc(module == "NRS_new")) %>%
  group_by(patient, time) %>%
  slice(1) %>%           # 每个 patient-time 只留一行
  ungroup() %>%
  select(patient, module, age_std, sex, disease, Marital, Profession,
         time)           # 剩下的基于 time 现算 basis

basis <- make_basis(newdata_obs$time)
newdata_obs <- bind_cols(newdata_obs, basis[, c("log_time_std","ns_time_1","ns_time_2")])

ndraws_theta <- 500  # 可调大/小

theta_draws <- posterior_linpred(
  fit_fast,
  newdata    = newdata_obs,
  nlpar      = "theta",
  re_formula = NULL,    # 保留随机效应
  ndraws     = ndraws_theta,
  transform  = FALSE
)  # 维度：draw × obs

theta_summ <- as_tibble(t(theta_draws)) %>%
  mutate(obs_id = row_number()) %>%
  pivot_longer(-obs_id, names_to = "draw", values_to = "theta_draw") %>%
  group_by(obs_id) %>%
  summarise(
    theta_mean = mean(theta_draw),
    theta_q025 = quantile(theta_draw, 0.025),
    theta_q975 = quantile(theta_draw, 0.975),
    .groups = "drop"
  )

######### 2.2 计算“近期斜率” #########
delta_days <- 7

# t（用现成列）与 t-Δ（需要重算）
newdata_t  <- newdata_obs

newdata_tm <- newdata_obs
newdata_tm$time <- pmax(newdata_tm$time - delta_days, 0)  # 防负值
basis_tm <- make_basis(newdata_tm$time)
newdata_tm$log_time_std <- basis_tm$log_time_std
newdata_tm$ns_time_1    <- basis_tm$ns_time_1
newdata_tm$ns_time_2    <- basis_tm$ns_time_2

theta_t_draw <- posterior_linpred(
  fit_fast, newdata = newdata_t,
  nlpar = "theta", re_formula = NULL, ndraws = ndraws_theta, transform = FALSE
)
theta_tm_draw <- posterior_linpred(
  fit_fast, newdata = newdata_tm,
  nlpar = "theta", re_formula = NULL, ndraws = ndraws_theta, transform = FALSE
)

slope_draw <- (theta_t_draw - theta_tm_draw) / delta_days

slope_summ <- as_tibble(t(slope_draw)) %>%
  mutate(obs_id = row_number()) %>%
  pivot_longer(-obs_id, names_to = "draw", values_to = "slope_draw") %>%
  group_by(obs_id) %>%
  summarise(
    slope_mean = mean(slope_draw),
    slope_q025 = quantile(slope_draw, 0.025),
    slope_q975 = quantile(slope_draw, 0.975),
    .groups = "drop"
  )

######### 2.3 随机截距 #########
# θ 的随机效应（患者层）：返回 [n_patient × n_coef × 4] 的数组
re_pat <- brms::ranef(fit_fast, nlpar = "theta", summary = TRUE)$patient

# 首先检查 re_pat 的结构
print(dim(re_pat))
print(dimnames(re_pat))

# 已有：
# re_pat <- brms::ranef(fit_fast, nlpar = "theta", summary = TRUE)$patient

pat_ids    <- dimnames(re_pat)[[1]]
stat_names <- dimnames(re_pat)[[2]]  # "Estimate","Est.Error","Q2.5","Q97.5"
coef_names <- dimnames(re_pat)[[3]]  # "theta_Intercept"

# 保险起见，做个小匹配（即使名字有变体也能适配）
pick <- function(cands, pool) {
  hit <- intersect(cands, pool)[1]
  if (is.na(hit)) stop("未在 ", paste(pool, collapse = ", "),
                       " 中找到任何匹配：", paste(cands, collapse = "/"))
  hit
}

coef_intercept <- pick(c("theta_Intercept","Intercept","(Intercept)"), coef_names)
stat_mean  <- pick(c("Estimate","Mean","mean"), stat_names)
stat_se    <- pick(c("Est.Error","SE","sd"),   stat_names)
stat_q025  <- pick(c("Q2.5","2.5%","q2.5"),    stat_names)
stat_q975  <- pick(c("Q97.5","97.5%","q97.5"), stat_names)

b0_map <- tibble::tibble(
  patient = pat_ids,
  b0_mean = re_pat[, stat_mean,  coef_intercept],
  b0_sd   = re_pat[, stat_se,    coef_intercept],
  b0_q025 = re_pat[, stat_q025,  coef_intercept],
  b0_q975 = re_pat[, stat_q975,  coef_intercept]
)

# 看看前几行
print(head(b0_map))

######### 2.4 汇总 #########
landmark_features <- newdata_obs %>%
  mutate(obs_id = row_number()) %>%
  left_join(theta_summ, by = "obs_id") %>%
  left_join(slope_summ, by = "obs_id") %>%
  left_join(b0_map,    by = "patient") %>%
  select(obs_id, patient, time, module, age_std, sex, disease, Marital, Profession,
         theta_mean, theta_q025, theta_q975,
         slope_mean, slope_q025, slope_q975,
         b0_mean)

# 写文件
saveRDS(landmark_features, "landmark_features_theta_slope_b0.rds")
# 导入文件
landmark_features <- readRDS("landmark_features_theta_slope_b0.rds")

#######  3.1 为landmark cox做数据准备 #######
# 检查data_geren2 诊断名称列中出现次数为前20的名称，并显示有多少行
top20_diagnoses <- data_geren2 %>%
  count(诊断名称, sort = TRUE) %>%
  head(50)
# 显示结果
print("诊断名称出现次数前30名:")
print(top20_diagnoses)
## 检查这些排名前20的名称中对应诊断归转情况的名称情况
top20_diag_transfer <- data_geren2 %>%
  filter(诊断名称 %in% top20_diagnoses$诊断名称) %>%
  group_by(诊断名称, 诊断归转情况) %>%
  count() %>%
  arrange(desc(n))

print("前30名诊断名称的诊断归转情况:")
print(top20_diag_transfer)

# 更详细的分析：显示每个诊断名称的各种归转情况占比
top20_transfer_percentage <- data_geren2 %>%
  filter(诊断名称 %in% top20_diagnoses$诊断名称) %>%
  group_by(诊断名称, 诊断归转情况) %>%
  count() %>%
  group_by(诊断名称) %>%
  mutate(percentage = round(n / sum(n) * 100, 2)) %>%
  arrange(诊断名称, desc(n))
print("前30名诊断名称的诊断归转情况(含百分比):")
print(top20_transfer_percentage)

# 添加一列time，每个患者的第一个入院日期的行记为0，下一次入院日期的行的time则用该入院日期的date减去第一次入院日期，以此类推，得到随访天数
data_geren_updated <-  data_geren2 %>%
group_by(患者编号) %>%
mutate(first_admission = min(入院日期, na.rm = TRUE), 
       time = as.numeric(入院日期 - first_admission)) %>%
  ungroup() %>%
  dplyr::select(-first_admission)

# 查看死亡的人数
table(data_geren_updated$诊断归转情况)
length(unique(data_geren_updated$患者编号))
length(unique(meas_long$patient))

########## 3.2 处理个人史 #########
# 检查营养数据中血液恶性肿瘤患者的数目
length(unique(meas_long$patient))
table(meas_long$disease)
# 计算各类疾病的患者数量（确保每个患者只计数一次）
cat("=== 各类疾病的唯一患者数量 ===\n")
# 获取每个患者的首要疾病类型（假设每个患者只有一种主要疾病）
if("patient" %in% names(meas_long) && "disease" %in% names(meas_long)) {
  unique_patients_disease <- meas_long %>%
    distinct(patient, .keep_all = TRUE) %>%
    count(disease, name = "patient_count")
  
  print(unique_patients_disease)
  
  # 可视化疾病分布
  cat("\n=== 疾病分布可视化 ===\n")
  library(ggplot2)
  
  p <- ggplot(unique_patients_disease, aes(x = reorder(disease, -patient_count), y = patient_count, fill = disease)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = patient_count), vjust = -0.5, size = 4) +
    labs(title = "血液恶性肿瘤患者疾病分布",
         x = "疾病类型", 
         y = "患者数量") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
  
  print(p)
}

### 利用data_geren3的个人史，提取喝酒和吸烟情况作为基线控制
# 展示第一行个人史和既往史
head(data_geren3$个人史, 49)
head(data_geren3$既往史, 1)
head(data_geren3$现病史, 1)

# 从个人史中提取吸烟信息
# 利用data_geren3的个人史，提取喝酒和吸烟情况作为基线控制
##### 每个患者只用保留一条记录作为基线资料的一部分
# 若个人史为 NA，先转成空字符，避免正则出错
txt <- coalesce(data_geren3$个人史, "")
# —— 辅助函数：提取第一个匹配到的数字为数值型 —— 
get_num <- function(x, pattern) {
  m <- str_match(x, pattern)
  suppressWarnings(as.numeric(m[,2]))
}
# —— 吸烟状态：1=吸烟，0=不吸烟，NA=不明 —— 
smoke_flag <- case_when(
  str_detect(txt, "吸烟史\\(年\\)[:：]\\s*有") ~ 1L,
  str_detect(txt, "吸烟史\\(年\\)[:：]\\s*无") ~ 0L,
  # 出现详细吸烟段也视为吸烟
  str_detect(txt, "吸烟史[:：]") ~ 1L,
  TRUE ~ NA_integer_
)

# —— 饮酒状态：1=饮酒，0=不饮酒，NA=不明 —— 
drink_flag <- case_when(
  str_detect(txt, "饮酒史\\(年\\)[:：]\\s*有") ~ 1L,
  str_detect(txt, "饮酒史\\(年\\)[:：]\\s*无") ~ 0L,
  # 出现详细饮酒段或饮酒时间也视为饮酒
  str_detect(txt, "饮酒史[:：]") | str_detect(txt, "饮酒时间\\(年\\)") ~ 1L,
  TRUE ~ NA_integer_
)

# —— 吸烟平均频率（支/日），只在“吸烟段”里找 —— 
# 形如：吸烟史: 平均（支/日）: 6
smoke_freq_num <- get_num(
  txt,
  "吸烟史[:：][^；。]*?平均（?支/日）?[:：]\\s*([0-9]+\\.?[0-9]*)"
)

# —— 饮酒平均频率（ml/日），形如 “饮酒史: 平均（ml/日）: 200” —— 
drink_freq_num <- get_num(
  txt,
  "饮酒史[:：][^；。]*?平均（?ml/日）?[:：]\\s*([0-9]+\\.?[0-9]*)"
)

# —— 吸烟时间(年) —— 
get_smoke_years <- function(x) {
  # 对每条文本单独处理
  sapply(x, function(xx) {
    seg <- str_match(xx, "吸烟史[:：].*?吸烟史\\(年\\)")[,1]
    if (is.na(seg)) return(NA_real_)
    m <- str_match(seg, "时间\\(年\\)[:：]\\s*([0-9]+\\.?[0-9]*)")
    as.numeric(m[,2])
  })
}
smoke_years <- get_smoke_years(txt)

# —— 饮酒时间（年） —— 
# 形如：饮酒时间(年): 10
drink_years <- get_num(
  txt,
  "饮酒时间\\(年\\)[:：]\\s*([0-9]+\\.?[0-9]*)"
)

# —— 按状态填充：不饮/不吸时，频率与时间强制为0；其余保持数值/NA —— 
data_geren3_updated <- data_geren3 %>%
  mutate(
    吸烟状态 = smoke_flag,
    饮酒状态 = drink_flag,
    吸烟平均频率_支每日 = ifelse(吸烟状态 == 0L, 0, smoke_freq_num),
    吸烟时间_年       = ifelse(吸烟状态 == 0L, 0, smoke_years),
    饮酒平均频率_ml每日 = ifelse(饮酒状态 == 0L, 0, drink_freq_num),
    饮酒时间_年       = ifelse(饮酒状态 == 0L, 0, drink_years)
  )
colnames(data_geren3_updated)
# 查看第258行的数据
cat("第258行个人史数据:\n")
print(data_geren3_updated$个人史[258])

# 对相同患者编号和就诊编号的行进行合并，只保留所需要的列：患者编号、就诊编号、吸烟平均频率_支每日、吸烟时间_年、饮酒平均频率_ml每日、饮酒平均频率_原文、饮酒时间_年
# —— 数值聚合：优先非0；若多个非0取最大；全为0取0；全NA则NA —— 
agg_num <- function(x) {
  xv <- suppressWarnings(as.numeric(x))
  nz <- xv[!is.na(xv) & xv != 0]
  if (length(nz) > 0) return(max(nz))   # 若多个非0取最大值
  z <- xv[!is.na(xv) & xv == 0]
  if (length(z) > 0) return(0)          # 否则如果有0取0
  return(NA_real_)                      # 全NA则NA
}


# —— 只保留需要的列后按“患者编号+就诊编号”聚合 —— 
need_cols <- c("患者编号",
               "吸烟平均频率_支每日", "吸烟时间_年",
               "饮酒平均频率_ml每日", "饮酒时间_年")

data_geren3_updated <- data_geren3_updated %>%
  select(any_of(need_cols)) %>%
  group_by(患者编号) %>%
  summarise(
    吸烟平均频率_支每日 = agg_num(吸烟平均频率_支每日),
    吸烟时间_年       = agg_num(吸烟时间_年),
    饮酒平均频率_ml每日 = agg_num(饮酒平均频率_ml每日),
    饮酒时间_年       = agg_num(饮酒时间_年),
    .groups = "drop"
  )

# 如果你还想补一个“基线二分类状态”（吸烟/饮酒），可基于数值列再派生：
data_geren3_updated <- data_geren3_updated %>%
  mutate(
    吸烟状态 = case_when(
      is.na(吸烟平均频率_支每日) & is.na(吸烟时间_年) ~ NA_integer_,
      TRUE ~ as.integer((coalesce(吸烟平均频率_支每日, 0) > 0) | (coalesce(吸烟时间_年, 0) > 0))
    ),
    饮酒状态 = case_when(
      is.na(饮酒平均频率_ml每日) & is.na(饮酒时间_年) ~ NA_integer_,
      TRUE ~ as.integer(
        (coalesce(饮酒平均频率_ml每日, 0) > 0) |
          (coalesce(饮酒时间_年, 0) > 0)
      )
    )
  )
### 需要人工核对年份是否准确
#  查看患者编号为0019656681和0019642213的行的个人史的数据
data_geren3[data_geren3$患者编号 == "0023135145" , "个人史"] 

# 将144行的第3列值改为20
data_geren3_updated[144, 3] <- 20
# 将175行的第3列值改为20
data_geren3_updated[175, 3] <- 20
# 将443行的第3列值改为0
data_geren3_updated[443, 3] <- 0
# 将461行的第3列值改为10
data_geren3_updated[461, 3] <- 10
# 将714行的第3列值改为20
data_geren3_updated[714, 3] <- 20
# 将数据中第2列到第7列所有NA都转为0
data_geren3_updated[, 2:7][is.na(data_geren3_updated[, 2:7])] <- 0

# 若该行第2列值为0，第3列值不为0，则设第2列值为1
idx_update <- which(data_geren3_updated[,2] == 0 & data_geren3_updated[,3] != 0)
data_geren3_updated[idx_update, 2] <- 1
# 若该行第3列值为0，第2列值不为0，则去除该行
idx_remove <- which(data_geren3_updated[,3] == 0 & data_geren3_updated[,2] != 0)
data_geren3_updated <- data_geren3_updated[-idx_remove, ]


### 确认第4列为numeric
# 首先查看数据结构
str(data_geren3_updated)
# 查看列名
colnames(data_geren3_updated)

# 查看第4列的列名
col4_name <- colnames(data_geren3_updated)[4]
cat("第4列列名:", col4_name, "\n")
# 确认第4列的数据类型
col4_class <- class(data_geren3_updated[[4]])
cat("第4列数据类型:", col4_class, "\n")
# 检查是否为numeric类型
is_numeric <- is.numeric(data_geren3_updated[[4]])
cat("第4列是否为numeric类型:", is_numeric, "\n")
# 计算第4列平均值
col4_mean <- mean(data_geren3_updated[[4]], na.rm = TRUE)
cat("第4列平均值:", col4_mean, "\n")
# 查看第4列的摘要统计
summary(data_geren3_updated[[4]])

# 若该行第4列值为0，第5列不为0，则设改行第4列值为第4列均值
idx_update_col4 <- which(data_geren3_updated[,4] == 0 & data_geren3_updated[,5] != 0)
data_geren3_updated[idx_update_col4, 4] <- 10
# 查看处理结果
cat("第4列值为0且第5列不为0的行数:", length(idx_update_col4), "\n")
## 若该行第2列和第3列都为0，而第6列不为0，则该行第6列值设为0
idx_update_col6 <- which(data_geren3_updated[,2] == 0 & data_geren3_updated[,3] == 0 & data_geren3_updated[,6] != 0)
data_geren3_updated[idx_update_col6, 6] <- 0
# 查看处理结果
cat("第2列和第3列都为0而第6列不为0的行数:", length(idx_update_col6), "\n")

# 重命名列名分别为patient,smoking_frequency,smoking_year,drinking_frequency,drinking_year,smoking,drinking
colnames(data_geren3_updated) <- c("patient", "smoking_frequency", "smoking_year", 
                                   "drinking_frequency", "drinking_year", "smoking", "drinking")

## 统计饮酒和吸烟的人数
table(data_geren3_updated$smoking)  # 吸烟的有542人，不吸烟的有2857人
table(data_geren3_updated$drinking) # 喝酒的有500人，不喝酒的有2899人  
colnames(data_geren3_updated)


########## 3.3 处理现病史 ###########
# 完整显示第1行和第11行的现病史数据
str(data_geren3)
# 使用pull函数函数显示完整文本
cat("第1行现病史：\n")
cat(pull(data_geren3[1, "现病史"]), "\n\n")
cat("第11行现病史：\n")
cat(pull(data_geren3[11, "现病史"]), "\n\n")
cat("第63行现病史：\n")
cat(pull(data_geren3[63, "现病史"]), "\n\n")

# 保留第1，2，3，4，5列
data_geren3_cleaned <- data_geren3[, 1:5]
# 清理数据
data_geren3_cleaned <- data_geren3_cleaned %>%
  filter(!is.na(现病史) & nchar(trimws(现病史)) > 0)
# 确认患者数
length(unique(data_geren3_cleaned$患者编号))  ### 3389个患者

# 进行文本探索
library(tidytext)
library(jiebaR)
library(devtools)

###  医学文本提取，我们是希望得到有价值的结局指标来用作我们的营养研究，这样子更准确，不能依赖于诊断归转情况，我们希望根据文件能得到分类结局
# 同时，应该控制治疗方案，需要先匹配HyperCVADcourseA，hyperCVADcourseB，HyperCVAD-A，CVAD-B，CAM，mM

# 只保留有现病史的记录
text_df <- data_geren3_cleaned %>%
  filter(!is.na(现病史)) %>%
  select(患者编号, 就诊编号, 现病史)
# 完整显示第1行，第10行和第49行的现病史数据
# 使用pull函数函数显示完整文本
cat("第1行现病史：\n")
cat(pull(text_df[1, "现病史"]), "\n\n")
cat("第10行现病史：\n")
cat(pull(text_df[10, "现病史"]), "\n\n")
cat("第49行现病史：\n")
cat(pull(text_df[49, "现病史"]), "\n\n")

#######  3.4 现病史词频特征提取 #########
# 有个问题就是每行的现病史都会包括整个患者之前时间段的请况，也就是说可能匹配到的结局特征或者骨髓抑制特征与这次的入院时间不匹配，需要在这个特征提取的句子往前一直匹配到最近的时间点
# 匹配个入院日期
# 保留患者编号、就诊编号、time和入院日期
data_huanzhe1 <- data_geren_updated %>%
  select(患者编号, 就诊编号,入院日期)
# 按照患者编号和就诊编号合并到data_geren3
text_df <- text_df %>%
  left_join(data_huanzhe1, by = c("患者编号", "就诊编号"))

# —— 你的基础配置（原样保留）——
outcome_specs <- tibble::tribble(
  ~label,       ~priority, ~patterns,
  # 死亡类
  "死亡",          1L,     c("死亡","去世","过世","离世","宣告死亡","死亡时间"),
  # 危重/极危类（若想把“ICU/插管”视为重症事件，也可放事件字典）
  "危重",          2L,     c("病危","危重","极危","生命体征不稳","多器官衰竭","休克"),
  # 疾病进展/加重（PD/Progression）
  "加重进展",      3L,     c("进展","病情进展","疾病进展","加重","病情加重","恶化","病情恶化","转差","恶性进展","PD\\b"),
  # 复发（Recurrence/Relapse）
  "复发",          4L,     c("复发","再发","复燃","复出现","再次出现"),
  # 缓解（Response）
  "缓解",          5L,     c("完全缓解","部分缓解","缓解","CR\\b","PR\\b","缓解期"),
  # 好转/稳定
  "好转稳定",      6L,     c("好转","症状改善","病情改善","稳定","病情平稳","SD\\b","稳定期"),
  # 出院
  "出院",          7L,     c("出院(办理|医嘱)?","已出院","拟出院","居家休养")
)


neg_pat <- "(未见|未予|未行|未|无|不|否认|除外|排除|暂未|暂无)"
unc_pat <- "(考虑|疑似|可能|待查|待排|倾向于|拟)"

snippet <- function(text, start, end, window = 20) {
  s <- max(1, start - window)
  e <- min(nchar(text), end + window)
  substr(text, s, e)
}

# —— 时间锚定：从命中附近 ±span 内寻找最近的日期（或“入院当日/本次/近日”等） —— 
date_rx <- "(20\\d{2}[\\.\\-/年]\\s*\\d{1,2}[\\.\\-/月]\\s*\\d{1,2}日?)|(\\b\\d{1,2}[\\.-/]\\d{1,2}\\b)|(\\d{1,2}月\\d{1,2}日)"
recent_markers_rx <- "(入院当日|当日|本次(?:入院)?|入院后|近日|近[0-9一二三四五六七八九十]{1,2}(?:天|周|月))"

parse_date_token <- function(tok, admit_date) {
  if (is.na(tok) || !nzchar(tok)) return(as.Date(NA))
  tok <- gsub("\\s+", "", tok)
  # 2019-01-14 / 2019.01.14 / 2019/01/14 / 2019年01月14日
  if (grepl("^20\\d{2}", tok)) {
    tok2 <- gsub("[年/月\\.]", "-", tok)
    tok2 <- gsub("日$", "", tok2)
    return(suppressWarnings(lubridate::ymd(tok2)))
  }
  # 09-06 / 9-6
  if (grepl("^\\d{1,2}[\\.-/]\\d{1,2}$", tok) && !is.na(admit_date)) {
    mm <- sub("^([0-9]{1,2}).*$", "\\1", tok)
    dd <- sub("^.*[\\.-/]([0-9]{1,2})$", "\\1", tok)
    return(suppressWarnings(lubridate::ymd(sprintf("%d-%02d-%02d", lubridate::year(admit_date), as.integer(mm), as.integer(dd)))))
  }
  # 9月6日
  if (grepl("月\\d{1,2}日$", tok) && !is.na(admit_date)) {
    mm <- sub("^(\\d{1,2})月.*$", "\\1", tok)
    dd <- sub("^.*月(\\d{1,2})日$", "\\1", tok)
    return(suppressWarnings(lubridate::ymd(sprintf("%d-%02d-%02d", lubridate::year(admit_date), as.integer(mm), as.integer(dd)))))
  }
  as.Date(NA)
}

# 在命中位置附近寻找最近日期；若无具体日期但有“入院当日/本次/近日”等，则用入院日期作为锚定日期
anchor_recent_date <- function(text, hit_start, hit_end, admit_date, span = 80) {
  if (is.na(text) || !nzchar(text)) return(as.Date(NA))
  s <- max(1, hit_start - span); e <- min(nchar(text), hit_end + span)
  ctx <- substr(text, s, e)
  
  # 1) 明确日期
  ds <- stringr::str_match_all(ctx, date_rx)[[1]]
  if (nrow(ds) > 0) {
    # 拿到所有匹配的原文 token
    toks <- ds[,1]
    dts <- sapply(toks, parse_date_token, admit_date = admit_date)
    # 选择 <= 入院日期 的最新日期；若 admit_date 缺失，则选文本里最新日期
    if (!all(is.na(dts))) {
      if (!is.na(admit_date)) {
        cand <- dts[!is.na(dts) & dts <= admit_date]
        if (length(cand)) return(max(cand))
      } 
      return(max(dts, na.rm = TRUE))
    }
  }
  
  # 2) 近时态标记：入院当日/本次/近日/近x天(周/月) 等
  if (!is.na(admit_date) && stringr::str_detect(ctx, recent_markers_rx)) {
    return(as.Date(admit_date))
  }
  
  as.Date(NA)
}

# 旧：detect_label_one <- function(text, patterns, window = 20) {
# 新：增加 admit_date，并返回 anchor_date
detect_label_one <- function(text, patterns, admit_date = as.Date(NA), window = 20) {
  if (is.na(text) || !nzchar(text)) return(list(hit = 0L, evidence = NA_character_, anchor_date = as.Date(NA)))
  rx <- str_c(patterns, collapse = "|")
  locs <- str_locate_all(text, regex(rx))[[1]]
  if (nrow(locs) == 0) return(list(hit = 0L, evidence = NA_character_, anchor_date = as.Date(NA)))
  
  for (k in seq_len(nrow(locs))) {
    st <- locs[k, 1]; ed <- locs[k, 2]
    ctx <- snippet(text, st, ed, window = window)
    has_neg <- str_detect(ctx, regex(neg_pat))
    has_unc <- str_detect(ctx, regex(unc_pat))
    if (has_neg || has_unc) next
    
    # —— 锚定时间到本次入院 —— 
    ad <- anchor_recent_date(text, st, ed, admit_date, span = 80)
    if (!is.na(ad)) {
      return(list(hit = 1L, evidence = ctx, anchor_date = ad))
    }
  }
  list(hit = 0L, evidence = NA_character_, anchor_date = as.Date(NA))
}


# —— 严格复发：复发/再发附近需出现疾病词，且无否定/不确定 —— 
recur_words   <- c("复发","再发","复燃","再次出现")
disease_words <- c("白血病","淋巴瘤","骨髓瘤","肿瘤","癌","病情","疾病","病程","复发期","进展期","恶性")

detect_recur_strict <- function(text, admit_date = as.Date(NA), span = 10) {
  if (is.na(text) || !nzchar(text)) return(0L)
  locs <- stringr::str_locate_all(text, stringr::str_c(recur_words, collapse="|"))[[1]]
  if (nrow(locs) == 0) return(0L)
  for (k in seq_len(nrow(locs))) {
    st <- max(1, locs[k,1]-span); ed <- min(nchar(text), locs[k,2]+span)
    ctx <- substr(text, st, ed)
    if (stringr::str_detect(ctx, regex(neg_pat)) || stringr::str_detect(ctx, regex(unc_pat))) next
    if (!stringr::str_detect(ctx, stringr::str_c(disease_words, collapse="|"))) next
    # —— 必须能锚定到本次入院最近日期 —— 
    ad <- anchor_recent_date(text, st, ed, admit_date, span = 80)
    if (!is.na(ad)) return(1L)
  }
  0L
}

# 定义严格进展检测函数
progress_words <- c("进展","病情进展","疾病进展","加重","病情加重","恶化","病情恶化","转差","恶性进展","PD\\b")
disease_words <- c("白血病","淋巴瘤","骨髓瘤","肿瘤","癌","病情","疾病","病程","复发期","进展期","恶性")

detect_progress_strict <- function(text, admit_date = as.Date(NA), span = 10) {
  if (is.na(text) || !nzchar(text)) return(0L)
  locs <- stringr::str_locate_all(text, stringr::str_c(progress_words, collapse="|"))[[1]]
  if (nrow(locs) == 0) return(0L)
  for (k in seq_len(nrow(locs))) {
    st <- max(1, locs[k,1]-span); ed <- min(nchar(text), locs[k,2]+span)
    ctx <- substr(text, st, ed)
    if (stringr::str_detect(ctx, regex(neg_pat)) || stringr::str_detect(ctx, regex(unc_pat))) next
    if (!stringr::str_detect(ctx, stringr::str_c(disease_words, collapse="|"))) next
    ad <- anchor_recent_date(text, st, ed, admit_date, span = 80)
    if (!is.na(ad)) return(1L)
  }
  0L
}

# 旧：extract_outcomes_row <- function(text, admit_date) {
# 新：收集每个标签的 anchor_date，并输出“结局_日期”=最佳标签的日期
# 修改 extract_outcomes_row 函数，确保日期列的数据类型一致
extract_outcomes_row <- function(text, admit_date) {
  res <- purrr::map2(outcome_specs$patterns, outcome_specs$label, ~{
    detect_label_one(text, .x, admit_date = admit_date)
  })
  hits    <- vapply(res, function(z) z$hit,         integer(1))
  evid    <- vapply(res, function(z) z$evidence,    character(1))
  anchors <- vapply(res, function(z) z$anchor_date, as.Date(NA))  # << 关键
  
  out_bin <- as.list(hits); names(out_bin) <- outcome_specs$label
  
  if (any(hits == 1L)) {
    idx <- which(hits == 1L)
    prios <- outcome_specs$priority[idx]
    best_pos <- idx[which.min(prios)]
    best_label  <- outcome_specs$label[best_pos]
    best_prio   <- outcome_specs$priority[best_pos]
    best_ev     <- evid[best_pos]
    best_anchor <- anchors[best_pos]                # << 关键
  } else {
    best_label <- NA_character_; best_prio <- NA_integer_; best_ev <- NA_character_
    best_anchor <- as.Date(NA)
  }
  
  recur_strict    <- detect_recur_strict(text, admit_date = admit_date)
  progress_strict <- detect_progress_strict(text, admit_date = admit_date)
  
  good_or_discharge <- as.integer( (out_bin[["好转稳定"]] == 1L) | (out_bin[["出院"]] == 1L) )
  pfs_event <- as.integer( recur_strict == 1L |
                             progress_strict == 1L |
                             out_bin[["加重进展"]] == 1L |
                             out_bin[["死亡"]] == 1L )
  
  # 确保日期列始终为 Date 类型
  if (is.null(best_anchor) || (is.numeric(best_anchor) && is.na(best_anchor))) {
    best_anchor <- as.Date(NA)
  } else {
    best_anchor <- as.Date(best_anchor)
  }
  
  c(out_bin,
    list(
      好转或出院   = good_or_discharge,
      PFS_event   = pfs_event,
      结局_最佳标签 = best_label,
      结局_优先级   = best_prio,
      结局_证据     = best_ev,
      结局_日期     = best_anchor,          # << 就这一列
      加重进展_严格 = progress_strict,
      复发_严格     = recur_strict
    ))
}

# —— 向量化到全表（把 入院日期 传入）—— 
outcome_list <- purrr::map2(text_df$现病史, text_df$入院日期, extract_outcomes_row)
# 使用更安全的方式创建 outcome_df
outcome_df <- purrr::map2_dfr(
    text_df$现病史, text_df$入院日期,
    ~ tibble::as_tibble(as.list(extract_outcomes_row(.x, .y))) )

# 得到结局
text_outcomes <- dplyr::bind_cols(
  text_df %>% dplyr::select(患者编号, 就诊编号),
  outcome_df
)
# 保存结局的数据
saveRDS(text_outcomes, "text_outcomes1.rds")

# 统计各列的数值分布
table(text_outcomes$死亡)
table(text_outcomes$危重)
table(text_outcomes$加重进展)
table(text_outcomes$复发)
table(text_outcomes$缓解)
table(text_outcomes$好转稳定)
table(text_outcomes$复发_严格)
table(text_outcomes$加重进展_严格)

# —— 宽容版骨髓抑制抽取：命中关键词即记事件；能抓分级则填分级 —— 
# 骨髓抑制的分级程度由接在骨髓抑制后面的句子或者骨髓抑制伴随的句子决定，“白细胞及血小板减少”为3级，存在“重度骨髓抑制期”、“粒细胞缺乏”的词则为4级，“IV度骨髓抑制”等也为4级
# 时间只定位到“骨髓抑制”后向前抓取，可能有如下形式：8/4/2016（指2016-04-08）;如26/9，指（20xx-09-26）,年份则再向前抓取，匹配第一个如“2016”作为年份）；
# 2015-08-02；2016/9/28；
# 如一个现病史里同时出现两种骨髓移植的匹配，则分别把两种情况都进行记录
# 不用记录骨髓严重程度（BMS_severe）

library(stringr)
library(purrr)
library(dplyr)
library(tibble)
library(lubridate)

# 规则
bms_rx <- "(重度骨髓抑制|严重骨髓抑制|骨髓抑制期|骨髓抑制)"
bms_grade_rx <- "(IV|III|II|I|Ⅳ|Ⅲ|Ⅱ|Ⅰ|[1-4])\\s*度"
kw_grade4 <- "(重度骨髓抑制|粒细胞缺乏|严重骨髓抑制)"
kw_grade3 <- "白细胞(及|和|、)?\\s*血小板减少"
neg_pat <- "(无|否认|未见|未发生|不考虑|预防重度|预防).{0,10}骨髓抑制"
unc_pat <- "(待查|考虑|可能|倾向|疑.*骨髓抑制)"

# 工具函数 
# 两位年份→四位（不使用入院年；<=30 → 2000+yy，否则 1900+yy）
resolve_year <- function(y_tok) {
  y_tok <- suppressWarnings(as.integer(y_tok))
  if (is.na(y_tok)) return(NA_integer_)
  if (y_tok >= 100) return(y_tok)
  if (y_tok <= 30) 2000 + y_tok else 1900 + y_tok
}

# 给药上下文过滤：更窄，只拦“D日程/第N天/周期”这类 
is_dose_like <- function(text, s, e, win = 8) {
  n <- nchar(text)
  pre  <- if (s > 1) substr(text, max(1, s - win), s - 1) else ""
  post <- if (e < n) substr(text, e + 1, min(n, e + win)) else ""
  near <- paste0(pre, "|", post)
  
  # 仅保留“D日程 / 第N天 / 周期N”等硬信号
  rx_cues <- paste0(
    "(?i)",
    paste(c(
      "\\b[Dd]\\s*\\d{1,2}\\s*([\\-–~到至]\\s*\\d{1,2})?\\b", # D1-7 / d1~7
      "第\\s*\\d{1,2}\\s*(天|日)",                           # 第3天
      "第\\s*\\d{1,2}\\s*周期"                               # 第2周期
    ), collapse="|")
  )
  
  # 窗口内出现硬信号则认为是给药/疗程片段
  if (grepl(rx_cues, near, perl = TRUE)) return(TRUE)
  # 紧挨在候选前面就是 D/d，也强拦
  if (grepl("(?i)[Dd]\\s*$", pre, perl = TRUE)) return(TRUE)
  FALSE
}

# 日期解析（不依赖入院年）
# 支持：YYYY-MM-DD / YYYY/MM/DD / YYYY年M月D日 / 22年6月23日
#      d/m/YYYY；无年：d/m、m-d、m.d、M月D日/号
extract_date_before <- function(text, stop_pos) {
  if (!nzchar(text) || is.na(stop_pos) || stop_pos <= 1) {
    return(list(date = as.Date(NA), start = NA_integer_, end = NA_integer_,
                raw = NA_character_, y = NA_integer_, m = NA_integer_, d = NA_integer_,
                year_source = NA_character_, distance = NA_real_, type = NA_character_))
  }
  left_txt <- substr(text, 1, stop_pos - 1)
  
  # 年份 token（含两位，要求后接“年”）
  year_token_pat <- "(?:19\\d{2}|20\\d{2})|(?:\\d{2}(?=年))"
  year_locs <- stringr::str_locate_all(left_txt, year_token_pat)[[1]]
  years_vec <- if (nrow(year_locs) > 0) substring(left_txt, year_locs[,1], year_locs[,2]) else character(0)
  
  candidates <- list()
  add_cand <- function(y, m, d, start, end, raw, year_source, type) {
    y_i <- suppressWarnings(as.integer(y))
    m_i <- suppressWarnings(as.integer(m))
    d_i <- suppressWarnings(as.integer(d))
    if (!is.na(m_i) && (m_i < 1 || m_i > 12)) return(invisible(NULL))
    if (!is.na(d_i) && (d_i < 1 || d_i > 31)) return(invisible(NULL))
    y_i <- resolve_year(y_i)
    dt  <- suppressWarnings(lubridate::make_date(year = y_i, month = m_i, day = d_i))
    dist <- stop_pos - end; if (dist < 0) dist <- 0
    candidates[[length(candidates) + 1L]] <<- list(
      date = if (!is.na(dt)) dt else as.Date(NA),
      start = start, end = end, raw = raw,
      y = y_i, m = m_i, d = d_i,
      year_source = year_source, distance = dist, type = type
    )
  }
  
  # ① 年在前：YYYY-MM-DD / YYYY/MM/DD / YYYY年M月D日
  ymd_pat <- "(?<y>19\\d{2}|20\\d{2})[年/\\-.](?<m>\\d{1,2})[月/\\-.](?<d>\\d{1,2})(?:日)?"
  ymd_m <- stringr::str_match_all(left_txt, ymd_pat)[[1]]
  if (nrow(ymd_m) > 0) {
    spans <- stringr::str_locate_all(left_txt, ymd_pat)[[1]]
    raws  <- substring(left_txt, spans[,1], spans[,2])
    for (i in seq_len(nrow(ymd_m))) {
      add_cand(ymd_m[i,"y"], ymd_m[i,"m"], ymd_m[i,"d"],
               spans[i,1], spans[i,2], raws[i], "explicit_ymd", "ymd")
    }
  }
  
  # ② 两位年中文：YY年M月D日
  ymd_cn2_pat <- "(?<y>\\d{2})年\\s*(?<m>\\d{1,2})月\\s*(?<d>\\d{1,2})(?:日)?"
  ymd_cn2_m <- stringr::str_match_all(left_txt, ymd_cn2_pat)[[1]]
  if (nrow(ymd_cn2_m) > 0) {
    spans <- stringr::str_locate_all(left_txt, ymd_cn2_pat)[[1]]
    raws  <- substring(left_txt, spans[,1], spans[,2])
    for (i in seq_len(nrow(ymd_cn2_m))) {
      add_cand(ymd_cn2_m[i,"y"], ymd_cn2_m[i,"m"], ymd_cn2_m[i,"d"],
               spans[i,1], spans[i,2], raws[i], "explicit_ymd_cn2", "ymd_cn2")
    }
  }
  
  # ③ d/m/YYYY（按“日/月/年”）
  dmy_pat <- "(?<d>\\d{1,2})\\s*/\\s*(?<m>\\d{1,2})\\s*/\\s*(?<y>19\\d{2}|20\\d{2})"
  dmy_m <- stringr::str_match_all(left_txt, dmy_pat)[[1]]
  if (nrow(dmy_m) > 0) {
    spans <- stringr::str_locate_all(left_txt, dmy_pat)[[1]]
    raws  <- substring(left_txt, spans[,1], spans[,2])
    for (i in seq_len(nrow(dmy_m))) {
      add_cand(dmy_m[i,"y"], dmy_m[i,"m"], dmy_m[i,"d"],
               spans[i,1], spans[i,2], raws[i], "explicit_dmy", "dmy")
    }
  }
  
  # ④ 无年：d/m（如 28/8）——不再因 mg/方案等屏蔽
  dm_pat <- "(?<d>\\d{1,2})\\s*/\\s*(?<m>\\d{1,2})(?!\\s*/)"
  dm_loc <- stringr::str_locate_all(left_txt, dm_pat)[[1]]
  if (nrow(dm_loc) > 0) {
    raws <- substring(left_txt, dm_loc[,1], dm_loc[,2])
    for (i in seq_len(nrow(dm_loc))) {
      s <- dm_loc[i,1]; e <- dm_loc[i,2]
      if (is_dose_like(left_txt, s, e)) next
      mm <- stringr::str_match(substr(left_txt, s, e), dm_pat)
      d <- mm[,"d"]; m <- mm[,"m"]
      # 向左找最近“..年”的年份；找不到则保留月日，date=NA
      y_pick <- NA_character_; best_gap <- Inf
      if (nrow(year_locs) > 0) {
        for (j in seq_len(nrow(year_locs))) {
          y_end <- year_locs[j,2]
          if (y_end <= e) { gap <- e - y_end; if (gap < best_gap) { best_gap <- gap; y_pick <- years_vec[j] } }
        }
      }
      if (nzchar(y_pick)) {
        add_cand(y_pick, m, d, s, e, raws[i], "nearest_year_left", "dm")
      } else {
        candidates[[length(candidates) + 1L]] <- list(
          date = as.Date(NA), start = s, end = e, raw = raws[i],
          y = NA_integer_, m = as.integer(m), d = as.integer(d),
          year_source = "missing_year", distance = max(0, stop_pos - e), type = "dm"
        )
      }
    }
  }
  
  # ⑤ 无年：中文 “M月D日/号”
  md_cn_pat <- "(?<m>\\d{1,2})\\s*月\\s*(?<d>\\d{1,2})\\s*(?:日|号)?"
  md_cn_loc <- stringr::str_locate_all(left_txt, md_cn_pat)[[1]]
  if (nrow(md_cn_loc) > 0) {
    raws <- substring(left_txt, md_cn_loc[,1], md_cn_loc[,2])
    for (i in seq_len(nrow(md_cn_loc))) {
      s <- md_cn_loc[i,1]; e <- md_cn_loc[i,2]
      if (is_dose_like(left_txt, s, e)) next
      mm <- stringr::str_match(substr(left_txt, s, e), md_cn_pat)
      m <- mm[,"m"]; d <- mm[,"d"]
      
      y_pick <- NA_character_; best_gap <- Inf
      if (nrow(year_locs) > 0) {
        for (j in seq_len(nrow(year_locs))) {
          y_end <- year_locs[j,2]
          if (y_end <= e) { gap <- e - y_end; if (gap < best_gap) { best_gap <- gap; y_pick <- years_vec[j] } }
        }
      }
      if (nzchar(y_pick)) {
        add_cand(y_pick, m, d, s, e, raws[i], "nearest_year_left", "md_cn")
      } else {
        candidates[[length(candidates) + 1L]] <- list(
          date = as.Date(NA), start = s, end = e, raw = raws[i],
          y = NA_integer_, m = as.integer(m), d = as.integer(d),
          year_source = "missing_year", distance = max(0, stop_pos - e), type = "md_cn"
        )
      }
    }
  }
  
  # ⑥ 无年：m-d（如 9-15；避免 YYYY-MM-DD；避免紧邻 D/d）
  md_dash_pat <- "(?<!\\d{4}-)(?<![Dd]\\s?)(?<m>\\d{1,2})\\s*[-–~到至]\\s*(?<d>\\d{1,2})(?!\\s*\\d)"
  md_dash_loc <- stringr::str_locate_all(left_txt, md_dash_pat)[[1]]
  if (nrow(md_dash_loc) > 0) {
    raws <- substring(left_txt, md_dash_loc[,1], md_dash_loc[,2])
    for (i in seq_len(nrow(md_dash_loc))) {
      s <- md_dash_loc[i,1]; e <- md_dash_loc[i,2]
      if (is_dose_like(left_txt, s, e)) next
      mm <- stringr::str_match(substr(left_txt, s, e), md_dash_pat)
      m <- mm[,"m"]; d <- mm[,"d"]
      
      y_pick <- NA_character_; best_gap <- Inf
      if (nrow(year_locs) > 0) {
        for (j in seq_len(nrow(year_locs))) {
          y_end <- year_locs[j,2]
          if (y_end <= e) { gap <- e - y_end; if (gap < best_gap) { best_gap <- gap; y_pick <- years_vec[j] } }
        }
      }
      if (nzchar(y_pick)) {
        add_cand(y_pick, m, d, s, e, raws[i], "nearest_year_left", "md_dash")
      } else {
        candidates[[length(candidates) + 1L]] <- list(
          date = as.Date(NA), start = s, end = e, raw = raws[i],
          y = NA_integer_, m = as.integer(m), d = as.integer(d),
          year_source = "missing_year", distance = max(0, stop_pos - e), type = "md_dash"
        )
      }
    }
  }
  
  # ⑦ 无年：m.d（如 9.21；支持全角点；避免版本号/小数/百分比）
  md_dot_pat <- "(?<!\\d{4}[\\.．·])(?<![0-9A-Za-z])(\\d{1,2})\\s*[\\.．·]\\s*(\\d{1,2})(?!\\s*[\\.．·]\\s*\\d)(?!\\s*[%％])(?![0-9A-Za-z])"
  md_dot_loc <- stringr::str_locate_all(left_txt, md_dot_pat)[[1]]
  if (nrow(md_dot_loc) > 0) {
    raws <- substring(left_txt, md_dot_loc[,1], md_dot_loc[,2])
    for (i in seq_len(nrow(md_dot_loc))) {
      s <- md_dot_loc[i,1]; e <- md_dot_loc[i,2]
      if (is_dose_like(left_txt, s, e)) next
      mm <- stringr::str_match(substr(left_txt, s, e), md_dot_pat)
      m <- suppressWarnings(as.integer(mm[,2]))
      d <- suppressWarnings(as.integer(mm[,3]))
      if (is.na(m) || is.na(d) || m < 1 || m > 12 || d < 1 || d > 31) next
      
      y_pick <- NA_character_; best_gap <- Inf
      if (nrow(year_locs) > 0) {
        for (j in seq_len(nrow(year_locs))) {
          y_end <- year_locs[j,2]
          if (y_end <= e) { gap <- e - y_end; if (gap < best_gap) { best_gap <- gap; y_pick <- years_vec[j] } }
        }
      }
      if (nzchar(y_pick)) {
        add_cand(y_pick, m, d, s, e, raws[i], "nearest_year_left", "md_dot")
      } else {
        candidates[[length(candidates) + 1L]] <- list(
          date = as.Date(NA), start = s, end = e, raw = raws[i],
          y = NA_integer_, m = m, d = d,
          year_source = "missing_year", distance = max(0, stop_pos - e), type = "md_dot"
        )
      }
    }
  }
  
  if (!length(candidates)) {
    return(list(date = as.Date(NA), start = NA_integer_, end = NA_integer_,
                raw = NA_character_, y = NA_integer_, m = NA_integer_, d = NA_integer_,
                year_source = NA_character_, distance = NA_real_, type = NA_character_))
  }
  
  # 选择离命中词最近；同距时无年样式优先（更可能是就近的事件）
  pri <- function(t) if (t %in% c("dm","md_cn","md_dash","md_dot")) 3L else if (t %in% c("dmy","ymd_cn2")) 2L else 1L
  dist_vec <- vapply(candidates, function(z) z$distance, numeric(1))
  pri_vec  <- vapply(candidates, function(z) pri(z$type), integer(1))
  pick <- order(dist_vec, -pri_vec)[1]
  candidates[[pick]]
}

map_degree_token <- function(tok) {
  if (is.na(tok)) return(NA_integer_)
  x <- toupper(tok)
  if (grepl("IV|Ⅳ|4", x)) return(4L)
  if (grepl("III|Ⅲ|3", x)) return(3L)
  if (grepl("II|Ⅱ|2", x)) return(2L)
  if (grepl("I|Ⅰ|1", x))  return(1L)
  NA_integer_
}

# 单条现病史抽取：命中的每一次事件都返回一行
detect_bms_events_one <- function(text, span_left = 80, span_right = 80) {
  if (is.na(text) || !nzchar(text)) return(tibble())
  locs <- stringr::str_locate_all(text, regex(bms_rx, ignore_case = TRUE))[[1]]
  if (nrow(locs) == 0) return(tibble())
  
  out_rows <- vector("list", nrow(locs))
  got <- 0L
  
  for (k in seq_len(nrow(locs))) {
    st <- locs[k,1]; ed <- locs[k,2]
    ctx_st <- max(1, st - span_left); ctx_ed <- min(nchar(text), ed + span_right)
    ctx <- substr(text, ctx_st, ctx_ed)
    
    if (stringr::str_detect(ctx, regex(neg_pat)) || stringr::str_detect(ctx, regex(unc_pat))) next
    
    dres <- extract_date_before(text, st)
    ad   <- dres$date
    d_st <- dres$start; d_raw <- dres$raw
    if (!is.na(d_st)) {
      exp_st <- max(1, d_st - 20L)
      ctx <- substr(text, exp_st, ctx_ed)
    }
    
    g4   <- stringr::str_detect(ctx, regex(kw_grade4))
    g3   <- stringr::str_detect(ctx, regex(kw_grade3))
    g_tok<- stringr::str_match(ctx, bms_grade_rx)[,1]
    g_num<- map_degree_token(g_tok)
    grade <- if (isTRUE(g4)) 4L else if (!is.na(g_num)) g_num else if (isTRUE(g3)) 3L else 2L
    
    got <- got + 1L
    out_rows[[got]] <- tibble::tibble(
      BMS_event    = 1L,
      BMS_grade    = grade,
      BMS_日期      = ad,
      BMS_日期文本   = if (!is.na(d_st)) d_raw else NA_character_,
      BMS_证据      = ctx
    )
  }
  
  if (got == 0L) return(tibble())
  dplyr::bind_rows(out_rows[seq_len(got)])
}

# 只处理“每个患者最后一行且最长现病史” 
# 要求 text_df 至少包含：患者编号, 就诊编号(可选), 现病史
extract_bms_per_patient_last_longest <- function(text_df) {
  stopifnot(all(c("患者编号","现病史") %in% names(text_df)))
  
  df_idxed <- text_df %>%
    mutate(.ord = row_number(),
           .len = nchar(as.character(现病史)))
  
  # 对每个患者：先选“现病史长度最大”的行；若并列，取“原表中更靠后的那行”（最后一行）
  pick_df <- df_idxed %>%
    group_by(患者编号) %>%
    slice_max(.len, n = 1, with_ties = TRUE) %>%
    slice_max(.ord, n = 1) %>%
    ungroup()
  
  # 逐患者仅对这1行做事件抽取
  purrr::pmap_dfr(
    list(pick_df$现病史,
         if ("患者编号" %in% names(pick_df)) pick_df$患者编号 else NA_character_,
         if ("就诊编号" %in% names(pick_df)) pick_df$就诊编号 else NA_character_),
    function(hist, pid, enc) {
      res <- detect_bms_events_one(hist)
      if (nrow(res) == 0) return(tibble())
      res %>%
        mutate(患者编号 = pid,
               就诊编号 = enc) %>%
        relocate(患者编号, 就诊编号, .before = 1)
    }
  )
}

# 使用示例
# bms_df 仅来自“每位患者的最后且最长现病史”那一行；同一行内多次命中会多行返回
# text_df: 需包含 患者编号, 就诊编号(可选), 现病史
bms_df <- extract_bms_per_patient_last_longest(text_df)
# 类型确认（BMS_日期 为 Date）
str(bms_df$BMS_日期)

# 先找寻BMS_日期为NA的行
bms_na_date <- bms_df %>%
  filter(is.na(BMS_日期))
# 先对text_df去重，确保每个患者编号+就诊编号组合唯一
text_df_unique <- text_df %>%
  select(患者编号, 就诊编号, 现病史, 入院日期) %>%
  distinct(患者编号, 就诊编号, .keep_all = TRUE)
# 进行匹配
bms_na_date <- bms_na_date %>%
  left_join(text_df_unique, by = c("患者编号", "就诊编号"))
# 验证行数
cat("匹配后行数:", nrow(bms_na_date), "\n")

# 将你手动修正的日期放进一个向量
#    假设你要修正前 1:35 行（按 bms_na_date 当前排序）
#    建议统一写成 "YYYY-MM-DD"；也可以用 ymd() 自动识别
manual_dates_chr <- c(
  # 在这里依次填入 35 个字符串，如：
  "2016-01-09", "2016-07-07", "2016-08-18", "2015-10-13","2016-01-12","2016-07-01","2016-10-15","2016-10-15","2016-09-18","2016-11-25","2017-01-23",
  "2017-01-30","2017-02-05","2017-03-28","2017-09-27","2019-06-20","2023-06-14","2020-05-30","2020-07-29","2020-07-29","2020-11-07",
  "2020-12-09","2021-02-05","2020-10-19","2020-11-08","2020-11-12","2020-11-15","2021-07-20","2021-07-20","2021-10-31","2022-02-14","2022-02-26",
  "2022-09-14","2023-02-21","2024-01-31"
)
stopifnot(length(manual_dates_chr) >= 35)

# 直接更新前35行的BMS_日期
for(i in 1:35) {
  if(i <= nrow(bms_na_date)) {
    bms_na_date[i, "BMS_日期"] <- ymd(manual_dates_chr[i])
  }
}
cat("已成功更新前35行的BMS_日期\n")
# 验证更新结果
cat("前5行更新后的BMS_日期:\n")
print(bms_na_date[1:5, c("BMS_日期")])

# 将补全好的bms_na_date的BMS_日期送回bms_events_unique
# 假设bms_na_date中保存了与原始bms_df中相同的行标识信息
# 先创建一个只包含需要更新信息的数据框
update_data <- bms_na_date %>%
  select(患者编号, 就诊编号, BMS_日期) %>%
  filter(!is.na(BMS_日期))
# 从原始bms_df中分离出需要更新和不需要更新的部分
bms_df_no_na <- bms_df %>% filter(!is.na(BMS_日期))  # 已有日期的记录
bms_df_na <- bms_df %>% filter(is.na(BMS_日期))      # NA日期的记录
# 对NA日期的记录进行更新
bms_df_na_updated <- bms_df_na %>%
  select(-BMS_日期) %>%  # 移除原有的NA日期列
  left_join(update_data, by = c("患者编号", "就诊编号"))  # 用更新的数据进行匹配
# 合并所有记录
bms_df <- bind_rows(bms_df_no_na, bms_df_na_updated) %>%
  arrange(患者编号, 就诊编号)  # 重新排序
cat("更新完成，当前BMS_日期为NA的记录数:", 
    nrow(bms_df %>% filter(is.na(BMS_日期))), "\n")

length(unique(bms_df$患者编号))
# 保存数据
saveRDS(bms_df, "bms_df.rds")

# 与 text_outcomes 合并
# 说明：会产生“多行/就诊”（每个不同 BMS 日期一行）；未命中的就诊只保留一行，BMS_* 置 0/NA
text_outcomes <- text_outcomes %>%
  left_join(bms_df, by = c("患者编号", "就诊编号")) %>%
  mutate(
    # 是否命中任何 BMS 事件：只要有一个字段不全空就算 1
    BMS_event = dplyr::coalesce(BMS_event,
                                ifelse(!is.na(BMS_grade) | !is.na(BMS_日期), 1L, 0L)),
    # 等级：未命中置 0（命中但文本未写等级也可能为 0）
    BMS_grade = dplyr::coalesce(BMS_grade, 0L),
    # 日期文本/证据：未命中保留 NA（需要空字符串可改为 ""）
    BMS_日期文本 = dplyr::coalesce(BMS_日期文本, NA_character_),
    BMS_证据     = dplyr::coalesce(BMS_证据, NA_character_)
  ) %>%
  # 为便于下游分析，也可按患者/就诊/日期排序
  arrange(患者编号, 就诊编号, BMS_日期)

# 检查骨髓抑制的患者数
table(text_outcomes$BMS_event)
# 进行去重
# 若患者编号、就诊编号相同，其他列的值也相同，则只保留一行
text_outcomes_unique <- text_outcomes %>%
  distinct(患者编号, 就诊编号, .keep_all = TRUE)
# 检查去重效果
cat("去重前记录数:", nrow(text_outcomes), "\n")
cat("去重后记录数:", nrow(text_outcomes_unique), "\n")
# 查看去重后的BMS_event分布
cat("去重后BMS_event分布:\n")
print(table(text_outcomes_unique$BMS_event))
length(unique(text_outcomes_unique$患者编号))


######### 保存结局数据 ###########
# 保存text_outcomes为rds
saveRDS(text_outcomes_unique, "text_outcomes2.rds")
# 导入rds
text_outcomes <- readRDS("text_outcomes2.rds")
################################### 下一步，调整时间，匹配准确，控制治疗方案，分别对危重、疾病加重、死亡、骨髓抑制四种结局进行分析


# 进行时间匹配
# 保留患者编号、就诊编号、time和入院日期
data_huanzhe <- data_geren_updated %>%
  select(患者编号, 就诊编号,诊断归转情况 ,疾病分类,time, 入院日期,出院日期)
# 按照患者编号和就诊编号合并到data_geren3
text_outcomes <- text_outcomes %>%
  left_join(data_huanzhe, by = c("患者编号", "就诊编号"))

######### 3.5 分析提取更多的不良结局 ###########

##### 3.5.1 控制既往史 ######
# 这样能最大程度保证减小基线差异



#### 3.5.2 治疗方案（计算RDI） ####


#### 3.5.3 移植患者的完全缓解率 ####



#### 3.5.4 骨髓抑制患者的完全缓解率 ####




############################################### 

######### 处理一下text_outcomes
# 去除就诊编号
text_outcomes <- text_outcomes %>%
  select(-就诊编号)
# 对数据进行去重，对患者编号相同，且其他行的值也相同的行进行去重
text_outcomes <- text_outcomes %>%
  distinct()
# 查看有多少患者
print(paste("患者总数:", length(unique(text_outcomes$患者编号))))   ### 3389个患者
# 去除time为NA的行
text_outcomes <- text_outcomes %>%
  filter(!is.na(time))
# 重命名第11列为结局
colnames(text_outcomes)[11] <- "结局"

# 查看数据分布
table(text_outcomes$PFS_event)
table(text_outcomes$结局)
table(text_outcomes$BMS_event)
table(text_outcomes$诊断归转情况)
table(text_outcomes$危重)
table(text_outcomes$加重进展)
table(text_outcomes$复发)

# 了解骨髓抑制的生理指标
# 查看text_outcomes，landmark_features，data_platelet_model_clean_1,data_red_model_clean_1,data_white_model_clean_1相同的患者编号数
patients_list <- list(
  `outcomes` = unique(text_outcomes$患者编号),
  `Nutritional trajectory` = unique(landmark_features$patient),
  `Platelet` = unique(data_platelet_model_clean_1$患者编号),
  `Hemoglobin` = unique(data_red_model_clean_1$患者编号),
  `Neutrophils` = unique(data_white_model_clean_1$患者编号)
)

# 计算交集矩阵
n <- length(patients_list)
intersect_matrix <- matrix(0, nrow = n, ncol = n, dimnames = list(names(patients_list), names(patients_list)))

for (i in 1:n) {
  for (j in 1:n) {
    intersect_matrix[i, j] <- length(intersect(patients_list[[i]], patients_list[[j]]))
  }
}

# 转为数据框
intersect_df <- reshape2::melt(intersect_matrix)
colnames(intersect_df) <- c("量表1", "量表2", "交集人数")

# 画热力图
p <- ggplot(intersect_df, aes(x = 量表1, y = 量表2, fill = 交集人数)) +
  geom_tile(color = "white") +
  geom_text(aes(label = 交集人数), color = "black", size = 5) +
  scale_fill_gradient(low = "#f0f9e8", high = "#084081") +
  theme_minimal(base_size = 14) +
  labs(title = "数据框交集矩阵", fill = "交集人数") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p
ggsave("热图.pdf", plot = p, width = 15, height = 10)

######### 3.6 计算随访时间 ##########
# 若诊断归转情况这一列的值为死亡，则将结局列的相应行的值改为死亡
text_outcomes$结局[which(text_outcomes$诊断归转情况 == "死亡")] <- "死亡"

# 计算不同结局的随访时间
# 结局日期减去患者最早的入院时间为结局随访时间
# ========== 1) 索引日 
index_date_df <- text_outcomes %>%
  group_by(患者编号) %>%
  summarise(index_date = min(as.Date(入院日期), na.rm = TRUE), .groups = "drop")

# ========== 2) 最后随访日（容错修复版，适配 landmark_features）
# 先用病历得到结局/入院的最后日期
out_last <- text_outcomes %>%
  group_by(患者编号) %>%
  summarise(
    last_outcome_date = suppressWarnings(max(as.Date(结局_日期), na.rm = TRUE)),
    last_admit_date   = suppressWarnings(max(as.Date(入院日期), na.rm = TRUE)),
    .groups = "drop"
  )

# 若存在 landmark_features：只有随访时间time，需要先求“每个患者最大time”
# 再把它换算成日期： last_nutri_date = index_date + last_nutri_time（单位：天）
if (exists("landmark_features")) {
  nut_last_time <- landmark_features %>%
    group_by(patient) %>%
    summarise(last_nutri_time = suppressWarnings(max(as.numeric(time), na.rm = TRUE)),
              .groups = "drop") %>%
    rename(患者编号 = patient)
  
  # 合并 index_date 后，把 time 转成日期
  out_last <- out_last %>%
    left_join(index_date_df, by = "患者编号") %>%
    left_join(nut_last_time,  by = "患者编号") %>%
    mutate(
      # 若 last_nutri_time 为有限值，转成日期；否则给 NA
      last_nutri_date = ifelse(is.finite(last_nutri_time),
                               as.Date(index_date + last_nutri_time),
                               as.Date(NA))
    )
} else {
  out_last <- out_last %>%
    left_join(index_date_df, by = "患者编号") %>%
    mutate(last_nutri_date = as.Date(NA))
}
# 统一得到“最后已知日期”
last_contact_df <- out_last %>%
  mutate(last_follow = pmax(last_outcome_date, last_admit_date, last_nutri_date, na.rm = TRUE)) %>%
  select(患者编号, last_follow)

# ========== 3) PFS 数据框（基于“结局_日期”）
# 事件：加重进展/复发（含严格）或死亡；事件日=最早的“结局_日期”
p_event_df <- text_outcomes %>%
  mutate(
    is_prog_relapse = if_else(结局 %in% c("加重进展","复发") |
                                加重进展_严格 == 1L | 复发_严格 == 1L, 1L, 0L),
    is_death = if_else(结局 == "死亡", 1L, 0L),
    date_use = as.Date(结局_日期)
  ) %>%
  filter(is_prog_relapse == 1L | is_death == 1L) %>%
  group_by(患者编号) %>%
  summarise(pfs_event_date = suppressWarnings(min(date_use, na.rm = TRUE)), .groups = "drop")

data_PFS <- index_date_df %>%
  left_join(p_event_df,     by = "患者编号") %>%
  left_join(last_contact_df, by = "患者编号") %>%
  mutate(
    pfs_event = if_else(!is.na(pfs_event_date), 1L, 0L),
    pfs_time  = as.numeric(if_else(pfs_event == 1L, pfs_event_date, last_follow) - index_date, units = "days")
  ) %>%
  filter(is.finite(pfs_time) & pfs_time >= 0)
table(data_PFS$pfs_event)

# ========== 4) OS 数据框（按你要求直接用已有的 time 随访时间）
# 假设 text_outcomes 里有每个患者的随访时间列 time（单位=天）。若同一患者多行，取最大 time。
# 事件=是否出现“死亡”
death_df <- text_outcomes %>%
  group_by(患者编号) %>%
  summarise(os_event = as.integer(any(结局 == "死亡", na.rm = TRUE)), .groups = "drop")

# 聚合出每位患者的随访时间 os_time（按你要求直接用 time）；若无 time 列则回退为 (last_follow - index_date)
if ("time" %in% names(text_outcomes)) {
  time_df <- text_outcomes %>%
    group_by(患者编号) %>%
    summarise(os_time = suppressWarnings(max(as.numeric(time), na.rm = TRUE)), .groups = "drop")
} else {
  # 回退方案：按日期计算随访时间
  message("未发现 'time' 列，OS时间回退为 last_follow - index_date（天）")
  time_df <- index_date_df %>%
    left_join(last_contact_df, by = "患者编号") %>%
    mutate(os_time = as.numeric(last_follow - index_date, units = "days")) %>%
    select(患者编号, os_time)
}

data_OS <- death_df %>%
  left_join(time_df, by = "患者编号") %>%
  filter(is.finite(os_time) & os_time >= 0)

table(data_OS$os_event)

# ========== 5) 基线营养对齐（索引±7天内最近一次） 
# 基线营养匹配（索引±7天内最接近 time=0 的一条）
# landmark_features 列：patient, time, theta_mean(营养风险), 
#                       age_std, sex, disease, Marital, Profession,
#                       b0_mean(截距), slope_mean(斜率),
#                       theta_q025, theta_q975 ...
# 使用 theta_mean(time=0) 作为基线营养风险
nutri_base_df <- landmark_features %>%
  rename(患者编号 = patient) %>%
  filter(time == 0) %>%
  transmute(
    患者编号,
    nutri_baseline = theta_mean,   # 主分析用这个
    nutri_intercept = b0_mean,     # 敏感性分析备用
    nutri_slope     = slope_mean,
    nutri_l95       = theta_q025,
    nutri_u95       = theta_q975,
    age_std, sex, disease, Marital, Profession
  )

data_OS  <- data_OS  %>% left_join(nutri_base_df, by = "患者编号")
data_PFS <- data_PFS %>% left_join(nutri_base_df, by = "患者编号")
# 查看disease的分布
table(nutri_base_df$disease)

# 把 12 类疾病映射为 5 大类 + 血液/非血液标签
# 现有 data_OS / data_PFS 已含: 患者编号, os_time, os_event, pfs_time, pfs_event,nutri_baseline, age_std, sex, disease(因子，水平是 1..12)
# 数值型 disease -> 5 大类
map_to_5cat <- function(x_num) {
  case_when(
    x_num %in% c(5,6,7,8,9,10) ~ "白血病",        # 含 ALL/AML/APL/CLL/CML/其他白血病/MDS
    x_num %in% c(1)              ~ "多发性骨髓瘤",
    x_num %in% c(2,4)            ~ "淋巴瘤",        # 非霍奇金/霍奇金
    x_num %in% c(3)              ~ "MDS", 
    x_num %in% c(11,12)          ~ "非血液肿瘤",
    TRUE                         ~ NA_character_
  )
}
# 数值型 disease -> 是否血液肿瘤（1=血液肿瘤，0=非血液）
map_heme_flag <- function(x_num) ifelse(x_num %in% 11:12, 0L, 1L)
# 确保 disease 是数值（若当前是字符因子 1..12，先转为数值）
to_num <- function(f) as.integer(as.character(f))
# 在 OS 数据集上生成新变量
data_OS2 <- data_OS %>%
  mutate(
    disease_num = to_num(disease),
    disease4 = factor(map_to_5cat(disease_num),
                      levels = c("白血病", "多发性骨髓瘤", "淋巴瘤", "MDS", "非血液肿瘤")),
    heme_flag = factor(map_heme_flag(disease_num), levels = c(1, 0),
                       labels = c("血液肿瘤", "非血液肿瘤")),
    # 添加数值编码变量
    disease4_numeric = case_when(
      disease4 == "白血病" ~ 1,
      disease4 == "多发性骨髓瘤" ~ 2,
      disease4 == "淋巴瘤" ~ 3,
      disease4 == "MDS" ~ 4,
      disease4 == "非血液肿瘤" ~ 5,
      TRUE ~ NA_real_
    ),
    heme_flag_numeric = case_when(
      heme_flag == "血液肿瘤" ~ 1,
      heme_flag == "非血液肿瘤" ~ 2,
      TRUE ~ NA_real_
    )
  )
# 在 PFS 数据集上生成新变量
data_PFS2 <- data_PFS %>%
  mutate(
    disease_num = to_num(disease),
    disease4 = factor(map_to_5cat(disease_num),
                      levels = c("白血病", "多发性骨髓瘤", "淋巴瘤", "MDS", "非血液肿瘤")),
    heme_flag = factor(map_heme_flag(disease_num), levels = c(1, 0),
                       labels = c("血液肿瘤", "非血液肿瘤")),
    # 添加数值编码变量
    disease4_numeric = case_when(
      disease4 == "白血病" ~ 1,
      disease4 == "多发性骨髓瘤" ~ 2,
      disease4 == "淋巴瘤" ~ 3,
      disease4 == "MDS" ~ 4,
      disease4 == "非血液肿瘤" ~ 5,
      TRUE ~ NA_real_
    ),
    heme_flag_numeric = case_when(
      heme_flag == "血液肿瘤" ~ 1,
      heme_flag == "非血液肿瘤" ~ 2,
      TRUE ~ NA_real_
    )
  )
# 重命名第17，18列为disease_small和disease_large
colnames(data_OS2)[17:18] <- c("disease_small", "disease_large")
# 重命名第20，21列为disease_small和disease_large
colnames(data_PFS2)[20:21] <- c("disease_small", "disease_large")


# 确保这些变量是合适的类型
data_OS2  <- data_OS2  %>% mutate(sex = as.factor(sex), disease_small = as.factor(disease_small))
data_OS2  <- data_OS2  %>% mutate(sex = as.factor(sex), disease_large = as.factor(disease_large))
data_PFS2 <- data_PFS2 %>% mutate(sex = as.factor(sex), disease_small = as.factor(disease_small))
data_PFS2 <- data_PFS2 %>% mutate(sex = as.factor(sex), disease_large = as.factor(disease_large))

colnames(data_OS2)
# 补充一个吸烟饮酒的协变量控制
# 从 data_geren3_updated 提取吸烟/饮酒信息
hist_df <- data_geren3_updated %>%
  dplyr::select(
    patient,
    smoking, drinking,                # 是否吸烟/饮酒（若是0/1或是/否）
    smoking_frequency, smoking_year,  # 频次/年数（可做敏感性或次要分析）
    drinking_frequency, drinking_year
  ) %>%
  dplyr::rename(患者编号 = patient) %>%
  dplyr::mutate(
    # 统一成分类变量；优先使用 smoking/drinking，其次用 frequency 占位；都缺失则“未知”
    smoke_status = dplyr::case_when(
      !is.na(smoking) ~ as.character(smoking),
      !is.na(smoking_frequency) ~ paste0("freq_", smoking_frequency),
      TRUE ~ "未知"
    ),
    alcohol_status = dplyr::case_when(
      !is.na(drinking) ~ as.character(drinking),
      !is.na(drinking_frequency) ~ paste0("freq_", drinking_frequency),
      TRUE ~ "未知"
    )
  ) %>%
  dplyr::transmute(
    患者编号,
    smoke_status   = factor(smoke_status),
    alcohol_status = factor(alcohol_status),
    smoking_year,
    drinking_year
  )
table(hist_df$smoke_status)
table(hist_df$alcohol_status)

# 将吸烟和饮酒信息合并到 data_OS2 和 data_PFS2 中
data_OS2 <- data_OS2 %>%
  dplyr::left_join(hist_df, by = "患者编号")

data_PFS2 <- data_PFS2 %>%
  dplyr::left_join(hist_df, by = "患者编号")

# ===== Cox：基线营养风险
fit_OS_baseline1  <- survival::coxph(
  survival::Surv(os_time,  os_event)  ~ nutri_baseline + age_std + sex + smoke_status + alcohol_status + disease_large,
  data = data_OS2, ties = "efron"
)
fit_PFS_baseline1 <- survival::coxph(
  survival::Surv(pfs_time, pfs_event) ~ nutri_baseline + age_std + sex + smoke_status + alcohol_status + disease_large,
  data = data_PFS2, ties = "efron"
)
summary(fit_OS_baseline1)
summary(fit_PFS_baseline1)
cox.zph(fit_OS_baseline1)
cox.zph(fit_PFS_baseline1)

# 只保留disease_large为1的行(血液肿瘤患者)
data_OS3 <- data_OS2[data_OS2$disease_large == 1, ]
data_PFS3 <- data_PFS2[data_PFS2$disease_large == 1, ]
# 重新设置因子水平
data_OS3$disease_small <- factor(data_OS3$disease_small, 
                                 levels = c("1", "2", "3", "4"))
data_PFS3$disease_small <- factor(data_PFS3$disease_small, 
                                  levels = c("1", "2", "3", "4"))

fit_OS_baseline2  <- survival::coxph(
  survival::Surv(os_time,  os_event)  ~ nutri_baseline + age_std + sex + smoke_status + alcohol_status + disease_small,
  data = data_OS3, ties = "efron"
)
fit_PFS_baseline2 <- survival::coxph(
  survival::Surv(pfs_time, pfs_event) ~ nutri_baseline + age_std + sex + smoke_status + alcohol_status + disease_small,
  data = data_PFS3, ties = "efron"
)
summary(fit_OS_baseline2)
summary(fit_PFS_baseline2)
cox.zph(fit_OS_baseline2)
cox.zph(fit_PFS_baseline2)

# 交互模型,基本上交互不显著，说明营养风险对患者的影响较为一致
fit_OS_interact1 <- coxph(Surv(os_time, os_event) ~ nutri_baseline*disease_large + age_std + sex,
                         data = data_OS2, ties="efron")
summary(fit_OS_interact1)
fit_OS_interact2 <- coxph(Surv(os_time, os_event) ~ nutri_baseline*disease_small + age_std + sex,
                          data = data_OS3, ties="efron")
summary(fit_OS_interact2)

########### 可视化K-M曲线
# ===== K-M曲线图绘制 (英文标题) 
# 1. OS | Hematologic malignancies（disease_large=1）
data_temp <- data_OS2 %>% 
  filter(disease_large == 1) %>%
  mutate(nutri_tertile = cut(nutri_baseline, 
                             breaks = quantile(nutri_baseline, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), 
                             include.lowest = TRUE, 
                             labels = c("Low","Mid","High")))

fit_temp <- survfit(Surv(os_time, os_event) ~ nutri_tertile, data = data_temp)

pdf("OS_1.pdf", width = 15, height = 10)
plt_OS_dlarge1 <- ggsurvplot(fit_temp, data = data_temp,
                             risk.table = TRUE, pval = TRUE,
                             legend.title = "Baseline nutrition (tertiles)",
                             legend.labs = c("Low", "Mid", "High"),
                             xlab = "Days", ylab = "Overall survival",
                             title = "OS | Hematologic malignancies (disease_large=1)",
                             palette = c("#de2d26","#74c476","#6baed6"))
print(plt_OS_dlarge1)
dev.off()

# 2. OS | Non-hematological cancers（disease_large=2）
data_temp <- data_OS2 %>% 
  filter(disease_large == 2) %>%
  mutate(nutri_tertile = cut(nutri_baseline, 
                             breaks = quantile(nutri_baseline, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), 
                             include.lowest = TRUE, 
                             labels = c("Low","Mid","High")))

fit_temp <- survfit(Surv(os_time, os_event) ~ nutri_tertile, data = data_temp)

pdf("OS_2.pdf", width = 15, height = 10)
plt_OS_dlarge2 <- ggsurvplot(fit_temp, data = data_temp,
                             risk.table = TRUE, pval = TRUE,
                             legend.title = "Baseline nutrition (tertiles)",
                             legend.labs = c("Low", "Mid", "High"),
                             xlab = "Days", ylab = "Overall survival",
                             title = "OS | Non-hematological cancers (disease_large=2)",
                             palette = c("#de2d26","#74c476","#6baed6"))
print(plt_OS_dlarge2)
dev.off()

# 3. OS | disease_small=1 (白血病)
data_temp <- data_OS3 %>% 
  filter(disease_small == 1) %>%
  mutate(nutri_tertile = cut(nutri_baseline, 
                             breaks = quantile(nutri_baseline, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), 
                             include.lowest = TRUE, 
                             labels = c("Low","Mid","High")))

fit_temp <- survfit(Surv(os_time, os_event) ~ nutri_tertile, data = data_temp)

pdf("OS_3.pdf", width = 15, height = 10)
plt_OS_small1 <- ggsurvplot(fit_temp, data = data_temp,
                            risk.table = TRUE, pval = TRUE,
                            legend.title = "Baseline nutrition (tertiles)",
                            legend.labs = c("Low", "Mid", "High"),
                            xlab = "Days", ylab = "Overall survival",
                            title = "OS | leukemia (disease_small=1)",
                            palette = c("#de2d26","#74c476","#6baed6"))
print(plt_OS_small1)
dev.off()


# 4. OS | disease_small=2 (多发性骨髓瘤)
data_temp <- data_OS3 %>% 
  filter(disease_small == 2) %>%
  mutate(nutri_tertile = cut(nutri_baseline, 
                             breaks = quantile(nutri_baseline, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), 
                             include.lowest = TRUE, 
                             labels = c("Low","Mid","High")))

fit_temp <- survfit(Surv(os_time, os_event) ~ nutri_tertile, data = data_temp)

pdf("OS_4.pdf", width = 15, height = 10)
plt_OS_small2 <- ggsurvplot(fit_temp, data = data_temp,
                            risk.table = TRUE, pval = TRUE,
                            legend.title = "Baseline nutrition (tertiles)",
                            legend.labs = c("Low", "Mid", "High"),
                            xlab = "Days", ylab = "Overall survival",
                            title = "OS | Multiple myeloma (disease_small=2)",
                            palette = c("#de2d26","#74c476","#6baed6"))
print(plt_OS_small2)
dev.off()

# 5. OS | disease_small=3 (淋巴瘤)
data_temp <- data_OS3 %>% 
  filter(disease_small == 3) %>%
  mutate(nutri_tertile = cut(nutri_baseline, 
                             breaks = quantile(nutri_baseline, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), 
                             include.lowest = TRUE, 
                             labels = c("Low","Mid","High")))

fit_temp <- survfit(Surv(os_time, os_event) ~ nutri_tertile, data = data_temp)

pdf("OS_5.pdf", width = 15, height = 10)
plt_OS_small3 <- ggsurvplot(fit_temp, data = data_temp,
                            risk.table = TRUE, pval = TRUE,
                            legend.title = "Baseline nutrition (tertiles)",
                            legend.labs = c("Low", "Mid", "High"),
                            xlab = "Days", ylab = "Overall survival",
                            title = "OS | Lymphoma (disease_small=2)",
                            palette = c("#de2d26","#74c476","#6baed6"))
print(plt_OS_small3)
dev.off()

# 6. OS | disease_small=4 (MDS)
data_temp <- data_OS3 %>% 
  filter(disease_small == 4) %>%
  mutate(nutri_tertile = cut(nutri_baseline, 
                             breaks = quantile(nutri_baseline, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), 
                             include.lowest = TRUE, 
                             labels = c("Low","Mid","High")))

fit_temp <- survfit(Surv(os_time, os_event) ~ nutri_tertile, data = data_temp)

pdf("OS_6.pdf", width = 15, height = 10)
plt_OS_small4 <- ggsurvplot(fit_temp, data = data_temp,
                            risk.table = TRUE, pval = TRUE,
                            legend.title = "Baseline nutrition (tertiles)",
                            legend.labs = c("Low", "Mid", "High"),
                            xlab = "Days", ylab = "Overall survival",
                            title = "OS | MDS (disease_small=2)",
                            palette = c("#de2d26","#74c476","#6baed6"))
print(plt_OS_small4)
dev.off()

# 想画 PFS 六张，把 data_OS2 换 data_PFS2，time/event 换 pfs_time/pfs_event 即可
# 1. PFS | Hematologic malignancies（disease_large=1）
data_temp <- data_PFS2 %>% 
  filter(disease_large == 1) %>%
  mutate(nutri_tertile = cut(nutri_baseline, 
                             breaks = quantile(nutri_baseline, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), 
                             include.lowest = TRUE, 
                             labels = c("Low","Mid","High")))

fit_temp <- survfit(Surv(pfs_time, pfs_event) ~ nutri_tertile, data = data_temp)

pdf("PFS_1.pdf", width = 15, height = 10)
plt_PFS_dlarge1 <- ggsurvplot(fit_temp, data = data_temp,
                              risk.table = TRUE, pval = TRUE,
                              legend.title = "Baseline nutrition (tertiles)",
                              legend.labs = c("Low", "Mid", "High"),
                              xlab = "Days", ylab = "Progression-free survival",
                              title = "PFS | Hematologic malignancies (disease_large=1)",
                              palette = c("#de2d26","#74c476","#6baed6"))
print(plt_PFS_dlarge1)
dev.off()


# 2. PFS | Non-hematological cancers（disease_large=2）
data_temp <- data_PFS2 %>% 
  filter(disease_large == 2) %>%
  mutate(nutri_tertile = cut(nutri_baseline, 
                             breaks = quantile(nutri_baseline, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), 
                             include.lowest = TRUE, 
                             labels = c("Low","Mid","High")))

fit_temp <- survfit(Surv(pfs_time, pfs_event) ~ nutri_tertile, data = data_temp)

pdf("PFS_2.pdf", width = 15, height = 10)
plt_PFS_dlarge2 <- ggsurvplot(fit_temp, data = data_temp,
                              risk.table = TRUE, pval = TRUE,
                              legend.title = "Baseline nutrition (tertiles)",
                              legend.labs = c("Low", "Mid", "High"),
                              xlab = "Days", ylab = "Progression-free survival",
                              title = "PFS | Non-hematological cancers (disease_large=2)",
                              palette = c("#de2d26","#74c476","#6baed6"))
print(plt_PFS_dlarge2)
dev.off()

# 3. PFS | disease_small=1 (白血病)
data_temp <- data_PFS3 %>% 
  filter(disease_small == 1) %>%
  mutate(nutri_tertile = cut(nutri_baseline, 
                             breaks = quantile(nutri_baseline, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), 
                             include.lowest = TRUE, 
                             labels = c("Low","Mid","High")))

fit_temp <- survfit(Surv(pfs_time, pfs_event) ~ nutri_tertile, data = data_temp)

pdf("PFS_3.pdf", width = 15, height = 10)
plt_PFS_small1 <- ggsurvplot(fit_temp, data = data_temp,
                             risk.table = TRUE, pval = TRUE,
                             legend.title = "Baseline nutrition (tertiles)",
                             legend.labs = c("Low", "Mid", "High"),
                             xlab = "Days", ylab = "Progression-free survival",
                             title = "PFS | leukemia (disease_small=1)",
                             palette = c("#de2d26","#74c476","#6baed6"))
print(plt_PFS_small1)
dev.off()

# 4. PFS | disease_small=2 (多发性骨髓瘤)
data_temp <- data_PFS3 %>% 
  filter(disease_small == 2) %>%
  mutate(nutri_tertile = cut(nutri_baseline, 
                             breaks = quantile(nutri_baseline, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), 
                             include.lowest = TRUE, 
                             labels = c("Low","Mid","High")))

fit_temp <- survfit(Surv(pfs_time, pfs_event) ~ nutri_tertile, data = data_temp)

pdf("PFS_4.pdf", width = 15, height = 10)
plt_PFS_small2 <- ggsurvplot(fit_temp, data = data_temp,
                             risk.table = TRUE, pval = TRUE,
                             legend.title = "Baseline nutrition (tertiles)",
                             legend.labs = c("Low", "Mid", "High"),
                             xlab = "Days", ylab = "Progression-free survival",
                             title = "PFS | Multiple myeloma (disease_small=2)",
                             palette = c("#de2d26","#74c476","#6baed6"))
print(plt_PFS_small2)
dev.off()

# 5. PFS | disease_small=3 (淋巴瘤)
data_temp <- data_PFS3 %>% 
  filter(disease_small == 3) %>%
  mutate(nutri_tertile = cut(nutri_baseline, 
                             breaks = quantile(nutri_baseline, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), 
                             include.lowest = TRUE, 
                             labels = c("Low","Mid","High")))

fit_temp <- survfit(Surv(pfs_time, pfs_event) ~ nutri_tertile, data = data_temp)

pdf("PFS_5.pdf", width = 15, height = 10)
plt_PFS_small3 <- ggsurvplot(fit_temp, data = data_temp,
                             risk.table = TRUE, pval = TRUE,
                             legend.title = "Baseline nutrition (tertiles)",
                             legend.labs = c("Low", "Mid", "High"),
                             xlab = "Days", ylab = "Progression-free survival",
                             title = "PFS | Lymphoma (disease_small=3)",
                             palette = c("#de2d26","#74c476","#6baed6"))
print(plt_PFS_small3)
dev.off()

# 6. PFS | disease_small=4 (MDS)
data_temp <- data_PFS3 %>% 
  filter(disease_small == 4) %>%
  mutate(nutri_tertile = cut(nutri_baseline, 
                             breaks = quantile(nutri_baseline, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), 
                             include.lowest = TRUE, 
                             labels = c("Low","Mid","High")))

fit_temp <- survfit(Surv(pfs_time, pfs_event) ~ nutri_tertile, data = data_temp)

pdf("PFS_6.pdf", width = 15, height = 10)
plt_PFS_small4 <- ggsurvplot(fit_temp, data = data_temp,
                             risk.table = TRUE, pval = TRUE,
                             legend.title = "Baseline nutrition (tertiles)",
                             legend.labs = c("Low", "Mid", "High"),
                             xlab = "Days", ylab = "Progression-free survival",
                             title = "PFS | MDS (disease_small=4)",
                             palette = c("#de2d26","#74c476","#6baed6"))
print(plt_PFS_small4)
dev.off()


###### 3.7 Cox 预测曲线（固定协变量；nutri 取 Q25/Q50/Q75 ######
Mode <- function(x) {
  ux <- na.omit(x)
  if (length(ux) == 0) return(NA)
  ux[which.max(tabulate(base::match(ux, ux)))]
}

# OS：选择预测所用的 disease、sex 水平和 age_std
ref_dis_OS  <- Mode(data_OS2$disease_large)
ref_sex_OS  <- Mode(data_OS2$sex)
mean_age_OS <- mean(data_OS2$age_std, na.rm = TRUE)

q_OS <- quantile(data_OS2$nutri_baseline, probs = c(.25,.5,.75), na.rm = TRUE)
new_OS <- data.frame(
  nutri_baseline = q_OS,
  age_std = mean_age_OS,
  sex = factor(ref_sex_OS, levels = levels(data_OS2$sex)),
  disease_large = factor(ref_dis_OS, levels = levels(data_OS2$disease_large))
)

sf_OS <- survfit(fit_OS_baseline1, newdata = new_OS)
p_pred_OS <- ggsurvplot(sf_OS,
                        data = new_OS,   # ???? 一定要加上
                        legend.labs = paste0("Nutri Q", c(25,50,75)),
                        xlab = "Days", ylab = "Predicted OS",
                        title = "Cox predicted curves (OS)",
                        palette = c("#1b9e77","#d95f02","#7570b3"))
print(p_pred_OS)
ggsave("pred_OS.pdf", plot = p_pred_OS$plot, 
       width = 15, height = 10, device = "pdf")

# 假设 sf_OS 是 survfit 对象
sf_OS_df <- broom::tidy(sf_OS)
# 查看前几行
head(sf_OS_df)
# 保存到文件
write.csv(sf_OS_df, "cox_predicted_OS.csv", row.names = FALSE)


# PFS：同理
ref_dis_PFS  <- Mode(data_PFS2$disease_large)
ref_sex_PFS  <- Mode(data_PFS2$sex)
mean_age_PFS <- mean(data_PFS2$age_std, na.rm = TRUE)

q_PFS <- quantile(data_PFS2$nutri_baseline, probs = c(.25,.5,.75), na.rm = TRUE)
new_PFS <- data.frame(
  nutri_baseline = q_PFS,
  age_std = mean_age_PFS,
  sex = factor(ref_sex_PFS, levels = levels(data_PFS$sex)),
  disease_large = factor(ref_dis_PFS, levels = levels(data_PFS2$disease_large))
)

sf_PFS <- survfit(fit_PFS_baseline1, newdata = new_PFS)
p_pred_PFS <- ggsurvplot(sf_PFS,
                         data = new_PFS,   # ???? 一定要加上
                         legend.labs = paste0("Nutri Q", c(25,50,75)),
                         xlab = "Days", ylab = "Predicted PFS",
                         palette = c("#1b9e77","#d95f02","#7570b3"))

print(p_pred_PFS)
ggsave("pred_PFS.pdf", plot = p_pred_PFS$plot, 
       width = 15, height = 10, device = "pdf")

#####  3.8 森林图（展示关键自变量的 HR：营养/年龄/性别，分别在 OS 和 PFS）####
install.packages("forestmodel")
# 加载必要的包
library(forestmodel)

# OS基线模型1 (全人群)
pdf("forest_OS_baseline1.pdf", width = 15, height = 10)
forest_plot_OS1 <- forest_model(fit_OS_baseline1,
                                covariates = c("nutri_baseline", "age_std", "sex","smoke_status","alcohol_status", "disease_large"),
                                factor_separate_line = TRUE)
print(forest_plot_OS1)
dev.off()

# PFS基线模型1 (全人群)
pdf("forest_PFS_baseline1.pdf", width = 15, height = 10)
forest_plot_PFS1 <- forest_model(fit_PFS_baseline1,
                                 covariates = c("nutri_baseline", "age_std", "sex", "smoke_status","alcohol_status","disease_large"),
                                 factor_separate_line = TRUE)
print(forest_plot_PFS1)
dev.off()

# OS基线模型2 (仅血液肿瘤患者)
pdf("forest_OS_baseline2.pdf", width = 15, height = 10)
forest_plot_OS2 <- forest_model(fit_OS_baseline2,
                                covariates = c("nutri_baseline", "age_std", "sex", "smoke_status","alcohol_status","disease_small"),
                                factor_separate_line = TRUE)
print(forest_plot_OS2)
dev.off()

# PFS基线模型2 (仅血液肿瘤患者)
pdf("forest_PFS_baseline2.pdf", width = 15, height = 10)
forest_plot_PFS2 <- forest_model(fit_PFS_baseline2,
                                 covariates = c("nutri_baseline", "age_std", "sex", "smoke_status","alcohol_status","disease_small"),
                                 factor_separate_line = TRUE)
print(forest_plot_PFS2)
dev.off()

######## 3.9 时变cox分析 #######

## ───────────────────────────────────────────────
## A. 需要的基础对象
## ───────────────────────────────────────────────
# 1) 生存数据：data_OS2, data_PFS2（含 os_time/os_event, pfs_time/pfs_event, age_std, sex, disease4）
# 2) 营养轨迹观测：landmark_features（patient, time, theta_mean, age_std, sex, disease, …）
# 3) brms 拟合：fit_fast；以及你已定义好的 make_basis(), mu_log, sd_log, ns_knots, ns_bks

## ───────────────────────────────────────────────
## B. 先把“已有观测”的 θ(t) 整理成时变表（LOCF 会用到）
## ───────────────────────────────────────────────
nutri_long_obs <- landmark_features %>%
  rename(患者编号 = patient) %>%
  transmute(
    患者编号,
    t_meas = pmax(0, as.numeric(time)),
    nutri  = as.numeric(theta_mean)
  ) %>%
  filter(is.finite(t_meas), is.finite(nutri)) %>%
  arrange(患者编号, t_meas) %>%
  distinct(患者编号, t_meas, .keep_all = TRUE)

# 用基线兜底（保证 t=0 都有值）
nutri0_df <- nutri_base_df %>%
  transmute(患者编号, t_meas = 0, nutri = as.numeric(nutri_baseline)) %>%
  filter(is.finite(nutri))
nutri_long_base <- bind_rows(nutri_long_obs, nutri0_df) %>%
  arrange(患者编号, t_meas) %>%
  distinct(患者编号, t_meas, .keep_all = TRUE)

## ───────────────────────────────────────────────
## C. 构造 “事件前 L 天” 的评估时点，并用 brms 预测 θ
## ───────────────────────────────────────────────
LAGS <- c(7, 14, 30, 60)  # 你想比较的滞后窗口（单位：天）

# OS：仅对有事件的患者生成 t_eval = max(0, os_time - L)
eval_OS <- data_OS2 %>%
  filter(os_event == 1L) %>%
  select(患者编号, os_time, age_std, sex, disease4) %>%
  crossing(lag = LAGS) %>%
  transmute(患者编号,
            time = pmax(0, as.numeric(os_time) - lag),
            lag, age_std, sex, disease4)

# PFS：同理
eval_PFS <- data_PFS2 %>%
  filter(pfs_event == 1L) %>%
  select(患者编号, pfs_time, age_std, sex, disease4) %>%
  crossing(lag = LAGS) %>%
  transmute(患者编号,
            time = pmax(0, as.numeric(pfs_time) - lag),
            lag, age_std, sex, disease4)

# 合并 OS/PFS 的评估点（去重）
eval_all <- bind_rows(
  eval_OS %>% mutate(source = "OS"),
  eval_PFS %>% mutate(source = "PFS")
) %>%
  arrange(患者编号, time, lag, source) %>%
  distinct(患者编号, time, lag, .keep_all = TRUE)

# 为 brms 预测准备协变量（使用每位患者在 landmark_features 中的基线/常量协变量）
pat_cov <- landmark_features %>%
  arrange(patient, time) %>%
  group_by(patient) %>%
  slice(1) %>% ungroup() %>%
  transmute(patient,
            age_std = as.numeric(age_std),
            sex     = factor(sex),
            disease = factor(disease),
            Marital = factor(Marital),
            Profession = factor(Profession),
            module  = factor("NRS_new", levels = levels(landmark_features$module)))  # 占位，不影响 theta

## —— 用参与 landmark 模型训练的患者做交集（避免新水平 & 列冲突）——
# 训练集里的水平
train_pat <- levels(droplevels(factor(landmark_features$patient)))
lvl_sex   <- levels(droplevels(factor(landmark_features$sex)))
lvl_dis   <- levels(droplevels(factor(landmark_features$disease)))
lvl_mar   <- levels(droplevels(factor(landmark_features$Marital)))
lvl_prof  <- levels(droplevels(factor(landmark_features$Profession)))
lvl_mod   <- levels(droplevels(factor(landmark_features$module)))

# 先做内连接，然后处理列名冲突
new_eval <- eval_all %>%
  rename(patient = 患者编号) %>%
  inner_join(pat_cov, by = "patient") %>%
  # 选择使用pat_cov中的协变量（.y列），并重命名
  rename(
    age_std = age_std.y,
    sex = sex.y,
  ) %>%
  # 删除不需要的重复列
  select(-age_std.x, -sex.x, -disease4) %>%
  # 对齐因子水平到训练集（很关键）
  mutate(
    patient    = factor(patient,    levels = train_pat),
    sex        = factor(sex,        levels = lvl_sex),
    disease    = factor(disease,    levels = lvl_dis),
    Marital    = factor(Marital,    levels = lvl_mar),
    Profession = factor(Profession, levels = lvl_prof),
    module     = factor(module,     levels = lvl_mod)
  )
# 重新生成与训练一致的时间样条（放在 join 和过滤之后再算，行数/顺序更稳妥）
basis_eval <- make_basis(new_eval$time)
new_eval$log_time_std <- basis_eval$log_time_std
new_eval$ns_time_1    <- basis_eval$ns_time_1
new_eval$ns_time_2    <- basis_eval$ns_time_2

# 现在再做 posterior_linpred（带随机效应的个体化预测）
ndraws_theta <- 400
theta_draws_eval <- brms::posterior_linpred(
  fit_fast,
  newdata    = new_eval,
  nlpar      = "theta",
  re_formula = NULL,      # 包含随机截距
  ndraws     = ndraws_theta,
  transform  = FALSE
)

theta_eval_summ <- as_tibble(t(theta_draws_eval)) %>%
  mutate(obs_id = row_number()) %>%
  pivot_longer(-obs_id, names_to = "draw", values_to = "theta_draw") %>%
  group_by(obs_id) %>%
  summarise(
    nutri_pred  = mean(theta_draw),
    nutri_l95   = quantile(theta_draw, 0.025),
    nutri_u95   = quantile(theta_draw, 0.975),
    .groups = "drop"
  )

new_eval$nutri_pred <- theta_eval_summ$nutri_pred

# 整理成 tmerge 需要的长表（按患者、时点、取值）
nutri_long_lag <- new_eval %>%
  transmute(患者编号 = patient,
            t_meas   = as.numeric(time),
            nutri    = as.numeric(nutri_pred)) %>%
  arrange(患者编号, t_meas) %>%
  distinct(患者编号, t_meas, .keep_all = TRUE)

# 把“观测时点 + 基线 + 事件前滞后时点”的取值合在一起
nutri_long_all <- bind_rows(nutri_long_base, nutri_long_lag) %>%
  arrange(患者编号, t_meas) %>%
  distinct(患者编号, t_meas, .keep_all = TRUE)

## ───────────────────────────────────────────────
## D. 时变 Cox（全体 & 血液肿瘤）
## ───────────────────────────────────────────────
## —— OS 全体
base_OS <- data_OS2 %>%
  transmute(患者编号, tstart = 0, tstop = as.numeric(os_time),
            os_event = as.integer(os_event),
            age_std, sex, smoke_status,alcohol_status, disease_large) %>%
  filter(is.finite(tstop) & tstop > 0)

tm_OS1 <- tmerge(data1 = base_OS, data2 = base_OS, id = 患者编号,
                endpt = event(tstop, os_event))
tm_OS2 <- tmerge(data1 = tm_OS1, data2 = nutri_long_all, id = 患者编号,
                nutri_tv = tdc(t_meas, nutri))  # 分段常数

fit_OS_tv_bayes1 <- coxph(Surv(tstart, tstop, endpt) ~ nutri_tv + age_std + sex + smoke_status + alcohol_status + disease_large,
                         data = tm_OS2, ties = "efron")
summary(fit_OS_tv_bayes1)
cox.zph(fit_OS_tv_bayes1)

## —— PFS 全体
base_PFS <- data_PFS2 %>%
  transmute(患者编号, tstart = 0, tstop = as.numeric(pfs_time),
            pfs_event = as.integer(pfs_event),
            age_std, sex, smoke_status,alcohol_status,disease_large) %>%
  filter(is.finite(tstop) & tstop > 0)

tm_PFS1 <- tmerge(data1 = base_PFS, data2 = base_PFS, id = 患者编号,
                 endpt = event(tstop, pfs_event))
tm_PFS2 <- tmerge(data1 = tm_PFS1, data2 = nutri_long_all, id = 患者编号,
                 nutri_tv = tdc(t_meas, nutri))

fit_PFS_tv_bayes1 <- coxph(Surv(tstart, tstop, endpt) ~ nutri_tv + age_std + sex + smoke_status + alcohol_status + disease_large,
                          data = tm_PFS2, ties = "efron")
summary(fit_PFS_tv_bayes1)
cox.zph(fit_PFS_tv_bayes1)

## —— 仅血液肿瘤患者OS
base_OS2 <- data_OS3 %>%
  transmute(患者编号, tstart = 0, tstop = as.numeric(os_time),
            os_event = as.integer(os_event),
            age_std, sex, smoke_status,alcohol_status,disease_small) %>%
  filter(is.finite(tstop) & tstop > 0)

tm_OS3 <- tmerge(data1 = base_OS2, data2 = base_OS2, id = 患者编号,
                endpt = event(tstop, os_event))
tm_OS4 <- tmerge(data1 = tm_OS3, data2 = nutri_long_all, id = 患者编号,
                nutri_tv = tdc(t_meas, nutri))  # 分段常数

fit_OS_tv_bayes2 <- coxph(Surv(tstart, tstop, endpt) ~ nutri_tv + age_std + sex + smoke_status + alcohol_status +  disease_small,
                         data = tm_OS4, ties = "efron")
summary(fit_OS_tv_bayes2)
cox.zph(fit_OS_tv_bayes2)

# —— 仅血液肿瘤患者PFS
base_PFS2 <- data_PFS3 %>%
  transmute(患者编号, tstart = 0, tstop = as.numeric(pfs_time),
            pfs_event = as.integer(pfs_event),
            age_std, sex, smoke_status,alcohol_status,disease_small) %>%
  filter(is.finite(tstop) & tstop > 0)

tm_PFS3 <- tmerge(data1 = base_PFS2, data2 = base_PFS2, id = 患者编号,
                 endpt = event(tstop, pfs_event))
tm_PFS4 <- tmerge(data1 = tm_PFS3, data2 = nutri_long_all, id = 患者编号,
                 nutri_tv = tdc(t_meas, nutri))

fit_PFS_tv_bayes2 <- coxph(Surv(tstart, tstop, endpt) ~ nutri_tv + age_std + sex + smoke_status + alcohol_status + disease_small,
                          data = tm_PFS4, ties = "efron")
summary(fit_PFS_tv_bayes2)
cox.zph(fit_PFS_tv_bayes2)

#### 可视化
# 森林图
library(forestploter)  
library(forestmodel)
library(survminer)
## 让四个模型都“记住数据” —— 用 update 最省事
fit_OS_tv_bayes1  <- update(fit_OS_tv_bayes1,  data = tm_OS2,  x = TRUE, y = TRUE, model = TRUE)
fit_PFS_tv_bayes1 <- update(fit_PFS_tv_bayes1, data = tm_PFS2, x = TRUE, y = TRUE, model = TRUE)
fit_OS_tv_bayes2  <- update(fit_OS_tv_bayes2,  data = tm_OS4,  x = TRUE, y = TRUE, model = TRUE)
fit_PFS_tv_bayes2 <- update(fit_PFS_tv_bayes2, data = tm_PFS4, x = TRUE, y = TRUE, model = TRUE)

## 森林图，用ggforest包
# 通用封装：列名英文 + 样式一致
plot_ggforest <- function(fit, data, title){
  ggforest(
    fit, data = data,
    main = title,
    cpositions = c(0.02, 0.22, 0.4),  # 左/中/右列位置
    fontsize = 1.0,
    refLabel = "Ref",
    noDigits = 2
  ) +
    theme(
      plot.title = element_text(size = 13, face = "bold"),
      text = element_text(family = "", color = "black")
    )
}

# —— OS 全体
pdf("forest_OS_时变全体.pdf", width = 15, height = 10)
print(plot_ggforest(fit_OS_tv_bayes1, tm_OS2, "OS (All patients)"))
dev.off()

# —— PFS 全体
pdf("forest_PFS_时变全体.pdf", width = 15, height = 10)
print(plot_ggforest(fit_PFS_tv_bayes1, tm_PFS2, "PFS (All patients)"))
dev.off()

# —— 仅血液肿瘤 OS
pdf("forest_OS_时变血液肿瘤.pdf", width = 15, height = 10)
print(plot_ggforest(fit_OS_tv_bayes2, tm_OS4, "OS (Hematologic only)"))
dev.off()

# —— 仅血液肿瘤 PFS
pdf("forest_PFS_时变血液肿瘤.pdf", width = 15, height = 10)
print(plot_ggforest(fit_PFS_tv_bayes2, tm_PFS4, "PFS (Hematologic only)"))
dev.off()

# 效应曲线
library(splines)
library(dplyr)
library(tibble)

# 基于已有 coxph 拟合对象：将 var 替换成 ns(var, df) 后重拟合并画效应曲线
plot_effect_curve_ns <- function(fit, data, var = "nutri_tv",
                                 df = 3, q_lo = 0.05, q_hi = 0.95, n = 200,
                                 ref = c("median","min"),
                                 title = "Effect of nutrition risk") {
  ref <- match.arg(ref)
  
  # 1) 从原模型公式提取左侧 Surv(...) 与右侧项
  fml <- formula(fit)
  lhs <- deparse(fml[[2]])
  rhs_terms <- attr(terms(fit), "term.labels")
  
  # 2) 把目标变量替换成 ns(var, df)
  if(!var %in% rhs_terms){
    stop(sprintf("The variable '%s' is not a main term in the model.", var))
  }
  rhs_new <- ifelse(rhs_terms == var, paste0("ns(", var, ", df=", df, ")"), rhs_terms) %>%
    paste(collapse = " + ")
  fml_ns <- as.formula(paste(lhs, "~", rhs_new))
  
  # 3) 以同一数据重拟合（保存 x/y/model）
  fit_ns <- coxph(fml_ns, data = data, ties = "efron", x = TRUE, y = TRUE, model = TRUE)
  
  # 4) 构造绘图横轴（按分位数范围）
  stopifnot(var %in% names(data))
  vx  <- data[[var]]
  vx  <- vx[is.finite(vx)]
  rng <- stats::quantile(vx, c(q_lo, q_hi), na.rm = TRUE)
  xs  <- seq(rng[1], rng[2], length.out = n)
  
  # 5) 构造“代表性”协变量：数值取中位数，因子取第一个水平
  proto <- lapply(data, function(col){
    if (is.numeric(col)) stats::median(col, na.rm = TRUE)
    else if (is.factor(col)) levels(col)[1]
    else if (is.logical(col)) FALSE
    else NA
  }) %>% as.data.frame()
  names(proto) <- names(data)
  
  newdat <- proto[rep(1, n), , drop = FALSE]
  newdat[[var]] <- xs
  
  xref <- if (ref == "median") stats::median(vx, na.rm = TRUE) else min(vx, na.rm = TRUE)
  refdat <- proto[1, , drop = FALSE]
  refdat[[var]] <- xref
  
  # 6) 预测相对参考值的 log-HR 与 95%CI（delta 近似）
  p   <- predict(fit_ns, newdata = newdat, type = "lp", se.fit = TRUE)
  pr  <- predict(fit_ns, newdata = refdat, type = "lp", se.fit = TRUE)
  lp  <- p$fit - pr$fit
  se  <- sqrt(p$se.fit^2 + pr$se.fit^2)
  lo  <- lp - 1.96 * se
  hi  <- lp + 1.96 * se
  
  df_plot <- tibble(x = xs, HR = exp(lp), HR_lo = exp(lo), HR_hi = exp(hi), xref = xref)
  
  # 7) 画图（更清晰的颜色/线宽；HR 用 log 轴）
  ggplot(df_plot, aes(x = x, y = HR)) +
    geom_ribbon(aes(ymin = HR_lo, ymax = HR_hi, fill = "95% CI"), alpha = 0.22, show.legend = TRUE) +
    geom_line(aes(color = "Effect curve"), linewidth = 1.2, linetype = "solid") +
    geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5, color = "gray50") +
    geom_vline(aes(xintercept = xref, color = "Reference"), 
               linetype = "dotted", linewidth = 1.0)  +
    scale_y_log10(name = "Hazard Ratio (log scale)") +
    scale_color_manual(values = c("Effect curve" = "#2563EB",  # 蓝
                                  "Reference"   = "black")) +
    scale_fill_manual(values  = c("95% CI" = "#93C5FD")) +      # 浅蓝
    labs(x = paste0(pretty_map[[var]] %||% var, " (ref = ", signif(xref, 3), ")"),
         title = title) +
    theme_minimal(base_size = 12) +
    theme(legend.title = element_blank(),
          plot.title   = element_text(face = "bold"))
}

# OS 全体
pdf("效应曲线OS全体.pdf", width = 15, height = 10)
plot_effect_curve_ns(
  fit_OS_tv_bayes1, tm_OS2, var = "nutri_tv",
  df = 3, title = "OS (All): non-linear effect of nutrition risk"
)
dev.off()

# PFS 全体
pdf("效应曲线PFS全体.pdf", width = 15, height = 10)
plot_effect_curve_ns(
  fit_PFS_tv_bayes1, tm_PFS2, var = "nutri_tv",
  df = 3, title = "PFS (All): non-linear effect of nutrition risk"
)
dev.off()

# 仅血液肿瘤 OS
pdf("效应曲线OS血液肿瘤.pdf", width = 15, height = 10)
plot_effect_curve_ns(
  fit_OS_tv_bayes2, tm_OS4, var = "nutri_tv",
  df = 3, title = "OS (Hematologic only): non-linear effect"
)
dev.off()

# 仅血液肿瘤 PFS
pdf("效应曲线PFS血液肿瘤.pdf", width = 15, height = 10)
plot_effect_curve_ns(
  fit_PFS_tv_bayes2, tm_PFS4, var = "nutri_tv",
  df = 3, title = "PFS (Hematologic only): non-linear effect"
)
dev.off()



######### 提取了个人史似乎还没有派上用场，记得在前面可以考虑加入吸烟饮酒情况作为人口学协变量
#######  4.1尝试做骨髓抑制的分析 #########
# text_outcomes中BMS_event为1或0，表示是否发生骨髓抑制，伴有BMS_日期
# 索引日（最早入院）
index_date_df <- text_outcomes %>%
  group_by(患者编号) %>%
  summarise(index_date = min(as.Date(入院日期), na.rm = TRUE), .groups = "drop")

# 每位患者的BMS：是否发生 & 最早BMS日期 & 最高分级/是否重度
# 第一步：获取所有患者的BMS汇总（保留所有患者）
bms_all <- text_outcomes %>%
  group_by(患者编号) %>%
  summarise(
    BMS_event = as.integer(any(BMS_event == 1L, na.rm = TRUE)),
    BMS_grade = suppressWarnings(max(BMS_grade, na.rm = TRUE)),   
    admission_date = first(入院日期),
    .groups = "drop"
  )

# 第二步：计算入院后最早BMS日期（仅用于有BMS事件的患者）
bms_dates <- text_outcomes %>%
  filter(BMS_日期 > 入院日期 | is.na(入院日期)) %>%
  group_by(患者编号) %>%
  summarise(
    BMS_date = suppressWarnings(min(BMS_日期, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(BMS_date = as.Date(as.character(BMS_date)))

# 合并结果
bms_outcome <- bms_all %>%
  left_join(bms_dates, by = "患者编号") %>%
  # 对于没有入院后BMS事件但有BMS事件的患者，BMS_date将为NA
  # 对于完全没有BMS事件的患者，BMS_date也将为NA（这是正确的）
  select(患者编号, BMS_event, BMS_date, BMS_grade)

# 每位患者的BMS：是否发生 & 最早BMS日期 & 最高分级/是否重度（不做日期筛选，事实上有的患者的入院日期大于患者BMS的日期）
bms_outcome <- text_outcomes %>%
  group_by(患者编号) %>%
  summarise(
    BMS_event = as.integer(any(BMS_event == 1L, na.rm = TRUE)),
    BMS_date  = suppressWarnings(min(BMS_日期, na.rm = TRUE)),
    BMS_grade = suppressWarnings(max(BMS_grade, na.rm = TRUE)),   
    .groups = "drop"
  ) %>%
  # 确保日期格式正确
  mutate(
    BMS_date = as.Date(as.character(BMS_date)),
    # 或者如果是数值型日期：
    # BMS_date = ifelse(is.finite(as.numeric(BMS_date)) & !is.na(BMS_date),
    #                   as.Date(as.numeric(BMS_date), origin = "1970-01-01"),
    #                   as.Date(NA))
  )
table(bms_outcome$BMS_event)
# 从 landmark_features 取基线营养与主要协变量
nutri_base_df <- landmark_features %>%
  dplyr::rename(患者编号 = patient) %>%
  filter(time == 0) %>%
  transmute(
    患者编号,
    nutri_baseline = theta_mean,
    age_std, sex, disease, Marital, Profession
  )

time_df <- index_date_df %>%
  left_join(select(data_OS2, 患者编号, os_time, os_event), by = "患者编号") %>%
  left_join(select(bms_outcome, 患者编号, BMS_date), by = "患者编号") %>%
  mutate(
    follow_end = as.numeric(os_time),                                  # 随访总时长（天/你当前单位）
    bms_time   = as.numeric(BMS_date - index_date),                    # BMS至索引日的时长
    # 仅保留在随访窗内的 BMS
    bms_time   = ifelse(is.finite(bms_time) & bms_time >= 0 &
                          bms_time <= follow_end, bms_time, NA_real_)
  )



# 把disease区分一下
# 把 12 类疾病映射为 5 大类 + 血液/非血液标签
# 现有 data_OS / data_PFS 已含: 患者编号, os_time, os_event, pfs_time, pfs_event,nutri_baseline, age_std, sex, disease(因子，水平是 1..12)
# 数值型 disease -> 5 大类
map_to_5cat <- function(x_num) {
  case_when(
    x_num %in% c(5,6,7,8,9,10) ~ "白血病",        # 含 ALL/AML/APL/CLL/CML/其他白血病/MDS
    x_num %in% c(1)              ~ "多发性骨髓瘤",
    x_num %in% c(2,4)            ~ "淋巴瘤",        # 非霍奇金/霍奇金
    x_num %in% c(3)              ~ "MDS", 
    x_num %in% c(11,12)          ~ "非血液肿瘤",
    TRUE                         ~ NA_character_
  )
}
# 数值型 disease -> 是否血液肿瘤（1=血液肿瘤，0=非血液）
map_heme_flag <- function(x_num) ifelse(x_num %in% 11:12, 0L, 1L)
# 确保 disease 是数值（若当前是字符因子 1..12，先转为数值）
to_num <- function(f) as.integer(as.character(f))
# 在 OS 数据集上生成新变量
nutri_base_df <- nutri_base_df %>%
  mutate(
    disease_num = to_num(disease),
    disease_1 = factor(map_to_5cat(disease_num),
                      levels = c("白血病", "多发性骨髓瘤", "淋巴瘤", "MDS", "非血液肿瘤")),
    disease_2 = factor(map_heme_flag(disease_num), levels = c(1, 0),
                       labels = c("血液肿瘤", "非血液肿瘤")),
    # 添加数值编码变量
    disease_small = case_when(
      disease_1 == "白血病" ~ 1,
      disease_1 == "多发性骨髓瘤" ~ 2,
      disease_1 == "淋巴瘤" ~ 3,
      disease_1 == "MDS" ~ 4,
      disease_1 == "非血液肿瘤" ~ 5,
      TRUE ~ NA_real_
    ),
    disease_large = case_when(
      disease_2 == "血液肿瘤" ~ 1,
      disease_2 == "非血液肿瘤" ~ 2,
      TRUE ~ NA_real_
    )
  )

# 从 data_geren3_updated 提取吸烟/饮酒信息
hist_df <- data_geren3_updated %>%
  dplyr::select(
    patient,
    smoking, drinking,                # 是否吸烟/饮酒（若是0/1或是/否）
    smoking_frequency, smoking_year,  # 频次/年数（可做敏感性或次要分析）
    drinking_frequency, drinking_year
  ) %>%
  dplyr::rename(患者编号 = patient) %>%
  dplyr::mutate(
    # 统一成分类变量；优先使用 smoking/drinking，其次用 frequency 占位；都缺失则“未知”
    smoke_status = dplyr::case_when(
      !is.na(smoking) ~ as.character(smoking),
      !is.na(smoking_frequency) ~ paste0("freq_", smoking_frequency),
      TRUE ~ "未知"
    ),
    alcohol_status = dplyr::case_when(
      !is.na(drinking) ~ as.character(drinking),
      !is.na(drinking_frequency) ~ paste0("freq_", drinking_frequency),
      TRUE ~ "未知"
    )
  ) %>%
  dplyr::transmute(
    患者编号,
    smoke_status   = factor(smoke_status),
    alcohol_status = factor(alcohol_status),
    smoking_year,
    drinking_year
  )

# 合并到协变量表
covar_df <- index_date_df %>%
  dplyr::left_join(nutri_base_df, by = "患者编号") %>%
  dplyr::left_join(hist_df,       by = "患者编号") %>%   # ← 用新的 hist_df
  dplyr::mutate(
    disease_large = as.factor(disease_large),
    disease_small = as.factor(disease_small),
    sex     = as.factor(sex)
  )


# 主数据集（一个患者一行）
dat_bms_base <- covar_df %>%
  left_join(bms_outcome, by = "患者编号") %>%
  # 只保留有随访信息的（若你有last_follow，可再加限制）
  filter(!is.na(BMS_event))

# 拟合（协变量按可用性自动进入）
form_base <- as.formula(
  paste0("BMS_event ~ nutri_baseline + age_std + sex + disease_large",
         if ("smoke_status" %in% names(dat_bms_base))   " + smoke_status"   else "",
         if ("alcohol_status" %in% names(dat_bms_base)) " + alcohol_status" else "")
)

fit_bms_logit_base <- glm(form_base, data = dat_bms_base, family = binomial())
summary(fit_bms_logit_base)

# 只在血液肿瘤患者中计算
# 只保留disease_large为1的行(血液肿瘤患者)
dat_bms_base2 <- dat_bms_base[dat_bms_base$disease_large == 1, ]
# 重新设置因子水平
dat_bms_base2$disease_small <- factor(dat_bms_base2$disease_small, 
                                 levels = c("1", "2", "3", "4"))
# 拟合（协变量按可用性自动进入）
form_base2 <- as.formula(
  paste0("BMS_event ~ nutri_baseline + age_std + sex + + smoke_status + alcohol_status + disease_small")
)

fit_bms_logit_base2 <- glm(form_base2, data = dat_bms_base2, family = binomial())
summary(fit_bms_logit_base2)


# LAGS 可按需要调整
LAGS <- c(7, 14)

# 生成 t_eval
eval_bms <- index_date_df %>%
  inner_join(bms_outcome %>% filter(BMS_event == 1L), by = "患者编号") %>%
  mutate(bms_time = as.numeric(BMS_date - index_date)) %>%
  tidyr::crossing(lag = LAGS) %>%
  mutate(t_eval = pmax(0, bms_time - lag))

# nutri_long_all 需要包含：患者编号、t_meas（天）、nutri（θ）
# 如果你的列名不同，请在这里修改
stopifnot(all(c("患者编号","t_meas","nutri") %in% names(nutri_long_all)))

# 就近匹配函数：优先匹配 t_meas <= t_eval 最近一次，否则最近一次
closest_prior_or_any <- function(meas_tbl, t_eval){
  # meas_tbl: 单个患者的 {t_meas, nutri}
  before <- meas_tbl %>% filter(t_meas <= t_eval)
  if (nrow(before) > 0) {
    before %>% arrange(desc(t_meas)) %>% slice(1)
  } else {
    meas_tbl %>% arrange(abs(t_meas - t_eval)) %>% slice(1)
  }
}

# 为每个患者-滞后构造 θ(t_eval)
nutri_eval <- eval_bms %>%
  group_by(患者编号, lag, t_eval) %>%
  group_modify(~{
    meas <- nutri_long_all %>% filter(患者编号 == .y$患者编号)
    if (nrow(meas) == 0) return(tibble::tibble(t_meas = NA_real_, nutri = NA_real_))
    closest_prior_or_any(meas %>% select(t_meas, nutri), .y$t_eval)
  }) %>%
  ungroup() %>%
  rename(nutri_eval = nutri)

# 构建“每位患者一行”的分析集（轨迹暴露），对每个L单独拟合
fit_list <- list(); res_list <- list()

for (L in LAGS) {
  dat_L <- covar_df %>%
    left_join(bms_outcome, by = "患者编号") %>%
    left_join(nutri_eval %>% filter(lag == L) %>% select(患者编号, nutri_eval), by = "患者编号") %>%
    mutate(nutri_eval = as.numeric(nutri_eval)) %>%
    filter(!is.na(BMS_event) & !is.na(nutri_eval))
  
  form_traj <- as.formula(
    paste0("BMS_event ~ scale(nutri_eval) + age_std + sex + disease",
           if ("smoke_status" %in% names(dat_L))   " + smoke_status"   else "",
           if ("alcohol_status" %in% names(dat_L)) " + alcohol_status" else "")
  )
  
  fit <- glm(form_traj, data = dat_L, family = binomial())
  fit_list[[paste0("L",L)]] <- fit
  res_list[[paste0("L",L)]] <- or_ci(fit) %>% mutate(model = paste0("Lag_", L, "d"))
}

bms_traj_or <- bind_rows(res_list)
print(bms_traj_or)


# 二分类：严重BMS
dat_severe <- covar_df %>%
  left_join(bms_outcome, by = "患者编号") %>%
  filter(!is.na(BMS_severe))

form_severe <- as.formula(
  paste0(BMS_severe ~ scale(nutri_baseline) + age_std + sex + smoke_status + alcohol_status + disease))
fit_bms_severe <- glm(form_severe, data = dat_severe, family = binomial())
or_ci(fit_bms_severe)

# 有序Logistic：BMS分级（0/1/2/3/4），需保证是有序因子
if ("BMS_grade" %in% names(bms_outcome)) {
  library(MASS)
  dat_grade <- covar_df %>%
    left_join(bms_outcome, by = "患者编号") %>%
    filter(is.finite(BMS_grade)) %>%
    mutate(BMS_grade = factor(BMS_grade, ordered = TRUE))
  
  form_grade <- as.formula(
    paste0("BMS_grade ~ scale(nutri_baseline) + age_std + sex + disease",
           if ("smoke_status" %in% names(dat_grade))   " + smoke_status"   else "",
           if ("alcohol_status" %in% names(dat_grade)) " + alcohol_status" else "")
  )
  fit_bms_grade <- MASS::polr(form_grade, data = dat_grade, Hess = TRUE)
  summ <- coef(summary(fit_bms_grade))
  OR   <- exp(coef(fit_bms_grade))
  CI   <- exp(confint(fit_bms_grade))
  print(list(summary = summ, OR = OR, CI = CI))
}








# 诊断归转情况为死亡的行，则结局列相应行的值也改为死亡
# —— 诊断归转=死亡 时，将死亡列兜底为1（文本漏检时不至于漏记 OS/PFS）——
text_outcomes <- text_outcomes %>%
  mutate(
    死亡 = ifelse(诊断归转情况 %in% c("死亡","死亡出院","死亡/死亡"), 1L,
                ifelse(is.na(死亡), 0L, as.integer(死亡)))
  )

# —— 患者级 OS/PFS（事件与时间）——
patient_surv <- text_outcomes %>%
  group_by(患者编号) %>%
  summarise(
    # OS 事件
    OS_event = as.integer(any(死亡 == 1L, na.rm = TRUE)),
    OS_time  = ifelse(OS_event == 1L,
                      suppressWarnings(min(time[死亡 == 1L], na.rm = TRUE)),
                      suppressWarnings(max(time, na.rm = TRUE))),
    # PFS 事件（严格复发/严格进展/死亡 之一）
    PFS_event = as.integer(any(复发_严格 == 1L | 加重进展_严格 == 1L | 死亡 == 1L, na.rm = TRUE)),
    PFS_time  = ifelse(PFS_event == 1L,
                       suppressWarnings(min(time[(复发_严格 == 1L | 加重进展_严格 == 1L | 死亡 == 1L)], na.rm = TRUE)),
                       suppressWarnings(max(time, na.rm = TRUE))),
    .groups = "drop"
  )

# —— 若 time 全NA 的患者会得到 Inf/NA，可做兜底清理 —— 
patient_surv <- patient_surv %>%
  mutate(
    OS_time  = ifelse(is.infinite(OS_time),  NA, OS_time),
    PFS_time = ifelse(is.infinite(PFS_time), NA, PFS_time)
  )

### 人工复核
to_review <- text_outcomes %>%
  left_join(text_df, by = c("患者编号","就诊编号")) %>%
  mutate(现病史_截断 = stringr::str_trunc(现病史, 200))

# 复发==1 但 复发_严格==0（多为“症状再发”）
suspect_recur <- to_review %>%
  filter(复发 == 1L, 复发_严格 == 1L*0) %>%
  select(患者编号, 就诊编号, 结局_证据, 现病史_截断)

# 同条里既“好转/出院”又“加重进展/严格进展”
suspect_conflict <- to_review %>%
  filter((好转稳定 == 1L | 出院 == 1L) & (加重进展 == 1L | 加重进展_严格 == 1L)) %>%
  select(患者编号, 就诊编号, 结局_证据, 现病史_截断)


# admission_level：入院级数据（含 BMS_event / BMS_grade / 营养风险 / 协变量）
admission_level <- text_outcomes %>%
  select(患者编号, 就诊编号, 入院日期, time, BMS_event, BMS_grade,
         复发_严格, 加重进展_严格, 死亡, 好转或出院) %>%
  left_join(your_covariates_df, by = c("患者编号","就诊编号"))  # <- 你已有的协变量表

# 例：重复入院 → 混合效应逻辑回归（以 BMS_event 为因变量）
# library(lme4)
# m_bms <- glmer(BMS_event ~ 营养风险 + 年龄 + 性别 + 是否HyperCVAD + 基线HGB + 基线WBC + 基线PLT +
#                  (1 | 患者编号),
#                data = admission_level, family = binomial)
# summary(m_bms)



###### 还需控制治疗方案
# 方案字典（可继续补充）
regimen_dict <- list(
  "HyperCVAD-A" = c("Hyper\\s*CVAD[- ]?A","hyper\\s*cvad.*course\\s*A"),
  "HyperCVAD-B" = c("Hyper\\s*CVAD[- ]?B","hyper\\s*cvad.*course\\s*B"),
  "CVAD-B"      = c("CVAD[- ]?B"),
  "CAM"         = c("\\bCAM\\b"),
  "mM"          = c("\\bmM\\b"),
  "DVLD"        = c("\\bDVLD\\b"),
  "鞘注/腰穿"     = c("鞘注","腰穿\\+?鞘注"),
  # 多发骨髓瘤常见方案（如需）
  "VD"          = c("\\bVD\\b"),
  "VCD"         = c("\\bVCD\\b"),
  "VRD"         = c("\\bVRD\\b"),
  "VTD"         = c("\\bVTD\\b"),
  "CHOP"        = c("\\bCHOP\\b"),
  "R-CHOP"      = c("\\bR-?CHOP\\b")
)

# 日期正则：支持 2019-01-14 / 2019.01.14 / 09-06 / 9月6日
date_rx <- "(20\\d{2}[\\.\\-/年]\\s*\\d{1,2}[\\.\\-/月]\\s*\\d{1,2}日?)|(\\b\\d{1,2}[\\.-/]\\d{1,2}\\b)|(\\d{1,2}月\\d{1,2}日)"

# 从单条现病史抽取“方案事件”
extract_regimen_events_one <- function(text, admit_date = NA) {
  if (is.na(text) || !nzchar(text)) return(tibble(方案=character(), 日期原文=character(), 证据=character()))
  res <- list()
  for (nm in names(regimen_dict)) {
    rx <- str_c(regimen_dict[[nm]], collapse="|")
    locs <- str_locate_all(text, regex(rx, ignore_case = TRUE))[[1]]
    if (nrow(locs) == 0) next
    for (k in seq_len(nrow(locs))) {
      st <- locs[k,1]; ed <- locs[k,2]
      ctx <- substring(text, max(1, st-40), min(nchar(text), ed+40))
      # 就近找日期
      dt_match <- str_match(ctx, date_rx)[,1]
      res[[length(res)+1]] <- tibble(方案 = nm, 日期原文 = dt_match, 证据 = ctx)
    }
  }
  out <- if (length(res)) bind_rows(res) else tibble(方案=character(), 日期原文=character(), 证据=character())
  # 尝试解析日期；若缺年份，用入院年份补齐
  if (nrow(out)) {
    out <- out %>%
      mutate(
        年份 = if (!is.na(admit_date)) year(admit_date) else NA_integer_,
        日期_guess = case_when(
          # 2019-01-14 或 2019.01.14
          str_detect(日期原文 %||% "", "20\\d{2}") ~ suppressWarnings(ymd(str_replace_all(日期原文, "[年/月\\.]", "-"))),
          # 9月6日
          str_detect(日期原文 %||% "", "月") ~ suppressWarnings(
            ymd(str_glue("{coalesce(年份, NA_integer_)}-{str_match(日期原文, '(\\d{1,2})月')[,2]}-{str_match(日期原文, '月(\\d{1,2})日')[,2]}"))
          ),
          # 09-06
          str_detect(日期原文 %||% "", "^\\d{1,2}[\\.-/]\\d{1,2}$") ~ suppressWarnings(
            ymd(str_glue("{coalesce(年份, NA_integer_)}-{str_match(日期原文, '^(\\d{1,2})')[,2]}-{str_match(日期原文, '[\\.-/](\\d{1,2})$')[,2]}"))
          ),
          TRUE ~ as.Date(NA)
        )
      ) %>%
      select(-年份)
  }
  out
}

`%||%` <- function(a,b) if (is.null(a) || length(a)==0) b else a

# 应用到整表（若 text_df 里有 入院日期 列会自动利用）
has_admit <- "入院日期" %in% names(text_df)
chemo_events <- pmap_dfr(
  list(text_df$现病史, if (has_admit) text_df$入院日期 else rep(NA, nrow(text_df))),
  ~ {
    ev <- extract_regimen_events_one(..1, ..2)
    if (!nrow(ev)) return(NULL)
    tibble(患者编号 = NA, 就诊编号 = NA) # 占位
  }
)
# 绑定患者/就诊编号
chemo_events <- map2_dfr(
  seq_len(nrow(text_df)), 1:nrow(text_df),
  ~ {
    ev <- extract_regimen_events_one(text_df$现病史[.x], if (has_admit) text_df$入院日期[.x] else NA)
    if (!nrow(ev)) return(NULL)
    bind_cols(text_df[.x, c("患者编号","就诊编号")], ev)
  }
)
# —— 衍生一些可用于建模的特征（每次就诊级）
chemo_features <- chemo_events %>%
  group_by(患者编号, 就诊编号) %>%
  summarise(
    首个方案 = first(方案),
    是否HyperCVAD = as.integer(any(方案 %in% c("HyperCVAD-A","HyperCVAD-B"))),
    是否鞘注 = as.integer(any(方案 == "鞘注/腰穿")),
    方案个数 = n_distinct(方案),
    首次方案日期 = suppressWarnings(min(日期_guess, na.rm = TRUE)),
    方案列表 = str_c(unique(方案), collapse = "; "),
    .groups = "drop"
  )






# 保留患者现病史字数最多的行
data_geren3_unique <- data_geren3 %>%
  group_by(患者编号) %>%
  slice_max(nchar(现病史), n = 1) %>%
  ungroup()
# 查看结果
cat("去重前行数:", nrow(data_geren3), "\n")
cat("去重后行数:", nrow(data_geren3_unique), "\n")
# 对患者编号且现病史相同的行进行去重
data_geren3_unique <- data_geren3_unique %>%
  distinct(患者编号, 现病史, .keep_all = TRUE)




# 先匹配入院时间
# 将data_geren_updated的time和入院时间加入data_geren3，按照相同患者编号和就诊编号匹配
# 保留患者编号、就诊编号、time和入院日期
data_huanzhe <- data_geren_updated %>%
  select(患者编号, 就诊编号, time, 入院日期)
# 按照患者编号和就诊编号合并到data_geren3
data_geren3 <- data_geren3 %>%
  left_join(data_huanzhe, by = c("患者编号", "就诊编号"))

## 利用data_geren3的既往史，提取基线罹患的慢性病
# 根据ICD-10诊断标准制定慢性病词表

# 匹配，出现该慢性病则记为1，未出现则记为0



### 定义患者最常出现的慢性疾病，用诊断名称来评估该疾病的发展情况，疾病稳定记为1，疾病恶化记为2，疾病好转记为3，死亡记为4


### 需要人工核对，慢性疾病需加入病史的描述，可以作为单独的数据框






## 首先需要对诊断归转状态进行处理
# 将每个患者第一个入院日期的行，诊断归转情况的值改为入院
data_geren_updated <- data_geren %>%
  arrange(患者编号, 入院日期) %>%
  group_by(患者编号) %>%
  mutate(
    诊断归转情况 = ifelse(row_number() == 1, "入院", 诊断归转情况)
  ) %>%
  ungroup()

# 查看诊断归转情况为死亡的有多少行
death_count <- nutri_hematologic_updated %>%
  filter(诊断归转情况 == "死亡") %>%
  nrow()
cat("诊断归转情况为死亡的行数:", death_count, "\n")
# 查看诊断归转情况为NA的行数
na_count <- nutri_hematologic_updated %>%
  filter(is.na(诊断归转情况)) %>%
  nrow()
cat("诊断归转情况为NA的行数:", na_count, "\n")
table(nutri_hematologic_updated$诊断归转情况)




###### 3.2 进行landmark cox分析








######### module之间异方差，且采用标准化y值进行的分析，也没有锚定量表 
# 调整格式
nutri_merged <- nutri_merged %>%
  mutate(
    # 分组 & 因子
    patient    = factor(as.character(patient)),
    sex        = factor(sex),
    disease    = factor(disease),
    Marital    = factor(Marital),
    Profession = factor(Profession),
    # 连续变量
    age        = as.numeric(as.character(age)),
    log_time   = log1p(time),
    # 标准化（数值稳定）
    age_std        = as.numeric(scale(age)),
    log_time_std   = as.numeric(scale(log_time)),
    y_std          = as.numeric(scale(y))
  )

# 线性公式
formula1 <- bf(
  y_std ~ log_time_std + age_std + sex + disease + Marital + Profession +
    (1 + log_time_std || patient))
# 非线性公式
# 直接使用 df=3（≈4 个结点）自动样条，无需手动 knots
B_time <- ns(nutri_merged$log_time, df = 2)   # 时间样条（群体层）
colnames(B_time) <- paste0("ns_time_", 1:ncol(B_time))
nutri_spline_df <- dplyr::bind_cols(nutri_merged, as.data.frame(B_time))

formula2 <- bf(
  y_std ~ ns_time_1 + ns_time_2 +
    age_std + sex + disease + Marital + Profession +
    (1 + log_time_std || patient))

# 查看合适的先验分布（建议运行以核对默认）
default_prior(
  object = formula1,
  data = nutri_merged,
  family = gaussian()
)

default_prior(
  object = formula2,
  data = nutri_spline_df,
  family = gaussian()
)

# 更强的先验（略收缩，有助稳定）
priors_student <- c(
  # 固定效应（含 ns_time_*、age_std、性别/婚姻/职业/疾病虚拟变量）
  # y_std 的尺度是 1，给到 N(0,0.5) 的温和收缩，有助稳住采样
  set_prior("normal(0, 0.5)", class = "b"),
  
  # 截距：y_std 以 0 为中心，截距通常接近 0，稍宽松一点
  set_prior("student_t(3, 0, 1)", class = "Intercept"),
  
  # 患者层随机效应 SD（解相关 ||）——收缩到 ~[0,1] 量级
  set_prior("student_t(3, 0, 0.5)", class = "sd"),
  set_prior("student_t(3, 0, 0.5)", class = "sigma"),
  
  # Student-t 自由度 nu：适度重尾（均值≈20）；可用更保守的 exponential(1/30) 也行
  set_prior("gamma(2, 0.1)", class = "nu")
)

# 线性
fit_linear <- brm(
  formula = bf(
    y_std ~ log_time_std + age_std + sex + disease + Marital + Profession +
      (1 + log_time_std || patient)
  ),
  data = nutri_merged,
  family = student(),
  prior  = priors_student,
  chains = 4, iter = 2000, warmup = 1000, cores = 4,
  control = list(adapt_delta = 0.95, max_treedepth = 15),
  backend = "cmdstanr"
)
print(fit_linear)
# 样条（群体层样条 + 个体线性随机斜率）
fit_spline <- brm(
  formula = bf(
    y_std ~ ns_time_1 + ns_time_2 +
      age_std + sex + disease + Marital + Profession +
      (1 + log_time_std || patient)
  ),
  data = nutri_spline_df,
  family = student(),
  prior  = priors_student,
  chains = 4, iter = 2000, warmup = 1000, cores = 4,
  control = list(adapt_delta = 0.95, max_treedepth = 15),
  backend = "cmdstanr"
)
print(fit_spline)

######### 对照分析，gaussian() 误差
priors_gauss <- c(
  set_prior("normal(0, 0.5)", class = "b"),
  set_prior("student_t(3, 0, 1)", class = "Intercept"),
  set_prior("student_t(3, 0, 0.5)", class = "sd"),
  set_prior("student_t(3, 0, 0.5)", class = "sigma")
  # 无 nu 先验（仅 Student 家族需要）
)
fit_linear_gauss <- brm(
  y_std ~ log_time_std + age_std + sex + disease + Marital + Profession +
    (1 + log_time_std || patient),
  data = nutri_merged,
  family = gaussian(),
  prior  = priors_gauss,
  chains = 4, iter = 2000, warmup = 1000, cores = 4,
  control = list(adapt_delta = 0.95, max_treedepth = 15),
  backend = "cmdstanr"
)
print(fit_spline)
##  非线性
fit_spline_gauss <- brm(
  y_std ~ ns_time_1 + ns_time_2 +
    age_std + sex + disease + Marital + Profession +
    (1 + log_time_std || patient),
  data = nutri_spline_df,
  family = gaussian(),
  prior  = priors_gauss,
  chains = 4, iter = 2000, warmup = 1000, cores = 4,
  control = list(adapt_delta = 0.95, max_treedepth = 15),
  backend = "cmdstanr"
)
print(fit_linear)

#########  比较模型性能 
# 1) 收敛与取样质量 
check_sampling <- function(fit) {
  # 1) 抽样矩阵/数组（优先 cmdstanr 原生）
  draws <- tryCatch({
    if (!is.null(fit$fit)) {
      # cmdstanr backend
      fit$fit$draws()  # draws_array
    } else {
      # 其他后端
      posterior::as_draws_array(fit)
    }
  }, error = function(e) {
    # 最后兜底
    posterior::as_draws_array(fit)
  })
  
  # 2) 诊断量
  rhat_max     <- max(posterior::rhat(draws),     na.rm = TRUE)
  ess_bulk_min <- min(posterior::ess_bulk(draws), na.rm = TRUE)
  ess_tail_min <- min(posterior::ess_tail(draws), na.rm = TRUE)
  
  # 3) 发散次数（nuts_params 有时可能为 NULL）
  np  <- tryCatch(brms::nuts_params(fit), error = function(e) NULL)
  div <- if (is.null(np)) NA_integer_ else
    sum(np$Parameter == "divergent__" & np$Value == 1, na.rm = TRUE)
  
  data.frame(
    model        = deparse(substitute(fit)),
    rhat_max     = rhat_max,
    ess_bulk_min = ess_bulk_min,
    ess_tail_min = ess_tail_min,
    n_divergent  = div,
    stringsAsFactors = FALSE
  )
}

# 一次性跑两个模型
diag_tbl <- dplyr::bind_rows(
  check_sampling(fit_linear),
  check_sampling(fit_spline)
)
print(diag_tbl)

# 2) 后验预测检验（PPC） 
# 密度对比
pp_check(fit_linear, type = "dens_overlay", ndraws = 100)
pp_check(fit_spline, type = "dens_overlay", ndraws = 100)

# 随时间的区间覆盖（把时间变量名改成你的列名，这里用 log_time）
pp_check(fit_linear, type = "intervals", x = "log_time", ndraws = 200)
pp_check(fit_spline, type = "intervals", x = "log_time", ndraws = 200)

# PIT 直方图（均匀性越好越理想）
pp_check(fit_linear, type = "bars", ndraws = 200)
pp_check(fit_spline, type = "bars", ndraws = 200)

# 3) 拟合度指标：Bayes-R2、RMSE、MAE（样本内 + 后验均值）
fit_metrics <- function(fit, dat, yvar = "y") {
  # Bayes-R2
  r2 <- bayes_R2(fit, summary = TRUE)
  r2_mean <- r2[,"Estimate"]; r2_l <- r2[,"Q2.5"]; r2_u <- r2[,"Q97.5"]
  
  # 用后验均值做点预测
  yhat <- fitted(fit, summary = TRUE)[,"Estimate"]
  yobs <- dat[[yvar]]
  stopifnot(length(yhat) == length(yobs))
  
  rmse <- sqrt(mean((yobs - yhat)^2, na.rm = TRUE))
  mae  <- mean(abs(yobs - yhat), na.rm = TRUE)
  
  data.frame(
    model = deparse(substitute(fit)),
    bayes_R2 = r2_mean,
    bayes_R2_l95 = r2_l,
    bayes_R2_u95 = r2_u,
    RMSE_in = rmse,
    MAE_in = mae
  )
}
fit_tbl <- bind_rows(
  fit_metrics(fit_linear,  nutri_merged, yvar = "y"),
  fit_metrics(fit_spline,  nutri_merged, yvar = "y")
)
print(fit_tbl)

#  4) 泛化能力：LOO/ELPD、模型比较与权重
# 样条模型复杂，建议 moment_match=TRUE
loo_lin <- loo(fit_linear)
loo_spl <- loo(fit_spline)

print(loo_lin)
print(loo_spl)

# ELPD 差值（越大越好）；负值表示行名模型更好
print(loo_compare(loo_lin, loo_spl))

# stacking / pseudo-BMA 权重（解释“谁在预报上更好”）
mw_stack <- model_weights(list(fit_linear, fit_spline),
                          weights = "stacking", loo = list(loo_lin, loo_spl))
mw_pbma  <- model_weights(list(fit_linear, fit_spline),
                          weights = "pseudobma", BB = TRUE, loo = list(loo_lin, loo_spl))
data.frame(model = c("linear","spline"),
           w_stacking = mw_stack,
           w_pseudobma = mw_pbma) %>% print()




#######  1.5  Time-dependent Cox（连续动态）########



#######  1.6 联立模型 ##########



### 6.1动态营养状态对结局的即时风险动态营养状态对结局的即时风险 ###
# 去营养风险评分为NA的行
nutri_1 <- data_yinyang_model %>%
  # 对相同患者编号中各列值完全相同的行进行去重
  distinct() %>%
  # 去除营养风险评分为NA的行
  filter(!is.na(yinyang_score))
# 去营养风险评分为NA的行
nutri_2 <- data_fengxian_model %>%
  # 对相同患者编号中各列值完全相同的行进行去重
  distinct() %>%
  # 去除营养风险评分为NA的行
  filter(!is.na(fengxian_score))
# 检查nutri_1与nutri_2相同患者编号的数量
common_patients <- intersect(nutri_1$患者编号, nutri_2$患者编号)
cat("相同患者编号的数量:", length(common_patients), "\n")
# 去除BMI列
nutri_1 <- nutri_1 %>% select(-BMI)
# 重命名nutri_2的fengxian_score和fengxian_score_date为yinyang_score和yinyang_score_date
nutri_2 <- nutri_2 %>%
  rename(
    yinyang_score = fengxian_score,
    yinyang_score_date = fengxian_score_date)

# 对nutri_1与nutri_2进行合并，两个数据的列名均相同，相同患者编号且相同入院日期和疾病分类的行进行去重，对相同行保留第一个值即可
# 相同患者编号且相同入院日期和疾病分类的行进行去重，对相同行保留第一个值即可
nutri_3 <- bind_rows(nutri_1, nutri_2) %>%
  # 对相同患者编号且相同入院日期和疾病分类的行进行去重，保留第一个
  distinct(患者编号, 入院日期, 疾病分类, .keep_all = TRUE) %>%
  # 按患者编号和入院日期排序
  arrange(患者编号, 入院日期)
# 确认有多少患者
cat("总患者数:", n_distinct(nutri_3$患者编号), "\n")

# 构建血液恶性肿瘤词典
hematologic_subtypes <- list(
  "急性早幼粒细胞白血病" = c("急性早幼粒细胞白血病", "早幼粒细胞", "m3"),
  "急性髓系白血病" = c("急性髓系白血病"),
  "急性淋巴细胞白血病" = c("急性淋巴细胞白血病"),
  "慢性髓系白血病" = c("慢性髓系白血病"),
  "慢性淋巴细胞白血病" = c("慢性淋巴细胞白血病","慢性淋巴细胞性白血病"),
  "多发性骨髓瘤" = c("多发性骨髓瘤", "骨髓瘤"),
  # 先放“非霍奇金”
  "非霍奇金淋巴瘤" = c("非霍奇金淋巴瘤", "b细胞淋巴瘤", "t细胞淋巴瘤"),
  "霍奇金淋巴瘤" = c("霍奇金淋巴瘤"),
  "骨髓增生异常综合征" = c("骨髓增生异常综合征", "mds"),
  "其他白血病" = c("白血病", "其他白血病")
)
# 匹配
nutri_4 <- nutri_3 %>%
  mutate(
    disease_category = case_when(
      str_detect(诊断名称, regex(paste(hematologic_subtypes[["急性早幼粒细胞白血病"]], collapse = "|"), ignore_case = TRUE)) ~ "急性早幼粒细胞白血病",
      str_detect(诊断名称, regex(paste(hematologic_subtypes[["急性髓系白血病"]], collapse = "|"), ignore_case = TRUE)) ~ "急性髓系白血病",
      str_detect(诊断名称, regex(paste(hematologic_subtypes[["急性淋巴细胞白血病"]], collapse = "|"), ignore_case = TRUE)) ~ "急性淋巴细胞白血病",
      str_detect(诊断名称, regex(paste(hematologic_subtypes[["慢性髓系白血病"]], collapse = "|"), ignore_case = TRUE)) ~ "慢性髓系白血病",
      str_detect(诊断名称, regex(paste(hematologic_subtypes[["慢性淋巴细胞白血病"]], collapse = "|"), ignore_case = TRUE)) ~ "慢性淋巴细胞白血病",
      str_detect(诊断名称, regex(paste(hematologic_subtypes[["多发性骨髓瘤"]], collapse = "|"), ignore_case = TRUE)) ~ "多发性骨髓瘤",
      str_detect(诊断名称, regex(paste(hematologic_subtypes[["非霍奇金淋巴瘤"]], collapse = "|"), ignore_case = TRUE)) ~ "非霍奇金淋巴瘤",
      str_detect(诊断名称, regex(paste(hematologic_subtypes[["霍奇金淋巴瘤"]], collapse = "|"), ignore_case = TRUE)) ~ "霍奇金淋巴瘤",
      str_detect(诊断名称, regex(paste(hematologic_subtypes[["骨髓增生异常综合征"]], collapse = "|"), ignore_case = TRUE)) ~ "骨髓增生异常综合征",
      str_detect(诊断名称, regex(paste(hematologic_subtypes[["其他白血病"]], collapse = "|"), ignore_case = TRUE)) ~ "其他白血病",
      TRUE ~ NA_character_  # 没匹配上的记为NA
    )
  )

# 查看匹配结果
print("疾病分类统计:")
print(table(nutri_4$disease_category, useNA = "always"))

# 若一个患者的疾病分类均为NA，则保留这些行，并将疾病分类记为非血液恶性肿瘤
patient_na_status <- nutri_4 %>%
  group_by(患者编号) %>%
  summarise(
    all_na = all(is.na(disease_category)),  # 是否所有行都是NA
    .groups = "drop"
  )

nutri_5 <- nutri_4 %>%
  left_join(patient_na_status, by = "患者编号") %>%
  mutate(
    disease_category = ifelse(all_na & is.na(disease_category), "非血液恶性肿瘤", disease_category)
  ) %>%
  select(-all_na)
# 去除疾病分类为NA的行
nutri_final <- nutri_5 %>%
  filter(!is.na(disease_category))

# 新建一个数据框，只选择疾病分类为非血液恶性肿瘤的行
nutri_non_hematologic <- nutri_final %>%
  filter(disease_category == "非血液恶性肿瘤")

# 剔除疾病分类为非血液恶性肿瘤的行
nutri_hematologic <- nutri_final %>%
  filter(disease_category != "非血液恶性肿瘤")

# 查看有多少患者
cat("血液恶性肿瘤患者数:", n_distinct(nutri_hematologic$患者编号), "\n")
cat("非血液恶性肿瘤患者数:", n_distinct(nutri_non_hematologic$患者编号), "\n")

# 将每个患者第一个入院日期的行，诊断归转情况的值改为入院
nutri_hematologic_updated <- nutri_hematologic %>%
  arrange(患者编号, 入院日期) %>%
  group_by(患者编号) %>%
  mutate(
    诊断归转情况 = ifelse(row_number() == 1, "入院", 诊断归转情况)
  ) %>%
  ungroup()

# 查看诊断归转情况为死亡的有多少行
death_count <- nutri_hematologic_updated %>%
  filter(诊断归转情况 == "死亡") %>%
  nrow()
cat("诊断归转情况为死亡的行数:", death_count, "\n")
# 查看诊断归转情况为NA的行数
na_count <- nutri_hematologic_updated %>%
  filter(is.na(诊断归转情况)) %>%
  nrow()
cat("诊断归转情况为NA的行数:", na_count, "\n")
table(nutri_hematologic_updated$诊断归转情况)




# 查看疾病分类为NA的行，诊断名称出现次数最多的前二十个，以及对应患者数最多的前二十个，给出具体数量
nutri_5_na <- nutri_5 %>%
  filter(is.na(disease_category))
# 确认有多少患者数
length(unique(nutri_5_na$患者编号))
# 诊断名称出现次数最多的前二十个
top_diagnoses <- nutri_5_na %>%
  count(诊断名称, sort = TRUE) %>%
  head(20)
cat("诊断名称出现次数最多的前20个:\n")
print(top_diagnoses)
# 对应患者数最多的前二十个诊断名称
top_diagnoses_patients <- nutri_5_na %>%
  group_by(诊断名称) %>%
  summarise(
    patient_count = n_distinct(患者编号),
    .groups = "drop"
  ) %>%
  arrange(desc(patient_count)) %>%
  head(20)
cat("\n对应患者数最多的前20个诊断名称:\n")
print(top_diagnoses_patients)


# 查看疾病分类不为NA的行，诊断名称出现次数最多的前二十个，以及对应患者数最多的前二十个，给出具体数量
nutri_5_not_na <- nutri_5 %>%
  filter(!is.na(disease_category))
# 确认有多少患者数
patient_count <- n_distinct(nutri_5_not_na$患者编号)
cat("疾病分类不为NA的患者数:", patient_count, "\n")
# 诊断名称出现次数最多的前二十个
top_diagnoses <- nutri_5_not_na %>%
  count(诊断名称, name = "记录数", sort = TRUE) %>%
  head(20)
cat("\n诊断名称出现次数最多的前20个:\n")
print(top_diagnoses, n = 20)
# 对应患者数最多的前二十个诊断名称
top_diagnoses_patients <- nutri_5_not_na %>%
  group_by(诊断名称) %>%
  summarise(
    患者数 = n_distinct(患者编号),
    记录数 = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(患者数)) %>%
  head(20)
cat("\n对应患者数最多的前20个诊断名称:\n")
print(top_diagnoses_patients, n = 20)



### 6.2确认阈值（3分）对血液恶性肿瘤患者是否合适


### 确认data_pinggu_combined_1,data_pinggu_combined_2,data_pinggu_combined_3,data_pinggu_combined_4,data_pinggu_combined_5的score_data的范围
# 方法1: 使用基础R函数逐一检查每个数据框
cat("data_pinggu_combined_1 score_date 范围:\n")
print(range(data_pinggu_combined_1$score_date, na.rm = TRUE))

cat("\ndata_pinggu_combined_2 score_date 范围:\n")
print(range(data_pinggu_combined_2$score_date, na.rm = TRUE))

cat("\ndata_pinggu_combined_3 score_date 范围:\n")
print(range(data_pinggu_combined_3$score_date, na.rm = TRUE))

cat("\ndata_pinggu_combined_4 score_date 范围:\n")
print(range(data_pinggu_combined_4$score_date, na.rm = TRUE))

cat("\ndata_pinggu_combined_5 score_date 范围:\n")
print(range(data_pinggu_combined_5$score_date, na.rm = TRUE))



#### 6. 复核骨髓抑制的时间点是否正确(任意结局可参考复核) #####
# 选择text_outcomes的患者编号、就诊编号、BMS_event、BMS_日期、BMS_证据、time、入院日期列
selected_cols <- c("患者编号", "就诊编号", "BMS_event", "BMS_日期", "BMS_grade","BMS_证据", "time", "入院日期","出院日期")
BMS_data <- text_outcomes[, selected_cols]

# 选择BMS_event为1的行
BMS_positive <- BMS_data[BMS_data$BMS_event == 1, ]
# 检查是否找到了BMS_event为1的行
if(nrow(BMS_positive) > 0) {
  # 找出存在BMS_event为1的患者编号（确保转换为字符型）
  BMS_positive_patients <- unique(as.character(BMS_positive$患者编号))
  cat("存在BMS_event为1的患者编号:", BMS_positive_patients, "\n")
  
  # 保留这些患者编号的所有行（不仅限于BMS_event为1的行）
  # 确保两边都转换为字符型进行比较
  BMS_patients_all_data <- BMS_data[as.character(BMS_data$患者编号) %in% BMS_positive_patients, ]
  
  cat("筛选出的记录行数:", nrow(BMS_patients_all_data), "\n")
} else {
  cat("未找到BMS_event为1的记录\n")
  BMS_patients_all_data <- BMS_data[0, ]  # 创建空数据框但保持结构
}
# 将这些患者的所有数据同原始文本text_df进行匹配
if(nrow(BMS_patients_all_data) > 0) {
  BMS_matched <- merge(BMS_patients_all_data, text_df, by = c("患者编号", "就诊编号"), all.x = TRUE)
} else {
  BMS_matched <- BMS_data[0, ]  # 保持列结构的空数据框
}

str(text_outcomes)
# 在text_outcomes中填入值，封装函数
# 定义更新BMS数据的函数（最终修正版本）
update_BMS_data <- function(text_outcomes_df, patient_id, visit_id, new_BMS_grade, new_BMS_date, new_BMS_event = NULL) {
  # 在text_outcomes中填入值
  # 选择患者号为xx，就诊编号为xx的行
  
  # 将该行的BMS_grade的值填为xx，BMS_日期的值填为xx
  row_index <- which(text_outcomes_df$患者编号 == patient_id & text_outcomes_df$就诊编号 == visit_id)
  
  # 检查是否找到了匹配的行
  if(length(row_index) > 0) {
    # 处理BMS_grade的数据类型转换
    if(is.na(new_BMS_grade) || new_BMS_grade == "") {
      text_outcomes_df[row_index, "BMS_grade"] <- NA_integer_
    } else {
      # 尝试转换为整数
      tryCatch({
        text_outcomes_df[row_index, "BMS_grade"] <- as.integer(new_BMS_grade)
      }, warning = function(w) {
        cat("警告: BMS_grade值", new_BMS_grade, "转换为整数时出现问题\n")
        text_outcomes_df[row_index, "BMS_grade"] <- NA_integer_
      }, error = function(e) {
        cat("错误: BMS_grade值", new_BMS_grade, "无法转换为整数\n")
        text_outcomes_df[row_index, "BMS_grade"] <- NA_integer_
      })
    }
    
    # 处理BMS_日期的数据类型转换
    if(is.na(new_BMS_date) || new_BMS_date == "") {
      text_outcomes_df[row_index, "BMS_日期"] <- NA_Date_
    } else {
      # 尝试转换为日期
      tryCatch({
        text_outcomes_df[row_index, "BMS_日期"] <- as.Date(new_BMS_date)
      }, warning = function(w) {
        cat("警告: BMS_日期值", new_BMS_date, "转换为日期时出现问题\n")
        text_outcomes_df[row_index, "BMS_日期"] <- NA_Date_
      }, error = function(e) {
        cat("错误: BMS_日期值", new_BMS_date, "无法转换为日期\n")
        text_outcomes_df[row_index, "BMS_日期"] <- NA_Date_
      })
    }
    
    # 如果提供了BMS_event值，则更新该列
    if(!is.null(new_BMS_event)) {
      if(is.na(new_BMS_event) || new_BMS_event == "") {
        text_outcomes_df[row_index, "BMS_event"] <- NA_integer_
      } else {
        # 尝试转换为整数
        tryCatch({
          text_outcomes_df[row_index, "BMS_event"] <- as.integer(new_BMS_event)
        }, warning = function(w) {
          cat("警告: BMS_event值", new_BMS_event, "转换为整数时出现问题\n")
          text_outcomes_df[row_index, "BMS_event"] <- NA_integer_
        }, error = function(e) {
          cat("错误: BMS_event值", new_BMS_event, "无法转换为整数\n")
          text_outcomes_df[row_index, "BMS_event"] <- NA_integer_
        })
      }
      cat("BMS_event更新为:", new_BMS_event, "\n")
    }
    
    cat("成功更新患者", patient_id, "就诊", visit_id, "的数据\n")
    cat("BMS_grade更新为:", new_BMS_grade, "\n")
    cat("BMS_日期更新为:", new_BMS_date, "\n")
    
    # 显示更新后的数据
    cat("更新后的记录:\n")
    columns_to_show <- c("患者编号", "就诊编号", "BMS_event", "BMS_grade", "BMS_日期")
    existing_columns <- columns_to_show[columns_to_show %in% names(text_outcomes_df)]
    print(text_outcomes_df[row_index, existing_columns])
  } else {
    cat("未找到患者", patient_id, "就诊", visit_id, "的记录\n")
  }
  
  # 返回更新后的数据框
  return(text_outcomes_df)
}

# 也相应修正清除数据的函数
clear_BMS_data_by_date <- function(text_outcomes_df, patient_id, bms_date) {
  # 将text_outcomes中指定患者编号且BMS_日期为指定日期的行的BMS_日期、BMS_grade、BMS_证据全部改为NA
  # 并将BMS_event改为0
  
  # 找到需要修改的行
  rows_to_modify <- which(text_outcomes_df$患者编号 == patient_id & text_outcomes_df$BMS_日期 == bms_date)
  
  if(length(rows_to_modify) > 0) {
    # 将指定列改为NA（根据列的类型使用相应的NA）
    text_outcomes_df[rows_to_modify, "BMS_日期"] <- NA_Date_
    text_outcomes_df[rows_to_modify, "BMS_grade"] <- NA_integer_
    text_outcomes_df[rows_to_modify, "BMS_证据"] <- NA_character_
    # 将BMS_event改为0
    text_outcomes_df[rows_to_modify, "BMS_event"] <- 0L
    
    cat("成功将患者", patient_id, "BMS_日期为", bms_date, "的记录修改\n")
    cat("BMS_日期、BMS_grade、BMS_证据已设为NA，BMS_event已设为0\n")
    cat("共修改了", length(rows_to_modify), "行记录\n")
    
    # 显示修改后的数据
    cat("修改后的记录:\n")
    BMS_columns <- c("患者编号", "就诊编号", "BMS_event", "BMS_grade", "BMS_日期", "BMS_证据")
    existing_columns <- BMS_columns[BMS_columns %in% names(text_outcomes_df)]
    print(text_outcomes_df[rows_to_modify, existing_columns])
  } else {
    cat("未找到患者", patient_id, "BMS_日期为", bms_date, "的记录\n")
  }
  
  # 返回更新后的数据框
  return(text_outcomes_df)
}

# 开始人工核对
# 第37行时间不一致
cat(as.character(BMS_matched[37, "现病史"]), "\n\n")
# 第222行时间不一致
cat(as.character(BMS_matched[222, "现病史"]), "\n\n")
# 入院时间与病史描述不一致，查看
patient_id_to_check <- "0000003102"
selected_patient_data <- data_geren2[data_geren2$患者编号 == patient_id_to_check, ]
# 查到最早入院是2016-2-18，因此骨髓抑制发生在这个期间，修改时间
# 0000003102，ZY010000959435，
text_outcomes <- update_BMS_data(text_outcomes, "0000003102", "ZY010000959435", "4", "2016-03-01","1")
# 其他的改为NA
text_outcomes <- clear_BMS_data_by_date(text_outcomes, "0000003102", "2015-02-27")
# 查看是否修改
patient_id_to_check <- "0000003102"
selected_patient_data <- text_outcomes[text_outcomes$患者编号 == patient_id_to_check, ]

# 继续人工核对
# 查看第二个不同的患者编号中，BMS_event为1的现病史和第一次BMS_event为0的就诊编号和现病史
# 先看看类型
sapply(BMS_matched[c("入院日期","BMS_日期")], class)
#  小工具：安全打印日期（Date 或 NA） 
fmt_date <- function(x) {
  if (is.na(x)) "NA" else format(x, "%Y-%m-%d")
}
# 获取存在 BMS_event 为 1 的唯一患者编号
BMS_positive_patients <- unique(as.character(BMS_matched[BMS_matched$BMS_event == 1, "患者编号"]))

###### （2）检查是否有第二个患者 #######
if (length(BMS_positive_patients) >= 2) {
  # 获取第二个患者编号
  second_patient_id <- BMS_positive_patients[2]
  cat("第二个存在BMS_event为1的患者编号:", second_patient_id, "\n\n")
  
  # 该患者一条 BMS_event == 1 的记录 
  second_patient_BMS_1 <- BMS_matched[BMS_matched$患者编号 == second_patient_id & 
                                        BMS_matched$BMS_event == 1, ]
  if (nrow(second_patient_BMS_1) > 0) {
    cat("该患者一条BMS_event为1的记录:\n")
    cat("就诊编号:", second_patient_BMS_1[1, "就诊编号"], "\n")
    cat("BMS_grade:", second_patient_BMS_1[1, "BMS_grade"], "\n")
    cat("BMS_日期:", fmt_date(second_patient_BMS_1[1, "BMS_日期"]), "\n")
    cat("入院日期:", fmt_date(second_patient_BMS_1[1, "入院日期"]), "\n")
    cat("现病史内容:\n")
    cat(as.character(second_patient_BMS_1[1, "现病史"]), "\n\n")
  }
  
  # 该患者一条 BMS_event == 0 的记录（按入院日期最早）
  second_patient_BMS_0 <- BMS_matched[BMS_matched$患者编号 == second_patient_id & 
                                        BMS_matched$BMS_event == 0, ]
  if (nrow(second_patient_BMS_0) > 0) {
    # 直接用 Date 排序（不要 as.numeric）
    second_patient_BMS_0 <- second_patient_BMS_0[order(second_patient_BMS_0$入院日期), ]
    
    cat("该患者一条BMS_event为0的记录:\n")
    cat("就诊编号:", second_patient_BMS_0[1, "就诊编号"], "\n")
    cat("BMS_grade:", second_patient_BMS_0[1, "BMS_grade"], "\n")
    cat("BMS_日期:", fmt_date(second_patient_BMS_0[1, "BMS_日期"]), "\n")
    cat("入院日期:", fmt_date(second_patient_BMS_0[1, "入院日期"]), "\n")
    cat("现病史内容:\n")
    cat(as.character(second_patient_BMS_0[1, "现病史"]), "\n\n")
  } else {
    cat("该患者没有BMS_event为0的记录\n\n")
  }
} else {
  cat("总共只有", length(BMS_positive_patients), "个患者存在BMS_event为1的记录\n")
}
# 查找BMS_matched数据中0017525420患者中离2016-06-09最近的入院日期和相应行的就诊编号
target_date <- as.Date("2016-06-09")
patient_id <- "0017525420"

target_date <- as.Date("2016-09-26")
patient_id <- "0017525420"

# 筛选患者记录
patient_records <- BMS_matched[BMS_matched$患者编号 == patient_id, ]
if(nrow(patient_records) > 0) {
  # 处理日期列
  if(is.numeric(patient_records$入院日期)) {
    admission_dates <- as.Date(as.character(patient_records$入院日期))
  } else {
    admission_dates <- as.Date(patient_records$入院日期)
  }
  # 找到最近的记录
  date_diffs <- abs(as.numeric(admission_dates - target_date))
  closest_index <- which.min(date_diffs)
  
  cat("最近的就诊记录:\n")
  cat("就诊编号:", patient_records[closest_index, "就诊编号"], "\n")
  cat("入院日期:", fmt_date(admission_dates[closest_index]), "\n")
  cat("相差天数:", date_diffs[closest_index], "天\n")
} else {
  cat("未找到该患者记录\n")
}

# 修改0017525420患者,就诊编号为ZY010000988019 ，2016-06-09
text_outcomes <- update_BMS_data(text_outcomes, "0017525420", "ZY010000988019", "4", "2016-06-09","1")
# 修改0017525420患者,就诊编号为ZY010000988019 ，2016-09-26
text_outcomes <- update_BMS_data(text_outcomes, "0017525420", "ZY010001011995", "4", "2016-09-26","1")
# 对于已有的日期为2017-09-06的改为NA
text_outcomes <- clear_BMS_data_by_date(text_outcomes, "0017525420", "2017-09-06 ") 	

# 查看是否修改
patient_id_to_check <- "0017525420"
selected_patient_data <- text_outcomes[text_outcomes$患者编号 == patient_id_to_check, ]

###### （3）查看第三个患者编号 ######
if (length(BMS_positive_patients) >= 3) {
  # 获取第四个患者编号
  second_patient_id <- BMS_positive_patients[3]
  cat("第四个存在BMS_event为1的患者编号:", second_patient_id, "\n\n")
  
  # 该患者一条 BMS_event == 1 的记录 
  second_patient_BMS_1 <- BMS_matched[BMS_matched$患者编号 == second_patient_id & 
                                        BMS_matched$BMS_event == 1, ]
  if (nrow(second_patient_BMS_1) > 0) {
    cat("该患者一条BMS_event为1的记录:\n")
    cat("就诊编号:", second_patient_BMS_1[1, "就诊编号"], "\n")
    cat("BMS_grade:", second_patient_BMS_1[1, "BMS_grade"], "\n")
    cat("BMS_日期:", fmt_date(second_patient_BMS_1[1, "BMS_日期"]), "\n")
    cat("入院日期:", fmt_date(second_patient_BMS_1[1, "入院日期"]), "\n")
    cat("现病史内容:\n")
    cat(as.character(second_patient_BMS_1[1, "现病史"]), "\n\n")
  }
  
  # 该患者一条 BMS_event == 0 的记录（按入院日期最早）
  second_patient_BMS_0 <- BMS_matched[BMS_matched$患者编号 == second_patient_id & 
                                        BMS_matched$BMS_event == 0, ]
  if (nrow(second_patient_BMS_0) > 0) {
    # 直接用 Date 排序（不要 as.numeric）
    second_patient_BMS_0 <- second_patient_BMS_0[order(second_patient_BMS_0$入院日期), ]
    
    cat("该患者一条BMS_event为0的记录:\n")
    cat("就诊编号:", second_patient_BMS_0[1, "就诊编号"], "\n")
    cat("BMS_grade:", second_patient_BMS_0[1, "BMS_grade"], "\n")
    cat("BMS_日期:", fmt_date(second_patient_BMS_0[1, "BMS_日期"]), "\n")
    cat("入院日期:", fmt_date(second_patient_BMS_0[1, "入院日期"]), "\n")
    cat("现病史内容:\n")
    cat(as.character(second_patient_BMS_0[1, "现病史"]), "\n\n")
  } else {
    cat("该患者没有BMS_event为0的记录\n\n")
  }
} else {
  cat("总共只有", length(BMS_positive_patients), "个患者存在BMS_event为1的记录\n")
}
# 只用改分级
text_outcomes <- update_BMS_data(text_outcomes, "0017621527", "ZY010001188627", "3", "2016-09-28","1")

###### （4）检查是否有第四个患者 #######
if (length(BMS_positive_patients) >= 4) {
  # 获取第二个患者编号
  second_patient_id <- BMS_positive_patients[4]
  cat("第二个存在BMS_event为1的患者编号:", second_patient_id, "\n\n")
  
  # 该患者一条 BMS_event == 1 的记录 
  second_patient_BMS_1 <- BMS_matched[BMS_matched$患者编号 == second_patient_id & 
                                        BMS_matched$BMS_event == 1, ]
  if (nrow(second_patient_BMS_1) > 0) {
    cat("该患者一条BMS_event为1的记录:\n")
    cat("就诊编号:", second_patient_BMS_1[1, "就诊编号"], "\n")
    cat("BMS_grade:", second_patient_BMS_1[1, "BMS_grade"], "\n")
    cat("BMS_日期:", fmt_date(second_patient_BMS_1[1, "BMS_日期"]), "\n")
    cat("入院日期:", fmt_date(second_patient_BMS_1[1, "入院日期"]), "\n")
    cat("现病史内容:\n")
    cat(as.character(second_patient_BMS_1[1, "现病史"]), "\n\n")
  }
  
  # 该患者一条 BMS_event == 0 的记录（按入院日期最早）
  second_patient_BMS_0 <- BMS_matched[BMS_matched$患者编号 == second_patient_id & 
                                        BMS_matched$BMS_event == 0, ]
  if (nrow(second_patient_BMS_0) > 0) {
    # 直接用 Date 排序（不要 as.numeric）
    second_patient_BMS_0 <- second_patient_BMS_0[order(second_patient_BMS_0$入院日期), ]
    
    cat("该患者一条BMS_event为0的记录:\n")
    cat("就诊编号:", second_patient_BMS_0[1, "就诊编号"], "\n")
    cat("BMS_grade:", second_patient_BMS_0[1, "BMS_grade"], "\n")
    cat("BMS_日期:", fmt_date(second_patient_BMS_0[1, "BMS_日期"]), "\n")
    cat("入院日期:", fmt_date(second_patient_BMS_0[1, "入院日期"]), "\n")
    cat("现病史内容:\n")
    cat(as.character(second_patient_BMS_0[1, "现病史"]), "\n\n")
  } else {
    cat("该患者没有BMS_event为0的记录\n\n")
  }
} else {
  cat("总共只有", length(BMS_positive_patients), "个患者存在BMS_event为1的记录\n")
}

# 修改0017525420患者,就诊编号为ZY010000988019 ，2016-06-09
text_outcomes <- update_BMS_data(text_outcomes, "0017525420", "ZY010000988019", "4", "2016-06-09","1")
# 修改0017525420患者,就诊编号为ZY010000988019 ，2016-09-26
text_outcomes <- update_BMS_data(text_outcomes, "0017525420", "ZY010001011995", "4", "2016-09-26","1")
# 对于已有的日期为2017-09-06的改为NA
text_outcomes <- clear_BMS_data_by_date(text_outcomes, "0017525420", "2017-09-06 ") 	

# 查看是否修改
patient_id_to_check <- "0017633099"
selected_patient_data <- text_outcomes[text_outcomes$患者编号 == patient_id_to_check, ]


