library(brms)
library(loo)
library(posterior)
library(bayesplot)
library(dplyr)
library(ggplot2)
conflict_prefer("ar", "brms") 
conflicts_prefer(patchwork::area)  # 如果你需要 patchwork 的布局函数
conflicts_prefer(posterior::rhat)
conflicted::conflicts_prefer(posterior::sd)
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
intersect_df <- melt(intersect_matrix)
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

# 合并（此步仍先不放 SGA）
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
  
  # ??? 全局 sigma 只有截距：给 dpar = "sigma" 的截距设先验
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
  select(patient, module, age_std, sex, disease, Marital, Profession,
         time, log_time_std, ns_time_1, ns_time_2)

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
  left_join(b0_map, by = "patient") %>%
  select(
    obs_id,
    patient, time, module, age_std, sex, disease, Marital, Profession,
    theta_mean, theta_q025, theta_q975,
    slope_mean, slope_q025, slope_q975,
    b0_mean
  )

# 写文件
saveRDS(landmark_features, "landmark_features_theta_slope_b0.rds")
# 导入文件


#######  3.1 为landmar cox做数据准备 #######
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
table(meas_long$treatment)

########## 3.2 选择性保留诊断名称 #########
### 3.2.1 选择诊断名称为





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

#########  1.2 比较模型性能 ########
# ===== 1) 收敛与取样质量 =====
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

# ===== 2) 后验预测检验（PPC） =====
# 密度对比
pp_check(fit_linear, type = "dens_overlay", ndraws = 100)
pp_check(fit_spline, type = "dens_overlay", ndraws = 100)

# 随时间的区间覆盖（把时间变量名改成你的列名，这里用 log_time）
pp_check(fit_linear, type = "intervals", x = "log_time", ndraws = 200)
pp_check(fit_spline, type = "intervals", x = "log_time", ndraws = 200)

# PIT 直方图（均匀性越好越理想）
pp_check(fit_linear, type = "bars", ndraws = 200)
pp_check(fit_spline, type = "bars", ndraws = 200)

# ===== 3) 拟合度指标：Bayes-R2、RMSE、MAE（样本内 + 后验均值） =====
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

# ===== 4) 泛化能力：LOO/ELPD、模型比较与权重 =====
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



####### 1.3 处理生存预后数据  ############




####### 1.4 Landmark Cox（临床时点决策）########




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
