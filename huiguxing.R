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

library(conflicted)
conflict_prefer("select", "dplyr")  # 明确指定选择 dplyr 中的 `select` 函数
conflict_prefer("filter", "dplyr")  # 明确指定选择 dplyr 中的 `filter` 函数 
conflicts_prefer(dplyr::first)

################### 2.回顾性重构DMI ################### 
###### 2.1读取数据 ######
# 初诊AML患者数据
data_AML <- read.csv("F:/血液科数据/初诊AML患者/data_AML.csv")
length(unique(data_AML$患者编号))
length(unique(data_AML$检验子项英文名))

# 导入一阶段清洗后的数据，包含护理记录
data_huli_cleaned <- read.csv("F:/血液科数据/一阶段清洗后数据/data_huli_cleaned.csv")
data_huli_combined6 <- read.csv("F:/血液科数据/一阶段清洗后数据/data_huli_combined6.csv")

# 确认相同的患者编号
data_huli <- data_AML%>%
  left_join(data_huli_combined6, by = "患者编号")

length(unique(data_huli$患者编号))

#### 先处理营养和护理数据
# 保留第1，5，6，7列
data_yinyang <- data_huli_combined6[,c(1,5,6,7)]
# 检查是否为数值型
data_yinyang$总评分 <- as.numeric(data_yinyang$总评分)

# 对每位患者在同一护理记录类型下的评分按日期取平均
data_yinyang_avg <- data_yinyang %>%
  group_by(患者编号, 记录日期, 护理记录类型) %>%
  summarise(平均总评分 = mean(总评分, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = 护理记录类型, values_from = 平均总评分)
# 检查每列缺失值
colSums(is.na(data_yinyang_avg))

data_yinyang_avg$填写项数量 <- rowSums(!is.na(data_yinyang_avg[, -c(1, 2)]))
######## 展示一下数据
# 打印data_AML个人史第一行的值
data_AML$个人史[1]
data_AML$既往史[1]
data_AML$现病史[1]
data_AML$婚姻史[1]
data_AML$家族史[1]
data_AML$生育史[1]
data_AML$月经史[1]
#  展示实验名称
unique(data_AML$检验子项中文名)
# 选择护理记录类型为营养风险筛查表(成人)的数据
data_1<- data_yinyang %>%
  filter(护理记录类型 %in% c("营养风险筛查表(成人)"))
length(unique(data_1$患者编号))


######### 处理实验标本数据，data_shiyan是AML的实验标本数据
#保留第1,17,19,27,28,29,30,31列
data_shiyan <- data_huli[,c(1,17,19,27,28,29,30,31)]
# 检查每列缺失值
colSums(is.na(data_shiyan))
# 确认为character(因为有一些文字表述)
data_shiyan$检验子项结果 <- as.character(data_shiyan$检验子项结果)

# 对每位患者在同一检验子项中文名下的检验子项结果数值按日期取平均
data_shiyan_avg <- data_shiyan %>%
  group_by(患者编号, 采集时间, 检验子项英文名) %>%
  summarise(Result = first(检验子项结果, na.rm = TRUE),.groups = "drop") %>%
  pivot_wider(names_from = 检验子项英文名, values_from = Result)
# 检查每列缺失值  
colSums(is.na(data_shiyan_avg))
length(is.na(data_shiyan_avg$GGT))
# 选择
data_2 <- data_shiyan %>%
  group_by(检验子项英文名) %>%
  summarise(患者数 = n_distinct(患者编号)) %>%
  arrange(desc(患者数))

#### 2.2抽取常住地、籍贯与出生地 #####
# 对血液恶性肿瘤患者数据处理
# 保留第1，4，8，24，29列
data_zhuzai <- data_huli_cleaned[,c(1,4,8,24,29)]

# 提取个人史中包含“经常留居地:”后的内容
data_zhuzai$常住地 <- str_extract(data_zhuzai$个人史, "(?<=经常留居地[:：])[^\n；；，,]*")

# 将同一患者编号的诊断名称合并到一个框里，相同的诊断名称保留一个
data_zhuzai_combined <- data_zhuzai %>%
  group_by(患者编号) %>%
  summarise(
    诊断名称 = paste(unique(诊断名称), collapse = "；"),
    籍贯 = first(籍贯),
    出生地 = first(出生地),
    常住地 = first(常住地)
  )

# 引入中文行政区划词典表
admin_mapping_raw <- fromJSON("F:/血液科数据/行政区划/location.json")
readLines("F:/血液科数据/行政区划/location.json", n = 10)

# 提取 provinces 列表
provinces_list <- admin_mapping_raw$provinces
# 提取 city_mapping
provinces_df <- admin_mapping_raw$provinces
city_mapping <- do.call(rbind, lapply(1:nrow(provinces_df), function(i) {
  province_name <- provinces_df$provinceName[i]
  city_df <- provinces_df$citys[[i]]  # 这是一个 data.frame，含 cityName 和 cityType
  if (nrow(city_df) == 0) return(NULL)  # 跳过空行
  data.frame(
    省 = rep(province_name, nrow(city_df)),
    市 = city_df$cityName,
    stringsAsFactors = FALSE
  )
})) %>%
  mutate(标准化市级 = paste0(省, 市))
# 查看前几行结果
head(city_mapping)

# 定义模糊匹配函数
standardize_city_fuzzy_improved <- function(location, mapping) {
  if (is.na(location) || location == "") return(NA)
  location <- gsub("\\s+", "", location)
  location <- gsub("[：:，,。；;]", "", location)
  
  for (i in seq_len(nrow(mapping))) {
    city <- mapping$市[i]
    city_short <- gsub("市$", "", city)  # 去掉“市”字做模糊匹配
    full <- mapping$标准化市级[i]
    
    if (grepl(city_short, location) || grepl(city, location)) {
      return(full)
    }
  }
  return(NA)
}

# 应用于数据
# 对“常住地”字段进行标准化
data_zhuzai_combined$常住地市级 <- sapply(data_zhuzai_combined$常住地, function(loc) {
  standardize_city_fuzzy_improved(loc, city_mapping)
})

# 可重复用于“出生地”“籍贯”等字段
data_zhuzai_combined$出生地市级 <- sapply(data_zhuzai_combined$籍贯, function(loc) {
  standardize_city_fuzzy_improved(loc, city_mapping)
})

# 出生地2，尽量让籍贯和出生地的信息给足
data_zhuzai_combined$出生地2 <- sapply(data_zhuzai_combined$出生地, function(loc) {
  standardize_city_fuzzy_improved(loc, city_mapping)
})

##### 当某一行的 常住地市级 是 NA，但该行的 出生地市级 有有效值时，就用 出生地市级 的值来填充 常住地市级
data_zhuzai_combined$常住地市级 <- ifelse(
  is.na(data_zhuzai_combined$常住地市级) & !is.na(data_zhuzai_combined$出生地市级),
  data_zhuzai_combined$出生地市级,
  data_zhuzai_combined$常住地市级
)

# 补足出生地市级的信息（这可能存在问题，但是这是最便捷的办法）
data_zhuzai_combined$出生地市级 <- ifelse(
  is.na(data_zhuzai_combined$出生地市级) & !is.na(data_zhuzai_combined$出生地2),
  data_zhuzai_combined$出生地2,
  data_zhuzai_combined$出生地市级
)

data_zhuzai_combined$出生地市级 <- ifelse(
  is.na(data_zhuzai_combined$出生地市级) & !is.na(data_zhuzai_combined$常住地市级),
  data_zhuzai_combined$常住地市级,
  data_zhuzai_combined$出生地市级
)

## 提取常住地市级为NA的行，可以做后续人工匹配，或者街道乡镇匹配
data_zhuzai_combined_na <- data_zhuzai_combined %>%
  filter(is.na(常住地市级))
# 剔除常住地市级为NA的行
data_zhuzai_combined <- data_zhuzai_combined %>%
  filter(!is.na(常住地市级))


##### 2.3 对诊断名称进行语义提取和匹配#####
# 定义恶性血液肿瘤关键词字典（可修改/扩展）
hematologic_subtypes <- list(
  "急性髓系白血病" = c("急性髓系白血病", "AML", "M3", "M2"),
  "急性淋巴细胞白血病" = c("急性淋巴细胞白血病", "ALL", "B-ALL", "T-ALL"),
  "慢性髓系白血病" = c("慢性髓系白血病", "CML"),
  "慢性淋巴细胞白血病" = c("慢性淋巴细胞白血病", "CLL"),
  "多发性骨髓瘤" = c("多发性骨髓瘤", "MM"),
  "淋巴瘤" = c("霍奇金淋巴瘤","非霍奇金淋巴瘤", "淋巴瘤", "B细胞淋巴瘤", "T细胞淋巴瘤"),
  "骨髓增生异常综合征" = c("MDS", "骨髓增生异常综合征"),
  "其他白血病" = c("白血病","其他白血病")  # 兜底
)

#  分类函数：按优先顺序匹配
assign_subtype <- function(diagnosis_text, subtype_dict) {
  for (subtype in names(subtype_dict)) {
    keywords <- subtype_dict[[subtype]]
    for (keyword in keywords) {
      if (str_detect(diagnosis_text, keyword)) {
        return(subtype)
      }
    }
  }
  return("未分类")
}

# 应用
data_zhuzai_combined$疾病分类 <- sapply(data_zhuzai_combined$诊断名称, function(x) {
  assign_subtype(x, hematologic_subtypes)
})

# 显示第2列的第2行
data_zhuzai_combined$诊断名称[4]
# 确认疾病分类列每个值的个数
table(data_zhuzai_combined$疾病分类)

## 考虑获取常住地的英文描述
### 或者不导出，直接unique
unique(data_zhuzai_combined$出生地市级)
unique(data_zhuzai_combined$常住地市级)

# 导入location英文版
location_en <- read_excel("F:/血液科数据/行政区划/location.xlsx")
# 去除第一列和第三列值中的“”,如去除"湖北省武汉市"两端的“”
location_en[, c(1, 3)] <- lapply(location_en[, c(1, 3)], function(x) gsub('^"|"$', '', x))
# 分割，保留第1，2列
changzhudi <-  location_en[, c(1, 2)]
chushengdi <-  location_en[, c(3, 4)]

# 剔除NA值
changzhudi <- changzhudi %>%
  filter(!is.na(常住地)) 

# 分别进行合并
data_zhuzai_combined <- data_zhuzai_combined %>%
  left_join(changzhudi, by = c("常住地市级" = "常住地")) 
data_zhuzai_combined <- data_zhuzai_combined %>%
  left_join(chushengdi, by = c("出生地市级" = "出生地")) 

############ 2.4可视化 #############
# 2.4.1 指定多发性骨髓瘤
disease_focus <- "多发性骨髓瘤"

# 获取该疾病下常住地分布前 10 地点
focus_data <- data_zhuzai_combined %>%
  filter(疾病分类 == disease_focus, !is.na(常住地市级)) %>%
  count(常住地市级,常住地英文, name = "人数") %>%
  slice_max(order_by = 人数, n = 10)

# 绘图
p <- ggplot(focus_data, aes(x = reorder(常住地英文, 人数), y = 人数)) +
  geom_col(fill = "#4682B4") +
  coord_flip() +
  geom_text(aes(label = 人数), hjust = -0.2, size = 4) +
  labs(
    title = paste0( "Top 10 Permanent Residents of Multiple Myeloma Patients"),
    x = NULL, y = "Number of patients"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))
p
ggsave("多发性骨髓瘤患者常住地分布.pdf", plot = p, device = "pdf", width = 10, height = 10)

# 2.4.2 指定急性髓系白血病
disease_focus2 <- "急性髓系白血病"
# 获取该疾病下常住地分布前 10 地点
focus_data2 <- data_zhuzai_combined %>%
  filter(疾病分类 == disease_focus2, !is.na(常住地市级)) %>%
  count(常住地市级,常住地英文, name = "人数") %>%
  slice_max(order_by = 人数, n = 10)

# 绘图
p2 <- ggplot(focus_data2, aes(x = reorder(常住地英文, 人数), y = 人数)) +
  geom_col(fill = "#4682B4") +
  coord_flip() +
  geom_text(aes(label = 人数), hjust = -0.2, size = 4) +
  labs(
    title = paste0( "Top 10 Permanent Residents of Acute Myeloid Leukemia Patients"),
    x = NULL, y = "Number of patients"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))
p2
ggsave("急性髓系白血病患者常住地分布.pdf", plot = p2, device = "pdf", width = 10, height = 10)

# 2.4.3 指定急性淋巴细胞白血病
disease_focus3 <- "急性淋巴细胞白血病"
# 获取该疾病下常住地分布前 10 地点
focus_data3 <- data_zhuzai_combined %>%
  filter(疾病分类 == disease_focus3, !is.na(常住地市级)) %>%
  count(常住地市级,常住地英文, name = "人数") %>%
  slice_max(order_by = 人数, n = 10)
# 绘图
p3 <- ggplot(focus_data3, aes(x = reorder(常住地英文, 人数), y = 人数)) +
  geom_col(fill = "#4682B4") +
  coord_flip() +
  geom_text(aes(label = 人数), hjust = -0.2, size = 4) +
  labs(
    title = paste0( "Top 10 Permanent Residents of Acute Lymphoblastic Leukemia Patients"),
    x = NULL, y = "Number of patients"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))
p3
ggsave("急性淋巴细胞白血病患者常住地分布.pdf", plot = p3, device = "pdf", width = 10, height = 10)

# 2.4.4 指定淋巴瘤
disease_focus4 <- "淋巴瘤"
# 获取该疾病下常住地分布前 10 地点
focus_data4 <- data_zhuzai_combined %>%
  filter(疾病分类 == disease_focus4, !is.na(常住地市级)) %>%
  count(常住地市级,常住地英文, name = "人数") %>%
  slice_max(order_by = 人数, n = 10)
# 绘图
p4 <- ggplot(focus_data4, aes(x = reorder(常住地英文, 人数), y = 人数)) +
  geom_col(fill = "#4682B4") +
  coord_flip() +
  geom_text(aes(label = 人数), hjust = -0.2, size = 4) +
  labs(
    title = paste0( "Top 10 Permanent Residents of Lymphoma Patients"),
    x = NULL, y = "Number of patients"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))
p4
ggsave("淋巴瘤患者常住地分布.pdf", plot = p4, device = "pdf", width = 10, height = 10)

# 2.4.5 指定未分类
disease_focus5 <- "未分类"
# 获取该疾病下常住地分布前 10 地点
focus_data5 <- data_zhuzai_combined %>%
  filter(疾病分类 == disease_focus5, !is.na(常住地市级)) %>%
  count(常住地市级,常住地英文, name = "人数") %>%
  slice_max(order_by = 人数, n = 10)
# 绘图
p5 <- ggplot(focus_data5, aes(x = reorder(常住地英文, 人数), y = 人数)) +
  geom_col(fill = "#4682B4") +
  coord_flip() +
  geom_text(aes(label = 人数), hjust = -0.2, size = 4) +
  labs(
    title = paste0( "Top 10 Permanent Residents of Unclassified Patients"),
    x = NULL, y = "Number of patients"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))
p5
ggsave("未分类患者常住地分布.pdf", plot = p5, device = "pdf", width = 10, height = 10)

# 2.4.6 指定慢性淋巴细胞白血病
disease_focus6 <- "慢性淋巴细胞白血病"
# 获取该疾病下常住地分布前 10 地点
focus_data6 <- data_zhuzai_combined %>%
  filter(疾病分类 == disease_focus6, !is.na(常住地市级)) %>%
  count(常住地市级,常住地英文, name = "人数") %>%
  slice_max(order_by = 人数, n = 10)
# 绘图
p6 <- ggplot(focus_data6, aes(x = reorder(常住地英文, 人数), y = 人数)) +
  geom_col(fill = "#4682B4") +
  coord_flip() +
  geom_text(aes(label = 人数), hjust = -0.2, size = 4) +
  labs(
    title = paste0( "Top 10 Permanent Residents of Chronic Lymphocytic Leukemia Patients"),
    x = NULL, y = "Number of patients"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))
p6
ggsave("慢性淋巴细胞白血病患者常住地分布.pdf", plot = p6, device = "pdf", width = 10, height = 10)

# 2.4.7 指定慢性髓系白血病
disease_focus7 <- "慢性髓系白血病"
# 获取该疾病下常住地分布前 10 地点
focus_data7 <- data_zhuzai_combined %>%
  filter(疾病分类 == disease_focus7, !is.na(常住地市级)) %>%
  count(常住地市级,常住地英文, name = "人数") %>%
  slice_max(order_by = 人数, n = 10)
# 绘图
p7 <- ggplot(focus_data7, aes(x = reorder(常住地英文, 人数), y = 人数)) +
  geom_col(fill = "#4682B4") +
  coord_flip() +
  geom_text(aes(label = 人数), hjust = -0.2, size = 4) +
  labs(
    title = paste0( "Top 10 Permanent Residents of Chronic Myeloid Leukemia Patients"),
    x = NULL, y = "Number of patients"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))
p7
ggsave("慢性髓系白血病患者常住地分布.pdf", plot = p7, device = "pdf", width = 10, height = 10)

# 2.4.8 指定其他白血病
disease_focus8 <- "其他白血病"
# 获取该疾病下常住地分布前 10 地点
focus_data8 <- data_zhuzai_combined %>%
  filter(疾病分类 == disease_focus8, !is.na(常住地市级)) %>%
  count(常住地市级,常住地英文, name = "人数") %>%
  slice_max(order_by = 人数, n = 10)
# 绘图
p8 <- ggplot(focus_data8, aes(x = reorder(常住地英文, 人数), y = 人数)) +
  geom_col(fill = "#4682B4") +
  coord_flip() +
  geom_text(aes(label = 人数), hjust = -0.2, size = 4) +
  labs(
    title = paste0( "Top 10 Permanent Residents of Other Leukemia Patients"),
    x = NULL, y = "Number of patients"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))
p8
ggsave("其他白血病患者常住地分布.pdf", plot = p8, device = "pdf", width = 10, height = 10)



# 2.5 导入每个市级地点的经纬度
city_coords <- read_excel("F:/血液科数据/行政区划/coords.xlsx")

# 创建省市合并列
city_coords <- city_coords %>%
  mutate(省市 = ifelse(省 == 市, 市, paste0(省, 市))) %>%  # 直辖市（省==市）只保留一次
  distinct(省市, .keep_all = TRUE)
# 查看你的疾病患者前 10 城市
head(focus_data)

# 2.5.1 匹配并合并经纬度，多发性骨髓瘤
focus_map_data <- focus_data %>%
  left_join(city_coords, by = c("常住地市级" = "省市"))
# 需要仔细检查一下，恩施土家苗族自治州就是恩施市
# 在第5列第8行填入
focus_map_data$经度[8] <- 109.49459
focus_map_data$纬度[8] <- 30.27794
# 仙桃经纬度错误
focus_map_data$经度[6] <- 113.453974
focus_map_data$纬度[6] <- 30.364953

# 2.5.2 急性髓系白血病
focus_map_data2 <- focus_data2 %>%
  left_join(city_coords, by = c("常住地市级" = "省市"))
# 进行经纬度补充，广水
focus_map_data2$经度[8] <- 113.8259
focus_map_data2$纬度[8] <- 31.6169
# 武穴
focus_map_data2$经度[9] <- 115.5612
focus_map_data2$纬度[9] <- 29.8441
# 恩施
focus_map_data2$经度[12] <- 109.49459
focus_map_data2$纬度[12] <- 30.27794
# 洪湖
focus_map_data2$经度[13] <- 113.4357
focus_map_data2$纬度[13] <- 29.8644
# 江西省九江市的经纬度有问题
focus_map_data2$经度[10] <- 115.992811
focus_map_data2$纬度[10] <- 29.712034

# 2.5.3 急性淋巴细胞白血病
focus_map_data3 <- focus_data3 %>%
  left_join(city_coords, by = c("常住地市级" = "省市"))
# 麻城
focus_map_data3$经度[6] <- 115.0081
focus_map_data3$纬度[6] <- 31.1727
# 应城
focus_map_data3$经度[11] <- 113.5727
focus_map_data3$纬度[11] <- 30.9284
# 恩施
focus_map_data3$经度[12] <- 109.49459
focus_map_data3$纬度[12] <- 30.27794
# 仙桃经纬度错误
focus_map_data3$经度[10] <- 113.453974
focus_map_data3$纬度[10] <- 30.364953

# 2.5.4 淋巴瘤
focus_map_data4 <- focus_data4 %>%
  left_join(city_coords, by = c("常住地市级" = "省市"))
# 洪湖
focus_map_data4$经度[10] <- 113.4357
focus_map_data4$纬度[10] <- 29.8644

# 2.5.5 未分类
focus_map_data5 <- focus_data5 %>%
  left_join(city_coords, by = c("常住地市级" = "省市"))
# 广水
focus_map_data5$经度[6] <- 113.8259
focus_map_data5$纬度[6] <- 31.6169
# 洪湖
focus_map_data5$经度[7] <- 113.4357
focus_map_data5$纬度[7] <- 29.8644
# 恩施
focus_map_data5$经度[10] <- 109.49459
focus_map_data5$纬度[10] <- 30.27794

# 2.5.6 慢性淋巴细胞白血病
focus_map_data6 <- focus_data6 %>%
  left_join(city_coords, by = c("常住地市级" = "省市"))
# 大冶
focus_map_data6$经度[9] <- 114.979875
focus_map_data6$纬度[9] <- 30.095643
# 应城
focus_map_data6$经度[12] <- 113.5727
focus_map_data6$纬度[12] <- 30.9284
# 武穴
focus_map_data6$经度[13] <- 115.5612
focus_map_data6$纬度[13] <- 29.8441

# 2.5.7 慢性髓系白血病
focus_map_data7 <- focus_data7 %>%
  left_join(city_coords, by = c("常住地市级" = "省市"))
# 麻城
focus_map_data7$经度[3] <- 115.0081
focus_map_data7$纬度[3] <- 31.1727
# 北京
focus_map_data7$经度[6] <- 116.4134
focus_map_data7$纬度[6] <- 39.91092

# 京山
focus_map_data7$经度[10] <- 116.4134
focus_map_data7$纬度[10] <- 39.91092
# 广水
focus_map_data7$经度[12] <- 113.8259
focus_map_data7$纬度[12] <- 31.6169
# 枣阳
focus_map_data7$经度[13] <- 112.771959
focus_map_data7$纬度[13] <- 32.128818
# 洪湖
focus_map_data7$经度[14] <- 113.4357
focus_map_data7$纬度[14] <- 29.8644
# 钟祥
focus_map_data7$经度[17] <- 112.588121
focus_map_data7$纬度[17] <- 31.16782

# 2.5.8 其他白血病
focus_map_data8 <- focus_data8 %>%
  left_join(city_coords, by = c("常住地市级" = "省市"))
# 广水
focus_map_data8$经度[5] <- 113.8259
focus_map_data8$纬度[5] <- 31.6169
# 恩施
focus_map_data8$经度[6] <- 109.49459
focus_map_data8$纬度[6] <- 30.27794
# 武穴
focus_map_data8$经度[9] <- 115.5612
focus_map_data8$纬度[9] <- 29.8441
# 洪湖
focus_map_data8$经度[10] <- 113.4357
focus_map_data8$纬度[10] <- 29.8644


########  3.获取中国地图
# 民政部
# link: https://mp.weixin.qq.com/s/qj1SRc6D8sgYJYaZzDux6Q
######################################
##  API前缀
API_pre = "http://xzqh.mca.gov.cn/data/"
## 1.全国
China = st_read(dsn = paste0(API_pre, "quanguo.json"), 
                stringsAsFactors=FALSE) 
st_crs(China) = 4326   # 设置坐标系为 WGS84 (经纬度)

# 可视化
custom_palette <- c(
  "#bdd7e7", "#6baed6", "#3182bd", "#08519c", 
  "#fee5d9", "#fcae91", "#fb6a4a", "#de2d26"
)

# 3.1 多发性骨髓瘤患者的分布
# 计算经纬度范围
summary(focus_map_data$经度)
summary(focus_map_data$纬度)

p <- ggplot(data = China) +
  geom_sf(fill = "gray95", color = "gray80", size = 0.2) +
  
  geom_point(data = focus_map_data, 
             aes(x = 经度, y = 纬度, size = 人数, color = 人数), 
             alpha = 0.85) +
  
  geom_text(data = focus_map_data, 
            aes(x = 经度, y = 纬度, label = 常住地英文),
            size = 3, vjust = -1) +
  
  # 使用自定义渐变调色板
  scale_color_gradientn(colors = custom_palette) +
  scale_size_continuous(range = c(4, 10)) +
  
  labs(
    title = paste0("Map of the top 10 places of residence for multiple myeloma patients"),
    x = NULL, y = NULL,
    color = "Number of patients", size = "Patients"
  ) +
  
  coord_sf(xlim = c(105, 120), ylim = c(25, 40), expand = FALSE) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid = element_blank()
  )
p
ggsave("多发性骨髓瘤患者常住地分布地图.pdf", plot = p, device = "pdf", width = 10, height = 10)

# 3.2 急性髓系白血病患者的分布
# 计算经纬度范围
summary(focus_map_data2$经度)
summary(focus_map_data2$纬度)

p2 <- ggplot(data = China) +
  geom_sf(fill = "gray95", color = "gray80", size = 0.2) +
  
  geom_point(data = focus_map_data2, 
             aes(x = 经度, y = 纬度, size = 人数, color = 人数), 
             alpha = 0.85) +
  
  geom_text(data = focus_map_data2, 
            aes(x = 经度, y = 纬度, label = 常住地英文),
            size = 3, vjust = -1) +
  
  # 使用自定义渐变调色板
  scale_color_gradientn(colors = custom_palette) +
  scale_size_continuous(range = c(4, 10)) +
  
  labs(
    title = paste0("Map of the top 10 places of residence for acute myeloid leukemia patients"),
    x = NULL, y = NULL,
    color = "患者人数", size = "人数"
  ) +
  
  coord_sf(xlim = c(105, 120), ylim = c(25, 40), expand = FALSE) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid = element_blank()
  )
p2
ggsave("急性髓系白血病患者常住地分布地图.pdf", plot = p2, device = "pdf", width = 10, height = 10)

## 3.3 急性淋巴细胞白血病患者的分布
# 计算经纬度范围
summary(focus_map_data3$经度)
summary(focus_map_data3$纬度)

p3 <- ggplot(data = China) +
  geom_sf(fill = "gray95", color = "gray80", size = 0.2) +
  
  geom_point(data = focus_map_data3, 
             aes(x = 经度, y = 纬度, size = 人数, color = 人数), 
             alpha = 0.85) +
  
  geom_text(data = focus_map_data3, 
            aes(x = 经度, y = 纬度, label = 常住地英文),
            size = 3, vjust = -1) +
  
  # 使用自定义渐变调色板
  scale_color_gradientn(colors = custom_palette) +
  scale_size_continuous(range = c(4, 10)) +
  
  labs(
    title = paste0("Map of the top 10 places of residence for acute lymphoblastic leukemia patients"),
    x = NULL, y = NULL,
    color = "患者人数", size = "人数"
  ) +
  
  coord_sf(xlim = c(105, 130), ylim = c(25, 50), expand = FALSE) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid = element_blank()
  )
p3
ggsave("急性淋巴细胞白血病患者常住地分布地图.pdf", plot = p3, device = "pdf", width = 10, height = 10)

# 3.4 淋巴瘤患者的分布
# 计算经纬度范围
summary(focus_map_data4$经度)
summary(focus_map_data4$纬度)

p4 <- ggplot(data = China) +
  geom_sf(fill = "gray95", color = "gray80", size = 0.2) +
  
  geom_point(data = focus_map_data4, 
             aes(x = 经度, y = 纬度, size = 人数, color = 人数), 
             alpha = 0.85) +
  
  geom_text(data = focus_map_data4, 
            aes(x = 经度, y = 纬度, label = 常住地英文),
            size = 3, vjust = -1) +
  
  # 使用自定义渐变调色板
  scale_color_gradientn(colors = custom_palette) +
  scale_size_continuous(range = c(4, 10)) +
  
  labs(
    title = paste0("Map of the top 10 places of residence for lymphoma patients"),
    x = NULL, y = NULL,
    color = "患者人数", size = "人数"
  ) +
  
  coord_sf(xlim = c(105, 120), ylim = c(25, 40), expand = FALSE) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid = element_blank()
  )
p4
ggsave("淋巴瘤患者常住地分布地图.pdf", plot = p4, device = "pdf", width = 10, height = 10)

# 3.5 未分类的分布
# 计算经纬度范围
summary(focus_map_data5$经度)
summary(focus_map_data5$纬度)

p5 <- ggplot(data = China) +
  geom_sf(fill = "gray95", color = "gray80", size = 0.2) +
  
  geom_point(data = focus_map_data5, 
             aes(x = 经度, y = 纬度, size = 人数, color = 人数), 
             alpha = 0.85) +
  
  geom_text(data = focus_map_data5, 
            aes(x = 经度, y = 纬度, label = 常住地英文),
            size = 3, vjust = -1) +
  
  # 使用自定义渐变调色板
  scale_color_gradientn(colors = custom_palette) +
  scale_size_continuous(range = c(4, 10)) +
  
  labs(
    title = paste0("Map of the top 10 places of residence for unclassified patients"),
    x = NULL, y = NULL,
    color = "患者人数", size = "人数"
  ) +
  
  coord_sf(xlim = c(105, 120), ylim = c(25, 40), expand = FALSE) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid = element_blank()
  )
p5

############# 在导出数据之前，我们先分析缺失的具体情况 ###########
# 查看第3列到第10列的缺失情况
missing_summary <- sapply(data_yinyang_avg[, 3:10], function(x) sum(is.na(x)))
# 计算每列的缺失比例
missing_percentage <- sapply(data_yinyang_avg[, 3:10], function(x) mean(is.na(x)) * 100)

# 创建汇总数据框
missing_df <- data.frame(
  Column = names(missing_summary),
  Missing_Count = missing_summary,
  Missing_Percentage = missing_percentage,
  row.names = NULL
)

# 打印结果
print(missing_df)

# 查看第4列到第752列的缺失情况
missing_summary2 <- sapply(data_shiyan_avg[, 4:752], function(x) sum(is.na(x)))
# 计算每列的缺失比例
missing_percentage2 <- sapply(data_shiyan_avg[, 4:752], function(x) mean(is.na(x)) * 100)

# 创建汇总数据框
missing_df2 <- data.frame(
  Column = names(missing_summary2),
  Missing_Count = missing_summary2,
  Missing_Percentage = missing_percentage2,
  row.names = NULL
)

# 打印结果
print(missing_df2)


##### 4.1将护理量表评分导出适合LLM读取的数据格式
# 护理记录
#  转换为英文表述
# 按患者、日期排序并格式化每条记录
nursing_text_df <- data_yinyang %>%
  arrange(患者编号, 记录日期) %>%
  mutate(记录文本 = paste0("- Date: ", 记录日期,
                       " | Type: ", 护理记录类型,
                       " | Score: ", 总评分)) %>%
  group_by(患者编号) %>%
  summarise(
    nursing_summary = paste(记录文本, collapse = "\n"),
    .groups = "drop"
  ) %>%
  mutate(
    patient_text = paste0("Patient ID: ", 患者编号, "\nNursing Assessments:\n", nursing_summary)
  )
# 保存为 TXT，每个患者一段文字
writeLines(nursing_text_df$patient_text, "nursing_for_llm.txt")

##### 4.2导出病历等结构化文本和非结构化文本 
# 保存需要的列
data_wenben <- data_huli_cleaned %>%
  dplyr::select(患者编号, 入院日期, 诊断名称, 诊断类型,诊断归转情况,主诉,现病史,既往史,个人史,婚姻史,生育史,月经史,家族史)
# 将同一患者编号的诊断名称合并到一个框里，相同的诊断名称保留一个
data_wenben <- data_wenben %>%
  group_by(患者编号,入院日期,现病史,诊断归转情况) %>%
  reframe(
    诊断名称 = paste(unique(诊断名称), collapse = "；"),
    诊断类型 = paste(unique(诊断类型), collapse = "；"),
    主诉 = paste(unique(主诉), collapse = "；"),
    既往史 = paste(unique(既往史), collapse = "；"),
    个人史 = paste(unique(个人史), collapse = "；"),
    婚姻史 = paste(unique(婚姻史), collapse = "；"),
    生育史 = paste(unique(生育史), collapse = "；"),
    月经史 = paste(unique(月经史), collapse = "；"),
    家族史 = paste(unique(家族史), collapse = "；")
  )

# 显示列名
colnames(data_wenben)

# 构造每条入院记录的段落文本
data_wenben_text <- data_wenben %>%
mutate(
  文本组合 = paste0(
    "Patient ID: ", 患者编号, "\n",
    "Admission Date: ", 入院日期, "\n",
    "Chief Complaint: ", 主诉, "\n",
    "Present Illness: ", 现病史, "\n",
    "Diagnosis: ", 诊断名称, "\n",
    "Diagnosis Type: ", 诊断类型, "\n",
    "Diagnosis Status: ", 诊断归转情况, "\n",
    "History:\n",
    "- Past History: ", 既往史, "\n",
    "- Personal History: ", 个人史, "\n",
    "- Marital History: ", 婚姻史, "\n",
    "- Fertility History: ", 生育史, "\n",
    "- Menstrual History: ", 月经史, "\n",
    "- Family History: ", 家族史
  )
)
# 保存为纯文本（每段一个病例）
writeLines(data_wenben_text$文本组合, "patient_text_admission_level.txt")

##### 4.3导出患者一些人口学信息  ######
data_renkou <- data_huli_cleaned %>%
  dplyr::select(患者编号, 入院日期, 性别,籍贯,ABO血型名称,RH血型,民族,年龄,婚姻状况,国籍,职业,入院科室名称,出院科室名称,
                出院日期,实际住院天数)


#### 2.3 提取个人疾病及生存数据 #######
# 保留第1，2，4，5，6，12，15，18，19列
data_jibing <- data_huli_cleaned[,c(1,2,4,5,6,12,15,18,19)]
# 导入新给出的数据,这是血液科的临床和预后数据
data_yuhou <- read_excel("C:/Users/pyz/Desktop/血液肿瘤/血液科数据/武汉大学中南医院数据.xlsx")
data_linchuang <- read_excel("C:/Users/pyz/Desktop/血液肿瘤/血液科数据/中南医院临床数据.xlsx")



#### 2.4 提取个人生活质量和营养 #######
# data_yinyang_avg包含血液肿瘤患者的量表评分


#### 2.5 提取个人实验检查数据 ########
# 注意：data_shiyan是AML的实验标本数据
length(unique(data_shiyan$患者编号)) # 共有 1000 个患者
data_jianyan_1 <- read.csv("F:/血液科数据/一阶段清洗后数据/data_jianyan_1.csv")
data_jianyan_2 <- read.csv("F:/血液科数据/一阶段清洗后数据/data_jianyan_2.csv")
data_jianyan_3 <- read.csv("F:/血液科数据/一阶段清洗后数据/data_jianyan_3.csv")

length(unique(data_jianyan_1$患者编号)) 
length(unique(data_jianyan_2$患者编号))

length(unique(data_zhuzai$患者编号))
length(unique(data_huli_cleaned$患者编号))


# 计算相同患者编号的数量
column_name <- "检验子项中文名"  
common_patients <- length(intersect(data_jianyan_1[[column_name]], data_jianyan_2[[column_name]]))
print(paste("📌 两个数据集中相同的患者编号数量为：", common_patients))
length(unique(data_jianyan_1$检验子项中文名)) 

# 保留1，17，19，20，21列
data_shiyan_1 <- data_jianyan_1 %>%
  dplyr::select(患者编号, 检验项目名称,采集时间,检验子项中文名, 检验子项英文名,检验子项结果 ,检验子项结果数值, 检验子项单位)
data_shiyan_2 <- data_jianyan_2 %>%
  dplyr::select(患者编号, 检验项目名称,采集时间,检验子项中文名, 检验子项英文名,检验子项结果 ,检验子项结果数值, 检验子项单位)

#  尽管两个检验项目数据存在很多相似的患者号，但是许多患者号检验项目是NA，两者最初是从不同的excel导入合并的，我觉得没有什么问题，分开导出即可
# 构造每条检验记录的格式
lab_df <- data_shiyan_1  %>%
  mutate(
    检验记录 = paste0(
      "- Item: ", 检验子项中文名, " (", 检验子项英文名, ")",
      " | Result: ", 检验子项结果数值, " ", 检验子项单位
    )
  )

# 每位患者按检验时间合并文本记录
lab_text_df <- lab_df %>%
  arrange(患者编号, 采集时间) %>%
  group_by(患者编号, 采集时间) %>%
  summarise(
    lab_summary = paste(检验记录, collapse = "\n"),
    .groups = "drop"
  ) %>%
  group_by(患者编号) %>%
  summarise(
    lab_text = paste0("Lab Tests (Collected on ", 采集时间, "):\n", lab_summary, collapse = "\n"),
    .groups = "drop"
  ) %>%
  mutate(
    patient_text = paste0("Patient ID: ", 患者编号, "\n", lab_text)
  )
# 保存为txt
writeLines(lab_text_df$patient_text, "lab1_for_llm.txt")

# 构造每条检验记录的格式
lab_df2 <- data_shiyan_2  %>%
  mutate(
    检验记录 = paste0(
      "- Item: ", 检验子项中文名, " (", 检验子项英文名, ")",
      " | Result: ", 检验子项结果数值, " ", 检验子项单位
    )
  )

# 每位患者按检验时间合并文本记录
lab_text_df2 <- lab_df2 %>%
  arrange(患者编号, 采集时间) %>%
  group_by(患者编号, 采集时间) %>%
  summarise(
    lab_summary = paste(检验记录, collapse = "\n"),
    .groups = "drop"
  ) %>%
  group_by(患者编号) %>%
  summarise(
    lab_text = paste0("Lab Tests (Collected on ", 采集时间, "):\n", lab_summary, collapse = "\n"),
    .groups = "drop"
  ) %>%
  mutate(
    patient_text = paste0("Patient ID: ", 患者编号, "\n", lab_text)
  )
# 保存为txt
writeLines(lab_text_df2$patient_text, "lab2_for_llm.txt")



length(unique(data_shiyan_1$患者编号))
length(unique(data_shiyan_2$患者编号))
length(unique(data_shiyan_3$患者编号))



#### 2.6 获得环境数据（ PM2.5、NO₂、SO₂、O₃） #######
### 难点是时间匹配，住院期间应该匹配武汉区间的环境情况，住院期间以外应该匹配常住地的环境情况，但是这样会非常麻烦，不要太过复杂化问题
### 这里先不考虑时间匹配问题，直接使用常住地、出生地、籍贯的环境情况及变化












################## 3.cox回归分析 ####################












################# 4.前瞻性评估####################







################# 5.地理加权cox回归分析 ####################







######################## 一、重新导入数据 ########################
###### 1.导入鑫琦的数据 ######
data_jiben <- read_excel("F:/血液科数据/鑫琦数据/血液内科数据提取-基本信息0402.xlsx")
data_jianyan <- read_excel("F:/血液科数据/鑫琦数据/血液内科数据提取-检验结果0402.xlsx")

length(unique(data_jiben$住院号))
length(unique(data_jianyan$住院号))

# 提取个人史中包含“经常留居地:”后的内容
data_jiben$常住地 <- str_extract(data_jiben$个人史, "(?<=经常留居地[:：])[^\n；；，,]*")
# 显示各病史第1行的值
data_jiben$个人史[1]
data_jiben$主诉[1]
data_jiben$现病[1]
data_jiben$疾病史[1]


####### 2.导入吕的数据 #########
data_geren1 <- read_excel("C:/Users/pyz/Desktop/血液肿瘤/血液科数据/护理记录/非隐私信息.患者基本信息.xlsx")
# 删除data_geren1第二列
data_geren1 <- data_geren1[,-2]
# 重命名第一列为患者编号
colnames(data_geren1)[1] <- "患者编号"
# 检查患者数
length(unique(data_geren1$患者编号))

# 吕的数据中的诊断数据
data_geren2 <- read_excel("C:/Users/pyz/Desktop/血液肿瘤/血液科数据/护理记录/非隐私信息.就诊基本信息.病案首页基本信息.病案首页诊断信息.xlsx")
# 重命名
colnames(data_geren2)[3] <- "就诊编号"
# 仅保留患者编号，就诊编号，性别，年龄，婚姻状况，国籍，职业，诊断名称，诊断归转情况
data_geren2 <- data_geren2 %>%
  dplyr::select(患者编号, 就诊编号, 性别, 年龄, 婚姻状况, 国籍, 职业, 诊断名称, 诊断归转情况)
length(unique(data_geren2$患者编号))

# 病史数据
data_geren3 <- read_excel("C:/Users/pyz/Desktop/血液肿瘤/血液科数据/护理记录/非隐私信息.就诊基本信息.病史信息.xlsx")
# 删除data_geren3第二列
data_geren3 <- data_geren3[,-2]
length(unique(data_geren3$患者编号))
########## 由此，我们从吕的数据中获得了个人信息的数据（籍贯，出生地，个人史等）

########## 3.导入护理评分数据 ##########
# 这是护理评分数据
data_huli <- read_excel("C:/Users/pyz/Desktop/血液肿瘤/血液科数据/护理评分.xlsx")
# 重命名
colnames(data_huli)[1] <- "患者编号"
colnames(data_huli)[2] <- "就诊编号"
length(unique(data_huli$患者编号))    #### 有1738名患者，但是应该没太多缺失值
# 检查是否为数值型
data_huli$总分 <- as.numeric(data_huli$总分)
# 对每位患者在同一护理记录类型下的评分按日期取平均
data_huli_avg <- data_huli %>%
  group_by(患者编号, 就诊编号, 表单名称,日期) %>%
  summarise(平均总分 = mean(总分, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = 表单名称, values_from = 平均总分)
# 检查每列缺失值
colSums(is.na(data_huli_avg)) ### 日常生活能力评定量表缺失2404行，其余量表均缺失超过5000行
# 由于考虑了测量时间作为分组，许多患者整个住院期间可能只测量了几次，而一个患者各个量表的所有测量时间都在一个患者里有所体现，
# 这个患者没在这个时间测量过，当然就没有数据了，简单来说，就是各量表的测量时间不匹配

# 新加入一列time
data_huli2 <- data_huli %>%
  # 先按患者编号、就诊编号、表单名称、日期排序
  arrange(患者编号, 就诊编号, 表单名称, 日期) %>%
  # 按患者和表单分组，计算出现顺序
  group_by(患者编号, 就诊编号, 表单名称) %>%
  mutate(time = row_number()) %>%  # 计算该表单是第几次出现
  ungroup()  # 取消分组

# 利用time作为时间切片，缺点就是在患者疾病进展过程中时，需要同其他时间段进行模糊匹配，所以相应的时间点建议保留
data_huli_avg2 <- data_huli2 %>%
  group_by(患者编号, 就诊编号, 表单名称, time) %>%
  summarise(平均总分 = mean(总分, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = 表单名称, values_from = 平均总分)
# 检查每列缺失值
colSums(is.na(data_huli_avg2)) ### 日常生活能力评定量表缺失293行，其余量表均缺失超过3000行
#### 看来这个表中的护理评分数据不太够，日常生活能力评定量表缺失最少，但是营养风险筛查量表缺失接近50%，不太好

### 我们纳入吕的原先数据进行处理一下
data_huli12 <- read_excel("C:/Users/pyz/Desktop/血液肿瘤/血液科数据/护理记录/非隐私信息.就诊基本信息.护理记录.护理记录子项 (2).xlsx")
data_huli13 <- read_excel("C:/Users/pyz/Desktop/血液肿瘤/血液科数据/护理记录/非隐私信息.就诊基本信息.护理记录.护理记录子项 (3).xlsx")
data_huli14 <- read_excel("C:/Users/pyz/Desktop/血液肿瘤/血液科数据/护理记录/非隐私信息.就诊基本信息.护理记录.护理记录子项 (4).xlsx")
data_huli15 <- read_excel("C:/Users/pyz/Desktop/血液肿瘤/血液科数据/护理记录/非隐私信息.就诊基本信息.护理记录.护理记录子项 (5).xlsx")
data_huli16 <- read_excel("C:/Users/pyz/Desktop/血液肿瘤/血液科数据/护理记录/非隐私信息.就诊基本信息.护理记录.护理记录子项 (6).xlsx")
data_huli17 <- read_excel("C:/Users/pyz/Desktop/血液肿瘤/血液科数据/护理记录/非隐私信息.就诊基本信息.护理记录.护理记录子项 (7).xlsx")
data_huli18 <- read_excel("C:/Users/pyz/Desktop/血液肿瘤/血液科数据/护理记录/非隐私信息.就诊基本信息.护理记录.护理记录子项 (8).xlsx")
data_huli19 <- read_excel("C:/Users/pyz/Desktop/血液肿瘤/血液科数据/护理记录/非隐私信息.就诊基本信息.护理记录.护理记录子项 (9).xlsx")
data_huli20 <- read_excel("C:/Users/pyz/Desktop/血液肿瘤/血液科数据/护理记录/非隐私信息.就诊基本信息.护理记录.护理记录子项 (10).xlsx")
data_huli21 <- read_excel("C:/Users/pyz/Desktop/血液肿瘤/血液科数据/护理记录/非隐私信息.就诊基本信息.护理记录.护理记录子项 (11).xlsx")
data_huli22 <- read_excel("C:/Users/pyz/Desktop/血液肿瘤/血液科数据/护理记录/非隐私信息.就诊基本信息.护理记录.护理记录子项.xlsx")
# 合并
data_huli_combined <- rbind(data_huli12, data_huli13, data_huli14, data_huli15, 
                             data_huli16, data_huli17, data_huli18, data_huli19, 
                             data_huli20, data_huli21, data_huli22)
length(unique(data_huli_combined$患者编号))

###### 由于量表与护理记录的评估项目和结果描述存在差异，我建议分开罗列数据框
# 量表的数据，注意data_liangbiao同样重要，因为其中包含测量的时间和具体条目
data_liangbiao <- data_huli_combined %>%
  filter(护理记录类型 %in% c("日常生活能力评定量表", "NEW营养风险筛查评估表", 
                       "普通患者营养评定表（SGA）", "营养风险筛查表(成人)", 
                       "营养风险筛查评估表", "跌倒风险评分及防范措施记录表",
                       "跌倒风险评估表及预防措施（血液内科）", 
                       "压力性损伤评估及预防护理措施观察记录表"))
# 确认总评分为numeric
data_liangbiao$总评分 <- as.numeric(data_liangbiao$总评分)
# 剔除第2，4，8，9列
data_liangbiao <- data_liangbiao[,-c(2,4,8,9,12)]
# 重命名
colnames(data_liangbiao)[2] <- "就诊编号"
# 去除第6，7列
data_liangbiao2 <- data_liangbiao[,-c(6,7)]
# 有大量冗余行，去重
data_liangbiao2 <- data_liangbiao2 %>%
  distinct(患者编号, 就诊编号, 护理记录类型, 记录日期, .keep_all = TRUE)
# 新加入一列time
data_liangbiao2 <- data_liangbiao2 %>%
  # 先按患者编号、就诊编号、表单名称、日期排序
  arrange(患者编号, 护理记录类型, 记录日期) %>%
  # 按患者和表单分组，计算出现顺序
  group_by(患者编号, 护理记录类型) %>%
  mutate(time = row_number()) %>%  # 计算该表单是第几次出现
  ungroup()  # 取消分组
# 利用time作为时间切片，缺点就是在患者疾病进展过程中时，需要同其他时间段进行模糊匹配，所以相应的时间点建议保留
data_liangbiao2 <- data_liangbiao2 %>%
  group_by(患者编号, 护理记录类型, time) %>%
  summarise(平均总分 = mean(总评分, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = 护理记录类型, values_from = 平均总分)
# 检查每列缺失值
colSums(is.na(data_liangbiao2)) ### 一共15767行，日常生活能力评定量表缺失1727行，营养风险筛查表(成人) 缺失9674行， NEW营养风险筛查评估表 缺失10703行

# 计算每列的缺失数量和比例
missing_summary <- data.frame(
  量表 = names(data_liangbiao2)[-c(1:2)],  # 排除前两列：患者编号、time
  缺失数量 = colSums(is.na(data_liangbiao2[,-c(1:2)])),
  总记录数 = nrow(data_liangbiao2)
) %>%
  mutate(缺失比例 = 缺失数量 / 总记录数)

# 绘图：缺失数量条形图
CairoPDF("missing1.pdf",family = "msyh")
showtext_begin()
ggplot(missing_summary, aes(x = reorder(量表, -缺失数量), y = 缺失数量)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0("(", round(缺失比例*100, 1), "%)")), 
            vjust = -0.5, size = 4) +
  labs(title = "各护理量表缺失情况",
       x = "护理记录类型", y = "缺失记录数") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
showtext_end()
dev.off()

######## 3.筛选出缺失较少的量表 ########
## 日常生活能力评定量表，两个营养量表也可以勉强纳入
# 日常生活能力评定量表
data_richang <- data_liangbiao %>%
  filter(护理记录类型 == "日常生活能力评定量表")
# 确认有多少项目名称
length(unique(data_richang$项目名称)) # 有 17 个项目名称
unique(data_richang$项目名称) # 显示所有项目名称
# 转置数据框
pivot_type1 <- data_richang %>%
  group_by(患者编号, 就诊编号,护理记录类型,记录日期) %>%
  summarise(平均总分 = mean(总评分, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = 护理记录类型, values_from = 平均总分)
# 确认测量结果列为字符形式
data_richang$测量结果 <- as.character(data_richang$测量结果)
# 确认项目名称NA的个数
sum(is.na(data_richang$项目名称)) # 149
# 去除项目名称为NA的行
data_richang <- data_richang %>%
  filter(!is.na(项目名称))
# 去除第3列和第5列
data_richang <- data_richang[,-c(3,5)]
# 转置项目名称的数据框
pivot_type2 <- data_richang %>%
  group_by(患者编号, 就诊编号, 记录日期, 项目名称) %>%
  slice(1) %>%  # 保留每组中的第一条记录
  ungroup() %>%
  pivot_wider(names_from = 项目名称, values_from = 测量结果)
# 检查警告信息，发现存在重复组，因此对重复的只保留第一个信息
data_richang %>%
  group_by(患者编号, 就诊编号, 记录日期, 项目名称) %>%
  summarise(n = n()) %>%
  filter(n > 1)
# 合并，这样我们就得到了日常活动能力量表的数据
data_richang_combined <- merge(pivot_type1, pivot_type2, by = c("患者编号", "就诊编号", "记录日期"), all = TRUE)
# 检查患者数
length(unique(data_richang_combined$患者编号)) # 共有 1935 个患者
# 检查缺失值
colSums(is.na(data_richang_combined)) # 共有14040行，前13个项目缺失很少，在100多一些，护理分级、病情等级缺失有3000多行，可以不用考虑

## 营养风险筛查表(成人)
data_yinyang <- data_liangbiao %>%
  filter(护理记录类型 == "营养风险筛查表(成人)")
# 确认有多少项目名称
length(unique(data_yinyang$项目名称)) # 有 40个项目名称
unique(data_yinyang$项目名称) # 显示所有项目名称
# 转置数据框
pivot_yinyang1 <- data_yinyang %>%
  group_by(患者编号, 就诊编号,护理记录类型,记录日期) %>%
  summarise(平均总分 = mean(总评分, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = 护理记录类型, values_from = 平均总分)
# 确认测量结果列为字符形式
data_yinyang$测量结果 <- as.character(data_yinyang$测量结果)
# 确认项目名称NA的个数
sum(is.na(data_yinyang$项目名称)) # 53
# 去除项目名称为NA的行
data_yinyang <- data_yinyang %>%
  filter(!is.na(项目名称))
# 去除第3列和第5列
data_yinyang <- data_yinyang[,-c(3,5)]
# 转置项目名称的数据框
pivot_yinyang2 <- data_yinyang %>%
  group_by(患者编号, 就诊编号, 记录日期, 项目名称) %>%
  slice(1) %>%  # 保留每组中的第一条记录
  ungroup() %>%
  pivot_wider(names_from = 项目名称, values_from = 测量结果)
# 合并，这样我们就得到了营养风险筛查表(成人)的数据
data_yinyang_combined <- merge(pivot_yinyang1, pivot_yinyang2, by = c("患者编号", "就诊编号", "记录日期"), all = TRUE)
# 检查患者数
length(unique(data_yinyang_combined$患者编号)) # 共有 809 个患者
# 检查缺失值
colSums(is.na(data_yinyang_combined)) #体重、父表单传参、身高、身高指数、体质指数(BMI) 相对缺失较少（少于700），仅保留这几列
# 保留第1，2，3，4，6列
data_yinyang_combined <- data_yinyang_combined[,c(1,2,3,4,6,10,11,12,15)]

## NEW营养风险筛查评估表
data_fengxian <- data_liangbiao %>%
  filter(护理记录类型 == "NEW营养风险筛查评估表")
# 确认有多少项目名称
length(unique(data_fengxian$项目名称)) # 有 38个项目名称
unique(data_fengxian$项目名称) # 显示所有项目名称
# 转置数据框
pivot_fengxian1 <- data_fengxian %>%
  group_by(患者编号, 就诊编号,护理记录类型,记录日期) %>%
  summarise(平均总分 = mean(总评分, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = 护理记录类型, values_from = 平均总分)
# 确认测量结果列为字符形式
data_fengxian$测量结果 <- as.character(data_fengxian$测量结果)
# 确认项目名称NA的个数
sum(is.na(data_fengxian$项目名称)) # 232个NA
# 去除项目名称为NA的行
data_fengxian <- data_fengxian %>%
  filter(!is.na(项目名称))
# 去除第3列和第5列
data_fengxian <- data_fengxian[,-c(3,5)]
# 转置项目名称的数据框
pivot_fengxian2 <- data_fengxian %>%
  group_by(患者编号, 就诊编号, 记录日期, 项目名称) %>%
  slice(1) %>%  # 保留每组中的第一条记录
  ungroup() %>%
  pivot_wider(names_from = 项目名称, values_from = 测量结果)
# 合并，这样我们就得到了NEW营养风险筛查评估表的数据
data_fengxian_combined <- merge(pivot_fengxian1, pivot_fengxian2, by = c("患者编号", "就诊编号", "记录日期"), all = TRUE)
# 检查患者数
length(unique(data_fengxian_combined$患者编号)) # 共有 1288 个患者
# 检查缺失值
colSums(is.na(data_fengxian_combined)) # 共有 5064 行，体重、身高、体质指数(BMI)、营养风险评分、营养风险评估结果、营养风险评估结论 相对缺失较少（少于500），仅保留这几列
# 保留第1，2，3，4，6列
data_fengxian_combined <- data_fengxian_combined[,c(1,2,3,4,6,7,12,13)] 


### 护理记录的数，注意护理记录的总评分没有参照性，用这个比对只是想比较时间上和连续性上的可比性，选几个感兴趣的护理记录即可
data_jilu <- data_huli_combined %>%
  filter(护理记录类型 %in% c("出入液量记录单","(新)非手术科室护理记录单","护理计划单(普通病房用)",
                        "临床输血核对护理记录单", "住院患者首次护理评估单(NEW)","手指血糖（七次血糖）监测记录单",
                       "压力性损伤评估及预防护理措施观察记录表"))
# 确认总评分为numeric
data_jilu$总评分 <- as.numeric(data_jilu$总评分)
# 剔除第2，4，8，9列
data_jilu <- data_jilu[,-c(2,4,8,9,12)]
# 重命名
colnames(data_jilu)[2] <- "就诊编号"
# 去除第6，7列
data_jilu2 <- data_jilu[,-c(6,7)]
# 有大量冗余行，去重
data_jilu2 <- data_jilu2 %>%
  distinct(患者编号, 就诊编号, 护理记录类型, 记录日期, .keep_all = TRUE)
# 新加入一列time
data_jilu2 <- data_jilu2 %>%
  # 先按患者编号、就诊编号、表单名称、日期排序
  arrange(患者编号, 护理记录类型, 记录日期) %>%
  # 按患者和表单分组，计算出现顺序
  group_by(患者编号, 护理记录类型) %>%
  mutate(time = row_number()) %>%  # 计算该表单是第几次出现
  ungroup()  # 取消分组
# 利用time作为时间切片，缺点就是在患者疾病进展过程中时，需要同其他时间段进行模糊匹配，所以相应的时间点建议保留
data_jilu2 <- data_jilu2 %>%
  group_by(患者编号, 护理记录类型, time) %>%
  summarise(平均总分 = mean(总评分, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = 护理记录类型, values_from = 平均总分)
# 检查每列缺失值
colSums(is.na(data_jilu2)) ### 一共1014306行，选用(新)非手术科室护理记录单，临床输血核对护理记录单，手指血糖（七次血糖）监测记录单，出入液量记录单 

# 计算缺失数量和缺失比例
missing_summary_jilu <- data.frame(
  护理记录类型 = names(data_jilu2)[-c(1:2)],  # 去掉“患者编号”和“time”
  缺失数量 = colSums(is.na(data_jilu2[,-c(1:2)])),
  总记录数 = nrow(data_jilu2)
) %>%
  mutate(缺失比例 = 缺失数量 / 总记录数)
# 可视化
CairoPDF("missing2.pdf",family = "msyh")
showtext_begin()
ggplot(missing_summary_jilu, aes(x = reorder(护理记录类型, -缺失数量), y = 缺失数量)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  geom_text(aes(label = paste0(round(缺失比例 * 100, 1), "%")),
            vjust = -0.3, size = 4) +
  labs(title = "各护理记录类型的缺失情况",
       x = "护理记录类型", y = "缺失记录数") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
showtext_end()
dev.off()


####### 4.针对护理记录进行筛选 ###########
# (新)非手术科室护理记录单
data_record <- data_jilu %>%
  filter(护理记录类型 == "(新)非手术科室护理记录单")
# 确认有多少项目名称
length(unique(data_record$项目名称)) # 有 104 个项目名称
unique(data_record$项目名称) # 显示所有项目名称
# 确认测量结果列为字符形式
data_record$测量结果 <- as.character(data_record$测量结果)
# 确认项目名称NA的个数
sum(is.na(data_record$项目名称)) # 0个NA
# 去除项目名称为NA的行
data_record <- data_record %>%
  filter(!is.na(项目名称))
# 去除第3列和第5列
data_record <- data_record[,-c(3,5)]
# 转置项目名称的数据框
data_record_combined <- data_record %>%
  group_by(患者编号, 就诊编号, 记录日期, 项目名称) %>%
  slice(1) %>%  # 保留每组中的第一条记录
  ungroup() %>%
  pivot_wider(names_from = 项目名称, values_from = 测量结果)
# 检查患者数
length(unique(data_record_combined$患者编号)) # 共有 2540 个患者
# 检查缺失值
colSums(is.na(data_record_combined)) #共有1010178行
# 保留第1，2，3，5，6，7列
data_record_combined <- data_record_combined[,c(1,2,3,5,6,7,8,9,10,11,12,14,15,16,17,18,21,27,29,30)] 

# 临床输血核对护理记录单
data_blood <- data_jilu %>%
  filter(护理记录类型 == "临床输血核对护理记录单")
# 确认有多少项目名称
length(unique(data_blood$项目名称)) # 有 30 个项目名称
unique(data_blood$项目名称) # 显示所有项目名称
# 确认测量结果列为字符形式
data_blood$测量结果 <- as.character(data_blood$测量结果)
# 确认项目名称NA的个数
sum(is.na(data_blood$项目名称)) # 259个NA
# 去除项目名称为NA的行
data_blood <- data_blood %>%
  filter(!is.na(项目名称))
# 去除第3列和第5列
data_blood <- data_blood[,-c(3,5)]
# 转置项目名称的数据框
data_blood_combined <- data_blood %>%
  group_by(患者编号, 就诊编号, 记录日期, 项目名称) %>%
  slice(1) %>%  # 保留每组中的第一条记录
  ungroup() %>%
  pivot_wider(names_from = 项目名称, values_from = 测量结果)
# 检查患者数
length(unique(data_blood_combined$患者编号)) # 共有 1273 个患者
# 检查缺失值
colSums(is.na(data_blood_combined)) #共有 17627 行，ABO血型、Rh血型等缺失较少
# 保留第1，2，3，4，5，6，7列
data_blood_combined <- data_blood_combined[,c(1,2,3,4,5,6,7,10,12,18,21,24,25,26,27,28)]

# 手指血糖（七次血糖）监测记录单
data_blood_glucose <- data_jilu %>%
  filter(护理记录类型 == "手指血糖（七次血糖）监测记录单")
# 确认有多少项目名称
length(unique(data_blood_glucose$项目名称)) # 有 11 个项目名称
unique(data_blood_glucose$项目名称) # 显示所有项目名称
# 确认测量结果列为字符形式
data_blood_glucose$测量结果 <- as.character(data_blood_glucose$测量结果)
# 确认项目名称NA的个数
sum(is.na(data_blood_glucose$项目名称)) # 546个NA
# 去除项目名称为NA的行
data_blood_glucose <- data_blood_glucose %>%
  filter(!is.na(项目名称))
# 去除第3列和第5列
data_blood_glucose <- data_blood_glucose[,-c(3,5)]
# 转置项目名称的数据框
data_blood_glucose_combined <- data_blood_glucose %>%
  group_by(患者编号, 就诊编号, 记录日期, 项目名称) %>%
  slice(1) %>%  # 保留每组中的第一条记录
  ungroup() %>%
  pivot_wider(names_from = 项目名称, values_from = 测量结果)
# 检查患者数
length(unique(data_blood_glucose_combined$患者编号)) # 共有 1211 个患者
# 检查缺失值
colSums(is.na(data_blood_glucose_combined)) #共有9112行，缺失都不少
# 保留第1，2，3，4，5，6，7，8，9，10列
data_blood_glucose_combined <- data_blood_glucose_combined[,c(1,2,3,4,5,6,7,8,9,10)]

# 出入液量记录单
data_chuliang <- data_jilu %>%
  filter(护理记录类型 == "出入液量记录单")
# 确认有多少项目名称
length(unique(data_chuliang$项目名称)) # 有 4个项目名称
unique(data_chuliang$项目名称) # 显示所有项目名称
# 检测患者数
length(unique(data_chuliang$患者编号)) # 共有 10个患者，不考虑这个护理记录了，在第一个护理记录单里有出入量的描述

## 保存数据和环境
save.image("护理记录与量表.RData")

# 加载数据
load("护理记录与量表.RData")

########## 二、检验数据的处理（重点） ###########
#### 1.导入吕的检验数据 ######
data_jianyan_1 <- read_excel("C:/Users/pyz/Desktop/血液肿瘤/血液科数据/检验报告/非隐私信息.就诊基本信息.普通检验报告.普通检验报告子项结果.药敏及结果 (2).xlsx")
data_jianyan_2 <- read_excel("C:/Users/pyz/Desktop/血液肿瘤/血液科数据/检验报告/非隐私信息.就诊基本信息.普通检验报告.普通检验报告子项结果.药敏及结果 (3).xlsx")
data_jianyan_3 <- read_excel("C:/Users/pyz/Desktop/血液肿瘤/血液科数据/检验报告/非隐私信息.就诊基本信息.普通检验报告.普通检验报告子项结果.药敏及结果 (4).xlsx")
data_jianyan_4 <- read_excel("C:/Users/pyz/Desktop/血液肿瘤/血液科数据/检验报告/非隐私信息.就诊基本信息.普通检验报告.普通检验报告子项结果.药敏及结果 (5).xlsx")
data_jianyan_5 <- read_excel("C:/Users/pyz/Desktop/血液肿瘤/血液科数据/检验报告/非隐私信息.就诊基本信息.普通检验报告.普通检验报告子项结果.药敏及结果 (6).xlsx")
data_jianyan_6 <- read_excel("C:/Users/pyz/Desktop/血液肿瘤/血液科数据/检验报告/非隐私信息.就诊基本信息.普通检验报告.普通检验报告子项结果.药敏及结果.xlsx")

# 我们可以分别进行精简再合并
#  检查每个数据框包含的患者数
length(unique(data_jianyan_1$患者编号)) # 共有 457 个患者
length(unique(data_jianyan_2$患者编号)) # 共有 618 个患者
length(unique(data_jianyan_3$患者编号)) # 共有 842 个患者
length(unique(data_jianyan_4$患者编号)) # 共有 793 个患者
length(unique(data_jianyan_5$患者编号)) # 共有 372 个患者
length(unique(data_jianyan_6$患者编号)) # 共有 392 个患者
#  进行相同行检查
column_name <- "患者编号"  # 如果列名不同，请修改这里
# 计算相同患者编号的数量
common_patients1 <- length(intersect(data_jianyan_1[[column_name]], data_jianyan_2[[column_name]]))
common_patients2 <- length(intersect(data_jianyan_1[[column_name]], data_jianyan_3[[column_name]]))
common_patients3 <- length(intersect(data_jianyan_1[[column_name]], data_jianyan_4[[column_name]]))
common_patients4 <- length(intersect(data_jianyan_1[[column_name]], data_jianyan_5[[column_name]]))
common_patients5 <- length(intersect(data_jianyan_1[[column_name]], data_jianyan_6[[column_name]]))
# 打印结果
print(paste("两个数据集中相同的患者编号数量为：", common_patients1))
print(paste("三个数据集中相同的患者编号数量为：", common_patients2))
print(paste("四个数据集中相同的患者编号数量为：", common_patients3))
print(paste("五个数据集中相同的患者编号数量为：", common_patients4))
print(paste("六个数据集中相同的患者编号数量为：", common_patients5))  ###  其他的排列组合也进行了比较，似乎缺失没有相同的患者编号

# 确认有多少检测项目
length(unique(data_jianyan_1$检验项目名称)) # 有 2475个检测项目
length(unique(data_jianyan_2$检验项目名称)) # 有 2277个检测项目
length(unique(data_jianyan_3$检验项目名称)) # 有 2284个检测项目
length(unique(data_jianyan_4$检验项目名称)) # 有 2171个检测项目
length(unique(data_jianyan_5$检验项目名称)) # 有 889个检测项目
length(unique(data_jianyan_6$检验项目名称)) # 有 2404个检测项目

# 确认有多少检验子项中文名
length(unique(data_jianyan_1$检验子项中文名)) # 有1408个检验子项中文名
length(unique(data_jianyan_2$检验子项中文名)) # 有 1350 个检验子项中文名
length(unique(data_jianyan_3$检验子项中文名)) # 有 1287 个检验子项中文名
length(unique(data_jianyan_4$检验子项中文名)) # 有 984 个检验子项中文名
length(unique(data_jianyan_5$检验子项中文名)) # 有 656 个检验子项中文名
length(unique(data_jianyan_6$检验子项中文名)) # 有 1426 个检验子项中文名

# 检查鑫琦的数据
length(unique(data_jianyan$项目名称)) # 有 3798个检查项目
length(unique(data_jianyan$子项名称)) # 有 1234个子项

#  保留第1，3，5，6，7，8列
data_jianyan_1 <- data_jianyan_1[,c(1,3,5,6,7,8,14,15,16,17,18,19)]
data_jianyan_2 <- data_jianyan_2[,c(1,3,5,6,7,8,14,15,16,17,18,19)]
data_jianyan_3 <- data_jianyan_3[,c(1,3,5,6,7,8,14,15,16,17,18,19)]
data_jianyan_4 <- data_jianyan_4[,c(1,3,5,6,7,8,14,15,16,17,18,19)]
data_jianyan_5 <- data_jianyan_5[,c(1,3,5,6,7,8,14,15,16,17,18,19)]
data_jianyan_6 <- data_jianyan_6[,c(1,3,5,6,7,8,14,15,16,17,18,19)]

# 造血功能相关关键词（中文）
keywords_cn_zaoxue <- c(
  "白细胞", "红细胞", "血红蛋白", "血小板", 
  "中性粒", "嗜酸", "嗜碱", "淋巴", "单核", 
  "平均血红蛋白", "红细胞分布", "血细胞比容",
  "MCH", "MCV", "RDW", "HCT", "PLT", "PDW"
)
# 免疫功能相关关键词（中文）
keywords_cn_mianyi <- c(
  "免疫球蛋白", "IgG", "IgA", "IgM", "IgE", 
  "补体", "C3", "C4", "CD3", "CD4", "CD8", "CD19", "CD56",
  "IL-2", "IL-6", "IL-10", "TNF", "干扰素", "INF", "白细胞介素", 
  "炎症因子", "细胞因子", "肿瘤坏死因子", "趋化因子",
  "巨噬细胞", "吞噬功能", "M1", "M2", "髓系抑制细胞", "MDSC", 
  "树突状细胞", "DC", "CD68", "CD163",
  "免疫细胞", "淋巴细胞亚群", "T细胞亚群", "B细胞亚群", 
  "NK细胞", "自然杀伤细胞", "CD标记"
)

# 对data_jianyan_1进行模糊匹配
df_filtered_1 <- data_jianyan_1 %>%
  mutate(
    功能分类 = case_when(
      str_detect(检验子项中文名, paste(keywords_cn_zaoxue, collapse = "|")) ~ "造血功能",
      str_detect(检验子项中文名, paste(keywords_cn_mianyi, collapse = "|")) ~ "免疫功能",
      TRUE ~ "其他"
    )
  ) %>%
  filter(功能分类 != "其他")
# 输出一些信息
cat("总共筛选出的相关检验子项条数：", nrow(df_filtered_1), "\n")
cat("其中独立中文子项名数量：", length(unique(df_filtered_1$检验子项中文名)), "\n")
# 确认还有多少检验项目
length(unique(df_filtered_1$检验项目名称)) # 有 997 个检验项目

# 对data_jianyan_2进行模糊匹配
df_filtered_2 <- data_jianyan_2 %>%
  mutate(
    功能分类 = case_when(
      str_detect(检验子项中文名, paste(keywords_cn_zaoxue, collapse = "|")) ~ "造血功能",
      str_detect(检验子项中文名, paste(keywords_cn_mianyi, collapse = "|")) ~ "免疫功能",
      TRUE ~ "其他"
    )
  ) %>%
  filter(功能分类 != "其他")
# 输出一些信息
cat("总共筛选出的相关检验子项条数：", nrow(df_filtered_2), "\n")
cat("其中独立中文子项名数量：", length(unique(df_filtered_2$检验子项中文名)), "\n")
# 确认还有多少检验项目
length(unique(df_filtered_2$检验项目名称)) # 有 945 个检验项目

# 对data_jianyan_3进行模糊匹配
df_filtered_3 <- data_jianyan_3 %>%
  mutate(
    功能分类 = case_when(
      str_detect(检验子项中文名, paste(keywords_cn_zaoxue, collapse = "|")) ~ "造血功能",
      str_detect(检验子项中文名, paste(keywords_cn_mianyi, collapse = "|")) ~ "免疫功能",
      TRUE ~ "其他"
    )
  ) %>%
  filter(功能分类 != "其他")
# 输出一些信息
cat("总共筛选出的相关检验子项条数：", nrow(df_filtered_3), "\n")
cat("其中独立中文子项名数量：", length(unique(df_filtered_3$检验子项中文名)), "\n")
# 确认还有多少检验项目
length(unique(df_filtered_3$检验项目名称)) # 有944个检验项目

# 对data_jianyan_4进行模糊匹配
df_filtered_4 <- data_jianyan_4 %>%
  mutate(
    功能分类 = case_when(
      str_detect(检验子项中文名, paste(keywords_cn_zaoxue, collapse = "|")) ~ "造血功能",
      str_detect(检验子项中文名, paste(keywords_cn_mianyi, collapse = "|")) ~ "免疫功能",
      TRUE ~ "其他"
    )
  ) %>%
  filter(功能分类 != "其他")
# 输出一些信息
cat("总共筛选出的相关检验子项条数：", nrow(df_filtered_4), "\n")
cat("其中独立中文子项名数量：", length(unique(df_filtered_4$检验子项中文名)), "\n")
# 确认还有多少检验项目
length(unique(df_filtered_4$检验项目名称)) # 有 718 个检验项目

# 对data_jianyan_5进行模糊匹配
df_filtered_5 <- data_jianyan_5 %>%
  mutate(
    功能分类 = case_when(
      str_detect(检验子项中文名, paste(keywords_cn_zaoxue, collapse = "|")) ~ "造血功能",
      str_detect(检验子项中文名, paste(keywords_cn_mianyi, collapse = "|")) ~ "免疫功能",
      TRUE ~ "其他"
    )
  ) %>%
  filter(功能分类 != "其他")
# 输出一些信息
cat("总共筛选出的相关检验子项条数：", nrow(df_filtered_5), "\n")
cat("其中独立中文子项名数量：", length(unique(df_filtered_5$检验子项中文名)), "\n")
# 确认还有多少检验项目
length(unique(df_filtered_5$检验项目名称)) # 有 200 个检验项目

# 对data_jianyan_6进行模糊匹配
df_filtered_6 <- data_jianyan_6 %>%
  mutate(
    功能分类 = case_when(
      str_detect(检验子项中文名, paste(keywords_cn_zaoxue, collapse = "|")) ~ "造血功能",
      str_detect(检验子项中文名, paste(keywords_cn_mianyi, collapse = "|")) ~ "免疫功能",
      TRUE ~ "其他"
    )
  ) %>%
  filter(功能分类 != "其他")
# 输出一些信息
cat("总共筛选出的相关检验子项条数：", nrow(df_filtered_6), "\n")
cat("其中独立中文子项名数量：", length(unique(df_filtered_6$检验子项中文名)), "\n")
# 确认还有多少检验项目
length(unique(df_filtered_6$检验项目名称)) # 有 896 个检验项目

##### 将检验项目合并
data_jianyan_combined <- rbind(df_filtered_1,df_filtered_2,df_filtered_3,df_filtered_4,df_filtered_5,df_filtered_6)

#  确认标本名称有多少个
length(unique(data_jianyan_combined$标本名称)) # 有 45 个标本名称
unique(data_jianyan_combined$标本名称) # 显示所有标本名称

# 将阴性/NEGATIVE/负号”等表述统一标准化
data_jianyan_combined <- data_jianyan_combined %>%
  mutate(
    检验子项结果 = case_when(
      str_detect(检验子项结果, regex("(?i)negative|阴性|\\-", ignore_case = TRUE)) ~ "negative",
      TRUE ~ 检验子项结果
    )
  )
# 将“阳性/positive/正号”等表述统一标准化
data_jianyan_combined <- data_jianyan_combined %>%
  mutate(
    检验子项结果 = case_when(
      str_detect(检验子项结果, regex("(?i)positive|阳性|\\+", ignore_case = TRUE)) ~ "positive",
      TRUE ~ 检验子项结果
    )
  )

# 创建词表（去重后）
wordbook <- data_jianyan_combined %>%
  dplyr::select(检验子项英文名, 检验子项中文名, 检验子项单位) %>%
  distinct() %>%
  arrange(检验子项中文名)

# 查看前几行
head(wordbook)
# 保存wordbook,包含中文
write_excel_csv(wordbook, '检验子项词表.csv')

## 保存RData
save.image("检验数据与护理数据.RData")

load("检验数据与护理数据.RData")

######## 2.分离标本名称 ######## 
# 将第2列重命名为就诊编号
colnames(data_jianyan_combined)[2] <- "就诊编号"
# 尿液的检测
jianyan_niao <- data_jianyan_combined %>%
  filter(标本名称 == "尿")
# 确认有多少检验子项
length(unique(jianyan_niao$检验子项英文名)) # 有 11 个检验子项
unique(jianyan_niao$检验子项英文名) # 显示所有项目名称
# 确认有多少检验项目名称
length(unique(jianyan_niao$检验项目名称)) # 有 86 个检验项目名称
unique(jianyan_niao$检验项目名称) # 显示所有检验项目名称
# 找到哪种检验项目名称的行最多
jianyan_niao %>%
  group_by(检验项目名称) %>%
  summarise(行数 = n()) %>%
  arrange(desc(行数)) # 检验项目名称为“尿液分析检测”的行最多，共有17768行，其次是尿液干化学分析+尿沉渣定量检测
# 确认测量结果列为字符形式
jianyan_niao$检验子项结果 <- as.character(jianyan_niao$检验子项结果)
# 确认检验子项为NA的个数
sum(is.na(jianyan_niao$检验子项中文名)) # 0个NA
# 剔除第4,6,7,8,11,12列
jianyan_niao <- jianyan_niao[,-c(4,6,7,8,11,12)]
# 由于一个检验项目的有多个检测指标，而多个检验项目测量时间不同，所以还是需要固定在一个检验项目中去收集指标
# 我们取行数排名前三的检验项目
# 尿液分析检测
jianyan_niao_1 <- jianyan_niao %>%
  filter(检验项目名称 == "尿液分析检测")
# 尿液干化学分析+尿沉渣定量检测
jianyan_niao_2 <- jianyan_niao %>%
  filter(检验项目名称 == "尿液干化学分析+尿沉渣定量检测")
# 尿液干化学分析
jianyan_niao_3 <- jianyan_niao %>%
  filter(检验项目名称 == "尿液干化学分析")

# 转置项目名称的数据框
jianyan_niao_combined_1 <- jianyan_niao_1 %>%
  group_by(患者编号, 就诊编号, 采集时间) %>%
  slice(1) %>%  # 保留每组中的第一条记录
  ungroup() %>%
  pivot_wider(names_from = 检验子项英文名, values_from = 检验子项结果)
# 检查患者数
length(unique(jianyan_niao_combined_1$患者编号)) # 共有 1266 个患者
# 检查缺失值
colSums(is.na(jianyan_niao_combined_1)) #缺失值依然不少，最大的问题是即使是同一个检测，每个指标的检测时间点依然存在差异，一次可能只检测了一个指标




################ 三、进行泰勒展开 ###############
## 1.梳理数据
#### 血型和输血情况（开始、结束时间与输血前体温）：data_blood_combined, 血糖情况：data_blood_glucose_combined
#### New营养风险级身高体重、BMI：data_fengxian_combined， 护理操作、病情及处置记录单：data_record_combined
####  日常生活能力量表：data_richang_combined， 营养风险筛查表(成人): data_yingyang_combined

#### 2.首先是日常生活能力评定量表 ####
# 保证日期为 Date 类型
data_richang_combined$记录日期 <- as.Date(data_richang_combined$记录日期)
# 检查缺失值
colSums(is.na(data_richang_combined)) #共有 14040 行，量表评分没有缺失值

## 去除第二列就诊编号，存在相同患者编号但就诊编号不同的情况，时间应该算到一块
data_richang_filtered <- data_richang_combined[, -2]

# 构建“天数”变量（相对时间）
data_richang_filtered  <- data_richang_filtered  %>%
  group_by(患者编号) %>%
  arrange(记录日期) %>%
  mutate(day_index = as.numeric(记录日期 - min(记录日期))) %>%
  ungroup()

# 对每位患者进行二次多项式拟合（f(t) = a + b*t + c*t^2）
# 添加 R² 与 p 值作为检验
taylor_fit_results_detailed <- data_richang_filtered %>%
  group_by(患者编号) %>%
  filter(n() >= 3) %>%
  do({
    model <- lm(日常生活能力评定量表 ~ day_index + I(day_index^2), data = .)
    r2 <- summary(model)$r.squared  # 提取 R²
    coef_df <- tidy(model) %>%
      filter(term != "(Intercept)") %>%
      dplyr::select(term, estimate) %>%
      pivot_wider(names_from = term, values_from = estimate)%>%
    rename(b = day_index, c = `I(day_index^2)`)
    coef_df$r2 <- r2  # 添加 r² 列
    coef_df
  }) %>%
  ungroup()

# 输出示例列名将是：day_index（一阶导数），I(day_index^2)（二阶导数）
head(taylor_fit_results_detailed)

# 简单符号统计
summary_stats <- taylor_fit_results_detailed %>%
  mutate(
    trend = case_when(
      b > 0 & c > 0 ~ "持续改善",
      b > 0 & c < 0 ~ "先改善后趋缓",
      b < 0 & c > 0 ~ "先恶化后缓解",
      b < 0 & c < 0 ~ "持续恶化",
      TRUE ~ "不确定"
    )
  ) %>%
  count(trend)
print(summary_stats)

# 选取有完整拟合系数的患者（去除NA）
matrix_ready <- taylor_fit_results_detailed %>%
  filter(!is.na(b), !is.na(c)) %>%
  mutate(
    trend_coef = b,
    curvature_coef = c
  ) %>%
  dplyr::select(患者编号,trend_coef, curvature_coef)
# 提取矩阵（只要系数部分）
rhythm_matrix <- as.matrix(matrix_ready[, c("trend_coef", "curvature_coef")])
rownames(rhythm_matrix) <- paste0(matrix_ready$患者编号)

# 可视化
p <- ggplot(taylor_fit_results_detailed, aes(x = b, y = c)) +
  geom_point(aes(color = case_when(
    b > 0 & c > 0 ~ "Continuous Improvement",
    b > 0 & c < 0 ~ "Initial Improvement Then Plateau",
    b < 0 & c > 0 ~ "Initial Deterioration Then Recovery",
    b < 0 & c < 0 ~ "Continuous Deterioration",
    TRUE ~ "Uncertain"
  ),
  alpha = r2), size = 2) +
  scale_color_manual(values = c(
    "Continuous Improvement" = "#1b9e77",
    "Initial Improvement Then Plateau" = "#7570b3",
    "Initial Deterioration Then Recovery" = "#d95f02",
    "Continuous Deterioration" = "#e7298a",
    "Uncertain" = "grey50")) +
  labs(title = "Quadrant Plot of Clinical Progression Trends",
       x = "Linear Coefficient (b)",
       y = "Quadratic Coefficient (c)",
       color = "Trend Type",
       alpha = "R-squared") +
  theme_minimal()
p
ggsave("clinical_progression_trends.pdf", plot = p, width = 15, height = 10)


#### 3.营养风险筛查表(成人) ####
# 保证日期为 Date 类型
data_yinyang_combined$记录日期 <- as.Date(data_yinyang_combined$记录日期)
# 重命名第4列为score
colnames(data_yinyang_combined)[4] <- "score"
# 检查缺失值
colSums(is.na(data_yinyang_combined)) #共有 6093 行，量表评分没有缺失值
## 去除第二列就诊编号，存在相同患者编号但就诊编号不同的情况，时间应该算到一块
data_yinyang_filtered <- data_yinyang_combined[, -2]

# 构建“天数”变量（相对时间）
data_yinyang_filtered  <- data_yinyang_filtered  %>%
  group_by(患者编号) %>%
  arrange(记录日期) %>%
  mutate(day_index = as.numeric(记录日期 - min(记录日期))) %>%
  ungroup()

# 对每位患者进行二次多项式拟合（f(t) = a + b*t + c*t^2）
# 添加 R² 与 p 值作为检验
taylor_fit_results_yinyang <- data_yinyang_filtered %>%
  group_by(患者编号) %>%
  filter(n() >= 3) %>%
  do({
    model <- lm(score ~ day_index + I(day_index^2), data = .)
    r2 <- summary(model)$r.squared  # 提取 R²
    coef_df <- tidy(model) %>%
      filter(term != "(Intercept)") %>%
      dplyr::select(term, estimate) %>%
      pivot_wider(names_from = term, values_from = estimate)%>%
      rename(b = day_index, c = `I(day_index^2)`)
    coef_df$r2 <- r2  # 添加 r² 列
    coef_df
  }) %>%
  ungroup()

# 输出示例列名将是：day_index（一阶导数），I(day_index^2)（二阶导数）
head(taylor_fit_results_yinyang)

# 简单符号统计
summary_stats2 <- taylor_fit_results_yinyang %>%
  mutate(
    trend = case_when(
      b > 0 & c > 0 ~ "持续改善",
      b > 0 & c < 0 ~ "先改善后趋缓",
      b < 0 & c > 0 ~ "先恶化后缓解",
      b < 0 & c < 0 ~ "持续恶化",
      TRUE ~ "不确定"
    )
  ) %>%
  count(trend)
print(summary_stats2)

# 选取有完整拟合系数的患者（去除NA）
matrix_ready <- taylor_fit_results_yinyang %>%
  filter(!is.na(b), !is.na(c)) %>%
  mutate(
    trend_coef = b,
    curvature_coef = c
  ) %>%
  dplyr::select(患者编号,trend_coef, curvature_coef)
# 提取矩阵（只要系数部分）
rhythm_matrix <- as.matrix(matrix_ready[, c("trend_coef", "curvature_coef")])
rownames(rhythm_matrix) <- paste0(matrix_ready$患者编号)

# 可视化
p <- ggplot(taylor_fit_results_yinyang, aes(x = b, y = c)) +
  geom_point(aes(color = case_when(
    b > 0 & c > 0 ~ "Continuous Improvement",
    b > 0 & c < 0 ~ "Initial Improvement Then Plateau",
    b < 0 & c > 0 ~ "Initial Deterioration Then Recovery",
    b < 0 & c < 0 ~ "Continuous Deterioration",
    TRUE ~ "Uncertain"
  ),
  alpha = r2), size = 2) +
  scale_color_manual(values = c(
    "Continuous Improvement" = "#1b9e77",
    "Initial Improvement Then Plateau" = "#7570b3",
    "Initial Deterioration Then Recovery" = "#d95f02",
    "Continuous Deterioration" = "#e7298a",
    "Uncertain" = "grey50")) +
  labs(title = "Quadrant Plot of Clinical Progression Trends",
       x = "Linear Coefficient (b)",
       y = "Quadratic Coefficient (c)",
       color = "Trend Type",
       alpha = "R-squared") +
  theme_minimal()
p
ggsave("营养评分变化.pdf", plot = p, width = 15, height = 10)

### 4.New营养风险筛查表：data_fengxian_combined
# 保证日期为 Date 类型
data_fengxian_combined$记录日期 <- as.Date(data_fengxian_combined$记录日期)
# 重命名第4列为score
colnames(data_fengxian_combined)[4] <- "score"
# 检查缺失值
colSums(is.na(data_fengxian_combined)) #共有 6093 行，量表评分没有缺失值
## 去除第二列就诊编号，存在相同患者编号但就诊编号不同的情况，时间应该算到一块
data_fengxian_combined <- data_fengxian_combined[, -2]
# 构建“天数”变量（相对时间）
data_fengxian_combined  <- data_fengxian_combined  %>%
  group_by(患者编号) %>%
  arrange(记录日期) %>%
  mutate(day_index = as.numeric(记录日期 - min(记录日期))) %>%
  ungroup()
# 对每位患者进行二次多项式拟合（f(t) = a + b*t + c*t^2）
# 添加 R² 与 p 值作为检验
taylor_fit_results_fengxian <- data_fengxian_combined %>%
  group_by(患者编号) %>%
  filter(n() >= 3) %>%
  do({
    model <- lm(score ~ day_index + I(day_index^2), data = .)
    r2 <- summary(model)$r.squared  # 提取 R²
    coef_df <- tidy(model) %>%
      filter(term != "(Intercept)") %>%
      dplyr::select(term, estimate) %>%
      pivot_wider(names_from = term, values_from = estimate)%>%
      rename(b = day_index, c = `I(day_index^2)`)
    coef_df$r2 <- r2  # 添加 r² 列
    coef_df
  }) %>%
  ungroup()
# 输出示例列名将是：day_index（一阶导数），I(day_index^2)（二阶导数）
head(taylor_fit_results_fengxian)
# 简单符号统计
summary_stats3 <- taylor_fit_results_fengxian %>%
  mutate(
    trend = case_when(
      b > 0 & c > 0 ~ "持续改善",
      b > 0 & c < 0 ~ "先改善后趋缓",
      b < 0 & c > 0 ~ "先恶化后缓解",
      b < 0 & c < 0 ~ "持续恶化",
      TRUE ~ "不确定"
    )
  ) %>%
  count(trend)
print(summary_stats3)
# 选取有完整拟合系数的患者（去除NA）
matrix_ready <- taylor_fit_results_fengxian %>%
  filter(!is.na(b), !is.na(c)) %>%
  mutate(
    trend_coef = b,
    curvature_coef = c
  ) %>%
  dplyr::select(患者编号,trend_coef, curvature_coef)
# 提取矩阵（只要系数部分）
rhythm_matrix3 <- as.matrix(matrix_ready[, c("trend_coef", "curvature_coef")])
rownames(rhythm_matrix3) <- paste0(matrix_ready$患者编号)

# 可视化
p <- ggplot(taylor_fit_results_fengxian, aes(x = b, y = c)) +
  geom_point(aes(color = case_when(
    b > 0 & c > 0 ~ "Continuous Improvement",
    b > 0 & c < 0 ~ "Initial Improvement Then Plateau",
    b < 0 & c > 0 ~ "Initial Deterioration Then Recovery",
    b < 0 & c < 0 ~ "Continuous Deterioration",
    TRUE ~ "Uncertain"
  ),
  alpha = r2), size = 2) +
  scale_color_manual(values = c(
    "Continuous Improvement" = "#1b9e77",
    "Initial Improvement Then Plateau" = "#7570b3",
    "Initial Deterioration Then Recovery" = "#d95f02",
    "Continuous Deterioration" = "#e7298a",
    "Uncertain" = "grey50")) +
  labs(title = "Nutritional risk trends",
       x = "Linear Coefficient (b)",
       y = "Quadratic Coefficient (c)",
       color = "Trend Type",
       alpha = "R-squared") +
  theme_minimal()
p
ggsave("营养风险变化.pdf", plot = p, width = 15, height = 10)

######### 四、尝试构建cox比例风险模型 #########
# 导入数据
load("检验数据与护理数据.RData")
# 重新处理一下一般人口学数据
# 吕的数据中的诊断数据
data_geren2 <- read_excel("C:/Users/pyz/Desktop/血液肿瘤/血液科数据/护理记录/非隐私信息.就诊基本信息.病案首页基本信息.病案首页诊断信息.xlsx")
# 重命名
colnames(data_geren2)[3] <- "就诊编号"
# 保留患者编号，就诊编号，性别，年龄，婚姻状况，国籍，职业，诊断名称，诊断归转情况
data_geren2 <- data_geren2 %>%
  dplyr::select(患者编号, 就诊编号, 性别, 年龄, 入院日期,出院日期,实际住院天数,婚姻状况, 国籍, 职业, 诊断名称, 
                诊断归转情况,诊断类型,入院科室名称,出院科室名称)
length(unique(data_geren2$患者编号))
# 将日期转换为Date类型
data_geren2$入院日期 <- as.Date(data_geren2$入院日期)
data_geren2$出院日期 <- as.Date(data_geren2$出院日期)
# 检索诊断归转情况分布
table(data_geren2$诊断归转情况) # 共有 5 种情况，好转（92958）、其他（31314）、
# 死亡（1557）、未愈（947）、治愈（11173）

# 检索诊断名称分布，有几千个名称
# 只选择重要血液肿瘤疾病类型
# 先处理缺失值,将"-"和"无"转变为对应行诊断类列的值
data_geren2$诊断名称 <- as.character(data_geren2$诊断名称)
data_geren2$诊断名称 <- ifelse(is.na(data_geren2$诊断名称) | data_geren2$诊断名称 %in% c("-", "无"), 
                           data_geren2$诊断类型, 
                           data_geren2$诊断名称)

# 标准化诊断名称为小写，去空格
data_geren2$诊断名称 <- data_geren2$诊断名称 %>%
  str_squish() %>%
  str_to_lower()

# 更新后的关键词字典
hematologic_subtypes <- list(
  "急性早幼粒细胞白血病" = c("急性早幼粒细胞白血病", "早幼粒细胞", "m3"),
  "急性髓系白血病" = c("急性髓系白血病"),
  "急性淋巴细胞白血病" = c("急性淋巴细胞白血病"),
  "慢性髓系白血病" = c("慢性髓系白血病"),
  "慢性淋巴细胞白血病" = c("慢性淋巴细胞白血病"),
  "多发性骨髓瘤" = c("多发性骨髓瘤", "骨髓瘤"),
  # 先放“非霍奇金”
  "非霍奇金淋巴瘤" = c("非霍奇金淋巴瘤", "b细胞淋巴瘤", "t细胞淋巴瘤"),
  "霍奇金淋巴瘤" = c("霍奇金淋巴瘤"),
  "骨髓增生异常综合征" = c("骨髓增生异常综合征", "mds"),
  "其他白血病" = c("白血病", "其他白血病")
)

# 分类函数
assign_subtype <- function(diagnosis_text, subtype_dict) {
  if (is.na(diagnosis_text)) return("未分类")
  if (str_detect(diagnosis_text, "良性|疑似|待排除")) return("其他良性或未确诊")
  
  for (subtype in names(subtype_dict)) {
    keywords <- subtype_dict[[subtype]]
    for (keyword in keywords) {
      if (str_detect(diagnosis_text, keyword)) {
        return(subtype)
      }
    }
  }
  return("未分类")
}

# 应用归类
data_geren2$疾病分类 <- sapply(data_geren2$诊断名称, assign_subtype, subtype_dict = hematologic_subtypes)

# 替换函数：将每个患者的“未分类”替换为已有的分类
data_geren2 <- data_geren2 %>%
  group_by(患者编号) %>%
  mutate(
    # 提取非“未分类”的唯一分类
    非未分类标签 = unique(疾病分类[疾病分类 != "未分类"])[1],
    # 替换“未分类”为该患者的已有分类
    疾病分类更新 = ifelse(疾病分类 == "未分类" & !is.na(非未分类标签), 非未分类标签, 疾病分类)
  ) %>%
  ungroup() %>%
  # 更新列名，删除临时列
  dplyr::select(-疾病分类, -非未分类标签) %>%
  rename(疾病分类 = 疾病分类更新)

### 将一般人口学资料整理好
####### 4.1 日常生活能力量表 #######
data_richang_combined <- data_richang_combined %>%
  mutate(记录日期 = as.Date(记录日期))
# 重命名第4列为richang_score
colnames(data_richang_combined)[4] <- "richang_score"
# 去除data_geren1第2列
data_geren1 <- data_geren1[, -2]

## 构建每个协变量模块的数据框
data_richang_model <- data_geren2 %>%
  left_join(data_geren1, by = "患者编号") %>%
  mutate(
    richang_score = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在评估表中的记录
      df <- data_richang_combined[data_richang_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次评估
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次评估
      df <- df[which.max(df$记录日期), ]
      return(df$richang_score)  
    }),
    
    # 添加对应的记录日期（保持Date类型）
    richang_score_date = map_chr(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在评估表中的记录
      df <- data_richang_combined[data_richang_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_character_)
      
      # 找到入院日期之前/最近的一次评估
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_character_)
      
      # 最近一次评估
      df <- df[which.max(df$记录日期), ]
      return(as.character(df$记录日期))  # 转换为字符型保存
    }) %>% as.Date()  # 最后转换回Date类型
  )

# 删除诊断名称列，去重，然后去除日常功能评分为NA的行
data_richang_model_clean <- data_richang_model %>%
  # 删除诊断名称列（假设列名为"诊断名称"）
  dplyr::select(-"诊断名称") %>%
  # 或者如果不确定具体列名，可以使用：
  # select(!contains("诊断名称")) %>%
  
  # 对相同患者编号中各列值完全相同的行进行去重
  distinct() %>%
  
  # 去除日常功能评分为NA的行
  filter(!is.na(richang_score))
# 检测日常功能模块的患者数
length(unique(data_richang_model_clean$患者编号)) ##有1831个患者


###### 4.2 成人营养量表 #######
# 重命名data_yinyang_combined的第4，5，9列为yingyang_score，weight,BMI
colnames(data_yinyang_combined)[c(4, 5, 7,9)] <- c("yingyang_score", "weight","height","BMI")
# 将这四列转化为数值类型
data_yinyang_combined <- data_yinyang_combined %>%
  mutate(
    yingyang_score = as.numeric(as.character(yingyang_score)),
    weight = as.numeric(as.character(weight)),
    height = as.numeric(as.character(height)),
    BMI = as.numeric(as.character(BMI))
  )

# 将记录日期转化为Date
data_yinyang_combined <- data_yinyang_combined %>%
  mutate(记录日期 = as.Date(记录日期)) %>%
  # 计算BMI：当BMI为空时，根据weight和height计算
  mutate(
    BMI = case_when(
      # 如果BMI已经有值，则保持原值
      !is.na(BMI) ~ BMI,
      # 如果weight和height都有值，则计算BMI
      !is.na(weight) & !is.na(height) & height > 0 ~ weight / (height^2),
      # 其他情况保持NA
      TRUE ~ NA_real_
    )
  ) %>%
  # 去除weight、BMI和height都为NA的行
  filter(!(is.na(weight) & is.na(BMI) & is.na(height)))

# 去除BMI为NA的行
data_yinyang_combined <- data_yinyang_combined %>%
  filter(!is.na(BMI))
# 检查去除后还有多少患者
length(unique(data_yinyang_combined$患者编号)) # 799个患者

# 处理数据
data_yinyang_model <- data_geren2 %>%
  left_join(data_geren1, by = "患者编号") %>%
  mutate(
    yinyang_score = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在营养评估表中的记录
      df <- data_yinyang_combined[data_yinyang_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次评估
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次评估
      df <- df[which.max(df$记录日期), ]
      return(df$yingyang_score)  
    }),
    
    # BMI数据
    BMI = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在营养评估表中的记录
      df <- data_yinyang_combined[data_yinyang_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次评估
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次评估
      df <- df[which.max(df$记录日期), ]
      return(df$BMI)  
    }),
    
    # 成人营养量表对应的记录日期
    yinyang_score_date = map_chr(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在营养评估表中的记录
      df <- data_yinyang_combined[data_yinyang_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_character_)
      
      # 找到入院日期之前/最近的一次评估
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_character_)
      
      # 最近一次评估
      df <- df[which.max(df$记录日期), ]
      return(as.character(df$记录日期))  # 转换为字符型保存
    }) %>% as.Date()  # 最后转换回Date类型
  )

summary(data_yinyang_model$yinyang_score)
# 删除诊断名称列，去重，然后去除日常功能评分为NA的行
data_yinyang_model_clean <- data_yinyang_model %>%
  # 删除诊断名称列（假设列名为"诊断名称"）
  dplyr::select(-"诊断名称") %>%
  # 或者如果不确定具体列名，可以使用：
  # select(!contains("诊断名称")) %>%
  
  # 对相同患者编号中各列值完全相同的行进行去重
  distinct() %>%
  
  # 去除日常功能评分为NA的行
  filter(!is.na(yinyang_score))
# 检测日常功能模块的患者数
length(unique(data_yinyang_model_clean$患者编号)) ##有767个患者


###### 4.3 营养风险筛查表 #######
# 重命名data_fengxian_combined的第4,5列为fengxian_score
colnames(data_fengxian_combined)[c(4, 5)] <- c("fengxian_score","BMI")
# 将这四列转化为数值类型
data_fengxian_combined <- data_fengxian_combined %>%
  mutate(
    fengxian_score = as.numeric(as.character(fengxian_score)),
    BMI = as.numeric(as.character(BMI))
  )

# 将记录日期转化为Date
data_fengxian_combined <- data_fengxian_combined %>%
  mutate(记录日期 = as.Date(记录日期)) %>%
  # 去除weight、BMI和height都为NA的行
  filter(!(is.na(fengxian_score)))

# 处理数据
data_fengxian_model <- data_geren2 %>%
  left_join(data_geren1, by = "患者编号") %>%
  mutate(
    fengxian_score = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在营养评估表中的记录
      df <- data_fengxian_combined[data_fengxian_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次评估
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次评估
      df <- df[which.max(df$记录日期), ]
      return(df$fengxian_score)  
    }),
    
    # 营养风险量表对应的记录日期
    fengxian_score_date = map_chr(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在营养评估表中的记录
      df <- data_fengxian_combined[data_fengxian_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_character_)
      
      # 找到入院日期之前/最近的一次评估
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_character_)
      
      # 最近一次评估
      df <- df[which.max(df$记录日期), ]
      return(as.character(df$记录日期))  # 转换为字符型保存
    }) %>% as.Date()  # 最后转换回Date类型
  )
summary(data_fengxian_model$fengxian_score)
# 删除诊断名称列，去重，然后去除日常功能评分为NA的行
data_fengxian_model_clean <- data_fengxian_model %>%
  # 删除诊断名称列（假设列名为"诊断名称"）
  dplyr::select(-"诊断名称") %>%
  # 或者如果不确定具体列名，可以使用：
  # select(!contains("诊断名称")) %>%
  
  # 对相同患者编号中各列值完全相同的行进行去重
  distinct() %>%
  
  # 去除日常功能评分为NA的行
  filter(!is.na(fengxian_score))
# 检测营养风险模块的患者数
length(unique(data_fengxian_model_clean$患者编号)) ##有1209个患者

# 计算两个数据集中相同患者编号的数量（去重后）
common_patients <- intersect(
  unique(data_fengxian_model_clean$患者编号),
  unique(data_yinyang_model_clean$患者编号)
)
# 相同患者编号的数量
common_patient_count <- length(common_patients)
cat("两个数据集中相同的患者数量:", common_patient_count, "\n")

###### 4.4 生命体征数据 ######
# 重新处理一下护理记录中的数据,没有收缩压
# 转置项目名称的数据框
data_record_combined <- data_record %>%
  group_by(患者编号, 就诊编号, 记录日期, 项目名称) %>%
  slice(1) %>%  # 保留每组中的第一条记录
  ungroup() %>%
  pivot_wider(names_from = 项目名称, values_from = 测量结果)
# 检查缺失值
colSums(is.na(data_record_combined)) #共有1010178行
# 保留第1，2，3，5，6，7列
data_record_combined <- data_record_combined[,c(1,2,3,5,6,7,8,9,10,11,12,13,14,15,16,17,18,21,27,29,30)] 
# 先确认有多少患者
length(unique(data_record_combined$患者编号))  ### 2540个患者

##################################################################### 时间：2025/7/29，还需要补足对生命体征数据的处理，每一天做完分析都保存一下
# 保存R.Data
save.image("第一步数据.RData")
# 导入数据
load("第一步数据.RData")

##################################################################### 时间：2025/7/30
# 我们需要体温，呼吸，收缩压，舒张压，脉搏以及记录日期这六个变量，实际利用五个变量
# 将记录日期转化为Date
data_record_combined_clean <- data_record_combined %>%
  mutate(记录日期 = as.Date(记录日期)) %>%
  # 去除体温，呼吸，收缩压，舒张压，脉搏都为NA的行
  filter(!(is.na(体温) & is.na(呼吸) & is.na(收缩压) & is.na(舒张压) & is.na(脉搏)))
# 只保留需要的列
data_record_combined_clean <- data_record_combined_clean[,c(1,2,3,4,5,12,16,17)] 
# 确认这几列为numeric
data_record_combined_clean <- data_record_combined_clean %>%
  mutate(体温 = as.numeric(体温)) %>%
  mutate(呼吸 = as.numeric(呼吸)) %>%
  mutate(收缩压 = as.numeric(收缩压)) %>%
  mutate(舒张压 = as.numeric(舒张压)) %>%
  mutate(脉搏 = as.numeric(脉搏))
  
# 对于患者编号相同且记录日期相同的重复行进行合并
# 就诊编号列合并时取第一个值，其余列合并取平均值
data_record_combined_clean <- data_record_combined_clean %>%
  group_by(患者编号, 记录日期) %>%
  summarise(
    就诊编号 = first(就诊编号),  # 取第一个就诊编号
    体温 = mean(体温, na.rm = TRUE),     # 取平均值
    呼吸 = mean(呼吸, na.rm = TRUE),     # 取平均值
    收缩压 = mean(收缩压, na.rm = TRUE), # 取平均值
    舒张压 = mean(舒张压, na.rm = TRUE), # 取平均值
    脉搏 = mean(脉搏, na.rm = TRUE),     # 取平均值
    .groups = 'drop'
  )
#  确认样本数
length(unique(data_record_combined_clean$患者编号))  ### 2432个患者
#  检查每个列的缺失数
colSums(is.na(data_record_combined_clean)) #共有75456行
# 检查每个变量的范围
summary(data_record_combined_clean$体温)
summary(data_record_combined_clean$呼吸)
summary(data_record_combined_clean$收缩压)
summary(data_record_combined_clean$舒张压)
summary(data_record_combined_clean$脉搏)

## 进行多重插补
library(mice)
# 保存原始数据用于后续比较和分析
original_data <- data_record_combined_clean[, c("体温", "呼吸", "收缩压", "舒张压", "脉搏")]
# 准备用于插补的数据
mice_data <- data_record_combined_clean[, c("体温", "呼吸", "收缩压", "舒张压", "脉搏")]
# 进行多重插补
set.seed(123)  # 设置随机种子以保证结果可重复
mice_model <- mice(mice_data, 
                   m = 5,           # 生成5个插补数据集
                   method = "pmm",  # 预测均值匹配
                   printFlag = FALSE,
                   seed = 123)
# 获取插补后的完整数据
data_imputed <- complete(mice_model)
# 将插补后的数据合并回原数据框
data_record_combined_clean[, c("体温", "呼吸", "收缩压", "舒张压", "脉搏")] <- data_imputed
# 检查插补结果
cat("插补后各变量的统计摘要：\n")
print(summary(data_record_combined_clean[, c("体温", "呼吸", "收缩压", "舒张压", "脉搏")]))
cat("\n插补后缺失值检查：\n")
print(colSums(is.na(data_record_combined_clean)))

# 处理生命体征数据
data_record_model <- data_geren2 %>%
  left_join(data_geren1, by = "患者编号") %>%
  mutate(
    # 体温
    体温 = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在生命体征记录中的记录
      df <- data_record_combined_clean[data_record_combined_clean$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$体温)) return(NA_real_)
      return(df$体温)  
    }),
    
    # 呼吸
    呼吸 = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在生命体征记录中的记录
      df <- data_record_combined_clean[data_record_combined_clean$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$呼吸)) return(NA_real_)
      return(df$呼吸)  
    }),
    
    # 收缩压
    收缩压 = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在生命体征记录中的记录
      df <- data_record_combined_clean[data_record_combined_clean$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$收缩压)) return(NA_real_)
      return(df$收缩压)  
    }),
    
    # 舒张压
    舒张压 = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在生命体征记录中的记录
      df <- data_record_combined_clean[data_record_combined_clean$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$舒张压)) return(NA_real_)
      return(df$舒张压)  
    }),
    
    # 脉搏
    脉搏 = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在生命体征记录中的记录
      df <- data_record_combined_clean[data_record_combined_clean$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$脉搏)) return(NA_real_)
      return(df$脉搏)  
    }),
    
    # 生命体征记录对应的记录日期
    vital_record_date = map_chr(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在生命体征记录中的记录
      df <- data_record_combined_clean[data_record_combined_clean$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_character_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_character_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$记录日期)) return(NA_character_)
      return(as.character(df$记录日期))  # 转换为字符型保存
    }) %>% as.Date()  # 最后转换回Date类型
  )

# 删除诊断名称列，去重，然后去除体温为NA的行
data_record_model_clean <- data_record_model %>%
  # 删除诊断名称列（假设列名为"诊断名称"）
  dplyr::select(-"诊断名称") %>%
  # 对相同患者编号中各列值完全相同的行进行去重
  distinct() %>%
  # 去除体温为NA的行
  filter(!is.na(体温))
# 检测生命体征的患者数
length(unique(data_record_model_clean$患者编号)) ##有2314个患者

###### 4.5 输血数据 #######
length(unique(data_blood_combined$患者编号))   ### 共有1273个患者
table(data_blood_combined$ABO)
table(data_blood_combined$Rh)
# 重命名第4，5列
colnames(data_blood_combined)[c(4, 5)] <- c("ABO", "Rh")

# 处理ABO血型和Rh血型列中的异常值
data_blood_combined_clean <- data_blood_combined %>%
  mutate(
    ABO = case_when(
      ABO %in% c("1", "0") ~ NA_character_,  # 将"1"和"0"替换为NA
      TRUE ~ ABO
    ),
    Rh = case_when(
      Rh %in% c("1", "0", "01") ~ NA_character_,  # 将"1","0","01"替换为NA
      TRUE ~ Rh
    )
  ) %>%
  # 按患者编号分组，填充缺失值
  group_by(患者编号) %>%
  mutate(
    # 对于ABO血型，用该患者的有效值填充NA值
    ABO = ifelse(is.na(ABO) & any(!is.na(ABO)), 
                 first(ABO[!is.na(ABO)]), 
                 ABO),
    # 对于Rh血型，用该患者的有效值填充NA值
    Rh = ifelse(is.na(Rh) & any(!is.na(Rh)), 
                first(Rh[!is.na(Rh)]), 
                Rh)
  ) %>%
  ungroup() %>%
  # 去除仍然没有有效血型信息的行
  filter(!is.na(ABO) | !is.na(Rh))

## 我们需要将血量的单位统一为ml，需要血量，血型和记录日期
# 检查有哪些单位，单位在数字后面
# 查看血量列的前几行数据，了解数据格式
head(data_blood_combined_clean$血量)
# 检查血量数据中的不同单位
# 提取单位部分（假设单位在数字后面）
units <- unique(gsub("^[0-9.]+", "", as.character(data_blood_combined_clean$血量)))
units <- units[units != ""]  # 去除空字符串
cat("发现的单位:\n")
print(units)
unique(data_blood_combined_clean$血液种类)

# 更详细的单位分析
unit_analysis <- data_blood_combined_clean %>%
  mutate(血量字符 = as.character(血量)) %>%
  mutate(单位 = gsub("^[0-9.]+", "", 血量字符)) %>%
  mutate(数值 = as.numeric(gsub("[^0-9.]+$", "", 血量字符))) %>%
  group_by(单位) %>%
  summarise(
    记录数 = n(),
    数值范围 = paste0(round(min(数值, na.rm = TRUE), 2), "-", round(max(数值, na.rm = TRUE), 2)),
    平均值 = round(mean(数值, na.rm = TRUE), 2),
    .groups = 'drop'
  )

cat("\n各单位数据统计:\n")
print(unit_analysis)

#### 两个重点，一个是统一血量单位，另一个是计算每个患者当天的输血量
#### 1. 清洗单位（只保留首个单位关键词）
clean_unit <- function(unit_raw) {
  unit <- tolower(trimws(unit_raw))
  unit <- gsub("\\(.*?\\)", "", unit)                         # 去括号
  unit <- gsub("^[-~]+", "", unit)                            # 去前缀符号
  unit <- strsplit(unit, "[/+]|\\s+")[[1]][1]                 # 取第一个单位
  unit <- gsub("毫升|ml", "ml", unit)
  unit <- gsub("单位|个单位|一个单位|u|U|iu|Iu|治疗量|dose|人份|一人份|个治疗量|一个治疗量|个治疗单位|一个治疗单位|质量量", "u", unit)
  return(unit)
}

#### 2. 提取数值并计算平均（如多个数值如“100ml/150ml”）
extract_and_average <- function(text) {
  nums <- str_extract_all(text, "[0-9.]+")[[1]]
  nums <- suppressWarnings(as.numeric(nums))
  if (length(nums) == 0 || all(is.na(nums))) return(NA_real_)
  mean(nums, na.rm = TRUE)
}

#### 3. 单位到 ml 的换算因子
conversion_lookup <- function(blood_type, unit) {
  if (is.na(unit) || is.na(blood_type)) return(NA_real_)
  if (unit == "ml") return(1)
  if (unit == "u") {
    if (blood_type %in% c("新鲜冰冻血浆", "病毒灭活血浆")) return(225)
    if (blood_type == "去白细胞悬浮红细胞") return(200)
    if (blood_type == "单采血小板") return(250)
    if (blood_type == "冷沉淀") return(12.5)
    if (blood_type == "全血") return(425)
    return(200)  # 默认
  }
  return(NA_real_)
}

#### 4. 应用数值提取与单位清洗
# 应用提取
data_blood_combined_clean <- data_blood_combined_clean %>%
  mutate(
    数值 = sapply(as.character(血量), extract_and_average),
    原始单位 = str_replace(血量, "^[0-9.]+", ""),
    单位标准 = sapply(原始单位, clean_unit),
    单位标准 = case_when(
      str_detect(单位标准, "ml") ~ "ml",
      str_detect(单位标准, "u") ~ "u",
      is.na(单位标准) & !is.na(数值) & 数值 > 60 ~ "ml",
      is.na(单位标准) & !is.na(数值) & 数值 < 10 ~ "u",
      is.na(单位标准) ~ "u",
      TRUE ~ "u"
    )
  )
#### 5. 换算为 ml
data_blood_combined_clean <- data_blood_combined_clean %>%
  rowwise() %>%
  mutate(
    转换系数 = conversion_lookup(血液种类, 单位标准),
    血量_ml = 数值 * 转换系数
  ) %>%
  ungroup()


# 若患者编号、输血开始时间和血液种类相同，则对血量_ml取平均值，若患者编号、血液种类相同，输血开始时间不同，则对同一天的血量_ml取和值
# 输血开始时间为 2020-12-25 10:16 格式
######## 输血量数据的计算
# 1. 去除 NA 血量
data_blood_clean <- data_blood_combined_clean %>%
  filter(!is.na(血量_ml))
# 2. 第13列重命名为“输血开始时间”
colnames(data_blood_clean)[13] <- "输血开始时间"
#  检查每个列的缺失数
colSums(is.na(data_blood_combined_clean)) #共有75456行
# 3. 确保“输血开始时间”为 character，再转为 datetime 和 date
# 转为字符后统一解析多种时间格式
data_blood_clean <- data_blood_clean %>%
  mutate(
    输血开始时间 = as.character(输血开始时间),
    输血时间_dt = parse_date_time(输血开始时间, orders = c("ymd HMS", "ymd HM", "ymd")),
    输血日期 = as.Date(输血时间_dt)  # 最终只保留日期
  )

# 对患者编号为0023796133，输血日期为2022-07-03,血液种类为病毒灭活血浆和新鲜冰冻血浆的行，转换系数的值改为1
data_blood_clean <- data_blood_clean %>%
  mutate(
    转换系数 = case_when(
      患者编号 == "0023796133" & 
        输血日期 == as.Date("2022-07-03") & 
        血液种类 %in% c("病毒灭活血浆", "新鲜冰冻血浆") ~ 1,
      TRUE ~ 转换系数  # 保持其他行的原有值
    )
  )

# 删除数值中大于5000的行
data_blood_clean <- data_blood_clean %>%
  filter(数值 <= 5000)
# 重新计算血量_ml,血量_ml=数值×转换系数
data_blood_clean <- data_blood_clean %>%
  mutate(血量_ml = 数值 * 转换系数)
# 检查处理结果
cat("处理后的数据统计:\n")
cat("总记录数:", nrow(data_blood_clean), "\n")
cat("血量_ml范围:", range(data_blood_clean$血量_ml, na.rm = TRUE), "\n")
print(summary(data_blood_clean$血量_ml))

# 第一步：按 患者编号 + 输血开始时间 + 血液种类 取平均值
step1 <- data_blood_clean %>%
  group_by(患者编号, 输血开始时间, 血液种类) %>%
  summarise(
    血量_ml = mean(血量_ml, na.rm = TRUE),
    输血日期 = first(输血日期),  # 保留日期信息
    .groups = "drop"
  )
# 第二步：如果同一患者编号+血液种类在同一天多次输血，汇总为当日总量
step2 <- step1 %>%
  group_by(患者编号, 输血日期, 血液种类) %>%
  summarise(
    总血量_ml = sum(血量_ml, na.rm = TRUE),
    .groups = "drop"
  )

# 检查异常值
# 列出step2数据中 总血量_ml 大于2000的行
high_volume_blood <- step2 %>%
  filter(总血量_ml > 2000)
# 查看结果
cat("总血量_ml大于2000的记录数:", nrow(high_volume_blood), "\n")
print(high_volume_blood)

# 将high_volume_blood中的患者编号提出，选择data_blood_clean中有这些患者编号的行
high_volume_patients <- unique(high_volume_blood$患者编号)
# 查看提取的患者编号数量
cat("血量_ml大于1000的患者数量:", length(high_volume_patients), "\n")
# 选择data_blood_clean中有这些患者编号的行
data_blood_high_volume_patients <- data_blood_clean %>%
  filter(患者编号 %in% high_volume_patients)

# 找到异常情况
# 对患者编号为0023796133，输血日期为2022-07-03,血液种类为病毒灭活血浆的行，转换系数的值改为1。转到step1之前做一个筛选

# 确认step2的输血日期为Date类型，血液种类为character类型，总血量_ml为numeric类型
step2 <- step2 %>%
  mutate(
    输血日期 = as.Date(输血日期),
    血液种类 = as.character(血液种类),
    总血量_ml = as.numeric(总血量_ml)
  )

# 处理输血数据
data_blood_model <- data_geren2 %>%
  left_join(data_geren1, by = "患者编号") %>%
  mutate(
    # 总血量_ml
    总血量_ml = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在输血记录中的记录
      df <- step2[step2$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(输血日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$输血日期), ]
      if (nrow(df) == 0 || is.na(df$总血量_ml)) return(NA_real_)
      return(df$总血量_ml)  
    }),
    
    # 血液种类
    血液种类 = map_chr(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在输血记录中的记录
      df <- step2[step2$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_character_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(输血日期 <= admission_date)
      if (nrow(df) == 0) return(NA_character_)
      
      # 最近一次记录
      df <- df[which.max(df$输血日期), ]
      if (nrow(df) == 0 || is.na(df$血液种类)) return(NA_character_)
      return(as.character(df$血液种类))  
    }),
    
    # 输血记录对应的记录日期
    blood_record_date = map_chr(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在输血记录中的记录
      df <- step2[step2$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_character_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(输血日期 <= admission_date)
      if (nrow(df) == 0) return(NA_character_)
      
      # 最近一次记录
      df <- df[which.max(df$输血日期), ]
      if (nrow(df) == 0 || is.na(df$输血日期)) return(NA_character_)
      return(as.character(df$输血日期))  # 转换为字符型保存
    }) %>% as.Date()  # 最后转换回Date类型
  )

# 删除诊断名称列，去重，然后去除总血量_ml为NA的行
data_blood_model_clean <- data_blood_model %>%
  # 删除诊断名称列（假设列名为"诊断名称"）
  dplyr::select(-"诊断名称") %>%
  # 对相同患者编号中各列值完全相同的行进行去重
  distinct() %>%
  # 去除总血量_ml为NA的行
  filter(!is.na(总血量_ml))
# 检测输血数据的患者数
length(unique(data_blood_model_clean$患者编号)) ### 匹配上的有907个患者
length(unique(data_blood_combined_clean$患者编号)) ### 有1206个患者

###### 4.6 生理检测指标 #######
length(unique(data_jianyan_combined$检验子项中文名))
unique(data_jianyan_combined$检验子项中文名)
# 检查多个重要的造血指标
造血指标列表 <- c("促红细胞生成素", "血红蛋白", "红细胞","平均红细胞体积", "白细胞计数", "红细胞分布宽度CV","血小板计数", "糖化血红蛋白")

# 批量检查这些指标
for(指标 in 造血指标列表) {
  指标_data <- data_jianyan_combined %>%
    filter(检验子项中文名 == 指标)
  
  if(nrow(指标_data) > 0) {
    cat(sprintf("\n%s:\n", 指标))
    cat(sprintf("  记录数: %d\n", nrow(指标_data)))
    cat(sprintf("  涉及患者数: %d\n", length(unique(指标_data$患者编号))))
    cat(sprintf("  缺失值: %d\n", sum(is.na(指标_data$检验子项结果))))
    print(summary(指标_data$检验子项结果))
  } else {
    cat(sprintf("\n%s: 无数据\n", 指标))
  }
}

# 计算每个检验指标的记录数和涉及患者数
指标统计 <- data_jianyan_combined %>%
  group_by(检验子项中文名) %>%
  summarise(
    记录数 = n(),
    涉及患者数 = n_distinct(患者编号),
    .groups = 'drop'
  ) %>%
  arrange(涉及患者数)
# 按涉及患者数排序，找出最相近的指标
cat("按涉及患者数排序的指标（显示前20个）:\n")
head(指标统计[, c("检验子项中文名", "涉及患者数", "记录数")], 20)
cat("\n按涉及患者数排序的指标（显示后20个）:\n")
tail(指标统计[, c("检验子项中文名", "涉及患者数", "记录数")], 20)

##### 计算相近的指标
# 按患者数分组显示相近指标
cat("\n按患者数分组显示相近指标:\n")
指标统计$患者数分组 <- cut(指标统计$涉及患者数, 
                  breaks = seq(0, max(指标统计$涉及患者数) + 50, by = 50),
                  right = FALSE)
# 显示每组中的指标
for(分组 in unique(指标统计$患者数分组)) {
  if(!is.na(分组)) {
    组内指标 <- 指标统计[指标统计$患者数分组 == 分组, ]
    if(nrow(组内指标) > 1) {
      cat(sprintf("\n患者数范围 %s 的指标:\n", 分组))
      print(组内指标[, c("检验子项中文名", "涉及患者数", "记录数")])
    }
  }
}

# 导入基因数据
data_gene <- read_excel("C:/Users/pyz/Desktop/血液肿瘤/血液科数据/基因.xlsx")
# 去除第2，4，5，7，9列
data_gene <- data_gene[, -c(2, 4, 5, 7, 9,11,12,14,15,17,20,21,22,23,25,26,28,29,31)]
# 展示病理诊断列的第1，2行
data_gene$病理诊断[1:2]
# 检查有多少个患者
length(unique(data_gene$住院号)) ## 有9844个患者
# 这部分基因数据的核心变量：病理诊断如下格式：
# [1] "白血病融合基因定量检测：BCR-ABL(p210型)基因表达阳性，BCR-ABL(p210型)拷贝数575524，BCR-ABL/ABL 91.50%，请结合临床考虑。\r\nMPN诊断相关基因突变检测：未检测到JAK2V617F、JAK2exon12、MPLexon10、CALRexon9基因突变，请结合临床考虑。"                                                                                            
# [2] "MPN诊断相关基因突变检测：检测到JAK2V617F突变阳性，请结合临床结果考虑。\r\n白血病融合基因定量检测：BCR-ABL(p210型)基因表达阴性，BCR-ABL(p210型)拷贝数0，BCR-ABL/ABL 0.00%，请结合临床考虑。\r\nJAK2V617F基因突变定量检测：JAK2V617F基因突变阳性，JAK2V617F突变型拷贝数16538，突变型/（突变型+野生型）76.05%，请结合临床考虑。"
# 因此

# 导入流式和染色体数据
data_liushi <- read_excel("C:/Users/pyz/Desktop/血液肿瘤/血液科数据/流式和染色体.xlsx")
# 去除第2，4，5，7，9列
data_liushi <- data_liushi[, -c(2, 4, 5, 7, 9,11,12,14,15,17,20,21,22,23,25,26,28,29,31)]
# 展示病理诊断列的第1，2，3行
data_liushi$病理诊断[1:3]
# 这部分流式和染色体数据的核心变量：病理诊断如下格式：
#[1] "在本次实验检测范围内：\r\n1.CD34+CD117+髓系幼稚细胞比值不高，占有核细胞0.2%。\r\n2.中性粒细胞为各阶段粒细胞，以中幼及以下阶段为主，未见发育模式异常。\r\n3.单核细胞比值不高，均为成熟阶段。\r\n4.淋巴细胞比值不高，CD3+T淋巴细胞占淋巴细胞58.6%，CD4/CD8=1.17,CD19+B淋巴细胞占淋巴细胞30.2%，kappa/lambda=1.35，未检出异常表型。\r\n"
#[2] "46,XY[15]"                                                                                                                                                                                                                                                                                                                           
#[3] "47,XX,+mark,t(9;22)(q34;q11.2)[3]   可见克隆性异常t(9;22)及mark染色体。"   

### 按照相近的患者数和医学知识，分成几个部分来作为协变量模块
# 红系：红细胞，红细胞压积，血红蛋白，平均红细胞体积，平均血红蛋白含量，平均血红蛋白浓度
# 白系：中性粒细胞绝对值，单核细胞绝对值，嗜碱细胞绝对值 ,嗜酸细胞绝对值,淋巴细胞绝对值，白细胞
# 血小板：平均血小板体积,血小板
# 抗体：抗心磷脂抗体IgG,抗心磷脂抗体IgM,免疫球蛋白A(IgA),免疫球蛋白G(IgG),免疫球蛋白M(IgM),免疫球蛋白E(IgE),补体3(C3),补体4(C4)
# 类风湿因子:类风湿因子IgA,类风湿因子IgG,类风湿因子IgM
# 病毒抗体：副流感病毒IgM抗体,呼吸道合胞病毒IgM抗体,嗜肺军团菌IgM抗体,流感病毒A型IgM抗体,流感病毒B型IgM抗体,腺病毒IgM抗体

## 处理一下data_jianyan_combined
# 保留第1，2，3，5，6，8，9，10，12列
data_jianyan_combined_clean <- data_jianyan_combined[, c(1, 2, 3, 5, 6, 8, 9, 10, 12)]
# 重命名第2，4列为就诊编号、记录日期
colnames(data_jianyan_combined_clean)[c(2, 4)] <- c("就诊编号", "记录日期")
# 将记录日期转化为Date
data_jianyan_combined_clean <- data_jianyan_combined_clean %>%
  mutate(记录日期 = as.Date(记录日期))
#  检查每个列的缺失数
colSums(is.na(data_jianyan_combined_clean)) #共有2443835行
# 选择标本名称为NA的行
标本名称为NA的行 <- data_jianyan_combined_clean %>%
  filter(is.na(标本名称))
# 查看结果
cat("标本名称为NA的记录数:", nrow(标本名称为NA的行), "\n")
head(标本名称为NA的行)

# 查看标本名称有多少分类，查看每个分类下有多少患者和记录数
标本分类统计 <- data_jianyan_combined_clean %>%
  group_by(标本名称) %>%
  summarise(
    记录数 = n(),
    患者数 = n_distinct(患者编号),
    .groups = 'drop'
  )
print(标本分类统计)

###### 4.7 红系功能模块 #######
# 选择检验子项中文名为红细胞，红细胞压积，血红蛋白，平均红细胞体积，平均血红蛋白含量，平均血红蛋白浓度的行
红系指标 <- c("红细胞", "红细胞压积", "血红蛋白", "平均红细胞体积", "平均血红蛋白含量", "平均血红蛋白浓度")
data_red <- data_jianyan_combined_clean %>%
  filter(检验子项中文名 %in% 红系指标)
length(unique(data_red$患者编号))

# 找到检验子项结果中不为数值的行
非数值行 <- data_red %>%
  filter(!grepl("^[0-9.]+$", as.character(检验子项结果)) | is.na(检验子项结果))
print(非数值行)
unique(非数值行$检验项目名称)

# 根据标本名称，选择全血的行
data_red_quanxie <- data_red %>%
  filter(标本名称 == "全血")
# 查看检验项目名称
unique(data_red_quanxie$检验项目名称)
# 再测一次有多少非数值的行
非数值行 <- data_red_quanxie %>%
  filter(!grepl("^[0-9.]+$", as.character(检验子项结果)) | is.na(检验子项结果))
print(非数值行)

# 确认检验子项结果为numeric
data_red_quanxie <- data_red_quanxie %>%
  mutate(检验子项结果 = as.numeric(as.character(检验子项结果)))
# 除去检验子项结果为NA的行
data_red_quanxie <- data_red_quanxie %>%
  filter(!is.na(检验子项结果))
# 查看有多少患者
length(unique(data_red_quanxie$患者编号)) 
# 转置数据框，将检验子项中文名的值作为新的列，新列中的值取对应检验子项结果中的值，保留患者编号、检验项目名称、记录日期、标本名称列
# 若在相同患者编号和相同记录日期和相同检验项目名称下有多个值，则取平均值
data_red_combined <- data_red_quanxie %>%
  dplyr::select(患者编号, 检验项目名称, 记录日期, 标本名称, 检验子项中文名, 检验子项结果) %>%
  pivot_wider(
    names_from = 检验子项中文名,
    values_from = 检验子项结果,
    values_fn = ~mean(.x, na.rm = TRUE),  # 对重复值取平均值，忽略NA
    values_fill = list(检验子项结果 = NA_real_)
  )
# 检查缺失数
colSums(is.na(data_red_combined)) 
# 去除第2列和第4列
data_red_combined <- data_red_combined[, -c(2, 4)]
# 找到第3列到第8列有NA的行
na_rows <- data_red_combined[!complete.cases(data_red_combined[, 3:8]), ]
# 去除包含NA的行
data_red_combined <- data_red_combined[complete.cases(data_red_combined[, 3:8]), ]

# 查看记录日期为0001-01-01的行，为什么之前as.Date时会有这种格式的记录日期
invalid_date_rows <- data_red_combined %>%
  filter(记录日期 == as.Date("0001-01-01") | 记录日期 == as.Date("1900-01-01"))
print(invalid_date_rows)
# 查看了原来的数据，原来的记录日期就是0001-01-01 00：00,所以删除记录日期为0001-01-01的行
data_red_combined <- data_red_combined %>%
  filter(记录日期 != as.Date("0001-01-01"))
# 查看还有多少患者
length(unique(data_red_combined$患者编号))   ## 有3464个患者，每个指标的单位在data_red_quanxie中

# 处理红系数据（大概运行20多分钟）
data_red_model <- data_geren2 %>%
  left_join(data_geren1, by = "患者编号") %>%
  mutate(
    # 平均红细胞体积
    平均红细胞体积 = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在红系记录中的记录
      df <- data_red_combined[data_red_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$平均红细胞体积)) return(NA_real_)
      return(as.numeric(df$平均红细胞体积))  
    }),
    
    # 红细胞
    红细胞 = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在红系记录中的记录
      df <- data_red_combined[data_red_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$红细胞)) return(NA_real_)
      return(as.numeric(df$红细胞))  
    }),
    
    # 平均血红蛋白浓度
    平均血红蛋白浓度 = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在红系记录中的记录
      df <- data_red_combined[data_red_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$平均血红蛋白浓度)) return(NA_real_)
      return(as.numeric(df$平均血红蛋白浓度))  
    }),
    
    # 红细胞压积
    红细胞压积 = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在红系记录中的记录
      df <- data_red_combined[data_red_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$红细胞压积)) return(NA_real_)
      return(as.numeric(df$红细胞压积))  
    }),
    
    # 血红蛋白
    血红蛋白 = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在红系记录中的记录
      df <- data_red_combined[data_red_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$血红蛋白)) return(NA_real_)
      return(as.numeric(df$血红蛋白))  
    }),
    
    # 平均血红蛋白含量
    平均血红蛋白含量 = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在红系记录中的记录
      df <- data_red_combined[data_red_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$平均血红蛋白含量)) return(NA_real_)
      return(as.numeric(df$平均血红蛋白含量))  
    }),
    
    # 红系记录对应的记录日期
    red_record_date = map_chr(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在红系记录中的记录
      df <- data_red_combined[data_red_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_character_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_character_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$记录日期)) return(NA_character_)
      return(as.character(df$记录日期))  # 转换为字符型保存
    }) %>% as.Date()  # 最后转换回Date类型
  )

# 删除诊断名称列，去重，然后去除红细胞为NA的行
data_red_model_clean <- data_red_model %>%
  # 删除诊断名称列（假设列名为"诊断名称"）
  dplyr::select(-"诊断名称") %>%
  # 对相同患者编号中各列值完全相同的行进行去重
  distinct() %>%
  # 去除红细胞为NA的行
  filter(!is.na(红细胞))
# 检测红系数据的患者数
length(unique(data_red_model_clean$患者编号)) ### 匹配上的有3150个患者
length(unique(data_red_combined$患者编号)) ### 有3464个患者


###### 4.8 白系免疫模块 #######
# 选择检验子项中文名为中性粒细胞绝对值，单核细胞绝对值，嗜碱细胞绝对值 ,嗜酸细胞绝对值,淋巴细胞绝对值，白细胞的行
粒细胞指标 <- c("中性粒细胞绝对值", "单核细胞绝对值", "嗜碱细胞绝对值", "嗜酸细胞绝对值", "淋巴细胞绝对值", "白细胞")
data_white <- data_jianyan_combined_clean %>%
  filter(检验子项中文名 %in% 粒细胞指标)
# 根据标本名称，选择全血的行
data_white_blood <- data_white %>%
  filter(标本名称 == "全血")
# 查看患者数
length(unique(data_white_blood$患者编号))   # 有3471个患者

# 确认检验子项结果为numeric
data_white_blood <- data_white_blood %>%
  mutate(检验子项结果 = as.numeric(as.character(检验子项结果)))
# 除去检验子项结果为NA的行
data_white_blood <- data_white_blood %>%
  filter(!is.na(检验子项结果))
# 查看有多少患者
length(unique(data_white_blood$患者编号))

# 转置数据框，将检验子项中文名的值作为新的列，新列中的值取对应检验子项结果中的值，保留患者编号、检验项目名称、记录日期、标本名称列
# 若在相同患者编号和相同记录日期和相同检验项目名称下有多个值，则取平均值
data_white_blood_combined <- data_white_blood %>%
  dplyr::select(患者编号, 检验项目名称, 记录日期, 标本名称, 检验子项中文名, 检验子项结果) %>%
  pivot_wider(
    names_from = 检验子项中文名,
    values_from = 检验子项结果,
    values_fn = ~mean(.x, na.rm = TRUE),  # 对重复值取平均值，忽略NA
    values_fill = NA_real_  # 填充NA值
  )

# 检查缺失数
colSums(is.na(data_white_blood_combined)) 
# 去除第2列和第4列
data_white_blood_combined <- data_white_blood_combined %>%
  dplyr::select(-2, -4)
# 去除包含NA的行
data_white_blood_combined <- data_white_blood_combined %>%
  filter(complete.cases(data_white_blood_combined))
# 删除记录日期为0001-01-01的行
data_white_blood_combined <- data_white_blood_combined %>%
  filter(记录日期 != as.Date("0001-01-01"))
# 查看还有多少患者
length(unique(data_white_blood_combined$患者编号))   ##  有3464个患者

# 处理白系数据
data_white_model <- data_geren2 %>%
  left_join(data_geren1, by = "患者编号") %>%
  mutate(
    # 中性粒细胞绝对值
    中性粒细胞绝对值 = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在白系记录中的记录
      df <- data_white_blood_combined[data_white_blood_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$中性粒细胞绝对值)) return(NA_real_)
      return(as.numeric(df$中性粒细胞绝对值))  
    }),
    
    # 单核细胞绝对值
    单核细胞绝对值 = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在白系记录中的记录
      df <- data_white_blood_combined[data_white_blood_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$单核细胞绝对值)) return(NA_real_)
      return(as.numeric(df$单核细胞绝对值))  
    }),
    
    # 嗜碱细胞绝对值
    嗜碱细胞绝对值 = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在白系记录中的记录
      df <- data_white_blood_combined[data_white_blood_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$嗜碱细胞绝对值)) return(NA_real_)
      return(as.numeric(df$嗜碱细胞绝对值))  
    }),
    
    # 嗜酸细胞绝对值
    嗜酸细胞绝对值 = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在白系记录中的记录
      df <- data_white_blood_combined[data_white_blood_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$嗜酸细胞绝对值)) return(NA_real_)
      return(as.numeric(df$嗜酸细胞绝对值))  
    }),
    
    # 淋巴细胞绝对值
    淋巴细胞绝对值 = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在白系记录中的记录
      df <- data_white_blood_combined[data_white_blood_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$淋巴细胞绝对值)) return(NA_real_)
      return(as.numeric(df$淋巴细胞绝对值))  
    }),
    
    # 白细胞
    白细胞 = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在白系记录中的记录
      df <- data_white_blood_combined[data_white_blood_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$白细胞)) return(NA_real_)
      return(as.numeric(df$白细胞))  
    }),
    
    # 白系记录对应的记录日期
    white_record_date = map_chr(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在白系记录中的记录
      df <- data_white_blood_combined[data_white_blood_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_character_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_character_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$记录日期)) return(NA_character_)
      return(as.character(df$记录日期))  # 转换为字符型保存
    }) %>% as.Date()  # 最后转换回Date类型
  )
# 删除诊断名称列，去重，然后去除白细胞为NA的行
data_white_model_clean <- data_white_model %>%
  # 删除诊断名称列（假设列名为"诊断名称"）
  dplyr::select(-"诊断名称") %>%
  # 对相同患者编号中各列值完全相同的行进行去重
  distinct() %>%
  # 去除白细胞为NA的行
  filter(!is.na(白细胞))
# 检测白系数据的患者数
length(unique(data_white_model_clean$患者编号)) ### 匹配上的有3149个患者
length(unique(data_white_blood_combined$患者编号)) ### 有3464个患者


###### 4.9 凝血模块 #######
# 选择检验子项中文名为平均血小板体积,血小板的行
血小板指标 <- c("平均血小板体积", "血小板")
data_platelet <- data_jianyan_combined_clean %>%
  filter(检验子项中文名 %in% 血小板指标)
# 查看标本名称有哪些
unique(data_platelet$标本名称)
length(unique(data_platelet$患者编号))
# 根据标本名称，选择全血的行
data_platelet_blood <- data_platelet %>%
  filter(标本名称 == "全血")

# 分别查看患者数
length(unique(data_platelet_blood$患者编号))  # 有3471个患者

# 确认检验子项结果为numeric
data_platelet_blood <- data_platelet_blood %>%
  mutate(检验子项结果 = as.numeric(as.character(检验子项结果)))
# 除去检验子项结果为NA的行
data_platelet_blood <- data_platelet_blood %>%
  filter(!is.na(检验子项结果))
# 查看有多少患者
length(unique(data_platelet_blood$患者编号))

# 转置数据框，将检验子项中文名的值作为新的列，新列中的值取对应检验子项结果中的值，保留患者编号、检验项目名称、记录日期、标本名称列
# 若在相同患者编号和相同记录日期和相同检验项目名称下有多个值，则取平均值
data_platelet_blood_combined <- data_platelet_blood %>%
  dplyr::select(患者编号, 检验项目名称, 记录日期, 标本名称, 检验子项中文名, 检验子项结果) %>%
  pivot_wider(
    names_from = 检验子项中文名,
    values_from = 检验子项结果,
    values_fn = ~mean(.x, na.rm = TRUE),  # 对重复值取平均值，忽略NA
    values_fill = NA_real_  # 填充NA值
  )
# 检查缺失数
colSums(is.na(data_platelet_blood_combined)) 

# 去除第2列和第4列
data_platelet_blood_combined <- data_platelet_blood_combined %>%
  dplyr::select(-2, -4)
# 找到有NA的行
na_rows <- data_platelet_blood_combined[!complete.cases(data_platelet_blood_combined), ]
# 去除包含NA的行
data_platelet_blood_combined <- data_platelet_blood_combined %>%
  filter(complete.cases(data_platelet_blood_combined))
# 删除记录日期为0001-01-01的行
data_platelet_blood_combined <- data_platelet_blood_combined %>%
  filter(记录日期 != as.Date("0001-01-01"))
# 查看还有多少患者
length(unique(data_platelet_blood_combined$患者编号))    # 有3457个患者

# 处理血小板数据
data_platelet_model <- data_geren2 %>%
  left_join(data_geren1, by = "患者编号") %>%
  mutate(
    # 平均血小板体积
    平均血小板体积 = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在血小板记录中的记录
      df <- data_platelet_blood_combined[data_platelet_blood_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$平均血小板体积)) return(NA_real_)
      return(as.numeric(df$平均血小板体积))  
    }),
    
    # 血小板
    血小板 = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在血小板记录中的记录
      df <- data_platelet_blood_combined[data_platelet_blood_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$血小板)) return(NA_real_)
      return(as.numeric(df$血小板))  
    }),
    
    # 血小板记录对应的记录日期
    platelet_record_date = map_chr(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在血小板记录中的记录
      df <- data_platelet_blood_combined[data_platelet_blood_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_character_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_character_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$记录日期)) return(NA_character_)
      return(as.character(df$记录日期))  # 转换为字符型保存
    }) %>% as.Date()  # 最后转换回Date类型
  )
# 删除诊断名称列，去重，然后去除血小板为NA的行
data_platelet_model_clean <- data_platelet_model %>%
  # 删除诊断名称列（假设列名为"诊断名称"）
  dplyr::select(-"诊断名称") %>%
  # 对相同患者编号中各列值完全相同的行进行去重
  distinct() %>%
  # 去除血小板为NA的行
  filter(!is.na(血小板))
# 检测血小板数据的患者数
length(unique(data_platelet_model_clean$患者编号)) ### 匹配上的有907个患者
length(unique(data_platelet_blood_combined$患者编号)) ### 有1206个患者


###### 4.10 免疫抗体模块 #######
# 选择检验子项中文名为抗心磷脂抗体IgG,抗心磷脂抗体IgM,免疫球蛋白A(IgA),免疫球蛋白G(IgG),免疫球蛋白M(IgM),免疫球蛋白E(IgE),补体3(C3),补体4(C4)的行
免疫指标 <- c("抗心磷脂抗体IgG", "抗心磷脂抗体IgM", "免疫球蛋白A(IgA)", "免疫球蛋白G(IgG)", "免疫球蛋白M(IgM)", "免疫球蛋白E(IgE)", "补体3(C3)", "补体4(C4)")
data_mianyi <- data_jianyan_combined_clean %>%
  filter(检验子项中文名 %in% 免疫指标)

# 查看每个标本名称分别有多少患者数和记录数
标本统计 <- data_mianyi %>%
  group_by(标本名称) %>%
  summarise(
    记录数 = n(),
    患者数 = n_distinct(患者编号),
    .groups = 'drop'
  )
print(标本统计)

# 根据标本名称，选择血清的行
data_mianyi_1 <- data_mianyi %>%
  filter(标本名称 == "血清")
# 分别查看患者数
length(unique(data_mianyi_1$患者编号))

# 检查 检验子项结果是否存在除数值外的字符
非数值行 <- data_mianyi_1 %>%
  filter(!grepl("^[0-9.]+$", as.character(检验子项结果)) | is.na(检验子项结果))
print(非数值行)
# 检验子项结果中有<号和>号,去除符号，保留数值
data_mianyi_1 <- data_mianyi_1 %>%
  mutate(检验子项结果 = gsub("[<>=]", "", as.character(检验子项结果)))
# 确认检验子项结果为numeric
data_mianyi_1 <- data_mianyi_1 %>%
  mutate(检验子项结果 = as.numeric(as.character(检验子项结果)))

# 除去检验子项结果为NA的行
data_mianyi_1 <- data_mianyi_1 %>%
  filter(!is.na(检验子项结果))

# 查看有多少患者
length(unique(data_mianyi_1$患者编号))  #  2371个患者

# 转置数据框，将检验子项中文名的值作为新的列，新列中的值取对应检验子项结果中的值，保留患者编号、检验项目名称、记录日期、标本名称列
# 若在相同患者编号和相同记录日期和相同检验项目名称下有多个值，则取平均值
data_mianyi_combined <- data_mianyi_1 %>%
  dplyr::select(患者编号, 检验项目名称, 记录日期, 标本名称, 检验子项中文名, 检验子项结果) %>%
  pivot_wider(
    names_from = 检验子项中文名,
    values_from = 检验子项结果,
    values_fn = ~mean(.x, na.rm = TRUE),  # 对重复值取平均值，忽略NA
    values_fill = NA_real_  # 填充NA值
  )

# 检查缺失数
colSums(is.na(data_mianyi_combined)) 

# 去除第2列和第4列,抗心磷脂抗体IgG和抗心磷脂抗体IgM缺失多，去除
data_mianyi_combined <- data_mianyi_combined %>%
  dplyr::select(-2, -4, -5, -6)
# 去除第3，4，5，6，7，8列中同行的值都为NA的行
data_mianyi_combined <- data_mianyi_combined %>%
  filter(!is.na(.[[3]]) | !is.na(.[[4]]) | !is.na(.[[5]]) | !is.na(.[[6]]) | !is.na(.[[7]]) | !is.na(.[[8]]))
# 检查缺失数
colSums(is.na(data_mianyi_combined))    ### 1549行缺失

## 进行一下多重插补
# 保存原始数据用于后续比较和分析
original_data2 <- data_mianyi_combined[, c("补体3(C3)", "免疫球蛋白G(IgG)", "免疫球蛋白E(IgE)", "补体4(C4)", "免疫球蛋白M(IgM)","免疫球蛋白A(IgA)")]
# 准备用于插补的数据
mice_data2 <- data_mianyi_combined[, c("补体3(C3)", "免疫球蛋白G(IgG)", "免疫球蛋白E(IgE)", "补体4(C4)", "免疫球蛋白M(IgM)","免疫球蛋白A(IgA)")]
# 进行多重插补
set.seed(123)  # 设置随机种子以保证结果可重复
mice_model2 <- mice(mice_data2, 
                   m = 5,           # 生成5个插补数据集
                   method = "pmm",  # 预测均值匹配
                   printFlag = FALSE,
                   seed = 123)
# 获取插补后的完整数据
data_imputed2 <- complete(mice_model2)
# 将插补后的数据合并回原数据框
data_mianyi_combined[, c("补体3(C3)", "免疫球蛋白G(IgG)", "免疫球蛋白E(IgE)", "补体4(C4)", "免疫球蛋白M(IgM)","免疫球蛋白A(IgA)")] <- data_imputed2
# 检查插补结果
cat("插补后各变量的统计摘要：\n")
print(summary(data_mianyi_combined[, c("补体3(C3)", "免疫球蛋白G(IgG)", "免疫球蛋白E(IgE)", "补体4(C4)", "免疫球蛋白M(IgM)","免疫球蛋白A(IgA)")]))
cat("\n插补后缺失值检查：\n")
print(colSums(is.na(data_mianyi_combined)))

# 删除记录日期为0001-01-01的行
data_mianyi_combined <- data_mianyi_combined %>%
  filter(记录日期 != as.Date("0001-01-01"))
# 查看还有多少患者
length(unique(data_mianyi_combined$患者编号))  # 1876个患者

# 重命名data_mianyi_combined列名
colnames(data_mianyi_combined)[c(3,4,5,6,7,8)] <- c("补体3", "IgG","IgE", "补体4","IgM", "IgA")

# 处理免疫数据
data_mianyi_model <- data_geren2 %>%
  left_join(data_geren1, by = "患者编号") %>%
  mutate(
    # 补体3
    补体3 = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在免疫记录中的记录
      df <- data_mianyi_combined[data_mianyi_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$补体3)) return(NA_real_)
      return(as.numeric(df$补体3))  
    }),
    
    # IgG
    IgG = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在免疫记录中的记录
      df <- data_mianyi_combined[data_mianyi_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$IgG)) return(NA_real_)
      return(as.numeric(df$IgG))  
    }),
    
    # IgE
    IgE = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在免疫记录中的记录
      df <- data_mianyi_combined[data_mianyi_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$IgE)) return(NA_real_)
      return(as.numeric(df$IgE))  
    }),
    
    # 补体4
    补体4 = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在免疫记录中的记录
      df <- data_mianyi_combined[data_mianyi_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$补体4)) return(NA_real_)
      return(as.numeric(df$补体4))  
    }),
    
    # IgM
    IgM = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在免疫记录中的记录
      df <- data_mianyi_combined[data_mianyi_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$IgM)) return(NA_real_)
      return(as.numeric(df$IgM))  
    }),
    
    # IgA
    IgA = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在免疫记录中的记录
      df <- data_mianyi_combined[data_mianyi_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$IgA)) return(NA_real_)
      return(as.numeric(df$IgA))  
    }),
    
    # 免疫记录对应的记录日期
    mianyi_record_date = map_chr(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在免疫记录中的记录
      df <- data_mianyi_combined[data_mianyi_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_character_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_character_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$记录日期)) return(NA_character_)
      return(as.character(df$记录日期))  # 转换为字符型保存
    }) %>% as.Date()  # 最后转换回Date类型
  )

# 删除诊断名称列，去重，然后去除补体3为NA的行
data_mianyi_model_clean <- data_mianyi_model %>%
  # 删除诊断名称列（假设列名为"诊断名称"）
  dplyr::select(-"诊断名称") %>%
  # 对相同患者编号中各列值完全相同的行进行去重
  distinct() %>%
  # 去除补体3为NA的行
  filter(!is.na(补体3))
# 检测免疫数据的患者数
length(unique(data_mianyi_model_clean$患者编号)) 


###### 4.11 类风湿免疫模块 #######
# 选择检验子项中文名为类风湿因子IgA,类风湿因子IgG,类风湿因子IgM的行
类风湿指标 <- c("类风湿因子IgA", "类风湿因子IgG", "类风湿因子IgM")
data_fengshi <- data_jianyan_combined_clean %>%
  filter(检验子项中文名 %in% 类风湿指标)
# 查看每个标本名称分别有多少患者数和记录数
标本统计 <- data_fengshi %>%
  group_by(标本名称) %>%
  summarise(
    记录数 = n(),
    患者数 = n_distinct(患者编号),
    .groups = 'drop'
  )
print(标本统计)

# 根据标本名称，选择血清的行
data_fengshi_1 <- data_fengshi %>%
  filter(标本名称 == "血清")
# 分别查看患者数
length(unique(data_fengshi_1$患者编号))
# 检查 检验子项结果是否存在除数值外的字符，没有发现非数值行
非数值行 <- data_fengshi_1 %>%
  filter(!grepl("^[0-9.]+$", as.character(检验子项结果)) | is.na(检验子项结果))
print(非数值行)

# 确认检验子项结果为numeric
data_fengshi_1 <- data_fengshi_1 %>%
  mutate(检验子项结果 = as.numeric(as.character(检验子项结果)))
# 除去检验子项结果为NA的行
data_fengshi_1 <- data_fengshi_1 %>%
  filter(!is.na(检验子项结果))
# 查看有多少患者
length(unique(data_fengshi_1$患者编号))  # 1708个患者

# 转置数据框，将检验子项中文名的值作为新的列，新列中的值取对应检验子项结果中的值，保留患者编号、检验项目名称、记录日期、标本名称列
# 若在相同患者编号和相同记录日期和相同检验项目名称下有多个值，则取平均值
data_fengshi_combined <- data_fengshi_1 %>%
  dplyr::select(患者编号, 检验项目名称, 记录日期, 标本名称, 检验子项中文名, 检验子项结果) %>%
  pivot_wider(
    names_from = 检验子项中文名,
    values_from = 检验子项结果,
    values_fn = ~mean(.x, na.rm = TRUE),  # 对重复值取平均值，忽略NA
    values_fill = NA_real_  # 填充NA值
  )

# 检查缺失数，没有NA
colSums(is.na(data_fengshi_combined)) 
# 去除第2列和第4列
data_fengshi_combined <- data_fengshi_combined %>%
  dplyr::select(-2, -4)
# 删除记录日期为0001-01-01的行
data_fengshi_combined <- data_fengshi_combined %>%
  filter(记录日期 != as.Date("0001-01-01"))
# 查看还有多少患者
length(unique(data_fengshi_combined$患者编号))   # 1666个患者

# 处理风湿数据
data_fengshi_model <- data_geren2 %>%
  left_join(data_geren1, by = "患者编号") %>%
  mutate(
    # 类风湿因子IgA
    类风湿因子IgA = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在风湿记录中的记录
      df <- data_fengshi_combined[data_fengshi_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$类风湿因子IgA)) return(NA_real_)
      return(as.numeric(df$类风湿因子IgA))  
    }),
    
    # 类风湿因子IgG
    类风湿因子IgG = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在风湿记录中的记录
      df <- data_fengshi_combined[data_fengshi_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$类风湿因子IgG)) return(NA_real_)
      return(as.numeric(df$类风湿因子IgG))  
    }),
    
    # 类风湿因子IgM
    类风湿因子IgM = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在风湿记录中的记录
      df <- data_fengshi_combined[data_fengshi_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$类风湿因子IgM)) return(NA_real_)
      return(as.numeric(df$类风湿因子IgM))  
    }),
    
    # 风湿记录对应的记录日期
    fengshi_record_date = map_chr(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在风湿记录中的记录
      df <- data_fengshi_combined[data_fengshi_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_character_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_character_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$记录日期)) return(NA_character_)
      return(as.character(df$记录日期))  # 转换为字符型保存
    }) %>% as.Date()  # 最后转换回Date类型
  )

# 删除诊断名称列，去重，然后去除类风湿因子IgA为NA的行
data_fengshi_model_clean <- data_fengshi_model %>%
  # 删除诊断名称列（假设列名为"诊断名称"）
  dplyr::select(-"诊断名称") %>%
  # 对相同患者编号中各列值完全相同的行进行去重
  distinct() %>%
  # 去除类风湿因子IgA为NA的行
  filter(!is.na(类风湿因子IgA))
# 检测风湿数据的患者数
length(unique(data_fengshi_model_clean$患者编号))  ## 1123个患者

###### 4.12 病毒抗体 #######
# 选择检验子项中文名为副流感病毒IgM抗体,呼吸道合胞病毒IgM抗体,嗜肺军团菌IgM抗体,流感病毒A型IgM抗体,流感病毒B型IgM抗体,腺病毒IgM抗体的行
病毒指标 <- c("副流感病毒IgM抗体", "呼吸道合胞病毒IgM抗体", "嗜肺军团菌IgM抗体", "流感病毒A型IgM抗体", "流感病毒B型IgM抗体", "腺病毒IgM抗体")
data_virus <- data_jianyan_combined_clean %>%
  filter(检验子项中文名 %in% 病毒指标)
# 查看每个标本名称分别有多少患者数和记录数
标本统计 <- data_virus %>%
  group_by(标本名称) %>%
  summarise(
    记录数 = n(),
    患者数 = n_distinct(患者编号),
    .groups = 'drop'
  )
print(标本统计)

# 根据标本名称，选择血清的行
data_virus_1 <- data_virus %>%
  filter(标本名称 == "血清")
# 分别查看患者数
length(unique(data_virus_1$患者编号)) # 1449个患者

# 检查 检验子项结果是否存在除数值外的字符
非数值行 <- data_virus_1 %>%
  filter(!grepl("^[0-9.]+$", as.character(检验子项结果)) | is.na(检验子项结果))
print(非数值行)

# 将检验子项结果中positive转换为1，negative转换为0
data_virus_1 <- data_virus_1 %>%
  mutate(检验子项结果 = case_when(
    tolower(as.character(检验子项结果)) == "positive" ~ 1,
    tolower(as.character(检验子项结果)) == "negative" ~ 0,
    TRUE ~ as.numeric(as.character(检验子项结果))
  ))

# 检查缺失数
colSums(is.na(data_virus_1)) 
# 除去检验子项结果为NA的行
data_virus_1 <- data_virus_1 %>%
  filter(!is.na(检验子项结果))
# 查看有多少患者
length(unique(data_virus_1$患者编号))    # 1449个患者

# 转置数据框，将检验子项中文名的值作为新的列，新列中的值取对应检验子项结果中的值，保留患者编号、检验项目名称、记录日期、标本名称列
# 若在相同患者编号和相同记录日期和相同检验项目名称下有多个值，对重复值取第一个值，因为这个本质上是分类变量
data_virus_combined <- data_virus_1 %>%
  dplyr::select(患者编号, 检验项目名称, 记录日期, 标本名称, 检验子项中文名, 检验子项结果) %>%
  pivot_wider(
    names_from = 检验子项中文名,
    values_from = 检验子项结果,
    values_fn = first,  # 对重复值取第一个值
    values_fill = NA_real_  # 填充NA值
  )
# 检查缺失数
colSums(is.na(data_virus_combined))

# 去除第2列和第4列,没有缺失
data_virus_combined <- data_virus_combined %>%
  dplyr::select(-2, -4)
# 删除记录日期为0001-01-01的行
data_virus_combined <- data_virus_combined %>%
  filter(记录日期 != as.Date("0001-01-01"))
# 查看还有多少患者
length(unique(data_virus_combined$患者编号))  ## 1412个患者

# 处理病毒数据
data_virus_model <- data_geren2 %>%
  left_join(data_geren1, by = "患者编号") %>%
  mutate(
    # 副流感病毒IgM抗体
    副流感病毒IgM抗体 = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在病毒记录中的记录
      df <- data_virus_combined[data_virus_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$`副流感病毒IgM抗体`)) return(NA_real_)
      return(as.numeric(df$`副流感病毒IgM抗体`))  
    }),
    
    # 呼吸道合胞病毒IgM抗体
    呼吸道合胞病毒IgM抗体 = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在病毒记录中的记录
      df <- data_virus_combined[data_virus_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$`呼吸道合胞病毒IgM抗体`)) return(NA_real_)
      return(as.numeric(df$`呼吸道合胞病毒IgM抗体`))  
    }),
    
    # 嗜肺军团菌IgM抗体
    嗜肺军团菌IgM抗体 = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在病毒记录中的记录
      df <- data_virus_combined[data_virus_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$`嗜肺军团菌IgM抗体`)) return(NA_real_)
      return(as.numeric(df$`嗜肺军团菌IgM抗体`))  
    }),
    
    # 流感病毒A型IgM抗体
    流感病毒A型IgM抗体 = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在病毒记录中的记录
      df <- data_virus_combined[data_virus_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$`流感病毒A型IgM抗体`)) return(NA_real_)
      return(as.numeric(df$`流感病毒A型IgM抗体`))  
    }),
    
    # 流感病毒B型IgM抗体
    流感病毒B型IgM抗体 = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在病毒记录中的记录
      df <- data_virus_combined[data_virus_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$`流感病毒B型IgM抗体`)) return(NA_real_)
      return(as.numeric(df$`流感病毒B型IgM抗体`))  
    }),
    
    # 腺病毒IgM抗体
    腺病毒IgM抗体 = map_dbl(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在病毒记录中的记录
      df <- data_virus_combined[data_virus_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_real_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_real_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$`腺病毒IgM抗体`)) return(NA_real_)
      return(as.numeric(df$`腺病毒IgM抗体`))  
    }),
    
    # 病毒记录对应的记录日期
    virus_record_date = map_chr(1:n(), ~{
      # 获取当前行的患者编号和入院日期
      patient_id <- .data$患者编号[.x]
      admission_date <- .data$入院日期[.x]
      
      # 找出该患者在病毒记录中的记录
      df <- data_virus_combined[data_virus_combined$患者编号 == patient_id, ]
      if (nrow(df) == 0) return(NA_character_)
      
      # 找到入院日期之前/最近的一次记录
      df <- df %>% filter(记录日期 <= admission_date)
      if (nrow(df) == 0) return(NA_character_)
      
      # 最近一次记录
      df <- df[which.max(df$记录日期), ]
      if (nrow(df) == 0 || is.na(df$记录日期)) return(NA_character_)
      return(as.character(df$记录日期))  # 转换为字符型保存
    }) %>% as.Date()  # 最后转换回Date类型
  )

# 删除不需要的列，去重，然后去除副流感病毒IgM抗体为NA的行
data_virus_model_clean <- data_virus_model %>%
  dplyr::select(-"诊断名称") %>%
  # 对相同患者编号中各列值完全相同的行进行去重
  distinct() %>%
  # 去除副流感病毒IgM抗体为NA的行
  filter(!is.na(副流感病毒IgM抗体))

# 检测病毒数据的患者数
length(unique(data_virus_model_clean$患者编号))  # 1083个患者

############################################################## 保存这一阶段的结果
# 保存数据
save.image("第二阶段.RData")
# 加载数据
load("第二阶段.RData")
##############################################################

###### 4.13 流式和染色体指标 #######


###### 4.14 基因检测指标 #######


###### 五 对每个协变量模块构建受限三次方样条函数和分布多项式的模型 ########
library(survival)
library(rms)
library(mfp)
library(mstate)
library(xgboost)

###### 5.1 cox模型 ########

##### 5.1.1 日常生活能力 ####### 
length(unique(data_richang_model_clean$患者编号)) ##有1831个患者
# 查看诊断归转情况的分类，以及每个分类的患者数和记录数
table(data_richang_model_clean$诊断归转情况, useNA = "ifany")
# 将诊断归转情况中的NA都变为 其他
data_richang_model_clean <- data_richang_model_clean %>%
  mutate(
    诊断归转情况 = ifelse(is.na(诊断归转情况), "其他", 诊断归转情况))

# 添加一列time，每个患者的第一个入院日期的行记为0，下一次入院日期的行的time则用该入院日期的date减去第一次入院日期，以此类推，得到随访天数
data_richang_model_clean <- data_richang_model_clean %>%
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
disease_priority <- data_richang_model_clean %>%
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
data_richang_model_clean_1 <- data_richang_model_clean %>%
  dplyr::select(-疾病分类) %>%
  left_join(disease_priority %>% dplyr::select(患者编号, 疾病分类), by = "患者编号")
# 查看疾病分类的情况
table(data_richang_model_clean_1$疾病分类, useNA = "ifany")
# 将疾病类型中未分类都改为其他疾病
data_richang_model_clean_1 <- data_richang_model_clean_1 %>%
  mutate(
    疾病分类 = ifelse(疾病分类 == "未分类", "其他疾病", 疾病分类)
  )

# 删除诊断类型列
data_richang_model_clean_1 <- data_richang_model_clean_1 %>%
  dplyr::select(-"诊断类型")

# 对相同行进行去重
data_richang_model_clean_1 <- data_richang_model_clean_1 %>%
  distinct()
# 对诊断归转情况进行去重，同患者编号、同入院日期下如果同时存在“其他”和非“其他”项，保留非“其他”项，若都为其他，则保留为其他
data_richang_model_clean_1 <- data_richang_model_clean_1 %>%
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
data_richang_model_clean_1 <- data_richang_model_clean_1 %>%
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

# 查看诊断归转的情况
table(data_richang_model_clean_1$诊断归转情况, useNA = "ifany")
# 好转8397，其他94，死亡88，未愈166，治愈1812
# 查看疾病分类的情况
table(data_richang_model_clean_1$疾病分类, useNA = "ifany")
# 将诊断归转情况中的其他都转为未愈
data_richang_model_clean_1 <- data_richang_model_clean_1 %>%
  mutate(
    诊断归转情况 = ifelse(诊断归转情况 == "其他", "未愈", 诊断归转情况)
  )
################################################################################ 截止到这，转为高层贝叶斯模型，多状态模型不合适

#### 多状态模型状态单向不可逆，不适合于该建模，xgboost建模得不到效应，也不利用时间
# 可逆马尔可夫多状态模型
# 状态映射
# 映射状态名称为数值编号（可根据实际需要调整顺序）
state_map <- c("好转" = 1, "未愈" = 2, "治愈" = 3, "死亡" = 4)

# 创建新数据集（只保留合法状态）
data_msm <- data_richang_model_clean_1 %>%
  filter(诊断归转情况 %in% names(state_map)) %>%
  mutate(
    state = state_map[诊断归转情况],
    time = as.numeric(time),  # 这里替换成你真正的随访时间列名，比如 实际住院天数
    性别 = as.factor(性别),
    疾病分类 = as.factor(疾病分类)
  ) %>%
  arrange(患者编号, time)

# 删除只有一次观察的患者（无法形成转移）
data_msm <- data_msm %>%
  group_by(患者编号) %>%
  filter(n() > 1) %>%
  ungroup()

# 构建转移矩阵
Q_init <- matrix(
  c(0, 0.01, 0.01, 0.01,
    0.01, 0, 0.01, 0.01,
    0.01, 0.01, 0, 0.01,
    0, 0, 0, 0),
  nrow = 4, byrow = TRUE
)
colnames(Q_init) <- rownames(Q_init) <- c("好转", "未愈", "治愈", "死亡")


# 检查缺失值
summary(data_msm[, c("richang_score", "年龄", "性别", "疾病分类")])
# 确认格式
data_msm <- data_msm %>%
  mutate(
    年龄 = as.numeric(年龄),  # 转换为数值型
    性别 = as.factor(性别),
    疾病分类 = as.factor(疾病分类),
    richang_score = as.numeric(richang_score)  # 保证为数值型
  )
# 去除性别为未知的行
data_msm <- data_msm %>%
  filter(!is.na(性别) & 性别 != "未知")
# 将天数转换为月份
data_msm$month <- data_msm$time / 30
# 确认state情况
table(data_msm$state, useNA = "ifany")  # 应该只出现 1, 2, 3, 4


msm_model <- msm(
  state ~ month,
  subject = 患者编号,
  data = data_msm,
  qmatrix = Q_init,
  covariates = ~ richang_score ,
  center = FALSE,
  gen.inits = TRUE
)

# 构建受限三次样条的多状态模型


# 构建分数多项式的多状态模型
########################################### 马尔可夫多状态模型暂时不适用，许多患者诊断的疾病很多，但是诊断归转都写得其他或者好转，占比太大，这会导致结果失真
# 现在有两个思路，一个是根据患者诊断名称来调控诊断归转状态，我们去优化结局变量，让它更贴近真实情况，这需要NLP等或构建词表或大语言模型进行词向量匹配，比如一个患者的疾病种类越多，
# 疾病得分越严重

# 第二个是转变研究思路，以每个协变量模块中我们最感兴趣的变量作为结局变量，比如日常功能模块就使用richang_score作为结局变量，一般人口学协变量包括年龄、性别、疾病分类
# 职业、婚姻状况，但是问题在于，我那么多原定规划为协变量的模块，每一个都有一个单独的结局变量，最后还能实现贝叶斯整合吗，用MCMC呢？

## 实际上这个数据中的诊断转归情况并不可信，一个人随着随访时间延长，他的日常生活能力量表评分不断下降，最低达到45，但是对应的诊断归转状态居然是治愈、好转，这不瞎扯吗
# 我发现可能是我之前处理诊断名称时有误，不能删掉这一列，因为诊断归转情况对应的是该诊断名称的转归情况，不代表患者变化，但是诊断名称的条目太多了

### 查看data_richang_model,data_yingyang_model,data_fengxian_model
unique(data_richang_model_clean_1$婚姻状况)

##### 5.1.1 日常生活能力 ####### 
length(unique(data_richang_model_clean$患者编号)) ##有1831个患者

##### 5.1.2 成人营养 ####### 
length(unique(data_yinyang_model_clean$患者编号)) ##有767个患者


##### 5.1.3 营养风险 #######
length(unique(data_fengxian_model_clean$患者编号)) ##有1209个患者


##### 5.1.4 生命体征 ####### 
length(unique(data_record_model_clean$患者编号)) ##有2314个患者


##### 5.1.5 输血数据 #######
length(unique(data_blood_model_clean$患者编号)) ### 匹配上的有907个患者

##### 5.1.6 红系数据 #######
length(unique(data_red_model_clean$患者编号)) ### 匹配上的有3150个患者

##### 5.1.7 白系数据 #######
length(unique(data_white_model_clean$患者编号)) ### 匹配上的有3149个患者

##### 5.1.8 凝血数据 #######
length(unique(data_platelet_model_clean$患者编号)) ### 匹配上的有907个患者

##### 5.1.9 免疫数据 #######
length(unique(data_mianyi_model_clean$患者编号)) # 1279个患者

##### 5.1.10 风湿数据 #######
length(unique(data_fengshi_model_clean$患者编号))  ## 1123个患者

##### 5.1.11 病毒数据 #######
length(unique(data_virus_model_clean$患者编号))  # 1083个患者



###### 5.2 高层贝叶斯模型 ########
# 将职业为NA的转为其他
data_richang_model_clean_1 <- data_richang_model_clean_1 %>%
  mutate(
    职业 = ifelse(is.na(职业), "其他", 职业))
# 创建同一的integer代码本
unique(data_geren2$性别)
unique(data_geren2$疾病分类)
unique(data_geren2$婚姻状况)
unique(data_geren2$职业)
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



# 以日常功能模块为例
data_richang_model_clean_2 <- data_richang_model_clean_1 %>%
  transmute(
    y = richang_score,
    patient = 患者编号,
    time = time,
    age = 年龄,
    sex = ifelse(性别 == "男性", 1, 0),
    disease = as.integer(as.factor(疾病分类)),
    Marital = as.integer(as.factor(婚姻状况)),
    Profession = as.integer(as.factor(职业)),
    module = "richang"
  )
# 导出疾病分类的编码
disease_codes <- data_richang_model_clean_1 %>%
  dplyr::select(疾病分类) %>%
  distinct() %>%
  mutate(disease_code = as.integer(as.factor(疾病分类))) %>%
  arrange(disease_code)
print("疾病分类编码:")
print(disease_codes)
# 导出婚姻状况的编码
marital_codes <- data_richang_model_clean_1 %>%
  dplyr::select(婚姻状况) %>%
  distinct() %>%
  mutate(Marital_code = as.integer(as.factor(婚姻状况))) %>%
  arrange(Marital_code)
print("婚姻状况编码:")
print(marital_codes)

# 导出职业的编码
profession_codes <- data_richang_model_clean_1 %>%
  dplyr::select(职业) %>%
  distinct() %>%
  mutate(Profession_code = as.integer(as.factor(职业))) %>%
  arrange(Profession_code)

print("职业编码:")
print(profession_codes)

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

# 使用示例：
# data_richang_model_clean_final <- process_medical_data(data_richang_model_clean)
# data_yinyang_model_clean_final <- process_medical_data(data_yinyang_model_clean)

# 使用标准编码处理各模块数据
# 营养数据
data_yinyang_model_clean_1 <- process_medical_data(data_yinyang_model_clean)
data_yinyang_model_clean_1 <- apply_standard_mapping(data_yinyang_model_clean_1)

data_yinyang_model_clean_2 <- data_yinyang_model_clean_1 %>%
  transmute(
    y = yinyang_score,
    patient = 患者编号,
    time = as.numeric(time),
    age = 年龄,
    BMI = as.numeric(BMI),
    sex = ifelse(性别 == "男性", 1, 0),
    disease = disease_code,
    Marital = marital_code,
    Profession = profession_code,
    module = "yinyang"
  )

# 营养风险数据
data_fengxian_model_clean_1 <- process_medical_data(data_fengxian_model_clean)
data_fengxian_model_clean_1 <- apply_standard_mapping(data_fengxian_model_clean_1)

data_fengxian_model_clean_2 <- data_fengxian_model_clean_1 %>%
  transmute(
    y = fengxian_score,
    patient = 患者编号,
    time = as.numeric(time),
    age = 年龄,
    sex = ifelse(性别 == "男性", 1, 0),
    disease = disease_code,
    Marital = marital_code,
    Profession = profession_code,
    module = "fengxian"
  )

# 生命体征数据
data_record_model_clean_1 <- process_medical_data(data_record_model_clean)
data_record_model_clean_1 <- apply_standard_mapping(data_record_model_clean_1)

data_shousuoya_model_clean_2 <- data_record_model_clean_1 %>%
  transmute(
    y = 收缩压,
    patient = 患者编号,
    time = as.numeric(time),
    age = 年龄,
    sex = ifelse(性别 == "男性", 1, 0),
    disease = disease_code,
    Marital = marital_code,
    Profession = profession_code,
    module = "systolic_pressure"
  )
# 血压数据（舒张压）
data_shuzhangya_model_clean_2 <- data_record_model_clean_1 %>%
  transmute(
    y = 舒张压,
    patient = 患者编号,
    time = as.numeric(time),
    age = 年龄,
    sex = ifelse(性别 == "男性", 1, 0),
    disease = disease_code,
    Marital = marital_code,
    Profession = profession_code,
    module = "diastolic_pressure"
  )

# 红细胞数据
data_red_model_clean_1 <- process_medical_data(data_red_model_clean)
data_red_model_clean_1 <- apply_standard_mapping(data_red_model_clean_1)

data_erythrocyte_model_clean_2 <- data_red_model_clean_1 %>%
  transmute(
    y = 红细胞,
    patient = 患者编号,
    time = as.numeric(time),
    age = 年龄,
    sex = ifelse(性别 == "男性", 1, 0),
    disease = disease_code,
    Marital = marital_code,
    Profession = profession_code,
    module = "erythrocyte"
  )
#  平均血红蛋白浓度
data_xuehongdanbai_model_clean_2 <- data_red_model_clean_1 %>%
  transmute(
    y = 平均血红蛋白浓度,
    patient = 患者编号,
    time = as.numeric(time),
    age = 年龄,
    sex = ifelse(性别 == "男性", 1, 0),
    disease = disease_code,
    Marital = marital_code,
    Profession = profession_code,
    module = "mean_corpuscular_hemoglobin_concentration"
  )

# 白细胞
data_white_model_clean_1 <- process_medical_data(data_white_model_clean)
data_white_model_clean_1 <- apply_standard_mapping(data_white_model_clean_1)

data_white_model_clean_2 <- data_white_model_clean_1 %>%
  transmute(
    y = 白细胞,
    patient = 患者编号,
    time = as.numeric(time),
    age = 年龄,
    sex = ifelse(性别 == "男性", 1, 0),
    disease = disease_code,
    Marital = marital_code,
    Profession = profession_code,
    module = "leukocyte"
  ) 
# 淋巴细胞
data_linba_model_clean_2 <- data_white_model_clean_1 %>%
  transmute(
    y = 淋巴细胞绝对值,
    patient = 患者编号,
    time = as.numeric(time),
    age = 年龄,
    sex = ifelse(性别 == "男性", 1, 0),
    disease = disease_code,
    Marital = marital_code,
    Profession = profession_code,
    module = "lymphocyte"
  ) 
# 中性粒细胞
data_zhongxing_model_clean_2 <- data_white_model_clean_1 %>%
  transmute(
    y = 中性粒细胞绝对值,
    patient = 患者编号,
    time = as.numeric(time),
    age = 年龄,
    sex = ifelse(性别 == "男性", 1, 0),
    disease = disease_code,
    Marital = marital_code,
    Profession = profession_code,
    module = "Neutrophils"
  ) 


# 血小板数据
data_platelet_model_clean_1 <- process_medical_data(data_platelet_model_clean)
data_platelet_model_clean_1 <- apply_standard_mapping(data_platelet_model_clean_1)

data_platelet_model_clean_2 <- data_platelet_model_clean_1 %>%
  transmute(
    y = 血小板,
    patient = 患者编号,
    time = as.numeric(time),
    age = 年龄,
    sex = ifelse(性别 == "男性", 1, 0),
    disease = disease_code,
    Marital = marital_code,
    Profession = profession_code,
    module = "platelet"
  )
# 平均血小板体积
data_pingjunxuexiaoban_model_clean_2 <- data_platelet_model_clean_1 %>%
  transmute(
    y = 平均血小板体积,
    patient = 患者编号,
    time = as.numeric(time),
    age = 年龄,
    sex = ifelse(性别 == "男性", 1, 0),
    disease = disease_code,
    Marital = marital_code,
    Profession = profession_code,
    module = "Mean_platelet_volume"
  )

# IgG数据
data_mianyi_model_clean_1 <- process_medical_data(data_mianyi_model_clean)
data_mianyi_model_clean_1 <- apply_standard_mapping(data_mianyi_model_clean_1)

data_IgG_model_clean_2 <- data_mianyi_model_clean_1 %>%
  transmute(
    y = IgG,
    patient = 患者编号,
    time = as.numeric(time),
    age = 年龄,
    sex = ifelse(性别 == "男性", 1, 0),
    disease = disease_code,
    Marital = marital_code,
    Profession = profession_code,
    module = "IgG"
  )
# IgE数据
data_IgE_model_clean_2 <- data_mianyi_model_clean_1 %>%
  transmute(
    y = IgE,
    patient = 患者编号,
    time = as.numeric(time),
    age = 年龄,
    sex = ifelse(性别 == "男性", 1, 0),
    disease = disease_code,
    Marital = marital_code,
    Profession = profession_code,
    module = "IgE"
  )
# IgM数据
data_IgM_model_clean_2 <- data_mianyi_model_clean_1 %>%
  transmute(
    y = IgM,
    patient = 患者编号,
    time = as.numeric(time),
    age = 年龄,
    sex = ifelse(性别 == "男性", 1, 0),
    disease = disease_code,
    Marital = marital_code,
    Profession = profession_code,
    module = "IgM"
  )
# IgA数据
data_IgA_model_clean_2 <- data_mianyi_model_clean_1 %>%
  transmute(
    y = IgA,
    patient = 患者编号,
    time = as.numeric(time),
    age = 年龄,
    sex = ifelse(性别 == "男性", 1, 0),
    disease = disease_code,
    Marital = marital_code,
    Profession = profession_code,
    module = "IgA"
  )


# 类风湿因子IgM
data_fengshi_model_clean_1 <- process_medical_data(data_fengshi_model_clean)
data_fengshi_model_clean_1 <- apply_standard_mapping(data_fengshi_model_clean_1)

data_fengshi_model_clean_2 <- data_fengshi_model_clean_1 %>%
  transmute(
    y = 类风湿因子IgM,
    patient = 患者编号,
    time = as.numeric(time),
    age = 年龄,
    sex = ifelse(性别 == "男性", 1, 0),
    disease = disease_code,
    Marital = marital_code,
    Profession = profession_code,
    module = "fengshi"
  )

# 病毒数据
#  当时代码有问题，重新处理一下
# 删除不需要的列，去重，然后去除副流感病毒IgM抗体为NA的行
data_virus_model_clean <- data_virus_model %>%
  dplyr::select(-"诊断名称") %>%
  # 对相同患者编号中各列值完全相同的行进行去重
  distinct() %>%
  # 去除副流感病毒IgM抗体为NA的行
  filter(!is.na(副流感病毒IgM抗体))

# 检测病毒数据的患者数
length(unique(data_virus_model_clean$患者编号))  # 1038个患者
data_virus_model_clean_1 <- process_medical_data(data_virus_model_clean)
data_virus_model_clean_1 <- apply_standard_mapping(data_virus_model_clean_1)

data_virus_model_clean_2 <- data_virus_model_clean_1 %>%
  transmute(
    y = 副流感病毒IgM抗体,
    patient = 患者编号,
    time = as.numeric(time),
    age = 年龄,
    sex = ifelse(性别 == "男性", 1, 0),
    disease = disease_code,
    Marital = marital_code,
    Profession = profession_code,
    module = "virus"
  )


### 对营养和风险进行合并，都是NRS-2002
## 先把两张营养表拼起来（保持你已有的字段名）
nutri_raw <- bind_rows(
  data_yinyang_model_clean_2,
  data_fengxian_model_clean_2
) %>%
  mutate(
    time    = as.numeric(time),
    patient = as.character(patient)   # 保险起见，先转成字符
  )
## 在 patient × time 维度上合并：
##    - y: 取均值（忽略 NA；两表本质同量表）
##    - 其他变量：取第一条（通常相同；若不同也按“第一条”规则）
nutri_merged <- nutri_raw %>%
  arrange(patient, time) %>%
  group_by(patient, time) %>%
  summarise(
    y         = if (all(is.na(y))) NA_real_ else mean(y, na.rm = TRUE),
    age       = first(age),
    sex       = first(sex),
    disease   = first(disease),
    Marital   = first(Marital),
    Profession= first(Profession),
    .groups   = "drop"
  ) %>%
  mutate(module = "NRS") %>%  # 统一一个模块名（你也可用 "nutri"）
  select(y, patient, time, everything())  # 排序，第一列为y，第二列为patient，第三列为time

# 合并所有模块，列名如下:y,patient,time,age,sex,disease,Martial,Profession,module
all_data <- bind_rows(
  data_richang_model_clean_2,
  nutri_merged,
  data_shousuoya_model_clean_2,
  data_shuzhangya_model_clean_2,
  data_erythrocyte_model_clean_2,
  data_xuehongdanbai_model_clean_2,
  data_white_model_clean_2,
  data_linba_model_clean_2,
  data_zhongxing_model_clean_2,
  data_platelet_model_clean_2,
  data_pingjunxuexiaoban_model_clean_2,
  data_IgG_model_clean_2,
  data_IgE_model_clean_2,
  data_IgM_model_clean_2,
  data_IgA_model_clean_2,
  data_fengshi_model_clean_2,
  data_virus_model_clean_2
)

# 整理模块编号
# 强制转换模块为因子
all_data <- all_data %>%
  mutate(
    module = as.factor(module),
    sex = as.factor(sex),
    age = as.numeric(age),
    disease = as.factor(disease),
    Marital = as.factor(Marital),
    Profession = as.factor(Profession)
  )

############### 5.2.1 马尔科夫链蒙特卡罗模拟 #############
library(splines)
library(brms)
library(cmdstanr)
library(bayesplot)
### 安装并编译cmdstan
###### remotes::install_github("stan-dev/cmdstanr", dependencies = TRUE)
# 从GitHub上手动下载安装包并解压
# 设置路径
set_cmdstan_path("F:/R/cmdstan/cmdstan-2.36.0") 
###### 编译
###### rebuild_cmdstan()
# 检查
cmdstanr::cmdstan_path()
cmdstanr::cmdstan_version()

# 分模块标准化y值
all_data <- all_data %>%
  group_by(module) %>%
  mutate(y_z = scale(y)) %>%
  ungroup()
all_data <- all_data %>%
  mutate(
    age_std = scale(age),
    log_time = log1p(time),
    log_time_std = scale(log_time)
  )
# 构建线性的公式
formula_linear <- bf(
  y_z ~ log_time_std + age_std + sex + disease + Marital + Profession +
    (1 +log_time_std | module)
)
# 查看合适的先验分布
default_prior(
object = formula_linear,
data = all_data,
family = gaussian()
)
# 推荐的先验方案
# 设置先验
priors <- c(
  set_prior("student_t(3, 0, 2.5)", class = "b"),                     # 所有固定效应
  set_prior("student_t(3, 0, 2.5)", class = "Intercept"),             # 截距
  set_prior("student_t(3, 0, 2.5)", class = "sd"),                    # 所有随机效应SD
  set_prior("student_t(3, 0, 2.5)", class = "sigma"),                 # 残差标准差
  set_prior("lkj(2)", class = "cor")                                  # 更强一点的相关结构先验（更保守）
)

# 运行模型
fit_linear_prior <- brm(
  formula = formula_linear,
  data = all_data,
  family = gaussian(),
  prior = priors,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = 4,
  control = list(adapt_delta = 0.95),
  backend = "cmdstanr"
)
print(fit_linear_prior) 

# 提取固定效应 point estimate + 95% CI
fixef(fit_linear_prior, summary = TRUE, probs = c(0.025, 0.975))
# 提取所有随机效应（group-level effects）
ranef(fit_linear_prior, summary = TRUE, probs = c(0.025, 0.975))

# 提取所有 posterior draws of coefficients
coef_full <- coef(fit_linear_prior)
# 查看有哪些模块名、变量名
dimnames(coef_full$module)
# 选中营养模块
coef_full$module["yinyang", , "log_time_std"]
# 风险模块
coef_full$module["fengxian", , "log_time_std"]

###### 进行模型评估
summ <- summary(fit_linear_prior)
summ$fixed                    # 固定效应估计 + Rhat + Bulk/Tail ESS
summ$random
# 先验/后验预测检验（PPC）与残差
pp_check(fit_linear_prior)                        # 总体分布
pp_check(fit_linear_prior, type = "dens_overlay")

# 分组PPC（看营养模块是否拟合得好）
pp_check(fit_linear_prior, type = "dens_overlay_grouped", group = "module")
# 残差
res <- residuals(fit_linear_prior, summary = TRUE)
plot(predict(fit_linear_prior)[,1], res[,"Estimate"])   # 拟合值 vs 残差（应无结构）
# 解释度与比较
bayes_R2(fit_linear_prior)           # Bayes R^2（整体与分组）
loo1 <- loo(fit_linear_prior)        # LOOIC + Pareto-k 诊断（k<0.7 更稳）
print(loo1)
# 随机效应结构是否合理
VarCorr(fit_linear_prior)            # 看 module 层随机截距/斜率方差是否>0
ranef(fit_linear_prior)              # 各模块的偏差是否有意义




library(bayesplot)
# 提取 posterior draws of group-specific coefficients
post <- as_draws_df(fit_linear_prior)

# 绘制 profession2 在不同 module 下的后验估计分布
mcmc_intervals(
  post,
  pars = grep("b_.*log_time_std.*module.*", colnames(post), value = TRUE),
  prob = 0.95
)

############################################################ 非线性模型

# 基于 log_time 创建 spline basis
all_data <- all_data %>%
  mutate(log_time = log1p(time)) %>%
  bind_cols(
    as.data.frame(ns(.$log_time, df = 3)) %>%
      rename_with(~ paste0("spline", seq_along(.)))
  )

# 构建 Profession × Time 的非线性交互模型公式（注意，已构建的模型中使用的是y而不是y_z，
# 且已构建的是样条时间与profession的交互模型）
formula_spline <- bf(
  y_z ~ spline1 + spline2 + spline3 +
    age_std + sex + disease + Marital + Profession +
    (1 + spline1 + spline2 + spline3 | module)
)

# 查看合适的先验分布（建议运行以核对默认）
default_prior(
  object = formula_spline,
  data = all_data,
  family = gaussian()
)

# 设置与线性模型一致的先验
priors_spline <- c(
  set_prior("student_t(3, 0, 2.5)", class = "b"),
  set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
  set_prior("student_t(3, 0, 2.5)", class = "sigma"),
  set_prior("student_t(3, 0, 2.5)", class = "sd"),
  set_prior("lkj(2)", class = "cor")
)

# 拟合模型
fit_spline<- brm(
  formula = formula_spline,
  data = all_data,
  family = gaussian(),
  prior = priors_spline,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = 4,
  control = list(adapt_delta = 0.95),
  backend = "cmdstanr"
)
print(fit_spline)

#########  5.2.2 比较模型性能 ########
loo_lin <- loo(fit_linear_prior)
loo_spl <- loo(fit_spline)
loo_compare(loo_lin, loo_spl)

# 2.3 模块层面的 RMSE（用 posterior_predict 的均值作为预测）
get_rmse_by_module <- function(fit) {
  yhat <- colMeans(posterior_predict(fit))         # 每行的预测均值（含噪声预测）
  tibble(
    .row   = seq_along(yhat),
    yhat   = yhat,
    y_obs  = fit$data[[all.vars(formula(fit)$formula)[1]]],  # 左侧响应变量名
    module = fit$data$module
  ) %>%
    group_by(module) %>%
    summarise(RMSE = sqrt(mean((y_obs - yhat)^2, na.rm=TRUE)),
              n = n(), .groups="drop")
}

rmse_lin <- get_rmse_by_module(fit_linear_prior) %>% mutate(model="linear")
rmse_spl <- get_rmse_by_module(fit_spline)       %>% mutate(model="spline")

rmse_compare <- bind_rows(rmse_lin, rmse_spl) %>% arrange(module, model)
print(rmse_compare, n=100)

# 3.1 提取“模块特异”的系数（均值/SE/CrI）
tidy_by_module <- function(coef_list) {
  obj <- coef_list$module
  wanted <- c("Estimate", "Est.Error", "Q2.5", "Q97.5")
  
  # helper：把一个 level 的矩阵/数据框整到“行=term，列=统计量”
  tidy_one <- function(m, level_name) {
    m <- as.matrix(m)
    
    # 情形1：统计量在列（最常见） -> OK
    if (all(wanted %in% colnames(m))) {
      Kept <- intersect(wanted, colnames(m))
      df <- as.data.frame(m[, Kept, drop = FALSE])
      df$term <- rownames(m)
    }
    # 情形2：统计量在行（你现在的情况） -> 转置
    else if (all(wanted %in% rownames(m))) {
      mT <- t(m)
      Kept <- intersect(wanted, rownames(m))
      mT <- mT[, Kept, drop = FALSE]   # 现在列是统计量
      df <- as.data.frame(mT)
      df$term <- rownames(mT)
    }
    else {
      stop("既不是‘统计量在列’也不是‘统计量在行’，请先 str() 看结构。")
    }
    
    df$module <- level_name
    df[, c("module", "term", intersect(wanted, names(df)))]
  }
  
  # A) list：每个元素一个 level
  if (is.list(obj)) {
    out <- lapply(names(obj), function(L) tidy_one(obj[[L]], L))
    return(dplyr::bind_rows(out))
  }
  
  # B) 3维 array：[level, term, stat] -> 按 level 拆开后 tidy
  if (is.array(obj) && length(dim(obj)) == 3) {
    dn <- dimnames(obj); levs <- dn[[1]]; terms <- dn[[2]]; stats <- dn[[3]]
    out <- lapply(levs, function(L) {
      a <- obj[L, , , drop = FALSE]                # 1×term×stat
      mat <- matrix(a, nrow = length(terms), ncol = length(stats),
                    dimnames = list(terms, stats))
      tidy_one(mat, L)
    })
    return(dplyr::bind_rows(out))
  }
  
  # C) 2维矩阵：只有一个 level
  if (is.matrix(obj) || is.data.frame(obj)) {
    return(tidy_one(obj, "(single_level)"))
  }
  
  stop("$module 既不是 list、也不是 array/矩阵。用 str(coef_list$module, 2) 检查。")
}
coef_lin <- coef(fit_linear_prior)
coef_spl <- coef(fit_spline)

tb_lin <- tidy_by_module(coef_lin) |> dplyr::mutate(model = "linear")
tb_spl <- tidy_by_module(coef_spl) |> dplyr::mutate(model = "spline")
tb_all <- dplyr::bind_rows(tb_lin, tb_spl)


# 3.3 合并后可筛选你关心的协变量（例如 age_std、sex、disease...）
# 示例：看 age_std 在不同模块、不同模型下的效应
eff_age <- tb_all %>% filter(term == "spline1") %>%
  arrange(module, model)

# 3.4 森林图（浅蓝/浅绿配色）
plot_term_forest <- function(term_name) {
  df <- tb_all %>% filter(term == term_name)
  ggplot(df, aes(x = Estimate, y = reorder(module, Estimate),
                 xmin = Q2.5, xmax = Q97.5, color = model)) +
    geom_pointrange(position = position_dodge(width = 0.6)) +
    geom_vline(xintercept = 0, linetype = 3, color = "#457B9D") +
    scale_color_manual(values = c(linear="#1D3557", spline="#2A9D8F")) +
    labs(x = paste0(term_name, " 的效应估计（95% CrI）"),
         y = "模块", color = "模型",
         title = paste0("各模块上 ", term_name, " 的贝叶斯效应对比")) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(color = "#1D3557", face = "bold", hjust = 0.5),
      axis.title = element_text(color = "#457B9D")
    )
}

# 例：画 age_std、sex1（按你的编码可能是 "sex1"）等
p_age <- plot_term_forest("spline1")
print(p_age)
# p_sex <- plot_term_forest("sex1"); print(p_sex)


########  5.2.3 提取营养模块的轨迹 #########
# 只保留营养两个子模块
nutri_rows <- all_data %>% filter(module %in% c("yinyang","fengxian"))
# 确认y的分布
summary(nutri_rows$y)
# 确认患者数目
length(unique(nutri_rows$patient))
length(unique(data_yinyang_model_clean_2$patient))
length(unique(data_fengxian_model_clean_2$patient))
# 确认模块数
unique(all_data$module)
# 确认time的分布
summary(all_data$time)

## ------- 1) 准备：一个函数，给“单个 module”做数据准备 --------
prepare_module_for_gbtm <- function(all_data, module_name, fit_linear_prior,
                                    ndraws = 200, jitter_floor = 0.05,
                                    min_timepoints = 2) {
  # 取该模块全部行（含协变量，供 posterior_predict 使用）
  dat_mod <- all_data %>%
    filter(module == module_name) %>%
    select(patient, time, log_time_std, module, y, y_z,
           age_std, sex, disease, Marital, Profession)
  
  # 后验预测抽样 → 行级预测SD（在 y_z 尺度上）
  set.seed(2025)
  pp_draws <- posterior_predict(fit_linear_prior, newdata = dat_mod, ndraws = ndraws)
  pp_sd <- apply(pp_draws, 2, sd)
  dat_mod <- dat_mod %>% mutate(pp_sd = pp_sd)
  
  # 对该模块内“个体几乎不变”的患者加很小的抖动（仅作用在 y_z）
  pat_stat <- dat_mod %>%
    group_by(patient) %>%
    summarise(sd_i   = sd(y_z, na.rm = TRUE),
              se_med = median(pp_sd, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(sd_jit = ifelse(is.finite(se_med), pmax(0.5 * se_med, jitter_floor), jitter_floor),
           flag_flat = (is.na(sd_i) | sd_i < 1e-4))
  
  dat_mod_jit <- dat_mod %>%
    left_join(pat_stat, by = "patient") %>%
    mutate(y_z_jit = ifelse(flag_flat, y_z + rnorm(n(), 0, sd_jit), y_z)) %>%
    select(patient, time, log_time_std, y, y_z = y_z_jit) %>%  # 之后就用 y_z 这个列名
    mutate(log_time_std = as.numeric(log_time_std)) %>%
    group_by(patient) %>%
    filter(dplyr::n_distinct(log_time_std) >= min_timepoints) %>%
    ungroup()
  
  dat_mod_jit
}
## ------- 2) 你的 GBTM 搜索函数（保持不变，按需要用你当前版本即可） -------
fit_gbtm_one_module <- function(dat_module,
                                subj_col   = "patient",
                                time_col   = "log_time_std",
                                resp_col   = "y_z",          # 单模块时直接用 y_z
                                K_vec      = 2:4,
                                deg_vec    = c(1,2),
                                min_meas   = 2,
                                min_ids    = 20,
                                rep_init   = 50,
                                maxiter    = 200,
                                allow_nwg  = FALSE) {
  
  d <- dat_module %>%
    dplyr::select(all_of(c(subj_col, time_col, resp_col))) %>%
    rename(id = !!sym(subj_col), time = !!sym(time_col), y = !!sym(resp_col)) %>%
    arrange(id, time)
  
  # —— 强制为“干净”数据框
  d <- d %>%
    mutate(
      id   = as.integer(as.factor(id)),  # 一次性转成整数ID
      time = as.numeric(time),
      y    = as.numeric(y)
    ) %>%
    filter(is.finite(y), is.finite(time)) %>%
    group_by(id) %>% filter(dplyr::n_distinct(time) >= min_meas) %>% ungroup() %>%
    as.data.frame()
  names(d) <- make.names(names(d), unique = TRUE)
  
  # —— 时间缩放到 [0,1]（若全相同则直接报错，避免除0）
  tr <- range(d$time, na.rm = TRUE)
  if (diff(tr) <= .Machine$double.eps) stop("该模块的 time 无变化，无法建模。")
  d$time <- (d$time - tr[1]) / (tr[2] - tr[1])
  
  if (dplyr::n_distinct(d$id) < min_ids) stop("有效个体数过少，请检查数据或降低 min_ids。")
  
  # —— 单组初值（注意：random = ~0）
  base1 <- hlme(
    fixed   = y ~ time,
    random  = ~ 1,        # 关键：不放类内随机效应
    subject = "id",
    data    = d,
    verbose = FALSE
  )
  
  # —— 给 class 成员模型一点分离信息（可选，稳收敛）
  by_id <- d %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      base_y = dplyr::first(y[order(time)]),
      .groups = "drop"
    )
  d <- d %>% dplyr::left_join(by_id, by = "id")
  
  fits <- list(); summ <- list()
  
  for (K in K_vec) for (deg in deg_vec) {
    form_fix <- if (deg == 1) y ~ time else as.formula(paste("y ~ poly(time,", deg, ", raw=TRUE)"))
    mix_fix  <- if (deg == 1) ~ time   else as.formula(paste("~ poly(time,", deg, ", raw=TRUE)"))
    
    set.seed(2025)
    # 暖启动：只分截距 + 随机初值
    minit_int <- try(
      hlme(
        fixed   = y ~ time,
        random  = ~ 1,
        subject = "id",
        ng      = K,
        data    = d,
        mixture = ~ 1,
        nwg     = FALSE,
        B       = random(K),
        idiag   = TRUE,
        maxiter = maxiter,
        verbose = FALSE
      ), silent = TRUE
    )
    
    if (!inherits(minit_int, "try-error")) {
      fitK <- try(
        gridsearch(
          rep     = rep_init,
          maxiter = maxiter,
          minit   = hlme(
            fixed   = form_fix,
            random  = ~ 1,
            subject = "id",
            ng      = K,
            data    = d,
            mixture = mix_fix,
            nwg     = allow_nwg,
            B       = minit_int$best,
            idiag   = TRUE,
            verbose = FALSE
          )
        ), silent = TRUE
      )
    } else {
      fitK <- try(
        hlme(
          fixed   = form_fix,
          random  = ~ 1,
          subject = "id",
          ng      = K,
          data    = d,
          mixture = mix_fix,
          nwg     = allow_nwg,
          B       = random(K),
          idiag   = TRUE,
          maxiter = maxiter,
          verbose = FALSE
        ), silent = TRUE
      )
    }
    
    if (!inherits(fitK, "try-error")) {
      pp    <- postprob(fitK)
      avepp <- colMeans(pp[, grepl("^prob.class", names(pp)), drop = FALSE])
      prop  <- as.numeric(table(pp$class)) / nrow(pp)
      
      fits[[paste0("K",K,"_deg",deg)]] <- fitK
      summ[[paste0("K",K,"_deg",deg)]] <- data.frame(
        model     = paste0("K",K,"_deg",deg),
        K         = K,
        deg       = deg,
        bic_val   = fitK$BIC,
        AvePP_min = min(avepp),
        min_prop  = min(prop)
      )
    }
  }
  
  if (length(summ) == 0) stop("该模块在当前设定下未收敛：先 K=2、deg=1、allow_nwg=FALSE，再逐步放宽。")
  summary_df <- bind_rows(summ) %>% arrange(.data$bic_val)
  list(fits = fits, summary = summary_df)
}

# ------- 3) 依次对 yinyang / fengxian / richang 跑 --------
# （richang 必须确实存在于 all_data$module，否则先检查）
mods <- c("yinyang", "fengxian", "richang","systolic_pressure","erythrocyte","lymphocyte","Neutrophils","leukocyte","platelet","IgG")

res_list  <- list()
summ_list <- list()
err_list  <- list()

for (m in mods) {
  if (!m %in% unique(all_data$module)) {
    message("模块缺失：", m, " —— 跳过")
    next
  }
  
  message("准备并拟合模块：", m)
  
  # 1) 数据准备也做容错（极端情况 prepare 失败也不中断循环）
  dat_m <- tryCatch(
    prepare_module_for_gbtm(all_data, m, fit_linear_prior,
                            ndraws = 200, jitter_floor = 0.05,
                            min_timepoints = 2),
    error = function(e) {
      warning(sprintf("[prepare %s] 失败：%s", m, e$message))
      err_list[[m]] <<- paste0("prepare error: ", e$message)
      return(NULL)
    }
  )
  if (is.null(dat_m)) next
  
  # 2) 个体数检查
  if (dplyr::n_distinct(dat_m$patient) < 20) {
    msg <- sprintf("模块 %s 有效个体数不足，跳过。", m)
    message(msg)
    err_list[[m]] <- msg
    next
  }
  
  # 3) 拟合也做 tryCatch，不收敛就记录并继续
  res_m <- tryCatch(
    fit_gbtm_one_module(
      dat_module = dat_m,
      subj_col   = "patient",
      time_col   = "log_time_std",
      resp_col   = "y_z",
      K_vec      = 2:4,
      deg_vec    = 1,
      min_meas   = 2,
      rep_init   = 60,
      maxiter    = 300,
      allow_nwg  = FALSE
    ),
    error = function(e) {
      warning(sprintf("[fit %s] 未收敛/报错：%s", m, e$message))
      err_list[[m]] <<- paste0("fit error: ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(res_m)) next
  
  res_list[[m]]  <- res_m
  summ_list[[m]] <- res_m$summary %>% dplyr::mutate(module = m, .before = 1)
}

# 汇总可用结果（忽略 NULL）
if (length(summ_list)) {
  summary_all <- dplyr::bind_rows(summ_list) %>% dplyr::arrange(module, bic_val)
  print(summary_all, n = 50)
} else {
  message("没有成功收敛的模块。查看 err_list 了解原因。")
}

# Optional：看看哪些模块报过错
err_list


# 汇总各模块的候选模型表现
bind_rows(summ_list) %>% arrange(module, bic_val)


library(ggplot2)
for (m in mods) {
  if (!m %in% names(err_list)) next
  dat_m <- prepare_module_for_gbtm(all_data, m, fit_linear_prior)
  p <- ggplot(dat_m, aes(x = log_time_std, y = y_z, group = patient)) +
    geom_line(alpha = 0.2) +
    labs(title = paste("Module:", m))
  print(p)
}

library(dplyr)
library(ggplot2)
library(lcmm)
library(stats)

## —— 帮你写两个小工具函数 —— ##

# A. 计算个体“粗斜率”与基线（用于直方图 & k-means）
get_baseline_slope <- function(dat_m, id_col = "patient", t_col = "log_time_std", y_col = "y_z") {
  d <- dat_m %>%
    select(id = all_of(id_col), t = all_of(t_col), y = all_of(y_col)) %>%
    filter(is.finite(t), is.finite(y)) %>%
    arrange(id, t)
  
  # 每人至少 2 个不同时间点
  d <- d %>% group_by(id) %>% filter(dplyr::n_distinct(t) >= 2) %>% ungroup()
  
  # 基线取最早时间点的观测
  base_df <- d %>% group_by(id) %>%
    slice_min(order_by = t, n = 1, with_ties = FALSE) %>%
    summarise(baseline = first(y), .groups = "drop")
  
  # 粗斜率：每人简单 OLS
  slp_df <- d %>% group_by(id) %>%
    summarise(slope = coef(lm(y ~ t))[2], .groups = "drop")
  
  out <- base_df %>% inner_join(slp_df, by = "id")
  out
}

# B. 1组 vs 2组（线性时间）的 hlme 诊断
hlme_1v2_linear <- function(dat_m, id_col="patient", t_col="log_time_std", y_col="y_z",
                            maxiter=300) {
  
  d <- dat_m %>%
    select(ID_raw = all_of(id_col), Time_raw = all_of(t_col), Resp_raw = all_of(y_col)) %>%
    mutate(
      ID      = as.integer(as.factor(ID_raw)),
      TimeVar = as.numeric(Time_raw),
      Resp    = as.numeric(Resp_raw)
    ) %>%
    select(ID, TimeVar, Resp) %>%
    arrange(ID, TimeVar) %>%
    group_by(ID) %>% filter(dplyr::n_distinct(TimeVar) >= 2) %>% ungroup() %>%
    as.data.frame()
  names(d) <- make.names(names(d), unique = TRUE)
  
  rng <- range(d$TimeVar, na.rm = TRUE)
  if (diff(rng) <= .Machine$double.eps) stop("时间无变化")
  d$TimeVar <- (d$TimeVar - rng[1]) / (rng[2] - rng[1])
  
  # 1组
  m1 <- lcmm::hlme(
    fixed    = Resp ~ TimeVar,
    random   = ~ 1,
    subject  = "ID",
    data     = d,
    na.action= na.exclude,
    verbose  = FALSE
  )
  
  # 2组：先只分截距暖启动
  m2_int <- try(
    lcmm::hlme(
      fixed    = Resp ~ TimeVar,
      random   = ~ 1,
      subject  = "ID",
      ng       = 2,
      data     = d,
      mixture  = ~ 1,
      nwg      = FALSE,
      B        = lcmm::random(2),
      idiag    = TRUE,
      na.action= na.exclude,
      maxiter  = maxiter,
      verbose  = FALSE
    ), silent = TRUE
  )
  
  if (inherits(m2_int, "try-error")) return(list(m1 = m1, m2 = NULL, comp = NULL))
  
  # 2组：分时间斜率
  m2 <- try(
    lcmm::hlme(
      fixed    = Resp ~ TimeVar,
      random   = ~ 1,
      subject  = "ID",
      ng       = 2,
      data     = d,
      mixture  = ~ TimeVar,
      nwg      = FALSE,
      B        = m2_int$best,
      idiag    = TRUE,
      na.action= na.exclude,
      maxiter  = maxiter,
      verbose  = FALSE
    ), silent = TRUE
  )
  if (inherits(m2, "try-error")) return(list(m1 = m1, m2 = NULL, comp = NULL))
  
  # 计算 AvePP & 最小组占比
  pp <- postprob(m2)
  avepp <- colMeans(pp[, grepl("^prob.class", names(pp)), drop = FALSE])
  prop  <- as.numeric(table(pp$class)) / nrow(pp)
  
  comp <- data.frame(
    model     = c("ng=1","ng=2"),
    BIC       = c(m1$BIC, m2$BIC),
    AvePP_min = c(NA, min(avepp)),
    min_prop  = c(NA, min(prop))
  )
  list(m1 = m1, m2 = m2, comp = comp)
}

## —— 针对“未收敛的模块”做三项小诊断 —— ##
library(stats)

diag_list <- list()

for (m in mods) {
  if (!m %in% names(err_list)) next   # 只对之前未收敛的模块诊断
  message("小诊断：", m)
  
  dat_m <- prepare_module_for_gbtm(all_data, m, fit_linear_prior,
                                   ndraws = 200, jitter_floor = 0.05,
                                   min_timepoints = 2)
  
  ## ① 个体粗斜率分布
  bs <- get_baseline_slope(dat_m)
  cat("# 病人数量：", nrow(bs), "\n")
  cat("# 斜率摘要：\n"); print(summary(bs$slope))
  p_hist <- ggplot(bs, aes(slope)) +
    geom_histogram(bins = 40, alpha = .7) +
    geom_vline(xintercept = 0, linetype = 2) +
    labs(title = paste("Individual slope histogram -", m))
  print(p_hist)
  
  ## ② 1组 vs 2组（线性时间）对照
  comp <- try(hlme_1v2_linear(dat_m), silent = TRUE)
  if (!inherits(comp, "try-error") && !is.null(comp$comp)) {
    cat("# 1组 vs 2组 BIC/AvePP/最小组占比：\n"); print(comp$comp)
  } else {
    cat("# hlme 1v2 未能完成（可能时间无变化或极端不稳）。\n")
  }
  
  ## ③ 基线×斜率 的 k-means=2（粗分群基准）
  set.seed(123)
  km <- kmeans(scale(bs[, c("baseline","slope")]), centers = 2, nstart = 50)
  tab_km <- table(cluster = km$cluster)
  cat("# k-means(2) 组大小：\n"); print(tab_km)
  p_scatter <- ggplot(bs, aes(baseline, slope, color = factor(km$cluster))) +
    geom_point(alpha = .6) +
    labs(title = paste("Baseline vs slope k-means (K=2) -", m), color = "cluster")
  print(p_scatter)
  
  diag_list[[m]] <- list(slopes = bs, km = km, comp = comp)
}









# —— 0) 只保留营养两个子模块的原始观测（含 y_z 及所有协变量）
nutri_rows <- all_data %>%
  filter(module %in% c("yinyang","fengxian")) %>%
  select(patient, time, log_time_std, module, y, y_z,  
         age_std, sex, disease, Marital, Profession)
# —— 1) 后验预测（包含观测噪声），若还担心新水平可加 allow_new_levels=TRUE
set.seed(2025)
pp_draws <- posterior_predict(
  fit_linear_prior,
  newdata = nutri_rows,
  ndraws = 200
  # , allow_new_levels = TRUE  # 如报“新水平”再放开
)
# pp_draws: ndraws x nrow(newdata). 对每一行计算预测标准差
pp_sd <- apply(pp_draws, 2, sd)

nutri_rows_pp <- nutri_rows %>%
  mutate(pp_sd = pp_sd)   # 行级的后验预测 SD（单位：y_z 的 z 尺度）

# —— 2) 合并两个模块到一条 NRS-2002 序列（同 patient+time 只留一行）
#      用逆方差权重（w = 1/pp_sd^2）做“加权均值”，并得到合并后的 SE
nutri_merged <- nutri_rows_pp %>%
  group_by(patient, time) %>%
  reframe(
    log_time_std = dplyr::first(log_time_std),
    n_row        = n(), # 合并前条数（1或2）
    y            = if (all(is.na(y))) NA_real_ else mean(y, na.rm = TRUE),
    
    # 权重（若某行 pp_sd 无效或为0，用等权 1）
    w            = ifelse(is.finite(pp_sd) & pp_sd > 0, 1/(pp_sd^2), 1),
    y_z_comb     = sum(y_z * w, na.rm = TRUE) / sum(w, na.rm = TRUE),    # 加权均值
    se_comb      = sqrt( 1 / sum(w, na.rm = TRUE) )                      # 合并后的 SE（y_z 尺度）
  ) %>%
  ungroup() %>%
  distinct(patient, time, .keep_all = TRUE) %>%
  arrange(patient, time)

# —— 3) 仅对“个体内几乎无变化”的患者加很小的“nugget”噪声（幅度来自贝叶斯不确定性）
# 判定“无变化”：该患者合并后的 y_z_comb 的 SD 很小（阈值可微调：1e-8 或 1e-6）
set.seed(2025)
# 先做个体级的 sd 和一个合适的抖动尺度（用该患者 se_comb 的中位数，至少给个地板值）
pat_stat <- nutri_merged %>%
  group_by(patient) %>%
  summarise(sd_i   = sd(y_z_comb, na.rm = TRUE),
            se_med = median(se_comb, na.rm = TRUE)) %>%
  mutate(
    # 抖动标准差：取患者 se_med 的 80%，但不低于 0.05（在 z 尺度很小）
    sd_jit = pmax(0.8 * se_med, 0.05)
  )

# 合并回逐行数据，并在“sd_i≈0”的患者上注入噪声；其他人保持原值
nutri_merged_jit <- nutri_merged %>%
  left_join(pat_stat, by = "patient") %>%
  mutate(
    y_z_comb_jit = ifelse(is.na(sd_i) | sd_i < 1e-8,
                          y_z_comb + rnorm(n(), 0, sd_jit),
                          y_z_comb)
  ) %>%
  select(-sd_i, -se_med, -sd_jit)

# 检查抖动后的数据分布
summary(nutri_merged_jit$y_z_comb_jit)
hist(nutri_merged_jit$y_z_comb_jit, breaks = 50)
# 确认时间变量范围
summary(nutri_merged_jit$log_time_std)
# 个体观测次数分布
nutri_merged_jit %>% 
  count(patient) %>% 
  pull(n) %>% 
  summary()
# 确保时间列是纯 numeric 向量（不是 matrix/array 列）
nutri_merged_jit <- nutri_merged_jit %>%
  mutate(log_time_std = as.numeric(log_time_std))

# 过滤：每个 patient 至少有“两个不同的时间点”
nutri_merged_jit <- nutri_merged_jit %>%
  dplyr::group_by(patient) %>%
  dplyr::filter(dplyr::n_distinct(log_time_std) >= 2) %>%
  dplyr::ungroup()
# 确认患者数
length(unique(nutri_merged_jit$patient))  # 1053个患者

#  用合并后（且必要时已抖动）的 y_z_comb_jit 做 GBTM
resp_var <- "y_z_comb_jit"

# —— 4) 封装：在“合并后数据”上跑一次 GBTM 网格搜索（带详细注释）
fit_gbtm_one_module <- function(dat_module,
                                subj_col   = "patient",       # 个体ID列
                                time_col   = "log_time_std",  # 时间列（建议与上游一致）
                                resp_col   = "y_z_comb_jit",      # 响应列（这里用合并后的 y_z）
                                K_vec      = 2:4,             # 先 2~4 组，成功再扩到 5
                                deg_vec    = 1,               # 先线性；稳定后再试 2
                                min_meas   = 2,               # 每人最少观测次数
                                min_ids    = 20,              # 最少个体数（防崩）
                                rep_init   = 30,              # gridsearch 重启次数
                                maxiter    = 100,             # 每次最大迭代
                                allow_nwg  = FALSE            # 先关组别特异残差
) {
  # —— 只保留需要的列并统一命名；按 id-time 排序
  d <- dat_module %>%
    dplyr::select(all_of(c(subj_col, time_col, resp_col))) %>%
    rename(
      id   = !!sym(subj_col),
      time = !!sym(time_col),
      y    = !!sym(resp_col)
    ) %>%
    arrange(id, time)
  
  # —— hlme() 需要 numeric 的 id
  d$id <- as.numeric(as.factor(d$id))
  
  # —— 过滤缺失与非有限值
  d <- d %>% filter(is.finite(y), is.finite(time))
  
  # —— 过滤“不同时间点数”不足的个体（关键！）
  d <- d %>%
    dplyr::group_by(id) %>%
    dplyr::filter(dplyr::n_distinct(time) >= min_meas) %>%
    dplyr::ungroup()
  
  # —— 时间缩放到 0–1，改善数值稳定性
  tr <- range(d$time, na.rm = TRUE)
  d  <- d %>% mutate(time = (time - tr[1]) / (tr[2] - tr[1]))
  
  # —— 足够样本检查
  if (n_distinct(d$id) < min_ids) stop("有效个体数过少，请检查数据或降低 min_ids。")
  
  # —— 单组初值（多组用其 B 作模板）
  base1 <- hlme(
    fixed   = y ~ poly(time, 1, raw = TRUE),  # 线性时间项
    random  = ~ 1,                             # 常用随机截距
    subject = "id",
    data    = d,
    verbose = FALSE
  )
  
  fits <- list(); summ <- list()
  
  # —— 遍历组数与时间阶数
  for (K in K_vec) for (deg in deg_vec) {
    form_fix <- as.formula(sprintf("y ~ poly(time, %d, raw = TRUE)", deg))
    mix_fix  <- as.formula(sprintf("~ poly(time, %d, raw = TRUE)", deg))
    
    set.seed(2025)
    ## —— 先用“只分截距”的 K 组模型 + 随机初值 做暖启动（更易收敛）
    minit_int <- try(
      hlme(
        fixed   = y ~ poly(time, 1, raw = TRUE),  # 固定项保持线性时间
        random  = ~ 1,
        subject = "id",
        ng      = K,
        data    = d,
        mixture = ~ 1,            # 先只让截距在组间不同
        nwg     = FALSE,          # 暖启动阶段更简单
        B       = random(K),      # 关键：随机初值，而不是 base1$best
        idiag   = TRUE,
        maxiter = maxiter,        # 给足迭代
        verbose = FALSE
      ),
      silent = TRUE
    )
    
    ## —— 若暖启动成功，用其 best 作为真正多项式 mixture 的初值再 gridsearch
    if (!inherits(minit_int, "try-error")) {
      fitK <- try(
        gridsearch(
          rep     = rep_init,
          maxiter = maxiter,
          minit   = hlme(
            fixed   = form_fix,
            random  = ~ 1,
            subject = "id",
            ng      = K,
            data    = d,
            mixture = mix_fix,        # 现在让时间多项式在组间不同
            nwg     = allow_nwg,
            B       = minit_int$best, # 用暖启动解当初值
            idiag   = TRUE,
            verbose = FALSE
          )
        ),
        silent = TRUE
      )
    } else {
      ## —— 暖启动也失败时，再退一步：直接用随机初值起真正模型
      fitK <- try(
        hlme(
          fixed   = form_fix,
          random  = ~ 1,
          subject = "id",
          ng      = K,
          data    = d,
          mixture = mix_fix,
          nwg     = allow_nwg,
          B       = random(K),    # 随机初值
          idiag   = TRUE,
          maxiter = maxiter,
          verbose = FALSE
        ),
        silent = TRUE
      )
    }
    
    if (!inherits(fitK, "try-error")) {
      pp    <- postprob(fitK)
      avepp <- colMeans(pp[, grepl("^prob.class", names(pp)), drop = FALSE])
      prop  <- as.numeric(table(pp$class)) / nrow(pp)
      
      fits[[paste0("K",K,"_deg",deg)]] <- fitK
      summ[[paste0("K",K,"_deg",deg)]] <- data.frame(
        model     = paste0("K",K,"_deg",deg),
        K         = K,
        deg       = deg,
        bic_val   = fitK$BIC,      # 用 bic_val 避免与 stats::BIC 混淆
        AvePP_min = min(avepp),
        min_prop  = min(prop)
      )
    }
  }
  
  if (length(summ) == 0) {
    stop("当前设定下未收敛：先用 K=2、deg=1、allow_nwg=FALSE，再逐步放宽。")
  }
  
  summary_df <- bind_rows(summ) %>% arrange(.data$bic_val)
  list(fits = fits, summary = summary_df, data_used = d)
}
# —— 5) 在“合并后的数据”上跑一次 GBTM 搜索
# 用“抖动后的数据”和“抖动后的变量名”来跑；先极简：K=2、线性、更多重启/迭代
res_nrs <- fit_gbtm_one_module(
  dat_module = nutri_merged_jit,   # ← 改这里
  subj_col   = "patient",
  time_col   = "log_time_std",
  resp_col   = "y_z_comb_jit",     # ← 改这里
  K_vec      = 2,                  # 先从最简单的开始
  deg_vec    = 1,
  min_meas   = 2,
  rep_init   = 50,                 # 提高重启次数
  maxiter    = 200,                # 提高迭代上限
  allow_nwg  = FALSE
)

print(res_nrs$summary, n = 20)
best_key   <- res_nrs$summary$model[1]
best_model <- res_nrs$fits[[best_key]]


####### 模型没找到合适的解，先找原因，再倒过去修改代码
# 每个 id 的观测次数与独立时间点数
nutri_merged %>%
  group_by(patient) %>%
  summarise(n = n(),
            n_time = n_distinct(log_time_std),
            sd_y = sd(y_hat_comb, na.rm = TRUE)) %>%
  summarise(min_n = min(n), median_n = median(n),
            min_n_time = min(n_time), median_n_time = median(n_time),
            zero_sd_ids = sum(sd_y %in% c(0, NA)))
# 查是否存在每个 id 的 y 完全不变（GBTM 很难分）
bad_ids <- nutri_merged %>%
  group_by(patient) %>%
  summarise(sd_y = sd(y_hat_comb, na.rm = TRUE)) %>%
  filter(is.na(sd_y) | sd_y == 0) %>% pull(patient)

length(bad_ids)  # 太多就需要处理：剔除或加噪声/不纳入GBTM

# 3) 时间是否有重复（同一 patient 同一 log_time_std 多行）
dup_ct <- nutri_merged %>% count(patient, log_time_std) %>% filter(n > 1) %>% nrow()
dup_ct




# —— 6) 选 BIC 最优模型，并导出分类结果与质量指标
print(res_nrs$summary, n = 20)
best_key   <- res_nrs$summary$model[1]
best_model <- res_nrs$fits[[best_key]]

pp_best <- postprob(best_model)
avepp_best <- colMeans(pp_best[, grepl("^prob.class", names(pp_best)), drop = FALSE])
prop_best  <- as.numeric(table(pp_best$class)) / nrow(pp_best)
data.frame(AvePP = avepp_best, Prop = prop_best)



# 导出：合并后的时间序列 + 轨迹标签（便于下游多状态/因果分析）
# write.csv(nutri_out, "nutri_merged_timeseries_and_traj.csv", row.names = FALSE)


############ 保存结果
save.image("第三阶段.RData")
####### 加载模型
load("第三阶段.RData")

####################################### 5.3 高层贝叶斯模型结果解读  ################################
# 确认模块数
unique(all_data$module)
# 提取固定效应 point estimate + 95% CI
fixef(fit_spline, summary = TRUE, probs = c(0.025, 0.975))
# 提取所有随机效应（group-level effects）
ranef(fit_spline, summary = TRUE, probs = c(0.025, 0.975))

# 提取所有 posterior draws of coefficients
coef_full_spline <- coef(fit_spline)
# 查看有哪些模块名、变量名
dimnames(coef_full_spline$module)
# 选中营养模块
coef_full_spline$module["yinyang",, "spline2"]
# 风险模块
coef_full_spline$module["fengxian", , "spline2"]

library(tidybayes)
library(ggplot2)
# 作图解读整体时间效应曲线
# 创建一个新的时间变量序列
time_grid <- seq(min(all_data$log_time_std), max(all_data$log_time_std), length.out = 100)

# 用相同的方式构造样条基函数
spline_grid <- as.data.frame(ns(time_grid, df = 3))
colnames(spline_grid) <- c("spline1", "spline2", "spline3")

############ 5.3.1关注营养风险 #############
newdata <- cbind( 
  spline_grid,
  age_std = 0.5,
  sex = "1",                  # 注意也可能是字符型因子
  disease = "1",              # 用模型里已有的 levels，比如 "1"
  Marital = "1",              # 同样要保证在原始数据 levels 中
  Profession = "1",           # 或者换成你感兴趣的职业编码，比如 "3"
  module = "NRS"                # 这个随机效应可以忽略
)
# 得到预测均值
preds <- posterior_epred(fit_spline, newdata = newdata)
pred_mean <- apply(preds, 2, mean)
pred_lower <- apply(preds, 2, quantile, 0.025)
pred_upper <- apply(preds, 2, quantile, 0.975)
# 画图
plot_df <- data.frame(
  time = time_grid,
  estimate = pred_mean,
  lower = pred_lower,
  upper = pred_upper
)

###### 对时间进行反解
# 先获取标准化参数
log_time_mean <- attr(all_data$log_time_std, "scaled:center")
log_time_sd <- attr(all_data$log_time_std, "scaled:scale")
# 还原时间
plot_df$log_time <- plot_df$time * log_time_sd + log_time_mean
plot_df$time_raw <- expm1(plot_df$log_time)
# 然后画图时用 time_raw（即原始时间）
p <- ggplot(plot_df, aes(x = time_raw, y = estimate)) +
  # 置信区间带
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "#9ecae1", alpha = 0.2) +
  # 主预测曲线（较浅的蓝色）
  geom_line(color = "#1f77b4", linewidth = 1.2) +
  # 坐标与标题
  labs(
    x = "Time",
    y = "y hat",
    title = "The trend of nutritional risk over time in nonlinear bayesian hierarchical model "
  ) +
  # 简洁主题 + 自定义配色
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )
p
ggsave("营养风险变化.pdf", plot = p, width = 15, height = 10)

############ 5.3.2关注日常功能 #############
newdata <- cbind(
  spline_grid,
  age_std = 0.5,
  sex = "1",                  # 男性
  disease = "1",              # 用模型里已有的 levels，比如 "1",多发性骨髓瘤
  Marital = "1",              # 同样要保证在原始数据 levels 中
  Profession = "1",           # 或者换成你感兴趣的职业编码，比如 "3"
  module = "richang"                
)
# 得到预测均值
preds <- posterior_epred(fit_spline, newdata = newdata)
pred_mean <- apply(preds, 2, mean)
pred_lower <- apply(preds, 2, quantile, 0.025)
pred_upper <- apply(preds, 2, quantile, 0.975)
# 画图
plot_df <- data.frame(
  time = time_grid,
  estimate = pred_mean,
  lower = pred_lower,
  upper = pred_upper
)

# 对时间进行反解
# 还原时间
plot_df$log_time <- plot_df$time * log_time_sd + log_time_mean
plot_df$time_raw <- expm1(plot_df$log_time)
# 然后画图时用 time_raw（即原始时间）
p <- ggplot(plot_df, aes(x = time_raw, y = estimate)) +
  # 置信区间带
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "#fcae91", alpha = 0.2) +
  # 主预测曲线
  geom_line(color = "#ff7f0e", linewidth = 1.2) +
  # 坐标与标题
  labs(
    x = "Time",
    y = "y hat",
    title = "The ADL over time in nonlinear bayesian hierarchical model "
  ) +
  # 简洁主题 + 自定义配色
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )
p
ggsave("日常生活能力变化.pdf", plot = p, width = 15, height = 10)

############ 5.3.3关注收缩压 #############
newdata <- cbind(
  spline_grid,
  age_std = 0.6,
  sex = "0",                  # 男性
  disease = "1",              # 用模型里已有的 levels，比如 "1",多发性骨髓瘤
  Marital = "1",              # 同样要保证在原始数据 levels 中
  Profession = "1",           # 或者换成你感兴趣的职业编码，比如 "3"
  module = "systolic_pressure"                
)
# 得到预测均值
preds <- posterior_epred(fit_spline, newdata = newdata)
pred_mean <- apply(preds, 2, mean)
pred_lower <- apply(preds, 2, quantile, 0.025)
pred_upper <- apply(preds, 2, quantile, 0.975)
# 画图
plot_df <- data.frame(
  time = time_grid,
  estimate = pred_mean,
  lower = pred_lower,
  upper = pred_upper
)

# 对时间进行反解
# 还原时间
plot_df$log_time <- plot_df$time * log_time_sd + log_time_mean
plot_df$time_raw <- expm1(plot_df$log_time)
# 然后画图时用 time_raw（即原始时间）
p <- ggplot(plot_df, aes(x = time_raw, y = estimate)) +
  # 置信区间带
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "#90ee90", alpha = 0.2) +
  # 主预测曲线
  geom_line(color = "#2ca02c", linewidth = 1.2) +
  # 坐标与标题
  labs(
    x = "Time",
    y = "y hat",
    title = "The systolic_pressure over time in nonlinear bayesian hierarchical model "
  ) +
  # 简洁主题 + 自定义配色
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )
p
ggsave("systolic_pressure变化.pdf", plot = p, width = 15, height = 10)


############ 5.3.4关注红细胞 #############
newdata <- cbind(
  spline_grid,
  age_std = 0.5,
  sex = "0",                  # 男性
  disease = "1",              # 用模型里已有的 levels，比如 "1",多发性骨髓瘤
  Marital = "1",              # 同样要保证在原始数据 levels 中
  Profession = "1",           # 或者换成你感兴趣的职业编码，比如 "3"
  module = "erythrocyte"                
)
# 得到预测均值
preds <- posterior_epred(fit_spline, newdata = newdata)
pred_mean <- apply(preds, 2, mean)
pred_lower <- apply(preds, 2, quantile, 0.025)
pred_upper <- apply(preds, 2, quantile, 0.975)
# 画图
plot_df <- data.frame(
  time = time_grid,
  estimate = pred_mean,
  lower = pred_lower,
  upper = pred_upper
)

# 对时间进行反解
# 还原时间
plot_df$log_time <- plot_df$time * log_time_sd + log_time_mean
plot_df$time_raw <- expm1(plot_df$log_time)
# 然后画图时用 time_raw（即原始时间）
p <- ggplot(plot_df, aes(x = time_raw, y = estimate)) +
  # 置信区间带
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "#90ee90", alpha = 0.2) +
  # 主预测曲线
  geom_line(color = "#2ca02c", linewidth = 1.2) +
  # 坐标与标题
  labs(
    x = "Time",
    y = "y hat",
    title = "The erythrocyte over time in nonlinear bayesian hierarchical model "
  ) +
  # 简洁主题 + 自定义配色
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )
p

############ 5.3.5关注营养的年龄变化 #############
newdata <- cbind(
  spline_grid,
  age_std = 0.5,
  sex = "1",                  # 男性
  disease = "1",              # 用模型里已有的 levels，比如 "1",多发性骨髓瘤
  Marital = "1",              # 同样要保证在原始数据 levels 中
  Profession = "1",           # 或者换成你感兴趣的职业编码，比如 "3"
  module = "NRS"                
)
# 得到预测均值
preds <- posterior_epred(age_std, newdata = newdata)
pred_mean <- apply(preds, 2, mean)
pred_lower <- apply(preds, 2, quantile, 0.025)
pred_upper <- apply(preds, 2, quantile, 0.975)
# 画图
plot_df <- data.frame(
  age = age_std,
  estimate = pred_mean,
  lower = pred_lower,
  upper = pred_upper
)

###### 对年龄进行反解
# 先获取标准化参数
log_age_mean <- attr(all_data$log_age_std, "scaled:center")
log_age_sd <- attr(all_data$log_age_std, "scaled:scale")
# 还原年龄
plot_df$log_time <- plot_df$time * log_time_sd + log_time_mean
plot_df$time_raw <- expm1(plot_df$log_time)
# 然后画图时用 time_raw（即原始时间）
p <- ggplot(plot_df, aes(x = time_raw, y = estimate)) +
  # 置信区间带
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "#fcae91", alpha = 0.2) +
  # 主预测曲线
  geom_line(color = "#ff7f0e", linewidth = 1.2) +
  # 坐标与标题
  labs(
    x = "Time",
    y = "y hat",
    title = "The ADL over time in nonlinear bayesian hierarchical model "
  ) +
  # 简洁主题 + 自定义配色
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )
p
ggsave("日常生活能力变化.pdf", plot = p, width = 15, height = 10)


### 把这个图可以画好点


############################ 5.4  构建新的模型 #################################
# profession只作为亚组判断,职业对生理功能的效应不显著 
# 以可改变的因素作为研究重心（如一些护理干预和治疗措施）
### 重点利用文本数据（护理记录中的操作）
# 查看护理记录有多少患者
length(unique(data_record_combined$患者编号))  # 2540个患者，我想将record作为协变量来考虑，但是这样会损失一些样本量
# 查看病史数据中的样本量
length(unique(data_geren3$患者编号))
# 查看输血数据中的样本量
length(unique(data_blood_model_clean$患者编号))

#########  我们将重心放到data_record_combined
#  只提取项目列中非NA值
治疗项目汇总 <- data_record_combined %>%
  filter(!is.na(项目)) %>%
  pull(项目) %>%
  unique() %>%
  sort()
# 转为 data.frame（或 tibble）
治疗项目汇总_df <- data.frame(项目 = 治疗项目汇总)
# 保存中文字符
write_excel_csv(治疗项目汇总_df, '治疗项目汇总.csv')


### 药物关键词字典，给自己看就行
drug_keywords_units <- list(
  "舒普深" = "mg",
  "比阿培南" = "mg",
  "头孢他啶他唑巴坦钠" = "mg",
  "头孢噻肟钠舒巴坦"="mg",
  "舒普深"= "mg",
  "止血敏" = "ml",
  "去甲肾上腺素" = "mg",
  "葡萄糖酸钙"="mg",
  "速尿针" ="mg",
  "葡萄糖" = "ml",
  "氯化钠" = "ml",
  "氯化钾" = "ml",
  "碳酸氢钠" = "ml",
  "硫酸镁" = "mg",
  "美罗华" = "ml",
  "还原型谷胱甘肽" = "mg",
  "西艾克" ="mg",
  "阿霉素"="mg",
  "胰岛素" = "IU",
  "氨基酸" = "ml",
  "脂肪乳" = "ml",
  "赛德萨" = "mg",
  "地塞米松" = "mg",
  "乐凡命" = "ml",
  "甲强龙" = "mg",
  "阿拓莫兰" = "mg",
  "多巴胺" = "ml",
  "丙氨酰谷氨酰胺" = "ml",
  "烟酰胺" = "mg",
  "顺铂"= "mg"    ,
  "环磷酰胺" ="mg"  ,
  "氟达拉滨" ="mg" ,
  "苯达莫司汀"=  "mg" ,
  "地西他滨" =  "mg"  ,
  "达托霉素" = "mg",
  "甲泼尼龙琥珀酸钠" = "mg",
  "维生素B6注射液" = "mg",
  "静注人免疫球蛋白" = "mg",
  "甲氨蝶呤" ="mg",
  "利妥昔单抗" = "mg"
)

### 药物关键词字典
drug_keywords <- c(
  "舒普深", "比阿培南", "头孢他啶他唑巴坦钠", "头孢噻肟钠舒巴坦",
  "止血敏", "去甲肾上腺素", "葡萄糖酸钙", "速尿针", "葡萄糖", "氯化钠", "氯化钾", 
  "碳酸氢钠", "硫酸镁", "美罗华", "还原型谷胱甘肽", "西艾克", "阿霉素", "胰岛素",
  "氨基酸", "脂肪乳", "赛德萨", "地塞米松", "乐凡命", "甲强龙", "阿拓莫兰", "多巴胺",
  "丙氨酰谷氨酰胺", "烟酰胺", "顺铂", "环磷酰胺", "氟达拉滨", "苯达莫司汀", 
  "地西他滨", "达托霉素", "甲泼尼龙琥珀酸钠", "维生素B6注射液", "静注人免疫球蛋白",
  "甲氨蝶呤", "利妥昔单抗"
)

# -------------------------------
# 辅助函数：提取剂量并按规则换算
# -------------------------------
extract_total_dose_for_drug <- function(txt_vec, keyword) {
  sapply(txt_vec, function(txt) {
    if (!grepl(keyword, txt)) return(0)
    
    # 括号包围剂量，例如“舒普深(1.5g)”或“舒普深注射剂(500mg)”
    pattern1 <- paste0(keyword, ".*?[（(](\\d+(?:\\.\\d+)?)(mg|g|ml|IU)[)）]")
    match1 <- str_match_all(txt, pattern1)[[1]]
    
    # 不带括号的剂量，例如“舒普深 1.5g”或“舒普深注射液500mg”
    pattern2 <- paste0(keyword, ".*?(\\d+(?:\\.\\d+)?)[\\s]*?(mg|g|ml|IU)")
    match2 <- str_match_all(txt, pattern2)[[1]]
    
    # 合并所有匹配
    matches <- rbind(match1, match2)
    if (nrow(matches) == 0) return(0)
    
    vals <- as.numeric(matches[, 2])
    units <- matches[, 3]
    
    # 单位转换：g → mg（×1000）
    vals[units == "g"] <- vals[units == "g"] * 1000
    
    # 其余单位（mg, ml, IU）保持原值
    sum(vals, na.rm = TRUE)
  })
}
# -------------------------------
# 清洗文本
# -------------------------------
data_record_combined_1 <- data_record_combined %>%
  dplyr::select(-starts_with("drug_")) %>%
  mutate(
    项目_clean = str_trim(str_replace_all(项目, "\n", "")),
    项目_clean = str_replace_all(项目_clean, "＋", "+")
  )
# -------------------------------
# 提取每个药物的总剂量
# -------------------------------
for (drug in drug_keywords) {
  var_name <- paste0("drug_", str_replace_all(drug, "[^\\w]", "_"))
  data_record_combined_1[[var_name]] <- extract_total_dose_for_drug(data_record_combined_1$项目_clean, keyword = drug)
}

# 查看整体情况
summary(data_record_combined_1)


########### 测试用，不用运行
grep("舒普深.*?(\\d+(\\.\\d+)?)[\\s]*mg", data_record_combined_1$项目_clean, value = TRUE, perl = TRUE)
grep("舒普深", data_record_combined_1$项目_clean, value = TRUE)[1:5]
string <- "舒普深注射液500mg"
str_match(string, "舒普深.*?(\\d+(\\.\\d+)?)[\\s]*mg")
# 提取包含舒普深的所有行
subset_lines <- grep("舒普深", data_record_combined_1$项目_clean, value = TRUE)

# 测试从中提取剂量（g）
str_match(subset_lines, "舒普深.*?(\\d+(\\.\\d+)?)[^\\d]{0,5}g")

txt_sample <- data_record_combined_1$项目_clean[1:5000]
doses <- extract_total_dose_for_drug(txt_sample, keyword = "舒普深")
summary(doses)
table(doses[doses > 0])
######################  测试用


# 提取指定列的数据
data_zhiliao <- data_record_combined_1[, c(1, 2, 3, 22:61)]
# 查看患者数
length(unique(data_zhiliao$患者编号))
length(unique(data_record_combined_1$患者编号))

# 生成一个新列，将记录日期转换为Date形式
data_zhiliao$记录日期_Date <- as.Date(data_zhiliao$记录日期)
# 将同患者且同一个记录日期_Date的行进行合并，合并值取相应值的和
data_zhiliao_merged <- data_zhiliao %>%
  group_by(患者编号, 记录日期_Date) %>%
  summarise(across(c(4:42), sum, na.rm = TRUE), .groups = "drop")
# 查看患者数
length(unique(data_zhiliao_merged$患者编号))  #  2540个患者


# 处理护理干预措施
# 展示护理干预措施
unique(data_record_combined$护理操作项目类目名称)
unique(data_record_combined$护理操作名称)
unique(data_record_combined$护理观察项目名称)
unique(data_record_combined$心理护理)
unique(data_record_combined$安全护理)
unique(data_record_combined$气管护理)
length(unique(data_record_combined$病情及处置))

#  只提取护理干预列中非NA值
护理操作汇总 <- data_record_combined %>%
  filter(!is.na(病情及处置)) %>%
  pull(病情及处置) %>%
  unique() %>%
  sort()
# 转为 data.frame（或 tibble）
护理操作汇总_df <- data.frame(病情及处置 = 护理操作汇总)
# 保存中文字符
write_excel_csv(护理操作汇总_df, '护理操作汇总.csv')


############ 六、聚焦 ###################
############ 保存结果
save.image("贝叶斯诊断结果.RData")
####### 加载模型
load("贝叶斯诊断结果.RData")

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




### 6.3 确认动态体能/虚弱（frailty）因子


# 给陈国鹏导出红系数据
write.csv(data_red_model_clean, "F:/血液科数据/红系数据.csv", row.names = FALSE)
