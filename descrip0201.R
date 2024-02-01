#设置工作路径
setwd("E:/team_compst")

#加载必要的包
library(haven)
library("readr")
library(readxl)
library(dplyr)
library(tidyr)
library(psych) 
library(ggplot2)

#表1基本信息表
basif1<- read_excel("表1 课题组人员基本信息0130.xlsx")

file_path <- "表1 课题组人员基本信息0130.xlsx"
sheet_name <- "Sheet2"
basif2 <- read_excel(file_path, sheet = sheet_name)

file_path <- "表1 课题组人员基本信息0130.xlsx"
sheet_name <- "Sheet3"
basif3 <- read_excel(file_path, sheet = sheet_name)


#表2课题组人员名册
teamsci <- read_excel("表2 课题组人员名册0130.xlsx")


#表4学生信息名单
stuif <- read_excel("表4学生信息名单0124.xlsx")

#表5绩效0801表
file_path<- "表5绩效0130.xlsx"
sheet_name<- "Sheet2"
compsci<- read_excel(file_path, sheet = sheet_name)


#表3-1论文
pubsci<- read_excel("表3-1-论文-0805.xlsx")

#表3-2专利
patsci<- read_excel("表3-2专利0126.xlsx")



#表1生成唯一识别码
basif1<- basif1 %>% 
  mutate(yeid= paste(年份,员工编号,sep = "&")) %>%
  select(yeid, 课题组ID)

#表2匹配表1得课题组ID
teamsci <- teamsci %>%
  filter(分配职务分类 != "工人") %>%
  mutate(yeid = paste(年份, 员工编号, sep = "&")) %>%
  left_join(basif1, by = "yeid") %>% 
  mutate(课题组ID = ifelse(is.na(课题组ID),
                        basif2$课题组ID[match(课题组, basif2$课题组)],
                        课题组ID))  

#IV课题组人数  
nbsci<- teamsci %>%
  group_by(课题组ID,年份) %>%
  summarise(nb_sci = n())



#IV课题组各类各类职称老师数-分配职务分类
nbtitl1<- teamsci %>%
  group_by(课题组ID,年份,分配职务分类) %>%
  summarise(nb_title1= n())

#将分配职务分类这个长数据转化为宽数据
nbtitl1w <- nbtitl1 %>%
  pivot_wider(names_from = "分配职务分类", values_from = "nb_title1") %>% 
  replace(is.na(.), 0)


#IV课题组各类各类职称老师数-分配职务名称
nbtitl2<- teamsci %>%
  group_by(课题组ID,年份,分配职务名称) %>%
  summarise(nb_title2= n())

#将分配职务名称这个长数据转化为宽数据
nbtitl2w <- nbtitl2 %>%
  pivot_wider(names_from = "分配职务名称", values_from = "nb_title2") %>% 
  replace(is.na(.), 0)


#IV课题组各类各类职称老师数-主要分配职称
nbtitl3<- teamsci %>%
  group_by(课题组ID,年份,主要分配职称) %>%
  summarise(nb_title3= n())

#将主要分配职称这个长数据转化为宽数据
nbtitl3w <- nbtitl3 %>%
  pivot_wider(names_from = "主要分配职称", values_from = "nb_title3") %>% 
  replace(is.na(.), 0)


#IV课题组的博后人数
nbposd<- teamsci %>%
  group_by(课题组ID,年份,员工类型) %>%
  summarise(博士后数量 = n()) %>%
  filter(员工类型 %in% c("03.博士后","13.博士后","13.全职博士后","14.在职博士后")) %>% 
  group_by(课题组ID,年份) %>%
  summarise(nb_posd = sum(博士后数量))



#表4生成唯一识别码

str(stuif[c("学号", "入学年份", "毕业年度","学制")])
#将字符串类型改为数值型
stuif$入学年份 <- as.numeric(stuif$入学年份)
stuif$毕业年度 <- as.numeric(stuif$毕业年度)
stuif$学制 <- as.numeric(stuif$学制)

# 使用 mutate 函数创建一个新的字段 "填充后的毕业年度"
stuif <- stuif %>%
  mutate(填充后的毕业年度 = ifelse(is.na(毕业年度), 入学年份 + 学制-1, 毕业年度-1))


#使用 rowwise() 确保扩行对每个学生生成在读年份序列
stuif <- stuif %>%
  rowwise() %>%
  mutate(Year = list(seq(入学年份, 填充后的毕业年度))) %>%
  unnest(Year) 

  
#IV课题组学生数量
stuif<-stuif %>% 
  mutate(yeid= paste(Year,导师ID,sep = "&")) %>% 
  left_join(basif1, by = "yeid")

nbstu1<- stuif %>%
  group_by(导师ID,Year) %>%
  summarise(nb_sstu = n())

nbstu<- stuif %>%
  group_by(课题组ID,Year) %>%
  summarise(nb_stu= n())


#表5生成唯一识别码
compsci <- compsci %>% 
  mutate(yeid = paste(年份, 编号, sep = "&")) %>% 
  left_join(basif1, by = "yeid") %>%
  mutate(课题组ID = ifelse(is.na(课题组ID),
                        basif2$课题组ID[match(课题组, basif2$课题组)],
                        课题组ID)) 
  

#IV课题组平均工资
avwage<- compsci %>%
  group_by(课题组ID,年份) %>%
  summarise(wage_ave = mean(实发合计,na.rm = TRUE))


gini <- compsci %>%
  left_join(teamsci %>% select(yeid, 主要分配职称), by = "yeid") %>% 
  group_by(课题组ID, 年份, 主要分配职称) %>% 
  arrange(实发合计) %>%
  summarise(
    Gini = (2 * sum(row_number() * 实发合计)) / (n() * sum(实发合计)) - ((n() + 1) / n())
  )


horwage<- gini %>% 
  group_by(课题组ID,年份) %>%
  summarise(wage_hor = mean(Gini,na.rm = TRUE))


vetwage <- compsci %>%
  left_join(teamsci %>% select(yeid, 主要分配职称), by = "yeid") %>% 
  group_by(课题组ID, 年份, 主要分配职称) %>% 
  summarise(实发合计 = mean(实发合计, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(课题组ID, 年份) %>% 
  arrange(实发合计) %>% 
  summarise(
    wage_vet = (2 * sum(row_number() * 实发合计)) / (n() * sum(实发合计)) - ((n() + 1) / n())
  )



#表3生成唯一识别码
#IV论文产出

##课题组层面的论文产出
# 获取所有包含 "Term_ID" 的列
term_id_cols <- grep("TERM_ID", colnames(pubsci), value = TRUE)
#循环生成yeid1,yeid2..yeid21列
for (term_id_col in term_id_cols) {
  join_col <- sub("TERM_ID", "yeid", term_id_col)
  pubsci[[join_col]] <- paste(pubsci$YEAR, pubsci[[term_id_col]], sep = "&")
}


#用yeid1,2..21列匹配basif1的yeid列，生成相应的课题组ID1，2..21列
yeid_cols <- grep("yeid", colnames(pubsci), value = TRUE)
for (yeid_col in yeid_cols) {
  join_col <- sub("yeid", "课题组ID", yeid_col)
  pubsci[[join_col]] <- basif1$课题组ID[match(pubsci[[yeid_col]], basif1$yeid)]
}



# 定义逐行查找和替换重复元素的函数
replace_duplicates <- function(row) {
  unique_values <- unique(row)
  duplicated_values <- duplicated(row)
  row[duplicated_values] <- 0
  return(row)
}

# 使用 apply 逐行应用函数到pubsci 中以 "课题组ID" 开头的列
selected_cols <- grep("^课题组ID", colnames(pubsci), value = TRUE)
pubsci[, selected_cols] <- t(apply(pubsci[, selected_cols], 1, replace_duplicates))

#将pubsci这个宽数据转换为长数据
nbpub <- pubsci %>% pivot_longer(cols = starts_with("课题组ID"), names_to = "课题组序号", values_to = "课题组ID") %>% 
  filter(课题组ID != 0) %>%
  group_by(YEAR,课题组ID) %>%
  summarise(nb_pub = n())

##个体层面论文产出，但注意，个体层面的计算结果不能直接匹到课题组！！！
nbpub1 <- pubsci %>%
  mutate(across(starts_with("TERM_ID"), as.character)) %>%
  pivot_longer(cols = starts_with("TERM_ID"), names_to = "TERM_ID", values_to = "scid") %>%
  filter(TERM_ID != "&NA") %>%
  group_by(YEAR, scid) %>%
  summarise(nb_spub = n())



#对patsci的发明人列匹配 basif3的员工姓名列，生成员工ID列
patsci <- patsci %>%
  separate_rows(发明人, sep = "[,]") %>%
  left_join(basif3, by=c("发明人" = "员工姓名")) %>% 
  mutate(yeid= paste(申请年,员工编号,sep = "&")) %>%
  left_join(basif1, by = "yeid") 


#同一个申请号中的课题组ID第二次出现则替换为NA
patsci <- patsci %>%
  group_by(申请号) %>%
  mutate(课题组ID = replace(课题组ID, duplicated(课题组ID), NA)) %>%
  ungroup()

#IV课题组专利产出
nbpat<- patsci %>% 
  group_by(申请年,课题组ID) %>%
  summarise(nb_pat = n())


#发明人专利产出
nbpat1<- patsci %>%
  group_by(申请年, 员工编号) %>%
  summarise(nb_spat = n())



names(nbsci)[1] <- "idteam"
names(nbsci)[2] <- "year"

names(nbstu)[1] <- "idteam"
names(nbstu)[2] <- "year"

names(nbposd)[1] <- "idteam"
names(nbposd)[2] <- "year"

names(nbtitl3w)[1] <- "idteam"
names(nbtitl3w)[2] <- "year"

names(avwage)[1] <- "idteam"
names(avwage)[2] <- "year"

names(horwage)[1] <- "idteam"
names(horwage)[2] <- "year"
names(vetwage)[1] <- "idteam"
names(vetwage)[2] <- "year"

names(nbpub)[1] <- "year"
names(nbpub)[2] <- "idteam"
names(nbpat)[1] <- "year"
names(nbpat)[2] <- "idteam"



#合并数据
ceram <- nbsci %>%
  left_join(nbposd, by = c("idteam", "year")) %>%
  mutate(nb_posd = ifelse(is.na(nb_posd), 0, nb_posd)) %>%
  left_join(nbstu, by = c("idteam", "year")) %>%
  mutate(nb_stu = ifelse(is.na(nb_stu), 0, nb_stu)) %>%
  left_join(avwage, by = c("idteam", "year")) %>%
  left_join(horwage, by = c("idteam", "year")) %>%
  left_join(vetwage, by = c("idteam", "year")) %>%
  left_join(nbpub, by = c("idteam", "year")) %>%
  left_join(nbpat, by = c("idteam", "year")) %>%
  left_join(nbtitl3w, by = c("idteam", "year")) %>% 
  filter(!is.na(idteam)) 

names(nbstu1)[1] <- "idsci"
names(nbstu1)[2] <- "year"
names(nbpub1)[1] <- "year"
names(nbpub1)[2] <- "idsci"
names(nbpat1)[1] <- "year"
names(nbpat1)[2] <- "idsci"

head(nbstu1)
head(nbpub1)
head(nbpat1)


#将nbpub1中的idteam列变成数值型
nbstu1$idteam <- as.numeric(nbstu1$idteam)
cerams<- nbpub1 %>% 
  full_join(nbpat1, by = c("idsci", "year")) %>%
  full_join(nbstu1, by = c("idsci", "year")) %>% 
  filter(!(is.na(nb_spub) & is.na(nb_spat) & is.na(nb_sstu))) 



#输出ceram到csv
write.csv(ceram, file = "ceram.csv", row.names = FALSE)
write.csv(cerams, file = "cerams.csv", row.names = FALSE)



#描述性分析表格，并保留三位小数
descrip<- round(describe(ceram[,3:10]),3)

#输出des到csv文本
write.csv(descrip, file = "descrip.csv", row.names = TRUE)


# 计算相关系数矩阵
cor_matrix <- cor(ceram[,3:10], use = "pairwise.complete.obs", method = "pearson")

cor_matrix <- round(cor_matrix, 3)

#输出
write.csv(cor_matrix, file = "cor_matrix.csv", row.names = TRUE)













