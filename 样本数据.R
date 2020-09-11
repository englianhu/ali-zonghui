## 3210448065@qq.com
## leiou123
## 
## 日期	总添加	总注册	总首冲	总投注	总流水	总充值	总盈利	净投注	总体留存率	人均充值

## 读取程序包
if(!require('BBmisc')) {install.packages('BBmisc')}
require('BBmisc')
lib(c('readxl', 'lme4', 'forecast', 'openxlsx', 'plyr', 
      'xts', 'dplyr', 'knitr', 'quantmod', 'magrittr', 
      'purrr', 'lubridate', 'stats', 'knitr', 'kableExtra', 
      'formattable', 'ggplot2'))

## 读取数据
smp <- suppressAll(read_excel('巅峰9月各组每日报表.xlsx') %>% tbl_df)
names(smp) <- smp[1,]; smp %<>% .[-1,]
# 总盈利, 总添加, 总注册, 总首冲, 总投注, 总流水, 总充值
# names(smp) <- c('Date', 'PL', 'Add', 'RG', 'FD', 'TB', 'TO', 'TD')
smp %<>% mutate_if(is.character, as.numeric)
smp$日期 %<>% as.numeric %>% as.Date()


## 模型比较
m1 <- lm(总盈利 ~ 总添加 + 总注册 + 总首冲 + 总投注 + 总流水 + 总充值, data = smp)
m2 <- lm(总盈利 ~ 总添加 + 总注册 + 总首冲 + 总投注 + 总流水, data = smp)
m3 <- lm(总盈利 ~ 总添加 + 总注册 + 总首冲 + 总投注, data = smp)
m4 <- lm(总盈利 ~ 总添加 + 总注册 + 总首冲, data = smp)
m5 <- lm(总盈利 ~ 总添加 + 总注册 + 总首冲 + 总投注 + 总流水 + 总充值 + 总添加*总注册 + 总添加:总首冲 + 总注册:总首冲 + 总投注:总流水 + 总投注:总充值 + 总流水:总充值, data = smp)
m6 <- lm(总盈利 ~ 总添加 + 总注册 + 总首冲 + 总投注 + 总流水 + 总充值 + 总添加*总注册 + 总添加:总首冲 + 总注册:总首冲 + 总投注:总流水 + 总投注:总充值, data = smp)
m7 <- lm(总盈利 ~ 总添加 + 总注册 + 总首冲 + 总投注 + 总流水 + 总充值 + 总添加*总注册 + 总添加:总首冲 + 总注册:总首冲 + 总投注:总流水, data = smp)
m8 <- lm(总盈利 ~ 总添加 + 总注册 + 总首冲 + 总投注 + 总流水 + 总充值 + 总添加*总注册 + 总添加:总首冲 + 总注册:总首冲, data = smp)
m9 <- lm(总盈利 ~ 总添加 + 总注册 + 总首冲 + 总投注 + 总流水 + 总充值 + 总添加*总注册 + 总添加:总首冲, data = smp)

m <- list(m1 = m1, m2 = m2, m3 = m3, m4 = m4, m5 = m5, m6 = m6, m7 = m7, m8 = m8, m9 = m9)
rm(m1, m2, m3, m4, m5, m6, m7, m8, m9)

aic <- suppressAll(llply(1:length(m), function(i) {
  y <- data.frame(paste0('m', i), t(data.frame(extractAIC(m[[i]]))))
  names(y) <- (c('模型', '自由度', 'aic'))
  y
}) %>% bind_rows)


## 筛选最佳模型
summary(m[[5]])


## 转换数据类型
nm <- c('Date', 'PL', 'Add', 'RG', 'FD', 'TB', 'TO', 'TD', 'NB', 'NBR', 'AD')
names(smp) <- nm

smp %<>% na.omit
datt <- llply(smp[,-1], function(x) {
  x %<>% xts(order.by = smp$Date)
  x
})


## 预测三个月、半年、一年
## 样本数据 14天、21天
dbt <- smp$Date[14:length(smp$Date)]
datt <- llply(dbt, function(x) {
  dbtt <- smp$Date[smp$Date <= x]
  dbtt <- dbtt[(length(dbtt) - 14):length(dbtt)]
  z <- llply(datt, function(y) {
    y <- y[index(y) %in% dbtt]
    xx <- auto.arima(y, D=1)
    fst <- forecast(xx, h=1) %>% 
      tbl_df %>% select('Point Forecast') %>% 
      rename(`FP` = `Point Forecast`)
    names(y) %<>% str_extract_all('[A-Z]{1,}')
    y <- y[nrow(y)]
    zz <- data.frame(Date = index(y), y, fst) %>% tbl_df
    zz
  }) %>% bind_cols
}) %>% bind_rows

datt %<>% select(-Date1, -Date2, -Date3, -Date4, -Date5, 
                 -Date6, -Date7, -Date8, -Date9)  %>% 
  mutate(FP = c(NA, FP[-length(FP)]), FP1 = c(NA, FP1[-length(FP1)]), FP2 = c(NA, FP2[-length(FP2)]), FP3 = c(NA, FP3[-length(FP3)]), FP5 = c(NA, FP5[-length(FP5)]), FP6 = c(NA, FP6[-length(FP6)]), FP7 = c(NA, FP7[-length(FP7)]), FP8 = c(NA, FP8[-length(FP8)]), FP9 = c(NA, FP9[-length(FP9)]))
datt$FP4 = c(NA, datt$FP4[-length(datt$FP4)])

names(datt) <- c('日期',	'总添加',	'预测总添加',	
   '总注册',	'预测总注册',	'总首冲',	'预测总首冲',	
   '总投注',	'预测总投注',	'总流水',	'预测总流水',	
   '总充值',	'预测总充值',	'总盈利',	'预测总盈利',	
   '净投注',	'预测净投注',	'总体留存率',	'预测总体留存率',	
   '人均充值', '预测人均充值')


## 绘图
## 预测总添加 - 总添加
datt %>% ggplot(aes(日期)) + 
  geom_line(aes(y = 总添加, colour = 总添加)) + 
  geom_line(aes(y = 预测总添加, colour = 预测总添加))

## 预测总注册 - 总注册
datt %>% ggplot(aes(日期)) + 
  geom_line(aes(y = 总注册, colour = 总注册)) + 
  geom_line(aes(y = 预测总注册, colour = 预测总注册))

## 预测总投注 - 总投注
datt %>% ggplot(aes(日期)) + 
  geom_line(aes(y = 总投注, colour = 总投注)) + 
  geom_line(aes(y = 预测总投注, colour = 预测总投注))

## 预测总流水 - 总流水
datt %>% ggplot(aes(日期)) + 
  geom_line(aes(y = 总流水, colour = 总流水)) + 
  geom_line(aes(y = 预测总流水, colour = 预测总流水))

## 预测总充值 - 总充值
datt %>% ggplot(aes(日期)) + 
  geom_line(aes(y = 总充值, colour = 总充值)) + 
  geom_line(aes(y = 预测总充值, colour = 预测总充值))

## 预测总充值 - 总充值
datt %>% ggplot(aes(日期)) + 
  geom_line(aes(y = 总盈利, colour = 总盈利)) + 
  geom_line(aes(y = 预测总盈利, colour = 预测总盈利))

## 预测净投注 - 净投注
datt %>% ggplot(aes(日期)) + 
  geom_line(aes(y = 净投注, colour = 净投注)) + 
  geom_line(aes(y = 预测净投注, colour = 预测净投注))

## 预测总体留存率 - 总体留存率
datt %>% ggplot(aes(日期)) + 
  geom_line(aes(y = 总体留存率, colour = 总体留存率)) + 
  geom_line(aes(y = 预测总体留存率, colour = 预测总体留存率))

## 预测人均充值 - 人均充值
datt %>% ggplot(aes(日期)) + 
  geom_line(aes(y = 人均充值, colour = 人均充值)) + 
  geom_line(aes(y = 预测人均充值, colour = 预测人均充值))

## 预测人均充值 - 人均充值
datt %>% ggplot(aes(日期)) + 
  geom_line(aes(y = 人均充值, colour = 人均充值)) + 
  geom_line(aes(y = 预测人均充值, colour = 预测人均充值))


## MSE 比较预测值精准度
mse <- datt %>% 
  mutate(均方误差.总添加 = mean((预测总添加 - 总添加)^2),
             均方误差.总注册 = mean((预测总注册 - 总注册)^2),
             均方误差.总投注 = mean((预测总投注 - 总投注)^2),
             均方误差.总流水 = mean((预测总流水 - 总流水)^2),
             均方误差.总充值 = mean((预测总充值 - 总充值)^2),
             均方误差.总盈利 = mean((预测总盈利 - 总盈利)^2),
             均方误差.净投注 = mean((预测净投注 - 净投注)^2),
             均方误差.总体留存率 = mean((预测总体留存率 - 总体留存率)^2),
             均方误差.人均充值 = mean((预测人均充值 - 人均充值)^2)) %>% 
  select(均方误差.总添加, 均方误差.总注册, 均方误差.总投注, 
             均方误差.总流水, 均方误差.总充值, 均方误差.总盈利, 
             均方误差.净投注, 均方误差.总体留存率, 均方误差.人均充值) %>% 
  unique




