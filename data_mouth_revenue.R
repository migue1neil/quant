gc()
source("func_caculate_index.R" , encoding = "utf-8")
setwd("C:/Users/Neil/Documents/git-repos/backtest_in_R/stock_data")
library(readxl)
library(stringr)
library(data.table)
library(plyr)
library(tidyverse)
library(TTR)

#單月營收 : 從TEJ抓下來整理好的資料:處理完年月直接跑z分數即可
month_revenue = read_excel("C:/Users/Neil/Documents/git-repos/backtest_in_R/stock_data/8月營收.xlsx")
month_revenue = month_revenue %>% plyr::rename(c( "代號"="證券代碼", "名稱" = "公司名稱" ,
                                                  "單月營收成長率％" = "月營收YoY" , "單月營收與上月比％" = "月營收MoM","單月營收(千元)" = "單月營收_千元"))
month_revenue$證券代碼 = month_revenue$證券代碼 %>% as.numeric()
month_revenue = month_revenue[,c(1:8)]
month_revenue$年月 = gsub("/","",month_revenue$年月) %>% as.numeric()
month_revenue = month_revenue %>% na.omit()
month_revenue = month_revenue %>% arrange(年月,證券代碼)
month_revenue$年 = substr(month_revenue$年月 , 1 , 4) %>% as.numeric()
month_revenue$月 = substr(month_revenue$年月 , 5 , 6) %>% as.numeric()
industry_tag = fread("industry_tag.txt" ,encoding = "unknown" , header =  T ,sep = ",")
month_revenue = left_join(month_revenue , industry_tag  , by = c("證券代碼","公司名稱"))
month_revenue = month_revenue %>% filter( is.na(證券代碼) == F 
                                             & TSE產業別 != 91 
                                             & TSE產業別 != 0 
                                             & TSE產業別 != 14
                                             & TSE產業別 != 17
                                                  )
                                             #& TSE產業別 != 20
# z-score

month_revenue = ddply( month_revenue, c("年","月") , 
                       .fun= function(x){
                         transform(x, 
                                   z_月營收YoY = 月營收YoY %>% scale() ,
                                   z_月營收MoM = 月營收MoM %>% scale()
                         )
                       } )

#時間標籤 
month_revenue$時間標籤 =  ifelse( month_revenue$月 < 12 , month_revenue$年*10000 + (month_revenue$月+1)*100 + 10 ,(month_revenue$年+1)*10000 + 0110 )

write.table(month_revenue, "month_revenue202209.txt" , row.names = FALSE , sep = ",")

                                                                               



#全部營收
month_revenue = fread("month_revenue20220819.txt")
month_revenue = read.csv("month_revenue20220819下市櫃.csv", encoding = "utf-8",header = T)
colnames(month_revenue) = c("證券代碼","公司名稱","年月","單月營收_千元","單月營收年增率")
month_revenue= month_revenue[,c("證券代碼","公司名稱","年月","單月營收_千元")]
month_revenue$證券代碼 = month_revenue$證券代碼 %>% as.numeric()
month_revenue$年月 = gsub("/","",month_revenue$年月) %>% as.numeric()
month_revenue = month_revenue %>% na.omit()
month_revenue = month_revenue %>% arrange(年月,證券代碼)
month_revenue$年 = substr(month_revenue$年月 , 1 , 4) %>% as.numeric()
month_revenue$月 = substr(month_revenue$年月 , 5 , 6) %>% as.numeric()
#產業代碼
#
# industry_tag = table_data[,c("證券代碼","公司名稱","TSE產業別")]
# industry_tag = industry_tag %>% unique(by = "證券代碼")
# write.table(industry_tag, "industry_tag.txt" , row.names = FALSE , sep = ",")
#
industry_tag = fread("industry_tag.txt" ,encoding = "unknown" , header =  T ,sep = ",")


month_revenue = left_join(month_revenue , industry_tag  , by = c("證券代碼","公司名稱"))
#排除特定產業 #排除DR股(91) 建築(14) 金融(17)
month_revenue = month_revenue %>% filter(TSE產業別 != 0 & TSE產業別 != 91 & TSE產業別 != 14 & TSE產業別 != 17 )

#test
#month_revenue = month_revenue %>% filter(年月>201001)

month_revenue = ddply( month_revenue, c("證券代碼","月") , 
             .fun= function(x){
               transform(x, 
              月營收YoY = ((單月營收_千元-lag(單月營收_千元,1))/lag(單月營收_千元,1) ) %>% round(digits = 4)
               )
             } )
month_revenue = month_revenue %>% arrange(證券代碼,年月)
month_revenue = ddply( month_revenue, c("證券代碼") , 
                       .fun= function(x){
                         transform(x, 
                         月營收MoM = ((單月營收_千元-lag(單月營收_千元,1))/lag(單月營收_千元,1) ) %>% round(digits = 4)
                         )
                       } )

month_revenue = month_revenue %>% na.omit()

#移除極端值
month_revenue = month_revenue %>% filter( is.infinite(月營收YoY) == F & is.infinite(月營收MoM) == F )


# z-score

month_revenue = ddply( month_revenue, c("年","月") , 
                       .fun= function(x){
                         transform(x, 
                                   z_月營收YoY = 月營收YoY %>% scale() ,
                                   z_月營收MoM = 月營收MoM %>% scale()
                         )
                       } )

library(lattice) # 畫機率密度函數座使用
testt = month_revenue %>% filter(年 == 2022)
table(abs(testt$z_月營收YoY) > 3)
densityplot(testt$z_月營收YoY)
#時間標籤 
month_revenue$時間標籤 =  ifelse( month_revenue$月 < 12 , month_revenue$年*10000 + (month_revenue$月+1)*100 + 10 ,(month_revenue$年+1)*10000 + 0110 )

#rank()
#月營收佔資產比之類的?
#想一想要怎麼排除極端值

write.table(month_revenue, "month_revenue202209.txt" , row.names = FALSE , sep = ",")



## 合併功能
month_revenue_old  = fread("C:/Users/Neil/Documents/git-repos/backtest_in_R/stock_data/month_revenue202208.txt",
                                          encoding = "unknown" , header = T,sep = ",")
month_revenue_new  = fread("C:/Users/Neil/Documents/git-repos/backtest_in_R/stock_data/month_revenue202209.txt",
                                          encoding = "unknown" , header = T,sep = ",")

month_revenue = merge(month_revenue_old , month_revenue_new , by = c("證券代碼","公司名稱","年月","單月營收_千元","年","月","TSE產業別","月營收YoY","月營收MoM","z_月營收YoY","z_月營收MoM","時間標籤"), all = T )
write.table(month_revenue, "month_revenue202209.txt" , row.names = FALSE , sep = ",")
