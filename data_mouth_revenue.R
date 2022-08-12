gc()
source("func_caculate_index.R" , encoding = "utf-8")
setwd("C:/Users/Neil/Documents/git-repos/backtest_in_R/stock_data")
library(readxl)
library(stringr)
library(data.table)
library(plyr)
library(tidyverse)
library(TTR)

month_revenue = read_excel("月營收20220810.xlsx")
colnames(month_revenue) = c("證券代碼","公司名稱","年月","單月營收_千元","單月營收年增率")
month_revenue$證券代碼 = month_revenue$證券代碼 %>% as.numeric()
month_revenue$年月 = gsub("/","",month_revenue$年月) %>% as.numeric()
month_revenue = month_revenue %>% na.omit()
month_revenue = month_revenue %>% arrange(年月,證券代碼)
month_revenue$年 = substr(month_revenue$年月 , 1 , 4) %>% as.numeric()
month_revenue$月 = substr(month_revenue$年月 , 5 , 6) %>% as.numeric()

#test
#month_revenue = month_revenue %>% filter(年月>201001)

month_revenue = ddply( month_revenue, c("證券代碼","月") , 
             .fun= function(x){
               transform(x, 
                         月營收YOY = ((單月營收_千元-lag(單月營收_千元,1))/lag(單月營收_千元,1) ) %>% round(digits = 4)
               )
             } )
month_revenue = ddply( month_revenue, c("證券代碼","年") , 
                       .fun= function(x){
                         transform(x, 
                                   月營收MoM = ((單月營收_千元-lag(單月營收_千元,1))/lag(單月營收_千元,1) ) %>% round(digits = 4)
                         )
                       } )