# 之前還在摸索時設計的函數，現在大部分都不用使用了，留著當作備份

library(data.table)
library(lubridate)
library(stringr)
library(plyr)
library(tidyverse)
library(TTR)

##嘗試不用模組自己分組作計算單日漲跌幅，等風險獲利指標
# source("C:/Users/Neil/Documents/git-repos/TW-telecompany-portfolio/portfolio_function.R" , encoding = "utf-8")

 #  setwd("C:/Users/Neil/Documents/git-repos/backtest_in_R/quant")
 #  stock_price_data = read.csv2("C:/Users/Neil/Documents/git-repos/backtest_in_R/stock_data/tidy_stock_price_data20130101_20220607.csv", encoding = "mbcs" , header = T,sep = ",") %>% data.table()
 #  table_data = stock_price_data
 #  table_data$調整開盤價 = as.numeric(table_data$調整開盤價)
 #  table_data$調整收盤價 = as.numeric(table_data$調整收盤價)
 # # #table_data = table_data[table_data$年月日> 20220101,]
 # # #table_data = table_data %>% filter(table_data$證券代碼 < 60)
 #  rownames(table_data) = NULL

# library(parallel)
# cpu.cores <- detectCores()
# cl <- makeCluster(cpu.cores)

##### 範例的漲跌幅
#設計一個函數，可以分組後往下移n個單位
# group_daily_change_function = function( table_data , n = 1){ #設計一個函數，可以分組後往下移n個單位
#   shift_data = ddply( table_data , c("證券代碼","公司名稱") , 
#                       .fun= function(x){ #這裡的x是指根據上面分完組切出來的table_data
#                         transform(x, 前一日價格 = with(x, shift(調整收盤價 , n )))
#                       } )
#   shift_data = na.omit(shift_data)
#   shift_data$daily_change = (shift_data$調整收盤價 - shift_data$前一日價格) / shift_data$前一日價格
#   return (shift_data)
# }
# table_data = group_daily_change_function(table_data)

#####
##### 自己寫的單日漲跌幅 慢很多
# daily_change_function = function(table_data, n = 1){ #輸入整理好的股價資料，會根據股票分類計算，噴出每日變動， n = 移動距離
#   all_stock_list = unique(table_data$證券代碼)
#   daily_change_df = data.table()
#   for (i in all_stock_list){
#     tmp_df = table_data[table_data$證券代碼 == i ,]
#     tmp_df$前一日價格 = tmp_df$調整收盤價 %>% shift(n = n)  
#     tmp_df$單日漲跌幅 = (tmp_df$調整收盤價 - tmp_df$前一日價格) / tmp_df$前一日價格
#     daily_change_df = rbind(daily_change_df,tmp_df)
#   }
# daily_change_df = na.omit(daily_change_df)
# daily_change_df$單日漲跌幅 = round(daily_change_df$單日漲跌幅,digits = 4)
# return(daily_change_df)
# }

#設計一個函數，可以計算累積乘積(每天的複利)
# group_cumprod_func = function(table_data){
#   table_data$tmp_index = table_data$daily_change + 1 #tmp_index是每日變動+1，不是複利
#   cumprod_index = ddply( table_data , c("證券代碼","公司名稱") , 
#                          .fun= function(x){
#                            transform(x, cumprod_return_rate = with(x, cumprod(tmp_index)))
#                          } )
#   #cumprod_index$cumprod_index = cumprod_index$cumprod_return_rate #施工用看一下還沒-1前的變化
#   cumprod_index$cumprod_return_rate = cumprod_index$cumprod_return_rate - 1 #這樣是變成那個時點的複利，所以到時候只要用投入資金乘上1+這個值就是這個時間點的複利效果
#   cumprod_index$cumprod_return_rate = round(cumprod_index$cumprod_return_rate,digit = 3)
#   return(cumprod_index)
# }
#table_data = group_cumprod_func(table_data)

## 自己寫的慢很多，不考慮日期，從資料開頭算複利算到期末，
# daily_cumprod_return_rate_function = function(table_data){ #輸入整理好的股價資料，會噴出單日變動幅度，以及複利報酬
#   all_stock_list = unique(table_data$證券代碼)
#   cumpord_rate_df = data.table()
#   for (i in all_stock_list){ #可以把開始日期以後的單日報酬率以及
#     tmp_df = table_data[table_data$證券代碼 == i ,]
#     tmp_df$前一日價格 = tmp_df$調整收盤價 %>% shift(1)  
#     tmp_df = na.omit(tmp_df)
#     tmp_df$單日漲跌幅 = (tmp_df$調整收盤價 - tmp_df$前一日價格) / tmp_df$前一日價格
#     tmp_df$tmp_index = tmp_df$單日漲跌幅+1 
#     tmp_df$cumpord_return_rate = tmp_df$tmp_index %>% cumprod()
#     tmp_df$cumpord_return_rate = round(tmp_df$cumpord_return_rate -1 , digits = 3)
#     cumpord_rate_df = rbind(cumpord_rate_df,tmp_df)
#     print(i)
# }
# return(cumpord_rate_df)
# }
#x = daily_cumprod_return_rate_function(table_data)


#移除小於設定天數的股票，例如: 有些股票只出現30天，如果我們設定60那他就會被移除
delete_less_than_ndays_func = function(table_data, ndays){  #設計成函數，要導入收盤價表，與小於多少的天數，會移除小於設定天數的股價
  ndays_stock_price = data.table(table(table_data$證券代碼)) #先統計證券代碼出現的次數，再將資料轉換成data.table格式
  colnames(ndays_stock_price) = c("證券代碼", "出現天數") #修改欄位名稱
  ndays_stock_price$證券代碼 = as.numeric(ndays_stock_price$證券代碼) #將證券代碼轉換成數字
  ndays_stock_price = ndays_stock_price[ndays_stock_price$出現天數 >= ndays,] #移除小於ndays的row
  forMA_stock_price = semi_join(table_data,ndays_stock_price, by = "證券代碼") #比較資料，如果左邊的ID有出現在右邊的話，將他留下。
  return (forMA_stock_price) #回傳dataframe出去
}

##### MA平均報酬率
# mean_daily_return_func = function(table_data, ndays){
#   table_data = delete_less_than_ndays_func(table_data , ndays)
#   MA_data = ddply(table_data, c("證券代碼") , 
#                   .fun= function(x){
#                     transform(x, mean_daily_return = with(x, SMA( daily_change , n = ndays )))
#                   } )
#   return(MA_data)
# }


# group_price_rolling_sd_func = function( table_data , n ){ #設計一個函數，可以分組後往下移n個單位
#   table_data = delete_less_than_ndays_func(table_data, n)
#   sd_data = ddply( table_data , c("證券代碼") , 
#                       .fun= function(x){ #這裡的x是指根據上面分完組切出來的table_data
#                         transform(x, roll_sd = with(x, runSD(調整收盤價, n = n, )) )
#                       } )
#   return (sd_data)
# }
# table_data = group_price_rolling_sd_func(table_data , n = 252 )




