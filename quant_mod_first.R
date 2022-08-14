setwd("C:/Users/Neil/Documents/git-repos/backtest_in_R/quant") # 設定工作目錄

# #施工使用
 start_day = 20210101
 end_day = 20220601
 discount = 0
 trade_frequency = "season"
 A = 100
 global_market_index = 0050
 stop_loss_point = -0.10
stop_loss_func = F
 # #       

# package_list = c("data.table","dplyr","plyr","readr","ggplot2","lubridate","tseries","magrittr","foreach","cowplot","lattice")
library(data.table)
library(dplyr)
library(plyr)
library(readr) #讀取檔案必備
library(ggplot2) # 畫圖使用
library(lubridate) #轉換日期使用 
library(tseries) #會用到最大回落
library(magrittr) # %>% 水管工人
library(TTR) # 量化套件，結合dplyr使用可以快速分組做計算
library(cowplot)
library(lattice) # 畫機率密度函數座使用
# library(stringr)
# library(tidyr)
source("portfolio_function.R" , encoding = "utf-8")
source("caculate_index_function.R" , encoding = "utf-8")
source("trade_time_choose_function.R" , encoding = "utf-8")


table_data = fread("C:/Users/Neil/Documents/git-repos/backtest_in_R/stock_data/tidy_stock_price_data20100104_20220809.txt", encoding = "unknown" , header = T,sep = ",")
table_data = table_data[年月日 > 20100101,]
IFRS = fread("C:/Users/Neil/Documents/git-repos/backtest_in_R/stock_data/tidy_z_score_fin_index_data20220814.txt", encoding = "unknown" , header = T,sep = ",")

quant_func = function(table_data, start_day , end_day , A = 100 , global_market_index = 0050 , discount = 0 , trade_frequency = "season" , stop_loss_func = F, stop_loss_point = -0.25){
  exe_time = Sys.time()
  
  # 交易時間篩選
  trade_pair = trade_time_func(table_data , start_day = start_day , end_day = end_day , trade_frequency = trade_frequency )  
  trade_day = trade_pair$買
  
  # 篩出stock list，設計一個for迴圈，把每次交易日要篩選的股票篩選出來，在合併，形成一籃子要交易的股票
  # 篩選的回合，根據上面給出的交易日期，使用迴圈方式，把每期要交易的股票篩選出來
   # i = 1 
   # buy_day= trade_day[i]

  standby_stock_list = data.table()
  for(i in   1:length(trade_day)){
    buy_day= trade_day[i] # buy_day會等於那一天的資料
    tmp_a = table_data[table_data$年月日 == buy_day,]  # 目的:生產出要買進股票那一天的資料，TEJ的話感覺可以不用改
    fin_factor = IFRS %>% filter(時間標籤 <= buy_day) %>% group_by(證券代碼) %>% filter(時間標籤 == max(時間標籤)) #小於交易日中選最大的(最近的)
    tmp_a = tmp_a %>% na.omit()
    ##### 篩選環節 先用抵著
     #基本過濾
     #cardinal = order(tmp_a$調整收盤價, decreasing = T) # 範例 #排序當天的股票 收盤價最高的 
     tmp_a = tmp_a %>% filter(TSE產業別 > 0) # 排除ETF 有先幫ETF設定成TSE產業別為0
     tmp_a = tmp_a %>% filter(TSE產業別 != 91) # 移除台灣存託憑證股票 DR股 二次上櫃股票
     tmp_a = tmp_a %>% filter(證券代碼 > 1000 & 證券代碼< 10000) # 避免沒排除的ETF以及DR股
     tmp_a = tmp_a %>% filter(tmp_a$Price_MA_20 > 10) #排除雞蛋水餃股
     tmp_a = tmp_a[tmp_a$調整收盤價 > tmp_a$Price_MA_60, ] #季線上 動能
     tmp_a = tmp_a[tmp_a$成交張數 > 300 ,] #流動性
     #cardinal = order(tmp_a$CV股價離散程度, decreasing = F) #依照CV做排序，由小排到大
     
     #新想到的財報數據篩選 優點: 不用合併財報 缺點:沒有練到功，還是不會最近月份合併
     fin_factor = fin_factor %>% filter(淨利 > 0)
     sum_table = inner_join(tmp_a , fin_factor , by = c("證券代碼","公司名稱","TSE產業別"))
     
     sum_table$total_score = (sum_table$score_profit + sum_table$score_growth - 
                             (sum_table$z_sigma_ROE+ sum_table$z_sigma_ROA + sum_table$sigma_ROE + sum_table$z_CV_price)/4  )  #-號是正確的 台灣大z_CV_price在-1.35
    
     cardinal = order(sum_table$total_score, decreasing = T) #依照排序，由大排到小
     
    #####篩選結束
    # 收集篩選出來的股票資料，難的地方在於對於list不熟，只能用df硬轉
    sum_table = sum_table[cardinal,]
    rownames(sum_table) = NULL
    sum_table = sum_table[1:30,] #前30隻最大的股票 
    tmp_b = data.table(stock_list = sum_table$證券代碼) #收集這30隻的證券代碼
    standby_stock_list = cbind(standby_stock_list,tmp_b) #匯出
  }
 
  # 把每期的股票丟到(portfolio_func)回測裡面，
  log_trade_list = data.table()
  log_portfolio_stock_trade = data.table()
  each_portfolio_return_rate = data.table()
  for(i in  1:length(standby_stock_list)  ){
    aa = standby_stock_list[,i , with = FALSE] %>% na.omit() #不知道為什麼要這樣寫才可以提取出來
    aa = aa$stock_list %>% as.vector()
    bb = trade_pair$買[i]
    cc = trade_pair$賣[i]
    dd = portfolio_function(table_data, start_day = bb , end_day = cc , stock_list = aa ,
                            A = A ,discount = discount, global_market_index = global_market_index, stop_loss_func = stop_loss_func , stop_loss_point = stop_loss_point)
    log_trade_list = rbind(log_trade_list,dd[[1]])
    log_portfolio_stock_trade = rbind(log_portfolio_stock_trade,dd[[2]])
    each_portfolio_return_rate = rbind(each_portfolio_return_rate, dd[[3]] )
  }
  
  package = multiple_period_portfolio_index_graph_func(log_trade_list,log_portfolio_stock_trade,each_portfolio_return_rate)
  print(Sys.time()-exe_time )
  
  return(package)
 
  
  
  
}

log_list_package = quant_func(table_data , start_day = 20130101 , end_day = 20220807 , trade_frequency = "season",stop_loss_func = F ,stop_loss_point = -0.25)
log_final_report = log_list_package[[2]]
log_trade_list = log_list_package[[3]]
log_portfolio_stock_trade = log_list_package[[4]]

daily_portfolio_change = log_list_package[[1]]  #輸出日報酬變動，方便後面學術上要畫分布圖的時候可以使用

print(log_final_report)
#設計一下輸出表格


gc()