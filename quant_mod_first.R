setwd("C:/Users/Neil/Documents/git-repos/backtest_in_R/quant") # 設定工作目錄

# #施工使用
 # start_day = 20200101
 # end_day = 20220601
 # discount = 0
 # trade_frequency = "season"
 # A = 100
 # global_market_index = 0050
 # # #       

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

quant_func = function(table_data, start_day , end_day , A = 100 , global_market_index = 0050 , discount = 0 , trade_frequency = "season" ){
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
     #tmp_a = tmp_a[tmp_a$調整收盤價 > tmp_a$Price_MA_60, ] #季線上 動能
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
    dd = portfolio_function(table_data, start_day = bb , end_day = cc , stock_list = aa , A = A ,discount = discount, global_market_index = global_market_index)
    log_trade_list = rbind(log_trade_list,dd[[1]])
    log_portfolio_stock_trade = rbind(log_portfolio_stock_trade,dd[[2]])
    each_portfolio_return_rate = rbind(each_portfolio_return_rate, dd[[3]] )
  }

  # 把每隻股票日報酬平均之後合併的df，這樣這張df就會有每天分配過後的漲跌幅，用他算報酬。
  each_portfolio_return_rate$投組累積報酬 = (cumprod( each_portfolio_return_rate$分配後的漲跌幅+1 ) -1) %>% round(digits = 4)
  each_portfolio_return_rate$分配後的漲跌幅 = each_portfolio_return_rate$分配後的漲跌幅 %>% round(digits = 4)
  each_portfolio_return_rate$市場累積報酬 = (cumprod( each_portfolio_return_rate$市場漲跌幅+1 ) -1) %>% round(digits = 4)
  each_portfolio_return_rate$市場漲跌幅 = each_portfolio_return_rate$市場漲跌幅 %>% round(digits = 4)
  
  # 畫DD線 : 下面的最大回洛也是正確的，只是這邊自己算的會比較大的原因是因為，這邊是用歷史高點過去的歷史高點，所以如果有漲幅的話這個不考慮，
  # 但是下面的那個會考慮，應該啦，用人加函數的缺點就是不知道確切來說是怎麼去做計算的
  each_portfolio_return_rate$投組累積報酬指數 = each_portfolio_return_rate$投組累積報酬+1
  each_portfolio_return_rate$cummax = cummax(each_portfolio_return_rate$投組累積報酬指數) 
  each_portfolio_return_rate$dd =  ((each_portfolio_return_rate$投組累積報酬指數)/cummax(each_portfolio_return_rate$投組累積報酬指數))-1
  dd_ratio = min(each_portfolio_return_rate$dd) %>% round(4)
  dd_day = each_portfolio_return_rate[each_portfolio_return_rate$dd == min(each_portfolio_return_rate$dd),]
  dd_start = dd_day$年月日
  dd_end = each_portfolio_return_rate[each_portfolio_return_rate$投組累積報酬指數 == dd_day$cummax,]
  dd_end = dd_end$年月日
  dd_during_period = dd_start-dd_end
  dd_df = data.table(投組最大回落 = dd_ratio , 回落開始日期 =  dd_start ,
                            回落結束日期 = dd_end , 回落持續天數 = dd_during_period )

  
  # 最大回落
  #mdd =  each_portfolio_return_rate$投組累積報酬指數 %>% arrange(年月日) 
  mdd =  each_portfolio_return_rate$投組累積報酬指數 %>% maxdrawdown()
  mdd_ratio = (each_portfolio_return_rate$投組累積報酬指數[mdd$to] - each_portfolio_return_rate$投組累積報酬指數[mdd$from]) / each_portfolio_return_rate$投組累積報酬指數[mdd$from]
  mdd_ratio = round(mdd_ratio,digits = 3)
  mdd_start_day = each_portfolio_return_rate$年月日[mdd$from] #最大回落高點日期
  mdd_end_day = each_portfolio_return_rate$年月日[mdd$to] #最大回落低點日期
  mdd_during_period = (mdd_end_day - mdd_start_day) %>% as.numeric()   #回落時間
  each_portfolio_return_rate$市場累積報酬指數 = each_portfolio_return_rate$市場累積報酬+1
  mdd = maxdrawdown(each_portfolio_return_rate$市場累積報酬指數)
  market_mdd_ratio = (each_portfolio_return_rate$市場累積報酬指數[mdd$to] - each_portfolio_return_rate$市場累積報酬指數[mdd$from]) / each_portfolio_return_rate$市場累積報酬指數[mdd$from] %>% round(digits = 3)
  market_mdd_ratio = round(market_mdd_ratio,digits = 3)
  mdd_df = data.table(投組最大回落 = mdd_ratio , 回落開始日期 =  mdd_start_day ,
                            回落結束日期 = mdd_end_day , 回落持續天數 = mdd_during_period , 市場最大回落 = market_mdd_ratio)
  
  
  
  
  
  # 總報酬
  pf_total_return = each_portfolio_return_rate$投組累積報酬[length(each_portfolio_return_rate$投組累積報酬)]
  market_total_return = each_portfolio_return_rate$市場累積報酬[length(each_portfolio_return_rate$市場累積報酬)]
  
  # 年化報酬
  year = length(each_portfolio_return_rate$投組累積報酬) / 252
  pf_annual_return = ( (1+pf_total_return)^(1/year) -1 ) %>% round(digits = 3)
  market_annual_return = ( (1+market_total_return)^(1/year) -1 ) %>% round(digits = 3)
  return_df = data.table(投組總報酬 = pf_total_return , 投組年化報酬 = pf_annual_return , 
                              市場總報酬 = market_total_return , 市場年化報酬 = market_annual_return)
  
 
  # 勝率
  winning_percentage = data.table(平均勝率 = mean(log_trade_list$勝率)) %>% round(digits = 2)
  
  # 把指標打包起來
  trade_period = data.table(投資開始日期 = start_day , 投資結束日期 = end_day)
  trading_ndays = data.table(交易次數 = unique(log_portfolio_stock_trade$買入時間) %>% length() )
  n_stock = data.table(平均投資檔數 = mean(log_trade_list$投資股票數量))
  log_correct_portfolio_final_report = data.table( trade_period, return_df , mdd_df , 
                                            winning_percentage , trading_ndays , n_stock ) 
  
  # 統計股票出現次數
  stock_appear_count_list = log_portfolio_stock_trade$證券代碼 
  stock_appear_count_list = table(stock_appear_count_list) %>% as.data.table()
  colnames(stock_appear_count_list) = c("證券代碼","出現次數")
  stock_appear_count_list$證券代碼 = stock_appear_count_list$證券代碼 %>% as.numeric()
  log_portfolio_stock_trade = merge(log_portfolio_stock_trade, stock_appear_count_list , by = "證券代碼")
  #cardinal = order(log_portfolio_stock_trade$出現次數, decreasing = T ) 
  #rownames(log_portfolio_stock_trade) = NULL
   
  # 把df打包成list，方便之後return
  list_package = list( log_correct_portfolio_final_report , log_trade_list , log_portfolio_stock_trade )
  
  # 畫圖的部分 thank to 姿雅
  
  return_index_image = ggplot(each_portfolio_return_rate , aes(x = 年月日)) +
    geom_line(aes(y = 投組累積報酬, color = "Portfolio Return")) +
    geom_line(aes(y = 市場累積報酬, color = "Market Return" )) +
    ggtitle("投資組合報酬與市場比較") +
    xlab("投資期間") +
    ylab("投資累積報酬率 %" ) +
    scale_color_manual("", values = c("Portfolio Return" = "blue" , "Market Return" = "red" )) +
    theme(
      legend.position = "bottom"
    )  
  #print( return_index_image )
  
  drawdown_image = ggplot(each_portfolio_return_rate , aes(x = 年月日)) +
     geom_line(aes(y = dd , color = "Drawdown"  )   ) +
     ggtitle("Downside Risk") +
     xlab("投資期間") +
     ylab("Drawdown Rate ")+
     scale_color_manual("", values = c("Drawdown" = "black")) +
     theme(
        legend.position = "bottom"
     )
    # print( drawdown_image )
    # 
   combine_image = plot_grid( return_index_image, drawdown_image ,nrow = 2 , align = "v" , rel_heights = c(2,1))
   print(combine_image)

  gc()
  print(Sys.time()-exe_time )
return(list_package)
}

log_list_package = quant_func(table_data , start_day = 20130101 , end_day = 20220807 , trade_frequency = "season")
log_final_report = log_list_package[[1]]
log_trade_list = log_list_package[[2]]
log_portfolio_stock_trade = log_list_package[[3]]


#     #
# densityplot(IFRS$z_負債比率)
