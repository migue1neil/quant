setwd("C:/Users/Neil/Documents/git-repos/backtest_in_R/quant") # 設定工作目錄

# package_list = c("data.table","dplyr","plyr","readr","ggplot2","lubridate","tseries","magrittr","foreach","cowplot")
library(data.table)
library(dplyr)
library(plyr)
library(readr) #讀取檔案必備
library(ggplot2) # 畫圖使用
library(lubridate) #轉換日期使用 
library(tseries) #會用到最大回落
library(magrittr) # %>% 水管工人
library(TTR) #我的超人，量化套件，結合dplyr使用可以快速分組做計算
library(cowplot)
# library(stringr)
# library(tidyr)
source("portfolio_function.R" , encoding = "utf-8")
source("caculate_index_function.R" , encoding = "utf-8")

table_data = fread("C:/Users/Neil/Documents/git-repos/backtest_in_R/stock_data/tidy_stock_price_data20000101_20220613.txt", encoding = "unknown" , header = T,sep = ",")
table_data = table_data[年月日 > 20100101,]

quant_func = function(table_data, start_day , end_day , A = 100 , global_market_index = 0050 , discount = 0 , trade_frequency = "season" ){
  exe_time = print(Sys.time())
  # 交易時間篩選
  trade_time_func = function( table_data , start_day = 20130101 , end_day = 20220101 , trade_frequency = "season" ){
  #指交易一次 : 開發中，比較不重要
  once_trade = c()  
  #年報截止日
  year_trade = c(0331)
  #季報截止日
  season_trade = c(0331,0515,0814,1114)
  #月營收截止日
  monthly_trade = seq(from = 110 , to = 1210 , by = 100) #每個月10號
  #設計一個要月交易還是季交易的判斷式
  #trade_frequency
  check_day = 0
  trade_frequency = trade_frequency %>% tolower()
  if(trade_frequency == "month"){
    check_day = 1
    trade_frequency = monthly_trade
  }else if (trade_frequency == "season"){
    trade_frequency = season_trade
  }else if (trade_frequency == "year"){
    trade_frequency = year_trade
  }else{
    print("設定頻率可能有錯，請檢查 ~")
    break
  }
  # 找出指定日期的第一筆時間
  # 筆記 subset 是vector的篩選函數

  # 找出最後end_day前的最後一筆交易日  
  end_day = subset(table_data$年月日,table_data$年月日 <= end_day) %>% max() %>% as.numeric()
  year = c( year(ymd(start_day)) : year(ymd(end_day)) )  #取出年，用Vector c(起始年:結束年)

  #檢查交易日，生成一個要交易的日期，會根據上面上定的條件，產生一筆交易日期
  trade_day = data.frame(年月日 = NA)
  for (i in year){
    for (j in trade_frequency){
      dd = subset(table_data$年月日, table_data$年月日 >= (i*10000+j))[1]
    trade_day = rbind( trade_day,dd )
    }
  }
  trade_day = na.omit(trade_day)
  rownames(trade_day) = NULL
  
  # 生產一個買入日期和賣出日期的df，假設不是持有N天，而是每期都把上期的東西賣掉，重新買入新的。
  trade_pair = data.frame(買 = NA ,賣 = NA)
  for (i in c(1:length(trade_day$年月日))){
  # cat("***************","\n")
  # cat("買",trade_day$年月日[i],"\n")
  # cat("賣",trade_day$年月日[i+1],"\n")
  # cat("***************","\n")
  tmp = data.frame(買 = trade_day$年月日[i])
  tmp$賣 = trade_day$年月日[i+1] #幫tmp生成一個賣
  trade_pair = rbind(trade_pair,tmp)
  }
  # 如果時間提前結束的話最後一筆交易日是空的，所以要拿截止時間點當賣出的資料補齊
  trade_pair$賣[length(trade_pair$賣)] = end_day # 將最後一筆資料放到df，
  trade_pair = trade_pair %>% na.omit()
  rownames(trade_pair) = NULL # reset index
  
  if(check_day == 1){  
    if ( length(which(trade_pair$買 > trade_pair$賣)) > 0 ){  #檢查如果最後一天小於買入日期的話要修正
      trade_pair = trade_pair[-nrow(trade_pair),]
      trade_pair$賣[length(trade_pair$賣)] = end_day
    }
  }
  return(trade_pair)
}
  trade_pair = trade_time_func(table_data , start_day = start_day , end_day = end_day , trade_frequency = trade_frequency )  
  trade_day = trade_pair$買
  
  # 篩出stock list，設計一個for迴圈，把每次交易日要篩選的股票篩選出來，在合併，形成一籃子要交易的股票
  # 篩選的回合，根據上面給出的交易日期，使用迴圈方式，把每期要交易的股票篩選出來
  i = 1 

  standby_stock_list = data.table()
  for(i in   1:length(trade_day)){
    buy_day= trade_day[i] #buy_day會等於那一天的資料
    tmp_a = table_data[table_data$年月日 == buy_day,]  #目的:生產出要買進股票那一天的資料，TEJ的話感覺可以不用改
    tmp_a = tmp_a %>% na.omit()
    #####篩選環節 先用抵著
     #基本過濾
     #cardinal = order(tmp_a$調整收盤價, decreasing = T) # 範例 #排序當天的股票 收盤價最高的 
     tmp_a = tmp_a %>% filter(TSE產業別 > 0)  
     tmp_a = tmp_a %>% filter(證券代碼 > 1000 & 證券代碼< 10000)
     tmp_a = tmp_a %>% filter(tmp_a$Price_MA_20 > 10) #排除雞蛋水餃股
     tmp_a = tmp_a[tmp_a$調整收盤價 > tmp_a$Price_MA_60, ] #季線上 動能
     tmp_a = tmp_a[tmp_a$成交張數 > 300 ,] #流動性
     cardinal = order(tmp_a$CV股價離散程度, decreasing = F) #依照CV做排序，由小排到大
     #財報數據篩選
     tmp_a$rank_ROE = rank(tmp_a$ROE)
     tmp_a$rank_ROA = rank(tmp_a$ROA)
     tmp_a$rank_營業毛利率 = rank(tmp_a$營業毛利率)
     tmp_a$rank_GPOA = rank(tmp_a$GPOA)
     tmp_a$rank_CFOA = rank(tmp_a$CFOA)
     #tmp_a$rank_ACC = rank(tmp_a$ACC應計項目) #這個不知道要怎麼使用先不做計算
     tmp_a$total_rank = tmp_a$rank_ROE + tmp_a$rank_ROA + tmp_a$rank_營業毛利率 + tmp_a$rank_GPOA + tmp_a$rank_CFOA 
    #錯的 tmp_a$total_rank = sum(tmp_a$rank_ROE,tmp_a$rank_ROA, tmp_a$rank_營業毛利率, tmp_a$rank_GPOA,tmp_a$rank_CFOA )
     #cardinal = order(tmp_a$股價年標準差, decreasing = F ) 
     #tmp_a = tmp_a[證券代碼 == 0050 ,]
 
    #####篩選結束
    # 收集篩選出來的股票資料，難的地方在於對於list不熟，只能用df硬轉
    tmp_a = tmp_a[cardinal,]
    rownames(tmp_a) = NULL
    tmp_a = tmp_a[1:30,] #前30隻最大的股票 
    tmp_b = data.table(stock_list = tmp_a$證券代碼) #收集這30隻的證券代碼
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
  
  # 畫DD線
  each_portfolio_return_rate$dd =  ((1+each_portfolio_return_rate$投組累積報酬)/cummax(1+each_portfolio_return_rate$投組累積報酬))-1
  #min(each_portfolio_return_rate$dd)
  #plot(each_portfolio_return_rate$dd)
  
  # 總報酬
  pf_total_return = each_portfolio_return_rate$投組累積報酬[length(each_portfolio_return_rate$投組累積報酬)]
  market_total_return = each_portfolio_return_rate$市場累積報酬[length(each_portfolio_return_rate$市場累積報酬)]
  
  # 年化報酬
  year = length(each_portfolio_return_rate$投組累積報酬) / 252
  pf_annual_return = ( (1+pf_total_return)^(1/year) -1 ) %>% round(digits = 3)
  market_annual_return = ( (1+market_total_return)^(1/year) -1 ) %>% round(digits = 3)
  return_df = data.table(投組總報酬 = pf_total_return , 投組年化報酬 = pf_annual_return , 
                              市場總報酬 = market_total_return , 市場年化報酬 = market_annual_return)
  
  # 最大回落
  each_portfolio_return_rate$投組累積報酬指數 = each_portfolio_return_rate$投組累積報酬+1
  mdd = maxdrawdown(each_portfolio_return_rate$投組累積報酬指數)
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
  
  ##### 這邊是封包指標 之後移除。
  # 計算封包裡的每期報酬率
  # log_trade_list$投組累積報酬 = ( cumprod(1+log_trade_list$期間總報酬) - 1 ) %>% round(digits = 4)
  # log_trade_list$市場累積報酬 = ( cumprod(1+log_trade_list$市場期間總報酬) - 1 ) %>% round(digits = 4)
  # 總結之前算的
  # annual_func = function(each_period_return){ #設計一個算年化報酬的func
  # accumulate_return = cumprod(1+each_period_return)-1
  # total_return = accumulate_return[length(accumulate_return)] #期末總報酬
  # ndays = (max(log_trade_list$結束日期) - min(log_trade_list$開始日期) ) %>% as.numeric()
  # year = ndays/365
  # annual_return = (total_return+1)^(1/year)-1
  # final_report = data.table(總報酬 = total_return , 年化報酬 = annual_return)
  # return(final_report)
  # }
  # return_pf = annual_func(log_trade_list$期間總報酬)
  # return_market = annual_func(log_trade_list$市場期間總報酬)
  # colnames(return_market) = c("市場總報酬","市場年化報酬")
  # n_stock = data.table(平均投資檔數 = mean(log_trade_list$投資股票數量) )
  # winning_percentage = data.table(平均勝率 = mean(log_trade_list$勝率)  )
  # mdd = data.table(投組最大回落 = max(log_trade_list$投組最大回落) ,
  #                   市場最大回落 = max(log_trade_list$市場最大回落) )
  # trading_ndays = data.table(交易次數 = unique(log_portfolio_stock_trade$買入時間) %>% length() )
  # trade_period = data.table(投資開始日期 = start_day , 投資結束日期 = end_day)
  # log_final_report = cbind(trade_period ,return_pf, return_market, n_stock , mdd , winning_percentage ,trading_ndays )
  # list_package = list( log_final_report , log_trade_list , log_portfolio_stock_trade )
  
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
     ggtitle("Downdise Risk") +
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

log_list_package = quant_func(table_data , start_day = 20130101 , end_day = 20220601 , trade_frequency = "month")
log_final_report = log_list_package[[1]]
log_trade_list = log_list_package[[2]]
log_portfolio_stock_trade = log_list_package[[3]]

#施工使用
   #  start_day = 20160101
   #  end_day = 20220601
   #  discount = 0
   #  trade_frequency = "season"
   # A = 100
   #  global_market_index = 0050
   #  
  
