setwd("C:/Users/Neil/Documents/git-repos/backtest_in_R/quant") # 設定工作目錄
# 此檔案的作用式在於說回測系統的控制台，可以在這邊篩選條件以及把算好的報酬與風險指標提出來

# package_list = c("data.table","tidyverse","dplyr","plyr","readr","ggplot2","lubridate","tseries","magrittr","foreach","cowplot","lattice")
library(data.table)
library(plyr)
library(tidyverse)
library(lubridate) #轉換日期使用 
library(tseries) #會用到最大回落
library(magrittr) # %>% 水管工人
library(TTR) # 量化套件，結合dplyr使用可以快速分組做計算
library(cowplot)
library(lattice) # 畫機率密度函數座使用
library(grDevices)

source("portfolio_function.R" , encoding = "utf-8")
source("func_caculate_index.R" , encoding = "utf-8")
source("func_trade_time_choose.R" , encoding = "utf-8")

# 施工使用
  #  start_day = 20160101
  #  end_day = 20171231
  #  discount = 0
  #  invest_nstock = 30
  #  trade_frequency = "month"
  #  A = 100
  # global_market_index = 0050
  # stop_loss_point = -0.10
  # stop_loss_func = F


######載入資料#####
table_data = fread("C:/Users/Neil/Documents/git-repos/backtest_in_R/stock_data/tidy_current_stock_price_data20100104_20220908.txt", encoding = "unknown" , header = T,sep = ",")
table_data = table_data[年月日 > 20100101,]
IFRS = fread("C:/Users/Neil/Documents/git-repos/backtest_in_R/stock_data/tidy_z_score_fin_index_data20220814.txt", encoding = "unknown" , header = T,sep = ",")
month_revenue = fread("C:/Users/Neil/Documents/git-repos/backtest_in_R/stock_data/month_revenue202209.txt",
                      encoding = "unknown" , header = T,sep = ",")
month_revenue = month_revenue[,c(1:12)]

# 目前沒辦法排除證券代碼相同的ETF跟股票例如股票6203跟元大MSCI6203只能手動先排除
table_data = table_data %>% filter(公司名稱 != "元大富櫃50" &
                                   公司名稱 != "元大MSCI台灣" &
                                   公司名稱 != "永豐臺灣加權" &
                                   公司名稱 != "富邦台50")
  
######回測程式#####
quant_func = function(table_data, start_day , end_day , A = 100 , global_market_index = 0050 , discount = 0 , trade_frequency = "season", invest_nstock = 30 ,stop_loss_func = F, stop_loss_point = -0.25){
  exe_time = Sys.time()
  
  ######交易時間篩選######
  trade_pair = trade_time_func(table_data , start_day = start_day , end_day = end_day , trade_frequency = trade_frequency )  
  trade_day = trade_pair$買
  
  # 篩出stock list，設計一個for迴圈，把每次交易日要篩選的股票篩選出來，在合併，形成一籃子要交易的股票
  # 篩選的回合，根據上面給出的交易日期，使用迴圈方式，把每期要交易的股票篩選出來
  #  i = 1 
  # buy_day= trade_day[i]

  ######投資組合篩選######
  standby_stock_list = data.table()
  for(i in   1:length(trade_day)){
    buy_day= trade_day[i] # buy_day會等於那一天的資料
    tmp_a = table_data[table_data$年月日 == buy_day,]  # 目的:生產出要買進股票那一天的資料，TEJ的話感覺可以不用改
    fin_factor = IFRS %>% filter(時間標籤 <= buy_day) %>% group_by(證券代碼) %>% filter(時間標籤 == max(時間標籤)) #小於交易日中選最大的(最近的)
    month_revenue_sheet = month_revenue %>% filter(時間標籤 <= buy_day) %>% group_by(證券代碼) %>% filter(時間標籤 == max(時間標籤))
    tmp_a = tmp_a %>% na.omit()
    
     #基本條件過濾，避免實際上買不到股票
     #cardinal = order(tmp_a$調整收盤價, decreasing = T) # 範例 #排序當天的股票 收盤價最高的 
     tmp_a = tmp_a %>% filter(TSE產業別 > 0) # 排除ETF 有先幫ETF設定成TSE產業別為0
     tmp_a = tmp_a %>% filter(TSE產業別 != 91) # 移除台灣存託憑證股票 DR股 二次上櫃股票
     tmp_a = tmp_a %>% filter(證券代碼 > 1000 & 證券代碼< 10000) # 避免沒排除的ETF以及DR股
     tmp_a = tmp_a %>% filter(tmp_a$Price_MA_20 > 10) #排除雞蛋水餃股
     
     #排除高價股
    # tmp_a = tmp_a %>% filter(tmp_a$Price_MA_20 < 150) 
     tmp_a = tmp_a[tmp_a$調整收盤價 > tmp_a$Price_MA_60, ] #季線上 動能
     tmp_a = tmp_a[tmp_a$成交張數_MA_20 > 300 ,] #流動性
     #cardinal = order(tmp_a$CV股價離散程度, decreasing = F) #依照CV做排序，由小排到大
     #移除股價離善程度z值超過3的股票
     tmp_a = tmp_a %>% filter(abs(z_CV_price) <= 3 )
     month_revenue_sheet = month_revenue_sheet %>% filter(abs(z_月營收YoY) <= 3 )
     #新想到的財報數據篩選 優點: 不用合併財報 缺點:沒有練到功，還是不會最近月份合併
     fin_factor = fin_factor %>% filter(淨利 > 0)
     sum_table = inner_join(tmp_a , fin_factor , by = c("證券代碼","公司名稱","TSE產業別"))
     sum_table = inner_join(sum_table , month_revenue_sheet , by = c("證券代碼","公司名稱","TSE產業別"))
     
     sum_table$total_score = sum_table$z_月營收YoY +
                             (sum_table$score_profit + sum_table$score_growth - 
                             (sum_table$z_sigma_ROE+ sum_table$z_sigma_ROA + sum_table$z_負債比率 + sum_table$z_CV_price)/4  )  #-號是正確的 台灣大z_CV_price在-1.35
                             
     cardinal = order(sum_table$total_score, decreasing = T) #依照排序，由大排到小
     
    #####篩選結束
    # 收集篩選出來的股票資料，難的地方在於對於list不熟，只能用df硬轉
    sum_table = sum_table[cardinal,]
    rownames(sum_table) = NULL
    sum_table = sum_table[1:invest_nstock,] #前30隻最大的股票 
    tmp_b = data.table(stock_list = sum_table$證券代碼) #收集這30隻的證券代碼
    standby_stock_list = cbind(standby_stock_list,tmp_b) #匯出
  }
 
  # 把每期的股票丟到(portfolio_func)回測裡面，
  log_trade_list = data.table()
  log_portfolio_stock_trade = data.table()
  log_portfolio_daily_change = data.table()
  for(i in  1:length(standby_stock_list)  ){
    aa = standby_stock_list[,i , with = FALSE] %>% na.omit() #不知道為什麼要這樣寫才可以提取出來
    aa = aa$stock_list %>% as.vector()
    bb = trade_pair$買[i]
    cc = trade_pair$賣[i]
    dd = portfolio_function(table_data, start_day = bb , end_day = cc , stock_list = aa ,
                            A = A ,discount = discount, global_market_index = global_market_index, stop_loss_func = stop_loss_func , stop_loss_point = stop_loss_point)
    #dd會噴出list(投組日報酬變動,每期績效表現,個股交易紀錄)
    log_portfolio_daily_change = rbind(log_portfolio_daily_change, dd[[1]] ) #合併每日日報酬
    log_trade_list = rbind(log_trade_list,dd[[2]]) #合併每期投組績效表現
    log_portfolio_stock_trade = rbind(log_portfolio_stock_trade,dd[[3]]) #合併每期個股績效表現
  }
  
  #輸入 (投組日報酬變動,每期績效表現,個股交易紀錄 )
  box = multiple_period_portfolio_index_graph_func(log_portfolio_daily_change,log_trade_list,log_portfolio_stock_trade)
  #會丟出list(投組日報酬變動,投組績效總表,每期績效表現,個股交易紀錄,報酬年表)
  print(Sys.time()-exe_time )

  return(box)
 
  
}


for(stop_loss in c( T , F )){
  for(time in c("month","season")){
    compare_return_sheet = data.table()
    compare_daily_change_sheet = data.table()
      for(invest_nstock in c(5,10,20,30)){  
       #invest_nstock = 30#invest_nstock
        sname = "月營收YoY+品質因子_n"
        trade_frequency = time
        stop_loss_func = stop_loss
        stop_loss_name = ifelse(stop_loss_func == T , "_停損" , "_不停損" )
        box = quant_func(table_data , start_day = 20130101 , end_day = max(table_data$年月日)-2 , trade_frequency = trade_frequency , invest_nstock = invest_nstock 
                         ,stop_loss_func = stop_loss_func ,stop_loss_point = -0.1 )
        
        daily_portfolio_change = box[[1]]    #輸出日報酬變動，方便後面學術上要畫分布圖的時候可以使用
        log_final_report = box[[2]]          #輸出績效總表
        log_period_performance = box[[3]]    #輸出每期績效表現
        log_stock_trade = box[[4]]           #輸出每檔個股交易紀錄
        year_return_sheet = box[[5]]
        return_image = box[[6]]
          
        print(log_final_report)
        
        #設計一下輸出表格
        
        filename_quant = paste( sname ,invest_nstock,"_",trade_frequency,stop_loss_name,"_排除極端值",sep="")
        filename_quant_1 = paste(filename_quant,"_每期投組日報酬表",".csv",sep="")
        filename_quant_2 = paste(filename_quant,"_績效總表",".csv",sep="")
        filename_quant_3 = paste(filename_quant,"_每期投組績效表",".csv",sep="")
        filename_quant_4 = paste(filename_quant,"_個股交易績效表",".csv",sep="")
        filename_quant_5 = paste(filename_quant,"_年績效表",".csv",sep="")
        filename_quant_6 = paste(filename_quant,"_報酬圖表",".png",sep="")
        filename_quant
        #存檔
        write.csv(daily_portfolio_change, filename_quant_1 , row.names = FALSE )
        write.table(log_final_report, filename_quant_2 , row.names = FALSE,sep = "," )
        write.table(log_period_performance, filename_quant_3 , row.names = FALSE ,sep = "," )
        write.table(log_stock_trade, filename_quant_4 , row.names = FALSE ,sep = "," )
        write.table(year_return_sheet, filename_quant_5 , row.names = FALSE ,sep = "," )
        
        #test 
        #合併績效總表
        tmp = data.table(策略名稱 =  filename_quant , log_final_report )
        compare_return_sheet = rbind(compare_return_sheet , tmp)
        #合併投組日報酬表
        tmp2 = daily_portfolio_change[,c("投組累積報酬")]
        name = paste(sname,invest_nstock,"投組累積報酬",sep ="")
        colnames(tmp2) = c(name)
        compare_daily_change_sheet = cbind(compare_daily_change_sheet ,tmp2)
      }
        ymd = daily_portfolio_change[,c("年月日")]
        compare_daily_change_sheet = cbind(ymd ,compare_daily_change_sheet)
        write.table(compare_return_sheet,  paste("合併績效表_",sname,"_",trade_frequency,stop_loss_name,".csv" ,sep="") , row.names = FALSE ,sep = "," )
        write.table(compare_daily_change_sheet,  paste("合併投組日報酬變動表_",sname,"_",trade_frequency,stop_loss_name,".csv" ,sep="") , row.names = FALSE ,sep = "," )
      }
}
  
gc()

#

box = quant_func(table_data , start_day = 20150101 , end_day = 20220906 
                 , trade_frequency = "season" , invest_nstock = 10
                ,stop_loss_func = F ,stop_loss_point = -0.1 )

