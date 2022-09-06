# this file was encoding by UTF-8
# this function need data to caculate. The data must contain "證券代碼","公司名稱"."年月日"(date) and "每日的報酬變動"(open_price_daily_change) column
options(scipen=999)

##### 算出單一期的報酬，並且匯出投組每日報酬變動 #####
portfolio_function = function(table_data, start_day , end_day  , stock_list , A  , global_market_index , discount , stop_loss_func , stop_loss_point ){ 
  
  # table_data = 整理好的dataframe 
  # A = 投入的金額 
  # start_day = 開始的日期 範例: 20200101
  # end_day = 結束的日期 範例: 20220501
  # A = 起始投資金額，預設為100
  # global_market_index = 全域市場變數，要比較的標的
  # stop_loss = 是否執行停損功能
  # stop_loss_point = 接受損失幅度,停損點
  
  # 先篩選資料的時間
  table_data$年月日 = ymd(table_data$年月日)
  start_day = ymd(start_day)
  end_day = ymd(end_day)
  table_data = table_data %>% filter(年月日>= start_day) %>% filter(年月日<= end_day)
  table_data = table_data[,c("證券代碼","公司名稱","年月日","隔日開盤價","open_price_daily_change")]
  
  # 設計一個函數，可以計算累積乘積(每天的複利)
  group_cumprod_func = function(table_data){
    cumprod_index = ddply( table_data , c("證券代碼","公司名稱") , 
                           .fun= function(x){
                             transform(x, cumprod_return_rate = with(x, cumprod(open_price_daily_change + 1)))
                           } )
    cumprod_index$cumprod_return_rate = cumprod_index$cumprod_return_rate - 1 #這樣是變成那個時點的複利，所以到時候只要用投入資金乘上1+這個值就是這個時間點的複利效果
    return(cumprod_index)
  }
  table_data = group_cumprod_func(table_data)

  ##### 用上面算完的東西計算指標，從上面的資料中,取出我們要的股票來計算投資報酬率  ##### 
  portfolio_risk_return_func = function(stock_list , discount , tax = 0.003 , stop_loss_func , stop_loss_point ){
    
    n = as.numeric(length(stock_list)) # 投資股票的數量
    w = 1/n  # 分配的比重:假設平均分配，希望每檔股票的貢獻程度相同
    fee = 0.001425*(1-discount)
    trade_cost = ((1-fee)*(1-(fee+tax)))/1
    
    # 先篩出要的股票，再計算指數成長
    portfolio = filter(table_data,證券代碼 %in% stock_list ) # 筆記:多重篩選用filter比較好用
    
    # 停損功能
    if(stop_loss_func == T){  #要先把分配前，達到停損的股票那天之後的每日報酬變動改成0 
      source("func_check_stock_loss.R",encoding = "UTF-8")
      stop_loss_point_table = check_stop_loss_point_func(portfolio ,stop_loss_point)
      
      portfolio = merge(portfolio , stop_loss_point_table , by = c("證券代碼","公司名稱") , all = T) %>%　arrange(證券代碼,年月日)
      
      portfolio$open_price_daily_change = ifelse(portfolio$年月日 > portfolio$停損時間點  & is.na(portfolio$停損時間點) == F , 0, portfolio$open_price_daily_change )
      
      portfolio = ddply( portfolio , c("證券代碼","公司名稱") , 
                             .fun= function(x){
                               transform(x, cumprod_return_rate = with(x, cumprod(open_price_daily_change + 1)))
                             } )
      portfolio$cumprod_return_rate = portfolio$cumprod_return_rate - 1 #?????
    }
          
    ##### 輸出投組的日報酬 : 提出去外面合併之後再做計算 #####
    portfolio$分配後的漲跌幅 = w*trade_cost*(portfolio$open_price_daily_change)
    check_na = which(is.na(portfolio$分配後的漲跌幅) )
    if (length(check_na) > 0 ){
      portfolio$分配後的漲跌幅 = nafill( portfolio$分配後的漲跌幅 ,fill = 0 )
      print(portfolio[check_na,])
      #cat("當期資料有缺失值，或者下市，以損失100%處理，請檢查","\n")
      portfolio$隔日開盤價 = nafill( portfolio$隔日開盤價 ,fill = 0 ) #要注意如果選到最新的一天，那會沒有後面兩天的資料鐵定為0
    }
    period_portfolio_daily_change = portfolio[,c("年月日","分配後的漲跌幅")]
    #將各股的日報酬變動平均起來　－＞　變成投組每天的日報酬驗動
    period_portfolio_daily_change = period_portfolio_daily_change %>% group_by(年月日) %>% summarise_all(sum)
    #扣掉最後一天，因為報酬是用後日減隔日／隔日，所以最後一天不會用到
    period_portfolio_daily_change = period_portfolio_daily_change[-nrow(period_portfolio_daily_change),]
    
    
    
    
    ##### 計算單一期的報酬率，個股報酬表，每期的風險指標，每期的最大回落 #####
    # 目前是拿來當作另類衡量風險的參考指標
    
    ##### 個股報酬表 ##### 
    #個股交易報酬如何
    period_log_stock_trade = portfolio[,c("證券代碼","公司名稱","年月日","隔日開盤價")]
    portfolio_start = period_log_stock_trade[period_log_stock_trade$年月日 == min(period_log_stock_trade$年月日), ]
    portfolio_end = period_log_stock_trade[period_log_stock_trade$年月日 == max(period_log_stock_trade$年月日), ]
    
    period_log_stock_trade = merge(portfolio_start, portfolio_end , by= c("證券代碼","公司名稱") , all = T)
    colnames(period_log_stock_trade)= c("證券代碼","公司名稱","買入時間","買入開盤價","賣出時間","賣出價格")
    period_log_stock_trade$單筆報酬 = ( (period_log_stock_trade$賣出價格 - period_log_stock_trade$買入開盤價) / period_log_stock_trade$買入開盤價 ) %>% round(digits = 4)
    
    #停損設計 : 如果有執行停損的話個股報酬表格會多出停損相關欄位
    if(stop_loss_func == T){
    stop_loss_point_table$執行停損 = "V"
    period_log_stock_trade = merge(period_log_stock_trade,stop_loss_point_table, by = c("證券代碼","公司名稱"), all =T　)
    }
    #每期勝率
    winning_percentage = ifelse(period_log_stock_trade$單筆報酬 > 0 , 1 , 0) %>% mean( na.rm = T ) %>% round(digits = 4)
    
    ##### 單一期績效表 ##### 
    
    # 先篩出要的股票，在計算指數成長
    portfolio$分配後的投資報酬指數 = A*w*trade_cost*(portfolio$cumprod_return_rate+1) #分配後的比重
    
    # 檢查有NA值的項目，print出來跟補0 
    check_na = which(is.na(portfolio$分配後的投資報酬指數) )
    if (length(check_na) > 0 ){
      portfolio$分配後的投資報酬指數 = nafill( portfolio$分配後的投資報酬指數 ,fill = 0 )
      print(portfolio[check_na,])
      cat("當期資料有缺失值，或者下市，以損失100%處理，請檢查","\n")
    }
    portfolio_return_index = portfolio[,c("年月日","分配後的投資報酬指數")]
    portfolio_return_index = portfolio_return_index %>% group_by(年月日) %>% summarise_all(sum)
    colnames(portfolio_return_index)[2] = "投資報酬指數"
    portfolio_return_index = portfolio_return_index[-nrow(portfolio_return_index),] #會多算到一天的報酬，要把它刪掉
    
    # 計算單期投資組合風險指標
    x = portfolio_return_index
    
    # 期末報酬率
    total_return = ((x$投資報酬指數[length(x$投資報酬指數)]- x$投資報酬指數[1])/x$投資報酬指數[1]) #期末報酬率
  
    # # 最大回落
    # mdd = maxdrawdown(x$投資報酬指數)
    # mdd_ratio = (x$投資報酬指數[mdd$to] - x$投資報酬指數[mdd$from]) / x$投資報酬指數[mdd$from]
    # mdd_ratio = round(mdd_ratio,digits = 4)
    # mdd_start_day = x$年月日[mdd$from] #最大回落高點日期
    # mdd_end_day = x$年月日[mdd$to] #最大回落低點日期
    # mdd_during_period = (mdd_end_day - mdd_start_day) %>% as.numeric()   #回落時間
    
    # 畫DD線 : 下面的最大回洛也是正確的，只是這邊自己算的會比較大的原因是因為，這邊是用歷史高點過去的歷史高點，所以如果有漲幅的話這個不考慮，
    x$cummax = cummax(x$投資報酬指數) 
    x$dd =  ((x$投資報酬指數)/cummax(x$投資報酬指數))-1
    dd_ratio = min(x$dd) %>% round(4)
    dd_day = x[x$dd == min(x$dd),] #找出回落最深的dd,那一天就是結束的一天
    dd_end = dd_day$年月日 #最大回落低點日期
    dd_start = x[x$投資報酬指數 == dd_day$cummax,] #最大回落高點日期
    dd_start = min( dd_start$年月日 )
    dd_during_period = (dd_end-dd_start) %>% as.numeric()  #回落時間

    # 整理單一期的績效表
    pf_value = data.table(  期間總報酬 = total_return , 投組最大回落 = dd_ratio , 
                            投組回落開始時間 = dd_start , 投組回落結束時間 = dd_end 
                            ,投組回落持續天數 =  dd_during_period ,勝率 = winning_percentage ,投資股票數量 = n )
    
    # 如果剛好有N個日期最大回落一樣的話，df表格會出現超過一個，紀錄上會對不起來，取回落天數最大的
    #pf_value = pf_value[pf_value$投組回落持續天數 == max(pf_value$投組回落持續天數),]
    
    
    
    ###### 打包成list輸出，輸出:單一期績效表,個股交易績效表,投組日報酬表 ######
    log_package = list( pf_value,  period_log_stock_trade, period_portfolio_daily_change) #打包股票表格還有報酬表格輸出
    return(log_package) 
  }
  
  #portfolio_df的判斷式，輸出投組df
 
  if(length(stock_list) != 0){ # 這邊是預防它丟一個空集合過來(完全選不到股票的情況)
    # discount = discount
    # stop_loss_func = stop_loss_func
    # stop_loss_point = stop_loss_point
    portfolio_df = portfolio_risk_return_func(stock_list, discount = discount, stop_loss_func = stop_loss_func , stop_loss_point = stop_loss_point )
    # 把剛剛輸出的list解開
    period_portfolio_daily_change = portfolio_df[[3]] # 輸出只有日報酬，要拿去外面算的表格
    period_log_stock_trade = portfolio_df[[2]]  # 輸出股票交易紀錄 
    portfolio_df = portfolio_df[[1]] # 輸出單一期間報酬紀錄
    add_trade_time = data.table(開始日期 = start_day , 結束日期 = end_day )
    portfolio_df = cbind( add_trade_time , portfolio_df )
     
  }else{  
    period_portfolio_daily_change = data.table(年月日=NA ,分配後的漲跌幅 = NA)
    portfolio_df = data.table( 開始日期 = start_day , 結束日期 = end_day,
                               期間總報酬 = 0 , 投組最大回落 = 0 , 
                               投組回落開始時間 = ymd(20000101) , 投組回落結束時間 = ymd(20000101)
                              ,投組回落持續天數 =  0 ,勝率 = 0  , 投資股票數量 = 0 )
    period_log_stock_trade = NA
    print("當期stock_list數量為0 ,會生成一個空df")
  } 
  
  market_df = portfolio_risk_return_func( global_market_index , discount = 1 , tax = 0 , stop_loss_func = F , stop_loss_point = 0) #算市場指數 ，假設什麼都不做，就單買0050的話
  market_daily_change = market_df[[3]] # 輸出期間內市場指數日報酬(非複利)的資料
  colnames(market_daily_change) = c("年月日","市場漲跌幅")
  market_df = market_df[[1]] # 提取出市場的報酬資訊(pf_value)
  colnames(market_df) = c("市場期間總報酬","市場最大回落","市場回落開始時間","市場回落結束時間","市場回落持續天數" ,"市場勝率", "市場標的數")
  period_portfolio_performance = cbind(portfolio_df,market_df)
  period_portfolio_daily_change = merge(period_portfolio_daily_change, market_daily_change , by = "年月日") # 將期間內的投組資料還有市場資料合併
  
  # 打包成表格之後輸出
  log_package = list( period_portfolio_daily_change, period_portfolio_performance , period_log_stock_trade )
  return(log_package)  
}

#施工設定投資參數
 #all_stock_list = unique(table_data$證券代碼)
 #stock_list = all_stock_list
 # table_data = fread("C:/Users/Neil/Documents/git-repos/backtest_in_R/stock_data/tidy_stock_price_data20100104_20220809.txt", encoding = "unknown" , header = T,sep = ",")
  #  stop_loss_point = -0.10
  # stop_loss_func = F
     #stock_list = c(3008,1101,1102,1103,1104,1108,1109,1110,1201,1203,1210,1213,1215,1216,1217,1218,1219,1220,1225,1227,1229,1231,1232,1233,1234)
       # start_day = 20220101
       # end_day = 20220331
 #      A = 100
 #       discount = 1
      # tax = 0.003
 #     global_market_index = 0050
 #     stock_list = 0050
 #    aa = standby_stock_list[,4, with =FALSE]  #測試quant_mod first跑出來的資料正不正確
 #   stock_list = aa$stock_list %>% as.vector()
 #    back_test_datapoint = portfolio_function(table_data,start_day = 20130101, end_day = 20220511,stock_list = stock_list ,global_market_index = 0050)
 # 
 # # xx = table_data %>% filter(證券代碼 %in% stock_list) %>% filter(年月日 > 20180301 & 年月日 < 20190501)

##### 算出每期報酬 & 年為單位的報酬 #####
multiple_period_portfolio_index_graph_func = function(log_portfolio_daily_change,log_trade_list,log_portfolio_stock_trade){ #輸入 (投組績效總表,個股交易紀錄,投組日報酬變動 )
  
  #把每隻股票日報酬平均之後合併的df，這樣這張df就會有每天分配過後的漲跌幅，用他算報酬。
  log_portfolio_daily_change$投組累積報酬 = (cumprod( log_portfolio_daily_change$分配後的漲跌幅+1 ) -1) 
  log_portfolio_daily_change$市場累積報酬 = (cumprod( log_portfolio_daily_change$市場漲跌幅+1 ) -1) 
  
  #報酬指數
  log_portfolio_daily_change$投組累積報酬指數 = log_portfolio_daily_change$投組累積報酬+1
  log_portfolio_daily_change$市場累積報酬指數　= log_portfolio_daily_change$市場累積報酬 + 1
  
  # 畫DD線 : 下面的最大回洛也是正確的，只是這邊自己算的會比較大的原因是因為，這邊是用歷史高點過去的歷史高點，所以如果有漲幅的話這個不考慮，
  log_portfolio_daily_change$cummax = cummax(log_portfolio_daily_change$投組累積報酬指數) 
  log_portfolio_daily_change$dd =  ((log_portfolio_daily_change$投組累積報酬指數)/cummax(log_portfolio_daily_change$投組累積報酬指數))-1
  dd_ratio = min(log_portfolio_daily_change$dd) %>% round(4)
  dd_day = log_portfolio_daily_change[log_portfolio_daily_change$dd == min(log_portfolio_daily_change$dd),] #找出回落最深的dd,那一天就是結束的一天
  dd_end = dd_day$年月日
  dd_start = log_portfolio_daily_change[log_portfolio_daily_change$投組累積報酬指數 == dd_day$cummax,]
  dd_start = dd_start$年月日
  dd_during_period = dd_end-dd_start
  
  #市場dd 
  log_portfolio_daily_change$market_dd =  ((log_portfolio_daily_change$市場累積報酬指數)/cummax(log_portfolio_daily_change$市場累積報酬指數))-1
  market_dd_ratio = min(log_portfolio_daily_change$market_dd) %>% round(4)
  dd_df = data.table(投組最大回落 = dd_ratio , 回落開始日期 =  dd_start ,
                     回落結束日期 = dd_end , 回落持續天數 = dd_during_period , 市場最大回落 = market_dd_ratio )
  
  # 總報酬
  pf_total_return = log_portfolio_daily_change$投組累積報酬[length(log_portfolio_daily_change$投組累積報酬)]
  market_total_return = log_portfolio_daily_change$市場累積報酬[length(log_portfolio_daily_change$市場累積報酬)]
  
  # 年化報酬
  year = length(log_portfolio_daily_change$投組累積報酬) / 252
  pf_annual_return = ( (1+pf_total_return)^(1/year) -1 ) %>% round(digits = 3)
  market_annual_return = ( (1+market_total_return)^(1/year) -1 ) %>% round(digits = 3)
  return_df = data.table(投組總報酬 = pf_total_return , 投組年化報酬 = pf_annual_return , 
                         市場總報酬 = market_total_return , 市場年化報酬 = market_annual_return)
  
  # 勝率
  winning_percentage = data.table(平均勝率 = mean(log_trade_list$勝率)) %>% round(digits = 2)
  
  # 統計股票出現次數
  stock_appear_count_list = log_portfolio_stock_trade$證券代碼 
  stock_appear_count_list = table(stock_appear_count_list) %>% as.data.table()
  colnames(stock_appear_count_list) = c("證券代碼","出現次數")
  stock_appear_count_list$證券代碼 = stock_appear_count_list$證券代碼 %>% as.numeric()
  log_portfolio_stock_trade = merge(log_portfolio_stock_trade, stock_appear_count_list , by = "證券代碼")
  
  ##### 年績效指標 #####
  daily_portfolio_change = log_portfolio_daily_change %>% mutate( 年　= (substr(年月日, 1 , 4)  %>% as.numeric()) )　
  daily_portfolio_change = ddply( daily_portfolio_change, c("年") , 
                                  .fun= function(x){
                                    transform(x, 
                                              #投組年化報酬
                                              annual_return　=  (cumprod(1+分配後的漲跌幅)-1 ),
                                              market_annual_return = (cumprod(1+市場漲跌幅)-1 )
                                    )
                                  } )
  
  year_sheet = daily_portfolio_change %>% group_by(年) %>% filter(年月日 == max(年月日) )
  year_sheet = year_sheet[,c("年月日","年","annual_return","market_annual_return")]
  year_sheet = year_sheet %>% mutate( excess_return = ( annual_return - market_annual_return ))
  
  #####年績效指標衡量#####
  #risk_free_rate
  Rf = 0.012    
  #投組年標準差
  annual_return_sigma = sd(year_sheet$annual_return) %>% round(digits = 4)
  # sharpe ratio
  sharpe_ratio = ( ( mean(year_sheet$annual_return) - Rf ) / annual_return_sigma ) %>% round(digits = 3)
  #下檔標準差
  year_sheet$downsideside_change_year = ifelse(year_sheet$annual_return >=0 , 0 , year_sheet$annual_return )
  downside_sigma = (sd(year_sheet$downsideside_change_year)) 
  # sortino ratio
  sortino_ratio =  ((mean(year_sheet$annual_return) - Rf) / downside_sigma ) %>% round(digits = 3)
  sortino_ratio = ifelse(is.infinite(sortino_ratio) == T , NA , sortino_ratio) #如果沒有賠錢的話，downsigma為0，數值會變無限大 
  
  year_sheet = year_sheet[,c("年","annual_return","market_annual_return","excess_return")]
  year_sheet = year_sheet %>% round(digits = 4)
  
  ##### 把所有指標打包起來#####
  start_day = log_portfolio_daily_change$年月日 %>% min()
  end_day = log_portfolio_daily_change$年月日 %>% max()
  trade_period = data.table(投資開始日期 = start_day , 投資結束日期 = end_day)
  trading_ndays = data.table(交易次數 = unique(log_portfolio_stock_trade$買入時間) %>% length() )
  n_stock = data.table(平均投資檔數 = mean(log_trade_list$投資股票數量))
  portfolio_final_report = data.table( trade_period,
                                       return_df ,
                                       dd_df , 
                                       winning_percentage ,
                                       trading_ndays ,
                                       n_stock ,
                                       投組標準差 = annual_return_sigma ,
                                       下檔標準差 = downside_sigma ,
                                       sharpe_ratio = sharpe_ratio ,
                                       sortino_ratio = sortino_ratio

                                                  ) 
  
  
  #####畫圖出來#####
  
  # 畫圖的部分 thank to 姿雅
  
  return_index_image = ggplot(log_portfolio_daily_change , aes(x = 年月日)) +
    geom_line(aes(y = 投組累積報酬*100, color = "Portfolio Return")) +
    geom_line(aes(y = 市場累積報酬*100, color = "Market Return" )) +
    ggtitle("投資組合報酬與市場比較") +
    xlab("投資期間") +
    ylab("投資累積報酬率 %" ) +
    scale_color_manual("", values = c("Portfolio Return" = "blue" , "Market Return" = "red" )) +
    theme(
      legend.position = "bottom"
    )  
  #print( return_index_image )
  
  drawdown_image = ggplot(log_portfolio_daily_change , aes(x = 年月日)) +
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
  
  
  # 把df打包成list，方便之後return
  list_package = list( log_portfolio_daily_change ,portfolio_final_report , log_trade_list , log_portfolio_stock_trade ,year_sheet,combine_image)
  return(list_package)
  
  }


























