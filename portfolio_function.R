# this file was encoding by UTF-8
# this function need data to caculate. The data must contain "年月日"(date) and "調整收盤價"(stock price) column
table_data = fread("C:/Users/Neil/Documents/git-repos/backtest_in_R/stock_data/tidy_stock_price_data20100104_20220809.txt", encoding = "unknown" , header = T,sep = ",")
stop_loss_point = -0.25

portfolio_function = function(table_data, start_day , end_day  , stock_list , A  ,global_market_index  , discount ){ 
  #table_data = 整理好的dataframe 
  #A = 投入的金額 
  #start_day = 開始的日期 範例: 20200101
  #end_day = 結束的日期 範例: 20220501
  #A = 起始投資金額，預設為100
  #global_market_index = 全域市場變數 
  # print(start_day)
  # print(end_day)
  
  #先篩選資料的時間
  table_data$年月日 = ymd(table_data$年月日)
  start_day = ymd(start_day)
  end_day = ymd(end_day)
  table_data = table_data %>% filter(年月日>= start_day) %>% filter(年月日<= end_day)
  table_data = table_data[,c("證券代碼","公司名稱","年月日","隔日開盤價","open_price_daily_change")]
  ##### 這邊是為了計算投資報酬指數，所做的資料整理  # 感覺daily change可以在一開始的時候就先算好了
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
  # 設計一個函數，可以計算累積乘積(每天的複利)
  group_cumprod_func = function(table_data){
    table_data$tmp_index = table_data$open_price_daily_change + 1 #tmp_index是每日變動+1，不是複利
    cumprod_index = ddply( table_data , c("證券代碼","公司名稱") , 
                           .fun= function(x){
                             transform(x, cumprod_return_rate = with(x, cumprod(tmp_index)))
                           } )
    #cumprod_index$cumprod_index = cumprod_index$cumprod_return_rate #施工用看一下還沒-1前的變化
    cumprod_index$cumprod_return_rate = cumprod_index$cumprod_return_rate - 1 #這樣是變成那個時點的複利，所以到時候只要用投入資金乘上1+這個值就是這個時間點的複利效果
    #cumprod_index$cumprod_return_rate = round(cumprod_index$cumprod_return_rate,digit = 3)
    return(cumprod_index)
  }
  table_data = group_cumprod_func(table_data)

  ##### 接下來要用到上面算完的東西來計算指標，要把上面的資料取出我們要的股票來計算投資報酬率
  portfolio_risk_return_func = function(stock_list , discount , tax = 0.003){
    n = as.numeric(length(stock_list)) #投資股票的數量
    w = 1/n  #分配的比重 #假設平均分配
    fee = 0.001425*(1-discount)
    trade_cost = ((1-fee)*(1-(fee+tax)))/1
    
    # 先篩出要的股票，再計算指數成長
    portfolio = filter(table_data,證券代碼 %in% stock_list ) #多重篩選用filter比較好用
    portfolio$分配後的投資報酬指數 = A*w*trade_cost*(portfolio$cumprod_return_rate+1) #分配後的比重
    
    # 輸出日報酬使用(現役)
    portfolio$分配後的漲跌幅 = w*trade_cost*(portfolio$open_price_daily_change)
    check_na = which(is.na(portfolio$分配後的漲跌幅) )
    if (length(check_na) > 0 ){
      print(portfolio[check_na,])
      portfolio$分配後的漲跌幅 = nafill( portfolio$分配後的漲跌幅 ,fill = 0 )
      cat("當期資料有缺失值，或者下市，以損失100%處理，請檢查","\n")
    }
    portfolio_return_rate = portfolio[,c("年月日","分配後的漲跌幅")]
    portfolio_return_rate = portfolio_return_rate %>% group_by(年月日) %>% summarise_all(sum)
    portfolio_return_rate = portfolio_return_rate[-nrow(portfolio_return_rate),]
    
    #停損應該在這裡加入
    
    
    
    #勝率，想記錄當期買了甚麼
    portfolio_record = portfolio[,c("證券代碼","公司名稱","年月日","隔日開盤價")]
    portfolio_start = portfolio_record[portfolio_record$年月日 == min(portfolio_record$年月日), ]
    portfolio_end = portfolio_record[portfolio_record$年月日 == max(portfolio_record$年月日), ]
    
    portfolio_record = merge(portfolio_start, portfolio_end , by= c("證券代碼","公司名稱") )
    colnames(portfolio_record)= c("證券代碼","公司名稱","買入時間","買入開盤價","賣出時間","賣出價格")
    portfolio_record$單筆報酬 = ( (portfolio_record$賣出價格 - portfolio_record$買入開盤價) / portfolio_record$買入開盤價 ) %>% round(digits = 4)
    winning_percentage = ifelse(portfolio_record$單筆報酬 > 0 , 1 , 0) %>% mean() %>% round(digits = 2)
    
    #####
    # 下面這一段修改之後決定把報酬率匯出拿到外面去算(#只剩下勝率，跟股票紀錄繼續使用)，
    # 目前是拿來當作另類衡量風險的參考指標  
    
    # 先篩出要的股票，在計算指數成長
    portfolio$分配後的投資報酬指數 = A*w*trade_cost*(portfolio$cumprod_return_rate+1) #分配後的比重
    
    # 檢查有NA值的項目，print出來跟補0 (次要項目，可以不用算)
    check_na = which(is.na(portfolio$分配後的投資報酬指數) )
    if (length(check_na) > 0 ){
      print(portfolio[check_na,])
      portfolio$分配後的投資報酬指數 = nafill( portfolio$分配後的投資報酬指數 ,fill = 0 )
      cat("當期資料有缺失值，或者下市，以損失100%處理，請檢查","\n")
    }
    portfolio_return_index = portfolio[,c("年月日","分配後的投資報酬指數")]
    portfolio_return_index = portfolio_return_index %>% group_by(年月日) %>% summarise_all(sum)
    colnames(portfolio_return_index)[2] = "投資報酬指數"
    portfolio_return_index = portfolio_return_index[-nrow(portfolio_return_index),] #會多算到一天的報酬，要把它刪掉
    
    # 計算投資組合風險指標
    x = portfolio_return_index
    
    # 期末報酬率
    total_return = ((x$投資報酬指數[length(x$投資報酬指數)]- x$投資報酬指數[1])/x$投資報酬指數[1]) #期末報酬率
  
    # 最大回落
    mdd = maxdrawdown(x$投資報酬指數)
    mdd_ratio = (x$投資報酬指數[mdd$to] - x$投資報酬指數[mdd$from]) / x$投資報酬指數[mdd$from]
    mdd_ratio = round(mdd_ratio,digits = 4)
    mdd_start_day = x$年月日[mdd$from] #最大回落高點日期
    mdd_end_day = x$年月日[mdd$to] #最大回落低點日期
    mdd_during_period = (mdd_end_day - mdd_start_day) %>% as.numeric()   #回落時間
  
    # 輸出
    pf_value = data.table(  期間總報酬 = total_return , 投組最大回落 = -mdd_ratio , 
                            投組回落開始時間 = mdd_start_day , 投組回落結束時間 = mdd_end_day 
                            ,投組回落持續天數 =  mdd_during_period ,勝率 = winning_percentage ,投資股票數量 = n )
    
    # 如果剛好有N個日期最大回落一樣的話，df表格會出現超過一個，紀錄上會對不起來，取回落天數最大的
    pf_value = pf_value[pf_value$投組回落持續天數 == max(pf_value$投組回落持續天數),]
    
    #####
    
    # 打包成list後輸出
    log_package = list( pf_value,portfolio_record, portfolio_return_rate) #打包股票表格還有報酬表格輸出
    return(log_package) 
  }
  
  if(length(stock_list) != 0){
    discount = discount
    portfolio_df = portfolio_risk_return_func(stock_list, discount = discount)
    # 把剛剛輸出的list解開
    portfolio_return_rate = portfolio_df[[3]] # 輸出只有日報酬，要拿去外面算的表格
    portfolio_record = portfolio_df[[2]]  # 輸出股票交易紀錄 
    portfolio_df = portfolio_df[[1]] # 輸出期間報酬紀錄
    add_trade_time = data.table(開始日期 = start_day , 結束日期 = end_day )
    portfolio_df = cbind( add_trade_time , portfolio_df )
     
  }else{  # 這邊是預防它丟一個空集合過來(完全選不到股票的情況)
    portfolio_return_rate = data.table(年月日=NA ,分配後的漲跌幅 = NA)
    portfolio_df = data.table( 開始日期 = start_day , 結束日期 = end_day,
                               期間總報酬 = 0 , 投組最大回落 = 0 , 
                               投組回落開始時間 = ymd(20000101) , 投組回落結束時間 = ymd(20000101)
                              ,投組回落持續天數 =  0 ,勝率 = 0  , 投資股票數量 = 0 )
    portfolio_record = NA
  } 
  
  market_df = portfolio_risk_return_func( global_market_index , discount = 1 , tax = 0) #算市場指數 ，假設什麼都不做，就單買0050的話
  market_daily_change = market_df[[3]] # 輸出期間內市場指數日報酬(非複利)的資料
  colnames(market_daily_change) = c("年月日","市場漲跌幅")
  market_df = market_df[[1]] # 提取出市場的報酬資訊(pf_value)
  colnames(market_df) = c("市場期間總報酬","市場最大回落","市場回落開始時間","市場回落結束時間","市場回落持續天數" ,"市場勝率", "市場標的數")
  trade_log = cbind(portfolio_df,market_df)
  portfolio_return_rate = merge(portfolio_return_rate, market_daily_change , by = "年月日") # 將期間內的投組資料還有市場資料合併
  
  # 打包成表格之後輸出
  log_package = list( trade_log , portfolio_record , portfolio_return_rate)
  return(log_package)  
}

#施工設定投資參數
#all_stock_list = unique(table_data$證券代碼)
 #stock_list = all_stock_list
   stock_list = c(3008,1101,1102,1103,1104,1108,1109,1110,1201,1203,1210,1213,1215,1216,1217,1218,1219,1220,1225,1227,1229,1231,1232,1233,1234)
     start_day = 20220101
      end_day = 20220331
     A = 100
      discount = 1
      tax = 0.003
 #     global_market_index = 0050
 #     stock_list = 0050
 #    aa = standby_stock_list[,4, with =FALSE]  #測試quant_mod first跑出來的資料正不正確
 #   stock_list = aa$stock_list %>% as.vector()
 #    back_test_datapoint = portfolio_function(table_data,start_day = 20130101, end_day = 20220511,stock_list = stock_list ,global_market_index = 0050)
 # 
 # # xx = table_data %>% filter(證券代碼 %in% stock_list) %>% filter(年月日 > 20180301 & 年月日 < 20190501)

