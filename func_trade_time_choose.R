
# 交易時間篩選
trade_time_func = function( table_data , start_day = 20130101 , end_day = 20220101 , trade_frequency = "season" ){
  # 指交易一次 : 開發中，比較不重要
  once_trade = c()  
  # 年報截止日
  year_trade = c(0331)
  # 季報截止日
  season_trade = c(0331,0515,0814,1114)
  # 月營收截止日
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