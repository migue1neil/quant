
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
group_daily_change_function = function( table_data , n = 1){ #設計一個函數，可以分組後往下移n個單位
  shift_data = ddply( table_data , c("證券代碼","公司名稱") , 
                      .fun= function(x){ #這裡的x是指根據上面分完組切出來的table_data
                        transform(x, 前一日價格 = with(x, shift(調整收盤價 , n )))
                      } )
  shift_data = na.omit(shift_data)
  shift_data$daily_change = (shift_data$調整收盤價 - shift_data$前一日價格) / shift_data$前一日價格
  return (shift_data)
}
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
group_cumprod_func = function(table_data){
  table_data$tmp_index = table_data$daily_change + 1 #tmp_index是每日變動+1，不是複利
  cumprod_index = ddply( table_data , c("證券代碼","公司名稱") , 
                         .fun= function(x){
                           transform(x, cumprod_return_rate = with(x, cumprod(tmp_index)))
                         } )
  #cumprod_index$cumprod_index = cumprod_index$cumprod_return_rate #施工用看一下還沒-1前的變化
  cumprod_index$cumprod_return_rate = cumprod_index$cumprod_return_rate - 1 #這樣是變成那個時點的複利，所以到時候只要用投入資金乘上1+這個值就是這個時間點的複利效果
  cumprod_index$cumprod_return_rate = round(cumprod_index$cumprod_return_rate,digit = 3)
  return(cumprod_index)
}
#table_data = group_cumprod_func(table_data)

## 不考慮日期，從資料開頭算複利算到期末，自己寫的 慢很多
daily_cumprod_return_rate_function = function(table_data){ #輸入整理好的股價資料，會噴出單日變動幅度，以及複利報酬
  all_stock_list = unique(table_data$證券代碼)
  cumpord_rate_df = data.table()
  for (i in all_stock_list){ #可以把開始日期以後的單日報酬率以及
    tmp_df = table_data[table_data$證券代碼 == i ,]
    tmp_df$前一日價格 = tmp_df$調整收盤價 %>% shift(1)  
    tmp_df = na.omit(tmp_df)
    tmp_df$單日漲跌幅 = (tmp_df$調整收盤價 - tmp_df$前一日價格) / tmp_df$前一日價格
    tmp_df$tmp_index = tmp_df$單日漲跌幅+1 
    tmp_df$cumpord_return_rate = tmp_df$tmp_index %>% cumprod()
    tmp_df$cumpord_return_rate = round(tmp_df$cumpord_return_rate -1 , digits = 3)
    cumpord_rate_df = rbind(cumpord_rate_df,tmp_df)
    print(i)
}
return(cumpord_rate_df)
}
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
library(TTR)
##### MA平均報酬率
mean_daily_return_func = function(table_data, ndays){
  table_data = delete_less_than_ndays_func(table_data , ndays)
  MA_data = ddply(table_data, c("證券代碼") , 
                  .fun= function(x){
                    transform(x, mean_daily_return = with(x, SMA( daily_change , n = ndays )))
                  } )
  return(MA_data)
}
# table_data = mean_daily_return_func(table_data , ndays = 60)


#table_data = delete_less_than_ndays_func(table_data , 60)


#####
# 假設我要的是交易日前20天的所提諾比率
# [採用]方案一 計算每個時間點前20天的所提諾比率 -> 看起來較為合理，因為時間拉長可以用rolling的方式
# 方案二 計算上個交易日開始的所提諾比率 -> 以所提諾來說可能比較不合理 > 但以財報來說比較合理?
# [採用]方案一 用單日漲跌幅
# 方案二 用複利報酬率
# 計算前20天的索提諾比率
sortino_ratio_func = function(table_data, n = 60){ #n = 幾天的移動所提諾比率
  sortino_ratio_df = table_data
  sortino_ratio_df = group_daily_change_function(sortino_ratio_df) # 幫資料新增單日漲跌幅
  sortino_ratio_df = mean_daily_return_func(sortino_ratio_df,ndays = n )
  all_stock_list = unique(sortino_ratio_df$證券代碼) #求出輸入資料的每檔股票
  all_day_list = unique(sortino_ratio_df$年月日) #求出書日資料的每個日期
  Rf = 0
  sortino_ratio_df$downside_volatility = ifelse(sortino_ratio_df$daily_change > 0 ,0 ,sortino_ratio_df$daily_change)
  # all_stock_list = c(0050,3045) #施工使用
  # all_day_list = unique(table_data$年月日)[1:50]  #施工使用
   n = 20 # 施工使用
  
  downside_combine_list = data.table()
  # downside_sigama_list = data.table() 
  # sortino_rate_list = data.table()
  for (stock_number in all_stock_list){
    for(day in all_day_list){
      tmp_df = sortino_ratio_df  %>% filter(年月日 <= day) %>% filter(證券代碼 == stock_number) 
      if(length(tmp_df$年月日)< n ){next}
      end = length(tmp_df$年月日)
      start = end-n + 1
      tmp_df = tmp_df[start:end,] #提取出日期最後n天
        #downside_volatility = c() 
        # for(i in tmp_df$單日漲跌幅){ #這邊是在處理正報酬，把正報酬換成0，留下負報酬 -> #用單日漲跌幅的方式排除好的單日波動
        #   if (i >0){i = 0}
        #   downside_volatility = rbind(downside_volatility,i)
        # }
    #downside_volatility = downside_volatility %>% round(digits = 4)
    #參數設計 先求時間點上的值，幫她加入時間標籤後合併到df裡
    # ma_mean_return = mean(tmp_df$單日漲跌幅)
    # ma_mean_return = round(ma_mean_return , digits = 4)
    downside_sigama = sd(tmp_df$downside_volatility)
    sortino_ratio = (mean_daily_return - Rf) / downside_sigama
    #加入時間標籤
    downside_combine_point = data.table(證券代碼 = stock_number,年月日 = day , downside_sigama = downside_sigama ,sortino_ratio = sortino_ratio )
    #合併
    downside_combine_list =  rbind(downside_combine_list, downside_combine_point)
    cat("執行標的",stock_number,"執行日期:",day,"\n")
 }
}
  sortino_ratio_df = full_join(sortino_ratio_df, downside_combine_list , by = c("證券代碼","年月日"))
  return(sortino_ratio_df)
}




#table_data = sortino_ratio_func(table_data, n = 120)

# plot(y = table_data[table_data$證券代碼 == 2412,]$sortino_ratio, x = ymd(table_data[table_data$證券代碼 == 2412,]$年月日))
# plot(y = table_data[table_data$證券代碼 == 0050,]$sortino_ratio, x = ymd(table_data[table_data$證券代碼 == 0050,]$年月日))

#年化報酬率練習
# days = 1
# N = days/252
# TR = 0.0008
# IRR = (1+TR)^(1/N)-1
# IRR

### Sortino指標分組計算

# sortino example
# annual_return = c(4, 10, 15, 20, -5, -2, -6, 8, 23, 13)
# annual_return = annual_return/100
# annual_return
# 
# sortino_rate = c()
# for (i in annual_return){
#   if (i > 0){
#     i = 0
#   }
#   sortino_rate = rbind(sortino_rate,i)
# }
# sd(sortino_rate)

# write.csv(table_data,file = "C:/Users/Neil/Documents/sortino_data.csv" , row.names = FALSE)

# rolling mean by TTR

group_price_rolling_sd_func = function( table_data , n ){ #設計一個函數，可以分組後往下移n個單位
  table_data = delete_less_than_ndays_func(table_data, n)
  sd_data = ddply( table_data , c("證券代碼") , 
                      .fun= function(x){ #這裡的x是指根據上面分完組切出來的table_data
                        transform(x, roll_sd = with(x, runSD(調整收盤價, n = n, )) )
                      } )
  return (sd_data)
}
# table_data = group_price_rolling_sd_func(table_data , n = 252 )

group_ma_function = function(table_data , n ){ #設計一個函數，裡面包含移除小於天數的股票以及計算60ma
  table_data = delete_less_than_ndays_func(table_data, n)
  MA_stock_price = ddply( table_data, c("證券代碼","公司名稱") , 
                         .fun= function(x){
                         transform(x, ma_stock_price = with(x, SMA( 調整收盤價 , n = n) ))
                         } )
  return (MA_stock_price)
}
#table_data = group_ma_function(table_data , n = 60)

group_daily_change_rolling_sd_func = function( table_data , n ){ #設計一個函數，可以分組後往下移n個單位
  table_data = delete_less_than_ndays_func(table_data, n)
  sd_data = ddply( table_data , c("證券代碼") , 
                   .fun= function(x){ #這裡的x是指根據上面分完組切出來的table_data
                     transform(x, roll_daily_change_sd = with(x, runSD(open_price_daily_change, n = n, )) )
                   } )
  return (sd_data)
}

# table_data = group_daily_change_rolling_sd_func(table_data , n = 252 )

#算下行波動標準差
#先過濾負正報酬 

# table_data$downside_daily_change = ifelse(table_data$open_price_daily_change >0 , 0 , table_data$open_price_daily_change )
# 
# group_downside_daily_change_rolling_sd_func = function( table_data , n = 252 ){ #設計一個函數，可以分組後往下移n個單位
#   table_data = delete_less_than_ndays_func(table_data, n)
#   sd_data = ddply( table_data , c("證券代碼") , 
#                      .fun= function(x){ #這裡的x是指根據上面分完組切出來的table_data
#                        transform(x, downside_sigama = with(x, runSD(downside_daily_change, n = n )) )
#                      } )
#   return (sd_data)
#   }
# 
# table_data = group_downside_daily_change_rolling_sd_func(table_data)
# table_data = table_data %>% filter(年月日 > 20100101 & 年月日 < 20220601)
# 







