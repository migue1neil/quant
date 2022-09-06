library(data.table)
library(plyr)
library(tidyverse)
library(TTR)

#判斷行情func
table_data = fread("C:/Users/Neil/Documents/git-repos/backtest_in_R/stock_data/tidy_stock_price_data20080102_20220812.txt", encoding = "unknown" , header = T,sep = ",")
TW_index = table_data

TW_index = TW_index %>% filter(證券代碼 == 50)
TW_index = TW_index[,c(1:12)]
TW_index = TW_index %>% na.omit()
TW_index = ddply( TW_index , c("證券代碼","公司名稱") , 
                    .fun= function(x){ #這裡的x是指根據上面分完組切出來的table_data
                      transform(x, 近60日漲跌幅變異數 = with(x, runVar(open_price_daily_change , n = 60 ))
                      )
                    } )

hedge_index_func = function(vector){
  hedge_index = c()
  for( i in (1:length(vector)) ){
    if(i < 1250){ #小於1250天的直接給他NA
        vol = NA
        hedge_index = c( hedge_index , vol ) #R的append方法
    }
    if(i >= 1250){ #大於1250天的話再來運算
        start = (i-1249) %>% as.numeric()
        quantile = vector[ start : i] #取出近五年的漲跌幅變異數  
        low_quantile = quantile(quantile,c(0.33) , na.rm = T ) %>% as.numeric() #33百分位數
        high_quantile = quantile(quantile,c(0.67) , na.rm = T ) %>% as.numeric() #67百分位數
        if (quantile[length(quantile)] < low_quantile ){
          vol = "小波動,0"  }
        if (quantile[length(quantile)] > low_quantile & quantile[length(quantile)] < high_quantile  ){
          vol = "中波動,1"  }
        if (quantile[length(quantile)] > high_quantile ){
          vol = "大波動,2"  }
        hedge_index = c( hedge_index , vol )
    }
  }
    return(hedge_index)
    }


TW_index = ddply( TW_index , c("證券代碼","公司名稱") , 
                    .fun= function(x){ #這裡的x是指根據上面分完組切出來的table_data
                      transform(x, hedge_index = hedge_index_func(近60日漲跌幅變異數)
                      )
                    } )
TW_index = separate(TW_index, hedge_index , c("hedge_index","hedge_index_code"),",")

#看一下過去期間的波動占比
table(TW_index$hedge_index)
table(TW_index$hedge_index_code)

TW_index = TW_index %>% na.omit()
plot(y = TW_index$hedge_index_code , x = TW_index$年月日  )

cat("現在的波動行情是:",TW_index$hedge_index[nrow(TW_index)])
