#資料處理 : 股價資料表
source("func_caculate_index.R" , encoding = "utf-8")
setwd("C:/Users/Neil/Documents/git-repos/backtest_in_R/stock_data")

library(data.table)
library(plyr)
library(tidyverse)
library(magrittr)
library(TTR)

######股價報表資料######
stock_price_data = fread("stock_price_data20080101_20220813.txt", encoding = "unknown" , header = T,sep = "\t")
table_data = stock_price_data 
#table_data = table_data[table_data$年月日>20210101,]
table_data = table_data %>% plyr::rename(c( "TSE 產業別"="TSE產業別" ,"開盤價(元)"="調整開盤價", "收盤價(元)" = "調整收盤價",
                                     "成交量(千股)" = "成交張數","CAPM_Beta 一年"="CAPM一年beta"))
#切割證券代碼與公司
table_data = separate(table_data, 證券代碼 , c("證券代碼","公司名稱")," ")

table_data$證券代碼 = as.numeric(table_data$證券代碼)
table_data$TSE產業別 = as.numeric(table_data$TSE產業別) 
table_data$調整開盤價 = as.numeric(table_data$調整開盤價)
#table_data$調整最高價 = as.numeric(table_data$調整最高價)
#table_data$調整最低價 = as.numeric(table_data$調整最低價)
table_data$調整收盤價 = as.numeric(table_data$調整收盤價)
table_data$成交張數 = table_data$成交張數 %>% as.numeric()
table_data$CAPM一年beta = table_data$CAPM一年beta %>% as.numeric()
table_data$TSE產業別 =  ifelse(is.na(table_data$TSE產業別) == T , 0 , table_data$TSE產業別)

# table_data = na.omit(table_data) #因為上面把證券代碼數值化，所以證券號碼有英文的會被移除，通常是ETF 例如0050反一
table_data = table_data[order(table_data$證券代碼,table_data$年月日),]
rownames(table_data) = NULL
table_data = table_data %>% na.omit()

######生成一個隔日開盤價的表格######
group_shift_function = function( table_data , n = -1){ #設計一個函數，可以分組後往下移n個單位
  shift_data = ddply( table_data , c("證券代碼","公司名稱") , 
                      .fun= function(x){ #這裡的x是指根據上面分完組切出來的table_data
                        transform(x, 隔日開盤價 = with(x, shift(調整開盤價 , n )))
                      } )
  return (shift_data)
}
table_data = group_shift_function(table_data)

group_shift_function = function( table_data , n = -1){ #設計一個函數，可以分組後往下移n個單位
  shift_data = ddply( table_data , c("證券代碼","公司名稱") , 
                      .fun= function(x){ #這裡的x是指根據上面分完組切出來的table_data
                        transform(x, 後日開盤價 = with(x, shift(隔日開盤價 , n )))
                      } )
  return (shift_data)
}
table_data = group_shift_function(table_data)
table_data$open_price_daily_change = (table_data$後日開盤價 - table_data$隔日開盤價) / (table_data$隔日開盤價)

######設計均線以及其他指標######
table_data = delete_less_than_ndays_func(table_data, ndays = 252)
table_data = ddply( table_data , c("證券代碼","公司名稱") , 
                 .fun= function(x){ #這裡的x是指根據上面分完組切出來的table_data
                   transform(x, 股價年標準差 = with(x, runSD(調整收盤價, n = 252))
                              , Price_MA_20  = with(x,  SMA(調整收盤價, n = 20 ))
                              , Price_MA_60  = with(x,  SMA(調整收盤價, n = 60 ))
                              , Price_MA_120 = with(x,  SMA(調整收盤價, n = 120 ))
                              , Price_MA_252 = with(x,  SMA(調整收盤價, n = 252 ))
                              , 成交張數_MA_20 = with(x,  SMA(成交張數, n = 20 ))
                             )
                 } )

######設計CV 股價離散程度######
table_data$CV股價離散程度 = table_data$股價年標準差/ table_data$Price_MA_252

table_data = ddply( table_data , c("年月日") , 
                    .fun= function(x){ 
                     transform(x, z_CV_price = with(x, scale(CV股價離散程度) )
                      )
                    } )

#table_data = table_data[,c(1:2,4:10,3,11:18)]

######存檔######
stock_filename = paste("C:/Users/Neil/Documents/git-repos/backtest_in_R/stock_data/tidy_stock_price_data"
                       ,min(table_data$年月日)%>% as.character(),"_",max(table_data$年月日)%>% as.character(),".txt",sep="" )
print(stock_filename)
write.table(table_data,stock_filename , row.names = FALSE , sep = ",")

gc()



