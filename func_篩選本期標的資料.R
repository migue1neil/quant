
#source("C:/Users/Neil/Documents/git-repos/backtest_in_R/quant/caculate_index_function.R" , encoding = "utf-8")
library(data.table)
library(plyr)
library(tidyverse)
library(lubridate) #轉換日期使用 
library(tseries) #會用到最大回落
library(magrittr) # %>% 水管工人
library(TTR) # 量化套件，結合dplyr使用可以快速分組做計算

#載入資料
table_data = fread("C:/Users/Neil/Documents/git-repos/backtest_in_R/stock_data/tidy_current_stock_price_data20210104_20220822.txt", encoding = "unknown" , header = T,sep = ",")
table_data = table_data[年月日 > 20100101,]
IFRS = fread("C:/Users/Neil/Documents/git-repos/backtest_in_R/stock_data/tidy_z_score_fin_index_data20220814.txt", encoding = "unknown" , header = T,sep = ",")
month_revenue = fread("C:/Users/Neil/Documents/git-repos/backtest_in_R/stock_data/month_revenue202208.txt",
                      encoding = "unknown" , header = T,sep = ",")

# 目前沒辦法排除證券代碼相同的ETF跟股票例如股票6203跟元大MSCI6203只能手動先排除
table_data = table_data %>% filter(公司名稱 != "元大富櫃50" &
                                     公司名稱 != "元大MSCI台灣" &
                                     公司名稱 != "永豐臺灣加權" &
                                     公司名稱 != "富邦台50")

selected_stock_price = table_data %>% filter(年月日 == max(table_data$年月日) & is.na(證券代碼) == F 
                                             &  TSE產業別 != 91 & TSE產業別 != 0 
                                             & Price_MA_20 > 10 & 成交張數_MA_20 >　300
                                             & 調整收盤價 > Price_MA_60 &
                                             Price_MA_20 < 150 )

selected_stock_price = selected_stock_price %>% filter(abs(z_CV_price) <= 3 )
month_revenue_sheet = month_revenue %>% filter(abs(z_月營收YoY) <= 3 )

IFRS$時間標籤 = ymd(IFRS$時間標籤)
fin_factor = IFRS %>% filter(時間標籤 <= Sys.Date()) %>% group_by(證券代碼) %>% filter(時間標籤 == max(時間標籤)) #小於交易日中選最大的(最近的)
fin_factor = fin_factor %>% filter(淨利 > 0)

sum_table = inner_join(selected_stock_price , fin_factor , by = c("證券代碼","公司名稱","TSE產業別"))
sum_table = inner_join(sum_table , month_revenue_sheet , by = c("證券代碼","公司名稱","TSE產業別"))

sum_table <- sum_table %>% distinct(證券代碼, .keep_all = TRUE)

sum_table$total_score = sum_table$z_月營收YoY +
  (sum_table$score_profit + sum_table$score_growth - 
     (sum_table$z_sigma_ROE+ sum_table$z_sigma_ROA + sum_table$z_負債比率 + sum_table$z_CV_price)/4  )  
  

cardinal = order(sum_table$total_score, decreasing = T) #依照排序，由大排到小

sum_table = sum_table[is.na(sum_table$季別) == F , ]
sum_table = sum_table[cardinal,]
rownames(sum_table) = NULL

sum_table = sum_table[1:10,] #前30隻最大的股票 


#先存個檔給ㄩㄇ
#sum_table = sum_table[,-c("隔日開盤價","後日開盤價","open_price_daily_change")]
#sum_table=sum_table[,c(1:2,8,16,3:7,17:68)]
#sum_table = sum_table %>% arrange(total_score,TSE產業別,上市別)
filename = paste("waited_to_buy" , max(table_data$年月日) %>% as.character(), ".csv" ,sep="" )
write.csv(sum_table,filename,row.names = F)


?write.csv
wait_to_buy = sum_table[,c("證券代碼","公司名稱","年月日","調整收盤價")]
# my_money = 200000
# # wait_to_buy = 
# equal = my_money/nrow(wait_to_buy)

gc()