
source("C:/Users/Neil/Documents/git-repos/backtest_in_R/quant/caculate_index_function.R" , encoding = "utf-8")
#載入資料
table_data = fread("C:/Users/Neil/Documents/git-repos/backtest_in_R/stock_data/tidy_stock_price_data20100104_20220809.txt", encoding = "unknown" , header = T,sep = ",")
table_data = table_data[年月日 > 20100101,]
IFRS = fread("C:/Users/Neil/Documents/git-repos/backtest_in_R/stock_data/tidy_z_score_fin_index_data20220814.txt", encoding = "unknown" , header = T,sep = ",")

# # colnames(table_data) = c("證券代碼","TSE產業別","年月日","調整開盤價","調整收盤價","成交張數","capm_beta_52w") 
# table_data$證券代碼 = str_trim( table_data$證券代碼 , side='both')
# table_data = separate(table_data, 證券代碼 , c("證券代碼","公司名稱")," ")
# table_data$證券代碼 = as.numeric(table_data$證券代碼)
# table_data$調整開盤價 = as.numeric(table_data$調整開盤價)
# table_data$調整收盤價 = as.numeric(table_data$調整收盤價)
# table_data$TSE產業別 = as.numeric(table_data$TSE產業別) 
# table_data$成交張數 = table_data$成交張數 %>% as.numeric()
# table_data$TSE產業別 =  ifelse(is.na(table_data$TSE產業別) == T , 0 , table_data$TSE產業別)
# table_data = table_data %>% na.omit()
# 
# 
# #[已修正,IFRS未修正]下面是錯的 IFRS應該也是錯的要檢查
# #table_data = table_data %>%　filter(證券代碼 == 2412 | 證券代碼 == 3045 & 年月日>20180101)
# table_data = group_ma_function(table_data , n = 20)
# table_data = table_data %>% rename(c("ma_stock_price"="MA20_price"))
# table_data = group_ma_function(table_data , n = 60)
# table_data = table_data %>% rename(c("ma_stock_price"="MA60_price"))
# table_data = group_ma_function(table_data , n = 252)
# table_data = table_data %>% rename(c("ma_stock_price"="MA252_price"))
# table_data = group_daily_change_rolling_sd_func2(table_data , n =60)
# table_data = table_data %>% rename(c("roll_sd"="roll_sd_price_60day"))
# # table_data = table_data %>% arrange(證券代碼,年月日) %>% mutate( CV_price = roll_sd_price_60day/MA60_price )

selected_stock_price = table_data %>% filter(年月日 == max(table_data$年月日) & is.na(證券代碼) == F 
                                             &  TSE產業別 != 91 & TSE產業別 != 0 
                                             & Price_MA_20 > 10 & 成交張數 >　300
                                             & 調整收盤價 > Price_MA_60 )

IFRS$時間標籤 = ymd(IFRS$時間標籤)
fin_factor = IFRS %>% filter(時間標籤 <= Sys.Date()) %>% group_by(證券代碼) %>% filter(時間標籤 == max(時間標籤)) #小於交易日中選最大的(最近的)
fin_factor = fin_factor %>% filter(淨利 > 0)


sum_table = inner_join(selected_stock_price , fin_factor , by = c("證券代碼","公司名稱","TSE產業別"))

sum_table$total_score = (sum_table$score_profit + sum_table$score_growth - 
                           (sum_table$z_sigma_ROE+ sum_table$z_sigma_ROA + sum_table$sigma_ROE + sum_table$z_CV_price)/4  )  #-號是正確的 台灣大z_CV_price在-1.35

cardinal = order(sum_table$total_score, decreasing = T) #依照排序，由大排到小


sum_table = sum_table[is.na(sum_table$季別) == F , ]
sum_table = sum_table[cardinal,]
rownames(sum_table) = NULL
sum_table = sum_table[1:30,] #前30隻最大的股票 


#先存個檔給ㄩㄇ
sum_table = sum_table[,-c("隔日開盤價","後日開盤價","open_price_daily_change")]
sum_table=sum_table[,c(1:2,8,16,3:7,17:68)]
sum_table = sum_table %>% arrange(total_score,TSE產業別,上市別)
filename = paste("waited_to_buy" , max(table_data$年月日) %>% as.character(), ".csv" ,sep="" )
write.csv(sum_table,filename,row.names = F)


?write.csv
wait_to_buy = sum_table[,c("證券代碼","公司名稱","年月日","調整收盤價")]
# my_money = 200000
# # wait_to_buy = 
# equal = my_money/nrow(wait_to_buy)


gc()