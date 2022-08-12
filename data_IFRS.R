gc()
source("func_caculate_index.R", encoding = "utf-8")
setwd("C:/Users/Neil/Documents/git-repos/backtest_in_R/stock_data")
library(data.table)
library(plyr)
library(tidyverse)
library(lattice) # 畫機率密度函數使用
library(TTR)


"
其他相關指標說明請參閱因子投資相關文獻
GPOA = 營業毛利/資產總額
ROA－綜合損益
ROE(A)－稅後
營業毛利率
CFOA = 自由現金流量(D)/資產總額
ACC = 折舊－CFO - (流動資產-流動負債) / 資產總額
淨利 = 常續性稅後淨利
"
#財務報表資料
IFRS = fread("IFRS20220810.txt" , sep = "," )
IFRS$證券代碼 = str_trim( IFRS$證券代碼 , side='both')
IFRS = separate(IFRS, 證券代碼 , c("證券代碼","公司名稱")," ")
IFRS$證券代碼 = IFRS$證券代碼 %>% as.numeric()
IFRS$`單季(Q)/單半年(H)` = ifelse(IFRS$`單季(Q)/單半年(H)` == "Q" , 1 ,0)
IFRS = IFRS[IFRS$`單季(Q)/單半年(H)`==1,]
IFRS = IFRS %>% plyr::rename(c( "TSE 產業別"="TSE產業別"))

# 排除股票
IFRS$TSE產業別 = IFRS$TSE產業別 %>% as.numeric()
IFRS$TSE產業別 = ifelse(is.na(IFRS$TSE產業別)==T , 0 , IFRS$TSE產業別 )
IFRS = IFRS %>% filter(TSE產業別 != 91 & TSE產業別 != 14 & TSE產業別 != 17 ) #排除DR股(91) 建築(14) 金融(17)

#####獲利因子#####
profit = IFRS[,1:9]  
profit$資產總額 = IFRS$資產總額 %>% as.numeric()
profit$權益總額 = IFRS$股東權益總額 %>% as.numeric()
profit$營業毛利 = IFRS$營業毛利 %>% as.numeric()
profit$淨利 = IFRS$常續性稅後淨利 %>% as.numeric()

profit$GPOA = ( profit$營業毛利/profit$資產總額 ) #%>% round(digits = 4) 
profit$營業毛利率 = IFRS$營業毛利率 %>% as.numeric()
#profit$ROA = IFRS$`ROA(A)稅後息前` %>% as.numeric()
#profit$ROE = IFRS$`ROE(A)－稅後` %>% as.numeric()
#用常續性淨利自己算缺失值比較少
profit$ROA = profit$淨利/profit$資產總額
profit$ROE = profit$淨利/profit$權益總額  

profit$自由現金流量 = IFRS$`自由現金流量(D)` %>% as.numeric()
profit$CFOA = profit$自由現金流量 / profit$資產總額
profit$折舊 = IFRS$`折舊－CFO` %>% as.numeric()
profit$流動資產 = IFRS$流動資產 %>% as.numeric()
profit$流動負債 = IFRS$流動負債 %>% as.numeric()
profit$ACC應計項目 = (profit$折舊 - (profit$流動資產-profit$流動負債)) / profit$資產總額

#####成長因子#####
# 應該是正確的V
# profit = profit %>% filter(年月>201501)
# profit = profit %>% filter(證券代碼 == 3045 | 證券代碼 == 2412)
profit = profit %>% arrange(證券代碼,季別) %>% group_by(證券代碼,季別) %>% mutate(change_GPOA = (GPOA/lag(GPOA,1)) -1)
profit = profit %>% arrange(證券代碼,季別) %>% group_by(證券代碼,季別) %>% mutate(change_營業毛利率 = (營業毛利率/lag(營業毛利率,1)) -1)
profit = profit %>% arrange(證券代碼,季別) %>% group_by(證券代碼,季別) %>% mutate(change_ROA = (  (ROA/lag(ROA,1)) -1 ) ) 
profit = profit %>% arrange(證券代碼,季別) %>% group_by(證券代碼,季別) %>% mutate(change_ROE = (ROE/lag(ROE,1)) -1)
profit = profit %>% arrange(證券代碼,季別) %>% group_by(證券代碼,季別) %>% mutate(change_CFOA = (CFOA/lag(CFOA,1)) -1)
profit = profit %>% arrange(證券代碼,季別) %>% group_by(證券代碼,季別) %>% mutate(change_ACC應計項目 = (ACC應計項目/lag(ACC應計項目,1)) -1) %>% ungroup()

#移除極端值
profit = profit[is.infinite(profit$change_GPOA) == F,] #移除極端值
profit$change_GPOA = ifelse(is.nan(profit$change_GPOA) == T , 0 , profit$change_GPOA ) #把0/0的轉乘0 上面的要先用

profit = profit[is.infinite(profit$change_ROE) == F,] #移除極端值
profit$change_GPOA = ifelse(is.nan(profit$change_ROE) == T , 0 , profit$change_ROE ) #把0/0的轉乘0 上面的要先用

profit = profit[is.infinite(profit$change_ROE) == F,] #移除極端值
profit$change_GPOA = ifelse(is.nan(profit$change_ROE) == T , 0 , profit$change_ROA ) #把0/0的轉乘0 上面的要先用

profit = profit[is.infinite(profit$change_CFOA) == F,] #移除極端值
profit$change_GPOA = ifelse(is.nan(profit$change_CFOA) == T , 0 , profit$change_CFOA ) #把0/0的轉乘0 上面的要先用

profit = profit[is.infinite(profit$change_ACC應計項目) == F,] #移除極端值
profit$change_GPOA = ifelse(is.nan(profit$ACC應計項目) == T , 0 , profit$change_ACC應計項目 ) #把0/0的轉乘0 上面的要先用

#profit = profit %>% na.omit()

#####安全因子#####
#設計近四季ROE
ROE = profit[,c("證券代碼","公司名稱","TSE產業別","上市別","年月","季別","ROE")]
ROE = delete_less_than_ndays_func(ROE, ndays = 4)
ROE = ROE %>% na.omit()

ROE = ddply( ROE, c("證券代碼") , 
            .fun= function(x){
            transform(x, 
                        MA_ROE = SMA(ROE , n = 4),
                        sigma_ROE = runSD(ROE , n = 4)
            )
            } )


#近四季ROA
ROA = profit[,c("證券代碼","公司名稱","TSE產業別","上市別","年月","季別","ROA")]
ROA = delete_less_than_ndays_func(ROA, ndays =  4)
ROA = ROA %>% na.omit()
# ROA = ROA %>% arrange(證券代碼,年月) %>% group_by(證券代碼) %>% mutate( MA_ROA = SMA(ROA , n = 4) )
# ROA = ROA %>% arrange(證券代碼,年月) %>% group_by(證券代碼) %>% mutate( sigma_ROA = runSD(ROA , n = 4) )
ROA = ddply( ROA, c("證券代碼") , 
             .fun= function(x){
               transform(x, 
                           MA_ROA = SMA(ROA , n = 4),
                           sigma_ROA = runSD(ROA , n = 4)
               )
             } )

#####負債比率#####
Liabilities = IFRS[,1:9]
Liabilities$負債總額 = IFRS$負債總額 %>% as.numeric()
Liabilities$資產總額 = IFRS$資產總額 %>% as.numeric()
Liabilities$負債比率 = (Liabilities$負債總額 / Liabilities$資產總額)
Liabilities = Liabilities %>% na.omit()
#####合併安全因子#####
safety = merge(Liabilities,ROE, by = c("證券代碼","公司名稱","TSE產業別","上市別","年月","季別") )
safety = merge(safety,ROA, by = c("證券代碼","公司名稱","TSE產業別","上市別","年月","季別") ) 

#####股利因子 學姊沒用哈哈#####
# dividend = IFRS$股利支付率

#####合併所有財務指標#####
fin_index = merge(profit,safety,by = c("證券代碼","公司名稱","TSE產業別","上市別","年月","季別","合併(Y/N)","單季(Q)/單半年(H)","月份","ROE","ROA","資產總額") ) %>% na.omit()

#####把指標規模化#####
z_score_fin_index = fin_index
z_score_fin_index = ddply( z_score_fin_index, c("年月") , 
                          .fun= function(x){
                            transform(x,
                                      #獲利因子
                                      z_ROA = scale(ROA),
                                      z_ROE = scale(ROE),
                                      z_GPOA = scale(GPOA),
                                      z_營業毛利率 = scale(營業毛利率),
                                      z_CFOA = scale(CFOA),
                                      z_ACC = scale(ACC應計項目),
                                      #成長因子
                                      z_change_GPOA =  scale(change_GPOA),
                                      z_change_ROE =  scale(change_ROE),
                                      z_change_ROA =  scale(change_ROA),
                                      z_change_營業毛利率 =  scale(change_營業毛利率),
                                      z_change_CFOA =  scale(change_CFOA),
                                      z_change_ACC應計項目 =  scale(change_ACC應計項目),
                                      #安全因子:越高越不好
                                      z_sigma_ROE = scale(sigma_ROE),
                                      z_sigma_ROA = scale(sigma_ROA),
                                      z_負債比率 = scale(負債比率)
                                      )
                          } )

#####算分數#####
attach(z_score_fin_index)
z_score_fin_index$score_profit = ( z_GPOA + z_ROE + z_ROA + z_CFOA + z_營業毛利率 - z_ACC ) / 6
z_score_fin_index$score_growth = ( z_change_GPOA + z_change_ROE + z_change_ROA + z_change_CFOA + z_change_營業毛利率 + z_change_ACC應計項目 ) / 6
z_score_fin_index$score_safety = -( z_負債比率 + z_sigma_ROE + z_sigma_ROA ) / 3


#z_score_fin_index_fin_index = z_score_fin_index_fin_index[z_score_fin_index_fin_index$年月 == 201306,]
#densityplot(z_score_fin_index$score_safety)


#####季別調整 : 設計時間標籤#####
# 目的:避免偷看，在買賣日期的時後檢查是否會買到該股票

z_score_fin_index$年 = substr(z_score_fin_index$年月 , 1 , 4) #提取年月前四個字
z_score_fin_index$年 = z_score_fin_index$年 %>% as.numeric()
z_score_fin_index$時間標籤 = ifelse(z_score_fin_index$季別 == 1 , z_score_fin_index$年*10000+0515 , NA)
z_score_fin_index$時間標籤 = ifelse(z_score_fin_index$季別 == 2 , z_score_fin_index$年*10000+0814 , z_score_fin_index$時間標籤)
z_score_fin_index$時間標籤 = ifelse(z_score_fin_index$季別 == 3 , z_score_fin_index$年*10000+1114 , z_score_fin_index$時間標籤)
z_score_fin_index$時間標籤 = ifelse(z_score_fin_index$季別 == 4 , (z_score_fin_index$年+1)*10000+0331 , z_score_fin_index$時間標籤)


#####儲存表格#####
IFRSfilename = paste("C:/Users/Neil/Documents/git-repos/backtest_in_R/stock_data/tidy_z_score_fin_index_data",max(z_score_fin_index$時間標籤)%>% as.character(),".txt",sep="")
write.table(z_score_fin_index, IFRSfilename , row.names = FALSE , sep = ",")

gc()
