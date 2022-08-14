library(lattice) # 畫機率密度函數座使用
library(data.table)
library(plyr)
library(tidyverse)
options(scipen = 999) #數值不用科學記號呈現
IFRS = fread("C:/Users/Neil/Documents/git-repos/backtest_in_R/stock_data/tidy_z_score_fin_index_data20220814.txt", encoding = "unknown" , header = T,sep = ",")

IFRS_check = IFRS[,c(1:5,10:53)] #36(z分數開始位置)
IFRS_check = IFRS_check %>% arrange(年月,z_ROA)

densityplot((IFRS_check$z_ROA))
IFRS_check = IFRS_check %>% filter( #獲利因子
                                    abs(z_ROE)< 3 &
                                    abs(z_ROE)< 3 &
                                    abs(z_GPOA)< 3 &
                                    abs(z_營業毛利率)< 3 &
                                    abs(z_CFOA) < 3 &
                                    abs(z_ACC) < 3 &
                                    #成長因子
                                    abs(z_change_GPOA) < 3 &
                                    abs(z_change_營業毛利率) < 3 &
                                    abs(z_change_CFOA) < 3 &
                                    # abs(z_change_ACC應計項目) < 3 & #目前沒用到
                                    #安全因子
                                    abs(z_sigma_ROE) < 3 &
                                    abs(z_sigma_ROA) < 3 &
                                    abs(z_負債比率) < 3 
                                    
                                    ) 
attach(IFRS_check)
IFRS_check$score_profit = ( z_GPOA + z_ROE + z_ROA + z_CFOA + z_營業毛利率 - z_ACC ) / 6
IFRS_check$score_growth = ( z_change_GPOA + z_change_ROE + z_change_ROA + z_change_CFOA + z_change_營業毛利率  ) / 5
IFRS_check$score_safety = ( z_負債比率 + z_sigma_ROE + z_sigma_ROA ) / 3


IFRS_check = IFRS_check %>% mutate(total_score =  score_profit + score_growth - score_safety ) 
rownames(IFRS_check) = NULL

tmp = IFRS_check %>% filter(年月== 201103) %>% arrange(total_score)
densityplot((tmp$z_GPOA))

densityplot((IFRS_check$z_ROA))
arrang

#
table_data = fread("C:/Users/Neil/Documents/git-repos/backtest_in_R/stock_data/tidy_stock_price_data20080102_20220812.txt", encoding = "unknown" , header = T,sep = ",")
table_data = table_data %>% filter(年月日 > 20220101 & 年月日 <= 20220810 ) 
densityplot(table_data$z_CV_price)

final = fread("不排除極端值品質因子績效 季報 不停損_績效總表.txt", encoding = "unknown" , header = T,sep = ",")
