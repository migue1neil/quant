#主要是將總表轉換成以年為單位，把一年生成一個指標
library(data.table)
library(plyr)
library(tidyverse)
daily_portfolio_change = fread("排除極端值品質因子績效_季報_不停損_每期投組日報酬表.txt", encoding = "unknown" , header = T,sep = ",")
compare_group = fread("排除極端值品質因子績效_季報_不停損_績效總表.txt", encoding = "unknown" , header = T,sep = ",")


daily_portfolio_change = daily_portfolio_change %>% mutate( 年　= (substr(年月日, 1 , 4)  %>% as.numeric()) )　
daily_portfolio_change = ddply( daily_portfolio_change, c("年") , 
                                .fun= function(x){
                                  transform(x, 
                                            #投組年化報酬
                                            annual_return　=  (cumprod(1+分配後的漲跌幅)-1 )
                                            )
                                } )

year_sheet = daily_portfolio_change %>% group_by(年) %>% filter(年月日 == max(年月日) )
year_sheet = year_sheet[,c("年月日","年","annual_return")]

#####年績效指標衡量#####

#risk_free_rate
Rf = 0.012    

#投組年標準差
annual_return_sigma = sd(year_sheet$annual_return) %>% round(digits = 4)

# sharpe ratio
sharpe_ratio = ( ( mean(year_sheet$annual_return) - RF ) / annual_return_sigma ) %>% round(digits = 3)

#下檔標準差
year_sheet$downsideside_change_year = ifelse(year_sheet$annual_return >=0 , 0 , year_sheet$annual_return )
downside_sigma = (sd(year_sheet$downsideside_change)) 

# sortino ratio
sortino_ratio =  (mean(year_sheet$annual_return) - Rf) / downside_sigma %>% round(digits = 3)


# 
# year_sheet_filename = "年報表.txt"
# 
# write.table(year_sheet, year_sheet_filename , row.names = FALSE , sep = ",")
# 
# 



