#施工區
# start_day = 20200101
# end_day = 20220601
# stock_list = c(3008,0050,0056,1101,1102,1103,1104,1108,1109,1110,1201,1203,1210,1213,1215,1216,1217,1218,1219,1220,1225,1227,1229,1231,1232,1233,1234)
# table_data = fread("C:/Users/Neil/Documents/git-repos/backtest_in_R/stock_data/tidy_stock_price_data20100104_20220809.txt", encoding = "unknown" , header = T,sep = ",")
# table_data = table_data %>% filter(年月日>= start_day) %>% filter(年月日<= end_day)
# stop_loss_point = -0.25
# table_data =  table_data %>% filter(證券代碼 %in% stock_list )

# 功能:給他一份含有隔日開盤價的資料表，他會計算出每檔股票的回落幅度%~
#     再根據我們設定的Stop_point回報那些股票達到停損，取第一次達到停損點之跌幅當作當次停損損失%


check_stop_loss_point_func = function(daily_stock_price_table , stop_loss_point = stop_loss_point){

  
  daily_stock_price_table = daily_stock_price_table[,c("證券代碼","公司名稱","年月日","隔日開盤價","open_price_daily_change")]
  daily_stock_price_table = ddply( daily_stock_price_table , c("證券代碼","公司名稱") , 
                          .fun= function(x){
                           transform(x, cumprod_return_rate = with(x, cumprod(1+open_price_daily_change))
                                     )
                               } )

  daily_stock_price_table = ddply( daily_stock_price_table , c("證券代碼","公司名稱") , 
                                   .fun= function(x){
                                     transform(x, cummax = with(x, cummax(cumprod_return_rate))
                                               )
                               } )

  daily_stock_price_table$individual_stock_dd = (daily_stock_price_table$cumprod_return_rate - daily_stock_price_table$cummax) / daily_stock_price_table$cummax
  daily_stock_price_table = daily_stock_price_table %>% filter(individual_stock_dd <= stop_loss_point)
  daily_stock_price_table = daily_stock_price_table %>% group_by(證券代碼,公司名稱) %>%　filter(年月日 == min(年月日))

  return(daily_stock_price_table)
}


stop_loss_point_table = check_stop_loss_point_func(portfolio ,stop_loss_point)

