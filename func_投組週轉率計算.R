
trade_pair$買 = ymd(trade_pair$買)
trade_pair$賣 = ymd(trade_pair$賣)

for(i in trade_day){
#print(i)
alist = log_stock_trade %>% filter(買入時間 == i$買)
alist = alist[,c("證券代碼")]
blist = log_stock_trade %>% filter(買入時間 == i$賣)
blist = blist[,c("證券代碼")]
 
c = (alist == blist) %>% data.table()
c = c %>% filter(c == F)
x = nrow(c)/nrow(alist)
print(x)
}
