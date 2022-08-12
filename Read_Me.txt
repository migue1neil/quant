Hi, my name is Neil, and this is my backtesting program for Taiwan stock trading made in R

The function of this program can be used to back-test the historical performance of Taiwan stocks and the design of the stop loss point according to your own ideas. Input according to the screening method you want. The current default is the quality factor of factor investment.

It will run out three major reports plus a picture, the report is as follows

1. log_final_report, the investment summary table, tells you how the performance over the years and the performance of the relevant risk indicators

2. log_trade_list, the total investment table of individual stocks, tells you which companies you bought at these times, how the performance of each transaction is, and whether you have reached the stop loss point and exited the market.

3. log_portfolio_stock_trade, the investment summary table of each period, tells you the performance of each entry and exit in these years, you can see how the winning rate of the recent entry and exit is, plus or minus evaluation

4.daily_portfolio_change, the change of the investment group’s daily trading remuneration, this part will not be used at present, if you need to run academic regression analysis or remuneration distribution, it will be used.
But if you just want to buy stocks then this table is useless.

The program mainly uses the quant_mod file to complete all the work. The current functions of each program will be introduced below.

The program is mainly divided into two parts: data and func. Data is mainly used for data sorting and indicator design of the report.
The part of func is mainly the function of the program, such as determining the trading time and stop loss function, etc.

This is the first time I have written a program, and many places are very simple, such as variable naming methods, different grammar styles, etc. It will be modified as much as possible during maintenance in the future. Please bear with me.


嗨嗨，我是Neil，這是我利用R製作的台股交易回測程式

此程式的功能可以根據自己的想法去回測台股的歷史績效如何，以及停損點的設計，根據自己要的篩選方式輸入，目前預設是因子投資的品質因子

會跑出三大報表加一張圖，報表如下所述

1.log_final_report,投資總表，告訴你這些年來的績效如何以及相關風險指標的表現

2.log_trade_list,個股投資總表，告訴你這些時間分別買進了那些公司，每次交易的績效如何，是否有達到停損點出場

3.log_portfolio_stock_trade,每期投資總表，告訴你這些年每次進出的時間點的績效，可以看一下最近幾次進出的勝率怎麼樣，再加減評估

4.daily_portfolio_change,投組每天交易的報酬變化，這部分目前不會用到之後如果需要跑學術上的迴歸分析或是報酬分布才會使用到，
但如果只是想要買股票那這張表就沒有用處了。

程式主要是利用quant_mod這個檔案去完成所有的工作，下面會介紹各個程式目前的功能

程式主要分為data和func兩個部分，data主要是再做資料整理以及該報表的的指標設計,
func的部分主要是程式的功能，像是決定買賣時間以及停損功能等等。

這是小的第一次寫程式，很多地方非常簡陋，像是變數命名方式不統一，語法風格不一等等，未來在維護的時候會盡量修改，還請讀者多多包涵。