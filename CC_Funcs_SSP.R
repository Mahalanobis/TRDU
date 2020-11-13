###### Single Sell Profit ######

# https://www.baeldung.com/cs/maximum-single-sell-profit
# https://stackoverflow.com/questions/7086464/maximum-single-sell-profit/16883860#16883860

algo_SingleSellProfit <- function( o , h , l ){

  IndexOfLowest = 1
  Lowest = o
  Answer = h[1] - o
  BuyIndex = 1
  SellIndex = 1

  for( i in 2:length(h) ){
    
    if( l[i] <= Lowest ){
      
      IndexOfLowest = i
      Lowest = l[i]
      
    }
    
    if( Answer <= h[i] - Lowest ){
      
      Answer = h[i] - Lowest
      BuyIndex = IndexOfLowest
      SellIndex = i
      
    }
    
  }
  
  return( list(BuyIndex = BuyIndex, SellIndex = SellIndex, Profit = h[SellIndex]/l[BuyIndex]-1 ) )
  
}
