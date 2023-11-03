numeric_season_sort <- function(df,cid){

  s <- filter(df, city == cid)
  sgpt <- if(n == 2){
    sgpt <- ifelse(s == "dry", 1,0)
  }
  algo_wet <- s %>% group_by(season) %>% summarise(w = max(prec))
  algo_cold <- s %>% group_by(season) %>% summarise(t = min(tmin))
  
  
  
}