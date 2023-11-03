rmse_season_error <- function(df, city){
  df <- filter(df, city == city)
  sqrt(sum((dfalgoseason - dfgptseason) ^ 2))
}