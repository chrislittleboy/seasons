# puts name to factor variables based on climate info
nameseasons <- function(vars){

naming <- data.frame(vars) %>% 
  group_by(fixseasons) %>%
  mutate(tmins = min(tmid),
         pmaxs = max(prec),
         tmaxs = max(tmid),
         tlag_maxs = max(tmid_lag),
         tlag_mins = min(tmid_lag))

ls <- length(unique(naming$fixseasons))

if(ls == 2){
  naming$fixseasons[naming$pmax == max(naming$pmax)] <- "wet"
  naming$fixseasons[naming$v != 6] <- "dry"
}

if(ls == 4){

  naming$fixseasons[naming$tmins == min(naming$tmins)] <- "winter"
  naming$fixseasons[naming$tmaxs == max(naming$tmaxs)] <- "summer"
  naming$fixseasons[naming$tlag_maxs == max(naming$tlag_maxs)] <- "spring"
  naming$fixseasons[naming$tlag_mins == min(naming$tlag_mins)] <- "autumn"
}

return(naming$fixseasons)

}
