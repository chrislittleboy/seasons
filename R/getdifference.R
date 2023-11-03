getdifference <- function(df,instance,dif,ndvilag_high,ndvilag_low,tlag_high,tlag_low,prec_high,prec_low) {
seasons_algo <- getseasons(dif,instance, dif,ndvilag_high, ndvilag_low, tlag_high, tlag_low, prec_high, prec_low)
seasons_gpt <- read_csv("/home/chris/Documents/manuscripts/data/processed/gptreference.csv") %>% filter(id == instance)
return(quantify_difference(seasons_algo, seasons_gpt))
}
