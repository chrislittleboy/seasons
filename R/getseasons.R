getseasons <- function(df,
                       dif,
                       ndvilag_high, ndvilag_low, 
                       tlag_high, tlag_low, 
                       temp_low, temp_high, 
                       prec_high, prec_low){

df <- df %>%
    mutate(tran = tmax - tmin) %>%
    mutate(tmid = tmin + (tran/2)) %>%
    mutate(tmid_lag = tmid - lag(tmid, n = 1),
           ndvi_lag = ndvi - lag(ndvi, n = 1))
df[1,10] <- df[1,9] - df[12,9] # tmid jan lag value
df[1,11] <- df[1,2] - df[12,2] # ndvi jan lag value
vars <- df %>% select(ndvi, ndvi_lag, prec, tmid, tmid_lag)
vars <- data.frame(scale(vars))
# get temparature difference in year
x <- max(df$tmax) - min(df$tmax)

# set lag variables
f_ndvilag <- ifelse(x >= dif, ndvilag_high, ndvilag_low)
f_tlag <- ifelse(x >= dif, tlag_high, tlag_low)
f_prec <- ifelse(x >= dif, prec_high, prec_low)
f_temp <- ifelse(x >= dif, temp_low, temp_high)


# do weighting
vars$ndvi_lag <- vars$ndvi_lag * f_ndvilag
vars$tmid_lag <- vars$tmid_lag * f_tlag
vars$prec <- vars$prec * f_prec
vars$tmid <- vars$tmid * f_temp

vars <- vars[ , colSums(is.na(vars))==0]

# check whether 2 season or 4 season  system
krange <- c(2,4)
avg_sil <- sapply(X = krange, FUN = silhouette_score, vars = vars)
k <- which(avg_sil == max(avg_sil)) *2
cs <- kmeans(vars, centers = k)$cluster
vars$fixseasons <- fix(cs)
vars$season <- nameseasons(vars)
vars$month <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

# unscaling for reporting and visualisation
vars$tmid <- df$tmid
vars$prec <- df$prec
vars$ndvi <- df$ndvi
vars$tmid_lag <- df$tmid_lag
vars$ndvi_lag <- df$ndvi_lag

vars <- vars %>%
  select(month, season, tmid, prec, ndvi, tmid_lag, ndvi_lag)
return(vars)
}
