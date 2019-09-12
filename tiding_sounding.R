library(data.table)
library(dplyr)

## https://ruc.noaa.gov/raobs/
# Leo línea por línea
lines <- readLines("data/raob_soundings26577.tmp")

# Separo cada linea en cachitos y me quedo con todo lo que no es espacio ""
temp <- strsplit(lines, " ")
temp <- lapply(temp, function(x) x[x != ""])

# Indices donde comienza cada sondeo
idx <- which(vapply(temp, function(x) x[1] == "254", TRUE))
idx <- c(idx, length(temp)+1)

soundings <- list()
for (i in seq_len(length(idx)-1)) { 
  
  out <- transpose(as.data.table((temp[(idx[i] + 4):(idx[i + 1] - 1)])))
  out[, names(out) := lapply(.SD, as.numeric)]
  colnames(out) <- c("type", "pressure", "height", "temp", "dewpt", "winddir", "windspd")
  
  out <- out[, lapply(.SD, function(x) {
    x[x == 99999] <- NA
    x
  } 
  )]
  
  f <- temp[(idx[i])]
  out[, time := ymd_hm(paste(paste(f[[1]][5], f[[1]][4], f[[1]][3], sep = "-"), f[[1]][2], ":00", sep = " "))]
  soundings[[i]] <- out
  
}

soundings <- rbindlist(soundings)

fwrite(soundings, "data/soundings_87155.csv")

# ------------------------------------------------------------------------------------------------------------

## http://weather.uwyo.edu/upperair/sounding.html
# Leo línea por línea
lines <- readLines("data/wyoming_soundings.raw")

# Separo cada linea en cachitos y me quedo con todo lo que no es espacio ""
temp <- strsplit(lines, " ")
temp <- lapply(temp, function(x) x[x != ""])

# Indices donde comienza cada sondeo
idx <- which(vapply(temp, function(x) x[1] == "87155", TRUE))
idx <- c(idx, length(temp)+1)


soundings <- list()
for (i in seq_len(length(idx)-1)) { 
  
  subset <- temp[(idx[i] + 6):(idx[i + 1] - 35)]
    out <- transpose(as.data.table(subset[lengths(subset) == 11]))
  out[, names(out) := lapply(.SD, as.numeric)]
  colnames(out) <- c("pressure", "height", "temp", "dewpt", "relh", "mixr", "dir", "spd", "thta", "thte", "thtv")
  out[, spd := spd*0.514444]
  
  out <- out[, lapply(.SD, function(x) {
    x[x == 99999] <- NA
    x
  } 
  )]
  
  f <- temp[(idx[i])]
  out[, time := ymd_h(paste(paste(f[[1]][10], f[[1]][9], f[[1]][8], sep = "-"), f[[1]][7], sep = " "), locale = "en_US.UTF-8")]
  print(out[, time])
  soundings[[i]] <- out
  
}

soundings <- rbindlist(soundings)

fwrite(soundings, "data/soundings_wyoming_87155.csv")
