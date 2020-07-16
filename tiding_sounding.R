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
lines <- readLines("data/wyoming_soundings_201812.raw")

idx <- which(stringi::stri_startswith(lines, fixed = "87155"))
idx <- c(idx, length(temp)+1)

idx_end <- which(stringi::stri_startswith(lines, fixed = "Station"))
# 
# # Separo cada linea en cachitos y me quedo con todo lo que no es espacio ""
# temp <- strsplit(lines, " ")
# temp <- lapply(temp, function(x) x[x != ""])
# 
# # Indices donde comienza cada sondeo
# idx <- which(vapply(temp, function(x) x[1] == "87155", TRUE))
# idx <- c(idx, length(temp)+1)

widths <- c(8, rep(7, 10))
soundings <- list()
for (i in seq_len(length(idx)-1)) { 
  subset <- lines[(idx[i] + 5):(idx_end[i] - 1)]
  
  tmpfile <- tempfile()
  writeLines(subset, tmpfile)
  out <- read.fwf(file = tmpfile, widths = widths, na.strings = "99999")
  colnames(out) <- c("pressure", "height", "temp", "dewpt", "relh", "mixr", "dir", "spd", "thta", "thte", "thtv")
  setDT(out)
  out[, spd := spd*0.514444]
  
  
  date <- lines[(idx[i])]
  date <- gsub("87155 SARE Resistencia Aero Observations at ", "", date)
 
  out[, time := lubridate::parse_date_time(date, "%H %d %b %y")]
  print(i)
  soundings[[i]] <- out
  
}

soundings <- rbindlist(soundings)

fwrite(soundings, "data/soundings_wyoming_87155_201812.csv")
# 