library(data.table)
library(dplyr)

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
