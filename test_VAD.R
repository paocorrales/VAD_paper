# Test VAD

## Imputs
# - date: fecha y hora del perfil
# - VAD: dataframe que devuelve vad_fit()
# - sounding: dataframe con sondeos

# soundings <- fread("data/soundings_wyoming_87155.csv") %>% 
#   .[, time := as_datetime(time)]
# 
# radial_wind <- ReadNetCDF("../VAD/RMA4/cfrad.20181121_105940.0000_to_20181121_110221.0000_RMA4_0200_02.nc", vars = c("Vda", "azimuth", "elevation"))
# VAD <- with(radial_wind, vad_fit(Vda, azimuth, range, elevation, r2 = 0.8, outlier_threshold = 2.5)) %>% 
#   setDT()
# 
# date <- ymd_hms("20181121120000")

test_VAD <- function(real_time, VAD, soundings) {

  date <- floor_date(real_time, unit = "day") + hours(12)
  if (!(date %in% unique(soundings$time))) {
    stop(paste("No hay sondeo para la fecha", date))
      } else {
    sounding <- soundings[time == date] %>% 
      .[, ":="(u = sin(dir*pi/180),
               v = cos(dir*pi/180))]
  }
  
  perfil <- vad_regrid(VAD, layer_width = 100, ht_out = sounding[, height-119]) %>% 
    setDT() %>% 
    .[, ":="(height = height + 119,
             spd_vad = sqrt(u^2 + v^2),
             dir_vad = 180+atan2(u, v)*180/pi)] %>% 
    setnames(c("u", "v"), c("u_vad", "v_vad")) %>% 
    .[sounding, on = "height"] 
  
  test <- perfil[, .(rmse_spd = sd((spd_vad - spd), na.rm = TRUE),
                     bias_spd = mean(spd_vad - spd, na.rm = TRUE),
                     rmse_dir = sqrt(mean((circular(dir_vad, units = "degrees") - circular(dir, units = "degrees"))^2, na.rm = TRUE)),
                     # rmse_dir = angular.deviation((circular(dir_vad, units = "degrees") - circular(dir, units = "degrees")), na.rm = TRUE),
                     bias_dir = mean(circular(dir_vad, units = "degrees") - circular(dir, units = "degrees"), na.rm = TRUE),
                     frac_n = mean(!is.na(spd_vad)))]
  
}





