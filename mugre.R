library(data.table)
library(rvad)
library(metR)
library(ggplot2)

radial_wind <- ReadNetCDF("data/cfrad.20181111_120018.0000_to_20181111_120300.0000_RMA4_0200_02.nc", vars = c("Vda", "azimuth", "elevation"))
radial_wind2 <- ReadNetCDF("data/cfrad.20181111_120018.0000_to_20181111_120300.0000_RMA4_0200_02.nc", vars = c("Vda2", "azimuth", "elevation"))
radial_wind3 <- ReadNetCDF("data/cfrad.20181111_120018.0000_to_20181111_120300.0000_RMA4_0200_02.nc", vars = c("Vda3", "azimuth", "elevation"))
radial_wind_ori <- ReadNetCDF("data/cfrad.20181111_120018.0000_to_20181111_120300.0000_RMA4_0200_02.nc", vars = c("VRAD", "azimuth", "elevation"))
VAD <- with(radial_wind, vad_fit(Vda, azimuth, range, elevation))
plot(VAD)

wind_profile <- vad_regrid(VAD, layer_width = 100)

plot(wind_profile)


raobs <- fread("data/sounding.csv", na.strings = "99999")

na.omit(raobs) %>% 
  ggplot(aes(V7/10, V3)) + 
  geom_point() +
  geom_path() +
  scale_y_continuous(limits = c(0, 5000)) +
  geom_point(data = VAD, aes(sqrt(u^2+v^2), height+120, color = r2), na.rm = TRUE)

one_elevation <- subset(radial_wind, elevation == unique(elevation)[1] &
                          !is.na(Vda))
ggplot(one_elevation, aes(azimuth, range)) +
  stat_subset(aes(subset = is.cross(azimuth, range), color = Vda, size = range^2), geom = "point") +
  # geom_point(aes(color = Vda, size = range^2)) +
  scale_radius(range = c(0, 0.05), guide = "none") +
  scale_color_gradient2(low = "blue", high = "red") +
  scale_y_continuous(limits = c(0, 100000)) +
  coord_polar()

# Busquemos el mejor tiempo para usar en las pruebas menores.

files <- Sys.glob("../VAD/RMA4/cfrad.20181*_120*.nc")

soundings <- fread("data/soundings_87155.csv")
soundings[, time := as_datetime(time)]

for (i in seq_along(files)) { 
  # Leo el volumen y calculo el VAD
  radial_wind <- ReadNetCDF(files[i], vars = c("Vda", "azimuth", "elevation"))
  VAD <- with(radial_wind, vad_fit(Vda, azimuth, range, elevation))
  
  # Leo el sondeo
  date_time <- ymd_hm(paste0(substr(basename(files[i]), 7, 14), " 12:00"))
  
  soundings[time == date_time] %>% 
    na.omit() %>% 
    ggplot(aes(windspd/10, height)) +
    geom_path() +
    geom_point(data = VAD, aes(sqrt(u^2 + v^2), height + 119, color = factor(elevation))) +
    coord_cartesian(ylim = c(0, 2000), xlim = c(0, 25))
  
  ggsave(paste0("../VAD/RMA4/fig/vad_", substr(basename(files[i]), 7, 21), ".png"))
  
  }
