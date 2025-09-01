library("sf")
library("dplyr")
library("lubridate")

# Clean municipality data from 
# https://www.colombiaenmapas.gov.co/?e=-84.08030383789077,-1.8478721626066288,-64.414776494146,11.854162824833724,4686&b=igac&u=0&t=2901&servicio=610
munis <- read_sf("Municipios_Agosto_2025.gpkg")
munis <- munis[munis$Depto == "Valle del Cauca",]
munis <- rename(munis,
                "munip_code" = "MpCodigo",
                "munip_name" = "MpNombre") |> 
  select(munip_code, munip_name)
write_sf(munis, "munip_vallecauco.gpkg")

# Clean station data from
# https://www.datos.gov.co/Ambiente-y-Desarrollo-Sostenible/Temperatura-Ambiente-del-Aire/sbwg-7ju4/explore/query/SELECT%0A%20%20%60codigoestacion%60%2C%0A%20%20%60codigosensor%60%2C%0A%20%20%60fechaobservacion%60%2C%0A%20%20%60valorobservado%60%2C%0A%20%20%60nombreestacion%60%2C%0A%20%20%60departamento%60%2C%0A%20%20%60municipio%60%2C%0A%20%20%60zonahidrografica%60%2C%0A%20%20%60latitud%60%2C%0A%20%20%60longitud%60%2C%0A%20%20%60descripcionsensor%60%2C%0A%20%20%60unidadmedida%60%0AWHERE%0A%20%20caseless_eq%28%60departamento%60%2C%20%22VALLE%20DEL%20CAUCA%22%29%0A%20%20AND%20%60fechaobservacion%60%0A%20%20%20%20%20%20%20%20BETWEEN%20%222010-01-01T00%3A00%3A00%22%20%3A%3A%20floating_timestamp%0A%20%20%20%20%20%20%20%20AND%20%222012-12-31T23%3A45%3A00%22%20%3A%3A%20floating_timestamp/page/filter
stations <- read.csv("Temperatura_Ambiente_del_Aire_20250901.csv")
stations <- rename(stations,
                   "temp" = "ValorObservado",
                   "station_code" = "CodigoEstacion",
                   "datetime" = "FechaObservacion",
                   "lat" = "Latitud",
                   "lon" = "Longitud") |> 
  select(station_code, datetime, lat, lon, temp) |> 
  mutate(datetime = ymd_hms(datetime),
         date = as.Date(datetime)) |> 
  filter(temp != 0) # Wrong readings
stations <- stations |> 
  group_by(station_code, lat, lon, date) |> 
  summarise(n = n(),
            temp_mean = mean(temp),
            temp_min = min(temp),
            temp_max = max(temp)) |> 
  ungroup()
barplot(table(stations$n))
stations <- stations |> 
  filter(n >= 17) |> 
  select(-n)
write.csv(stations, "stations_vallecauco.csv", row.names = FALSE)