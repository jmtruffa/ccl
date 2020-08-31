# Calcula el CCL  de los últimos 90 o la cantidad de días que se determine en inicio
# calcula la fecha final del rango como la fecha de hoy ajustada si es hábil - 4 así tiene los úlitmos 5 días

library(tidyquant)
library(bizdays)
library(tidyverse)
library(ggthemes) # solo para las implementaciones de abajo

cal <- create.calendar("Argentina/ANBIMA", holidaysANBIMA, weekdays=c("saturday", "sunday"))
final <- adjust.previous(Sys.Date(), cal)
inicio <- adjust.previous(final - 90, cal)

# cargo los adr argentinos y los cedears. Cada uno viene con sus symbol, symbol_local y ratio.
adr_argentinos <- read_csv("~/Documents/Data/ADRs_Argentinos/adr_argentinos.csv", 
                           col_types = cols(empresa = col_skip(), 
                                            ratio = col_number()))
cedears <- read_csv("~/Documents/Data/Cedear/cedears.csv", 
                    col_types = cols(Nombre = col_skip(), 
                                     Cod_Caja = col_skip(), ISIN_Cedear = col_skip(), 
                                     ISIN_Suby = col_skip(), CUSIP = col_skip(), 
                                     ratio = col_number()))

# creo un df con todos los activos a analizar con su correspondiente activo local
# luego hay que reciclarla para pedir a tq_get todos los archivos (externo y local) y no hacer varias llamadas
activos <- bind_rows(adr_argentinos, cedears)
lista_activos <- bind_rows(activos %>% transmute(symbol1 = symbol, symbol2 = symbol_local, ratio = ratio),
                           activos %>% transmute(symbol1 = symbol_local, symbol2 = symbol, ratio = ratio))
colnames(lista_activos) <- c("symbol", "symbol2", "ratio")
rm(activos) # lo borro

# esto me devuelve un df con los precios en formato OHLCVA.
precios <- lista_activos$symbol %>%
  tq_get(get  = "stock.prices",
         from = inicio,
         to   = final) %>%
  group_by(symbol)

#ahora los separo entre local y externo
local <- precios %>% filter(str_detect(symbol, fixed(".")))
afuera <- precios %>% filter(!str_detect(symbol, fixed(".")))
rm(precios) # lo descarto

# ahora agregamos el simbolo correspondiente a cada uno, tomandolo de lista_activos
local <- left_join(local, lista_activos)
afuera <- left_join(afuera, lista_activos)

# ahora que ambos tiene su correspondiente symbolo de la otra bolsa los juntamos
df_ccl <- left_join(local, afuera, by = c("symbol2" = "symbol", "date" = "date"))

# hacemos una bajada a dir temporal para guardar la data
file = paste("~/Documents/Data/Temp/", "ccl", inicio, "-", final, ".csv")
write_csv(df_ccl, file, col_names = TRUE)

# ahora tenemos en final los activos locales y su precio afuera
# ahora vamos a calcularle el ccl
# y luego borrarles los que tienen volumen 0

df_ccl <- df_ccl  %>% mutate(
  ccl = close.x * ratio.x / close.y) %>% 
  select(date, symbol, volume.x, close.x, adjusted.x, symbol2, ratio.x, volume.y, close.y, adjusted.y, ccl) %>% 
  filter (volume.x != 0)


##################################################################
# Hasta acá fue la rutina en sí.
# Abajo hay pruebas de graficar


GGAL <- df_ccl %>% filter(symbol == "GGAL.BA") %>% drop_na()
GBYE <- df_ccl %>% filter(symbol == "GGAL.BA" | symbol == "BMA.BA" | symbol == "YPFD.BA" | symbol == "EDN.BA") %>% drop_na()
GBYE %>% group_by(date) %>% summarise (CCL_prom = mean(ccl))


GBYE %>%
  filter(date >= inicio - days(2 * n_mavg)) %>%
  ggplot(aes(x = date, y = adjusted.x, color = symbol)) +
  geom_line(size = 1) +
  geom_ma(n = 15, color = "darkblue", size = 1) +
  geom_ma(n = n_mavg, color = "red", size = 1) +
  labs(title = "Dark Theme",
       x = "", y = "Closing Price") +
  coord_x_date(xlim = c(start, end)) +
  facet_wrap(~ symbol, scales = "free_y") +
  theme_tq_dark() +
  scale_color_tq(theme = "dark") +
  scale_y_continuous(labels = scales::dollar)

GGAL %>%
  ggplot(aes(x = date, y = volume.x)) +
  geom_segment(aes(xend = date, yend = 0, color = volume.x)) +
  geom_smooth(method = "loess", se = FALSE) +
  theme_tq() +
  theme(legend.position = "none") +
  scale_y_continuous(sec.axis = sec_axis(~. / 50000, name = "CCL")) +
  geom_line(aes(x = date, y = ccl)) +
  labs(title = "GGAL Gráfico de Volumen",
       subtitle = "Volumen en bolsa local",
       y = "Volumen", x = "") 
  
GBYE %>% 
  ggplot(aes(x = date, y = ccl, group=symbol, color = symbol)) +
  geom_line(color = palette_light()[[1]])  +
  labs(title = "CCL a través de GGAL",
       y = "CCL calculado con precios de Cierre", x = "") +
  theme_tq()


GBYE %>%
  ggplot(aes(x = date, y = ccl, color = symbol)) +
  geom_line() +
  #facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
  theme_tq()

GBYE %>% 
  group_by(date) %>%
  summarise (CCL_prom = mean(ccl)) %>% 
  ggplot(aes(x = date, y = CCL_prom)) +
  geom_line() +
  theme_economist() + 
  scale_color_economist() +
  labs(title = "CCL prom con GGAL BMA YPFD EDN",
       y = "CCL calculado con precios de Cierre", x = "")

