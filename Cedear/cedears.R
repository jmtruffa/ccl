# Viene de la página https://www.comafi.com.ar/2254-CEADEAR-SHARES.note.aspx
# También puede estar el link directo en http://www.comafi.com.ar/Multimedios/otros/6517.xlsx
# La graba en el directorio "~/Documents/Data/" que es donde voy a grabar la data en csv
# Hay un directorio temporal dentro de ese que es donde descargo el archivo en xlsx para luego convertirlo.


library(readxl)
download.file("http://www.comafi.com.ar/Multimedios/otros/6517.xlsx", "~/Documents/Data/Temp/6517.xlsx", quiet = TRUE, method = "auto")

# Leo el archivo excel que bajé a la sección Data temporal y le asigno las columnas
cedears_multip <- read_excel("~/Documents/Data/Cedear/6517.xlsx", 
                             col_types = c("skip", "text", "text", 
                                           "text", "text", "text", "text", "text"), 
                             skip = 15)
colnames(cedears_multip) <- c("Nombre", "symbol", "Cod_Caja", "ISIN_Cedear", "ISIN_Suby", "CUSIP", "RATIO")
# separo RATIO, lo calculo y lo grabo en la columna ratio. los casteo como numeric
# al final agrego la columna symbol_local y reordeno las columnas con relocate
cedears_multip <- cedears_multip %>% 
  separate(RATIO, c("q_local", "q_ext")) %>% 
  mutate(
    ratio = as.numeric(q_local) / as.numeric(q_ext),
    symbol_local = paste(cedears_multip$symbol, ".BA", sep ="")) %>% 
  select(-q_local, -q_ext) %>% 
  relocate(Nombre, symbol, symbol_local)

# filtrados varios
cedears_multip <- cedears_multip %>% drop_na() # filtramos los NAs
cedears_multip <- cedears_multip %>% filter(!grepl('^\\w+\\s\\w+$', symbol)) # filtramos los que tengan más de una palabra en ticker -> no son tickers US válidos
cedears_multip <- cedears_multip %>% filter(str_detect(ISIN_Suby, "^US")) # filtramos aquellos ISIN_Subyacente que no sea de US

# grabo
write_csv(cedears_multip, "~/Documents/Data/cedears.csv", col_names = TRUE)
