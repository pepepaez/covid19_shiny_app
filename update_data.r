temp <- tempfile()
download.file("http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip",temp)
file_name <- unzip(temp, list = TRUE)$Name[1]
datos <- read.table(unz(temp, file_name), sep = ",", header = TRUE)
unlink(temp)
save(datos, file = "./data/datos.Rdata")
save.image()
