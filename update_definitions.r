#Actualizar catalogos de definiciones del diccionario de datos

library(readxl)    
filename = "./data/Catalogos_0412.xlsx"
sheets <- readxl::excel_sheets(filename)
catalogo <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
catalogo <- lapply(catalogo, as.data.frame)
names(catalogo) <- sheets
save(catalogo, file = "./data/catalogo.Rdata")
save.image()
