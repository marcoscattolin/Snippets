library(maptools)


reg <- readShapeLines("Regione_polygon.shp")
acqua <- readShapePoints("Uso_prioritario_di_corsi_acqua_e_laghi_point.shp")
train <- readShapeLines("Rete_ferroviaria_10000_CT10_line.shp")

