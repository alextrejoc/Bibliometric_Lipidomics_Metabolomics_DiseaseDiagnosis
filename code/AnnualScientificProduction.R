#### Script to plot Annual Scientific Production
# rm(list = ls())
# setwd("G:/Mi unidad/Investigación Trejo-Castro/E31 - DBC Metabolic Health and Disease/E31.2 Bibliometric Lipidome and Metabolome")
# 
# # Cargar datos
# dataAP <- read.csv("6. Bibliometrix Results/1. Overview/Annual Scientific Production/Annual_Production_bibliometrix_2025-02-17.csv")
# 
# # Librerías necesarias
# library(ggplot2)
# library(RColorBrewer)
# 
# # Convertir datos a numéricos
# dataAP$Articles <- as.numeric(dataAP$Articles)
# dataAP$Year <- as.numeric(dataAP$Year)
# 
# # Filtrar datos desde 2002 a 2025
# dataAP <- subset(dataAP, Year >= 2004 & Year <= 2025)
# 
# # Modelo exponencial basado en datos desde 2010 en adelante
# dataAP2 <- subset(dataAP, Year > 2004)
# log.model <- lm(log(Articles) ~ Year, dataAP2)
# 
# # Crear puntos para la curva exponencial
# xvalues <- seq(2004, 2025, 0.1)  
# Y.values <- exp(predict(log.model, list(Year = xvalues)))
# data_curve <- data.frame(Year = xvalues, Articles = Y.values)
# 
# # Valor estimado en 2025
# year_2025 <- 2025
# articles_2025 <- round(exp(predict(log.model, newdata = data.frame(Year = year_2025))), 0)  # Sin decimales
# 
# # Construir ecuación con formato correcto
# slope <- round(coef(log.model)[2], 4)
# intercept <- round(coef(log.model)[1], 4)
# adj_r2 <- round(summary(log.model)$adj.r.squared, 3)
# 
# eq_label <- paste0("y == e^{", slope, " * x ", ifelse(intercept < 0, "- ", "+ "), abs(intercept), "}")
# r2_label <- paste0("Adjusted~R^2 == ", adj_r2)
# 
# # Ajustar tamaño de la figura
# width_in_inches <- 10  
# height_in_inches <- 6  
# 
# # Crear la gráfica
# plot_AP <- ggplot(dataAP, aes(x = Year, y = Articles)) +
#   geom_bar(stat = "identity", colour = "Black", fill = "Lightblue") +
#   geom_text(aes(label = Articles), vjust = -1, size = 4) +  # Reducir tamaño del texto de etiquetas
#   geom_point() +
#   geom_line(data = data_curve, aes(x = Year, y = Articles), colour = "red", linewidth = 1, linetype = "dashed") +  # Línea roja punteada
#   geom_segment(aes(x = year_2025, xend = year_2025, y = 0, yend = articles_2025), 
#                linetype = "dotted", color = "blue", linewidth = 1) +  # Línea vertical corregida
#   annotate("text", x = year_2025 - 1.5, y = articles_2025 - 200,  # Se mueve ligeramente la etiqueta
#            label = "Estimated value 2610", color = "blue", size = 5, hjust = 1) +  # Texto alineado a la izquierda del punto
#   annotate("rect", xmin = 2006, xmax = 2012, ymin = max(dataAP$Articles) * 0.75, ymax = max(dataAP$Articles), 
#            fill = "white", color = "black") +  # Recuadro ajustado para ecuación
#   annotate("text", x = 2009, y = max(dataAP$Articles) * 0.85, label = eq_label, parse = TRUE, size = 5, hjust = 0.5) +  # Ecuación centrada
#   annotate("text", x = 2009, y = max(dataAP$Articles) * 0.78, label = r2_label, parse = TRUE, size = 5, hjust = 0.5) +  # R² centrado
#   ggtitle("Annual Scientific Production") +
#   ylab("Number of Documents") +
#   theme_bw() +
#   theme(legend.position = "none")  # Quitar la leyenda

# Guardar la imagen con alta resolución
#ggsave("Annual_Scientific_Production_Optimized.png", 
   #    plot = plot_AP, width = width_in_inches, height = height_in_inches, dpi = 300)

# También puedes exportarla en otros formatos si la revista lo requiere
#ggsave("Annual_Scientific_Production_Optimized.pdf", plot = plot_AP, width = width_in_inches, height = height_in_inches, dpi = 300)
#ggsave("Annual_Scientific_Production_Optimized.tiff", plot = plot_AP, width = width_in_inches, height = height_in_inches, dpi = 300, compression = "lzw")



#### Script para Modelo Polinómico de Producción Científica (Correcciones)
rm(list = ls())
setwd("G:/Mi unidad/Investigación Trejo-Castro/E31 - DBC Metabolic Health and Disease/E31.2 Bibliometric Lipidome and Metabolome")

# Cargar datos
dataAP <- read.csv("6. Bibliometrix Results/1. Overview/Annual Scientific Production/Annual_Production_bibliometrix_2025-02-17.csv")

# Librerías necesarias
library(ggplot2)

# Convertir datos a numéricos
dataAP$Articles <- as.numeric(dataAP$Articles)
dataAP$Year <- as.numeric(dataAP$Year)

# Ajustar modelo polinómico de segundo grado
poly_model <- lm(Articles ~ poly(Year, 2, raw = TRUE), data = dataAP)

# Extraer coeficientes
a_poly <- coef(poly_model)[3]
b_poly <- coef(poly_model)[2]
c_poly <- coef(poly_model)[1]

# Calcular R² ajustado
r2_poly <- round(summary(poly_model)$adj.r.squared, 3)

# Predicción para 2025
year_2025 <- 2025
articles_2025_poly <- round(predict(poly_model, newdata = data.frame(Year = year_2025)), 0)

# Generar puntos de la curva polinómica hasta 2025
x_pred <- seq(min(dataAP$Year), 2025, 0.1)
y_poly_pred <- predict(poly_model, newdata = data.frame(Year = x_pred))

# Crear la gráfica simplificada
plot_poly <- ggplot(dataAP, aes(x = Year, y = Articles)) +
  geom_bar(stat = "identity", colour = "Black", aes(fill = "Observed Data"), show.legend = TRUE) +  
  geom_point(size = 4, color = "black", aes(shape = "Observed Data"), show.legend = TRUE) +  
  geom_text(aes(label = Articles), vjust = -1, size = 6) +  
  geom_line(data = data.frame(Year = x_pred, Articles = y_poly_pred), 
            aes(x = Year, y = Articles, color = "Polynomial Model"), linetype = "dashed", linewidth = 2) +  
  annotate("segment", x = year_2025, xend = year_2025, y = 0, yend = articles_2025_poly, 
           linetype = "dotted", linewidth = 1.5, color = "blue") +  
  annotate("text", x = year_2025, y = articles_2025_poly + 50, 
           label = paste0(articles_2025_poly), 
           color = "blue", size = 7, fontface = "bold", hjust = 0.5) +  
  annotate("text", x = 2004, y = max(dataAP$Articles) * 0.70, 
           label = paste0("y = ", a_poly, " * x^2 + ", b_poly, " * x + ", c_poly,2, 
                          "\nAdjusted R² = ", r2_poly), 
           size = 6, hjust = 0, fontface = "italic") +  
  ggtitle("Annual Scientific Production - Polynomial Model") +
  ylab("Number of Documents") +
  xlab("Year") +
  scale_color_manual(values = c("Polynomial Model" = "red", "Estimated Value 2025" = "blue")) +
  scale_fill_manual(values = c("Observed Data" = "lightblue")) +
  scale_shape_manual(values = c("Observed Data" = 16)) +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.background = element_blank(),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18, face = "bold"),
    plot.title = element_text(size = 20, face = "bold")
  ) +
  guides(fill = "none", shape = guide_legend(title = ""), color = guide_legend(title = ""))

# Guardar la imagen con alta resolución
ggsave("3. Figures/Annual_Scientific_Production_Polynomial.png", plot = plot_poly, width = 14, height = 7, dpi = 600)
ggsave("3. Figures/Annual_Scientific_Production_Polynomial.pdf", plot = plot_poly, width = 14, height = 7, dpi = 600)
ggsave("3. Figures/Annual_Scientific_Production_Polynomial.tiff", plot = plot_poly, width = 14, height = 7, dpi = 600, compression = "lzw")
