################################################################################
#              FUNCIONES PARA REPRESENTAR GRÁFICOS ICE CON GGPLOT2             #
#                                                                              #
# This work by Joaquín Amat Rodrigo is licensed under a Creative Commons       #
# Attribution 4.0 International License.                                       #
################################################################################


# Las siguientes funciones generan los gráficos ICE, c-ICE y d-ICE a partir de
# los los objetos ice y dice generados con la librería ICEbox.

library(tidyverse)
library(ICEbox)
library(ggpubr)
library(purrr)
library(furrr)
library(gridExtra)

plot_ice <- function(objeto, pdp = TRUE, color_by = NULL){
  # Esta función devuelve el gráfico de las curvas ICE
  # Argumentos
  #   objeto: un objeto devuelto por la función ICEbox::ice
  #   pdp:    si se muestra o no la curva promedio pdp
  #   color_by: predictor por el cual que quiere colorear las curvas.
  # Output
  #   Gráfico ggplot
  
  predictor <- objeto$predictor
  
  datos_ice <- objeto$ice_curves %>%
    as.data.frame() %>%
    mutate(observacion_id = rownames(objeto$Xice)) %>%
    gather(key = !!predictor, value = "y", -observacion_id) %>%
    mutate(!!sym(predictor) := as.numeric(!!sym(predictor)))
  
  if(!is.null(color_by)){
    datos_color <- objeto$Xice %>%
      rownames_to_column(var = "observacion_id") %>%
      select(observacion_id, !!sym(color_by))
    
    datos_ice <- datos_ice %>% 
      left_join(datos_color, by = "observacion_id")
  }
  
  datos_observaciones <- objeto$xj %>%
    as.data.frame() %>%
    rename(!!predictor := !!sym(".")) %>%
    mutate(y = objeto$actual_prediction,
           observacion_id = rownames(objeto$Xice))
  
  datos_pdp <- objeto$pdp %>%
    as.data.frame() %>%
    rownames_to_column(var = predictor) %>%
    mutate(!!sym(predictor) := as.numeric(!!sym(predictor))) %>%
    rename(y := !!sym("."))
  
  if(pdp & is.null(color_by)){
    
    plot <- ggplot(data = datos_ice, aes(x = !!sym(predictor), y = y)) +
      geom_path(aes(group = observacion_id), color = "gray30", alpha=0.5) +
      geom_point(data=datos_observaciones, aes(x= !!sym(predictor), y=y),
                 colour = "black", pch = 21, fill = "gray20") +
      geom_line(data = datos_pdp, aes(x = !!sym(predictor), y = y),
                colour = "red", size = 1.2)  +
      labs(title = paste("Curvas ICE:", predictor)) +
      theme_bw() +
      theme(legend.position = "bottom")
    
  }else if(pdp & !is.null(color_by)){
    plot <- ggplot(data = datos_ice, aes(x = !!sym(predictor), y = y)) +
      geom_path(aes(group = observacion_id, color = !!sym(color_by)),
                alpha = 0.5) +
      geom_point(data=datos_observaciones, aes(x = !!sym(predictor), y=y),
                 colour = "black", pch = 21, fill = "gray20") +
      geom_line(data = datos_pdp, aes(x = !!sym(predictor), y = y),
                colour = "red", size = 1.2) +
      labs(title = paste("Curvas ICE:", predictor)) +
      theme_bw() +
      theme(legend.position = "bottom")
    
  }else if(!pdp & is.null(color_by)){
    plot <- ggplot(data = datos_ice, aes(x = !!sym(predictor), y = y)) +
      geom_path(aes(group=observacion_id), color="gray30", alpha=0.5) +
      geom_point(data=datos_observaciones, aes(x= !!sym(predictor), y=y),
                 colour = "black", pch = 21, fill = "gray20") +
      labs(title = paste("Curvas ICE:", predictor)) +
      theme_bw() +
      theme(legend.position = "bottom")
    
  }else{
    plot <- ggplot(data = datos_ice, aes(x = !!sym(predictor), y = y)) +
      geom_path(aes(group = observacion_id, color = !!sym(color_by)),
                alpha = 0.5) +
      geom_point(data=datos_observaciones, aes(x= !!sym(predictor), y=y),
                 colour = "black", pch = 21, fill = "gray20") +
      labs(title = paste("Curvas ICE:", predictor)) +
      theme_bw() +
      theme(legend.position = "bottom")
    
  }
  plot
  return(plot)
}


plot_c_ice <- function(objeto, pdp = TRUE, color_by = NULL){
  # Esta función devuelve el gráfico de las curvas ICE centrados
  # Argumentos
  #   objeto: un objeto devuelto por la función ICEbox::ice
  #   pdp:    si se muestra o no la curva promedio pdp
  #   color_by: predictor por el cual que quiere colorear las curvas.
  # Output
  #   Gráfico ggplot
  
  predictor <- objeto$predictor
  
  datos_ice <- objeto$ice_curves %>%
    as.data.frame() %>%
    mutate(observacion_id = rownames(objeto$Xice)) %>%
    gather(key = !!predictor, value = "y", -observacion_id) %>%
    mutate(!!sym(predictor) := as.numeric(!!sym(predictor)))
  
  minimo_por_curva <- datos_ice %>%
    group_by(observacion_id) %>% 
    summarize(minimo = y[which.min(!! sym(predictor))])
  
  datos_c_ice <- datos_ice %>%
    left_join(minimo_por_curva, by = "observacion_id") %>%
    mutate(y = y - minimo)
  
  if(!is.null(color_by)){
    datos_color <- objeto$Xice %>%
      rownames_to_column(var = "observacion_id") %>%
      select(observacion_id, !!sym(color_by))
    
    datos_c_ice <- datos_c_ice %>% 
      left_join(datos_color, by = "observacion_id")
  }
  
  datos_observaciones <- objeto$xj %>%
    as.data.frame() %>%
    rename(!!predictor := !!sym(".")) %>%
    mutate(y = objeto$actual_prediction,
           observacion_id = rownames(objeto$Xice))
  datos_c_observaciones <- datos_observaciones %>%
    left_join(minimo_por_curva, by = "observacion_id") %>%
    mutate(y = y - minimo)
  
  datos_c_pdp <- objeto$pdp %>%
    as.data.frame() %>%
    rownames_to_column(var = predictor) %>%
    mutate(!!sym(predictor) := as.numeric(!!sym(predictor))) %>%
    rename(y := !!sym(".")) %>%
    mutate(y = y - y[which.min(!! sym(predictor))])
  
  
  if(pdp & is.null(color_by)){
    
    plot <- ggplot(data = datos_c_ice, aes(x = !!sym(predictor), y = y)) +
      geom_path(aes(group = observacion_id), color = "gray30", alpha = 0.5) +
      geom_point(data=datos_c_observaciones, aes(x= !!sym(predictor), y=y),
                 colour = "black", pch = 21, fill = "gray20") +
      geom_line(data = datos_c_pdp, aes(x = !!sym(predictor), y = y),
                colour = "red", size = 1.2) +
      labs(title = paste("Curvas ICE centradas:", predictor)) +
      theme_bw()  +
      theme(legend.position = "bottom")
    
  }else if(pdp & !is.null(color_by)){
    plot <- ggplot(data = datos_c_ice, aes(x = !!sym(predictor), y = y)) +
      geom_path(aes(group = observacion_id, color = !!sym(color_by)),
                alpha = 0.5) +
      geom_point(data=datos_c_observaciones, aes(x= !!sym(predictor), y=y),
                 colour = "black", pch = 21, fill = "gray20") +
      geom_line(data = datos_c_pdp, aes(x = !!sym(predictor), y = y),
                colour = "red", size = 1.2) +
      labs(title = paste("Curvas ICE centradas:", predictor)) +
      theme_bw() +
      theme(legend.position = "bottom")
    
  }else if(!pdp & is.null(color_by)){
    plot <- ggplot(data = datos_c_ice, aes(x = !!sym(predictor), y = y)) +
      geom_path(aes(group = observacion_id), color = "gray30", alpha = 0.5) +
      geom_point(data=datos_c_observaciones, aes(x= !!sym(predictor), y=y),
                 colour = "black", pch = 21, fill = "gray20") +
      labs(title = paste("Curvas ICE centradas:", predictor)) +
      theme_bw() +
      theme(legend.position = "bottom")
  }else{
    
    plot <- ggplot(data = datos_c_ice, aes(x = !!sym(predictor), y = y)) +
      geom_path(aes(group = observacion_id, color = !!sym(color_by)),
                alpha = 0.5) +
      geom_point(data=datos_c_observaciones, aes(x = !!sym(predictor), y=y),
                 colour = "black", pch = 21, fill = "gray20") +
      labs(title = paste("Curvas ICE centradas:", predictor)) +
      theme_bw() +
      theme(legend.position = "bottom")
  }
  plot
  return(plot)
}

plot_d_ice <- function(objeto, pdp = TRUE, color_by = NULL){
  # Esta función devuelve el gráfico de las curvas d-ICE
  # Argumentos
  #   objeto: un objeto devuelto por la función ICEbox::dice
  #   pdp:    si se muestra o no la curva promedio pdp
  #   color_by: predictor por el cual que quiere colorear las curvas.
  # Output
  #   Gráfico ggplot
  
  predictor <- objeto$predictor
  
  datos_d_ice <- objeto$d_ice_curves %>%
    as.data.frame() %>%
    setNames(as.character(objeto$gridpts)) %>%
    mutate(observacion_id = rownames(objeto$Xice)) %>%
    gather(key = !!predictor, value = "y", -observacion_id) %>%
    mutate(!!sym(predictor) := as.numeric(!!sym(predictor)))
  
  if(!is.null(color_by)){
    datos_color <- objeto$Xice %>%
      rownames_to_column(var = "observacion_id") %>%
      select(observacion_id, !!sym(color_by))
    
    datos_d_ice <- datos_d_ice %>% 
      left_join(datos_color, by = "observacion_id")
  }
  
  datos_d_observaciones <- objeto$actual_deriv %>%
    as.data.frame() %>%
    rename(y = !!sym(".")) %>%
    mutate(!!sym(predictor) := objeto$xj,
           observacion_id = rownames(objeto$Xice))
  
  datos_d_pdp <- data.frame(y = objeto$dpdp) %>%
    mutate(!!sym(predictor) := objeto$gridpts)
  
  if(pdp & is.null(color_by)){
    
    plot <- ggplot(data = datos_d_ice, aes(x = !!sym(predictor), y = y)) +
      geom_path(aes(group = observacion_id), color = "gray30", alpha=0.5) +
      geom_point(data=datos_d_observaciones, aes(x= !!sym(predictor), y=y),
                 colour = "black", pch = 21, fill = "gray20") +
      geom_line(data = datos_d_pdp, aes(x = !!sym(predictor), y = y),
                colour = "red", size = 1.2)  +
      labs(title = paste("Derivada curvas ICE:", predictor)) +
      theme_bw() +
      theme(legend.position = "bottom",
            axis.title.x = element_blank())
    
  }else if(pdp & !is.null(color_by)){
    plot <- ggplot(data = datos_d_ice, aes(x = !!sym(predictor), y = y)) +
      geom_path(aes(group = observacion_id, color = !!sym(color_by)),
                alpha = 0.5) +
      geom_point(data=datos_d_observaciones, aes(x = !!sym(predictor), y=y),
                 colour = "black", pch = 21, fill = "gray20") +
      geom_line(data = datos_d_pdp, aes(x = !!sym(predictor), y = y),
                colour = "red", size = 1.2) +
      labs(title = paste("Derivada curvas ICE:", predictor)) +
      theme_bw() +
      theme(legend.position = "bottom",
            axis.title.x = element_blank())
    
  }else if(!pdp & is.null(color_by)){
    plot <- ggplot(data = datos_d_ice, aes(x = !!sym(predictor), y = y)) +
      geom_path(aes(group=observacion_id), color="gray30", alpha=0.5) +
      geom_point(data=datos_d_observaciones, aes(x= !!sym(predictor), y=y),
                 colour = "black", pch = 21, fill = "gray20") +
      labs(title = paste("Derivada curvas ICE:", predictor)) +
      theme_bw() +
      theme(legend.position = "bottom",
            axis.title.x = element_blank())
    
  }else{
    plot <- ggplot(data = datos_d_ice, aes(x = !!sym(predictor), y = y)) +
      geom_path(aes(group = observacion_id, color = !!sym(color_by)),
                alpha = 0.5) +
      geom_point(data=datos_d_observaciones, aes(x= !!sym(predictor), y=y),
                 colour = "black", pch = 21, fill = "gray20") +
      labs(title = paste("Derivada curvas ICE:", predictor)) +
      theme_bw() +
      theme(legend.position = "bottom",
            axis.title.x = element_blank())
    
  }
  
  datos_sd_derivada <- data.frame(sd = objeto$sd_deriv) %>%
    mutate(!!sym(predictor) := objeto$gridpts)
  
  plot_sd <- ggplot(data = datos_sd_derivada, aes(x = !!sym(predictor), y = sd)) +
    geom_line() +
    labs(title = "Desviación estándar") +
    theme_bw() +
    theme(legend.position = "bottom", 
          plot.title = element_text(size = 12))
  
  plot_combi <- ggpubr::ggarrange(plotlist = list(plot, plot_sd),
                                  nrow = 2,
                                  align = "v",
                                  heights = c(2.5,1))
  plot_combi
  
  return(plot_combi)
}

# EJEMPLOS
################################################################################

# library(MASS)
# library(randomForest)
# library(ICEbox)
# 
# # Entrenamiento del modelo
# datos <- Boston
# modelo_rf <- randomForest(formula = medv ~ ., data = datos, ntree = 500)
# 
# # Se separan los predictores de la variable respuesta
# datos_x      <- datos
# datos_x$medv <- NULL
# datos_y      <- datos$medv
# 
# ice_age <- ice(object = modelo_rf,
#                X = datos_x,
#                y = datos_y,
#                predictor = "age",
#                frac_to_build = 1,
#                verbose = FALSE)
# dice_age <- dice(ice_obj = ice_age)
# 
# plot_ice(objeto = ice_age, pdp = TRUE)
# 
# plot_c_ice(objeto = ice_age, pdp = TRUE)
# 
# # Si la variable no es uno de los predictores originales con los que se entrenó 
# # el modelo, hay que añadirla en el objeto $Xice.
# mediana_habitaciones <- median(x = ice_age$Xice$rm)
# ice_age$Xice$supera_mediana <- ifelse(ice_age$Xice$rm > mediana_habitaciones,
#                                       "si", "no")
# plot_c_ice(objeto = ice_age, pdp = TRUE, color_by = "supera_mediana")
# 
# plot_d_ice(objeto = dice_age, pdp = TRUE)

calcular_graficar_ice <- function(modelo, X, y, predictores = NULL,
                                  frac_to_build = 1, pdp = TRUE,
                                  color_by = NULL, parallel = TRUE){
  if(is.null(predictores)){
    predictores <- colnames(X)
  }
  
  if(any(!purrr::map_lgl(.x = X[, predictores], .f = is.numeric))){
    print("Solo pueden calcularse curvas ICE de predictores numéricos")
    predictores <- predictores[purrr::map_lgl(.x = X[, predictores],
                                              .f = is.numeric)]
  }
  
  if(parallel){
    # Paralelización en múltiples cores.
    future::plan(strategy = future::multiprocess,
                 workers = future::availableCores(constraints = "multicore") - 1)
  }
  
  # Cálculo de curvas ice para cada uno de los predictores.
  curvas_ice_predictores <- furrr::future_map(
    .x = predictores,
    .f = ICEbox::ice,
    object = modelo,
    X = X,
    y = y,
    frac_to_build = frac_to_build,
    verbose = FALSE
  )
  names(curvas_ice_predictores) <- predictores
  
  graficos <- furrr::future_map(
    .x = curvas_ice_predictores,
    .f = plot_ice,
    pdp = pdp,
    color_by = color_by
  )
  
  gridExtra::marrangeGrob(graficos, ncol = 1, nrow = length(predictores))
}

calcular_graficar_c_ice <- function(modelo, X, y, predictores = NULL,
                                    frac_to_build = 1, pdp = TRUE,
                                    color_by = NULL, parallel = TRUE){
  if(is.null(predictores)){
    predictores <- colnames(X)
  }
  
  if(any(!purrr::map_lgl(.x = X[, predictores], .f = is.numeric))){
    print("Solo pueden calcularse curvas ICE de predictores numéricos")
    predictores <- predictores[purrr::map_lgl(.x = X[, predictores],
                                              .f = is.numeric)]
  }
  
  if(parallel){
    # Paralelización en múltiples cores.
    future::plan(strategy = future::multiprocess,
                 workers = future::availableCores(constraints = "multicore") - 1)
  }
  
  # Cálculo de curvas ice para cada uno de los predictores.
  curvas_ice_predictores <- furrr::future_map(
    .x = predictores,
    .f = ICEbox::ice,
    object = modelo,
    X = X,
    y = y,
    frac_to_build = frac_to_build,
    verbose = FALSE
  )
  names(curvas_ice_predictores) <- predictores
  
  graficos <- furrr::future_map(
    .x = curvas_ice_predictores,
    .f = plot_c_ice,
    pdp = pdp,
    color_by = color_by
  )
  
  gridExtra::marrangeGrob(graficos, ncol = 1, nrow = length(predictores))
}

calcular_graficar_d_ice <- function(modelo, X, y, predictores = NULL,
                                    frac_to_build = 1, pdp = TRUE,
                                    color_by = NULL, parallel = TRUE){
  if(is.null(predictores)){
    predictores <- colnames(X)
  }
  
  if(any(!purrr::map_lgl(.x = X[, predictores], .f = is.numeric))){
    print("Solo pueden calcularse curvas ICE de predictores numéricos")
    predictores <- predictores[purrr::map_lgl(.x = X[, predictores],
                                              .f = is.numeric)]
  }
  
  if(parallel){
    # Paralelización en múltiples cores.
    future::plan(strategy = future::multiprocess,
                 workers = future::availableCores(constraints = "multicore") - 1)
  }
  
  # Cálculo de curvas ice para cada uno de los predictores.
  curvas_ice_predictores <- furrr::future_map(
    .x = predictores,
    .f = ICEbox::ice,
    object = modelo,
    X = X,
    y = y,
    frac_to_build = frac_to_build,
    verbose = FALSE
  )
  names(curvas_ice_predictores) <- predictores
  
  # Cálculo de derivadas ice para cada uno de los predictores.
  derivadas_ice_predictores <- furrr::future_map(
    .x = curvas_ice_predictores,
    .f = ICEbox::dice
  )
  
  
  graficos <- furrr::future_map(
    .x = derivadas_ice_predictores,
    .f = plot_d_ice,
    pdp = pdp,
    color_by = color_by
  )
  
  gridExtra::marrangeGrob(graficos, ncol = 1, nrow = length(predictores))
}
