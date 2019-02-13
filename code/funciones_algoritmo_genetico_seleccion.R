################################################################################
#        FUNCIONES ALGORITMO GENÉTICO  PARA SELECCIÓN DE PREDICTORES           #
################################################################################

library(foreach)
library(doParallel)

crear_poblacion <- function(n_poblacion, n_variables, n_max = NULL, n_min = NULL,
                            verbose = TRUE){
  # Argumentos:
  #   n_poblacion: número total de individuos de la población.
  #   n_variables: longitud de los individuos.
  #   n_max:       número máximo de TRUEs que puede contener un individuo.
  #   n_min:       número mínimo de TRUEs que puede contener un individuo.
  #   verbose:     mostrar información del proceso por pantalla.
  
  # Retorno: una matriz de tamaño n_poblacion x n_variables que representa
  #          una población.
  
  # Comprobaciones.
  if(isTRUE(n_max > n_variables)){
    stop("n_max no puede ser mayor que n_variables.")
  }
  
  # Si no se especifica n_max, el número máximo de predictores (TRUEs) que puede
  # contener un individuo es igual al número total de variables disponibles.
  if(is.null(n_max)){
    n_max <- n_variables
  }
  
  # Si no se especifica n_min, el número mínimo de predictores (TRUEs) que puede
  # contener un individuo es 1.
  if(is.null(n_min)){
    n_min <- 1
  }
  
  # Matriz donde almacenar los individuos generados.
  poblacion <- matrix(data = NA, nrow = n_poblacion, ncol = n_variables)
  
  # Bucle para crear cada individuo.   
  for(i in 1:n_poblacion){
    # Se selecciona (con igual probabilidad) el número de valores = TRUE que puede
    # tener el individuo, dentro del rango acotado por n_min y n_max.
    n_true <- sample(x = n_min:n_max, size = 1)
    
    # Se crea un vector con todo FALSE que representa el individuo.
    individuo <- rep(FALSE, times = n_variables)
    
    # Se sustituyen (n_true) posiciones aleatorias por valores TRUE.
    individuo[sample(x = 1:n_variables, size = n_true)] <- TRUE
    
    # Se añade el nuevo individuo a la población.
    poblacion[i,] <- individuo
  }
  
  if(verbose){
    print("Población inicial creada")
    print(paste("Número de individuos =", n_poblacion))
    print(paste("Número de predictores mínimo por individuo =", n_min))
    print(paste("Número de predictores máximo por individuo =", n_max))
    cat("\n")
  }
  
  return(poblacion)
}

calcular_fitness_individuo_rf <- function(x, y, cv, seed=123, verbose=TRUE,...){
  # Argumentos
  #   x:       matriz de predictores.
  #   y:       variable respuesta.
  #   cv:      número de particiones de validación cruzada.
  #   seed:    semilla para garantizar reproducibilidad en el proceso de CV.
  #   verbose: mostrar información del proceso por pantalla.
  
  # Retorno:
  #   fitness del individuo obtenido por validación cruzada empleando
  #   random forest como modelo.
  
  # Repartición de las observaciones para la validación cruzada (CV).
  set.seed(seed)
  indices_cv <- sample(x = 1:cv, size = nrow(x), replace = TRUE)
  
  # Vector para almacenar el fitness de cada iteración CV.
  fitness_cv <- rep(NA, times = cv)
  
  for(i in 1:cv){
    set.seed(seed)
    modelo <- randomForest::randomForest(x = x[indices_cv != i, , drop = FALSE],
                                         y = y[indices_cv != i],
                                         ntree = 100)
    predicciones <- predict(modelo, newdata = x[indices_cv == i, , drop = FALSE])
    
    if(is.numeric(y)){
      # Si y es numérico (regresión), el fitness es igual al -MSE.
      metrica <- "-MSE"
      residuos      <- predicciones - y[indices_cv == i]
      fitness_cv[i] <- -(mean(residuos^2))
    }else{
      # Si y es factor (clasificación), el fitness es igual al Accuracy.
      metrica <- "Accuracy"
      accuracy      <- mean(predicciones == y[indices_cv == i])
      fitness_cv[i] <- accuracy
    }
  }
  
  if(verbose){
    print(paste("El fitness calculado por CV =", cv, "empleando el",
                metrica,"es de:", mean(fitness_cv)))
    cat("\n")
  }
  return(mean(fitness_cv))
}

calcular_fitness_individuo_lm <- function(x, y, cv, seed=123, verbose=TRUE, ...){
  # Argumentos
  #   x:       matriz de predictores.
  #   y:       variable respuesta.
  #   cv:      número de particiones de validación cruzada.
  #   seed:    semilla para garantizar reproducibilidad en el proceso de CV.
  #   verbose: mostrar información del proceso por pantalla.
  
  # Retorno:
  #   fitness del individuo obtenido por validación cruzada empleando MSE como
  #   métrica y regresión lineal como tipo de modelo.
  
  # Repartición de las observaciones para la validación cruzada (CV).
  set.seed(seed)
  indices_cv <- sample(x = 1:cv, size = nrow(x), replace = TRUE)
  
  # Vector para almacenar el fitness de cada iteración CV.
  fitness_cv <- rep(NA, times = cv)
  
  for(i in 1:cv){
    set.seed(seed)
    datos_modelo <- as.data.frame(cbind(x[indices_cv != i, , drop = FALSE],
                                        y = y[indices_cv != i])
    )
    modelo <- lm(formula = y~.,
                 data = datos_modelo)
    
    predicciones <- predict(modelo,
                            newdata = as.data.frame(
                              x[indices_cv == i, , drop = FALSE])
    )
    residuos     <- predicciones - y[indices_cv == i]
    metrica <- "-MSE"
    fitness_cv[i] <- -(mean(residuos^2))
  }
  
  if(verbose){
    print(paste("El fitness calculado por CV =", cv, "empleando el",
                metrica,"es de:", mean(fitness_cv)))
    cat("\n")
  }
  return(mean(fitness_cv))
}


calcular_fitness_poblacion <- function(poblacion, x , y, cv, seed = 123,
                                       modelo = "rf", verbose = TRUE){
  # Argumentos
  #   poblacion: matriz que representa la población de individuos.
  #   x:         matriz de predictores.
  #   y:         variable respuesta.
  #   cv:        número de particiones de validación cruzada.
  #   seed:      semilla para garantizar reproducibilidad en el proceso de CV.
  #   verbose:   mostrar información del proceso por pantalla.
  #   modelo:    tipo de modelo empleado para calcular el fitness. Puede ser
  #              lm o rf.
  
  # Retorno:
  #   vector con el fitness de todos los individuos de la población, obtenido por
  #   validación cruzada. El orden de los valores se corresponde con el orden de
  #   las filas de la matriz población.
  
  # Vector donde almacenar el fitness de cada individuo.
  fitness_poblacion <- rep(NA, times = nrow(poblacion))
  
  # Tipo de modelo utilizado para calcular el fitness.
  if(modelo == "lm"){
    calcular_fitness_individuo <- calcular_fitness_individuo_lm
  }else if(modelo == "rf"){
    calcular_fitness_individuo <- calcular_fitness_individuo_rf
  }else{
    stop(paste("El modelo empleado para calcular el fitness debe ser",
               "lm (linear model)", "o rf (randomforest)."))
  }
  
  for (i in 1:nrow(poblacion)) {
    individuo <- poblacion[i, ]
    
    if(verbose){
      print(paste("Individuo", i,":", paste(individuo, collapse = " ")))
    }
    
    fitness_individuo <- calcular_fitness_individuo(
      x = x[,individuo, drop = FALSE],
      y = y,
      cv = cv,
      seed = seed,
      verbose = verbose
    )
    fitness_poblacion[i] <- fitness_individuo
  }
  
  if(verbose){
    print(paste("Fitness calculado para los",
                nrow(poblacion) ,
                "individuos de la población."))
    print(paste("Modelo empleado para el cálculo del fitness:", modelo))
    metrica_fitness <- ifelse(test = is.numeric(y), "-MSE", "Accuracy")
    print(paste("Métrica empleada para el cálculo del fitness:", metrica_fitness))
    cat("\n")
  }
  return(fitness_poblacion)
}

seleccionar_individuo <- function(vector_fitness, metrica,
                                  metodo_seleccion = "tournament",
                                  verbose = TRUE){
  # Argumentos:
  #   vector_fitness: un vector con el fitness de cada individuo.
  #   metrica:        métrica empleada para calcular el fitness.
  #   metodo_seleccion: método para establecer la probabilidad de selección. Puede
  #                     ser "ruleta", "rank", o "tournament".
  
  # Retorno:
  #   El índice que ocupa el individuo seleccionado.
  
  
  # Selección del individuo
  if(metodo_seleccion == "ruleta"){
    
    if (metrica == "mse") {
      probabilidad_seleccion <- (-1/vector_fitness) / sum(-1/vector_fitness)
    }else if (metrica == "accuracy") {
      probabilidad_seleccion <- (vector_fitness) / sum(vector_fitness)
    }
    
    ind_seleccionado <- sample(x = 1:length(vector_fitness),
                               size = 1,
                               prob = probabilidad_seleccion)
    
  }else if(metodo_seleccion == "rank"){
    
    probabilidad_seleccion <- 1 / order(vector_fitness, decreasing = TRUE)
    
    ind_seleccionado <- sample(x = 1:length(vector_fitness),
                               size = 1,
                               prob = probabilidad_seleccion)
    
  }else if(metodo_seleccion == "tournament"){
    
    # Se seleccionan aleatoriamente dos parejas de individuos.
    ind_candidatos_a <- sample(x = 1: length(vector_fitness), size = 2)
    ind_candidatos_b <- sample(x = 1: length(vector_fitness), size = 2)
    
    # De cada pareja se selecciona el de mayor fitness.
    ind_ganador_a <- ifelse(
      vector_fitness[ind_candidatos_a[1]] > vector_fitness[ind_candidatos_a[2]],
      ind_candidatos_a[1],
      ind_candidatos_a[2]
    )
    ind_ganador_b <- ifelse(
      vector_fitness[ind_candidatos_b[1]] > vector_fitness[ind_candidatos_b[2]],
      ind_candidatos_b[1],
      ind_candidatos_b[2]
    )
    
    # Se comparan los dos ganadores de cada pareja.
    
    ind_seleccionado <- ifelse(
      vector_fitness[ind_ganador_a] > vector_fitness[ind_ganador_b],
      ind_ganador_a,
      ind_ganador_b)
  }else{
    
    stop("El argumento metodo_seleccion debe ser: ruleta, rank o tournament")
  }
  
  if(verbose){
    print(paste("Métrica de selección empleada:", metrica))
    cat("\n")
    print(paste("Método de selección empleado:", metodo_seleccion))
  }
  return(ind_seleccionado)
}

cruzar_individuos <- function(parental_1, parental_2){
  # Esta función devuelve un individuo resultado de cruzar dos individuos
  # parentales con el método de cruzamiento uniforme.
  # 
  # Argumentos:
  #   parental_1: vector que representa a un individuo.
  #   parental_2: vector que representa a un individuo.
  
  # Retorno:
  # Un vector que representa a un nuevo individuo.
  
  # Para crear el nuevo individuo, se emplea el método de cruzamiento uniforme,
  # con la misma probabilidad de que el valor proceda de cada parental.
  
  if(length(parental_1) != length(parental_2)){
    stop(paste0("La longitud de los dos vectores que representan a los ",
                "individuos debe ser la misma."))
  }
  
  # Se crea el vector que representa el nuevo individuo
  descendencia <- rep(NA, times = length(parental_1))
  
  # Se seleccionan aleatoriamente las posiciones que se heredan del parental_1.
  herencia_parent_1 <- sample(x = c(TRUE, FALSE),
                              size = length(parental_1),
                              replace = TRUE)
  # El resto de posiciones se heredan del parental_2.
  herencia_parent_2 <- !herencia_parent_1
  
  descendencia[herencia_parent_1] <- parental_1[herencia_parent_1]
  descendencia[herencia_parent_2] <- parental_2[herencia_parent_2]
  
  return(descendencia)
}


mutar_individuo <- function(individuo, prob_mut = 0.01){
  # Argumentos:
  #   individuo: vector que representa a un individuo.
  #   prob_mut:  probabilidad que tiene cada posición del vector de mutar.
  
  # Retorno:
  # Un vector que representa al individuo tras someterse a las mutaciones.
  
  # Selección de posiciones a mutar. 
  posiciones_mutadas <- runif(n = length(individuo), min = 0, max = 1) < prob_mut
  
  # Se modifica el valor de aquellas posiciones que hayan sido seleccionadas para
  # mutar. Si el valor de prob_mut es muy bajo, las mutaciones serán muy poco 
  # frecuentes y el individuo devuelto será casi siempre igual al original.
  individuo[posiciones_mutadas] <- !(individuo[posiciones_mutadas])
  return(individuo)
}


calcular_fitness_poblacion_paral <- function(poblacion, x , y, cv, seed = 123,
                                             modelo = "rf", verbose = TRUE){
  # Argumentos
  #   poblacion: matriz que representa la población de individuos.
  #   x:         matriz de predictores.
  #   y:         variable respuesta.
  #   cv:        número de particiones de validación cruzada.
  #   seed:      semilla para garantizar reproducibilidad en el proceso de CV.
  #   verbose:   mostrar información del proceso por pantalla.
  #   modelo:    tipo de modelo empleado para calcular el fitness. Puede ser
  #              lm o rf.
  
  # Retorno:
  #   vector con el fitness de todos los individuos de la población, obtenido por
  #   validación cruzada. El orden de los valores se corresponde con el orden de
  #   las filas de la matriz población.
  
  # Tipo de modelo utilizado para calcular el fitness.
  if(modelo == "lm"){
    calcular_fitness_individuo <- calcular_fitness_individuo_lm
  }else if(modelo == "rf"){
    calcular_fitness_individuo <- calcular_fitness_individuo_rf
  }else{
    stop(paste("El modelo empleado para calcular el fitness debe ser",
               "lm (linear model)", "o rf (randomforest)."))
  }
  
  # Se emplean todos los cores del ordenador menos 1.
  registerDoParallel(parallel::detectCores()-1)
  
  # Se emplea la función foreach para paralelizar.
  fitness_poblacion <- foreach::foreach(i = 1:nrow(poblacion)) %dopar% {
    
    individuo <- poblacion[i, ]
    if(verbose){
      print(paste("Individuo", i,":", paste(individuo, collapse = " ")))
    }
    
    fitness_individuo <- calcular_fitness_individuo(
      x = x[,individuo, drop = FALSE],
      y = y,
      cv = cv,
      seed = seed,
      verbose = verbose
    )
    fitness_individuo
  }
  
  if(verbose){
    print(paste("Fitness calculado para los",
                nrow(poblacion) ,
                "individuos de la población."))
    print(paste("Modelo empleado para el cálculo del fitness:", modelo))
    metrica_fitness <- ifelse(test = is.numeric(y), "mse", "accuracy")
    print(paste("Métrica empleada para el cálculo del fitness:", metrica_fitness))
    cat("\n")
  }
  
  # Se vuelve a un único core.
  options(cores = 1)
  
  return(unlist(fitness_poblacion))
}

selecionar_predictores <- function(x, 
                                   y,
                                   n_poblacion = 20,
                                   n_generaciones = 10,
                                   n_max_predictores = NULL,
                                   n_min_predictores = NULL,
                                   modelo = "lm",
                                   cv = 5,
                                   elitismo = 0.1,
                                   prob_mut = 0.01,
                                   metodo_seleccion = "tournament",
                                   verbose = TRUE,
                                   parada_temprana = FALSE,
                                   rondas_parada = NULL,
                                   tolerancia_parada = NULL,
                                   paralelizado = FALSE,
                                   ...
){
  
  # COMPROBACIONES INICIALES
  # ============================================================================
  # Comprobación de que la variable respuesta es numérica si el modelo es lm.
  if(!is.numeric(y) & modelo == "lm"){
    stop(paste("El modelo lm solo puede aplicarse a problemas de regresión,",
               "(variable respuesta numérica)."))
  }
  
  # El número máximo de predictores no puede superar el número de columnas de x.
  if(n_max_predictores > ncol(x)){
    stop(paste("El número máximo de predictores no puede superar al número de",
               "variables disponibles en x."))
  }
  
  # Si se activa la parada temprana, hay que especificar los argumentos
  # rondas_parada y tolerancia_parada.
  if(isTRUE(parada_temprana) & (is.null(rondas_parada)|is.null(tolerancia_parada))){
    stop(paste("Para activar la parada temprana es necesario indicar un valor",
               "de rondas_parada y de tolerancia_parada."))
  }
  
  # ALMACENAMIENTO DE RESULTADOS
  # ============================================================================
  # Por cada generación se almacena el mejor individuo, su fitness, y el porcentaje
  # de mejora respecto a la última generación.
  resultados_fitness     <- vector(mode = "list", length = n_generaciones)
  resultados_individuo   <- vector(mode = "list", length = n_generaciones)
  porcentaje_mejora      <- vector(mode = "list", length = n_generaciones)
  
  # CREACIÓN DE LA POBLACIÓN INICIAL
  # ============================================================================
  poblacion <- crear_poblacion(n_poblacion = n_poblacion,
                               n_variables = ncol(x),
                               n_max = n_max_predictores,
                               n_min = n_min_predictores,
                               verbose = verbose
  )
  # ITERACIÓN DE POBLACIONES
  # ============================================================================
  for (i in 1:n_generaciones) {
    
    if(verbose){
      print("---------------------")
      print(paste("Generación:", i))
      print("---------------------")
    }
    
    
    # CALCULAR FITNESS DE LOS INDIVIDUOS DE LA POBLACIÓN
    # ==========================================================================
    if(!paralelizado){
      fitness_ind_poblacion <- calcular_fitness_poblacion(poblacion = poblacion,
                                                          x = x,
                                                          y = y,
                                                          modelo = modelo,
                                                          cv = cv,
                                                          verbose = verbose)
    }
    
    if(paralelizado){
      fitness_ind_poblacion <- calcular_fitness_poblacion_paral(
        poblacion = poblacion,
        x = x,
        y = y,
        modelo = modelo,
        cv = cv,
        verbose = verbose)
    }
    
    # SE ALMACENA EL MEJOR INDIVIDUO DE LA POBLACIÓN ACTUAL
    # ==========================================================================
    fitness_mejor_individuo   <- max(fitness_ind_poblacion)
    mejor_individuo           <- poblacion[which.max(fitness_ind_poblacion), ]
    resultados_fitness[[i]]   <- fitness_mejor_individuo
    resultados_individuo[[i]] <- colnames(x)[mejor_individuo]
    
    # SE CALCULA LA MEJORA RESPECTO A LA GENERACIÓN ANTERIOR
    # ==========================================================================
    # El porcentaje de mejora solo puede calcularse a partir de la segunda
    # generación.
    if(i > 1){
      porcentaje_mejora[[i]] <- 1 - (resultados_fitness[[i]] /
                                       resultados_fitness[[i-1]])
    }
    
    # NUEVA POBLACIÓN
    # ==========================================================================
    nueva_poblacion <- matrix(data = NA,
                              nrow = nrow(poblacion),
                              ncol = ncol(poblacion))
    
    # ELITISMO
    # ==========================================================================
    # El elitismo indica el porcentaje de mejores individuos de la población 
    # actual que pasan directamente a la siguiente población. De esta forma, se 
    # asegura que, la siguiente generación, no sea nunca inferior.
    
    if(elitismo > 0){
      n_elitismo         <- ceiling(nrow(poblacion)*elitismo)
      posicion_n_mejores <- order(fitness_ind_poblacion, decreasing = TRUE)
      posicion_n_mejores <- posicion_n_mejores[1:n_elitismo]
      nueva_poblacion[1:n_elitismo, ] <- poblacion[posicion_n_mejores, ]
    }else{
      n_elitismo <- 0
    }
    
    # CREACIÓN DE NUEVOS INDIVIDUOS POR CRUCES
    # ==========================================================================
    for (j in (n_elitismo + 1):nrow(nueva_poblacion)) {
      # Seleccionar parentales
      metrica <- ifelse(test = is.numeric(y), "mse", "accuracy")
      indice_parental_1 <- seleccionar_individuo(vector_fitness=fitness_ind_poblacion,
                                                 metrica=metrica,
                                                 metodo_seleccion = metodo_seleccion,
                                                 verbose = verbose)
      indice_parental_2 <- seleccionar_individuo(vector_fitness=fitness_ind_poblacion,
                                                 metrica=metrica,
                                                 metodo_seleccion = metodo_seleccion,
                                                 verbose = verbose)
      parental_1 <- poblacion[indice_parental_1, ]
      parental_2 <- poblacion[indice_parental_2, ]
      
      # Cruzar parentales para obtener la descendencia
      descendencia <- cruzar_individuos(parental_1 = parental_1,
                                        parental_2 = parental_2)
      # Mutar la descendencia
      descendencia <- mutar_individuo(individuo = descendencia,
                                      prob_mut = prob_mut) 
      
      # Almacenar el nuevo descendiente en la nueva población: puede ocurrir que
      # el individuo resultante del cruce y posterior mutación no contenga ningún
      # predictor (todas sus posiciones son FALSE). Si esto ocurre, y para evitar
      # que se produzca un error al ajustar el modelo, se introduce aleatoriamente
      # un valor TRUE.
      if(all(descendencia == FALSE)){
        descendencia[sample(x = 1:length(descendencia), size = 1)] <- TRUE
      }
      nueva_poblacion[j,] <- descendencia 
    }
    poblacion <- nueva_poblacion
    
    # CRITERIO DE PARADA
    # ==========================================================================
    # Si durante las últimas n generaciones el mejor individuo no ha mejorado por
    # una cantidad superior a tolerancia_parada, se detiene el algoritmo y no se
    # crean nuevas generaciones.
    
    if(parada_temprana && (i > rondas_parada)){
      ultimos_n <- tail(unlist(porcentaje_mejora), n = rondas_parada)
      if(all(ultimos_n < tolerancia_parada)){
        print(paste("Algoritmo detenido en la generacion", i,
                    "por falta de mejora mínima de", tolerancia_parada,
                    "durante", rondas_parada,
                    "generaciones consecutivas."))
        break()
      }
    }
  }
  
  # RESULTADOS
  # ============================================================================
  # La función devuelve una lista con 4 elementos:
  #   fitness:           una lista con el fitness del mejor individuo de cada
  #                      generación.
  #   mejor_individuo:   una lista con la combinación de predictores  del mejor
  #                      individuo de cada generación.
  #   porcentaje_mejora: una lista con el porcentaje de mejora entre el mejor
  #                      individuo de cada generación.    
  #   df_resultados:     un dataframe con todos los resultados anteriores.          
  
  # Para crear el dataframe se convierten las listas a vectores del mismo tamaño.
  fitness     <- unlist(resultados_fitness)
  predictores <- resultados_individuo[!sapply(resultados_individuo, is.null)]
  predictores <- sapply(predictores, function(x){paste(x, collapse = ", ")})
  mejora      <- c(NA, unlist(porcentaje_mejora))
  
  df_resultados <- data.frame(
    generacion   = seq_along(fitness),
    fitness      = fitness,
    predictores  = predictores,
    mejora       = mejora
  )
  
  return(list(fitness           = resultados_fitness,
              mejor_individuo   = resultados_individuo,
              porcentaje_mejora = porcentaje_mejora,
              df_resultados     = df_resultados)
  )
}
