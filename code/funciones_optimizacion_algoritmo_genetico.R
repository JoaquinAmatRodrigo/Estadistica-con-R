################################################################################
#              FUNCIONES OPTIMIZACION CON ALGORITMO GENÉTICO                   #
################################################################################



crear_poblacion <- function(n_poblacion, n_variables, limite_inf = NULL,
                            limite_sup = NULL, verbose = TRUE){
  
  # La siguiente función crea una matriz en la que, cada fila, está formada por 
  # una combinación de valores numéricos aleatorios. El rango de posibles valores
  # para cada variable puede estar estar acotado.
  # 
  # Argumentos:
  #   n_poblacion: número total de individuos de la población.
  #   n_variables: longitud de los individuos.
  #   limite_inf: vector con el límite inferior de cada variable. Si solo se 
  #               quiere imponer límites a algunas variables, emplear NA para
  #               las que no se quiere acotar. 
  #   limite_sup: vector con el límite superior de cada variable. Si solo se 
  #               quiere imponer límites a algunas variables, emplear NA para
  #               las que no se quieren acotar. 
  #   verbose:    mostrar información del proceso por pantalla.
  
  # Retorno: 
  # Una matriz de tamaño n_poblacion x n_variables que representa una población.
  
  # Comprobaciones
  if(!is.null(limite_inf) & (length(limite_inf) != n_variables)){
    stop(paste("limite_inf debe tener un valor por cada variable.",
               "Si para alguna variable no se quiere límite, emplear NA.",
               "Ejemplo: lim_sup = c(10, NA, 10)"))
  }
  
  if(!is.null(limite_sup) & length(limite_sup) != n_variables){
    stop(paste("limite_sup debe tener un valor por cada variable.",
               "Si para alguna variable no se quiere límite, emplear NA.",
               "Ejemplo: lim_sup = c(10, NA, 10)"))
  }
  
  if(is.null(limite_sup) | is.null(limite_inf)){
    warning(paste("Es altamente recomendable indicar los límites dentro de los",
                  "cuales debe buscarse la solución de cada variable.",
                  "Por defecto se emplea [-10^3, 10^3]."))
  }
  
  if(any(any(is.na(limite_sup)), any(is.na(limite_inf)))){
    warning(paste("Los límites empleados por defecto cuando no se han definido son:",
                  " [-10^3, 10^3]."))
    cat("\n")
  }
  
  # Si no se especifica limite_inf, el valor mínimo que pueden tomar las variables
  # es -10^3.
  if(is.null(limite_inf)){
    limite_inf <- rep(x = -10^3, times = n_variables)
  }
  
  # Si no se especifica limite_sup, el valor máximo que pueden tomar las variables
  # es 10^3.
  if(is.null(limite_sup)){
    limite_sup <- rep(x = 10^3, times = n_variables)
  }
  
  # Si los límites no son nulos, se reemplazan aquellas posiciones NA por el valor
  # por defecto -10^3 y 10^3
  if(!is.null(limite_inf)){
    limite_inf[is.na(limite_inf)] <- -10^3
  }
  
  if(!is.null(limite_sup)){
    limite_sup[is.na(limite_sup)] <- 10^3
  }
  
  # Matriz donde almacenar los individuos generados.
  poblacion <- matrix(data = NA, nrow = n_poblacion, ncol = n_variables)
  
  # Bucle para crear cada individuo.   
  for(i in 1:n_poblacion){
    # Se crea un vector de NA que representa el individuo.
    individuo <- rep(NA, times = n_variables)
    
    for(j in 1:n_variables) {
      # Para cada posición, se genera un valor aleatorio dentro del rango permitido
      # para cada variable.
      individuo[j] <- runif(n = 1, min = limite_inf[j], max = limite_sup[j])
    }
    # Se añade el nuevo individuo a la población.
    poblacion[i,] <- individuo
  }
  
  if(verbose){
    print("Población inicial creada")
    print(paste("Número de individuos =", n_poblacion))
    print(paste("Límites inferiores de cada variable:",
                paste(limite_inf, collapse = ", ")))
    print(paste("Límites superiores de cada variable:",
                paste(limite_sup, collapse = ", ")))
    cat("\n")
  }
  
  return(poblacion)
}

calcular_fitness_individuo <- function(individuo, funcion_objetivo, optimizacion,
                                       verbose=TRUE,...){
  # Argumentos
  #   individuo: vector con los valores de cada variable. El orden de los
  #              valores debe coincidir con el de los argumentos de la función.
  #   funcion_objetivo: nombre de la función que se desea optimizar. Debe de haber
  #                     sido definida previamente.
  #   optimizacion: "maximizar" o "minimizar". Dependiendo de esto, la relación 
  #                 del fitness es directamente o indirectamente proporcional al
  #                 valor de la función.
  #   verbose: mostrar información del proceso por pantalla.
  
  # Retorno:
  #   fitness del individuo.
  
  
  # Comprobaciones.
  if(length(individuo) != length(names(formals(funcion_objetivo)))){
    stop(paste("Los individuos deben tener tantos valores como argumentos tiene",
               "la función objetivo."))
  }
  
  # Cálculo fitness.
  if(optimizacion == "maximizar"){
    fitness <- do.call(funcion, args = as.list(individuo))
  }else if(optimizacion == "minimizar"){
    fitness <- 1 - do.call(funcion, args = as.list(individuo))
  }else{
    stop("El argumento optimización debe ser maximizar o minimizar.")
  }
  
  if(verbose){
    print(paste("El fitness calculado para", optimizacion, "es de:", fitness))
    cat("\n")
  }
  return(fitness)
}


calcular_fitness_poblacion <- function(poblacion, funcion_objetivo, optimizacion,
                                       verbose = TRUE, ...){
  # Esta función devuelve el fitness de cada individuo de una población.
  # 
  # Argumentos
  #   poblacion: matriz que representa la población de individuos.
  #   funcion: nombre de la función que se desea optimizar. Debe de haber sido
  #            definida previamente.
  #   optimizacion: "maximizar" o "minimizar". Dependiendo de esto, la relación 
  #                 del fitness es directamente o indirectamente proporcional al
  #                 valor de la función.
  #   verbose:   mostrar información del proceso por pantalla.
  
  
  # Retorno:
  #   vector con el fitness de todos los individuos de la población. El orden de
  #   los valores se corresponde con el orden de las filas de la matriz población.
  
  # Vector donde almacenar el fitness de cada individuo.
  fitness_poblacion <- rep(NA, times = nrow(poblacion))
  
  for (i in 1:nrow(poblacion)) {
    individuo <- poblacion[i, ]
    
    if(verbose){
      print(paste("Individuo", i,":", paste(individuo, collapse = " ")))
    }
    
    fitness_individuo <- calcular_fitness_individuo(
      individuo = individuo,
      funcion_objetivo = funcion_objetivo,
      optimizacion = optimizacion,
      verbose = verbose
    )  
    fitness_poblacion[i] <- fitness_individuo
  }
  
  if(verbose){
    print(paste("Fitness calculado para los",
                nrow(poblacion) ,
                "individuos de la población."))
    cat("\n")
  }
  return(fitness_poblacion)
}

seleccionar_individuo <- function(vector_fitness,  metodo_seleccion = "tournament",
                                  verbose = FALSE){
  # La siguiente función recibe como argumento un vector con el fitness de cada
  # individuo y selecciona una de las posiciones, donde la probabilidad de
  # selección es proporcional al fitness.
  
  # Argumentos:
  #   vector_fitness: un vector con el fitness de cada individuo.
  #   metodo_seleccion: método para establecer la probabilidad de selección. Puede
  #                     ser: "ruleta", "rank", o "tournament".
  #   verbose:   mostrar información del proceso por pantalla.
  #   
  # Retorno:
  #   El índice que ocupa el individuo seleccionado.
  
  if(metodo_seleccion == "ruleta"){
    
    probabilidad_seleccion <- (vector_fitness) / sum(vector_fitness)
    
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

mutar_individuo <- function(individuo, limite_inf, limite_sup,
                            prob_mut = 0.01, distribucion = "uniforme",
                            media_distribucion = 1, sd_distribucion = 1,
                            min_distribucion = -1, max_distribucion = 1){
  # Argumentos:
  #   individuo: vector que representa a un individuo.
  #   prob_mut:  probabilidad que tiene cada posición del vector de mutar.
  #   distribucion: distribución de la que obtener el factor de mutación: Puede
  #                 ser: "normal", "uniforme" o "aleatoria".
  #   media_distribucion: media de la distribución si se selecciona
  #                       distribucion = "normal".
  #   sd_distribucion: desviación estándar de la distribución si se selecciona
  #                    distribucion = "normal".  
  #   min_distribucion: mínimo la distribución si se selecciona
  #                     distribucion = "uniforme". 
  #   max_distribucion: máximo la distribución si se selecciona
  #                     distribucion = "uniforme".                             
  
  # Retorno:
  # Un vector que representa al individuo tras someterse a las mutaciones.
  
  # Selección de posiciones a mutar. 
  posiciones_mutadas <- runif(n = length(individuo), min = 0, max = 1) < prob_mut
  
  # Se modifica el valor de aquellas posiciones que hayan sido seleccionadas para
  # mutar. Si el valor de prob_mut es muy bajo, las mutaciones serán muy poco 
  # frecuentes y el individuo devuelto será casi siempre igual al original.
  
  # Si se emplea distribucion = "uniforme" o distribucion = "normal":
  if(distribucion == "normal" | distribucion == "uniforme"){
    # Se extrae un valor aleatorio de la distribución elegida que se suma 
    # para modificar la/las posiciones mutadas.
    if(distribucion == "normal"){
      factor_mut <- rnorm(n = sum(posiciones_mutadas),
                          mean = media_distribucion,
                          sd = sd_distribucion)
    }
    if(distribucion == "uniforme"){
      factor_mut <- runif(n = sum(posiciones_mutadas),
                          min = min_distribucion,
                          max = max_distribucion)
    }
    
    individuo[posiciones_mutadas] <- individuo[posiciones_mutadas] + factor_mut
    
    # Se comprueba si algún valor mutado supera los límites impuestos. En tal caso
    #  se sobrescribe con el valor del límite correspondiente.
    for (i in which(posiciones_mutadas)) {
      if(individuo[i] < limite_inf[i]){
        individuo[i] <- limite_inf[i]
      }
      if(individuo[i] > limite_sup[i]){
        individuo[i] <- limite_sup[i]
      }
    }
  }else if(distribucion == "aleatoria"){
    
    for(i in which(posiciones_mutadas)) {
      individuo[i] <- runif(n = 1, min = limite_inf[i], max = limite_sup[i])
    }
    
  }else{
    stop(paste("El argumento distribución debe ser: normal, uniforme o aleatoria."))
  }
  
  return(individuo)
}


calcular_fitness_poblacion_paral <- function(poblacion, funcion_objetivo,
                                             optimizacion, n_cores = NULL,
                                             verbose = TRUE, ...){
  
  # Esta función devuelve el fitness de cada individuo de una población.
  # 
  # Argumentos
  #   poblacion: matriz que representa la población de individuos.
  #   funcion:      nombre de la función que se desea optimizar. Debe de haber sido
  #                 definida previamente.
  #   optimizacion: "maximizar" o "minimizar". Dependiendo de esto, la relación del
  #                fitness es directamente o indirectamente proporcional al valor de
  #                la función.
  #   verbose:   mostrar información del proceso por pantalla.
  
  
  # Retorno:
  #   vector con el fitness de todos los individuos de la población. El orden de
  #   los valores se corresponde con el orden de las filas de la matriz población.
  
  # Paquetes necesarios para paralelizar.
  library(furrr)
  library(purrr)
  
  if(is.null(n_cores)){
    future::plan(strategy = future::multiprocess,
                 workers = future::availableCores(constraints = "multicore") - 1)
  }else{
    future::plan(strategy = future::multiprocess,
                 workers = n_cores)
  }
  
  # Se almacena cada individuo (cada fila de la matriz población) como un
  # elemento de una lista.
  poblacion <- purrr::map(.x = 1:nrow(poblacion),
                          .f = function(i){poblacion[i,]})
  
  # Se aplica la función "calcular_fitness_individuo" a cada elemento de la
  # lista población.
  fitness_poblacion <- furrr::future_map_dbl(
    .x = poblacion,
    .f = calcular_fitness_individuo,
    funcion_objetivo = funcion_objetivo,
    optimizacion = optimizacion,
    verbose = verbose)
  
  if(verbose){
    print(paste("Fitness calculado para los",
                nrow(poblacion) ,
                "individuos de la población."))
    cat("\n")
  }
  
  return(unlist(fitness_poblacion))
}

optimizar_ga <- function(
  funcion_objetivo,
  n_variables,
  optimizacion,
  limite_inf = NULL,
  limite_sup = NULL,
  n_poblacion = 20,
  n_generaciones = 10,
  elitismo = 0.1,
  prob_mut = 0.01,
  distribucion = "uniforme",
  media_distribucion = 1,
  sd_distribucion = 1,
  min_distribucion = -1,
  max_distribucion = 1,
  metodo_seleccion = "tournament",
  parada_temprana = FALSE,
  rondas_parada = NULL,
  tolerancia_parada = NULL,
  paralelizado = FALSE,
  n_cores = NULL,
  verbose = FALSE,
  ...){
  
  # COMPROBACIONES INICIALES
  # ============================================================================
  
  # Si se activa la parada temprana, hay que especificar los argumentos
  # rondas_parada y tolerancia_parada.
  if(isTRUE(parada_temprana) & (is.null(rondas_parada)|is.null(tolerancia_parada))){
    stop(paste("Para activar la parada temprana es necesario indicar un valor",
               "de rondas_parada y de tolerancia_parada."))
  }
  
  # ESTABLECER LOS LÍMITES DE BÚSQUEDA SI EL USUARIO NO LO HA HECHO
  # ============================================================================
  if(is.null(limite_sup) | is.null(limite_inf)){
    warning(paste("Es altamente recomendable indicar los límites dentro de los",
                  "cuales debe buscarse la solución de cada variable.",
                  "Por defecto se emplea: [-10^3, 10^3]."))
  }
  
  if(any(is.null(limite_sup), is.null(limite_inf), any(is.na(limite_sup)),
         any(is.na(limite_inf)))){
    warning(paste("Los límites empleados por defecto cuando no se han definido son:",
                  " [-10^3, 10^3]."))
    cat("\n")
  }
  
  # Si no se especifica limite_inf, el valor mínimo que pueden tomar las variables
  # es -10^3.
  if(is.null(limite_inf)){
    limite_inf <- rep(x = -10^3, times = n_variables)
  }
  
  # Si no se especifica limite_sup, el valor máximo que pueden tomar las variables
  # es 10^3.
  if(is.null(limite_sup)){
    limite_sup <- rep(x = 10^3, times = n_variables)
  }
  
  # Si los límites no son nulos, se reemplazan aquellas posiciones NA por el valor
  # por defecto -10^3 y 10^3
  if(!is.null(limite_inf)){
    limite_inf[is.na(limite_inf)] <- -10^3
  }
  
  if(!is.null(limite_sup)){
    limite_sup[is.na(limite_sup)] <- 10^3
  }
  
  
  # ALMACENAMIENTO DE RESULTADOS
  # ============================================================================
  # Por cada generación se almacena el mejor individuo, su fitness, y el porcentaje
  # de mejora respecto a la última generación.
  resultados_fitness     <- vector(mode = "list", length = n_generaciones)
  resultados_individuo   <- vector(mode = "list", length = n_generaciones)
  diferencia_abs         <- vector(mode = "list", length = n_generaciones)
  
  # CREACIÓN DE LA POBLACIÓN INICIAL
  # ============================================================================
  poblacion <- crear_poblacion(n_poblacion = n_poblacion,
                               n_variables = n_variables,
                               limite_inf = limite_inf,
                               limite_sup = limite_sup,
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
      fitness_ind_poblacion <- calcular_fitness_poblacion(
        poblacion = poblacion,
        funcion_objetivo = funcion_objetivo,
        optimizacion = optimizacion,
        verbose = verbose)
    }
    
    if(paralelizado){
      fitness_ind_poblacion <- calcular_fitness_poblacion_paral(
        poblacion = poblacion,
        funcion_objetivo = funcion_objetivo,
        optimizacion = optimizacion,
        verbose = verbose,
        n_cores = n_cores)
    }
    
    # SE ALMACENA EL MEJOR INDIVIDUO DE LA POBLACIÓN ACTUAL
    # ==========================================================================
    fitness_mejor_individuo   <- max(fitness_ind_poblacion)
    mejor_individuo           <- poblacion[which.max(fitness_ind_poblacion), ]
    resultados_fitness[[i]]   <- fitness_mejor_individuo
    resultados_individuo[[i]] <- mejor_individuo
    
    # SE CALCULA LA DIFERENCIA ABSOLUTA RESPECTO A LA GENERACIÓN ANTERIOR
    # ==========================================================================
    # La diferencia solo puede calcularse a partir de la segunda generación.
    if(i > 1){
      diferencia_abs[[i]] <- abs(resultados_fitness[[i-1]]-resultados_fitness[[i]])
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
      indice_parental_1 <- seleccionar_individuo(vector_fitness=fitness_ind_poblacion,
                                                 metodo_seleccion=metodo_seleccion)
      indice_parental_2 <- seleccionar_individuo(vector_fitness=fitness_ind_poblacion,
                                                 metodo_seleccion=metodo_seleccion)
      parental_1 <- poblacion[indice_parental_1, ]
      parental_2 <- poblacion[indice_parental_2, ]
      
      # Cruzar parentales para obtener la descendencia
      descendencia <- cruzar_individuos(parental_1 = parental_1,
                                        parental_2 = parental_2)
      # Mutar la descendencia
      descendencia <- mutar_individuo(individuo = descendencia,
                                      prob_mut = prob_mut,
                                      limite_inf = limite_inf,
                                      limite_sup = limite_sup,
                                      distribucion = distribucion,
                                      media_distribucion = media_distribucion,
                                      sd_distribucion = sd_distribucion,
                                      min_distribucion = min_distribucion,
                                      max_distribucion = max_distribucion) 
      
      nueva_poblacion[j,] <- descendencia 
    }
    poblacion <- nueva_poblacion
    
    # CRITERIO DE PARADA
    # ==========================================================================
    # Si durante las últimas n generaciones la diferencia absoluta entre mejores
    # individuos no es superior al valor de tolerancia_parada, se detiene el
    #  algoritmo y no se crean nuevas generaciones.
    
    if(parada_temprana && (i > rondas_parada)){
      ultimos_n <- tail(unlist(diferencia_abs), n = rondas_parada)
      if(all(ultimos_n < tolerancia_parada)){
        print(paste("Algoritmo detenido en la generacion", i,
                    "por falta cambio mínimo de", tolerancia_parada,
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
  #   diferencia_abs:    una lista con la diferencia absoluta entre el fitness 
  #                      del mejor individuo de generaciones consecutivas.   
  #   df_resultados:     un dataframe con todos los resultados anteriores.          
  
  # Para crear el dataframe se convierten las listas a vectores del mismo tamaño.
  fitness        <- unlist(resultados_fitness)
  predictores    <- resultados_individuo[!sapply(resultados_individuo, is.null)]
  predictores    <- sapply(predictores, function(x){paste(x, collapse = ", ")})
  diferencia_abs <- c(NA, unlist(diferencia_abs))
  
  df_resultados <- data.frame(
    generacion     = seq_along(fitness),
    fitness        = fitness,
    predictores    = predictores,
    diferencia_abs = diferencia_abs
  )
  
  return(list(fitness           = resultados_fitness,
              mejor_individuo   = resultados_individuo,
              diferencia_abs    = diferencia_abs,
              df_resultados     = df_resultados)
  )
}


################################################################################
##                        EJEMPLO                                             ##
################################################################################

# Función objetivo a optimizar.
funcion <- function(x, y){
  sin(y)*exp(1-cos(x))^2 + cos(x)*exp(1-sin(y))^2 + (x-y)^2
}
# 
# 
resultados <- optimizar_ga(
        funcion_objetivo = funcion,
        n_variables = 2,
        optimizacion = "minimizar",
        limite_inf = c(-10, -5),
        limite_sup = c(0, 0),
        n_poblacion = 50,
        n_generaciones = 500,
        elitismo = 0,
        prob_mut = 0.1,
        distribucion = "uniforme",
        min_distribucion = -1,
        max_distribucion = 1,
        parada_temprana = FALSE,
        rondas_parada = NULL,
        tolerancia_parada = NULL,
        paralelizado = FALSE,
        verbose = FALSE
        )