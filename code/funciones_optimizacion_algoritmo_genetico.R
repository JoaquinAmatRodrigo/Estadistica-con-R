################################################################################
#       FUNCIONES OPTIMIZACION CON ALGORITMO GENÉTICO Y NELDER-MEAD            #
#                                                                              #
# This work by Joaquín Amat Rodrigo is licensed under a Creative Commons       #
# Attribution 4.0 International License.                                       #
################################################################################

crear_poblacion <- function(n_poblacion, n_variables, limite_inf = NULL,
                            limite_sup = NULL, verbose = TRUE) {
  
  # Esta función crea una matriz en la que, cada fila, está formada por una
  # combinación de valores numéricos aleatorios. El rango de posibles valores
  # para cada variable puede estar acotado.
  #
  # ARGUMENTOS
  # ============================================================================
  # n_poblacion: número total de individuos de la población.
  # n_variables: longitud de los individuos.
  # limite_inf:  vector con el límite inferior de cada variable. Si solo se
  #              quiere imponer límites a algunas variables, emplear NA para
  #              las que no se quiere acotar.
  # limite_sup:  vector con el límite superior de cada variable. Si solo se
  #              quiere imponer límites a algunas variables, emplear NA para
  #              las que no se quieren acotar.
  # verbose:     mostrar información del proceso por pantalla.
  #   
  # RETORNO
  # ============================================================================
  # Una matriz de tamaño n_poblacion x n_variables que representa una población.
  
  # COMPROBACIONES
  # ----------------------------------------------------------------------------
  if (!is.null(limite_inf) & (length(limite_inf) != n_variables)) {
    stop(paste(
      "limite_inf debe tener un valor por cada variable.",
      "Si para alguna variable no se quiere límite, emplear NA.",
      "Ejemplo: lim_sup = c(10, NA, 10)"
    ))
  } else if (!is.null(limite_sup) & length(limite_sup) != n_variables) {
    stop(paste(
      "limite_sup debe tener un valor por cada variable.",
      "Si para alguna variable no se quiere límite, emplear NA.",
      "Ejemplo: lim_sup = c(10, NA, 10)"
    ))
  } else if (is.null(limite_sup) | is.null(limite_inf)) {
    warning(paste(
      "Es altamente recomendable indicar los límites dentro de los",
      "cuales debe buscarse la solución de cada variable.",
      "Por defecto se emplea [-10^3, 10^3]."
    ))
  } else if (any(any(is.na(limite_sup)), any(is.na(limite_inf)))) {
    warning(paste(
      "Los límites empleados por defecto cuando no se han definido son:",
      " [-10^3, 10^3]."
    ))
    cat("\n")
  }
  
  # Si no se especifica limite_inf, el valor mínimo que pueden tomar las variables
  # es -10^3.
  if (is.null(limite_inf)) {
    limite_inf <- rep(x = -10^3, times = n_variables)
  }
  
  # Si no se especifica limite_sup, el valor máximo que pueden tomar las variables
  # es 10^3.
  if (is.null(limite_sup)) {
    limite_sup <- rep(x = 10^3, times = n_variables)
  }
  
  # Si los límites no son nulos, se reemplazan aquellas posiciones NA por el valor
  # por defecto -10^3 y 10^3
  if (!is.null(limite_inf)) {
    limite_inf[is.na(limite_inf)] <- -10^3
  }
  
  if (!is.null(limite_sup)) {
    limite_sup[is.na(limite_sup)] <- 10^3
  }
  
  # CREAR POBLACIÓN
  # ----------------------------------------------------------------------------
  # Matriz donde almacenar los individuos generados.
  poblacion <- matrix(data = NA, nrow = n_poblacion, ncol = n_variables)
  
  # Bucle para crear cada individuo.
  for (i in 1:n_poblacion) {
    # Se crea un vector de NA que representa el individuo.
    individuo <- rep(NA, times = n_variables)
    
    for (j in 1:n_variables) {
      # Para cada posición, se genera un valor aleatorio dentro del rango permitido
      # para cada variable.
      individuo[j] <- runif(n = 1, min = limite_inf[j], max = limite_sup[j])
    }
    # Se añade el nuevo individuo a la población.
    poblacion[i, ] <- individuo
  }
  
  # INFORMACIÓN ALMACENADA EN LOS ATRIBUTOS
  # ----------------------------------------------------------------------------
  attr(poblacion, 'fecha_creacion')    <- Sys.time()
  attr(poblacion, 'numero_individuos') <- n_poblacion
  attr(poblacion, "class") <- c("matrix", "poblacion")
  
  if (verbose) {
    cat("Población inicial creada", "\n")
    cat("------------------------", "\n")
    cat("Fecha creación:", as.character(Sys.time()), "\n")
    cat("Número de individuos =", n_poblacion, "\n")
    cat("Límites inferiores de cada variable =", paste(limite_inf, collapse = ", "), "\n")
    cat("Límites superiores de cada variable =", paste(limite_sup, collapse = ", "), "\n")
    cat("\n")
  }
  
  return(poblacion)
}


calcular_fitness_individuo <- function(individuo, funcion_objetivo, optimizacion,
                                       verbose = TRUE, ...) {
  # Esta función devuelve el fitness de cada individuo de una población.
  #
  # ARGUMENTOS
  # ============================================================================
  # individuo:        vector con los valores de cada variable. El orden de los
  #                   valores debe coincidir con el de los argumentos de la
  #                   función.
  # funcion_objetivo: nombre de la función que se desea optimizar. Debe de haber
  #                   sido definida previamente.
  # optimizacion:    "maximizar" o "minimizar". Dependiendo de esto, la relación
  #                   del fitness es directamente o indirectamente proporcional
  #                   al valor de la función.
  # verbose:          mostrar información del proceso por pantalla.
  #
  # RETORNO
  # ============================================================================
  # fitness del individuo.
  
  # COMPROBACIONES INICIALES
  # ----------------------------------------------------------------------------
  if (length(individuo) != length(names(formals(funcion_objetivo)))) {
    stop(paste("Los individuos deben tener tantos valores como argumentos tiene",
               "la función objetivo."))
  }
  
  # CÁLCULO FITNESS
  # ----------------------------------------------------------------------------
  if (optimizacion == "maximizar") {
    fitness <- do.call(funcion_objetivo, args = as.list(individuo))
  } else if (optimizacion == "minimizar") {
    fitness <- -(do.call(funcion_objetivo, args = as.list(individuo)))
  } else {
    stop("El argumento optimización debe ser maximizar o minimizar.")
  }
  
  # INFORMACIÓN DEL PROCESO (VERBOSE)
  # ----------------------------------------------------------------------------
  if (verbose) {
    cat("El individuo ha sido evaluado", "\n")
    cat("-----------------------------", "\n")
    cat("Optimización =", optimizacion, "\n")
    cat("Individuo    =", paste(individuo, collapse = " "), "\n")
    cat("Fitness      =", fitness, "\n")
    cat("\n")
  }
  
  return(fitness)
}

calcular_fitness_poblacion <- function(poblacion, funcion_objetivo, optimizacion,
                                       verbose = TRUE, ...) {
  # Esta función devuelve el fitness de cada individuo de una población.
  #
  # ARGUMENTOS
  # ============================================================================
  # poblacion:        matriz que representa la población de individuos.
  # funcion_objetivo: nombre de la función que se desea optimizar. Debe de haber
  #                   sido definida previamente.
  # optimizacion:     "maximizar" o "minimizar". Dependiendo de esto, la relación
  #                   del fitness es directamente o indirectamente proporcional
  #                   al valor de la función.
  # verbose:          mostrar información del proceso por pantalla.
  #
  # RETORNO
  # ============================================================================
  # Vector con el fitness de todos los individuos de la población. El orden de
  # los valores se corresponde con el orden de las filas de la matriz población.
  
  
  # CÁLCULO DEL FITNESS DE CADA INDIVIDUO DE LA POBLACIÓN
  # ----------------------------------------------------------------------------
  # Vector donde almacenar el fitness de cada individuo.
  fitness_poblacion <- rep(NA, times = nrow(poblacion))
  
  for (i in 1:nrow(poblacion)) {
    individuo <- poblacion[i, ]
    
    fitness_individuo <- calcular_fitness_individuo(
      individuo        = individuo,
      funcion_objetivo = funcion_objetivo,
      optimizacion     = optimizacion,
      verbose          = verbose
    )
    fitness_poblacion[i] <- fitness_individuo
  }
  
  # MEJOR INDIVIDUO DE LA POBLACIÓN
  # ----------------------------------------------------------------------------
  # Se identifica el mejor individuo de toda la población, el de mayor
  # fitness.
  indice_mejor_individuo <- which.max(fitness_poblacion)
  
  # Se identifica el valor de la función objetivo para el mejor individuo.
  if (optimizacion == "maximizar") {
    valor_funcion <- fitness_poblacion[indice_mejor_individuo]
  } else {
    valor_funcion <- -1*fitness_poblacion[indice_mejor_individuo]
  }
  
  # INFORMACIÓN DEL PROCESO (VERBOSE)
  # ----------------------------------------------------------------------------
  if (verbose) {
    cat("------------------", "\n")
    cat("Población evaluada", "\n")
    cat("------------------", "\n")
    cat("Optimización              =", optimizacion, "\n")
    cat("Mejor fitness encontrado  =", fitness_poblacion[indice_mejor_individuo], "\n")
    cat("Mejor solución encontrada =",
        paste(poblacion[indice_mejor_individuo,], collapse = " "), "\n")
    cat("Valor función objetivo    =", valor_funcion, "\n")
    cat("\n")
  }
  
  return(fitness_poblacion)
}

seleccionar_individuo <- function(vector_fitness, metodo_seleccion = "tournament",
                                  verbose = FALSE) {
  # Esta función recibe como argumento un vector con el fitness de cada individuo
  # y selecciona una de las posiciones, donde la probabilidad de selección es
  # proporcional al fitness.
  
  # ARGUMENTOS
  # ============================================================================
  # vector_fitness:   un vector con el fitness de cada individuo.
  # metodo_seleccion: método para establecer la probabilidad de selección. Puede
  #                   ser: "ruleta", "rank", o "tournament".
  # verbose:          mostrar información del proceso por pantalla.
  #
  # RETORNO
  # ============================================================================
  # El índice que ocupa el individuo seleccionado.
  
  # COMPROBACIONES INICIALES
  # ---------------------------------------------------------------------------
  if (!metodo_seleccion %in% c("ruleta", "rank", "tournament")) {
    stop("El método de selección debe de ser ruleta, rank o tournament.")
  }
  
  # SELECCIÓN DE INDIVIDUOS
  # ----------------------------------------------------------------------------
  # Se calcula la probabilidad de selección de cada individuo en función
  # de su fitness.
  
  if (metodo_seleccion == "ruleta") {
    probabilidad_seleccion <- (vector_fitness) / sum(vector_fitness)
    
    ind_seleccionado <- sample(
      x    = 1:length(vector_fitness),
      size = 1,
      prob = probabilidad_seleccion
    )
  } else if (metodo_seleccion == "rank") {
    probabilidad_seleccion <- 1 / rank(-vector_fitness)
    
    ind_seleccionado <- sample(
      x    = 1:length(vector_fitness),
      size = 1,
      prob = probabilidad_seleccion
    )
  } else if (metodo_seleccion == "tournament") {
    
    # Se seleccionan aleatoriamente dos parejas de individuos.
    ind_candidatos_a <- sample(x = 1:length(vector_fitness), size = 2)
    ind_candidatos_b <- sample(x = 1:length(vector_fitness), size = 2)
    
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
      ind_ganador_b
    )
  }
  
  # INFORMACIÓN DEL PROCESO (VERBOSE)
  # ----------------------------------------------------------------------------
  if (verbose) {
    cat("----------------------", "\n")
    cat("Individuo seleccionado", "\n")
    cat("----------------------", "\n")
    cat("Método selección    =", metodo_seleccion, "\n")
    cat("Índice seleccionado =", ind_seleccionado, "\n")
  }
  
  return(ind_seleccionado)
}

cruzar_individuos <- function(parental_1,
                              parental_2,
                              metodo_cruce = "uniforme",
                              verbose = TRUE) {
  # Esta función devuelve un individuo resultado de cruzar dos individuos
  # parentales con el método de cruzamiento uniforme o punto simple.
  #
  # ARGUMENTOS
  # ============================================================================
  # parental_1: vector que representa a un individuo.
  # parental_2: vector que representa a un individuo.
  # metodo_cruce: estrategia de cruzamiento (uniforme", "punto_simple")
  
  # RETORNO
  # ============================================================================
  # Un vector que representa a un nuevo individuo.
  
  
  # COMPROBACIONES INICIALES
  # ----------------------------------------------------------------------------
  if (length(parental_1) != length(parental_2)) {
    stop(paste0(
      "La longitud de los dos vectores que representan a los ",
      "individuos debe ser la misma."
    ))
  }
  if (!(metodo_cruce %in% c("uniforme", "punto_simple"))) {
    stop("El método de cruzamiento debe ser: uniforme o punto_simple.")
  }
  
  # CRUCE
  # ----------------------------------------------------------------------------
  # Se crea el vector que representa el nuevo individuo
  descendencia <- rep(NA, times = length(parental_1))
  
  if (metodo_cruce == "uniforme") {
    # Se seleccionan aleatoriamente las posiciones que se heredan del parental_1.
    herencia_parent_1 <- sample(
      x       = c(TRUE, FALSE),
      size    = length(parental_1),
      replace = TRUE
    )
    # El resto de posiciones se heredan del parental_2.
    herencia_parent_2 <- !herencia_parent_1
    
    descendencia[herencia_parent_1] <- parental_1[herencia_parent_1]
    descendencia[herencia_parent_2] <- parental_2[herencia_parent_2]
  } else {
    punto_cruce <- sample(
      x    = 2:length(parental_1),
      size = 1
    )
    descendencia <- c(
      parental_1[1:(punto_cruce - 1)],
      parental_2[punto_cruce:length(parental_1)]
    )
  }
  
  # INFORMACIÓN DEL PROCESO (VERBOSE)
  # ----------------------------------------------------------------------------
  if (verbose) {
    cat("---------------", "\n")
    cat("Cruce realizado", "\n")
    cat("---------------", "\n")
    cat("Método =", metodo_cruce, "\n")
    cat("Parental 1 = ", "\n")
    cat(parental_1, "\n")
    cat("Parental 2 = ", "\n")
    cat(parental_2, "\n")
    cat("Descendencia = ", "\n")
    cat(descendencia, "\n")
    cat("\n")
  }
  return(descendencia)
}

mutar_individuo <- function(individuo, limite_inf, limite_sup,
                            prob_mut = 0.01, distribucion = "uniforme",
                            media_distribucion = 1, sd_distribucion = 1,
                            min_distribucion = -1, max_distribucion = 1,
                            verbose = TRUE) {
  # ARGUMENTOS
  # =============================================================================
  # individuo: vector que representa a un individuo.
  # prob_mut:  probabilidad que tiene cada posición del individuo de mutar.
  # distribucion: distribución de la que obtener el factor de mutación. Puede
  #               ser: "normal", "uniforme" o "aleatoria".
  # media_distribucion: media de la distribución si se selecciona
  #                     distribucion = "normal".
  # sd_distribucion:    desviación estándar de la distribución si se selecciona
  #                     distribucion = "normal".
  # min_distribucion:   mínimo la distribución si se selecciona
  #                     distribucion = "uniforme".
  # max_distribucion:   máximo la distribución si se selecciona
  #                     distribucion = "uniforme".
  # 
  # RETORNO
  # ============================================================================
  # Un vector que representa al individuo tras someterse a las mutaciones.
  
  # COMPROBACIONES INICIALES
  # ----------------------------------------------------------------------------
  if (!(distribucion %in% c("normal", "uniforme", "aleatoria"))) {
    stop("El argumento distribución debe ser: normal, uniforme o aleatoria.")
  }
  
  # CRUCE
  # ----------------------------------------------------------------------------
  # Selección de posiciones a mutar.
  posiciones_mutadas <- runif(n = length(individuo), min = 0, max = 1) < prob_mut
  
  # Se modifica el valor de aquellas posiciones que hayan sido seleccionadas para
  # mutar. Si el valor de prob_mut es muy bajo, las mutaciones serán muy poco
  # frecuentes y el individuo devuelto será casi siempre igual al original.
  
  # Si se emplea distribucion = "uniforme" o distribucion = "normal":
  if (distribucion == "normal" | distribucion == "uniforme") {
    # Se extrae un valor aleatorio de la distribución elegida que se suma
    # para modificar la/las posiciones mutadas.
    if (distribucion == "normal") {
      factor_mut <- rnorm(
        n = sum(posiciones_mutadas),
        mean = media_distribucion,
        sd = sd_distribucion
      )
    }
    if (distribucion == "uniforme") {
      factor_mut <- runif(
        n = sum(posiciones_mutadas),
        min = min_distribucion,
        max = max_distribucion
      )
    }
    
    individuo[posiciones_mutadas] <- individuo[posiciones_mutadas] + factor_mut
    
    # Se comprueba si algún valor mutado supera los límites impuestos. En tal caso
    #  se sobrescribe con el valor del límite correspondiente.
    for (i in which(posiciones_mutadas)) {
      if (individuo[i] < limite_inf[i]) {
        individuo[i] <- limite_inf[i]
      }
      if (individuo[i] > limite_sup[i]) {
        individuo[i] <- limite_sup[i]
      }
    }
  } else if (distribucion == "aleatoria") {
    for (i in which(posiciones_mutadas)) {
      individuo[i] <- runif(n = 1, min = limite_inf[i], max = limite_sup[i])
    }
  }
  
  # INFORMACIÓN DEL PROCESO (VERBOSE)
  # ----------------------------------------------------------------------------
  if (verbose) {
    cat("-----------------", "\n")
    cat("Individuo mutado", "\n")
    cat("-----------------", "\n")
    cat("Probabilidad =", prob_mut, "\n")
    cat("Individuo    = ", individuo, "\n")
    cat("\n")
  }
  
  return(individuo)
}

calcular_fitness_poblacion_paral <- function(poblacion, funcion_objetivo,
                                             optimizacion, n_cores = NULL,
                                             verbose = TRUE, ...) {
  
  # Esta función devuelve el fitness de cada individuo de una población.
  #
  # ARGUMENTOS
  # ============================================================================
  # poblacion: matriz que representa la población de individuos.
  # funcion_objetivo: nombre de la función que se desea optimizar. Debe de haber
  #                   sido definida previamente.
  # optimizacion:     "maximizar" o "minimizar". Dependiendo de esto, la relación
  #                   del fitness es directamente o indirectamente proporcional
  #                   al valor de la función.
  # verbose:          mostrar información del proceso por pantalla.
  
  # RETORNO
  # ============================================================================
  # vector con el fitness de todos los individuos de la población. El orden de
  # los valores se corresponde con el orden de las filas de la matriz población.
  
  # Paquetes necesarios para paralelizar.
  library("doFuture")
  
  # CÁLCULO DEL FITNESS DE CADA INDIVIDUO DE LA POBLACIÓN
  # ----------------------------------------------------------------------------
  registerDoFuture()
  
  if (is.null(n_cores)) {
    future::plan(
      strategy = multiprocess,
      workers  = future::availableCores() - 1
    )
  } else {
    future::plan(
      strategy = multiprocess,
      workers = n_cores
    )
  }
  
  fitness_poblacion <- foreach(i = 1:nrow(poblacion), .combine = c) %dopar% {
    calcular_fitness_individuo(
      individuo = poblacion[i, ],
      funcion_objetivo = funcion_objetivo,
      optimizacion     = optimizacion,
      verbose          = verbose
    )
  }
  
  # MEJOR INDIVIDUO DE LA POBLACIÓN
  # ----------------------------------------------------------------------------
  # Se identifica el mejor individuo de toda la población, el de mayor
  # fitness.
  indice_mejor_individuo <- which.max(fitness_poblacion)
  
  # Se identifica el valor de la función objetivo para el mejor individuo.
  if (optimizacion == "maximizar") {
    valor_funcion <- fitness_poblacion[indice_mejor_individuo]
  } else {
    valor_funcion <- -1*fitness_poblacion[indice_mejor_individuo]
  }
  
  # INFORMACIÓN DEL PROCESO (VERBOSE)
  # ----------------------------------------------------------------------------
  if (verbose) {
    cat("------------------", "\n")
    cat("Población evaluada", "\n")
    cat("------------------", "\n")
    cat("Optimización              =", optimizacion, "\n")
    cat("Mejor fitness encontrado  =", fitness_poblacion[indice_mejor_individuo], "\n")
    cat("Mejor solución encontrada =",
        paste(poblacion[indice_mejor_individuo,], collapse = " "), "\n")
    cat("Valor función objetivo    =", valor_funcion, "\n")
    cat("\n")
  }
  return(unlist(fitness_poblacion))
}

optimizar_ga <- function(
  funcion_objetivo,
  n_variables,
  optimizacion,
  limite_inf         = NULL,
  limite_sup         = NULL,
  n_poblacion        = 20,
  n_generaciones     = 50,
  elitismo           = 0.1,
  prob_mut           = 0.01,
  distribucion       = "uniforme",
  media_distribucion = 1,
  sd_distribucion    = 1,
  min_distribucion   = -1,
  max_distribucion   = 1,
  metodo_seleccion   = "tournament",
  metodo_cruce       = "uniforme",
  parada_temprana    = FALSE,
  rondas_parada      = NULL,
  tolerancia_parada  = NULL,
  Nelder_Mead        = TRUE,
  paralelizado       = FALSE,
  n_cores            = NULL,
  verbose            = 1,
  ...) {
  
  # ARGUMENTOS
  # =============================================================================
  # funcion_objetivo: nombre de la función que se desea optimizar. Debe de haber
  #                   sido definida previamente.
  # n_variables:      longitud de los individuos.
  # optimizacion:     "maximizar" o "minimizar". Dependiendo de esto, la relación
  #                   del fitness es directamente o indirectamente proporcional al
  #                   valor de la función.
  # limite_inf:       vector con el límite inferior de cada variable. Si solo se
  #                   quiere imponer límites a algunas variables, emplear NA para
  #                   las que no se quiere acotar.
  # limite_sup:       vector con el límite superior de cada variable. Si solo se
  #                   quiere imponer límites a algunas variables, emplear NA para
  #                   las que no se quieren acotar.
  # n_poblacion:      número total de individuos de la población.
  # n_generaciones:   número total de generaciones creadas.
  # elitismo:         porcentaje de mejores individuos de la población actual que
  #                   pasan directamente a la siguiente población.
  # prob_mut:         probabilidad que tiene cada posición del individuo de mutar.
  # distribucion:     distribución de la que obtener el factor de mutación. Puede
  #                   ser: "normal", "uniforme" o "aleatoria".
  # media_distribucion: media de la distribución si se selecciona distribucion="normal".
  # sd_distribucion:  desviación estándar de la distribución si se selecciona
  #                   distribucion="normal".
  # min_distribucion: mínimo la distribución si se selecciona distribucion="uniforme".
  # max_distribucion: máximo la distribución si se selecciona distribucion="uniforme".
  # metodo_seleccion: método para establecer la probabilidad de selección. Puede
  #                   ser: "ruleta", "rank" o "tournament".
  # metodo_seleccion: método para cruzar los individuos. Puede ser: "uniforme",
  #                  "punto_simple".
  # parada_temprana:  si durante las últimas "rondas_parada" generaciones la diferencia
  #                   absoluta entre mejores individuos no es superior al valor de
  #                  "tolerancia_parada", se detiene el algoritmo y no se crean
  #                   nuevas generaciones.
  # rondas_parada:    número de generaciones consecutivas sin mejora mínima para que
  #                   se active la parada temprana.
  # tolerancia_parada: valor mínimo que debe tener la diferencia de generaciones
  #                    consecutivas para considerar que hay cambio.
  # Nelder_Mead:      TRUE para que el mejor individuo devuelto por el algoritmo
  #                   genético se intente mejorar con optimización "Nelder_Mead".
  # paralelizado:     TRUE para paralelizar el algoritmo genético.
  # n_cores:          número de cores para la paralelización.
  # verbose:          Nivel de detalle para que se imprima por pantalla el 
  #                   resultado de cada paso del algoritmo (0, 1, 2)
  
  # RETORNO
  # =============================================================================
  # La función devuelve una lista con 5 elementos:
  # fitness:            una lista con el fitness del mejor individuo de cada
  #                     generación.
  # mejores_individuos: una lista con la combinación de predictores del mejor
  #                     individuo de cada generación.
  # mejor_individuo:    combinación de predictores del mejor individuo encontrado
  #                     en todo el proceso.
  # diferencia_abs:     una lista con la diferencia absoluta entre el fitness
  #                     del mejor individuo de generaciones consecutivas.
  # df_resultados:      un dataframe con todos los resultados anteriores.
  
  start_time <- Sys.time()
  
  # COMPROBACIONES INICIALES
  # ----------------------------------------------------------------------------
  # Si se activa la parada temprana, hay que especificar los argumentos
  # rondas_parada y tolerancia_parada.
  if (isTRUE(parada_temprana) &
      (is.null(rondas_parada) | is.null(tolerancia_parada)) ) {
    stop(paste(
      "Para activar la parada temprana es necesario indicar un valor",
      "de rondas_parada y de tolerancia_parada."
    ))
  }
  
  # ESTABLECER LOS LÍMITES DE BÚSQUEDA SI EL USUARIO NO LO HA HECHO
  # ----------------------------------------------------------------------------
  if (is.null(limite_sup) | is.null(limite_inf)) {
    warning(paste(
      "Es altamente recomendable indicar los límites dentro de los",
      "cuales debe buscarse la solución de cada variable.",
      "Por defecto se emplea: [-10^3, 10^3]."
    ))
  }
  
  if (any(
    is.null(limite_sup), is.null(limite_inf), any(is.na(limite_sup)),
    any(is.na(limite_inf))
  )) {
    warning(paste(
      "Los límites empleados por defecto cuando no se han definido son:",
      " [-10^3, 10^3]."
    ))
    cat("\n")
  }
  
  # Si no se especifica limite_inf, el valor mínimo que pueden tomar las variables
  # es -10^3.
  if (is.null(limite_inf)) {
    limite_inf <- rep(x = -10^3, times = n_variables)
  }
  
  # Si no se especifica limite_sup, el valor máximo que pueden tomar las variables
  # es 10^3.
  if (is.null(limite_sup)) {
    limite_sup <- rep(x = 10^3, times = n_variables)
  }
  
  # Si los límites no son nulos, se reemplazan aquellas posiciones NA por el valor
  # por defecto -10^3 y 10^3.
  if (!is.null(limite_inf)) {
    limite_inf[is.na(limite_inf)] <- -10^3
  }
  
  if (!is.null(limite_sup)) {
    limite_sup[is.na(limite_sup)] <- 10^3
  }
  
  
  # ALMACENAMIENTO DE RESULTADOS
  # ----------------------------------------------------------------------------
  # Por cada generación se almacena, la población, el mejor individuo, su fitness,
  # y la diferencia absoluta respecto a la última generación.
  poblaciones          <- vector(mode = "list", length = n_generaciones)
  resultados_fitness   <- vector(mode = "list", length = n_generaciones)
  resultados_individuo <- vector(mode = "list", length = n_generaciones)
  diferencia_abs       <- vector(mode = "list", length = n_generaciones)
  
  # ITERACIÓN DE POBLACIONES
  # ----------------------------------------------------------------------------
  for (i in 1:n_generaciones) {
    if (verbose %in% c(1,2)) {
      cat("-------------------", "\n")
      cat("Generación:", paste0(i, "\\", n_generaciones), "\n")
      cat("-------------------", "\n")
    }
    
    if (i == 1) {
      # CREACIÓN DE LA POBLACIÓN INICIAL
      # ------------------------------------------------------------------------
      poblacion <- crear_poblacion(
        n_poblacion = n_poblacion,
        n_variables = n_variables,
        limite_inf  = limite_inf,
        limite_sup  = limite_sup,
        verbose     = verbose %in% c(2)
      )
    }
    
    # CALCULAR FITNESS DE LOS INDIVIDUOS DE LA POBLACIÓN
    # --------------------------------------------------------------------------
    if (!paralelizado) {
      fitness_ind_poblacion <- calcular_fitness_poblacion(
        poblacion        = poblacion,
        funcion_objetivo = funcion_objetivo,
        optimizacion     = optimizacion,
        verbose          = verbose %in% c(2)
      )
    }
    
    if (paralelizado) {
      fitness_ind_poblacion <- calcular_fitness_poblacion_paral(
        poblacion        = poblacion,
        funcion_objetivo = funcion_objetivo,
        optimizacion     = optimizacion,
        n_cores          = n_cores,
        verbose          = verbose %in% c(2)
      )
    }
    
    # SE ALMACENA LA POBLACIÓN Y SU MEJOR INDIVIDUO
    # --------------------------------------------------------------------------
    poblaciones[[i]]          <- poblacion
    fitness_mejor_individuo   <- max(fitness_ind_poblacion)
    mejor_individuo           <- poblacion[which.max(fitness_ind_poblacion), ]
    resultados_fitness[[i]]   <- fitness_mejor_individuo
    resultados_individuo[[i]] <- mejor_individuo
    
    # SE CALCULA LA DIFERENCIA ABSOLUTA RESPECTO A LA GENERACIÓN ANTERIOR
    # --------------------------------------------------------------------------
    # La diferencia solo puede calcularse a partir de la segunda generación.
    if (i > 1) {
      diferencia_abs[[i]] <- abs(resultados_fitness[[i - 1]] - resultados_fitness[[i]])
    }
    
    # NUEVA POBLACIÓN
    # --------------------------------------------------------------------------
    nueva_poblacion <- matrix(
      data = NA,
      nrow = nrow(poblacion),
      ncol = ncol(poblacion)
    )
    
    # ELITISMO
    # --------------------------------------------------------------------------
    # El elitismo indica el porcentaje de mejores individuos de la población
    # actual que pasan directamente a la siguiente población. De esta forma, se
    # asegura que, la siguiente generación, no sea nunca inferior.
    
    if (elitismo > 0) {
      n_elitismo         <- ceiling(nrow(poblacion) * elitismo)
      posicion_n_mejores <- order(fitness_ind_poblacion, decreasing = TRUE)
      posicion_n_mejores <- posicion_n_mejores[1:n_elitismo]
      nueva_poblacion[1:n_elitismo, ] <- poblacion[posicion_n_mejores, ]
    } else {
      n_elitismo <- 0
    }
    
    # CREACIÓN DE NUEVOS INDIVIDUOS POR CRUCES
    # --------------------------------------------------------------------------
    for (j in (n_elitismo + 1):nrow(nueva_poblacion)) {
      # Seleccionar parentales
      indice_parental_1 <- seleccionar_individuo(
        vector_fitness   = fitness_ind_poblacion,
        metodo_seleccion = metodo_seleccion,
        verbose          = verbose %in% c(2)
      )
      indice_parental_2 <- seleccionar_individuo(
        vector_fitness   = fitness_ind_poblacion,
        metodo_seleccion = metodo_seleccion,
        verbose          = verbose %in% c(2)
      )
      parental_1 <- poblacion[indice_parental_1, ]
      parental_2 <- poblacion[indice_parental_2, ]
      
      # Cruzar parentales para obtener la descendencia
      descendencia <- cruzar_individuos(
        parental_1   = parental_1,
        parental_2   = parental_2,
        metodo_cruce = metodo_cruce,
        verbose      = verbose %in% c(2)
      )
      # Mutar la descendencia
      descendencia <- mutar_individuo(
        individuo    = descendencia,
        prob_mut     = prob_mut,
        limite_inf   = limite_inf,
        limite_sup   = limite_sup,
        distribucion = distribucion,
        media_distribucion = media_distribucion,
        sd_distribucion    = sd_distribucion,
        min_distribucion   = min_distribucion,
        max_distribucion   = max_distribucion,
        verbose            = verbose %in% c(2)
      )
      
      nueva_poblacion[j, ] <- descendencia
    }
    poblacion <- nueva_poblacion
    
    # CRITERIO DE PARADA
    # --------------------------------------------------------------------------
    # Si durante las últimas n generaciones, la diferencia absoluta entre mejores
    # individuos no es superior al valor de tolerancia_parada, se detiene el
    # algoritmo y no se crean nuevas generaciones.
    
    if (parada_temprana && (i > rondas_parada)) {
      ultimos_n <- tail(unlist(diferencia_abs), n = rondas_parada)
      if (all(ultimos_n < tolerancia_parada)) {
        cat(
          "Algoritmo detenido en la generacion", i,
          "por falta cambio mínimo de", tolerancia_parada,
          "durante", rondas_parada,
          "generaciones consecutivas.",
          "\n"
        )
        break()
      }
    }
  }
  
  # IDENTIFICACIÓN DEL MEJOR INDIVIDUO DE TODO EL PROCESO
  # ----------------------------------------------------------------------------
  indice_mejor_individuo_global <- which.max(unlist(resultados_fitness))
  mejor_fitness_global   <- resultados_fitness[[indice_mejor_individuo_global]]
  mejor_individuo_global <- resultados_individuo[[indice_mejor_individuo_global]]
  
  # Si Nelder_Mead = TRUE, el mejor individuo identificado mediante el
  # algoritmo genético se somete a un proceso de optimización Nelder-Mead con
  # el objetivo de mejorar la convergencia final.
  if (Nelder_Mead) {
    optmizacion_NM <- optim(
      par = mejor_individuo_global,
      fn = function(par) {
        if (optimizacion == "maximizar") {
          do.call(-funcion, args = as.list(par))
        }else{
          do.call(funcion, args = as.list(par))
        }
      },
      method = "Nelder-Mead"
    )
    mejor_individuo_global <- optmizacion_NM$par
    mejor_fitness_global <- optmizacion_NM$value
  }
  
  # Se identifica el valor de la función objetivo para el mejor individuo.
  if (optimizacion == "maximizar") {
    mejor_valor_global <- mejor_fitness_global
  } else {
    mejor_valor_global <- -1*mejor_fitness_global
  }
  
  
  # RESULTADOS
  # ----------------------------------------------------------------------------
  # Para crear el dataframe se convierten las listas a vectores del mismo tamaño.
  resultados_fitness <- unlist(resultados_fitness)
  diferencia_abs     <- c(NA, unlist(diferencia_abs))
  
  # Si hay parada temprana, algunas generaciones no se alcanzan: Se eliminan sus
  # posiciones de las listas de resultados
  resultados_individuo <- resultados_individuo[!sapply(resultados_individuo, is.null)]
  poblaciones          <- poblaciones[!sapply(poblaciones, is.null)]
  
  
  # Para poder añadir al dataframe la secuencia variables, se concatenan.
  variables <- sapply(
    X = resultados_individuo,
    FUN = function(x) {
      paste(x, collapse = ", ")
    }
  )
  
  df_resultados <- data.frame(
    generacion        = seq_along(resultados_fitness),
    fitness           = resultados_fitness,
    predictores       = variables,
    diferencia_abs    = diferencia_abs
  )
  
  resultados <- list(
    mejor_individuo_global = mejor_individuo_global,
    mejor_valor_global     = mejor_valor_global,
    mejor_fitness_por_generacion   = resultados_fitness,
    mejor_individuo_por_generacion = resultados_individuo,
    diferencia_abs         = diferencia_abs,
    df_resultados          = df_resultados,
    poblaciones            = poblaciones,
    funcion_objetivo       = funcion_objetivo
  )
  
  end_time <- Sys.time()
  
  # INFORMACIÓN ALMACENADA EN LOS ATRIBUTOS
  # ----------------------------------------------------------------------------
  attr(resultados, "class") <- "optimizacion_ga"
  attr(resultados, 'fecha_creacion')        <- as.character(end_time)
  attr(resultados, 'duracion_optimizacion') <- paste(
                                                difftime(end_time, start_time, "secs"),
                                                "secs"
                                               )
  attr(resultados, 'optimizacion')          <- optimizacion
  attr(resultados, 'Nelder_Mead')           <- Nelder_Mead
  attr(resultados, 'lim_inf')               <- limite_inf
  attr(resultados, 'lim_sup')               <- limite_sup
  attr(resultados, 'n_poblacion')           <- n_poblacion
  attr(resultados, 'generaciones')          <- i 
  attr(resultados, 'valor_variables')       <- mejor_individuo_global
  attr(resultados, 'mejor_fitness')         <- mejor_fitness_global 
  attr(resultados, 'optimo_encontrado')     <- mejor_valor_global 
  attr(resultados, 'n_poblacion')           <- n_poblacion 
  attr(resultados, 'elitismo')              <- elitismo
  attr(resultados, 'prob_mut')              <- prob_mut
  attr(resultados, 'metodo_seleccion')      <- metodo_seleccion
  attr(resultados, 'metodo_cruce')          <- metodo_cruce
  attr(resultados, 'parada_temprana')       <- parada_temprana
  attr(resultados, 'rondas_parada')         <- rondas_parada
  attr(resultados, 'tolerancia_parada')     <- tolerancia_parada
  
  
  # INFORMACIÓN DEL PROCESO (VERBOSE)
  # ----------------------------------------------------------------------------
  if (verbose %in% c(1,2)) {
    cat("-----------------------", "\n")
    cat("Optimización finalizada", "\n")
    cat("-----------------------", "\n")
    cat("Fecha finalización  =", as.character(Sys.time()), "\n")
    cat("Duración selección  = ")
    print(difftime(end_time, start_time))
    cat("Número generaciones =", i, "\n")
    cat("Límite inferior     =", paste(limite_inf, collapse = ", "), "\n")
    cat("Límite superior     =", paste(limite_sup, collapse = ", "), "\n")
    cat("Optimización        =", optimizacion,"\n")
    cat("Nelder_Mead         =", Nelder_Mead,"\n")
    cat("Óptimo encontrado   =", mejor_valor_global,"\n")
    cat("Valor variables     =", mejor_individuo_global, "\n")
    cat("\n")
  }
  return(resultados)
}

print.optimizacion_ga <- function(obj){
  # Función print para objetos optimizacion_ga
  cat("----------------------------------------------", "\n")
  cat("Resultados optimización por algoritmo genético", "\n")
  cat("----------------------------------------------", "\n")
  cat("Fecha creación      =", attr(obj, 'fecha_creacion'), "\n")
  cat("Duración selección  =", attr(obj, 'duracion_optimizacion'), "\n")
  cat("Número generaciones =", attr(obj, 'generaciones'), "\n")
  cat("Límite inferior     =", attr(obj, 'lim_inf'), "\n")
  cat("Límite superior     =", attr(obj, 'lim_sup'), "\n")
  cat("Optimización        =", attr(obj, 'optimizacion'), "\n")
  cat("Nelder_Mead         =", attr(obj, 'Nelder_Mead'), "\n")
  cat("Óptimo encontrado   =", attr(obj, 'optimo_encontrado'), "\n")
  cat("Valor variables     =", attr(obj, 'valor_variables'), "\n")
  cat("\n")
  cat("Función objetivo    :", "\n")
  cat("\n")
  print(obj$funcion_objetivo)
}


################################################################################
##                        EJEMPLO                                             ##
################################################################################

# Función objetivo a optimizar.
# Mínimo global en la región −10<=x1<=0 −6.5<=x2<=0 en f(−3.1302468,−1.5821422).

funcion <- function(x1, x2){
  sin(x2)*exp(1-cos(x1))^2 + cos(x1)*exp(1-sin(x2))^2 + (x1-x2)^2
}

resultados_ga <- optimizar_ga(
  funcion_objetivo = funcion,
  n_variables      = 2,
  optimizacion     = "minimizar",
  limite_inf       = c(-10, -10),
  limite_sup       = c(10, 10),
  n_poblacion      = 150,
  n_generaciones   = 500,
  elitismo         = 0.01,
  prob_mut         = 0.1,
  distribucion     = "uniforme",
  min_distribucion = -1,
  max_distribucion = 1,
  metodo_seleccion = "tournament",
  parada_temprana  = TRUE,
  rondas_parada    = 10,
  tolerancia_parada = 10^-8,
  Nelder_Mead      = TRUE,
  paralelizado     = FALSE,
  verbose          = 0
)

resultados_ga
