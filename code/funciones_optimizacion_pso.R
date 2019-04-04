################################################################################
#                FUNCIONES OPTIMIZACIÓN CON ALGORITMO PSO                     #
#                                                                              #
# This work by Joaquín Amat Rodrigo is licensed under a Creative Commons       #
# Attribution 4.0 International License.                                       #
################################################################################

crear_particula <- function(n_variables,
                            limites_inf = NULL,
                            limites_sup = NULL,
                            verbose     = TRUE) {
  
  # Esta función crea una nueva partícula con una posición inicial definida por 
  # una combinación de valores numéricos aleatorios y velocidad de 0. El rango de
  # posibles valores para cada variable (posición) puede estar acotado.
  #
  # ARGUMENTOS
  # ============================================================================
  # n_variables:   número de variables que definen la partícula.
  # limites_inf:   vector con el límite inferior de cada variable. Si solo se
  #                quiere imponer límites a algunas variables, emplear NA para
  #                las que no se quiere acotar.
  # limites_sup:   vector con el límite superior de cada variable. Si solo se
  #                quiere imponer límites a algunas variables, emplear NA para
  #                las que no se quieren acotar.
  # verbose:       mostrar información de la partícula creada.
  #   
  # RETORNO
  # ============================================================================
  # Un objeto de tipo lista que representa la estructura de una partícula.
  # Al crear una nueva partícula, solo se dispone de información sobre su 
  # posición inicial y velocidad, el resto de atributos están vacíos.

  #
  # posicion:       vector con la posición actual (valor de las variables) de la
  #                 partícula.
  # valor:          valor actual de la partícula. Resultado de evaluar la función
  #                 objetivo con la posición actual. 
  # velocidad:      vector con la velocidad actual de la partícula.
  # mejor_valor:    mejor valor que ha tenido la partícula hasta el momento.    
  # mejor_posicion: posición en la que la partícula ha tenido el mejor valor hasta
  #                 el momento.
  
  # Comprobaciones
  if (!is.null(limites_inf) & (length(limites_inf) != n_variables)) {
    stop(paste(
      "limites_inf debe tener un valor por cada variable.",
      "Si para alguna variable no se quiere límite, emplear NA.",
      "Ejemplo: limites_inf = c(10, NA, 10)"
    ))
  } else if (!is.null(limites_sup) & length(limites_sup) != n_variables) {
    stop(paste(
      "limites_sup debe tener un valor por cada variable.",
      "Si para alguna variable no se quiere límite, emplear NA.",
      "Ejemplo: limites_sup = c(10, NA, 10)"
    ))
  } else if (is.null(limites_sup) | is.null(limites_inf)) {
    warning(paste(
      "Es altamente recomendable indicar los límites dentro de los",
      "cuales debe buscarse la solución de cada variable.",
      "Por defecto se emplea [-10^3, 10^3]."
    ))
  } else if (any(c(is.na(limites_sup), is.na(limites_inf)))) {
    warning(paste(
      "Los límites empleados por defecto cuando no se han definido son:",
      " [-10^3, 10^3]."
    ))
    cat("\n")
  }
  
  # Si no se especifica limites_inf, el valor mínimo que pueden tomar las variables
  # es -10^3.
  if (is.null(limites_inf)) {
    limites_inf <- rep(x = -10^3, times = n_variables)
  }
  # Si no se especifica limites_sup, el valor máximo que pueden tomar las variables
  # es 10^3.
  if (is.null(limites_sup)) {
    limites_sup <- rep(x = 10^3, times = n_variables)
  }
  # Si los límites no son nulos, se reemplazan aquellas posiciones NA por el valor
  # por defecto -10^3 y 10^3.
  if (!is.null(limites_inf)) {
    limites_inf[is.na(limites_inf)] <- -10^3
  }
  if (!is.null(limites_sup)) {
    limites_sup[is.na(limites_sup)] <- 10^3
  }
  
  # Se crea una lista que representa la partícula.
  particula        <- vector(length = 5, mode = "list")
  names(particula) <- c("posicion", "valor", "velocidad", "mejor_valor",
                        "mejor_posicion")
  
  # Bucle para asignar un valor a cada una de las variables que definen la posición.
  posicion <- rep(NA, times = n_variables)
  for (i in 1:n_variables) {
      # Para cada posición, se genera un valor aleatorio dentro del rango permitido
      # para esa variable.
      posicion[i] <- runif(n = 1, min = limites_inf[i], max = limites_sup[i])
  }
  particula[["posicion"]] <- posicion
  
  # La velocidad inicial de la partícula es 0.
  velocidad <- rep(0, times = n_variables)
  particula[["velocidad"]] <- velocidad
  
  if (verbose) {
    print("Nueva partícula creada")
    print("----------------------")
    print(paste(
      "Posición:",
      paste(particula[["posicion"]], collapse = ", ")
      ))
    print(paste(
      "Límites inferiores de cada variable:",
      paste(limites_inf, collapse = ", ")
    ))
    print(paste(
      "Límites superiores de cada variable:",
      paste(limites_sup, collapse = ", ")
    ))
    cat("\n")
  }

  return(particula)
}


evaluar_particula <- function(particula,
                              funcion_objetivo,
                              optimizacion,
                              verbose = TRUE) {
  
  # Esta función evalúa una partícula calculando el valor que toma la función
  # objetivo en la posición en la que se encuentra la partícula. Además, compara
  # si la nueva posición es mejor que las anteriores.
  # 
  # ARGUMENTOS
  # ============================================================================
  # particula:        partícula que se quiere evaluar.
  # funcion_objetivo: función que se quiere optimizar.
  # optimizacion:     maximizar o minimizar. Dependiendo de esto, el mejor valor
  #                   histórico de la partícula será el mayor o el menor valor
  #                   que ha tenido hasta el momento.
  # verbose:          mostrar información del proceso por pantalla.
  #   
  # RETORNO
  # ============================================================================
  # Devuelve una nueva partícula en la que los campos valor, mejor_valor y
  # mejor_posicion han sido actualizados.
  
  # Comprobaciones.
  if (!optimizacion %in% c("maximizar", "minimizar")) {
      stop("El argumento optimizacion debe ser: maximizar o minimizar")
  }
  
  # Se evalúa la partícula con la función objetivo.
  valor <- do.call(
              funcion_objetivo,
              args = as.list(particula[["posicion"]])
           )
  # Se almacena el nuevo valor.
  particula[["valor"]] <- valor
  
  # Se compara el valor actual con el mejor valor histórico. La comparación es
  # distinta dependiendo de si se desea maximizar o minimizar. Si no existe ningún
  # valor histórico, se almacena el actual. Si ya existe algún valor histórico se
  # compara con el actual y, de ser mayor este último, se sobrescribe.
  if (is.null(particula[["mejor_valor"]])) {
    particula[["mejor_valor"]]    <- particula[["valor"]]
    particula[["mejor_posicion"]] <- particula[["posicion"]]
  } else{
    if (optimizacion == "minimizar") {
      if (particula[["valor"]] < particula[["mejor_valor"]]) {
        particula[["mejor_valor"]]    <- particula[["valor"]]
        particula[["mejor_posicion"]] <- particula[["posicion"]]
      }
    } else {
      if (particula[["valor"]] > particula[["mejor_valor"]]) {
        particula[["mejor_valor"]]    <- particula[["valor"]]
        particula[["mejor_posicion"]] <- particula[["posicion"]]
      }
    } 
  }
  
  if (verbose) {
    print(paste("La partícula ha sido evaluada"))
    print("-----------------------------")
    print(paste("Valor actual:", particula[["valor"]]))
    cat("\n")
  }
  return(particula)
}

mover_particula <- function(particula,
                            mejor_p_enjambre,
                            inercia        = 0.8,
                            peso_cognitivo = 2,
                            peso_social    = 2,
                            limites_inf    = NULL,
                            limites_sup    = NULL,
                            verbose        = TRUE) {
  
  # Esta función ejecuta el movimiento de una partícula, lo que implica actualizar
  # su velocidad y posición. Si se especifican límites, no se permite que la
  # partícula salga de la zona de búsqueda.
  # 
  # ARGUMENTOS
  # ============================================================================
  # particula:        partícula que se quiere evaluar.
  # mejor_p_enjambre: mejor posición de todo el enjambre.
  # inercia:          coeficiente de inercia.
  # peso_cognitivo:   coeficiente cognitivo.
  # peso_social:      coeficiente social.
  # limites_inf:      vector con el límite inferior de cada variable. Si solo se
  #                   quiere imponer límites a algunas variables, emplear NA para
  #                   las que no se quiere acotar.
  # limites_sup:      vector con el límite superior de cada variable. Si solo se
  #                   quiere imponer límites a algunas variables, emplear NA para
  #                   las que no se quieren acotar.
  # verbose:          mostrar información del proceso por pantalla.
  #   
  # RETORNO
  # ============================================================================
  # Devuelve una nueva partícula en la que la posición y velocidad han sido
  # actualizadas.
  
  # Actualización de la velocidad.
  componente_velocidad <- inercia * particula[["velocidad"]]
  r1 <- runif(n = length(particula[["velocidad"]]), min = 0, max = 1)
  r2 <- runif(n = length(particula[["velocidad"]]), min = 0, max = 1)
  componente_cognitivo <- peso_cognitivo * r1 * (particula[["mejor_posicion"]] -
                                                 particula[["posicion"]])
  componente_social    <- peso_social * r2 * (mejor_p_enjambre - 
                                              particula[["posicion"]])
  
  nueva_velocidad      <- componente_velocidad + 
                          componente_cognitivo +
                          componente_social
  
  # Actualización de la posción.
  nueva_posicion <- particula[["posicion"]] + nueva_velocidad
  
  # Se comprueba si algún valor de la nueva posición supera los límites impuestos.
  # En tal caso, se sobrescribe con el valor del límite correspondiente y se 
  # reinicia a 0 la velocidad de la partícula en esa componente.
  for (i in seq_along(nueva_posicion)) {
    if (isTRUE(nueva_posicion[i] < limites_inf[i])) {
        nueva_posicion[i]  <- limites_inf[i]
        nueva_velocidad[i] <- 0
    }
    if (isTRUE(nueva_posicion[i] > limites_sup[i])) {
         nueva_posicion[i] <- limites_sup[i]
         nueva_velocidad[i] <- 0
    }
  }
  
  particula[["velocidad"]] <- nueva_velocidad
  particula[["posicion"]]  <- nueva_posicion
  
  if (verbose) {
    print(paste("La partícula se ha desplazado"))
    print("-----------------------------")
    print(paste(
      "Nueva posición:",
      paste(particula[["posicion"]], collapse = ", ")
      ))
    cat("\n")
  }
  return(particula)
}

crear_enjambre <- function(n_particulas,
                           n_variables,
                           limites_inf = NULL,
                           limites_sup = NULL,
                           verbose     = TRUE) {
  
  # Esta función crea un enjambre con n partículas.
  #
  # ARGUMENTOS
  # ============================================================================
  # n_particulas;  número de partículas del enjambre.
  # n_variables:   número de variables que definen una partícula.
  # limites_inf:   vector con el límite inferior de cada variable. Si solo se
  #                quiere imponer límites a algunas variables, emplear NA para
  #                las que no se quiere acotar.
  # limites_sup:   vector con el límite superior de cada variable. Si solo se
  #                quiere imponer límites a algunas variables, emplear NA para
  #                las que no se quieren acotar.
  # verbose:       mostrar información del proceso por pantalla.
  #   
  # RETORNO
  # ============================================================================
  # Un objeto lista con dos elementos:
  # partículas:      lista con todas las partículas del enjambre.
  # mejor_particula: la mejor partícula del enjambre.

  # Se crea la lista que representa el enjambre y los dos elementos que lo forman.
  enjambre        <- vector(length = 2, mode = "list")
  names(enjambre) <- c("particulas", "mejor_particula")
  
  enjambre[["particulas"]]        <- vector(length = n_particulas, mode = "list")
  names(enjambre[["particulas"]]) <- paste0("particula_", 1:n_particulas)
  
  # Al crear el enjambre, las partículas no han sido todavía evaluadas, por lo 
  # no se conoce cuál es la mejor partícula.
  enjambre[["mejor_particula"]]   <- list()
  
  # Se crea cada una de las partículas y se almacenan en enjambre[["particulas"]]
  for (i in 1:n_particulas) {
    enjambre[["particulas"]][[i]] <- crear_particula(
                                        n_variables = n_variables,
                                        limites_inf = limites_inf,
                                        limites_sup = limites_sup,
                                        verbose     = verbose
                                      )
  }
  
  if (verbose) {
    print("Enjambre creado")
    print("---------------")
    print(paste("Número de partículas =", n_particulas))
    print(paste(
      "Límites inferiores de cada variable:",
      paste(limites_inf, collapse = ", ")
    ))
    print(paste(
      "Límites superiores de cada variable:",
      paste(limites_sup, collapse = ", ")
    ))
    cat("\n")
  }
  
  return(enjambre)
}

evaluar_enjambre <- function(enjambre,
                             funcion_objetivo,
                             optimizacion,
                             verbose = TRUE) {
  
  # Esta función evalúa todas las partículas del enjambre, actualiza sus valores,
  # e identifica la mejor partícula.
  # 
  # ARGUMENTOS
  # ============================================================================
  # enjambre:         enjambre que se quiere evaluar.
  # funcion_objetivo: función que se quiere optimizar.
  # optimizacion:     maximizar o minimizar. Dependiendo de esto, el mejor valor
  #                   histórico de la partícula será el mayor o el menor valor
  #                   que ha tenido hasta el momento.
  # 
  # RETORNO
  # ============================================================================
  # Devuelve una nuevo enjambre en el que el valor de todas las partículas
  # y el de la mejor partícula ha sido actualizado.
  
  # Se evalúa cada partícula del enjambre.
  enjambre[["particulas"]] <- purrr::map(
                                .x               = enjambre[["particulas"]],
                                .f               = evaluar_particula,
                                funcion_objetivo = funcion_objetivo,
                                optimizacion     = optimizacion,
                                verbose          = verbose
                               )
  # Se identifica la mejor partícula de todo el enjambre. Si se está maximizando,
  # la mejor partícula es aquella con mayor valor. Lo contrario si se está 
  # minimizando.
  
  # Se selecciona inicialmente como mejor partícula la primera.
  mejor_particula <- enjambre[["particulas"]][[1]]
  
  # Se comparan todas las partículas del enjambre.
  for (i in seq_along(enjambre[["particulas"]])) {
    if (optimizacion == "minimizar") {
      if (enjambre[["particulas"]][[i]][["valor"]] < mejor_particula[["valor"]]) {
        mejor_particula <- enjambre[["particulas"]][[i]]
      }
    } else {
      if (enjambre[["particulas"]][[i]][["valor"]] > mejor_particula[["valor"]]) {
        mejor_particula <- enjambre[["particulas"]][[i]]
      }
    }
  }
  
  # Se almacena la mejor partícula en enjambre[["mejor_particula"]].
  enjambre[["mejor_particula"]] <- mejor_particula
  
  if (verbose) {
    print("Enjambre evaluado")
    print("-----------------")
    print(paste(
      "Mejor posición encontrada:",
       paste(mejor_particula[["posicion"]], collapse = ", ")
    ))
    print(paste(
      "Mejor valor encontrado:",
       mejor_particula[["valor"]]
    ))
    cat("\n")
  }
  return(enjambre)
}

mover_enjambre <- function(enjambre,
                           inercia        = 0.8,
                           peso_cognitivo = 2,
                           peso_social    = 2,
                           limites_inf    = NULL,
                           limites_sup    = NULL,
                           verbose        = TRUE) {
  
  # Esta función mueve todas las partículas del enjambre.
  # 
  # ARGUMENTOS
  # ============================================================================
  # enjambre:       enjambre que se quiere mover.
  # optimizacion:   si se desea maximizar o minimizar la función.
  # inercia:        coeficiente de inercia.
  # peso_cognitivo: coeficiente cognitivo.
  # peso_social:    coeficiente social.
  # limites_inf:    vector con el límite inferior de cada variable. Si solo se
  #                 quiere imponer límites a algunas variables, emplear NA para
  #                 las que no se quiere acotar.
  # limites_sup:    vector con el límite superior de cada variable. Si solo se
  #                 quiere imponer límites a algunas variables, emplear NA para
  #                 las que no se quieren acotar. 
  # verbose:        mostrar información del proceso por pantalla.
  # 
  # RETORNO
  # ============================================================================
  # Devuelve una nuevo enjambre en el que la posición de todas las partículas
  # ha sido actualizada.
  
  # Se actualiza la posición de cada una de las partículas que forman el enjambre.
  mejor_posicion_enjambre <- enjambre[["mejor_particula"]][["posicion"]]
  enjambre[["particulas"]] <- purrr::map(
                                .x = enjambre[["particulas"]],
                                .f = mover_particula,
                                mejor_p_enjambre  = mejor_posicion_enjambre,
                                inercia           = inercia,
                                peso_cognitivo    = peso_cognitivo,
                                peso_social       = peso_social,
                                limites_inf       = limites_inf,
                                limites_sup       = limites_sup,
                                verbose           = verbose
                              )
  
  if (verbose){
    print("La posición de todas las partículas del enjambre ha sido actualizada.")
    cat("\n")
  }
  return(enjambre)
}


optimizar_enjambre <- function(
                         funcion_objetivo,
                         n_variables,
                         optimizacion,
                         limites_inf        = NULL,
                         limites_sup        = NULL,
                         n_particulas       = 100,
                         n_iteraciones      = 50,
                         inercia            = 0.8,
                         reduc_inercia      = TRUE,
                         inercia_max        = 0.9,
                         inercia_min        = 0.4,
                         peso_cognitivo     = 2,
                         peso_social        = 2,
                         parada_temprana    = FALSE,
                         rondas_parada      = NULL,
                         tolerancia_parada  = NULL,
                         verbose            = FALSE,
                         ...) {

  # ARGUMENTOS
  # =============================================================================
  # funcion_objetivo: nombre de la función que se desea optimizar. Debe de haber
  #                   sido definida previamente.
  # n_variables:      número de variables que definen una partícula.
  # optimizacion:     "maximizar" o "minimizar".
  # limites_inf:      vector con el límite inferior de cada variable. Si solo se
  #                   quiere imponer límites a algunas variables, emplear NA para
  #                   las que no se quiere acotar.
  # limites_sup:      vector con el límite superior de cada variable. Si solo se
  #                   quiere imponer límites a algunas variables, emplear NA para
  #                   las que no se quieren acotar.
  # n_particulas:     número total de partículas del enjambre.
  # n_iteraciones:    número total de iteraciones de optimización.
  # inercia:          coeficiente de inercia.
  # peso_cognitivo:   coeficiente cognitivo.
  # peso_social:      coeficiente social.
  # reduc_inercia:    activar la reducción del coeficiente de inercia. En tal caso,
  #                   el argumento inercia es ignorado.
  # inercia_max       valor inicial del coeficiente de inercia si se activa 
  #                   reduc_inercia.
  # inercia_min       valor minimo del coeficiente de inercia si se activa 
  #                   reduc_min.
  # parada_temprana:  si durante las últimas "rondas_parada" generaciones la diferencia
  #                   absoluta entre mejores individuos no es superior al valor de
  #                  "tolerancia_parada", se detiene el algoritmo y no se crean
  #                   nuevas generaciones.
  # rondas_parada:    número de generaciones consecutivas sin mejora mínima para que
  #                   se active la parada temprana.
  # tolerancia_parada: valor mínimo que debe tener la diferencia de generaciones
  #                    consecutivas para considerar que hay cambio.
  # verbose:          TRUE para que se imprima por pantalla el resultado de cada
  #                   paso del algoritmo.

  # RETORNO
  # =============================================================================
  # La función devuelve una lista con 4 elementos:
  # historico_enjambre: información sobre el enjambre en cada iteración.
  # optim_valor:        valor óptimo encontrado.
  # optim_posicion:     posición donde se ha encontrado el valor óptimo.
  # resultados_df:      data.frame con la información del mejor valor y posición
  #                     encontrado en cada iteración, así como la mejora respecto
  #                     a la iteración anterior.
 
  # LIBRERÍAS NECESARIAS
  # =============================================================================
  library(purrr)
  
  # COMPROBACIONES INICIALES
  # ============================================================================
  # Si se activa la parada temprana, hay que especificar los argumentos
  # rondas_parada y tolerancia_parada.
  if (isTRUE(parada_temprana) &
      (is.null(rondas_parada) | is.null(tolerancia_parada)) ) {
    stop(paste(
      "Para activar la parada temprana es necesario indicar un valor",
      "de rondas_parada y de tolerancia_parada."
    ))
  }
  
  # Si se activa la reducción de inercia, hay que especificar los argumentos
  # inercia_max y inercia_min.
  if (isTRUE(reduc_inercia) &
      (is.null(inercia_max) | is.null(inercia_min)) ) {
    stop(paste(
      "Para activar la reducción de inercia es necesario indicar un valor",
      "de inercia_max y de inercia_min."
    ))
  }

  # ESTABLECER LOS LÍMITES DE BÚSQUEDA SI EL USUARIO NO LO HA HECHO
  # ============================================================================
  if (is.null(limites_sup) | is.null(limites_inf)) {
    warning(paste(
      "Es altamente recomendable indicar los límites dentro de los",
      "cuales debe buscarse la solución de cada variable.",
      "Por defecto se emplea: [-10^3, 10^3]."
    ))
  } else if (any(c(is.na(limites_sup), is.na(limites_inf)))) {
    warning(paste(
      "Los límites empleados por defecto cuando no se han definido son:",
      " [-10^3, 10^3]."
    ))
    cat("\n")
  }

  # Si no se especifica limites_inf, el valor mínimo que pueden tomar las variables
  # es -10^3.
  if (is.null(limites_inf)) {
    limites_inf <- rep(x = -10^3, times = n_variables)
  }
  # Si no se especifica limites_sup, el valor máximo que pueden tomar las variables
  # es 10^3.
  if (is.null(limites_sup)) {
    limites_sup <- rep(x = 10^3, times = n_variables)
  }
  # Si los límites no son nulos, se reemplazan aquellas posiciones NA por el valor
  # por defecto -10^3 y 10^3.
  if (!is.null(limites_inf)) {
    limites_inf[is.na(limites_inf)] <- -10^3
  }
  if (!is.null(limites_sup)) {
    limites_sup[is.na(limites_sup)] <- 10^3
  }

  # ALMACENAMIENTO DE RESULTADOS
  # ============================================================================
  # Por cada iteración se almacena: todo el enjambre, el mejor valor y la mejor
  # posición encontradas en la iteración, y la diferencia respecto al mejor valor
  # de la iteración anterior.
  historico_enjambre        <- vector(mode = "list", length = n_iteraciones)
  names(historico_enjambre) <- paste("iter", 1:n_iteraciones, sep = "_")
  mejor_valor_enjambre      <- rep(NA, times = n_iteraciones)
  mejor_posicion_enjambre   <- rep(NA, times = n_iteraciones)
  diferencia_abs            <- rep(NA, times = n_iteraciones)
  
  # CREACIÓN DEL ENJAMBRE INICIAL
  # ============================================================================
  enjambre <- crear_enjambre(
                n_particulas = n_particulas,
                n_variables  = n_variables,
                limites_inf  = limites_inf,
                limites_sup  = limites_sup,
                verbose      = verbose
              )
  
  # ITERACIONES
  # ============================================================================
  for (i in 1:n_iteraciones) {
    if (verbose) {
      print("---------------------")
      print(paste("Iteración:", i))
      print("---------------------")
    }

    # EVALUAR PARTÍCULAS DEL ENJAMBRE
    # ==========================================================================
    enjambre <- evaluar_enjambre(
                  enjambre         = enjambre,
                  funcion_objetivo = funcion,
                  optimizacion     = optimizacion,
                  verbose          = verbose
                )
    
    # SE ALMACENA EL ENJAMBRE Y LA MEJOR SOLUCIÓN ENCONTRADA EN ÉL
    # ==========================================================================
    historico_enjambre[[i]]      <- enjambre
    mejor_valor_enjambre[[i]]    <- enjambre[["mejor_particula"]][["valor"]]
    mejor_posicion_enjambre[[i]] <- paste(
                                      enjambre[["mejor_particula"]][["posicion"]],
                                      collapse = ", "
                                    )
    
    # SE CALCULA LA DIFERENCIA ABSOLUTA RESPECTO A LA ITERACION ANTERIOR
    # ==========================================================================
    # La diferencia solo puede calcularse a partir de la segunda generación.
    if (i > 1) {
      diferencia_abs[[i]] <- abs(mejor_valor_enjambre[[i-1]]-mejor_valor_enjambre[[i]])
    }

    # CRITERIO DE PARADA
    # ==========================================================================
    # Si durante las últimas n generaciones, la diferencia absoluta entre mejores
    # partículas no es superior al valor de tolerancia_parada, se detiene el
    # algoritmo y no se crean nuevas generaciones.
    if (parada_temprana && (i > rondas_parada)) {
      ultimos_n <- tail(diferencia_abs[!is.na(diferencia_abs)], n = rondas_parada)
      if (all(ultimos_n < tolerancia_parada)) {
        print(paste(
          "Algoritmo detenido en la iteracion", i,
          "por falta cambio absoluto mínimo de", tolerancia_parada,
          "durante", rondas_parada,
          "iteraciones consecutivas."
        ))
        break()
      }
    }
    
    # MOVER PARTÍCULAS DEL ENJAMBRE
    # ==========================================================================
    # Si se ha activado la reducción de inercia, se recalcula su valor para la
    # iteración actual.
    if (isTRUE(reduc_inercia)) {
      inercia <- (inercia_max-inercia_min)*(n_iteraciones-i)/n_iteraciones+inercia_min
    }
    
    enjambre <- mover_enjambre(
                  enjambre       = enjambre,
                  inercia        = inercia,
                  peso_cognitivo = peso_cognitivo,
                  peso_social    = peso_social,
                  limites_inf    = limites_inf,
                  limites_sup    = limites_sup,
                  verbose        = verbose
                )
  }

  # IDENTIFICACIÓN DEL MEJOR INDIVIDUO DE TODO EL PROCESO
  # ==========================================================================
   optim_valor    <- min(mejor_valor_enjambre, na.rm = TRUE)
   optim_posicion <- mejor_posicion_enjambre[which.min(mejor_valor_enjambre)]
   optim_posicion <- as.numeric(unlist(strsplit(x = optim_posicion, split = ", ")))
  
  # RESULTADOS
  # ============================================================================
  # Se crea un dataframe con un resumen de la información de cada iteración.
  resultados_df <- data.frame(
                    iteracion               = 1:n_iteraciones,
                    mejor_valor_enjambre    = mejor_valor_enjambre,
                    mejor_posicion_enjambre = mejor_posicion_enjambre,
                    diferencia_abs          = diferencia_abs
                   )
  # Si el algoritmo se ha detenido de forma temprana, se eliminan las iteraciones
  # vacías de resultados_df.
  resultados_df <- resultados_df[!is.na(mejor_valor_enjambre), ]
  
  return(
    list(historico_enjambre = historico_enjambre,
         optim_valor        = optim_valor,
         optim_posicion     = optim_posicion,
         resultados_df      = resultados_df
         )
  )
}


################################################################################
##                        EJEMPLO                                             ##
################################################################################

# Función objetivo a optimizar.
funcion <- function(x1, x2){
             sin(x2)*exp(1-cos(x1))^2 + cos(x1)*exp(1-sin(x2))^2 + (x1-x2)^2
           }

optimizacion <- optimizar_enjambre(
                 funcion_objetivo = funcion,
                 n_variables      = 2,
                 optimizacion     = "minimizar",
                 limites_inf      = c(-10, -6.5),
                 limites_sup      = c(0, 0),
                 n_particulas     = 100,
                 n_iteraciones    = 250,
                 inercia          = 0.8,
                 reduc_inercia    = TRUE,
                 inercia_max      = 0.9,
                 inercia_min      = 0.4,
                 peso_cognitivo   = 2,
                 peso_social      = 2,
                 parada_temprana  = TRUE,
                 rondas_parada    = 5,
                 tolerancia_parada = 10^-8,
                 verbose          = FALSE
                )
optimizacion$optim_posicion
