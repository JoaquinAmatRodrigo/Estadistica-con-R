################################################################################
#        FUNCIONES ALGORITMO GENÉTICO  PARA SELECCIÓN DE PREDICTORES           #
#                                                                              #
# This work by Joaquín Amat Rodrigo is licensed under a Creative Commons       #
# Attribution 4.0 International License.                                       #
################################################################################

library(foreach)
library(doParallel)
library(randomForest)
library(caret)
library(MLmetrics)
library(tibble)

crear_poblacion <- function(n_poblacion,
                            n_variables,
                            n_max = NULL,
                            n_min = NULL,
                            verbose = TRUE) {
  # Parameters
  # ----------
  # n_poblacion: "integer"
  #   número total de individuos de la población.
  #   
  # n_variables: "integer"
  #   longitud de los individuos.
  #   
  # n_max: "integer"
  #   número máximo de TRUEs que puede contener un individuo.
  #   
  # n_min: "integer"
  #   número mínimo de TRUEs que puede contener un individuo.
  #   
  # verbose: "logical"
  #   mostrar información del proceso por pantalla.
  
  # Return
  # ----------
  # "matrix" de tamaño n_poblacion x n_variables que representa una población.
  
  # COMPROBACIONES
  # ----------------------------------------------------------------------------
  if (isTRUE(n_max > n_variables)) {
    stop("n_max no puede ser mayor que n_variables.")
  }
  # Si no se especifica n_max, el número máximo de predictores (TRUEs) que puede
  # contener un individuo es igual al número total de variables disponibles.
  if (is.null(n_max)) {
    n_max <- n_variables
  }
  
  # Si no se especifica n_min, el número mínimo de predictores (TRUEs) que puede
  # contener un individuo es 1.
  if (is.null(n_min)) {
    n_min <- 1
  }
  
  # CREAR POBLACIÓN
  # ----------------------------------------------------------------------------
  
  # Matriz donde almacenar los individuos generados.
  poblacion <- matrix(data = NA, nrow = n_poblacion, ncol = n_variables)
  
  # Bucle para crear cada individuo.
  for (i in 1:n_poblacion) {
    # Se selecciona (con igual probabilidad) el número de valores TRUE que puede
    # tener el individuo, dentro del rango acotado por n_min y n_max.
    n_true <- sample(x = n_min:n_max, size = 1)
    
    # Se crea un vector con todo FALSE que representa el individuo.
    individuo <- rep(FALSE, times = n_variables)
    
    # Se sustituyen n_true posiciones aleatorias por valores TRUE.
    individuo[sample(x = 1:n_variables, size = n_true)] <- TRUE
    
    # Se añade el nuevo individuo a la población.
    poblacion[i, ] <- individuo
  }
  
  # INFORMACIÓN ALMACENADA EN LOS ATRIBUTOS
  # ----------------------------------------------------------------------------
  attr(poblacion, 'fecha_creacion')    <- Sys.time()
  attr(poblacion, 'numero_individuos') <- n_poblacion
  attr(poblacion, "class") <- c(attr(poblacion, "class"), "poblacion")
  
  if (verbose) {
    print("Población inicial creada")
    print("------------------------")
    print(Sys.time())
    print(paste("Número de individuos =", n_poblacion))
    print(paste("Número de predictores mínimo por individuo =", n_min))
    print(paste("Número de predictores máximo por individuo =", n_max))
    cat("\n")
  }
  
  return(poblacion)
}

calcular_fitness_individuo_rf <- function(x,
                                          y,
                                          cv,
                                          metrica = NULL,
                                          nivel_referencia = NULL,
                                          n_tree  = 50,
                                          seed    = 123,
                                          verbose = TRUE,
                                          ...) {
  # Parameters
  # ----------
  # x: "matrix", "data.frame", tibble
  #   matriz de predictores.
  #   
  # y: "vector"
  #   variable respuesta.
  #   
  # cv: "integer"
  #   número de particiones de validación cruzada.
  #   
  # seed: "integer"
  #   semilla para garantizar reproducibilidad en el proceso de CV.
  #   
  # metrica: ("-mse", -mae, "f1", "kappa", "accuracy")
  #   métrica utilizada para calcular el fitness. Por defecto es el -mse para
  #   regresión y accuracy para clasificación. En el caso de clasificación
  #   binaria, se acepta también f1 y kappa.
  #   
  # nivel_referencia: "character"
  #   valor de la variable respuesta considerado como referencia. Necesario
  #   cuando la métrica es f1 o kappa.
  #   
  # n_tree: "integer"
  #   número de árboles del modelo randomforest.       
  #                   
  # verbose: "logical"
  #   mostrar información del proceso por pantalla.
  
  # Return
  # ----------
  # fitness ("numeric") del individuo obtenido por validación cruzada empleando
  # random forest como modelo.
  
  # LIBRERÍAS
  # ----------------------------------------------------------------------------
  library(MLmetrics)
  library(caret)
  
  # COMPROBACIONES INICIALES
  # ----------------------------------------------------------------------------
  if (!is.null(metrica)) {
    if (metrica %in% c("-mse", "-mae") && !is.numeric(y)) {
      stop(
        paste(
          "La métrica", metrica, "no es valida para problemas de regresión.",
          "Emplear el -mse o -mae."
        )
      )
    }
    if (metrica %in% c("accuracy", "f1", "kappa") && is.numeric(y)) {
      stop(
        paste(
          "La métrica", metrica, "no es valida para problemas de clasificación.",
          "Emplear accuracy, f1, kappa."
        )
      )
    }
    if (metrica %in% c("f1", "kappa") && length(unique(y)) > 2) {
      stop(
        paste(
          "La métrica", metrica, "no es valida para problemas de clasificación",
          "con más de dos clases. Emplear accuracy."
        )
      )
    }
    if (metrica %in% c("f1", "kappa") && is.null(nivel_referencia)) {
      stop(
        "Para la métrica kappa es necesario indicar el nivel de referencia."
      )
    }
  }
  
  if (is.null(metrica)) {
    if (is.numeric(y)) {
      metrica <- "-mse"
    }else{
      metrica <- "accuracy"
    }
  }
  
  # REPARTO DE LAS OBSERVACIONES PARA LA VALIDACIÓN CRUZADA (CV)
  # ----------------------------------------------------------------------------
  set.seed(seed)
  indices_cv <- caret::createFolds(y = y, k = cv, list = FALSE)
  
  # BUCLE DE ENTRENAMIENTO Y VALIDACIÓN CRUZADA (CV)
  # ----------------------------------------------------------------------------
  # Vector para almacenar el fitness de cada iteración CV.
  metrica_cv <- rep(NA, times = cv)
  
  for (i in 1:cv) {
    set.seed(seed)
    modelo <- randomForest::randomForest(
                x = x[indices_cv != i, , drop = FALSE],
                y = y[indices_cv != i],
                ntree = n_tree
              )
    predicciones <- predict(modelo, newdata = x[indices_cv == i, , drop = FALSE])
    
    if (metrica == "-mse") {
      residuos <- predicciones - y[indices_cv == i]
      metrica_cv[i] <- -(mean(residuos^2))
    } else if (metrica == "-mae") {
      residuos <- predicciones - y[indices_cv == i]
      metrica_cv[i] <- -(mean(abs(residuos)))
    } else if (metrica == "accuracy") {
      accuracy <- MLmetrics::Accuracy(
        y_pred = predicciones,
        y_true = y[indices_cv == i]
      )
      metrica_cv[i] <- accuracy
    } else if (metrica == "kappa") {
      mat_confusion <- caret::confusionMatrix(
        table(
          predicciones,
          y[indices_cv == i]
        ),
        positive = nivel_referencia
      )
      kappa <- mat_confusion$overall["Kappa"]
      metrica_cv[i] <- kappa
    } else if (metrica == "f1") {
      if (sum(predicciones == nivel_referencia) < 1) {
        print(
          paste(
            "Ningún valor predicho ha sido el nivel de referencia,",
            "por lo tanto, el valor de precisión no puede ser calculado",
            "y tampoco la métrica F1."
          )
        )
      } else {
        f1 <- MLmetrics::F1_Score(
          y_true = y[indices_cv == i],
          y_pred = predicciones,
          positive = nivel_referencia
        )
        metrica_cv[i] <- f1
      }
    }
    
    # Información detalla de la partición en caso de que el cálculo de la 
    # métrica falle.
    if (verbose) {
      if (is.na(metrica_cv[i])) {
        print(paste("En la particion", i, "de CV, fitness del individuo no",
                    "ha podido ser calculado."
        ))
        print("Valores reales:")
        print(y[indices_cv == i])
        print("Valores predichos:")
        print(predicciones)
      }
    }
  }
    
  # COMPROBACIÓN DE LAS MÉTRICAS OBTENIDAS POR CV
  # ------------------------------------------------------------------------
  if (any(is.na(metrica_cv))) {
    warning(paste(
      "En una o más particiones de CV, la métrica del individuo no",
      "ha podido ser calculado."
    ))
  }
  
  if (is.na(mean(metrica_cv, na.rm = TRUE))) {
    stop("El fitness del individuo no ha podido ser calculado.")
  }
  
  valor_metrica <- mean(metrica_cv, na.rm = TRUE)
  fitness       <- mean(metrica_cv, na.rm = TRUE)
  modelo        <- "Random Forest"
  
  # INFORMACIÓN DEL PROCESO (VERBOSE)
  # ------------------------------------------------------------------------
  if (verbose) {
    cat("El individuo ha sido evaluado", "\n")
    cat("-----------------------------", "\n")
    cat("Métrica:", metrica, "\n")
    cat("Valor métrica:", valor_metrica, "\n")
    cat("Fitness:", fitness, "\n")
    cat("Modelo de evaluación:", modelo, "\n")
    cat("\n")
  }

  return(fitness)
}


calcular_fitness_individuo_lm <- function(x,
                                          y,
                                          cv,
                                          metrica = NULL,
                                          seed    = 123,
                                          verbose = TRUE,
                                          ...) {
  
  # Parameters
  # ----------
  # x: "matrix", "data.frame", "tibble"
  #   matriz de predictores.
  #   
  # y: "vector"
  #   variable respuesta.
  #   
  # cv: "integer"
  #   número de particiones de validación cruzada.
  #   
  # metrica: ("-mse", "-mae)
  # métrica utilizada para calcular el fitness. Por defecto se emplea -mse.
  #   
  # seed: "integer"
  #   semilla para garantizar reproducibilidad en el proceso de CV.
  #   
  # verbose: "logical"
  #   mostrar información del proceso por pantalla.
  
  # Return
  # ----------
  # fitness ("numeric") del individuo obtenido por validación cruzada empleando
  # regresión lineal por mínimos cuadrados modelo.
  
  
  # LIBRERÍAS
  # ------------------------------------------------------------------------
  library(MLmetrics)
  library(caret)
  
  
  # COMPROBACIONES INICIALES
  # ------------------------------------------------------------------------
  if (!is.numeric(y)) {
    stop(
      "El método lm solo es valido para problemas de regresión."
    )
  }
  
  if (!is.null(metrica)) {
    if (!metrica %in% c("-mse", "-mae")) {
      stop(
        paste(
          "La métrica", metrica, "no es valida para problemas de regresión.",
          "Emplear el -mse o -mae."
        )
      )
    }
  }
  
  if (is.null(metrica)) {
    metrica <- "-mse"
  }
  
  # REPARTO DE LAS OBSERVACIONES PARA LA VALIDACIÓN CRUZADA (CV)
  # ------------------------------------------------------------------------
  set.seed(seed)
  indices_cv <- caret::createFolds(y = y, k = cv, list = FALSE)
  
  # BUCLE DE ENTRENAMIENTO Y VALIDACIÓN CRUZADA (CV)
  # ------------------------------------------------------------------------
  # Vector para almacenar la métrica de cada iteración CV.
  metrica_cv <- rep(NA, times = cv)
  
  for (i in 1:cv) {
    
    datos_modelo <- as.data.frame(
      cbind(
        x[indices_cv != i, , drop = FALSE],
        y = y[indices_cv != i]
      )
    )
    
    modelo <- lm(
                formula = y ~ .,
                data = datos_modelo
              )
    
    predicciones <- predict(
                      modelo,
                      newdata = as.data.frame(
                                  x[indices_cv == i, , drop = FALSE]
                                )
                    )
    
    residuos <- predicciones - y[indices_cv == i]
    
    if (metrica == "-mse") {
      metrica_cv[i] <- -(mean(residuos^2))
    }
    if (metrica == "-mae") {
      metrica_cv[i] <- -(mean(abs(residuos)))
    }
    
    # Información detalla de la partición en caso de que el cálculo de la 
    # métrica falle.
    if (verbose) {
      if (is.na(metrica_cv[i])) {
        print(paste("En la particion", i, "de CV, fitness del individuo no",
                    "ha podido ser calculado."
        ))
        print("Valores reales:")
        print(y[indices_cv == i])
        print("Valores predichos:")
        print(predicciones)
      }
    }
  }
  
  # COMPROBACIÓN DE LAS MÉTRICAS OBTENIDAS POR CV
  # ------------------------------------------------------------------------
  if (any(is.na(metrica_cv))) {
    warning(paste(
      "En una o más particiones de CV, la métrica del individuo no",
      "ha podido ser calculado."
    ))
  }
  
  if (is.na(mean(metrica_cv, na.rm = TRUE))) {
    stop("El fitness del individuo no ha podido ser calculado.")
  }
  
  valor_metrica <- mean(metrica_cv, na.rm = TRUE)
  fitness       <- mean(metrica_cv, na.rm = TRUE)
  modelo        <- "Regresión lineal (lm)"
  
  # INFORMACIÓN DEL PROCESO (VERBOSE)
  # ------------------------------------------------------------------------
  if (verbose) {
    cat("El individuo ha sido evaluado", "\n")
    cat("-----------------------------", "\n")
    cat("Métrica:", metrica, "\n")
    cat("Valor métrica:", valor_metrica, "\n")
    cat("Fitness:", fitness, "\n")
    cat("Modelo de evaluación:", modelo, "\n")
    cat("\n")
  }
  
  return(fitness)
}

calcular_fitness_individuo_glm <- function(x,
                                           y,
                                           cv,
                                           metrica = NULL,
                                           nivel_referencia = NULL,
                                           seed    = 123,
                                           verbose = TRUE,
                                           ...) {
  # Parameters
  # ----------
  # x: "matrix", "data.frame" , "tibble"
  #   matriz de predictores.
  #   
  # y: "vector"
  #   variable respuesta.
  #   
  # cv: "integer"
  #   número de particiones de validación cruzada.
  #   
  # seed: "integer"
  #   semilla para garantizar reproducibilidad en el proceso de CV.
  #   
  # metrica: {f1, kappa, accuracy}
  #   métrica utilizada para calcular el fitness. Por defecto es accuracy.
  #   
  # nivel_referencia: "character"
  #   valor de la variable respuesta considerado como referencia. Necesario
  #   cuando la métrica es f1 o kappa.
  #                   
  # verbose: "logical"
  #   mostrar información del proceso por pantalla.
  
  # Return
  # ----------
  # fitness ("numeric") del individuo obtenido por validación cruzada empleando
  # regresión logística como modelo.
  
  # LIBRERÍAS
  # ------------------------------------------------------------------------
  library(MLmetrics)
  library(caret)
  
  
  # COMPROBACIONES INICIALES
  # ------------------------------------------------------------------------
  if (!is.null(metrica)) {
    if (!metrica %in% c("accuracy", "f1", "kappa")) {
      stop(
        "Las métricas permitidas con el modelo glm son: accuracy, f1 y kappa."
      )
    }
  }
  
  if (is.null(metrica)) {
    metrica <- "accuracy"
  }
  
  if (!is.character(y) & !is.factor(y)) {
    stop(
      paste(
        "El modelo glm solo puede emplearse para problemas de clasificación,",
        "la variable respuesta debe ser character o factor."
      )
    )
  }
  
  if (is.character(y)) {
    y <- as.factor(y)
  }
  
  if (length(levels(y)) != 2) {
    stop(
      paste(
        "El modelo glm solo puede emplearse para problemas de clasificación binaria,",
        "la variable respuesta debe tener dos niveles."
      )
    )
  }
  

  # REPARTO DE LAS OBSERVACIONES PARA LA VALIDACIÓN CRUZADA (CV)
  # ------------------------------------------------------------------------
  set.seed(seed)
  indices_cv <- caret::createFolds(y = y, k = cv, list = FALSE)
  
  # BUCLE DE ENTRENAMIENTO Y VALIDACIÓN CRUZADA (CV)
  # ------------------------------------------------------------------------
  # Vector para almacenar la métrica de cada iteración CV.
  metrica_cv <- rep(NA, times = cv)
  
  for (i in 1:cv) {
    datos_modelo <- as.data.frame(
      cbind(
        x[indices_cv != i, , drop = FALSE],
        y = y[indices_cv != i]
      )
    )
    
    modelo <- glm(
                formula = y ~ .,
                family = "binomial",
                data = datos_modelo
              )
    
    predicciones <- predict(
                      modelo,
                      newdata = as.data.frame(
                                  x[indices_cv == i, , drop = FALSE]
                                ),
                      type = "response"
                    )
    predicciones <- unname(predicciones)
    predicciones <- ifelse(predicciones < 0.5, levels(y)[1], levels(y)[2])
    predicciones <- factor(x = predicciones, levels = levels(y))
    
    if (metrica == "accuracy") {
      accuracy <- MLmetrics::Accuracy(
        y_pred = predicciones,
        y_true = y[indices_cv == i]
      )
      metrica_cv[i] <- accuracy
    } else if (metrica == "kappa") {
      mat_confusion <- caret::confusionMatrix(
        table(
          predicciones,
          y[indices_cv == i]
        ),
        positive = nivel_referencia
      )
      kappa <- mat_confusion$overall["Kappa"]
      metrica_cv[i] <- kappa
    } else if (metrica == "f1") {
      if (sum(predicciones == nivel_referencia) < 1) {
        print(
          paste(
            "Ningún valor predicho ha sido el nivel de referencia,",
            "por lo tanto, el valor de precisión no puede ser calculado",
            "y tampoco la métrica F1."
          )
        )
      } else {
        f1 <- MLmetrics::F1_Score(
          y_true = y[indices_cv == i],
          y_pred = predicciones,
          positive = nivel_referencia
        )
        metrica_cv[i] <- f1
      }
    }
    
    # Información detalla de la partición en caso de que el cálculo de la 
    # métrica falle.
    if (verbose) {
      if (is.na(metrica_cv[i])) {
        print(paste("En la particion", i, "de CV, fitness del individuo no",
                    "ha podido ser calculado."
        ))
        print("Valores reales:")
        print(y[indices_cv == i])
        print("Valores predichos:")
        print(predicciones)
      }
    }
  }
  
  # COMPROBACIÓN DE LAS MÉTRICAS OBTENIDAS POR CV
  # ------------------------------------------------------------------------
  if (any(is.na(metrica_cv))) {
    warning(paste(
      "En una o más particiones de CV, la métrica del individuo no",
      "ha podido ser calculado."
    ))
  }
  
  if (is.na(mean(metrica_cv, na.rm = TRUE))) {
    stop("El fitness del individuo no ha podido ser calculado.")
  }
  
  valor_metrica <- mean(metrica_cv, na.rm = TRUE)
  fitness       <- mean(metrica_cv, na.rm = TRUE)
  modelo        <- "Regresión Logística (glm)"
  
  # INFORMACIÓN DEL PROCESO (VERBOSE)
  # ------------------------------------------------------------------------
  if (verbose) {
    cat("El individuo ha sido evaluado", "\n")
    cat("-----------------------------", "\n")
    cat("Métrica:", metrica, "\n")
    cat("Valor métrica:", valor_metrica, "\n")
    cat("Fitness:", fitness, "\n")
    cat("Modelo de evaluación:", modelo, "\n")
    cat("\n")
  }
  
  return(fitness)
}

calcular_fitness_poblacion <- function(poblacion,
                                       x,
                                       y,
                                       modelo,
                                       cv,
                                       seed = 123,
                                       metrica = NULL,
                                       nivel_referencia = NULL,
                                       verbose = TRUE) {
  # Parameters
  # ----------
  # poblacion: "matrix" 
  #   matriz boleana que representa la población de individuos.
  #   
  # x: "matrix" o "data.frame" 
  #   matriz de predictores.
  #   
  # y: "vector"
  #   variable respuesta.
  #   
  # cv: "integer"
  #   número de particiones de validación cruzada.
  #   
  # seed: "integer"
  #   semilla para garantizar reproducibilidad en el proceso de CV.
  # 
  # modelo: ("lm", "glm", "randomforest").  
  #   tipo de modelo empleado para calcular el fitness.
  #   
  # metrica: ("-mse", "-mae", "f1", "kappa", "accuracy").
  #   métrica utilizada para calcular el fitness. Por defecto es el -mse para
  #   regresión y accuracy para clasificación. En el caso de clasificación
  #   binaria, se acepta también f1 y kappa.
  #   
  # nivel_referencia: "character"
  #   valor de la variable respuesta considerado como referencia. Necesario
  #   cuando la métrica es f1 o kappa.
  #                   
  # verbose: "logical"
  #   mostrar información del proceso por pantalla.
  #   
  # Return
  # ----------
  # Vector ("numeric") con el fitness de todos los individuos de la población,
  # obtenido por validación cruzada. El orden de los valores se corresponde con
  # el orden de las filas de la matriz población.
  
  # COMPROBACIONES INICIALES
  # ----------------------------------------------------------------------------
  if (!modelo %in% c("lm", "glm", "randomforest")) {
    stop(
      paste("El modelo empleado para calcular el fitness debe ser",
            "lm (regresion lineal)",
            "glm (regresión logistica)",
            "o randomforest."
      )
    )
  }

  if (ncol(poblacion) != ncol(x)) {
    stop(
      "n_variables de la población debe ser igual al número de columnas de x."
    )
  }
  
  # Si el modelo es de regresión lineal
  if (modelo == "lm") {
    if (!is.numeric(y)) {
      stop(
        "El método lm solo es valido para problemas de regresión."
      )
    }
    
    if (!is.null(metrica)) {
      if (!metrica %in% c("-mse", "-mae")) {
        stop(
          paste(
            "La métrica", metrica, "no es valida para problemas de regresión.",
            "Emplear el -mse o -mae."
          )
        )
      }
    }else{
      # Por defecto se emplea -mse
      metrica <- "-mse"
    }
  }
  
  # Si el modelo es de regresión logística
  if (modelo == "glm") {
    
    if (!is.null(metrica)) {
      if (!metrica %in% c("accuracy", "f1", "kappa")) {
        stop(
          "Las métricas permitidas con el modelo glm son: accuracy, f1 y kappa."
        )
      }
    }else{
      # Por defecto se emplea accuracy
      metrica <- "accuracy"
    }
    
    if (!is.character(y) & !is.factor(y)) {
      stop(
        paste(
          "El modelo glm solo puede emplearse para problemas de clasificación,",
          "la variable respuesta debe ser character o factor."
        )
      )
    }
    
    if (is.character(y)) {
      y <- as.factor(y)
    }
    
    if (length(levels(y)) != 2) {
      stop(
        paste(
          "El modelo glm solo puede emplearse para problemas de clasificación binaria,",
          "la variable respuesta debe tener dos niveles."
        )
      )
    }
  }
  
  # Si el modelo es de random forest
  if (modelo == "randomforest") {
    
    if (!is.null(metrica)) {
      if (metrica %in% c("-mse", "-mae") && !is.numeric(y)) {
        stop(
          paste(
            "La métrica", metrica, "no es valida para problemas de regresión.",
            "Emplear el -mse o -mae."
          )
        )
      }
      if (metrica %in% c("accuracy", "f1", "kappa") && is.numeric(y)) {
        stop(
          paste(
            "La métrica", metrica, "no es valida para problemas de clasificación.",
            "Emplear accuracy, f1, kappa."
          )
        )
      }
      if (metrica %in% c("f1", "kappa") && length(unique(y)) > 2) {
        stop(
          paste(
            "La métrica", metrica, "no es valida para problemas de clasificación",
            "con más de dos clases. Emplear accuracy."
          )
        )
      }
      if (metrica %in% c("f1", "kappa") && is.null(nivel_referencia)) {
        stop(
          "Para la métrica kappa es necesario indicar el nivel de referencia."
        )
      }
    }
    
    if (is.null(metrica)) {
      if (is.numeric(y)) {
        metrica <- "-mse"
      }else{
        metrica <- "accuracy"
      }
    }
  }
  
  # SELECCIÓN FUNCIÓN AUXILIAR PARA CALCULAR EL FITNESS
  # ----------------------------------------------------------------------------
  # Tipo de modelo utilizado para calcular el fitness.
  if (modelo == "lm") {
    calcular_fitness_individuo <- calcular_fitness_individuo_lm
  } else if (modelo == "randomforest") {
    calcular_fitness_individuo <- calcular_fitness_individuo_rf
  } else if (modelo == "glm") {
    calcular_fitness_individuo <- calcular_fitness_individuo_glm
  }

  # CÁLCULO DEL FITNNES DE CADA INDIVIDUO DE LA POBLACIÓN
  # ----------------------------------------------------------------------------
  # Vector donde almacenar el fitness de cada individuo.
  fitness_poblacion <- rep(NA, times = nrow(poblacion))
  
  for (i in 1:nrow(poblacion)) {
    individuo <- poblacion[i, ]
    
    if (verbose) {
      print(paste("Individuo", i, ":", paste(individuo, collapse = " ")))
    }
    
    fitness_individuo <- calcular_fitness_individuo(
                          x  = x[, individuo, drop = FALSE],
                          y  = y,
                          cv = cv,
                          metrica = metrica,
                          nivel_referencia = nivel_referencia,
                          seed = seed,
                          verbose = verbose
                        )
    fitness_poblacion[i] <- fitness_individuo
  }
  
  # MEJOR INDIVIDUO DE LA POBLACIÓN
  # ------------------------------------------------------------------------
  # Se identifica el mejor individuo de toda la población, el de mayor
  # fitness.
  indice_mejor_individuo <- which.max(fitness_poblacion)

  # INFORMACIÓN DEL PROCESO (VERBOSE)
  # ----------------------------------------------------------------------------
  if (verbose) {
    cat("------------------", "\n")
    cat("Población evaluada", "\n")
    cat("------------------", "\n")
    cat("Modelo empleado para el cálculo del fitness:", modelo, "\n")
    cat("Métrica empleado para el cálculo del fitness:", metrica, "\n")
    cat("Mejor fitness encontrado:", fitness_poblacion[indice_mejor_individuo], "\n")
    cat("Mejor secuencia encontrada:", "\n")
    print(poblacion[indice_mejor_individuo,])
    cat("Mejores predictores encontrados:", "\n")
    print(colnames(x)[poblacion[indice_mejor_individuo,]])
    cat("\n")
  }
  
  return(fitness_poblacion)
}

seleccionar_individuo <- function(vector_fitness,
                                  metrica,
                                  metodo_seleccion = "tournament",
                                  verbose = TRUE) {
  # Parameters
  # ----------
  # vector_fitness: "numeric"
  #   un vector con el fitness de cada individuo.
  #   
  # metrica: ("-mse", "-mae", f1", "kappa", "accuracy")
  #   métrica empleada para calcular el fitness.
  #   
  # metodo_seleccion: ("ruleta", "rank", o "tournament").
  #   método para establecer la probabilidad de selección.
  
  # Return
  # ----------
  # El índice ("integer") que ocupa el individuo seleccionado.
  
  # COMPROBACIONES INICIALES
  # ----------------------------------------------------------------------
  if (!metodo_seleccion %in% c("ruleta", "rank", "tournament")) {
    stop("El método de selección debe de ser ruleta, rank o tournament.")
  }
  
  # SELECCIÓN DE INDIVIDUOS
  # ----------------------------------------------------------------------
  # Se calcula la probabilidad de selección de cada individuo en función
  # de su fitness.
  if (metodo_seleccion == "ruleta") {
    if (metrica %in% c("-mse", "-mae")) {
      # Si el fitness es [-inf,0] se emplea 1/fitness
      vector_fitness <- 1/vector_fitness
    }
    probabilidad_seleccion <- vector_fitness / sum(vector_fitness)
    ind_seleccionado <- sample(
      x    = 1:length(vector_fitness),
      size = 1,
      prob = probabilidad_seleccion,
      replace = TRUE
    )
  }else if (metodo_seleccion == "rank") {
    # La probabilidad con este método es inversamente proporcional a la
    # posición en la que quedan ordenados los individuos de menor a mayor
    # fitness.
    if (metrica %in% c("-mse", "-mae")) {
      vector_fitness <- (-1/vector_fitness)
    }
    probabilidad_seleccion <- 1 / rank(-1*vector_fitness)
    probabilidad_seleccion <- probabilidad_seleccion / sum(probabilidad_seleccion)
    
    ind_seleccionado <- sample(
      x    = 1:length(vector_fitness),
      size = 1,
      prob = probabilidad_seleccion
    )
  }else if (metodo_seleccion == "tournament") {
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
    cat("Método selección:", metodo_seleccion, "\n")
    cat("Índice seleccionado:", ind_seleccionado, "\n")
  }
  
  return(ind_seleccionado)
}


cruzar_individuos <- function(parental_1,
                              parental_2,
                              metodo_cruce = "uniforme",
                              verbose = TRUE) {
  # Esta función devuelve un individuo resultado de cruzar dos individuos
  # parentales con el método de cruzamiento uniforme.
  #
  # Parameters
  # ----------
  # parental_1: 
  #   vector que representa a un individuo.
  # parental_2: 
  #   vector que representa a un individuo.
  # metodo_cruce: (uniforme", "punto_simple")
  #   estrategia de cruzamiento.
  
  # Return
  # ----------
  # Un vector que representa a un nuevo individuo.
  
  # Para crear el nuevo individuo, se emplea el método de cruzamiento uniforme,
  # con la misma probabilidad de que el valor proceda de cada parental.
  
  if (length(parental_1) != length(parental_2)) {
    stop(paste0(
      "La longitud de los dos vectores que representan a los ",
      "individuos debe ser la misma."
    ))
  }
  if (!(metodo_cruce %in% c("uniforme", "punto_simple"))) {
    stop("El método de cruzamiento debe ser: uniforme o punto_simple.")
  }
  
  # Se crea el vector que representa el nuevo individuo
  descendencia <- rep(NA, times = length(parental_1))
  
  if (metodo_cruce == "uniforme") {
    # Se seleccionan aleatoriamente las posiciones que se heredan del parental_1.
    herencia_parent_1 <- sample(
                            x = c(TRUE, FALSE),
                            size = length(parental_1),
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
    print("---------------")
    print("Cruce realizado")
    print("---------------")
    print(paste("Método:", metodo_cruce))
    cat("\n")
    print("Parental 1")
    print(parental_1)
    print("Parental 2")
    print(parental_2)
    cat("\n")
    print("Descendencia")
    print(descendencia)
    cat("\n")
  }
  return(descendencia)
}


mutar_individuo <- function(individuo, prob_mut = 0.01) {
  # Este método somete al individuo a un proceso de mutación en el que, cada
  # una de sus posiciones, puede verse modificada con una probabilidad `prob_mut`.
  # 
  # Parameters
  # ----------
  # individuo: 
  #   vector que representa a un individuo.
  #   
  # prob_mut:  
  #   probabilidad que tiene cada posición del vector de mutar.
  #   
  # Return
  # ----------
  # Un vector que representa al individuo tras someterse a las mutaciones.
  
  # Selección de posiciones a mutar.
  posiciones_mutadas <- runif(n = length(individuo), min = 0, max = 1) < prob_mut
  
  # Se modifica el valor de aquellas posiciones que hayan sido seleccionadas para
  # mutar. Si el valor de prob_mut es muy bajo, las mutaciones serán muy poco
  # frecuentes y el individuo devuelto será casi siempre igual al original.
  individuo[posiciones_mutadas] <- !(individuo[posiciones_mutadas])
  return(individuo)
}


forzar_n_max = function(individuo, n_max){
  
  # Este método modifica la secuencia del individuo de forma que, como máximo
  # contenga n_max valores TRUE.
  # 
  # Parameters
  # ----------
  # individuo: 
  #   vector que representa a un individuo.
  #   
  # n_max:  
  #   número máximo de posiciones TRUE que puede tener el individuo.
  
  # Return
  # ----------
  # Un vector que representa al individuo tras someterse a la corrección
  
  # Se identifica si el número de TRUE es la secuencia supera a n_max.
  n_exceso <- sum(individuo) - n_max
  
  if (n_exceso > 0) {
    # Se seleccionan aleatoriamente n_max posiciones con valor TRUE en 
    # la secuencia del individuo.
    indices <- sample(
      x = which(individuo == TRUE),
      size = n_exceso,
      replace = FALSE
    )
    individuo[indices] <- !(individuo[indices])
  }
  
  return(individuo)
}


calcular_fitness_poblacion_paral <- function(poblacion,
                                       x,
                                       y,
                                       modelo,
                                       cv,
                                       seed = 123,
                                       metrica = NULL,
                                       nivel_referencia = NULL,
                                       verbose = TRUE) {
  # Parameters
  # ----------
  # poblacion: "matrix" 
  #   matriz boleana que representa la población de individuos.
  #   
  # x: "matrix" o "data.frame" 
  #   matriz de predictores.
  #   
  # y: "vector"
  #   variable respuesta.
  #   
  # cv: "integer"
  #   número de particiones de validación cruzada.
  #   
  # seed: "integer"
  #   semilla para garantizar reproducibilidad en el proceso de CV.
  # 
  # modelo: ("lm", "glm", "randomforest").  
  #   tipo de modelo empleado para calcular el fitness.
  #   
  # metrica: ("-mse", "-mae", "f1", "kappa", "accuracy").
  #   métrica utilizada para calcular el fitness. Por defecto es el -mse para
  #   regresión y accuracy para clasificación. En el caso de clasificación
  #   binaria, se acepta también f1 y kappa.
  #   
  # nivel_referencia: "character"
  #   valor de la variable respuesta considerado como referencia. Necesario
  #   cuando la métrica es f1 o kappa.
  #                   
  # verbose: "logical"
  #   mostrar información del proceso por pantalla.
  #   
  # Return
  # ----------
  # Vector ("numeric") con el fitness de todos los individuos de la población,
  # obtenido por validación cruzada. El orden de los valores se corresponde con
  # el orden de las filas de la matriz población.
  
  # LIBRERIAS NECESARIAS
  # ----------------------------------------------------------------------------
  library(foreach)
  library(doParallel)
  
  # COMPROBACIONES INICIALES
  # ----------------------------------------------------------------------------
  if (!modelo %in% c("lm", "glm", "randomforest")) {
    stop(
      paste("El modelo empleado para calcular el fitness debe ser",
            "lm (regresion lineal)",
            "glm (regresión logistica)",
            "o randomforest."
      )
    )
  }
  
  if (ncol(poblacion) != ncol(x)) {
    stop(
      "n_variables de la población debe ser igual al número de columnas de x."
    )
  }
  
  # Si el modelo es de regresión lineal
  if (modelo == "lm") {
    if (!is.numeric(y)) {
      stop(
        "El método lm solo es valido para problemas de regresión."
      )
    }
    
    if (!is.null(metrica)) {
      if (!metrica %in% c("-mse", "-mae")) {
        stop(
          paste(
            "La métrica", metrica, "no es valida para problemas de regresión.",
            "Emplear el -mse o -mae."
          )
        )
      }
    }else{
      # Por defecto se emplea -mse
      metrica <- "-mse"
    }
  }
  
  # Si el modelo es de regresión logística
  if (modelo == "glm") {
    
    if (!is.null(metrica)) {
      if (!metrica %in% c("accuracy", "f1", "kappa")) {
        stop(
          "Las métricas permitidas con el modelo glm son: accuracy, f1 y kappa."
        )
      }
    }else{
      # Por defecto se emplea accuracy
      metrica <- "accuracy"
    }
    
    if (!is.character(y) & !is.factor(y)) {
      stop(
        paste(
          "El modelo glm solo puede emplearse para problemas de clasificación,",
          "la variable respuesta debe ser character o factor."
        )
      )
    }
    
    if (is.character(y)) {
      y <- as.factor(y)
    }
    
    if (length(levels(y)) != 2) {
      stop(
        paste(
          "El modelo glm solo puede emplearse para problemas de clasificación binaria,",
          "la variable respuesta debe tener dos niveles."
        )
      )
    }
  }
  
  # Si el modelo es de random forest
  if (modelo == "randomforest") {
    
    if (!is.null(metrica)) {
      if (metrica %in% c("-mse", "-mae") && !is.numeric(y)) {
        stop(
          paste(
            "La métrica", metrica, "no es valida para problemas de regresión.",
            "Emplear el -mse o -mae."
          )
        )
      }
      if (metrica %in% c("accuracy", "f1", "kappa") && is.numeric(y)) {
        stop(
          paste(
            "La métrica", metrica, "no es valida para problemas de clasificación.",
            "Emplear accuracy, f1, kappa."
          )
        )
      }
      if (metrica %in% c("f1", "kappa") && length(unique(y)) > 2) {
        stop(
          paste(
            "La métrica", metrica, "no es valida para problemas de clasificación",
            "con más de dos clases. Emplear accuracy."
          )
        )
      }
      if (metrica %in% c("f1", "kappa") && is.null(nivel_referencia)) {
        stop(
          "Para la métrica kappa es necesario indicar el nivel de referencia."
        )
      }
    }
    
    if (is.null(metrica)) {
      if (is.numeric(y)) {
        metrica <- "-mse"
      }else{
        metrica <- "accuracy"
      }
    }
  }
  
  # SELECCIÓN FUNCIÓN AUXILIAR PARA CALCULAR EL FITNESS
  # ----------------------------------------------------------------------------
  # Tipo de modelo utilizado para calcular el fitness.
  if (modelo == "lm") {
    calcular_fitness_individuo <- calcular_fitness_individuo_lm
  } else if (modelo == "randomforest") {
    calcular_fitness_individuo <- calcular_fitness_individuo_rf
  } else if (modelo == "glm") {
    calcular_fitness_individuo <- calcular_fitness_individuo_glm
  }
  
  # CÁLCULO DEL FITNNES DE CADA INDIVIDUO DE LA POBLACIÓN
  # ----------------------------------------------------------------------------
  # Vector donde almacenar el fitness de cada individuo.
  fitness_poblacion <- rep(NA, times = nrow(poblacion))
  
  # Se emplean todos los cores del ordenador menos 1.
  registerDoParallel(parallel::detectCores() - 1)
  
  # Se emplea la función foreach para paralelizar.
  fitness_poblacion <- foreach::foreach(i = 1:nrow(poblacion)) %dopar% {
    
    individuo <- poblacion[i, ]
    
    if (verbose) {
      print(paste("Individuo", i, ":", paste(individuo, collapse = " ")))
    }
    
    fitness_individuo <- calcular_fitness_individuo(
      x = x[, individuo, drop = FALSE],
      y = y,
      cv = cv,
      seed = seed,
      metrica = metrica,
      nivel_referencia = nivel_referencia,
      verbose = verbose
    )
    fitness_individuo
  }
  
  fitness_poblacion <- unlist(fitness_poblacion)
  # Se vuelve a un único core.
  options(cores = 1)
  
  # MEJOR INDIVIDUO DE LA POBLACIÓN
  # ------------------------------------------------------------------------
  # Se identifica el mejor individuo de toda la población, el de mayor
  # fitness.
  indice_mejor_individuo <- which.max(fitness_poblacion)
  
  # INFORMACIÓN DEL PROCESO (VERBOSE)
  # ----------------------------------------------------------------------------
  if (verbose) {
    cat("------------------", "\n")
    cat("Población evaluada", "\n")
    cat("------------------", "\n")
    cat("Modelo empleado para el cálculo del fitness:", modelo, "\n")
    cat("Métrica empleado para el cálculo del fitness:", metrica, "\n")
    cat("Mejor fitness encontrado:", fitness_poblacion[indice_mejor_individuo], "\n")
    cat("Mejor secuencia encontrada:", "\n")
    print(poblacion[indice_mejor_individuo,])
    cat("Mejores predictores encontrados:", "\n")
    print(colnames(x)[poblacion[indice_mejor_individuo,]])
    cat("\n")
  }
  
  return(fitness_poblacion)
}

seleccionar_predictores_ga <- function(
                                   x,
                                   y,
                                   n_poblacion       = 20,
                                   n_generaciones    = 10,
                                   n_max_predictores = NULL,
                                   n_max_estricto    = FALSE,
                                   n_min_predictores = NULL,
                                   modelo            = "lm",
                                   cv                = 5,
                                   metrica           = NULL,
                                   nivel_referencia  = NULL,
                                   elitismo          = 0.1,
                                   prob_mut          = 0.01,
                                   metodo_seleccion  = "tournament",
                                   metodo_cruce      = "uniforme",
                                   n_tree            = 50,
                                   parada_temprana   = FALSE,
                                   rondas_parada     = NULL,
                                   tolerancia_parada = NULL,
                                   paralelizado      = FALSE,
                                   verbose           = TRUE,
                                   ...) {
  # Parameters
  # ----------
  # x: "matrix" o "data.frame" 
  #   matriz de predictores.
  #   
  # y: "vector"
  #   variable respuesta.
  #   
  # n_poblacion: "integer"
  #   número total de individuos de la población.
  #   
  # n_variables: "integer"
  #   longitud de los individuos.
  #
  # n_max_predictores: "integer"
  #   número máximo de predictores que puede contener un individuo.
  #   
  # n_min_predictores: "integer"
  #   número mínimo de predictores que puede contener un individuo.
  #   
  # modelo: ("lm", "glm", "randomforest").  
  #   tipo de modelo empleado para calcular el fitness.
  #   
  # cv: "integer"
  #   número de particiones de validación cruzada.
  #   
  # seed: "integer"
  #   semilla para garantizar reproducibilidad en el proceso de CV.
  #   
  # metrica: ("-mse", "mae", "f1", "kappa", "accuracy")
  #   métrica utilizada para calcular el fitness. Por defecto es el -mse para
  #   regresión y accuracy para clasificación. En el caso de clasificación
  #   binaria, se acepta también f1 y kappa.
  #   
  # nivel_referencia: "character"
  #   valor de la variable respuesta considerado como referencia. Necesario
  #   cuando la métrica es f1 o kappa.
  # 
  # elitismo: "numeric"
  #   porcentaje de mejores individuos de la población actual que pasan 
  #   directamente a la siguiente población. De esta forma, se asegura que, la 
  #   siguiente generación, no sea nunca peor.
  #
  # prob_mut: "numeric"
  #   probabilidad que tiene cada posición del individuo de mutar.
  #   
  # metodo_seleccion: ("ruleta", "rank", "tournament")
  #   método de selección de los individuos para los cruces.
  #   
  # metodo_cruce: (uniforme", "punto_simple")
  #   estrategia de cruzamiento.
  #   
  # parada_temprana: "logical"
  #   si durante las últimas `rondas_parada` generaciones la diferencia absoluta
  #   entre mejores individuos no es superior al valor de `tolerancia_parada`,
  #   se detiene el algoritmo y no se crean nuevas generaciones.
  #   
  # rondas_parada: "int"
  #   número de generaciones consecutivas sin mejora mínima para que se active
  #   la parada temprana.
  #   
  # tolerancia_parada: "numeric"
  #   valor mínimo que debe tener la diferencia de generaciones consecutivas 
  #   para considerar que hay cambio.
  # 
  # n_tree: "integer"
  #   número de árboles del modelo randomforest.       
  # 
  # paralelizado: "logical"
  #   si se paraleliza o no el algoritmo de busqueda.
  #              
  # verbose: "logical"
  #   mostrar información del proceso por pantalla.

  # Return
  # ----------
  # Objeto "lista" con los resultados del algoritmo genético:
  # fitness: fitness del mejor individuo de cada generación.
  # mejor_individuo: información del mejor individuo de cada generación.
  # diferencia_abs: mejora respecto a la generación anterior.
  # df_resultados: dataframe con los resultados del proceso en cada generación.
  # mejor_individuo_global: mejor individuo encontrado en todo el proceso.
  
  
  library(foreach)
  library(doParallel)
  library(randomForest)
  library(caret)
  library(MLmetrics)
  
  # COMPROBACIONES INICIALES
  # ----------------------------------------------------------------------------
  # Si se activa la parada temprana, hay que especificar los parameters
  # rondas_parada y tolerancia_parada.
  if (parada_temprana && (is.null(rondas_parada) || is.null(tolerancia_parada))) {
    stop(paste(
      "Para activar la parada temprana es necesario indicar un valor",
      "de rondas_parada y de tolerancia_parada."
    ))
  }
  
  start_time <- Sys.time()
  
  # ALMACENAMIENTO DE RESULTADOS
  # ----------------------------------------------------------------------------
  # Por cada generación se almacena el mejor individuo, su fitness, y el porcentaje
  # de mejora respecto a la última generación.
  resultados_fitness   <- vector(mode = "list", length = n_generaciones)
  resultados_individuo <- vector(mode = "list", length = n_generaciones)
  diferencia_abs       <- vector(mode = "list", length = n_generaciones)
  
  # CREACIÓN DE LA POBLACIÓN INICIAL
  # ----------------------------------------------------------------------------
  poblacion <- crear_poblacion(
                  n_poblacion = n_poblacion,
                  n_variables = ncol(x),
                  n_max       = n_max_predictores,
                  n_min       = n_min_predictores,
                  verbose     = verbose
                )
  
  # ITERACIÓN DE POBLACIONES
  # ----------------------------------------------------------------------------
  for (i in 1:n_generaciones) {
    if (verbose) {
      print("---------------------")
      print(paste("Generación:", i))
      print("---------------------")
    }
    
    
    # CALCULAR FITNESS DE LOS INDIVIDUOS DE LA POBLACIÓN
    # --------------------------------------------------------------------------
    if (!paralelizado) {
      fitness_ind_poblacion <- calcular_fitness_poblacion(
                                  poblacion = poblacion,
                                  x         = x,
                                  y         = y,
                                  modelo    = modelo,
                                  cv        = cv,
                                  metrica   = metrica,
                                  nivel_referencia = nivel_referencia,
                                  verbose   = verbose
                                )
    }
    
    if (paralelizado) {
      fitness_ind_poblacion <- calcular_fitness_poblacion_paral(
                                  poblacion = poblacion,
                                  x         = x,
                                  y         = y,
                                  modelo    = modelo,
                                  cv        = cv,
                                  metrica   = metrica,
                                  nivel_referencia = nivel_referencia,
                                  verbose   = verbose
                              )
    }
    
    # SE ALMACENA EL MEJOR INDIVIDUO DE LA POBLACIÓN ACTUAL
    # --------------------------------------------------------------------------
    fitness_mejor_individuo   <- max(fitness_ind_poblacion)
    mejor_individuo           <- poblacion[which.max(fitness_ind_poblacion), ]
    resultados_fitness[[i]]   <- fitness_mejor_individuo
    resultados_individuo[[i]] <- colnames(x)[mejor_individuo]
    
    # SE CALCULA LA MEJORA RESPECTO A LA GENERACIÓN ANTERIOR
    # ==========================================================================
    # La mejora solo puede calcularse a partir de la segunda generación.
    if (i > 1) {
      diferencia_abs[[i]] <- abs(resultados_fitness[[i]]- resultados_fitness[[i-1]])
    }
    
    # NUEVA POBLACIÓN
    # ==========================================================================
    nueva_poblacion <- matrix(
                          data = NA,
                          nrow = nrow(poblacion),
                          ncol = ncol(poblacion)
                        )
    
    # ELITISMO
    # ==========================================================================
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
    # ==========================================================================
    for (j in (n_elitismo + 1):nrow(nueva_poblacion)) {
      # Seleccionar parentales
      indice_parental_1 <- seleccionar_individuo(
                              vector_fitness   = fitness_ind_poblacion,
                              metrica          = metrica,
                              metodo_seleccion = metodo_seleccion,
                              verbose          = verbose
                            )
      indice_parental_2 <- seleccionar_individuo(
                            vector_fitness   = fitness_ind_poblacion,
                            metrica          = metrica,
                            metodo_seleccion = metodo_seleccion,
                            verbose          = verbose
                          )
      parental_1 <- poblacion[indice_parental_1, ]
      parental_2 <- poblacion[indice_parental_2, ]
      
      # Cruzar parentales para obtener la descendencia
      descendencia <- cruzar_individuos(
                        parental_1   = parental_1,
                        parental_2   = parental_2,
                        metodo_cruce = metodo_cruce,
                        verbose      = verbose
                      )
      # Mutar la descendencia
      descendencia <- mutar_individuo(
                        individuo = descendencia,
                        prob_mut  = prob_mut
                      )
      # Si n_max_estricto=TRUE, se elimina el exceso de TRUEs en la
      # secuencia de la descendencia.
      if (n_max_estricto) {
        descendencia <- forzar_n_max(
                          individuo = descendencia,
                          n_max     = n_max_predictores
                        )
      }
      
      # Almacenar el nuevo descendiente en la nueva población: puede ocurrir que
      # el individuo resultante del cruce y posterior mutación no contenga ningún
      # predictor (todas sus posiciones son FALSE). Si esto ocurre, y para evitar
      # que se produzca un error al ajustar el modelo, se introduce aleatoriamente
      # un valor TRUE.
      if (all(descendencia == FALSE)) {
        descendencia[sample(x = 1:length(descendencia), size = 1)] <- TRUE
      }
      
      nueva_poblacion[j, ] <- descendencia
    }
    
    poblacion <- nueva_poblacion
    
    # CRITERIO DE PARADA
    # ==========================================================================
    # Si durante las últimas n generaciones el mejor individuo no ha mejorado por
    # una cantidad superior a tolerancia_parada, se detiene el algoritmo y no se
    # crean nuevas generaciones.
    
    if (parada_temprana && (i > rondas_parada)) {
      ultimos_n <- tail(unlist(diferencia_abs), n = rondas_parada)
      if (all(ultimos_n < tolerancia_parada)) {
        print(paste(
          "Algoritmo detenido en la generacion", i,
          "por falta de mejora mínima de", tolerancia_parada,
          "durante", rondas_parada,
          "generaciones consecutivas."
        ))
        break()
      }
    }
  }
  
  # IDENTIFICACIÓN DEL MEJOR INDIVIDUO DE TODO EL PROCESO
  # ----------------------------------------------------------------------------
  indice_mejor_individuo_global <- which.max(unlist(resultados_fitness))
  mejor_fitness_global   <- resultados_fitness[[indice_mejor_individuo_global]]
  mejor_individuo_global <- resultados_individuo[[indice_mejor_individuo_global]]
  
  
  # RESULTADOS
  # ----------------------------------------------------------------------------
  # La función devuelve una lista con 4 elementos:
  # fitness:  
  #   un vector con el fitness del mejor individuo de cada generación.
  # mejor_individuo:   
  #   una lista con la combinación de predictores  del mejor individuo de cada
  #   generación.
  # diferencia_abs: 
  #   un vector con el porcentaje de mejora entre el mejor individuo de cada
  #   generación.
  # df_resultados:
  #   un dataframe con todos los resultados anteriores.
  # mejor_individuo_global: 
  #   la combinación de predictores  del mejor individuo encontrado en todo el
  #   proceso. 
  
  resultados_fitness <- unlist(resultados_fitness)
  diferencia_abs     <- c(NA, unlist(diferencia_abs))
  
  # Para poder añadir al dataframe la secuencia de predictores, se concatenan.
  predictores <- resultados_individuo[!sapply(resultados_individuo, is.null)]
  predictores <- sapply(
                  X = predictores,
                  FUN = function(x) {
                          paste(x, collapse = ", ")
                        }
                  )
  
  df_resultados <- data.frame(
                      generacion  = seq_along(resultados_fitness),
                      fitness     = resultados_fitness,
                      predictores = predictores,
                      mejora      = diferencia_abs
                    )
  
  # Frecuencia relativa con la que aparece cada predictor en el mejor individuo
  # a lo largo de las generaciones.
  frecuencia_seleccion <- as.data.frame(
                            x = table(unlist(resultados_individuo))
                          )
  colnames(frecuencia_seleccion) <- c("pred", "frec")

  frecuencia_seleccion <- frecuencia_seleccion[order(-frecuencia_seleccion$frec), ]
  frecuencia_seleccion$frec <- 100*(frecuencia_seleccion$frec/i)
  top_5_predictores <- as.character(head(frecuencia_seleccion, 5)$pred)
  
  resultados <- list(
                  fitness                = resultados_fitness,
                  mejor_individuo        = resultados_individuo,
                  diferencia_abs         = diferencia_abs,
                  df_resultados          = df_resultados,
                  mejor_individuo_global = mejor_individuo_global,
                  frecuencia_seleccion   = frecuencia_seleccion,
                  top_5_predictores      = top_5_predictores
                )
  
  end_time <- Sys.time()
  
  # INFORMACIÓN ALMACENADA EN LOS ATRIBUTOS
  # ----------------------------------------------------------------------------
  attr(resultados, "class") <- "seleccion_ga"
  attr(resultados, 'fecha_creacion')      <- end_time
  attr(resultados, 'duracion_selecion')   <- difftime(end_time, start_time)
  attr(resultados, 'generaciones')        <- i 
  attr(resultados, 'predictores_selecionados') <- mejor_individuo_global
  attr(resultados, 'top_5_predictores')   <- top_5_predictores
  attr(resultados, 'fitness_optimo')      <- mejor_fitness_global 
  attr(resultados, 'n_poblacion')         <- n_poblacion 
  attr(resultados, 'modelo')              <- modelo 
  attr(resultados, 'metrica')             <- metrica
  attr(resultados, 'elitismo')            <- elitismo
  attr(resultados, 'prob_mut')            <- prob_mut
  attr(resultados, 'metodo_seleccion')    <- metodo_seleccion
  attr(resultados, 'metodo_cruce')        <- metodo_cruce
  attr(resultados, 'parada_temprana')     <- parada_temprana
  attr(resultados, 'rondas_parada')       <- rondas_parada
  attr(resultados, 'tolerancia_parada')   <- tolerancia_parada

  
  # INFORMACIÓN DEL PROCESO (VERBOSE)
  # ----------------------------------------------------------------------------
  cat("--------------------", "\n")
  cat("Selección finalizada", "\n")
  cat("--------------------", "\n")
  cat("Fecha finalización:", as.character(Sys.time()), "\n")
  cat("Duración optimización: ")
  print(difftime(end_time, start_time))
  cat("Número de generaciones:", i, "\n")
  cat("Predictores seleccionados:", mejor_individuo_global,"\n")
  cat("Fitness final:", mejor_fitness_global, "\n")
  cat("Top 5 predictores:", top_5_predictores,"\n")
  cat("Modelo empleado:", modelo, "\n")
  cat("Métrica empleada:", metrica, "\n")
  cat("\n")
  
  return(resultados)
}


print.seleccion_ga <- function(obj){
  cat("-------------------------------", "\n")
  cat("Selección de predictores por GA", "\n")
  cat("-------------------------------", "\n")
  cat("Fecha creación:", as.character(attr(obj, 'fecha_creacion')), "\n")
  cat("Predictores seleccionados:", attr(obj, 'predictores_selecionados'),"\n")
  cat("Fitness final:", attr(obj, 'fitness_optimo'), "\n")
  cat("Top 5 predictores:", attr(obj, 'top_5_predictores'),"\n")
  cat("Duración optimización: ")
  print(attr(obj, 'duracion_selecion'))
  cat("Número de generaciones:", attr(obj, 'generaciones'), "\n")
  cat("Tamaño población:", attr(obj, 'n_poblacion'), "\n")
  cat("Modelo empleado:", attr(obj, 'modelo'), "\n")
  cat("Métrica empleada:", attr(obj, 'metrica'), "\n")
  cat("Probabilidad mutación:", attr(obj, 'prob_mut'), "\n")
  cat("Elitismo:", attr(obj, 'elitismo'), "\n")
  cat("Método selección:", attr(obj, 'metodo_seleccion'), "\n")
  cat("Método cruce:", attr(obj, 'metodo_cruce'), "\n")
  cat("\n")
}

plot.seleccion_ga <- function(obj){
  p1 <- ggplot(data = obj$df_resultados, aes(x = generacion, y = fitness)) +
        geom_line() +
        geom_point() +
        labs(title = 'Evolución del mejor individuo',
             x = 'generacion',
             y = 'fitness'
        ) +
        theme_bw()
  
  p2 <- ggplot(data = obj$frecuencia_seleccion,
               aes(x = reorder(pred, frec), y = frec, fill = frec)) +
        geom_col() +
        scale_fill_viridis_c() + 
        coord_flip() +
        labs(title = 'Frecuencia de selección',
             x = 'predictor',
             y = 'frecuencia'
        ) +
        theme_bw() +
        theme(legend.position = "none")
  
  print(p1)
  print(p2)
}

################################################################################
##                        EJEMPLO                                             ##
################################################################################

# library(titanic)
# library(dplyr)
# 
# # Se eliminan para este ejemplo 3 variables.
# datos <- titanic_train %>% select(-Name, -Ticket, -Cabin)
# # Se eliminan observaciones incompletas.
# datos <- na.omit(datos)
# # Se convierten a factor las variables de tipo character.
# datos <- datos %>% mutate_if(is.character, as.factor)
# datos <- datos %>% mutate(Survived = as.factor(Survived))
# 
# seleccion <- seleccionar_predictores_ga(
#                 x                = datos[, -2],
#                 y                = datos[, 2],
#                 n_poblacion      = 5,
#                 n_generaciones   = 5,
#                 n_max_predictores = 4,
#                 n_max_estricto   = TRUE,
#                 n_min_predictores = 1,
#                 elitismo         = 0.01,
#                 prob_mut         = 0.01,
#                 metodo_seleccion = "tournament",
#                 modelo           = "randomforest",
#                 metrica          = "accuracy",
#                 nivel_referencia = "1",
#                 cv               = 5,
#                 parada_temprana  = FALSE,
#                 rondas_parada    = NULL,
#                 tolerancia_parada = NULL,
#                 paralelizado     = TRUE,
#                 verbose          = FALSE
#               )
# 
# seleccion
# plot(seleccion)



library(mlbench)
library(dplyr)

set.seed(123)
simulacion <- mlbench.friedman1(n = 500, sd = 1)
# El objeto simulación es una lista que  contiene una matriz con los 10 predictores y
# un vector con la variable respuesta. Se unen todos los datos en un único dataframe.
datos           <- as.data.frame(simulacion$x)
datos$y         <- simulacion$y
colnames(datos) <- c(paste("x", 1:10, sep = ""), "y")
datos           <- datos %>% select(y, everything())
n <- 500
p <- 20
ruido           <- matrix(rnorm(n * p), nrow = n)
colnames(ruido) <- paste("x", 11:(10 + p), sep = "")
ruido           <- as.data.frame(ruido)
datos           <- bind_cols(datos, ruido)


seleccion <- seleccionar_predictores_ga(
                                    x = datos[, -1],
                                    y = datos$y,
                                    n_poblacion = 50,
                                    n_generaciones = 25,
                                    n_max_predictores = 5,
                                    n_max_estricto = FALSE,
                                    n_min_predictores = 1,
                                    elitismo = 0.01,
                                    prob_mut = 0.1,
                                    metodo_seleccion = "tournament",
                                    metodo_cruce = "uniforme",
                                    modelo = "randomforest",
                                    n_tree = 50,
                                    metrica = "-mse",
                                    cv = 5,
                                    parada_temprana = TRUE,
                                    rondas_parada = 5,
                                    tolerancia_parada = 0.01,
                                    paralelizado = TRUE,
                                    verbose = FALSE
)

seleccion
plot(seleccion)
