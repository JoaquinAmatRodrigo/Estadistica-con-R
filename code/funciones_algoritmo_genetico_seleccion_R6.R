################################################################################
#          CÓDIGO SELECCIÓN PREDICTORES CON ALGORITMO GENÉTICO                 #
#                                                                              #
# This work by Joaquín Amat Rodrigo is licensed under a Creative Commons       #
# Attribution 4.0 International License.                                       #
################################################################################
# coding=utf-8

################################################################################
#                              CLASE INDIVIDUO                                 #
################################################################################

library(R6)

Individuo <- R6Class("Individuo",
                     
  # Esta clase representa un individuo con una determinada selección de
  # predictores.
  # 
  # Parameters
  # ----------
  # n_variables: numeric
  #     longitud del vector que define al individuo. Debe coincidir con el
  #     número total de predictores disponibles.
  # 
  # n_max: numeric, optional
  #     número máximo de predictores que puede contener inicialmente un individuo.
  #     Si no se indica un valor (default NA) se emplea `n_variables`.
  # 
  # n_min: numeric, optional
  #     número mínimo de predictores que puede contener un individuo. (default 1)
  # 
  # n_max_estricto: logical, optional
  #     forzar a que el individuo no pueda contener más de `n_max` predictores.
  #     (default ``FALSE``) Ver notas para más info.
  # 
  # verbose: logical, optional
  #     mostrar información del proceso por pantalla. (default ``FALSE``)
  # 
  # Attributes
  # ----------
  # n_variables: numeric
  #     longitud del vector que define al individuo. Debe coincidir con el
  #     número total de predictores disponibles.
  # 
  # n_max: numeric
  #     número máximo de predictores que puede contener inicialmente un individuo.
  #     Si no se indica un valor (default NA) se emplea `n_variables`.
  # 
  # n_max_estricto: logical, optional
  #     forzar a que el individuo no pueda contener más de `n_max` predictores.
  #     (default ``FALSE``) Ver notas para más info.
  # 
  # n_min: numeric
  #     número mínimo de predictores que puede contener un individuo. (default 1)
  # 
  # secuencia: logical
  #     vector de ``TRUE`` y ``FALSE`` que define las columnas que incluye y
  #     excluye como predictores el individuo.
  # 
  # predictores: vector
  #     vector con el índice de las columnas empleadas como predictores.
  # 
  # fitness: numeric
  #     valor de fitness del individuo.
  # 
  # metrica: {-mse,-mae, kappa, f1, accuracy}
  #     métrica de evaluación con la que se calcula el fitness.
  # 
  # valor_metrica: numeric
  #     valor de la métrica empleada para calcular el fitness.
  # 
  # n_predictores_incluidos: numeric
  #     número de predictores que incluye el individuo.
  # 
  # modelo: {lm, glm, randomforest}
  #     modelo empleado para evaluar al individuo.
  # 
  # tipo_modelo: {regresión, clasificación}
  #     tipo de modelo.
  # 
  # Notes
  # -----
  # 
  # El argumento `n_max` establece el número máximo de predictores que puede
  # incluir el individuo en el momento de su creación. Sin emabrgo, como resultado
  # de cruces y mutaciones, los individuos creados en posteriores generaciones
  # pueden exceder este valor. Con `n_max_estricto` = ``TRUE`` se fuerza a que
  # el número de predictores incluidos nunca supere `n_max`. Para lograrlo,
  # tras el proceso de cruce y mutación, si el número de ``TRUE`` en la secuencia
  # del individuo supera `n_max`, se cambian a ``FALSE`` tantas posiciones
  # (seleccionadas aleatoriamente) hasta que cumplan la condición de `n_max`.
  # 
  # Examples
  # --------
  # Ejemplo creación individuo.
  # 
  # individuo = Individuo$new(
  #               n_variables = 13,
  #               n_max       = 3,
  #               n_min       = 1,
  #               n_max_estricto = FALSE,
  #               verbose     = TRUE
  #             )
  
  public = list(
   # Número de variables del individuo
   n_variables = NA,
   # Número máximo de predictores incluidos
   n_max = NA,
   n_max_estricto = NA,
   # Número mínimo de predictores incluidos
   n_min = NA,
   # Secuencia del individuo
   secuencia = NA,
   # Índices de las columnas empleadas como predictores
   predictores = NA,
   # Número predictores incluidos
   n_predictores_incluidos = NA,
   # Fitness del individuo
   fitness = NA,
   # Métrica
   metrica = NA,
   # Valor de la métrica con la que se calcula el fitness
   valor_metrica = NA,
   # Modelo empleado para evaluar el individuo
   modelo = NA,
   # Tipo de modelo
   tipo_modelo = NA,
   
   
   initialize = function(n_variables,
                         n_max = NULL,
                         n_min = 1,
                         n_max_estricto = FALSE,
                         verbose        = FALSE) {
     
     self$n_variables    <- n_variables
     self$n_max          <- n_max
     self$n_max_estricto <- n_max_estricto
     self$n_min          <- n_min
     
     # COMPROBACIONES INICIALES
     # ------------------------------------------------------------------------
     if (!is.null(self$n_max) && n_max > self$n_variables) {
       stop(
         "El valor de n_max no puede ser superior al de n_variables."
       )
     }
     
     # Si no se especifica n_max, se emplea por defecto el valor de n_variables.
     if (is.null(self$n_max)) {
       self$n_max <- self$n_variables
     }
     
     # CREACIÓN DE LA SECUENCIA BOLEANA QUE DEFINE AL INDIVIDUO
     # ------------------------------------------------------------------------
     # Se crea un vector boleano que representa el individuo.
     self$secuencia <- rep(FALSE, times = self$n_variables)
     
     # Se selecciona (con igual probabilidad) el número de valores TRUE que
     # puede tener el individuo, dentro del rango acotado por n_min y n_max.
     n_true <-  sample(x = self$n_min:self$n_max, size = 1)
     
     # Se sustituyen n_true posiciones aleatorias por valores TRUE.
     posiciones_true <- sample(1:self$n_variables, size = n_true, replace = FALSE)
     self$secuencia[posiciones_true] <- TRUE
     self$n_predictores_incluidos <- sum(self$secuencia)
     
     # Se almacenan los indices de las posiciones TRUE
     self$predictores <- posiciones_true
     
     # INFORMACIÓN DEL PROCESO (VERBOSE)
     # ------------------------------------------------------------------------
     if (verbose) {
       cat("----------------------", "\n")
       cat("Nuevo individuo creado", "\n")
       cat("----------------------", "\n")
       cat("Secuencia:", self$secuencia, "\n")
       cat("Índice predictores:", self$predictores, "\n")
       cat("Número de predictores incluidos:", self$n_predictores_incluidos, "\n")
       cat("Fitness:", self$fitness, "\n")
       cat("Métrica:", self$metrica, "\n")
       cat("Valor métrica:", self$valor_metrica, "\n")
       cat("Modelo empleado para calcular fitness:", self$modelo, "\n")
       cat("Tipo de modelo:", self$tipo_modelo, "\n")
       cat("\n")
     }
   },
   
   print = function(...) {
     
     # Información que se muestra cuando se imprime un objeto Individuo.
     
     cat("Individuo", "\n")
     cat("---------", "\n")
     cat("Secuencia:", self$secuencia, "\n")
     cat("Índice predictores:", self$predictores, "\n")
     cat("Número de predictores incluidos:", self$n_predictores_incluidos, "\n")
     cat("Fitness:", self$fitness, "\n")
     cat("Métrica:", self$metrica, "\n")
     cat("Valor métrica:", self$valor_metrica, "\n")
     cat("Modelo empleado para calcular fitness:", self$modelo, "\n")
     cat("Tipo de modelo:", self$tipo_modelo, "\n")
     cat("\n")
     
     invisible(self)
   },
   
   evaluar_individuo = function(modelo,
                                x,
                                y,
                                cv,
                                metrica = NULL,
                                nivel_referencia = NULL,
                                n_tree   = 50,
                                seed    = 123,
                                verbose = TRUE,
                                ...){
     
     # Este método calcula el fitness del individuo a partir del valor de la
     # métrica obtenida por validación cruzada al ajustar un modelo empleando
     # los predictores que indica su secuencia. Se trata de un método wrapper
     # de los métodos privados: evaluar_individuo_glm, evaluar_individuo_lm y
     # evaluar_individuo_rf.
     
     # Parameters
     # ----------
     # modelo: {lm, glm, randomforest}
     #   modelo empleado para calcular el fitness del individuo.
     # 
     # x: matrix, data.frame, tibble
     #   matriz de predictores.
     #   
     # y: vector
     #   variable respuesta.
     #   
     # cv: numeric
     #   número de particiones de validación cruzada.
     #   
     # seed: numeric
     #   semilla para garantizar reproducibilidad en el proceso de CV.
     #   
     # metrica: (-mse, -mae, f1, kappa, accuracy)
     #   métrica utilizada para calcular el fitness. Por defecto es el -mse para
     #   regresión y accuracy para clasificación. En el caso de clasificación
     #   binaria, se acepta también f1 y kappa.
     #   
     # nivel_referencia: character
     #   valor de la variable respuesta considerado como referencia. Necesario
     #   cuando la métrica es f1 o kappa.
     #   
     # n_tree: numeric
     #   número de árboles del modelo randomforest.       
     #                   
     # verbose: logical
     #   mostrar información del proceso por pantalla.
     
     if (modelo == "lm") {
       private$evaluar_individuo_lm(
         x  = x,
         y  = y,
         cv = cv,
         metrica = metrica,
         seed    = seed,
         verbose = verbose
       )
     }else if (modelo == "glm") {
       private$evaluar_individuo_glm(
         x  = x,
         y  = y,
         cv = cv,
         metrica = metrica,
         seed    = seed,
         nivel_referencia = nivel_referencia,
         verbose = verbose
       )
     }else {
       private$evaluar_individuo_rf(
         x  = x,
         y  = y,
         cv = cv,
         metrica = metrica,
         seed    = seed,
         n_tree  = n_tree,
         nivel_referencia = nivel_referencia,
         verbose = verbose
       )
     }
   },
   
   mutar = function(prob_mut = 0.01,
                    verbose  = TRUE) {
     
     # Este método somete al individuo a un proceso de mutación en el que, cada
     # una de sus posiciones, puede verse modificada con una probabilidad `prob_mut`.
     # 
     # Parameters
     # ----------
     # individuo: Individuo
     #   vector que representa a un individuo. 
     #   
     # prob_mut: numeric
     #   probabilidad que tiene cada posición del vector de mutar. (default 0.01)
     #   
     # Examples
     # --------
     # 
     # individuo = Individuo$new(
     #           n_variables = 13,
     #           n_max       = 5,
     #           n_min       = 1,
     #           n_max_estricto = FALSE,
     #           verbose     = TRUE
     #         )
     # 
     # individuo$mutar(
     #     prob_mut = 0.5,
     #     verbose  = TRUE
     # )
     
     # COMPROBACIONES INICIALES
     # --------------------------------------------------------------------------
     if (prob_mut < 0 || prob_mut > 1) {
       stop("El argumento prob_mut debe de estar en el rango [0,1].")
     }
     
     # SELECCIÓN PROBABILISTA DE POSICIONES (VARIABLES) QUE MUTAN
     #-----------------------------------------------------------------------
     posiciones_mutadas <- runif(n = self$n_variables, min = 0, max = 1)
     posiciones_mutadas <- posiciones_mutadas < prob_mut
     
     # Se modifica el valor de aquellas posiciones que hayan sido seleccionadas para
     # mutar. Si el valor de prob_mut es muy bajo, las mutaciones serán muy poco
     # frecuentes y el individuo devuelto será casi siempre igual al original.
     self$secuencia[posiciones_mutadas] <- !(self$secuencia[posiciones_mutadas])
     
     # Todo individuo debe tener como mínimo 1 predictor, si como consecuencia de
     # la mutación, ningun valor de la secuencia es TRUE, se selecciona una posición
     # aleatoria y se sobreescribe con TRUE.
     if (sum(self$secuencia == TRUE) == 0) {
       indice <- sample(x = 1:self$n_variables, size = 1)
       self$secuencia[indice] <- TRUE
     }
     # Se actualiza el índice de los predictores incluidos.
     self$predictores <- which(self$secuencia == TRUE)
     # Se actualiza el número total de predictores incluidos.
     self$n_predictores_incluidos <- sum(self$secuencia)
     
     # INFORMACIÓN DEL PROCESO (VERBOSE)
     # --------------------------------------------------------------------------
     if (verbose) {
       cat("El individuo ha sido mutado", "\n")
       cat("---------------------------", "\n")
       cat("Total mutaciones:", length(posiciones_mutadas), "\n")
       cat("Secuencia:", self$secuencia, "\n")
       cat("Índice predictores:", self$predictores, "\n")
       cat("\n")
     }
   },
   
   forzar_n_max = function(){
     
     # Este método modifica la secuencia del individuo de forma que, como máximo
     # contenga n_max valores TRUE.
     # 
     # Examples
     # --------
     # individuo = Individuo$new(
     #           n_variables = 13,
     #           n_max       = 3,
     #           n_min       = 1,
     #           n_max_estricto = TRUE,
     #           verbose     = FALSE
     #         )
     # 
     # individuo$mutar(
     #     prob_mut = 0.5,
     #     verbose  = FALSE
     # )
     # 
     # individuo
     # individuo$forzar_n_max()
     # individuo
     
     # Se identifica si el número de TRUE es la secuencia supera a n_max.
     n_exceso <- sum(self$secuencia) - self$n_max
     
     if (n_exceso > 0) {
       # Se seleccionan aleatoriamente n_max posiciones con valor TRUE en 
       # la secuencia del individuo.
       indices <- sample(
         x = which(self$secuencia == TRUE),
         size = n_exceso,
         replace = FALSE
       )
       self$secuencia[indices] <- !(self$secuencia[indices])
       
       # Se actualiza el indice de los predictores incluidos.
       self$predictores <- which(self$secuencia == TRUE)
       # Se actualiza el número total de predictores incluidos.
       self$n_predictores_incluidos <- sum(self$secuencia)
     }
   }
  ),
  
  private = list(
   
   evaluar_individuo_rf = function(x,
                                   y,
                                   cv,
                                   metrica = NULL,
                                   nivel_referencia = NULL,
                                   n_tree   = 50,
                                   seed    = 123,
                                   verbose = TRUE,
                                   ...) {
     
     # Este método calcula el fitness del individuo a partir del valor de la
     # métrica obtenida por validación cruzada al ajustar un modelo random forest
     # empleando los predictores que indica su secuencia.
     
     # Parameters
     # ----------
     # x: matrix, data.frame, tibble
     #   matriz de predictores.
     #   
     # y: vector
     #   variable respuesta.
     #   
     # cv: numeric
     #   número de particiones de validación cruzada.
     #   
     # seed: numeric
     #   semilla para garantizar reproducibilidad en el proceso de CV.
     #   
     # metrica: {-mse, -mae, f1, kappa, accuracy}
     #   métrica utilizada para calcular el fitness. Por defecto es el -mse para
     #   regresión y accuracy para clasificación. En el caso de clasificación
     #   binaria, se acepta también f1 y kappa.
     #   
     # nivel_referencia: character
     #   valor de la variable respuesta considerado como referencia. Necesario
     #   cuando la métrica es f1 o kappa.
     #   
     # n_tree: numeric
     #   número de árboles del modelo randomforest.       
     #                   
     # verbose: logical
     #   mostrar información del proceso por pantalla.
     #
     # Examples
     # --------
     # 
     # library(MASS)
     # data("Boston")
     # individuo = Individuo$new(
     #               n_variables = 13,
     #               n_max       = 3,
     #               n_min       = 1,
     #               n_max_estricto = FALSE,
     #               verbose     = TRUE
     #             )
     # 
     # individuo$evaluar_individuo_rf(
     #     x  = Boston[,-14],
     #     y  = Boston[,14],
     #     cv = 5,
     #     metrica = "-mse",
     #     n_tree  = 50,
     #     seed    = 123,
     #     verbose = TRUE
     # )
     # individuo
     # 
     # 
     # data("iris")
     # individuo = Individuo$new(
     #                     n_variables = 4,
     #                     n_max       = 3,
     #                     n_min       = 1,
     #                     n_max_estricto = FALSE,
     #                     verbose     = TRUE
     #                 )
     # 
     # individuo$evaluar_individuo_rf(
     #         x  = iris[, -5],
     #         y  = iris[, 5],
     #         cv = 5,
     #         metrica = "accuracy",
     #         n_tree  = 50,
     #         seed    = 123,
     #         verbose = TRUE
     #     )
     # individuo
     
     
     # LIBRERÍAS
     # ------------------------------------------------------------------------
     library(MLmetrics)
     library(caret)
     
     
     # COMPROBACIONES INICIALES
     # ------------------------------------------------------------------------
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
     
     self$modelo      <- "Random Forest"
     self$tipo_modelo <- ifelse(is.numeric(y), "regresión", "clasificación")
     self$metrica     <- metrica
     
     
     # REPARTO DE LAS OBSERVACIONES PARA LA VALIDACIÓN CRUZADA (CV)
     # ------------------------------------------------------------------------
     set.seed(seed)
     indices_cv <- caret::createFolds(y = y, k = cv, list = FALSE)
     
     # BUCLE DE ENTRENAMIENTO Y VALIDACIÓN CRUZADA (CV)
     # ------------------------------------------------------------------------
     # Vector para almacenar la métrica de cada iteración CV.
     metrica_cv <- rep(NA, times = cv)
     
     for (i in 1:cv) {
       set.seed(seed)
       modelo <- randomForest::randomForest(
         x = x[indices_cv != i, self$secuencia, drop = FALSE],
         y = y[indices_cv != i],
         ntree = n_tree
       )
       predicciones <- predict(modelo, newdata = x[indices_cv == i, , drop = FALSE])
       
       if (self$metrica == "-mse") {
         residuos <- predicciones - y[indices_cv == i]
         metrica_cv[i] <- -(mean(residuos^2))
       } else if (self$metrica == "-mae") {
         residuos <- predicciones - y[indices_cv == i]
         metrica_cv[i] <- -(mean(abs(residuos)))
       } else if (self$metrica == "accuracy") {
         accuracy <- MLmetrics::Accuracy(
           y_pred = predicciones,
           y_true = y[indices_cv == i]
         )
         metrica_cv[i] <- accuracy
       } else if (self$metrica == "kappa") {
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
     
     self$valor_metrica <- mean(metrica_cv, na.rm = TRUE)
     self$fitness       <- mean(metrica_cv, na.rm = TRUE)
     
     # INFORMACIÓN DEL PROCESO (VERBOSE)
     # ------------------------------------------------------------------------
     if (verbose) {
       cat("El individuo ha sido evaluado", "\n")
       cat("-----------------------------", "\n")
       cat("Métrica:", self$metrica, "\n")
       cat("Valor métrica:", self$valor_metrica, "\n")
       cat("Fitness:", self$fitness, "\n")
       cat("Modelo de evaluación:", self$modelo, "\n")
       cat("\n")
     }
   },
   
   evaluar_individuo_lm = function(x,
                                   y,
                                   cv,
                                   metrica = NULL,
                                   seed    = 123,
                                   verbose = TRUE,
                                   ...) {
     
     # Este método calcula el fitness del individuo a partir del valor de la
     # métrica obtenida por validación cruzada al ajustar un modelo lineal
     # empleando los predictores que indica su secuencia.
     
     # Parameters
     # ----------
     # x: matrix, data.frame, tibble
     #   matriz de predictores.
     #   
     # y: vector
     #   variable respuesta.
     #   
     # cv: numeric
     #   número de particiones de validación cruzada.
     #   
     # metrica: {-mse, -mae}
     #   métrica utilizada para calcular el fitness. Por defecto se emplea
     #   -mse.
     #   
     # seed: numeric
     #   semilla para garantizar reproducibilidad en el proceso de CV.
     #   
     # verbose: logical
     #   mostrar información del proceso por pantalla.
     #
     # Examples
     # --------
     # 
     # library(MASS)
     # data("Boston")
     # individuo = Individuo$new(
     #               n_variables = 13,
     #               n_max       = 5,
     #               n_min       = 1,
     #               n_max_estricto = FALSE,
     #               verbose     = TRUE
     #             )
     # 
     # individuo$evaluar_individuo_lm(
     #     x  = Boston[,-14],
     #     y  = Boston[,14],
     #     cv = 5,
     #     metrica = "-mse",
     #     seed    = 123,
     #     verbose = TRUE
     # )
     # individuo
     
     
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
     
     self$modelo      <- "Regresión lineal (lm)"
     self$tipo_modelo <- "regresión"
     self$metrica     <- metrica
     
     
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
           x[indices_cv != i, self$secuencia, drop = FALSE],
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
       if (self$metrica == "-mse") {
         metrica_cv[i] <- -(mean(residuos^2))
       }
       if (self$metrica == "-mae") {
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
     
     self$valor_metrica <- mean(metrica_cv, na.rm = TRUE)
     self$fitness       <- mean(metrica_cv, na.rm = TRUE)
     
     # INFORMACIÓN DEL PROCESO (VERBOSE)
     # ------------------------------------------------------------------------
     if (verbose) {
       cat("El individuo ha sido evaluado", "\n")
       cat("-----------------------------", "\n")
       cat("Métrica:", self$metrica, "\n")
       cat("Valor métrica:", self$valor_metrica, "\n")
       cat("Fitness:", self$fitness, "\n")
       cat("Modelo de evaluación:", self$modelo, "\n")
       cat("\n")
     }
   }, 
   
   evaluar_individuo_glm = function(x,
                                    y,
                                    cv,
                                    metrica = NULL,
                                    nivel_referencia = NULL,
                                    seed    = 123,
                                    verbose = TRUE,
                                    ...) {
     
     # Este método calcula el fitness del individuo a partir del valor de la
     # métrica obtenida por validación cruzada al ajustar un modelo de regresión
     # logística empleando los predictores que indica su secuencia.
     
     # Parameters
     # ----------
     # x: matrix, data.frame, tibble
     #   matriz de predictores.
     #   
     # y: vector
     #   variable respuesta.
     #   
     # cv: numeric
     #   número de particiones de validación cruzada.
     #   
     # seed: numeric
     #   semilla para garantizar reproducibilidad en el proceso de CV.
     #   
     # metrica: {f1, kappa, accuracy}
     #   métrica utilizada para calcular el fitness. Por defecto es accuracy.
     #   
     # nivel_referencia: character
     #   valor de la variable respuesta considerado como referencia. Necesario
     #   cuando la métrica es f1 o kappa.
     #                   
     # verbose: logical
     #   mostrar información del proceso por pantalla.
     #
     # Examples
     # --------
     # 
     # data("iris")
     # iris <- iris[iris$Species != "setosa", ]
     # iris$Species <- as.character(iris$Species)
     # individuo = Individuo$new(
     #                     n_variables = 4,
     #                     n_max       = 3,
     #                     n_min       = 1,
     #                     n_max_estricto = FALSE,
     #                     verbose     = TRUE
     #                 )
     # 
     # individuo$evaluar_individuo_glm(
     #         x  = iris[, -5],
     #         y  = iris[, 5],
     #         cv = 5,
     #         metrica = "f1",
     #         nivel_referencia = "versicolor",
     #         verbose = TRUE
     #     )
     # individuo
     
     
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
     
     self$modelo       <- "Regresión Logística (glm)"
     self$tipo_modelo  <-  "clasificación"
     self$metrica      <-  metrica
     
     
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
           x[indices_cv != i, self$secuencia, drop = FALSE],
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
       
       if (self$metrica == "accuracy") {
         accuracy <- MLmetrics::Accuracy(
           y_pred = predicciones,
           y_true = y[indices_cv == i]
         )
         metrica_cv[i] <- accuracy
       } else if (self$metrica == "kappa") {
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
     
     self$valor_metrica <- mean(metrica_cv, na.rm = TRUE)
     self$fitness       <- mean(metrica_cv, na.rm = TRUE)
     
     # INFORMACIÓN DEL PROCESO (VERBOSE)
     # ------------------------------------------------------------------------
     if (verbose) {
       cat("El individuo ha sido evaluado", "\n")
       cat("-----------------------------", "\n")
       cat("Métrica:", self$metrica, "\n")
       cat("Valor métrica:", self$valor_metrica, "\n")
       cat("Fitness:", self$fitness, "\n")
       cat("Modelo de evaluación:", self$modelo, "\n")
       cat("\n")
     }
   }
  )
)


################################################################################
#                              CLASE POBLACIÓN                                 #
################################################################################

library(R6)

Poblacion <- R6Class("Poblacion",
                     
  # Esta clase crea una población de n individuos.
  # 
  # Parameters
  # ----------
  # n_individuos: numeric
  #     número de individuos de la población.
  # 
  # n_variables: numeric
  #     longitud del vector que define a los individuos. Debe coincidir con el
  #     número total de predictores disponibles.
  # 
  # n_max: numeric, optional
  #     número máximo de predictores que pueden contener inicialmente los
  #     individuos. Si no se indica un valor (default NA) se emplea
  #     `n_variables`.
  # 
  # n_min: numeric, optional
  #     número mínimo de predictores que pueden contener los individuos.
  #     (default 1)
  # 
  # n_max_estricto: logical, optional
  #     forzar a que ningún individuo a lo largo del proceso pueda contener más
  #     de `n_max` predictores. (default ``FALSE``) Ver notas para más info.
  # 
  # verbose: logical, optional
  #     mostrar información del proceso por pantalla. (default ``FALSE``)
  # 
  # Attributes
  # ----------
  # individuos: list
  #     lista con todos los individuos de la población en su estado actual.
  # 
  # n_individuos: numeric
  #     número de individuos de la población.
  # 
  # n_variables: numeric
  #     longitud del vector que define a los individuos. Debe coincidir con el
  #     número total de predictores disponibles.
  # 
  # n_max: numeric, optional
  #     número máximo de predictores que pueden contener inicialmente los
  #     individuos. Si no se indica un valor (default NA) se emplea
  #     `n_variables`.
  # 
  # n_min: numeric, optional
  #     número mínimo de predictores que pueden contener los individuos.
  #     (default 1)
  # 
  # n_max_estricto: logical, optional
  #     forzar a que ningún individuo a lo largo del proceso pueda contener más
  #     de `n_max` predictores. (default ``FALSE``) Ver notas para más info.
  # 
  # metrica: {-mse,-mae, kappa, f1, accuracy}
  #     métrica empleada para calcular el fitness del individuo.
  # 
  # modelo: {lm, glm, randomforest}
  #         modelo empleado para evaluar al individuo.
  # 
  # mejor_individuo: object individuo
  #     mejor individuo de la población en su estado actual.
  # 
  # mejor_fitness: numeric
  #     fitness del mejor individuo de la población en su estado actual.
  #     
  # mejor_valor_metrica: numeric
  #     valor de la métrica del mejor individuo de la población en su
  #     estado actual.
  # 
  # mejor_secuencia: vector
  #     secuencia del mejor individuo de la población en su estado actual.
  # 
  # mejor_predictores: vector
  #     índice de las columnas que incluye como predictores el mejor individuo
  #     de la población
  # 
  # historico_individuos: list
  #     lista con la información de todos los individuos en cada una de las 
  #     generaciones que ha tenido la población.
  # 
  # historico_mejor_secuencia: list
  #     lista con valor de las secuencias del mejor individuo en cada una de las 
  #     generaciones que ha tenido la población.
  # 
  # historico_mejor_predictores: list
  #     lista con los índice de las columnas que incluye como predictores el 
  #     mejor individuo en cada una de las generaciones que ha tenido la
  #     población.
  # 
  # historico_mejor_fitness: vector
  #     vector con el mejor fitness en cada una de las generaciones que ha tenido
  #     la población.
  # 
  # historico_mejor_valor_metrica: vector
  #     vector con valor de la métrica del mejor individuo en cada una
  #     de las generaciones que ha tenido la población.
  # 
  # diferencia_abs: vector
  #     diferencia absoluta entre el mejor fitness de generaciones consecutivas.
  # 
  # resultados_df: dataframe
  #     dataframe con la información del mejor fitness y secuencia encontrada
  #     en cada generación, así como la diferencia respecto a la generación
  #     anterior.
  # 
  # fitness_optimo: numeric
  #     mejor fitness encontrado tras el proceso de optimización.
  # 
  # valor_metrica_optimo: numeric
  #     mejor valor de la métrica encontrado tras el proceso de optimización.
  # 
  # secuencia_optima: vector
  #     secuencia con la que se ha conseguido el mejor fitness tras el proceso
  #     de optimización.
  # 
  # predictores_optimos: vector
  #     índice de las columnas que incluye como predictores el individuo con
  #     mejor fitness tras el proceso de optimización.
  # 
  # evaluada: logical
  #     si la población ha sido evaluada.
  # 
  # optimizada: logical
  #     si la población ha sido optimizada.
  # 
  # iter_optimizacion: numeric
  #     número de iteraciones de optimización (generaciones).
  #     
  # Examples
  # --------
  # 
  # Ejemplo crear población
  # 
  # poblacion = Poblacion(
  #                 n_individuos = 5,
  #                 n_variables  = 10,
  #                 n_max        = 5,
  #                 n_min        = 1,
  #                 verbose      = TRUE
  #             )
  
  public = list(
   # Número de individuos de la población
   n_individuos = NA,
   # Número de variables de cada individuo
   n_variables = NA,
   # Número máximo de predictores incluidos
   n_max = NA,
   n_max_estricto = NA,
   # Número mínimo de predictores incluidos
   n_min = NA,
   # Métrica utilizada en la evaluación de los individuos
   metrica = NA,
   # Modelo empleado para evaluar el individuo
   modelo = NA,
   # Tipo de odelo empleado para evaluar el individuo
   tipo_modelo = NA,
   # Lista de los individuos de la población
   individuos = list(),
   # Etiqueta para saber si la población ha sido evaluada
   evaluada = FALSE,
   # Etiqueta para saber si la población ha sido optimizada
   optimizada = FALSE,
   # Número de iteraciones de optimización llevadas a cabo
   iter_optimizacion = NA,
   # Mejor individuo de la población
   mejor_individuo = NA,
   # Fitness del mejor individuo de la población (el de mayor fitness)
   mejor_fitness = NA,
   # Valor de la métrica del mejor individuo de la población
   mejor_valor_metrica = NA,
   # Secuencia del mejor individuo de la población
   mejor_secuencia = NA,
   # Índice de las columnas que incluye como predictores el mejor individuo
   # de la población
   mejor_predictores = NA,
   # Nombre de las columnas que incluye como predictores el mejor individuo
   # de la población
   nombre_mejor_predictores = NA,
   # Información de todas los individuos de la población en cada generación
   historico_individuos = list(),
   # Secuencia del mejor individuo en cada generación
   historico_mejor_secuencia = list(),
   # Índice de las columnas que incluye como predictores el mejor individuo
   # en cada generación.
   historico_mejor_predictores = list(),
   # Fitness del mejor individuo en cada generación
   historico_mejor_fitness = vector(mode = "numeric"),
   # Valor de la métrica del mejor individuo en cada generación
   historico_mejor_valor_metrica = vector(mode = "numeric"),
   # Diferencia absoluta entre el mejor fitness de generaciones consecutivas
   diferencia_abs = vector(mode = "numeric"),
   # data.frame con la información del mejor fitness y valor de variables
   # encontrado en cada generación, así como la diferencia respecto a la 
   # generación anterior.
   resultados_df = NA,
   # Fitness del mejor individuo de todas las generaciones
   fitness_optimo = NA,
   # Secuencia del mejor individuo de todas las generaciones
   secuencia_optima = NA,
   # Índice de las columnas incluidas como predictores en el mejor individuo
   # de todas las generaciones.
   predictores_optimos = NA,
   # Nombre de las columnas incluidas como predictores en el mejor individuo
   # de todas las generaciones.
   nombre_predictores_optimos = NA,
   # Valor de función objetivo del mejor individuo de todas las generaciones
   valor_metrica_optimo = NA,
   
   initialize = function(n_individuos,
                         n_variables,
                         n_max = NULL,
                         n_min = 1,
                         n_max_estricto = FALSE,
                         verbose        = FALSE) {
     
     self$n_individuos   <- n_individuos
     self$n_variables    <- n_variables
     self$n_max          <- n_max
     self$n_max_estricto <- n_max_estricto
     self$n_min          <- n_min
     
     # COMPROBACIONES INICIALES
     # ------------------------------------------------------------------------
     if (!is.null(self$n_max) && n_max > self$n_variables) {
       stop(
         "El valor de n_max no puede ser superior al de n_variables."
       )
     }
     # Si no se especifica n_max, se emplea por defecto el valor de n_variables.
     if (is.null(self$n_max)) {
       self$n_max <- self$n_variables
     }
     
     # SE CREAN LOS INDIVIDUOS DE LA POBLACIÓN Y SE ALMACENAN
     # ------------------------------------------------------------------------
     for (i in 1:n_individuos) {
       self$individuos[[i]] <- Individuo$new(
         n_variables = self$n_variables,
         n_max   = self$n_max,
         n_min   = self$n_min,
         verbose = verbose
       )
     }
     
     # INFORMACIÓN DEL PROCESO (VERBOSE)
     # ------------------------------------------------------------------------
     if (verbose) {
       cat("----------------", "\n")
       cat("Población creada", "\n")
       cat("----------------", "\n")
       cat("Número de individuos:", self$n_individuos, "\n")
       cat("Número máximo de predictores iniciales:", self$n_max, "\n")
       cat("Número mínimo de predictores iniciales:", self$n_min, "\n")
       cat("\n")
     }
   },
   
   print = function(...) {
     # Información que se muestra cuando se imprime un objeto Individuo.
     cat("============================", "\n")
     cat("         Población", "\n")
     cat("============================", "\n")
     cat("Número de individuos:", self$n_individuos, "\n")
     cat("Número máximo de predictores iniciales:", self$n_max, "\n")
     cat("Número mínimo de predictores iniciales:", self$n_min, "\n")
     cat("Evaluada:", self$evaluada, "\n")
     cat("Optimizada:", self$optimizada, "\n")
     cat("Métrica de evaluación:", self$metrica, "\n")
     cat("Modelo:", self$modelo, "\n")
     cat("Iteraciones optimización (generaciones):", self$iter_optimizacion,"\n")
     cat("\n")
     cat("Información del mejor individuo:", "\n")
     cat("--------------------------------", "\n")
     cat("Secuencia:", self$mejor_secuencia, "\n")
     cat("Índice predictores:", self$mejor_predictores, "\n")
     cat("Fitness:", self$mejor_fitness, "\n")
     cat("\n")
     cat("Resultados tras optimizar:", "\n")
     cat("--------------------------", "\n")
     cat("Secuencia óptima:", self$secuencia_optima, "\n")
     cat("Índice predictores óptimos:", self$predictores_optimos, "\n")
     cat("Nombre predictores óptimos:", self$nombre_mejor_predictores,"\n")
     cat("Valor óptimo métrica:", self$valor_metrica_optimo, "\n")
     cat("Fitness óptimo:", self$fitness_optimo, "\n")
     
     invisible(self)
   },
   
   mostrar_individuos = function(n = NULL) {
     # Este método muestra la información de cada uno de los n primeros 
     # individuos de la población.
     # 
     # Parameters
     # ----------
     # n: numeric
     #     número de individuos que se muestran. Si no se indica el valor
     #     (por defecto NA), se muestran todos. Si el valor es mayor
     #     que `self$n_individuos` se muestran todos.
     # 
     # Examples
     # --------
     # poblacion = Poblacion(
     #                 n_individuos = 5,
     #                 n_variables  = 10,
     #                 n_max        = 5,
     #                 n_min        = 1,
     #                 verbose      = FALSE
     #               )
     # poblacion$mostrar_individuos(n = 5)
     
     if (is.null(n)) {
       n <- self$n_individuos
     }else if (n > self$n_individuos) {
       n <- self$n_individuos
     }
     
     for (i in 1:n) {
       print(self$individuos[[i]])
     }
   },
   
   evaluar_poblacion = function(x,
                                y,
                                modelo,
                                metrica = NULL,
                                cv      = 5,
                                seed    = 123,
                                forzar_evaluacion = TRUE,
                                n_tree            = 50,
                                nivel_referencia  = NULL,
                                verbose           = FALSE,
                                ...) {
     
     # Este método calcula el fitness de todos los individuos de la población,
     # actualiza sus valores e identifica el mejor.
     # 
     # Parameters
     # ----------
     # x: matrix, data.frame, tobble
     #     matriz con el valor de los predictores.
     # 
     # y: vector
     #     vector con la variable respuesta
     # 
     # cv: numeric
     #     número de repeticiones para la validación cruzada. (default 5)
     # 
     # seed: numeric
     #     semilla empleada para el reparto aleatorio. (default 123)
     # 
     # modelo: {lm, glm, randomforest}
     #     modelo empleado para evaluar al individuo.
     # 
     # metrica: {-mse, -mae, acurracy, f1, kappa}
     #     métrica empleada para calcular el fitness del individuo.
     # 
     # forzar_evaluacion = logical
     #     si es ``FALSE`` los individuos que ya hayan sido evaluados
     #     anteriormente no se evaluan de nuevo. (default ``FALSE``).
     # 
     # nivel_referencia: character
     #     valor de la variable respuesta considerado como referencia.
     #     Necesario cuando la métrica es f1 o kappa.
     #     
     # n_tree: numeric
     #     número de árboles en los modelos random forest. (default 50)
     # 
     # verbose: logical, optional
     #     mostrar información del proceso por pantalla. (default ``FALSE``).
     # 
     # 
     # Examples
     # --------
     # library(MASS)
     # data("Boston")
     # poblacion = Poblacion$new(
     #                  n_individuos = 5,
     #                  n_variables = 13,
     #                  n_max       = 3,
     #                  n_min       = 1,
     #                  n_max_estricto = FALSE,
     #                  verbose     = FALSE
     #               )
     # 
     # poblacion$evaluar_poblacion(
     #      x  = Boston[,-14],
     #      y  = Boston[,14],
     #      modelo = "lm", 
     #      cv = 5,
     #      metrica = "-mse",
     #      seed = 123,
     #      forzar_evaluacion = TRUE,
     #      n_tree  = 50,
     #      verbose = TRUE
     # )
     # poblacion
     
     # COMPROBACIONES INICIALES
     # ------------------------------------------------------------------------
     if (!modelo %in% c("lm", "glm", "randomforest")) {
       stop(
         paste("El modelo empleado para calcular el fitness debe ser",
               "lm (regresion lineal)",
               "glm (regresión logistica)",
               "o randomforest."
         )
       )
     }
     self$modelo = modelo
     
     if (self$n_variables != ncol(x)) {
       stop(
         "n_variables debe ser igual al número de columnas de x."
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
       
       self$tipo_modelo <- "regresión"
       self$metrica     <- metrica
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
       
       self$tipo_modelo <- "clasificación"
       self$metrica     <- metrica
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
       
       self$tipo_modelo <- ifelse(is.numeric(y), "regresión", "clasificación")
       self$metrica     <- metrica
     }
     
     # SE EVALÚA CADA INDIVIDUO DE LA POBLACIÓN
     # ------------------------------------------------------------------------
     for (i in 1:self$n_individuos) {
       if (forzar_evaluacion) {
         #Se evaluan todos los individuos
         self$individuos[[i]]$evaluar_individuo(
           modelo  = modelo,
           x       = x,
           y       = y,
           cv      = cv,
           seed    = seed,
           metrica = metrica,
           nivel_referencia = nivel_referencia,
           n_tree  = n_tree,
           verbose = verbose
         )
       }else {
         if (is.na(self$individuos[[i]]$fitness)) {
           # Solo los no previamente evaluados se evaluan
           self$individuos[[i]]$evaluar_individuo(
             modelo  = modelo,
             x       = x,
             y       = y,
             cv      = cv,
             seed    = seed,
             metrica = metrica,
             nivel_referencia = nivel_referencia,
             n_tree  = n_tree,
             verbose = verbose
           )
         }
       }
     }
     
     # MEJOR INDIVIDUO DE LA POBLACIÓN
     # ------------------------------------------------------------------------
     # Se identifica el mejor individuo de toda la población, el de mayor
     # fitness.
     
     # Se selecciona inicialmente como mejor individuo el primero.
     self$mejor_individuo <- self$individuos[[1]]$clone(deep = TRUE)
     # Se comparan todas los individuos de la población.
     for (i in 1:self$n_individuos) {
       if (self$individuos[[i]]$fitness > self$mejor_individuo$fitness) {
         self$mejor_individuo <- self$individuos[[i]]$clone(deep = TRUE)
       }
     }
     
     # Se extrae la información del mejor individuo de la población.
     self$mejor_fitness            <- self$mejor_individuo$fitness
     self$mejor_valor_metrica      <- self$mejor_individuo$valor_metrica
     self$mejor_secuencia          <- self$mejor_individuo$secuencia
     self$mejor_predictores        <- self$mejor_individuo$predictores
     self$nombre_mejor_predictores <- colnames(x)[self$mejor_predictores]
     
     self$evaluada <- TRUE
     
     # INFORMACIÓN DEL PROCESO (VERBOSE)
     # ------------------------------------------------------------------------
     if (verbose) {
       cat("-----------------", "\n")
       cat("Población evaluado", "\n")
       cat("-----------------", "\n")
       cat("Mejor fitness encontrado:", self$mejor_fitness, "\n")
       cat("Mejor valor de la métrica:", self$mejor_valor_metrica, "\n")
       cat("Mejor secuencia encontrada:", self$mejor_secuencia, "\n")
       cat("Mejores predictores encontrados:", self$mejor_predictores, "\n")
       cat("Nombre mejores predictores encontrados:", self$nombre_mejor_predictores,"\n")
       cat("\n")
     }
   },
   
   cruzar_individuos = function(
     parental_1,
     parental_2,
     metodo_cruce = "uniforme",
     verbose      = FALSE) {
     
     # Este método genera un nuevo individuo a partir de dos individuos
     # parentales empleando el método de cruzamiento uniforme.
     # 
     # Parameters
     # ----------
     # parental_1: numeric
     #     índice del individuo de la población que se quiere emplear como
     #     parental 1 para el cruzamiento.
     # 
     # parental_2: numeric
     #     índice del individuo de la población que se quiere emplear como
     #     parental 2 para el cruzamiento.
     #     
     # metodo_cruce: {"uniforme", "punto_simple"}
     #     método de cruamiento empleado.
     # 
     # verbose: logical, optional
     #     mostrar información del proceso por pantalla. (default ``FALSE``)
     # 
     # Raises
     # ------
     # raise Exception
     #     si los índices parental_1 o parental_2 no son índices válidos.
     # 
     # Returns
     # ------
     # descendencia: `Individuo`
     #     Nuevo individuo generado por cruzamiento de dos parentales.
     # 
     # Examples
     # --------
     # poblacion = Poblacion$new(
     #               n_individuos = 2,
     #               n_variables  = 5,
     #               n_max        = 5,
     #               n_min        = 1,
     #               verbose      = TRUE
     #             )
     # 
     # descendencia = poblacion$cruzar_individuos(
     #                     parental_1 = 1,
     #                     parental_2 = 2,
     #                     metodo_cruce = "punto_simple",
     #                     verbose = TRUE
     #                 )
     # Notes
     # -----
     # El objetivo del cruzamiento es generar, a partir de individuos ya
     # existentes (parentales), nuevos individuos (descendencia) que combinen
     # las características de los anteriores. Este es otro de los puntos del
     # algoritmo en los que se puede seguir varias estrategias. Tres de las más
     # empleadas son:
     # Cruzamiento a partir de uno solo punto: se selecciona aleatoriamente
     # una posición que actúa como punto de corte. Cada individuo parental se
     # divide en dos partes y se intercambian las mitades. Como resultado de
     # este proceso, por cada cruce, se generan dos nuevos individuos.
     # 
     # Cruzamiento a partir múltiples puntos: se seleccionan aleatoriamente
     # varias posiciones que actúan como puntos de corte. Cada individuo
     # parental se divide por los puntos de corte y se intercambian las partes.
     # Como resultado de este proceso, por cada cruce, se generan dos nuevos
     # individuos.
     # 
     # Cruzamiento uniforme: el valor que toma cada posición del nuevo
     # individuo se obtiene de uno de los dos parentales. Por lo general,
     # la probabilidad de que el valor proceda de cada parental es la misma,
     # aunque podría, por ejemplo, estar condicionada al fitness de cada uno.
     # A diferencia de las anteriores estrategias, con esta, de cada cruce se
     # genera un único descendiente.     
     
     # COMPROBACIONES INICIALES
     # ----------------------------------------------------------------------
     if (!parental_1 %in% 1:self$n_individuos) {
       stop(
         paste(
           "El índice del parental_1 debe de ser un valor entre 1 y ",
           "el número de individuos de la población."
         )
       )
     }
     if (!parental_2 %in% 1:self$n_individuos) {
       stop(
         paste(
           "El índice del parental_2 debe de ser un valor entre 1 y ",
           "el número de individuos de la población."
         )
       )
     }
     if (!metodo_cruce %in% c("uniforme", "punto_simple")) {
       stop(
         "El argumento metodo_cruce debe de ser uniforme o punto_simple."
       )
     }
     
     # CREACIÓN DE LA DESCENDENCIA
     # ------------------------------------------------------------------------
     # Se extraen los parentales acorde a los índices indicados.
     parental_1 <- self$individuos[[parental_1]]
     parental_2 <- self$individuos[[parental_2]]
     
     # Se clona uno de los parentales para utilizarlo como plantilla del nuevo
     # individuo.
     descendencia               <- parental_1$clone(deep = TRUE)
     descendencia$secuencia     <- rep(NA, descendencia$n_variables)
     descendencia$predictores   <- NA
     descendencia$fitness       <- NA
     descendencia$valor_metrica <- NA
     descendencia$n_predictores_incluidos <- NA
     
     if (metodo_cruce == "uniforme") {
       # Se seleccionan aleatoriamente las posiciones que se heredan del parental_1.
       herencia_parent_1 <- sample(
         x = c(TRUE, FALSE),
         size = descendencia$n_variables,
         replace = TRUE
       )
       # El resto de posiciones se heredan del parental_2.
       herencia_parent_2 <- !herencia_parent_1
       
       # Se transfieren los valores al nuevo individuo.
       descendencia$secuencia[herencia_parent_1] <- parental_1$secuencia[herencia_parent_1]
       descendencia$secuencia[herencia_parent_2] <- parental_2$secuencia[herencia_parent_2]
     } else {
       punto_cruce <- sample(
         x    = 2:descendencia$n_variables,
         size = 1
       )
       descendencia$secuencia <- c(
         parental_1$secuencia[1:(punto_cruce - 1)],
         parental_2$secuencia[punto_cruce:descendencia$n_variables]
       )
     }
     # Todo individuo debe tener como mínimo 1 predictor, si como consecuencia del 
     # cruzamiento, ningun valor de la secuencia es TRUE, se selecciona una posición
     # aleatoria y se sobreescribe con TRUE.
     if (sum(descendencia$secuencia) == 0) {
       indice <- sample(
         x    = 1:descendencia$n_variables,
         size = 1
       )
       descendencia$secuencia[indice] <- TRUE
     }
     
     descendencia$predictores <- which(descendencia$secuencia == TRUE)
     descendencia$n_predictores_incluidos <- sum(descendencia$secuencia)
     # Se crea un deepcopy para que el nuevo individuo sea independiente de 
     # los parentales. Esto evita problemas si posteriormente se muta.
     descendencia <- descendencia$clone(deep = TRUE)
     
     # INFORMACIÓN DEL PROCESO (VERBOSE)
     # ------------------------------------------------------------------------
     if (verbose) {
       cat("---------------", "\n")
       cat("Cruce realizado", "\n")
       cat("---------------", "\n")
       cat("Secuencia:", descendencia$secuencia, "\n")
       cat("Índice predictores:", descendencia$predictores, "\n")
       cat("\n")
     }
     
     return(descendencia)
   },
   
   seleccionar_individuo = function(n,
                                    return_indices   = TRUE,
                                    metodo_seleccion = "tournament",
                                    verbose = FALSE) {
     
     # Este método selecciona los índices de n individuos de una población,
     # donde la probabilidad de selección está relacionada con el fitness de  
     # cada individuo. Si el argumento `return_indices=FALSE` en lugar de los
     # índices se devuelve una copia de los individuos seleccionados.
     # 
     # Parameters
     # ----------
     # n: numeric
     #     número de individuos de la población que se seleccionan.
     # 
     # return_indices: logical, optional
     #     cuando es ``TRUE``, se devuelve el índice que ocupan los individuos
     #     seleccionados, cuando es ``FALSE`` se devuelve una lista que contiene
     #     una copia de los individuos. (default ``TRUE``)
     # 
     # metodo_seleccion: {"ruleta", "rank", "tournament"}
     #     método de selección, ver notas para más información.
     #     (default "tournament")
     # 
     # verbose: logical, optional
     #     mostrar información del proceso por pantalla. (default ``FALSE``)
     # 
     # Returns
     # -------
     # indices: vector numérico
     #     índice de los individuos seleccionados (si `return_indices=TRUE`)
     # 
     # individuos: list
     #     lista con los individuos seleccionados (si `return_indices=FALSE`)
     # 
     # Examples
     # --------
     # 
     # library(MASS)
     # data("Boston")
     # poblacion = Poblacion$new(
     #                  n_individuos = 5,
     #                  n_variables = 13,
     #                  n_max       = 3,
     #                  n_min       = 1,
     #                  n_max_estricto = FALSE,
     #                  verbose     = FALSE
     #               )
     # 
     # poblacion$evaluar_poblacion(
     #      x  = Boston[,-14],
     #      y  = Boston[,14],
     #      modelo = "lm", 
     #      cv = 5,
     #      metrica = "-mse",
     #      seed = 123,
     #      forzar_evaluacion = TRUE,
     #      n_tree  = 50,
     #      verbose = TRUE
     # )
     # poblacion$seleccionar_individuo(
     #         n                = 2,
     #         return_indices   = TRUE,
     #         metodo_seleccion = "tournament",
     #         verbose          = TRUE
     #     )
     # 
     # Notes
     # -----
     # La forma en que se seleccionan los individuos que participan en cada cruce
     # difiere en las distintas implementaciones de los algoritmos genéticos.
     # Por lo general, todas ellas tienden a favorecer la selección de aquellos
     # individuos con mayor fitness. Algunas de las estrategias más comunes son:
     # Método de ruleta: la probabilidad de que un individuo sea seleccionado
     # es proporcional a su fitness relativo, es decir, a su fitness dividido
     # por la suma del fitness de todos los individuos de la población. Si el
     # fitness de un individuo es el doble que el de otro, también lo será la
     # probabilidad de que sea seleccionado. Este método presenta problemas si
     # el fitness de unos pocos individuos es muy superior (varios órdenes de
     # magnitud) al resto, ya que estos serán seleccionados de forma repetida y
     # casi todos los individuos de la siguiente generación serán “hijos” de
     # los mismos “padres” (poca variación).
     # Método rank: la probabilidad de selección de un individuo es inversamente
     # proporcional a la posición que ocupa tras ordenar todos los individuos
     # de mayor a menor fitness. Este método es menos agresivo que el método
     # ruleta cuando la diferencia entre los mayores fitness es varios órdenes
     # de magnitud superior al resto.
     # Selección competitiva (tournament): se seleccionan aleatoriamente dos
     # parejas de individuos de la población (todos con la misma probabilidad).
     # De cada pareja se selecciona el que tenga mayor fitness. Finalmente,
     # se comparan los dos finalistas y se selecciona el de mayor fitness. Este
     # método tiende a generar una distribución de la probabilidad de selección
     # más equilibrada que las dos anteriores.
     # Selección truncada (truncated selection): se realizan selecciones
     # aleatorias de individuos, habiendo descartado primero los n individuos
     # con menor fitness de la población.
     # 
     # La conversión de fitness a probabilidad es distinta dependiendo de la
     # métrica utilizada para calcular el fitness. Si el fitness toma valores en
     # el rango [-inf, 0], cuanto más próximo a 0 sea el fitness (menor la
     # magnitud del valor negativo), mayor debe ser la probabilidad de ser
     # seleccionado. Para lograr la conversión se emplea −1/fitness.
     # 
     # Si el fitness equivale al accuracy o f1, cuanto mayor sea el fitness,
     # mayor debe ser la probabilidad de ser seleccionado. En este caso, no es
     # necesaria ninguna modificación.
     
     
     # COMPROBACIONES INICIALES
     # ----------------------------------------------------------------------
     if (!metodo_seleccion %in% c("ruleta", "rank", "tournament")) {
       stop("El método de selección debe de ser ruleta, rank o tournament.")
     }
     
     # SELECCIÓN DE INDIVIDUOS
     # ----------------------------------------------------------------------
     # Se crea un vector con el fitness de cada individuo de la población.
     vector_fitness <- rep(NA, self$n_individuos)
     for (i in 1:self$n_individuos) {
       vector_fitness[i] <- self$individuos[[i]]$fitness
     }
     
     # Se calcula la probabilidad de selección de cada individuo en función
     # de su fitness.
     if (metodo_seleccion == "ruleta") {
       if (self$metrica %in% c("-mse", "-mae")) {
         # Si el fitness es [-inf,0] se emplea 1/fitness
         vector_fitness <- 1/vector_fitness
       }
       probabilidad_seleccion <- vector_fitness / sum(vector_fitness)
       ind_seleccionado <- sample(
         x    = 1:self$n_individuos,
         size = n,
         prob = probabilidad_seleccion,
         replace = TRUE
       )
     }else if (metodo_seleccion == "rank") {
       # La probabilidad con este método es inversamente proporcional a la
       # posición en la que quedan ordenados los individuos de menor a mayor
       # fitness.
       if (self$metrica %in% c("-mse", "-mae")) {
         vector_fitness <- (-1/vector_fitness)
       }
       probabilidad_seleccion <- 1 / rank(-1*vector_fitness)
       probabilidad_seleccion <- probabilidad_seleccion / sum(probabilidad_seleccion)
       
       ind_seleccionado <- sample(
         x    = 1:self$n_individuos,
         size = n,
         prob = probabilidad_seleccion
       )
     }else if (metodo_seleccion == "tournament") {
       ind_seleccionado <- rep(NA, n)
       for (i in 1:n) {
         # Se seleccionan aleatoriamente dos parejas de individuos.
         ind_candidatos_a <- sample(x = 1:self$n_individuos, size = 2)
         ind_candidatos_b <- sample(x = 1:self$n_individuos, size = 2)
         
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
         ind_seleccionado[i] <- ifelse(
           vector_fitness[ind_ganador_a] > vector_fitness[ind_ganador_b],
           ind_ganador_a,
           ind_ganador_b
         )
       }
     }
     
     # INFORMACIÓN DEL PROCESO (VERBOSE)
     # ----------------------------------------------------------------------
     if (verbose) {
       cat("----------------------", "\n")
       cat("Individuo seleccionado", "\n")
       cat("----------------------", "\n")
       cat("Método selección:", metodo_seleccion, "\n")
       cat("Índice seleccionado:", ind_seleccionado, "\n")
     }
     
     if (return_indices) {
       return(ind_seleccionado)
     }else{
       if (n == 1) {
         return(self$individuos[ind_seleccionado]$clone(deep = TRUE))
       }
       if (n > 1) {
         return(
           purrr::map(
             .x = poblacion$individuos[ind_seleccionado],
             .f = function(x){x$clone(deep = TRUE)}
           )
         )
       }
     }
   },
   
   crear_nueva_generacion = function(metodo_seleccion = "tournament",
                                     metodo_cruce    = "uniforme",
                                     elitismo = 0.1,
                                     prob_mut = 0.1,
                                     verbose  = FALSE,
                                     verbose_seleccion = FALSE,
                                     verbose_cruce     = FALSE,
                                     verbose_mutacion  = FALSE) {
     
     # Este método somete la población a una nueva generación.
     # 
     # Parameters
     # ----------
     # metodo_seleccion : {"ruleta", "rank", "tournament"}
     #     método de selección, ver notas para más información.
     #     (default tournament)
     # 
     # metodo_cruce : {"uniforme", "punto_simple"}
     #     método de cruamiento empleado.
     # 
     # elitismo : numeric
     #     porcentaje de mejores individuos de la población actual que pasan
     #     directamente a la siguiente población. De esta forma, se asegura
     #     que la siguiente generación no sea nunca peor. (default 0.1)
     # 
     # prob_mut : numeric
     #     probabilidad que tiene cada posición del individuo de mutar.
     #     (default 0.1)
     # 
     # verbose : logical
     #     mostrar información del proceso por pantalla. (default ``FALSE``)
     # 
     # verbose_seleccion : logical, optional
     #     mostrar información de cada selección por pantalla.
     #     (default ``FALSE``)
     # 
     # verbose_cruce : logical
     #     mostrar información de cada cruce por pantalla.
     #     (default ``FALSE``)
     # 
     # verbose_mutacion : logical
     #     mostrar información de cada mutación por pantalla.
     #     (default ``FALSE``)
     # 
     # Examples
     # --------
     # library(MASS)
     # data("Boston")
     # poblacion = Poblacion$new(
     #                  n_individuos = 5,
     #                  n_variables = 13,
     #                  n_max       = 3,
     #                  n_min       = 1,
     #                  n_max_estricto = FALSE,
     #                  verbose     = FALSE
     #               )
     # 
     # descendencia = poblacion$cruzar_individuos(
     #                     parental_1 = 1,
     #                     parental_2 = 2,
     #                     metodo_cruce = "punto_simple",
     #                     verbose    = TRUE
     #                 )
     # 
     # poblacion$mostrar_individuos()
     # 
     # poblacion$evaluar_poblacion(
     #      x  = Boston[,-14],
     #      y  = Boston[,14],
     #      modelo = "lm", 
     #      cv = 5,
     #      metrica = "-mse",
     #      seed = 123,
     #      forzar_evaluacion = TRUE,
     #      n_tree  = 50,
     #      verbose = TRUE
     # )
     # 
     # poblacion$crear_nueva_generacion(
     #   metodo_seleccion   = "tournament",
     #   metodo_cruce       = "uniforme",
     #   elitismo           = 0.1,
     #   prob_mut           = 0.01,
     #   verbose            = TRUE,
     #   verbose_seleccion  = FALSE,
     #   verbose_cruce      = FALSE,
     #   verbose_mutacion   = FALSE
     # )
     
     
     # Lista donde almacenar los individuos de la nueva generación.
     nuevos_individuos <- list()
     
     # ELITISMO
     # ----------------------------------------------------------------------
     if (elitismo > 0) {
       # Número de individuos que pasan directamente a la siguiente
       # generación.
       n_elitismo <- ceiling(self$n_individuos*elitismo)
       
       # Se identifican los n_elitismo individuos con mayor fitness (élite).
       array_fitness <- rep(NA, self$n_individuos)
       for (i in 1:self$n_individuos) {
         array_fitness[i] <- self$individuos[[i]]$fitness
       }
       rank <- order(array_fitness, decreasing = TRUE)
       elite <- purrr::map(
         .x = self$individuos[rank[1:n_elitismo]],
         .f = function(x){x$clone(deep = TRUE)}
       )
       # Se añaden los individuos élite a la lista de nuevos individuos.
       nuevos_individuos <- c(nuevos_individuos, elite)
     }else {
       n_elitismo <- 0
     }
     
     # CREACIÓN DE NUEVOS INDIVIDUOS POR CRUCES
     # ----------------------------------------------------------------------
     for (i in 1:(self$n_individuos - n_elitismo)) {
       # Seleccionar parentales
       indice_parentales <- self$seleccionar_individuo(
         n                = 2,
         return_indices   = TRUE,
         metodo_seleccion = metodo_seleccion,
         verbose          = verbose_seleccion
       )
       # Cruzar parentales para obtener la descendencia
       descendencia <- self$cruzar_individuos(
         parental_1   = indice_parentales[1],
         parental_2   = indice_parentales[2],
         metodo_cruce = metodo_cruce,
         verbose      = verbose_cruce
       )
       # Mutar la descendencia
       descendencia$mutar(
         prob_mut = prob_mut,
         verbose  = verbose_mutacion
       )
       
       # Si n_max_estricto=TRUE, se elimina el exceso de TRUEs en la
       # secuencia de la descendencia.
       if (self$n_max_estricto) {
         descendencia$forzar_n_max()
       }
       # Se añade la descendencia a la lista de nuevos individuos. Para
       # que no de error la unión, se introduce el individuo descendencia
       # dentro de una lista.
       nuevos_individuos <- c(nuevos_individuos, descendencia$clone(deep = TRUE))
     }
     # ACTUALIZACIÓN INFORMACIÓN DE LA POBLACIÓN
     # ----------------------------------------------------------------------
     self$individuos <- purrr::map(
       nuevos_individuos,
       .f = function(x){x$clone(deep = TRUE)}
     )
     self$mejor_individuo     <- NA
     self$mejor_fitness       <- NA
     self$mejor_valor_metrica <- NA
     self$mejor_secuencia     <- NA
     self$mejor_predictores   <- NA
     self$nombre_mejor_predictores <- NA
     self$evaluada            <- FALSE
     
     # INFORMACIÓN DEL PROCESO (VERBOSE)
     # ----------------------------------------------------------------------
     if (verbose) {
       cat("----------------------", "\n")
       cat("Nueva población creada", "\n")
       cat("----------------------", "\n")
       cat("Método selección:", metodo_seleccion, "\n")
       cat("Elitismo:", elitismo, "\n")
       cat("Número individuos élite:", n_elitismo, "\n")
       cat("Número de nuevos individuos:", self$n_individuos - n_elitismo, "\n")
       cat("\n")
     }
   },
   
   optimizar = function(x,
                        y,
                        modelo,
                        metrica,
                        cv   = 5,
                        seed = 123,
                        nivel_referencia = NULL,
                        n_generaciones   = 50,
                        metodo_seleccion = "tournament",
                        metodo_cruce     = "uniforme",
                        elitismo = 0.1,
                        prob_mut = 0.1,
                        n_tree   = 100,
                        parada_temprana   = FALSE,
                        rondas_parada     = NULL,
                        tolerancia_parada = NULL,
                        verbose           = FALSE,
                        verbose_nueva_generacion = FALSE,
                        verbose_seleccion  = FALSE,
                        verbose_cruce      = FALSE,
                        verbose_mutacion   = FALSE,
                        verbose_evaluacion = FALSE) {
     
     # Este método realiza el proceso de optimización de una población.
     # 
     # Parameters
     # ----------
     # 
     # x: matrix, data.frame, tibble
     #   matriz de predictores.
     #   
     # y: vector
     #   variable respuesta.
     #   
     # cv: numeric
     #   número de particiones de validación cruzada.
     #   
     # seed: numeric
     #   semilla para garantizar reproducibilidad en el proceso de CV.
     #   
     # metrica: (-mse, -mae, f1, kappa, accuracy)
     #   métrica utilizada para calcular el fitness. Por defecto es el -mse para
     #   regresión y accuracy para clasificación. En el caso de clasificación
     #   binaria, se acepta también f1 y kappa.
     #   
     # nivel_referencia: character
     #   valor de la variable respuesta considerado como referencia. Necesario
     #   cuando la métrica es f1 o kappa.
     # 
     # n_tree : integer
     #     número de árboles en los modelos random forest. (default 50)
     # 
     # n_generaciones : integer , optional
     #     número de generaciones de optimización. (default 50)
     # 
     # metodo_seleccion : {"ruleta", "rank", "tournament"}
     #     método de selección, ver notas para más información.
     #     (default `tournament`)
     # 
     # metodo_cruce : {"uniforme", "punto_simple"}
     #     método de cruamiento empleado.
     # 
     # elitismo : numeric, optional
     #     porcentaje de mejores individuos de la población actual que pasan
     #     directamente a la siguiente población. De esta forma, se asegura
     #     que, la siguiente generación, no sea nunca peor. (default 0.1)
     # 
     # prob_mut : numeric, optional
     #     probabilidad que tiene cada posición del individuo de mutar.
     #     (default 0.1)
     # 
     # parada_temprana : logical, optional
     #     si durante las últimas `rondas_parada` generaciones la diferencia
     #     absoluta entre mejores individuos no es superior al valor de 
     #     `tolerancia_parada`, se detiene el algoritmo y no se crean nuevas
     #     generaciones. (default ``FALSE``)
     # 
     # rondas_parada : integer, optional
     #     número de generaciones consecutivas sin mejora mínima para que se
     #     active la parada temprana. (default ``NULL``)
     # 
     # tolerancia_parada : numeric or integer, optional
     #     valor mínimo que debe tener la diferencia de generaciones consecutivas
     #     para considerar que hay cambio. (default ``NULL``)
     # 
     # verbose : logical, optional
     #     mostrar información del proceso por pantalla. (default ``FALSE``)
     # 
     # verbose_nueva_generacion : logical, optional
     #     mostrar información de cada nueva generación por pantalla.
     #     (default ``FALSE``)
     # 
     # verbose_seleccion : logical, optional
     #     mostrar información de cada selección por pantalla.
     #     (default ``FALSE``)
     # 
     # verbose_cruce : logical, optional
     #     mostrar información de cada cruce por pantalla.
     #     (default ``FALSE``)
     # 
     # verbose_mutacion : logical, optional
     #     mostrar información de cada mutación por pantalla.
     #     (default ``FALSE``)
     # 
     # Examples
     # --------
     # library(MASS)
     # data("Boston")
     # poblacion = Poblacion$new(
     #                  n_individuos = 5,
     #                  n_variables = 13,
     #                  n_max       = 3,
     #                  n_min       = 1,
     #                  n_max_estricto = FALSE,
     #                  verbose     = FALSE
     #               )
     # 
     # poblacion$optimizar(
     #     x                  = Boston[,-14],
     #     y                  = Boston[,14],
     #     cv                 = 5,
     #     modelo             = "randomforest",
     #     metrica            = "-mse",
     #     n_generaciones     = 25,
     #     metodo_seleccion   = "tournament",
     #     metodo_cruce       = "uniforme",
     #     elitismo           = 0.1,
     #     prob_mut           = 0.1,
     #     parada_temprana    = TRUE,
     #     rondas_parada      = 5,
     #     tolerancia_parada  = 10**-4,
     #     verbose            = TRUE,
     #     verbose_nueva_generacion = FALSE,
     #     verbose_seleccion        = FALSE,
     #     verbose_cruce            = FALSE,
     #     verbose_mutacion         = FALSE,
     #     verbose_evaluacion       = FALSE
     # )
     
     
     # COMPROBACIONES INICIALES
     # ------------------------------------------------------------------------
     # Si se activa la parada temprana, hay que especificar los argumentos
     # rondas_parada y tolerancia_parada.
     if (parada_temprana && (is.null(rondas_parada) || is.null(tolerancia_parada))) {
       stop(
         paste(
           "Para activar la parada temprana es necesario indicar un valor de", 
           "rondas_parada y de tolerancia_parada."
         )
       )
     }
     
     # Una vez conocido el número de generaciones se crean los vectores y listas
     # de almacenamiento con el tamaño adecuado.
     self$historico_individuos          <- vector(mode = "list", length = n_generaciones)
     self$historico_mejor_secuencia     <- vector(mode = "list", length = n_generaciones)
     self$historico_mejor_predictores   <- vector(mode = "list", length = n_generaciones)
     self$historico_mejor_fitness       <- rep(NA, n_generaciones)
     self$historico_mejor_valor_metrica <- rep(NA, n_generaciones)
     self$diferencia_abs                <- rep(NA, n_generaciones)
     
     # ITERACIONES (GENERACIONES)
     # ----------------------------------------------------------------------
     start_time <- Sys.time()
     
     for (i in 1:n_generaciones) {
       
       if (verbose) {
         cat("----------------", "\n")
         cat("Generación:", i, "\n")
         cat("----------------", "\n")
       }
       
       # En la primera iteración, la población ya está creada
       if (i > 1) {
         # CREAR UNA NUEVA GENERACIÓN
         # --------------------------------------------------------------------   
         self$crear_nueva_generacion(
           metodo_seleccion   = metodo_seleccion,
           metodo_cruce       = metodo_cruce,
           elitismo           = elitismo,
           prob_mut           = prob_mut,
           verbose            = verbose_nueva_generacion,
           verbose_seleccion  = verbose_seleccion,
           verbose_cruce      = verbose_cruce,
           verbose_mutacion   = verbose_mutacion
         )
       }
       
       # EVALUAR INDIVIDUOS DE LA POBLACIÓN
       # ----------------------------------------------------------------------
       self$evaluar_poblacion(
         x  = x,
         y  = y,
         cv = cv,
         seed        = seed,
         modelo      = modelo,
         metrica     = metrica,
         forzar_evaluacion = FALSE,
         n_trees     = n_trees,
         verbose     = verbose_evaluacion
       )
       
       # SE ALMACENA LA INFORMACIÓN DE LA GENERACIÓN EN LOS HISTÓRICOS
       # ----------------------------------------------------------------------
       self$historico_individuos[[i]] <- purrr::map(
         .x  = self$individuos,
         .f = function(x){x$clone(deep = TRUE)}
       )
       
       self$historico_mejor_secuencia[[i]]   <- self$mejor_secuencia
       
       self$historico_mejor_predictores[[i]] <- self$mejor_predictores
       
       self$historico_mejor_fitness[i]       <- self$mejor_fitness
       
       self$historico_mejor_valor_metrica[i] <- self$mejor_valor_metrica
       
       # SE CALCULA LA DIFERENCIA ABSOLUTA RESPECTO A LA GENERACIÓN ANTERIOR
       # ----------------------------------------------------------------------
       # La diferencia solo puede calcularse a partir de la segunda generación.
       if (i == 1) {
         self$diferencia_abs[i] <- NA
       }else {
         diferencia <- abs(
           self$historico_mejor_fitness[i] - 
             self$historico_mejor_fitness[i - 1]
         )
         self$diferencia_abs[i] <- diferencia
       }
       
       # CRITERIO DE PARADA
       # ----------------------------------------------------------------------
       # Si durante las últimas n generaciones, la diferencia absoluta entre
       # mejores individuos no es superior al valor de tolerancia_parada,
       # se detiene el algoritmo y no se crean nuevas generaciones.
       if (parada_temprana && i > rondas_parada) {
         ultimos_n <- self$diferencia_abs[(i - rondas_parada + 1):i]
         if (all(ultimos_n < tolerancia_parada)) {
           print(
             paste("Algoritmo detenido en la generación",  
                   i,
                   "por falta cambio absoluto mínimo de",
                   tolerancia_parada, 
                   "durante", 
                   rondas_parada, 
                   "generaciones consecutivas.")
           )
           break
         }
       }
     }
     
     end_time <- Sys.time()
     self$optimizada <- TRUE
     self$iter_optimizacion <- i 
     
     # IDENTIFICACIÓN DEL MEJOR INDIVIDUO DE TODO EL PROCESO
     # ------------------------------------------------------------------------
     indice_valor_optimo  <- which.max(self$historico_mejor_fitness)
     self$fitness_optimo  <- self$historico_mejor_fitness[indice_valor_optimo]
     self$valor_metrica_optimo <- self$historico_mejor_valor_metrica[indice_valor_optimo]
     self$secuencia_optima <- self$historico_mejor_secuencia[[indice_valor_optimo]]
     self$predictores_optimos <- self$historico_mejor_predictores[[indice_valor_optimo]]
     self$nombre_predictores_optimos <- colnames(x)[self$predictores_optimos]
     
     # CREACIÓN DE UN DATAFRAME CON LOS RESULTADOS
     # ------------------------------------------------------------------------
     # Si ha habido parada temprana, los vectores y listas de almacenamiento
     # contienen valores NA.
     self$historico_individuos          <- self$historico_individuos[1:i]
     self$historico_mejor_secuencia     <- self$historico_mejor_secuencia[1:i]
     self$historico_mejor_predictores   <- self$historico_mejor_predictores[1:i]
     self$historico_mejor_fitness       <- self$historico_mejor_fitness[1:i]
     self$historico_mejor_valor_metrica <- self$historico_mejor_valor_metrica[1:i]
     self$diferencia_abs                <- self$diferencia_abs[1:i]
     
     self$resultados_df <- tibble:::tibble(
       "mejor_fitness"            = self$historico_mejor_fitness,
       "mejor_valor_metrica"      = self$historico_mejor_valor_metrica,
       "mejor_secuencia"          = self$historico_mejor_secuencia,
       "indice_mejor_predictores" = self$historico_mejor_predictores,
       "diferencia_abs"           = self$diferencia_abs
     )
     self$resultados_df$generacion <- seq_len(nrow(self$resultados_df))
     
     cat("-----------------------", "\n")
     cat("Optimización finalizada", "\n")
     cat("-----------------------", "\n")
     cat("Fecha finalización:", as.character(lubridate::now(tzone = "UTC")), "\n")
     cat("Duración optimización: ")
     print(difftime(end_time, start_time))
     cat("\n")
     cat("Número de generaciones:", self$iter_optimizacion, "\n")
     cat("Secuencia óptima:", self$secuencia_optima, "\n")
     cat("Índice predictores óptimos:", self$predictores_optimos, "\n")
     cat("Nombre predictores óptimos:", colnames(x)[self$predictores_optimos],"\n")
     cat("Valor métrica óptimo:", self$valor_metrica_optimo, "\n")
     cat("\n")
   },
   
   plot_evolucion_fitness = function() {
     
     # Este método crea un gráfico con la evolución del fitness del mejor
     # individuo a lo largo de las generaciones.
     
     if (!self$optimizada) {
       stop(
         paste(
           "El gráfico solo puede generarse si la población ha sido",
           "optimizada previamente."
         )
       )
     }
     
     ggplot(data = self$resultados_df, aes(x = generacion, y = mejor_fitness)) +
       geom_line() +
       geom_point() +
       labs(title = 'Evolución del mejor individuo',
            x = 'generacion',
            y = 'fitness'
       ) +
       theme_bw()
   },
   
   plot_frecuencia_seleccion = function(nombre_predictores = TRUE,
                                        x) {
     
     # Este método crea un gráfico con la frecuencia relativa con la que
     # aparece cada predictor en el mejor individuo a lo largo de las
     # generaciones.
     
     if (!self$optimizada) {
       stop(
         paste(
           "El gráfico solo puede generarse si la población ha sido",
           "optimizada previamente."
         )
       )
     }
     
     frecuencia_selecion <- tibble::enframe(
       x = table(unlist(self$historico_mejor_predictores)),
       name = "pred",
       value = "frec"
     )
     frecuencia_selecion$frec <- 100*(frecuencia_selecion$frec/self$iter_optimizacion)
     
     if (nombre_predictores) {
       frecuencia_selecion$pred <- colnames(x)[as.numeric(frecuencia_selecion$pred)]
     }
     
     ggplot(data = frecuencia_selecion,
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
   }
  )
)


################################################################################
#                                 EJEMPLOS                                     #
################################################################################
# data("iris")
# iris <- iris[iris$Species != "setosa", ]
# iris$Species <- as.character(iris$Species)
# individuo = Individuo$new(
#   n_variables = 4,
#   n_max       = 3,
#   n_min       = 1,
#   n_max_estricto = FALSE,
#   verbose     = FALSE
# )
# 
# individuo$evaluar_individuo(
#   modelo = "glm",
#   x  = iris[, -5],
#   y  = iris[, 5],
#   cv = 5,
#   metrica = "f1",
#   nivel_referencia = "versicolor",
#   verbose = FALSE
# )
# individuo
# 
# 
# 
# library(mlbench)
# library(dplyr)
# 
# set.seed(123)
# simulacion <- mlbench.friedman1(n = 500, sd = 1)
# # El objeto simulación es una lista que  contiene una matriz con los 10 predictores y
# # un vector con la variable respuesta. Se unen todos los datos en un único dataframe.
# datos           <- as.data.frame(simulacion$x)
# datos$y         <- simulacion$y
# colnames(datos) <- c(paste("x", 1:10, sep = ""), "y")
# datos           <- datos %>% select(y, everything())
# 
# n <- 500
# p <- 20
# ruido           <- matrix(rnorm(n * p), nrow = n)
# colnames(ruido) <- paste("x", 11:(10 + p), sep = "")
# ruido           <- as.data.frame(ruido)
# datos           <- bind_cols(datos, ruido)
# 
# poblacion = Poblacion$new(
#   n_individuos = 50,
#   n_variables  = ncol(datos[, -1]),
#   n_max        = 5,
#   n_min        = 1,
#   n_max_estricto = FALSE,
#   verbose     = FALSE
# )
# 
# poblacion$optimizar(
#   x                  = datos[, -1],
#   y                  = datos$y,
#   cv                 = 5,
#   modelo             = "randomforest",
#   n_tree             = 50,
#   metrica            = "-mse",
#   n_generaciones     = 20,
#   metodo_seleccion   = "tournament",
#   metodo_cruce       = "uniforme",
#   elitismo           = 0.1,
#   prob_mut           = 0.1,
#   parada_temprana    = TRUE,
#   rondas_parada      = 5,
#   tolerancia_parada  = 0.01,
#   verbose            = TRUE,
#   verbose_nueva_generacion = FALSE,
#   verbose_seleccion        = FALSE,
#   verbose_cruce            = FALSE,
#   verbose_mutacion         = FALSE,
#   verbose_evaluacion       = FALSE
# )
# 
# poblacion$plot_evolucion_fitness()
# 
# poblacion$plot_frecuencia_seleccion(nombre_predictores = TRUE,
#                                     x = datos[, -1])