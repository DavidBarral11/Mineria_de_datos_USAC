#Este proyecto aplica el mismo algoritmo (FP-Growth), pero, en lugar de fim4r (usado en clase), utiliza mlxtend.frequent_patterns de Python vía reticulate. El cambio se debe a que fim4r no está disponible/estable para mi versión de R en Windows y RWeka/rJava presentaron errores.
#Adicionalmente, discretizo todas las variables numéricas con arules::discretize(method="frequency", categories=5) y genero un one-hot robusto que preserva filas y trata NA explícitamente. Las reglas se ordenan por lift (y luego por confianza y soporte), se muestran los Top-3 por segmento (Nacional, Guatemala vs. resto, y Agresor) y se exportan a CSV en out/.
#Metodológicamente, los resultados son comparables: mismo algoritmo y métricas (support, confidence, lift). Pueden existir pequeñas diferencias por discretización y manejo de NA, pero la interpretación y conclusiones siguen la línea de la clase.


need <- c("readxl","arules","reticulate","glue")
to_install <- need[!need %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)

suppressPackageStartupMessages({
  library(readxl)
  library(arules)
  library(reticulate)
  library(glue)
})

# ==== CONFIG ====
ruta <- "C:\\Users\\David\\Downloads\\base-de-datos-violencia-intrafamiliar-ano-2024_v3.xlsx"
stopifnot(file.exists(ruta))

cols_victima <- c("HEC_MES","HEC_DEPTO","VIC_EDAD","VIC_ESCOLARIDAD",
                  "VIC_EST_CIV","VIC_GRUPET","VIC_TRABAJA","VIC_DEDICA")
cols_agresor <- c("HEC_MES","HEC_DEPTO","AGR_EDAD","AGR_ESCOLARIDAD",
                  "AGR_EST_CIV","AGR_TRABAJA","AGR_DEDICA")

min_support <- 0.20  

min_conf    <- 0.50

# ==== LECTURA ====
data <- read_excel(ruta)

prep_df <- function(df) {
  num_cols <- names(df)[sapply(df, is.numeric)]
  if (length(num_cols) > 0) {
    for (cn in num_cols) {
      df[[cn]] <- arules::discretize(df[[cn]], method = "frequency", categories = 5, ordered_result = TRUE)
    }
  }
  # Asegurar factor + nivel "NA" explícito
  df[] <- lapply(df, function(x) {
    if (!is.factor(x)) x <- factor(x)
    addNA(x)  # agrega nivel <NA> para que no se pierdan filas
  })
  df
}

make_onehot <- function(df) {
  n <- nrow(df)
  out <- NULL
  for (cn in names(df)) {
    levs <- levels(df[[cn]])
    # Para cada nivel, crear columna 0/1 (NA cuenta como 0)
    for (lv in levs) {
      col <- as.integer(df[[cn]] == lv)
      col[is.na(col)] <- 0L
      cname <- paste0(cn, "=", lv)
      out <- if (is.null(out)) matrix(col, nrow=n, dimnames = list(NULL, cname))
             else cbind(out, col)
      colnames(out)[ncol(out)] <- cname
    }
  }
  as.data.frame(out, check.names = FALSE)
}


ensure_py <- function() {
  if (!py_module_available("pandas") || !py_module_available("mlxtend")) {
    message("Instalando dependencias de Python (pandas, mlxtend)...")
    reticulate::py_install(c("pandas","mlxtend"), pip = TRUE)
  }
  list(
    pd = import("pandas", convert = TRUE),
    mp = import("mlxtend.frequent_patterns", convert = TRUE)
  )
}

run_fpgrowth <- function(df_items, etiqueta = "Nacional") {
  if (ncol(df_items) == 0) return(invisible(NULL))
  df_items <- prep_df(df_items)
  onehot <- make_onehot(df_items)

  py <- ensure_py()
  pdf <- py$pd$DataFrame(onehot)

  # conjuntos frecuentes + reglas
  freq_itemsets <- py$mp$fpgrowth(pdf, min_support = min_support, use_colnames = TRUE)
  rules_py <- py$mp$association_rules(freq_itemsets, metric = "confidence", min_threshold = min_conf)

  to_text <- function(py_set) paste(sort(as.character(unlist(py_to_r(py_set)))), collapse = " ∧ ")
  rf <- data.frame(
    rules      = paste(sapply(rules_py$antecedents, to_text), "=>", sapply(rules_py$consequents, to_text)),
    support    = as.numeric(rules_py$support),
    confidence = as.numeric(rules_py$confidence),
    lift       = as.numeric(rules_py$lift),
    stringsAsFactors = FALSE
  )
  if (!nrow(rf)) return(invisible(NULL))
  rf <- rf[order(-rf$lift, -rf$confidence, -rf$support), ]

  cat(glue("\n=== TOP-3 {etiqueta} (FP-Growth / mlxtend) por LIFT ===\n"))
  print(utils::head(rf[, c("rules","support","confidence","lift")], 3), row.names = FALSE)
  rf
}

# ==== VÍCTIMA: nacional ====
data_v <- data[, intersect(cols_victima, names(data)), drop = FALSE]
rf_v_nac <- run_fpgrowth(data_v, "Víctima — Nacional")

if ("HEC_DEPTO" %in% names(data_v)) {
  data_v_gt  <- subset(data_v, HEC_DEPTO == 1)
  data_v_out <- subset(data_v, HEC_DEPTO != 1)

  # quitar HEC_DEPTO para evitar reglas triviales
  data_v_gt  <- data_v_gt[,  setdiff(names(data_v_gt),  "HEC_DEPTO"), drop = FALSE]
  data_v_out <- data_v_out[, setdiff(names(data_v_out), "HEC_DEPTO"), drop = FALSE]

  rf_v_gt  <- run_fpgrowth(data_v_gt,  "Víctima — Guatemala (HEC_DEPTO==1)")
  rf_v_out <- run_fpgrowth(data_v_out, "Víctima — Resto del país (HEC_DEPTO!=1)")
}

# ==== AGRESOR: nacional (opcional) ====
data_a <- data[, intersect(cols_agresor, names(data)), drop = FALSE]
rf_a_nac <- if (ncol(data_a)) run_fpgrowth(data_a, "Agresor — Nacional") else NULL

# ==== Guardar CSVs ====
if (!dir.exists("out")) dir.create("out")
if (!is.null(rf_v_nac) && nrow(rf_v_nac)>0) utils::write.csv(rf_v_nac, "C:\Users\David\Downloads\out\reglas_victima_nacional.csv", row.names = FALSE)
if (exists("rf_v_gt")  && !is.null(rf_v_gt)  && nrow(rf_v_gt)>0)  utils::write.csv(rf_v_gt,  "C:\Users\David\Downloads\out\reglas_victima_gt.csv",   row.names = FALSE)
if (exists("rf_v_out") && !is.null(rf_v_out) && nrow(rf_v_out)>0) utils::write.csv(rf_v_out, "C:\Users\David\Downloads\out\reglas_victima_resto.csv", row.names = FALSE)
if (!is.null(rf_a_nac) && nrow(rf_a_nac)>0) utils::write.csv(rf_a_nac, "C:\Users\David\Downloads\out\reglas_agresor_nacional.csv", row.names = FALSE)

cat("\nListo. Si salen pocas reglas, baja min_support a 0.10 o 0.05 y vuelve a correr.\n")


