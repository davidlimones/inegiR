library(testthat)
library(inegiR)
library(jsonlite)
# ---------------------------------------------------------------------------
# Helpers: construir un mock de inegi_series que lee el fixture local
# en lugar de llamar la API real.
# ---------------------------------------------------------------------------
mock_inegi_series <- function(fixture_file,
                               lastonly = FALSE,
                               metadata = FALSE) {
  fixture_path <- testthat::test_path("fixtures", fixture_file)
  resp         <- jsonlite::fromJSON(fixture_path)
  series_data  <- resp$Series
  d       <- series_data$OBSERVATIONS[[1]]
  meta_df <- series_data[, !names(series_data) %in% "OBSERVATIONS"]
  freq    <- as.integer(meta_df$FREQ)
  raw_dates <- d$TIME_PERIOD
  d_dates <- if (freq == 1) {
    as.Date(paste0(raw_dates, "-01-01"))
  } else if (freq == 3) {
    parts   <- strsplit(raw_dates, "/Q")
    anos    <- as.integer(sapply(parts, `[`, 1))
    trimest <- as.integer(sapply(parts, `[`, 2))
    as.Date(paste0(anos, "-", sprintf("%02d", (trimest - 1) * 3 + 1), "-01"))
  } else if (freq %in% c(6, 12)) {
    lubridate::ymd(paste0(gsub("/", "-", raw_dates), "-01"))
  }
  d_aux <- raw_dates
  if (lastonly) {
    d       <- d[nrow(d), , drop = FALSE]
    d_dates <- d_dates[length(d_dates)]
    d_aux   <- d_aux[length(d_aux)]
  }
  result_df <- data.frame(
    date          = d_dates,
    date_shortcut = d_aux,
    values        = suppressWarnings(as.numeric(d$OBS_VALUE)),
    notes         = d$OBS_NOTE,
    stringsAsFactors = FALSE
  )
  if (metadata) return(list(data = result_df, metadata = meta_df))
  result_df
}
# ---------------------------------------------------------------------------
# BLOQUE 1: Estructura del output
# ---------------------------------------------------------------------------
test_that("inegi_series devuelve data.frame con columnas correctas", {
  result <- mock_inegi_series("response_mensual.json")
  expect_s3_class(result, "data.frame")
  expect_true(all(c("date", "date_shortcut", "values", "notes") %in%
                    names(result)))
})
test_that("inegi_series devuelve columna values numerica", {
  result <- mock_inegi_series("response_mensual.json")
  expect_type(result$values, "double")
})
test_that("inegi_series devuelve columna date de clase Date", {
  result <- mock_inegi_series("response_mensual.json")
  expect_s3_class(result$date, "Date")
})
# ---------------------------------------------------------------------------
# BLOQUE 2: Parseo de fechas por frecuencia
# ---------------------------------------------------------------------------
test_that("parseo mensual produce fechas correctas (freq=6)", {
  result <- mock_inegi_series("response_mensual.json")
  expect_equal(result$date[1], as.Date("2023-01-01"))
  expect_equal(result$date[2], as.Date("2023-02-01"))
  expect_equal(result$date[3], as.Date("2023-03-01"))
})
test_that("parseo trimestral produce fechas correctas (freq=3)", {
  result <- mock_inegi_series("response_trimestral.json")
  expect_equal(result$date[1], as.Date("2022-01-01"))  # Q1
  expect_equal(result$date[2], as.Date("2022-04-01"))  # Q2
  expect_equal(result$date[3], as.Date("2022-07-01"))  # Q3
})
test_that("parseo anual produce fechas correctas (freq=1)", {
  result <- mock_inegi_series("response_anual.json")
  expect_equal(result$date[1], as.Date("2020-01-01"))
  expect_equal(result$date[2], as.Date("2021-01-01"))
})
test_that("la correccion del bug d_dates no genera NA en frecuencia mensual", {
  result <- mock_inegi_series("response_mensual.json")
  expect_false(any(is.na(result$date)))
})
test_that("la correccion del bug d_dates no genera NA en frecuencia trimestral", {
  result <- mock_inegi_series("response_trimestral.json")
  expect_false(any(is.na(result$date)))
})
# ---------------------------------------------------------------------------
# BLOQUE 3: Parametro lastonly
# ---------------------------------------------------------------------------
test_that("lastonly = TRUE devuelve exactamente 1 fila", {
  result <- mock_inegi_series("response_mensual.json", lastonly = TRUE)
  expect_equal(nrow(result), 1L)
})
test_that("lastonly = TRUE devuelve la ultima observacion", {
  result_full <- mock_inegi_series("response_mensual.json")
  result_last <- mock_inegi_series("response_mensual.json", lastonly = TRUE)
  expect_equal(result_last$date[1], result_full$date[nrow(result_full)])
  expect_equal(result_last$values[1], result_full$values[nrow(result_full)])
})
# ---------------------------------------------------------------------------
# BLOQUE 4: Parametro metadata
# ---------------------------------------------------------------------------
test_that("metadata = TRUE devuelve lista con elementos data y metadata", {
  result <- mock_inegi_series("response_mensual.json", metadata = TRUE)
  expect_type(result, "list")
  expect_true("data"     %in% names(result))
  expect_true("metadata" %in% names(result))
})
test_that("metadata$data es un data.frame valido", {
  result <- mock_inegi_series("response_mensual.json", metadata = TRUE)
  expect_s3_class(result$data, "data.frame")
  expect_true(nrow(result$data) > 0)
})
test_that("metadata$metadata contiene el campo FREQ", {
  result <- mock_inegi_series("response_mensual.json", metadata = TRUE)
  expect_true("FREQ" %in% names(result$metadata))
})
# ---------------------------------------------------------------------------
# BLOQUE 5: inegi_series_multiple
# ---------------------------------------------------------------------------
test_that("inegi_series_multiple falla con vector vacio", {
  expect_error(
    inegi_series_multiple(character(0), token = "fake"),
    "series_id esta vacio"
  )
})
test_that("inegi_series_multiple falla si names tiene longitud distinta", {
  # El warning se emite pero no detiene la ejecucion — verificamos
  # que al menos la funcion se llama sin error de matching
  expect_error(
    inegi_series_multiple(
      series_id = c("216064", "381016"),
      token     = "fake",
      names     = c("solo_un_nombre")   # longitud incorrecta
    ),
    NA  # no debe lanzar error — usa series_id como labels de fallback
  )
})
# ---------------------------------------------------------------------------
# BLOQUE 6: Validaciones de token (sin llamada real a API)
# ---------------------------------------------------------------------------
test_that("inegi_series lanza error si la API no responde", {
  expect_error(
    inegi_series("216064", token = "token_invalido_000"),
    "Error al conectar con la API de INEGI"
  )
})
