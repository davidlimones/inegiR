#' Returns INEGI data series
#'
#' Returns a data.frame with the time series chosen from INEGI webservice.
#' If the parameter Metadata is TRUE, a list is returned with two objects:
#' data and metadata.
#'
#' @param series_id an indicator ID from INEGI API documentation.
#' @param token API token supplied by INEGI.
#' @param geography Geography code. Defaults to "00" (National).
#' @param database "BIE" or "BISE". Defaults to "BIE".
#' @param metadata If TRUE, returns list with data and metadata.
#' @param lastonly If TRUE, returns only last observation.
#' @param as_tt If TRUE, returns tibble time object.
#' @param as_compact If TRUE, returns compact data.frame.
#' @return data.frame or list
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate ymd
#' @importFrom dplyr bind_rows
#' @examples
#' \dontrun{
#' token <- "webservice_token"
#' INPC <- inegi_series("216064", token)
#' }
#' @export
inegi_series <- function(series_id,
                         token,
                         geography  = "00",
                         database   = "BIE",
                         metadata   = FALSE,
                         lastonly   = FALSE,
                         as_tt      = FALSE,
                         as_compact = FALSE) {
  url <- paste0(
    "https://www.inegi.org.mx/app/api/indicadores/desarrolladores/",
    "jsonxml/INDICATOR/", series_id, "/es/", geography, "/false/",
    database, "/2.0/", token, "?type=json"
  )
  resp <- tryCatch(
    jsonlite::fromJSON(url),
    error = function(e) stop("Error al conectar con la API de INEGI: ", e$message)
  )
  series_data <- resp$Series
  if (is.null(series_data)) stop("La serie ", series_id, " no devolvio datos.")
  d       <- series_data$OBSERVATIONS[[1]]
  meta_df <- series_data[, !names(series_data) %in% "OBSERVATIONS"]
  freq    <- as.integer(meta_df$FREQ)
  raw_dates <- d$TIME_PERIOD
  d_dates <- tryCatch({
    if (freq == 1) {
      # Anual: "2020"
      as.Date(paste0(raw_dates, "-01-01"))
    } else if (freq == 3) {
      # Trimestral: "2020/Q1"
      parts   <- strsplit(raw_dates, "/Q")
      anos    <- as.integer(sapply(parts, `[`, 1))
      trimest <- as.integer(sapply(parts, `[`, 2))
      as.Date(paste0(anos, "-", sprintf("%02d", (trimest - 1) * 3 + 1), "-01"))
    } else if (freq %in% c(6, 12)) {
      # Mensual o bimestral: "2020/01"
      lubridate::ymd(paste0(gsub("/", "-", raw_dates), "-01"))
    } else {
      # Frecuencia no reconocida: intento generico con fallback a NA
      warning("Frecuencia no reconocida (", freq,
              "). Intentando parseo generico.")
      result <- suppressWarnings(lubridate::ymd(raw_dates))
      if (all(is.na(result))) {
        warning("No se pudo parsear la fecha. Se devuelve NA.")
      }
      result
    }
  }, error = function(e) {
    warning("Error al parsear fechas: ", e$message, ". Se devuelve NA.")
    rep(as.Date(NA), length(raw_dates))
  })
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
  if (metadata) {
    return(list(data = result_df, metadata = meta_df))
  }
  result_df
}
#' Returns multiple INEGI data series
#'
#' @param series_id Vector of indicator IDs.
#' @param token API token supplied by INEGI.
#' @param names Optional vector of names for each series.
#' @param geography Geography code. Defaults to "00".
#' @param database "BIE" or "BISE". Defaults to "BIE".
#' @return data.frame
#' @examples
#' \dontrun{
#' token <- "webservice_token"
#' ids   <- c("216064", "381016", "444952")
#' datos <- inegi_series_multiple(ids, token,
#'            names = c("INPC", "PIB", "Desocupacion"))
#' }
#' @export
inegi_series_multiple <- function(series_id,
                                  token,
                                  names     = NULL,
                                  geography = "00",
                                  database  = "BIE") {
  if (length(series_id) == 0) stop("series_id esta vacio.")
  labels <- if (!is.null(names) && length(names) == length(series_id)) {
    names
  } else {
    series_id
  }
  resultados <- vector("list", length(series_id))
  for (i in seq_along(series_id)) {
    resultados[[i]] <- tryCatch({
      df        <- inegi_series(series_id[i], token, geography, database)
      df$series <- labels[i]
      df
    }, error = function(e) {
      warning("Error en serie ", series_id[i], ": ", e$message)
      NULL
    })
  }
  dplyr::bind_rows(resultados)
}
