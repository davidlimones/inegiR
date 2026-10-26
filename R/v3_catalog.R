#' Local catalog of INEGI indicators (search the BIE/BISE by name)
#'
#' INEGI's API has no free-text search: you need the indicator ID in advance.
#' \code{incat_download} downloads, in a single call, the whole indicator catalog
#' (about 90,000 IDs with their short name) and stores it on disk so that
#' \code{incat_search} can look for indicators offline, ignoring case and accents.
#' \code{inegi_info} makes a live call for one indicator to learn its frequency
#' and latest period before using it.
#'
#' Notes: (1) the catalog uses INEGI's NEW indicator IDs and only has short,
#' non-hierarchical names (many indicators share a name, e.g. "Tasa de
#' desocupacion"), so use \code{inegi_info} to check frequency and vigency;
#' (2) it covers the BIE/BISE bank served under \code{"BIE-BISE"}, not every
#' sociodemographic BISE indicator nor discontinued series.
#'
#' @param token API token supplied by INEGI.
#' @param path File where the catalog is stored (csv). Defaults to the user cache directory of this package.
#' @param equivalences Optional path to INEGI's BIE/BISE equivalence table (xlsx). If given (needs the \code{readxl} package) the catalog is enriched with the old IDs (\code{old_ids}), thematic route and geographic areas when available.
#' @param ... Words to look for. All of them must appear (AND) in the name, ID, old IDs or route.
#' @param max Maximum number of rows to return.
#' @param catalog Optional data.frame returned by \code{incat_download}; if NULL it is read from \code{path}.
#' @param id Indicator ID.
#' @param geography Geography code of INEGI. Defaults to 00 (National).
#'
#' @return \code{incat_download}: invisibly, the catalog data.frame. \code{incat_search}: data.frame with id, description, old_ids and route. \code{inegi_info}: one-row data.frame with frequency code (8 = monthly, 6 = quarterly, 3 = yearly, 9 = biweekly), latest period, latest value and last update.
#'
#' @author David Limones
#'
#' @importFrom jsonlite fromJSON
#' @examples
#' \dontrun{
#' token <- "webservice_token"
#' incat_download(token)
#' incat_search("tasa", "desocupacion")
#' inegi_info("444603", token)
#' }
#' @name incat_catalog
NULL

.incat_default_path <- function(){
  d <- if(exists("R_user_dir", envir = asNamespace("tools"))) tools::R_user_dir("inegiR", "cache") else tempdir()
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  file.path(d, "catalogo_bie_bise.csv")
}

# Lowercase and strip accents. iconv(//TRANSLIT) turns "o with acute" into "'o" on
# macOS, so the leftover punctuation is dropped (applied to both text and query).
.incat_plain <- function(x) gsub("[^a-z0-9 ]", "", tolower(iconv(x, "UTF-8", "ASCII//TRANSLIT")))

#' @export
#' @rdname incat_catalog
incat_download <- function(token, path = .incat_default_path(), equivalences = NULL){
  u <- paste0("https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/CL_INDICATOR/null/es/BIE-BISE/2.0/", token, "?type=json")
  s <- jsonlite::fromJSON(u, simplifyVector = FALSE)
  cat <- data.frame(id = vapply(s$CODE, `[[`, "", "value"),
                    description = trimws(gsub("\\s+", " ", vapply(s$CODE, `[[`, "", "Description"))),
                    old_ids = NA_character_, route = NA_character_, areas = NA_character_,
                    stringsAsFactors = FALSE)
  if(!is.null(equivalences)){
    if(!requireNamespace("readxl", quietly = TRUE)) stop("Package readxl is needed to read the equivalence table")
    eq <- as.data.frame(readxl::read_excel(equivalences))
    names(eq) <- c("id", "new_id", "state", "municipality", "city", "country", "area", "route")
    by_new <- split(eq, eq$new_id)
    info <- t(vapply(by_new, function(d) c(paste(setdiff(unique(d$id), d$new_id[1]), collapse = " "),
                                          d$route[1],
                                          substr(paste(unique(d$area), collapse = "; "), 1, 80)), character(3)))
    m <- match(cat$id, names(by_new))
    cat$old_ids <- ifelse(is.na(m), NA, info[m, 1])
    cat$route   <- ifelse(is.na(m), NA, info[m, 2])
    cat$areas   <- ifelse(is.na(m), NA, info[m, 3])
  }
  cat$downloaded <- format(Sys.Date())
  utils::write.csv(cat, path, row.names = FALSE, fileEncoding = "UTF-8")
  message(nrow(cat), " indicators saved in ", path)
  invisible(cat)
}

#' @export
#' @rdname incat_catalog
incat_search <- function(..., max = 40, catalog = NULL, path = .incat_default_path()){
  if(is.null(catalog)){
    if(!file.exists(path)) stop("No local catalog found. Run incat_download(token) first.")
    catalog <- utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8", colClasses = "character")
  }
  txt <- .incat_plain(paste(catalog$id, catalog$old_ids, catalog$description, catalog$route))
  ok <- rep(TRUE, nrow(catalog))
  for(w in c(...)) ok <- ok & grepl(.incat_plain(w), txt, fixed = TRUE)
  r <- catalog[ok, c("id", "description", "old_ids", "route")]
  message(nrow(r), " matches (showing ", min(max, nrow(r)), ")")
  utils::head(r, max)
}

#' @export
#' @rdname incat_catalog
inegi_info <- function(id, token, geography = "00"){
  u <- paste0("https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/",
              id, "/es/", geography, "/true/BIE-BISE/2.0/", token, "?type=json")
  # warnings/errors from the connection contain the full URL (and the token): hide them
  s <- tryCatch(suppressWarnings(jsonlite::fromJSON(u, simplifyVector = FALSE))$Series[[1]],
                error = function(e) NULL)
  if(is.null(s)) return(data.frame(id = id, status = "no data for this id/geography", stringsAsFactors = FALSE))
  o <- s$OBSERVATIONS[[1]]
  data.frame(id = id, frequency = s$FREQ, last_period = o$TIME_PERIOD,
             last_value = as.numeric(o$OBS_VALUE), last_update = s$LASTUPDATE,
             status = "ok", stringsAsFactors = FALSE)
}
