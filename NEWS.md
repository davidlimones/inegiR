# inegiR 3.1.0
* Fix crítico: corregido el error `'d_dates' no encontrado` causado
  por breaking change en la API de INEGI (issue #25).
* Parseo de fechas reescrito con manejo robusto para todas las
  frecuencias: anual (freq=1), trimestral (freq=3), mensual (freq=6),
  bimestral (freq=12).
* Dependencias modernizadas: reemplazado `plyr` y `zoo` por
  `dplyr` y `lubridate`.
* Mejorado manejo de errores en `inegi_series_multiple()`:
  ahora emite warning por serie fallida en lugar de detener
  toda la descarga.
* Agregada suite de tests con testthat (24 tests).
# inegiR 3.0.0
* Versión original de Eduardo Flores compatible con API v2.0 de INEGI.
* Compatibilidad con nuevo endpoint de INEGI.
* Nuevas funciones incat_* para catálogos.
* Nueva función inegi_series_multiple().
