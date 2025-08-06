#' Compute IQAr index considering several air pollutants
#'
#' This function computes the IQAr index considering several air pollutants using the Brazilian CONAMA/MMA specification.
#'
#' @details
#' Units: mp10: mug/m3, mp2.5: mug/m3, o3: mug/m3, co: PPM, no2: mug/m3, so2: mug/m3
#'
#' @param pm10 numeric. Pollutant concentration. See details bellow for the correct unit.
#' @param pm2.5 numeric. Pollutant concentration. See details bellow for the correct unit.
#' @param o3 numeric. Pollutant concentration. See details bellow for the correct unit.
#' @param co numeric. Pollutant concentration. See details bellow for the correct unit.
#' @param no2 numeric. Pollutant concentration. See details bellow for the correct unit.
#' @param so2 numeric. Pollutant concentration. See details bellow for the correct unit.
#' @param label logical. If `FALSE`, the function will return the IQAr value, if `TRUE` the function will return the IQAr label. Defaults to `FALSE`.
#'
#' @returns numeric. IQAr index value or classification.
#'
#' @export
#' @examples
#' iqar(pm10 = 50, pm2.5 = 80, o3 = 60)
#' iqar(pm10 = 50, pm2.5 = 80, o3 = 60, label = TRUE)
iqar <- function(
  pm10 = NA,
  pm2.5 = NA,
  o3 = NA,
  co = NA,
  no2 = NA,
  so2 = NA,
  label = FALSE
) {
  checkmate::assert_numeric(x = pm10)
  checkmate::assert_numeric(x = pm2.5)
  checkmate::assert_numeric(x = o3)
  checkmate::assert_numeric(x = co)
  checkmate::assert_numeric(x = no2)
  checkmate::assert_numeric(x = so2)
  checkmate::assert_logical(x = label)

  res <- max(
    iqar_pol(x = pm10, pol = "pm10"),
    iqar_pol(x = pm2.5, pol = "pm2.5"),
    iqar_pol(x = o3, pol = "o3"),
    iqar_pol(x = co, pol = "co"),
    iqar_pol(x = no2, pol = "so2"),
    iqar_pol(x = so2, pol = "so2"),
    na.rm = TRUE
  ) |>
    round(digits = 0)

  if (label == TRUE) {
    res <- dplyr::case_when(
      res >= 0 & res <= 40 ~ "N1 - Boa",
      res >= 41 & res <= 80 ~ "N2 - Moderada",
      res >= 81 & res <= 120 ~ "N3 - Ruim",
      res >= 121 & res <= 200 ~ "N4 - Muito Ruim",
      res >= 201 & res <= 400 ~ "N5 - P\u00e9ssima",
      .default = "Sem classifica\u00e7\u00e3o."
    )
  }

  return(res)
}
