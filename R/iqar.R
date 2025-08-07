#' Compute IQAr index considering several air pollutants
#'
#' This function computes the IQAr index considering several air pollutants using the Brazilian CONAMA/MMA specification.
#'
#' @param pm10 numeric. Particulate matter \eqn{PM_{10}} concentration, \eqn{\mu g/m^{3}}.
#' @param pm2.5 numeric. Particulate matter \eqn{PM_{2.5}} concentration, \eqn{\mu g/m^{3}}.
#' @param o3 numeric. Ozone \eqn{O_3} concentration, \eqn{\mu g/m^{3}}.
#' @param co numeric. Carbon monoxide \eqn{CO} concentration, \eqn{PPM}.
#' @param no2 numeric. Nitrogen dioxide \eqn{NO_2} concentration, \eqn{\mu g/m^{3}}.
#' @param so2 numeric. Sulfur dioxide \eqn{SO_2} concentration, \eqn{\mu g/m^{3}}.
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
