#' Compute specific IQAr index to several air pollutants
#'
#' This function computes specific IQAr index to several air pollutants using the Brazilian CONAMA/MMA specification.
#'
#' @details
#' Units: pm10: mug/m3, pm2.5: mug/m3, o3: mug/m3, co: PPM, no2: mug/m3, so2: mug/m3
#'
#'
#' @param x numeric. Pollutants concentration. See details bellow for the correct unit.
#' @param pol character. Pollutant abbreviation: pm10, pm2.5, o3, co, no2, so2.
#'
#' @returns numeric. Pollutant specific IQAr index
#'
#' @export
#' @examples
#' iqar_pol(x = 10, pol = "pm2.5")
iqar_pol <- function(x, pol = "pm2.5") {
  checkmate::assert_numeric(x = x)
  checkmate::assert_choice(
    x = pol,
    choices = c("pm10", "pm2.5", "o3", "co", "no2", "so2")
  )

  lim <- NULL
  if (pol == "pm10") {
    lim <- dplyr::case_when(
      x >= 0 & x < 45 ~ c(0, 40, 0, 45),
      x >= 45 & x < 100 ~ c(41, 80, 46, 100),
      x >= 100 & x < 150 ~ c(81, 120, 101, 150),
      x >= 150 & x < 250 ~ c(121, 200, 151, 250),
      x >= 250 ~ c(201, 400, 251, 600),
      .default = NA
    )
  } else if (pol == "pm2.5") {
    lim <- dplyr::case_when(
      x >= 0 & x < 15 ~ c(0, 40, 0, 15),
      x >= 15 & x < 50 ~ c(41, 80, 16, 50),
      x >= 50 & x < 75 ~ c(81, 120, 51, 75),
      x >= 75 & x < 125 ~ c(121, 200, 76, 125),
      x >= 125 ~ c(201, 400, 126, 300),
      .default = NA
    )
  } else if (pol == "o3") {
    lim <- dplyr::case_when(
      x >= 0 & x < 100 ~ c(0, 40, 0, 100),
      x >= 100 & x < 130 ~ c(41, 80, 101, 130),
      x >= 130 & x < 160 ~ c(81, 120, 131, 160),
      x >= 160 & x < 200 ~ c(121, 200, 161, 200),
      x >= 200 ~ c(201, 400, 201, 800),
      .default = NA
    )
  } else if (pol == "co") {
    lim <- dplyr::case_when(
      x >= 0 & x < 9 ~ c(0, 40, 0, 9),
      x >= 9 & x < 11 ~ c(41, 80, 10, 11),
      x >= 11 & x < 13 ~ c(81, 120, 12, 13),
      x >= 13 & x < 15 ~ c(121, 200, 14, 15),
      x >= 15 ~ c(201, 400, 16, 50),
      .default = NA
    )
  } else if (pol == "no2") {
    lim <- dplyr::case_when(
      x >= 0 & x < 200 ~ c(0, 40, 0, 200),
      x >= 200 & x < 240 ~ c(41, 80, 201, 240),
      x >= 240 & x < 320 ~ c(81, 120, 241, 320),
      x >= 320 & x < 1130 ~ c(121, 200, 321, 1130),
      x >= 1130 ~ c(201, 400, 1131, 3750),
      .default = NA
    )
  } else if (pol == "so2") {
    lim <- dplyr::case_when(
      x >= 0 & x < 40 ~ c(0, 40, 0, 40),
      x >= 40 & x < 50 ~ c(41, 80, 41, 50),
      x >= 50 & x < 125 ~ c(81, 120, 51, 125),
      x >= 125 & x < 800 ~ c(121, 200, 126, 800),
      x >= 800 ~ c(201, 400, 801, 2620),
      .default = NA
    )
  }

  res <- lim[1] + ((lim[2] - lim[1]) / (lim[4] - lim[3])) * (x - lim[3])
  res <- round(x = res, digits = 0)

  return(res)
}
