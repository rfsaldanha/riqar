iqar <- function(mp10 = NA, mp2.5 = NA, o3 = NA, co = NA, no2 = NA, so2 = NA) {
  max(
    iqar_gas(mp10, "mp10"),
    iqar_gas(mp2.5, "mp2.5"),
    iqar_gas(o3, "o3"),
    iqar_gas(co, "co"),
    iqar_gas(no2, "so2"),
    iqar_gas(so2, "so2"),
    na.rm = TRUE
  ) |>
    round(digits = 0)
}
