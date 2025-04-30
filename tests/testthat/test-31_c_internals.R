# test data needed


## Radiation functions tests ####
test_that("radiation c internals work as expected", {

  # data for radiation test
  year_test <- 2022
  month_test <- 4
  day_test <- 25
  dateStrings_test <- c("2022-04-25", "2022-04-26")
  J_test <- 2459695
  latrad_test <- 42 * (pi / 180)
  slorad_test <- 2 * (pi / 180)
  asprad_test <- 270 * (pi / 180)
  delta_test <- 0.2288457
  hrad_test <- -1.734900
  solarConstant_test <- 1.352879
  elevation_test <- 900
  diffTemp_test <- 16
  diffTempMonth_test <- 13
  vpa_test <- 2.5
  precipitation_test <- 15
  R_s_test <- 6
  clearday_test <- TRUE
  nsteps_test <- 12L
  Tair_test <- 25
  c_test <- 0.2
  tmin_test <- 5
  tmax_test <- 5 + 16
  alpha_test <- 0.08

  ## radiation_julianDay
  expect_type(
    (julianDay_test <- radiation_julianDay(year_test, month_test, day_test)),
    "integer"
  )
  expect_error(
    radiation_julianDay("2022", "April", "25")
  )

  # radiation_dateStringToJulianDays
  expect_type(
    (dateStringToJulianDays_test <- radiation_dateStringToJulianDays(dateStrings_test)),
    "integer"
  )
  expect_error(
    radiation_dateStringToJulianDays(C("tururu", "larara"))
  )
  expect_identical(julianDay_test, dateStringToJulianDays_test[1])

  # radiation_solarDeclination
  expect_type(
    (solarDeclination_test <- radiation_solarDeclination(J_test)),
    "double"
  )
  expect_error(
    radiation_solarDeclination("tururu")
  )
  expect_error(
    radiation_solarDeclination(c(J_test, J_test + 1))
  )

  # radiation_solarConstant
  expect_type(
    (solarConstant_test <- radiation_solarConstant(J_test)),
    "double"
  )
  expect_error(
    radiation_solarConstant("tururu")
  )
  expect_error(
    radiation_solarConstant(c(J_test, J_test + 1))
  )

  # radiation_sunRiseSet
  expect_type(
    (sunRiseSet_test <- radiation_sunRiseSet(latrad_test, slorad_test, asprad_test, delta_test)),
    "double"
  )
  expect_error(
    radiation_sunRiseSet("tururu", slorad_test, asprad_test, delta_test)
  )
  expect_error(
    radiation_sunRiseSet(c(latrad_test, latrad_test), slorad_test, asprad_test, delta_test)
  )

  # radiation_solarElevation
  expect_type(
    (solarElevation_test <- radiation_solarElevation(latrad_test, delta_test, hrad_test)),
    "double"
  )
  expect_error(
    radiation_solarElevation("tururu", delta_test, hrad_test)
  )
  expect_error(
    radiation_solarElevation(c(latrad_test, latrad_test), delta_test, hrad_test)
  )

  # radiation_daylength
  expect_type(
    (daylength_test <- radiation_daylength(latrad_test, slorad_test, asprad_test, delta_test)),
    "double"
  )
  expect_error(
    radiation_daylength("tururu", slorad_test, asprad_test, delta_test)
  )
  expect_error(
    radiation_daylength(c(latrad_test, latrad_test), slorad_test, asprad_test, delta_test)
  )

  # radiation_daylengthseconds
  expect_type(
    (daylengthseconds_test <- radiation_daylengthseconds(latrad_test, slorad_test, asprad_test, delta_test)),
    "double"
  )
  expect_error(
    radiation_daylengthseconds("tururu", slorad_test, asprad_test, delta_test)
  )
  expect_error(
    radiation_daylengthseconds(c(latrad_test, latrad_test), slorad_test, asprad_test, delta_test)
  )

  # radiation_potentialRadiation
  expect_type(
    (potentialRadiation_test <- radiation_potentialRadiation(
      solarConstant_test, latrad_test, slorad_test, asprad_test, delta_test
    )),
    "double"
  )
  expect_error(
    radiation_potentialRadiation(
      "tururu", latrad_test, slorad_test, asprad_test, delta_test
    )
  )
  expect_error(
    radiation_potentialRadiation(
      c(solarConstant_test, solarConstant_test), latrad_test, slorad_test, asprad_test, delta_test
    )
  )

  # radiation_solarRadiation
  expect_type(
    (solarRadiation_test <- radiation_solarRadiation(
      solarConstant_test, latrad_test, elevation_test, slorad_test, asprad_test,
      delta_test, diffTemp_test, diffTempMonth_test, vpa_test, precipitation_test
    )),
    "double"
  )
  expect_error(
    radiation_solarRadiation(
      "tururu", latrad_test, elevation_test, slorad_test, asprad_test,
      delta_test, diffTemp_test, diffTempMonth_test, vpa_test, precipitation_test
    )
  )
  expect_error(
    radiation_solarRadiation(
      c(solarConstant_test, solarConstant_test), latrad_test, elevation_test,
      slorad_test, asprad_test, delta_test, diffTemp_test, diffTempMonth_test,
      vpa_test, precipitation_test
    )
  )

  # radiation_directDiffuseInstant
  expect_type(
    (directDiffuseInstant_test <- radiation_directDiffuseInstant(
      solarConstant_test, latrad_test, slorad_test, asprad_test,
      delta_test, hrad_test, R_s_test, clearday_test
    )),
    "double"
  )
  expect_length(directDiffuseInstant_test, 8)
  expect_named(directDiffuseInstant_test, c(
    "SolarElevation", "Rpot", "Rpot_flat", "Rg",
    "SWR_direct", "SWR_diffuse", "PAR_direct", "PAR_diffuse"
  ))
  expect_error(
    radiation_directDiffuseInstant(
      "tururu", latrad_test, slorad_test, asprad_test,
      delta_test, hrad_test, R_s_test, clearday_test
    )
  )
  expect_error(
    radiation_directDiffuseInstant(
      c(solarConstant_test, solarConstant_test), latrad_test, slorad_test, asprad_test,
      delta_test, hrad_test, R_s_test, clearday_test
    )
  )

  # radiation_directDiffuseDay
  expect_s3_class(
    (directDiffuseDay_test <- radiation_directDiffuseDay(
      solarConstant_test, latrad_test, slorad_test, asprad_test,
      delta_test, R_s_test, clearday_test, nsteps_test
    )),
    "data.frame"
  )
  expect_identical(nrow(directDiffuseDay_test), nsteps_test)
  expect_named(directDiffuseDay_test, c(
    "SolarHour", "SolarElevation", "Rpot", "Rpot_flat", "Rg",
    "SWR_direct", "SWR_diffuse", "PAR_direct", "PAR_diffuse"
  ))
  expect_error(
    radiation_directDiffuseDay(
      "tururu", latrad_test, slorad_test, asprad_test,
      delta_test, R_s_test, clearday_test, nsteps_test
    )
  )
  expect_error(
    radiation_directDiffuseDay(
      c(solarConstant_test, solarConstant_test), latrad_test, slorad_test, asprad_test,
      delta_test, R_s_test, clearday_test, nsteps_test
    )
  )

  # radiation_skyLongwaveRadiation
  expect_type(
    (skyLongwaveRadiation_test <- radiation_skyLongwaveRadiation(Tair_test, vpa_test, c_test)),
    "double"
  )
  expect_error(
    radiation_skyLongwaveRadiation("tururu", vpa_test, c_test)
  )
  expect_error(
    radiation_skyLongwaveRadiation(c(Tair_test, Tair_test), vpa_test, c_test)
  )

  # radiation_outgoingLongwaveRadiation
  expect_type(
    (outgoingLongwaveRadiation_test <- radiation_outgoingLongwaveRadiation(
      solarConstant_test, latrad_test, elevation_test, slorad_test, asprad_test,
      delta_test, vpa_test, tmin_test, tmax_test, R_s_test
    )),
    "double"
  )
  expect_error(
    radiation_outgoingLongwaveRadiation(
      "tururu", latrad_test, elevation_test, slorad_test, asprad_test,
      delta_test, vpa_test, tmin_test, tmax_test, R_s_test
    )
  )
  expect_error(
    radiation_outgoingLongwaveRadiation(
      c(solarConstant_test, solarConstant_test), latrad_test, elevation_test,
      slorad_test, asprad_test, delta_test, vpa_test, tmin_test, tmax_test,
      R_s_test
    )
  )

  # radiation_netRadiation
  expect_type(
    (netRadiation_test <- radiation_netRadiation(
      solarConstant_test, latrad_test, elevation_test, slorad_test, asprad_test,
      delta_test, vpa_test, tmin_test, tmax_test, R_s_test, alpha_test
    )),
    "double"
  )
  expect_error(
    radiation_netRadiation(
      "tururu", latrad_test, elevation_test, slorad_test, asprad_test,
      delta_test, vpa_test, tmin_test, tmax_test, R_s_test, alpha_test
    )
  )
  expect_error(
    radiation_netRadiation(
      c(solarConstant_test, solarConstant_test), latrad_test, elevation_test,
      slorad_test, asprad_test, delta_test, vpa_test, tmin_test, tmax_test,
      R_s_test, alpha_test
    )
  )
})

## Utils functions tests ####
test_that("utils c internals work as expected", {

  # data for utils test
  temperature_test <- 25
  Tmin_test <- 5
  Tmax_test <- 26
  RHmin_test <- 25
  RHmax_test <- 99
  elevation_test <- 900
  Patm_test <- 100

  # utils_saturationVP
  expect_type(
    (saturationVP_test <- utils_saturationVP(temperature_test)),
    "double"
  )
  expect_error(
    utils_saturationVP("tururu")
  )
  expect_error(
    utils_saturationVP(c(temperature_test, temperature_test))
  )

  # utils_averageDailyVP
  expect_type(
    (averageDailyVP_test <- utils_averageDailyVP(Tmin_test, Tmax_test, RHmin_test, RHmax_test)),
    "double"
  )
  expect_error(
    utils_averageDailyVP("tururu", Tmax_test, RHmin_test, RHmax_test)
  )
  expect_error(
    utils_averageDailyVP(c(Tmin_test, Tmin_test), Tmax_test, RHmin_test, RHmax_test)
  )

  # utils_atmosphericPressure
  expect_type(
    (atmosphericPressure_test <- utils_atmosphericPressure(elevation_test)),
    "double"
  )
  expect_error(
    utils_atmosphericPressure("tururu")
  )
  expect_error(
    utils_atmosphericPressure(c(elevation_test, elevation_test))
  )

  # utils_airDensity
  expect_type(
    (airDensity_test <- utils_airDensity(temperature_test, Patm_test)),
    "double"
  )
  expect_error(
    utils_airDensity("tururu")
  )
  expect_error(
    utils_airDensity(c(temperature_test, temperature_test), Patm_test)
  )

  # utils_averageDaylightTemperature
  expect_type(
    (averageDaylightTemperature_test <- utils_averageDaylightTemperature(Tmin_test, Tmax_test)),
    "double"
  )
  expect_error(
    utils_averageDaylightTemperature("tururu")
  )
  expect_error(
    utils_averageDaylightTemperature(c(Tmin_test, Tmin_test), Tmax_test)
  )

  # utils_latentHeatVaporisation
  expect_type(
    (latentHeatVaporisation_test <- utils_latentHeatVaporisation(temperature_test)),
    "double"
  )
  expect_error(
    utils_latentHeatVaporisation("tururu")
  )
  expect_error(
    utils_latentHeatVaporisation(c(temperature_test, temperature_test))
  )

  # utils_latentHeatVaporisationMol
  expect_type(
    (latentHeatVaporisationMol_test <- utils_latentHeatVaporisationMol(temperature_test)),
    "double"
  )
  expect_error(
    utils_latentHeatVaporisationMol("tururu")
  )
  expect_error(
    utils_latentHeatVaporisationMol(c(temperature_test, temperature_test))
  )

  # utils_psychrometricConstant
  expect_type(
    (psychrometricConstant_test <- utils_psychrometricConstant(temperature_test, Patm_test)),
    "double"
  )
  expect_error(
    utils_psychrometricConstant("tururu")
  )
  expect_error(
    utils_psychrometricConstant(c(temperature_test, temperature_test), Patm_test)
  )

  # utils_saturationVaporPressureCurveSlope
  expect_type(
    (saturationVaporPressureCurveSlope_test <- utils_saturationVaporPressureCurveSlope(temperature_test)),
    "double"
  )
  expect_error(
    utils_saturationVaporPressureCurveSlope("tururu")
  )
  expect_error(
    utils_saturationVaporPressureCurveSlope(c(temperature_test, temperature_test))
  )
})

## PET functions tests ####
test_that("PET c internals work as expected", {

  # data for PET test
  latrad_test <- 42 * (pi / 180)
  elevation_test <- 900
  slorad_test <- 2 * (pi / 180)
  asprad_test <- 270 * (pi / 180)
  J_test <- 2459695
  tmax_test <- 5 + 16
  tmin_test <- 5
  rhmin_test <- 10
  rhmax_test <- 99
  R_s_test <- 6
  u_test <- 5
  z_test <- 2
  z0_test <- 0.001
  alpha_test <- 0.08
  windfun_test <- "1956"
  rc_test <- 80
  Rn_test <- 25

  # penman
  expect_type(
    (penman_test <- penman(
      latrad_test, elevation_test, slorad_test, asprad_test, J_test, tmin_test,
      tmax_test, rhmin_test, rhmax_test, R_s_test, u_test, z_test, z0_test,
      alpha_test, windfun_test
    )),
    "double"
  )

  expect_error(
    penman(
      "latrad_test", elevation_test, slorad_test, asprad_test, J_test, tmin_test,
      tmax_test, rhmin_test, rhmax_test, R_s_test, u_test, z_test, z0_test,
      alpha_test, windfun_test
    )
  )
  expect_error(
    penman(
      c(latrad_test,latrad_test), elevation_test, slorad_test, asprad_test, J_test, tmin_test,
      tmax_test, rhmin_test, rhmax_test, R_s_test, u_test, z_test, z0_test,
      alpha_test, windfun_test
    )
  )
  # expect_error(
  #   penman(
  #     latrad_test, elevation_test, slorad_test, asprad_test, J_test, tmin_test,
  #     tmax_test, rhmin_test, rhmax_test, R_s_test, u_test, z_test, z0_test,
  #     alpha_test, "2022"
  #   )
  # )
  expect_error(
    penman(
      latrad_test, elevation_test, slorad_test, asprad_test, J_test, tmin_test,
      tmax_test, "rhmin_test", rhmax_test, R_s_test, u_test, z_test, z0_test,
      alpha_test, windfun_test
    )
  )
  expect_error(
    penman(
      latrad_test, elevation_test, slorad_test, asprad_test, J_test, tmin_test,
      tmax_test, c(rhmin_test, rhmin_test), rhmax_test, R_s_test, u_test, z_test, z0_test,
      alpha_test, windfun_test
    )
  )

  # penmanmonteith
  expect_type(
    (penmanmonteith_test <- penmanmonteith(
      rc_test, elevation_test, tmin_test, tmax_test, rhmin_test,
      rhmax_test, Rn_test, u_test
    )),
    "double"
  )

  expect_error(
    penmanmonteith(
      "rc_test", elevation_test, tmin_test, tmax_test, rhmin_test,
      rhmax_test, Rn_test, u_test
    )
  )
  expect_error(
    penmanmonteith(
      c(rc_test, rc_test), elevation_test, tmin_test, tmax_test, rhmin_test,
      rhmax_test, Rn_test, u_test
    )
  )
})

## Interpolation functions tests ####
test_that("interpolation c lowlevel internals work as expected", {

  # data for testing
  reference_data <-
    interpolate_data(
      points_to_interpolate_example, meteoland_interpolator_example,
      dates = as.Date("2022-04-01"), verbose = FALSE
    ) |>
    tidyr::unnest(cols = "interpolated_data")
  Xp <- as.numeric(sf::st_coordinates(points_to_interpolate_example)[,1])
  Yp <- as.numeric(sf::st_coordinates(points_to_interpolate_example)[,2])
  Zp <- points_to_interpolate_example$elevation
  X <- as.numeric(
    sf::st_coordinates(stars::st_get_dimension_values(meteoland_interpolator_example, "station"))[,1]
  )
  Y <- as.numeric(
    sf::st_coordinates(stars::st_get_dimension_values(meteoland_interpolator_example, "station"))[,2]
  )
  Z <- as.numeric(meteoland_interpolator_example[["elevation"]][1,])
  Temp <- as.numeric(meteoland_interpolator_example[["MinTemperature"]][1,])
  P <- as.numeric(meteoland_interpolator_example[["Precipitation"]][1,])
  Psmooth <- as.numeric(meteoland_interpolator_example[["SmoothedPrecipitation"]][1,])
  WS <- as.numeric(meteoland_interpolator_example[["WindSpeed"]][1,])
  WD <- as.numeric(meteoland_interpolator_example[["WindDirection"]][1,])
  iniRp <- get_interpolation_params(meteoland_interpolator_example)$initial_Rp
  alpha <- get_interpolation_params(meteoland_interpolator_example)$alpha_MinTemperature
  N <- get_interpolation_params(meteoland_interpolator_example)$N_MinTemperature
  alpha_event <- get_interpolation_params(meteoland_interpolator_example)$alpha_PrecipitationEvent
  N_event <- get_interpolation_params(meteoland_interpolator_example)$N_PrecipitationEvent
  alpha_amount <- get_interpolation_params(meteoland_interpolator_example)$alpha_PrecipitationAmount
  N_amount <- get_interpolation_params(meteoland_interpolator_example)$N_PrecipitationAmount
  alpha_wind <- get_interpolation_params(meteoland_interpolator_example)$alpha_Wind
  N_wind <- get_interpolation_params(meteoland_interpolator_example)$N_Wind
  iterations <- get_interpolation_params(meteoland_interpolator_example)$iterations
  popcrit <- get_interpolation_params(meteoland_interpolator_example)$pop_crit
  fmax <- get_interpolation_params(meteoland_interpolator_example)$f_max
  debug <- get_interpolation_params(meteoland_interpolator_example)$debug

  expect_type(
    (res_temperature_test <- interpolation_temperature(
      Xp, Yp, Zp,
      X[!is.na(Temp)], Y[!is.na(Temp)], Z[!is.na(Temp)],
      Temp[!is.na(Temp)],
      iniRp, alpha, N, iterations, debug
    )),
    "double"
  )
  expect_identical(
    res_temperature_test,
    reference_data$MinTemperature
  )

  expect_type(
    (res_precipitation_test <- interpolation_precipitation(
      Xp, Yp, Zp,
      X[!is.na(P)], Y[!is.na(P)], Z[!is.na(P)],
      P[!is.na(P)], Psmooth[!is.na(P)],
      iniRp, alpha_event, alpha_amount, N_event, N_amount,
      iterations, popcrit, fmax, debug
    )),
    "double"
  )
  expect_identical(
    res_precipitation_test,
    reference_data$Precipitation
  )

  # Wind test
  # directionsAvailable is FALSE because internally in .interpolateWindStationSeriesPoints, when
  # there is more than one missing WD this argument is set to FALSE (wind.cpp, line 236)
  expect_type(
    (res_wind_test <- interpolation_wind(
      Xp, Yp,
      WS[!is.na(WD)], WD[!is.na(WD)],
      X[!is.na(WD)], Y[!is.na(WD)],
      iniRp, alpha_wind, N_wind, iterations, directionsAvailable = FALSE
    )),
    "double"
  )
  expect_equal(res_wind_test[,1], reference_data$WindSpeed, tolerance = 0.001)
  expect_identical(res_wind_test[,2], reference_data$WindDirection)

  # skip("not implemented yet")
})
