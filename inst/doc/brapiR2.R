## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
con <- brapiR2::brapi_connection("https://test-server.brapi.org")
server_up <- isTRUE(tryCatch(
  brapiR2::brapi_ping(con),
  error = function(e) FALSE
))

## ----server-down-notice, eval = !server_up, echo = FALSE, results = "asis"----
# cat(
#   "> **Note:** the public BrAPI test server",
#   "(`https://test-server.brapi.org`) was unreachable when this vignette",
#   "was built, so the live output below was skipped. The code and its",
#   "expected shape are still shown."
# )

## ----install, eval = FALSE----------------------------------------------------
# # Not run here: this would reinstall the package while building its own
# # documentation.
# install.packages(
#   "brapiR2",
#   repos = c("https://ropensci.r-universe.dev", "https://cloud.r-project.org")
# )

## ----connect, eval = server_up------------------------------------------------
library(brapiR2)

con <- brapi_connection("https://test-server.brapi.org")
con

## ----explore, eval = server_up------------------------------------------------
library(dplyr)

# List all breeding programs
programs <- brapi_programs(con)
programs

# List trials in the first program
trials <- brapi_trials(con, programDbId = programs$programDbId[1])
trials

# List studies within that trial
studies <- brapi_studies(con, trialDbId = trials$trialDbId[1])
studies

## ----pheno, eval = server_up--------------------------------------------------
# Get analysis-ready wide format: one row per plot, one column per trait
data <- brapi_study_data(con, studies$studyDbId[1], fallback = TRUE)
data

## ----pheno-summary, eval = server_up------------------------------------------
if (nrow(data) > 0) {
  trait_cols <- setdiff(
    names(data),
    c(
      "observationUnitDbId", "observationUnitName",
      "germplasmDbId", "germplasmName", "studyDbId", "studyName"
    )
  )
  # Some observation units have more than one recorded value for the same
  # trait (repeated measurements), which brapi_study_data() keeps as a
  # list-column. Unnest those into one row per observation before
  # summarising, so mean() sees plain numbers either way.
  data |>
    tidyr::unnest_longer(dplyr::any_of(trait_cols)) |>
    mutate(across(all_of(trait_cols), as.numeric)) |>
    summarise(across(all_of(trait_cols), \(x) mean(x, na.rm = TRUE)))
} else {
  cat("No observations available for this study on the public test server.\n")
}

## ----geno, eval = server_up---------------------------------------------------
# List available variant sets (genotyping datasets)
vsets <- brapi_variant_sets(con)
vsets

vs_id <- vsets$variantSetDbId[1]

## ----geno-map, eval = server_up-----------------------------------------------
# Positions for every variant in the set, wherever they have been placed
markers <- brapi_get_marker_map(con, variantSetDbId = vs_id)
markers

## ----geno-map-by-id, eval = server_up-----------------------------------------
maps <- brapi_maps(con)
maps[, c("mapDbId", "mapName", "type", "unit")]

## ----geno-map-by-id-2, eval = server_up---------------------------------------
brapi_get_marker_map(con, mapDbId = maps$mapDbId[1])

## ----geno-dosage, eval = server_up--------------------------------------------
# Get dosage matrix for genomic selection (samples x markers, values 0/1/2)
dosage <- brapi_get_dosage_matrix(con, vs_id)
dim(dosage)
dosage[seq_len(min(3, nrow(dosage))), seq_len(min(5, ncol(dosage)))]

## ----auth, eval = FALSE-------------------------------------------------------
# # Username/password login (tested on Breedbase)
# con <- brapi_connection("https://my-breedbase.org")
# con <- brapi_login(con, "username", "password")
# 
# # OAuth 2.0 client-credentials grant (not yet tested against a
# # production BrAPI server)
# con <- brapi_login_oauth2(
#   con,
#   client_id     = "my_id",
#   client_secret = "my_secret",
#   access_url    = "https://auth.example.org/token"
# )
# 
# # Bearer token (any server that issues one)
# con <- brapi_set_token(con, Sys.getenv("BRAPI_TOKEN"))

## ----renviron, eval = FALSE---------------------------------------------------
# con <- brapi_connection("https://my-breedbase.org")
# con <- brapi_login(
#   con, Sys.getenv("BRAPI_USERNAME"), Sys.getenv("BRAPI_PASSWORD")
# )

## ----keyring, eval = FALSE----------------------------------------------------
# # Run once, interactively - prompts for the password and stores it
# keyring::key_set("brapiR2_my-breedbase", username = "my_username")
# 
# # In scripts, from then on:
# con <- brapi_connection("https://my-breedbase.org")
# con <- brapi_login(
#   con,
#   username = "my_username",
#   password = keyring::key_get("brapiR2_my-breedbase", username = "my_username")
# )

## ----performance, eval = server_up--------------------------------------------
# Enable caching — repeated calls within the TTL return instantly
cache_dir <- tempfile("brapi_cache_")
dir.create(cache_dir)
perf_con <- brapi_cache_enable(con, ttl = 3600, dir = cache_dir)

# First call: hits the server
invisible(brapi_programs(perf_con))

# Second call: reads from disk (sub-millisecond)
brapi_programs(perf_con)

# Clear all cached files
brapi_cache_clear(perf_con)

## ----parallel, eval = server_up && requireNamespace("furrr", quietly = TRUE) && requireNamespace("future", quietly = TRUE)----
future::plan(future::multisession, workers = 2)

# Fetch study data from multiple studies in parallel (all studies on the
# server, not just the one attached to the trial filtered above)
study_ids <- brapi_studies(con)$studyDbId
all_data <- brapi_fetch_parallel(
  perf_con, brapi_study_data, study_ids, fallback = TRUE
)
all_data

## ----cleanup-parallel, eval = server_up && requireNamespace("future", quietly = TRUE)----
future::plan(future::sequential)

## ----qbms-example, eval = FALSE-----------------------------------------------
# library(QBMS)
# 
# set_crop("Wheat")
# login_bms("https://my-bms.org", "user", "pass")
# 
# list_programs()
# set_program("Wheat Breeding")
# 
# list_trials()
# set_trial("Yield Trial 2022")
# 
# list_studies()
# set_study("Ithaca 2022")
# 
# data <- get_study_data()

## ----brapiR2-example, eval = FALSE--------------------------------------------
# library(brapiR2)
# library(dplyr)
# 
# con <- brapi_connection("https://my-bms.org") |>
#   brapi_login("user", "pass")
# 
# data <- brapi_programs(con) |>
#   filter(programName == "Wheat Breeding") |>
#   pull(programDbId) |>
#   (\(pid) brapi_trials(con, programDbId = pid))() |>
#   filter(trialName == "Yield Trial 2022") |>
#   pull(trialDbId) |>
#   (\(tid) brapi_studies(con, trialDbId = tid))() |>
#   filter(studyName == "Ithaca 2022") |>
#   pull(studyDbId) |>
#   brapi_study_data(con = con)

