# ---- Add to _targets.R ----------------------------------------------------------

# 1. Add these packages to your tar_option_set(packages = ...):
#    "dplyr", "tidyr", "tibble", "openxlsx"

# 2. Add these settings above the target list:
release_years         <- 2021:2024
release_revised_years <- 2021:2023
release_dir           <- "output/release"

# 3. Add these targets to the list, after mastersheet_output_regional:

  # ---- Release tables ----
  tar_target(release_obs, make_release_obs(mastersheet_output_regional)),

  tar_target(release_file_1_1,
             write_release_table(build_table_1_1(release_obs, release_years),
                                 release_dir),
             format = "file"),

  tar_target(release_file_1_2,
             write_release_table(build_table_1_2(release_obs, release_years,
                                                 release_revised_years),
                                 release_dir),
             format = "file"),

  tar_target(release_file_1_4,
             write_release_table(build_table_1_4(release_obs, release_years,
                                                 release_revised_years),
                                 release_dir),
             format = "file")
