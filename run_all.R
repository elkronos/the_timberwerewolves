#!/usr/bin/env Rscript
# Runs the whole analysis and rebuilds the site.
#
#   Rscript run_all.R                  every step
#   Rscript run_all.R figures site     only the steps named
#
# Steps, in order: data, models, figures, site. Each step saves what the next
# one needs in cache/, so a later step can be rerun alone.

# Figure labels and the page use a real minus sign and en dashes, which need a
# UTF-8 session. Almost every R session is one already.
if (!isTRUE(l10n_info()[["UTF-8"]])) {
  invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8")))
}

needed  <- c("lme4", "Matrix", "suncalc", "jsonlite", "ggplot2", "ragg")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Install these packages first: install.packages(c(",
       paste0('"', missing, '"', collapse = ", "), "))", call. = FALSE)
}

for (f in c("config", "data", "moon", "models", "placebo", "breakdown", "analysis",
            "theme_moon", "figures", "site")) {
  source(file.path("R", paste0(f, ".R")))
}

steps <- commandArgs(trailingOnly = TRUE)
all_steps <- c("data", "models", "figures", "site")
if (!length(steps)) steps <- all_steps
if (length(setdiff(steps, all_steps))) {
  stop("Unknown step. Choose from: ", paste(all_steps, collapse = ", "), call. = FALSE)
}

dir.create(cfg$dir_cache, showWarnings = FALSE)
cache <- function(name) file.path(cfg$dir_cache, paste0(name, ".rds"))
need_cache <- function(name, made_by) {
  if (!file.exists(cache(name))) {
    stop("Run the '", made_by, "' step first: Rscript run_all.R ", made_by, call. = FALSE)
  }
  readRDS(cache(name))
}

if ("data" %in% steps) {
  tick("Reading games and computing the moon at each tip-off")
  saveRDS(build_tables(cfg), cache("data"))
  saveRDS(build_tables(cfg, cfg$second_sample), cache("second_sample"))
  saveRDS(build_tables(cfg, cfg$current_season), cache("this_season"))
}

if ("models" %in% steps) {
  out <- run_analysis(need_cache("data", "data"), cfg,
                      second = need_cache("second_sample", "data"),
                      this_season = need_cache("this_season", "data"))
  write_results(out$results, cfg, out$w)
  saveRDS(out, cache("analysis"))
}

if ("figures" %in% steps) {
  tick("Drawing figures")
  make_figures(need_cache("analysis", "models"), cfg)
}

if ("site" %in% steps) {
  tick("Building the site")
  build_site(need_cache("analysis", "models"), cfg)
}

tick("Done")
