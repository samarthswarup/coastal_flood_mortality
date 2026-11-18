# Step 8a — Random-forest cross-validation and Mundlak model fitting
#
# Reconstructs the retrospective Step 8 model-fitting sample, compares a
# cluster-aware regression forest with the primary negative-binomial model
# using held-out-country folds, and fits a negative-binomial correlated-
# random-effects (Mundlak) specification with country-mean covariate terms.
# Calculates model-fit statistics and joint country-mean tests.
#
# Requires: Step 8 predMod.3 and trendData RDS files; Step 7 event data
# when PREFER_STEP7_DATA = TRUE (the default).
# Outputs: Output/Diagnostics_RF_Mundlak/ (RF comparison tables and summaries,
# fitted Mundlak model, coefficient mapping and test statistics).
# Does not run future mortality projections.
# -----------------------------------------------------------------------------

# ------------------------- USER SETTINGS --------------------------------------
PROJECT_DIR <- getwd()
# Use the same project directory as Steps 8 and 9.
if (!dir.exists(file.path(PROJECT_DIR, "Intermediate outputs", "Output from Step 8")) &&
    dir.exists(file.path(PROJECT_DIR, "Claude", "Intermediate outputs", "Output from Step 8"))) {
  PROJECT_DIR <- file.path(PROJECT_DIR, "Claude")
}
PROJECT_DIR <- normalizePath(PROJECT_DIR, mustWork = TRUE)

STEP8_DATE  <- "2026-11-13"  # matches Step 8 saved models
STEP7_DATE  <- "2026-07-05"  # matches Step 8 input
PREFER_STEP7_DATA <- TRUE     # most faithful reconstruction of Step 8 fit

RUN_RF <- TRUE
RUN_MUNDLAK <- TRUE
CV_FOLDS <- 5L
RF_TREES <- 2000L
SEED <- 256L

OUT_DIR <- file.path(PROJECT_DIR, "Output", "Diagnostics_RF_Mundlak")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ------------------------- DEPENDENCIES ---------------------------------------
needed <- c("glmmTMB", "splines2", if (RUN_RF) "grf", if (PREFER_STEP7_DATA) "readxl")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Missing packages: ", paste(missing, collapse = ", "),
       ". Install with install.packages(c(",
       paste(sprintf('"%s"', missing), collapse = ", "), ")).")
}

# ------------------------- FILE DISCOVERY -------------------------------------
step8_dir <- file.path(PROJECT_DIR, "Intermediate outputs", "Output from Step 8")
step7_dir <- file.path(PROJECT_DIR, "Intermediate outputs", "Output from Step 7")

latest_file <- function(files, kind) {
  if (!length(files)) stop("No matching ", kind, " files found. Check PROJECT_DIR or date.")
  if (length(files) > 1L) {
    files <- files[order(file.info(files)$mtime, decreasing = TRUE)]
    message("Several ", kind, " files found; selecting newest by modification time: ",
            basename(files[[1]]), ". Set STEP8_DATE/STEP7_DATE to override.")
  }
  files[[1]]
}

if (!is.na(STEP8_DATE)) {
  model_path <- file.path(step8_dir, paste0("predMod.3 ", STEP8_DATE, ".RDS"))
  if (!file.exists(model_path)) stop("Model file not found: ", model_path)
} else {
  model_path <- latest_file(
    list.files(step8_dir, pattern = "^predMod\\.3 .*\\.RDS$", full.names = TRUE),
    "Step 8 predMod.3")
}
step8_date <- sub("^predMod\\.3 (.*)\\.RDS$", "\\1", basename(model_path))
trend_path <- file.path(step8_dir, paste0("trendData ", step8_date, ".RDS"))
if (!file.exists(trend_path)) stop("Matched Step 8 trendData not found: ", trend_path)

predMod.3 <- readRDS(model_path)
trendData_saved <- readRDS(trend_path)
if (!inherits(predMod.3, "glmmTMB")) stop("The saved model is not glmmTMB.")

if (!is.na(STEP7_DATE)) {
  step7_path <- file.path(step7_dir, paste0(
    "EMDAT events with exposed pops, HDI GNI and prev events ", STEP7_DATE, ".xlsx"))
  if (!file.exists(step7_path)) stop("Step 7 file not found: ", step7_path)
} else {
  step7_candidates <- if (dir.exists(step7_dir)) list.files(
    step7_dir,
    pattern = "^EMDAT events with exposed pops, HDI GNI and prev events .*\\.xlsx$",
    full.names = TRUE) else character()
  step7_path <- if (length(step7_candidates)) latest_file(step7_candidates, "Step 7") else NA_character_
}

cat("\nSaved model: ", model_path, "\nSaved trendData: ", trend_path,
    "\nStep 7 input: ", ifelse(is.na(step7_path), "unavailable", step7_path), "\n", sep = "")

# ------------------------- RECONSTRUCT FITTING DATA ---------------------------
# Step 8 uses df.gni, recomputes the previous-five-year event count, and
# builds a three-column B-spline basis from Start.Year - 1990.

if (PREFER_STEP7_DATA && !is.na(step7_path)) {
  raw <- as.data.frame(readxl::read_excel(step7_path))
  source_description <- "Step 7 final Excel (faithful Step 8 reconstruction)"
  if (!all(c("Start.Year", "ISO", "gni.group") %in% names(raw))) {
    stop("Step 7 file is missing Start.Year, ISO, or gni.group.")
  }
  raw$no.events.5.years <- NA_integer_
  for (i in seq_len(nrow(raw))) {
    yr <- raw$Start.Year[i]
    if (!is.na(yr) && yr >= 1990 && yr <= 2024) {
      raw$no.events.5.years[i] <- sum(
        (raw$Start.Year %in% ((yr - 5):yr)) & (raw$ISO == raw$ISO[i]))
    }
  }
  raw <- raw[!is.na(raw$Start.Year) & raw$Start.Year > 1989, , drop = FALSE]
  raw <- raw[!is.na(raw$gni.group), , drop = FALSE]
  raw$Year <- raw$Start.Year - 1990
  raw$Country_Code <- raw$SLIIDERS.country.code.flat
  raw$expPop100 <- raw$pop.exposed.local.else.adm1.else.national.100yr
  raw$spline_year <- splines2::bSpline(raw$Year, df = 3)
} else {
  message("Using saved Step 8 trendData as fallback (rather than Step 7 Excel).")
  raw <- as.data.frame(trendData_saved)
  source_description <- "Step 8 trendData RDS (fallback reconstruction)"
  if ("gni.group" %in% names(raw)) raw <- raw[!is.na(raw$gni.group), , drop = FALSE]
  raw <- raw[!is.na(raw$Start.Year) & raw$Start.Year > 1989, , drop = FALSE]
  if (!"Country_Code" %in% names(raw)) raw$Country_Code <- raw$SLIIDERS.country.code.flat
  raw$Year <- raw$Start.Year - 1990
  raw$expPop100 <- raw$pop.exposed.local.else.adm1.else.national.100yr
  raw$spline_year <- splines2::bSpline(raw$Year, df = 3)
}

req <- c("total.deaths.na.as.0", "hdi", "Declaration", "no.events.5.years",
         "pop.total.adm0", "Disaster.Subtype", "Country_Code", "expPop100",
         "spline_year")
misscols <- setdiff(req, names(raw))
if (length(misscols)) stop("Input data missing: ", paste(misscols, collapse = ", "))

# Only variables used by model 3: exclude observations omitted by glmmTMB.
baseKeep <- raw[, req, drop = FALSE]
baseKeep$log_expPop100 <- log(baseKeep$expPop100)
baseKeep$Country_Code <- factor(baseKeep$Country_Code)
baseKeep$Declaration <- factor(baseKeep$Declaration)
baseKeep$Disaster.Subtype <- factor(baseKeep$Disaster.Subtype)
baseKeep <- baseKeep[
  !is.na(baseKeep$expPop100) & baseKeep$expPop100 > 0 &
    !is.na(baseKeep$pop.total.adm0) & baseKeep$pop.total.adm0 > 0, , drop = FALSE]
baseKeep <- baseKeep[stats::complete.cases(baseKeep), , drop = FALSE]
baseKeep <- droplevels(baseKeep)
row.names(baseKeep) <- NULL

n_model <- stats::nobs(predMod.3)
cat("Reconstructed rows: ", nrow(baseKeep), "; saved-model nobs: ", n_model,
    "; countries: ", length(unique(baseKeep$Country_Code)), "\n", sep = "")
if (nrow(baseKeep) != n_model) {
  stop("FITTING-SAMPLE MISMATCH: reconstructed data contain ", nrow(baseKeep),
       " complete rows but saved model used ", n_model, ".\n",
       "Set STEP7_DATE to the Step 7 source used in this Step 8 run.\n",
       "Check fitting sample before using the diagnostics.")
}
if (length(unique(baseKeep$Country_Code)) < CV_FOLDS && RUN_RF) {
  stop("Fewer countries than CV folds.")
}

# Check the data predict correctly through the original fitted model.
check_prediction <- tryCatch(predict(predMod.3, newdata = baseKeep[1L, , drop = FALSE],
                                      type = "response", allow.new.levels = TRUE),
                             error = function(e) e)
if (inherits(check_prediction, "error") || !is.finite(check_prediction[1L])) {
  stop("Saved model cannot predict on reconstructed baseKeep. Check matching input dates.")
}

# Match the original Step 8 model formula.
nb_formula <- total.deaths.na.as.0 ~ offset(log_expPop100) +
  log(pop.total.adm0) + spline_year + hdi + Declaration +
  no.events.5.years + Disaster.Subtype + (1 | Country_Code)

# Stronger verification than matching N alone: refit the reference model on
# reconstructed data and compare its log-likelihood with the saved model.
# If a wrong Step 7 vintage happens to have the same number of events,
# this generally catches the mismatch before the diagnostics are run.
cat("Checking saved model against reconstructed fitting rows...\n")
reference_check <- glmmTMB::glmmTMB(nb_formula, data = baseKeep,
                                    family = glmmTMB::nbinom2())
ll_saved <- as.numeric(stats::logLik(predMod.3))
ll_rebuilt <- as.numeric(stats::logLik(reference_check))
ll_difference <- abs(ll_saved - ll_rebuilt)
cat("Saved logLik:", ll_saved, "; reconstructed logLik:", ll_rebuilt,
    "; absolute difference:", ll_difference, "\n")
if (!is.finite(ll_difference) || ll_difference > 0.1) {
  stop("Reconstructed model differs materially from saved predMod.3 (logLik difference=",
       ll_difference, "). Check STEP7_DATE against the Step 8 run.\n",
       "Diagnostics were NOT calculated on potentially mismatched data.")
}
rm(reference_check)

writeLines(c(
  paste("Model:", model_path), paste("Step 8 data:", trend_path),
  paste("Step 7 data:", ifelse(is.na(step7_path), "unavailable", step7_path)),
  paste("Reconstruction:", source_description),
  paste("N events:", nrow(baseKeep)),
  paste("N countries:", length(unique(baseKeep$Country_Code))),
  paste("glmmTMB:", as.character(utils::packageVersion("glmmTMB"))),
  paste("Saved vs reconstructed logLik difference:", ll_difference),
  paste("Date executed:", Sys.time())
), file.path(OUT_DIR, "input_manifest.txt"))

# ------------------------- RANDOM FOREST --------------------------------------
# Same event outcome and predictors; the RF includes log(exposure) as an
# ordinary covariate because it has no offset facility like glmmTMB.
# Uses held-out-country cross-validation for both models.
if (RUN_RF) {
  cat("\n=== RF vs NB: held-out-country cross-validation ===\n")
  Y <- as.numeric(baseKeep$total.deaths.na.as.0)
  Xfixed <- stats::model.matrix(
    ~ log(pop.total.adm0) + spline_year + hdi + Declaration +
      no.events.5.years + Disaster.Subtype,
    data = baseKeep)
  X <- cbind(log_exposed_population = baseKeep$log_expPop100,
             Xfixed[, colnames(Xfixed) != "(Intercept)", drop = FALSE])
  storage.mode(X) <- "double"
  if (any(!is.finite(X))) stop("Non-finite values in RF predictors.")

  # Build folds by country, balancing number of held-out events across folds.
  set.seed(SEED)
  counts <- table(as.character(baseKeep$Country_Code))
  country_order <- sample(names(counts))
  country_order <- country_order[order(as.integer(counts[country_order]), decreasing = TRUE)]
  load_per_fold <- numeric(CV_FOLDS)
  country_fold <- setNames(integer(length(country_order)), country_order)
  for (cc in country_order) {
    f <- which.min(load_per_fold)
    country_fold[[cc]] <- f
    load_per_fold[[f]] <- load_per_fold[[f]] + as.integer(counts[[cc]])
  }
  fold_id <- unname(country_fold[as.character(baseKeep$Country_Code)])

  nb_hat <- rep(NA_real_, length(Y))
  rf_hat <- rep(NA_real_, length(Y))
  for (f in seq_len(CV_FOLDS)) {
    idx_train <- which(fold_id != f)
    idx_test  <- which(fold_id == f)
    cat("Fold ", f, "/", CV_FOLDS, ": ", length(idx_train),
        " training events, ", length(idx_test), " held-out events; ",
        length(unique(baseKeep$Country_Code[idx_test])), " held-out countries\n", sep = "")
    train <- baseKeep[idx_train, , drop = FALSE]
    test  <- baseKeep[idx_test, , drop = FALSE]

    # Refit NB on training countries; predict new countries at population level.
    nb_fold <- glmmTMB::glmmTMB(nb_formula, data = train, family = glmmTMB::nbinom2())
    if (nb_fold$fit$convergence != 0L || !isTRUE(nb_fold$sdr$pdHess)) {
      warning("NB fold ", f, " had optimizer/Hessian warnings. Inspect fold outputs.")
    }
    nb_hat[idx_test] <- as.numeric(predict(nb_fold, newdata = test,
       type = "response", re.form = NA, allow.new.levels = TRUE))

    # grf's clusters argument samples at the country-cluster level.
    rf_fold <- grf::regression_forest(
      X = X[idx_train, , drop = FALSE], Y = Y[idx_train],
      clusters = as.factor(as.character(baseKeep$Country_Code[idx_train])),
      num.trees = RF_TREES, seed = SEED + f)
    rf_hat[idx_test] <- as.numeric(predict(rf_fold,
       newdata = X[idx_test, , drop = FALSE])$predictions)
    rm(nb_fold, rf_fold)
  }

  if (any(!is.finite(nb_hat)) || any(!is.finite(rf_hat))) {
    stop("Some cross-validated predictions are not finite.")
  }
  eval <- data.frame(
    Country_Code = as.character(baseKeep$Country_Code), fold = fold_id,
    deaths = Y, NB_pred = nb_hat, RF_pred = rf_hat,
    NB_abs_error = abs(Y - nb_hat), RF_abs_error = abs(Y - rf_hat))
  utils::write.csv(eval, file.path(OUT_DIR, "RF_NB_grouped_CV_event_predictions.csv"), row.names = FALSE)

  # Country-based paired comparison: one independent unit per country.
  by_country <- do.call(rbind, lapply(split(eval, eval$Country_Code), function(d) {
    data.frame(Country_Code = d$Country_Code[1L], nEvents = nrow(d),
               NB_MedAE = median(d$NB_abs_error),
               RF_MedAE = median(d$RF_abs_error),
               NB_MeanAE = mean(d$NB_abs_error),
               RF_MeanAE = mean(d$RF_abs_error))
  }))
  row.names(by_country) <- NULL
  utils::write.csv(by_country, file.path(OUT_DIR, "RF_NB_grouped_CV_country_errors.csv"), row.names = FALSE)

  country_wilcox <- suppressWarnings(stats::wilcox.test(
    by_country$NB_MedAE, by_country$RF_MedAE,
    paired = TRUE, exact = FALSE))
  report <- c(
    "RF vs negative-binomial GLMM: 5-fold grouped cross-validation",
    "Test countries are excluded from BOTH training sets.",
    "NB uses zero random effects when predicting unseen countries.",
    "grf samples clusters via clusters=Country_Code.",
    "RF includes log(exposed population) as a predictor (NB uses it as offset).",
    paste("N events:", nrow(eval), "; N countries:", nrow(by_country)),
    paste("Pooled event Median Absolute Error, NB:", median(eval$NB_abs_error)),
    paste("Pooled event Median Absolute Error, RF:", median(eval$RF_abs_error)),
    paste("Pooled event Mean Absolute Error, NB:", mean(eval$NB_abs_error)),
    paste("Pooled event Mean Absolute Error, RF:", mean(eval$RF_abs_error)),
    paste("Unweighted mean of country-specific MedAE, NB:", mean(by_country$NB_MedAE)),
    paste("Unweighted mean of country-specific MedAE, RF:", mean(by_country$RF_MedAE)),
    paste("Country-level paired Wilcoxon p:", country_wilcox$p.value),
    paste("Number of cross-validation folds:", CV_FOLDS)
  )
  cat(paste(report, collapse = "\n"), "\n")
  writeLines(report, file.path(OUT_DIR, "RF_NB_grouped_CV_summary.txt"))
}

# ------------------------- MUNDLAK / CORRELATED RANDOM EFFECTS --------------
if (RUN_MUNDLAK) {
  cat("\n=== Mundlak correlated random effects within NB glmmTMB ===\n")
  # Create within-country means of the regression design-matrix terms,
  # including log national population and categorical predictor dummies.
  M <- stats::model.matrix(
    ~ log(pop.total.adm0) + spline_year + hdi + Declaration +
      no.events.5.years + Disaster.Subtype,
    data = baseKeep)
  M <- M[, colnames(M) != "(Intercept)", drop = FALSE]
  country <- as.character(baseKeep$Country_Code)
  group_n <- table(country)
  mean_by_country <- rowsum(M, group = country, reorder = TRUE)
  mean_by_country <- sweep(mean_by_country, 1L,
                           as.numeric(group_n[rownames(mean_by_country)]), "/")
  means <- mean_by_country[country, , drop = FALSE]
  # Remove globally constant mean columns to avoid obvious singularities.
  varying <- apply(means, 2L, function(z) is.finite(stats::sd(z)) && stats::sd(z) > 1e-10)
  means <- means[, varying, drop = FALSE]
  if (!ncol(means)) stop("All candidate Mundlak means are constant.")
  mean_names <- sprintf("MUND_%02d", seq_len(ncol(means)))
  label_map <- data.frame(term = mean_names,
                          original_covariate = colnames(means),
                          stringsAsFactors = FALSE)
  colnames(means) <- mean_names
  for (z in seq_along(mean_names)) baseKeep[[mean_names[[z]]]] <- means[, z]
  utils::write.csv(label_map, file.path(OUT_DIR, "Mundlak_mean_term_mapping.csv"), row.names = FALSE)

  cre_formula <- stats::update(nb_formula,
    paste(". ~ . +", paste(mean_names, collapse = " + ")))
  cre_fit <- glmmTMB::glmmTMB(cre_formula, data = baseKeep,
                                family = glmmTMB::nbinom2())
  convergence_ok <- cre_fit$fit$convergence == 0L && isTRUE(cre_fit$sdr$pdHess)
  if (!convergence_ok) {
    warning("Mundlak-augmented NB model has convergence/Hessian warnings.")
  }

  # Joint likelihood-ratio test. Null = all country-mean terms jointly zero.
  ll0 <- stats::logLik(predMod.3)
  ll1 <- stats::logLik(cre_fit)
  df_test <- as.integer(attr(ll1, "df") - attr(ll0, "df"))
  LR <- 2 * (as.numeric(ll1) - as.numeric(ll0))
  p_LR <- if (df_test > 0 && LR >= 0) stats::pchisq(LR, df = df_test,
                                                   lower.tail = FALSE) else NA_real_

  # Joint Wald statistic from the conditional-model coefficient covariance.
  beta <- glmmTMB::fixef(cre_fit)$cond
  V <- as.matrix(stats::vcov(cre_fit)$cond)
  test_names <- intersect(mean_names, names(beta))
  b <- beta[test_names]
  Vb <- V[test_names, test_names, drop = FALSE]
  wald <- tryCatch(as.numeric(crossprod(b, solve(Vb, b))),
                   error = function(e) NA_real_)
  p_wald <- if (is.finite(wald)) stats::pchisq(wald,
                  df = length(test_names), lower.tail = FALSE) else NA_real_

  report <- c(
    "Mundlak / correlated random effects test in negative binomial glmmTMB",
    "The saved predMod.3 is the unaugmented reference model.",
    "Country means were added for fixed predictors (not exposure offset).",
    paste("N events:", nrow(baseKeep)),
    paste("N countries:", length(unique(baseKeep$Country_Code))),
    paste("Number of tested mean terms:", length(test_names)),
    paste("Augmented-model optimizer convergence code:", cre_fit$fit$convergence),
    paste("Augmented-model positive definite Hessian:", isTRUE(cre_fit$sdr$pdHess)),
    paste("Likelihood ratio statistic:", LR),
    paste("Likelihood ratio df:", df_test),
    paste("Likelihood ratio p:", p_LR),
    paste("Joint Wald chi-square:", wald),
    paste("Joint Wald df:", length(test_names)),
    paste("Joint Wald p:", p_wald),
    "Model family: negative binomial (nbinom2)."
  )
  cat(paste(report, collapse = "\n"), "\n")
  writeLines(c(report, "\nAugmented model summary:",
               capture.output(summary(cre_fit))),
             file.path(OUT_DIR, "Mundlak_NB_summary.txt"))
  saveRDS(cre_fit, file.path(OUT_DIR, "Mundlak_NB_augmented_model.rds"))
}

cat("\nDiagnostics finished. Outputs saved in:\n", OUT_DIR, "\n", sep = "")
