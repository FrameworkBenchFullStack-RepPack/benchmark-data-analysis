CSV_DIR <- "benchmark-data"
ENERGY_COL <- "Combined Energy Average (J)"
WILCOX_ALPHA <- 0.05
MIN_SEGMENT <- 3

wilcoxon_binary_seg <- function(s, e, data, alpha_corrected, out_cps) {
  if ((e - s + 1) < MIN_SEGMENT + 1) {
    return(out_cps)
  }
  
  min_p <- Inf
  best_m <- -1
  
  for (m in s:(e - 1)) {
    left_segment <- data[s:m]
    right_segment <- data[(m + 1):e]
    
    # Run Mann-Whitney
    test_res <- suppressWarnings(
      wilcox.test(left_segment, right_segment, alternative = "two.sided", exact = FALSE)
    )
    
    if (!is.na(test_res$p.value) && test_res$p.value < min_p) {
      min_p <- test_res$p.value
      best_m <- m
    }
  }
  
  if (best_m == -1 || min_p >= alpha_corrected) {
    return(out_cps)
  }
  
  out_cps <- c(out_cps, best_m)
  
  out_cps <- wilcoxon_binary_seg(s, best_m, data, alpha_corrected, out_cps)
  out_cps <- wilcoxon_binary_seg(best_m + 1, e, data, alpha_corrected, out_cps)
  
  return(out_cps)
}

wilcoxon_binary <- function(data) {
  n <- length(data)
  if (n < 4) return(numeric(0))
  
  alpha_corrected <- WILCOX_ALPHA / (n - 1)
  changepoints <- wilcoxon_binary_seg(1, n, data, alpha_corrected, numeric(0))
  
  return(sort(unique(changepoints)))
}

# Reccommendations
get_warmup_rounds <- function(changepoints, n_rounds) {
  if (length(changepoints) == 0) return(0)
  
  candidates <- sort(changepoints, decreasing = TRUE)
  for (cp in candidates) {
    if ((n_rounds - cp) >= MIN_SEGMENT) {
      return(cp)
    }
  }
  return(0)
}

# Format

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  csv_dir <- CSV_DIR
  energy_col <- ENERGY_COL
  
  if (length(args) > 0) {
    i <- 1
    while (i <= length(args)) {
      if (args[i] == "--csv-dir" && i < length(args)) {
        csv_dir <- args[i + 1]
        i <- i + 2
      } else if (args[i] == "--column" && i < length(args)) {
        energy_col <- args[i + 1]
        i <- i + 2
      } else {
        stop(sprintf("Unknown argument: %s", args[i]))
      }
    }
  }
  
  # Read csv
  files <- list.files(path = csv_dir, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) {
    stop(sprintf("No CSV files found in '%s'.", csv_dir))
  }
  
  results <- list()
  
  for (f in files) {
    scenario <- tools::file_path_sans_ext(basename(f))
    df <- read.csv(f, check.names = FALSE)
    
    if (!energy_col %in% colnames(df)) {
      warning(sprintf("Column '%s' not found in %s. Skipping.", energy_col, scenario))
      next
    }
    
    frameworks <- unique(df$Framework)
    for (fw in frameworks) {
      df_fw <- df[df$Framework == fw, ]
      df_fw <- df_fw[order(df_fw$Round), ]
      
      values <- as.numeric(df_fw[[energy_col]])
      
      med <- median(values)
      mad_val <- mad(values, constant = 1.4826)
      if (mad_val > 1e-15) {
        z_scores <- 0.6745 * (values - med) / mad_val
        outliers_idx <- which(abs(z_scores) > 3.5)
        
        if (length(outliers_idx) > 0) {
          cat(sprintf("  [!] %s / %s: %d outlier(s) (MAD z > 3.5)\n", scenario, fw, length(outliers_idx)))
          for (idx in outliers_idx) {
            cat(sprintf("    round %d: %.4f J (z=%.2f)\n", idx, values[idx], z_scores[idx]))
          }
        }
      }
      
      if (length(values) < 4) {
        cat(sprintf("  [!] %s / %s: only %d rounds — skipping\n", scenario, fw, length(values)))
        next
      }
      
      cps <- wilcoxon_binary(values)
      results[[length(results) + 1]] <- list(
        scenario = scenario,
        framework = fw,
        values = values,
        changepoints = cps
      )
    }
  }
  
  # Format output
  sep <- paste0(rep("─", 120), collapse = "")
  cat("\nWilcoxon Rank-Sum Binary Segmentation — Warmup Detection Results\n")
  cat(paste0(rep("=", 120), collapse = ""), "\n\n")
  cat("Summary\n", sep, "\n", sep = "")
  
  # Print headers
  cat(sprintf("%-20s │ %-15s │ %6s │ %9s │ %10s │ %10s │ %s\n", 
              "scenario", "framework", "wWilc", "recommend", "stableMean", "stableSD", "changepoints"))
  cat(sep, "\n")
  
  # Print rows
  for (res in results) {
    n <- length(res$values)
    rec <- get_warmup_rounds(res$changepoints, n)
    
    stable_vals <- res$values[(rec + 1):n]
    sMean <- ifelse(length(stable_vals) > 0, mean(stable_vals), NaN)
    sSD <- ifelse(length(stable_vals) > 1, sd(stable_vals), 0)
    cp_str <- ifelse(length(res$changepoints) > 0, paste(res$changepoints, collapse = ", "), "—")
    
    cat(sprintf("%-20s │ %-15s │ %6d │ %9d │ %10.4f │ %10.4f │ %s\n",
                res$scenario, res$framework, rec, rec, sMean, sSD, cp_str))
  }
  
  cat(sep, "\n\n")
  cat(sprintf("%20s = Wilcoxon rank-sum + binary seg., α = 0.05 (Bonferroni-corrected)\n", "wWilc"))
  cat(sprintf("%20s = recommended warmup (last valid changepoint)\n", "recommend"))
  cat(sprintf("%20s = Combined Energy (J) after recommended warmup\n", "stableMean/SD"))
  cat("Changepoints are 1-based round numbers.\n")
  
  # Print Detailed Profiles
  cat("\n", sep, "\n\nDetailed Changepoint Profiles\n\n", sep = "")
  
  for (res in results) {
    label <- sprintf("%s  ·  %s", res$framework, res$scenario)
    cat(sprintf("  %s\n", label))
    cat("  ", paste0(rep("─", max(nchar(label) + 2, 40)), collapse = ""), "\n", sep = "")
    
    cat("  Round  Energy(J)  WIL\n")
    
    for (i in seq_along(res$values)) {
      is_cp <- if (i %in% res$changepoints) " ●" else "  "
      cat(sprintf("  %5d  %9.4f  %s\n", i, res$values[i], is_cp))
    }
    
    n <- length(res$values)
    rec <- get_warmup_rounds(res$changepoints, n)
    stable_vals <- res$values[(rec + 1):n]
    
    cat(sprintf("  → Recommend: %d round(s)\n", rec))
    cat(sprintf("  → Stable regime: rounds %d–%d (n=%d, mean=%.4f J, SD=%.4f J)\n\n",
                rec + 1, n, length(stable_vals), 
                ifelse(length(stable_vals) > 0, mean(stable_vals), NaN),
                ifelse(length(stable_vals) > 1, sd(stable_vals), 0)))
  }
}

# Run
main()