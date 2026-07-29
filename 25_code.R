#  Mirko Balli - Lorenzo Attanasio ----- GLOBAL FACTOR DATA ANALYSIS ----
#  Asset/Risk Management I
#  Factors: Altman Z-score (Altman, 1968) & Piotroski F-score (Piotroski, 2000)
#
#
#  We divided the code in 2 sections:
#    SECTION 1 -- DATA PREPARATION (execute just one time to create the .RData)
#    SECTION 2 -- ANALYSIS (we upload the .RData and produce all the output)
#
# .RData and this .R must in the same directory.


# SECTION 0 ---- PACKAGES (auto-install from CRAN) ----


required_pkgs <- c("data.table", "zoo", "ggplot2", "sandwich",
                   "lmtest", "readxl", "scales")
#sandwich for the theory. for the covariance matrix HAC (Heteroskedasticity and Autocorrelation Consistent)

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE)
}


# SECTION 1 ---- DATA PREPARATION ----
# This block reads the raw files from the subdirectory data/ and creates
# the file 25_data.RData. It is executed just if  the .RData doesn't exist.


if (!file.exists("25_data.RData")) {
  
  cat(" DATA PREPARATION: creation 25_data.RData \n")
  DATA_DIR <- "data"
  

  # 1a. Helper: read a CSV inside a .zip file of JKP ----

  read_jkp_zip <- function(zip_path) {
    flist <- unzip(zip_path, list = TRUE)$Name
    csv_name <- flist[grepl("\\.csv$", flist)][1]
    df <- read.csv(unz(zip_path, csv_name), stringsAsFactors = FALSE)
    df$date <- as.Date(df$date)
    df$ym   <- as.yearmon(df$date)
    return(df)
  }
  
  # 1b. JKP Factor Returns (long-short, capped VW, USA, monthly) ----
  #     The returns are in decimals (0.01 = 1%). direction = 1 for
  #     both: the factor is long high (good/strenght), short low.

  z_fct <- read_jkp_zip(file.path(DATA_DIR,
                                  "[usa]_[z_score]_[monthly]_[vw_cap]_[factor].zip"))
  f_fct <- read_jkp_zip(file.path(DATA_DIR,
                                  "[usa]_[f_score]_[monthly]_[vw_cap]_[factor].zip"))
  qual_fct <- read_jkp_zip(file.path(DATA_DIR,
                                     "[usa]_[quality]_[monthly]_[vw_cap]_[factor].zip"))
  mkt_vw <- read_jkp_zip(file.path(DATA_DIR,
                                   "[usa]_[mkt]_[monthly]_[vw]_[factor].zip"))
  mkt_ew <- read_jkp_zip(file.path(DATA_DIR,
                                   "[usa]_[mkt]_[monthly]_[ew]_[factor].zip"))
  
  jkp_factors <- data.table(
    ym       = z_fct$ym,
    z_score  = z_fct$ret
  )
  jkp_factors <- merge(jkp_factors,
                       data.table(ym = f_fct$ym,   f_score  = f_fct$ret),  all = TRUE)
  jkp_factors <- merge(jkp_factors,
                       data.table(ym = qual_fct$ym, quality  = qual_fct$ret), all = TRUE)
  jkp_factors <- merge(jkp_factors,
                       data.table(ym = mkt_vw$ym,  mkt_vw   = mkt_vw$ret),  all = TRUE)
  jkp_factors <- merge(jkp_factors,
                       data.table(ym = mkt_ew$ym,  mkt_ew   = mkt_ew$ret),  all = TRUE)
  
  cat("  JKP factors: ", nrow(jkp_factors), "months,",
      format(min(jkp_factors$ym)), "->", format(max(jkp_factors$ym)), "\n")
  

  # 1c. JKP Portfolio Returns (tercili: pf=1 low, pf=2 mid, pf=3 high) ----
  #     used for the long leg vs low leg decomposition 

  z_pf <- read_jkp_zip(file.path(DATA_DIR,
                                 "[usa]_[z_score]_[monthly]_[vw_cap]_[portfolio].zip"))
  f_pf <- read_jkp_zip(file.path(DATA_DIR,
                                 "[usa]_[f_score]_[monthly]_[vw_cap]_[portfolio].zip"))
  
  # Pivot: a column for each pf (1 = low, 3 = high)
  # We create a column pf_name clean for avoid problems with dcast

  z_pf_tmp <- data.table(ym = z_pf$ym,
                         pf_name = paste0("z_pf", as.integer(z_pf$pf)),
                         ret = z_pf$ret)
  z_pf_dt <- dcast(z_pf_tmp, ym ~ pf_name, value.var = "ret")
  
  f_pf_tmp <- data.table(ym = f_pf$ym,
                         pf_name = paste0("f_pf", as.integer(f_pf$pf)),
                         ret = f_pf$ret)
  f_pf_dt <- dcast(f_pf_tmp, ym ~ pf_name, value.var = "ret")
  # Result columns: z_pf1, z_pf2, z_pf3 / f_pf1, f_pf2, f_pf3
  
  cat("  Portfolios: z_score", nrow(z_pf_dt), "months; f_score",
      nrow(f_pf_dt), "months\n")
  

  # 1d. Hou et al. (2021) q5 Factors ----
  #     R_F = risk-free, R_MKT = excess market, R_ME = size,
  #     R_IA = investment, R_ROE = profitability, R_EG = expected growth.
  #     Warning: values are in % -> let's divide by 100.

  q5_raw <- read.csv(file.path(DATA_DIR, "q5_factors_monthly_2024.csv"),
                     stringsAsFactors = FALSE)
  q5_dt <- data.table(
    ym     = as.yearmon(paste(q5_raw$year, q5_raw$month, sep = "-"), "%Y-%m"),
    q5_RF  = q5_raw$R_F   / 100,
    q5_MKT = q5_raw$R_MKT / 100,
    q5_ME  = q5_raw$R_ME  / 100,
    q5_IA  = q5_raw$R_IA  / 100,
    q5_ROE = q5_raw$R_ROE / 100,
    q5_EG  = q5_raw$R_EG  / 100
  )
  cat("  q5 factors:", nrow(q5_dt), "months,",
      format(min(q5_dt$ym)), "->", format(max(q5_dt$ym)), "\n")
  
  # 1e. Fama-French Factors (FF3, FF5, Momentum) ----
  #     Download direct from Ken French Data Library.
  #     Values in % -> let's divide by 100.
  
  # Helper to parse the CSV of Ken French (inside the zip)
  parse_french_csv <- function(zip_path) {
    csv_name <- unzip(zip_path, list = TRUE)$Name[1]
    lines <- readLines(unz(zip_path, csv_name), warn = FALSE)
    # The monthly rows start with and integer of 6 (YYYYMM)
    is_monthly <- grepl("^\\s*\\d{6}\\s*,", lines)
    if (sum(is_monthly) == 0) {
      stop("No monthly data found in the file: ", zip_path)
    }
    first_idx <- which(is_monthly)[1]
    header <- lines[first_idx - 1]
    block <- c(header, lines[is_monthly])
    df <- read.csv(textConnection(block), stringsAsFactors = FALSE,
                   strip.white = TRUE)
    names(df)[1] <- "date_ym"
    df$date_ym <- as.integer(trimws(as.character(df$date_ym)))
    df <- df[!is.na(df$date_ym) & df$date_ym > 100000, ]
    return(df)
  }
  
  ff_base_url <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"
  ff_datasets <- list(
    FF3 = "F-F_Research_Data_Factors_CSV.zip",
    FF5 = "F-F_Research_Data_5_Factors_2x3_CSV.zip",
    Mom = "F-F_Momentum_Factor_CSV.zip"
  )
  
  ff_list <- list()
  for (ds_name in names(ff_datasets)) {
    temp <- tempfile(fileext = ".zip")
    cat("  Downloading", ds_name, "from Ken French...\n")
    download.file(paste0(ff_base_url, ff_datasets[[ds_name]]),
                  temp, mode = "wb", quiet = TRUE)
    ff_list[[ds_name]] <- parse_french_csv(temp)
    unlink(temp)
  }
  
  # Merging FF3 + extra columns from FF5 + Mom, in line with yearmon
  ff3 <- ff_list$FF3
  ff5 <- ff_list$FF5
  mom <- ff_list$Mom
  
  ff_dt <- data.table(
    ym     = as.yearmon(as.character(ff3$date_ym), "%Y%m"),
    Mkt.RF = ff3$Mkt.RF / 100,
    SMB    = ff3$SMB    / 100,
    HML    = ff3$HML    / 100,
    RF     = ff3$RF     / 100
  )
  
  # RMW and CMA from FF5
  ff5_dt <- data.table(
    ym  = as.yearmon(as.character(ff5$date_ym), "%Y%m"),
    RMW = ff5$RMW / 100,
    CMA = ff5$CMA / 100
  )
  ff_dt <- merge(ff_dt, ff5_dt, by = "ym", all.x = TRUE)
  
  # Momentum
  mom_dt <- data.table(
    ym  = as.yearmon(as.character(mom$date_ym), "%Y%m"),
    Mom = mom$Mom / 100
  )
  ff_dt <- merge(ff_dt, mom_dt, by = "ym", all.x = TRUE)
  
  cat("  FF factors:", nrow(ff_dt), "months,",
      format(min(ff_dt$ym)), "->", format(max(ff_dt$ym)), "\n")
  
  # 1f. Bloomberg Benchmarks (from Excel file imported from the terminal) ----
  #     Data = index levels-> we compute monthly returns
  #     Exception: LF980AS (HY OAS) is a spread, so we take it as a level.
  
  bbg_file <- file.path(DATA_DIR, "bloomberg_benchmarks.xlsx")
  bbg_sheets <- list(
    spxt      = "SPXT",       # S&P 500 Total Return (1989+)
    msci_qual = "M1USQU",     # MSCI USA Quality (1994+)
    msci_val  = "M1US000V",   # MSCI USA Value (1974+)
    bond_agg  = "LBUSTRUU",   # Bloomberg US Agg Bond TR (1976+)
    gold      = "XAU",        # Oro spot USD (1970+)
    hy_oas    = "LF980AS"     # ICE BofA US HY OAS spread (1994+)
  )
  
  bbg_dt <- data.table(ym = as.yearmon(NA_real_))[0]  # empty table
  
  for (col_name in names(bbg_sheets)) {
    sheet <- bbg_sheets[[col_name]]
    raw <- as.data.frame(read_excel(bbg_file, sheet = sheet))
    # Columns: Dates, Last Price (could be extra NA columns)
    raw <- raw[, 1:2]
    names(raw) <- c("date_raw", "price")
    raw <- raw[!is.na(raw$date_raw) & !is.na(raw$price), ]
    raw$date_raw <- as.Date(raw$date_raw)
    raw <- raw[order(raw$date_raw), ]   # chronological order
    raw$ym <- as.yearmon(raw$date_raw)
    
    if (col_name == "hy_oas") {
      # HY OAS is a spread (level), not a return
      piece <- data.table(ym = raw$ym, hy_oas = as.numeric(raw$price))
    } else {
      # Monthly arithmetic return: (P_t / P_{t-1}) - 1
      raw$ret <- c(NA_real_, diff(as.numeric(raw$price)) /
                     as.numeric(raw$price[-nrow(raw)]))
      piece <- data.table(ym = raw$ym, ret = raw$ret)
      setnames(piece, "ret", col_name)
    }
    
    if (nrow(bbg_dt) == 0) {
      bbg_dt <- piece
    } else {
      bbg_dt <- merge(bbg_dt, piece, by = "ym", all = TRUE)
    }
  }
  
  cat("  Bloomberg:", nrow(bbg_dt), "months,",
      format(min(bbg_dt$ym)), "->", format(max(bbg_dt$ym)), "\n")
  

  # 1g. FRED Macro Variables (regime indicators) ----
  # Are levels, not returns. We use the monthly average (already computed).

    fred_files <- list(
    nfci        = "NFCI.csv",
    term_spread = "T10Y2Y.csv",
    recession   = "USRECD.csv",
    vix         = "VIXCLS.csv"
  )
  
  fred_dt <- data.table(ym = as.yearmon(NA_real_))[0]
  
  for (col_name in names(fred_files)) {
    raw <- read.csv(file.path(DATA_DIR, fred_files[[col_name]]),
                    stringsAsFactors = FALSE)
    names(raw) <- c("date_raw", "value")
    # Manage eventual "." or "" as NA
    raw$value <- suppressWarnings(as.numeric(raw$value))
    raw <- raw[!is.na(raw$value), ]
    raw$ym <- as.yearmon(as.Date(raw$date_raw))
    piece <- data.table(ym = raw$ym, val = raw$value)
    setnames(piece, "val", col_name)
    
    if (nrow(fred_dt) == 0) {
      fred_dt <- piece
    } else {
      fred_dt <- merge(fred_dt, piece, by = "ym", all = TRUE)
    }
  }
  
  cat("  FRED:", nrow(fred_dt), "months,",
      format(min(fred_dt$ym)), "->", format(max(fred_dt$ym)), "\n")
  

  # 1h. MERGE: merging al in a unique data.table on yearmon ----

  master <- jkp_factors
  master <- merge(master, z_pf_dt,  by = "ym", all = TRUE)
  master <- merge(master, f_pf_dt,  by = "ym", all = TRUE)
  master <- merge(master, q5_dt,    by = "ym", all = TRUE)
  master <- merge(master, ff_dt,    by = "ym", all = TRUE)
  master <- merge(master, bbg_dt,   by = "ym", all = TRUE)
  master <- merge(master, fred_dt,  by = "ym", all = TRUE)
  
  # Adding Date column(last day of the month) to plotting
  master[, date := as.Date(ym, frac = 1)]
  setkey(master, ym)
  

  cat("\n  MASTER DATASET \n")
  cat("  Rows:", nrow(master), "| Columns:", ncol(master), "\n")
  cat("  Period:", format(min(master$ym)), "->", format(max(master$ym)), "\n")
  cat("  Columns:", paste(names(master), collapse = ", "), "\n")
  
  # 1i. SANITY CHECKS ----

  cat("\n  --- Sanity Checks ---\n")
  cat("  z_score not-NA:", sum(!is.na(master$z_score)), "months\n")
  cat("  f_score not-NA:", sum(!is.na(master$f_score)), "months\n")
  cat("  Mkt.RF  not-NA:", sum(!is.na(master$Mkt.RF)),  "months\n")
  cat("  q5_MKT  not-NA:", sum(!is.na(master$q5_MKT)),  "months\n")
  

  # 1j. SAVE ----

  save(master, file = "25_data.RData")
  cat("\n  >>> Saved: 25_data.RData <<<\n\n")
  
}  # ending final preparation block


# SECTION 2 ---- DATA CHARGING AND ANALYSIS SETUP ----

load("25_data.RData")
cat("Database loaded:", nrow(master), "months,", ncol(master), "columns\n")

# --- 2a. Helper functions for the analysis ---

# Average with t-stat Newey-West (H0: mean = 0)
nw_mean_test <- function(x) {
  x <- na.omit(x)
  if (length(x) < 12) return(c(mean=NA, se=NA, tstat=NA, pval=NA, n=length(x)))
  fit <- lm(x ~ 1)
  ct  <- coeftest(fit, vcov = NeweyWest(fit, prewhite = FALSE))
  c(mean = ct[1,1], se = ct[1,2], tstat = ct[1,3], pval = ct[1,4], n = length(x))
}

# Annualizing average and vol (monthly -> annual)
ann_mean <- function(x) mean(x, na.rm = TRUE) * 12
ann_vol  <- function(x) sd(x, na.rm = TRUE) * sqrt(12)
ann_sr   <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(NA)
  (m / s) * sqrt(12)
}

# Max drawdown (from return series)
max_drawdown <- function(r) {
  r <- na.omit(r)
  if (length(r) == 0) return(NA)
  cum  <- cumprod(1 + r)
  peak <- cummax(cum)
  dd   <- (cum - peak) / peak
  min(dd)
}

# small helper: excess kurtosis (used for the quality outlier check)
ex_kurt <- function(x) { x <- na.omit(x); mean(((x - mean(x))/sd(x))^4) - 3 }

# Spanning regression with Newey-West
run_spanning <- function(lhs_col, rhs_cols, dt, model_label) {
  cols <- c(lhs_col, rhs_cols)
  sub  <- dt[complete.cases(dt[, ..cols])]
  y    <- sub[[lhs_col]]
  X    <- as.data.frame(sub[, ..rhs_cols])
  fit  <- lm(y ~ ., data = X)
  nw   <- coeftest(fit, vcov = NeweyWest(fit, prewhite = FALSE))
  
  list(
    factor  = lhs_col,
    model   = model_label,
    alpha   = nw["(Intercept)", "Estimate"],
    alpha_t = nw["(Intercept)", "t value"],
    alpha_p = nw["(Intercept)", "Pr(>|t|)"],
    betas   = setNames(nw[-1, "Estimate"],  rhs_cols),
    betas_t = setNames(nw[-1, "t value"],   rhs_cols),
    r2      = summary(fit)$r.squared,
    adj_r2  = summary(fit)$adj.r.squared,
    nobs    = nrow(sub)
  )
}

# GRS test (Gibbons, Ross & Shanken, 1989)
grs_test <- function(factor_cols, rhs_cols, dt) {
  cols <- c(factor_cols, rhs_cols)
  sub  <- dt[complete.cases(dt[, ..cols])]
  Tobs <- nrow(sub)
  N    <- length(factor_cols)
  K    <- length(rhs_cols)
  
  alphas    <- numeric(N)
  resid_mat <- matrix(NA_real_, Tobs, N)
  
  for (i in seq_along(factor_cols)) {
    y   <- sub[[factor_cols[i]]]
    X   <- as.data.frame(sub[, ..rhs_cols])
    fit <- lm(y ~ ., data = X)
    alphas[i]    <- coef(fit)["(Intercept)"]
    resid_mat[,i] <- residuals(fit)
  }
  
  Sigma_eps <- crossprod(resid_mat) / Tobs
  F_mat     <- as.matrix(sub[, ..rhs_cols])
  mu_f      <- colMeans(F_mat)
  Sigma_f   <- cov(F_mat)
  
  grs_stat <- (Tobs / N) * ((Tobs - N - K) / (Tobs - K - 1)) *
    as.numeric(t(alphas) %*% solve(Sigma_eps) %*% alphas) /
    (1 + as.numeric(t(mu_f) %*% solve(Sigma_f) %*% mu_f))
  
  p_val <- 1 - pf(grs_stat, N, Tobs - N - K)
  list(stat = grs_stat, pval = p_val, df1 = N, df2 = Tobs - N - K, N_obs = Tobs)
}

# --- 2b. Plot theme e colors palette ---
theme_arm <- theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40", size = 10),
    legend.position = "bottom",
    legend.title    = element_blank(),
    panel.grid.minor = element_blank()
  )

pal <- c("Z-Score" = "#1565C0", "F-Score" = "#C62828",
         "Quality Cluster" = "#7B1FA2", "Market VW" = "#616161",
         "Market EW" = "#9E9E9E",
         "Long Leg (pf=3)" = "#2E7D32", "Short Leg (pf=1)" = "#E65100",
         "S&P 500 TR" = "#37474F", "MSCI Quality" = "#00838F",
         "MSCI Value" = "#4E342E", "Bond Agg" = "#558B2F",
         "Gold" = "#F9A825", "Combined 50/50" = "#AD1457")

# Creating the directory for graphs
dir.create("plots", showWarnings = FALSE)



# SECTION 3 -- DESCRIPTIVE STATISTICS [Part (b) and (c)] ----

cat("  SECTION 3 -- DESCRIPTIVE STAT FULL-SAMPLE\n")

# --- 3a. Summary Statistics with t-stat Newey-West ---

stat_cols <- c("z_score", "f_score", "quality", "mkt_vw", "mkt_ew")
stat_labels <- c("Z-Score", "F-Score", "Quality Cluster", "Market VW", "Market EW")

summary_table <- data.table(
  Factor     = stat_labels,
  Start      = sapply(stat_cols, function(c)
    format(min(master[!is.na(get(c))]$ym))),
  End        = sapply(stat_cols, function(c)
    format(max(master[!is.na(get(c))]$ym))),
  N_months     = sapply(stat_cols, function(c) sum(!is.na(master[[c]]))),
  Ann_Mean   = sapply(stat_cols, function(c)
    round(ann_mean(master[[c]]) * 100, 2)),
  Ann_Vol    = sapply(stat_cols, function(c)
    round(ann_vol(master[[c]]) * 100, 2)),
  Sharpe     = sapply(stat_cols, function(c)
    round(ann_sr(master[[c]]), 3)),
  Skewness   = sapply(stat_cols, function(c) {
    x <- na.omit(master[[c]]); round(mean(((x - mean(x))/sd(x))^3), 3)
  }),
  Kurtosis   = sapply(stat_cols, function(c) {
    x <- na.omit(master[[c]]); round(mean(((x - mean(x))/sd(x))^4) - 3, 3)
  }),
  MaxDD      = sapply(stat_cols, function(c)
    round(max_drawdown(master[[c]]) * 100, 1)),
  Pct_Pos    = sapply(stat_cols, function(c)
    round(mean(master[[c]] > 0, na.rm = TRUE) * 100, 1)),
  NW_tstat   = sapply(stat_cols, function(c)
    round(nw_mean_test(master[[c]])["tstat"], 2))
)

cat("Table 1 -- Summary Statistics (annualized returns in %)\n")
cat("NB: t-stat computed with standard error HAC of Newey-West.\n")
cat("    Harvey-Liu-Zhu (2016): threshold of about 3.0 for new factors.\n\n")
print(summary_table, row.names = FALSE)

# --- 3a-bis. Quality cluster outlier check [FIX 3] ---
# The full-sample Quality stats show extreme skew/kurtosis: the cluster starts
# in 1927 with sparse early coverage, so one early month dominates the moments.
# We flag the extreme month and report Quality stats on the common sample
# (post-1962, same start as our factors) for a fair comparison.
q_start <- min(master[!is.na(z_score)]$ym)
cat("\n\n--- Quality Cluster: outlier check ---\n")
cat(sprintf("  Worst month: %s (%+.1f%%) | Best month: %s (%+.1f%%)\n",
            format(master[which.min(quality)]$ym), min(master$quality, na.rm = TRUE) * 100,
            format(master[which.max(quality)]$ym), max(master$quality, na.rm = TRUE) * 100))
q_common <- master[ym >= q_start, quality]
cat(sprintf("  Quality on common sample (post-%s): Ann.Mean=%.2f%%, Vol=%.2f%%, Sharpe=%.3f, ExKurt=%.1f\n",
            format(q_start), ann_mean(q_common) * 100, ann_vol(q_common) * 100,
            ann_sr(q_common), ex_kurt(q_common)))
cat("  -> the extreme kurtosis is an early-sample artifact; on the common\n")
cat("     sample the Quality cluster is well-behaved and comparable to our factors.\n")

# --- 3b. Correlation Matrix ---

cor_cols   <- c("z_score", "f_score", "quality", "mkt_vw")
cor_labels <- c("Z-Score", "F-Score", "Quality", "Mkt VW")
cor_sub    <- master[, ..cor_cols]
cor_mat    <- cor(cor_sub, use = "pairwise.complete.obs")
rownames(cor_mat) <- cor_labels
colnames(cor_mat) <- cor_labels

cat("\n\nTable 2 -- Correlation matrix (pairwise complete)\n\n")
print(round(cor_mat, 3))

# --- 3c. Cumulative Excess Return Plot (log scale) ---
# long-short factors and market are all excess return.
# We plot the growth of 1$ invested

cum_cols <- c("z_score", "f_score", "quality", "mkt_vw")
cum_names <- c("Z-Score", "F-Score", "Quality Cluster", "Market VW")

# Common sample: from first month with z_score (the shortest))
cum_start <- min(master[!is.na(z_score)]$ym)
cum_data  <- master[ym >= cum_start]

cum_long <- rbindlist(lapply(seq_along(cum_cols), function(i) {
  x <- cum_data[!is.na(get(cum_cols[i]))]
  x[, cum := cumprod(1 + get(cum_cols[i]))]
  data.table(date = x$date, Factor = cum_names[i], value = x$cum)
}))

p_cum <- ggplot(cum_long, aes(x = date, y = value, color = Factor)) +
  geom_line(linewidth = 0.7) +
  scale_y_log10(labels = scales::dollar_format(prefix = "$")) +
  scale_color_manual(values = pal) +
  labs(title = "Cumulative Excess Returns -- Growth of $1",
       subtitle = paste("Common sample:", format(cum_start), "- Dec 2025"),
       x = NULL, y = "Growth of $1 (log scale)") +
  theme_arm
ggsave("plots/fig1_cumulative_returns.pdf", p_cum, width = 10, height = 6)
cat("\n\n>>> Saved: plots/fig1_cumulative_returns.pdf\n")

# --- 3d. Decomposition long leg vs short leg ---
# Per Z-score: pf=3 (high Z = safe) vs pf=1 (low Z = distressed)
# Per F-score: pf=3 (high F = strong) vs pf=1 (low F = weak)

leg_data <- master[!is.na(z_pf1) & !is.na(z_pf3) & !is.na(z_score)]

leg_long <- rbindlist(list(
  data.table(date = leg_data$date, Factor = "Z-Score: Long Leg (pf=3, safe)",
             value = cumprod(1 + leg_data$z_pf3)),
  data.table(date = leg_data$date, Factor = "Z-Score: Short Leg (pf=1, distressed)",
             value = cumprod(1 + leg_data$z_pf1)),
  data.table(date = leg_data$date, Factor = "Z-Score: Long-Short",
             value = cumprod(1 + leg_data$z_score))
))

p_legs_z <- ggplot(leg_long, aes(x = date, y = value, color = Factor)) +
  geom_line(linewidth = 0.7) +
  scale_y_log10(labels = scales::dollar_format(prefix = "$")) +
  scale_color_manual(values = c(
    "Z-Score: Long Leg (pf=3, safe)" = "#2E7D32",
    "Z-Score: Short Leg (pf=1, distressed)" = "#E65100",
    "Z-Score: Long-Short" = "#1565C0")) +
  labs(title = "Z-Score: decomposition long Leg vs short Leg",
       subtitle = "Both legs grow alike: distressed do NOT underperform -> no Z premium",
       x = NULL, y = "Growth of $1 (log scale)") +
  theme_arm
ggsave("plots/fig2a_legs_zscore.pdf", p_legs_z, width = 10, height = 6)

# Same analysis for F-score
leg_data_f <- master[!is.na(f_pf1) & !is.na(f_pf3) & !is.na(f_score)]
leg_long_f <- rbindlist(list(
  data.table(date = leg_data_f$date, Factor = "F-Score: Long Leg (pf=3, strong)",
             value = cumprod(1 + leg_data_f$f_pf3)),
  data.table(date = leg_data_f$date, Factor = "F-Score: Short Leg (pf=1, weak)",
             value = cumprod(1 + leg_data_f$f_pf1)),
  data.table(date = leg_data_f$date, Factor = "F-Score: Long-Short",
             value = cumprod(1 + leg_data_f$f_score))
))

p_legs_f <- ggplot(leg_long_f, aes(x = date, y = value, color = Factor)) +
  geom_line(linewidth = 0.7) +
  scale_y_log10(labels = scales::dollar_format(prefix = "$")) +
  scale_color_manual(values = c(
    "F-Score: Long Leg (pf=3, strong)" = "#2E7D32",
    "F-Score: Short Leg (pf=1, weak)" = "#E65100",
    "F-Score: Long-Short" = "#C62828")) +
  labs(title = "F-Score: decomposition long Leg vs short Leg",
       subtitle = "Wide gap: strong fundamentals beat weak -> real F premium",
       x = NULL, y = "Growth of $1 (log scale)") +
  theme_arm
ggsave("plots/fig2b_legs_fscore.pdf", p_legs_f, width = 10, height = 6)
cat(">>> Saved: plots/fig2a_legs_zscore.pdf & fig2b_legs_fscore.pdf\n")


# SECTION 4 -- ROLLING WINDOW ANALYSIS [Part (c)] ----

cat("  SECTION 4 -- ROLLING WINDOW ANALYSIS (60 months)\n")

ROLL_W <- 60  # rolling window in months

# Common sample for z_score and f_score
roll_dt <- master[!is.na(z_score) & !is.na(f_score) & !is.na(Mkt.RF)]
roll_dt <- roll_dt[order(ym)]

# --- 4a. Rolling Sharpe Ratio ---

roll_sr <- function(x) {
  if (sum(!is.na(x)) < ROLL_W * 0.8) return(NA)
  (mean(x, na.rm = TRUE) / sd(x, na.rm = TRUE)) * sqrt(12)
}

roll_dt[, z_roll_sr := frollapply(z_score, ROLL_W, roll_sr)]
roll_dt[, f_roll_sr := frollapply(f_score, ROLL_W, roll_sr)]

roll_sr_long <- rbindlist(list(
  data.table(date = roll_dt$date, Factor = "Z-Score",
             value = roll_dt$z_roll_sr),
  data.table(date = roll_dt$date, Factor = "F-Score",
             value = roll_dt$f_roll_sr)
))
roll_sr_long <- roll_sr_long[!is.na(value)]

p_roll_sr <- ggplot(roll_sr_long, aes(x = date, y = value, color = Factor)) +
  geom_line(linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = pal) +
  labs(title = paste0("Rolling Sharpe Ratio (", ROLL_W, " months)"),
       subtitle = "Negative periods = the factor 'stops working'",
       x = NULL, y = "Annualized Sharpe Ratio") +
  theme_arm
ggsave("plots/fig3_rolling_sharpe.pdf", p_roll_sr, width = 10, height = 5)
cat(">>> Saved: plots/fig3_rolling_sharpe.pdf\n")

# --- 4b. Rolling CAPM Beta and Alpha ---

roll_capm <- function(y, x, what = "beta") {
  ok <- !is.na(y) & !is.na(x)
  if (sum(ok) < ROLL_W * 0.8) return(NA)
  fit <- lm(y[ok] ~ x[ok])
  if (what == "beta")  return(coef(fit)[2])
  if (what == "alpha") return(coef(fit)[1] * 12)  # annualized
}

# We use rollapply on zoo to have two variables
z_zoo  <- zoo(roll_dt$z_score, roll_dt$date)
f_zoo  <- zoo(roll_dt$f_score, roll_dt$date)
mkt_zoo <- zoo(roll_dt$Mkt.RF, roll_dt$date)

roll_beta_z <- rollapply(merge(z_zoo, mkt_zoo), ROLL_W,
                         function(m) roll_capm(m[,1], m[,2], "beta"), by.column = FALSE, align = "right")
roll_beta_f <- rollapply(merge(f_zoo, mkt_zoo), ROLL_W,
                         function(m) roll_capm(m[,1], m[,2], "beta"), by.column = FALSE, align = "right")
roll_alpha_z <- rollapply(merge(z_zoo, mkt_zoo), ROLL_W,
                          function(m) roll_capm(m[,1], m[,2], "alpha"), by.column = FALSE, align = "right")
roll_alpha_f <- rollapply(merge(f_zoo, mkt_zoo), ROLL_W,
                          function(m) roll_capm(m[,1], m[,2], "alpha"), by.column = FALSE, align = "right")

roll_ba <- rbindlist(list(
  data.table(date = index(roll_beta_z), Factor = "Z-Score",
             Beta = coredata(roll_beta_z), Alpha = coredata(roll_alpha_z)),
  data.table(date = index(roll_beta_f), Factor = "F-Score",
             Beta = coredata(roll_beta_f), Alpha = coredata(roll_alpha_f))
))

# [FIX 4] subtitle corrected: Z-Score is pro-cyclical (beta>0), only F-Score is defensive
p_roll_beta <- ggplot(roll_ba, aes(x = date, y = Beta, color = Factor)) +
  geom_line(linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = pal) +
  labs(title = paste0("Rolling CAPM Beta (", ROLL_W, " months)"),
       subtitle = "F-Score defensive (beta<0); Z-Score pro-cyclical (beta>0): opposite quality",
       x = NULL, y = "Market beta") +
  theme_arm
ggsave("plots/fig4a_rolling_beta.pdf", p_roll_beta, width = 10, height = 5)

p_roll_alpha <- ggplot(roll_ba, aes(x = date, y = Alpha * 100, color = Factor)) +
  geom_line(linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = pal) +
  labs(title = paste0("Rolling CAPM Alpha annualized (", ROLL_W, " months)"),
       x = NULL, y = "Alpha (% annual)") +
  theme_arm
ggsave("plots/fig4b_rolling_alpha.pdf", p_roll_alpha, width = 10, height = 5)
cat(">>> Saved: plots/fig4a_rolling_beta.pdf & fig4b_rolling_alpha.pdf\n")

# --- 4c. Rolling Correlation z_score vs f_score ---

roll_cor_zf <- rollapply(merge(z_zoo, f_zoo), ROLL_W,
                         function(m) cor(m[,1], m[,2], use = "complete.obs"),
                         by.column = FALSE, align = "right")

p_roll_cor <- ggplot(data.table(date = index(roll_cor_zf),
                                corr = coredata(roll_cor_zf)),
                     aes(x = date, y = corr)) +
  geom_line(linewidth = 0.7, color = "#7B1FA2") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = paste0("Rolling Correlation Z-Score vs F-Score (", ROLL_W, " months)"),
       subtitle = "Surprise: mainly negative correlation (level vs direction)",
       x = NULL, y = "Correlation") +
  ylim(-0.9, 0.7) +
  theme_arm
ggsave("plots/fig5_rolling_correlation.pdf", p_roll_cor, width = 10, height = 5)
cat(">>> Saved: plots/fig5_rolling_correlation.pdf\n")

# --- 4d. Regime Overlay: Rolling Sharpe + HY OAS + Recessions ---

# We now overlay BOTH factor Sharpes (not just Z-Score) on the credit-stress
# regime (HY OAS) and NBER recessions. The question is whether the quality
# factors break down when distress rises: Z-Score is the distress-LEVEL factor,
# F-Score the improvement/DIRECTION factor.
overlay_dt <- merge(
  dcast(roll_sr_long, date ~ Factor, value.var = "value"),
  master[, .(date, hy_oas, recession)],
  by = "date", all.x = TRUE
)
overlay_dt <- overlay_dt[!is.na(`Z-Score`) | !is.na(`F-Score`)]

p_regime <- ggplot(overlay_dt, aes(x = date)) +
  # Recession bands
  geom_rect(data = {
    rec <- master[recession == 1]
    if (nrow(rec) > 0) {
      rec[, grp := cumsum(c(1, diff(as.numeric(ym)) > 0.15))]
      rec[, .(xmin = min(date), xmax = max(date)), by = grp]
    } else { data.table(xmin = as.Date(NA), xmax = as.Date(NA), grp = 1)[0] }
  }, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
  fill = "gray85", alpha = 0.5, inherit.aes = FALSE) +
  # Both factor rolling Sharpes
  geom_line(aes(y = `Z-Score`, color = "Z-Score Rolling Sharpe"),
            linewidth = 0.6, na.rm = TRUE) +
  geom_line(aes(y = `F-Score`, color = "F-Score Rolling Sharpe"),
            linewidth = 0.6, na.rm = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  # HY OAS (scaled) as credit-stress regime indicator
  geom_line(aes(y = hy_oas * 0.5, color = "HY OAS (scaled)"),
            linewidth = 0.5, linetype = "dashed", na.rm = TRUE) +
  scale_color_manual(values = c("Z-Score Rolling Sharpe" = "#1565C0",
                                "F-Score Rolling Sharpe" = "#C62828",
                                "HY OAS (scaled)" = "#E65100")) +
  labs(title = "Rolling Sharpe (Z & F) vs HY OAS and Recessions",
       subtitle = "Grey bands = NBER recessions. High HY OAS = distress in credit markets",
       x = NULL, y = "Sharpe / OAS (scaled)") +
  coord_cartesian(xlim = range(overlay_dt[!is.na(`Z-Score`)]$date)) +
  theme_arm
ggsave("plots/fig6_regime_overlay.pdf", p_regime, width = 11, height = 5)
cat(">>> Saved: plots/fig6_regime_overlay.pdf\n")


# SECTION 5 -- PRE/POST PUBLICATION [Part (c)] ----


cat("  SECTION 5 -- PRE/POST PUBLICATION ANALYSIS\n")

cat("  Framework: McLean & Pontiff (2016)\n")
cat("  Z-score: Altman (1968). F-score: Piotroski (2000), sample 1976-1996.\n\n")

pub_analysis <- function(col, pub_year, sample_end_year, label) {
  dt <- master[!is.na(get(col))]
  # Periods
  pre_pub  <- dt[as.numeric(format(date, "%Y")) < pub_year]
  post_pub <- dt[as.numeric(format(date, "%Y")) >= pub_year]
  # Also the original paper sample-end if available
  in_sample  <- dt[as.numeric(format(date, "%Y")) <= sample_end_year]
  out_sample <- dt[as.numeric(format(date, "%Y")) > sample_end_year &
                     as.numeric(format(date, "%Y")) < pub_year]
  
  make_row <- function(sub, period_name) {
    if (nrow(sub) < 12) {
      return(data.table(Factor = label, Period = period_name,
                        N = nrow(sub), Ann_Mean_pct = NA, Ann_Vol_pct = NA,
                        Sharpe = NA, NW_tstat = NA))
    }
    nw <- nw_mean_test(sub[[col]])
    data.table(
      Factor      = label,
      Period      = period_name,
      N           = nrow(sub),
      Ann_Mean_pct = round(ann_mean(sub[[col]]) * 100, 2),
      Ann_Vol_pct  = round(ann_vol(sub[[col]]) * 100, 2),
      Sharpe       = round(ann_sr(sub[[col]]), 3),
      NW_tstat     = round(nw["tstat"], 2)
    )
  }
  
  rbindlist(list(
    make_row(dt,        "Full Sample"),
    make_row(in_sample, paste0("In-Sample (<=", sample_end_year, ")")),
    make_row(out_sample, paste0("Post-Sample, Pre-Pub (",
                                sample_end_year+1, "-", pub_year-1, ")")),
    make_row(post_pub,  paste0("Post-Publication (>=", pub_year, ")"))
  ))
}

pub_z <- pub_analysis("z_score", pub_year = 1968, sample_end_year = 1965,
                      label = "Z-Score")
pub_f <- pub_analysis("f_score", pub_year = 2000, sample_end_year = 1996,
                      label = "F-Score")
pub_table <- rbindlist(list(pub_z, pub_f))

cat("Table 3 -- Performance before and after publication\n")
cat("NB: for Z-Score the pre-pub window is short (data start in 1962).\n\n")
print(pub_table, row.names = FALSE)


# SECTION 6 -- SPANNING REGRESSIONS [Part (d)] ----

cat("  SECTION 6 -- SPANNING REGRESSIONS\n")

# Models to test
models <- list(
  CAPM = c("Mkt.RF"),
  FF3  = c("Mkt.RF", "SMB", "HML"),
  q5   = c("q5_MKT", "q5_ME", "q5_IA", "q5_ROE", "q5_EG"),
  FF5  = c("Mkt.RF", "SMB", "HML", "RMW", "CMA"),
  FF6  = c("Mkt.RF", "SMB", "HML", "RMW", "CMA", "Mom")
)

factors_to_test <- c("z_score", "f_score")
factor_labels   <- c("Z-Score", "F-Score")

all_results <- list()
idx <- 1

for (i in seq_along(factors_to_test)) {
  for (m in names(models)) {
    res <- run_spanning(factors_to_test[i], models[[m]], master, m)
    all_results[[idx]] <- res
    idx <- idx + 1
  }
}

# --- 6a. Alpha e R^2 summary tables ---
# [optional fix] we add an annualized alpha column (x12) for readability
span_table <- rbindlist(lapply(all_results, function(r) {
  data.table(
    Factor    = r$factor,
    Model     = r$model,
    Alpha_m   = round(r$alpha * 100, 3),        # monthly %
    Alpha_ann = round(r$alpha * 12 * 100, 2),   # annualized %
    Alpha_t   = round(r$alpha_t, 2),
    R2        = round(r$r2 * 100, 1),           # in %
    N         = r$nobs
  )
}))

cat("Table 4 -- Spanning Regressions: Alpha and t-stat NW\n")
cat("Columns: Alpha_m (% monthly), Alpha_ann (% annualized), t(Alpha) NW, R^2 in %\n\n")
print(span_table, row.names = FALSE)

# --- 6b. Beta detail for each model (complete print) ---
cat("\n\n--- Coefficient details (selected) ---\n\n")

for (res in all_results) {
  cat(sprintf("  %s ~ %s (N=%d, R^2=%.1f%%)\n",
              res$factor, res$model, res$nobs, res$r2 * 100))
  cat(sprintf("    Alpha: %.4f  [t = %.2f, p = %.4f]\n",
              res$alpha, res$alpha_t, res$alpha_p))
  for (b in names(res$betas)) {
    cat(sprintf("    %-8s: %7.4f  [t = %6.2f]\n",
                b, res$betas[b], res$betas_t[b]))
  }
  cat("\n")
}

# --- 6c. Internal Spanning: f_score ~ z_score (and viceversa) ---
cat("--- Internal Spanning: are the two factors explaining each other? ---\n\n")

int_fz <- run_spanning("f_score", "z_score", master, "f~z")
int_zf <- run_spanning("z_score", "f_score", master, "z~f")

cat(sprintf("  f_score ~ z_score: alpha = %.4f [t=%.2f], R^2 = %.1f%%\n",
            int_fz$alpha, int_fz$alpha_t, int_fz$r2*100))
cat(sprintf("  z_score ~ f_score: alpha = %.4f [t=%.2f], R^2 = %.1f%%\n",
            int_zf$alpha, int_zf$alpha_t, int_zf$r2*100))
cat("  If alpha is significative: the two factors bring different info.\n\n")

# --- 6d. GRS Test ---
cat("--- GRS Test (Gibbons-Ross-Shanken, 1989) ---\n")
cat("  H0: all alphas = 0 jointly\n\n")

for (m in names(models)) {
  g <- tryCatch(
    grs_test(factors_to_test, models[[m]], master),
    error = function(e) NULL
  )
  if (!is.null(g)) {
    cat(sprintf("  %s: GRS = %.3f, p = %.4f (df: %d, %d, N_obs = %d)\n",
                m, g$stat, g$pval, g$df1, g$df2, g$N_obs))
  }
}


# SECTION 7 -- SUPPLEMENTARY ANALYSIS ----

cat("  SECTION 7 -- Supplementary analysis\n")

# --- 7a. Combined factor 50/50 and risk-parity ---

master[, combined_eq := (z_score + f_score) / 2]

# Risk-parity: inverse weights of rolling volatility
rp_vol_z <- master[, frollapply(z_score, 36, sd, na.rm = TRUE)]
rp_vol_f <- master[, frollapply(f_score, 36, sd, na.rm = TRUE)]
w_z <- (1/rp_vol_z) / (1/rp_vol_z + 1/rp_vol_f)
w_f <- 1 - w_z
# [FIX 2] lag weights by 1 month to avoid look-ahead bias: month t is weighted
# using volatility known only up to t-1
w_z <- shift(w_z, 1)
w_f <- shift(w_f, 1)
master[, combined_rp := w_z * z_score + w_f * f_score]

cat("--- Combined factor ---\n")
cat("  50/50 equal-weight and risk-parity (inverse proportional weights\n")
cat("  to rolling volatility 36m, lagged 1 month to avoid look-ahead).\n\n")

combo_cols <- c("z_score", "f_score", "combined_eq", "combined_rp")
combo_labs <- c("Z-Score", "F-Score", "Combined 50/50", "Combined RP")

combo_table <- data.table(
  Factor     = combo_labs,
  Ann_Mean   = sapply(combo_cols, function(c)
    round(ann_mean(master[[c]]) * 100, 2)),
  Ann_Vol    = sapply(combo_cols, function(c)
    round(ann_vol(master[[c]]) * 100, 2)),
  Sharpe     = sapply(combo_cols, function(c)
    round(ann_sr(master[[c]]), 3)),
  NW_tstat   = sapply(combo_cols, function(c)
    round(nw_mean_test(master[[c]])["tstat"], 2))
)
print(combo_table, row.names = FALSE)

# Interpretation note: combining with Z-Score (no premium) DILUTES F-Score, so
# the 50/50 Sharpe is below F-Score alone. Risk-parity does better by under-
# weighting the high-vol Z-Score. The benefit here is diversification, not return.
cat("\n  Note: the 50/50 Sharpe is below F-Score alone -> combining with the\n")
cat("  premium-less Z-Score dilutes F-Score. Risk-parity is better because it\n")
cat("  underweights the high-vol Z-Score. The gain is diversification, not return.\n")

# --- 7a-bis. Plot: combined factors vs single factors (growth of $1) ---
# Common sample = where combined_rp is defined (needs 36m rolling vol, lagged 1m).
combo_plot_dt <- master[!is.na(combined_rp) & !is.na(z_score) & !is.na(f_score)]
combo_plot_cols <- c("z_score", "f_score", "combined_eq", "combined_rp")
combo_plot_labs <- c("Z-Score", "F-Score", "Combined 50/50", "Combined RP")

combo_cum <- rbindlist(lapply(seq_along(combo_plot_cols), function(i) {
  col <- combo_plot_cols[i]
  x <- combo_plot_dt[!is.na(get(col))]
  data.table(date = x$date, Series = combo_plot_labs[i],
             value = cumprod(1 + x[[col]]))
}))

p_combo <- ggplot(combo_cum, aes(x = date, y = value, color = Series)) +
  geom_line(linewidth = 0.7) +
  scale_y_log10(labels = scales::dollar_format(prefix = "$")) +
  scale_color_manual(values = c(
    "Z-Score" = "#1565C0", "F-Score" = "#C62828",
    "Combined 50/50" = "#AD1457", "Combined RP" = "#6A1B9A")) +
  labs(title = "Combined Factor vs Single Factors -- Growth of $1",
       subtitle = "Combining smooths the ride (lower vol) but does not beat F-Score alone",
       x = NULL, y = "Growth of $1 (log scale)") +
  theme_arm
ggsave("plots/fig8_combined_factor.pdf", p_combo, width = 10, height = 6)
cat("\n>>> Saved: plots/fig8_combined_factor.pdf\n")

# --- 7b. Benchmark Comparison (Bloomberg) ---

cat("\n--- Comparison with benchmark and alternative assets ---\n\n")

bench_cols <- c("z_score", "f_score", "spxt", "msci_qual", "msci_val",
                "bond_agg", "gold")
bench_labs <- c("Z-Score", "F-Score", "S&P 500 TR", "MSCI Quality",
                "MSCI Value", "Bond Agg", "Gold")

# For Bloomberg benchmark : we get the excess (we subtract RF)
# NB: factors JKP are already excess/zero-cost.
bench_dt <- copy(master)
# RF from FF (monthly, decimal) -- we subtract from benchmark returns
for (bc in c("spxt", "msci_qual", "msci_val", "bond_agg", "gold")) {
  bench_dt[, (paste0(bc, "_xs")) := get(bc) - RF]
}


# Table with statistics (using excess returns for the benchmark)
bench_cols_xs <- c("z_score", "f_score",
                   "spxt_xs", "msci_qual_xs", "msci_val_xs",
                   "bond_agg_xs", "gold_xs")

bench_table <- data.table(
  Benchmark  = bench_labs,
  N          = sapply(bench_cols_xs, function(c) sum(!is.na(bench_dt[[c]]))),
  Ann_Mean   = sapply(bench_cols_xs, function(c)
    round(ann_mean(bench_dt[[c]]) * 100, 2)),
  Ann_Vol    = sapply(bench_cols_xs, function(c)
    round(ann_vol(bench_dt[[c]]) * 100, 2)),
  Sharpe     = sapply(bench_cols_xs, function(c)
    round(ann_sr(bench_dt[[c]]), 3))
)

cat("Table 5 -- Comparing factor vs benchmark (excess returns)\n\n")
print(bench_table, row.names = FALSE)

# Correlation between factors vs benchmark
bench_cor_cols <- c("z_score", "f_score", "spxt", "msci_qual",
                    "bond_agg", "gold", "hy_oas")
bench_cor_labs <- c("Z-Score", "F-Score", "S&P 500",
                    "MSCI Qual", "Bond Agg", "Gold", "HY OAS")
bench_cor <- cor(master[, ..bench_cor_cols], use = "pairwise.complete.obs")
rownames(bench_cor) <- bench_cor_labs
colnames(bench_cor) <- bench_cor_labs

cat("\nTable 6 -- Factor correlation vs benchmark and asset class\n\n")
print(round(bench_cor, 3))

# --- 7c. Plot benchmark comparison ---

# Using common sample at SPXT (from 1989)
bench_start <- as.yearmon("1989-09", "%Y-%m")   # locale-robust (no month name)
bench_plot_dt <- master[ym >= bench_start]

bench_plot_cols <- c("z_score", "f_score", "spxt", "msci_qual", "bond_agg", "gold")
bench_plot_labs <- c("Z-Score", "F-Score", "S&P 500 TR",
                     "MSCI Quality", "Bond Agg", "Gold")

# [FIX 1] apples-to-apples: everything in EXCESS returns.
# JKP factors are already excess/zero-cost; Bloomberg benchmarks -> subtract RF.
# This makes the plot consistent with Table 5 (before, benchmarks were total
# return and got an unfair boost from the compounded risk-free rate).
bench_cum <- rbindlist(lapply(seq_along(bench_plot_cols), function(i) {
  col <- bench_plot_cols[i]
  x <- bench_plot_dt[!is.na(get(col)) & !is.na(RF)]
  if (col %in% c("z_score", "f_score")) {
    r <- x[[col]]                 # already excess
  } else {
    r <- x[[col]] - x$RF          # make benchmark excess
  }
  data.table(date = x$date, Series = bench_plot_labs[i], value = cumprod(1 + r))
}))

p_bench <- ggplot(bench_cum, aes(x = date, y = value, color = Series)) +
  geom_line(linewidth = 0.6) +
  scale_y_log10(labels = scales::dollar_format(prefix = "$")) +
  scale_color_manual(values = c(
    "Z-Score" = "#1565C0", "F-Score" = "#C62828",
    "S&P 500 TR" = "#37474F", "MSCI Quality" = "#00838F",
    "Bond Agg" = "#558B2F", "Gold" = "#F9A825")) +
  labs(title = "Factors vs Benchmark -- Growth of $1 (excess returns)",
       subtitle = paste("From", format(bench_start), "- all series in excess of RF"),
       x = NULL, y = "Growth of $1 (log scale)") +
  theme_arm
ggsave("plots/fig7_benchmark_comparison.pdf", p_bench, width = 10, height = 6)
cat("\n>>> Saved: plots/fig7_benchmark_comparison.pdf\n")

cat("  The analysis is complete\n")
