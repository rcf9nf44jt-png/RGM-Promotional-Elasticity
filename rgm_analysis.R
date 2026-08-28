# =====================================================================
# PROMOTIONAL EFFECTIVENESS & PRICE ELASTICITY ANALYSIS
# Dataset: dunnhumby "Breakfast at the Frat"
# Author:  David Mantilla Jaramillo
# =====================================================================
# Run top to bottom. Each block prints a sanity check before the next
# one runs. Comments explain the reasoning behind each modelling choice.
# =====================================================================


# ---------------------------------------------------------------------
# 0. SETUP
# ---------------------------------------------------------------------
# Run this line ONCE, then comment it out:
# install.packages(c("readxl","dplyr","tidyr","ggplot2","broom","purrr","scales","lubridate"))

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(purrr)
library(scales)
library(lubridate)

# --- Parameters ---------------------------------------------------------
FILE <- "Breakfast_at_the_Frat.xlsx"   # path to the workbook — edit if it
                                       # lives in a different directory
GROSS_MARGIN <- 0.30                   # assumed retailer gross margin on
                                       # base price — a stated assumption,
                                       # not observed data; tested for
                                       # sensitivity at 0.25 and 0.35 in
                                       # section 4g
MIN_OBS      <- 200                    # min store-weeks to model a product
# ----------------------------------------------------------------------

setwd("~/Desktop/CV y Job Search/RStudio Project")   # working directory — edit to match


# ---------------------------------------------------------------------
# 1. LOAD THE THREE SHEETS
# ---------------------------------------------------------------------
# First, see what the sheets are actually called:
excel_sheets(FILE)

# Sheet names may vary by workbook version — edit the strings below if
# these don't match. If the columns import as "...1", "...2" etc, the
# sheet has a title row above the headers — add skip = 1 to read_excel().

tx    <- read_excel(FILE, sheet = "dh Transaction Data", skip = 1)
products  <- read_excel(FILE, sheet = "dh Products Lookup", skip = 1)
store <- read_excel(FILE, sheet = "dh Store Lookup", skip = 1)

# Normalise column names to uppercase so nothing breaks on a typo
names(tx)    <- toupper(trimws(names(tx)))
names(products)  <- toupper(trimws(names(products)))
names(store) <- toupper(trimws(names(store)))

cat("\nTransactions:", nrow(tx), "rows\n")
cat("Products:    ", nrow(products), "rows\n")
cat("Stores:      ", nrow(store), "rows\n")
glimpse(tx)

# KNOWN DATA QUALITY ISSUE: the store lookup has 2 more rows than distinct
# stores. Two STORE_IDs (4503 Rockwall TX, 17627 Flower Mound TX) each
# appear twice with identical address/size fields but a DIFFERENT
# SEG_VALUE_NAME (e.g. one row MAINSTREAM, the other UPSCALE). Left-joining
# without deduplicating fans every transaction row for those two stores out
# into two rows, silently inflating their weight in every downstream table.
# The source data gives no way to tell which tier label is correct, so we
# keep the first occurrence and disclose this — do not average or "fix" the
# tier silently. Any output that splits results by SEG_VALUE_NAME should
# flag this explicitly.
dupe_stores <- store %>% count(STORE_ID) %>% filter(n > 1) %>% pull(STORE_ID)
if (length(dupe_stores) > 0) {
  cat("\nWARNING: STORE_ID(s) with duplicate rows in store lookup:",
      paste(dupe_stores, collapse = ", "),
      "- keeping first occurrence, tier label is ambiguous for these stores.\n")
}
store <- store %>% distinct(STORE_ID, .keep_all = TRUE)


# ---------------------------------------------------------------------
# 2. JOIN AND BUILD THE ANALYSIS TABLE
# ---------------------------------------------------------------------
# This is the "merge dimension tables onto the fact table" step. In a
# real RGM team this is done every single day.

panel <- tx %>%
  left_join(products  %>% select(UPC, DESCRIPTION, MANUFACTURER,
                             CATEGORY, SUB_CATEGORY, PRODUCT_SIZE),
            by = "UPC") %>%
  # NOTE: the store sheet keys on STORE_ID, not STORE_NUM. The join below
  # maps one to the other. SEG_VALUE_NAME is the store price tier
  # (UPSCALE / MAINSTREAM / VALUE) — the most useful column on this sheet.
  left_join(store %>% select(STORE_ID, ADDRESS_STATE_PROV_CODE,
                             SEG_VALUE_NAME, SALES_AREA_SIZE_NUM,
                             AVG_WEEKLY_BASKETS),
            by = c("STORE_NUM" = "STORE_ID")) %>%
  mutate(
    WEEK_END_DATE = as.Date(WEEK_END_DATE),
    MONTH         = month(WEEK_END_DATE),
    YEAR          = year(WEEK_END_DATE),

    # Discount depth: the single most important derived variable
    DISCOUNT_PCT  = ifelse(BASE_PRICE > 0,
                           (BASE_PRICE - PRICE) / BASE_PRICE, NA_real_),

    # Promotional mechanic. Order matters — check richest support first.
    MECHANIC = case_when(
      FEATURE == 1 & DISPLAY == 1 ~ "Feature + Display",
      FEATURE == 1                ~ "Feature only",
      DISPLAY == 1                ~ "Display only",
      TPR_ONLY == 1               ~ "TPR only (price cut, no support)",
      TRUE                        ~ "No promotion"
    ),
    ON_PROMO = MECHANIC != "No promotion"
  ) %>%
  filter(!is.na(UNITS), UNITS > 0, !is.na(PRICE), PRICE > 0,
         !is.na(BASE_PRICE), BASE_PRICE > 0)

# Sanity checks — ALWAYS look at these before modelling
cat("\n--- Rows by mechanic ---\n"); print(table(panel$MECHANIC))
cat("\n--- Weeks covered ---\n");    print(range(panel$WEEK_END_DATE))
cat("\n--- Discount depth on promoted weeks ---\n")
print(summary(panel$DISCOUNT_PCT[panel$ON_PROMO]))

# Red flag to check: does PRICE ever exceed BASE_PRICE? A few rows is
# normal (price rounding); a lot would indicate a column was misread.
cat("\nRows where price > base price:",
    sum(panel$PRICE > panel$BASE_PRICE), "\n")


# ---------------------------------------------------------------------
# 3. PRICE ELASTICITY BY PRODUCT
# ---------------------------------------------------------------------
# Specification: log(units) ~ log(price) + promo support + store + season
#
# WHY THIS SPECIFICATION:
#  - log-log means the coefficient on log(PRICE) IS the elasticity:
#    a 1% price change produces a beta% volume change. No conversion.
#  - FEATURE and DISPLAY are included because promoted products are both
#    cheaper AND more visible. Omit them and the price coefficient
#    absorbs the visibility effect, overstating true elasticity. This is
#    the single most important modelling choice in this analysis.
#  - factor(STORE_NUM) absorbs permanent differences between stores
#    (size, affluence, footfall).
#  - factor(MONTH) absorbs seasonality (pizza in winter, etc).

fit_elasticity <- function(df) {
  if (nrow(df) < MIN_OBS)               return(NULL)
  if (n_distinct(round(df$PRICE, 2)) < 5) return(NULL)  # no price variation

  m <- lm(log(UNITS) ~ log(PRICE) + FEATURE + DISPLAY +
            factor(STORE_NUM) + factor(MONTH),
          data = df)

  tidy(m) %>%
    filter(term == "log(PRICE)") %>%
    transmute(elasticity = estimate,
              std_error  = std.error,
              p_value    = p.value,
              adj_r2     = summary(m)$adj.r.squared,
              n_obs      = nrow(df))
}

elasticities <- panel %>%
  group_by(UPC, DESCRIPTION, MANUFACTURER, CATEGORY) %>%
  group_modify(~ fit_elasticity(.x) %||% tibble()) %>%
  ungroup() %>%
  arrange(elasticity)

print(elasticities, n = 60)

# Category-level summary: median is more robust than mean to one odd product
elasticity_by_category <- elasticities %>%
  filter(p_value < 0.05) %>%          # keep only statistically meaningful
  group_by(CATEGORY) %>%
  summarise(
    median_elasticity = median(elasticity),
    n_products        = n(),
    .groups = "drop"
  ) %>%
  arrange(median_elasticity)

cat("\n=== ELASTICITY BY CATEGORY ===\n")
print(elasticity_by_category)

# HOW TO READ THIS:
#  Elasticity of -2.5 => a 10% price cut lifts volume ~25%.
#  Elasticity between -1 and 0 => INELASTIC. Volume gain does not
#    compensate the price given up. Promoting here destroys margin.
#  A POSITIVE elasticity means something is wrong with the model or
#    the product is barely promoted. Investigate before reporting it.


# ---------------------------------------------------------------------
# 3b. ELASTICITY BY STORE TIER (SEG_VALUE_NAME)
# ---------------------------------------------------------------------
# Does price sensitivity differ between UPSCALE / MAINSTREAM / VALUE
# stores? If yes, that is evidence for channel-differentiated pricing.
#
# METHOD NOTE: the natural first attempt is to pool every product in a
# category into ONE regression with a log(PRICE) x SEG_VALUE_NAME
# interaction. That approach was tried and rejected — it produced
# interaction coefficients an order of magnitude larger than anything
# plausible (e.g. +1.13 for frozen pizza UPSCALE, when comparing
# per-product tier medians directly showed a gap of ~0.14). The pooled
# model was picking up which SKUs happen to sell more in which tier of
# store (assortment differences), confounded with each SKU's own price
# level and elasticity — not a clean tier effect. Same failure mode
# flagged above: one regression should not average over things that are
# structurally different.
#
# The fix: fit the interaction PER PRODUCT (log(UNITS) ~ log(PRICE) *
# SEG_VALUE_NAME + FEATURE + DISPLAY + factor(STORE_NUM) + factor(MONTH)),
# then look at how many products show a significant tier effect and in
# which direction. This keeps every regression apples-to-apples within a
# single SKU, exactly like the main elasticity model.

fit_tier_interaction <- function(df) {
  if (nrow(df) < MIN_OBS)                 return(NULL)
  if (n_distinct(round(df$PRICE, 2)) < 5) return(NULL)
  if (n_distinct(df$SEG_VALUE_NAME) < 3)  return(NULL)  # need all 3 tiers present

  m <- tryCatch(
    lm(log(UNITS) ~ log(PRICE) * SEG_VALUE_NAME + FEATURE + DISPLAY +
         factor(STORE_NUM) + factor(MONTH), data = df),
    error = function(e) NULL)
  if (is.null(m)) return(NULL)

  tidy(m) %>%
    filter(grepl("^log[(]PRICE[)]:SEG_VALUE_NAME", term)) %>%
    transmute(tier = sub("log[(]PRICE[)]:SEG_VALUE_NAME", "", term),
              # negative = MORE elastic than MAINSTREAM (the reference level)
              # positive = LESS elastic than MAINSTREAM
              interaction_estimate = estimate,
              p_value = p.value)
}

panel_tiered <- panel %>%
  filter(!is.na(SEG_VALUE_NAME)) %>%
  mutate(SEG_VALUE_NAME = relevel(factor(SEG_VALUE_NAME), ref = "MAINSTREAM"))

tier_interactions <- panel_tiered %>%
  group_by(UPC, DESCRIPTION, CATEGORY) %>%
  group_modify(~ fit_tier_interaction(.x) %||% tibble()) %>%
  ungroup()

elasticity_by_tier_summary <- tier_interactions %>%
  group_by(CATEGORY, tier) %>%
  summarise(
    n_products          = n(),
    n_significant       = sum(p_value < 0.05),
    n_sig_more_elastic  = sum(p_value < 0.05 & interaction_estimate < 0),
    n_sig_less_elastic  = sum(p_value < 0.05 & interaction_estimate > 0),
    median_interaction  = median(interaction_estimate),
    .groups = "drop"
  )

cat("\n=== PRICE SENSITIVITY BY STORE TIER (vs MAINSTREAM baseline) ===\n")
cat("Negative median_interaction = MORE price-sensitive than MAINSTREAM.\n")
cat("Positive median_interaction = LESS price-sensitive than MAINSTREAM.\n")
cat("Trust a row only when n_significant is a large share of n_products AND\n")
cat("n_sig_more_elastic / n_sig_less_elastic lopsidedly agree on direction -\n")
cat("a near-even split (e.g. 3 vs 4) means there is no real tier effect,\n")
cat("just noise, regardless of what the median says.\n\n")
print(elasticity_by_tier_summary)

# INTERPRETATION NOTE:
#  Only report a tier effect for a category where the sign is consistent
#  across most of the SIGNIFICANT products, not just where the median sits
#  on one side of zero. A category split e.g. 4 significant-negative vs 3
#  significant-positive is not a finding — it is noise, and reporting it
#  as "VALUE shoppers are more price sensitive" would be an overclaim not
#  supported by the data. The intuitive direction (VALUE tier = more
#  elastic) should not be assumed just because it sounds right — check
#  whether this category's numbers actually show it.


# ---------------------------------------------------------------------
# 4. POST-EVENT ANALYSIS (PEA): BASELINE, UPLIFT, ROI
# ---------------------------------------------------------------------
# This mirrors the post-event analysis (PEA) process used by retail
# revenue-management teams to score individual promotions.

# 4a. Baseline = typical non-promoted volume for that product in that store.
#     Median, not mean, so a single spike does not inflate the baseline.
baseline <- panel %>%
  filter(!ON_PROMO) %>%
  group_by(UPC, STORE_NUM) %>%
  summarise(BASE_UNITS = median(UNITS), n_base_weeks = n(), .groups = "drop") %>%
  filter(n_base_weeks >= 10, BASE_UNITS > 0)   # need enough clean weeks

# 4b. Evaluate every promoted store-week against its baseline
events <- panel %>%
  filter(ON_PROMO) %>%
  inner_join(baseline, by = c("UPC", "STORE_NUM")) %>%
  mutate(
    COGS            = BASE_PRICE * (1 - GROSS_MARGIN),

    INCR_UNITS      = UNITS - BASE_UNITS,
    UPLIFT_PCT      = INCR_UNITS / BASE_UNITS,

    # Margin actually earned during the promoted week
    PROMO_MARGIN    = UNITS      * (PRICE      - COGS),
    # Margin that would have been earned with no promotion
    BASE_MARGIN     = BASE_UNITS * (BASE_PRICE - COGS),
    INCR_PROFIT     = PROMO_MARGIN - BASE_MARGIN,

    # The investment: revenue given up by discounting every unit sold
    PROMO_INVEST    = (BASE_PRICE - PRICE) * UNITS,
    ROI             = ifelse(PROMO_INVEST > 0, INCR_PROFIT / PROMO_INVEST, NA),

    # Breakeven uplift: the volume lift needed just to stand still.
    # Derived from setting INCR_PROFIT = 0.
    BREAKEVEN_UPLIFT = (BASE_PRICE - COGS) / (PRICE - COGS) - 1,
    BEATS_BREAKEVEN  = UPLIFT_PCT > BREAKEVEN_UPLIFT
  ) %>%
  filter(is.finite(ROI), PRICE > COGS)

cat("\nPromotional events evaluated:", nrow(events), "\n")

# 4c. THE HEADLINE TABLE: performance by promotional mechanic
roi_by_mechanic <- events %>%
  group_by(MECHANIC) %>%
  summarise(
    events            = n(),
    avg_discount      = mean(DISCOUNT_PCT),
    median_uplift     = median(UPLIFT_PCT),
    median_breakeven  = median(BREAKEVEN_UPLIFT),
    pct_profitable    = mean(INCR_PROFIT > 0),
    total_incr_profit = sum(INCR_PROFIT),
    total_investment  = sum(PROMO_INVEST),
    portfolio_roi     = sum(INCR_PROFIT) / sum(PROMO_INVEST),
    .groups = "drop"
  ) %>%
  arrange(desc(portfolio_roi))

cat("\n=== PROMOTIONAL ROI BY MECHANIC ===\n")
print(roi_by_mechanic)

# NOTE ON portfolio_roi: it is computed as total profit / total investment,
# NOT the average of individual ROIs. Averaging ratios overweights small
# events. Aggregating the numerator and denominator first, then dividing
# once, is what avoids that — a common aggregation mistake otherwise.

# 4d. Performance by category
roi_by_category <- events %>%
  group_by(CATEGORY) %>%
  summarise(
    events            = n(),
    median_uplift     = median(UPLIFT_PCT),
    pct_profitable    = mean(INCR_PROFIT > 0),
    portfolio_roi     = sum(INCR_PROFIT) / sum(PROMO_INVEST),
    .groups = "drop"
  ) %>%
  arrange(desc(portfolio_roi))

cat("\n=== PROMOTIONAL ROI BY CATEGORY ===\n")
print(roi_by_category)

# 4e. Does deeper discounting pay? The classic RGM question.
roi_by_depth <- events %>%
  mutate(DEPTH_BUCKET = cut(DISCOUNT_PCT,
                            breaks = c(-Inf, 0.05, 0.15, 0.25, 0.35, Inf),
                            labels = c("0-5%","5-15%","15-25%","25-35%","35%+"))) %>%
  group_by(DEPTH_BUCKET) %>%
  summarise(
    events         = n(),
    median_uplift  = median(UPLIFT_PCT),
    pct_profitable = mean(INCR_PROFIT > 0),
    portfolio_roi  = sum(INCR_PROFIT) / sum(PROMO_INVEST),
    .groups = "drop"
  )

cat("\n=== ROI BY DISCOUNT DEPTH ===\n")
print(roi_by_depth)

# 4f. Cross-check: mechanic x category, the actual recommendation grid
grid <- events %>%
  group_by(CATEGORY, MECHANIC) %>%
  summarise(events = n(),
            portfolio_roi = sum(INCR_PROFIT) / sum(PROMO_INVEST),
            .groups = "drop") %>%
  filter(events >= 50) %>%
  arrange(CATEGORY, desc(portfolio_roi))

cat("\n=== RECOMMENDATION GRID ===\n")
print(grid, n = 40)

# 4g. Margin sensitivity: does the ranking of mechanics survive changing the
# 30% margin assumption to 25% or 35%? Every ROI number above depends on
# that assumption, so a mechanic's verdict should hold across this range
# before being treated as a finding rather than "true under one specific
# assumption". Re-derives events at each margin from scratch rather than
# re-scaling INCR_PROFIT, so the PRICE > COGS filter etc. is recomputed
# consistently at each margin too.
roi_at_margin <- function(margin) {
  ev <- panel %>%
    filter(ON_PROMO) %>%
    inner_join(baseline, by = c("UPC", "STORE_NUM")) %>%
    mutate(
      COGS         = BASE_PRICE * (1 - margin),
      PROMO_MARGIN = UNITS      * (PRICE      - COGS),
      BASE_MARGIN  = BASE_UNITS * (BASE_PRICE - COGS),
      INCR_PROFIT  = PROMO_MARGIN - BASE_MARGIN,
      PROMO_INVEST = (BASE_PRICE - PRICE) * UNITS,
      ROI          = ifelse(PROMO_INVEST > 0, INCR_PROFIT / PROMO_INVEST, NA)
    ) %>%
    filter(is.finite(ROI), PRICE > COGS)

  ev %>% group_by(MECHANIC) %>%
    summarise(portfolio_roi = sum(INCR_PROFIT) / sum(PROMO_INVEST), .groups = "drop") %>%
    mutate(margin = margin)
}

margin_sensitivity <- bind_rows(roi_at_margin(0.25), roi_at_margin(0.30), roi_at_margin(0.35)) %>%
  tidyr::pivot_wider(names_from = margin, values_from = portfolio_roi, names_prefix = "roi_at_margin_")

cat("\n=== MARGIN SENSITIVITY: PORTFOLIO ROI BY MECHANIC AT 25% / 30% / 35% GROSS MARGIN ===\n")
print(margin_sensitivity)

# INTERPRETATION NOTE:
#  Only treat a mechanic's verdict as robust if its sign does not change
#  across 25%-35%. In this dataset: Feature+Display stays positive and
#  TPR-only stays deeply negative at every margin tested — safe to lead
#  with those two. Display-only and Feature-only cross zero somewhere in
#  this range, so their verdict is margin-assumption-dependent and should
#  be reported as such, not as a single settled number.


# ---------------------------------------------------------------------
# 5. CHARTS FOR THE DECK
# ---------------------------------------------------------------------
theme_set(theme_minimal(base_size = 12))

p1 <- ggplot(elasticity_by_category,
             aes(x = reorder(CATEGORY, median_elasticity),
                 y = median_elasticity)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = -1, linetype = "dashed", colour = "red") +
  coord_flip() +
  labs(title = "Price elasticity by category",
       subtitle = "Below the dashed line: volume gain outweighs price given up",
       x = NULL, y = "Median own-price elasticity")
ggsave("chart_elasticity.png", p1, width = 8, height = 5, dpi = 150)

p2 <- ggplot(roi_by_mechanic,
             aes(x = reorder(MECHANIC, portfolio_roi), y = portfolio_roi)) +
  geom_col(fill = "darkorange") +
  geom_hline(yintercept = 0, colour = "grey30") +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  labs(title = "Return on promotional investment by mechanic",
       subtitle = "Incremental profit per euro of discount given away",
       x = NULL, y = "ROI")
ggsave("chart_roi_mechanic.png", p2, width = 8, height = 5, dpi = 150)

p3 <- events %>%
  filter(UPLIFT_PCT < quantile(UPLIFT_PCT, 0.99)) %>%
  ggplot(aes(x = DISCOUNT_PCT, y = UPLIFT_PCT, colour = BEATS_BREAKEVEN)) +
  geom_point(alpha = 0.15, size = 0.7) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  scale_colour_manual(values = c("FALSE" = "grey60", "TRUE" = "seagreen"),
                      name = "Beats breakeven") +
  facet_wrap(~ CATEGORY) +
  labs(title = "Discount depth vs volume uplift",
       subtitle = "Grey points are promotions that lost money",
       x = "Discount depth", y = "Volume uplift")
ggsave("chart_depth_vs_uplift.png", p3, width = 10, height = 7, dpi = 150)


# ---------------------------------------------------------------------
# 6. EXPORT FOR EXCEL AND POWERPOINT
# ---------------------------------------------------------------------
write.csv(elasticities,              "out_elasticity_by_product.csv",   row.names = FALSE)
write.csv(elasticity_by_category,    "out_elasticity_by_category.csv",  row.names = FALSE)
write.csv(elasticity_by_tier_summary,"out_elasticity_by_tier.csv",      row.names = FALSE)
write.csv(roi_by_mechanic,        "out_roi_by_mechanic.csv",        row.names = FALSE)
write.csv(roi_by_category,        "out_roi_by_category.csv",        row.names = FALSE)
write.csv(roi_by_depth,           "out_roi_by_depth.csv",           row.names = FALSE)
write.csv(grid,                   "out_recommendation_grid.csv",    row.names = FALSE)
write.csv(margin_sensitivity,     "out_margin_sensitivity.csv",     row.names = FALSE)

cat("\nDone. CSVs and PNGs written to:", getwd(), "\n")


# =====================================================================
# HOW TO READ THE RESULTS
# =====================================================================
# The goal is one clear commercial story. The most likely one, given the
# tables above:
#
#   "Feature + Display generates the highest return; TPR-only promotions
#    are close to value-destructive. In [category X], which is inelastic,
#    a large share of promoted weeks fail to beat breakeven. Reallocating
#    that investment toward [mechanic/category Y] would improve
#    promotional profitability without additional spend."
#
# That is a real RGM recommendation. It reports the decision the numbers
# support, not every number produced.
#
# POSSIBLE EXTENSIONS:
#  - Elasticity by SEG_VALUE_NAME is done in section 3b above (not as a
#    pooled regression — an earlier draft pooled all products in a
#    category into one regression, which produced implausible interaction
#    coefficients confounded by assortment differences between store
#    tiers. Section 3b fits the interaction per product instead, same
#    principle as the main elasticity model: don't pool SKUs with
#    different price levels and different true elasticities into one
#    regression).
#  - Separate base-price elasticity from promotional-discount elasticity
#    by using log(BASE_PRICE) and log(PRICE/BASE_PRICE) as two regressors.
#    Consumers respond differently to a permanent price and a temporary
#    cut, and RGM teams treat them as different levers.
#  - Test cannibalisation: when product A is promoted, what happens to
#    sibling products in the same category in the same store-week?
#
# KNOWN LIMITATIONS:
#  - The 30% gross margin is an assumption, not observed data; section 4g
#    tests how sensitive the conclusions are to it.
#  - Baseline via median non-promoted volume is a simplification. Real
#    RGM tools model the baseline with seasonality and trend.
#  - Promoted weeks are not randomly assigned; retailers tend to promote
#    what they already expect to sell, which biases the estimates.
# =====================================================================
