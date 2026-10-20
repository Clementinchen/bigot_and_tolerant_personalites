# =============================================================================
# MAIN TEXT – When the Collective Outweighs the Individual
# -----------------------------------------------------------------------------
# Reproduces all values, tables and figures reported in the main text
# (Studies 1–5).
#
# Input : ./1_analysis_data/*.rds  (created by "Data Wrangling.R";
#         ds4_geo.rds and base_europe.rds by "Study_4_geodata.R")
# Output: console (values as reported in the manuscript), ./4_plots/*.png
#
# Note: Cronbach's alphas (Table 1) are computed in "Data Wrangling.R",
#       because the analysis data sets no longer contain the single items.
# Runtime: the parametric bootstrap of the ICCs (Table 2) takes long,
#       especially for Study 4 (~340,000 observations).
# =============================================================================


# 0  SETUP --------------------------------------------------------------------

library(tidyverse)
library(lme4)
library(patchwork)
library(sf)
library(spdep)

set.seed(1234)                # bootstrap CIs
n_boot   <- 100               # parametric bootstrap iterations (ICC CIs)
data_dir <- "./1_analysis_data"
plot_dir <- "./4_plots"
dir.create(plot_dir, showWarnings = FALSE)

for (f in c("ds1_wde", "ds1_lng", "ds2_wde", "ds2_lng",
            "ds3_wde", "ds3_lng", "ds4_wde", "ds5_wde"))
  assign(f, readRDS(file.path(data_dir, paste0(f, ".rds"))))


# 0.1 Helper functions --------------------------------------------------------

# Print a labelled result
show <- function(title, x) {
  cat("\n==", title, "==\n")
  if (is.data.frame(x)) print(as.data.frame(x), row.names = FALSE, right = FALSE)
  else cat(x, sep = "\n")
}

msd <- function(x) sprintf("%.2f (%.2f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
num <- function(x) formatC(x, format = "d", big.mark = ",")
p_txt <- function(p) ifelse(p < .001, "p < .001", sprintf("p = %.3f", p))

# Sample description: N, gender (n, %), age M (SD)
describe_sample <- function(d) {
  g <- tolower(as.character(d$gender))
  n <- c(women = sum(g == "female", na.rm = TRUE), men = sum(g == "male", na.rm = TRUE))
  n <- c(n, "other / not reported" = nrow(d) - sum(n))
  sprintf("N = %s (%s); Mage = %.2f, SD = %.2f",
          num(nrow(d)),
          paste(sprintf("%s %s [%.1f%%]", num(n), names(n), 100 * n / nrow(d)), collapse = ", "),
          mean(d$age, na.rm = TRUE), sd(d$age, na.rm = TRUE))
}

# Pearson correlation as reported in the text
cor_txt <- function(x, y) {
  ct <- cor.test(x, y)
  sprintf("r = %.2f, %s, 95%% CI [%.2f, %.2f]",
          ct$estimate, p_txt(ct$p.value), ct$conf.int[1], ct$conf.int[2])
}

# Variance components as ICCs (proportion of total variance)
var_props <- function(m) {
  v <- as.data.frame(VarCorr(m))
  setNames(v$vcov / sum(v$vcov), v$grp)
}

icc_labels <- c("case"              = "Participant",
                "prj_target"        = "Target",
                "prj_target:case"   = "Target x Participant",
                "region_id:prj_target" = "Region x Target")

# One row of Table 2: ICCs [95% percentile bootstrap CI], b (SE), R² marginal / conditional
var_decomp <- function(m, digits = c(b = 2, se = 2)) {
  boot <- bootMer(m, var_props, nsim = n_boot, type = "parametric", use.u = FALSE)
  ci   <- apply(boot$t, 2, quantile, probs = c(.025, .975), na.rm = TRUE)
  est  <- var_props(m)
  icc  <- setNames(sprintf("%.2f [%.2f, %.2f]", est, ci[1, ], ci[2, ]), icc_labels[names(est)])
  fe   <- coef(summary(m))
  r2   <- performance::r2_nakagawa(m)
  tibble(!!!as.list(icc[!is.na(names(icc))]),
         "b (SE)" = if (nrow(fe) > 1)
           sprintf("%.*f (%.*f)", digits["b"], fe[2, 1], digits["se"], fe[2, 2]) else "-",
         "Marginal / Cond. R2" = sprintf("%.3f / %.3f", r2$R2_marginal, r2$R2_conditional))
}

# Fit the Table 2 models (one per fixed effect) and decompose their variance
table2 <- function(data, random, predictors, digits = c(b = 2, se = 2)) {
  map(predictors, \(p) lmer(reformulate(c(p, random), "prj_rating"), data = data)) |>
    map(var_decomp, digits = digits) |>
    list_rbind(names_to = "Model") |>
    relocate(any_of(unname(icc_labels)), .after = Model)
}

predictors <- c("Random Effects Only"                    = "1",
                "Fixed effect: RWA"                      = "rwa_scl",
                "Fixed effect: SDO"                      = "sdo_scl",
                "Fixed effect: Political Self-Placement" = "polid_scl")


# =============================================================================
# 1  FIGURE 1 (conceptual illustration, simulated distributions)
# =============================================================================

group_cols <- c(A = "#F8766D", B = "#0072B2")

plot_targets <- function(means, sds, x_axis = FALSE) {
  grp  <- names(group_cols)
  peak <- dnorm(0, sd = min(sds))

  dens <- map2_dfr(means, sds, \(m, s) {
    x <- seq(m - 4 * s, m + 4 * s, length.out = 400)
    tibble(x = x, y = dnorm(x, m, s))
  }, .id = "i") |> mutate(Group = grp[as.integer(i)])

  # -1SD / M / +1SD per group; A above, B below
  marks <- tibble(Group = rep(grp, each = 3),
                  x     = c(means[1] + c(-1, 0, 1) * sds[1], means[2] + c(-1, 0, 1) * sds[2]),
                  y     = rep(c(0.24, 0.12) * peak, each = 3),
                  label = rep(c("-1SD", "M", "+1SD"), 2)) |>
    mutate(label_y = y + ifelse(Group == grp[1], 1, -1) * 0.07 * peak)
  bar_y <- 1.08 * peak

  p <- ggplot(dens, aes(x, y, colour = Group, fill = Group)) +
    geom_area(alpha = .3, linewidth = 1.2, position = "identity") +
    geom_line(data = marks, linetype = "dashed", linewidth = 1, show.legend = FALSE) +
    geom_segment(data = marks, aes(xend = x, y = y - 0.025 * peak, yend = y + 0.025 * peak),
                 linewidth = 1, show.legend = FALSE) +
    geom_text(data = marks, aes(y = label_y, label = label),
              fontface = "bold.italic", size = 3.5, show.legend = FALSE) +
    annotate("segment", x = means, xend = means, y = 0, yend = bar_y,
             linetype = "dotted", colour = "grey50") +
    annotate("segment", x = means, xend = means,
             y = bar_y - 0.03 * peak, yend = bar_y + 0.03 * peak, linewidth = 1) +
    annotate("segment", x = means[1], xend = means[2], y = bar_y, yend = bar_y, linewidth = 1) +
    scale_colour_manual(values = group_cols, aesthetics = c("colour", "fill"),
                        name = "Prejudice Target Group") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    coord_cartesian(xlim = c(-10, 10)) +
    theme_void(base_size = 12)

  if (x_axis) p <- p +
    labs(x = "Latent Valence Dimension") +
    theme(axis.line.x  = element_line(arrow = arrow(length = unit(.3, "cm"), type = "closed")),
          axis.title.x = element_text(face = "bold", margin = margin(t = 6)))
  p
}

fig1 <- plot_targets(c(-5, 5), c(1, 1)) / plot_targets(c(-2, 2), c(3, 3), x_axis = TRUE) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom", plot.tag = element_text(face = "bold"))

ggsave(file.path(plot_dir, "fig1.png"), fig1, width = 8, height = 5, dpi = 300, bg = "white")


# =============================================================================
# 2  STUDIES 1–3
# =============================================================================

## 2.1 Participants ----
ds3_wde <- ds3_wde |> mutate(gender = c("female", "male")[gender])   # 1 = female, 2 = male

show("Studies 1-3: Participants",
     c(paste("Study 1:", describe_sample(ds1_wde)),
       paste("Study 2:", describe_sample(ds2_wde)),
       paste("Study 3:", describe_sample(ds3_wde))))

## 2.2 Table 1: M (SD) ----
# Study 3: person mean across the three waves (participants with all waves)
ds3_pm <- ds3_lng |>
  distinct(case, wave, rwa, sdo, polid) |>
  summarise(rwa = mean(rwa), sdo = mean(sdo), polid = first(polid), .by = case)

show("Table 1: Studies 1-3, M (SD)",
     tibble(Measure = c("RWA", "SDO", "Political Self-Placement"),
            "Study 1" = map_chr(ds1_wde[c("rwa", "sdo", "polid")], msd),
            "Study 2" = map_chr(ds2_wde[c("rwa", "sdo", "polid")], msd),
            "Study 3" = map_chr(ds3_pm[c("rwa", "sdo", "polid")], msd)))

## 2.3 Figure 2: within-/between-target variance and correlations ----
fig2_dat <- list("Study 1" = ds1_lng, "Study 2" = ds2_lng, "Study 3" = ds3_lng) |>
  map(\(d) select(d, rwa, sdo, prj_target, prj_rating)) |>
  list_rbind(names_to = "Study") |>
  mutate(target    = str_to_title(str_replace_all(str_remove(prj_target, "^prj_"), "_", " ")),
         prejudice = as.numeric(scale(prj_rating)), .by = Study)   # z-score within study

fig2_cor <- function(x, y) {
  ct <- cor.test(x, y)
  tibble(r = ct$estimate, low = ct$conf.int[1], high = ct$conf.int[2])
}

fig2_stats <- fig2_dat |>
  reframe(mean = mean(prejudice, na.rm = TRUE),
          sd   = sd(prejudice, na.rm = TRUE),
          RWA  = fig2_cor(rwa, prejudice),
          SDO  = fig2_cor(sdo, prejudice),
          .by  = c(Study, target)) |>
  mutate(target = fct_reorder(target, mean, .fun = base::mean))   # order by mean across studies

# Superscripts: studies that included the target
target_labels <- fig2_stats |>
  summarise(lab = paste0("'", first(target), "'^'", paste(str_extract(Study, "\\d"), collapse = ","), "'"),
            .by = target) |>
  deframe()

pd         <- position_dodge(width = .6)
study_cols <- c("Study 1" = "#4A7169", "Study 2" = "#CB87B4", "Study 3" = "#EFB344")

fig2_panel <- function(x, xmin, xmax, xlab, vline = 0) {
  ggplot(fig2_stats, aes({{ x }}, target, colour = Study, shape = Study)) +
    geom_vline(xintercept = vline, linetype = 2, colour = "grey70") +
    geom_errorbar(aes(xmin = {{ xmin }}, xmax = {{ xmax }}), width = .15, position = pd,
                  orientation = "y") +
    geom_point(size = 2, position = pd) +
    scale_colour_manual(values = study_cols) +
    labs(x = xlab, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", panel.grid.major.y = element_blank())
}

no_y <- theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

fig2 <- (fig2_panel(mean, mean - sd, mean + sd, "Mean (±1 SD)") +
           scale_y_discrete(labels = \(x) parse(text = target_labels[x])) |
         fig2_panel(RWA$r, RWA$low, RWA$high, "r (RWA)") + no_y |
         fig2_panel(SDO$r, SDO$low, SDO$high, "r (SDO)") + no_y) +
  plot_layout(widths = c(1.5, 1, 1), guides = "collect") &
  theme(legend.position = "bottom")

ggsave(file.path(plot_dir, "fig2.png"), fig2, width = 8, height = 8, dpi = 300, bg = "white")

## 2.4 Table 2: variance decomposition ----
# Each model uses all available observations (lme4 default: rows with missing
# values in the model variables are dropped per model)
show("Table 2: Study 1", table2(ds1_lng, c("(1|case)", "(1|prj_target)"), predictors))
show("Table 2: Study 2", table2(ds2_lng, c("(1|case)", "(1|prj_target)"), predictors))

# Study 3: repeated measures -> additional target x participant component
show("Table 2: Study 3",
     table2(ds3_lng, c("(1|case)", "(1|prj_target)", "(1|prj_target:case)"), predictors))


# =============================================================================
# 3  STUDY 4 (ESS round 4)
# =============================================================================

prj_4 <- c(immigrants = "Immigrants", gay = "Gay people", unemployed = "Unemployed people",
           women = "Women", age20 = "People in their 20s",
           age70 = "People over 70")

## 3.1 Participants ----
show("Study 4: Participants",
     c(describe_sample(ds4_wde),
       sprintf("%d countries, %d regions", n_distinct(ds4_wde$cntry_lbl),
               n_distinct(ds4_wde$region_id, na.rm = TRUE))))

## 3.2 Table 1: M (SD) ----
show("Table 1: Study 4, M (SD)",
     tibble(Measure = c("Traditionalism", "Political Self-Placement", paste("Prejudice:", prj_4)),
            "M (SD)" = map_chr(ds4_wde[c("trad_raw", "polid_raw", paste0("prj_", names(prj_4), "_raw"))], msd)))

## 3.3 Correlations reported in the text ----
show("Study 4: Correlations",
     with(ds4_wde, c(
       paste("Gay people      ~ traditionalism:           ", cor_txt(trad_raw,  prj_gay_raw)),
       paste("Gay people      ~ political self-placement: ", cor_txt(polid_raw, prj_gay_raw)),
       paste("People over 70  ~ traditionalism:           ", cor_txt(trad_raw,  prj_age70_raw)),
       paste("People over 70  ~ political self-placement: ", cor_txt(polid_raw, prj_age70_raw)))))

## 3.4 Figure 3: region, country and grand means per target ----
fig3_dat <- ds4_wde |>
  select(cntry_lbl, region_id, all_of(paste0("prj_", names(prj_4), "_lin"))) |>
  pivot_longer(starts_with("prj_"), names_to = "target", values_to = "rating") |>
  mutate(target = fct_recode(target, "Old (70)" = "prj_age70_lin", "Young (20)" = "prj_age20_lin",
                             "Gay/Lesbian" = "prj_gay_lin", "Women" = "prj_women_lin",
                             "Immigrants" = "prj_immigrants_lin", "Unemployed" = "prj_unemployed_lin"),
         target = fct_reorder(target, rating, .fun = mean, .na_rm = TRUE))

fig3_region  <- fig3_dat |> filter(!is.na(region_id)) |>
  summarise(m = mean(rating, na.rm = TRUE), .by = c(region_id, target))
fig3_country <- fig3_dat |> summarise(m = mean(rating, na.rm = TRUE), .by = c(cntry_lbl, target))
fig3_grand   <- fig3_dat |> summarise(m = mean(rating, na.rm = TRUE), sd = sd(rating, na.rm = TRUE),
                                      .by = target)

fig3 <- ggplot(mapping = aes(target, m)) +
  geom_line(data = fig3_region,  aes(group = region_id, colour = "Region mean"),
            alpha = .25, linewidth = .3) +
  geom_line(data = fig3_country, aes(group = cntry_lbl, colour = "Country mean"), alpha = .6) +
  geom_errorbar(data = fig3_grand, aes(ymin = m - sd, ymax = m + sd), width = .1) +
  geom_line(data = fig3_grand, aes(group = 1, colour = "Grand mean"), linewidth = 1) +
  geom_point(data = fig3_grand, aes(colour = "Grand mean"), shape = 18, size = 4) +
  scale_colour_manual(values = c("Country mean" = "#2A7BDE", "Grand mean" = "black",
                                 "Region mean" = "grey55"), name = NULL) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = NULL, y = "Prejudice rating (0–1)",
       caption = "Grey lines = region profiles · blue lines = country profiles ·\nblack = grand mean per item, error bars = ±1 SD (within-target variance)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

ggsave(file.path(plot_dir, "fig3.png"), fig3, width = 10, height = 5, dpi = 300, bg = "white")

## 3.5 ICCs of null models: region vs. country ----
show("Study 4: ICC region vs. country (null models per target)",
     map(names(prj_4), \(t) {
       icc <- map_dbl(c(Region = "region_id", Country = "cntry_lbl"), \(g)
         var_props(lmer(reformulate(sprintf("(1|%s)", g), paste0("prj_", t, "_lin")), data = ds4_wde))[g])
       tibble(Target = prj_4[t], Region = sprintf("%.3f", icc["Region"]),
              Country = sprintf("%.3f", icc["Country"]))
     }) |> list_rbind())

## 3.6 Table 2: variance decomposition ----
# Prejudice on a common 0–1 metric; predictors z-standardised
ds4_lng <- ds4_wde |>
  select(case, region_id, trad_scl, polid_scl, all_of(paste0("prj_", names(prj_4), "_lin"))) |>
  pivot_longer(starts_with("prj_"), names_to = "prj_target", values_to = "prj_rating")

re_nested <- c("(1|case)", "(1|prj_target)", "(1|region_id)", "(1|region_id:prj_target)")

show("Table 2: Study 4",
     bind_rows(
       table2(ds4_lng, c("(1|case)", "(1|prj_target)"),
              c("Random Effects Only: No Regional Nesting" = "1")),
       table2(ds4_lng, re_nested,
              c("Random Effects Only: Regional Nesting"  = "1",
                "Fixed effect: Traditionalism"           = "trad_scl",
                "Fixed effect: Political Self-Placement" = "polid_scl"),
              digits = c(b = 3, se = 4))))

## 3.7 Spatial analysis: Moran's I and Figure 4 ----
# Region polygons and background map are created in "Study_4_geodata.R"
ds4_geo     <- readRDS(file.path(data_dir, "ds4_geo.rds"))
base_europe <- readRDS(file.path(data_dir, "base_europe.rds"))

ds4_sp <- ds4_wde |>
  mutate(region_id = as.character(region_id)) |>
  filter(region_id %in% ds4_geo$region_id)

show("Study 4: Spatial analysis sample",
     c(sprintf("%d of %d regions matched to geographical information",
               nrow(ds4_geo), n_distinct(ds4_wde$region_id, na.rm = TRUE)),
       describe_sample(ds4_sp)))

# Regional prejudice means (0–1 metric) merged onto the region polygons
geo4 <- ds4_geo |>
  left_join(summarise(ds4_sp, across(all_of(paste0("prj_", names(prj_4), "_lin")),
                                     \(x) mean(x, na.rm = TRUE)), .by = region_id),
            by = "region_id")

# Contiguity weights: regions sharing a border at any point are neighbours (queen).
# snap = 1 km closes the gaps between NUTS and GADM (Ukraine) borders;
# regions without a shared border (islands) are linked to the nearest region (centroids)
nb      <- suppressWarnings(poly2nb(geo4, queen = TRUE, snap = 1000))   # islands handled below
nearest <- knearneigh(st_coordinates(st_centroid(st_geometry(geo4))), k = 1)$nn
for (i in which(card(nb) == 0)) nb[[i]] <- nearest[i, 1]
nb <- make.sym.nb(nb)
lw <- nb2listw(nb, style = "W")

show("Study 4: Moran's I (regional prejudice means)",
     map(names(prj_4), \(t) {
       mt <- moran.test(geo4[[paste0("prj_", t, "_lin")]], lw)
       tibble(Target = prj_4[t], "Moran's I" = sprintf("%.2f", mt$estimate[["Moran I statistic"]]),
              p = p_txt(mt$p.value))
     }) |> list_rbind())

# Figure 4: regional prejudice per target
fig4 <- geo4 |>
  pivot_longer(starts_with("prj_"), names_to = "target", values_to = "value") |>
  mutate(target = factor(prj_4[str_remove_all(target, "^prj_|_lin$")], levels = prj_4)) |>
  ggplot() +
  geom_sf(data = base_europe, fill = "grey92", colour = "white", linewidth = .1) +
  geom_sf(aes(fill = value), colour = "white", linewidth = .05) +
  facet_wrap(~ target, nrow = 2) +
  coord_sf(xlim = c(2.5e6, 7.4e6), ylim = c(0.9e6, 5.4e6), crs = 3035, expand = FALSE) +
  scale_fill_viridis_c(name = "Prejudice\n(0–1)", limits = c(0, 1), option = "magma") +
  theme_void(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))

ggsave(file.path(plot_dir, "fig4.png"), fig4, width = 10, height = 7, dpi = 300, bg = "white")


# =============================================================================
# 4  STUDY 5 (ESS round 9)
# =============================================================================

# Education (ES-ISCED 1–7) as numeric control; 55 = "Other" is not ordinal -> NA
ds5 <- ds5_wde |> mutate(education_num = na_if(as.numeric(as.character(education_num)), 55))

## 4.1 Participants ----
n_rgn <- count(ds5, region_cde)$n
show("Study 5: Participants",
     c(describe_sample(ds5),
       sprintf("%d countries, %d regions", n_distinct(ds5$cntry_lbl), n_distinct(ds5$region_cde)),
       sprintf("Observations per region: M = %.2f, SD = %.2f, range = %d–%s",
               mean(n_rgn), sd(n_rgn), min(n_rgn), num(max(n_rgn)))))

## 4.2 Table 1: M (SD) ----
show("Table 1: Study 5, M (SD)",
     tibble(Measure = c("Traditionalism", "Political Self-Placement",
                        "Prejudice: Immigrants", "Prejudice: Gay people"),
            "M (SD)" = map_chr(ds5[c("trad", "polid", "anti_mig", "anti_gay")], msd)))

## 4.3 ICCs of null models ----
show("Study 5: ICC (null models, region)",
     sprintf("%-15s ICC = %.2f", c("Anti-gay:", "Anti-immigrant:"),
             map_dbl(c("anti_gay", "anti_mig"), \(y)
               var_props(lmer(reformulate("(1|region_cde)", y), data = ds5, REML = FALSE))["region_cde"])))

## 4.4 Multilevel models ----
# Level 1: traditionalism and prejudice toward the other target, centred within region
# Level 2: regional prejudice toward the other target (grand-mean centred)
controls <- "polid + age + gender + education_num"

f_gay <- anti_gay ~ cwc_trad + cwc_anti_mig + rgn_lvl_anti_mig.gmc + (cwc_trad + cwc_anti_mig | region_cde)
f_mig <- anti_mig ~ cwc_trad + cwc_anti_gay + rgn_lvl_anti_gay.gmc + (cwc_trad + cwc_anti_gay | region_cde)

mods_5 <- list(
  "Anti-Gay"       = f_gay,
  "Anti-Immigrant" = f_mig,
  "Anti-Gay (no controls)"       = f_gay,
  "Anti-Immigrant (no controls)" = f_mig) |>
  imap(\(f, nm) {
    if (!str_detect(nm, "no controls")) f <- update(f, paste(". ~ . +", controls))
    lmer(f, data = ds5, REML = FALSE)
  })

# Standardised coefficients (refit on z-standardised variables), Wald CIs
coefs_5 <- mods_5 |>
  map(\(m) as_tibble(parameters::model_parameters(m, standardize = "refit", effects = "fixed"))) |>
  list_rbind(names_to = "Model") |>
  filter(Parameter != "(Intercept)") |>
  mutate(term = c(cwc_trad             = "Traditionalism",
                  cwc_anti_mig         = "Individual Prejudice",
                  cwc_anti_gay         = "Individual Prejudice",
                  rgn_lvl_anti_mig.gmc = "Contextual Prejudice",
                  rgn_lvl_anti_gay.gmc = "Contextual Prejudice",
                  polid                = "Political Self-Placement",
                  age                  = "Age",
                  education_num        = "Education",
                  genderFemale         = "Gender [Female]")[Parameter])

show("Study 5: Standardised coefficients",
     coefs_5 |>
       filter(term %in% c("Contextual Prejudice", "Traditionalism", "Individual Prejudice")) |>
       transmute(Model, Predictor = term,
                 Result = sprintf("B = %.2f, SE = %.2f, %s, 95%% CI [%.2f, %.2f]",
                                  Coefficient, SE, p_txt(p), CI_low, CI_high)))

## 4.5 Figure 5: coefficients of the models with controls ----
fig5 <- coefs_5 |>
  filter(Model %in% c("Anti-Gay", "Anti-Immigrant")) |>
  mutate(term = factor(term, levels = rev(c("Traditionalism", "Individual Prejudice",
                                            "Contextual Prejudice", "Political Self-Placement",
                                            "Age", "Education", "Gender [Female]")))) |>
  ggplot(aes(Coefficient, term, colour = Model)) +
  geom_point(position = position_dodge(width = .3), size = 3) +
  geom_errorbar(aes(xmin = CI_low, xmax = CI_high), position = position_dodge(width = .3),
                width = .2, orientation = "y") +
  scale_colour_manual(values = c("Anti-Gay" = "#990000", "Anti-Immigrant" = "#336699")) +
  labs(x = "Standardized Coefficient", y = "Predictor Variables") +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.y = element_text(face = "bold"),
        axis.title = element_text(size = 14))

ggsave(file.path(plot_dir, "fig5.png"), fig5, width = 10, height = 8, dpi = 300, bg = "white")


# R SESSION -------------------------------------------------------------------
sessionInfo()
