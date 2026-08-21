## ============================================================================
## Spain Wildfires: The Frequency-Severity Paradox
## A statistical and spatial analysis of large-fire concentration in Spain
##
## Author: Sergio Romera Martínez
## Purpose: Portfolio piece / technical writing sample
##
## METHOD NOTE ON DATA HONESTY
## ----------------------------------------------------------------------------
## This is NOT a continuous 1968-2025 annual reconstruction. Spain's official
## fire database (EGIF, Ministry for Ecological Transition - MITECO) is not
## openly downloadable in bulk without a data request / registration process.
## Instead, this analysis uses verified, individually-cited data points drawn
## from official or officially-sourced publications (EFFIS/Copernicus annual
## reports, MITECO-EGIF figures as reported in peer-reviewed/press analyses).
## Every number in data/*.csv carries an explicit source column. Where the
## underlying series is incomplete, the analysis uses the SAME anomaly /
## reference-climatology method that EFFIS itself uses operationally
## (comparing a given year against a fixed reference-period average), rather
## than fitting a regression line through sparse points, which would overstate
## statistical confidence.
## ============================================================================

library(tidyverse)
library(sf)
library(scales)
library(cowplot)

dir.create("output", showWarnings = FALSE)

# ---- THEME ------------------------------------------------------------------
theme_fire <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 17, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 12, color = "grey30", margin = margin(b = 12)),
    plot.caption = element_text(size = 8, color = "grey50", hjust = 0, margin = margin(t = 10)),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 11),
    legend.position = "top"
  )

fire_red   <- "#C1440E"
fire_amber <- "#E8A317"
ash_grey   <- "#4A4A4A"

# ggplot does NOT auto-wrap plot titles/subtitles/captions - long strings get
# clipped by the device viewport unless wrapped explicitly. This helper fixes
# that systematically across every chart below.
wrap_txt <- function(x, width) paste(strwrap(x, width = width), collapse = "\n")

# ==============================================================================
# PART 0 — THE 2026 SEASON, AS OF 17 AUGUST 2026
# Two separate mega-fires define Spain's 2026 season so far:
#   1. Avila-Madrid (started 22 Jul, merged 24 Jul): 77,000 ha, 280 km
#      perimeter, satellite-perimeter derived (~30 Jul); stabilized 6 Aug;
#      the largest wildfire ever recorded in Spain.
#   2. Niebla, Huelva (started 6 Aug): stabilized 16 Aug after 10 days, 210
#      km perimeter. CRITICAL DISTINCTION, stated repeatedly by Andalusian
#      officials themselves: the >38,000 ha figure is the area ENCLOSED BY
#      THE PERIMETER, not confirmed burned area - unburned "islands" up to
#      400 ha exist inside it, and the definitive Copernicus satellite
#      report was still pending as of 17 Aug. The last VALIDATED Copernicus
#      delineation (EMSR913) found was only 6,456.7 ha, dated 9 Aug - already
#      badly outdated by the time of stabilization. Both figures are shown,
#      clearly labeled, rather than picking one and hiding the gap.
#
# METHODOLOGY NOTE: EFFIS (satellite) figures are PRIMARY throughout, MITECO
# (ground reporting) is kept only as an explicitly labeled secondary
# reference. EU-wide totals come DIRECTLY from the JRC's own official EFFIS
# current-situation page - the single most authoritative source used here.
#
# VOLATILITY, CONFIRMED AGAIN: at 5 Aug, Spain's 2026 EFFIS total (222,783 ha)
# was already 4.9x the same-date 2025 figure. By 12 Aug, Spain reached
# 265,457 ha - but 2025 had ALSO surged by then (144,158 ha in that single
# week alone), closing the gap to roughly 1.4x (derived, see CSV). EU-wide,
# the JRC's own data shows the same reversal: 2026 was AHEAD of 2025 at 5
# Aug, then BEHIND 2025 again by 13 Aug. This is shown explicitly below with
# three checkpoints, not smoothed into one headline ratio.
# ==============================================================================

crisis <- read_csv("data/crisis_2026.csv", show_col_types = FALSE)
get_val <- function(m) crisis$value[crisis$metric == m]
gv_num <- function(m) as.numeric(get_val(m))

# --- 0a. The two mega-fires, side by side: perimeter-enclosed vs last-validated-satellite area
fires_df <- tibble(
  fire = factor(c("\u00c1vila-Madrid\n(stabilized 6 Aug)", "Niebla, Huelva\n(stabilized 16 Aug)"),
                levels = c("\u00c1vila-Madrid\n(stabilized 6 Aug)", "Niebla, Huelva\n(stabilized 16 Aug)")),
  metric_type = c("Satellite-perimeter figure\n(most current available)", "Satellite-perimeter figure\n(most current available)"),
  ha = c(gv_num("burgohondo_complex_ha_final"), gv_num("niebla_area_within_perimeter_ha"))
)

p0a <- ggplot(fires_df, aes(x = fire, y = ha, fill = fire)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(aes(label = paste0(comma(ha), " ha")), vjust = -0.6, size = 5, fontface = "bold", color = ash_grey) +
  scale_fill_manual(values = c(fire_red, fire_amber)) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.22))) +
  labs(
    title = wrap_txt("Spain's two mega-fires of 2026, both now stabilized", 36),
    subtitle = wrap_txt("\u00c1vila-Madrid: 280 km perimeter, satellite-perimeter derived (~30 Jul). Niebla: 210 km perimeter; its 38,000 ha is the AREA ENCLOSED, explicitly NOT confirmed burned area per Junta de Andalucia - the definitive Copernicus report was still pending as of 17 Aug", 56),
    x = NULL, y = "Hectares (see caption for exact definition per fire)",
    caption = wrap_txt(paste0(
      "\u00c1vila-Madrid: MITECO/EFFIS joint attribution via El Diario (~30 Jul). Niebla: operational perimeter estimate, Junta de Andalucia (15 Aug) - ",
      "the last VALIDATED Copernicus satellite delineation for Niebla was only ", gv_num("niebla_last_validated_satellite_delineation_ha"), " ha (9 Aug), now outdated. See README."
    ), 88)
  ) +
  theme_fire

ggsave("output/00a_burgohondo_share.png", p0a, width = 9, height = 7, dpi = 200, bg = "white")

# --- 0b. 2026 vs 2025, same-date national comparison - EFFIS (satellite) ONLY,
# methodologically matched. THREE checkpoints now, not two, to make the
# volatility fully visible: the gap widens, then narrows sharply, as both
# years' seasons unfold unevenly.
yoy_df <- tibble(
  checkpoint = factor(c("22 Jul 2025", "22 Jul 2026", "5 Aug 2025", "5 Aug 2026", "12 Aug 2025*", "12 Aug 2026"),
                       levels = c("22 Jul 2025", "22 Jul 2026", "5 Aug 2025", "5 Aug 2026", "12 Aug 2025*", "12 Aug 2026")),
  yr = factor(c("2025", "2026", "2025", "2026", "2025", "2026"), levels = c("2025", "2026")),
  ha = c(gv_num("national_ytd_ha_22jul_2025_effis"), gv_num("national_ytd_ha_22jul_effis"),
         gv_num("national_ytd_ha_5aug_2025_effis"), gv_num("national_ytd_ha_5aug_effis"),
         gv_num("national_ytd_ha_12aug_2025_effis_derived"), gv_num("national_ytd_ha_12aug_effis"))
)

p0b <- ggplot(yoy_df, aes(x = checkpoint, y = ha, fill = yr)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = comma(ha)), vjust = -0.6, size = 3.6, fontface = "bold", color = ash_grey) +
  scale_fill_manual(values = c("2025" = fire_amber, "2026" = fire_red), name = NULL) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = wrap_txt("The 2025-vs-2026 gap widens, then narrows sharply", 38),
    subtitle = wrap_txt("Spain, cumulative burnt area, EFFIS satellite data at three checkpoints. The ratio moved from 3.4x to 4.9x to roughly 1.4x in three weeks, as 2025's own season surged too", 58),
    x = NULL, y = "Hectares burnt (cumulative)",
    caption = wrap_txt("Source: EFFIS (Copernicus/JRC), via ScienceMediaCentre.es, TheObjective.com and Diario en Positivo. *12 Aug 2025 figure is DERIVED (5 Aug 2025 total + EFFIS-reported 6-12 Aug 2025 weekly increase), not a single direct citation - see README and CSV.", 88)
  ) +
  theme_fire

ggsave("output/00b_2026_vs_2025.png", p0b, width = 9.5, height = 6, dpi = 200, bg = "white")

# --- 0c. Satellite (EFFIS) vs ground (MITECO): the same season, two totals, same date
# This is not an error to hide - it's a real, officially-acknowledged
# methodological divergence (the JRC's own page states EFFIS figures "may
# differ from those reported by national monitoring systems"). Showing both,
# for the SAME date, clearly labeled, is more rigorous than picking one.
method_df <- tibble(
  source = factor(c("MITECO\n(ground report,\nto 5 Aug)", "EFFIS\n(satellite,\nto 5 Aug)"),
                   levels = c("MITECO\n(ground report,\nto 5 Aug)", "EFFIS\n(satellite,\nto 5 Aug)")),
  ha = c(gv_num("national_ytd_ha_5aug_miteco_ground"), gv_num("national_ytd_ha_5aug_effis"))
)

p0c <- ggplot(method_df, aes(x = source, y = ha, fill = source)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(aes(label = comma(ha)), vjust = -0.6, size = 5, fontface = "bold", color = ash_grey) +
  scale_fill_manual(values = c(ash_grey, fire_red)) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = wrap_txt("Two official methods, same date, two different totals", 38),
    subtitle = wrap_txt("Spain's cumulative burnt area to 5 Aug 2026, as reported by ground monitoring (MITECO) vs satellite mapping (EFFIS/Copernicus)", 50),
    x = NULL, y = "Hectares burnt (cumulative)",
    caption = wrap_txt("Sources: MITECO via El Diario Cantabria/Publico; EFFIS via Diario en Positivo (both 5 Aug 2026). This divergence is expected and officially acknowledged by the JRC. See README.", 80)
  ) +
  theme_fire

ggsave("output/00c_effis_vs_miteco.png", p0c, width = 8, height = 5.5, dpi = 200, bg = "white")

# --- 0d. THE REVERSAL: as of 17 Aug, 2026 fell BELOW 2025 for the first time
# this season (MITECO ground-reporting, kept separate from the EFFIS series
# above rather than mixed into it - same-methodology comparison only).
reversal_df <- tibble(
  yr = factor(c("2025\n(to 17 Aug)", "2026\n(to 17 Aug)"), levels = c("2025\n(to 17 Aug)", "2026\n(to 17 Aug)")),
  ha = c(295580, 254730)
)

p0d <- ggplot(reversal_df, aes(x = yr, y = ha, fill = yr)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(aes(label = comma(ha)), vjust = -0.6, size = 5, fontface = "bold", color = ash_grey) +
  scale_fill_manual(values = c(fire_amber, fire_red)) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = wrap_txt("For the first time this season, 2026 fell below 2025", 38),
    subtitle = wrap_txt("Spain, cumulative burnt area to 17 August, MITECO ground-reporting (own methodology, not blended with the EFFIS series above) - 2026 is now running 13.8% below the same date in 2025", 58),
    x = NULL, y = "Hectares burnt (cumulative)",
    caption = wrap_txt("Source: MITECO (Ministerio para la Transicion Ecologica), official press release, 18 Aug 2026. This MITECO series is shown separately from the EFFIS checkpoints above, not combined with them, to avoid mixing measurement methods. See README.", 90)
  ) +
  theme_fire

ggsave("output/00d_reversal_17aug.png", p0d, width = 8, height = 5.5, dpi = 200, bg = "white")

hero <- plot_grid(p0a, p0d, ncol = 2, rel_widths = c(0.95, 1.1))
ggsave("output/hero_1200x628.png", hero, width = 14, height = 6.28, dpi = 100, bg = "white")

cat("\n=== 2026 SEASON STATUS (data verified 17 Aug 2026, EFFIS-primary methodology) ===\n")
cat(sprintf("Avila-Madrid complex (satellite-perimeter, ~30 Jul, 280km): %s ha\n", comma(gv_num("burgohondo_complex_ha_final"))))
cat(sprintf("Niebla, Huelva (perimeter-enclosed, NOT confirmed burned, 15 Aug, 210km): %s ha\n", comma(gv_num("niebla_area_within_perimeter_ha"))))
cat(sprintf("  -> last VALIDATED satellite delineation for Niebla (EMSR913, 9 Aug): %s ha (outdated)\n", gv_num("niebla_last_validated_satellite_delineation_ha")))
cat(sprintf("Spain EFFIS same-date comparison: 22 Jul: %.2fx | 5 Aug: %.2fx | 12 Aug: %.2fx (derived)\n",
            gv_num("national_ytd_ha_22jul_effis")/gv_num("national_ytd_ha_22jul_2025_effis"),
            gv_num("national_ytd_ha_5aug_effis")/gv_num("national_ytd_ha_5aug_2025_effis"),
            gv_num("national_ytd_ha_12aug_effis")/gv_num("national_ytd_ha_12aug_2025_effis_derived")))
cat(sprintf("EU-wide total to 13 Aug (JRC PRIMARY SOURCE): %s ha (2026) vs %s ha (2025, worst on record) - 2026 now BELOW 2025 pace, a reversal from 5 Aug\n",
            comma(gv_num("eu_total_ha_13aug2026_jrc_official")), comma(gv_num("eu_total_ha_13aug2025_jrc_official"))))
cat("NOTE: the 2025-vs-2026 ratio moved from 3.4x -> 4.9x -> ~1.4x across three checkpoints in three\n")
cat("weeks. This is disclosed explicitly as the finding, not smoothed into one headline number.\n\n")

# ==============================================================================
# PART 1 — ANOMALY ANALYSIS (completed seasons, MITECO-consistent)
# Same logic EFFIS itself uses operationally: compare a year against its own
# reference-period climatology, rather than force a trend line through
# sparse points. Reference average used here: MITECO's own last-decade
# figure (~105,600 ha/yr), NOT an EFFIS figure - see methodology note below.
#
# DATA CORRECTION, 19 Aug 2026: a prior version of this chart showed 2017 at
# ~540,000 ha and included 2003 (425,000 ha) and 2005 (350,000 ha) as
# benchmark years, sourced to EFFIS/World Weather Attribution. On review
# (prompted by a direct question about whether 2017 really burned more than
# 2025), the 2017 figure was found to be WRONG: it was Portugal's 2017 total
# (from the October 2017 Ophelia-driven fires, which were catastrophic in
# Portugal and comparatively much smaller in Spain), not Spain's. Spain's
# actual MITECO-official 2017 total is 183,498.68 ha - dramatically lower,
# and correctly LOWER than 2025 as intuition would suggest. 2003 and 2005
# were REMOVED rather than replaced with an unverified guess, since no
# MITECO-consistent figure for either year could be confidently located in
# the time available. The full benchmark-year series was also rebuilt to
# use ONE consistent source (MITECO) throughout, instead of mixing EFFIS and
# MITECO figures across different years, which was a real methodological
# weakness in the prior version even beyond the 2017 error. See README for
# the full correction note.
#

# 2026 is now INCLUDED, per request, as a clearly-marked provisional bar:
# the season is not over (runs to mid-October), so this is a minimum, not a
# final figure, and is visually and textually distinguished from the
# completed years rather than presented as equivalent to them.
# ==============================================================================

burnt_all <- read_csv("data/annual_burnt_area_spain.csv", show_col_types = FALSE) %>%
  filter(!is.na(burnt_area_ha))

ref_avg <- 105614  # MITECO last-decade average, Spain (ha/yr), as reported alongside the 2025 "worst season" analysis

burnt_all <- burnt_all %>%
  mutate(
    anomaly_multiple = burnt_area_ha / ref_avg,
    status = if_else(year == 2026, "2026 - PROVISIONAL\n(year not over)", "Completed season")
  )

burnt <- burnt_all %>% filter(year != 2026)  # completed years only, for the trend test

cat("=== Anomaly vs. MITECO last-decade reference average (", ref_avg, "ha/yr) ===\n")
print(burnt_all %>% select(year, burnt_area_ha, anomaly_multiple, status) %>% arrange(year))

# Kendall trend test on the COMPLETED-season anomaly series only (2026
# deliberately excluded - it is not a finished value and would distort a
# trend test). Reported honestly: with only n=6 benchmark years, this test
# is under-powered and is NOT presented as proof of a statistically
# significant trend.
kendall_test <- cor.test(burnt$year, burnt$anomaly_multiple, method = "kendall")
cat("\n=== Kendall trend test (year vs. anomaly multiple), completed seasons only ===\n")
cat(sprintf("tau = %.2f, p-value = %.3f, n = %d\n", kendall_test$estimate, kendall_test$p.value, nrow(burnt)))
cat("Interpretation: NOT statistically significant (p > 0.05) at this small\n")
cat("sample size. This is the honest result: a handful of benchmark years\n")
cat("cannot support a trend claim, however dramatic individual years look in\n")
cat("isolation. The robust, better-supported finding in this analysis is the\n")
cat("CONCENTRATION effect in Part 2 (large-fire share of total burnt area),\n")
cat("which rests on independently reported percentages rather than a fitted\n")
cat("trend line. A properly powered trend test would require the full\n")
cat("continuous EGIF annual series, available on request from MITECO.\n\n")

p1 <- ggplot(burnt_all, aes(x = reorder(as.factor(year), year), y = anomaly_multiple, fill = status)) +
  geom_col(width = 0.65, show.legend = TRUE) +
  geom_hline(yintercept = 1, linetype = "dashed", color = ash_grey, linewidth = 0.5) +
  geom_text(aes(label = paste0(round(anomaly_multiple, 1), "x")),
            vjust = -0.6, size = 3.6, fontface = "bold", color = ash_grey) +
  scale_fill_manual(values = c("Completed season" = fire_red, "2026 - PROVISIONAL\n(year not over)" = fire_amber), name = NULL) +
  scale_y_continuous(labels = label_number(suffix = "x"), expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = wrap_txt("Spain's fire seasons swing from 0.5x to 4.1x the decade average", 42),
    subtitle = wrap_txt("Burnt area as a multiple of MITECO's own last-decade reference average (\u2248105,600 ha/yr). 2026 is a MINIMUM \u2014 the season runs to mid-October and this figure will rise further", 62),
    x = NULL, y = "Multiple of reference average",
    caption = wrap_txt("Dashed line = reference average (1.0x). Source: MITECO Avance Informativo de Incendios Forestales throughout, for full methodological consistency. 2026 figure to 17 Aug (provisional, incomplete). An earlier version of this chart incorrectly used Portugal's 2017 total for Spain and mixed EFFIS/MITECO sources across years - corrected 19 Aug 2026, see README.", 100)
  ) +
  theme_fire

ggsave("output/01_anomaly_vs_reference.png", p1, width = 9, height = 6, dpi = 200, bg = "white")


# ==============================================================================
# PART 2 — THE CONCENTRATION EFFECT
# Share of total burnt area caused by "Grandes Incendios Forestales" (GIF,
# large fires >500 ha under the Spanish classification), and the declining
# frequency of those same large fires.
# Source: EGIF/MITECO data as compiled in ARBA-S (2025) historical analysis.
# ==============================================================================

gif <- read_csv("data/large_fires_spain.csv", show_col_types = FALSE)

# --- 2a. Share of total burnt area from large fires (only years with real data)
share_df <- gif %>% filter(!is.na(pct_area_from_large_fires)) %>%
  select(year, pct_area_from_large_fires) %>%
  bind_rows(tibble(year = NA, pct_area_from_large_fires = 43.81)) %>%
  mutate(label = if_else(is.na(year), "1968-2025\naverage", as.character(year)))

p2 <- ggplot(share_df, aes(x = reorder(label, pct_area_from_large_fires), y = pct_area_from_large_fires)) +
  geom_col(fill = fire_red, width = 0.6) +
  geom_text(aes(label = paste0(round(pct_area_from_large_fires, 1), "%")),
            hjust = -0.15, size = 4, fontface = "bold", color = ash_grey) +
  coord_flip(clip = "off") +
  scale_y_continuous(limits = c(0, 95), expand = c(0, 0)) +
  labs(
    title = wrap_txt("Large fires now cause 4 out of every 5 burnt hectares", 44),
    subtitle = wrap_txt("Share of Spain's total annual burnt area caused by large fires (>500 ha)", 58),
    x = NULL, y = "% of total burnt area",
    caption = wrap_txt("Source: EGIF (MITECO) data compiled in ARBA-S historical analysis, Aug 2025. See README.", 90)
  ) +
  theme_fire

ggsave("output/02_concentration_share.png", p2, width = 8, height = 4.5, dpi = 200, bg = "white")

# --- 2b. Frequency of large fires vs. average size per event (the paradox)
freq_df <- gif %>% filter(!is.na(large_fires_count)) %>% select(year, large_fires_count)
size_df <- gif %>% filter(!is.na(avg_ha_per_large_fire)) %>% select(year, avg_ha_per_large_fire)

p3a <- ggplot(freq_df, aes(x = reorder(as.factor(year), year), y = large_fires_count)) +
  geom_col(fill = ash_grey, width = 0.65) +
  geom_text(aes(label = large_fires_count), vjust = -0.5, size = 3.4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Fewer large fires\u2026", x = NULL, y = "Large fires per year (count)") +
  theme_fire + theme(plot.subtitle = element_blank())

p3b <- ggplot(size_df, aes(x = reorder(as.factor(year), year), y = avg_ha_per_large_fire)) +
  geom_col(fill = fire_red, width = 0.5) +
  geom_text(aes(label = comma(avg_ha_per_large_fire)), vjust = -0.5, size = 3.4) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.18))) +
  labs(title = wrap_txt("\u2026each one far more destructive", 18), x = NULL, y = "Avg. hectares burnt per large fire") +
  theme_fire + theme(plot.subtitle = element_blank())

p3 <- plot_grid(p3a, p3b, ncol = 2)
title3 <- ggdraw() + draw_label(
  wrap_txt("The paradox behind Spain's fire statistics", 38), fontface = "bold", size = 17, x = 0, hjust = 0, lineheight = 1
) + theme(plot.margin = margin(l = 7))
caption3 <- ggdraw() + draw_label(
  wrap_txt("Source: EGIF (MITECO) data compiled in ARBA-S historical analysis, Aug 2025 (years with published values only). See README.", 95),
  size = 8, color = "grey50", x = 0, hjust = 0, lineheight = 1
) + theme(plot.margin = margin(l = 7))

p3_full <- plot_grid(title3, p3, caption3, ncol = 1, rel_heights = c(0.16, 1, 0.1))
ggsave("output/03_frequency_vs_severity.png", p3_full, width = 10, height = 5.3, dpi = 200, bg = "white")


# ==============================================================================
# PART 3 — SPATIAL COMPONENT (GIS)
# Real administrative boundaries (Spain, 19 CCAA) via sf. Highlights the
# historically documented highest-incidence quadrant (Galicia, Asturias,
# Castilla y Leon) per MITECO's 2006-2015 decade report, as cited in the
# ARBA-S analysis. This is a QUALITATIVE risk-concentration map: no
# region-level hectare figures are invented; only the documented pattern
# is encoded (regions flagged vs. not flagged).
# ==============================================================================

spain <- st_read("data/spain_ccaa.geojson", quiet = TRUE)

high_incidence <- c("Galicia", "Principado de Asturias", "Asturias",
                     "Castilla y Leon", "Castilla-Leon", "Castilla y León")

spain <- spain %>%
  mutate(
    risk_zone = if_else(name %in% high_incidence, "Historically highest incidence\n(NW quadrant, MITECO 2006-2015)",
                         "Rest of Spain")
  )

p4 <- ggplot(spain) +
  geom_sf(aes(fill = risk_zone), color = "white", linewidth = 0.3) +
  scale_fill_manual(values = c(
    "Historically highest incidence\n(NW quadrant, MITECO 2006-2015)" = fire_red,
    "Rest of Spain" = "grey85"
  )) +
  labs(
    title = wrap_txt("Where the fires concentrate: the NW quadrant", 34),
    subtitle = wrap_txt("Galicia, Asturias and Castilla y Le\u00f3n: highest fire count and burnt area, 2006-2015 decade (most recent full MITECO decade report)", 46),
    fill = NULL,
    caption = wrap_txt("Boundaries: administrative CCAA geometry (public domain). Risk pattern: MITECO 2006-2015 decade report, as cited in ARBA-S (2025). See README.", 68)
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 17, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 11, color = "grey30", margin = margin(b = 10)),
    plot.caption = element_text(size = 8, color = "grey50", hjust = 0, margin = margin(t = 10)),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "bottom"
  )

ggsave("output/04_spatial_concentration.png", p4, width = 7, height = 8, dpi = 200, bg = "white")


# ==============================================================================
# DONE
# ==============================================================================

cat("\nDone. Files written to output/:\n")
print(list.files("output"))
