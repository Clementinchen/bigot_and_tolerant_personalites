## ---------------------------------------------------------------------------
## Study 2: Attach spatial polygons to region-level ESS4 prejudice scores.
##
## Builds `prj_reg` (mean anti-immigrant prejudice per country/region) and
## matches it against three shapefiles:
##   - europe   : Eurostat NUTS shapefile (EU / EFTA countries)
##   - ukraine  : GADM administrative shapefile (oblast level = NAME_1)
##   - bulgaria : GADM administrative shapefile (oblast level = NAME_1)
## then combines all matched regions into a single sf object, `prj_reg_sf`,
## ready for spatial analysis (mapping, Moran's I, etc.).
##
## NOT covered (see the notes at the end of the script for why):
##   - russia            : needs a many-to-one aggregation crosswalk, not a
##                         simple rename (10 macro-regions vs. 83 subjects)
##   - CY                : no sub-national NUTS divisions exist for Cyprus
##   - IL, TR            : no shapefile identified yet
##   - 3 France regions  : old ZEAT study-zone labels with no clean NUTS match
## ---------------------------------------------------------------------------

library(tidyverse)
library(sf)
library(janitor)

europe <- read_sf("./0_data/NUTS_Europe_2006.shp")
ukraine <- read_sf("./0_data/Ukraine.json")
bulgaria <- read_sf("./0_data/Bulgaria.json")
israel <- read_sf("./0_data/Israel.json")


# =============================================================================
# 0. Region-level prejudice scores
# =============================================================================

prj_reg <-
  ess4_ana |>
  filter(!is.na(region)) |>
  select(cntry_cde, region, idno, (starts_with("prj")&ends_with("_lin"))) |>
  group_by(cntry_cde, region) |>
  summarise(
    prj_immigrants = mean(prj_immigrants_lin, na.rm = TRUE),
    prj_gay        = mean(prj_gay_lin, na.rm = TRUE), 
    prj_women      = mean(prj_women_lin, na.rm = TRUE), 
    prj_unemployed = mean(prj_unemployed_lin, na.rm = TRUE), 
    prj_age20      = mean(prj_age20_lin , na.rm = TRUE),
    prj_age70      = mean(prj_age70_lin , na.rm = TRUE),
    .groups = "drop") |>
  mutate(
    cntry_cde = as.character(cntry_cde),
    # Eurostat codes the UK as "UK", not "GB"
    cntry_cde = ifelse(cntry_cde == "GB", "UK", cntry_cde),
    region    = as.character(region)
  )

# =============================================================================
# 1. europe (Eurostat NUTS)
# =============================================================================

europe_clean <- europe |>
  st_transform(4326) |>
  mutate(
    # NAME_LATN (not NUTS_NAME) is used here: NUTS_NAME holds the native-
    # script name (Cyrillic for BG, Greek for CY, etc.), which can't be
    # cleaned into meaningful ASCII words for matching against the English
    # ESS labels. NAME_LATN is Eurostat's Latin-transliterated version,
    # provided for exactly this purpose. janitor::make_clean_names() already
    # transliterates diacritics correctly (e.g. "Dolnośląskie" ->
    # "dolnoslaskie", "Trøndelag" -> "trondelag").
    region_raw = NAME_LATN |>
      str_remove_all("\\s*\\((England|Scotland|Wales|Northern Ireland)\\)"),
    region = janitor::make_clean_names(region_raw)
  )

# Manual crosswalk for regions where NAME_LATN uses a different naming
# convention/language than the ESS labels (transliteration for BG, native-
# language names for HU/NO/SK, bilingual names for BE) or an ESS-specific
# quirk (UK, NL). Keyed on NUTS_ID rather than name, since some of these
# names collide with (or are ambiguous against) other regions.
# NOTE: BG "sofia" is deliberately mapped to BG411 (Sofia city), not the
# auto-matching BG412 (Sofia province) - the naive string join would link
# "sofia" to the province while leaving "sofia_region" unmatched.
europe_overrides <- tribble(
  ~region,                    ~NUTS_ID,
  # --- Bulgaria: transliteration variants ---
  "bourgas",                   "BG341",
  "iambol",                    "BG343",
  "kurdjali",                  "BG425",
  "kustendil",                 "BG415",
  "lovetch",                   "BG315",
  "pazardjik",                 "BG423",
  "rouse",                     "BG323",
  "shoumen",                   "BG333",
  "smolian",                   "BG424",
  "sofia_region",              "BG412",
  "sofia",                     "BG411",
  "vratca",                    "BG313",
  # --- Hungary: native-language NAME_LATN, mapped to NUTS2 ---
  "central_regio",             "HU10",
  "middle_transdanubia",       "HU21",
  "north_plain",               "HU32",
  "north_regio",               "HU31",
  "south_plain",                "HU33",
  "south_transdanubia",         "HU23",
  "west_transdanubia",           "HU22",
  # --- Norway: native-language NAME_LATN, mapped to NUTS2 ---
  "agder_and_rogaland",           "NO04",
  "hedmark_and_oppland",          "NO02",
  "northern_norway",              "NO07",
  "oslo_and_akershus",            "NO01",
  "south_eastern_norway",         "NO03",
  "trondelag",                    "NO06",
  "western_norway",               "NO05",
  # --- Slovakia: native-language NAME_LATN ("kraj"), mapped to NUTS3 ---
  "banska_bystrica_reg",          "SK032",
  "bratislava_reg",               "SK01",
  "kosice_reg",                   "SK042",
  "nitra_reg",                    "SK023",
  "presov_reg",                   "SK041",
  "trencin_reg",                  "SK022",
  "trnava_reg",                   "SK021",
  "zilina_reg",                   "SK031",
  # --- Belgium: bilingual French/Dutch NAME_LATN, mapped to NUTS1 ---
  "brussels_region",              "BE1",
  "flemish_region",               "BE2",
  "walloon_region",               "BE3",
  # --- Netherlands: ESS spelling/formatting quirks ---
  "i_jmond",                      "NL323",   # "IJmond"
  "agglomeraties_gravenhage",     "NL332",   # "Agglomeratie 's-Gravenhage"
  # --- UK: ESS-specific disambiguation suffix not present in Eurostat ---
  "south_east_2",                 "UKJ",
  # --- France: Ile-de-France maps cleanly onto FR1. The other 4 unmatched
  # FR regions are old ZEAT study-zone labels that split/combine NUTS1 units
  # in ways with no exact Eurostat equivalent (see closing notes) and are
  # deliberately left unmatched rather than guessed at.
  "region_parisienne",            "FR1"
)

# Automated match on cleaned names, filled in with the manual overrides above
europe_matched <- prj_reg |>
  left_join(europe_clean |> st_drop_geometry() |> select(region, NUTS_ID), by = "region") |>
  left_join(europe_overrides |> rename(NUTS_ID_override = NUTS_ID), by = "region") |>
  mutate(NUTS_ID = coalesce(NUTS_ID_override, NUTS_ID)) |>
  select(-NUTS_ID_override) |>
  # europe contains multiple NUTS levels sharing the same name (e.g. Northern
  # Ireland appears as both UKN and UKN0); keep a single match per region
  distinct(cntry_cde, region, .keep_all = TRUE) |>
  # NUTS_ID is unique in `europe_clean`, so this join is now 1:1
  left_join(europe_clean |> select(NUTS_ID, geometry), by = "NUTS_ID") |>
  st_as_sf()

cat("europe: ", sum(!is.na(europe_matched$NUTS_ID)), "/", nrow(europe_matched),
    " regions matched\n", sep = "")

# =============================================================================
# 2. ukraine (GADM)
# =============================================================================

# `ukraine` is a level-2 (raion/district) shapefile: every oblast (NAME_1)
# appears once per district, each with its own small polygon. Dissolve up to
# the oblast level before matching, rather than just de-duplicating rows.
ukraine_l1 <- ukraine |>
  st_transform(4326) |>
  filter(!is.na(NAME_1), NAME_1 != "?") |>
  group_by(GID_1, NAME_1) |>
  summarise(.groups = "drop")

# GADM's NAME_1 uses transliterations with apostrophes (e.g. "Donets'k",
# "L'viv") that never coincide with ESS-style labels, so there's no point
# attempting an automatic clean-name match - go straight to a manual
# crosswalk. 4 GADM oblasts have no ESS counterpart: Khmel'nyts'kyy, Kiev
# (oblast), Poltava, Sevastopol'.
ukraine_crosswalk <- tribble(
  ~region,                      ~NAME_1,
  "cherkasska_oblast",           "Cherkasy",
  "chernigivska_oblast",         "Chernihiv",
  "chernovytska_oblast",         "Chernivtsi",
  "crimea_autonomy_republic",    "Crimea",
  "dnipropetrovska_oblast",      "Dnipropetrovs'k",
  "donetska_oblast",             "Donets'k",
  "ivano_frankivska_oblast",     "Ivano-Frankivs'k",
  "kharkivska_oblast",           "Kharkiv",
  "khersonska_oblast",           "Kherson",
  "kirovogradska_oblast",        "Kirovohrad",
  "kyiv_city",                   "KievCity",
  "luganska_oblast",             "Luhans'k",
  "lvivska_oblast",              "L'viv",
  "mykolaivska_oblast",          "Mykolayiv",
  "odesska_oblast",              "Odessa",
  "rivenska_oblast",             "Rivne",
  "sumska_oblast",               "Sumy",
  "ternopilska_oblast",          "Ternopil'",
  "volynska_oblast",             "Volyn",
  "vynnytska_oblast",            "Vinnytsya",
  "zakarpatska_oblast",          "Zakarpattia",
  "zaporizska_oblast",           "Zaporizhia",
  "zhytomyrska_oblast",          "Zhytomyr"
)

ukraine_matched <- prj_reg |>
  filter(cntry_cde == "UA") |>
  left_join(ukraine_crosswalk, by = "region") |>
  left_join(ukraine_l1 |> select(NAME_1, geometry), by = "NAME_1") |>
  st_as_sf()

cat("ukraine: ", sum(!st_is_empty(ukraine_matched$geometry)), "/", nrow(ukraine_matched),
    " regions matched\n", sep = "")

# =============================================================================
# 3. bulgaria (GADM)
# =============================================================================

# As with ukraine, `bulgaria` is a level-2 (municipality) shapefile - dissolve
# up to the oblast level (NAME_1) before matching.
bulgaria_l1 <- bulgaria |>
  st_transform(4326) |>
  group_by(GID_1, NAME_1) |>
  summarise(.groups = "drop")

# As with ukraine, GADM's NAME_1 transliterations don't overlap with ESS
# labels, so this goes straight to a manual crosswalk. Sofia city ("sofia")
# vs. Sofia province ("sofia_region") is disambiguated the same way as in
# europe_overrides.
bulgaria_crosswalk <- tribble(
  ~region,           ~NAME_1,
  "blagoevgrad",      "Blagoevgrad",
  "bourgas",          "Burgas",
  "dobrich",          "Dobrich",
  "gabrovo",          "Gabrovo",
  "haskovo",          "Haskovo",
  "iambol",           "Yambol",
  "kurdjali",         "Kardzhali",
  "kustendil",        "Kyustendil",
  "lovetch",          "Lovech",
  "montana",          "Montana",
  "pazardjik",        "Pazardzhik",
  "pernik",           "Pernik",
  "pleven",           "Pleven",
  "plovdiv",          "Plovdiv",
  "razgrad",          "Razgrad",
  "rouse",            "Ruse",
  "shoumen",          "Shumen",
  "silistra",         "Silistra",
  "sliven",           "Sliven",
  "smolian",          "Smolyan",
  "sofia",            "GradSofiya",   # Sofia city
  "sofia_region",     "Sofia",        # Sofia province
  "stara_zagora",     "StaraZagora",
  "targovishte",      "Targovishte",
  "varna",            "Varna",
  "veliko_tarnovo",   "VelikoTarnovo",
  "vidin",            "Vidin",
  "vratca",           "Vratsa"
)

bulgaria_matched <- prj_reg |>
  filter(cntry_cde == "BG") |>
  left_join(bulgaria_crosswalk, by = "region") |>
  left_join(bulgaria_l1 |> select(NAME_1, geometry), by = "NAME_1") |>
  st_as_sf()

cat("bulgaria: ", sum(!st_is_empty(bulgaria_matched$geometry)), "/", nrow(bulgaria_matched),
    " regions matched\n", sep = "")

# =============================================================================
# 4. Israel (GADM)
# =============================================================================

# `ukraine` is a level-2 (raion/district) shapefile: every oblast (NAME_1)
# appears once per district, each with its own small polygon. Dissolve up to
# the oblast level before matching, rather than just de-duplicating rows.
israel_l1 <- israel |>
  st_transform(4326) |>
  filter(!is.na(NAME_1), NAME_1 != "?") |>
  group_by(GID_1, NAME_1) |>
  summarise(.groups = "drop")

# GADM's NAME_1 uses transliterations with apostrophes (e.g. "Donets'k",
# "L'viv") that never coincide with ESS-style labels, so there's no point
# attempting an automatic clean-name match - go straight to a manual
# crosswalk. 4 GADM oblasts have no ESS counterpart: Khmel'nyts'kyy, Kiev
# (oblast), Poltava, Sevastopol'.
israel_crosswalk <- tribble(
  ~region,                      ~NAME_1,
  "central",                     "HaMerkaz",
  "haifa",                       "Haifa",
  "jerusalem",                   "Jerusalem",
  "northern",                    "HaZafon",
  "southern",                    "HaDarom",
  "tel_aviv",                    "TelAviv"
)

israel_matched <- prj_reg |>
  filter(cntry_cde == "IL") |>
  left_join(israel_crosswalk, by = "region") |>
  left_join(israel_l1 |> select(NAME_1, geometry), by = "NAME_1") |>
  st_as_sf()

cat("israel: ", sum(!st_is_empty(israel_matched$geometry)), "/", nrow(israel_matched),
    " regions matched\n", sep = "")

# =============================================================================
# 5. Combine into one analysis-ready spatial dataset
# =============================================================================

europe_ok <- europe_matched |> filter(!is.na(NUTS_ID))

# Bulgaria is an EU member and is already fully covered by `europe` (28/28) -
# `bulgaria_matched` (GADM) would otherwise duplicate every BG row. Only keep
# GADM rows for regions `europe` didn't already resolve.
bulgaria_ok <- bulgaria_matched |>
  filter(!st_is_empty(geometry)) |>
  anti_join(st_drop_geometry(europe_ok), by = c("cntry_cde", "region"))

prj_reg_sf <- bind_rows(
  europe_ok        |> mutate(source = "europe")   |> select(-NUTS_ID),
  ukraine_matched  |> filter(!st_is_empty(geometry)) |> mutate(source = "ukraine"),
  bulgaria_ok      |> mutate(source = "bulgaria"),
  israel_matched   |> filter(!st_is_empty(geometry)) |> mutate(source = "israel")
) |>
  select(cntry_cde, region, starts_with("prj"),source, geometry) |>
  st_as_sf()

cat("\nprj_reg_sf: ", nrow(prj_reg_sf), "/", nrow(prj_reg),
    " total regions matched to a shapefile\n", sep = "")

# =============================================================================
# 5. Still unmatched (not in prj_reg_sf) - see comments below for why
# =============================================================================
# - RU (10 regions): Soviet-era macro-regions requiring a many-to-one
#   aggregation of `russia`'s 83 GADM federal subjects - not yet built.
# - IL, TR: no matching shapefile identified yet.
# - CY (9 regions): Cyprus has NO sub-national NUTS divisions - `europe`
#   contains a single "Kypros" polygon at every NUTS level, so the ESS's
#   district-level rural/urban splits (e.g. "larnaka_rural") have no NUTS
#   counterpart at all. Would need a different (e.g. GADM) Cyprus shapefile.
# - FR (3 regions): "bassin_parisien_est"/"_ouest" and "sud_est_2" are old
#   ZEAT study-zone labels that don't map cleanly onto NUTS1 units (ZEAT's
#   Bassin Parisien split has no NUTS code, and "Sud-Est" straddles FR7 and
#   FR8). Left unmatched rather than guessed at.

anti_join(prj_reg, st_drop_geometry(prj_reg_sf), by = c("cntry_cde", "region")) |>
  count(cntry_cde, sort = TRUE) 

