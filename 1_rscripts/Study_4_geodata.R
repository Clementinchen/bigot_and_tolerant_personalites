# =============================================================================
# STUDY 4 – GEODATA OF THE ESS ROUND 4 REGIONS
# -----------------------------------------------------------------------------
# Attaches a polygon to every ESS4 region that can be matched to an official
# boundary file. Only geography: regional prejudice means are added in the
# next step (spatial analysis).
#
# Input : ./1_analysis_data/ds4_wde.rds  (region names, created by "Data Wrangling.R")
#         ./0_data/shapefiles_nuts_europe_2006.shp  (Eurostat NUTS 2006, valid 2008–2011)
#         ./0_data/shapefiles_ukraine.json, ./0_data/shapefiles_israel.json  (GADM)
# Output: ./1_analysis_data/ds4_geo.rds – sf object, one row per matched region:
#         cntry_cde, region, region_id (keys of ds4_wde), geo_id (NUTS code(s)
#         or GADM name), geo_source, geometry (EPSG:3035, ETRS89-LAEA)
#         ./1_analysis_data/base_europe.rds – country outlines (background map)
# =============================================================================


# 0  SETUP --------------------------------------------------------------------

library(tidyverse)
library(sf)

data_dir <- "./0_data"
out_dir  <- "./1_analysis_data"
crs_out  <- 3035


# 1  ESS4 REGIONS -------------------------------------------------------------

ess_regions <- readRDS(file.path(out_dir, "ds4_wde.rds")) |>
  filter(!is.na(region_id)) |>
  distinct(cntry_cde, region, region_id) |>
  mutate(across(everything(), as.character))


# 2  BOUNDARY FILES -----------------------------------------------------------

# NUTS 2006; names cleaned like the ESS labels (Latin transliteration, without
# the "(England)" etc. suffix of the UK regions); Eurostat "UK" = ESS "GB"
nuts <- read_sf(file.path(data_dir, "shapefiles_nuts_europe_2006.shp",
                          "NUTS_RG_20M_2006_3035.shp")) |>
  mutate(cntry_cde = if_else(CNTR_CODE == "UK", "GB", CNTR_CODE),
         region    = NAME_LATN |>
           str_remove(" \\((England|Scotland|Wales|Northern Ireland)\\)") |>
           janitor::make_clean_names(allow_dupes = TRUE)) |>
  select(cntry_cde, region, geo_id = NUTS_ID, level = LEVL_CODE)

# GADM: dissolve to the first administrative level (oblast / district)
read_gadm <- function(file, cntry) {
  read_sf(file.path(data_dir, file)) |>
    filter(!is.na(NAME_1), NAME_1 != "?") |>
    group_by(geo_id = NAME_1) |>
    summarise(.groups = "drop") |>
    st_transform(crs_out) |>
    mutate(cntry_cde = cntry)
}

gadm <- bind_rows(read_gadm("shapefiles_ukraine.json", "UA"),
                  read_gadm("shapefiles_israel.json",  "IL"))


# 3  AUTOMATIC MATCH ON REGION NAMES ------------------------------------------
# Restricted to the NUTS level the ESS4 uses in each country, because many
# names occur on several levels (e.g. "berlin" = DE3, DE30, DE300).
# Not listed: IE (provinces, not NUTS), CY (no sub-national NUTS), and the
# GADM countries IL, UA; RU is not covered by any boundary file.

nuts_level <- c(BE = 1, DE = 1, FR = 1, GB = 1, TR = 1,
                CH = 2, CZ = 2, DK = 2, ES = 2, FI = 2, GR = 2, HR = 2, HU = 2,
                NO = 2, PL = 2, PT = 2, RO = 2, SE = 2,
                BG = 3, EE = 3, LV = 3, NL = 3, SI = 3, SK = 3)


# 4  MANUAL CROSSWALK ---------------------------------------------------------
# Regions whose ESS label differs from the boundary file (translation,
# transliteration, ESS-specific spelling). Several rows per region = union of
# these units (ESS region combines two NUTS regions).

crosswalk_nuts <- tribble(
  ~cntry_cde, ~region,                                ~geo_id,
  # Belgium: bilingual NUTS names
  "BE", "brussels_region",                            "BE1",
  "BE", "flemish_region",                             "BE2",
  "BE", "walloon_region",                             "BE3",
  # Bulgaria: transliteration; "sofia" = city (BG411), not province (BG412)
  "BG", "bourgas",                                    "BG341",
  "BG", "iambol",                                     "BG343",
  "BG", "kurdjali",                                   "BG425",
  "BG", "kustendil",                                  "BG415",
  "BG", "lovetch",                                    "BG315",
  "BG", "pazardjik",                                  "BG423",
  "BG", "rouse",                                      "BG323",
  "BG", "shoumen",                                    "BG333",
  "BG", "smolian",                                    "BG424",
  "BG", "sofia",                                      "BG411",
  "BG", "sofia_region",                               "BG412",
  "BG", "vratca",                                     "BG313",
  # Denmark
  "DK", "midjylland",                                 "DK04",
  # Finland: English names incl. NUTS code; Southern Finland includes Åland
  "FI", "eastern_finland_fi13",                       "FI13",
  "FI", "southern_finland_fi18_and_aland_fi20",       "FI18",
  "FI", "southern_finland_fi18_and_aland_fi20",       "FI20",
  "FI", "western_finland_fi19",                       "FI19",
  "FI", "northern_finland_fi1a",                      "FI1A",
  # France: ZEAT labels; Bassin Parisien Est/Ouest and Sud-Est have no
  # unambiguous NUTS counterpart and remain unmatched
  "FR", "nord",                                       "FR3",
  "FR", "region_parisienne",                          "FR1",
  # Greece: English names; four ESS regions combine two NUTS 2 regions
  "GR", "east_macedonia_thrace",                      "GR11",
  "GR", "central_macedonia_thessaloniki",             "GR12",
  "GR", "west_macedonia_epirus",                      "GR13",
  "GR", "west_macedonia_epirus",                      "GR21",
  "GR", "west_greece_ionian_islands",                 "GR22",
  "GR", "west_greece_ionian_islands",                 "GR23",
  "GR", "central_greece",                             "GR24",
  "GR", "peloponnese",                                "GR25",
  "GR", "attica_athens",                              "GR30",
  "GR", "aegean_islands",                             "GR41",
  "GR", "aegean_islands",                             "GR42",
  "GR", "crete",                                      "GR43",
  # Hungary: English names
  "HU", "central_regio",                              "HU10",
  "HU", "middle_transdanubia",                        "HU21",
  "HU", "west_transdanubia",                          "HU22",
  "HU", "south_transdanubia",                         "HU23",
  "HU", "north_regio",                                "HU31",
  "HU", "north_plain",                                "HU32",
  "HU", "south_plain",                                "HU33",
  # Ireland: provinces; only Dublin corresponds to a NUTS region
  "IE", "dublin",                                     "IE021",
  # Netherlands: ESS spelling
  "NL", "agglomeraties_gravenhage",                   "NL332",
  # Norway: English names
  "NO", "oslo_and_akershus",                          "NO01",
  "NO", "hedmark_and_oppland",                        "NO02",
  "NO", "south_eastern_norway",                       "NO03",
  "NO", "agder_and_rogaland",                         "NO04",
  "NO", "western_norway",                             "NO05",
  "NO", "northern_norway",                            "NO07",
  # Portugal: English names
  "PT", "north",                                      "PT11",
  "PT", "center",                                     "PT16",
  "PT", "lisbon",                                     "PT17",
  # Slovakia: English names of the kraje (NUTS 3)
  "SK", "bratislava_reg",                             "SK010",
  "SK", "trnava_reg",                                 "SK021",
  "SK", "trencin_reg",                                "SK022",
  "SK", "nitra_reg",                                  "SK023",
  "SK", "zilina_reg",                                 "SK031",
  "SK", "banska_bystrica_reg",                        "SK032",
  "SK", "presov_reg",                                 "SK041",
  "SK", "kosice_reg",                                 "SK042",
  # Turkey: English names of the 12 NUTS 1 regions
  "TR", "western_marmara",                            "TR2",
  "TR", "aegean",                                     "TR3",
  "TR", "eastern_marmara",                            "TR4",
  "TR", "western_anatolia",                           "TR5",
  "TR", "mediterranean",                              "TR6",
  "TR", "central_anatolia",                           "TR7",
  "TR", "western_black_sea",                          "TR8",
  "TR", "eastern_black_sea",                          "TR9",
  "TR", "north_eastern_anatolia",                     "TRA",
  "TR", "east",                                       "TRB",
  "TR", "south_east",                                 "TRC",
  # United Kingdom: ESS suffix
  "GB", "south_east_2",                               "UKJ"
)

crosswalk_gadm <- tribble(
  ~cntry_cde, ~region,                       ~geo_id,
  # Israel ("judea_samaria_and_gaza" has no counterpart)
  "IL", "central",                           "HaMerkaz",
  "IL", "haifa",                             "Haifa",
  "IL", "jerusalem",                         "Jerusalem",
  "IL", "northern",                          "HaZafon",
  "IL", "southern",                          "HaDarom",
  "IL", "tel_aviv",                          "TelAviv",
  # Ukraine (no ESS counterpart: Khmel'nyts'kyy, Kiev oblast, Poltava, Sevastopol')
  "UA", "cherkasska_oblast",                 "Cherkasy",
  "UA", "chernigivska_oblast",               "Chernihiv",
  "UA", "chernovytska_oblast",               "Chernivtsi",
  "UA", "crimea_autonomy_republic",          "Crimea",
  "UA", "dnipropetrovska_oblast",            "Dnipropetrovs'k",
  "UA", "donetska_oblast",                   "Donets'k",
  "UA", "ivano_frankivska_oblast",           "Ivano-Frankivs'k",
  "UA", "kharkivska_oblast",                 "Kharkiv",
  "UA", "khersonska_oblast",                 "Kherson",
  "UA", "kirovogradska_oblast",              "Kirovohrad",
  "UA", "kyiv_city",                         "KievCity",
  "UA", "luganska_oblast",                   "Luhans'k",
  "UA", "lvivska_oblast",                    "L'viv",
  "UA", "mykolaivska_oblast",                "Mykolayiv",
  "UA", "odesska_oblast",                    "Odessa",
  "UA", "rivenska_oblast",                   "Rivne",
  "UA", "sumska_oblast",                     "Sumy",
  "UA", "ternopilska_oblast",                "Ternopil'",
  "UA", "volynska_oblast",                   "Volyn",
  "UA", "vynnytska_oblast",                  "Vinnytsya",
  "UA", "zakarpatska_oblast",                "Zakarpattia",
  "UA", "zaporizska_oblast",                 "Zaporizhia",
  "UA", "zhytomyrska_oblast",                "Zhytomyr"
)

# Guard against typos: every crosswalk entry must exist in the data and in the boundary file
stopifnot(all(paste(crosswalk_nuts$cntry_cde, crosswalk_nuts$region) %in%
                paste(ess_regions$cntry_cde, ess_regions$region)),
          all(paste(crosswalk_gadm$cntry_cde, crosswalk_gadm$region) %in%
                paste(ess_regions$cntry_cde, ess_regions$region)),
          all(crosswalk_nuts$geo_id %in% nuts$geo_id),
          all(crosswalk_gadm$geo_id %in% gadm$geo_id))


# 5  MATCH AND COMBINE --------------------------------------------------------

nuts_auto <- ess_regions |>
  anti_join(crosswalk_nuts, by = c("cntry_cde", "region")) |>
  inner_join(nuts |> st_drop_geometry() |> filter(level == nuts_level[cntry_cde]),
             by = c("cntry_cde", "region"))

stopifnot(!anyDuplicated(nuts_auto$region_id))   # one NUTS unit per region

# Attach polygons; combined regions are dissolved into one polygon
attach_geometry <- function(keys, shapes, source) {
  keys |>
    inner_join(select(shapes, geo_id), by = "geo_id") |>
    st_as_sf() |>
    group_by(cntry_cde, region) |>
    summarise(geo_id = paste(geo_id, collapse = "+"), .groups = "drop") |>
    mutate(geo_source = source)
}

ds4_geo <- bind_rows(
  attach_geometry(bind_rows(select(nuts_auto, cntry_cde, region, geo_id), crosswalk_nuts),
                  nuts, "NUTS 2006"),
  attach_geometry(crosswalk_gadm, gadm, "GADM")) |>
  inner_join(ess_regions, by = c("cntry_cde", "region")) |>
  select(cntry_cde, region, region_id, geo_id, geo_source, geometry) |>
  arrange(region_id)

stopifnot(!anyDuplicated(ds4_geo$region_id), all(st_is_valid(ds4_geo)))


# 6  MATCHING SUMMARY ---------------------------------------------------------

unmatched <- anti_join(ess_regions, st_drop_geometry(ds4_geo), by = "region_id")

cat(sprintf("\nMatched %d of %d ESS4 regions (%d countries)\n",
            nrow(ds4_geo), nrow(ess_regions), n_distinct(ds4_geo$cntry_cde)))
cat("\nUnmatched regions:\n")
unmatched |>
  summarise(n = n(), regions = paste(region, collapse = ", "), .by = cntry_cde) |>
  as.data.frame() |>
  print(row.names = FALSE, right = FALSE)


# 7  BACKGROUND MAP -----------------------------------------------------------
# Country outlines (NUTS 0 + GADM countries), shown in grey where no data exist

base_europe <- bind_rows(
  nuts |> filter(level == 0) |> select(cntry_cde),
  gadm |> group_by(cntry_cde) |> summarise(.groups = "drop"))


# 8  SAVE ---------------------------------------------------------------------

saveRDS(ds4_geo,     file.path(out_dir, "ds4_geo.rds"))
saveRDS(base_europe, file.path(out_dir, "base_europe.rds"))
