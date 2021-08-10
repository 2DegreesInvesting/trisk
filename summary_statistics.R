library(dplyr)
library(ggplot2)
library(r2dii.plot)
# =================================
# paths
# =================================
path_db_analysis_inputs <- fs::path(r2dii.utils::dbox_port_00(),"07_AnalysisInputs", "2020Q4_05172021_2020_MFM")
path_db_datastore <- fs::path(r2dii.utils::dbox_port_00(),"06_DataStore", "DataStore_export_05172021", "2020Q4")


# =================================
# load data
# =================================
masterdata_ownership <- readRDS(fs::path(path_db_analysis_inputs, "masterdata_ownership_datastore", ext = "rda"))
# masterdata_ownership <- readr::read_rds(fs::path(path_db_analysis_inputs, "masterdata_ownership_datastore", ext = "rda"))
masterdata_debt <- readRDS(fs::path(path_db_analysis_inputs, "masterdata_debt_datastore", ext = "rda"))
# masterdata_debt <- readr::read_rds(fs::path(path_db_analysis_inputs, "masterdata_debt_datastore", ext = "rda"))

production_coverage_by_sector <- readr::read_csv(
  fs::path(path_db_datastore, "production_coverage_by_sector", ext = "csv"),
  col_types = readr::cols(
    sector = "c",
    .default = readr::col_number()
  )
)

sector_production_mapped_per_country <- readr::read_csv(
  fs::path(path_db_datastore, "sector_production_mapped_per_country", ext = "csv"),
  col_types = readr::cols(
    sector = "c",
    country_name = "c",
    iso2 = "c",
    region_name = "c",
    .default = readr::col_number()
  )
)

sector_production_mapped_per_technology_country_and_year <- readr::read_csv(
  fs::path(path_db_datastore, "sector_production_mapped_per_technology_country_and_year", ext = "csv"),
  col_types = readr::cols(
    sector = "c",
    technology = "c",
    country_name = "c",
    iso2 = "c",
    region_name = "c",
    .default = readr::col_number()
  )
)

masterdata <- bind_rows(masterdata_ownership, masterdata_debt)

# ==============
# country region bridge
# ==============
country_region_bridge <- rworldmap::countryRegions %>%
  dplyr::rename(iso_a3 = ISO3) %>%
  dplyr::left_join(ISOcodes::ISO_3166_1, by = c("iso_a3" = "Alpha_3")) %>%
  dplyr::rename(iso_a2 = Alpha_2) %>%
  dplyr::select(iso_a2, iso_a3, everything()) %>%
  dplyr::distinct(iso_a2, .keep_all = TRUE) %>%
  dplyr::as_tibble()

# choose which region should be used
country_region_bridge <- country_region_bridge %>%
  dplyr::transmute(iso_a2, subregion = REGION) %>%
  tidyr::drop_na()

# =================================
# summary stats
# =================================


unique_company_names <- masterdata %>%
  filter(year == 2019) %>%
  distinct(company_name, .keep_all = TRUE) %>%
  left_join(country_region_bridge, by = c("ald_location" = "iso_a2"))

companies_per_sector <- unique_company_names %>%
  group_by(ald_sector, year) %>%
  summarise(number_of_companies = n()) %>%
  mutate(subregion = "Global")

companies_per_sector_region <- unique_company_names %>%
  group_by(ald_sector, year, subregion) %>%
  summarise(number_of_companies = n())

sector_production_mapped_per_region <- sector_production_mapped_per_country %>%
  filter(production_year == 2019) %>%
  left_join(country_region_bridge, by = c("iso2" = "iso_a2")) %>%
  group_by(sector, production_year, subregion) %>%
  summarise(
    total_production = sum(total_production, na.rm = TRUE),
    mapped_production = sum(mapped_production, na.rm = TRUE),
    number_of_assets = sum(number_of_assets, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  transmute(
    sector,
    subregion,
    year = production_year,
    production_fraction = mapped_production/total_production,
    number_of_assets
  )

global_production_coverage_by_sector <- production_coverage_by_sector %>%
  transmute(
    sector,
    subregion = "Global",
    production_fraction = mapped_production_coverage,
    year = 2019
  ) %>%
  left_join(
    sector_production_mapped_per_region %>% group_by(sector) %>% summarise(number_of_assets = sum(number_of_assets, na.rm = TRUE)),
    by = "sector"
  )



sector_production_mapped_per_region_per_technology <- sector_production_mapped_per_technology_country_and_year %>%
  filter(production_year == 2019) %>%
  left_join(country_region_bridge, by = c("iso2" = "iso_a2")) %>%
  mutate(
    technology = dplyr::if_else(
      sector == "Coal",
      "Coal",
      technology
    )
  ) %>%
  mutate(
    technology = dplyr::if_else(
      technology == "Oil and Condensate",
      "Oil",
      technology
    )
  ) %>%
  mutate(
    technology = dplyr::if_else(
      technology == "Natural Gas Liquids",
      "Gas",
      technology
    )
  ) %>%
  group_by(sector, technology, production_year, subregion) %>%
  summarise(
    total_production = sum(total_production, na.rm = TRUE),
    mapped_production = sum(mapped_production, na.rm = TRUE),
    number_of_assets = sum(number_of_assets, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  transmute(
    sector,
    technology,
    subregion,
    year = production_year,
    production_fraction = mapped_production/total_production,
    number_of_assets
  )


final_number_companies <- companies_per_sector %>%
  rbind.data.frame(companies_per_sector_region) %>%
  rename(sector = ald_sector)


final_production_coverage <- global_production_coverage_by_sector %>%
  bind_rows(sector_production_mapped_per_region, sector_production_mapped_per_region_per_technology)

final_summary <- final_number_companies %>%
  left_join(final_production_coverage)

final_summary <- final_summary %>%
  mutate(technology = if_else(is.na(technology), "Sector Level", technology)) %>%
  mutate(subregion = if_else(is.na(subregion), "Missing", subregion))

# why NAns?
# why fraction somestimes missing?

companies_covered <- final_summary %>%
  filter(subregion == "Global") %>%
  ggplot() +
  geom_col(aes(x = sector, y = number_of_companies, fill = sector)) +
  labs(
    x = "Sector",
    y = "Number of companies",
    title = "Number of companies covered per sector"
  ) +
  r2dii.plot::theme_2dii() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggsave("companies_covered.png", plot = companies_covered, path = "plots")
