avg_prod_coverage <- production_coverage_by_sector %>%
  mutate(
    avg_production_coverage = mapped_production_coverage/100
  ) %>%
  select(
    sector,
    production_year,
    avg_production_coverage
  )



coverage_data <- sector_production_mapped_per_region_per_technology %>%
  filter(
    sector %in% c("Automotive", "Coal", "Oil&Gas", "Power"),
    !technology %in% c("FuelCell", "Natural Gas Liquids"),
    !is.na(subregion)
  ) %>%
  mutate(
    technology = dplyr::if_else(
      technology == "Oil and Condensate",
      "Oil",
      technology
    )
  ) %>%
  group_by(sector, technology) %>%
  summarise(
    min_coverage = min(production_fraction, na.rm = TRUE),
    mean_coverage = mean(production_fraction, na.rm = TRUE),
    max_coverage = max(production_fraction, na.rm = TRUE),
    sum_assets = sum(number_of_assets, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # inner_join(avg_prod_coverage, by = c("sector")) %>%
  inner_join(companies_per_sector, by = c("sector" = "ald_sector")) %>%
  mutate(sec_tech = glue::glue("{sector}_{technology}"))

coverage_data %>% readr::write_csv(file.path("plots", "production_coverage.csv"))

coverage_plot <- coverage_data %>%
  # ggplot(aes(x = sector, y = avg_production_coverage)) +
  ggplot(aes(x = sec_tech, y = mean_coverage, fill = sector)) +
  geom_col() +
  geom_point(aes(y = min_coverage)) +
  geom_point(aes(y = max_coverage)) +
  labs(y = "Production Coverage in % of Global Sector Production") +
  theme(
    axis.text.x = ggplot2::element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  )

ggsave("production_coverage.png", plot = coverage_plot, path = "plots")


regional_coverage <- sector_production_mapped_per_region %>%
  filter(
    sector %in% c("Automotive", "Coal", "Oil&Gas", "Power"),
    !is.na(subregion)
  )

regional_coverage %>% readr::write_csv(file.path("plots", "regional_production_coverage.csv"))

regional_coverage_plot <- regional_coverage_power %>%
  ggplot(aes(x = subregion, y = production_fraction, fill = sector)) +
  geom_col(position = "dodge") +
  labs(y = "Production Coverage in % of Sector Production by Region") +
  theme(
    axis.text.x = ggplot2::element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  )

ggsave("regional_production_coverage.png", plot = regional_coverage_plot, path = "plots")




power_tech_mix_usa_plot <- sector_production_mapped_per_technology_country_and_year %>%
  # group_by(country_name, production_year, sector) %>%
  # mutate(
  #   percentage_production = mapped_production/sum(mapped_production, na.rm = TRUE)
  # ) %>%
  # ungroup()%>%
  filter(
    country_name == "United States of America",
    sector == "Power",
    production_year <= 2025
  ) %>%
  ggplot(aes(x = production_year, y = mapped_production, fill = technology)) +
  geom_area()

ggsave("power_tech_mix_us.png", plot = power_tech_mix_usa_plot, path = "plots")


power_tech_mix_china_plot <- sector_production_mapped_per_technology_country_and_year %>%
  # group_by(country_name, production_year, sector) %>%
  # mutate(
  #   percentage_production = mapped_production/sum(mapped_production, na.rm = TRUE)
  # ) %>%
  # ungroup()%>%
  filter(
    country_name == "China",
    sector == "Power",
    production_year <= 2025
  ) %>%
  ggplot(aes(x = production_year, y = mapped_production, fill = technology)) +
  geom_area()

ggsave("power_tech_mix_china.png", plot = power_tech_mix_china_plot, path = "plots")

