library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(sf)

vehicles <- ggplot2::mpg
gems <- ggplot2::diamonds

dir.create("plots", showWarnings = FALSE)
dir.create("data", showWarnings = FALSE)
dir.create("data/raw", showWarnings = FALSE)


cat("\n===== PROBLEM 1 PART A =====\n")

pA <- ggplot(vehicles, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv), alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ drv) +
  labs(x = "Engine displacement (litres)", y = "Highway fuel economy (mpg)", color = "drv") +
  theme_bw()
print(pA)
ggsave("plots/p1_partA.png", pA, width = 9, height = 4)

cat("Part A explanation: facet_wrap(~ drv) splits the data into three panels, one per drive type.\n")
cat("geom_smooth is computed separately on the data inside each panel, so each panel gets its own fitted line,\n")
cat("even though drv is mapped to color only in geom_point.\n")


cat("\n===== PROBLEM 1 PART B =====\n")

pB_count <- ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar() +
  labs(x = "drv", y = "count") +
  theme_bw()
print(pB_count)
ggsave("plots/p1_partB_count.png", pB_count, width = 5, height = 4)

pB_prop <- ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar(position = "fill") +
  labs(x = "drv", y = "Proportion within drive type") +
  theme_bw()
print(pB_prop)
ggsave("plots/p1_partB_prop.png", pB_prop, width = 5, height = 4)

print(count(vehicles, drv))

cat("Part B explanation: position = 'fill' rescales every bar to a total of 1, so each bar shows shares, not counts.\n")
cat("The drive types have very different sample sizes (see the table above: r has far fewer cars than 4 or f),\n")
cat("but the right plot hides this because all bars are forced to the same height.\n")


cat("\n===== PROBLEM 1 PART C =====\n")

small_gems <- gems %>% filter(carat <= 3)
retained <- nrow(small_gems)
excluded <- nrow(gems) - retained
cat("Smaller table: diamonds with carat <= 3\n")
cat("Records retained:", retained, "\n")
cat("Records excluded:", excluded, "\n")

p_fine <- ggplot(small_gems, aes(x = carat)) +
  geom_histogram(binwidth = 0.01, boundary = 0, closed = "right") +
  coord_cartesian(xlim = c(0, 3)) +
  labs(title = "Carat histogram, bin width = 0.01", x = "Carat", y = "Count") +
  theme_bw()

p_coarse <- ggplot(small_gems, aes(x = carat)) +
  geom_histogram(binwidth = 0.5, boundary = 0, closed = "right") +
  coord_cartesian(xlim = c(0, 3)) +
  labs(title = "Carat histogram, bin width = 0.5", x = "Carat", y = "Count") +
  theme_bw()

print(p_fine)
print(p_coarse)
ggsave("plots/p1_partC_fine.png", p_fine, width = 7, height = 4)
ggsave("plots/p1_partC_coarse.png", p_coarse, width = 7, height = 4)

carat_check <- gems %>% filter(carat %in% c(0.99, 1.00)) %>% count(carat)
print(carat_check)

cat("Part C explanation: With 0.01 bins you can see sharp spikes at round sizes such as 0.3, 0.5, 0.7, 1.0, 1.5 and 2.0 carats.\n")
cat("Very few diamonds sit just below these values; for example", carat_check$n[carat_check$carat == 0.99],
    "diamonds are 0.99 carat while", carat_check$n[carat_check$carat == 1.00], "are exactly 1.00 carat.\n")
cat("This suggests stones are cut or recorded to reach round weights. The 0.5 bins merge these spikes and hide the pattern.\n")


cat("\n===== PROBLEM 2 =====\n")

lib_url <- "https://data.cityofchicago.org/resource/x8fc-8rcq.csv?$limit=5000"
area_url <- "https://data.cityofchicago.org/resource/igwz-8jzy.geojson?$limit=5000"
lib_file <- "data/raw/libraries_raw.csv"
area_file <- "data/raw/community_areas_raw.geojson"

if (!file.exists(lib_file)) download.file(lib_url, lib_file, mode = "wb")
if (!file.exists(area_file)) download.file(area_url, area_file, mode = "wb")

data_sources <- data.frame(
  dataset = c("Libraries - Locations, Contact Information, and Usual Hours of Operation",
              "Boundaries - Community Areas"),
  publisher = "City of Chicago Data Portal",
  dataset_id = c("x8fc-8rcq", "igwz-8jzy"),
  page_url = c("https://data.cityofchicago.org/d/x8fc-8rcq",
               "https://data.cityofchicago.org/d/igwz-8jzy"),
  download_url = c(lib_url, area_url),
  local_file = c(lib_file, area_file),
  format = c("CSV", "GeoJSON"),
  license = "City of Chicago Data Portal Terms of Use",
  accessed = as.character(Sys.Date())
)
write_csv(data_sources, "data_sources.csv")
print(data_sources)

libs_raw <- read_csv(lib_file, show_col_types = FALSE)
n_raw <- nrow(libs_raw)
cat("Raw records downloaded:", n_raw, "(limit was 5000, so the download was not cut off)\n")

libs <- libs_raw %>%
  extract(location, into = c("lat", "lon"),
          regex = "\\(([-0-9.]+), ([-0-9.]+)\\)",
          convert = TRUE, remove = FALSE)

n_missing_coords <- sum(is.na(libs$lat) | is.na(libs$lon))
outside_box <- libs %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  filter(lat < 41.6 | lat > 42.1 | lon < -88 | lon > -87.5)

libs_clean <- libs %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  filter(lat >= 41.6, lat <= 42.1, lon >= -88, lon <= -87.5) %>%
  mutate(site_id = row_number(), name = branch_) %>%
  select(site_id, name, address, lat, lon)

dup_coords <- sum(duplicated(libs_clean[, c("lat", "lon")]))
dup_names <- sum(duplicated(libs_clean$name))

cleaning_report <- data.frame(
  step = c("Raw records", "Missing coordinates (removed)", "Coordinates outside Chicago area (removed)",
           "Duplicate coordinates (kept)", "Duplicate names (kept)", "Records after cleaning"),
  n = c(n_raw, n_missing_coords, nrow(outside_box), dup_coords, dup_names, nrow(libs_clean))
)
print(cleaning_report)
write_csv(libs_clean, "data/sites_clean.csv")

sites <- st_as_sf(libs_clean, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
sites_m <- st_transform(sites, 26916)
cat("sites_m CRS: EPSG:26916 (NAD83 / UTM zone 16N), units:", st_crs(sites_m)$units, "\n")

areas_raw <- st_read(area_file, quiet = TRUE)
areas <- areas_raw %>%
  transmute(district_id = as.integer(area_numbe), district_name = community) %>%
  st_make_valid() %>%
  st_transform(26916)

source_caption <- paste0("Source: City of Chicago Data Portal (libraries x8fc-8rcq; community areas igwz-8jzy), accessed ",
                         Sys.Date())

p_sites <- ggplot() +
  geom_sf(data = areas, fill = "grey95", color = "grey70") +
  geom_sf(data = sites_m, aes(color = "Library branch"), size = 1.8) +
  scale_color_manual(values = "darkblue", name = NULL) +
  labs(title = paste0("Chicago Public Library branches (n = ", nrow(sites_m), ")"),
       caption = source_caption) +
  theme_bw()
print(p_sites)
ggsave("plots/p_sites.png", p_sites, width = 6, height = 7)

buf500 <- st_union(st_buffer(sites_m, 500))
buf1000 <- st_union(st_buffer(sites_m, 1000))

buffer_summary <- data.frame(
  radius_m = c(500, 1000),
  union_area_km2 = c(as.numeric(st_area(buf500)) / 1e6,
                     as.numeric(st_area(buf1000)) / 1e6)
)
print(buffer_summary)
cat("1000 m area >= 500 m area:", buffer_summary$union_area_km2[2] >= buffer_summary$union_area_km2[1], "\n")

buffer_zones <- st_sf(radius_m = c(500, 1000), geometry = c(buf500, buf1000))

p_buffers <- ggplot() +
  geom_sf(data = areas, fill = "grey95", color = "grey70") +
  geom_sf(data = st_sf(layer = "Combined 500 m buffer", geometry = buf500),
          aes(fill = layer), alpha = 0.4, color = NA) +
  geom_sf(data = sites_m, aes(color = "Library branch"), size = 1) +
  scale_fill_manual(values = "orange", name = NULL) +
  scale_color_manual(values = "darkblue", name = NULL) +
  labs(title = "Area within 500 m of a Chicago library branch",
       caption = source_caption) +
  theme_bw()
print(p_buffers)
ggsave("plots/p_buffers.png", p_buffers, width = 6, height = 7)

city_km2 <- as.numeric(st_area(st_union(areas))) / 1e6
in_city_500 <- as.numeric(st_area(st_intersection(buf500, st_union(areas)))) / 1e6
in_city_1000 <- as.numeric(st_area(st_intersection(buf1000, st_union(areas)))) / 1e6

cat("Problem 2 interpretation: The", nrow(sites_m), "library branches are spread across the whole city,",
    "with the densest cluster near downtown and the north side.\n")
cat("The combined 500 m buffer covers", round(buffer_summary$union_area_km2[1], 1), "km2 and the 1000 m buffer covers",
    round(buffer_summary$union_area_km2[2], 1), "km2 (overlaps counted once, areas outside the city boundary included).\n")
cat("Inside the city that is about", round(100 * in_city_500 / city_km2), "% of Chicago within 500 m and about",
    round(100 * in_city_1000 / city_km2), "% within 1000 m of a library.\n")


cat("\n===== PROBLEM 3 =====\n")

boundary_checks <- data.frame(
  check = c("Number of areas", "Missing IDs", "Duplicate IDs", "Empty shapes", "Invalid shapes before repair",
            "Invalid shapes after repair"),
  n = c(nrow(areas), sum(is.na(areas$district_id)), sum(duplicated(areas$district_id)),
        sum(st_is_empty(areas)), sum(!st_is_valid(areas_raw)), sum(!st_is_valid(areas)))
)
print(boundary_checks)
cat("Boundaries CRS: EPSG:26916, same as sites_m:", st_crs(areas) == st_crs(sites_m), "\n")

matches <- st_intersects(sites_m, areas)
site_match_audit <- data.frame(
  site_id = sites_m$site_id,
  name = sites_m$name,
  n_matches = lengths(matches)
)
write_csv(site_match_audit, "site_match_audit.csv")

print(count(site_match_audit, n_matches))
cat("Sites without exactly one match:\n")
print(filter(site_match_audit, n_matches != 1))

one <- lengths(matches) == 1
assigned <- sites_m[one, ]
assigned$district_id <- areas$district_id[unlist(matches[one])]
n_unassigned <- sum(!one)

counts <- assigned %>%
  st_drop_geometry() %>%
  count(district_id, name = "n_sites")

district_counts_sf <- areas %>%
  mutate(area_km2 = as.numeric(st_area(.)) / 1e6) %>%
  left_join(counts, by = "district_id") %>%
  mutate(n_sites = replace_na(n_sites, 0L),
         sites_per_km2 = n_sites / area_km2)

district_counts <- district_counts_sf %>%
  st_drop_geometry() %>%
  select(district_id, district_name, n_sites, area_km2, sites_per_km2) %>%
  arrange(desc(n_sites))

print(as.data.frame(district_counts))
write_csv(district_counts, "district_counts.csv")

cat("All areas positive:", all(district_counts$area_km2 > 0), "\n")
cat("Counts total equals assigned sites:", sum(district_counts$n_sites) == nrow(assigned), "\n")
cat("Areas with zero sites:", sum(district_counts$n_sites == 0), "\n")

map_caption <- paste0(source_caption, "\n", n_unassigned,
                      " site(s) not assigned to exactly one area. Grey = missing value.")

p_count <- ggplot(district_counts_sf) +
  geom_sf(aes(fill = n_sites), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(name = "Libraries\n(count)", na.value = "grey60") +
  labs(title = "Library branches per community area", caption = map_caption) +
  theme_bw()
print(p_count)
ggsave("plots/p_count.png", p_count, width = 6, height = 7)

p_density <- ggplot(district_counts_sf) +
  geom_sf(aes(fill = sites_per_km2), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(name = "Libraries\nper km²", na.value = "grey60") +
  labs(title = "Library branches per square kilometer by community area", caption = map_caption) +
  theme_bw()
print(p_density)
ggsave("plots/p_density.png", p_density, width = 6, height = 7)

top_count <- district_counts %>% arrange(desc(n_sites)) %>% slice(1)
top_density <- district_counts %>% arrange(desc(sites_per_km2)) %>% slice(1)

cat("Problem 3 interpretation: Most community areas have zero or one library, so raw counts vary little.\n")
cat("The area with the most libraries is", top_count$district_name, "with", top_count$n_sites, "branches.\n")
cat("After dividing by area, the highest density is", top_density$district_name, "at",
    round(top_density$sites_per_km2, 2), "libraries per km2.\n")
cat("Large areas with one library look average on the count map but low on the density map, while small areas\n")
cat("with one library rank high on density, so accounting for area size changes the comparison.\n")