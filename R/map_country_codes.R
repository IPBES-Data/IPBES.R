#' Draw Map Map of Country Codes
#'
#' This function downloads and plots the GADM and IPBES regions according to a list of ISO codes provided.
#'
#' @param iso2c A character vector of ISO2 country codes. Will be converted to ISO3 country codes.
#' @param iso3c A character vector of ISO3 country codes. Default is NULL. If `iso2c` and `iso3c` are `NULL`, an example is used.
#' @param map_type A character string specifying the type of map to plot. Must be one of 'countries', 'regions', or 'subregions'. Default is 'countries'.
#' @param geodata_path A character string specifying the path to store the geospatial directory to download data to. Default is a temporary file.
#'
#' @importFrom ggplot2 aes ggplot scale_fill_viridis_d coord_sf
#' @importFrom tidyterra filter geom_spatvector
#' @importFrom terra vect
#' @importFrom dplyr left_join select distinct filter rename
#' @importFrom countrycode countrycode
#' @importFrom geodata world
#'
#' @md
#'
#' @return A ggplot object representing the map of the specified map_type.
#'
#' @examples
#' map_country_codes()
#' map_country_codes(iso3c = c("USA", "CAN"), map_type = "regions")
#'
#' @export
map_country_codes <- function(
    iso2c = NULL,
    iso3c = NULL,
    map_type = "countries",
    geodata_path = tempfile()) {
    if (!(map_type %in% c("countries", "regions", "subregions"))) {
        stop("`map_type` must be one of 'countries', 'regions', 'subregions'")
    }

    # This script downloads and plots the GADM and IPBES regions according to a list of iso codes provided
    # written by @yanisica

    if (!is.null(iso2c) & is.null(iso3c)) {
        iso3 <- countrycode::countrycode(origin = "iso2c", destination = "iso3c", iso2c)
    }
    if (is.null(iso3c)) {
        ## List of countries (convert to iso3)----
        iso3 <- system.file(
            package = "IPBES.R",
            "spatial", "countrycodes.csv"
        ) |>
            read.csv() |>
            dplyr::select(iso2 = x) |>
            dplyr::distinct() |>
            dplyr::filter(!is.na(iso2)) |>
            select(iso2) |>
            unlist() |>
            countrycode::countrycode(origin = "iso2c", destination = "iso3c") |>
            data.frame() |>
            rename(iso3 = 1) |>
            filter(!is.na(iso3))

        # Some values were not matched unambiguously: XK --> Kosovo??
        # iso3[97, 1] <- "XKO"
    }

    if (map_type == "countries") {
        # Get GADM world (v3.6)
        world <- geodata::world(
            resolution = 5, # low
            level = 0, # country level
            path = geodata_path,
            version = "3.6" # only available
        )

        # Get selected countries-----
        selected_country <- tidyterra::filter(world, GID_0 %in% unique(iso3$iso3))

        # Plot----
        countries <- tidyterra::filter(
            world,
            GID_0 %in% unique(iso3$iso3)
        ) |>
            ggplot2::ggplot() +
            tidyterra::geom_spatvector(
                mapping = ggplot2::aes(fill = NAME_0),
                color = NA,
                data = NULL,
                na.rm = FALSE,
                show.legend = FALSE
            ) +
            ggplot2::scale_fill_viridis_d() +
            ggplot2::coord_sf(crs = 4326) # latlong
        return(countries)
    } else {
        ipbes_regions_simp <- terra::vect(
            system.file(
                package = "IPBES.R",
                "spatial", "IPBES_Regions_Subregions2fixed", "IPBES_Regions_Subregions2OK_simp.gpkg"
            )
        )
        ipbes_regions_df <- read.csv(
            system.file(
                package = "IPBES.R",
                "spatial", "IPBES_Regions_Subregions2fixed", "IPBES_Regions_Subregions2.csv"
            )
        )

        selected_iso_regions <- iso3 |>
            data.frame() |>
            dplyr::left_join(
                ipbes_regions_df,
                by = c("iso3" = "GID_0")
            ) |>
            dplyr::select(
                -ISO_3,
                -Area
            )
    }

    if (map_type == "regions") {
        region <- tidyterra::filter(
            ipbes_regions_simp,
            Region %in% unique(selected_iso_regions$Region)
        ) |>
            ggplot2::ggplot() +
            tidyterra::geom_spatvector(
                mapping = ggplot2::aes(fill = Region),
                color = NA,
                data = NULL,
                na.rm = FALSE,
                show.legend = TRUE
            ) +
            ggplot2::scale_fill_viridis_d() +
            ggplot2::coord_sf(crs = 4326) # latlong
        return(region)
    } else if (map_type == "subregions") {
        sub_region <- tidyterra::filter(
            ipbes_regions_simp,
            Sub_Region %in% unique(selected_iso_regions$Sub_Region)
        ) |>
            ggplot2::ggplot() +
            tidyterra::geom_spatvector(
                mapping = ggplot2::aes(fill = Sub_Region),
                color = NA,
                data = NULL,
                na.rm = FALSE,
                show.legend = FALSE
            ) +
            ggplot2::scale_fill_viridis_d() +
            ggplot2::coord_sf(crs = 4326) # latlong
        return(sub_region)
    }
}







# Get GADM selected countries (v4.1) --> needs ISO3 codes
# selected_iso = gadm(country = iso3, #character. Three-letter ISO code or full country name
#                     level=0,        #country level
#                     path = "C:/Users/yanis/Documents/regions/gadm4/",
#                     version="latest",
#                     resolution=2    #low
#                     )
# plot(selected_iso)

## IPBES regions-----
## If no local version of IPBES regions and subregions --> download from Zenodo
# dir_git = "C:/Users/yanis/Documents/regions/"
# recordID <- "3923633"
# url_record <- paste0("https://zenodo.org/api/records/", recordID)
# record <- httr::GET(url_record)
# record # Status 200 indicates a successful download
# url_shape
## GADM----
## If local version of GADM available(https://gadm.org/download_world.html)
# gadm <- terra::vect("inst/spatial/gadm_410.gpkg") |>
#     dplyr::select(GID_0, NAME_0)
# # plot(gadm)

## If no local version of GADM --> download using geodata
# Get GADM world (v3.6)
# world <- geodata::world(
#     resolution = 5, # low
#     level = 0, # country level
#     path = "inst/spatial",
#     version = "3.6" # only available
# )

# plot(world)

# Get GADM selected countries (v4.1) --> needs ISO3 codes
# selected_iso = gadm(country = iso3, #character. Three-letter ISO code or full country name
#                     level=0,        #country level
#                     path = "C:/Users/yanis/Documents/regions/gadm4/",
#                     version="latest",
#                     resolution=2    #low
#                     )
# plot(selected_iso)

## IPBES regions-----
## If no local version of IPBES regions and subregions --> download from Zenodo
# dir_git = "C:/Users/yanis/Documents/regions/"
# recordID <- "3923633"
# url_record <- paste0("https://zenodo.org/api/records/", recordID)
# record <- httr::GET(url_record)
# record # Status 200 indicates a successful download
# url_shape <- content(record)$files[[5]]$links$download # Contains the url to download the shapefile
# httr::GET(url_shape, write_disk(paste0(dir_git,"/","ipbes_regions_subregions.zip"), overwrite = T)) # Downloads shapefile
# unzip(paste0(dir_git,"/","ipbes_regions_subregions.zip")) # unzips shapefile
# ipbes_regions <- terra::vect(paste0(dir_git,"/","IPBES_Regions_Subregions/IPBES_Regions_Subregions2.shp"))
# ipbes_regions_df <- as.data.frame(ipbes_regions)
# write_csv(ipbes_regions_df,paste0(dir_git,"IPBES_Regions_Subregions/IPBES_Regions_Subregions2.csv"))

# WARNING!
# Russian and Fiji were nor wrap to the date line and created a number of geometry issues
# Fixes: created single features, reduced the extent and merged again Russia and Fiji and appended to other countries
# ipbes_regions_fixed <- terra::vect("C:/Users/yanis/Documents/regions/IPBES_Regions_Subregions/IPBES_Regions_Subregions2OK.gpkg")

# Simplify Geometries
# ipbes_regions_simp <- simplifyGeom(ipbes_regions_fixed, tolerance=0.1, preserveTopology=TRUE, makeValid=TRUE)
# writeVector(ipbes_regions_simp,"C:/Users/yanis/Documents/regions/IPBES_Regions_Subregions/IPBES_Regions_Subregions2OK_simp.gpkg")
# plot(ipbes_regions_fixed)




# Create temp datasets----
## Attach IPBES regions to selected countries-----
# selected_iso_regions <- iso3 |>
#     data.frame() |>
#     dplyr::left_join(
#         ipbes_regions_df,
#         by = c("iso3" = "GID_0")
#     ) |>
#     dplyr::select(
#         -ISO_3,
#         -Area
#     )

# ## Get selected countries-----
# selected_country <- tidyterra::filter(world, GID_0 %in% unique(selected_iso_regions$iso3))
# ## Get selected regions/subregions-----
# selected_Region = filter(ipbes_regions_fixed, Region %in% unique(selected_iso_regions$Region))
# selected_Sub_Region = filter(ipbes_regions_fixed, Sub_Region %in% unique(selected_iso_regions$Sub_Region))

# Plot----

# countries <- tidyterra::filter(
#     world,
#     GID_0 %in% unique(selected_iso_regions$iso3)
# ) |>
#     ggplot2::ggplot() +
#     tidyterra::geom_spatvector(
#         mapping = ggplot2::aes(fill = NAME_0),
#         color = NA,
#         data = NULL,
#         na.rm = FALSE,
#         show.legend = FALSE
#     ) +
#     ggplot2::scale_fill_viridis_d() +
#     ggplot2::coord_sf(crs = 4326) # latlong
# countries

# region <- tidyterra::filter(
#     ipbes_regions_simp,
#     Region %in% unique(selected_iso_regions$Region)
# ) |>
#     ggplot2::ggplot() +
#     tidyterra::geom_spatvector(
#         mapping = ggplot2::aes(fill = Region),
#         color = NA,
#         data = NULL,
#         na.rm = FALSE,
#         show.legend = TRUE
#     ) +
#     ggplot2::scale_fill_viridis_d() +
#     ggplot2::coord_sf(crs = 4326) # latlong
# region

# sub_region <- tidyterra::filter(
#     ipbes_regions_simp,
#     Sub_Region %in% unique(selected_iso_regions$Sub_Region)
# ) |>
#     ggplot2::ggplot() +
#     tidyterra::geom_spatvector(
#         mapping = ggplot2::aes(fill = Sub_Region),
#         color = NA,
#         data = NULL,
#         na.rm = FALSE,
#         show.legend = FALSE
#     ) +
#     ggplot2::scale_fill_viridis_d() +
#     ggplot2::coord_sf(crs = 4326) # latlong
# sub_region
