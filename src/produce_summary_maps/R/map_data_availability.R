## data_features has at least the following columns:
## long, lat, datasource_type, data_available and adm_level
## adm_level is a string specified as ADM0, ADM1, ADM2, ADM3,
## or, specific_location
map_data_availability <- function(data_features) {
  fname <- ("shapefiles/afr_g2014_2013_0")
  africa <- st_read(fname)

  types <- unique(data_features$datasource_type)
  values <- datasource_palette[types]
  labels <- datasource_labels[types]
  p <- ggplot() +
    theme_map() +
    xlab("") +
    ylab("")
  p <- p + geom_sf(data = africa, lwd = 0.1, alpha = 0.3)
  p <- p + coord_sf(datum = NA)
  p <- p +
    geom_point(
      data = data_features,
      aes(
        x = long,
        y = lat,
        col = datasource_type,
        alpha = data_available,
        size = scale
      ),
      position = "jitter",
      shape = 21 ## hollow circle; migh help see overlapping points.
    )

  p <- p + scale_color_manual(
    values = values,
    labels = labels,
    aesthetics = c("colour", "fill"),
    guide = guide_legend(nrow = length(types))
  )

  p <- p + scale_alpha_manual(
    values = availability_scale, guide = "none"
  )
  p <- p + scale_size_manual(
    values = size_scale,
    guide = "none"
  )
  p <- p + paper_theme$theme +
          paper_theme$legend

  p
}
