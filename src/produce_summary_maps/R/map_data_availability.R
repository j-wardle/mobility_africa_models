## data_features has at least the following columns:
## long, lat, datasource_type, data_available and adm_level
## adm_level is a string specified as ADM0, ADM1, ADM2, ADM3,
## or, specific_location
## We can't directly use data_features to make the barplot because
## data_features has multiple rows for each study, data_category, datasource_type
## and each country - one row for each adm unit modelled.
map_data_availability <- function(data_features, datasources) {
  Fname <- ("shapefiles/afr_g2014_2013_0")
  africa <- st_read(fname)

  types <- unique(data_features$datasource_type)
  values <- datasource_palette[types]
  labels <- datasource_labels[types]
  x <- count(datasources, datasource_grped)
  x$label <- datasource_labels[x$datasource_grped]
  x$label <- glue::glue("{x$label} ({x$n} / {sum(x$n)})")
  bar <- ggplot(x,aes(n, datasource_grped, fill = datasource_grped)) +
    geom_col(alpha = 0.8) +
    geom_text(
      aes(x = 0, y = datasource_grped, label = label)
    ) +
    scale_fill_manual(
      values = datasource_palette,
      labels = datasource_labels,
      guide = "none"
    ) + theme_void()

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
