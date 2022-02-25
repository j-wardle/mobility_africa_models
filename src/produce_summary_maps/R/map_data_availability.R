## data_features has at least the following columns:
## long, lat, datasource_type, data_available and adm_level
## adm_level is a string specified as ADM0, ADM1, ADM2, ADM3,
## or, specific_location
## We can't directly use data_features to make the barplot because
## data_features has multiple rows for each study, data_category, datasource_type
## and each country - one row for each adm unit modelled.
map_data_availability <- function(data_features, datasources) {
  fname <- ("shapefiles/afr_g2014_2013_0")
  africa <- st_read(fname)

  types <- unique(data_features$datasource_type)
  values <- datasource_palette[types]
  labels <- datasource_labels[types]
  x <- count(datasources, datasource_grped)
  x$label <- datasource_labels[x$datasource_grped]
  x$label <- glue::glue(" {x$label} ({x$n})")
  x$total <- sum(x$n)
  ##x <- arrange(x, n)
  x$datasource_grped <- reorder(x$datasource_grped, x$n)
  bar <- ggplot(x) +
    geom_col(aes(total, datasource_grped), fill = "gray", alpha = 0) +
    geom_col(
      aes(total/10, datasource_grped, fill = datasource_grped), alpha = 0.7
    ) +
    geom_text(
      aes(total/10, datasource_grped, label = label),
      hjust = 0, vjust = 0.5, size = 3
    ) +
    scale_fill_manual(
      values = datasource_palette,
      labels = datasource_labels,
      guide = "none"
    ) + theme_void() +
    theme(aspect.ratio = 1/3) +
    coord_cartesian(clip = "off")


  p <- ggplot() +
    theme_map() +
    xlab("") +
    ylab("")
  p <- p + geom_sf(data = africa, lwd = 0.1, alpha = 0.6)
  p <- p + coord_sf(datum = NA)
  p <- p +
    geom_jitter(
      data = data_features,
      aes(
        x = long,
        y = lat,
        col = datasource_type,
        ##alpha = data_available,
        size = scale
      ),
      key_glyph = draw_key_rect,
      width = 0.05,
      shape = 21 ## hollow circle; migh help see overlapping points.
    )

  p <- p + scale_color_manual(
    values = values,
    labels = labels,
    aesthetics = c("colour", "fill"),
    guide = "none" ##guide_legend(nrow = length(types))
  )

  p <- p + scale_alpha_manual(
    values = availability_scale, guide = "none"
  )
  p <- p + scale_size_manual(
    values = size_scale,
    guide = "none"
  )
  p <- p + paper_theme$theme


  df <- tibble(
    x = 0.01, y = 0.25,
    plot = list(bar)
  )
  p2 <- p +
    geom_plot_npc(
      data = df, aes(npcx = x, npcy = y, label = plot)
    )
  p2
}
