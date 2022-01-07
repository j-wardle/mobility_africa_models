## data_features has at least the following columns:
## long, lat, datasource_type, data_available and adm_level
## adm_level is a string specified as ADM0, ADM1, ADM2, ADM3,
## or, specific_location
map_data_availability <- function(data_features) {

    fname <- ("shapefiles/Africa_SHP")
    africa <- sf::st_read(fname)

    p <- ggplot() + theme_map() + xlab("") + ylab("")
    p <- p + geom_sf(data = africa, lwd = 0.1, alpha = 0.3)
    p <- p + coord_sf(datum = NA)
    p <- p +
        geom_point(data = data_features,
                   aes(x = long,
                       y = lat,
                       col = datasource_type,
                       alpha = data_available,
                       size = scale
                       ),
                   position = "jitter",
                   shape = 21 ## hollow circle; migh help see overlapping points.
                   )

    p <- p + scale_color_manual(
                 values = datasource_palette,
                 labels = datasource_labels,
                 aesthetics = c("colour", "fill")
             )

    p <- p + scale_alpha_manual(
                 values = availability_scale, guide = FALSE
             )
    p <- p + scale_size_manual(
               values = size_scale,
                 guide = FALSE
             )
    p <- p + paper_theme$theme + paper_theme$legend

    p

}
