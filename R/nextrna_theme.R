

#' NextRNA Theme
#'

#' @param base_size font size
#'
nextrna_theme<-function (base_size = 14){
    palette = "black_and_white"
    base_family = "sans"
    base_fontface = "bold"
    base_line_size = base_size/14
    base_rect_size = base_size/14
    axis_text_angle = 0
    border = FALSE
    angle <- axis_text_angle[1]
    if (!angle %in% c(0, 45, 90, 270))
      stop(sprintf("'axis_text_angle' must be one of [%s]",
                   paste(c(0, 45, 90, 270), collapse = ", ")), ".\nFor other angles, use the guide_axis() function in ggplot2 instead",
           call. = FALSE)
    if (!palette %in% names(ggprism::ggprism_data$themes)) {
      stop("The palette ", paste(palette), " does not exist.\n         See names(ggprism_data$themes) for valid palette names")
    }
    colours <- deframe(ggprism::ggprism_data$themes[[palette]])
    if (!rlang::is_bool(border)) {
      stop("border must be either: TRUE or FALSE")
    }
    else {
      if (border) {
        panel.border <- element_rect(fill = NA)
        axis.line <- element_blank()
      }
      else if (!border) {
        panel.border <- element_blank()
        axis.line <- element_line()
      }
    }
    t <- theme(line = element_line(colour = colours["axisColor"],
                                   size = base_line_size, linetype = 1, lineend = "square"),
               rect = element_rect(fill = "white", colour = colours["axisColor"],
                                   size = base_rect_size, linetype = 1), text = element_text(family = base_family,
                                                                                             face = base_fontface, colour = colours["graphTitleColor"],
                                                                                             size = base_size, lineheight = 0.9, hjust = 0.5,
                                                                                             vjust = 0.5, angle = 0, margin = margin(), debug = FALSE),
               prism.ticks.length = unit(base_size/5, "pt"), axis.line = axis.line,
               axis.line.x = NULL, axis.line.y = NULL, axis.text = element_text(size = rel(0.95),
                                                                                colour = colours["axisLabelColor"]), axis.text.x = element_text(margin = margin(t = 0.8 *
                                                                                                                                                                  base_size/4), angle = axis_text_angle, hjust = ifelse(axis_text_angle %in%
                                                                                                                                                                                                                          c(45, 90, 270), 1, 0.5), vjust = ifelse(axis_text_angle %in%
                                                                                                                                                                                                                                                                    c(0, 90, 270), 0.5, 1)), axis.text.x.top = element_text(margin = margin(b = 0.8 *
                                                                                                                                                                                                                                                                                                                                              base_size/4), vjust = 0), axis.text.y = element_text(margin = margin(r = 0.5 *
                                                                                                                                                                                                                                                                                                                                                                                                                     base_size/4), hjust = 1), axis.text.y.right = element_text(margin = margin(l = 0.5 *
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  base_size/4), hjust = 0), axis.ticks = element_line(),
               axis.ticks.length = unit(base_size/2.5, "pt"), axis.ticks.length.x = NULL,
               axis.ticks.length.x.top = NULL, axis.ticks.length.x.bottom = NULL,
               axis.ticks.length.y = NULL, axis.ticks.length.y.left = NULL,
               axis.ticks.length.y.right = NULL, axis.title = element_text(colour = colours["axisTitleColor"]),
               axis.title.x = element_text(margin = margin(t = base_size *
                                                             0.6), vjust = 1), axis.title.x.top = element_text(margin = margin(b = base_size *
                                                                                                                                 0.6), vjust = 0), axis.title.y = element_text(angle = 90,
                                                                                                                                                                               margin = margin(r = base_size * 0.6), vjust = 1),
               axis.title.y.right = element_text(angle = -90, margin = margin(l = base_size *
                                                                                0.6), vjust = 0), legend.background = element_blank(),
               legend.spacing = unit(base_size, "pt"), legend.spacing.x = NULL,
               legend.spacing.y = NULL, legend.margin = margin(base_size/2,
                                                               base_size/2, base_size/2, base_size/2), legend.key = element_blank(),
               legend.key.size = unit(1.2, "lines"), legend.key.height = NULL,
               legend.key.width = unit(base_size * 1.8, "pt"), legend.text = element_text(size = rel(0.8),
                                                                                          face = "plain"), legend.text.align = NULL, legend.title = element_blank(),
               legend.title.align = NULL, legend.position = "right",
               legend.direction = NULL, legend.justification = "center",
               legend.box = NULL, legend.box.margin = margin(0, 0, 0,
                                                             0, "cm"), legend.box.background = element_blank(),
               legend.box.spacing = unit(base_size, "pt"), panel.background = element_rect(fill = ifelse(palette ==
                                                                                                           "office", colours["plottingAreaColor"], NA), colour = NA),
               panel.border = panel.border, panel.grid = element_line(linetype = 1,size = rel(0.5),colour = "grey92"),
               panel.grid.minor = element_blank(), panel.spacing = unit(base_size/2,
                                                                        "pt"), panel.spacing.x = NULL, panel.spacing.y = NULL,
               panel.ontop = FALSE, strip.background = element_blank(),
               strip.text = element_text(colour = colours["axisTitleColor"],
                                         size = rel(0.8), margin = margin(base_size/2.5, base_size/2.5,
                                                                          base_size/2.5, base_size/2.5)), strip.text.x = element_text(margin = margin(b = base_size/3)),
               strip.text.y = element_text(angle = -90, margin = margin(l = base_size/3)),
               strip.text.y.left = element_text(angle = 90), strip.placement = "inside",
               strip.placement.x = NULL, strip.placement.y = NULL, strip.switch.pad.grid = unit(base_size/4,
                                                                                                "pt"), strip.switch.pad.wrap = unit(base_size/4,
                                                                                                                                    "pt"), plot.background = element_rect(fill = colours["pageBackgroundColor"],
                                                                                                                                                                          colour = NA), plot.title = element_text(size = rel(1.2),
                                                                                                                                                                                                                  hjust = 0.5, vjust = 1, margin = margin(b = base_size)),
               plot.title.position = "panel", plot.subtitle = element_text(hjust = 0.5,
                                                                           vjust = 1, margin = margin(b = base_size/2)), plot.caption = element_text(size = rel(0.8),
                                                                                                                                                     hjust = 1, vjust = 1, margin = margin(t = base_size/2)),
               plot.caption.position = "panel", plot.tag = element_text(size = rel(1.2),
                                                                        hjust = 0.5, vjust = 0.5), plot.tag.position = "topleft",
               plot.margin = margin(base_size/2, base_size/2, base_size/2,
                                    base_size/2), complete = TRUE)
    ggprism::ggprism_data$themes[["all_null"]] %+replace% t
}
