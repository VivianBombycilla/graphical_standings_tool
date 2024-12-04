library(ggplot2)
library(ggimage)

segment_yoffset <- function(
    shared_with,
    num_in_group,
    target_height = 0.1
) {
  return(target_height - ((num_in_group * 2 - 1) * target_height / shared_with))
}

plot_image_GS <- function(
    dots_data,
    segment_data,
    team_data,
    filter_by = NA,
    filter_key = NA,
    plot_title,
    
    #label_adjust = c(),
    label_adjust_x_mult = 1,
    label_adjust_y_mult = 1,
    label_shared_offset_x = 0.5,
    label_logo_size = 0.1,
    label_image_folder_path,
    
    margins = margin(t=0.5,b=0.5,l=0.5,r=0.5, unit = "cm"),
    
    segment_base_linewidth = 1,
    segment_target_height,
    
    dot_size = 2,
    
    x_break_size = 4,
    y_break_size = 4,
    font_family = "Roboto",
    tag_position = c(0.8, 0.02),
    text_scale = 1,
    x_axis_label = "",
    y_axis_label = "",
    max_X = NA,
    max_Y = NA,
    
    file_output_name,
    save_settings_path = NA,
    png_width = 2560,
    png_height = 1440
) {
  # Filter teams
  if (!is.na(filter_by)) {
    if (is.na(filter_key)) {
      stop("filter_key empty, but filter_by nonempty")
    }
    dots_data    <- subset(dots_data,   dots_data[[filter_by]]==filter_key)
    segment_data <- subset(segment_data,segment_data[[filter_by]]==filter_key)
    team_data    <- subset(team_data,   team_data[[filter_by]]==filter_key) 
    segment_data$SharedWith <- segment_data[[paste0("SharedWith",filter_by)]]
    segment_data$NumInGroup <- segment_data[[paste0("NumInGroup",filter_by)]]
    team_data$XOffset <- team_data[[paste0("XOffset",filter_by)]]
    team_data$YOffset <- team_data[[paste0("YOffset",filter_by)]]
  }
  if (is.na(max_X)) {max_X <- max(dots_data[["X"]])}
  if (is.na(max_Y)) {max_Y <- max(abs(dots_data[["Y"]]))}
  p <- (
    ggplot()
    
    # draw x-axis
    + geom_hline(yintercept = 0, colour = "gray50")
    
    # draw y-axis
    + geom_vline(xintercept = 0, colour = "gray50")
    
    # draw segments
    + geom_segment(
      data = segment_data,
      mapping = aes(
        x = StartX,
        y = StartY + segment_yoffset(shared_with = SharedWith,
                                     num_in_group = NumInGroup,
                                     target_height = segment_target_height),
        xend = EndX,
        yend = EndY + segment_yoffset(shared_with = SharedWith,
                                      num_in_group = NumInGroup,
                                      target_height = segment_target_height),
        colour = Team,
        linetype = SegmentType
      ),
      lineend = "round",
      linewidth = segment_base_linewidth/(segment_data$SharedWith) #TKTKTK do we move these into the aes?
    )
    
    # draw dots
    + geom_point(
      data = dots_data,
      mapping = aes(
        x = X,
        y = Y
      ),
      size = dot_size,
      alpha=1,
      color = "gray30")
    
    # draw team labels
    + geom_image(
      data = team_data, 
      aes(
        #label = Team_Names[Team],
        image = paste0(label_image_folder_path,ImagePath),
        x = (X
             + label_shared_offset_x
             + XOffset*label_adjust_x_mult),
        y = (Y
             + YOffset*label_adjust_y_mult)
      ),
      size = label_logo_size, #TKTK should this be in aes?
      inherit.aes = FALSE,
    )
    
    # colour segments
    + scale_colour_manual(
      guide = 'none',
      values = team_data[["LineColour"]],
      na.value = "grey60", #TKTK is this necessary
    )
    
    # style segments
    + scale_linetype_manual(
      values = c("solid" = "solid", "dashed" = "22")
    )
    + coord_cartesian(#fixed(
      xlim = c(-0.5,max_X+0.5),
      ylim = c(-max_Y,max_Y),
      clip = "off"
    )
    + scale_x_continuous(
      breaks = seq( 0, max_X, by=x_break_size),
      minor_breaks = seq( 0, max_X, by=1),
      expand = c(0,0),
    )
    + scale_y_continuous(
      breaks       = seq(-max_Y - (-max_Y %%y_break_size),  max_Y, by=y_break_size),
      minor_breaks = seq(-max_Y                          ,  max_Y, by=1),
      expand = c(0,0.5),
    )
    + labs(
      title = plot_title,
      x = x_axis_label,
      y = y_axis_label,
      tag = "Graphic by Vivian (vbbcla)"
    )
    + theme(
      panel.grid.major = element_line(colour = "gray80"),
      panel.grid.minor = element_line(colour = "gray90"),
      panel.background = element_rect(fill = "white"),
      axis.ticks.length = unit(0,"cm"),
      axis.text.x = element_text(size = 12*text_scale), 
      axis.text.y = element_text(size = 12*text_scale), 
      axis.title = element_text(size=13*text_scale),
      text = element_text(family=font_family),
      plot.margin = margins,
      legend.position="none",
      plot.title = element_text(
        size=16*text_scale,
        hjust = 0.5
      ),
      plot.tag = element_text(size = 8*text_scale),
      plot.tag.position = tag_position
    )
  )
  ggsave(
    filename = file_output_name,
    plot = p,
    device = "png",
    scale = 1,
    width = png_width,
    height = png_height,
    units = "px"
  )
  if (!is.na(save_settings_path)) {
    settings_file <- file(save_settings_path)
    writeLines(c(
      paste0("filter_by <- ",filter_by),
      paste0("filter_key <- ",filter_key),
      paste0("label_adjust_x_mult <- ",label_adjust_x_mult),
      paste0("label_adjust_y_mult <- ",label_adjust_y_mult),
      paste0("label_shared_offset_x <- ",label_shared_offset_x),
      paste0("label_logo_size <- ",label_logo_size),
      paste0("segment_base_linewidth <- ",segment_base_linewidth),
      paste0("segment_target_height <- ",segment_target_height),
      paste0("dot_size <- ",dot_size),
      paste0("max_X <- ",max_X),
      paste0("max_Y <- ",max_Y)
      ),
      settings_file)
    close(settings_file)
  }
}

save_last_graphical_standings <- function(name) {
  ggsave(
    filename = name,
    plot = last_plot(),
    device = "png",
    scale = 1,
    width = 2560,
    height = 1440,
    units = "px"
  )
}

