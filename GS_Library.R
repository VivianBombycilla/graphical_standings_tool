library(ggplot2)
library(ggimage)

# library(gridDebug)
# library(svglite)
# library(rsvg)
# library(gridSVG)
# library(egg)
library(ggh4x)

segment_yoffset <- function(
    shared_with,
    num_in_group,
    target_height
) {
    return ((target_height - ((num_in_group * 2 - 1) * target_height / shared_with))/2)
}
segment_yoffset_new <- function(
    shared_with,
    num_in_group,
    target_height
) {
    parts = 3*shared_with - 1
    first_part = 1
    increase_by = 3
    part = 1 + (num_in_group - 1)*increase_by
    return(
        (part/parts * target_height) - target_height/2
    )
}
segment_linewidth_factor <- function(
    shared_with
) {
    return(
        2/(3*shared_with-1)
    )
}


plot_image_GS <- function(
    ## PRIMARY DATA
    dots_data,
    segment_data,
    team_data,
    filter_by = NA,
    filter_key = NA,
    plot_title,
    
    ## LABEL ADJUSTMENT
    label_adjust_x_mult = 1,
    label_adjust_y_mult = 1,
    label_shared_offset_x = 0.5,
    label_logo_size = 0.1,
    label_image_folder_path,
    
    ## SEGMENTS AND DOTS
    # segment_base_linewidth = 0.5,
    segment_width_scaling = 0.9,
    dot_size = 0.5,
    
    ## AXES AND BREAKS
    x_break_size = 4,
    y_break_size = 4,
    x_minor_break_size = NA,
    y_minor_break_size = NA,
    x_third_break_size = NA,
    y_third_break_size = NA,
    x_axis_label = "",
    y_axis_label = "",
    max_X = NA,
    max_Y = NA,
    
    ## GRAPH ELEMENT COLOURS
    background_colour = 'white',
    dot_colour   = 'grey30',
    axis_colour  = 'grey50',
    major_colour = 'grey80',
    minor_colour = 'grey90',
    third_colour = 'grey94',
    
    ## OTHER GRAPH SETTINGS
    # margins = margin(t=0.5,b=0.5,l=0.5,r=0.5, unit = "cm"),
    font_family = "Roboto",
    text_scale = 1,
    tag_position = c(0.8, 0.02),
    
    ## OUTPUT SETTINGS
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
        dots_data    <- subset(
            dots_data,
            dots_data[[filter_by]]==filter_key
        )
        segment_data <- subset(
            segment_data,
            segment_data[[filter_by]]==filter_key
            )
        team_data    <- subset(
            team_data,
            team_data[[filter_by]]==filter_key
            ) 
        segment_data$SharedWith <- segment_data[[paste0("SharedWith",filter_by)]]
        segment_data$NumInGroup <- segment_data[[paste0("NumInGroup",filter_by)]]
        team_data$XOffset <- team_data[[paste0("XOffset",filter_by)]]
        team_data$YOffset <- team_data[[paste0("YOffset",filter_by)]]
    }
    if (is.na(max_X)) {max_X <- max(dots_data[["X"]])}
    if (is.na(max_Y)) {max_Y <- max(abs(dots_data[["Y"]]))}
    
    margins <- margin(t=71/300,b=35/300,l=62/300,r=120/300,unit = "in")
    segment_target_height <- segment_width_scaling*((1120/(max_Y*2+1))*dot_size)  /300*cm(1)
    print(paste("segment_target_height in cm",segment_target_height))
    scaled_dot_size <- ((1120/(max_Y*2+1))*dot_size) /300*cm(1)*10
    print(scaled_dot_size)
    print(paste("segment_target_height in units",dot_size * segment_width_scaling / cos(pi/4)*cos(pi/4)))
    
    # Get axis lines
    x_major_axes = seq(
        0,
        max_X,
        by=x_break_size
    )
    y_major_axes = seq(
        -max_Y + (max_Y %% y_break_size),
        max_Y,
        by=y_break_size
    )
    if (!is.na(x_minor_break_size)) {
        x_minor_axes = seq(
            0,
            max_X,
            by=x_minor_break_size
        )
    }
    if (!is.na(y_minor_break_size)) {
        y_minor_axes = seq(
            -max_Y + (max_Y %% y_minor_break_size),
            max_Y,
            by=y_minor_break_size
        )
    }
    if (!is.na(x_third_break_size)) {
        x_third_axes = seq(
            0,
            max_X,
            by=x_third_break_size
        )
    }
    if (!is.na(y_third_break_size)) {
        y_third_axes = seq(
            -max_Y + (max_Y %% y_third_break_size),
            max_Y,
            by=y_third_break_size
        )
    }
    
    # print(x_major_axes)
    # print(y_major_axes)
    # print(x_minor_axes)
    # print(y_minor_axes)
    # print(x_third_axes)
    # print(y_third_axes)
    
    # measurements
    pixels_per_Y = 1120/(max_Y*2+1)
    pixels_per_X = 2240/(max_X+1)
    print(paste("pixel diameter of dot",dot_size*pixels_per_Y))
    print(paste("cm diameter of dot",dot_size*pixels_per_Y/300*cm(1)))
    
    print(segment_width_scaling*dot_size*pixels_per_Y/300*cm(1)*10*cos(pi/4)* (.stroke / .pt))
    p <- (
    ggplot()
    
    ## DRAW AXES
    # draw third x-axes
    + {if (!is.na(x_third_break_size)) {
        geom_vline(
            xintercept = x_third_axes,
            colour = third_colour
        )
    }}
    # draw third y-axes
    + {if(!is.na(y_third_break_size)) {
        geom_hline(
            yintercept = y_third_axes,
            colour = third_colour
        )
    }}
    # draw minor x-axes
    + {if (!is.na(x_minor_break_size)) {
        geom_vline(
            xintercept = x_minor_axes,
            colour = minor_colour
        )
    }}
    # draw minor y-axes
    + {if(!is.na(y_minor_break_size)) {
        geom_hline(
            yintercept = y_minor_axes,
            colour = minor_colour
        )
    }}
    # draw major x-axes
    + geom_vline(
        xintercept = x_major_axes,
        colour = major_colour
    )
    # draw major y-axes
    + geom_hline(
        yintercept = y_major_axes,
        colour = major_colour
    )
    # draw x-axis
    + geom_hline(yintercept = 0, colour = axis_colour)
    
    # draw y-axis
    + geom_vline(xintercept = 0, colour = axis_colour)
    
    ## DRAW GRAPH
    # draw segments
    + geom_segment(
        data = segment_data,
        mapping = aes(
            x = StartX,
            y = StartY + segment_yoffset(
                shared_with = SharedWith,
                num_in_group = NumInGroup,
                target_height = dot_size
                ) * segment_width_scaling / cos(Angle)*cos(pi/4),
            xend = EndX,
            yend = EndY + segment_yoffset(
                shared_with = SharedWith,
                num_in_group = NumInGroup,
                target_height = dot_size
                ) * segment_width_scaling / cos(Angle)*cos(pi/4),
            colour = Team,
            linetype = "solid"
        ),
        lineend = "butt",
        linewidth = segment_width_scaling*dot_size*pixels_per_Y/300*cm(1)*10*cos(pi/4)* (.stroke / .pt)*segment_linewidth_factor(segment_data$SharedWith)
    )
    
    # draw dots
    + geom_point(
        data = dots_data,
        mapping = aes(
            x = X,
            y = Y
        ),
        size = scaled_dot_size,
        alpha = 1,
        color = dot_colour
    )
    
    # draw team labels
    + geom_image(
        data = team_data, 
        aes(
            #label = Team_Names[Team],
            image = paste0(label_image_folder_path,ImagePath),
            x = (X + label_shared_offset_x + XOffset*label_adjust_x_mult),
            y = (Y + YOffset*label_adjust_y_mult)
        ),
        size = label_logo_size,
        inherit.aes = FALSE,
    )
    
    ## GRAPH AESTHETICS
    # colour segments
    + scale_colour_manual(
        guide = 'none',
        values = team_data[["LineColour"]],
    )
    
    # style segments
    + scale_linetype_manual(
        values = c("solid" = "solid", "dashed" = "22")
    )
    
    ## GRAPH LIMITS AND LABELS
    + coord_cartesian(#fixed(
        xlim = c(0,max_X),
        ylim = c(-max_Y,max_Y),
        clip = "off"
    )
    + scale_x_continuous(
        breaks = seq( 0, max_X, by=x_break_size),
        expand = expansion(mult = 0, add = 0.5),
    )
    + scale_y_continuous(
        breaks = seq(
            -max_Y - (-max_Y %%y_break_size),
            max_Y + y_break_size,
            by=y_break_size
        ),
        expand = expansion(mult = 0, add = 0.5),
    )
    + labs(
        title = plot_title,
        x = x_axis_label,
        y = y_axis_label,
        tag = "Graphic by Vivian (vbbcla)"
    )
    + force_panelsizes(
        rows = unit(1120/300, "in"),
        cols = unit(2240/300, "in")
    )
    # + theme_void()
    ## THEME
    + theme(
        panel.background = element_rect(fill = background_colour),
        axis.ticks.length = unit(0,"cm"), # no ticks
        axis.text.x = element_text(size = 12*text_scale),
        axis.text.y = element_text(size = 12*text_scale),
        axis.title  = element_text(size = 13*text_scale),
        text = element_text(family=font_family),
        plot.margin = margins,
        legend.position="none",
        plot.title = element_text(
            size=16*text_scale,
            hjust = 0.5
        ),
        plot.tag = element_text(
            size = 8*text_scale,
            hjust = 1
        ),
        plot.tag.position = tag_position
    )
    )
    
    # data <- ggplot_build(p)
    # gtable <- ggplot_gtable(data)
    # return(gtable)
    
    # new_p <- set_panel_size(
    #     p,
    #     width = unit(2240/300,"inch"),
    #     height = unit(1120/300,"inch")
    # )
    # print(new_p)
    
    # print(current.vpTree())
    # print(seekViewport("panel.9-7-9-7"))
    # print(convertWidth(unit(1,'npc'), 'inch', TRUE))
    # print(ggplot_build(p))
    # sceneListing <- grid.ls(viewports=T, print=FALSE)
    # print(do.call("cbind", sceneListing))
    # print(grid.get(gPath = c("")))
    ggsave(
        filename = file_output_name,
        plot = p,
        # device = "png",
        scale = 1,
        width = png_width,
        height = png_height,
        units = "px",
        dpi = 300,
        # fix_text_size=FALSE
    )
    if (!is.na(save_settings_path)) {
        settings_file <- file(save_settings_path)
        writeLines(c(
                paste0("## PRIMARY DATA ##"),
                paste0("filter_by <- ",filter_by),
                paste0("filter_key <- ",filter_key),
                paste0("plot_title <- ",plot_title),
                paste0(""),
                paste0("## LABEL ADJUSTMENT ##"),
                paste0("label_adjust_x_mult <- ",label_adjust_x_mult),
                paste0("label_adjust_y_mult <- ",label_adjust_y_mult),
                paste0("label_shared_offset_x <- ",label_shared_offset_x),
                paste0("label_logo_size <- ",label_logo_size),
                paste0("label_image_folder_path <- ",label_image_folder_path),
                paste0(""),
                paste0("## SEGMENTS AND DOTS ##"),
                # paste0("segment_base_linewidth <- ",segment_base_linewidth),
                paste0("segment_target_height <- ",segment_target_height),
                paste0("dot_size <- ",dot_size),
                paste0(""),
                paste0("## AXES AND BREAKS ##"),
                paste0("x_break_size <- ",x_break_size),
                paste0("y_break_size <- ",y_break_size),
                paste0("x_minor_break_size <- ",x_minor_break_size),
                paste0("y_minor_break_size <- ",y_minor_break_size),
                paste0("x_third_break_size <- ",x_third_break_size),
                paste0("y_third_break_size <- ",y_third_break_size),
                paste0("x_axis_label <- ",x_axis_label),
                paste0("y_axis_label <- ",y_axis_label),
                paste0("max_X <- ",max_X),
                paste0("max_Y <- ",max_Y),
                paste0(""),
                paste0("## GRAPH ELEMENT COLOURS ##"),
                paste0("background_colour <- ",background_colour),
                paste0("dot_colour <- ",dot_colour),
                paste0("axis_colour <- ",axis_colour),
                paste0("major_colour <- ",major_colour),
                paste0("minor_colour <- ",minor_colour),
                paste0("third_colour <- ",third_colour),
                paste0(""),
                paste0("## OTHER GRAPH SETTINGS ##"),
                paste0("margins <- ",margins),
                paste0("font_family <- ",font_family),
                paste0("text_scale <- ",text_scale),
                paste0("tag_position <- ",tag_position),
                paste0(""),
                paste0("## OTHER GRAPH SETTINGS ##"),
                paste0("file_output_name <- ",file_output_name),
                paste0("save_settings_path <- ",save_settings_path),
                paste0("png_width <- ",png_width),
                paste0("png_height <- ",png_height)
            ),
            settings_file)
        close(settings_file)
    }
}

save_last_graphical_standings <- function(name) {
    ggsave(
        filename = name,
        plot = last_plot(),
        # device = "png",
        scale = 1,
        width = 2240,#2560,
        height = 1120,#1440,
        units = "px"
    )
}


