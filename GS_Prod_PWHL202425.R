rm(list = ls())

library(readxl)
library(showtext)
source("GS_Library.R")

font_add("Saira Regular", "./Fonts/Saira_wdth100_wght400.ttf")
font_add("Saira Semi-Expanded", "./Fonts/Saira_wdth112-5_wght400.ttf")
font_add("Noto Sans", "./Fonts/NotoSans-Regular.ttf")

showtext_auto()

## IMPORT DATA
import_file <- "GSTool_PWHL202425.xlsx"
Dots_Data <- read_excel(
    path = import_file,
    sheet = "Output_Dots",
)
Team_Data <- read_excel(
    path = import_file,
    sheet = "Output_Teams",
)
Segment_Data <- read_excel(
    path = import_file,
    sheet = "Output_Segments",
)

### SETTINGS TO CHANGE ###
## PRIMARY DATA
Dots_Data    <- subset(Dots_Data,!is.na(Team))
Team_Data    <- subset(Team_Data,!is.na(Team))
Segment_Data <- subset(Segment_Data,!is.na(Team))
# filter_by = NA
# filter_key = NA
Plot_Title <- "PWHL 2024-25 Graphical Standings – Apr 08, 2025"

## LABEL ADJUSTMENT
Label_Adjust_X_Mult <- 0.7
Label_Adjust_Y_Mult <- 0.7
Label_Shared_Offset_X <- 0.5
Logo_Size <- 0.08
Label_Image_Folder_Path <- "./Logos/PWHL_202425/"

## SEGMENTS AND DOTS
# Base_Linewidth <- 3.6
# Target_Height <- 0.34
Dot_Size <- 0.5

## AXES AND BREAKS
X_Break_Size <- 2
Y_Break_Size <- 2
X_Minor_Break_Size <- 1
Y_Minor_Break_Size <- 1
X_Third_Break_Size <- NA
Y_Third_Break_Size <- 1/3
X_Axis_Label <- "Game"
Y_Axis_Label <- "Regulation wins above .500"
Max_X <- max(Dots_Data[["X"]])
# Max_Y <- max(abs(Dots_Data[["Y"]]))
Max_Y <- 8

## GRAPH ELEMENT COLOURS
Background_Colour <- 'white'
Dot_Colour   <- 'grey30'
Axis_Colour  <- 'grey50'
Major_Colour <- 'grey80'
Minor_Colour <- 'grey90'
Third_Colour <- 'grey94'

## OTHER GRAPH SETTINGS
Margins <- margin(t=0.5,b=0.25,l=0.5,r=1, unit = "cm")
Font_Family <- "Saira Semi-Expanded"
Text_Scale <- 3.5#*96/300
Tag_Position <- c(0.945,0.0)

## OUTPUT SETTINGS
File_Output_Name <- "./Outputs/PWHL/PWHL202425_W17.png"
Save_Settings_Path <- "./Outputs/PWHL/PWHL202425_W17.txt"
# png_width = 2560,
# png_height = 1440

### LEAGUE
output <- plot_image_GS(
    ## PRIMARY DATA
    dots_data = Dots_Data,
    segment_data = Segment_Data,
    team_data = Team_Data,
    # filter_by = NA,
    # filter_key = NA,
    plot_title = Plot_Title,
    
    ## LABEL ADJUSTMENT
    label_adjust_x_mult = Label_Adjust_X_Mult,
    label_adjust_y_mult = Label_Adjust_Y_Mult,
    label_shared_offset_x = Label_Shared_Offset_X,
    label_logo_size = Logo_Size,
    label_image_folder_path = Label_Image_Folder_Path,
    
    ## SEGMENTS AND DOTS
    # segment_base_linewidth = Base_Linewidth,
    # segment_target_height = Target_Height,
    dot_size = Dot_Size,
    
    ## AXES AND BREAKS
    x_break_size = X_Break_Size,
    y_break_size = Y_Break_Size,
    x_minor_break_size = X_Minor_Break_Size,
    y_minor_break_size = Y_Minor_Break_Size,
    x_third_break_size = X_Third_Break_Size,
    y_third_break_size = Y_Third_Break_Size,
    x_axis_label = X_Axis_Label,
    y_axis_label = Y_Axis_Label,
    max_X = Max_X,
    max_Y = Max_Y,
    
    ## GRAPH ELEMENT COLOURS
    background_colour = Background_Colour,
    dot_colour   = Dot_Colour,
    axis_colour  = Axis_Colour,
    major_colour = Major_Colour,
    minor_colour = Minor_Colour,
    third_colour = Third_Colour,
    
    ## OTHER GRAPH SETTINGS
    # margins = Margins,
    font_family = Font_Family,
    text_scale = Text_Scale,
    tag_position = Tag_Position,
    
    ## OUTPUT SETTINGS
    file_output_name = File_Output_Name,
    save_settings_path = Save_Settings_Path,
    # png_width = 2560,
    # png_height = 1440
)
print("success")