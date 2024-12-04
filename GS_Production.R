library(readxl)
library(showtext)
source("GS_Library.R")

font_add("Saira Regular", "./Fonts/Saira_wdth100_wght400.ttf")
font_add("Saira Semi-Expanded", "./Fonts/Saira_wdth112-5_wght400.ttf")

showtext_auto()

Label_Shared_Offset_X <- 0.1
Label_Adjust_X_Mult <- 0.2
Label_Adjust_Y_Mult <- 0.7
Margins <- margin(t=0.5,b=0.25,l=0.5,r=0.5, unit = "cm")
BaseLinewidth <- 4
TargetHeight <- 0.055
LogoSize <- 0.18
DotSize <- 4
TextScale <- 3.5
TagPosition <- c(0.92,0.0)
PlotTitle <- "PWHL 2024-25 Graphical Standings – Dec 03, 2024"
YAxisLabel <- "Regulation wins above .500"
XAxisLabel <- "Game"
LabelImageFolderPath <- "./Logos/PWHL_202425/"
import_file <- "GSTool_PWHL202425.xlsx"
FontFamily <- "Saira Semi-Expanded"
FileOutputName <- "./Outputs/PWHL/PWHL202425_W1.png"
SaveSettingsPath <- "./Outputs/PWHL/PWHL202425_W1.txt"

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

Dots_Data    <- subset(Dots_Data,!is.na(Team))
Team_Data    <- subset(Team_Data,!is.na(Team))
Segment_Data <- subset(Segment_Data,!is.na(Team))



plot_image_GS(
  dots_data = Dots_Data,
  team_data = Team_Data,
  segment_data = Segment_Data,

  # filter_by = "Conference",
  # filter_key = "NFC",

  plot_title = PlotTitle,

  label_adjust_x_mult = Label_Adjust_X_Mult,
  label_adjust_y_mult = Label_Adjust_Y_Mult,
  label_shared_offset_x = Label_Shared_Offset_X,
  label_logo_size = LogoSize,
  label_image_folder_path = LabelImageFolderPath,

  margins = Margins,

  segment_base_linewidth = BaseLinewidth,
  segment_target_height = TargetHeight,

  dot_size = DotSize,

  x_break_size = 1,
  y_break_size = 1,
  x_axis_label = XAxisLabel,
  y_axis_label = YAxisLabel,
  font_family = FontFamily,
  text_scale = TextScale,
  tag_position = TagPosition,

  file_output_name = FileOutputName,
  save_settings_path = SaveSettingsPath
)
print("success")