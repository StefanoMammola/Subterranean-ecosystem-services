## ------------------------------------------------------------------------
## 'Subterranean ecosystem services'
## ------------------------------------------------------------------------

# Mammola, S. et al.

## ------------------------------------------------------------------------
# 'R script to reproduce the analyses'
## ------------------------------------------------------------------------

# Analysis performed with R (v. R 4.0.3) and R studio (v. 1.4.1103)
# Author: Stefano Mammola

# Loading R packages ------------------------------------------------------

if(!require("pacman")) {install.packages("pacman")}
pacman::p_load("dplyr","ggalluvial","ggplot2","tibble","tidyr")

# Script settings -----------------------------------------------------------
theme_set(theme_minimal())

theme_update(
  legend.background = element_blank(), #No background (legend)
  plot.background = element_blank(), #No background
  panel.grid = element_blank(), #No gridlines
  axis.text  = element_text(size = 10, colour = "grey10"), #Size and color of text
  axis.title = element_text(size = 12, colour = "grey10") #Size and color of text
)

# Loading the main database ------------------------------------------------

db <- read.csv(file = "Data/Database_ecosystem_services.csv", header = TRUE, sep = "\t", as.is = FALSE)

str(db)

# Data preparation --------------------------------------------------------

# Removing extra services non-specified
db <- db[db$Class != "Other",] |> droplevels()

# Numeric as numeric!
db$Subterranean.Any         <- as.numeric(as.character(db$Subterranean.Any))
db$Subterranean.Terrestrial <- as.numeric(as.character(db$Subterranean.Terrestrial))
db$Subterranean.Freshwater  <- as.numeric(as.character(db$Subterranean.Freshwater))
db$Subterranean.Marine      <- as.numeric(as.character(db$Subterranean.Marine))

colnames(db)

#Selecting only important columns for analysis
db <- db |>
  dplyr::select(Type,
                Section,
                Division,
                Class,
                Subt = Subterranean.Any,
                Terrestrial = Subterranean.Terrestrial,
                Freshwater = Subterranean.Freshwater,
                Marine = Subterranean.Marine,
                Stakeholder = Primary_stakeholder_std,
                Assessed_Ter = Assessed_quantitatively_terrestrial,
                Assessed_Fre = Assessed_quantitatively_freshwater,
                Assessed_Mar = Assessed_quantitatively_marine)

head(db,3)

# Analysis ----------------------------------------------------------------

#% services
sum(db$Subt)/nrow(db)
sum(db$Ter)/nrow(db)
sum(db$Fre)/nrow(db)
sum(db$Mar)/nrow(db)

#% by type
table(db[db$Subt > 0,]$Section)/table(db$Section)*100

# Plotting ecosystem services vs stakeholders ---------------------------------------------

my.colors.system <- c("Terrestrial" = "#A0522D", 
                      "Freshwater" = "#5DADE2", 
                      "Marine" = "#154360")

db |>
  tidyr::separate_rows(Stakeholder, sep = ";") |> # Splits rows by ";"
  tidyr::pivot_longer(cols = c(Terrestrial, Freshwater, Marine),
                        names_to = "System",
                        values_to = "Presence") |> 
                 dplyr::group_by(System, Section, Stakeholder) |>
                 dplyr::summarise(TotalPresence = sum(Presence), .groups = 'drop') |> # Summarize counts
                 dplyr::mutate( # Sort
                   System = factor(System, levels = c("Terrestrial", "Freshwater", "Marine")), 
                   Section = factor(Section, levels = c("Provisioning", "Regulation & Maintenance", "Cultural")),
                   Stakeholder = factor(Stakeholder, levels = c("Primary", "Secondary", "Tertiary", "Quaternary", "All (society)"))) |> 
                 ggplot(aes(axis1 = System, axis2 = Section, axis3 = Stakeholder, y = TotalPresence)) + #plot
                 geom_alluvium(aes(fill = System), width = 1/12, alpha = 0.4, color = NA) +
                 geom_stratum(aes(fill = System), width = 1/12,alpha = 0.4, color = "grey5") +
                 geom_text(stat = "stratum", aes(label = after_stat(stratum)), color = "grey5", hjust = 1, angle = 25) +
                 scale_x_discrete(limits = c("Subterranean system", "Service type\n(CICES Section)", "Main beneficiary\n(Economic sector)"), expand = c(0.15, 0.15)) +
                 scale_fill_manual(values = my.colors.system) +
                 labs(title = NULL, x = NULL, y = NULL) +
                 theme(
                   legend.position = "none",           # Remove legend
                   #axis.text.y = element_blank(),      # Remove y-axis text
                   #axis.ticks.y = element_blank(),     # Remove y-axis ticks
                   axis.text.x = element_text(size = 12) # Enlarge x-axis tick text
                 )

####
(figure2 <- db |> dplyr::select(Section, 
                    Terrestrial, 
                    Freshwater,
                    Marine, 
                    Assessed_Ter, 
                    Assessed_Fre, 
                    Assessed_Mar) |>
  {\(.) {replace(.,is.na(.),0)}}()  |> #replace NA
  tidyr::pivot_longer(cols = c(Terrestrial, Freshwater, Marine),
                      names_to = "System",
                      values_to = "Presence") |> 
  dplyr::group_by(Section,System) |>
  dplyr::summarise(TotalPresence = sum(Presence, na.rm = TRUE),
                   .groups = 'drop') |>
  dplyr::mutate(TotalTested = #adding total number of tested services
                  db |> dplyr::select(Section, 
                                      Assessed_Ter, 
                                      Assessed_Fre, 
                                      Assessed_Mar) |>
                  {\(.) {replace(.,is.na(.),0)}}()  |> #replace NA
                  tidyr::pivot_longer(cols = c(Assessed_Ter, Assessed_Fre, Assessed_Mar),
                                      names_to = "Tested",
                                      values_to = "Assessed") |> 
                  dplyr::group_by(Section,Tested) |>
                  dplyr::summarise(TotalTested = sum(Assessed, na.rm = TRUE),
                                   .groups = 'drop') |>
                  dplyr::pull(TotalTested),
                TotalCount = #adding total number of services
                  table(db$Section) |> 
                  data.frame() |> 
                  dplyr::pull(Freq) |> 
                  rep(each = 3)
  ) |>
  mutate(
    PercentTotal = (TotalPresence / TotalCount) * 100,
    PercentTested = (TotalTested / TotalPresence) * 100,
    Label = paste0(round(PercentTotal, 1), "% (",round(PercentTested, 1),"%)"),
    System = factor(System, levels = c("Terrestrial", "Freshwater", "Marine")), 
    Section = factor(Section, levels = c("Provisioning", "Regulation & Maintenance", "Cultural"))) |>
  #plotting
  ggplot(aes(x = System)) +
  geom_bar(aes(y = TotalCount), stat = "identity", fill = "white",color = "grey10", show.legend = FALSE) +   # Outer bar for TotalCount
  geom_bar(aes(y = TotalPresence, fill = System), stat = "identity", alpha = 0.6, color = NA, show.legend = FALSE) +   # Inner bar for TotalCount
  geom_bar(aes(y = TotalTested, fill = System), stat = "identity", color = NA, show.legend = FALSE) +   # Inner bar for TotalCount
  geom_text(aes(y = TotalPresence + 2, label = Label, color = System), 
            size = 3, show.legend = FALSE) +
  facet_wrap(~ Section) +
  scale_fill_manual(values = my.colors.system) +
  scale_color_manual(values = my.colors.system) +
  labs(title = NULL, x = NULL, y = "Number of ecosystem services") + 
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1, "lines"),
    axis.text.x = element_text(angle = 45, hjust = .8)
  ))

## Custom legend

library(patchwork) # For combining plots

# Create a dummy dataset for the legend
legend_data <- data.frame(
  Category = factor(c("TotalCount", "TotalPresence", "TotalTested"),
                    levels = c("TotalCount", "TotalPresence", "TotalTested")),
  Value = c(100, 60, 40),
  Label = rev(c("All\nservices", "Subterranean\nservices", "Tested\nsubterranean\nservices"))
)

# Calculate cumulative values for positioning
legend_data <- legend_data |> 
  dplyr::mutate(CumValue = cumsum(Value) - (Value / 2))  # Midpoint of each segment

# Create the custom legend
(custom_legend <- ggplot(legend_data, aes(x = 1, y = Value, fill = Category)) +
  geom_bar(stat = "identity", position = "identity", width = 0.5, alpha = 0.5,color = "black") +
  geom_text(aes(y = c(20,50,80), label = Label),  # Use calculated midpoints
            size = 3, color = c("white","white","black")) +
  # geom_text(aes(y = 65, label = "Subterranean %\n(Tested subterranean %)"), color = "darkblue",
  #            size = 2, show.legend = FALSE) +
  scale_fill_manual(values = c(
    "TotalCount" = "white",
    "TotalPresence" = "blue",
    "TotalTested" = "darkblue"
  )) +
  theme_void() +  # Remove unnecessary elements
  labs(title = NULL, x = "                                             ") +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    legend.position = "none",
    plot.margin = margin(10, 50, 50, 10),
    axis.title.x = element_text(angle = 90, hjust = .8))
  )




library(cowplot)

# Combine the main plot and the legend
final_plot <- cowplot::plot_grid(
  figure2,                      # Your existing main plot
  custom_legend,                  # The custom legend
  ncol = 2,                       # Arrange side by side
  rel_widths = c(5, 1)            # Adjust the width ratio (main plot wider)
)

final_plot



# 
# db |> dplyr::select(Section, 
#                     Terrestrial, 
#                     Freshwater,
#                     Marine, 
#                     Assessed_Ter, 
#                     Assessed_Fre, 
#                     Assessed_Mar) |>
#   {\(.) {replace(.,is.na(.),0)}}()  |> #replace NA
#   tidyr::pivot_longer(cols = c(Terrestrial, Freshwater, Marine),
#                       names_to = "System",
#                       values_to = "Presence") |> 
#   dplyr::group_by(Section,System) |>
#   dplyr::summarise(TotalPresence = sum(Presence, na.rm = TRUE),
#                    .groups = 'drop') |>
#   dplyr::mutate(TotalTested = #adding total number of tested services
#                   db |> dplyr::select(Section, 
#                                       Assessed_Ter, 
#                                       Assessed_Fre, 
#                                       Assessed_Mar) |>
#                   {\(.) {replace(.,is.na(.),0)}}()  |> #replace NA
#                   tidyr::pivot_longer(cols = c(Assessed_Ter, Assessed_Fre, Assessed_Mar),
#                                       names_to = "Tested",
#                                       values_to = "Assessed") |> 
#                   dplyr::group_by(Section,Tested) |>
#                   dplyr::summarise(TotalTested = sum(Assessed, na.rm = TRUE),
#                                    .groups = 'drop') |>
#                   dplyr::pull(TotalTested),
#                 TotalCount = #adding total number of services
#                   table(db$Section) |> 
#                   data.frame() |> 
#                   dplyr::pull(Freq) |> 
#                   rep(each = 3)
#   ) |>
#   mutate(
#     PercentTested = (TotalPresence / TotalCount) * 100,
#     Label = paste0(round(PercentTested, 1), "%")) |>
#   dplyr::mutate( # Sort
#     System = factor(System, levels = c("Terrestrial", "Freshwater", "Marine")), 
#     Section = factor(Section, levels = c("Provisioning", "Regulation & Maintenance", "Cultural"))) |>
#   #plotting
#   ggplot(aes(x = Section)) +
#   geom_bar(aes(y = TotalCount), stat = "identity", fill = "white",color = "grey10", show.legend = FALSE) +   # Outer bar for TotalCount
#   geom_bar(aes(y = TotalPresence, fill = Section), stat = "identity", alpha = 0, color = NA, show.legend = FALSE) +   # Inner bar for TotalCount
#   geom_bar(aes(y = TotalPresence, fill = Section), stat = "identity", alpha = 0.6, color = NA, show.legend = FALSE) +   # Inner bar for TotalCount
#   geom_bar(aes(y = TotalTested, fill = Section), stat = "identity", color = NA, show.legend = FALSE) +   # Inner bar for TotalCount
#   geom_text(aes(y = TotalCount + 1.5, label = Label), color = "grey5", size = 4) +
#   facet_wrap(~ System) +
#   labs(title = NULL, x = NULL, y = "Frequency") + 
#   theme_minimal()
