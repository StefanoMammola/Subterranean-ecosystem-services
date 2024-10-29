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
theme_set(theme_bw())

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

db |>
  tidyr::separate_rows(Stakeholder, sep = ";") |> # Splits rows by ";"
  pivot_longer(cols = c(Terrestrial, Freshwater, Marine),
                        names_to = "System",
                        values_to = "Presence") |> 
                 # Sum the presences
                 group_by(System, Section, Stakeholder) |> 
                 summarise(TotalPresence = sum(Presence), .groups = 'drop') |> # Summarize counts
                 dplyr::mutate( # Sort
                   System = factor(System, levels = c("Terrestrial", "Freshwater", "Marine")), 
                   Section = factor(Section, levels = c("Provisioning", "Regulation & Maintenance", "Cultural")),
                   Stakeholder = factor(Stakeholder, levels = c("Primary", "Secondary", "Tertiary", "Quaternary", "All (society)"))) |> 
                 # Plot
                 ggplot(aes(axis1 = System, axis2 = Section, axis3 = Stakeholder, y = TotalPresence)) + 
                 geom_alluvium(aes(fill = System), width = 1/12, alpha = 0.4, color = NA) +
                 geom_stratum(aes(fill = System), width = 1/12,alpha = 0.4, color = "grey35") +
                 geom_text(stat = "stratum", aes(label = after_stat(stratum)), color = "grey5", hjust = 1, angle = 25) +
                 scale_x_discrete(limits = c("Subterranean system", "Service type\n(CICES Section)", "Main beneficiary\n(Economic sector)"), expand = c(0.15, 0.15)) +
                 scale_fill_manual(values = c("Terrestrial" = "#8B4513", "Freshwater" = "#4682B4", "Marine" = "darkblue")) +
                 labs(title = NULL, x = NULL, y = NULL) +
                 theme_minimal() + 
                 theme(
                   legend.position = "none",           # Remove legend
                   #axis.text.y = element_blank(),      # Remove y-axis text
                   #axis.ticks.y = element_blank(),     # Remove y-axis ticks
                   axis.text.x = element_text(size = 12) # Enlarge x-axis tick text
                 )


# Trim white spaces if needed
db_names$author  <- gsub("\\s+", " ", db_names$author)  # Replace multiple spaces with a single space
db_names$author  <- gsub("\\s+$", "", db_names$author)  # Remove trailing space at the end of the string

db_authors$Name  <- gsub("\\s+", " ", db_authors$Name)  # Replace multiple spaces with a single space
db_authors$Name  <- gsub("\\s+$", "", db_authors$Name)  # Remove trailing space at the end of the string

db_authors$Country  <- gsub("\\s+", " ", db_authors$Country)  # Replace multiple spaces with a single space
db_authors$Country  <- gsub("\\s+$", "", db_authors$Country)  # Remove trailing space at the end of the string

# db_authors: Separate the first name and subset
db_authors <- db_authors |>
  dplyr::mutate(first_name = sub(",.*| &.*", "", Name)) 

# create an unique ID based on country and sex to identify multi-name authors
db_authors$ID <- paste(db_authors$first_name,db_authors$Sex,db_authors$Country, sep = "_")

# # take distinct
# db_authors <- db_authors |> 
#   dplyr::distinct(ID, .keep_all = TRUE)

# Calculate proportion of etymologies by author ---------------------------

# db_name: Separate the first name and select relevant columns
db_names <- db_names |>
  dplyr::mutate(first_name = sub(",.*| &.*", "", author)) |>
  dplyr::select(author, 
                first_name,
                year,
                size,
                shape,
                colour,
                behaviour,
                ecology,
                geography,
                scientists,
                otherPeople,
                modernCulture,
                pastCulture,
                others)

ncol_db_names <- ncol(db_names)

db_authors <- db_authors |>
  dplyr::mutate_if(is.character, as.factor)
 
for(i in 1 : nlevels(db_authors$ID)){
  
  db_i <- db_authors[db_authors$ID == levels(db_authors$ID)[i],]

  db_names_i <- db_names[db_names$author %in% as.character(db_i$Name),]
  
  colSums_i     <- colSums(db_names_i[, 4 : ncol_db_names])
  rowSum_i      <- sum(colSums_i)
  year_min_i    <- min(db_names_i$year, na.rm = TRUE)
  year_max_i    <- max(db_names_i$year, na.rm = TRUE)
  year_range_i  <- year_max_i-year_min_i
  
  #Store
      if(i > 1) {
        db_analysis2 <- c(name = as.character(db_i$first_name)[1], 
                          ID   = as.character(db_i$ID)[1], 
                          sex = as.character(db_i$Sex)[1], 
                          country = as.character(db_i$Country)[1], 
                          continent = as.character(db_i$Continent)[1], 
                          colSums_i, 
                          tot = rowSum_i, 
                          year_min = year_min_i, 
                          year_max = year_max_i, 
                          year_range = year_range_i)
        
        db_analysis  <- rbind(db_analysis, db_analysis2)
        
      } else {
        db_analysis <- c(name = as.character(db_i$first_name)[1], 
                         ID   = as.character(db_i$ID)[1], 
                         sex = as.character(db_i$Sex)[1], 
                         country = as.character(db_i$Country)[1], 
                         continent = as.character(db_i$Continent)[1], 
                         colSums_i, 
                         tot = rowSum_i, 
                         year_min = year_min_i, 
                         year_max = year_max_i, 
                         year_range = year_range_i)
      }
} 

# Set the final database
db_analysis <- data.frame(db_analysis)
db_analysis[,6:ncol(db_analysis)] <- apply(db_analysis[,6:ncol(db_analysis)],2,as.numeric)
rownames(db_analysis) <- NULL

db_analysis <- db_analysis |> 
  dplyr::mutate_if(is.character, as.factor)

head(db_analysis,5)

# double-check very longeve authors ---------------------------------------

db_check <- db_analysis[db_analysis$year_range > 50,] ; db_check <- droplevels(db_check)

dev.off()
par(mfrow = c(5,6), mar = c(rep(2,4)))
for(i in 1 : nlevels(db_check$ID)){ 
  
  db_i <- db_names[db_names$author %in% db_authors[db_authors$ID == levels(db_check$ID)[i],]$Name,]
  dotchart(db_i$year, main = db_i$first_name[1])

}

#after checking WSC: Butler, Costa, Lucas, Edwards, González, Hirst, Machado, Mcheidze, Miller, Müller, Rossi, Saito, Schmidt, Smith, Wang
db_check <- db_check[db_check$name %in% c("Butler",
                                          "Costa",
                                          "Edwards", 
                                          "González", 
                                          "Hirst",
                                          "Lucas",
                                          "Machado", 
                                          "Mcheidze", 
                                          "Miller", 
                                          "Müller", 
                                          "Rossi", 
                                          "Saito", 
                                          "Schmidt", 
                                          "Smith", 
                                          "Wang"),]
db_check <- droplevels(db_check)

dev.off()
par(mfrow = c(4,4), mar = c(rep(2,4)))
for(i in 1 : nlevels(db_check$ID)){ 
  
  db_i <- db_names[db_names$author %in% db_authors[db_authors$ID == levels(db_check$ID)[i],]$Name,]
  dotchart(db_i$year, main = db_i$first_name[1])
  
}

year_split <- c(1900,1950,1990,1980,1960,1900,2000,1990,1990,1900,1810,1960,1940,1960,1970)

db_check <- data.frame(ID = db_check$ID, 
                       Name = db_check$name, 
                       year_split, 
                       sex = db_check$sex,
                       country = db_check$country,
                       continent = db_check$continent)

# Correct the names that are multiple authors
db_analysis <- db_analysis[!db_analysis$ID %in% db_check$ID,] #remove the uncorrect assignment from the analysis database

for(i in 1 : nlevels(db_check$ID)){
  
  db_i <- db_check[db_check$ID == levels(db_check$ID)[i],]
  
  db_names_i <- db_names[db_names$author %in% as.character(db_i$Name),]
  
  db_names_before <- db_names_i[db_names_i$year < db_i$year_split, ]
  db_names_after  <- db_names_i[db_names_i$year > db_i$year_split, ]
  
  
  colSums <- rbind(colSums(db_names_before[, 4 : ncol_db_names]), colSums(db_names_after[, 4 : ncol_db_names]))
  rowSum  <- rowSums(colSums)
  min     <-  c(min(db_names_before$year, na.rm = TRUE), min(db_names_after$year, na.rm = TRUE))
  max     <-  c(min(db_names_before$year, na.rm = TRUE), min(db_names_after$year, na.rm = TRUE))
  
  db_analysis_i <- cbind(name = paste(db_names_i$first_name[1:2], 1:2),
                         ID   = paste(rep(db_i$ID,2), 1:2),
                         sex  = as.character(db_i$sex)[1], 
                         country = as.character(db_i$country)[1], 
                         continent = as.character(db_i$continent)[1],
                         colSums, 
                         tot = rowSum,
                         year_min = min, 
                         year_max = max,
                         year_range = max - min)
  
  db_analysis <- rbind(db_analysis, db_analysis_i)
 
} 

### clean the workspace
all_objects <- ls() ; keep_objects <- c("db_analysis", "db_names", "db_authors")
rm(list = setdiff(all_objects, keep_objects)) ; rm(all_objects, keep_objects)

# Set and check the final database --------------------------------------------------

db_analysis <- db_analysis |> 
  dplyr::mutate_if(is.character, as.numeric)

tail(db_analysis, 30)

str(db_analysis)

# percentage female coauthors

nrow(db_analysis[db_analysis$sex == "F",])/nrow(db_analysis)

# Group etymologies by Type (see Mammola et al., 2023 ZJLS)
db_analysis <- db_analysis |>
  dplyr::mutate(
    morphology = size + shape + colour,  
    ecology2 = behaviour + ecology,
    people = scientists + otherPeople,
    culture = modernCulture + pastCulture + others)

# Remove etymologies for which we didn't find a meaning
db_analysis <- db_analysis[db_analysis$tot > 0,] #removed 5 observations

# Remove missing genders and rename
db_analysis <- db_analysis[db_analysis$sex != "?",] #removed 12 observations

db_analysis <- droplevels(db_analysis)

db_analysis <- db_analysis |>
        dplyr::mutate(gender = dplyr::recode(sex, F = "Female", M = "Male"))

# General plots & stats ------------------------------------------------------

# average ± s.d. number of species description per author
mean(db_analysis$tot, na.rm = TRUE) ; sd(db_analysis$tot, na.rm = TRUE)
range(db_analysis$tot, na.rm = TRUE)

# N authors by Continent
table(db_analysis$continent)

# N authors by Gender
table(db_analysis$gender)

##### plot N authors by continent & gender
(plot_1 <- 
    table(db_analysis$continent, db_analysis$gender) |> 
    data.frame() |> 
    dplyr::mutate(continent = forcats::fct_relevel(Var1, c("Europe", "Asia", "N America", "S America", "Oceania", "Africa"))) |>
    dplyr::mutate(gender = dplyr::recode(Var2, F = "Female", M = "Male")) |>
    ggplot2::ggplot(aes(y = Freq, x = continent, fill = gender))+
    geom_bar(stat="identity", colour = "grey5", size = .4)+
    labs(x = NULL, 
         y = "Number of authors")+
    scale_fill_manual("",values = c("purple","grey20"))+
    #scale_x_discrete(guide = guide_axis(n.dodge = 2))+
    theme(legend.position = c(0.8, 0.7))
)
#####

db_analysis_temporal <- db_names |> 
                    dplyr::select(author, year, scientists, otherPeople) |>
                    dplyr::left_join(db_authors |> 
                                           dplyr::rename("author" = "Name") |>
                                           dplyr::select(author, Sex, Continent),
                                     by = "author",
                                     relationship = "many-to-many") |> 
                    dplyr::mutate(continent = forcats::fct_relevel(Continent, 
                                                                   c("Asia", "Africa", "Europe", "N America", "S America", "Oceania"))) |>
                    dplyr::mutate(gender = dplyr::recode(Sex, F = "Female", M = "Male")) |>
                    dplyr::select(author,gender,continent,year, scientists, otherPeople) |> 
                    dplyr::mutate(people = scientists + otherPeople) |> 
                    na.omit()

# Remove missing genders and NA
db_analysis_temporal <- db_analysis_temporal[db_analysis_temporal$gender != "?",]
db_analysis_temporal <- droplevels(db_analysis_temporal)

(plot_2 <- 
table(db_analysis_temporal$year,db_analysis_temporal$gender,db_analysis_temporal$continent) |>
  data.frame() |>
  dplyr::rename("year" = "Var1","gender" = "Var2", "instances" = "Freq") |>
  dplyr::mutate(year = as.numeric(as.character(year))) |>
  ggplot2::ggplot( aes(x = year, y = instances, color = gender, group = gender)) +
  facet_wrap( ~ Var3, nrow = 3, ncol = 3)+ 
  geom_line(size = .5)+
  scale_x_continuous(breaks = seq(from=1757,to=2010,by=60))+
  scale_color_manual("",values = c("purple","grey20"))+
  labs(x = NULL, 
       y = "NUmber of species descriptions")
)

db_plot_3 <- db_analysis_temporal[db_analysis_temporal$people > 0, ] |> 
  droplevels()

(plot_3 <- 
    table(db_plot_3$year,db_plot_3$gender,db_plot_3$continent) |>
    data.frame() |>
    dplyr::rename("year" = "Var1","gender" = "Var2", "continent" = "Var3", "instances" = "Freq") |>
    dplyr::mutate(year = as.numeric(as.character(year))) |>
    ggplot2::ggplot( aes(x = year, y = instances, color = gender, group = gender)) +
    facet_wrap( ~ continent, nrow = 3, ncol = 3)+ 
    geom_line(size = .5)+
    scale_x_continuous(breaks = seq(from=1757,to=2010,by=60))+
    scale_color_manual("",values = c("purple","grey20"))+
    labs(x = NULL, 
         y = "Number of species dedicated to people")
)

rm(db_plot_3) #clean

# Regression analysis -----------------------------------------------------
db_analysis <- na.omit(db_analysis)

#People
m1 <- glm(cbind(people,tot) ~ gender + scale(year_min) + scale(year_range) + continent,
          family = binomial(link = "logit"), data = db_analysis)

performance::check_overdispersion(m1)
performance::check_collinearity(m1)
summary(aov(m1))
summary(m1)

(plot_trend1 <- 
    ggplot2::ggplot(db_analysis, aes(x = year_min, y = people/tot, fill = gender, color = gender, size = year_range)) + 
    #facet_wrap( ~ continent, nrow = 3, ncol = 3, scale = "free")+
    geom_point(alpha = 0.6, shape = 21) +
    geom_smooth(se = TRUE, 
                method = "glm", 
                formula = y ~ x,
                method.args = list(family = binomial(link = "logit")), size = 1, alpha = 0.2) +
    scale_color_manual("",values = c("purple","grey20"))+
    scale_fill_manual("",values = c("purple","grey20"))+
    scale_size("Years of\nactivity", breaks = c(1,20,40,60), labels = c(1,20,40,60))+
    labs(x = "Year of first species description", 
         y = "Proportion of etymologies dedicated to people")
)

#Scientists
m2 <- glm(cbind(scientists,tot) ~ gender + scale(year_min) + scale(year_range) + continent,
          family = binomial(link = "logit"), data = db_analysis)

performance::check_overdispersion(m2)
performance::check_collinearity(m2)
summary(aov(m2))
summary(m2)

(plot_trend2 <- 
    ggplot2::ggplot(db_analysis, aes(x = year_min, y = scientists/tot, fill = gender, color = gender, size = year_range)) + 
    #facet_wrap( ~ continent, nrow = 3, ncol = 3, scale = "free")+
    geom_point(alpha = 0.6, shape = 21) +
    geom_smooth(se = TRUE, 
                method = "glm", 
                formula = y ~ x,
                method.args = list(family = binomial(link = "logit")), size = 1, alpha = 0.2) +
    scale_color_manual("",values = c("purple","grey20"))+
    scale_fill_manual("",values = c("purple","grey20"))+
    scale_size("Years of\nactivity", breaks = c(1,20,40,60), labels = c(1,20,40,60))+
    labs(x = "Year of first species description", 
         y = "Proportion of etymologies dedicated to scientists")
)

#non-scientists
m3 <- glm(cbind(otherPeople,tot) ~ gender + scale(year_min) + scale(year_range) + continent,
          family = binomial(link = "logit"), data = db_analysis)

performance::check_overdispersion(m3)

m3bis <- glm(cbind(otherPeople,tot) ~ gender + scale(year_min) + scale(year_range) + continent,
          family = quasibinomial(link = "logit"), data = db_analysis)

performance::check_collinearity(m3bis)
summary(aov(m3bis))
summary(m3bis)

(plot_trend3 <- 
    ggplot2::ggplot(db_analysis, aes(x = year_min, y = otherPeople/tot, fill = gender, color = gender,size = year_range)) + 
    #facet_wrap( ~ continent, nrow = 3, ncol = 3, scale = "free")+
    geom_point(alpha = 0.6, fill = "grey20", shape = 21) +
    geom_smooth(se = TRUE, 
                method = "glm", 
                formula = y ~ x,
                method.args = list(family = quasibinomial(link = "logit")), size = 1, alpha = 0.2) +
    scale_color_manual("",values = c("purple","grey20"))+
    scale_fill_manual("",values = c("purple","grey20"))+
    scale_size("Years of\nactivity", breaks = c(1,20,40,60), labels = c(1,20,40,60))+
    labs(x = "Year of first species description", 
         y = "Proportion of etymologies dedicated to non-scientists")
)

############################################################################

# Combining & saving figures

############################################################################

pdf(file = "Figures/Figure_1.pdf", width = 5, height = 3)

plot_1

dev.off()

pdf(file = "Figures/Figure_2.pdf", width = 8, height = 5)

plot_2

dev.off()


pdf(file = "Figures/Figure_3.pdf", width = 16, height = 6)

ggpubr::ggarrange(plot_trend1, plot_trend2, plot_trend3,
                  common.legend = TRUE,
                  hjust = 0,
                  align = "h",
                  labels = c("A", "B", "C"),
                  ncol=3, nrow=1)

dev.off()

# 
# 
# #Ecology
# m2 <- glm(cbind(ecology2,tot) ~ sex + continent + year_range,
#           family = binomial(link = "logit"), data = db_analysis)
# 
# summary(m2)
# 
# performance::check_overdispersion(m2)
# 
# (plot_trend2 <- ggplot2::ggplot(db_analysis, aes(x = year_range, y = ecology2/tot)) + 
#     facet_wrap( ~ continent, nrow = 3, ncol = 3, scale = "free")+
#     geom_point(alpha = 1, shape = 19, size = 2) +
#     geom_smooth(se = TRUE, 
#                 method = "glm", 
#                 formula = y ~ x,
#                 method.args = list(family = binomial(link = "logit"))) +
#     labs(x = "Number of years of activity", 
#          y = "Proportion of ecology etymologies"))
# 
# #Morphology
# m3 <- glm(cbind(morphology,tot) ~ sex + continent + year_range,
#           family = binomial(link = "logit"), data = db_analysis)
# 
# summary(m3)
# 
# performance::check_overdispersion(m3)
# 
# m3bis <- glm(cbind(morphology,tot) ~ sex + continent + year_range,
#           family = quasibinomial(link = "logit"), data = db_analysis)
# 
# summary(m3bis)
# 
# (plot_trend3 <- ggplot2::ggplot(db_analysis, aes(x = year_range, y = morphology/tot)) + 
#     facet_wrap( ~ continent, nrow = 3, ncol = 3, scale = "free")+
#     geom_point(alpha = 1, shape = 19, size = 2) +
#     geom_smooth(se = TRUE, 
#                 method = "glm", 
#                 formula = y ~ x,
#                 method.args = list(family = binomial(link = "logit"))) +
#     labs(x = "Number of years of activity", 
#          y = "Proportion of morphology etymologies"))
# 
# #Culture
# m4 <- glm(cbind(culture,tot) ~ sex + continent + year_range,
#           family = binomial(link = "logit"), data = db_analysis)
# 
# summary(m4)
# 
# performance::check_overdispersion(m4)
# 
# m4bis <- glm(cbind(culture,tot) ~ sex + continent + year_range,
#              family = quasibinomial(link = "logit"), data = db_analysis)
# 
# summary(m4bis)
# 
# (plot_trend4 <- ggplot2::ggplot(db_analysis, aes(x = year_range, y = culture/tot)) + 
#     facet_wrap( ~ continent, nrow = 3, ncol = 3, scale = "free")+
#     geom_point(alpha = 1, shape = 19, size = 2) +
#     geom_smooth(se = TRUE, 
#                 method = "glm", 
#                 formula = y ~ x,
#                 method.args = list(family = binomial(link = "logit"))) +
#     labs(x = "Number of years of activity", 
#          y = "Proportion of morphology etymologies"))
# 
# #Geography
# m5 <- glm(cbind(geography,tot) ~ sex + continent + year_range,
#           family = binomial(link = "logit"), data = db_analysis)
# 
# summary(m5)
# 
# performance::check_overdispersion(m5)
# 
# m5bis <- glm(cbind(geography,tot) ~ sex + continent + year_range,
#              family = quasibinomial(link = "logit"), data = db_analysis)
# 
# summary(m5bis)
# 
# (plot_trend5 <- ggplot2::ggplot(db_analysis, aes(x = year_range, y = geography/tot)) + 
#     facet_wrap( ~ continent, nrow = 3, ncol = 3, scale = "free")+
#     geom_point(alpha = 1, shape = 19, size = 2) +
#     geom_smooth(se = TRUE, 
#                 method = "glm", 
#                 formula = y ~ x,
#                 method.args = list(family = quasibinomial(link = "logit"))) +
#     labs(x = "Number of years of activity", 
#          y = "Proportion of morphology etymologies"))
# 
# 
# ## ------------------------------------------------------------------------
# # What are the most frequent species names?
# Bar_plot <- data.frame(sort(table(db$species))) ; colnames(Bar_plot) <- c("sp","N")
# top30 <- Bar_plot[Bar_plot$N>30,] ; rm(Bar_plot)
# col <- droplevels(top30$sp)
# levels(col) <- c(Names_variables[4],
#                  rep(Names_variables[1],2),
#                  Names_variables[4],
#                  rep(Names_variables[1],4),
#                  rep(Names_variables[4],2), #raveni
#                  rep(Names_variables[1],3),
#                  Names_variables[4], #cambridgei
#                  Names_variables[1],
#                  rep(Names_variables[3],2),
#                  Names_variables[4],#kochi
#                  Names_variables[1],
#                  rep(Names_variables[2],2),
#                  Names_variables[4],#strandi
#                  Names_variables[1],
#                  Names_variables[2],
#                  Names_variables[1],
#                  Names_variables[2],
#                  Names_variables[3],
#                  rep(Names_variables[1],6),
#                  Names_variables[3], #australis
#                  rep(Names_variables[1],3),
#                  rep(Names_variables[4],2))
# 
# top30 <- data.frame(top30,col)
# top30$col <- factor(top30$col, levels = Names_variables[1:4])
# 
# (top_names <- ggplot(top30, aes(x= sp, y=N))+
#     geom_bar(stat="identity", colour = "grey10", size = .1,
#              aes(fill= col))+
#     scale_fill_manual(values = COL[1:4])+
#     labs(title="Most frequent spider names",
#          subtitle="[N > 30 occurrences across the World Spider Catalog]", 
#          x=NULL, 
#          y = "Frequency")+
#     coord_flip()+
#     theme_custom()+
#     theme(
#       legend.position = c(0.8, 0.2),
#       axis.text.y = element_text(size = 9,face = "italic")
#       ))
# 
# # Save
# pdf("Figures/Figure 2.pdf",width = 7, height = 5, paper = 'special')
# top_names
# dev.off()
# 
# # What are the longest and shortest species name?
# 
# # Genus + species
# db[db$Ncar_GenSp == range(Ncar_GenSp)[2],]$GenSp #Longest binomial name: "Chilobrachys jonitriantisvansickleae" (35 char)
# db[db$Ncar_GenSp == range(Ncar_GenSp)[1],]$GenSp #Shortest binomial name: "Gea eff" (6 char)
# 
# # Only species
# db[db$Ncar_Sp == range(Ncar_Sp)[2],]$species #Longest specific epithet: "santaritadopassaquatrensis" (26 char)
# db[db$Ncar_Sp == 2,]$species #Shortest specific epithet: ab an ef fo la kh mi no oz oz wa wu yi zu
# 
# # What is the distribution of etymologies by letters and number of letters?
# 
# # Summarizing character data by year
# db_year_chr <- db %>% 
#   group_by(year) %>% 
#   summarise(Ncar_GenSp_mean = mean(Ncar_GenSp), 
#             Ncar_GenSp_se = std(Ncar_GenSp),
#             Ncar_Sp_mean = mean(Ncar_Sp), 
#             Ncar_Sp_se = std(Ncar_Sp)) 
# 
# (plot_char1 <- ggplot(data.frame(Ncar_GenSp),aes(x=Ncar_GenSp))+
#     geom_bar() +
#     labs(title = "A",
#          x = "N° of characters (Genus name + species epithet)", 
#          y = "Frequency")+
#     theme_custom()) 
# 
# (plot_char2 <- ggplot(data.frame(Ncar_Sp),aes(x=Ncar_Sp))+
#     geom_bar()+
#     labs(title = "B",
#          x = "N° of characters (species epithet)", 
#          y = NULL)+
#     theme_custom())
# 
# (plot_char3 <- ggplot(db_year_chr[db_year_chr$year<2020,], aes(x=year, y=Ncar_Sp_mean)) + 
#     geom_line(linetype = 1, alpha = 1, col = "grey10") + 
#     geom_vline(aes(xintercept = 1900),linetype = 1, color = "gray70", size = 0.2) +
#     scale_x_continuous(breaks = yrs)+ 
#     labs(title = "C",
#          x = NULL, 
#          y = "N° of characters (species epithet)\n[Annual average]")+
#     theme_custom()
# )
# 
# (plot_char4 <- ggplot(data.frame(table(Letter)),aes(x= Letter, y=Freq))+
#     geom_bar(stat="identity", colour = "grey30", fill = "grey30")+
#     labs(title="D", x = "Initial letter (species epithet)", y = "Frequency")+
#     theme_custom())
# 
# pdf("Figures/Figure Box1.pdf",width = 9, height = 8, paper = 'special')
# lay_char <- rbind(c(1,2),c(3,3),c(4,4))
# gridExtra::grid.arrange(plot_char1,plot_char2,plot_char3,plot_char4, layout_matrix = lay_char)
# dev.off()
# 
# # How many etymologies are Arbitrary combinaton of letters?
# table(startsWith(as.character(db$Notes), "Arbitrary combination of letters")) #465
# 
# ###########################################################################
# # Temporal patterns -------------------------------------------------------
# ###########################################################################
# 
# # reorganize the dataset
# db2 <- db[db$N_meanings > 0,] #remove no meanings
# 
# db2 <- db2 %>% dplyr::select(year,
#                       size,
#                       shape,
#                       colour,
#                       behaviour,
#                       ecology,
#                       geography,
#                       scientists,
#                       otherPeople,
#                       modernCulture,
#                       pastCulture,
#                       others) %>% data.frame
# 
# db2 <- data.frame(year = db2$year,
#                   morpho = rowSums(db2[,2:4]),
#                   ecol = rowSums(db2[,5:6]), 
#                   geo = db2[,7],
#                   people = rowSums(db2[,8:9]),
#                   culture = rowSums(db2[,10:11]),
#                   other = db2[,12])
# 
# db2[,2:7] <- apply(db2[,2:7], 2, function (x) ifelse(x > 1, 1 , x)) %>% data.frame
# db2[is.na(db2)] <- 0
# 
# # Database absolute values
# db_year <- apply(db2[,2:7], 2, function (x) tapply(x, as.factor(db2$year), sum)) %>% data.frame
# 
# db_year_plot <- data.frame(Year  = as.numeric(rep(rownames(db_year), 6 )),
#                        Value = c(db_year$morpho,
#                                  db_year$ecol,
#                                  db_year$geo,
#                                  db_year$people,
#                                  db_year$culture,
#                                  db_year$other),
#                        Type = c(rep(Names_variables[1],nrow(db_year)),
#                                 rep(Names_variables[2],nrow(db_year)),
#                                 rep(Names_variables[3],nrow(db_year)),
#                                 rep(Names_variables[4],nrow(db_year)),
#                                 rep(Names_variables[5],nrow(db_year)),
#                                 rep(Names_variables[6],nrow(db_year))),
#                        Tot = rep(rowSums(db_year),6)
#                        )
# 
# db_year_plot$Type <- as.factor(db_year_plot$Type)
# db_year_plot$Type <- factor(db_year_plot$Type, levels = Names_variables)
# 
# # Database temporal trends
# db_year       <- db_year %>% rownames_to_column("year")
# db_year$year  <- as.numeric(db_year$year)
# db_model      <- data.frame(db_year,  tot = rowSums(db_year[,2:7])) ;
# 
# #Most frequent etymologies in the last 10 years
# db_model_2010 <- db_model[db_model$year > 2009,]
# 
# apply(db_model_2010[,-c(1,8)], 2, sum)/sum(db_model_2010[,8])
# 
# # Modelling ---------------------------------------------------------------
# db_year_plot <- db_year_plot[db_year_plot$Year<2020,] #remove 2020
# 
# r1 <- mgcv::gam(cbind(Value,Tot) ~ s(Year) + Type + s(Year, by = Type),
#                 family=binomial(link = "logit"), data = db_year_plot)
# 
# performance::check_overdispersion(r1) # overdispersed
# 
# r2 <- mgcv::gam(cbind(Value,Tot) ~ s(Year) + Type + s(Year, by = Type),
#                 family = quasibinomial(link = "logit"), data = db_year_plot)
# 
# performance::r2(r2)
# summary(r2)
# pairs(emmeans::emmeans(r2, ~ Type * s(Year)), simple="Type")
# 
# # Plot
# (plot_gam <- ggplot(data = tidymv::predict_gam(r2), aes(Year, fit)) +
#     geom_line(aes(y = fit, x = Year, colour = Type), linetype="solid",size=1.1,alpha=1) +
#     geom_ribbon(aes(ymin = fit - (se.fit * ci_z), ymax = fit + (se.fit * ci_z), group = Type, fill = Type),
#                 alpha = 0.2)+
#     scale_x_continuous(breaks = yrs)+
#     labs(x = NULL, 
#          y = "Model fit",
#          title = "C")+
#     scale_color_manual(values = COL) +
#     scale_fill_manual(values = COL) + 
#     theme_custom() + theme(legend.position = "none"))
# 
# (plot_trend2 <- ggplot2::ggplot(db_year_plot, aes(x=Year, y = Value/Tot)) + 
#   geom_point(aes(colour=Type, fill = Type), alpha =0.6, shape = 21) +
#   geom_smooth(aes(colour=Type, fill = Type), se = TRUE, 
#               method = "gam", 
#               formula = y ~ s(x),
#               method.args = list(family = quasibinomial(link = "logit"))) +
#   scale_x_continuous(breaks = yrs)+ 
#   scale_color_manual(values = COL) +
#   scale_fill_manual(values = COL)  + 
#     labs(x = NULL, 
#          y = "Proportion",
#          title = "D")+  
#     theme_custom() + 
#     theme(legend.position = "none")
#   )
# 
# (plot_trend1 <- ggplot(db_year_plot) +
#     geom_line(aes(x = Year, y = Value, color=Type),size=.5,linetype = 1) + 
#     scale_color_manual(values = COL)+
#     scale_x_continuous(breaks = yrs)+ 
#     labs(x = NULL, 
#          y = "Frequency",
#          title = "B")+
#     theme_custom())
# 
# # Save
# pdf("Figures/Figure 1.pdf",width = 14, height = 8, paper = 'special')
# gridExtra::grid.arrange(plot_type, plot_trend1,
#                         plot_gam, plot_trend2, nrow = 2, ncol = 2)
# dev.off()
# 
# ## ------------------------------------------------------------------------
# 
# ###########################################################################
# # Spatial patterns -------------------------------------------------------
# ###########################################################################
# 
# #Re-arrange the data
# db3 <- db[db$N_meanings>0,] %>% dplyr::select(year,
#                                               Asia,
#                                               Europe,
#                                               Africa,
#                                               N_America,
#                                               S_America,
#                                               Oceania,
#                                               size,
#                                               shape,
#                                               colour,
#                                               behaviour,
#                                               ecology,
#                                               geography,
#                                               scientists,
#                                               otherPeople,
#                                               modernCulture,
#                                               pastCulture,
#                                               others) %>% data.frame
# 
# db3 <- data.frame(db3[,c(1:7)],
#                   morpho = rowSums(db3[,c(8:10)]),
#                   ecol = rowSums(db3[,c(11:12)]), 
#                   geo = db3[,13],
#                   people = rowSums(db3[,c(14:15)]),
#                   culture = rowSums(db3[,c(16:17)]),
#                   other = db3[,18])
# 
# db3[,8:ncol(db3)] <- apply(db3[,8:ncol(db3)], 2, function (x) ifelse(x > 1, 1 , x)) %>% data.frame
# db3[is.na(db3)] <- 0
# 
# # How many species occur in multiple continents? 
# table(rowSums(db3[,c(2:7)]))
# 
# #reorganize the dataset
# db3 <- data.frame(db3, SUM_Continent = rowSums(db3[,c(2:7)]))
# 
# db3_single <- db3[db3$SUM_Continent == 1,]
# db3_single <- eatATA::dummiesToFactor(dat = db3_single, dummies = colnames(db3_single[,c(2:7)]), 
#                         facVar = "Continent")
# 
# db3_single$Continent <- factor(db3_single$Continent, levels = c("Europe", "Africa", "Asia", "N_America", "S_America", "Oceania"))
# db3_single <- within(db3_single, Continent <- relevel(Continent, ref = "Europe"))
# 
# names_var <-  c(paste0(Names_variables[1]," [n= ", sum(db3_single$morpho),"]"),
#                 paste0(Names_variables[2]," [n= ", sum(db3_single$ecol),"]"),
#                 paste0(Names_variables[3]," [n= ", sum(db3_single$geo),"]"),
#                 paste0(Names_variables[4]," [n= ", sum(db3_single$people),"]"),
#                 paste0(Names_variables[5]," [n= ", sum(db3_single$culture),"]"),
#                 paste0(Names_variables[6]," [n= ", sum(db3_single$other),"]"))
# 
# # Models:
# model <- list()
# for(i in 8:13) { 
#   message(paste0("-------- Model for ", paste0(colnames(db3_single)[i]), " --------"))
#   formula_i <- as.formula(paste0(colnames(db3_single)[i]," ~ ", colnames(db3_single)[15]))
#   m_i <- glm(formula_i, data = db3_single, family = binomial(link= "cloglog"))
#   model[[i-7]] <- m_i
# }
# 
# #Contrasts:
# contrast <- list()
# for(i in 1:length(model)) { 
#   message(paste0("-------- Model for ", Names_variables[i], " --------"))
#   (contrast[[i]] <- pairs(emmeans::emmeans(model[[i]], "Continent")))
#   print(contrast[[i]])
# }
# 
# # Extract estimates
# for(i in 1:length(model)) { 
#    
#   Estimates_i <- 
#     model[[i]] %>% 
#     summary %>% 
#     magrittr::extract2("coefficients") %>% # extract estimates
#     as.data.frame %>% rownames_to_column("Variable") %>% 
#     dplyr::filter(!row_number() %in% 1) %>%  #remove intercept
#     dplyr::rename(SE = 3, z = 4, p = 5) #rename
#   
#   Estimates_i$Variable <- c("Africa","Asia","North America","Oceania", "South America")
#   Estimates_i <- data.frame(Estimates_i, Type = rep(names_var[i],nrow(Estimates_i)))
#   
#   #Store model output
#   if(i > 1)
#     Estimates <- rbind(Estimates, Estimates_i)
#   else
#     Estimates <- Estimates_i
# }
# 
# # Plot
# col_p <- ifelse(Estimates$p > 0.001, "grey5", ifelse(Estimates$Estimate>0,"cyan4","brown4") )
# 
# Estimates$Type <- factor(Estimates$Type, levels = names_var)
# 
# (plot_regional <- ggplot2::ggplot(data = Estimates, aes(Variable, Estimate)) +
#   facet_wrap( ~ Type, nrow = 2, ncol = 3) +
#   geom_hline(lty = 3, size = 0.7, col = "grey50", yintercept = 0) +
#   geom_errorbar(aes(ymin = Estimate-SE, ymax = Estimate+SE), width = 0, col = col_p) +
#   geom_text(aes(Variable, Estimate), 
#             label = round(Estimates$Estimate,2),
#             vjust = -1, 
#             color = col_p, size = 2) +
#   geom_point(size = 2, pch = 21, col = col_p, fill = col_p) +
#   labs(y = expression(paste("Estimated beta" %+-% "Standard error")),
#        x = NULL)+
#   theme_custom() + coord_flip())
# 
# # Add a Map 
# 
# # Loading data
# world <- map_data("world")
# 
# # Frequency by Continent
# for (i in 1:nlevels(db3_single$Continent)){
#   
#   db_i <- db3[db3[,i+1] == 1, ]
#   db_i <- apply(db_i[,8:13], 2, sum)
#   
#   if(i>1)
#     pie <- rbind(pie,db_i)
#   else
#     pie <- db_i
# } 
# 
# pie <- data.frame(continent = colnames(db3)[2:7],
#                   x = c(103.82,10.38,15.21,-102.52,-58.23,131.42),
#                   y = c(36.56,51.10,-0.83,50.94,-13.38,-24.20),
#                   n = rowSums(pie),
#                   radius = log(rowSums(pie))*3,
#                   pie)
# 
# #Plot
# map <- ggplot() +
#   geom_map(map = world, data = world,
#            aes(long, lat, map_id = region), 
#            color = "gray45", fill = "gray45", size = 0.3) +
#   labs(title = NULL) + theme_map()
# 
# (map2 <- map + scatterpie::geom_scatterpie(data = pie, aes(x=x, y=y, group=continent, r=20),
#                                            cols = colnames(pie)[6:11], color="grey10", alpha=.9) +
#     scale_fill_manual("",labels = Names_variables ,values = COL) + 
#     theme(
#       legend.position = c(0.3, 0.1),
#       legend.background = element_rect(linetype = 1, size = .1, color = "grey5", 
#                                        fill=alpha('white', 0.8)),
#       legend.title = element_blank(),
#       legend.direction="horizontal",
#       legend.text = element_text(size=9)))
# 
# # Save
# pdf("Figures/Figure 4.pdf",width = 8, height = 8, paper = 'special')
# gridExtra::grid.arrange(map2,plot_regional, nrow = 2, ncol = 1)
# dev.off()
# 
# ## ------------------------------------------------------------------------
# 
# ###########################################################################
# # Temporal patterns by region ----------------------------------------------
# ###########################################################################
# 
# #Re-arrange the data
# for(i in 1:nlevels(db3_single$Continent)) { 
#   
#   db_i <- db3[db3[,i+1] == 1,] #select continent
#   db_year_i <- apply(db_i[,c(8:13)], 2, function (x) tapply(x, as.factor(db_i$year), sum)) %>% data.frame
#   
#   db_year_i <- data.frame(db_year_i)
#   
#   db_year_i_plot <- data.frame(Year  = as.numeric(rep(rownames(db_year_i), 6 )),
#                              Value = c(db_year_i$morpho,
#                                        db_year_i$ecol,
#                                        db_year_i$geo,
#                                        db_year_i$people,
#                                        db_year_i$culture,
#                                        db_year_i$other),
#                              Continent = rep(colnames(db3)[i+1] ,  nrow(db_year_i)*6  ),
#                              Type = c(rep(Names_variables[1],nrow(db_year_i)),
#                                       rep(Names_variables[2],nrow(db_year_i)),
#                                       rep(Names_variables[3],nrow(db_year_i)),
#                                       rep(Names_variables[4],nrow(db_year_i)),
#                                       rep(Names_variables[5],nrow(db_year_i)),
#                                       rep(Names_variables[6],nrow(db_year_i))),
#                              Tot = rep(rowSums(db_year_i),6))
#   
#   if(i>1)
#     db_yr_reg <- rbind(db_yr_reg,db_year_i_plot)
#   else
#     db_yr_reg <- db_year_i_plot
#   }
# 
# db_yr_reg$Type <- as.factor(db_yr_reg$Type)
# db_yr_reg$Type <- factor(db_yr_reg$Type, levels = Names_variables)
# 
# db_yr_reg$Continent <- as.factor(db_yr_reg$Continent)
# db_yr_reg <- within(db_yr_reg, Continent <- relevel(Continent, ref = "Europe"))
# 
# # Modelling ---------------------------------------------------------------
# db_yr_reg <- db_yr_reg[db_yr_reg$Year<2020,]
# 
# t1 <- mgcv::gam(cbind(Value,Tot) ~  Continent * Type + s(Year, by = interaction(Continent, Type)),
#                 family = binomial(link = "logit"), data = db_yr_reg)
# 
# performance::check_overdispersion(t1) #minimal overdispersion
# # dispersion ratio = 1.265
# # Pearson's Chi-Squared = 9019.402
# # p-value =  < 0.001
# 
# performance::r2(t1) #0.837
# 
# levels(db_yr_reg$Continent)[c(4,6)] <- c("North America", "South America")
# 
# (plot_reg <- ggplot2::ggplot(db_yr_reg, aes(x = Year, y = Value/Tot)) + 
#     facet_wrap( ~ Continent, nrow = 2, ncol = 3) +
#     geom_smooth(aes(colour=Type, fill=Type), se = TRUE,
#                 method = "gam",
#                 formula = y ~ s(x, bs = "cs"),
#                 method.args = list(family = quasibinomial(link = "logit"))) +
#     scale_x_continuous(breaks = yrs)+ 
#     labs(x = NULL, 
#          y = "Relative proportion of etymologies",
#          title = NULL)+
#     scale_color_manual(values = COL) +
#     scale_fill_manual(values = COL) + theme_custom() + theme(legend.position = "top"))
# 
# # Save
# pdf("Figures/Figure 3.pdf",width = 16, height = 8, paper = 'special')
# grid::grid.draw(shift_legend(plot_reg))
# dev.off()

# Save supplementary tables -----------------------------------------------
set_flextable_defaults(table.layout = "autofit")

flextable::save_as_docx('Table S1' = flextable::as_flextable(r2), 
                        'Table S2' = flextable::as_flextable(t1),
                        'Table S3' = flextable::as_flextable(model[[1]]),
                        'Table S4' = flextable::as_flextable(model[[2]]),
                        'Table S5' = flextable::as_flextable(model[[3]]),
                        'Table S6' = flextable::as_flextable(model[[4]]),
                        'Table S7' = flextable::as_flextable(model[[5]]),
                        'Table S8' = flextable::as_flextable(model[[6]]),
                        path = "Tables/Supplementary_tables_S1_S8.docx")

# End of analyses