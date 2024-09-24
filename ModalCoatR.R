#Find modal coat colour for each camera R 
#Steps to do this: 

#1. Generate dataframe with one row per camera trap
#2. Find number of individuals per camera trap 
#3. Exclude unknown images from dataframe 
#4. Find number of images per camera trap 
#5. Find number of identifiable images per camera trap 
#6. Divide identifiable images by the number of individuals = IPC 
#7. Get number of unmarked cat images per cam
#8. Divide the number of unmarked cats by IPC 
#9. Need to then sort out the number of individuals per coat type 

cats.df <- read.csv("survey.df.cats.9.2024.csv")
cats.df

#1. Get dataframe with one row per camera + enviro 
CoatCam <- read.csv("survey.covariates.csv")

#2. Get number of individuals per camera trap 
#3. Exclude unknown images from dataframe 
cats.df.ID <- cats.df[!(cats.df$Individual.name == "" | cats.df$Individual.name == "Unknown"),]
cats.df.ID.noUnmarked <- cats.df.ID[!(cats.df.ID$Individual.name == "Unmarked"),]
cats.df.ID.noUnmarked <- cats.df.ID.noUnmarked %>%
  filter(!is.na(Individual.name))
# Create a new dataframe with count of unique cats per camera
distinct_cats <- cats.df.ID.noUnmarked %>%
  distinct(cam, Individual.name)
new_df <- as.data.frame(table(distinct_cats$cam))
names(new_df) <- c("cam", "count_unique_cats")
CoatCam <- merge(CoatCam, new_df, by = "cam", all.x = TRUE)

#4. Find number of images per camera trap 
n.ID.img <- as.data.frame(plyr::count(cats.df.ID.noUnmarked$cam))
colnames(n.ID.img) <- c("cam", "IDImg")
CoatCam <- merge(CoatCam, n.ID.img, by = "cam", all.x = TRUE)

#Pause to plot number of images and individual ID, show correlation 
# calculate the linear model
lm_model <- lm(IDImg ~ count_unique_cats, data = CoatCam)

# Extract R-squared value
r_squared <- summary(lm_model)$r.squared

# Create scatter plot with ggplot2
scatter_plot <- ggplot(CoatCam, aes(x = count_unique_cats, y = IDImg)) +
  geom_point(color = "black", size = 2) +          # Scatter points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add regression line
  labs(x = "Number of Individual Cats",
       y = "Number of Images") +
  annotate("text", x = Inf, y = Inf, label = paste("R² = ", round(r_squared, 2)),
           hjust = 1.1, vjust = 2, size = 5, color = "black") +  # Add R-squared value
  theme_classic()

#6. Divide identifiable images by the number of individuals = IPC 
CoatCam$IPC <- CoatCam$IDImg/CoatCam$count_unique_cats
hist(CoatCam$IPC)

#7. Get number of unmarked cat images per cam
cats.df.ID.un <- cats.df.ID[cats.df.ID$Individual.name=="Unmarked",]
un.cat <- as.data.frame(plyr::count(cats.df.ID.un$cam))
colnames(un.cat) <- c("cam", "unImg")
CoatCam <- merge(CoatCam, un.cat, by = "cam", all.x = TRUE)

#8. Divide the number of unmarked cats by IPC 
CoatCam$Un.Est <- CoatCam$unImg / CoatCam$IPC
hist(CoatCam$Un.Est)
CoatCam$Un.Est <- ceiling(CoatCam$Un.Est)

#9. Need to then sort out the number of individuals per coat type 
 
#Need to find the number of individuals per coat type per camera
#Potentially use unique function for individual and camera, then count for camera
  #should be extra rows if extra individuals. 
  #Do for each coat type and merge into dataframe, keepig all rows. 
#format coat types 
BB <- survey.df.cats[(survey.df.cats$Coat.colour=="Brown/Black") & (survey.df.cats$Coat.pattern=="Blotched"),] #include only classified cats 
BB$Type <- "BB"

BBunique <- BB %>%
  distinct(cam, Individual.name, .keep_all = TRUE)

#Provides number of individuals per cam for BB coats 
BBCounts <- as.data.frame(plyr::count(BBunique$cam))
colnames(BBCounts) <- c("cam", "BB")

#Blotched orange 
BO <- survey.df.cats[(survey.df.cats$Coat.colour=="Orange") & (survey.df.cats$Coat.pattern=="Blotched"),]
BO$Type <- "BO"

BOunique <- BO %>%
  distinct(cam, Individual.name, .keep_all = TRUE)

#Provides number of individuals per cam for BB coats 
BOCounts <- as.data.frame(plyr::count(BOunique$cam))
colnames(BOCounts) <- c("cam", "BO")

#Tabby Brown 
TB <- survey.df.cats[(survey.df.cats$Coat.colour=="Brown/Black") & ((survey.df.cats$Coat.pattern=="Broken") | (survey.df.cats$Coat.pattern=="Spotted" | (survey.df.cats$Coat.pattern=="Striped"))),]
TB$Type <- "TB"

TBunique <- TB %>%
  distinct(cam, Individual.name, .keep_all = TRUE)

#Provides number of individuals per cam for BB coats 
TBCounts <- as.data.frame(plyr::count(TBunique$cam))
colnames(TBCounts) <- c("cam", "TB")

#Tabby orange 
TO <- survey.df.cats[(survey.df.cats$Coat.colour=="Orange") & ((survey.df.cats$Coat.pattern=="Broken") | (survey.df.cats$Coat.pattern=="Spotted" | (survey.df.cats$Coat.pattern=="Striped"))),]
TO$Type <- "TO"

TOunique <- TO %>%
  distinct(cam, Individual.name, .keep_all = TRUE)

#Provides number of individuals per cam for BB coats 
TOCounts <- as.data.frame(plyr::count(TOunique$cam))
colnames(TOCounts) <- c("cam", "TO")

#Tortoiseshell 
TS <- survey.df.cats[(survey.df.cats$Coat.colour=="Tortoiseshell" | survey.df.cats$Coat.pattern=="Tortoiseshell"),]
TS$Type <- "TS"

TSunique <- TS %>%
  distinct(cam, Individual.name, .keep_all = TRUE)

#Provides number of individuals per cam for BB coats 
TSCounts <- as.data.frame(plyr::count(TSunique$cam))
colnames(TSCounts) <- c("cam", "TS")


#Orange grouped 
O <- survey.df.cats[(survey.df.cats$Coat.colour=="Orange"),]
O$Type <- "O"

Ounique <- O %>%
  distinct(cam, Individual.name, .keep_all = TRUE)

#Provides number of individuals per cam for BB coats 
OCounts <- as.data.frame(plyr::count(Ounique$cam))
colnames(OCounts) <- c("cam", "O")


#Merge data frames 
CoatCam1 <- merge(CoatCam, BBCounts, by = "cam", all.x = TRUE)
CoatCam2 <- merge(CoatCam1, BOCounts, by = "cam", all.x = TRUE)
CoatCam3 <- merge(CoatCam2, TBCounts, by = "cam", all.x = TRUE)
CoatCam4 <- merge(CoatCam3, TOCounts, by = "cam", all.x = TRUE)
CoatCam5 <- merge(CoatCam4, TSCounts, by = "cam", all.x = TRUE)
CoatCam6 <- merge(CoatCam5, OCounts, by = "cam", all.x = TRUE)

#Select mode
CoatCam6$B <- round(CoatCam6$Un.Est)
CoatCam6$B <- as.integer(CoatCam6$B )
str(CoatCam6)

#Find the mode 
CoatCam7 <- CoatCam6[,c("cam", "BB", "O", "B", "TS", "TB")]

CoatCam7$Coat <- apply(CoatCam7[, c("BB", "TB", "O", "B", "TS")], 1, function(x) {
  x_non_na <- x[!is.na(x)]  # Remove NA values
  
  if (length(x_non_na) == 0) {
    return(NA)  # All values are NA
  } else if (length(unique(x_non_na)) == 1) {
    return("TIE")  # All non-NA values are the same
  } else {
    return(names(CoatCam7[, c("BB", "TB", "O", "B", "TS")])[which.max(x)])  # Return column name with max value
  }
})


plyr::count(CoatCam7$Coat) #234 cases with "TIE". In these, take the "dominant" individual, individual with most captures. 

#Need to obtain number of events per coat type. Use the same dataframes but don't use distinct indiivduals, just cameras 
TSNum <- as.data.frame(plyr::count(TS$cam))
colnames(TSNum) <- c("cam", "TS.N")

ONum <- as.data.frame(plyr::count(O$cam))
colnames(ONum) <- c("cam", "O.N")

TBNum <- as.data.frame(plyr::count(TB$cam))
colnames(TBNum) <- c("cam", "TB.N")

BBNum <- as.data.frame(plyr::count(BB$cam))
colnames(BBNum) <- c("cam", "BB.N")

B <- survey.df.cats[(survey.df.cats$Coat.colour=="Black") & (survey.df.cats$Coat.pattern=="Solid"),]
B$Type <- "B"

BNum <- as.data.frame(plyr::count(B$cam))
colnames(BNum) <- c("cam", "B.N")

#Merge in counts as well
CoatCam8 <- merge(CoatCam7, TSNum, by = "cam", all.x = TRUE)
CoatCam9 <- merge(CoatCam8, ONum, by = "cam", all.x = TRUE)
CoatCam10 <- merge(CoatCam9, BBNum, by = "cam", all.x = TRUE)
CoatCam11 <- merge(CoatCam10, BNum, by = "cam", all.x = TRUE)
CoatCam12 <- merge(CoatCam11, TBNum, by = "cam", all.x = TRUE)

# Replace "TIE" with the N column with the highest value
CoatCam12$Coat <- apply(CoatCam12, 1, function(row) {
  if (!is.na(row["Coat"]) && row["Coat"] == "TIE") {
    # Check the corresponding N columns
    n_values <- row[c("TS.N", "O.N", "BB.N", "B.N", "TB.N")]
    n_values_non_na <- n_values[!is.na(n_values)]  # Remove NA values from N columns
    
    if (length(n_values_non_na) == 0) {
      return(NA)  # If all N columns are NA, keep as NA
    } else {
      return(names(n_values)[which.max(n_values)])  # Return the N column name with max value
    }
  } else {
    return(row["Coat"])  # If not a TIE, keep the original Coat value
  }
})
CoatCam12$Coat <- gsub("\\.N$", "", CoatCam12$Coat)


#Fix missed black cats as a result of cases where no other coat types and only black cats present 

# Modify Coat column: If Coat is NA and B.N > 0, set Coat to "B"
CoatCam13 <- CoatCam12 %>%
  mutate(Coat = ifelse(is.na(Coat) & !is.na(B.N) & B.N > 0, "B", Coat))


#Check on white cats 
White <- survey.df.cats[(survey.df.cats$Coat.pattern=="Tuxedo"),] #include only classified cats 
plyr::count(White$Individual.name) #11 individuals 
plyr::count(White$cam) #13 cameras 


#Merge coats back into original dataframe 
CoatCam14 <- CoatCam13[,c("cam", "Coat")]
CoatCamExport <- merge(CoatCam, CoatCam14, by = "cam")

plyr::count(CoatCamExport$Coat)

write.csv(CoatCamExport, "CoatCamExportModal.csv")
