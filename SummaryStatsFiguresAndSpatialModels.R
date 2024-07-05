
#Coat summary statistics 12.10.23
#Using export from original coat code and the cameras from formatted multispecies DF

#read in main csv
CoatDF <- read.csv("CoatExport12.10.csv")

#Read in camera dataframe 
multidf <- read.csv("CoatDFTallies.csv")
length(unique(multidf$cam))

CoatDF83 <- CoatDF[CoatDF$cam %in% multidf$cam, ]
CoatDF83$Date <- substr(CoatDF83$date.time, 1, 10)
CoatDF83$Date <- as.Date(CoatDF83$Date, format = "%d/%m/%Y")
#Checking regions for operating days 
library(dplyr)
max_dates <- CoatDF83 %>%
  group_by(region) %>%
  summarise(Max_Date = max(Date, na.rm = TRUE))

#Operating days for this study
op.df <- read.csv("opt_time_2021-7-30.csv")
op83 <- op.df[op.df$camera %in% multidf$cam, ]
#write.csv(op83, "op83.csv") #done manually 282363 days

#CoatDF83 contains only the cameras that lasted long enough to count.
length(CoatDF83)
#24076 images
table1 <- table(CoatDF83$Coat.colour, CoatDF83$Coat.pattern)
table1
prop.table(table1)

#Look at individual counts for each
library(dplyr)
IDDF<- CoatDF83 %>% distinct(Individual.name, .keep_all = TRUE)

names_to_remove <- c("Unknown", "unknown", "Unmarked", "Walking", "Unknown ", "mESHI", "", " Carmen")

# Create a logical vector that identifies rows with names to remove
rows_to_remove <- IDDF$Individual.name %in% names_to_remove

# Use logical indexing to remove the rows
Filtered_df <- IDDF[!rows_to_remove, ]
table(Filtered_df$Coat.colour, Filtered_df$Coat.pattern)
#Gives counts of individuals, use in summary table? 


#Get counts of camera traps 
CoatDF83

coat.DF<- CoatDF83 %>% distinct(Coat.colour, Coat.pattern, cam, .keep_all = TRUE)
table(coat.DF$Coat.pattern, coat.DF$Coat.colour)
#for the combos
coat.DF1 <- coat.DF[!coat.DF$Coat.colour=="Black",]
coat.DF2 <- coat.DF1[!coat.DF1$Coat.colour=="Tortoiseshell",]
coat.DF2 <- coat.DF2[!coat.DF2$Coat.colour=="Unknown",]
coat.DF2 <- coat.DF2[!coat.DF2$Coat.pattern=="Unknown",]
coat.DF2 <- coat.DF2[!coat.DF2$Coat.pattern=="Blotched",]
coat.DF2 <- coat.DF2[!coat.DF2$Coat.pattern=="Solid",]

table(coat.DF2$Coat.pattern, coat.DF2$Coat.colour)

coat.DFBrown <- coat.DF2[!coat.DF2$Coat.colour=="Orange",]
length(unique(coat.DFBrown$cam))

coat.DFOrange <- coat.DF2[coat.DF2$Coat.colour=="Orange",]
length(unique(coat.DFOrange$cam))




#-------------------White cats---------------------#


#distance to nearest town 
dist.df <- read.csv("cam_dtnt.csv")
CoatDF83.dist <- merge(CoatDF83, dist.df, by = "cam")
mean(CoatDF83.dist$dtnt)

CoatDF83White <- CoatDF83.dist[CoatDF83.dist$White.present == TRUE,]
table(CoatDF83White$Coat.colour, CoatDF83White$Coat.pattern)


CoatDF83White <- CoatDF83White %>% distinct(cam, .keep_all = TRUE)
mean(CoatDF83White$dtnt)
std.error(CoatDF83White$dtnt)
max(CoatDF83White$dtnt)

CoatType #created below if not working 
CoatType.dist <- merge(CoatType, dist.df, by = "cam")


#Get unique rows for camera and type 
CoatType.dist <- CoatType.dist %>% distinct(cam, Type, .keep_all = TRUE)

library(plotrix)
multidfUpdate

#-------running code below with updated multidfUpdate

coat_mean_dist<- aggregate(x= CoatType.dist$dtnt,
                       # Specify group indicator
                       by = list(CoatType.dist$Type),      
                       # Specify function (i.e. mean)
                       FUN = mean)
colnames(coat_mean_dist) <- c("Coat", "Mean")

coat_mean_dist.1 <- aggregate(x= CoatType.dist$dtnt,
                           # Specify group indicator
                           by = list(CoatType.dist$Type),      
                           # Specify function (i.e. mean)
                           FUN = std.error)

coat_mean_dist.1$x

coat_mean_dist <- cbind(coat_mean_dist, coat_mean_dist.1$x)
colnames(coat_mean_dist) <- c("Coat", "Mean", "SE")

white <- as.data.frame(cbind("WC", mean(CoatDF83White$dtnt),
                           std.error(CoatDF83White$dtnt)))
colnames(white) <- c("Coat", "Mean", "SE")

dist.df.coats <- rbind(coat_mean_dist, white)
str(dist.df.coats)
dist.df.coats$Coat <- as.factor(dist.df.coats$Coat)
dist.df.coats$Mean <- as.numeric(dist.df.coats$Mean)
dist.df.coats$SE <- as.numeric(dist.df.coats$SE)


dist.df.coats <- dist.df.coats[order(dist.df.coats$Mean), ]

library(ggplot2)
#plot to visulise
ggplot(dist.df.coats, aes(x = reorder(Coat, Mean), y = Mean)) +
  geom_point(color = "red") +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2) +
  labs(x = "Coat type", y = "Mean distance to nearest town") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#----------------------------MAP--------------------------------------------#

#Map of camera sites
#Stamen no longer works. 
#Need an API for google now 
#AIzaSyBLGXog4tqnoz4uKMWnUaJc-MY0GXv1Zc4
ggmap::register_google(key = "AIzaSyCXQUpYUmN8YJ3Tw9DLUi9dqTK8XABDAVQ")

#When using satelitte data, can't use bounding boxes.
  #But you can trim off excess 


library(ggmap)
test <- (get_googlemap(center = c(lon = 146.603081, lat = -42.092677),
                    zoom = 7,
                    maptype ='satellite',
                    color = 'color'))
ggmap(test)

# Create a ggmap object using ggmap
ggmap_obj <- ggmap(test)

ggmap_obj + geom_point(aes(x = lon, y = lat), data = CoatDF83, size = 0.5, colour = "white") +  
scale_y_continuous(limits = c(-43.7, -40.5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(144.5, 148.5), expand = c(0, 0))

#---------------------------- Maps of coat colours -------------------------#

##Trying to overlay with leaflet 
library(leaflet)
plyr::count(CoatDF83$Coat.colour)
#Oh wait we can customise the circles. Works!
pal <- colorFactor(c("black", "brown", "orange", "pink", "grey", "white" ), domain = c("Black", "Brown/Black", "Orange", "Tortoiseshell", "Unknown", "White"))
leaflet(CoatDF83) %>% addTiles() %>%
  addCircleMarkers(
    color = ~pal(Coat.colour),
    stroke = FALSE, fillOpacity = 0.5
  )


#With minicharts 


cat.chart <- subset(CoatDF83, select=c("cam", "lon","lat","count", "Coat.colour"))
plyr::count(cat.chart, "Coat.colour")

#Now we need columns for each colour type, with counts for each unqiue camera with location. 
cat.table <- table(cat.chart$cam, cat.chart$Coat.colour)
cat.chart1 <- data.frame(cat.chart$cam, cat.chart$lon, cat.chart$lat)
cam.loc <- unique(cat.chart1) # Delete rows
cam.loc  #DO NOT DELETE 
#length of columns matching. Now extract lon and lat and cbind 
cat.table1<- cbind(cat.table, cam.loc$cat.chart.lon)
cat.table2 <- cbind(cat.table1, cam.loc$cat.chart.lat)
cat.table2
cat.chart.done <- as.data.frame.matrix(cat.table2)
cat.chart.done$total <- rowSums(cat.chart.done[,1:6] )
colnames(cat.chart.done ) <- c("Black", "Brown/Black", "Orange", "Tortoiseshell", "Unknown", "White", "lon", "lat", "total")
##Cross checked on data frame and this has worked; lat and lons line up! 

#Re-do with coat types NOT COLOURS 

JitterCoat83 <- cat.chart.done
JitterCoat83$lat <- jitter(JitterCoat83$lat, factor = 0.01)
JitterCoat83$lon <- jitter(JitterCoat83$lon, factor = 0.01)

colors <- c("Black", "Brown", "Orange", "Pink", "Grey", "White")
library(leaflet.minicharts)
leaflet(CoatDF83)%>% addTiles() %>% #basemap, dataframe isn't important here I don't think 
  addMinicharts(
    JitterCoat83$lon, JitterCoat83$lat,
    type = "pie",
    chartdata = JitterCoat83[, c("Black", "Brown/Black", "Orange", "Tortoiseshell", "Unknown", "White")],
    width = 10,
    colorPalette = colors)


#--------------------------- MAP OF COAT TYPES ----------------------------#

#-+++___+++___+++___ FORMAT DATAFRAME ____+++___++__+++_+_+_

BB <- CoatDF83[(CoatDF83$Coat.colour=="Brown/Black") & (CoatDF83$Coat.pattern=="Blotched"),] #include only classified cats 
BB$Type <- "BB"
#Blotched orange 
BO <- CoatDF83[(CoatDF83$Coat.colour=="Orange") & (CoatDF83$Coat.pattern=="Blotched"),]
BO$Type <- "BO"
#Tabby Brown 
TB <- CoatDF83[(CoatDF83$Coat.colour=="Brown/Black") & ((CoatDF83$Coat.pattern=="Broken") | (CoatDF83$Coat.pattern=="Spotted" | (CoatDF83$Coat.pattern=="Striped"))),]
TB$Type <- "TB"
#Tabby orange 
TO <- CoatDF83[(CoatDF83$Coat.colour=="Orange") & ((CoatDF83$Coat.pattern=="Broken") | (CoatDF83$Coat.pattern=="Spotted" | (CoatDF83$Coat.pattern=="Striped"))),]
TO$Type <- "TO"
#Tortoiseshell 
TS <- CoatDF83[(CoatDF83$Coat.colour=="Tortoiseshell" | CoatDF83$Coat.pattern=="Tortoiseshell"),]
TS$Type <- "TS"
#Solid black 
SB <- CoatDF83[(CoatDF83$Coat.colour=="Black" & CoatDF83$Coat.pattern=="Solid"),]
SB$Type <- "SB"

CoatType <- rbind(BB, BO, TB, TO, TS, SB)



cat.chart <- subset(CoatType, select=c("cam", "lon","lat","count", "Type"))
plyr::count(cat.chart, "Type")


#format for mini-charts map
cat.table <- table(cat.chart$cam, cat.chart$Type)
cat.chart1 <- data.frame(cat.chart$cam, cat.chart$lon, cat.chart$lat)
cam.loc <- unique(cat.chart1) # Delete rows
cam.loc  #DO NOT DELETE 

cat.table1<- cbind(cat.table, cam.loc$cat.chart.lon)
cat.table2 <- cbind(cat.table1, cam.loc$cat.chart.lat)
cat.table2
cat.chart.done <- as.data.frame.matrix(cat.table2)
cat.chart.done$total <- rowSums(cat.chart.done[,1:6] )
colnames(cat.chart.done ) <- c("Blotched-Brown", "Blotched-Orange", "Solid-Black", "Tabby-Brown", "Tabby-Orange", "Tortoiseshell", "lon", "lat", "total")
##Cross checked on data frame and this has worked; lat and lons line up! 



colors <- c("tan","orange","black", "brown",  "orangered", "pink")
library(leaflet.minicharts)
library(leaflet)
leaflet(CoatType)%>% addTiles() %>% #basemap, dataframe isn't important here I don't think 
  addMinicharts(
    cat.chart.done$lon, cat.chart.done$lat,
    type = "pie",
    chartdata = cat.chart.done[, c("Blotched-Brown", "Blotched-Orange", "Solid-Black", "Tabby-Brown", "Tabby-Orange", "Tortoiseshell")],
    width = 10, # * sqrt(cat.chart.done$total) / sqrt(max(cat.chart.done$total)), 
    colorPalette = colors) 
##IT WORKS! 



#------------------Six maps, one for each coat type--------------------------#
#Use this and make 6 
BBMap <- ggmap_obj + geom_point(aes(x = lon, y = lat), data = BB, size = 0.5, colour = "brown") +  
  scale_y_continuous(limits = c(-43.7, -40.5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(144.5, 148.5), expand = c(0, 0)) + 
  ggtitle("Blotched brown")

MBMap <- ggmap_obj + geom_point(aes(x = lon, y = lat), data = TB, size = 0.5, colour = "tan") +  
  scale_y_continuous(limits = c(-43.7, -40.5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(144.5, 148.5), expand = c(0, 0)) + 
  ggtitle("Mackerel brown")

BOMap <- ggmap_obj + geom_point(aes(x = lon, y = lat), data = BO, size = 0.5, colour = "orange") +  
  scale_y_continuous(limits = c(-43.7, -40.5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(144.5, 148.5), expand = c(0, 0)) + 
  ggtitle("Blotched orange")

MOMap<- ggmap_obj + geom_point(aes(x = lon, y = lat), data = TO, size = 0.5, colour = "red") +  
  scale_y_continuous(limits = c(-43.7, -40.5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(144.5, 148.5), expand = c(0, 0)) + 
  ggtitle("Mackerel orange")

TSMap <- ggmap_obj + geom_point(aes(x = lon, y = lat), data = TS, size = 0.5, colour = "maroon2") +  
  scale_y_continuous(limits = c(-43.7, -40.5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(144.5, 148.5), expand = c(0, 0)) + 
  ggtitle("Tortoiseshell")

SBMap <- ggmap_obj + geom_point(aes(x = lon, y = lat), data = SB, size = 0.5, colour = "grey") +  
  scale_y_continuous(limits = c(-43.7, -40.5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(144.5, 148.5), expand = c(0, 0)) + 
  ggtitle("Solid Black")


#Arrange maps 

#Multiplot function 
multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
  
}



multiplot(MBMap, MOMap, SBMap, BBMap, BOMap, TSMap,
          cols = 3)






#---------------------------------------------------------#

#--------------Multinomial and examination of the site data-----------#

#---------------------------------------------------------#

multidf
cam_dtnt <- read.csv("cam_dtnt.csv")

multidfNew <- merge(cam_dtnt, multidf, by = "cam", all= T)
multidfNew <- multidfNew[!is.na(multidfNew$BB),]

#Lazy formatting 
#write.csv(multidfNew, "multidfNew.csv")

multidfUpdate <- read.csv("MultiUpdatedDTNT.csv")

#Re-format dataframe for multinomial regression 
#First make a new column for no cats 
multidfUpdate$sums <- (multidfUpdate$BB+ multidfUpdate$BO + multidfUpdate$TO + 
                         multidfUpdate$TB + multidfUpdate$TS + multidfUpdate$SB)

#Manipulate to make a presence absence version 
PAMulti <- multidfUpdate
PAMulti$BB <- ifelse(PAMulti$BB > 0, 1, 0)
PAMulti$BO <- ifelse(PAMulti$BO > 0, 1, 0)
PAMulti$TO <- ifelse(PAMulti$TO > 0, 1, 0)
PAMulti$TB <- ifelse(PAMulti$TB > 0, 1, 0)
PAMulti$TS <- ifelse(PAMulti$TS > 0, 1, 0)
PAMulti$SB <- ifelse(PAMulti$SB > 0, 1, 0)
PAMulti$Absent <- ifelse(PAMulti$sums < 1, 1, 0)
PAMulti

pairs(multidfUpdate[, c('Top.Roughness', 'FPAR','Elevation', 
      'isothermal', 'dtnt', 'sums')])


#Reformat dataframe to work with multinomial logistic regression

PAMultiBB <- PAMulti[PAMulti$BB > 0,]
PAMultiBB$Coat <- "BB"
PAMultiBO <- PAMulti[PAMulti$BO > 0,]
PAMultiBO$Coat <- "BO"
PAMultiTB <- PAMulti[PAMulti$TB > 0,]
PAMultiTB$Coat <- "TB"
PAMultiTO <- PAMulti[PAMulti$TO > 0,]
PAMultiTO$Coat <- "TO"
PAMultiSB <- PAMulti[PAMulti$SB > 0,]
PAMultiSB$Coat <- "SB"
PAMultiTS <- PAMulti[PAMulti$TS > 0,]
PAMultiTS$Coat <- "TS"
PAMultiAbsent <- PAMulti[PAMulti$Absent > 0,]
PAMultiAbsent$Coat <- "Absent"

PAMultiReady <- rbind(PAMultiBB, PAMultiBO, PAMultiTB, 
                      PAMultiTO, PAMultiSB, PAMultiTS, PAMultiAbsent)

plyr::count(PAMultiReady$Coat)

plyr::count(PAMultiReady$Veg)


#Orange combined
PAMultiReadyO <- PAMultiReady
PAMultiReadyO$Coat[PAMultiReadyO$Coat == 'BO'] <- 'O'
PAMultiReadyO$Coat[PAMultiReadyO$Coat == 'TO'] <- 'O'
plyr::count(PAMultiReadyO$Coat)

#Orange combined and scaled variables
PAMultiReadyS <- PAMultiReadyO
PAMultiReadyS$dtnt <- scale(PAMultiReadyS$dtnt)
PAMultiReadyS$Top.Roughness <- scale(PAMultiReadyS$Top.Roughness)
PAMultiReadyS$FPAR <- scale(PAMultiReadyS$FPAR)
PAMultiReadyS$isothermal <- scale(PAMultiReadyS$isothermal)
PAMultiReadyS$Elevation <- scale(PAMultiReadyS$Elevation)

651-128

#-----------------------modelling-----------------------------#
#Using multinomial regression modelling. 

# Fit multinomial logistic regression model
library(nnet)

PAMultiReadySSubset <- PAMultiReadyS[!(PAMultiReadyS$Coat=="Absent"),]
PAMultiReadySSubset$Coat <- as.factor(PAMultiReadySSubset$Coat)

#Set black cats as the reference 
PAMultiReadySSubset$Coat <- relevel(PAMultiReadySSubset$Coat, "SB")

MultiModel <- multinom(Coat ~ Veg + FPAR + Elevation + isothermal + Top.Roughness + dtnt, data = PAMultiReadySSubset) #change dataset depending


summary(MultiModel)
# Obtain confidence intervals for coefficients
conf_intervals <- exp(confint(MultiModel))
confintDF <- as.data.frame(conf_intervals)

#Can't be arsed figuring out how to do this in R.
#write.csv(confintDF, "ConfintEnviroSAll.csv")
# Extract coefficients
coeffDF <- as.data.frame(exp(coef(MultiModel)))
#write.csv(coeffDF, "coefEnviroSAll.csv")


EditCofint <- read.csv("SubsetMulti.csv") #Change depending on the csv

str(EditCofint)
EditCofint$Coat <- as.factor(EditCofint$Coat)
EditCofint$Predictor <- as.factor(EditCofint$Predictor)

#Plot for this 

#coat_titles <- c("Blotched brown", "Orange", "Mackerel brown", "Tortoiseshell")


library(ggplot2)
# Create a ggplot for each Coat type
plots <- ggplot(EditCofint, aes(x = Predictor, y = OddsRatio, ymin = Lower, ymax = Upper, fill = Predictor)) +
  geom_errorbar(position = position_dodge(width = 0.7), width = 0.25) +
  geom_point(position = position_dodge(width = 0.7), size = 1, shape = 21, fill = "black") +
  facet_wrap(~Coat, scales = "free_y") +
  geom_hline(yintercept = 1, linetype = "dotted", color = "red") +  # Add a red dotted line through 1
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "Spatial predictors", y = "Odds ratio")
plots

#OddsRatioMultinom
