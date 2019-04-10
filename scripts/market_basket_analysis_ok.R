
#### 0. INCLUDES ####
#Load Libraries: p_load can install, load,  and update packages
if(require("pacman")=="FALSE"){
  install.packages("pacman")
}

perl=TRUE

pacman::p_load(rstudioapi, dplyr,magrittr, tidyr, reshape2, readxl, stringi,
               ggplot2,caret,corrplot,rpart,e1071,arules,arulesViz,gdata)

# Setwd (1º current wd where is the script, then we move back to the
# general folder)
current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)


#DATA SETS

#Original Data set
electronidex<-read.transactions("../market_basket_analysis/datasets/ElectronidexTransactions2017.csv",
                                format = "basket", header = FALSE, sep = ",", cols=NULL, rm.duplicates = FALSE, 
                                quote = "\"'", skip = 0,encoding = "unknown")

#b2b splitted data set
b2b<-read.transactions("../market_basket_analysis/datasets/B2BTransacs.csv", header = T, sep = ",")

#b2c splitted data set
b2c<-read.transactions("../market_basket_analysis/datasets/B2CTransacs.csv", header = T, sep = ",")

#data set cointaining all Electronidex products with their category in the name
master_products<-read.csv("../market_basket_analysis/info/Master_products_categories.csv",header = FALSE, sep = ",")

#data set cointaining unique categories for blackwells and electronidex items
categories<-read.csv("../market_basket_analysis/datasets/ProductCategories(Blackwell_and_Electronidex).csv",header = TRUE, sep = ",")




#### 1. PRE-PROCESS ####

inspect (electronidex) # You can view the transactions. Is there a way to see a certain # of transactions?
length (electronidex) # Number of transactions.
size (electronidex) # Number of items per transaction
LIST(electronidex) # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(b2c)# To see the item labels

#table with top 5 sold products
tbl <- crossTable(electronidex, sort=TRUE)
tbl[1:5,1:5]

tbl['iMac','iMac']
tbl['iMac','HP Laptop']

#what products tend to compliment each other with high lift (more than 1 -> complementary | less than 1 -> substitute)
crossTable(electronidex, measure='lift', sort=T)[1:5,1:5]

#To convince ourselves that lower than 1 lift is not due to chance, let's apply chiSquared test:
crossTable(electronidex, measure='chi')['iMac', 'Apple MacBook Air'] #low p-value (0.0032) confirms lift results



# CREATING ELECTRONIDEX CATEGORIES
#data set cointaining all Electronidex products with their category in the name
master_products<-read.csv("../market_basket_analysis/info/Master_products_categories.csv",header = FALSE, sep = ",")

#transform data into a vector
master_products<-as.vector(t(master_products))
summary(master_products)

#replacing item's names with their respective category:
master_products[grep("Laptops.",master_products)]<-"laptop"
master_products[grep("Desktop.",master_products)]<-"desktop"
master_products[grep("Monitors.",master_products)]<-"monitor"
master_products[grep("Computer.Mice",master_products)]<-"mouse"
master_products[grep("Keyboard.",master_products)]<-"keyboard"
master_products[grep("Mouse.and.Keyboard.Combo.",master_products)]<-"M&K_combo"
master_products[grep("Computer.Headphones.",master_products)]<-"compu_headphone"
master_products[grep("Active.Headphones.",master_products)]<-"active_headphone"
master_products[grep("Computer.Cords.",master_products)]<-"compu_cord"
master_products[grep("Accessories.",master_products)]<-"accessory"
master_products[grep("Speakers.",master_products)]<-"speaker"
master_products[grep("Printers.",master_products)]<-"printer"
master_products[grep("Printer.Ink.",master_products)]<-"printer_ink"
master_products[grep("Computer.Stands.",master_products)]<-"compu_stand"
master_products[grep("Computer.Tablets.",master_products)]<-"tablet"
master_products[grep("External.Hardrives.",master_products)]<-"ext_hardrive"
master_products[grep("Smart.Home.Devices.",master_products)]<-"smart_home"

print(master_products)



# BLACKWELLS AND ELECTRONIDEX CATEGORIES

#reading the csv cointaining all products with their category in the name
categories<-read.csv("../market_basket_analysis/datasets/ProductCategories(Blackwell_and_Electronidex).csv",header = TRUE, sep = ",")
categories

#leaving empty labels level in transactional file and putting it into a data frame
TempDFB2B <- as.data.frame(b2b@itemInfo$labels )

#renaming first column of the data frame
colnames(TempDFB2B)[1] <- "ProdName"

#checking the vector
TempDFB2B

#merging the column ProdName of categories file and the column ProdName of TempDFB2B into only 1 column
TempDFB2B2 <- merge(TempDFB2B, categories, by.x="ProdName", by.y="ProdName")

#putting the values of BWCat column of the data set into the categories level of transactional file
b2b@itemInfo$categories <- TempDFB2B2$BWCat



# ELECTRONIDEX BRANDS
#reading the csv cointaining all products with their category in the name
master_products2<-read.csv("../market_basket_analysis/info/Master_products_categories.csv",header = FALSE, sep = ",")

#transform data into a vector
master_products2<-as.vector(t(master_products2))
summary(master_products2)

#replacing item's names with their respective brand:
master_products2[grep("Apple",master_products2)]<-"apple"
master_products2[grep("iMac",master_products2)]<-"apple"
master_products2[grep("iPhone",master_products2)]<-"apple"
master_products2[grep("iPad",master_products2)]<-"apple"
master_products2[grep("Acer",master_products2)]<-"acer"
master_products2[grep("Alienware",master_products2)]<-"alienware"
master_products2[grep("ASUS",master_products2)]<-"asus"
master_products2[grep("Dell",master_products2)]<-"dell"
master_products2[grep("HP",master_products2)]<-"hp"
master_products2[grep("Lenovo",master_products2)]<-"lenovo"
master_products2[grep("Logitech",master_products2)]<-"logitech"
master_products2[grep("Samsung",master_products2)]<-"samsung"
master_products2[grep("Belkin",master_products2)]<-"belkin"
master_products2[grep("Brother",master_products2)]<-"brother"
master_products2[grep("Canon",master_products2)]<-"canon"
master_products2[grep("Cambridge",master_products2)]<-"cambridge"
master_products2[grep("Cyber",master_products2)]<-"cyber"
master_products2[grep("DOSS",master_products2)]<-"doss"
master_products2[grep("DYMO",master_products2)]<-"dymo"
master_products2[grep("Eluktronics",master_products2)]<-"eluktronics"
master_products2[grep("Fire",master_products2)]<-"amazon"
master_products2[grep("Google",master_products2)]<-"google"
master_products2[grep("Halter",master_products2)]<-"halter"
master_products2[grep("XIBERIA",master_products2)]<-"xiberia"
master_products2[grep("Mackie",master_products2)]<-"mackie"
master_products2[grep("Kensington",master_products2)]<-"kensington"
master_products2[grep("Koss",master_products2)]<-"koss"
master_products2[grep("Monster",master_products2)]<-"monster"
master_products2[grep("Otium",master_products2)]<-"otium"
master_products2[grep("Rokono",master_products2)]<-"rokono"
master_products2[grep("LG",master_products2)]<-"lg"
master_products2[grep("JBL",master_products2)]<-"jbl"
master_products2[grep("Otium",master_products2)]<-"otium"
master_products2[grep("Ailihen",master_products2)]<-"ailihen"
master_products2[grep("ViewSonic",master_products2)]<-"viewsonic"
master_products2[grep("Sonos",master_products2)]<-"sonos"
master_products2[grep("Sceptre",master_products2)]<-"sceptre"
master_products2[grep("Rii",master_products2)]<-"rii"
master_products2[grep("Redragon",master_products2)]<-"redragon"
master_products2[grep("Panasonic",master_products2)]<-"panasonic"
master_products2[grep("Microsoft",master_products2)]<-"microsoft"
master_products2[grep("Roku",master_products2)]<-"roku"
master_products2[grep("Philips",master_products2)]<-"philips"
master_products2[grep("LG",master_products2)]<-"lg"
master_products2[grep("Kindle",master_products2)]<-"kindle"
master_products2[grep("EagleTec",master_products2)]<-"eagletec"
master_products2[grep("Epson",master_products2)]<-"epson"
master_products2[grep("Etekcity",master_products2)]<-"etekcity"
master_products2[grep("AOC",master_products2)]<-"aoc"
master_products2[grep("APIE",master_products2)]<-"apie"
master_products2[grep("Backlit",master_products2)]<-"backlit"
master_products2[grep("Bose",master_products2)]<-"bose"
master_products2[grep("CYBERPOWER",master_products2)]<-"cyberpower"
master_products2[grep("Intel",master_products2)]<-"intel"
master_products2[grep("Cable",master_products2)]<-"unknown"
master_products2[grep("Adapter",master_products2)]<-"unknown"
master_products2[grep("Hardrives",master_products2)]<-"unknown"
master_products2[grep("Computer",master_products2)]<-"unknown"
master_products2[grep("Pad",master_products2)]<-"unknown"
master_products2[grep("Bulb",master_products2)]<-"unknown"

print(master_products2)



#### B2B RULES ####

#Rules b2b
Rules_b2b<- apriori (b2b, parameter = list(supp = 0.002, conf = 0.8, minlen=1))

#looking for redundant rules
is.redundant(Rules_b2b)

#show if there are redundant rules
which(is.redundant(Rules_b2b), arr.ind = TRUE, useNames = TRUE)

#removing redundant rules
Rules_b2b <- Rules_b2b[!is.redundant(Rules_b2b)]

#confirming there are'nt more redundant rules
is.redundant(Rules_b2b)

#rules.sorted<-sort(Rules_b2b, by="lift")
#inspect(Rules_b2b)
ruleExplorer(Rules_b2b)


#B2B RULES FILTERED BY ELECTRONIDEX CATEGORIES

#adding renamed Electronidex items into categories level
b2b@itemInfo$categories<-master_products
str(b2b)

#creating transactional for categories
B2B_categories<- aggregate(b2b, by = "categories")
str(B2B_categories)
B2B_categories

#creating the rules
Rules_b2b_electronidex_cat<- apriori (B2B_categories, parameter = list(supp = 0.01, conf = 0.8, minlen=1))

#looking for redundant rules
is.redundant(Rules_b2b_electronidex_cat)

#show if there are redundant rules
which(is.redundant(Rules_b2b_electronidex_cat), arr.ind = TRUE, useNames = TRUE)

#removing redundant rules
Rules_b2b_electronidex_cat <- Rules_b2b_electronidex_cat[!is.redundant(Rules_b2b_electronidex_cat)]

#confirming there are'nt more redundant rules
is.redundant(Rules_b2b_electronidex_cat)

#rules.sorted<-sort(Rules_b2b_electronidex_cat, by="lift")
#inspect(Rules_b2b_electronidex_cat)
ruleExplorer(Rules_b2b_electronidex_cat)


#MIXING BLACKWELLS CATEGORIES AND ELECTRONIDEX CATEGORIES

#Rules b2b by categories
Rules_b2b_unique_cat<- apriori (TempDFB2B2, parameter = list(maxlen=3, supp = 0.003, conf = 0.6, minlen=1))


#looking for redundant rules
is.redundant(Rules_b2b_unique_cat)

#show if there are redundant rules
which(is.redundant(Rules_b2b_unique_cat), arr.ind = TRUE, useNames = TRUE)

#removing redundant rules
Rules_b2b_unique_cat <- Rules_b2b_unique_cat[!is.redundant(Rules_b2b_unique_cat)]

#confirming there are'nt more redundant rules
is.redundant(Rules_b2b_unique_cat)

#rules.sorted<-sort(Rules_b2b_unique_cat, by="lift")
#inspect(Rules_b2b_unique_cat)
ruleExplorer(Rules_b2b_unique_cat)


#B2B RULES FILTERED BY BRANDS

#adding renamed Electronidex items into brands level
b2b@itemInfo$brands<-master_products2

str(b2b)

#creating transactional for brands
B2B_brands <- aggregate(b2b, by = "brands")
str(B2B_brands)

#Rules b2b by brands
Rules_b2b_brands<- apriori (B2B_brands, parameter = list(supp = 0.01, conf = 0.7, minlen=1))

#looking for redundant rules
is.redundant(Rules_b2b_brands)

#show if there are redundant rules
which(is.redundant(Rules_b2b_brands), arr.ind = TRUE, useNames = TRUE)

#removing redundant rules
Rules_b2b_brands <- Rules_b2b_brands[!is.redundant(Rules_b2b_brands)]

#confirming there are'nt more redundant rules
is.redundant(Rules_b2b_brands)

#rules.sorted<-sort(Rules_b2b_brands, by="lift")
#inspect(Rules_b2b_brands)
ruleExplorer(Rules_b2b_brands)



#### B2C RULES ####

#B2C RULES FILTERED BY ELECTRONIDEX CATEGORIES
#adding renamed items into categories level
b2c@itemInfo$categories<-master_products
str(b2c)

#creating transactional for categories
B2C_categories <- aggregate(b2c, by = "categories")
str(B2C_categories)

#Rules b2c by categories
Rules_b2c_categories<- apriori (B2C_categories, parameter = list(supp = 0.001, conf = 0.7, minlen=1))

#looking for redundant rules
is.redundant(Rules_b2c_categories)

#show if there are redundant rules
which(is.redundant(Rules_b2c_categories), arr.ind = TRUE, useNames = TRUE)

#removing redundant rules
Rules_b2c_categories <- Rules_b2c_categories[!is.redundant(Rules_b2c_categories)]

#confirming there are'nt more redundant rules
is.redundant(Rules_b2c_categories)

#rules.sorted<-sort(Rules_b2c_categories, by="lift")
#inspect(Rules_b2c_categories)
ruleExplorer(Rules_b2c_categories)



# RULES B2C MIXING BLACKWELLS AND ELECTRONIDEX CATEGORIES

TempDFB2C <- as.data.frame(b2c@itemInfo$labels )

colnames(TempDFB2C)[1] <- "ProdName"

TempDFB2C

TempDFB2C2 <- merge(TempDFB2C, categories, by.x="ProdName", by.y="ProdName")

b2c@itemInfo$categories <- TempDFB2C2$BWCat


#Rules b2c Blackwells & Electronidex
Rules_b2c_unique_cat<- apriori (b2c, parameter = list(supp = 0.001, conf = 0.7, minlen=1))


#looking for redundant rules
is.redundant(Rules_b2c_unique_cat)

#show if there are redundant rules
which(is.redundant(Rules_b2c_unique_cat), arr.ind = TRUE, useNames = TRUE)

#removing redundant rules
Rules_b2c_unique_cat <- Rules_b2c_unique_cat[!is.redundant(Rules_b2c_unique_cat)]

#confirming there are'nt more redundant rules
is.redundant(Rules_b2c_unique_cat)

#rules.sorted<-sort(Rules_b2c_unique_cat, by="lift")
#inspect(Rules_b2c_unique_cat)
ruleExplorer(Rules_b2c_unique_cat)



#B2C RULES FILTERED BY BRANDS

#adding renamed Electronidex items into brands level
b2c@itemInfo$brands<-master_products2

str(b2c)

#creating transactional for brands
B2C_electronidex_brands <- aggregate(b2c, by = "brands")
str(B2C_brands)

#Rules b2c by brands
Rules_b2c_brands<- apriori (B2C_electronidex_brands, parameter = list(supp = 0.001, conf = 0.6, minlen=1))

#looking for redundant rules
is.redundant(Rules_b2c_brands)

#show if there are redundant rules
which(is.redundant(Rules_b2c_brands), arr.ind = TRUE, useNames = TRUE)

#removing redundant rules
Rules_b2c_brands <- Rules_b2c_brands[!is.redundant(Rules_b2c_brands)]

#confirming there are'nt more redundant rules
is.redundant(Rules_b2c_brands)

#rules.sorted<-sort(Rules_b2c_unique_cat, by="lift")
#inspect(Rules_b2c_unique_cat)
ruleExplorer(Rules_b2c_brands)



#### PLOTTING ####

##item frequencies

#top-sellers

itemFrequencyPlot(electronidex, topN=10)


itemFrequencyPlot(electronidex,
                  type="relative",
                  topN=30, # can be changed to the number of interest
                  horiz=FALSE,
                  col='yellow',
                  xlab='',
                  main='Item frequency, relative')

itemFrequencyPlot(electronidex,
                  type="absolute",
                  topN=30,
                  horiz=FALSE,
                  col='yellow',
                  xlab='',
                  main='Item frequency, absolute')



#least sellers
#1to10 relative
barplot(sort(table(unlist(LIST(electronidex))))[1:10]/9835,
        horiz=FALSE,
        las=1,
        col='yellow',
        xlab='',
        main='Frequency, relative')
#1to10 absolute
barplot(sort(table(unlist(LIST(Groceries))))[1:10],
        horiz=FALSE,
        las=1,
        col='yellow',
        xlab='',
        main='Frequency, absolute')


image(sample(electronidex, 125, bins=32))

image(electronidex@itemInfo$labels, 125)


plotly_arules(rules.sorted)
plot(rules.sorted, method="graph", max=5, control=list(type="items"))
plot(rules.sorted, method="paracoord", control=list(reorder=TRUE), max=5)
plot(rules.sorted[1:3], method="graph"))




#### TESTING ####


#CATEGORIZING BY CATEGORIES
master_products

grep("Laptops.",master_products) #checking for the word Laptops. inside the vector master_products

#replacing item's names with their respective category:
master_products[grep("Laptops.",master_products)]<-"laptops"
master_products[grep("Desktop.",master_products)]<-"desktop"
master_products[grep("Monitors.",master_products)]<-"monitor"
master_products[grep("Computer.Mice",master_products)]<-"mouse"
master_products[grep("Keyboard.",master_products)]<-"keyboard"
master_products[grep("Mouse.and.Keyboard.Combo.",master_products)]<-"M&K_combo"
master_products[grep("Computer.Headphones.",master_products)]<-"compu_headphone"
master_products[grep("Active.Headphones.",master_products)]<-"active_headphone"
master_products[grep("Computer.Cords.",master_products)]<-"compu_cord"
master_products[grep("Accessories.",master_products)]<-"accessory"
master_products[grep("Speakers.",master_products)]<-"speaker"
master_products[grep("Printers.",master_products)]<-"printer"
master_products[grep("Printer.Ink.",master_products)]<-"printer_ink"
master_products[grep("Computer.Stands.",master_products)]<-"compu_stand"
master_products[grep("Computer.Tablets.",master_products)]<-"tablet"
master_products[grep("External.Hardrives.",master_products)]<-"ext_hardrive"
master_products[grep("Smart.Home.Devices.",master_products)]<-"smart_home"

print(master_products)

#adding renamed items into categories label
electronidex@itemInfo$categories<-master_products 
str(electronidex)
str(b2b)

#CATEGORIZING BY BRANDS
#reading the csv cointaining all products with their category in the name
master_products2<-read.csv("../market_basket_analysis/info/Master_products_categories.csv",header = FALSE, sep = ",")

#transform data into a vector
master_products2<-as.vector(t(master_products2))
summary(master_products2)

#replacing item's names with their respective brand:
master_products2[grep("Apple",master_products2)]<-"apple"
master_products2[grep("iMac",master_products2)]<-"apple"
master_products2[grep("iPhone",master_products2)]<-"apple"
master_products2[grep("iPad",master_products2)]<-"apple"
master_products2[grep("Acer",master_products2)]<-"acer"
master_products2[grep("Alienware",master_products2)]<-"alienware"
master_products2[grep("ASUS",master_products2)]<-"asus"
master_products2[grep("Dell",master_products2)]<-"dell"
master_products2[grep("HP",master_products2)]<-"hp"
master_products2[grep("Lenovo",master_products2)]<-"lenovo"
master_products2[grep("Logitech",master_products2)]<-"logitech"
master_products2[grep("Samsung",master_products2)]<-"samsung"
master_products2[grep("Belkin",master_products2)]<-"belkin"
master_products2[grep("Brother",master_products2)]<-"brother"
master_products2[grep("Canon",master_products2)]<-"canon"
master_products2[grep("Cambridge",master_products2)]<-"cambridge"
master_products2[grep("Cyber",master_products2)]<-"cyber"
master_products2[grep("DOSS",master_products2)]<-"doss"
master_products2[grep("DYMO",master_products2)]<-"dymo"
master_products2[grep("Eluktronics",master_products2)]<-"eluktronics"
master_products2[grep("Fire",master_products2)]<-"fire"
master_products2[grep("Google",master_products2)]<-"google"
master_products2[grep("Halter",master_products2)]<-"halter"
master_products2[grep("XIBERIA",master_products2)]<-"xiberia"
master_products2[grep("Mackie",master_products2)]<-"mackie"
master_products2[grep("Kensington",master_products2)]<-"kensington"
master_products2[grep("Koss",master_products2)]<-"koss"
master_products2[grep("Monster",master_products2)]<-"monster"
master_products2[grep("Otium",master_products2)]<-"otium"
master_products2[grep("Rokono",master_products2)]<-"rokono"
master_products2[grep("LG",master_products2)]<-"lg"
master_products2[grep("JBL",master_products2)]<-"jbl"
master_products2[grep("Otium",master_products2)]<-"otium"
master_products2[grep("Ailihen",master_products2)]<-"ailihen"
master_products2[grep("ViewSonic",master_products2)]<-"viewsonic"
master_products2[grep("Sonos",master_products2)]<-"sonos"
master_products2[grep("Sceptre",master_products2)]<-"sceptre"
master_products2[grep("Rii",master_products2)]<-"rii"
master_products2[grep("Redragon",master_products2)]<-"redragon"
master_products2[grep("Panasonic",master_products2)]<-"panasonic"
master_products2[grep("Microsoft",master_products2)]<-"microsoft"
master_products2[grep("Roku",master_products2)]<-"roku"
master_products2[grep("Philips",master_products2)]<-"philips"
master_products2[grep("LG",master_products2)]<-"lg"
master_products2[grep("Kindle",master_products2)]<-"kindle"
master_products2[grep("EagleTec",master_products2)]<-"eagletec"
master_products2[grep("Epson",master_products2)]<-"epson"
master_products2[grep("Etekcity",master_products2)]<-"etekcity"
master_products2[grep("AOC",master_products2)]<-"aoc"
master_products2[grep("APIE",master_products2)]<-"apie"
master_products2[grep("Backlit",master_products2)]<-"backlit"
master_products2[grep("Bose",master_products2)]<-"bose"
master_products2[grep("CYBERPOWER",master_products2)]<-"cyberpower"
master_products2[grep("Intel",master_products2)]<-"intel"

print(master_products2)

#adding renamed items into categories label
electronidex@itemInfo$brands<-master_products2

str(electronidex)




as.data.frame(master_products)<-master_products


master_products

as.data.frame(master_products)<-master_products

itemLabels(electronidex)
?aggregate
apply()
#data frame to create b2b or b2c columns


## for better comparison we sort the rules by confidence and add Bayado's improvement
#rules <- sort(rules, by = "confidence")
#quality(rules)$improvement <- interestMeasure(rules, measure = "improvement")


#Rules b2b
# Rules_b2b<- apriori (b2b, parameter = list(supp = 0.01, conf = 0.7, minlen=1))
# rules.sorted<-sort(Rules_b2b, by="lift")
# inspect(Rules_b2b)
# 
# #Rules b2c
# Rules_b2c<- apriori (b2c, parameter = list(supp = 0.001, conf = 0.7, minlen=1))
# rules.sorted<-sort(Rules_b2c, by="lift")
# inspect(Rules_b2c)
# 
# ruleExplorer(BasketRules)

# 
# first_rules<-write.csv(BasketRules,"../market_basket_analysis/info/first_rules.csv")
# 
# summary(Rules_b2b)
# inspect(Rules_b2b, n=5)



# ## Select a subset of rules using partial matching on the items 
# ## in the right-hand-side and a quality measure
# BasketRules.sub <- subset(BasketRules, subset = rhs %pin% "HP" & lift > 1.3)
# inspect(BasketRules.sub)
# 
# ## Display the top 3 support rules
# inspect(head(BasketRules.sub, n = 3, by = "support"))
# 
# ## Display the first 3 rules
# inspect(BasketRules.sub[1:3])
# 
# ## Get labels for the first 3 rules
# labels(BasketRules.sub[1:3])
# labels(BasketRules.sub[1:3], itemSep = " + ", setStart = "", setEnd="", 
#        ruleSep = " ---> ")
# 
# 
# is.redundant(rules.sorted)
# # find redundant rules
# subset.matrix <- is.subset(rules.sorted, rules.sorted)
# subset.matrix
# 
# # subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
# redundant <- colSums(subset.matrix, na.rm=T) >= 1
# redundant
# summary(redundant)
# # which(redundant)
#   # [1] 2 4 7 8
# # remove redundant rules
# rules.pruned <- rules.sorted[!redundant]
# rules.pruned
# 




#Probando merge#