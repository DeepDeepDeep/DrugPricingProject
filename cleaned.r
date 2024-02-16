library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(reshape2)
library(stringr)
library(tibble)
library(forcats)
####Part 1: Datasets ####
####Part 1: Datasets ####
#2021 Dataset
main_dataset <- read.csv("vaFssPharmPrices.csv")
#2016 Dataset
second_dataset <- read.csv("foiaVApharmaceuticalPrices20161231.csv")

# Rename columns.
names(main_dataset) <- c("contract_number", "vendor", "contract_start",
              "contract_end", "NDC", "sub_item_identifier", "package", "generic_drug",
              "trade_name_drug", "classification", "covered", 
              "prime_vendor", "price", "price_start", "price_end", "price_type", "compliant")

names(second_dataset) <- c("contract_number", "vendor", "contract_start",
                "contract_end", "NDC", "sub_item_identifier", "package", "generic_drug",
                "trade_name_drug", "classification", "covered", 
                "prime_vendor", "price", "price_start", "price_end", "price_type")

# Remove the sub item identifier column
main_dataset <- select(main_dataset, -sub_item_identifier)
second_dataset <- select(second_dataset, -sub_item_identifier)

# Convert variables to logical variables.
main_dataset$covered <- as.logical(main_dataset$covered)
second_dataset$covered <- as.logical(second_dataset$covered)
main_dataset$prime_vendor <- as.logical(main_dataset$prime_vendor)
second_dataset$prime_vendor <- as.logical(second_dataset$prime_vendor)
main_dataset$compliant <- as.logical(factor(main_dataset$compliant, levels = c("Compliant", "Non-Compliant"), labels = c("T", "F")))


# Simplify classification variable -- the first two letters of a drug's classification represent the family of drugs they belong to,
# which is more than enough information for our analysis. There is a loss of specific information as a result, but our drug 
# classifications are now generalized down to groupings like "Immunological Agents" rather than "Immunoglobulins".
main_dataset$classification <- substring(main_dataset$classification, 1, 2)
second_dataset$classification <- substring(second_dataset$classification, 1, 2)

# Clean up fringe errors in the classification data. If the fringe case is clearly identifiable with what class it should fall into 
# (Using a 0 instead of a O, lowercase instead of uppercase, the same drugs in the file are all classified under a valid code) and 
# comparison and outside verification affirms that the nearest classification is correct for the particular drug, the drug is 
# reclassified appropriately. Else, the drug's classification is reassigned to NA.
main_dataset$classification <- toupper(main_dataset$classification)
second_dataset$classification <- toupper(second_dataset$classification)

main_dataset$classification[main_dataset$classification == "0T"] <- "OT"
second_dataset$classification[second_dataset$classification == "0T"] <- "OT"
main_dataset$classification[main_dataset$classification == "1M"] <- "IM"
second_dataset$classification[second_dataset$classification == "1M" | second_dataset$classification == "I,"] <- "IM"
main_dataset$classification[main_dataset$classification == "CA" | main_dataset$classification == "CH"] <- "CN"
second_dataset$classification[second_dataset$classification == "CA"] <- "CN"
second_dataset$classification[second_dataset$classification == " C"] <- "CV"
main_dataset$classification[main_dataset$classification == "HF"] <- "HS"

main_dataset$classification[main_dataset$classification == "10" | 
                              main_dataset$classification == "44" |
                              main_dataset$classification == "C0" |
                              main_dataset$classification == "C1" |
                              main_dataset$classification == "CA" |
                              main_dataset$classification == "CH" |
                              main_dataset$classification == "GO" |
                              main_dataset$classification == "IO" |
                              main_dataset$classification == "OY" |
                              main_dataset$classification == "XS"] <- NA

second_dataset$classification[second_dataset$classification == "CT" |
                                second_dataset$classification == "GO" |
                                second_dataset$classification == "IO" |
                                second_dataset$classification == "NO" |
                                second_dataset$classification == "TX"] <- NA


# Store class abbreviations and names in their respective character vectors
class_abbreviations <- c("AD", "AH", "AM", "AN", "AP", "AS", "AU", "BL", "CN", "CV", "DE",
                         "DX", "GA", "GU", "HA", "HS", "IM", "IN", "IR", "MS", "NT", "OP",
                         "OR", "OT", "PH", "RE", "RS", "TN", "VT", "XA", "XX")

class_names <- c("Antidotes and Deterrents", "Antihistamines", "Antimicrobials",
                 "Antineoplastics", "Antiparasitics", "Antiseptics and Disinfectants",
                 "Autonomic", "Blood", "Central Nervous System", "Cardiovascular",
                 "Dermatological", "Diagnostic", "Gastrointestinal", "Genitourinary",
                 "Herbal and Alternative", "Hormones and Synthetics", "Immunological",
                 "Investigational", "Irrigation and Dialysis", "Musculoskeletal",
                 "Nasal and Throat", "Ophthalmic", "Dental and Oral", "Otic",
                 "Pharmaceutical Aids", "Respiratory Tract", "Rectal", "Nutrients",
                 "Vitamins", "Prosthetics and Supplies", "Miscellaneous")

# Factor the classification variable and rename each factor to correspond with each classification code's meaning.
main_dataset$classification <- factor(main_dataset$classification,
                                      levels = class_abbreviations,
                                      labels = class_names)

second_dataset$classification <- factor(second_dataset$classification,
                                        levels = class_abbreviations,
                                        labels = class_names)

# Factor the price_type variable
main_dataset$price_type <- factor(main_dataset$price_type, labels = c("Big 4", "FSS", "VA National Contract"))
second_dataset$price_type <- factor(second_dataset$price_type, labels = c("Big 4", "FSS", "VA National Contract"))

# Convert the price variable from character to numeric
main_dataset$price <- as.numeric(main_dataset$price)
second_dataset$price <- as.numeric(second_dataset$price)

# Create a factored_price variable that categorizes drugs into log(10) price ranges.
price_groups <- c("< $1",  "$1 - $9", "$10 - $99", "$100 - $999", "$1,000 - $9,999", 
                  "$10,000 - $99,999", "$100,000 - $999,999", "> $1,000,000")

main_dataset$factored_price <- cut(log10(main_dataset$price), 
                                   breaks = -1:7, 
                                   labels = price_groups)
second_dataset$factored_price <- cut(log10(second_dataset$price), 
                                     breaks = -1:7, 
                                     labels = price_groups)

main_dataset$factored_price <- factor(main_dataset$factored_price, ordered = TRUE)
second_dataset$factored_price <- factor(second_dataset$factored_price, ordered = TRUE)

# Coerce date variables into Date objects
main_dataset$price_start <- paste(substring(main_dataset$price_start, 1, 6), substring(main_dataset$price_start, 9, 10), sep = "")
main_dataset$price_start <- as.Date(main_dataset$price_start, "%m/%d/%y")
main_dataset$price_end <- paste(substring(main_dataset$price_end, 1, 6), substring(main_dataset$price_end, 9, 10), sep = "")
main_dataset$price_end <- as.Date(main_dataset$price_end, "%m/%d/%y")

second_dataset$price_start <- paste(substring(second_dataset$price_start, 1, 6), substring(second_dataset$price_start, 9, 10), sep = "")
second_dataset$price_start <- as.Date(second_dataset$price_start, "%m/%d/%y")
second_dataset$price_end <- paste(substring(second_dataset$price_end, 1, 6), substring(second_dataset$price_end, 9, 10), sep = "")
second_dataset$price_end <- as.Date(second_dataset$price_end, "%m/%d/%y")

main_dataset$contract_start <- paste(substring(main_dataset$contract_start, 1, 6), substring(main_dataset$contract_start, 9, 10), sep = "")
main_dataset$contract_start <- as.Date(main_dataset$contract_start, "%m/%d/%y")
main_dataset$contract_end <- paste(substring(main_dataset$contract_end, 1, 6), substring(main_dataset$contract_end, 9, 10), sep = "")
main_dataset$contract_end <- as.Date(main_dataset$contract_end, "%m/%d/%y")

second_dataset$contract_start <- paste(substring(second_dataset$contract_start, 1, 6), substring(second_dataset$contract_start, 9, 10), sep = "")
second_dataset$contract_start <- as.Date(second_dataset$contract_start, "%m/%d/%y")
second_dataset$contract_end <- paste(substring(second_dataset$contract_end, 1, 6), substring(second_dataset$contract_end, 9, 10), sep = "")
second_dataset$contract_end <- as.Date(second_dataset$contract_end, "%m/%d/%y")

# Clean up fringe errors in the date data.

second_dataset$price_end[substring(second_dataset$price_end, 1, 4) == "2007"] <- NA
main_dataset$price_start[substring(main_dataset$price_start, 1, 4) == "2002" |
                           substring(main_dataset$price_start, 1, 4) == "2000"] <- NA
main_dataset$price_end[substring(main_dataset$price_end, 1, 4) == "2029"] <- NA

# Store price start dates as characters to be used later
price_start_dates <- levels(factor(main_dataset$price_start))

# Create contract_length variable
main_dataset$contract_length <- main_dataset$contract_end - main_dataset$contract_start
second_dataset$contract_length <- second_dataset$contract_end - second_dataset$contract_start

# Create vector of common vendors and subset x and x16 into only those drugs with common vendors
common_vendors <- unique((inner_join(main_dataset, second_dataset, by = "vendor"))$vendor)
x_common_vendors <- filter(main_dataset, vendor %in% common_vendors)
x16_common_vendors <- filter(second_dataset, vendor %in% common_vendors)

# Store all vendors as characters to be used later
vendor_names <- levels(factor(main_dataset$vendor))

# Store main color palette in character vector to more easily access specific colors
spectral_palette <- brewer.pal(11, "Spectral")


# --------------------- GRAPHS  ----------------------- #

# COMMON VENDORS AND HOW PRICE TYPES CHANGE
# Store the total number of contracts in each price type by vendor (of the vendors that are common between 2016 and 2021 data).
common_vendor_price_types <- by(x_common_vendors$price_type, x_common_vendors$vendor, summary)
common_vendor_price_types16 <- by(x16_common_vendors$price_type, x16_common_vendors$vendor, summary)

# Create vectors of the appropriate length for storage of information about vendor contracts for future usage.
VNC <- 1:length(common_vendors)
FSS <- 1:length(common_vendors)
Big4 <- 1:length(common_vendors)

VNC16 <- 1:length(common_vendors)
FSS16 <- 1:length(common_vendors)
Big416 <- 1:length(common_vendors)

# Iterate through each common vendor and store the total number of contracts in each price type in the respective vectors.
for (i in 1:length(common_vendors)) {
  VNC[i] <- common_vendor_price_types[[common_vendors[i]]][["VA National Contract"]]
  FSS[i] <- common_vendor_price_types[[common_vendors[i]]][["FSS"]]
  Big4[i] <- common_vendor_price_types[[common_vendors[i]]][["Big 4"]]
}

for (i in 1:length(common_vendors)) {
  VNC16[i] <- common_vendor_price_types16[[common_vendors[i]]][["VA National Contract"]]
  FSS16[i] <- common_vendor_price_types16[[common_vendors[i]]][["FSS"]]
  Big416[i] <- common_vendor_price_types16[[common_vendors[i]]][["Big 4"]]
}

# Create a data frame of the common vendors and the respective totals of each price type.
price_type_totals_2021 <- data.frame(cbind(common_vendors, VNC, FSS, Big4))
price_type_totals_2016 <- data.frame(cbind(common_vendors, VNC16, FSS16, Big416))

# Coerce into numeric types.
price_type_totals_2021$VNC <- as.numeric(price_type_totals_2021$VNC)
price_type_totals_2021$FSS <- as.numeric(price_type_totals_2021$FSS)
price_type_totals_2021$Big4 <- as.numeric(price_type_totals_2021$Big4)

price_type_totals_2016$VNC16 <- as.numeric(price_type_totals_2016$VNC16)
price_type_totals_2016$FSS16 <- as.numeric(price_type_totals_2016$FSS16)
price_type_totals_2016$Big416 <- as.numeric(price_type_totals_2016$Big416)

# Create a variable to keep track of the total number of contracts by a given vendor.
price_type_totals_2021$total_contracts <- price_type_totals_2021$VNC + price_type_totals_2021$FSS + 
  price_type_totals_2021$Big4
price_type_totals_2016$total_contracts16 <- price_type_totals_2016$VNC16 + price_type_totals_2016$FSS16 + 
  price_type_totals_2016$Big416

# Reassign the price type variables to the proportion of the total number of contracts they represent, rather than the raw count.
price_type_totals_2021$VNC <- price_type_totals_2021$VNC / price_type_totals_2021$total_contracts
price_type_totals_2021$FSS <- price_type_totals_2021$FSS / price_type_totals_2021$total_contracts
price_type_totals_2021$Big4 <- price_type_totals_2021$Big4 / price_type_totals_2021$total_contracts

price_type_totals_2016$VNC16 <- price_type_totals_2016$VNC16 / price_type_totals_2016$total_contracts16
price_type_totals_2016$FSS16 <- price_type_totals_2016$FSS16 / price_type_totals_2016$total_contracts16
price_type_totals_2016$Big416 <- price_type_totals_2016$Big416 / price_type_totals_2016$total_contracts16

# Combine the two data frames.
price_type_totals_combined <- inner_join(price_type_totals_2016, price_type_totals_2021, by = "common_vendors")

# Create a vector of the appropriate size and initialize counter variables to zero.
price_type_totals_combined$variations <- 1:length(price_type_totals_combined$FSS)
price_type_0range <- 0
price_type_5range <- 0
price_type_10range <- 0
price_type_15range <- 0
price_type_over15range <- 0

# Iterate through each vendor
for (i in 1:length(price_type_totals_combined$FSS)) {
  
  if (((price_type_totals_combined$FSS16[i] == price_type_totals_combined$FSS[i]) &
       (price_type_totals_combined$Big416[i] == price_type_totals_combined$Big4[i]) &
       (price_type_totals_combined$VNC16[i] == price_type_totals_combined$VNC[i]))) {
    
    price_type_totals_combined$variations[i] <- "0% change"
    price_type_0range <- price_type_0range + 1
    
  } else if (between(price_type_totals_combined$FSS16[i], 
                     (price_type_totals_combined$FSS[i] - 0.05), 
                     (price_type_totals_combined$FSS[i] + 0.05)) &
             between(price_type_totals_combined$Big416[i], 
                     (price_type_totals_combined$Big4[i] - 0.05), 
                     (price_type_totals_combined$Big4[i] + 0.05)) &
             between(price_type_totals_combined$VNC16[i], 
                     (price_type_totals_combined$VNC[i] - 0.05), 
                     (price_type_totals_combined$VNC[i] + 0.05))) {
    
    price_type_totals_combined$variations[i] <- "0% - 5% change"
    price_type_5range <- price_type_5range + 1
    
  } else if (between(price_type_totals_combined$FSS16[i], 
                     (price_type_totals_combined$FSS[i] - 0.10), 
                     (price_type_totals_combined$FSS[i] + 0.10)) &
             between(price_type_totals_combined$Big416[i], 
                     (price_type_totals_combined$Big4[i] - 0.10), 
                     (price_type_totals_combined$Big4[i] + 0.10)) &
             between(price_type_totals_combined$VNC16[i], 
                     (price_type_totals_combined$VNC[i] - 0.10), 
                     (price_type_totals_combined$VNC[i] + 0.10))) {
    
    price_type_totals_combined$variations[i] <- "5% - 10% change"
    price_type_10range <- price_type_10range + 1
    
  } else if (between(price_type_totals_combined$FSS16[i], 
                     (price_type_totals_combined$FSS[i] - 0.15), 
                     (price_type_totals_combined$FSS[i] + 0.15)) &
             between(price_type_totals_combined$Big416[i], 
                     (price_type_totals_combined$Big4[i] - 0.15), 
                     (price_type_totals_combined$Big4[i] + 0.15)) &
             between(price_type_totals_combined$VNC16[i], 
                     (price_type_totals_combined$VNC[i] - 0.15), 
                     (price_type_totals_combined$VNC[i] + 0.15))) {
    
    price_type_totals_combined$variations[i] <- "10% - 15% change"
    price_type_15range <- price_type_15range + 1
    
  } else {
    
    price_type_totals_combined$variations[i] <- "> 15% change"
    price_type_over15range <- price_type_over15range + 1
    
  }
}

price_type_totals_combined$variations <- factor(price_type_totals_combined$variations,
                                                ordered = TRUE,
                                                levels = c(1, 2, 3, 4, 5))

variation_distribution <- c(price_type_0range / 170, price_type_5range / 170, 
                            price_type_10range / 170,
                            price_type_15range / 170, price_type_over15range / 170)

# Plot the pie chart.
ggplot(mapping = aes(x = "", y = variation_distribution)) +
  geom_bar(stat = "identity", width = 1, mapping = aes(fill = levels(price_type_totals_combined$variations))) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_brewer(palette = "Spectral", direction = -1, labels = c("0% change", "0% - 5% change", 
                                                                     "5% - 10% change", "10% - 15% change",
                                                                     "> 15% change")) +
  theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) +
  labs(title = "Change in Contract Price Types
         with a Common Vendor")

# AVERAGE CONTRACT LENGTH BY PRICE TYPE
# Store the contract length means by price type
mean_contract_length_by_price_type <- by(main_dataset$contract_length / 365.0, main_dataset$price_type, mean)

# Mean Contract Length by Price Type
ggplot(mapping = aes(x = mean_contract_length_by_price_type, 
                     y = levels(main_dataset$price_type),
                     fill = levels(main_dataset$price_type))) + 
  geom_col(show.legend = FALSE) +
  theme_minimal() +
  labs(x = "Average Contract Length (Year)", y = "Price Type",
       title = "Average Contract Length in Years by Price Type") +
  scale_fill_manual(values = c(spectral_palette[2], spectral_palette[9], spectral_palette[10]))


# MEDIAN PRICE CHANGE BY CLASSIFICATION

# Store the summary data of price by classification
summary_prices_by_classification <- by(main_dataset$price, main_dataset$classification, summary)
summary_prices_by_classification16 <- by(second_dataset$price, second_dataset$classification, summary)

# Create a vector of the appropriate size
median_prices_by_classification <- 1:length(class_names)
median_prices_by_classification16 <- 1:length(class_names)

# Iterate through each classification and store the median prices in the previously created vector.
for (i in 1:length(class_names)) {
  
  # Store the median prices only in the vector
  median_prices_by_classification[i] <- summary_prices_by_classification[[class_names[i]]][["Median"]]
  median_prices_by_classification16[i] <- summary_prices_by_classification16[[class_names[i]]][["Median"]]
  
}

# Create a data frame out of the useful information and rename the columns
median_prices_by_classification_combined <- data.frame(cbind(class_names, median_prices_by_classification, 
                                                             median_prices_by_classification16))
names(median_prices_by_classification_combined) <- c("class", "median21", "median16")

# Coerce median variables into numeric types
median_prices_by_classification_combined$median16 <- as.numeric(median_prices_by_classification_combined
                                                                $median16)
median_prices_by_classification_combined$median21 <- as.numeric(median_prices_by_classification_combined
                                                                $median21)

# Create a variable to hold the difference in median prices from 2016 to 2021
median_prices_by_classification_combined$price_change <- 
  median_prices_by_classification_combined$median21 - median_prices_by_classification_combined$median16

# Create a variable to hold the percent price change from 2016 to 2021
median_prices_by_classification_combined$percent_change <- 
  median_prices_by_classification_combined$price_change / median_prices_by_classification_combined$median16

# Plot the percent change in median price for each classification
ggplot(data = subset(median_prices_by_classification_combined, class != "Antiseptics and Disinfectants" &
                       class != "Otic" &
                       class != "Investigational" &
                       class != "Miscellaneous" &
                       class != "Dental and Oral" &
                       class != "Irrigation and Dialysis")) +
  geom_col(mapping = aes(x = percent_change, y = fct_reorder(class, percent_change, sum, .desc = TRUE), 
                         fill = percent_change > 0), show.legend = FALSE) +
  scale_x_continuous(breaks = c(-1, -0.5, 0, 0.5, 1, 1.5),
                     labels = c("-100%", "-50%", "0%", "+50%", "+100%", "+150%")) +
  theme_minimal() +
  labs(title = "Percent Median Price Change by Classification",
       x = "Percent Change",
       subtitle = "Top 25 Classifications",
       y = "Classification") +
  scale_fill_manual(values = c(spectral_palette[2], spectral_palette[10]))




# MEDIAN PRICE BY DATE BY PRICE TYPE

# Store the summary data of price by price_start dates by price types
summary_prices_by_date_FSS <- by(main_dataset$price[main_dataset$price_type == "FSS"], main_dataset$price_start[main_dataset$price_type == "FSS"], summary)
summary_prices_by_date_Big4 <- by(main_dataset$price[main_dataset$price_type == "Big 4"], main_dataset$price_start[main_dataset$price_type == "Big 4"], summary)
summary_prices_by_date_VNC <- by(main_dataset$price[main_dataset$price_type == "VA National Contract"], 
                                 main_dataset$price_start[main_dataset$price_type == "VA National Contract"], summary)

# Create vectors of the appropriate size.
median_prices_by_date_FSS <- 1:length(price_start_dates)
median_prices_by_date_Big4 <- 1:length(price_start_dates)
median_prices_by_date_VNC <- 1:length(price_start_dates)

# Iterate through each date
for (i in 1:length(price_start_dates)) {
  
  if (as.Date(price_start_dates[i]) %in% main_dataset$price_start[main_dataset$price_type == "FSS"])
    median_prices_by_date_FSS[i] <- summary_prices_by_date_FSS[[price_start_dates[i]]][["Median"]]
  else
    median_prices_by_date_FSS[i] <- NA
  
  if (as.Date(price_start_dates[i]) %in% main_dataset$price_start[main_dataset$price_type == "Big 4"])
    median_prices_by_date_Big4[i] <- summary_prices_by_date_Big4[[price_start_dates[i]]][["Median"]]
  else
    median_prices_by_date_Big4[i] <- NA
  
  if (as.Date(price_start_dates[i]) %in% main_dataset$price_start[main_dataset$price_type == "VA National Contract"])
    median_prices_by_date_VNC[i] <- summary_prices_by_date_VNC[[price_start_dates[i]]][["Median"]]
  else
    median_prices_by_date_VNC[i] <- NA
}

# Create a data frame to hold the medians and rename the columns.
median_prices_by_date_and_price_type_df <- data.frame(cbind(price_start_dates, 
                                                            median_prices_by_date_FSS, 
                                                            median_prices_by_date_Big4,
                                                            median_prices_by_date_VNC))
names(median_prices_by_date_and_price_type_df) <- c("price_start", "median_price_FSS", 
                                                    "median_price_Big4", "median_price_VNC")

# Coerce price_start back to Date type and medians into numeric type
median_prices_by_date_and_price_type_df$price_start <- as.Date(median_prices_by_date_and_price_type_df
                                                               $price_start)
median_prices_by_date_and_price_type_df$median_price_FSS <- as.numeric(median_prices_by_date_and_price_type_df
                                                                       $median_price_FSS)
median_prices_by_date_and_price_type_df$median_price_Big4 <- as.numeric(median_prices_by_date_and_price_type_df
                                                                        $median_price_Big4)
median_prices_by_date_and_price_type_df$median_price_VNC <- as.numeric(median_prices_by_date_and_price_type_df
                                                                       $median_price_VNC)

# Plot the data
ggplot(mapping = aes(x = median_prices_by_date_and_price_type_df$price_start)) +
  scale_color_manual(name = "Price Type", values = c(FSS_color = spectral_palette[9], 
                                                     Big4_color = spectral_palette[2], 
                                                     VNC_color = spectral_palette[10]),
                     labels = c("FSS", "Big 4", "VA National Contract")) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  geom_point(mapping = aes(y = median_prices_by_date_and_price_type_df$median_price_FSS, color = "FSS_color"), 
             size = 0.3) +
  geom_point(mapping = aes(y = median_prices_by_date_and_price_type_df$median_price_Big4, color = "Big4_color"), 
             size = 0.3) +
  geom_point(mapping = aes(y = median_prices_by_date_and_price_type_df$median_price_VNC, color = "VNC_color"), 
             size = 0.3) +
  geom_rug(data = subset(main_dataset, price < 25000), mapping = aes(x = price_start, y = price), sides = "b", 
           inherit.aes = FALSE) +
  labs(title = "Median Price Per Day by Price Type",
       x = "Price Start Date",
       y = "Price ($)")


# PROPORTION OF PRICE TYPE BY CLASSIFICATION 
ggplot(data = subset(main_dataset, !is.na(classification) &
                       classification != "Antiseptics and Disinfectants" &
                       classification != "Otic" &
                       classification != "Investigational" &
                       classification != "Miscellaneous" &
                       classification != "Dental and Oral" &
                       classification != "Irrigation and Dialysis"), 
       mapping = aes(y = fct_infreq(factor(classification)), fill = price_type)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  scale_fill_manual(values = c("FSS" = spectral_palette[9], "Big 4" = spectral_palette[2], 
                               "VA National Contract" = spectral_palette[10])) +
  theme(legend.title = element_blank()) +
  labs(title = "Distribution of Different Price Types Per Classification",
       x = "Proportion",
       subtitle = "Top 25 Classifications",
       y = "Classification")

#Heatmap Vendor and Classification
t<-main_dataset[,c(2,9)]
t1<-t %>% 
  group_by(vendor,classification) %>% 
  summarise(Count=n())
t1<-t1[order(t1$Count,decreasing = T),]

t2<-t1[c(1:25),]

ggplot(t2,aes(x=classification,y=vendor)) +
  geom_tile(aes(fill=Count))+
  geom_text(aes(label=Count),col="black") +
  labs(title = "Top Vendor + Classification Pairings",
       y="Vendor",
       x="Classification") +
  theme_minimal() +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  scale_x_discrete(labels = function(main_dataset) str_wrap(main_dataset, width = 20)) +
  theme(axis.text.x.bottom = element_text(angle = 330, hjust = 0, size = 7))


#Mean
mean(main_dataset$contract_length) / 365


# NUM CLASSIFICATIONS SOLD PER VENDOR

# Find summary data about the number of classifications each vendor typically sells
num_vendor_appearances <- data.frame(table(t1$vendor))
names(num_vendor_appearances) <- c("vendors", "num_classifications")

# Plot a boxplot of the number of classifications per vendor
ggplot(data = num_vendor_appearances, mapping = aes(y = num_classifications, x = 1)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(size = 0.3) +
  theme_minimal() +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  theme(axis.text.x.bottom = element_blank(), 
        axis.title.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank()) +
  labs(title = "Number of Different Classifications 
                  Sold Per Vendor",
       y = "Number of Classifications")

# Plot a histogram of the number of classifications per vendor
ggplot(data = num_vendor_appearances, mapping = aes(x = num_classifications)) +
  geom_histogram(bins = ceiling(sqrt(length(num_vendor_appearances$vendors)))) +
  theme_minimal() +
  labs(title = "Number of Different Classifications Sold Per Vendor",
       y = "Count",
       x = "Number of Classifications")
