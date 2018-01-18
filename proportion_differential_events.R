# 1 load libraries
library(tidyverse)
library(reshape2)

# 2 load data


# 3 filters significant differential splicing events of dPsi >= 0.05 and labels each event with the even type
filtered_diff_event <- function(data_frame) {
	filter(data_frame, `MV[dPsi]_at_0.95` >= 0.05) %>%  # filters through dataset 
	inner_join(inclusion_levels_full, by = "EVENT") %>%  # joins the data_frame to the inclusion table by EVENT
	select(GENE.x:`MV[dPsi]_at_0.95`, COMPLEX)  # selects for variables in the original data frame with the COMPLEX (event type) 
}

# 4 groups the table by COMPLEX (event type) and finds the frequency of the occurence of each COMPLEX; gives a final table containing the COMPLEX and proportion
prop_diff_event <- function(data_frame) {
	group_by(data_frame, COMPLEX) %>%  # groups rows by COMPLEX
	summarise(count = n()) %>%  #  counts the number of occurances of each COMPLEX 
	mutate(prop = count / sum(count)) %>%  # adds variableto calculate the frequency of each COMPLEX
	select(COMPLEX, prop)  # selects for variables COMPLEX and prop
}

# 5 merge and melt data by COMPLEX
merged_prop_filtered_event <-  full_join(x, y, by = "COMPLEX")  # joins the two tables to form one master table containing the proportion of each event for the two KDs
melt_prop <- melt(merged_prop_filtered_event)  # melts into one variable, value, and adds variable titled variable

# 6 graphing the proportion data
ggplot(data = melt_prop, mapping = aes(x = COMPLEX, y = value, fill = variable)) +  # calls data from melt_prop, x axis = event, y axis = value, KD conditions are distinguished by fill
	geom_col(position = "dodge") +  # bars are placed side-to-side
	labs(x = "Event") +  # label for x axis
	labs(y = "Proportion")  # label for y axis


