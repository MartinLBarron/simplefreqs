# library(readxl)         # for reading in Excel data
# library(dplyr)          # for data manipulation
# library(tidyr)          # for data shaping
# library(ggplot2)        # for generating the visualizations
# 
# 
# # Get example data -------------------------------------------------------
# setwd("~/Documents/ggplot Examples/DotPlot Example")
# supermarket <- read_excel("Supermarket Transactions.xlsx", sheet = "Data")
# 
# head(supermarket)
# 
# # Sumarize data -------------------------------------------------------
# city_rev <- supermarket %>%
#   group_by(City) %>%
#   summarise(Revenue = sum(Revenue, na.rm = TRUE)) %>%
#   arrange(Revenue) %>%
#   mutate(City = factor(City, levels = .$City))
# 
# city_gender_rev <- supermarket %>%
#   group_by(City, Gender) %>%
#   summarise(Revenue = sum(Revenue, na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(City = factor(City, levels = city_rev$City))
# 
# 
# # As a bar chart -------------------------------------------------------
# 
# ggplot(city_rev, aes(City, Revenue)) +
#   geom_bar(stat = "identity") +
#   coord_flip()
# 
# 
# ggplot(city_gender_rev, aes(City, Revenue, fill = Gender)) +
#   geom_bar(stat = "identity") +
#   coord_flip()
# 
# ggplot(city_gender_rev, aes(City, Revenue, fill = Gender)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   coord_flip()
# 
# ggplot(city_gender_rev, aes(City, Revenue, fill = Gender)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   coord_flip() +
#   facet_wrap(~ Gender)
# 
# 
# # as a basic dot plot 
# ggplot(city_rev, aes(Revenue, City)) +
#   geom_point()
# 
# #with a second variable
# ggplot(city_gender_rev, aes(Revenue, City)) +
#   geom_point(aes(color = Gender))
# 
# #We can add a line between gender
# ggplot(city_gender_rev, aes(Revenue, City)) +
#   geom_line(aes(group = City)) +
#   geom_point(aes(color = Gender))
# 
# #We can add labels but they overlap
# ggplot(city_gender_rev, aes(Revenue, City, label = round(Revenue, 0))) +
#   geom_line(aes(group = City)) +
#   geom_point(aes(color = Gender)) +
#   geom_text(aes(color = Gender), size = 3)
# 
# # We can  make the lab els prettyier
# 
# right_label <- city_gender_rev %>%
#   group_by(City) %>%
#   arrange(desc(Revenue)) %>%
#   top_n(1)
# 
# left_label <- city_gender_rev %>%
#   group_by(City) %>%
#   arrange(desc(Revenue)) %>%
#   slice(2)
# 
# ggplot(city_gender_rev, aes(Revenue, City)) +
#   geom_line(aes(group = City)) +
#   geom_point(aes(color = Gender), size = 1.5) +
#   geom_text(data = right_label, aes(color = Gender, label = round(Revenue, 0)),
#             size = 3, hjust = -.5) +
#   geom_text(data = left_label, aes(color = Gender, label = round(Revenue, 0)),
#             size = 3, hjust = 1.5) +
#   scale_x_continuous(limits = c(-500, 10500))
# 
# 
# #We can emphasize particular ------------------------------------------------
# 
# # create data frame that identifies revenue differences over 20%
# big_diff <- city_gender_rev %>% 
#   spread(Gender, Revenue) %>% 
#   group_by(City) %>% 
#   mutate(Max = max(F, M),
#          Min = min(F, M),
#          Diff = Max / Min - 1) %>% 
#   arrange(desc(Diff)) %>%
#   filter(Diff > .2)
# 
# # filter the label data frames to only include those cities where the
# # difference exceeds 20%
# right_label <- filter(right_label, City %in% big_diff$City)
# left_label <- filter(left_label, City %in% big_diff$City)
# 
# # filter the main data frame to only include those cities where the 
# # difference exceeds 20%.
# highlight <- filter(city_gender_rev, City %in% big_diff$City)
# 
# ggplot(city_gender_rev, aes(Revenue, City)) +
#   geom_line(aes(group = City), alpha = .3) +
#   geom_point(aes(color = Gender), size = 1.5, alpha = .3) +
#   geom_line(data = highlight, aes(group = City)) +
#   geom_point(data = highlight, aes(color = Gender), size = 2) +
#   geom_text(data = right_label, aes(color = Gender, label = round(Revenue, 0)),
#             size = 3, hjust = -.5) +
#   geom_text(data = left_label, aes(color = Gender, label = round(Revenue, 0)),
#             size = 3, hjust = 1.5) +
#   scale_x_continuous(limits = c(-500, 10500))
# 
# 
# 
# 
# # Cleaning the code -------------------------------------------------------
# # create a new label data frame
# plot_label <- big_diff %>%
#   select(City, Revenue = Max, Diff) %>%
#   right_join(right_label)
# 
# p <- ggplot(city_gender_rev, aes(Revenue, City)) +
#   geom_line(aes(group = City), alpha = .3) +
#   geom_point(aes(color = Gender), size = 1.5, alpha = .3) +
#   geom_line(data = highlight, aes(group = City)) +
#   geom_point(data = highlight, aes(color = Gender), size = 2) +
#   geom_text(data = plot_label, aes(color = Gender, 
#                                    label = paste0("+", scales::percent(round(Diff, 2)))),
#             size = 3, hjust = -.5)
# 
# p
# 
# p + scale_color_discrete(labels = c("Female", "Male")) +
#   scale_x_continuous(labels = scales::dollar, expand = c(0.02, 0), 
#                      limits = c(0, 10500),
#                      breaks = seq(0, 10000, by = 2500)) +
#   scale_y_discrete(expand = c(.02, 0)) +
#   labs(title = "Total Revenue by City and Gender",
#        subtitle = "Out of 23 cities, eight locations experience a 20% or greater difference \nin revenue generated by males versus females. Hidalgo experiences the \ngreatest difference with females generating 86% more revenue than males.") +
#   theme_minimal() +
#   theme(axis.title = element_blank(),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor = element_blank(),
#         legend.title = element_blank(),
#         legend.justification = c(0, 1), 
#         legend.position = c(.1, 1.075),
#         legend.background = element_blank(),
#         legend.direction="horizontal",
#         text = element_text(family = "Georgia"),
#         plot.title = element_text(size = 20, margin = margin(b = 10)),
#         plot.subtitle = element_text(size = 10, color = "darkslategrey", margin = margin(b = 25)),
#         plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0))
# 
