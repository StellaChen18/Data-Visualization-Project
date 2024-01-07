# Restaurants near me - Uber Eats
This is an app for the FIT5147 DVP. 

The creator of this app is **Yunzhi Chen**.

The data used for this PROJECT is the `Restaurant and Uber data` that are in-built in R. 

The intended target audience for communicating this key information will be Uber Eats users, who will have a new perspective on U.S. restaurant data by communicating key information about nearby restaurants, choosing restaurants more informative and mutually beneficial for both restaurants and customers, thus contributing to the economic growth of the restaurant industry and even the take-out industry. The integration of restaurant data also makes it easier for people to understand the overall information of the nation's gourmet restaurants and provides a more comprehensive picture for data analysts.

## Introduction
In 2014, Uber introduced Uber Eats, an online food ordering and delivery platform accessible via a mobile app or web browser. Users can explore menus, check restaurant locations and ratings, and conveniently order and pay for their favorite foods with a simple tap.

This report highlights the outcomes of the Data Exploration Project, utilizing R for data wrangling and R Shiny for visualization. The focus is on presenting the distribution of popular restaurants by price type and their geographical locations on the U.S. map. The visual representation includes the flow route of people's spending on food, providing a dynamic perspective.

The primary audience for this information is Uber Eats users. By offering insights into nearby restaurants, this data enhances the decision-making process for users and fosters a mutually beneficial relationship between restaurants and customers. This, in turn, contributes to the growth of the restaurant and takeout industry. The integrated restaurant data also aids data analysts in gaining a comprehensive understanding of the nation's gourmet landscape.

## Implementation
In this project, I utilized R Shiny for chart interaction and visualization, employing libraries such as "shinythemes," "tidyverse," "lubridate," "plotly," "leaflet," "maps," "geosphere," and "RColorBrewer." The "shinythemes" library was used to modify the app's basic theme, while "lubridate" and "maps" assisted in data wrangling and cleaning. The remaining libraries, including "plotly," "leaflet," "geosphere," and "RColorBrewer," contributed to the interactivity and visualization of the Shiny app.

To enhance visualization based on the 5 design sheets, notable changes were made. In the first part concerning the price situation of popular restaurants, I employed the 'facet_wrap' function to segregate the bar chart by price type, allowing better identification of smaller data. Exact values for each bar were added for clarity, especially for color-blind users. A summarized text was included to conclude the distribution of price types for well-reviewed restaurants.

In the section on the distribution of popular restaurants, I added the price type to the marker for enhanced information visibility. Users can now click on markers to access detailed restaurant information. For the traveling flow for delicious foods section, I changed the interactive section from a select drop-down menu to a checklist box, providing a more dynamic user experience.

A notable challenge involved processing Uber data for the flow map represented by a leaflet. This required significant upfront effort to add latitude and longitude for starting and ending cities, integrating the data for effective flow visualization.
