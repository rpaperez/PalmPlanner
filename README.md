R.PEREZ (June 2024)

# Designing Oil Palm Based Intercropping Systems

 
-   [Description](#description)
-   [Installation](#install)
-   [Usage](#usage)


## Description {#description}

This Shiny app allows users to represent intercropping designs through different oil palm-based intercropping systems. Users can select various design patterns, adjusting inter row and intra row distances, intercropping distances, and customize colors and names for visualization. Up to three intercrops can be represented within a given oil palm design. The plot shows the density of each crop based on the selected design.

## Installation {#install}
To run this app, ensure you have installed R and R studio software (https://posit.co/).
Then open the file Optipalmex_R.Rproj with RStudio. This will help R locate all the necessary data on your computer to run the code.

Once RStudio is open, launch the file App_design.R. The code will appear in the RStudio console; click on the "Run App" button in the top right corner of the console window.

The following packages will be automatically installed:

    shiny
    shinythemes
    shinycssloaders
    lubridate
    stringr
    tidyverse
    viridis
    plotly
    devtools

Additionally, the colourpicker package is required and will be installed from GitHub. You may need to launch the application several times to install all the packages.

## Usage {#usage}

On the right, you'll find 5 tab sets where you can modify the parameters for each crop. The first tab, ``PALMS`` contains arguments for designing the oil palm planting pattern. The other tabs, ``INTERCROP1``, ``INTERCROP2`` and ``INTERCROP3`` are for the intercrops and the last tab ``REPLANTING`` is for representing an old planting.

### Oil palm arguments

In the ``PALMS`` tab, you can modify the following arguments:


``Select design pattern``: type of design, quincunx (tresbolio) or square (cuadrado) 

``Number of lines removed for intercropping``: Number of oil palm rows removed from a complete design. ``1/ 3`` indicates one line removed every three rows. Hence the intercrops will be in located every two rows of oil palms.

``Inter palm row distance (m)``: distance between rows of palms

``Intra palm row distance (m)``: distance within rows of oil palm

``Intercropping distance (m)``: can only be modified when rows of oil palms were removed. Default value is twice the inter row distance

``Select point size`` : size of point in the plot

``Select colour`` : colour of  point in the plot.

### Intercrops arguments

In each ``INTERCROP`` tab, you can modify the following arguments:

``Enter the name of the intercrop``: Enter the name of the intercrop, that will update names in plot legend.

``Select design pattern``: type of design, quincunx (tresbolio) or square (cuadrado) 

``Number of lines``: Number of intercrop rows within the intercropping area. Be carefull to adapted the number of lines and the inter row distances to not exceeding the intercrop area.

``Inter row distance (m)``: distance between rows of the intercrop

``Intra row distance (m)``: distance within rows of the intercrop

``y offset (m)``: distance to adapt horizontal alignment between oil palms and intercrops. The value is negative to change intercrops point down in the plot.

``Select point size`` : size of point in the plot

``Select colour`` : colour of  point in the plot.

### Replanting  arguments

In the ``REPLANTING`` tab, you can modify the following arguments:

``visualize old palms``: check box for plotting the old palm positions (default is unchecked box)

``Select design pattern``: type of design, quincunx (tresbolio) or square (cuadrado) 

``Inter palm row distance (m)``: distance between rows of palms

``Intra palm row distance (m)``: distance within rows of oil palm

``x offset (m)``: distance to adapt vertical alignment between old oil palms and replanted palms. The value is negative to change intercrops point down in the plot.

``y offset (m)``: distance to adapt horizontal alignment between old oil palms and replanted palms. The value is negative to change intercrops point down in the plot.

``Select point size`` : size of point in the plot



### Plot arguments

Additional plot arguments include:

``plot limits (m)``: limits of the plot (x and y axis)

``origin``: set the origin of the plot to the first palm (default)

``Visalize the design``: button to update the design based on the entered arguments.

``DELETED SELECTED POINTS``: button to delete the points selected when clicking on the plot, or selected with the lasso tool. The selected points are listed in the ``SELECTION`` tab. To unselect points, double click and/or use the lasso on empty region of the plot.

``RESET DATA``: button to return to the original plot before removing points.


