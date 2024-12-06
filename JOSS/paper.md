
title: 'PalmPlanner: A R-Shiny application for designing and visualizing spatial arrangement of oil palm-based agroforestry systems'
tags:
  - R
  - shiny
  - agroforestry
  - planting design
  - oil palm
authors:
  - name: Raphaël P-A Perez
    orcid: 0000-0001-5270-9212
    affiliation: "1, 2"
  - name: Sylvain Rafflegeau
    orcid: 0000-0001-5267-1189
    affiliation: "3, 4"
  - name: Axel Labeyrie
    orcid: 
    affiliation: 5
  - name: Laurène Feintrenie
    orcid: 0000-0003-1621-396X
    affiliation: "6, 7"
affiliations:
 - name: CIRAD, UMR AGAP Institut, F-34398 Montpellier, France
   index: 1
 - name: UMR AGAP Institut, Univ Montpellier, CIRAD, INRAE, Institut Agro, Montpellier, France
   index: 2
 - name: CIRAD, UMR Innovation, 34398 Montpellier, France 
   index: 3
- name:  INNOVATION, Univ. Montpellier, CIRAD, INRAE, Institut Agro, Montpellier, France  
   index: 4
- name:  Afilliation Axel  
   index: 5 
- name:  afil. TETIS
   index: 6
- name:  afil. ECOSUR
   index: 7
   
year: 2024
bibliography: paper.bib


# Summary

This paper presents a Shiny application to design and visualize intercropping systems based on oil palm cultivation. The application features an interactive user interface that enables users to select various design patterns for oil palm, manage the number of rows to be removed for intercropping, and adjust the spacing between plants. Users can position intercrops or trees in lines or distribute them sparsely, either between or within the rows of oil palm. The primary objective is to streamline the planting process by providing access to the spatial arrangement of plants, estimating final density, and preventing the planting of new plants in areas previously occupied by oil palms during replanting.

# Statement of need

This application was developed as a tool for assisting farmers in designing innovative agroforestry systems in the context of the Optipalmex project (add ref), which aimed at supporting the creation of a network of oil palm plantation in Mexico. 
 The application enables users to rapidly visualize different configurations of planting systems and assist them to better harness potential difficulties during the planting process. The output map can be used to estimate the coordinates of each plant relatively to a reference tree, assess the number of plants required for the planting and estimate the final density of each species. 


# App Features

The application is built using R and the Shiny framework, incorporating several packages to enhance functionality and user experience. The user interface is designed using a navbarPage layout, featuring multiple tabs that allow users to input parameters for oil palm and intercrop designs. Each tab contains input fields for each species such as:
- Selection of design patterns (e.g., square or quincunx).
- Specification of distances between plants (both inter and intra-row).
- Customization of visual elements, including point sizes, colors and name of intercrops.

The interactive features allow to integrate until five species (crops or associated trees). Three tabs permit to design species between oil palms rows (intercrop), one tab for species within oil palm rows (crop in palm row), and a last tab for species to replace specific palms identified in the design (Figure 1). The selection of specific palms can be done directly through interactive selection of the plots by using click or the lasso/box tool provide by the renderPlotly function. 
In a similar way every point can be selected from the plot. The selected points are listed into a table, from which a deletion procedure can be applied when needed for specific designs.

All specific interactive procedures are detailed in the README.md provided on the github repository.

# Use-case

![Figure 1](images/screenApp.png)

# Perspective

This Shiny application serves as a tool for researchers, agronomists, and farmers to explore various intercropping designs for oil palm. The application serves as a valuable tool for rapidly visualizing designs and potential conflict in the planting procedure. Future enhancements could include the exportation of the map with the typology of each species, as an input for more complex simulation models that simulate light on 3D plants (ref).


# Acknowledgements

We acknowledge the contributions of the R community and the developers of the Shiny framework for providing the tools necessary to create this application. This work was support by the Optipalmex project funded by FASEP/PalmElit

# References
