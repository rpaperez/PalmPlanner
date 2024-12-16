---
title:'PalmPlanner: "A R-Shiny application for designing and visualizing spatial arrangement
  of oil palm-based agroforestry systems'"
tags:
- R
- R Shiny
- agroforestry
- planting design
- Elaeis guineensis
authors:
- name: "Raphaël P-A Perez"
  orcid: "0000-0001-5270-9212"
  affiliation: 1, 2
- name: Sylvain Rafflegeau
  orcid: "0000-0001-5267-1189"
  affiliation: 3, 4
- name: Axel Labeyrie
  orcid: null
  affiliation: 5
- name: Laurène Feintrenie
  orcid: "0000-0003-1621-396X"
  affiliation: 6, 7
output:
  word_document: default
  html_document:
    df_print: paged
  pdf_document: default
year: 2024
bibliography: paper.bib
affiliations:
- name: CIRAD, UMR AGAP Institut, F34398 Montpellier, France
  index: 1
- name: UMR AGAP Institut, Univ Montpellier, CIRAD, INRAE, Institut Agro, Montpellier,
    France
  index: 2
- name: CIRAD, UMR Innovation, 34398 Montpellier, France
  index: 3
- name: INNOVATION, Univ. Montpellier, CIRAD, INRAE, Institut Agro, Montpellier, France
  index: 4
- name: Afilliation Axel
  index: 5
- name: TETIS
  index: 6
- name: ECOSUR
  index: 7
---

# Summary

This paper presents a Shiny application to design and visualize intercropping systems based on oil palm cultivation. The application features an interactive user interface that enables users to select various design patterns for oil palm, manage the number of rows to be removed for intercropping, and adjust the spacing between plants. Users can position intercrops or trees in lines or distribute them sparsely, either between or within the rows of oil palm. The primary objective is to streamline the planting process by providing access to the spatial arrangement of plants, estimating final density, and preventing the planting of new plants in areas previously occupied by oil palms during replanting.

# Statement of need

This application was developed as a tool for assisting farmers and researchers in designing innovative agroforestry systems in the context of the Optipalmex project [@Optipalmex], which aimed at supporting the creation of a network of oil palm plantation in Mexico. The application enables users to rapidly visualize different configurations of planting systems and assist them to better harness potential difficulties during the planting process. The output map can be used to estimate the coordinates of each plant relatively to a reference tree, assess the number of plants required for the planting,  prevent the co-location of new plants in areas where old palms have been removed for replanting, and estimate the final density of each species. This application can also serve as a support tool for previous studies which investigate the representation of diversified ecosystems by introducing the concept of Ecosystem Services functional Spatial Unit [@rafflegeau2023essu].
Furthermore, future enhancements could include the exportation of the map with the typology of each species, as an input for more complex simulation models that simulate light on 3D plants [@perez2022architectural].


# App Features

The application is built using R and the Shiny framework [@shiny], incorporating several packages to enhance functionality and user experience. The shiny app load functions coded in the script helpers_App_Design.R. These functions allow the creation of a design with the (x,y) coordinates of plants based on input arguments given by the user interface. The user interface is designed using a navbarPage layout, featuring multiple tabs that allow users to input arguments for oil palm and intercrop designs. Each tab contains input fields for each species such as:

- Selection of design patterns (e.g., square or quincunx).
- Specification of distances between plants (both inter and intra-row).
- Customization of visual elements, including point sizes, colors and name of intercrops.

The interactive features allow to integrate until five species (crops or associated trees). Three tabs permit to design species between oil palms rows (intercrop), one tab for species within oil palm rows (crop in palm row), and a last tab for species to replace specific palms identified in the design (see Figure 1). The selection of specific palms can be done directly through interactive selection of the plots by using click or the lasso/box tool provide by the renderPlotly function. 
In a similar way every point can be selected from the plot. The selected points are listed into a table, from which a deletion procedure can be applied when needed for specific designs.

All specific interactive procedures are detailed in the README.md provided on the github repository.

![Figure 1: Visualization of a design with the PalmPlanner app. \label{fig:figure1}](images/screenApp.png)


# Acknowledgements

We acknowledge the contributions of the R community and the developers of the Shiny framework for providing the tools necessary to create this application. This work was support by the Optipalmex project funded by FASEP/PalmElit

# References
