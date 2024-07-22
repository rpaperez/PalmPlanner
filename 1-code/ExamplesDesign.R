source('./1-code/helpers_App_Design.R')

dist_inter=6
dist_intra=3
dist_intercrop=5
lim=60
orientation='NS'
pointSize=3

plot_design(dist_intra = dist_intra,dist_inter = dist_inter,dist_intercrop = dist_intercrop,designType = 'square3',orientation = orientation,pointSize = pointSize)

a$plot
nrow(a$design)/(unique(a$design$xmax)/100*unique(a$design$ymax)/100)

                             