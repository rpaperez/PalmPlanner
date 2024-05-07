source('./1-code/helpers_App_Design.R')

dist_inter=6
dist_intra=3
lim=60
orientation='NS'
twist=0
pointSize=3

gr1=plot_design(dist_intra =dist_intra,dist_inter =dist_inter,designType ='quincunx',dist_intercrop =dist_intercrop,orientation = orientation,twist = twist,pointSize =pointSize,lim = lim)$plot+ggtitle('quincunx')

gr2=plot_design(dist_intra =dist_intra,dist_inter =dist_inter,designType ='square',dist_intercrop =dist_intercrop,orientation = orientation,twist = twist,pointSize =pointSize,lim = lim)$plot+ggtitle('square')

gr3=plot_design(dist_intra =dist_intra,dist_inter =dist_inter,designType ='quincunx2',dist_intercrop =10,orientation = orientation,twist = twist,pointSize =pointSize,lim = lim)$plot+ggtitle('quincunx2')

gr4=plot_design(dist_intra =dist_intra,dist_inter =dist_inter,designType ='square2',dist_intercrop =10,orientation = orientation,twist = twist,pointSize =pointSize,lim = lim)$plot+ggtitle('square2')

gr5=plot_design(dist_intra =dist_intra,dist_inter =dist_inter,designType ='quincunx3',dist_intercrop =10,orientation = orientation,twist = twist,pointSize =pointSize,lim=lim )$plot+ggtitle('quincunx3')

gr6=plot_design(dist_intra =dist_intra,dist_inter =dist_inter,designType ='quincunx4',dist_intercrop =10,orientation = orientation,twist = twist,pointSize =pointSize,lim=lim )$plot+ggtitle('quincunx4')

gr7=plot_design(dist_intra =dist_intra,dist_inter =dist_inter,designType ='quincunx5',dist_intercrop =10,orientation = orientation,twist = twist,pointSize =pointSize,lim=lim )$plot+ggtitle('quincunx5')

plot_grid(gr1,gr2,gr3,gr4,gr5,gr6)

ggsave(filename = '2-outputs/examples.png',width = 15,height = 12)
