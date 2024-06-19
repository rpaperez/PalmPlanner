test2=test2 %>% 
  mutate(codeP=paste(x,y,'A'))

test2%>% 
  mutate(overlap=ifelse(codeP %in% unique(test2$code) & name!='A',T,F))
