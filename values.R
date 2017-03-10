MAX_LEVEL = eventReactive(input$update,{
  req(input$max_level)
  input$max_level
})

MAX_P = eventReactive(input$update,{
  req(input$max_p)
  input$max_p
})


#advanced settings
ignoreNA = reactive({
  if(input$treatNA == "Exclude in analysis") {1} else {0}
})


ATTR_CLUST_H =reactive({
  input$attr_clust_h
})

PARA_CLUST_H =reactive({
  input$para_clust_h
})

ATTR_MANY_LEVELS_CUTOFF = reactive({
  input$attr_many_levels_cutoff
})

NUM_CLUST_P = reactive({
  5
})