 

try({ 
  library(shiny) 
  library(DT)
  setwd("~/Desktop/Tensor_myeloid")
})
 

 


paperTitle <- "'Tensor Decomposition for Induced Monocyte and Macrophage Gene Expression Identifies Neurodegenerative Disease-specific Trans-eQTLs'"
abstract <- "<br>Recent human genetic studies suggest that cells of the innate immune system have a primary role in the pathogenesis of neurodegenerative diseases. However, the results from these studies often do not elucidate how the genetic variants affect the biology of these cells to modulate disease risk. Here, we applied a tensor decomposition method to uncover disease-associated gene networks linked to distal genetic variation in stimulated human monocytes and macrophages gene expression profiles. We report robust evidence that some disease-associated genetic variants affect the expression of multiple genes in trans. These include a Parkinson’s disease locus influencing the expression of genes mediated by a protease that controls lysosomal function, and Alzheimer’s disease loci influencing the expression of genes involved in type 1 interferon signaling, myeloid phagocytosis, and complement cascade pathways. Overall, we uncover gene networks in induced innate immune cells linked to disease-associated genetic variants, which may help elucidate the underlying biology of disease." 
authors <- HTML("<h4>Authors</h4>Satesh Ramdhani, Elisa Navarro, Evan Udine, Madison Parks, and Towfique Raj* <br>
<h4>Affiliations</h4>Ronald M. Loeb Center for Alzheimers Disease,<br>Department of Neuroscience and Friedman Brain Institute,<br>Department of Genetics and Genomic Sciences,<br>Icahn School of Medicine at Mount Sinai, New York, New York, USA.<br>
<h4>*Corresponding author</h4> towfique.raj@mssm.edu (T.R.)")
fig1Path <- tags$iframe(src="figures/fig1.pdf",
                        height="600px", width="100%", scrolling="no", seamless=NA) 

ui <- fluidPage( 
  theme = "css/main.css",
  titlePanel("Tensor Myeloid Supplemental Material"),
  em(paperTitle),br(),br(),
  a(href="http://labs.neuroscience.mssm.edu/project/raj-lab/", target="_blank", img(id="sinai",src="logos/sinai.png")),
  a(id="github", href="https://github.com/RajLabMSSM/Tensor_myeloid", target="_blank", img(src="logos/github.png"), "GithHub Repository"),
  br(),br(),br(),
 
  
  sidebarLayout(
    sidebarPanel(
        conditionalPanel("input.tabs == 'Abstract'", authors
        ),
        conditionalPanel("input.tabs == 'Table 2'",
                         HTML("<h3>Table 2</h3><br>Gene Ontology (GO) categories enrichment for the sparse components in FF and CG using R package topGo.")
        ),
        conditionalPanel("input.tabs == 'Table 3'",
                         HTML("<h3>Table 3</h3><br>Sparse component that were enriched for genes in disease-associated GWAS loci.")
        ),
        conditionalPanel("input.tabs == 'Table 4'",
                         HTML("<h3>Table 4</h3><br>Trans-eQTLs detected in Fairfax data (FF) at FDR < 0.15. <br><em>Note: </em>The trans-eSNPs are not LD-pruned.")
        ),
        conditionalPanel("input.tabs == 'Table 5'",
                         HTML("<h3>Table 5</h3><br>Trans-eQTLs detected in Cardiogenics data (CG) at FDR < 0.15. <br><em>Note: </em>The trans-eSNPs are not LD-pruned.")
        ),
        conditionalPanel("input.tabs == 'Table 6'",
                         HTML("<h3>Table 6</h3><br>Trans-eSNPs detected in FF dataset at FDR < 0.15 that colocalize with disease or trait-associated GWAS SNPs. The table lists the component number, tissue scores, gene name and scores, and cis-gene (if any)."),
                         helpText(br(),em("Click a Component in the first column of the table to display the genes within that component.")),
                         uiOutput("FFplot")
                         
        ),
        conditionalPanel("input.tabs == 'Table 7'",
                         HTML("<h3>Table 7</h3><br>Trans-eSNPs detected in CG dataset at FDR < 0.15 that colocalize with disease or trait-associated GWAS SNPs. The table lists the component number, tissue scores, gene name and scores, and cis-gene (if any).") 
        ),
        conditionalPanel("input.tabs == 'Table XX'",
                         HTML("<h3>Table XX</h3><br>SNP-based heritability enrichment for each component. Proportion of heritability for 18 selected complex traits that can be attributed to each sparse component from the FF data.")
        )
        
       
    ),
    mainPanel(
      tabsetPanel(
        id = 'tabs',
        tabPanel("Abstract",HTML(abstract), br(),br(), fig1Path),
        
        tabPanel("Table 2", DT::dataTableOutput("t2")),
        tabPanel("Table 3", DT::dataTableOutput("t3")),
        tabPanel("Table 4", DT::dataTableOutput("t4")),
        tabPanel("Table 5", DT::dataTableOutput("t5")),
        tabPanel("Table 6", DT::dataTableOutput("t6")),
        tabPanel("Table 7", DT::dataTableOutput("t7")),
        tabPanel("Table 8", DT::dataTableOutput("t24")),
        tabPanel("Table 9", DT::dataTableOutput("t29")),
        tabPanel("Table XX", DT::dataTableOutput("tXX"))
      )
    )
  )
)

server <- function(input, output, session) {  
  
  opts <- list( scrollY = 500, sScrollX="100%", bScrollCollapse=TRUE,  autoWidth=TRUE, pageLength=50,
                dom = 'frtipB', buttons = c('csv', 'excel', 'pdf', 'print', 'copy'), paging=FALSE)
  
  addTable <- function(tabNum){
    print(paste("Creating Table",toString(tabNum)))
    file <- read.delim(paste("supp_tables/SUPPLEMENTARY_TABLE_",toString(tabNum),".txt", sep=""), fill=NA, header=T, stringsAsFactors=F)
    if(tabNum %in% c(6,7)){ 
      file <- subset(file, select=c("COMP","SNP","PVAL","Disease", "FDR"))
      colnames(file) <- c("Component","SNP","P-value", "Disease", "FDR")
      ## FF component plots
      if(tabNum==6){
       # reactive_t6 <- react <- reactive({unique(file)})   
      }
      ## FF component plots
      
    }
    #else{react <- unique(file)}
    file <- unique(file)
    
    table <- DT::renderDT({
      DT::datatable(file, options=opts, filter='top', rownames=F, 
                    class='cell-border stripe', selection='single', extensions=c('Buttons','Scroller'))
    })
     
    return(table)
  }
  
  output$t2 <- addTable(2)
  output$t3 <- addTable(3)
  output$t4 <- addTable(4)
  output$t5 <- addTable(5) 
  output$t6 <- addTable(6)
  output$t7 <- addTable(7) 
  output$t24 <- addTable(24) 
  output$t29 <- addTable(29) 
  output$tXX <- addTable("XX") 
 
  ## Old
  #Load Component Genes plots 
  output$FFplot <- renderUI({
    rowNum = input$t6_rows_selected
    info = input$t6_cell_clicked
   
    if(length(info) & length(rowNum)){
      component <-  info$value
      colNum <- info[2]
      if(colNum==0){ 
        pdfPath <- paste("CG_Components/cardio_Component_",toString(component),".pdf",sep="")#normalizePath(file.path('./CG_Components', 'cardio_Component_1.pdf'))
       
        helpText(br(), paste("Genes within Component #: ", toString(component)),
                 br(),# "Row,Col=",info[1],colNum,  
                 tags$iframe(src=pdfPath, height="300px", width="100%", scrolling="no", seamless=NA))
      }
    }
  })# end renderUI



  
  }

shinyApp(ui, server)



# 
# 
# # Parse Table 2 correctly
# fixTable2 <- function(){
#   file <- read.delim(paste("supp_tables/SUPPLEMENTARY_TABLE_",toString(2),".txt", sep=""), fill=NA, header=T, stringsAsFactors=F)
#   ## Col 2
#   col2 <-file[,2] 
#   substrRight <- function(x, n){
#     substr(x, nchar(x)-n+1, nchar(x))
#   } 
#   GO.ID <- substr(col2, 1, 11)
#   Term <- substr(col2, 12, 11+43) 
#   SPLIT2 <- strsplit(substrRight(col2, 18), " ") 
#   # Remove all "", then get first and second item from each list, respectively
#   getElem <- function(x, n){
#     return( x[x != ""][n])
#   } 
#   Annotated <- unlist(lapply(SPLIT2, getElem, 1))
#   Significant  <- unlist(lapply(SPLIT2, getElem, 2))
#   
#   ## Col 3
#   col3 <-file[,3]
#   SPLIT3 <- strsplit(col3, " ") 
#   
#   Expected <- unlist(lapply(SPLIT3, getElem, 1))
#   Rank.in.classicFisher <- unlist(lapply(SPLIT3, getElem, 2))
#   classicFisher <- unlist(lapply(SPLIT3, getElem, 3))
#   elimFisher <- unlist(lapply(SPLIT3, getElem, 4))
#   topgoFisher <- unlist(lapply(SPLIT3, getElem, 5)) 
#   
#   newTable2 <- data.frame(Component=file[,1], GO.ID=GO.ID, Term=Term,
#                           Annotated=Annotated, Significant=Significant, Expected=Expected,
#                           Rank.in.classicFisher=Rank.in.classicFisher, 
#                           classicFisher=classicFisher, elimFisher=elimFisher, topgoFisher=topgoFisher)
#   write.table(newTable2, "supp_tables/SUPPLEMENTARY_TABLE_2.txt", quote = F, sep="\t")
# }

