 

try({ 
  library(shiny) 
  library(DT)
  library(stringr)
  library(dplyr)
  library(tidyr)
  setwd("~/Desktop/Tensor_myeloid")
})


## Old to New Table Names
# StAble 0 -> 1
# Stable 1 ->2
# Stable XXX >3 (NEW)
# Stable 2 >4
# Stable 3 >5
# Stable XX ->6
# Stable 4-> 7
# Stable 5-> 8
# Stable 6-> 9
# Stable 7-> 10
# DELETE Current Table 8 &9


paperTitle <- "'Tensor Decomposition for Stimulated Monocyte and Macrophage Gene Expression Identifies Neurodegenerative Disease-specific Trans-eQTLs'"
abstract <- "<br>Recent human genetic studies suggest that cells of the innate immune system have a primary role in the pathogenesis of neurodegenerative diseases. However, the results from these studies often do not elucidate how the genetic variants affect the biology of these cells to modulate disease risk. Here, we applied a tensor decomposition method to uncover disease-associated gene networks linked to distal genetic variation in stimulated human monocytes and macrophages gene expression profiles. We report robust evidence that some disease-associated genetic variants affect the expression of multiple genes in trans. These include a Parkinson’s disease locus influencing the expression of genes mediated by a protease that controls lysosomal function, and Alzheimer’s disease loci influencing the expression of genes involved in type 1 interferon signaling, myeloid phagocytosis, and complement cascade pathways. Overall, we uncover gene networks in induced innate immune cells linked to disease-associated genetic variants, which may help elucidate the underlying biology of disease." 
authors <- HTML("<h4>Authors</h4>
Satesh Ramdhani<sup>1-3</sup>, Elisa Navarro<sup>1-3</sup>, Evan Udine<sup>1-3</sup>, Anastasia G. Efthymiou<sup>1-3</sup>, Brian M. Schilder<sup>1-3</sup>, Madison Parks<sup>1-3</sup>, Alison Goate<sup>1-3</sup>, and Towfique Raj<sup>1-3</sup>*
<br>
<h4>Affiliations</h4>
<sup>1</sup>Ronald M. Loeb Center for Alzheimer’s Disease<br>
<sup>2</sup>Nash Family Department of Neuroscience and Friedman Brain Institute<br>
<sup>3</sup>Department of Genetics and Genomic Sciences<br><br>
Icahn School of Medicine at Mount Sinai<br>New York, New York, USA
<br>
<h4>Corresponding author *</h4>towfique.raj@mssm.edu") 
fig1Path <- tags$img(src="figures/fig1.png",height="400px")

ui <- fluidPage( 
  theme = "css/main.css",
  titlePanel("Tensor Myeloid Supplemental Material"),
  h4(em(paperTitle)), br(),
  a(href="http://labs.neuroscience.mssm.edu/project/raj-lab/", target="_blank", img(id="sinai",src="logos/sinai.png")),
  a(id="github", href="https://github.com/RajLabMSSM/Tensor_myeloid", target="_blank", img(src="logos/github.png"), "GithHub Repository"),
  br(),br(),br(),
 
  
  sidebarLayout(
    sidebarPanel(
        conditionalPanel("input.tabs == 'Abstract'", authors
        ),
        conditionalPanel("input.tabs == 'Table 1'", # Originally t0
                         HTML("<h3>Table 1</h3><br>List of sparse components with corresponding gene scores and stimuli (IFN and LPS) activity scores identied in the FF dataset."),
                         helpText(br(),em("Click any cell in the first column of the table to display the genes within that component.")),
                         uiOutput("FFplot_t0")
        ),
        conditionalPanel("input.tabs == 'Table 2'", # Originally t1
                         HTML("<h3>Table 2</h3><br>List of sparse components with corresponding gene scores and tissue (monocytes and macrophages) activity scores identied in the CG dataset."),
                         helpText(br(),em("Click any cell in the first column of the table to display the genes within that component.")),
                         uiOutput("CGplot_t1") 
        ), 
        conditionalPanel("input.tabs == 'Table 3'", # Originally tXXX
                         HTML("<h3>Table 3</h3><br>Components that replicate across the three datasets: Fairfax(FF), Cardiogenics(CG) and ImmVar(IMM). Two-way reverse correlation with the gene scores was conducted on the common intersected genes from the respective components.")
        ), 
        conditionalPanel("input.tabs == 'Table 4'",# Originally t2
                         HTML("<h3>Table 4</h3><br>Enrichment of Gene Ontology (GO) categories among the genes in the sparse components.")
        ),
        conditionalPanel("input.tabs == 'Table 5'", # Originally t3
                         HTML("<h3>Table 5</h3><br>Sparse components that are enriched for genes within disease-associated loci. The component number, P-value, and corresponding GWAS disease or traits are listed.")   
        ),
        conditionalPanel("input.tabs == 'Table 6'", # Originally tXX
                         HTML("<h3>Table 6</h3><br>SNP-based heritability enrichment for each component. Proportion of heritability and enrichment statistics for 18 selected complex traits that can be attributed to each sparse component from the FF data.")
        ),
        conditionalPanel("input.tabs == 'Table 7'", # Originally t4
                         HTML("<h3>Table 7</h3><br>Trans-eQTLs detected in Fairfax data (FF) at FDR < 0.15. The matrix-eQTL output with SNP, component number, beta, t-stat, P-value and FDR are listed here. <br><em>Note: </em>The trans-eSNPs are not LD-pruned.")
        ),
        conditionalPanel("input.tabs == 'Table 8'", # Originally t5 
                         HTML("<h3>Table 8</h3><br>Trans-eQTLs detected in Cardiogenics data (CG) at FDR < 0.15. The matrix-eQTL output with SNP, component number, beta, t-stat, P-value and FDR are listed here.<br><em>Note: </em>The trans-eSNPs are not LD-pruned.")
        ),
        conditionalPanel("input.tabs == 'Table 9'", # Originally t6
                         HTML("<h3>Table 9</h3><br>Trans-eSNPs detected in the FF dataset (FDR < 0.15) that co-localizewith trait-associated GWAS SNPs. The table lists SNP, P-value, trait, component number, cis-gene (if any), tissue or stimuli specicity scores, genes and gene loading scores for each component and FDR."),
                         helpText(br(),em("Click any cell in the first column of the table to display the genes within that component.")),
                         uiOutput("FFplot_t6") 
        ),
        conditionalPanel("input.tabs == 'Table 10'", # Originally t7
                         HTML("<h3>Table 10</h3><br>Trans-eSNPs detected in the CG dataset (FDR < 0.15) that co-localize with trait-associated GWAS SNPs. The table lists SNP, P-value, trait, component number, cis-gene (if any), tissue or stimuli specicity scores, genes and gene loading scores for each component, and FDR."),
                         helpText(br(),em("Click any cell in the first column of the table to display the genes within that component.")),
                         uiOutput("CGplot_t7")
        )
        
       
    ),
    mainPanel(
      tabsetPanel(
        id = 'tabs',
        tabPanel("Abstract",HTML(abstract), br(),br(), fig1Path, br(),br()),
        tabPanel("Table 1", DT::dataTableOutput("t0"), # !
                  br(), h2(uiOutput("geneTableHeader0")),
                  DT::dataTableOutput("geneTable0"), br(), br()
                ),
        tabPanel("Table 2", DT::dataTableOutput("t1"),# !
                 br(), h2(uiOutput("geneTableHeader1")),
                 DT::dataTableOutput("geneTable1"), br(), br() 
                 ),
        tabPanel("Table 3", DT::dataTableOutput("tXXX")), # !
        tabPanel("Table 4", DT::dataTableOutput("t2")), # ! 
        tabPanel("Table 5", DT::dataTableOutput("t3")), # !
        tabPanel("Table 6", DT::dataTableOutput("tXX")), # !
        tabPanel("Table 7", DT::dataTableOutput("t4")), # ! 
        tabPanel("Table 8", DT::dataTableOutput("t5")), # !  
        tabPanel("Table 9", DT::dataTableOutput("t6"), # !
                 br(), h2(uiOutput("geneTableHeader6")),
                 DT::dataTableOutput("geneTable6"), br(), br()
        ),
        tabPanel("Table 10", DT::dataTableOutput("t7"),# !
                 br(), h2(uiOutput("geneTableHeader7")),
                 DT::dataTableOutput("geneTable7"), br(), br()
        )
        # tabPanel("Table 8", DT::dataTableOutput("t24")),
        # tabPanel("Table 9", DT::dataTableOutput("t29")),
        # tabPanel("Table XX", DT::dataTableOutput("tXX")),
        # tabPanel("Table XXX", DT::dataTableOutput("tXXX"))
        # tabPanel("Table t_sparseFF",DT::dataTableOutput("t_sparseFF")),
        # tabPanel("Table t_sparseCG",DT::dataTableOutput("t_sparseCG"))
        
      )
      
    )
  )
)

translate <- function(tabNum){ 
  newNumber <-switch(toString(tabNum), "0"=1, "1"=2, "XXX"=3, "2"=4, 
         "3"=5, "XX"=6, "4"=7, "5"=8, "6"=9, "7"=10)
  return(toString(newNumber))
} 
# 
# convert_to_suppTables <- function(){ 
#   for(n in c(2,3,4,5,6,7,"XX","XXX")){
#     tabNum <- n
#     tn <- translate(n)
#     print(tn) 
#     dat <- read.delim(paste("supp_tables/SUPPLEMENTARY_TABLE_",toString(tabNum),".txt", sep=""), 
#                       fill=NA, header=T, stringsAsFactors=F, na.strings = "NA")
#     openxlsx::write.xlsx(dat,paste0("supp_excel/S",tn,"_Table.xlsx"))
#   } 
# }
#  

server <- function(input, output, session) {  
  # Save named list of genes/scores
  get_compDict <- function(tabNum){
    file <- read.delim(paste("supp_tables/SUPPLEMENTARY_TABLE_",toString(tabNum),".txt", sep=""), fill=NA, header=T, stringsAsFactors=F)
    # One gene set per component
    geneTables <- file %>% dplyr::group_by(COMP) %>% top_n(n=1, wt=COMP) %>%
      data.frame() %>% select(c("COMP","CompGenes_CompScores")) %>% unique()  
    geneTables["CompGenes_CompScores"] <-gsub("([\\])","@", geneTables$CompGenes_CompScores)  
    compDict <- setNames(object = geneTables$CompGenes_CompScores, nm = as.character(geneTables$COMP))
    return(compDict)
  }
  compDict_t6 <- get_compDict(6)
  compDict_t7 <- get_compDict(7) 
  
  
  # Save specific Component gene sets 
  calc_scaledZscores <- function(scores){ 
    scores_sd <- sd(scores)*sqrt((length(scores)-1)/(length(scores)))
    scores_mean <- mean(scores) 
    zscores <- (scores - scores_mean) / scores_sd
    scaled_zscores <- rescale(zscores, to=c(0,1))
    return(scaled_zscores)
  } 
  
  zscoreCSV <- function(filePath){ 
    library(scales)
    comp <- read.csv(filePath) 
    scores <- comp$GeneScore 
    scaled_zscores <-calc_scaledZscores(scores)
    newDF <- data.frame(GeneSymbol = comp$GeneSymbol, Z_score = scaled_zscores)
    
    fileName <- gsub(filePath, pattern = ".csv", replacement = "")
    write.csv(newDF, paste(fileName, "_Zscores.csv"), quote = F, row.names = F) 
  } 
  # zscoreCSV("geneTables/FF_component22.csv")
  # zscoreCSV(filePath="geneTables/FF_component26.csv")
  # zscoreCSV(filePath="geneTables/CG_component46.csv")
  
  
  
  
  
  
   createTable <- function(file, opts){ 
    table <-  DT::datatable(file, options=opts, filter='top', rownames=F, 
                    class='cell-border stripe', selection='single', extensions=c('Buttons','Scroller'))  
    # Make sure components are notably clickable
    if( opts$trans_tabNum %in% c(1,2,9,10)){
      table <- table %>% formatStyle('Component', color='mediumblue', fontWeight='bold', backgroundColor ="skyblue")
    }
    return(DT::renderDT({table}))
   }
   
   # Report 
   reportStats <- function(file, sparse){
     print(paste("Sparse components in list = ",length(unique(sparse))))
     ## Number of Components 
     print(paste("Unique Sparse Components = ", unique(file$Component) %>% length() )) 
     ## Number of SNP/Component pairs
     unique_CompSNPS <- file %>% group_by(Component,SNP) %>% count() %>% dim()
     print(paste("Unique Sparse Component/SNP pairs =", unique_CompSNPS[1]))
   }
   
   # Set DT options
   getOpts <- function(tabNum){ 
     opts <- list( scrollY = 500, sScrollX="100%", bScrollCollapse=TRUE, pageLength=50,
                   dom = 'frtipB', buttons = list( list(extend = 'csv', filename=paste("TensorMyeloid_ST", translate(tabNum),sep="_")),
                                                   list(extend = 'excel', filename=paste("TensorMyeloid_ST",translate(tabNum),sep="_")),
                                                   list(extend = 'pdf', filename=paste("TensorMyeloid_ST",translate(tabNum),sep="_")),
                                                   list(extend = 'print', filename=paste("TensorMyeloid_ST",translate(tabNum),sep="_")),
                                                   'copy'), paging=FALSE)
     opts$trans_tabNum <- translate(tabNum)
     return(opts)
   }
  
  # Create table
  addTable <- function(tabNum){
    opts <- getOpts(tabNum)
     
    # Import file
    print(paste("------Creating Table",toString(tabNum),"------"))
    file <- read.delim(paste("supp_tables/SUPPLEMENTARY_TABLE_",toString(tabNum),".txt", sep=""), fill=NA, header=T, stringsAsFactors=F, na.strings = "NA")
    #file$entryID <- paste(file$COMP,c(1:dim(file)[1]), sep=".")
    # Tables 6/7 Processing
    if(tabNum %in% c(6,7)){ 
      # Get the entries with the smallest P-vals per component / disease
      file <- conditionsTable <- file %>% dplyr::group_by(SNP, Disease) %>% top_n(n = -1, wt=PVAL) %>%
        data.frame()  
      file <- file %>% select(c("COMP","SNP","PVAL","Disease","FDR","CisGene")) 
      colnames(file) <- c("Component","SNP","P-value","Disease","FDR","CisGene")
      file$Component <- as.character(file$Component)  
      # Create ActivityScore and TissueScore tables
      conditionsTable <- conditionsTable %>% select(c("COMP", "TissScores")) 
       
      if(tabNum == 6){
        conditions <- c("Naive", "IFN", "LPS24", "LPS2")  
        # conditionsTable <- separate(data = conditionsTable, col = CisGene,
        #                             into=paste("CisGene",activities,sep="_"),sep = "_")
        conditionsTable <- separate(data = conditionsTable, col = TissScores,
                                    into=paste("ActivityScore",conditions,sep="_"),sep = "_") 
        colnames(conditionsTable)[colnames(conditionsTable)=="COMP"] <- "Component"
        conditionsTable <- conditionsTable %>% mutate(Component = as.character(Component))
        # Include only SPARSE components
        sparse <- lapply(read.csv("sparseComponents/sparseFF.csv",header = F, stringsAsFactors = F), as.character)$V1
        conditionsTable_sparse <- conditionsTable[conditionsTable$Component %in% sparse,]
        file <- file[file$Component %in% sparse,]
        # Add to output
        
        output$t0 <- createTable(unique(conditionsTable), getOpts(0))
        #output$t_sparseFF <- createTable(unique(select(conditionsTable,c("Component"))), opts)
      }
      if(tabNum == 7){
        conditions <- c("Monocytes","Macrophages") 
        # conditionsTable <- separate(data = conditionsTable, col = CisGene, 
        #                             into=paste("CisGene",tissues,sep="_"),sep = "_")
        conditionsTable <- separate(data = conditionsTable, col = TissScores, 
                                    into=paste("TissueScore",conditions,sep="_"),sep = "_")
        colnames(conditionsTable)[colnames(conditionsTable)=="COMP"] <- "Component"
        conditionsTable <- conditionsTable %>% mutate(Component = as.character(Component))
        # Include only SPARSE components
        sparse <- lapply(read.csv("sparseComponents/sparseCG.csv",header = F, stringsAsFactors = F), as.character)$V1
        conditionsTable_sparse <- conditionsTable[conditionsTable$Component %in% sparse,]
        file <- file[file$Component %in% sparse,]
        # Add to output
        output$t1 <- createTable(unique(conditionsTable),  getOpts(1))
       # output$t_sparseCG <- createTable(unique(select(conditionsTable,c("Component"))), opts)
      }
      file <- separate(data = file, col = CisGene,
                       into=paste("CisGene",conditions,sep="_"),sep = "_")  
      reportStats(file, sparse)  
    }
    # Filter Tables by FDR
    if(tabNum %in% c(4,5)){
      print(paste("Unfiltered = ",dim(file)[1]) )
      file <- subset(file, FDR < 0.05) 
      print(paste("Filtered (FDR < 0.05) =",dim(file)[1])) 
    } 
    if(translate(tabNum)==5){ 
      file  <- file[ , !(names(file) %in% "BONFERRONI")] 
    }
    
    # Create dataTable 
    table <- createTable(unique(file), opts)    
    return(table) 
  }
  
  output$t2 <- addTable(2)
  output$t3 <- addTable(3)
  output$t4 <- addTable(4)
  output$t5 <- addTable(5) 
  output$t6 <- addTable(6)
  output$t7 <- addTable(7) 
  # output$t24 <- addTable(24) 
  # output$t29 <- addTable(29) 
  output$tXX <- addTable("XX")
  output$tXXX <- addTable("XXX")   
  
  ######## Component Gene Details ######## 
  
  # Table 0: FF Gene Plots
  output$FFplot_t0 <- renderUI({
    #rowNum = input$t6_rows_selected
    info = input$t0_cell_clicked 
    if(length(info)){
      # Parse info
      component <- info$value 
      colNum <- info[2]
      output$geneTableHeader0 <- renderUI(paste("Genes in FF Component ",component))
      
      if(colNum==0){  
        # FF gene table 
        geneList <- compDict_t6[toString(component)]
        geneTable <- str_split_fixed(str_split_fixed(geneList, "@", Inf), "_", Inf)
        geneTable <- data.frame(GeneSymbol=geneTable[,1], GeneScore=as.numeric(geneTable[,2]), stringsAsFactors=F)
        
        opts <- getOpts(paste("component_FF",component,sep="_"))
        output$geneTable0 <- createTable(geneTable, opts) 
        # Gene plot
        pdfPath <- paste("FF_Components/FF_Component_",toString(component),".pdf",sep="")#normalizePath(file.path('./CG_Components', 'cardio_Component_1.pdf'))
        helpText(br(), paste("Genes in FF Component ", toString(component)),
                 br(), 
                 tags$iframe(src=pdfPath, height="300px", width="100%", scrolling="no", seamless=NA)) 
      }
    }
  }) 
  
  
  # Table 1: FF Gene Plots
  output$CGplot_t1 <- renderUI({
    #rowNum = input$t7_rows_selected
    info = input$t1_cell_clicked 
    if(length(info)){
      # Parse info 
      component <- info$value
      colNum <- info[2]
      output$geneTableHeader1 <- renderUI(paste("Genes in CG Component ",component))
      
      if(colNum==0){  
        # FF gene table 
        geneList <- compDict_t7[toString(component)]
        geneTable <-  str_split_fixed(str_split_fixed(geneList, "@", Inf), "_", Inf)
        geneTable <- data.frame(GeneSymbol=geneTable[,1], GeneScore=as.numeric(geneTable[,2]), stringsAsFactors=F)
        
        opts <- getOpts(paste("component_CG",component,sep="_"))
        output$geneTable1 <- createTable(geneTable, opts) 
        # Gene plot
        pdfPath <- paste("CG_Components/cardio_Component_",toString(component),".pdf",sep="")#normalizePath(file.path('./CG_Components', 'cardio_Component_1.pdf'))
        helpText(br(), paste("Genes in CG Component ", toString(component)),
                 br(),  
                 tags$iframe(src=pdfPath, height="300px", width="100%", scrolling="no", seamless=NA))
      }
    }
  }) 
  
  
  
  
  # Table 6: FF Gene Plots
  output$FFplot_t6 <- renderUI({
    #rowNum = input$t6_rows_selected
    info = input$t6_cell_clicked 
    if(length(info)){
      # Parse info
      component <- info$value 
      colNum <- info[2]
      output$geneTableHeader6 <- renderUI(paste("Genes in FF Component ",component))
      
      if(colNum==0){  
        # FF gene table 
        geneList <- compDict_t6[toString(component)]
        geneTable <- str_split_fixed(str_split_fixed(geneList, "@", Inf), "_", Inf)
        geneTable <- data.frame(GeneSymbol=geneTable[,1], GeneScore=as.numeric(geneTable[,2]), stringsAsFactors=F)
        
        opts <- getOpts(paste("component_FF",component,sep="_"))
        output$geneTable6 <- createTable(geneTable, opts) 
        # Gene plot
        pdfPath <- paste("FF_Components/FF_Component_",toString(component),".pdf",sep="")#normalizePath(file.path('./CG_Components', 'cardio_Component_1.pdf'))
        helpText(br(), paste("Genes in FF Component ", toString(component)),
                 br(), 
                 tags$iframe(src=pdfPath, height="300px", width="100%", scrolling="no", seamless=NA)) 
      }
    }
  }) 
  
  
  
  
  # Table 7: CG Gene Plots
  output$CGplot_t7 <- renderUI({
    #rowNum = input$t7_rows_selected
    info = input$t7_cell_clicked 
    if(length(info)){
      # Parse info 
      component <- info$value
      colNum <- info[2]
      output$geneTableHeader7 <- renderUI(paste("Genes in CG Component ",component))
      
      if(colNum==0){  
        # FF gene table 
        geneList <- compDict_t7[toString(component)]
        geneTable <- str_split_fixed(str_split_fixed(geneList, "@", Inf), "_", Inf)
        geneTable <- data.frame(GeneSymbol=geneTable[,1], GeneScore=as.numeric(geneTable[,2]), stringsAsFactors=F)
        
        opts <- getOpts(paste("component_CG",component,sep="_"))
        output$geneTable7 <- createTable(geneTable, opts) 
        # Gene plot
        pdfPath <- paste("CG_Components/cardio_Component_",toString(component),".pdf",sep="")#normalizePath(file.path('./CG_Components', 'cardio_Component_1.pdf'))
        helpText(br(), paste("Genes in CG Component ", toString(component)),
                 br(),  
                 tags$iframe(src=pdfPath, height="300px", width="100%", scrolling="no", seamless=NA))
      }
    }
  }) 
  






  
  # # Parse Table 2 correctly
  fixTable2 <- function(){
    file <- read.delim(paste("supp_tables/SUPPLEMENTARY_TABLE_",toString(2),".txt", sep=""), fill=NA, header=T, stringsAsFactors=F)
    ## Col 2  
    col2 <-file[,2]
    substrRight <- function(x, n){
      substr(x, nchar(x)-n+1, nchar(x))
    }
    GO.ID <- substr(col2, 1, 11)
    Term <- substr(col2, 12, 11+43)
    Term <- trimws(Term, which = "left")
    Annotated.Significant <- data.frame(Annotated.Significant = substrRight(col2, 18)) 
    SPLIT2 <- strsplit(substrRight(col2, 18), " ")
    # Remove all "", then get first and second item from each list, respectively
    getElem <- function(x, n){
      return( x[x != ""][n])
    }
    Annotated <- unlist(lapply(SPLIT2, getElem, 1))
    Significant  <- unlist(lapply(SPLIT2, getElem, 2))
    
    # getElem_mod <- function(x, n){
    #   numbers <- x[x != ""]
    #   print(numbers)
    #   if(length(numbers) != 5){ 
    #     n = n-1  
    #   }
    #   return(numbers[n]) 
    # }
    # 
    #x <- SPLIT3[[1]]
    ## Col 3
    col3 <- file[,3]
    SPLIT3 <- strsplit(col3, " ")
    
    Expected <- unlist(lapply(SPLIT3, getElem, 1))
    Rank.in.classicFisher <- unlist(lapply(SPLIT3, getElem, 2)) 
    classicFisher <- unlist(lapply(SPLIT3, getElem, 3))
    elimFisher <- unlist(lapply(SPLIT3, getElem, 4))
    topgoFisher <- unlist(lapply(SPLIT3, getElem, 5)) 
    
    newTable2 <- data.frame(Component=file[,1], GO.ID=GO.ID, Term=Term,
                            Annotated=Annotated, Significant=Significant, Expected=Expected,
                            Rank.in.classicFisher=Rank.in.classicFisher,
                            classicFisher=classicFisher, elimFisher=elimFisher, topgoFisher=topgoFisher,
                            parentchildFisher=file["parentchildFisher"]) 
    
    # newTable2 <- newTable2[order(Rank.in.classicFisher),] 
    # filter(newTable2, as.integer(Rank.in.classicFisher)<1)
    # messedUpRows <- newTable2[as.integer(Rank.in.classicFisher)<1,]
    # messedUpRows[c("")]
    # dplyr::select(messedUpRows, Annotated:topgoFisher)
    # 
    # Remove 0s, since as.integer() turns all non-integer values in 0
    # newTable2[newTable2$Rank.in.classicFisher == 0, "Rank.in.classicFisher"] <- NA
    #data.table::shift(messedUpRows, 1, fill=NA) 
    write.table(newTable2, "supp_tables/SUPPLEMENTARY_TABLE_2.txt", quote = F, sep="\t", row.names = F) 
  }
  

  
  }

shinyApp(ui, server)



 

 
