#' runShinyGradeStat
#' @title runShinyGradeStat
#'
#' @description Run Shiny application.
#'
#' @param launch.browser By default application is run in a new R window.
#'
#' @export runShinyGradeStat
#'
#' @examples
#' \dontrun{
#' runShinyGradeStat()
#' }
#'

runShinyGradeStat <- function( launch.browser = F ){

  app <- shinyApp(

    #### UI ####
    ui = navbarPage( "GradeStat",

      #### Description ####
      tabPanel( "Description",
        tags$head(
          tags$script(
            "$(document).on('shiny:inputchanged', function(event) {
              if (event.name != 'changed') {
                Shiny.setInputValue('changed', event.name);
              }
            });"
          )
        ),
        fluidPage(
          h2("GradeStat: An R Package For Grade Correspondence Analysis"),

          br(),

          h3("Description"),
          "The GradeStat software is a statistical package that enables the independent use of methods used in exploratory data analysis.
          The basis of GradeStat is gradation analysis - a modern set of algorithms, developed by a team of statisticians, enabling quick recognition of large data sets.
          A unique feature of the program is the simple visualization of data using charts called overrepresentation maps, raw data maps and dependency indicator maps.
          GradeStat is irreplaceable as a 'first contact system' with data, and then as a tool for multiple individual data processing in various variants.",
          br(),

          h3("Author"),

          a(href="https://bw.sggw.edu.pl/info/author/WULS522b880691d64b4193872d12ce653e17?aq=%40status%3APRACOWNIK*%2Cauthorprofile%2Fposition%2F%40namePL%3Aadiunkt%3Bauthorprofile%2F%40possitionPL%3Aadiunkt%2C%40active%3D%27true%27%2CmainDiscipline%3AWUTfef424a0094d4fc3865bb3aabd414c88%3BactivityDiscipline%2Fdiscipline%3AWUTfef424a0094d4fc3865bb3aabd414c88&r=author&ps=20&tab=&lang=en&title=Profil%2Bosoby%2B%25E2%2580%2593%2BKrzysztof%2BGajowniczek%2B%25E2%2580%2593%2BSzko%25C5%2582a%2BG%25C5%2582%25C3%25B3wna%2BGospodarstwa%2BWiejskiego%2Bw%2BWarszawie&pn=1&cid=79718", "Krzysztof Gajowniczek, PhD DSc"),

          h3("Data Preparation"),

          "Allows to load an external data and clean it to enable further analysis.
          Accepted file formats are .txt, .csv and .xlsx.
          If one select an Excel file, the program allows to select a proper spreadsheet.
          Acceptable column separators are, comma, semicolon, space or tab.
          The file can be loaded with column names from the first row and row names in the first column.
          After correctly defining the table, the user presses the Accept Table button, the table will appear in the Raw Tables sub-tab.
          If the table meets all the requirements for the grade transformation
          (only numerical records, there is no zero-sum rows or columns), the program displays in green that The table is ready for the Grade Transformation.
          If necessary, the user can modify the table by pressing the Edit Raw Table button.
          Thanks to this, one can edit the values directly in the table.
          The user can increase the impact of a row or column by changing their weights.
          If a column contains missing data, they can be imputed by inserting mean and median values,
          simulating values from an empirical distribution or using random forests taking into account the dependencies in the remaining columns.
          Unnecessary rows or columns can be deleted.
          Finally, the user presses the Transform Raw Table to Grade Table button.
          If the user no longer wants to use the tables, it can be deleted using the Remove Raw Table button.",

          h3("Grade Tables"),

          "The Grade Tables panel contains all the data needed for analyses,
          i.e. raw data, raw data after the clustering, after grade transformation and ordering based on the GCA algorithm.
          In order to run the GCA algorithm in the Raw Grade Tables tab,
          the user must define the number of iterations and then press the Start ordering button.
          Sorting results will be included in the Sorting Results tab.
          Column of the table contains the permutation of rows and columns while the last column shows the value of Spearman's rho coefficient.
          Among various local minima, the user must choose one result.",

          h3("Over-representation Maps"),

          "The Over-representation Maps panel presents all important charts, such as the over-representation map,
          clusters averages for rows and columns, and Sperman's rho.
          The above-mentioned charts are prepared for two versions: a raw table and one adapted according to the GCA algorithm.
          Changing the number of clusters for rows and columns will dynamically modify all charts.
          On the left side of the page there are various graphical parameters of the charts that allow one to
          change the font size, tilt and, in the case of over-representation maps, change the number of intervals.
          Each chart can be saved in various formats (e.g. .png or .jpeg).",

          h3("Literature"),

          p("This panel enumerates all substantial scientific papers and books related with this topic.")

        )
      )

      #### Data Preparation ####
      ,tabPanel("Data Preparation",

        shinyjs::useShinyjs()

        ,tabsetPanel(

          tabPanel( "Uploading Files",

            sidebarLayout(

              sidebarPanel(

                fileInput("file", "Choose Table", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv", ".arff"))

                ,hr()

                ,actionButton( "acceptUplTable", "Accept Table")

                ,hr()

                ,checkboxInput("header", "Header", TRUE)
                ,checkboxInput("RowNames", "Row names", FALSE)
                ,selectInput("sheet", "Sheet", "")

                ,radioButtons("sep", "Separator", choices = c(Comma = ",", Semicolon = ";", Space = " ", Tab = "\t"), selected = ",")

                ,radioButtons("quote", "Quote", choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"')
                ,width = 2

              )

              ,mainPanel(

                DTOutput("InputFile")
                ,width = 10

              )

            )

          )

          ,tabPanel( "Raw Tables",

            br()

            ,shinyjs::hidden( div( style = "display:inline-block",

              id = "AreaRemoveRawTable"

              ,actionButton( "removeRawTable", "Remove Raw Table" )

              ,actionButton( "transformRawTabletoGCA", "Transform Raw Table to Grade Table")

              ,shinyjs::hidden( div( style = "display:inline-block",

                id = "AreaEditRawTable"
                ,actionButton( "editRawTable", "Edit Raw Table" )

              ) )

              ,shinyjs::hidden( div( style = "display:inline-block",

                id = "AreaAcceptModTable"
                ,uiOutput( "acceptModTableUI" )

              ) )

            ) )

            ,hr()

            ,tabsetPanel( id = "TableR", type = "pills" )

          )

          # ,tabPanel( "Grade Tables",
          #
          #   tabsetPanel( id = "TableG", type = "pills" )
          #
          # )

          # ,tabPanel( "Tables Statistics",
          #
          #   tabsetPanel( id = "TableS", type = "pills" )
          #
          # )

        )

      )
      #### Grade Tables ####
      ,tabPanel("Grade Tables",

        shinyjs::useShinyjs()

        ,tabsetPanel(

          tabPanel( "Raw Grade Tables",

            tabsetPanel( id = "TableG", type = "pills" )

          )

          ,tabPanel( "Sorting Reslts",

            tabsetPanel( id = "TableO", type = "pills" )

          )

          ,tabPanel( "Grade Tables after Sorting",

            tabsetPanel( id = "TableS", type = "pills" )

          )

          ,tabPanel( "Input Tables with Clusters",

            tabsetPanel( id = "TableI", type = "pills" )

          )

          ,tabPanel( "Aggregated Input Tables",

            tabsetPanel( id = "TableIA", type = "pills" )

          )

          ,tabPanel( "Input Tables after Sorting",

            tabsetPanel( id = "TableIS", type = "pills" )

          )

          ,tabPanel( "Aggregated Input Tables after Sorting",

            tabsetPanel( id = "TableISA", type = "pills" )

          )

        )

      )

      #### Over-representation Maps ####
      ,tabPanel("Over-representation Maps",

        shinyjs::useShinyjs()

        ,tabsetPanel(

          tabPanel( "Raw Maps",

            tabsetPanel( id = "MapsR", type = "pills" )

          )

          ,tabPanel( "Aggregated Raw Maps",

            tabsetPanel( id = "MapsA", type = "pills" )

          )

          ,tabPanel( "Ordered Maps",

            tabsetPanel( id = "MapsS", type = "pills" )

          )

          ,tabPanel( "Aggregated Ordered Maps",

            tabsetPanel( id = "MapsB", type = "pills" )

          )

        )

      )

      #### Literature ####
      ,tabPanel("Literature",

        fluidPage(

          h3("Software Description:"),
          a(href="https://github.com/KrzyGajow/GradeStat",
            "1. Software page"),
          br(),
          a(href="https://github.com/KrzyGajow/moviesGradeStat",
            "1. Software tutorial"),
          br(),

          h3("Books and Chapters in Books:"),
          a(href="https://www.researchgate.net/publication/258211429_Models_and_Methods_of_Grade_Data_Analysis_Recent_Developments",
            "1. Szczesny W., Kowalczyk T., Wolińska-Welcz A., Wiech M., Dunicz-Sokolowska A., Grabowska G., Pleszczyńska E. (2012) Models and Methods of Grade Data Analysis: Recent Developments, Institute of Computer Science Polish Academy of Sciences, Warsaw."),
          br(),

          a(href="https://books.google.pl/books/about/Analiza_danych_medycznych_i_demograficzn.html?id=f6FcAAAACAAJ&redir_esc=y",
            "2. Książyk J., Matyja O., Pleszczyńska E., Wiech M. (2005) Analiza danych medycznych i demograficznych przy użyciu programu GradeStat. Intytut Podstaw Informatyki PAN, Instytut 'Pomnik - Centrum Zdrowia Dziecka', Warsaw."),
          br(),

          a(href="https://doi.org/10.1007/978-3-540-39928-5",
            "3. Kowalczyk T., Pleszczynska E., & Ruland F. (Eds.). (2004) Grade models and methods for data analysis: with applications for the analysis of data populations (Vol. 151). Springer Science & Business Media."),
          br(),

          a(href="https://doi.org/10.1007/978-3-7908-1773-7_26",
            "4. Ciok A. (2002) Grade Analysis of Repeated Multivariate Measurements. In: Grzegorzewski P., Hryniewicz O., Gil M.A. (eds.), Soft Methods in Probability, Statistics and Data Analysis, Physica-Verlag, pp. 266-273."),
          br(),

          a(href="https://doi.org/10.1007/978-94-017-0061-0_15",
            "5. Kowalczyk T., Niewiadomska-Bugaj M. (2002) A New Grade Measure of Monotone Multivariate Separability. In: Cuadras C., Fortiana J., Rodriquez-Lallena J. (Eds.), Distributions with Given Marginals and Statistical Modelling , Kluwer Academic Publishers, pp. 143-151."),
          br(),

          a(href="https://doi.org/10.1007/978-3-642-56181-8_23",
            "6. Ciok A. (2002) Grade Correspondence-Cluster Analysis Applied to Separate Components of Reversely Regular Mixtures. In: Jajuga K., Sokołowski A., Bock H.-H. (eds.), Classification, Clustering and Data Analysis, Recent Advances and Applications, Springer, pp. 211-218."),
          br(),

          a(href="https://doi.org/10.1007/978-3-642-59789-3_6",
            "7. Ciok A. (2000) Double versus optimal grade clusterings, in: Kiers H.A.L., Rasson J.-P., Groenen P.J.F., Schader M. (Eds.), Data Analysis, Classification, and Related Methods, Springer, pp. 41-46."),
          br(),

          h3("Articles:"),

          a(href="https://www.mdpi.com/2076-3417/8/9/1654",
            "1. Gajowniczek K., Ząbkowski T., Sodenkamp M. (2018) Revealing Household Characteristics from Electricity Meter Data with Grade Analysis and Machine Learning Algorithms, Applied Sciences 8(9), art. 1654."),
          br(),

          a(href="https://ieeexplore.ieee.org/document/8001151",
            "2. Ząbkowski T., Gajowniczek K. (2017) Grade analysis for households segmentation based on energy usage patterns, INnovations in Intelligent SysTems and Applications (INISTA), 2017 International Symposium on. IEEE, pp. 168-173."),
          br(),

          a(href="https://ieeexplore.ieee.org/document/7175938",
            "3. Ząbkowski T., Gajowniczek K., Szupiluk R. (2015) Grade analysis for energy usage patterns segmentation based on smart meter data, In Cybernetics (CYBCONF), 2015 IEEE 2nd International Conference on, pp. 234-239."),
          br(),

          a(href="https://doi.org/10.1007/3-540-33521-8_21",
            "4. Szczesny W., Wiech M. (2006) Visualizing Latent Structures in Grade Correspondence Cluster Analysis and Generalized Association Plots. In: 'Intelligent Information Systems 2006', Proceedings of the IIS'2006 Ustroń, Poland, June 19-22, 2006, Advances in Soft Computing, Physica-Verlag."),
          br(),

          a(href="http://ipipan.waw.pl/~gradestat/ppt/SzczesnyKowalczykWiech_ITIB2006.pdf",
            "5. Szczesny W., Kowalczyk T., Wiech M. (2006) On reversing selected performance indicators used to evaluate a set of business units. In: Wojciechowski I., Strzęciwilk D., Krawiec M. (eds.), Lecture Notes on Information Technology in Business ITIB 2006, Warsaw Agricultural University, pp. 371-381."),
          br(),

          a(href="https://www.infona.pl/resource/bwmeta1.element.baztech-article-BPZ1-0003-0002",
            "6. Pleszczyńska E., Szczesny W. (2002) Grade exploratory methods applied to some medical data sets. Biocybernetics and Biomedical Engineering 22(1), pp. 17-30."),
          br(),

          a(href="https://link.springer.com/chapter/10.1007/978-3-7908-1777-5_25",
            "7. Szczesny W., Kowalczyk T. (2002) On regularity of multivariate data sets. In: Kłopotek M., Wierzchoń T.S., Michalewicz M. (Eds.), Intelligent Information Systems'2002', Proceedings of the IIS'2002', Sopot, Poland, June 3-6, 2002, Advances in Soft Computing, Physica-Verlag, pp. 237-246."),
          br(),

          a(href="https://content.iospress.com/articles/intelligent-data-analysis/ida00077",
            "8. Szczesny W. (2002) Grade correspondence analysis applied to contingency tables and questionnaire data, Intelligent Data Analysis 6(1), pp. 17-51."),
          br(),

          a(href="https://link.springer.com/chapter/10.1007/978-3-7908-1813-0_10",
            "9.Szczesny W., Matyja O. (2001) Using grade correspondence analysis to merge populations and detect latent orders. In: Kłopotek M., Michalewicz M., Wierzchoń T.S. (Eds) Advances in soft computing: 'Intelligent Information Systems 2001', Proceedings of the 'Intelligent Information System X', June 18-22, Zakopane, Poland, Physica-Verlag, pp. 111-120."),
          br(),

          a(href="https://bibliotekanauki.pl/articles/205844",
            "10. Szczesny W. (2000) Detecting rows and columns of contingency table, which outlie from a total positivity pattern. Control and Cybernetics 29(4), pp. 1059-1073."),
          br(),

          a(href="https://link.springer.com/chapter/10.1007/978-3-7908-1846-8_26",
            "11. Matyja O., Szczesny W. (2000) Visualisation in prediction based on grade correspondence analysis, in: Kłopotek M.A., Michalewicz M., Wierzchoń S.T. (eds) Intelligent Information Systems. Advances in Soft Computing. Physica-Verlag (a Springer-Verlag Company), pp. 289-301."),
          br(),

          a(href="https://bibliotekanauki.pl/articles/205844",
            "12. Szczesny W. (1999) Outliers in grade correspondence analysis. In: Kłopotek M., Michalewicz M. (Eds) 'Intelligent Information Systems VIII',Proceedings of the Workshop held in Ustroń, pp. 332-336."),
          br(),

          a(href="https://www.infona.pl/resource/bwmeta1.element.baztech-article-BPZ3-0005-0048",
            "13. Szczesny W., Pleszczyńska E. (1999) Another look at the regression graphs in an experiment on glucose control. Biocybernetics and Biomedical Engineering 19(4), pp. 31-40."),
          br(),

        )

      )

    ),

    #### Server ####
    server <- function( input, output, session ){

      #### Init ####
      # Initialize global variables for further use
      sapply( c("TablesRawList","TablesGCAList","TablesGCAordList","TablesordList",
                "TablesRawAggList","TablesRawAggordList","GCArhoOrd","whichGCAtab","CondList",
                "OverMapListR","OverMapListRo","OverMapListRa","OverMapListRao",
                "OverMapListS","OverMapListSo","OverMapListSa","OverMapListSao",
                "RhoAggTestListR","RhoAggTestListRo","AvgPlotRowListRo","AvgPlotColListRo",
                "RhoAggTestListS","RhoAggTestListSo","AvgPlotRowListSo","AvgPlotColListSo",
                "GCArhoList","startGCArhoList"),
              function(x){ assign( x, list(), envir = .GlobalEnv ) } )
      sapply( c("TableRawEdit","TableRawProxy","editTable","tempTab"), function(x){ assign( x, F, envir = .GlobalEnv ) } )

      # Reactive and static variables needed for dynamic modification only eligible parts of the code
      sapply( c("whichTabModStat"), function(x){ assign( x, 0, envir = .GlobalEnv ) } )
      whichTabModReact <- reactiveVal( 0 )
      whichTabGCArhoReact <- reactiveVal( 0 )

      # Reactive value needed for creating or removing tab panels with input tables
      fileIndex <- reactiveVal( 0 )

      #### Input files ####
      # Data uploading

      input_file <- reactive({

        req( input$file )

        if( length( grep("arff",input$file$name) ) ){

          df <- read.arff( input$file$datapath )

        }else if( length( grep("xls",input$file$name) ) ){

          if( input$sheet == "" ){
            sheet <- 1
          }else{
            sheet <- input$sheet
          }

          df <- read_excel( input$file$datapath, sheet = sheet, col_names = input$header, .name_repair = "unique_quiet" )
          df <- as.data.frame( df )

          if( !input$header ){
            colnames( df ) <- paste0( "c", 1:ncol(df) )
          }

          if( input$RowNames ){
            names <- df[,1]
            names[ is.na(names) ]<- paste0( "r", 1:sum( is.na(names) ) )
            rownames( df ) <- names
            df <- df[,-1]
          }else{
            rownames( df ) <- paste0( "r", 1:nrow(df) )
          }

        }else{

          if( input$RowNames ){
            df <- read.table( input$file$datapath, header = input$header, sep = input$sep, quote = input$quote,
                              check.names = F, stringsAsFactors = F, row.names = 1 )
          }else{
            df <- read.table( input$file$datapath, header = input$header, sep = input$sep, quote = input$quote,
                              check.names = F, stringsAsFactors = F )
            rownames( df ) <- paste0( "r", 1:nrow(df) )
          }
          if( ncol(df) == 0 ){
            df <- data.frame( Info = "Table has 0 columns, please change separator or row names." )
          }

        }

        return( df )

      })

      observeEvent( input$file, ignoreInit = T, {

        req( length( grep("xls",input$file$name) ) != 0 )
        input_sheets <- excel_sheets( input$file$datapath )
        updateSelectInput( session, "sheet", choices = input_sheets, selected = "" )

      })

      #### Accept Raw Table ####
      observeEvent( input$acceptUplTable, {

        # Check if there is an input file
        req( input$file )
        req( editTable == F )

        # Increment reactive values
        fileIndex( fileIndex() + 1 )
        iter <- isolate( fileIndex() )

        # Insert table into the global table list
        TablesRawList[[ iter ]] <<- input_file()
        TablesRawList[[ iter ]] <<- AddClWe( TablesRawList[[ iter ]] )

        # Initialize information about iteration of modification, requires to optimize GCA table displaying
        attr( TablesRawList[[ iter ]], "nMod" ) <<- 0

        # Insert table into the Shiny output
        eval( parse( text = sprintf( "output$TablesRawOut%s <- renderDT( TablesRawList[[ iter ]], options = list( dom = 't', pageLength = 10000 ) )", iter ) ) )

        # Check if table is ready for further steps
        CondList[[ iter ]] <<- TableRawCheckShiny( TablesRawList[[ iter ]] )

        # Insert table into the Shiny output
        eval( parse( text = sprintf( "output$Cond%s <- renderText( CondList[[ iter ]] )", iter ) ) )

        # Create a new tab panel with the constant (and accepted) table
        appendTab( "TableR",
          tabPanel( fileIndex(),
            sidebarLayout(
              sidebarPanel(
                eval( parse( text = sprintf( "selectInput( 'modAttributes%s', 'Attribute to be Modified', '' )", iter ) ) )
                ,hr()
                ,selectInput( "imputMethod", "Imputation method",
                              choices = list( "mean","median","empirical dist", "random forest"), selected = "" )
                ,eval( parse( text = sprintf( "actionButton( 'startImputation%s', 'Start Imputation' )", iter ) ) )
                ,eval( parse( text = sprintf( "actionButton( 'discardImputation%s', 'Discard Imputation' )", iter ) ) )
                ,eval( parse( text = sprintf( "actionButton( 'acceptImputation%s', 'Accept Imputation' )", iter ) ) )
                ,hr()
                ,eval( parse( text = sprintf( "selectInput( 'transChartoNum%s', 'Attribute to be transformed from Char to Numeric', '' )", iter ) ) )
                ,eval( parse( text = sprintf( "actionButton( 'acceptTransChartoNum%s', 'Accept Transformation' )", iter ) ) )
                ,hr()
                ,eval( parse( text = sprintf( "selectInput( 'transChartoFac%s', 'Attribute to be transformed from Char to Factor', '' )", iter ) ) )
                ,eval( parse( text = sprintf( "selectInput( 'transChartoFacOrd%s', 'Levels order', '', multiple = T )", iter ) ) )
                ,eval( parse( text = sprintf( "actionButton( 'acceptTransChartoFac%s', 'Accept Transformation' )", iter ) ) )
                ,hr()
                ,eval( parse( text = sprintf( "selectInput( 'removeAttributes%s', 'Attribute to be Removed', '', multiple = T )", iter ) ) )
                ,eval( parse( text = sprintf( "actionButton( 'acceptRemoveAttributes%s', 'Accept Deletion' )", iter ) ) )
                ,hr()
                ,eval( parse( text = sprintf( "selectInput( 'removeRows%s', 'Observation to be Removed', '', multiple = T )", iter ) ) )
                ,eval( parse( text = sprintf( "actionButton( 'acceptRemoveRows%s', 'Accept Deletion' )", iter ) ) )
                ,hr()
                ,eval( parse( text = sprintf( "downloadButton( 'saveR%s', 'Save Table' )", iter ) ) )
                ,width = 2
              )
              ,mainPanel(
                hr(),
                fluidRow( htmlOutput( sprintf( "Cond%s", iter ) ) ),
                hr(),
                fluidRow( DTOutput( sprintf( "TablesRawOut%s", iter ) ) )
                ,width = 10
              )
            ) ), select = TRUE )

        # Unhide "Remove" and "Edit" buttons
        shinyjs::show("AreaRemoveRawTable")
        shinyjs::show("AreaEditRawTable")

        # Save Table
        eval( parse( text = sprintf(
          "output$saveR%s <- downloadHandler(
            filename = function(){
              paste0('RawTable-', Sys.time(), '.txt')
            },
            content = function( file ){
              write.table( TablesRawList[[ iter ]], file, sep = ';' )
            }
          )", iter
        ) ) )

      })

      #### Files render ####
      output$InputFile <- renderDT( input_file() )

      observeEvent( input$acceptUplTable, {

        Col_names <- colnames( input_file() )
        Row_names <- rownames( input_file() )

        eval( parse( text = sprintf( "updateSelectInput( session, 'modAttributes%s', choices = Col_names, selected = '' )",  isolate( fileIndex() ) ) ) )
        eval( parse( text = sprintf( "updateSelectInput( session, 'removeAttributes%s', choices = Col_names, selected = '' )",  isolate( fileIndex() ) ) ) )
        eval( parse( text = sprintf( "updateSelectInput( session, 'removeRows%s', choices = Row_names, selected = '' )",  isolate( fileIndex() ) ) ) )

        # Determine character columns
        ColType <- unlist( lapply( input_file(), typeof ) )
        ColType <- colnames( input_file() )[ which( ColType == "character" ) ]
        eval( parse( text = sprintf( "updateSelectInput( session, 'transChartoNum%s', choices = ColType, selected = '' )",  isolate( fileIndex() ) ) ) )
        eval( parse( text = sprintf( "updateSelectInput( session, 'transChartoFac%s', choices = ColType, selected = '' )",  isolate( fileIndex() ) ) ) )

      })

      #### Allow Raw Table modification ####
      observeEvent( input$editRawTable, {

        # Update global variables for latter use
        whichTabModReact( as.numeric( input$TableR ) )
        whichTabModStat <<- as.numeric( input$TableR )

        eval( parse( text = sprintf( "output$TablesRawOut%s <- renderDT( TablesRawList[[ whichTabModStat ]], options = list( dom = 't', pageLength = 10000 ), editable = 'cell' )", whichTabModStat ) ) )

        # Create temporary proxy table for modification
        TableRawProxy <<- dataTableProxy( sprintf( "TablesRawOut%s", whichTabModStat ) )

        if( editTable == F  ){

          # Update global variable allowing for many other fields modification
          editTable <<- T

          # Dynamically edit text displayed at the Accept Button
          output$acceptModTableUI <- renderUI({

            actionButton( "acceptModTable", sprintf( "Accept Table %s Modification", whichTabModStat ), style = "color:red" )

          })

          shinyjs::hide("AreaEditRawTable")
          shinyjs::show("AreaAcceptModTable")

        }

      })

      #### Start missing values imputation ####
      observeEvent( eval( parse( text = sprintf( "input$startImputation%s", whichTabModReact() ) ) ), {

        req( editTable )

        # Take attribute to be modified
        modAttribute <- eval( parse( text = sprintf( "input$modAttributes%s", whichTabModStat ) ) )
        req( modAttribute )

        # Create global temporary table which will be modified, eventually this table replaces final output Raw Table
        tempTab <<- TablesRawList[[ whichTabModStat ]]

        # Determine indexes of missing cells
        whichNA <- !is.finite( tempTab[, modAttribute ] )
        whichWC <- (1:nrow(tempTab)) %in% (nrow(tempTab)-1):(nrow(tempTab))

        # Determine number of missing cells, if the entire column is NA then imputation is not possible
        cntNA <- sum( whichNA & !whichWC )

        # Choose imputation method
        if( input$imputMethod == "mean" & cntNA < nrow(tempTab)-2 ){

          tempTab[ whichNA & !whichWC, modAttribute ] <<- mean( tempTab[ !whichNA & !whichWC, modAttribute ] )

        }else if( input$imputMethod == "median" & cntNA < nrow(tempTab)-2 ){

          tempTab[ whichNA & !whichWC, modAttribute ] <<- median( tempTab[ !whichNA & !whichWC, modAttribute ] )

        }else if( input$imputMethod == "empirical dist" & cntNA < nrow(tempTab)-2 ){

          tempTab[ whichNA & !whichWC, modAttribute ] <<- sample( tempTab[ !whichNA & !whichWC, modAttribute ], sum( whichNA & !whichWC ), T )

        }else if( input$imputMethod == "random forest" & cntNA < nrow(tempTab)-2 ){

          # Determine type of the attribute
          ColType <- which( unlist( lapply( tempTab, typeof ) ) != "character" )

          # Use only numeric attributes
          tempTaB <- tempTab[,ColType]

          # Replace undefined values with NA
          tempTaB <- as.data.frame( lapply( tempTaB, function(x){ ifelse( is.finite(x), x, NA ) } ) )

          tempTab[ whichNA, modAttribute ] <<- missForest( tempTaB[!whichWC,] )$ximp[ whichNA, modAttribute ]

        }

        # Create temporary table only for displaying purpose,
        # ADD column stores info which cells are missing for a particular feature,
        # ADD column is hidden
        # Target column is formatted based on the ADD column
        dispTab <- formatStyle( datatable( data.frame( ADD = whichNA & !whichWC, tempTab ), editable = TRUE,
                                           options = list( columnDefs = list( list( targets = 1, visible = FALSE) ) ) ),
                                modAttribute, "ADD", color = styleEqual( TRUE, 'red' ) )

        # Update the displayed output table
        eval( parse( text = sprintf( "output$TablesRawOut%s <- renderDT( dispTab )", whichTabModStat ) ) )

      })

      #### Accept missing values imputation ####
      observeEvent( eval( parse( text = sprintf( "input$acceptImputation%s", whichTabModReact() ) ) ), {

        req( editTable )

        # For each feature update global final Raw Table
        TablesRawList[[ whichTabModStat ]] <<- tempTab

        # Update information about iteration of modification, requires to optimize GCA table displaying
        attr( TablesRawList[[ whichTabModStat ]], "nMod" ) <<- attr( tempTab, "nMod" )

        # Update the displayed output table
        eval( parse( text = sprintf( "output$TablesRawOut%s <- renderDT( TablesRawList[[ whichTabModStat ]], options = list( dom = 't', pageLength = 10000 ), editable = 'cell' )", whichTabModStat ) ) )

      })

      #### Discard missing values imputation ####
      observeEvent( eval( parse( text = sprintf( "input$discardImputation%s", whichTabModReact() ) ) ), {

        req( editTable )

        # Display global final Raw Table
        eval( parse( text = sprintf( "output$TablesRawOut%s <- renderDT( TablesRawList[[ whichTabModStat ]], options = list( dom = 'ft', pageLength = 10000 ), editable = 'cell' )", whichTabModStat ) ) )

      })

      #### Accept transformation from Char to Numeric ####
      observeEvent( eval( parse( text = sprintf( "input$acceptTransChartoNum%s", whichTabModReact() ) ) ), {

        req( editTable )

        # Take attribute to be modified
        modAttribute <- eval( parse( text = sprintf( "input$transChartoNum%s", whichTabModStat ) ) )
        req( modAttribute )

        # For each feature update global final Raw Table
        TablesRawList[[ whichTabModStat ]][, modAttribute ] <<-
          as.numeric( TablesRawList[[ whichTabModStat ]][, modAttribute ] )

        # Update the displayed output table
        eval( parse( text = sprintf( "output$TablesRawOut%s <- renderDT( TablesRawList[[ whichTabModStat ]], options = list( dom = 't', pageLength = 10000 ), editable = 'cell' )", whichTabModStat ) ) )

      })

      #### Update levels in SelectInput in transformation from Char to Factor ####
      observeEvent( eval( parse( text = sprintf( "input$transChartoFac%s", whichTabModReact() ) ) ), {

        req( editTable )

        # Take attribute to be modified
        modAttribute <- eval( parse( text = sprintf( "input$transChartoFac%s", whichTabModStat ) ) )
        req( modAttribute )

        # Determine unique attribute values
        levUniq <- unique( TablesRawList[[ whichTabModStat ]][, modAttribute ] )

        # Create dynamic string with levels
        levUniq <- paste0( "c(", paste0( paste0( "'", levUniq, "'"), collapse = "," ), ")" )

        eval( parse( text = sprintf( "updateSelectInput( session, 'transChartoFacOrd%s', choices = %s, selected = '' )",
                                     whichTabModStat, levUniq ) ) )

      })

      #### Accept transformation from Char to Factor ####
      observeEvent( eval( parse( text = sprintf( "input$acceptTransChartoFac%s", whichTabModReact() ) ) ), {

        req( editTable )

        # Take attribute to be modified
        modAttribute <- eval( parse( text = sprintf( "input$transChartoFac%s", whichTabModStat ) ) )
        req( modAttribute )

        # Take levels order
        levOrder <- eval( parse( text = sprintf( "input$transChartoFacOrd%s", whichTabModStat ) ) )
        req( levOrder )

        # Create temporary vector
        NewAttribute <- factorTOnumeric( factor( TablesRawList[[ whichTabModStat ]][, modAttribute ],
                                                 levels = levOrder ), 1:length( levOrder ) )

        # For each feature update global final Raw Table
        TablesRawList[[ whichTabModStat ]][, modAttribute ] <<- NewAttribute

        # Update the displayed output table
        eval( parse( text = sprintf( "output$TablesRawOut%s <- renderDT( TablesRawList[[ whichTabModStat ]], options = list( dom = 't', pageLength = 10000 ), editable = 'cell' )", whichTabModStat ) ) )

      })

      #### Accept attribute deletion ####
      observeEvent( eval( parse( text = sprintf( "input$acceptRemoveAttributes%s", whichTabModReact() ) ) ), {

        req( editTable )

        # Create temporary attribute to be reassigned in further step
        attrTemp <- attr( TablesRawList[[ whichTabModStat ]], "nMod" )

        # Remove features from the global final Raw Table
        TablesRawList[[ whichTabModStat ]] <<-
          TablesRawList[[ whichTabModStat ]][, !colnames(TablesRawList[[ whichTabModStat ]]) %in%
                                           eval( parse( text = sprintf( "input$removeAttributes%s", whichTabModStat ) ) ), drop = F ]

        # Update information about iteration of modification, requires to optimize GCA table displaying
        attr( TablesRawList[[ whichTabModStat ]], "nMod" ) <<- attrTemp

        # Display global final Raw Table
        eval( parse( text = sprintf( "output$TablesRawOut%s <- renderDT( TablesRawList[[ whichTabModStat ]], options = list( dom = 't', pageLength = 10000 ), editable = 'cell' )", whichTabModStat ) ) )

        # Update list of possible attributes to be removed or imputed
        whichWC <- !(1:ncol(TablesRawList[[ whichTabModStat ]])) %in% (ncol(TablesRawList[[ whichTabModStat ]])-1):(ncol(TablesRawList[[ whichTabModStat ]]))
        eval( parse( text = sprintf( "updateSelectInput( session, 'modAttributes%s', choices = colnames( TablesRawList[[ whichTabModStat ]] )[whichWC], selected = '' )",  whichTabModStat ) ) )
        eval( parse( text = sprintf( "updateSelectInput( session, 'removeAttributes%s', choices = colnames( TablesRawList[[ whichTabModStat ]] )[whichWC], selected = '' )",  whichTabModStat ) ) )

      })

      #### Accept rows deletion ####
      observeEvent( eval( parse( text = sprintf( "input$acceptRemoveRows%s", whichTabModReact() ) ) ), {

        req( editTable )

        # Create temporary attribute to be reassigned in further step
        attrTemp <- attr( TablesRawList[[ whichTabModStat ]], "nMod" )

        # Remove rows from the global final Raw Table
        TablesRawList[[ whichTabModStat ]] <<-
          TablesRawList[[ whichTabModStat ]][ !rownames(TablesRawList[[ whichTabModStat ]]) %in%
                                            eval( parse( text = sprintf( "input$removeRows%s", whichTabModStat ) ) ), , drop = F ]

        # Update information about iteration of modification, requires to optimize GCA table displaying
        attr( TablesRawList[[ whichTabModStat ]], "nMod" ) <<- attrTemp

        # Display global final Raw Table
        eval( parse( text = sprintf( "output$TablesRawOut%s <- renderDT( TablesRawList[[ whichTabModStat ]], options = list( dom = 't', pageLength = 10000 ), editable = 'cell' )", whichTabModStat ) ) )

        # Update list of possible rows to be removed
        whichWC <- !(1:nrow(TablesRawList[[ whichTabModStat ]])) %in% (nrow(TablesRawList[[ whichTabModStat ]])-1):(nrow(TablesRawList[[ whichTabModStat ]]))
        eval( parse( text = sprintf( "updateSelectInput( session, 'removeRows%s', choices = rownames( TablesRawList[[ whichTabModStat ]] )[whichWC], selected = '' )",  whichTabModStat ) ) )

      })

      #### Edit Raw Table by User input ####
      # observeEvent( list( eval( parse( text = sprintf( "input$TablesRawOut%s_cell_edit", whichTabModReact() ) ) ),
      #                     eval( parse( text = sprintf( "input$TablesRawOut%s_cell_edit", as.numeric( input$TableR ) ) ) ) ), {
      observeEvent( eval( parse( text = sprintf( "input$TablesRawOut%s_cell_edit", input$TableR ) ) ), {

# print( input$changed )
# print(whichTabModReact())
# print(input$TableR)
        iter <- whichTabModStat
        info <- eval( parse( text = sprintf( "input$TablesRawOut%s_cell_edit", iter ) ) )

        if( editTable ){

          rowN <- nrow( TablesRawList[[ iter ]] )
          colN <- ncol( TablesRawList[[ iter ]] )

          if( if( is.null(info) ){ TRUE }else{
              ( info[1] == rowN | info[2] == colN ) | ( (info[1] == (rowN-1) & info[3] <= 0 ) | (info[2] == (colN-1) & info[3] <= 0 ) )
            } ){

            eval( parse( text = sprintf( "output$TablesRawOut%s <- renderDT( TablesRawList[[ iter ]], options = list( dom = 't', pageLength = 10000 ), editable = 'cell' )", iter ) ) )

          }else{

            TablesRawList[[ iter ]] <<- editData( TablesRawList[[ iter ]], info )

            # TO DO: If after change all values are numerical transform character to numeric
            replaceData( TableRawProxy, TablesRawList[[ iter ]], resetPaging = FALSE )

          }

        }else{

          eval( parse( text = sprintf( "output$TablesRawOut%s <- renderDT( TablesRawList[[ iter ]], options = list( dom = 't', pageLength = 10000 ), editable = 'cell' )", iter ) ) )

        }

      })

      #### Remove Raw Table ####
      observeEvent( input$removeRawTable, {

        # Remove from Raw tab panel active Raw tab
        removeTab( "TableR", target = input$TableR )
        removeTab( "MapsR", target = input$TableR )
        removeTab( "MapsA", target = input$TableR )

        # Remove from GCA tab panel active Raw tab
        removeTab( "TableG", target = input$TableR )
        removeTab( "TableO", target = input$TableR )

        removeTab( "TableS", target = input$TableS )
        removeTab( "MapsS", target = input$TableS )
        removeTab( "MapsB", target = input$TableS )

        removeTab( "TableI", target = input$TableR )
        removeTab( "TableIS", target = input$TableR )
        removeTab( "TableIA", target = input$TableR )
        removeTab( "TableISA", target = input$TableR )

        # Remove table from the global table list
        TablesRawList[[ as.numeric( input$TableR ) ]] <<- FALSE

        # If GCA is already created then remove this table as well
        if( as.numeric( input$TableR ) <= length( TablesRawList ) ){

          TablesGCAList[[ as.numeric( input$TableR ) ]] <<- FALSE

        }

        # If there is no accepted table hide "Remove" button
        if( all( unlist( lapply( TablesRawList, function(x){ is.null( dim(x) ) } ) ) ) ){

          shinyjs::hide("AreaRemoveRawTable")

        }

        shinyjs::hide("AreaAcceptModTable")
        shinyjs::show("AreaEditRawTable")

        # Update global variable allowing for many other fields modification
        editTable <<- F

      })

      #### Accept Raw Table Modification ####
      observeEvent( input$acceptModTable, {

        # Reset global variable, after this modification is not allowed
        editTable <<- F

        # Check if table is ready for further steps
        CondList[[ whichTabModStat ]] <<- TableRawCheckShiny( TablesRawList[[ whichTabModStat ]] )

        # Insert table into the Shiny output
        eval( parse( text = sprintf( "output$Cond%s <- renderText( CondList[[ whichTabModStat ]] )", whichTabModStat ) ) )

        # Update the displayed output table
        eval( parse( text = sprintf( "output$TablesRawOut%s <- renderDT( TablesRawList[[ whichTabModStat ]], options = list( dom = 't', pageLength = 10000 ), editable = 'cell' )", whichTabModStat ) ) )

        # Update information about iteration of modification, requires to optimize GCA table displaying
        attr( TablesRawList[[ whichTabModStat ]], "nMod" ) <<- attr( TablesRawList[[ whichTabModStat ]], "nMod" ) + 1

        shinyjs::show("AreaEditRawTable")
        shinyjs::hide("AreaAcceptModTable")

      })

      #### Transform Raw Table to GCA Table ####
      observeEvent( input$transformRawTabletoGCA, {

        iter <- as.numeric( input$TableR )

        req( !editTable )
        req( attr( CondList[[ iter ]], "isTabOk" ) )

        # Check if GCA table is not already created
        if( iter > length( TablesGCAList ) ){

          req( TRUE )
          typeUpdate <- 1

        }else if( is.null( TablesGCAList[[ iter ]] ) ) {

          req( TRUE )
          typeUpdate <- 2

        }else if( attr( TablesGCAList[[ iter ]], "nMod" ) != attr( TablesRawList[[ iter ]], "nMod" ) ){

          req( TRUE )
          typeUpdate <- 3

        }else{

          req( FALSE )

        }

        # Static global object preventing wrong GCA table displaying
        whichGCAtab[[ iter ]] <<- iter

        # Create new constant Grade table and insert table into the global GCA table list
        TablesGCAList[[ iter ]] <<- TableGCA( TablesRawList[[ iter ]] )

        # Update information about iteration of modification, requires to optimize GCA table displaying
        attr( TablesGCAList[[ iter ]], "nMod" ) <<- attr( TablesRawList[[ iter ]], "nMod" )

        if( any( typeUpdate %in% 1:2 ) ){

          # Insert table into the Shiny output
          eval( parse( text = sprintf( "output$TablesGCAOut%s <- renderDT( TablesGCAList[[ whichGCAtab[[ iter ]] ]], options = list( dom = 't', pageLength = 10000 ) )", whichGCAtab[[ iter ]] ) ) )

          # Create a new tab panel with the constant Grade table
          appendTab( "TableG",
            tabPanel( whichGCAtab[[ iter ]],
              sidebarLayout(
                sidebarPanel(
                  eval( parse( text = sprintf( "numericInput( 'GCArhoIter%s',
                                               'Number of iterations of Rho Spearman ordering', value = 100, 1, Inf, 1 )", iter ) ) )
                  ,eval( parse( text = sprintf( "actionButton( 'startGCArho%s', 'Start ordering' )", iter ) ) )
                  ,hr()
                  ,eval( parse( text = sprintf( "downloadButton( 'saveG%s', 'Save Table' )", iter ) ) )
                  ,width = 2
                )
                ,mainPanel(
                  DTOutput( sprintf( "TablesGCAOut%s", whichGCAtab[[ iter ]] ) )
                  ,width = 10
                )
            ) ), select = TRUE )

          # Save Table
          eval( parse( text = sprintf(
            "output$saveG%s <- downloadHandler(
              filename = function(){
                paste0('GCATable-', Sys.time(), '.txt')
              },
              content = function( file ){
                write.table( TablesGCAList[[ whichGCAtab[[ iter ]] ]], file, sep = ';' )
              }
            )", iter
          ) ) )

          eval( parse( text = sprintf( "output$TablesInOut%s <- renderDT( TablesRawList[[ whichGCAtab[[ iter ]] ]] )", iter ) ) )

          # Append new tab with input Table with clusters
          appendTab( "TableI",
            tabPanel( iter,
            sidebarLayout(
              sidebarPanel(
                eval( parse( text = sprintf( "downloadButton( 'saveI%s', 'Save Table' )", iter ) ) )
                ,width = 2
              )
              ,mainPanel(
                DTOutput( sprintf( "TablesInOut%s", iter ) )
                ,width = 10
              )
          ) ), select = TRUE )

          # Save Table
          eval( parse( text = sprintf(
            "output$saveI%s <- downloadHandler(
              filename = function(){
                paste0('InputTable-', Sys.time(), '.txt')
              },
              content = function( file ){
                write.table( TablesRawList[[ whichGCAtab[[ iter ]] ]], file, sep = ';' )
              }
            )", iter
          ) ) )

          Aggout <- AggTableDualRaw( TablesRawList[[ whichGCAtab[[ iter ]] ]], 1, 1 )
          TablesRawAggList[[ whichGCAtab[[ iter ]] ]] <<- RemoveClWe( Aggout$In )

          eval( parse( text = sprintf( "output$TablesInAOut%s <- renderDT( TablesRawAggList[[ whichGCAtab[[ iter ]] ]] )", iter ) ) )

          # Append new tab with input Table with clusters
          appendTab( "TableIA",
           tabPanel( iter,
             sidebarLayout(
               sidebarPanel(
                 eval( parse( text = sprintf( "downloadButton( 'saveIA%s', 'Save Table' )", iter ) ) )
                 ,width = 2
               )
               ,mainPanel(
                 DTOutput( sprintf( "TablesInAOut%s", iter ) )
                 ,width = 10
               )
             ) ), select = TRUE )

          # Save Table
          eval( parse( text = sprintf(
            "output$saveIA%s <- downloadHandler(
              filename = function(){
                paste0('InputTableAgg-', Sys.time(), '.txt')
              },
              content = function( file ){
                write.table( TablesRawAggList[[ whichGCAtab[[ iter ]] ]], file, sep = ';' )
              }
            )", iter
          ) ) )

          # Insert table into the Shiny output
          OverMapListRo[[ iter ]] <<- OverMapListR[[ iter ]] <<- OverMap( OverReep( RemoveClWe( TablesGCAList[[ whichGCAtab[[ iter ]] ]] ) ), 5 )
          eval( parse( text = sprintf( "output$MapsGCArhoRawOut%s <- renderPlot( OverMapListR[[ iter ]] )",
                                       whichGCAtab[[ iter ]] ) ) )

          # Calculate Rho Spearman for different number of clusters
          RhoAggTestListR[[ iter ]] <<- RhoAggTest( TablesGCAList[[ whichGCAtab[[ iter ]] ]] )
          RhoAggTestListRo[[ iter ]] <<- RhoPlot( session, iter, 'R', RhoAggTestListR[[ iter ]] )
          eval( parse( text = sprintf( "output$RhoAggTestRawOut%s <- renderPlot( RhoAggTestListRo[[ iter ]] )",
                                       whichGCAtab[[ iter ]] ) ) )

          # Table dimension
          rowN <- nrow( TablesGCAList[[ whichGCAtab[[ iter ]] ]] )-2
          colN <- ncol( TablesGCAList[[ whichGCAtab[[ iter ]] ]] )-2

          # Display Averages for clusters
          AvgPlotRowListRo[[ iter ]] <<- MeanStdPlot( session, iter, 'R', TableGCA( AggTable( TablesGCAList[[ whichGCAtab[[ iter ]] ]], TRUE, rep( 1, rowN ), 0 ) ), TRUE )
          AvgPlotColListRo[[ iter ]] <<- MeanStdPlot( session, iter, 'R', TableGCA( AggTable( TablesGCAList[[ whichGCAtab[[ iter ]] ]], FALSE, rep( 1, colN ), 0 ) ), FALSE )

          eval( parse( text = sprintf( "output$AvgPlotRowRawOut%s <- renderPlot( AvgPlotRowListRo[[ iter ]] )",
                                       whichGCAtab[[ iter ]] ) ) )
          eval( parse( text = sprintf( "output$AvgPlotColRawOut%s <- renderPlot( AvgPlotColListRo[[ iter ]] )",
                                       whichGCAtab[[ iter ]] ) ) )

          # Create a new tab panel with the Over-representation Map
          appendTab( "MapsR",
            tabPanel( whichGCAtab[[ iter ]],
              sidebarLayout(
                sidebarPanel(
                  eval( parse( text = sprintf( "numericInput( 'sizeXClustR%s', 'X font size Clusters', value = 10, 1, 30, 1 )", iter ) ) )
                  ,eval( parse( text = sprintf( "numericInput( 'sizeYClustR%s', 'Y font size Clusters', value = 10, 1, 30, 1 )", iter ) ) )
                  ,eval( parse( text = sprintf( "numericInput( 'sizeXlClustR%s', 'Label X font size Clusters', value = 10, 1, 30, 1 )", iter ) ) )
                  ,eval( parse( text = sprintf( "numericInput( 'sizeYlClustR%s', 'Label Y font size Clusters', value = 10, 1, 30, 1 )", iter ) ) )
                  ,eval( parse( text = sprintf( "numericInput( 'sizeLClustR%s', 'Legend font size Clusters', value = 10, 1, 30, 1 )", iter ) ) )
                  ,hr()
                  ,eval( parse( text = sprintf( "numericInput( 'sizeXAvgR%s', 'X font size Mean', value = 10, 1, 30, 1 )", iter ) ) )
                  ,eval( parse( text = sprintf( "numericInput( 'sizeYAvgR%s', 'Y font size Mean', value = 10, 1, 30, 1 )", iter ) ) )
                  ,eval( parse( text = sprintf( "numericInput( 'sizeXlAvgR%s', 'Label X font size Mean', value = 10, 1, 30, 1 )", iter ) ) )
                  ,eval( parse( text = sprintf( "numericInput( 'sizeYlAvgR%s', 'Label Y font size Mean', value = 10, 1, 30, 1 )", iter ) ) )
                  ,hr()
                  ,eval( parse( text = sprintf( "numericInput( 'clustNrowR%s', 'Number of clusters for rows',
                                                value = 1, 1, rowN, 1 )", iter ) ) )
                  ,br()
                  ,eval( parse( text = sprintf( "numericInput( 'clustNcolR%s', 'Number of clusters for columns',
                                                 value = 1, 1, colN, 1 )", iter ) ) )
                  ,hr()
                  ,eval( parse( text = sprintf( "numericInput( 'cutsMapR%s', 'Number of intervals', value = 5, 1, 30, 1 )", iter ) ) )
                  ,eval( parse( text = sprintf( "numericInput( 'sizeXMapR%s', 'X font size Map', value = 10, 1, 30, 1 )", iter ) ) )
                  ,eval( parse( text = sprintf( "numericInput( 'angleXMapR%s', 'X font angle Map', value = 0, 0, 90, 1 )", iter ) ) )
                  ,eval( parse( text = sprintf( "numericInput( 'sizeYMapR%s', 'Y font size Map', value = 10, 1, 30, 1 )", iter ) ) )
                  ,eval( parse( text = sprintf( "numericInput( 'angleYMapR%s', 'Y font angle Map', value = 0, 0, 90, 1 )", iter ) ) )
                  ,eval( parse( text = sprintf( "numericInput( 'sizeLMapR%s', 'Legend font size Map', value = 10, 1, 30, 1 )", iter ) ) )
                  ,eval( parse( text = sprintf( "numericInput( 'sizeWMapR%s', 'Width of the Map in %%', value = 100, 1, 300, 1 )", iter ) ) )
                  ,eval( parse( text = sprintf( "numericInput( 'sizeHMapR%s', 'Height of the Map in %%', value = 100, 1, 300, 1 )", iter ) ) )
                  ,hr()
                  ,eval( parse( text = sprintf( "downloadButton( 'saveRhoR%s', 'Save Number of clusters' )", iter ) ) )
                  ,br(),br()
                  ,eval( parse( text = sprintf( "selectInput( 'typeRhoR%s', 'Type Rho', choices = c('png','pdf','jpeg','eps','tex','tiff','bmp'),
                                                selected = 'png' )", iter ) ) )
                  ,eval( parse( text = sprintf( "numericInput( 'widthRhoR%s', 'Width Rho', value = 10, 1, 30, 0.1 )", iter ) ) )
                  ,eval( parse( text = sprintf( "numericInput( 'heightRhoR%s', 'Height Rho', value = 10, 1, 30, 0.1 )", iter ) ) )
                  ,eval( parse( text = sprintf( "selectInput( 'unitRhoR%s', 'Unit Rho', choices = c('cm','in','mm','px'), selected = 'cm' )", iter ) ) )
                  ,hr()
                  ,eval( parse( text = sprintf( "downloadButton( 'saveAvgRR%s', 'Save Row Means' )", iter ) ) )
                  ,br(),br()
                  ,eval( parse( text = sprintf( "downloadButton( 'saveAvgCR%s', 'Save Col Means' )", iter ) ) )
                  ,br(),br()
                  ,eval( parse( text = sprintf( "selectInput( 'typeAvgR%s', 'Type Means', choices = c('png','pdf','jpeg','eps','tex','tiff','bmp'),
                                                selected = 'png' )", iter ) ) )
                  ,eval( parse( text = sprintf( "numericInput( 'widthAvgR%s', 'Width Means', value = 10, 1, 30, 0.1 )", iter ) ) )
                  ,eval( parse( text = sprintf( "numericInput( 'heightAvgR%s', 'Height Means', value = 10, 1, 30, 0.1 )", iter ) ) )
                  ,eval( parse( text = sprintf( "selectInput( 'unitAvgR%s', 'Unit Means', choices = c('cm','in','mm','px'), selected = 'cm' )", iter ) ) )
                  ,hr()
                  ,eval( parse( text = sprintf( "downloadButton( 'saveMapR%s', 'Save Map' )", iter ) ) )
                  ,br(),br()
                  ,eval( parse( text = sprintf( "selectInput( 'typeMapR%s', 'Type Map', choices = c('png','pdf','jpeg','eps','tex','tiff','bmp'),
                                                selected = 'png' )", iter ) ) )
                  ,eval( parse( text = sprintf( "numericInput( 'widthMapR%s', 'Width Map', value = 10, 1, 30, 0.1 )", iter ) ) )
                  ,eval( parse( text = sprintf( "numericInput( 'heightMapR%s', 'Height Map', value = 10, 1, 30, 0.1 )", iter ) ) )
                  ,eval( parse( text = sprintf( "selectInput( 'unitMapR%s', 'Unit Map', choices = c('cm','in','mm','px'), selected = 'cm' )", iter ) ) )
                  ,width = 2
                )
                ,mainPanel(
                  fluidRow(
                    fluidRow( style = 'padding-bottom:50px;',
                      div( plotOutput( sprintf( "RhoAggTestRawOut%s", whichGCAtab[[ iter ]] ), width = "50%" ),
                           align = "center" )
                    )
                    ,fluidRow( style = 'border-top: 1px dashed gray; border-bottom: 1px dashed gray;
                               padding-top:50px; padding-bottom:50px;',
                      column(
                        plotOutput( sprintf( "AvgPlotRowRawOut%s", whichGCAtab[[ iter ]] ) )
                        ,width = 6
                      )
                      ,column(
                        plotOutput( sprintf( "AvgPlotColRawOut%s", whichGCAtab[[ iter ]] ) )
                        ,width = 6
                      )
                    )
                    ,fluidRow( style = 'padding-top:50px;',
                      div( plotOutput( sprintf( "MapsGCArhoRawOut%s", whichGCAtab[[ iter ]] ) ), align = "center" )
                    )
                  )
                  ,width = 10
                )
            ) ), select = TRUE )

          # Save Rho
          eval( parse( text = sprintf(
            "output$saveRhoR%s <- downloadHandler(
              filename = function(){
                paste0('RhoClustN-', Sys.time(), '.', input$typeRhoR%s )
              },
              content = function( file ){
                ggsave( file, plot = RhoAggTestListRo[[ iter ]], device = input$typeRhoR%s,
                width = input$widthRhoR%s, height = input$heightRhoR%s, units = input$unitRhoR%s )
              }
            )", iter, iter, iter, iter, iter, iter
          ) ) )

          # Save Mean
          eval( parse( text = sprintf(
            "output$saveAvgRR%s <- downloadHandler(
              filename = function(){
                paste0('MeanRow-', Sys.time(), '.', input$typeAvgR%s )
              },
              content = function( file ){
                ggsave( file, plot = AvgPlotRowListRo[[ iter ]], device = input$typeAvgR%s,
                width = input$widthAvgR%s, height = input$heightAvgR%s, units = input$unitAvgR%s )
              }
            )", iter, iter, iter, iter, iter, iter
          ) ) )
          eval( parse( text = sprintf(
            "output$saveAvgCR%s <- downloadHandler(
              filename = function(){
                paste0('MeanCol-', Sys.time(), '.', input$typeAvgR%s )
              },
              content = function( file ){
                ggsave( file, plot = AvgPlotColListRo[[ iter ]], device = input$typeAvgR%s,
                width = input$widthAvgR%s, height = input$heightAvgR%s, units = input$unitAvgR%s )
              }
            )", iter, iter, iter, iter, iter, iter
          ) ) )

          # Save Map R
          eval( parse( text = sprintf(
            "output$saveMapR%s <- downloadHandler(
              filename = function(){
                paste0('MapGCA-', Sys.time(), '.', input$typeMapR%s )
              },
              content = function( file ){
                ggsave( file, plot = OverMapListRo[[ iter ]], device = input$typeMapR%s,
                width = input$widthMapR%s, height = input$heightMapR%s, units = input$unitMapR%s )
              }
            )", iter, iter, iter, iter, iter, iter
          ) ) )

          # Insert table into the Shiny output
          OverMapListRao[[ iter ]] <<- OverMapListRa[[ iter ]] <<- OverMapAgg( session, iter, "A", OverMap( OverReep( RemoveClWe( TablesGCAList[[ whichGCAtab[[ iter ]] ]] ) ), 5 ) )
          eval( parse( text = sprintf( "output$MapsGCArhoRawOutA%s <- renderPlot( OverMapListRao[[ iter ]] )",
                                       whichGCAtab[[ iter ]] ) ) )

          # Create a new tab panel with the Over-representation Map
          appendTab( "MapsA",
            tabPanel( whichGCAtab[[ iter ]],
            sidebarLayout(
             sidebarPanel(
                eval( parse( text = sprintf( "numericInput( 'cutsMapA%s', 'Number of intervals', value = 5, 1, 30, 1 )", iter ) ) )
               ,eval( parse( text = sprintf( "numericInput( 'sizeXMapA%s', 'X font size Map', value = 10, 1, 30, 1 )", iter ) ) )
               ,eval( parse( text = sprintf( "numericInput( 'angleXMapA%s', 'X font angle Map', value = 0, 0, 90, 1 )", iter ) ) )
               ,eval( parse( text = sprintf( "numericInput( 'sizeYMapA%s', 'Y font size Map', value = 10, 1, 30, 1 )", iter ) ) )
               ,eval( parse( text = sprintf( "numericInput( 'angleYMapA%s', 'Y font angle Map', value = 0, 0, 90, 1 )", iter ) ) )
               ,eval( parse( text = sprintf( "numericInput( 'sizeLMapA%s', 'Legend font size Map', value = 10, 1, 30, 1 )", iter ) ) )
               ,eval( parse( text = sprintf( "numericInput( 'sizeWMapA%s', 'Width of the Map in %%', value = 100, 1, 300, 1 )", iter ) ) )
               ,eval( parse( text = sprintf( "numericInput( 'sizeHMapA%s', 'Height of the Map in %%', value = 100, 1, 300, 1 )", iter ) ) )
               ,hr()
               ,eval( parse( text = sprintf( "downloadButton( 'saveMapA%s', 'Save Map' )", iter ) ) )
               ,br(),br()
               ,eval( parse( text = sprintf( "selectInput( 'typeMapA%s', 'Type Map', choices = c('png','pdf','jpeg','eps','tex','tiff','bmp'),
                            selected = 'png' )", iter ) ) )
               ,eval( parse( text = sprintf( "numericInput( 'widthMapA%s', 'Width Map', value = 10, 1, 30, 0.1 )", iter ) ) )
               ,eval( parse( text = sprintf( "numericInput( 'heightMapA%s', 'Height Map', value = 10, 1, 30, 0.1 )", iter ) ) )
               ,eval( parse( text = sprintf( "selectInput( 'unitMapA%s', 'Unit Map', choices = c('cm','in','mm','px'), selected = 'cm' )", iter ) ) )
               ,width = 2
             )
             ,mainPanel(
               fluidRow(
                fluidRow( style = 'padding-top:50px;',
                   div( plotOutput( sprintf( "MapsGCArhoRawOutA%s", whichGCAtab[[ iter ]] ) ), align = "center" )
                 )
               )
               ,width = 10
             )
            ) ), select = TRUE )

          # Save Map A
          eval( parse( text = sprintf(
            "output$saveMapA%s <- downloadHandler(
              filename = function(){
                paste0('MapGCAagg-', Sys.time(), '.', input$typeMapA%s )
              },
              content = function( file ){
                ggsave( file, plot = OverMapListRao[[ iter ]], device = input$typeMapA%s,
                width = input$widthMapA%s, height = input$heightMapA%s, units = input$unitMapA%s )
              }
            )", iter, iter, iter, iter, iter, iter
          ) ) )

        }else{

          # Insert table into the Shiny output
          eval( parse( text = sprintf( "output$TablesGCAOut%s <- renderDT( TablesGCAList[[ whichGCAtab[[ iter ]] ]] )",
                                       whichGCAtab[[ iter ]] ) ) )

          # Insert map into the Shiny output
          eval( parse( text = sprintf( "output$MapsGCArhoRawOut%s <- renderPlot( OverMapListR[[ whichGCAtab[[ iter ]] ]] )",
                                        whichGCAtab[[ iter ]] ) ) )
        }

      })

      #### Add clustering lines Raw GCA tables ####
      observeEvent( eval( parse( text = sprintf( "input$clustNrowR%s", as.numeric( input$MapsR ) ) ) ), {

        req( !is.null( input$MapsR ) )
        iter <- as.numeric( input$MapsR )
        cuts1 <- eval( parse( text = sprintf( "input$cutsMapR%s", iter ) ) )

        # Number of clusters
        n <- eval( parse( text = sprintf( "input$clustNrowR%s", iter ) ) )

        # Table dimension
        rowN <- nrow( TablesGCAList[[ whichGCAtab[[ iter ]] ]] )-2
        N <- max( 1, min( n, rowN ) )

        TablesGCAList[[ whichGCAtab[[ iter ]] ]] <<- Cluster( TablesGCAList[[ whichGCAtab[[ iter ]] ]], N = N, byRow = TRUE )
        TablesRawList[[ whichGCAtab[[ iter ]] ]] <<- ClusterChange( TablesGCAList[[ whichGCAtab[[ iter ]] ]],
                                                                    TablesRawList[[ whichGCAtab[[ iter ]] ]], N = N, byRow = TRUE )

        # Insert lines into the Shiny output
        temp <- OverMapListR[[ iter ]]$clusN[2]
        OverMapListR[[ iter ]] <<- OverMap( OverReep( RemoveClWe( TablesGCAList[[ whichGCAtab[[ iter ]] ]] ) ), cuts1 )
        OverMapListR[[ iter ]]$clusN[1] <<- N
        OverMapListR[[ iter ]]$clusN[2] <<- temp
        OverMapListRo[[ iter ]] <<- AddClustOverMap( session, iter, "R", OverMapListR[[ iter ]] ,
                                                     TablesGCAList[[ whichGCAtab[[ iter ]] ]], N, byRow = TRUE )

        W1 <- 1500*(as.numeric( eval( parse( text = sprintf( "input$sizeWMapR%s", iter ) ) ) )/100)
        H1 <- 400*(as.numeric( eval( parse( text = sprintf( "input$sizeHMapR%s", iter ) ) ) )/100)
        eval( parse( text = sprintf( "output$MapsGCArhoRawOut%s <- renderPlot( OverMapListRo[[ iter ]],
                                     height = H1, width = W1 )",
                                     whichGCAtab[[ iter ]] ) ) )

        cuts2 <- eval( parse( text = sprintf( "input$cutsMapA%s", iter ) ) )
        if( is.null(cuts2) ){ cuts2 <- cuts1 }
        W2 <- 1500*(as.numeric( eval( parse( text = sprintf( "input$sizeWMapA%s", iter ) ) ) )/100)
        H2 <- 400*(as.numeric( eval( parse( text = sprintf( "input$sizeHMapA%s", iter ) ) ) )/100)
        if( length(W2) == 0 ){ W2 <- W1; H2 <- H1 }

        OverMapListRa[[ iter ]] <<- AggTableDual( TablesGCAList[[ whichGCAtab[[ iter ]] ]], N, temp )$GCA
        TablesRawAggList[[ iter ]] <<- RemoveClWe( AggTableDualRaw( TablesRawList[[ whichGCAtab[[ iter ]] ]], N, temp )$In )

        OverMapListRao[[ iter ]] <<- OverMapAgg( session, iter, "A", OverMap( OverReep( RemoveClWe( OverMapListRa[[ iter ]] ) ), cuts2 ) )
        eval( parse( text = sprintf( "output$MapsGCArhoRawOutA%s <- renderPlot( OverMapListRao[[ iter ]],
                                     height = H2, width = W2 )",
                                     whichGCAtab[[ iter ]] ) ) )

        # Display Averages for clusters
        if( N == 1 ){
          # AvgPlotRowListRo[[ iter ]] <<- MeanStdPlot( session, iter, 'R', TableGCA( AggTable( TablesGCAList[[ whichGCAtab[[ iter ]] ]], TRUE, rep( 1, rowN ), 0 ) ), TRUE )
          AvgPlotRowListRo[[ iter ]] <<- MeanStdPlot( session, iter, 'R', TableGCA( AggTableRaw( TablesRawList[[ whichGCAtab[[ iter ]] ]], TRUE, rep( 1, rowN ), 0 ) ), TRUE )
        }else{
          # AvgPlotRowListRo[[ iter ]] <<- MeanStdPlot( session, iter, 'R', TableGCA( AggTable( TablesGCAList[[ whichGCAtab[[ iter ]] ]], TRUE, NULL, N-1 ) ), TRUE )
          AvgPlotRowListRo[[ iter ]] <<- MeanStdPlot( session, iter, 'R', TableGCA( AggTableRaw( TablesRawList[[ whichGCAtab[[ iter ]] ]], TRUE, NULL, N-1 ) ), TRUE )
        }
        eval( parse( text = sprintf( "output$AvgPlotRowRawOut%s <- renderPlot( AvgPlotRowListRo[[ iter ]] )",
                                     whichGCAtab[[ iter ]] ) ) )

        # Insert table into the Shiny output
        eval( parse( text = sprintf( "output$TablesGCAOut%s <- renderDT( TablesGCAList[[ whichGCAtab[[ iter ]] ]], options = list( dom = 't', pageLength = 10000 ) )",
                                     whichGCAtab[[ iter ]] ) ) )

        eval( parse( text = sprintf( "output$TablesInOut%s <- renderDT( TablesRawList[[ whichGCAtab[[ iter ]] ]] )", whichGCAtab[[ iter ]] ) ) )
        eval( parse( text = sprintf( "output$TablesInAOut%s <- renderDT( TablesRawAggList[[ whichGCAtab[[ iter ]] ]] )", iter ) ) )

      })
      observeEvent( eval( parse( text = sprintf( "input$clustNcolR%s", as.numeric( input$MapsR ) ) ) ), {

        req( !is.null( input$MapsR ) )
        iter <- as.numeric( input$MapsR )
        cuts1 <- eval( parse( text = sprintf( "input$cutsMapR%s", iter ) ) )

        # Number of clusters
        n <- eval( parse( text = sprintf( "input$clustNcolR%s", iter ) ) )

        # Table dimension
        colN <- ncol( TablesGCAList[[ whichGCAtab[[ iter ]] ]] )-2
        N <- max( 1, min( n, colN ) )

        TablesGCAList[[ whichGCAtab[[ iter ]] ]] <<- Cluster( TablesGCAList[[ whichGCAtab[[ iter ]] ]], N = N, byRow = FALSE )
        TablesRawList[[ whichGCAtab[[ iter ]] ]] <<- ClusterChange( TablesGCAList[[ whichGCAtab[[ iter ]] ]],
                                                                    TablesRawList[[ whichGCAtab[[ iter ]] ]], N = N, byRow = FALSE )

        # Insert lines into the Shiny output
        temp <- OverMapListR[[ iter ]]$clusN[1]
        OverMapListR[[ iter ]] <<- OverMap( OverReep( RemoveClWe( TablesGCAList[[ whichGCAtab[[ iter ]] ]] ) ), cuts1 )
        OverMapListR[[ iter ]]$clusN[2] <<- N
        OverMapListR[[ iter ]]$clusN[1] <<- temp
        OverMapListRo[[ iter ]] <<- AddClustOverMap( session, iter, "R", OverMapListR[[ iter ]] ,
                                                     TablesGCAList[[ whichGCAtab[[ iter ]] ]], N, byRow = FALSE )

        W1 <- 1500*(as.numeric( eval( parse( text = sprintf( "input$sizeWMapR%s", iter ) ) ) )/100)
        H1 <- 400*(as.numeric( eval( parse( text = sprintf( "input$sizeHMapR%s", iter ) ) ) )/100)
        eval( parse( text = sprintf( "output$MapsGCArhoRawOut%s <- renderPlot( OverMapListRo[[ iter ]],
                                     height = H1, width = W1 )",
                                     whichGCAtab[[ iter ]] ) ) )

        cuts2 <- eval( parse( text = sprintf( "input$cutsMapA%s", iter ) ) )
        if( is.null(cuts2) ){ cuts2 <- cuts1 }
        W2 <- 1500*(as.numeric( eval( parse( text = sprintf( "input$sizeWMapA%s", iter ) ) ) )/100)
        H2 <- 400*(as.numeric( eval( parse( text = sprintf( "input$sizeHMapA%s", iter ) ) ) )/100)
        if( length(W2) == 0 ){ W2 <- W1; H2 <- H1 }

        OverMapListRa[[ iter ]] <<- AggTableDual( TablesGCAList[[ whichGCAtab[[ iter ]] ]], temp, N )$GCA
        TablesRawAggList[[ iter ]] <<- RemoveClWe( AggTableDualRaw( TablesRawList[[ whichGCAtab[[ iter ]] ]], temp, N )$In )

        OverMapListRao[[ iter ]] <<- OverMapAgg( session, iter, "A", OverMap( OverReep( RemoveClWe( OverMapListRa[[ iter ]] ) ), cuts2 ) )
        eval( parse( text = sprintf( "output$MapsGCArhoRawOutA%s <- renderPlot( OverMapListRao[[ iter ]],
                                     height = H2, width = W2 )",
                                     whichGCAtab[[ iter ]] ) ) )

        # Display Averages for clusters
        if( N == 1 ){
          # AvgPlotColListRo[[ iter ]] <<- MeanStdPlot( session, iter, 'R', TableGCA( AggTable( TablesGCAList[[ whichGCAtab[[ iter ]] ]], FALSE, rep( 1, colN ), 0 ) ), FALSE )
          AvgPlotColListRo[[ iter ]] <<- MeanStdPlot( session, iter, 'R', TableGCA( AggTableRaw( TablesRawList[[ whichGCAtab[[ iter ]] ]], FALSE, rep( 1, colN ), 0 ) ), FALSE )
        }else{
          # AvgPlotColListRo[[ iter ]] <<- MeanStdPlot( session, iter, 'R', TableGCA( AggTable( TablesGCAList[[ whichGCAtab[[ iter ]] ]], FALSE, NULL , N-1 ) ), FALSE )
          AvgPlotColListRo[[ iter ]] <<- MeanStdPlot( session, iter, 'R', TableGCA( AggTableRaw( TablesRawList[[ whichGCAtab[[ iter ]] ]], FALSE, NULL , N-1 ) ), FALSE )
        }
        eval( parse( text = sprintf( "output$AvgPlotColRawOut%s <- renderPlot( AvgPlotColListRo[[ iter ]] )",
                                     whichGCAtab[[ iter ]] ) ) )

        # Insert table into the Shiny output
        eval( parse( text = sprintf( "output$TablesGCAOut%s <- renderDT( TablesGCAList[[ whichGCAtab[[ iter ]] ]], options = list( dom = 't', pageLength = 10000 ) )",
                                     whichGCAtab[[ iter ]] ) ) )

        eval( parse( text = sprintf( "output$TablesInOut%s <- renderDT( TablesRawList[[ whichGCAtab[[ iter ]] ]] )", whichGCAtab[[ iter ]] ) ) )
        eval( parse( text = sprintf( "output$TablesInAOut%s <- renderDT( TablesRawAggList[[ whichGCAtab[[ iter ]] ]] )", iter ) ) )

      })
      observeEvent( list( eval( parse( text = sprintf( "input$sizeXMapR%s", as.numeric( input$MapsR ) ) ) ),
                          eval( parse( text = sprintf( "input$angleXMapR%s", as.numeric( input$MapsR ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeYMapR%s", as.numeric( input$MapsR ) ) ) ),
                          eval( parse( text = sprintf( "input$angleYMapR%s", as.numeric( input$MapsR ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeLMapR%s", as.numeric( input$MapsR ) ) ) ),
                          eval( parse( text = sprintf( "input$cutsMapR%s", as.numeric( input$MapsR ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeHMapR%s", as.numeric( input$MapsR ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeWMapR%s", as.numeric( input$MapsR ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeXAvgR%s", as.numeric( input$MapsR ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeYAvgR%s", as.numeric( input$MapsR ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeXlAvgR%s", as.numeric( input$MapsR ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeYlAvgR%s", as.numeric( input$MapsR ) ) ) ) ), {

        req( !is.null( input$MapsR ) )
        iter <- as.numeric( input$MapsR )
        cuts1 <- eval( parse( text = sprintf( "input$cutsMapR%s", iter ) ) )

        n1 <- eval( parse( text = sprintf( "input$clustNrowR%s", iter ) ) )
        n2 <- eval( parse( text = sprintf( "input$clustNcolR%s", iter ) ) )
        rowN <- nrow( TablesGCAList[[ whichGCAtab[[ iter ]] ]] )-2
        N1 <- max( 1, min( n1, rowN ) )
        colN <- ncol( TablesGCAList[[ whichGCAtab[[ iter ]] ]] )-2
        N2 <- max( 1, min( n2, colN ) )

        # Insert lines into the Shiny output
        OverMapListR[[ iter ]] <<- OverMap( OverReep( RemoveClWe( TablesGCAList[[ whichGCAtab[[ iter ]] ]] ) ), cuts1 )
        OverMapListR[[ iter ]]$clusN[1] <<- N1
        OverMapListR[[ iter ]]$clusN[2] <<- N2
        OverMapListRo[[ iter ]] <<- AddClustOverMap( session, iter, "R", OverMapListR[[ iter ]] ,
                                                     TablesGCAList[[ whichGCAtab[[ iter ]] ]], 1, byRow = TRUE )

        W1 <- 1500*(as.numeric( eval( parse( text = sprintf( "input$sizeWMapR%s", iter ) ) ) )/100)
        H1 <- 400*(as.numeric( eval( parse( text = sprintf( "input$sizeHMapR%s", iter ) ) ) )/100)
        eval( parse( text = sprintf( "output$MapsGCArhoRawOut%s <- renderPlot( OverMapListRo[[ iter ]],
                                     height = H1, width = W1 )",
                                     whichGCAtab[[ iter ]] ) ) )

        cuts2 <- eval( parse( text = sprintf( "input$cutsMapA%s", iter ) ) )
        if( is.null(cuts2) ){ cuts2 <- cuts1 }
        W2 <- 1500*(as.numeric( eval( parse( text = sprintf( "input$sizeWMapA%s", iter ) ) ) )/100)
        H2 <- 400*(as.numeric( eval( parse( text = sprintf( "input$sizeHMapA%s", iter ) ) ) )/100)
        if( length(W2) == 0 ){ W2 <- W1; H2 <- H1 }

        OverMapListRa[[ iter ]] <<- AggTableDual( TablesGCAList[[ whichGCAtab[[ iter ]] ]], N1, N2 )$GCA
        TablesRawAggList[[ iter ]] <<- RemoveClWe( AggTableDualRaw( TablesRawList[[ whichGCAtab[[ iter ]] ]], N1, N2 )$In )

        OverMapListRao[[ iter ]] <<- OverMapAgg( session, iter, "A", OverMap( OverReep( RemoveClWe( OverMapListRa[[ iter ]] ) ), cuts2 ) )
        eval( parse( text = sprintf( "output$MapsGCArhoRawOutA%s <- renderPlot( OverMapListRao[[ iter ]],
                                     height = H2, width = W2 )",
                                     whichGCAtab[[ iter ]] ) ) )

        # Display Averages for clusters
        if( N1 == 1 ){
          # AvgPlotRowListRo[[ iter ]] <<- MeanStdPlot( session, iter, 'R', TableGCA( AggTable( TablesGCAList[[ whichGCAtab[[ iter ]] ]], TRUE, rep( 1, rowN ), 0 ) ), TRUE )
          AvgPlotRowListRo[[ iter ]] <<- MeanStdPlot( session, iter, 'R', TableGCA( AggTableRaw( TablesRawList[[ whichGCAtab[[ iter ]] ]], TRUE, rep( 1, rowN ), 0 ) ), TRUE )
        }else{
          # AvgPlotRowListRo[[ iter ]] <<- MeanStdPlot( session, iter, 'R', TableGCA( AggTable( TablesGCAList[[ whichGCAtab[[ iter ]] ]], TRUE, NULL, N2-1 ) ), TRUE )
          AvgPlotRowListRo[[ iter ]] <<- MeanStdPlot( session, iter, 'R', TableGCA( AggTableRaw( TablesRawList[[ whichGCAtab[[ iter ]] ]], TRUE, NULL, N2-1 ) ), TRUE )
        }
        eval( parse( text = sprintf( "output$AvgPlotRowRawOut%s <- renderPlot( AvgPlotRowListRo[[ iter ]] )",
                                     whichGCAtab[[ iter ]] ) ) )
        if( N2 == 1 ){
          # AvgPlotColListRo[[ iter ]] <<- MeanStdPlot( session, iter, 'R', TableGCA( AggTable( TablesGCAList[[ whichGCAtab[[ iter ]] ]], FALSE, rep( 1, colN ), 0 ) ), FALSE )
          AvgPlotColListRo[[ iter ]] <<- MeanStdPlot( session, iter, 'R', TableGCA( AggTableRaw( TablesRawList[[ whichGCAtab[[ iter ]] ]], FALSE, rep( 1, colN ), 0 ) ), FALSE )
        }else{
          # AvgPlotColListRo[[ iter ]] <<- MeanStdPlot( session, iter, 'R', TableGCA( AggTable( TablesGCAList[[ whichGCAtab[[ iter ]] ]], FALSE, NULL , N2-1 ) ), FALSE )
          AvgPlotColListRo[[ iter ]] <<- MeanStdPlot( session, iter, 'R', TableGCA( AggTableRaw( TablesRawList[[ whichGCAtab[[ iter ]] ]], FALSE, NULL , N2-1 ) ), FALSE )
        }
        eval( parse( text = sprintf( "output$AvgPlotColRawOut%s <- renderPlot( AvgPlotColListRo[[ iter ]] )",
                                     whichGCAtab[[ iter ]] ) ) )

      })
      observeEvent( list( eval( parse( text = sprintf( "input$sizeXClustR%s", as.numeric( input$MapsR ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeYClustR%s", as.numeric( input$MapsR ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeXlClustR%s", as.numeric( input$MapsR ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeYlClustR%s", as.numeric( input$MapsR ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeLClustR%s", as.numeric( input$MapsR ) ) ) ) ), {

        req( !is.null( input$MapsR ) )
        iter <- as.numeric( input$MapsR )

        RhoAggTestListRo[[ iter ]] <<- RhoPlot( session, iter, 'R', RhoAggTestListR[[ iter ]] )
        eval( parse( text = sprintf( "output$RhoAggTestRawOut%s <- renderPlot( RhoAggTestListRo[[ iter ]] )",
                                     whichGCAtab[[ iter ]] ) ) )

      })
      observeEvent( list( eval( parse( text = sprintf( "input$sizeXMapA%s", as.numeric( input$MapsA ) ) ) ),
                          eval( parse( text = sprintf( "input$angleXMapA%s", as.numeric( input$MapsA ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeYMapA%s", as.numeric( input$MapsA ) ) ) ),
                          eval( parse( text = sprintf( "input$angleYMapA%s", as.numeric( input$MapsA ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeLMapA%s", as.numeric( input$MapsA ) ) ) ),
                          eval( parse( text = sprintf( "input$cutsMapA%s", as.numeric( input$MapsA ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeHMapA%s", as.numeric( input$MapsA ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeWMapA%s", as.numeric( input$MapsA ) ) ) ) ), {

        req( !is.null( input$MapsA ) )
        iter <- as.numeric( input$MapsA )
        cuts <- eval( parse( text = sprintf( "input$cutsMapA%s", iter ) ) )

        n1 <- eval( parse( text = sprintf( "input$clustNrowR%s", iter ) ) )
        n2 <- eval( parse( text = sprintf( "input$clustNcolR%s", iter ) ) )
        rowN <- nrow( TablesGCAList[[ whichGCAtab[[ iter ]] ]] )-2
        N1 <- max( 1, min( n1, rowN ) )
        colN <- ncol( TablesGCAList[[ whichGCAtab[[ iter ]] ]] )-2
        N2 <- max( 1, min( n2, colN ) )

        W <- 1500*(as.numeric( eval( parse( text = sprintf( "input$sizeWMapA%s", iter ) ) ) )/100)
        H <- 400*(as.numeric( eval( parse( text = sprintf( "input$sizeHMapA%s", iter ) ) ) )/100)

        OverMapListRa[[ iter ]] <<- AggTableDual( TablesGCAList[[ whichGCAtab[[ iter ]] ]], N1, N2 )$GCA
        TablesRawAggList[[ iter ]] <<- RemoveClWe( AggTableDualRaw( TablesRawList[[ whichGCAtab[[ iter ]] ]], N1, N2 )$In )

        OverMapListRao[[ iter ]] <<- OverMapAgg( session, iter, "A", OverMap( OverReep( RemoveClWe( OverMapListRa[[ iter ]] ) ), cuts ) )
        eval( parse( text = sprintf( "output$MapsGCArhoRawOutA%s <- renderPlot( OverMapListRao[[ iter ]],
                                      height = H, width = W )",
                                      whichGCAtab[[ iter ]] ) ) )

      })

      #### Start GCA Table ordering using Rho Spearman ####
      observeEvent( eval( parse( text = sprintf( "input$startGCArho%s", as.numeric( input$TableG ) ) ) ), {
        # whichTabGCArhoReact( as.numeric( input$TableR ) )
        iter <- as.numeric( input$TableG )

        req( if( length( startGCArhoList ) < iter ){ TRUE }else{
          if( is.null( startGCArhoList[[ iter ]] ) ){ TRUE }else{ startGCArhoList[[ iter ]] != iter } } )

        startGCArhoList[[ iter ]] <<- iter

        req( !editTable )

        # Ordering using Rho Spearman
        GCArhoOrd[[ iter ]] <<- GCA( RemoveClWe( TablesGCAList[[ iter ]] ), as.numeric( eval( parse( text = sprintf( "input$GCArhoIter%s", iter ) ) ) ) )

        # Insert output table into the Shiny output
        eval( parse( text = sprintf( "output$TablesGCArhoOrd%s <- renderDT( GCArhoOrd[[ iter ]] )", iter ) ) )

        # Create a new tab panel with the constant Grade table
        appendTab( "TableO",
          tabPanel( whichGCAtab[[ iter ]],
            sidebarLayout(
              sidebarPanel(
                eval( parse( text = sprintf( "selectInput( 'GCArhoRes%s', 'Choose Result',
                                             choices = as.list( 1:nrow(GCArhoOrd[[ iter ]]) ), selected = '1' )", iter ) ) )
                ,eval( parse( text = sprintf( "actionButton( 'chooseGCArhoRes%s', 'Accept Result' )", iter ) ) )
                ,hr()
                ,eval( parse( text = sprintf( "downloadButton( 'saveO%s', 'Save Table' )", iter ) ) )
                ,width = 2
              )
              ,mainPanel(
                DTOutput( sprintf( "TablesGCArhoOrd%s", whichGCAtab[[ iter ]] ) )
                ,width = 10
              )
            ) ), select = TRUE )

        # Save Table
        eval( parse( text = sprintf(
          "output$saveO%s <- downloadHandler(
            filename = function(){
              paste0('SortResTable-', Sys.time(), '.txt')
            },
            content = function( file ){
              write.table( GCArhoOrd[[ iter ]], file, sep = ';' )
            }
          )", iter
        ) ) )

      })

      #### Choose GCA Table ordering based Rho Spearman ####
      observeEvent( eval( parse( text = sprintf( "input$chooseGCArhoRes%s", as.numeric( input$TableO ) ) ) ), {

        iter <- as.numeric( input$TableO )

        # Determine result chosen by the User
        whichRes <- eval( parse( text = sprintf( "input$GCArhoRes%s", iter ) ) )

        req( if( length( GCArhoList ) < iter ){ TRUE }else{
               if( is.null( GCArhoList[[ iter ]] ) ){ TRUE }else{ GCArhoList[[ iter ]] != as.numeric( whichRes ) } } )

        GCArhoList[[ iter ]] <<- as.numeric( whichRes )

        req( !editTable )

        # Determine GCA Table dimensions
        Rows <- 1:(nrow(TablesGCAList[[ iter ]])-2)
        Cols <- max(Rows) + 1:(ncol(TablesGCAList[[ iter ]])-2)

        # Input re-ordered GCA Table into global table list
        TablesGCAordList[[ iter ]] <<- TableReorder( TablesGCAList[[ iter ]],
                                                     GCArhoOrd[[ iter ]][ whichRes, Rows ],
                                                     GCArhoOrd[[ iter ]][ whichRes, Cols ] )

        TablesordList[[ iter ]] <<- ClusterOrdChange( TablesGCAordList[[ iter ]], TablesRawList[[ iter ]] )
        TablesRawAggordList[[ iter ]] <<- RemoveClWe( AggTableDualRaw( TablesordList[[ iter ]], 1, 1 )$In )

        # Insert table into the Shiny output
        eval( parse( text = sprintf( "output$TablesGCAordOut%s <- renderDT( TablesGCAordList[[ iter ]] )", iter ) ) )

        # Append new tab with re-ordered GCA Table
        appendTab( "TableS",
          tabPanel( iter,
          sidebarLayout(
            sidebarPanel(
              eval( parse( text = sprintf( "downloadButton( 'saveS%s', 'Save Table' )", iter ) ) )
              ,width = 2
            )
            ,mainPanel(
              DTOutput( sprintf( "TablesGCAordOut%s", iter ) )
              ,width = 10
            )
          ) ), select = TRUE )

        # Save Table
        eval( parse( text = sprintf(
          "output$saveS%s <- downloadHandler(
            filename = function(){
              paste0('GCAsortTable-', Sys.time(), '.txt')
            },
            content = function( file ){
              write.table( TablesGCAordList[[ iter ]], file, sep = ';' )
            }
          )", iter
        ) ) )

        # Insert table into the Shiny output
        eval( parse( text = sprintf( "output$TablesordOut%s <- renderDT( TablesordList[[ iter ]] )", iter ) ) )

        appendTab( "TableIS",
          tabPanel( iter,
          sidebarLayout(
            sidebarPanel(
              eval( parse( text = sprintf( "downloadButton( 'saveIS%s', 'Save Table' )", iter ) ) )
              ,width = 2
            )
            ,mainPanel(
              DTOutput( sprintf( "TablesordOut%s", iter ) )
              ,width = 10
            )
        ) ), select = TRUE )

        # Save Table
        eval( parse( text = sprintf(
          "output$saveIS%s <- downloadHandler(
            filename = function(){
              paste0('InputsortTable-', Sys.time(), '.txt')
            },
            content = function( file ){
              write.table( TablesordList[[ iter ]], file, sep = ';' )
            }
          )", iter
        ) ) )

        # Insert table into the Shiny output
        eval( parse( text = sprintf( "output$TablesAggordOut%s <- renderDT( TablesRawAggordList[[ iter ]] )", iter ) ) )

        appendTab( "TableISA",
         tabPanel( iter,
           sidebarLayout(
             sidebarPanel(
               eval( parse( text = sprintf( "downloadButton( 'saveISA%s', 'Save Table' )", iter ) ) )
               ,width = 2
             )
             ,mainPanel(
               DTOutput( sprintf( "TablesAggordOut%s", iter ) )
               ,width = 10
             )
           ) ), select = TRUE )

        # Save Table
        eval( parse( text = sprintf(
          "output$saveISA%s <- downloadHandler(
            filename = function(){
              paste0('InputAggsortTable-', Sys.time(), '.txt')
            },
            content = function( file ){
              write.table( TablesRawAggordList[[ iter ]], file, sep = ';' )
            }
          )", iter
        ) ) )

        # Insert table into the Shiny output
        OverMapListSo[[ iter ]] <<- OverMapListS[[ iter ]] <<- OverMap( OverReep( RemoveClWe( TablesGCAordList[[ iter ]] ) ), 5 )
        eval( parse( text = sprintf( "output$MapsGCArhoOrdOut%s <- renderPlot( OverMapListS[[ iter ]] )",
                                     iter ) ) )

        # Calculate Rho Spearman for different number of clusters
        RhoAggTestListS[[ iter ]] <<- RhoAggTest( TablesGCAordList[[ iter ]] )
        RhoAggTestListSo[[ iter ]] <<- RhoPlot( session, iter, 'S', RhoAggTestListS[[ iter ]] )
        eval( parse( text = sprintf( "output$RhoAggTestOrdOut%s <- renderPlot( RhoAggTestListSo[[ iter ]] )",
                                     iter ) ) )

        # Table dimension
        rowN <- nrow( TablesGCAordList[[ iter ]] )-2
        colN <- ncol( TablesGCAordList[[ iter ]] )-2

        # Display Averages for clusters
        AvgPlotRowListSo[[ iter ]] <<- MeanStdPlot( session, iter, 'S', TableGCA( AggTable( TablesGCAordList[[ iter ]], TRUE, rep( 1, rowN ), 0 ) ), TRUE )
        AvgPlotColListSo[[ iter ]] <<- MeanStdPlot( session, iter, 'S', TableGCA( AggTable( TablesGCAordList[[ iter ]], FALSE, rep( 1, colN ), 0 ) ), FALSE )

        eval( parse( text = sprintf( "output$AvgPlotRowOrdOut%s <- renderPlot( AvgPlotRowListSo[[ iter ]] )",
                                     iter ) ) )
        eval( parse( text = sprintf( "output$AvgPlotColOrdOut%s <- renderPlot( AvgPlotColListSo[[ iter ]] )",
                                     iter ) ) )

        # Create a new tab panel with the Over-representation Map
        appendTab( "MapsS",
          tabPanel( iter,
            sidebarLayout(
             sidebarPanel(
               eval( parse( text = sprintf( "numericInput( 'sizeXClustS%s', 'X font size Clusters', value = 10, 1, 30, 1 )", iter ) ) )
               ,eval( parse( text = sprintf( "numericInput( 'sizeYClustS%s', 'Y font size Clusters', value = 10, 1, 30, 1 )", iter ) ) )
               ,eval( parse( text = sprintf( "numericInput( 'sizeXlClustS%s', 'Label X font size Clusters', value = 10, 1, 30, 1 )", iter ) ) )
               ,eval( parse( text = sprintf( "numericInput( 'sizeYlClustS%s', 'Label Y font size Clusters', value = 10, 1, 30, 1 )", iter ) ) )
               ,eval( parse( text = sprintf( "numericInput( 'sizeLClustS%s', 'Legend font size Clusters', value = 10, 1, 30, 1 )", iter ) ) )
               ,hr()
               ,eval( parse( text = sprintf( "numericInput( 'sizeXAvgS%s', 'X font size Mean', value = 10, 1, 30, 1 )", iter ) ) )
               ,eval( parse( text = sprintf( "numericInput( 'sizeYAvgS%s', 'Y font size Mean', value = 10, 1, 30, 1 )", iter ) ) )
               ,eval( parse( text = sprintf( "numericInput( 'sizeXlAvgS%s', 'Label X font size Mean', value = 10, 1, 30, 1 )", iter ) ) )
               ,eval( parse( text = sprintf( "numericInput( 'sizeYlAvgS%s', 'Label Y font size Mean', value = 10, 1, 30, 1 )", iter ) ) )
               ,hr()
               ,eval( parse( text = sprintf( "numericInput( 'clustNrowS%s', 'Number of clusters for rows',
                                                value = 1, 1, rowN, 1 )", iter ) ) )
               ,br()
               ,eval( parse( text = sprintf( "numericInput( 'clustNcolS%s', 'Number of clusters for columns',
                                                 value = 1, 1, colN, 1 )", iter ) ) )
               ,hr()
               ,eval( parse( text = sprintf( "numericInput( 'cutsMapS%s', 'Number of intervals', value = 5, 1, 30, 1 )", iter ) ) )
               ,eval( parse( text = sprintf( "numericInput( 'sizeXMapS%s', 'X font size Map', value = 10, 1, 30, 1 )", iter ) ) )
               ,eval( parse( text = sprintf( "numericInput( 'angleXMapS%s', 'X font angle Map', value = 0, 0, 90, 1 )", iter ) ) )
               ,eval( parse( text = sprintf( "numericInput( 'sizeYMapS%s', 'Y font size Map', value = 10, 1, 30, 1 )", iter ) ) )
               ,eval( parse( text = sprintf( "numericInput( 'angleYMapS%s', 'Y font angle Map', value = 0, 0, 90, 1 )", iter ) ) )
               ,eval( parse( text = sprintf( "numericInput( 'sizeLMapS%s', 'Legend font size Map', value = 10, 1, 30, 1 )", iter ) ) )
               ,eval( parse( text = sprintf( "numericInput( 'sizeWMapS%s', 'Width of the Map in %%', value = 100, 1, 300, 1 )", iter ) ) )
               ,eval( parse( text = sprintf( "numericInput( 'sizeHMapS%s', 'Height of the Map in %%', value = 100, 1, 300, 1 )", iter ) ) )
               ,hr()
               ,eval( parse( text = sprintf( "downloadButton( 'saveRhoS%s', 'Save Number of clusters' )", iter ) ) )
               ,br(),br()
               ,eval( parse( text = sprintf( "selectInput( 'typeRhoS%s', 'Type Rho', choices = c('png','pdf','jpeg','eps','tex','tiff','bmp'),
                                                selected = 'png' )", iter ) ) )
               ,eval( parse( text = sprintf( "numericInput( 'widthRhoS%s', 'Width Rho', value = 10, 1, 30, 0.1 )", iter ) ) )
               ,eval( parse( text = sprintf( "numericInput( 'heightRhoS%s', 'Height Rho', value = 10, 1, 30, 0.1 )", iter ) ) )
               ,eval( parse( text = sprintf( "selectInput( 'unitRhoS%s', 'Unit Rho', choices = c('cm','in','mm','px'), selected = 'cm' )", iter ) ) )
               ,hr()
               ,eval( parse( text = sprintf( "downloadButton( 'saveAvgRS%s', 'Save Row Means' )", iter ) ) )
               ,br(),br()
               ,eval( parse( text = sprintf( "downloadButton( 'saveAvgCS%s', 'Save Col Means' )", iter ) ) )
               ,br(),br()
               ,eval( parse( text = sprintf( "selectInput( 'typeAvgS%s', 'Type Means', choices = c('png','pdf','jpeg','eps','tex','tiff','bmp'),
                                                selected = 'png' )", iter ) ) )
               ,eval( parse( text = sprintf( "numericInput( 'widthAvgS%s', 'Width Means', value = 10, 1, 30, 0.1 )", iter ) ) )
               ,eval( parse( text = sprintf( "numericInput( 'heightAvgS%s', 'Height Means', value = 10, 1, 30, 0.1 )", iter ) ) )
               ,eval( parse( text = sprintf( "selectInput( 'unitAvgS%s', 'Unit Means', choices = c('cm','in','mm','px'), selected = 'cm' )", iter ) ) )
               ,hr()
               ,eval( parse( text = sprintf( "downloadButton( 'saveMapS%s', 'Save Map' )", iter ) ) )
               ,br(),br()
               ,eval( parse( text = sprintf( "selectInput( 'typeMapS%s', 'Type Map', choices = c('png','pdf','jpeg','eps','tex','tiff','bmp'),
                                                selected = 'png' )", iter ) ) )
               ,eval( parse( text = sprintf( "numericInput( 'widthMapS%s', 'Width Map', value = 10, 1, 30, 0.1 )", iter ) ) )
               ,eval( parse( text = sprintf( "numericInput( 'heightMapS%s', 'Height Map', value = 10, 1, 30, 0.1 )", iter ) ) )
               ,eval( parse( text = sprintf( "selectInput( 'unitMapS%s', 'Unit Map', choices = c('cm','in','mm','px'), selected = 'cm' )", iter ) ) )
               ,width = 2
             )
             ,mainPanel(
               fluidRow(
                 fluidRow( style = 'padding-bottom:50px;',
                   div( plotOutput( sprintf( "RhoAggTestOrdOut%s", iter ), width = "50%" ),
                        align = "center" )
                 )
                 ,fluidRow( style = 'border-top: 1px dashed gray; border-bottom: 1px dashed gray;
                            padding-top:50px; padding-bottom:50px;',
                    column(
                      plotOutput( sprintf( "AvgPlotRowOrdOut%s", iter ) )
                      ,width = 6
                    )
                    ,column(
                      plotOutput( sprintf( "AvgPlotColOrdOut%s", iter ) )
                      ,width = 6
                    )
                 )
                 ,fluidRow( style = 'padding-top:50px;',
                   div( plotOutput( sprintf( "MapsGCArhoOrdOut%s", iter ) ), align = "center" )
                 )
               )
               ,width = 10
             )
          ) ), select = TRUE )

        # Save Rho
        eval( parse( text = sprintf(
          "output$saveRhoS%s <- downloadHandler(
            filename = function(){
              paste0('RhoClustN-', Sys.time(), '.', input$typeRhoS%s )
            },
            content = function( file ){
              ggsave( file, plot = RhoAggTestListSo[[ iter ]], device = input$typeRhoS%s,
              width = input$widthRhoS%s, height = input$heightRhoS%s, units = input$unitRhoS%s )
            }
          )", iter, iter, iter, iter, iter, iter
        ) ) )

        # Save Mean
        eval( parse( text = sprintf(
          "output$saveAvgRS%s <- downloadHandler(
            filename = function(){
              paste0('MeanRow-', Sys.time(), '.', input$typeAvgS%s )
            },
            content = function( file ){
              ggsave( file, plot = AvgPlotRowListSo[[ iter ]], device = input$typeAvgS%s,
              width = input$widthAvgS%s, height = input$heightAvgS%s, units = input$unitAvgS%s )
            }
          )", iter, iter, iter, iter, iter, iter
        ) ) )
        eval( parse( text = sprintf(
          "output$saveAvgCS%s <- downloadHandler(
            filename = function(){
              paste0('MeanCol-', Sys.time(), '.', input$typeAvgS%s )
            },
            content = function( file ){
              ggsave( file, plot = AvgPlotColListSo[[ iter ]], device = input$typeAvgS%s,
              width = input$widthAvgS%s, height = input$heightAvgS%s, units = input$unitAvgS%s )
            }
          )", iter, iter, iter, iter, iter, iter
        ) ) )

        # Save Map
        eval( parse( text = sprintf(
          "output$saveMapS%s <- downloadHandler(
            filename = function(){
              paste0('MapGCA-', Sys.time(), '.', input$typeMapS%s )
            },
            content = function( file ){
              ggsave( file, plot = OverMapListSo[[ iter ]], device = input$typeMapS%s,
              width = input$widthMapS%s, height = input$heightMapS%s, units = input$unitMapR%s )
            }
          )", iter, iter, iter, iter, iter, iter
        ) ) )

        # Insert table into the Shiny output
        OverMapListSao[[ iter ]] <<- OverMapListSa[[ iter ]] <<- OverMapAgg( session, iter, "B", OverMap( OverReep( RemoveClWe( TablesGCAordList[[ iter ]] ) ), 5 ) )
        eval( parse( text = sprintf( "output$MapsGCArhoOrdOutB%s <- renderPlot( OverMapListSao[[ iter ]] )",
                                     iter ) ) )

        # Create a new tab panel with the Over-representation Map
        appendTab( "MapsB",
          tabPanel( iter,
            sidebarLayout(
              sidebarPanel(
                eval( parse( text = sprintf( "numericInput( 'cutsMapB%s', 'Number of intervals', value = 5, 1, 30, 1 )", iter ) ) )
                ,eval( parse( text = sprintf( "numericInput( 'sizeXMapB%s', 'X font size Map', value = 10, 1, 30, 1 )", iter ) ) )
                ,eval( parse( text = sprintf( "numericInput( 'angleXMapB%s', 'X font angle Map', value = 0, 0, 90, 1 )", iter ) ) )
                ,eval( parse( text = sprintf( "numericInput( 'sizeYMapB%s', 'Y font size Map', value = 10, 1, 30, 1 )", iter ) ) )
                ,eval( parse( text = sprintf( "numericInput( 'angleYMapB%s', 'Y font angle Map', value = 0, 0, 90, 1 )", iter ) ) )
                ,eval( parse( text = sprintf( "numericInput( 'sizeLMapB%s', 'Legend font size Map', value = 10, 1, 30, 1 )", iter ) ) )
                ,eval( parse( text = sprintf( "numericInput( 'sizeWMapB%s', 'Width of the Map in %%', value = 100, 1, 300, 1 )", iter ) ) )
                ,eval( parse( text = sprintf( "numericInput( 'sizeHMapB%s', 'Height of the Map in %%', value = 100, 1, 300, 1 )", iter ) ) )
                ,hr()
                ,eval( parse( text = sprintf( "downloadButton( 'saveMapB%s', 'Save Map' )", iter ) ) )
                ,br(),br()
                ,eval( parse( text = sprintf( "selectInput( 'typeMapB%s', 'Type Map', choices = c('png','pdf','jpeg','eps','tex','tiff','bmp'),
                                              selected = 'png' )", iter ) ) )
                ,eval( parse( text = sprintf( "numericInput( 'widthMapB%s', 'Width Map', value = 10, 1, 30, 0.1 )", iter ) ) )
                ,eval( parse( text = sprintf( "numericInput( 'heightMapB%s', 'Height Map', value = 10, 1, 30, 0.1 )", iter ) ) )
                ,eval( parse( text = sprintf( "selectInput( 'unitMapB%s', 'Unit Map', choices = c('cm','in','mm','px'), selected = 'cm' )", iter ) ) )
                ,width = 2
              )
              ,mainPanel(
                fluidRow(
                  fluidRow( style = 'padding-top:50px;',
                    div( plotOutput( sprintf( "MapsGCArhoOrdOutB%s", iter ) ), align = "center" )
                  )
                )
                ,width = 10
              )
        ) ), select = TRUE )

        # Save Map B
        eval( parse( text = sprintf(
          "output$saveMapB%s <- downloadHandler(
              filename = function(){
                paste0('MapGCAaggOrd-', Sys.time(), '.', input$typeMapB%s )
              },
              content = function( file ){
                ggsave( file, plot = OverMapListSao[[ iter ]], device = input$typeMapB%s,
                width = input$widthMapB%s, height = input$heightMapB%s, units = input$unitMapB%s )
              }
            )", iter, iter, iter, iter, iter, iter
        ) ) )

      })
      #### Add clustering lines Ordered GCA tables ####
      observeEvent( eval( parse( text = sprintf( "input$clustNrowS%s", as.numeric( input$MapsS ) ) ) ), {

        req( !is.null( input$MapsS ) )
        iter <- as.numeric( input$MapsS )
        cuts1 <- eval( parse( text = sprintf( "input$cutsMapS%s", iter ) ) )

        req( !editTable )

        # Number of clusters
        n <- eval( parse( text = sprintf( "input$clustNrowS%s", iter ) ) )

        # Table dimension
        rowN <- nrow( TablesGCAordList[[ iter ]] )-2
        N <- max( 1, min( n, rowN ) )

        TablesGCAordList[[ iter ]] <<- Cluster( TablesGCAordList[[ iter ]], N = N, byRow = TRUE )
        TablesordList[[ iter ]] <<- ClusterOrdChange( TablesGCAordList[[ iter ]], TablesRawList[[ iter ]] )

        # Insert lines into the Shiny output
        temp <- OverMapListS[[ iter ]]$clusN[2]
        OverMapListS[[ iter ]] <<- OverMap( OverReep( RemoveClWe( TablesGCAordList[[ iter ]] ) ), cuts1 )
        OverMapListS[[ iter ]]$clusN[1] <<- N
        OverMapListS[[ iter ]]$clusN[2] <<- temp
        OverMapListSo[[ iter ]] <<- AddClustOverMap( session, iter, "S", OverMapListS[[ iter ]] ,
                                                     TablesGCAordList[[ iter ]], N, byRow = TRUE )

        W1 <- 1500*(as.numeric( eval( parse( text = sprintf( "input$sizeWMapS%s", iter ) ) ) )/100)
        H1 <- 400*(as.numeric( eval( parse( text = sprintf( "input$sizeHMapS%s", iter ) ) ) )/100)
        eval( parse( text = sprintf( "output$MapsGCArhoOrdOut%s <- renderPlot( OverMapListSo[[ iter ]],
                                     height = H1, width = W1 )",
                                     iter ) ) )

        cuts2 <- eval( parse( text = sprintf( "input$cutsMapB%s", iter ) ) )
        if( is.null(cuts2) ){ cuts2 <- cuts1 }
        W2 <- 1500*(as.numeric( eval( parse( text = sprintf( "input$sizeWMapB%s", iter ) ) ) )/100)
        H2 <- 400*(as.numeric( eval( parse( text = sprintf( "input$sizeHMapB%s", iter ) ) ) )/100)
        if( length(W2) == 0 ){ W2 <- W1; H2 <- H1 }

        OverMapListSa[[ iter ]] <<- AggTableDual( TablesGCAordList[[ iter ]], N, temp )$GCA
        TablesRawAggordList[[ iter ]] <<- RemoveClWe( AggTableDualRaw( TablesordList[[ iter ]], N, temp )$In )

        OverMapListSao[[ iter ]] <<- OverMapAgg( session, iter, "B", OverMap( OverReep( RemoveClWe( OverMapListSa[[ iter ]] ) ), cuts2 ) )
        eval( parse( text = sprintf( "output$MapsGCArhoOrdOutB%s <- renderPlot( OverMapListSao[[ iter ]],
                                     height = H2, width = W2 )",
                                     iter ) ) )

        # Display Averages for clusters
        if( N == 1 ){
          # AvgPlotRowListSo[[ iter ]] <<- MeanStdPlot( session, iter, 'S', TableGCA( AggTable( TablesGCAordList[[ iter ]], TRUE, rep( 1, rowN ), 0 ) ), TRUE )
          AvgPlotRowListSo[[ iter ]] <<- MeanStdPlot( session, iter, 'S', TableGCA( AggTableRaw( TablesordList[[ iter ]], TRUE, rep( 1, rowN ), 0 ) ), TRUE )
        }else{
          # AvgPlotRowListSo[[ iter ]] <<- MeanStdPlot( session, iter, 'S', TableGCA( AggTable( TablesGCAordList[[ iter ]], TRUE, NULL, N-1 ) ), TRUE )
          AvgPlotRowListSo[[ iter ]] <<- MeanStdPlot( session, iter, 'S', TableGCA( AggTableRaw( TablesordList[[ iter ]], TRUE, NULL, N-1 ) ), TRUE )
        }
        eval( parse( text = sprintf( "output$AvgPlotRowOrdOut%s <- renderPlot( AvgPlotRowListSo[[ iter ]] )",
                                     iter ) ) )

        # Insert table into the Shiny output
        eval( parse( text = sprintf( "output$TablesGCAordOut%s <- renderDT( TablesGCAordList[[ iter ]], options = list( dom = 't', pageLength = 10000 ) )",
                                     iter ) ) )
        eval( parse( text = sprintf( "output$TablesAggordOut%s <- renderDT( TablesRawAggordList[[ iter ]], options = list( dom = 't', pageLength = 10000 ) )",
                                     iter ) ) )
        eval( parse( text = sprintf( "output$TablesordOut%s <- renderDT( TablesordList[[ iter ]], options = list( dom = 't', pageLength = 10000 ) )",
                                     iter ) ) )
      })
      observeEvent( eval( parse( text = sprintf( "input$clustNcolS%s", as.numeric( input$MapsS ) ) ) ), {

        req( !is.null( input$MapsS ) )
        iter <- as.numeric( input$MapsS )
        cuts1 <- eval( parse( text = sprintf( "input$cutsMapS%s", iter ) ) )

        # Number of clusters
        n <- eval( parse( text = sprintf( "input$clustNcolS%s", iter ) ) )

        # Table dimension
        colN <- ncol( TablesGCAordList[[ iter ]] )-2
        N <- max( 1, min( n, colN ) )

        TablesGCAordList[[ iter ]] <<- Cluster( TablesGCAordList[[ iter ]], N = N, byRow = FALSE )
        TablesordList[[ iter ]] <<- ClusterOrdChange( TablesGCAordList[[ iter ]], TablesRawList[[ iter ]] )

        # Insert lines into the Shiny output
        temp <- OverMapListS[[ iter ]]$clusN[1]
        OverMapListS[[ iter ]] <<- OverMap( OverReep( RemoveClWe( TablesGCAordList[[ iter ]] ) ), cuts1 )
        OverMapListS[[ iter ]]$clusN[2] <<- N
        OverMapListS[[ iter ]]$clusN[1] <<- temp
        OverMapListSo[[ iter ]] <<- AddClustOverMap( session, iter, "S", OverMapListS[[ iter ]] ,
                                                     TablesGCAordList[[ iter ]], N, byRow = FALSE )

        W1 <- 1500*(as.numeric( eval( parse( text = sprintf( "input$sizeWMapS%s", iter ) ) ) )/100)
        H1 <- 400*(as.numeric( eval( parse( text = sprintf( "input$sizeHMapS%s", iter ) ) ) )/100)
        eval( parse( text = sprintf( "output$MapsGCArhoOrdOut%s <- renderPlot( OverMapListSo[[ iter ]],
                                     height = H1, width = W1 )",
                                     iter ) ) )

        cuts2 <- eval( parse( text = sprintf( "input$cutsMapB%s", iter ) ) )
        if( is.null(cuts2) ){ cuts2 <- cuts1 }
        W2 <- 1500*(as.numeric( eval( parse( text = sprintf( "input$sizeWMapB%s", iter ) ) ) )/100)
        H2 <- 400*(as.numeric( eval( parse( text = sprintf( "input$sizeHMapB%s", iter ) ) ) )/100)
        if( length(W2) == 0 ){ W2 <- W1; H2 <- H1 }

        OverMapListSa[[ iter ]] <<- AggTableDual( TablesGCAordList[[ iter ]], temp, N )$GCA
        TablesRawAggordList[[ iter ]] <<- RemoveClWe( AggTableDualRaw( TablesordList[[ iter ]], temp, N )$In )

        OverMapListSao[[ iter ]] <<- OverMapAgg( session, iter, "B", OverMap( OverReep( RemoveClWe( OverMapListSa[[ iter ]] ) ), cuts2 ) )
        eval( parse( text = sprintf( "output$MapsGCArhoOrdOutB%s <- renderPlot( OverMapListSao[[ iter ]],
                                     height = H2, width = W2 )",
                                     iter ) ) )

        # Display Averages for clusters
        if( N == 1 ){
          # AvgPlotColListSo[[ iter ]] <<- MeanStdPlot( session, iter, 'S', TableGCA( AggTable( TablesGCAordList[[ iter ]], FALSE, rep( 1, colN ), 0 ) ), FALSE )
          AvgPlotColListSo[[ iter ]] <<- MeanStdPlot( session, iter, 'S', TableGCA( AggTableRaw( TablesordList[[ iter ]], FALSE, rep( 1, colN ), 0 ) ), FALSE )
        }else{
          # AvgPlotColListSo[[ iter ]] <<- MeanStdPlot( session, iter, 'S', TableGCA( AggTable( TablesGCAordList[[ iter ]], FALSE, NULL , N-1 ) ), FALSE )
          AvgPlotColListSo[[ iter ]] <<- MeanStdPlot( session, iter, 'S', TableGCA( AggTableRaw( TablesordList[[ iter ]], FALSE, NULL , N-1 ) ), FALSE )
        }
        eval( parse( text = sprintf( "output$AvgPlotColOrdOut%s <- renderPlot( AvgPlotColListSo[[ iter ]] )",
                                     iter ) ) )

        # Insert table into the Shiny output
        eval( parse( text = sprintf( "output$TablesGCAordOut%s <- renderDT( TablesGCAordList[[ iter ]], options = list( dom = 't', pageLength = 10000 ) )",
                                     iter ) ) )
        eval( parse( text = sprintf( "output$TablesAggordOut%s <- renderDT( TablesRawAggordList[[ iter ]], options = list( dom = 't', pageLength = 10000 ) )",
                                     iter ) ) )
        eval( parse( text = sprintf( "output$TablesordOut%s <- renderDT( TablesordList[[ iter ]], options = list( dom = 't', pageLength = 10000 ) )",
                                     iter ) ) )
      })
      observeEvent( list( eval( parse( text = sprintf( "input$sizeXMapS%s", as.numeric( input$MapsS ) ) ) ),
                          eval( parse( text = sprintf( "input$angleXMapS%s", as.numeric( input$MapsS ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeYMapS%s", as.numeric( input$MapsS ) ) ) ),
                          eval( parse( text = sprintf( "input$angleYMapS%s", as.numeric( input$MapsS ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeLMapS%s", as.numeric( input$MapsS ) ) ) ),
                          eval( parse( text = sprintf( "input$cutsMapS%s", as.numeric( input$MapsS ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeHMapS%s", as.numeric( input$MapsS ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeWMapS%s", as.numeric( input$MapsS ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeXAvgS%s", as.numeric( input$MapsS ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeYAvgS%s", as.numeric( input$MapsS ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeXlAvgS%s", as.numeric( input$MapsS ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeYlAvgS%s", as.numeric( input$MapsS ) ) ) ) ), {

        req( !is.null( input$MapsS ) )
        iter <- as.numeric( input$MapsS )
        cuts1 <- eval( parse( text = sprintf( "input$cutsMapS%s", iter ) ) )

        n1 <- eval( parse( text = sprintf( "input$clustNrowS%s", iter ) ) )
        n2 <- eval( parse( text = sprintf( "input$clustNcolS%s", iter ) ) )
        rowN <- nrow( TablesGCAordList[[ iter ]] )-2
        N1 <- max( 1, min( n1, rowN ) )
        colN <- ncol( TablesGCAordList[[ iter ]] )-2
        N2 <- max( 1, min( n2, colN ) )

        # Insert lines into the Shiny output
        OverMapListS[[ iter ]] <<- OverMap( OverReep( RemoveClWe( TablesGCAordList[[ iter ]] ) ), cuts1 )
        OverMapListS[[ iter ]]$clusN[1] <<- N1
        OverMapListS[[ iter ]]$clusN[2] <<- N2
        OverMapListSo[[ iter ]] <<- AddClustOverMap( session, iter, "S", OverMapListS[[ iter ]] ,
                                                     TablesGCAordList[[ iter ]], 1, byRow = FALSE )

        W1 <- 1500*(as.numeric( eval( parse( text = sprintf( "input$sizeWMapS%s", iter ) ) ) )/100)
        H1 <- 400*(as.numeric( eval( parse( text = sprintf( "input$sizeHMapS%s", iter ) ) ) )/100)
        eval( parse( text = sprintf( "output$MapsGCArhoOrdOut%s <- renderPlot( OverMapListSo[[ iter ]],
                                     height = H1, width = W1 )",
                                     iter ) ) )

        cuts2 <- eval( parse( text = sprintf( "input$cutsMapB%s", iter ) ) )
        if( is.null(cuts2) ){ cuts2 <- cuts1 }
        W2 <- 1500*(as.numeric( eval( parse( text = sprintf( "input$sizeWMapB%s", iter ) ) ) )/100)
        H2 <- 400*(as.numeric( eval( parse( text = sprintf( "input$sizeHMapB%s", iter ) ) ) )/100)
        if( length(W2) == 0 ){ W2 <- W1; H2 <- H1 }

        Aggout <- AggTableDual( TablesGCAordList[[ iter ]], N1, N2 )
        OverMapListSa[[ iter ]] <<- Aggout$GCA

        OverMapListSao[[ iter ]] <<- OverMapAgg( session, iter, "B", OverMap( OverReep( RemoveClWe( OverMapListSa[[ iter ]] ) ), cuts2 ) )
        eval( parse( text = sprintf( "output$MapsGCArhoOrdOutB%s <- renderPlot( OverMapListSao[[ iter ]],
                                     height = H2, width = W2 )",
                                     iter ) ) )

        # Display Averages for clusters
        if( N1 == 1 ){
          # AvgPlotRowListSo[[ iter ]] <<- MeanStdPlot( session, iter, 'S', TableGCA( AggTable( TablesGCAordList[[ iter ]], TRUE, rep( 1, rowN ), 0 ) ), TRUE )
          AvgPlotRowListSo[[ iter ]] <<- MeanStdPlot( session, iter, 'S', TableGCA( AggTableRaw( TablesordList[[ iter ]], TRUE, rep( 1, rowN ), 0 ) ), TRUE )
        }else{
          # AvgPlotRowListSo[[ iter ]] <<- MeanStdPlot( session, iter, 'S', TableGCA( AggTable( TablesGCAordList[[ iter ]], TRUE, NULL, N2-1 ) ), TRUE )
          AvgPlotRowListSo[[ iter ]] <<- MeanStdPlot( session, iter, 'S', TableGCA( AggTableRaw( TablesordList[[ iter ]], TRUE, NULL, N2-1 ) ), TRUE )
        }
        eval( parse( text = sprintf( "output$AvgPlotRowOrdOut%s <- renderPlot( AvgPlotRowListSo[[ iter ]] )",
                                     iter ) ) )
        if( N2 == 1 ){
          # AvgPlotColListSo[[ iter ]] <<- MeanStdPlot( session, iter, 'S', TableGCA( AggTable( TablesGCAordList[[ iter ]], FALSE, rep( 1, colN ), 0 ) ), FALSE )
          AvgPlotColListSo[[ iter ]] <<- MeanStdPlot( session, iter, 'S', TableGCA( AggTableRaw( TablesordList[[ iter ]], FALSE, rep( 1, colN ), 0 ) ), FALSE )
        }else{
          # AvgPlotColListSo[[ iter ]] <<- MeanStdPlot( session, iter, 'S', TableGCA( AggTable( TablesGCAordList[[ iter ]], FALSE, NULL , N2-1 ) ), FALSE )
          AvgPlotColListSo[[ iter ]] <<- MeanStdPlot( session, iter, 'S', TableGCA( AggTableRaw( TablesordList[[ iter ]], FALSE, NULL , N2-1 ) ), FALSE )
        }
        eval( parse( text = sprintf( "output$AvgPlotColOrdOut%s <- renderPlot( AvgPlotColListSo[[ iter ]] )",
                                     iter ) ) )

      })
      observeEvent( list( eval( parse( text = sprintf( "input$sizeXClustS%s", as.numeric( input$MapsS ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeYClustS%s", as.numeric( input$MapsS ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeXlClustS%s", as.numeric( input$MapsS ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeYlClustS%s", as.numeric( input$MapsS ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeLClustS%s", as.numeric( input$MapsS ) ) ) ) ), {

        req( !is.null( input$MapsS ) )
        iter <- as.numeric( input$MapsS )

        RhoAggTestListSo[[ iter ]] <<- RhoPlot( session, iter, 'S', RhoAggTestListS[[ iter ]] )
        eval( parse( text = sprintf( "output$RhoAggTestOrdOut%s <- renderPlot( RhoAggTestListSo[[ iter ]] )",
                                     iter ) ) )

      })
      observeEvent( list( eval( parse( text = sprintf( "input$sizeXMapB%s", as.numeric( input$MapsB ) ) ) ),
                          eval( parse( text = sprintf( "input$angleXMapB%s", as.numeric( input$MapsB ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeYMapB%s", as.numeric( input$MapsB ) ) ) ),
                          eval( parse( text = sprintf( "input$angleYMapB%s", as.numeric( input$MapsB ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeLMapB%s", as.numeric( input$MapsB ) ) ) ),
                          eval( parse( text = sprintf( "input$cutsMapB%s", as.numeric( input$MapsB ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeHMapB%s", as.numeric( input$MapsB ) ) ) ),
                          eval( parse( text = sprintf( "input$sizeWMapB%s", as.numeric( input$MapsB ) ) ) ) ), {

        req( !is.null( input$MapsB ) )
        iter <- as.numeric( input$MapsB )
        cuts <- eval( parse( text = sprintf( "input$cutsMapB%s", iter ) ) )

        n1 <- eval( parse( text = sprintf( "input$clustNrowS%s", iter ) ) )
        n2 <- eval( parse( text = sprintf( "input$clustNcolS%s", iter ) ) )
        rowN <- nrow( TablesGCAordList[[ iter ]] )-2
        N1 <- max( 1, min( n1, rowN ) )
        colN <- ncol( TablesGCAordList[[ iter ]] )-2
        N2 <- max( 1, min( n2, colN ) )

        W <- 1500*(as.numeric( eval( parse( text = sprintf( "input$sizeWMapB%s", iter ) ) ) )/100)
        H <- 400*(as.numeric( eval( parse( text = sprintf( "input$sizeHMapB%s", iter ) ) ) )/100)

        Aggout <- AggTableDual( TablesGCAordList[[ iter ]], N1, N2 )
        OverMapListSa[[ iter ]] <<- Aggout$GCA

        OverMapListSao[[ iter ]] <<- OverMapAgg( session, iter, "B", OverMap( OverReep( RemoveClWe( OverMapListSa[[ iter ]] ) ), cuts ) )
        eval( parse( text = sprintf( "output$MapsGCArhoOrdOutB%s <- renderPlot( OverMapListSao[[ iter ]],
                                      height = H, width = W )",
                                      whichGCAtab[[ iter ]] ) ) )

      })

    },

    #### Options ####

    # Function to clear global environment
    onStart <- function(){

      onStop( function(){

        rm( list = c("TablesRawList","TablesGCAList","TablesGCAordList","TablesordList",
                     "TablesRawAggList","TablesRawAggordList","GCArhoOrd","whichGCAtab","CondList",
                     "OverMapListR","OverMapListRo","OverMapListRa","OverMapListRao",
                     "OverMapListS","OverMapListSo","OverMapListSa","OverMapListSao",
                     "RhoAggTestListR","RhoAggTestListRo","AvgPlotRowListRo","AvgPlotColListRo",
                     "RhoAggTestListS","RhoAggTestListSo","AvgPlotRowListSo","AvgPlotColListSo",
                     "TableRawEdit","TableRawProxy","editTable","tempTab","whichTabModStat",
                     "GCArhoList","startGCArhoList"),
            envir = .GlobalEnv )

      } )

    }

  )

  #### RunApp ####
  if( launch.browser ){

    runApp( app, launch.browser = T )

  }else if( !launch.browser ){

    runApp( app )

  }

}
