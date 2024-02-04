#' OverMap
#' @title OverMap
#'
#' @description Preparing over-representation map.
#'
#' @param dat Imput data frame or matrix based on the OverReep() function.
#' @param cuts Number of intervals.
#'
#' @return Plot with over-representation map.
#'
#' @export OverMap
#'
#' @examples
#' \dontrun{
#' X <- matrix( c(0.25,0.1,0.1,0.05,0.2,0.05,0.04,0.03,0.03,0.06,0.02,0.07), 3, 4 )
#' colnames(X) <- c("j1","j2","j3","j4")
#' rownames(X) <- c("i1","i2","i3")
#' X <- AddClWe( X )
#' XGrade <- TableGCA( X )
#' XOver <- OverReep( RemoveClWe( XGrade ) )
#' OverMap( XOver, 5 )
#' }
#'

OverMap <- function( overrep, cuts = 5 ){

  if( is.null( dimnames( overrep ) ) ){

    rownames( overrep ) <- paste0( "r", 1:nrow(overrep) )
    colnames( overrep ) <- paste0( "c", 1:ncol(overrep) )

  }

  x <- melt( overrep )

  x$Var1 <- factor( x$Var1, levels = rev( unique( x$Var1 ) ) )
  x$Var2 <- factor( x$Var2, levels = unique( x$Var2 ) )

  x$value <- cut( x$value, breaks = seq( min( x$value ) - 0.00001, max( x$value ), length.out = cuts + 1 ) )
  x$value <- gsub("-1e-05","0",x$value)

  breaksR <- data.frame( Var1 = unique(x$Var1), mgR = attr(overrep,"Stats")$Marg.Dist[[1]] )
  breaksR$cumR <- cumsum( breaksR$mgR )
  breaksR$posR <- 1 - 0.5 * ( breaksR$cumR + c( 0, head( breaksR$cumR, -1 ) ) )

  breaksC <- data.frame( Var2 = unique(x$Var2), mgC = attr(overrep,"Stats")$Marg.Dist[[2]] )
  breaksC$cumC <- cumsum( breaksC$mgC )
  breaksC$posC <- 0.5 * ( breaksC$cumC + c( 0, head( breaksC$cumC, -1 ) ) )

  x <- merge( x, breaksR, by = "Var1" )
  x <- merge( x, breaksC, by = "Var2" )

  overMap <- ggplot( x, aes( x = posC, y = posR ) ) +
    geom_tile( aes( width = mgC, height = mgR, fill = value ) ) +
    xlab( "" ) + ylab( "" ) +
    scale_x_continuous( breaks = breaksC$posC, labels = breaksC$Var2, expand = c(0, 0) ) +
    scale_y_continuous( breaks = breaksR$posR, labels = breaksR$Var1, expand = c(0, 0) ) +
    scale_fill_manual( drop = FALSE , values = paste0( "gray", as.integer( seq( 100, 30, length.out = cuts ) ) ), name = "" ) +
    geom_hline( yintercept = 1 - c( 0, breaksR$cumR ) ) + geom_vline( xintercept = c( 0, breaksC$cumC ) )

  overMap$clusN <- c( 1, 1 )

  return( overMap )

}

AddClustOverMap <- function( session, iter, type, overMap, tab, N, byRow = TRUE ){

  if( overMap$clusN[1] > 1 ){

    n <- overMap$clusN[1]
    overMap <- overMap + geom_hline( yintercept = 1 - attr(tab,"Stats")$CDF[[1]][unlist(attr(tab,"Clustering")[[1]][[n-1]][,1:(n-1)])],
                                     lwd = 2, color="red", linetype = "dashed" )
  }
  if( overMap$clusN[2] > 1 ){

    n <- overMap$clusN[2]
    overMap <- overMap + geom_vline( xintercept = attr(tab,"Stats")$CDF[[2]][unlist(attr(tab,"Clustering")[[2]][[n-1]][,1:(n-1)])],
                                     lwd = 2, color="red", linetype = "dashed" )
  }

  overMap <- overMap +
    theme( axis.text.x = element_text( size = eval( parse( text = sprintf( "session$input$sizeXMap%s%s", type, iter ) ) ),
                                       angle = eval( parse( text = sprintf( "session$input$angleXMap%s%s", type, iter ) ) ) ),
           axis.text.y = element_text( size = eval( parse( text = sprintf( "session$input$sizeYMap%s%s", type, iter ) ) ),
                                       angle = eval( parse( text = sprintf( "session$input$angleYMap%s%s", type, iter ) ) ) ),
           legend.text = element_text( size = eval( parse( text = sprintf( "session$input$sizeLMap%s%s", type, iter ) ) ) ) )

  return( overMap )

}

OverMapAgg <- function( session, iter, type, overMap ){

  overMap <- overMap +
    theme( axis.text.x = element_text( size = eval( parse( text = sprintf( "session$input$sizeXMap%s%s", type, iter ) ) ),
                                       angle = eval( parse( text = sprintf( "session$input$angleXMap%s%s", type, iter ) ) ) ),
           axis.text.y = element_text( size = eval( parse( text = sprintf( "session$input$sizeYMap%s%s", type, iter ) ) ),
                                       angle = eval( parse( text = sprintf( "session$input$angleYMap%s%s", type, iter ) ) ) ),
           legend.text = element_text( size = eval( parse( text = sprintf( "session$input$sizeLMap%s%s", type, iter ) ) ) ) )

  return( overMap )

}

RhoPlot <- function( session, iter, type, dat ){

  tab <- data.frame( value = unlist( dat ) )
  tab <- data.frame( x = as.integer( gsub( "[^[:digit:]]", "", rownames(tab) ) ),
                     variable = gsub( "[[:digit:]]", "", rownames(tab) ), value = tab )

  p <- ggplot( tab, aes(x = x, y = value, color = variable )) +  geom_line() +
    scale_color_discrete( name = "" ) +
    xlab( "Number of clusters" ) + ylab( "Rho Spearman" ) +
    theme( axis.text.x = element_text( size = eval( parse( text = sprintf( "session$input$sizeXClust%s%s", type, iter ) ) ) ),
           axis.text.y = element_text( size = eval( parse( text = sprintf( "session$input$sizeYClust%s%s", type, iter ) ) ) ),
           axis.title.x = element_text( size = eval( parse( text = sprintf( "session$input$sizeXlClust%s%s", type, iter ) ) ) ),
           axis.title.y = element_text( size = eval( parse( text = sprintf( "session$input$sizeYlClust%s%s", type, iter ) ) ) ),
           legend.text = element_text( size = eval( parse( text = sprintf( "session$input$sizeLClust%s%s", type, iter ) ) ) ) )

  return( p )

}

MeanStdPlot <- function( session, iter, type, dat, byRow = TRUE ){

  lab <- ifelse( byRow, "Row Clusters", "Col Clusters" )
  byRow <- ifelse( byRow, 1, 2 )

  tab <- data.frame( Avg = attr( dat, "StatsRaw" )$Avg[[ byRow ]], Std = attr( dat, "StatsRaw" )$Std[[ byRow ]] )
  tab <- data.frame( x = rownames( tab ), tab )

  p <- ggplot( tab, aes(x = x, y = Avg )) +  geom_point() +
    xlab( lab ) + ylab( "Mean" ) +
    geom_errorbar( aes(ymin = Avg-Std, ymax = Avg+Std ), width = .2, position = position_dodge(0.05) ) +
    theme( axis.text.x = element_text( size = eval( parse( text = sprintf( "session$input$sizeXAvg%s%s", type, iter ) ) ) ),
           axis.text.y = element_text( size = eval( parse( text = sprintf( "session$input$sizeYAvg%s%s", type, iter ) ) ) ),
           axis.title.x = element_text( size = eval( parse( text = sprintf( "session$input$sizeXlAvg%s%s", type, iter ) ) ) ),
           axis.title.y = element_text( size = eval( parse( text = sprintf( "session$input$sizeYlAvg%s%s", type, iter ) ) ) ) )

  return( p )

}
