#' AddClWe
#' @title AddClWe
#'
#' @description Adding two columns and rows for weights and cluster numbers.
#'
#' @param dat Imput data frame or matrix.
#'
#' @return Data frame or matrix with two additional columns and rows.
#'
#' @export AddClWe
#'
#' @examples
#' \dontrun{
#' X <- matrix( c(0.25,0.1,0.1,0.05,0.2,0.05,0.04,0.03,0.03,0.06,0.02,0.07), 3, 4 )
#' colnames(X) <- c("j1","j2","j3","j4")
#' rownames(X) <- c("i1","i2","i3")
#' X <- AddClWe( X )
#' }

AddClWe <- function( dat ){

  dat <- rbind( cbind( dat, Weights = 1, Clusters = 1 ), Weights = 1, Clusters = 1 )
  dat[ nrow(dat):(nrow(dat)-1), ncol(dat):(ncol(dat)-1) ] <- NA
  rownames( dat )[ (nrow(dat)-1):nrow(dat) ] <- c( "Weights", "Clusters" )

  return( dat )

}

#' RemoveClWe
#' @title RemoveClWe
#'
#' @description Removing two additional columns and rows for weights and cluster numbers.
#'
#' @param dat Imput data frame or matrix.
#'
#' @return Data frame or matrix without two additional columns and rows.
#'
#' @export RemoveClWe
#'
#' @examples
#' \dontrun{
#' X <- matrix( c(0.25,0.1,0.1,0.05,0.2,0.05,0.04,0.03,0.03,0.06,0.02,0.07), 3, 4 )
#' colnames(X) <- c("j1","j2","j3","j4")
#' rownames(X) <- c("i1","i2","i3")
#' X <- AddClWe( X )
#' X <- RemoveClWe( X )
#' }
#'

RemoveClWe <- function( dat ){

  temp <- attributes( dat )
  dat <- dat[ -c( nrow(dat):(nrow(dat)-1) ), -c( ncol(dat):(ncol(dat)-1) ), drop = FALSE ]
  attr( dat ,"class") <- temp$class
  attr( dat, "Stats" ) <- temp$Stats
  attr( dat, "StatsRaw" ) <- temp$StatsRaw
  attr( dat, "Outliers" ) <- temp$Outliers
  attr( dat, "Clustering" ) <- temp$Clustering
  attr( dat, "Clusters" ) <- temp$Clusters

  return( dat )

}

#' TableGCA
#' @title TableGCA
#'
#' @description Preparing the table after grade transformation.
#'
#' @param dat Imput data frame or matrix based on the AddClWe() function.
#'
#' @return Data frame or matrix after grade transformation.
#'
#' @export TableGCA
#'
#' @examples
#' \dontrun{
#' X <- matrix( c(0.25,0.1,0.1,0.05,0.2,0.05,0.04,0.03,0.03,0.06,0.02,0.07), 3, 4 )
#' colnames(X) <- c("j1","j2","j3","j4")
#' rownames(X) <- c("i1","i2","i3")
#' X <- AddClWe( X )
#' XGrade <- TableGCA( X )
#' }
#'

TableGCA <- function( dat ){

  # Take original rows and columns names
  colNames <- colnames( dat )
  rowNames <- rownames( dat )

  datRaw <- temp <- dat[ -c( (nrow(dat)-1):(nrow(dat)) ), -c( (ncol(dat)-1):(ncol(dat)) ), drop = FALSE ]

  # Check if the required conditions are met:
  # 1) non-numerical columns
  # 2) undefined (Inf, NA, NaN)
  # 3) non-zero sums of rows and columns values
  Stop <- CharCols( temp ) | UndefinedRecords( temp )
  if( Stop ){

    return( invisible() )

  }else{

    Stop <- NonZeroRowsCols( temp )

    if( Stop ){

      return( invisible() )

    }

  }

  wRow <- as.numeric( dat[ -c( (nrow(dat)-1):(nrow(dat)) ), "Weights" ] )
  wCol <- as.numeric( dat[ "Weights", -c( (ncol(dat)-1):(ncol(dat)) ) ] )
  wRow <- wRow / sum( wRow )
  wCol <- wCol / sum( wCol )

  # Main Grade Transformation
  temp <- as.matrix( dat[ -c( (nrow(dat)-1):(nrow(dat)) ), -c( (ncol(dat)-1):(ncol(dat)) ), drop = FALSE ] )
  temp <- sweep( sweep( temp, 2, wCol, "*" ), 1, wRow, "*" )
  temp <- temp / sum( temp )
  dat <- as.matrix( dat )
  dat[ -c( (nrow(dat)-1):(nrow(dat)) ), -c( (ncol(dat)-1):(ncol(dat)) ) ] <- temp

  datRaw <- sweep( sweep( datRaw, 2, wCol, "*" ), 1, wRow, "*" )

  # Assign original rows and columns names
  colnames( dat ) <- colNames
  rownames( dat ) <- rowNames

  # Assign GCA class
  class( dat ) <- c( "TabGCA", class(dat) )

  # Metadata initialization
  attr( dat, "Stats" ) <- Stats( dat[ -c( (nrow(dat)-1):(nrow(dat)) ), -c( (ncol(dat)-1):(ncol(dat)) ), drop = FALSE ] )
  attr( dat, "StatsRaw" ) <- Stats( datRaw ) # attr( dat, "Stats" )
  attr( dat, "Outliers" ) <- list()
  attr( dat, "Clustering" ) <- list( NULL, NULL )
  attr( dat, "Clusters" ) <- list( NULL, NULL )

  return( dat )

}

#' TableReorder
#' @title TableReorder
#'
#' @description Reordering of columns and rows based on the GCA() function.
#'
#' @param dat Data frame or matrix based on the AddClWe() function.
#' @param rowOrd New ordering of rows.
#' @param colOrd New ordering of columns.
#'
#' @return Data frame or matrix with new ordering of rows and columns.
#'
#' @export TableReorder
#'
#' @examples
#' \dontrun{
#' X <- matrix( c(0.25,0.1,0.1,0.05,0.2,0.05,0.04,0.03,0.03,0.06,0.02,0.07), 3, 4 )
#' colnames(X) <- c("j1","j2","j3","j4")
#' rownames(X) <- c("i1","i2","i3")
#' X <- AddClWe( X )
#' XGrade <- TableGCA( X )
#' ord <- GCA( RemoveClWe( XGrade ), 100 )
#' XReord1 <- TableReorder( XGrade, ord[1,1:3], ord[1,4:7] )
#' }
#'

TableReorder <- function( dat, rowOrd = NULL, colOrd = NULL ){

  Attr <- attributes( dat )
  Dim <- dim( dat )

  rowOrd <- if( is.null(rowOrd) ){ 1:Dim[1] }else{ unlist( rowOrd ) }
  colOrd <- if( is.null(colOrd) ){ 1:Dim[2] }else{ unlist( colOrd ) }
  rowOrd <- c( rowOrd, Dim[1]-1, Dim[1] )
  colOrd <- c( colOrd, Dim[2]-1, Dim[2] )

  res <- dat[ rowOrd, colOrd ]

  attributes( res ) <- Attr

  # Stats <- attr( res, "Stats" )
  # StatsRaw <- attr( res, "StatsRaw" )
  #
  # for( i in 1:length(Stats) ){
  #
  #   Stat <- Stats[[ i ]]
  #   Stat[[ 1 ]] <- Stat[[ 1 ]][rowOrd]
  #   Stat[[ 2 ]] <- Stat[[ 2 ]][colOrd]
  #   Stats[[ i ]] <- Stat
  #
  #   StatR <- StatsRaw[[ i ]]
  #   StatR[[ 1 ]] <- StatR[[ 1 ]][rowOrd]
  #   StatR[[ 2 ]] <- StatR[[ 2 ]][colOrd]
  #   StatsRaw[[ i ]] <- StatR
  #
  # }

  rownames(res) <- rownames(res)[rowOrd]
  colnames(res) <- colnames(res)[colOrd]

  res[ 1:(nrow(res)-2), "Clusters" ] <- 1
  res[ "Clusters", 1:(ncol(res)-2) ] <- 1

  attr( res, "Stats" ) <- Stats( RemoveClWe( res ) )
  attr( res, "StatsRaw" ) <- attr( res, "StatsRaw" )
  # attr( res, "Stats" ) <- Stats
  # attr( res, "StatsRaw" ) <- StatsRaw

  return( res )

}

#' OverReep
#' @title OverReep
#'
#' @description Preparing over-representation values.
#'
#' @param dat Imput data frame or matrix based on the TableGCA() function.
#'
#' @return Data frame or matrix after with over-representation values.
#'
#' @export OverReep
#'
#' @examples
#' \dontrun{
#' X <- matrix( c(0.25,0.1,0.1,0.05,0.2,0.05,0.04,0.03,0.03,0.06,0.02,0.07), 3, 4 )
#' colnames(X) <- c("j1","j2","j3","j4")
#' rownames(X) <- c("i1","i2","i3")
#' X <- AddClWe( X )
#' XGrade <- TableGCA( X )
#' XOver <- OverReep( RemoveClWe( XGrade ) )
#' }
#'

OverReep <- function( dat ){

  if( !any( class( dat ) %in% "TabGCA" ) ){

    dat <- TableGCA( dat )

  }

  overrep <- dat / ( outer( attr(dat,"Stats")$Marg.Dist[[1]], attr(dat,"Stats")$Marg.Dist[[2]], "*" ) )
  attr( overrep, "Stats" ) <- Stats( dat )

  return( overrep )

}

factorTOnumeric <- function( factor, numbers ) {

  num <- NA
  for ( i in 1:length( levels(factor) ) ) {

    num <- ifelse( factor == levels(factor)[i], numbers[i], num )

  }
  return( num )

}

AggTable <- function( dat, byRow = TRUE, cl = NULL, N = 1 ){

  if( byRow ){

    WeCl <- dat[ (nrow(dat)-1):nrow(dat), ]
    clusters <- if( is.null(cl) ){ attr( dat, "Clusters" )[[1]][[N]] }else{ cl }
    # clusters <- if( is.null(cl) ){ na.omit(dat[,"Clusters"]) }else{ cl }
    datAgg <- sapply( 1:max(clusters), function(x,cl){ colSums( dat[ which( x == cl ),, drop = FALSE] ) },
                      cl = clusters, simplify = F )
    datAgg <- do.call( "rbind", datAgg )
    rownames( datAgg ) <- paste0( "r", 1:(N+1) )
    datAgg <- rbind( datAgg, WeCl )
    datAgg[ 1:(nrow(datAgg)-2), "Clusters" ] <- 1

  }else{

    WeCl <- dat[, (ncol(dat)-1):ncol(dat) ]
    clusters <- if( is.null(cl) ){ attr( dat, "Clusters" )[[2]][[N]] }else{ cl }
    # clusters <- if( is.null(cl) ){ na.omit(dat["Clusters",]) }else{ cl }
    datAgg <- sapply( 1:max(clusters), function(x,cl){ rowSums( dat[, which( x == cl ), drop = FALSE] ) },
                      cl = clusters, simplify = F )
    datAgg <- do.call( "cbind", datAgg )
    colnames( datAgg ) <- paste0( "c", 1:(N+1) )
    datAgg <- cbind( datAgg, WeCl )
    datAgg[ "Clusters", 1:(ncol(datAgg)-2) ] <- 1

  }

  return( datAgg )

}

AggTableRaw <- function( dat, byRow = TRUE, cl = NULL, N = 1 ){

  if( byRow ){

    WeCl <- dat[ (nrow(dat)-1):nrow(dat), ]
    clusters <- if( is.null(cl) ){ na.omit( unlist( dat[,"Clusters"]) ) }else{ cl }
    datAgg <- sapply( 1:max(clusters), function(x,cl){ colSums( dat[ which( x == cl ),, drop = FALSE] ) },
                      cl = clusters, simplify = F )
    datAgg <- do.call( "rbind", datAgg )
    rownames( datAgg ) <- paste0( "r", 1:(N+1) )
    datAgg <- rbind( datAgg, WeCl )
    datAgg[ 1:(nrow(datAgg)-2), "Clusters" ] <- 1

  }else{

    WeCl <- dat[, (ncol(dat)-1):ncol(dat) ]
    clusters <- if( is.null(cl) ){ na.omit( unlist( dat["Clusters",]) ) }else{ cl }
    datAgg <- sapply( 1:max(clusters), function(x,cl){ rowSums( dat[, which( x == cl ), drop = FALSE] ) },
                      cl = clusters, simplify = F )
    datAgg <- do.call( "cbind", datAgg )
    colnames( datAgg ) <- paste0( "c", 1:(N+1) )
    datAgg <- cbind( datAgg, WeCl )
    datAgg[ "Clusters", 1:(ncol(datAgg)-2) ] <- 1

  }

  return( datAgg )

}

AggTableDual <- function( dat, n1, n2 ){

  rowN <- nrow( dat )-2
  colN <- ncol( dat )-2

  dat <- if( n1 == 1 & n2 == 1 ){

    AggTable( AggTable( dat, TRUE, rep( 1, rowN ), 0 ), FALSE, rep( 1, colN ), 0 )

  }else if( n1 > 1 & n2 == 1 ){

    AggTable( AggTable( dat, TRUE, NULL, n1-1 ), FALSE, rep( 1, colN ), 0 )

  }else if( n1 == 1 & n2 > 1 ){

    AggTable( AggTable( dat, TRUE, rep( 1, rowN ), 0 ), FALSE, attr( dat, "Clusters" )[[2]][[n2-1]], n2-1 )

  }else{

    AggTable( AggTable( dat, TRUE, NULL, n1-1 ), FALSE, attr( dat, "Clusters" )[[2]][[n2-1]], n2-1 )

  }

  return( list( In = dat, GCA = TableGCA( dat ) ) )

}

AggTableDualRaw <- function( dat, n1, n2 ){

  rowN <- nrow( dat )-2
  colN <- ncol( dat )-2

  dat <- if( n1 == 1 & n2 == 1 ){

    AggTableRaw( AggTableRaw( dat, TRUE, rep( 1, rowN ), 0 ), FALSE, rep( 1, colN ), 0 )

  }else if( n1 > 1 & n2 == 1 ){

    AggTableRaw( AggTableRaw( dat, TRUE, NULL, n1-1 ), FALSE, rep( 1, colN ), 0 )

  }else if( n1 == 1 & n2 > 1 ){

    AggTableRaw( AggTableRaw( dat, TRUE, rep( 1, rowN ), 0 ), FALSE, na.omit( unlist( dat["Clusters",] ) ), n2-1 )

  }else{

    AggTableRaw( AggTableRaw( dat, TRUE, NULL, n1-1 ), FALSE, na.omit( unlist( dat["Clusters",] ) ), n2-1 )

  }

  return( list( In = dat ) )

}
