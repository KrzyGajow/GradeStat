GetNorm <- function( vec, n ){

  res <- vec[n]
  return( res )

}

GetNorm0 <- function( vec, n ){

  res <- ifelse( n, GetNorm(vec, n), 0 )
  return( res )

}


#' Cluster
#' @title Cluster
#'
#' @description Clustering algorithm.
#'
#' @param dat Data frame or matrix based on the XGrade() and RemoveClWe() functions.
#' @param N Number of clusters.
#' @param byRow Choose clustering dimension, rows or columns.
#'
#' @return Data frame with with additional attributes regarding clustering.
#'
#' @export Cluster
#'
#' @examples
#' \dontrun{
#' X <- matrix( c(0.25,0.1,0.1,0.05,0.2,0.05,0.04,0.03,0.03,0.06,0.02,0.07), 3, 4 )
#' colnames(X) <- c("j1","j2","j3","j4")
#' rownames(X) <- c("i1","i2","i3")
#' X <- AddClWe( X )
#' XGrade <- TableGCA( X )
#' Cluster( XGrade, N = 2, byRow = TRUE )
#' }
#'

Cluster <- function( tab, N = 2, byRow = TRUE ){

  if( N <= 1 ){

    if( byRow == 1 ){
      tab[ 1:(nrow(tab)-2), "Clusters" ] <- 1
    }else{
      tab[ "Clusters", 1:(ncol(tab)-2) ] <- 1
    }

    return( tab )

  }

  byRow <- ifelse( byRow, 1, 2 )
  if(
    if( is.null( attr( tab ,"Clustering" )[[ byRow ]] ) ){
      FALSE
    }else{
      if( (N-1) > length( attr( tab ,"Clustering" )[[ byRow ]] ) ){
        FALSE
      }else{
        !is.null( attr( tab ,"Clustering" )[[ byRow ]][[ N-1 ]] )
      }
    }
  ){
    if( byRow == 1 ){
      tab[ 1:(nrow(tab)-2), "Clusters" ] <- attr( tab, "Clusters" )[[ byRow ]][[ N-1 ]]
    }else{
      tab[ "Clusters", 1:(ncol(tab)-2) ] <- attr( tab, "Clusters" )[[ byRow ]][[ N-1 ]]
    }
    return( tab )
  }

  CDF <- c( 0, attr( tab,"Stats" )$CDF[[ byRow ]] )
  Cor.Grad <- c( 0, attr( tab,"Stats" )$Cor.Grad[[ byRow ]] )

  last <- length( CDF )
  prev <- div <- as.integer( seq( 1, last, length.out = N+1 ) )

  repeat{

    for( i in 2:N ){

      for( l in head( (div[i-1]+1):(div[i+1]), -1 ) ){

        w1 <- ( GetNorm( Cor.Grad, l ) - GetNorm0( Cor.Grad, l-1 ) ) /
          ( GetNorm( CDF, l ) - GetNorm0( CDF, l-1 ) )
        w2 <- ( GetNorm( Cor.Grad, div[i+1] ) - GetNorm0( Cor.Grad, div[i-1] ) ) /
          ( GetNorm( CDF, div[i+1] ) - GetNorm0( CDF, div[i-1] ) )
        w3 <- ( GetNorm( Cor.Grad, l+1 ) - GetNorm( Cor.Grad, l ) ) /
          ( GetNorm( CDF, l+1 ) - GetNorm( CDF, l ) )

        if( w1 <= w2 & w2 <= w3 ){
          div[i] <- l
        }

      }

    }

    if( all( prev == div ) ) break
    prev <- div

  }

  ar <- round( Ar( CDF[ div[-1] ], Cor.Grad[ div[-1] ], 0, 0 ), 4 )
  res <- as.data.frame( as.list( c( div[ -c( 1, length(div) ) ] - 1, ar ) ) )
  colnames( res ) <- c( paste0( "p", 1:(N-1) ), "Ar" )

  if( is.null( attr( tab ,"Clustering" )[[ byRow ]] ) ){

    Res <- vector( "list", N-1 )
    Res[[ N-1 ]] <- res
    attr( tab, "Clustering" )[[ byRow ]] <- Res
    Res <- vector( "list", N-1 )
    Res[[ N-1 ]] <- as.integer( cut( 1:(last-1), c( 0, res[ -length(res) ], (last-1) ) ) )
    names( Res[[ N-1 ]] ) <- names( attr( tab, "Stats" )$Score[[ byRow ]] )
    attr( tab, "Clusters" )[[ byRow ]] <- Res
    if( byRow == 1 ){
      tab[ 1:(nrow(tab)-2), "Clusters" ] <- Res[[ N-1 ]]
    }else{
      tab[ "Clusters", 1:(ncol(tab)-2) ] <- Res[[ N-1 ]]
    }

  }else{

    attr( tab, "Clustering" )[[ byRow ]][[ N-1 ]] <- res
    attr( tab, "Clusters" )[[ byRow ]][[ N-1 ]] <- as.integer( cut( 1:(last-1), c( 0, res[ -length(res) ], (last-1) ) ) )
    names( attr( tab, "Clusters" )[[ byRow ]][[ N-1 ]] ) <- names( attr( tab, "Stats" )$Score[[ byRow ]] )
    if( byRow == 1 ){
      tab[ 1:(nrow(tab)-2), "Clusters" ] <- attr( tab, "Clusters" )[[ byRow ]][[ N-1 ]]
    }else{
      tab[ "Clusters", 1:(ncol(tab)-2) ] <- attr( tab, "Clusters" )[[ byRow ]][[ N-1 ]]
    }

  }

  return( tab )

}

ClusterComb <- function( tab, N = 2, byRow = TRUE ){

  CDF <- attr( tab,"Stats" )$CDF[[ ifelse( byRow, 1, 2 ) ]]
  Cor.Grad <- attr( tab,"Stats" )$Cor.Grad[[ ifelse( byRow, 1, 2 ) ]]

  last <- length( CDF )
  Res <- vector( "list", (N-1) )

  for( i in 1:(N-1) ){

    indx <- vector( "list", i )

    if( i != 1){

      res <- Res[[i-1]]
      IndxS <- ( c( 1, unlist( res[,-ncol(res)] ), (last-1) ) )

    }else{

      IndxS <- ( c( 1, (last-1) ) )

    }

    for( j in 1:i ){
      indx[[ j ]] <- (IndxS[j]):(IndxS[j+1])
    }

    comb <- expand.grid( indx )
    resIter <- data.frame( comb, Ar = 0 )
    colnames(resIter) <- c( paste0( "p", 1:i ), "Ar" )

    for( j in 1:nrow(resIter) ){

      indx <- unlist( resIter[j,-ncol(resIter)] )
      resIter$Ar[j] <- Ar( CDF[ c(1,indx,last) ], Cor.Grad[ c(1,indx,last) ], 0, 0 )

    }

    Res[[ i ]] <- resIter[ which.min(resIter$Ar),]

  }

  attr( tab, "Clustering" )[[ ifelse( byRow, 1, 2 ) ]] <- Res

  return( tab )

}

ClusterChange <- function( tabIn, tabOut, N = 2, byRow = TRUE ){
  byRow <- ifelse( byRow, 1, 2 )
  if( N > 1 ){
    if( byRow == 1 ){
      tabOut[ 1:(nrow(tabOut)-2), "Clusters" ] <- attr( tabIn, "Clusters" )[[ byRow ]][[ N-1 ]]
    }else{
      tabOut[ "Clusters", 1:(ncol(tabOut)-2) ] <- attr( tabIn, "Clusters" )[[ byRow ]][[ N-1 ]]
    }
  }
  return( tabOut )
}

ClusterOrdChange <- function( tabIn, tabOut ){

  tabOut <- tabOut[ rownames(tabIn), colnames(tabIn) ]
  tabOut[ , c( "Weights","Clusters" ) ] <- tabIn[ , c( "Weights","Clusters" ) ]
  tabOut[ c( "Weights","Clusters" ), ] <- tabIn[ c( "Weights","Clusters" ), ]

  return( tabOut )

}
