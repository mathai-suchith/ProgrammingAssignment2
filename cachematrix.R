## Put comments here that give an overall description of what your
## functions do

#get_matrix - will display the entered matrix. (technically it is same as the set_cache_matrix function only
#the naming convention differs)
#set_cache_matrix - will explicitly cache the matrix.
#invert_matrix - will invert the matrix and cache the value

#Note - the function checks if the input matrix is a square matrix
#***Input run string - matrix <-creatematrix (matrix(rnorm(16,1,2),5,4))
#

creatematrix <- function(x = matrix())
{
  #check to ensure the input matrix is a square matrix.
  
  if (ncol(x)!= nrow(x))
    {
      print("matrix is not a square matrix , please use a square matrix")
      return()
    }
  #check to ensure that the matrix is not singular
  det <- det(x)
  if (det == 0)
    {
      print("the matrix is not invertible , please use a random number generating function")
      return()
    }
  else
  {
      get_matrix <- function()
        {
            inp_matrix <<- x
            inp_matrix
        } 
      
      set_cache_matrix <- function()
      {
        cache_inp_matrix <<- inp_matrix
        cache_inp_matrix
      }
      invert_matrix <- function()
        {
            matrix_inp <<- inp_matrix
            cache_inv_matrix <<- solve(matrix_inp)
            cache_inv_matrix        
        }
      
      list (get_matrix = get_matrix,set_cache_matrix = set_cache_matrix, invert_matrix = invert_matrix)
  }
}

# Function checks if the input matrix was previously used. If it was entered , retrieve
# the calculated inverse value. if the inverse is not available the inverse will be calculated.

cachesolve <- function(x,...)
{ 
  cache_matrix <- x$set_cache_matrix()
  if (!exists("cache_matrix"))
    { 
        print ("Use creatematrix to create matrix")
        return()
    }
  else
    {
        cache_inp_matrix <- x$set_cache_matrix()
        if (all(cache_inp_matrix == x$get_matrix()))
            {
                  #print("No change to matrix , cached inverse retrieved")
                  cache_inv_matrix <- x$invert_matrix()
                  cache_inv_matrix
            }
        else
            {
                  print("Matrix change , calculating new inverse")      
                  matrix_inp <- x$get_matrix()
                  output_inv_matrix <- solve(matrix_inp)
                  output_inv_matrix
            }
    }
}