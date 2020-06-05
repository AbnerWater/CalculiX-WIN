/*  BridgeMPI.h  */

/*
#include <mpi.h>
*/
#include "../MPI.h"

#define LOCAL  1
#define GLOBAL 2

/*--------------------------------------------------------------------*/
/*
   --------------------------------------------------------------------
   this object is the bridge between the lanczos and spooles codes.

   NOTE: MPI version

      neqns   -- number of equations
      prbtype -- problem type
         1 -- vibration, multiply with M or B
         2 -- buckling, multiply with K or A
         3 -- simple
      mxbsz -- maximum block size
      nproc -- number of processes
      myid  -- id of this process
      coordFlag -- coordinate flag
         1 -- global coordinates for local matrix used in mvm
         2 -- local coordinates for local matrix used in mvm
      seed    -- random number seed used in ordering
      msglvl  -- message level for SPOOLES programs
         set msglvl =  0 for no output
         set msglvl =  1 for scalar and timing output
         set msglvl >= 2 for lots of output
      msgFile -- message file for debug and diagnostic output

   external objects, not free'd in the cleanup
      A -- InpMtx object for the first matrix, 
      B -- InpMtx object for the second matrix

   internal objects, free'd in cleanup
      pencil       -- Pencil object that contains A + sigma B
      frontETree   -- object that contains the front tree information
      symbfacIVL   -- object that contains the symbolic factorization
      mtxmanager   -- SubMtx manager object that handles storage 
         for the submatrices of the factors.
      frontmtx     -- object that contains the factorization
      oldToNewIV   -- object that contains the old-to-new permutation
      newToOldIV   -- object that contains the new-to-old permutation
      vtxmapIV     -- object that contains the map 
                      from vertices to processes
      myownedIV    -- object that contains the vertices 
                      owned by this processor
      ownersIV     -- object that contains the map 
                      from fronts to processes
      solvemap     -- object that contains the map 
                      from submatrices to processes
      rowmapIV     -- if pivoting was performed, this object holds
         the map from rows of the factor to processors.
      Xloc         -- local matrix for X of Y = A*X
      Yloc         -- local matrix for Y of Y = A*X
      info         -- object for distributed matrix-matrix multiply

   created -- 98aug10, jcp & cca
   --------------------------------------------------------------------
*/
typedef struct bridgeMPI_ {
  int           neqns       ;
  int           nproc       ;
  int           myid        ;
  int           prbtype     ;
  int           mxbsz       ;
  int           coordFlag   ;
  InpMtx        *A          ;
  InpMtx        *B          ;
  Pencil        *pencil     ;
  ETree         *frontETree ;
  IVL           *symbfacIVL ;
  SubMtxManager *mtxmanager ;
  FrontMtx      *frontmtx   ;
  IV            *oldToNewIV ;
  IV            *newToOldIV ;
  IV            *vtxmapIV   ;
  IV            *myownedIV  ;
  IV            *ownersIV   ;
  IV            *rowmapIV   ;
  SolveMap      *solvemap   ;
  MatMulInfo    *info       ;
  DenseMtx      *Xloc       ;
  DenseMtx      *Yloc       ;
  int           seed        ;
  int           msglvl      ;
  FILE          *msgFile    ;
  MPI_Comm      comm        ;
} BridgeMPI ;

/*--------------------------------------------------------------------*/
/*
   ----------------------------------------------------------------
  purpose --

   given InpMtx objects that contain A and B, initialize the bridge
  data structure for the MPI factor's, solve's and mvm's.
  
   NOTE: all the input arguments are pointers 
         to allow calls from Fortran

   data -- pointer to a Bridge object
   pprbtype -- pointer to value containing problem type
     *prbtype = 1 --> A X = B X Lambda, vibration problem
     *prbtype = 2 --> A X = B X Lambda, buckling problem
     *prbtype = 3 --> A X = X Lambda, simple eigenvalue problem
   pneqns  -- pointer to value containing number of equations
   pmxbsz  -- pointer to value containing blocksize
   A       -- pointer to InpMtx object containing A
   B       -- pointer to InpMtx object containing B
   pseed   -- pointer to value containing a random number seed
   pmsglvl -- pointer to value containing a message level
   msgFile -- message file pointer

   return value --
      1 -- normal return
     -1 -- data is NULL
     -2 -- pprbtype is NULL
     -3 -- *pprbtype is invalid
     -4 -- pneqns is NULL
     -5 -- *pneqns is invalid
     -6 -- pmxbsz is NULL
     -7 -- *pmxbsz is invalid
     -8 -- A and B are NULL
     -9 -- pseed is NULL
    -10 -- pmsglvl is NULL
    -11 -- *pmsglvl > 0 and msgFile is NULL
    -12 -- comm is NULL

   created -- 98aug10, cca
   ----------------------------------------------------------------
*/
int
SetupMPI (
   void       *data,
   int        *pprbtype,
   int        *pneqns,
   int        *pmxbsz,
   InpMtx     *A,
   InpMtx     *B,
   int        *pseed,
   int        *pmsglvl,
   FILE       *msgFile,
   MPI_Comm   comm
) ;
/*
   ---------------------------------------------------------------------
   purpose -- to compute the factorization of A - sigma * B
 
   note: all variables in the calling sequence are references
         to allow call from fortran.
 
   input parameters
 
      data    -- pointer to bridge data object
      psigma  -- shift for the matrix pencil
      ppvttol -- pivot tolerance
         *ppvttol =  0.0 --> no pivoting used
         *ppvttol != 0.0 --> pivoting used, entries in factor are
                             bounded above by 1/pvttol in magnitude

   output parameters
 
      *pinertia -- on return contains the number of negative eigenvalues
      *perror   -- on return contains an error code
          1 -- error found during factorization
          0 -- normal return
         -1 -- psigma is NULL
         -2 -- ppvttol is NULL
         -3 -- data is NULL
         -4 -- pinertia is NULL
 
   created -- 98aug10, cca & jcp
   ---------------------------------------------------------------------
*/
void
FactorMPI (
   double     *psigma,
   double     *ppvttol,
   void       *data,
   int        *pinertia,
   int        *perror
) ;
/*
   -------------------------------------------------------------
   purpose --- to compute a matrix-vector multiply y[] = C * x[]
     where C is the identity, A or B (depending on *pprbtype).
 
   *pnrows -- # of rows in x[]
   *pncols -- # of columns in x[]
   *pprbtype -- problem type
      *pprbtype = 1 --> vibration problem, matrix is A
      *pprbtype = 2 --> buckling problem, matrix is B
      *pprbtype = 3 --> matrix is identity, y[] = x[]
 
   created -- 98aug11, cca & jcp
   -------------------------------------------------------------
*/
void
MatMulMPI (
   int      *pnrows,
   int      *pncols,
   double   x[],
   double   y[],
   int      *pprbtype,
   void     *data
) ;
/*
   ----------------------------------------------
   purpose -- to solve a linear system
     (A - sigma*B) sol[] = rhs[]
 
   data    -- pointer to bridge data object
   *pnrows -- # of rows in x[] and y[]
   *pncols -- # of columns in x[] and y[]
   rhs[]   -- vector that holds right hand sides
   sol[]   -- vector to hold solutions
 
   note: rhs[] and sol[] can be the same array.
 
   on return, *perror holds an error code.
 
   created -- 98aug10, cca & jcp
   ----------------------------------------------
*/
void
SolveMPI (
   int       *pnrows,
   int       *pncols,
   double    rhs[],
   double    sol[],
   void      *data,
   int       *perror
) ;
/*
   --------------------------------------------
   purpose -- to free the owned data structures

   return values --
      1 -- normal return
     -1 -- data is NULL

   created -- 98aug10, cca
   --------------------------------------------
*/
int
CleanupMPI (
   void   *data
) ;
/*--------------------------------------------------------------------*/
