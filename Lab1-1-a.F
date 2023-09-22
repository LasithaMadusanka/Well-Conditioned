
*     .. Parameters ..
      INTEGER          N, NRHS
      PARAMETER        ( N = 100, NRHS = 1 )
      INTEGER          LDA, LDB
      PARAMETER        ( LDA = N, LDB = N )
*
*     .. Local Scalars ..
      INTEGER          INFO
*
*     .. Local Arrays ..
      INTEGER          IPIV( N )
      DOUBLE PRECISION A( LDA, N ), B( LDB, NRHS ),X(N,NRHS)
      
      
      External Subroutines
      EXTERNAL         DGESV
      EXTERNAL         PRINT_MATRIX, PRINT_INT_VECTOR

      Do i=1, N
            Do j=1, N
                  if((i==j) .or. (j==N) )then
                        A(i,j)= 1
                  else if(j>i) then
                        A(i,j)= -10
                  else 
                        A(i,j)=0
                  end if
            end Do
            X(i,NRHS) = dble(i)/N
      end Do
      

      B = matmul(A, X)

*     
*
*     .. Executable Statements ..
      WRITE(*,*)'DGESV Example Program Results'
*
*     Solve the equations A*X = B.
*
      CALL DGESV( N, NRHS, A, LDA, IPIV, B, LDB, INFO )
*
*     Check for the exact singularity.
*
*      IF( INFO.GT.0 ) THEN
*         WRITE(*,*)'The diagonal element of the triangular factor of A,'
*         WRITE(*,*)'U(',INFO,',',INFO,') is zero, so that'
*        WRITE(*,*)'A is singular; the solution could not be computed.'
*        STOP
*      END IF
*
*     Print solution.
*
      
      CALL PRINT_MATRIX( 'Solution', N, 1, B, LDB )
*
*     Print details of LU factorization.
*
      !CALL PRINT_MATRIX( 'Solution', N, N, A, LDA )
*
*     Print pivot indices.
*
*      CALL PRINT_INT_VECTOR( 'Pivot indices', N, IPIV )
      STOP
      END
*
*     End of DGESV Example.
*
*  =============================================================================
*
*     Auxiliary routine: printing a matrix.
*
      SUBROUTINE PRINT_MATRIX( DESC, M, N, A, LDA )
      CHARACTER*(*)    DESC
      INTEGER          M, N, LDA
      DOUBLE PRECISION A( LDA, * )
*
      INTEGER          I, J
*
       WRITE(*,*)
       WRITE(*,*) DESC
       DO I = 1, M
          WRITE(*,9998) ( A( I, J ), J = 1, N )
       END DO
*
 9998 FORMAT( 11(:,1X,F100.2) )
      RETURN
      END
*
*     Auxiliary routine: printing a vector of integers.
*
      SUBROUTINE PRINT_INT_VECTOR( DESC, N, A )
      CHARACTER*(*)    DESC
      INTEGER          N
      INTEGER          A( N )
*
      INTEGER          I
*
      WRITE(*,*)
      WRITE(*,*) DESC
      WRITE(*,9999) ( A( I ), I = 1, N )
*
 9999 FORMAT( 11(:,1X,I6) )
      RETURN
      END
