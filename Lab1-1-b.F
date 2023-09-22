
*     .. Parameters ..
      INTEGER          N, NRHS
      PARAMETER        ( N = 2, NRHS = 1 )
      INTEGER          LDA, LDB
      PARAMETER        ( LDA = N, LDB = N )
*
*     .. Local Scalars ..
      INTEGER          INFO
*
*     .. Local Arrays ..
      INTEGER          IPIV( N )
      REAL(8) A( LDA, N ), B( LDB, NRHS ),C(LDB, NRHS )

      
      
      External Subroutines
      EXTERNAL         DGESV
      EXTERNAL         PRINT_MATRIX, PRINT_INT_VECTOR

*      integer :: i,j

      A = reshape([4.5, 1.6, 3.1, 1.1],[N,N])
      B = reshape([19.249, 6.843],[N,NRHS])
      C = reshape([19.25, 6.84],[N,NRHS])


      Call PRINT_MATRIX( "Matrix A", N, N, A, LDA )
      
      Call PRINT_MATRIX( "Matrix B", N, NRHS, B, LDB )
      
      Call PRINT_MATRIX( "Matrix C", N, NRHS, C, LDB )

*      B = matmul(A, X)

*     
*
*     .. Executable Statements ..
*      WRITE(*,*)'DGESV Example Program Results'
*
*     Solve the equations A*X = B.
*
      CALL DGESV( N, NRHS, A, LDA, IPIV, B, LDB, INFO )
      
      CALL PRINT_MATRIX( 'Solution B', N, 1, B, LDB )
      
      if(INFO == 0) then
              write(*, *) "Task implemented successfully for Ax = b!" 
      else
              print *, "INFO=", INFO
      end if

      A = reshape([4.5, 1.6, 3.1, 1.1],[N,N])
      CALL DGESV( N, NRHS, A, LDA, IPIV, C, LDB, INFO )

      CALL PRINT_MATRIX( 'Solution C', N, 1, C, LDB )

      if(INFO == 0) then
              write(*, *) "Task implemented successfully for Ax = c!" 
      else
              print *, "INFO=", INFO
      end if

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
      
*      CALL PRINT_MATRIX( 'Solution', N, 1, B, LDB )
*      CALL PRINT_MATRIX( 'Solution', N, 1, C, LDB )
*
*     Print details of LU factorization.
*
*      CALL PRINT_MATRIX( 'Solution', N, N, A, LDA )
*
*     Print pivot indices.
*
*      CALL PRINT_INT_VECTOR( 'Pivot indices', N, IPIV )
*      STOP
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
      REAL(8) A( LDA, * )
*
      INTEGER          I, J
*
       WRITE(*,*)
       WRITE(*,*) DESC
       DO I = 1, M
          WRITE(*,*) ( A( I, J ), J = 1, N )
       END DO

*  9998 FORMAT( 11(:,1X,F100.2) )
*      RETURN
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
