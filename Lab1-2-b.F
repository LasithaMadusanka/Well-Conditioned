      PROGRAM lab
      INTEGER, PARAMETER :: N = 100
      INTEGER, PARAMETER :: KL = 2
      INTEGER, PARAMETER :: KU = 2
      INTEGER, PARAMETER :: LDAB = 7
*      INTEGER, PARAMETER :: LDA = 2*KL +KU+1
      INTEGER, PARAMETER :: NRHS = 1, NRHS1 = 5
      INTEGER :: i,j,k
      REAL*8 :: A(LDAB,N),B(N,NRHS),X(N), ERROR(N),Bk(N, NRHS)
      REAL*8 :: l2_NORM, x_prev(N,NRHS) 
      INTEGER :: INFO
      INTEGER :: IPIV(N)
      
      External Subroutines
      EXTERNAL         DGBTRF
      EXTERNAL         DGBTRS

*Defining A
      A = 0.0d0

      do i =1, LDAB
        do j = 1, N
           if(i<2+1) then 
              A(i,j) = 0
           else if (i == 2 + 1) then
                if (j<2+1) then
                   A(i,j) = 0 
                else 
                   A(i,j) = 2.0
                end if
           else if (i == 4) then 
                if (j == 1) then 
                   A(i,j) = 0
                else 
                   A(i,j) = -1.0
                end if
           else if (i == 5) then
              A(i,j) = 8.0 
           else if (i == 6) then
                if(j<N) then 
                   A(i,j) = 1.0
                else
                   A(i,j) = 0.0
                end if 
           else
                   if(j<N-1) then
                      A(i,j) = 3.0
                   else
                      A(i,j) = 0.0
                  end if     
           end if        
        end do
      end do

      print *, " "

*Defining B   

      B = 0.0d0
      do i = 1, N  
         B(i,1) = 13.0
      end do
      B(1,1) = 9.0
      B(2,1) = 10.0
      B(N-1,1) = 11.0
      B(N,1) = 12.0

        print *, " "

*       ERROR = 0.0
*       l2_NORM = 0.0
*       do i= 1, N
*         l2_NORM = l2_NORM +ERROR(i)**2
*       end do
*       l2_NORM =sqrt(l2_NORM)


       
       call dgbtrf(N, N, KL, KU, A, LDAB, ipiv, info)
       !Solve the second liner system Ax=b+x

       x_prev = 0.0
       Bk= B


       do k = 1, NRHS1
          call DGBTRS('N',N, KL, KU, 1, A, LDAB, IPIV, Bk, N, INFO)
       write(*,'(1X,*(F8.3))') Bk
          Bk= B + Bk
          !x_prev(:,NRHS) = Bk(:,k) 
       end do   

      END PROGRAM lab

