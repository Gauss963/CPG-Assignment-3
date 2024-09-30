program MatrixInverse
    implicit none
    real :: A(3, 3), inverse_matrix(3, 3), determinant
    real :: cofactor_matrix(3, 3), adjugate_matrix(3, 3)
    integer :: I, J

    ! Read the matrix
    print *, 'Enter the 3x3 matrix elements (row-wise):'
    do I = 1, 3
        read *, (A(I, J), J = 1, 3)
    end do

    ! Compute determinant
    determinant = A(1, 1) * ( A(2, 2) * A(3, 3) - A(2, 3) * A(3, 2) ) - &
                  A(1, 2) * ( A(2, 1) * A(3, 3) - A(2, 3) * A(3, 1) ) + &
                  A(1, 3) * ( A(2, 1) * A(3, 2) - A(2, 2) * A(3, 1) )

    if (determinant == 0.0) then
        print *, 'Matrix is singular; inverse does not exist.'
        stop
    end if

    ! Compute cofactor matrix
    cofactor_matrix(1, 1) =  ( A(2, 2) * A(3, 3) - A(2, 3) * A(3, 2) )
    cofactor_matrix(1, 2) = -( A(2, 1) * A(3, 3) - A(2, 3) * A(3, 1) )
    cofactor_matrix(1, 3) =  ( A(2, 1) * A(3, 2) - A(2, 2) * A(3, 1) )
    cofactor_matrix(2, 1) = -( A(1, 2) * A(3, 3) - A(1, 3) * A(3, 2) )
    cofactor_matrix(2, 2) =  ( A(1, 1) * A(3, 3) - A(1, 3) * A(3, 1) )
    cofactor_matrix(2, 3) = -( A(1, 1) * A(3, 2) - A(1, 2) * A(3, 1) )
    cofactor_matrix(3, 1) =  ( A(1, 2) * A(2, 3) - A(1, 3) * A(2, 2) )
    cofactor_matrix(3, 2) = -( A(1, 1) * A(2, 3) - A(1, 3) * A(2, 1) )
    cofactor_matrix(3, 3) =  ( A(1, 1) * A(2, 2) - A(1, 2) * A(2, 1) )

    ! Transpose cofactor matrix to get adjugate matrix
    do I = 1, 3
    do J = 1, 3
        adjugate_matrix(I, J) = cofactor_matrix(J, I)
    end do
    end do

    ! Compute inverse matrix
    do I = 1, 3
    do J = 1, 3
        inverse_matrix(I, J) = adjugate_matrix(I, J) / determinant
    end do
    end do

    ! Output the inverse matrix
    print *, 'Inverse Matrix is:'
    do I = 1, 3
        print '(3F12.6)', ( inverse_matrix(I, J), J = 1, 3)
    end do

end program MatrixInverse