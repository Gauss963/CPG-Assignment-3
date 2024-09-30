program LinearRegression
    implicit none
    integer :: i, n
    real, dimension(:), allocatable :: Xi, Yi
    real :: seed

    ! The size of the array
    n = 30
    allocate(Xi(n), Yi(n))

    ! Random generate numbers in Xi and Yi
    do i = 1, n
        call random_number(Xi(i))
        call random_number(Yi(i))
    end do

    ! Print the arrays
    print *, 'Xi = ', Xi
    print *, 'Yi = ', Yi

    ! Deallocate array
    deallocate(Xi, Yi)
end program LinearRegression