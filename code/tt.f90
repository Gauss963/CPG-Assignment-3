program LinearRegressionMain

    implicit none

    integer, parameter :: dp = kind(1.0d0)
    real(dp), allocatable :: x(:), y(:), theta(:)
    real(dp) :: alpha, tol
    integer :: max_iter, i

    ! Initialize parameters
    alpha = 0.01_dp
    tol = 1.0e-6_dp
    max_iter = 1000

    ! Allocate memory for input data
    allocate(x(100), y(100), theta(2))
    ! Load or generate your data for x and y here

    ! Call the linear regression function
    call LinearRegression(x, y, theta, alpha, tol, max_iter)

    ! Deallocate memory
    deallocate(x, y, theta)

end program LinearRegressionMain