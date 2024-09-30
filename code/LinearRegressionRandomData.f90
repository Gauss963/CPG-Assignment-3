subroutine LinearRegressionRandomData(Xi, Yi, n)
    implicit none
    integer, intent(in) :: n
    real, dimension(n), intent(out) :: Xi, Yi
    integer :: i

    call random_seed()

    do i = 1, n
        call random_number(Xi(i))
        call random_number(Yi(i))
    end do
end subroutine LinearRegressionRandomData