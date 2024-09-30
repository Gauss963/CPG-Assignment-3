subroutine LinearRegressionReadData(Xi, Yi, n)
    implicit none
    integer, intent(inout) :: n
    real, allocatable, intent(inout) :: Xi(:), Yi(:)
    character(len = 100) :: line
    integer :: i, unit_num

    open(newunit=unit_num, file = "../data/ppfile.txt", status = "old", action = "read")

    ! Skipping header
    read(unit_num, '(A)') ! Do nothing
    
    ! Get the number of lines
    n = 0
    do while (.true.)
        read(unit_num, '(A)', iostat=i) line
        if (i /= 0) exit
        n = n + 1
    end do

    ! Read the datas and skipping header
    rewind(unit_num)
    read(unit_num, '(A)') ! Do nothing

    ! Allocate the arrays
    allocate(Xi(n), Yi(n))

    ! Read data and fill into Xi and Yi
    ! Fix the reading and filling here (2024/09/23)

    do i = 1, n
        read(unit_num, '(6X, F5.1, F4.0)') Xi(i), Yi(i)
    end do

    close(unit_num)

    print *, Xi
    print *, "--------------"

end subroutine LinearRegressionReadData
