module OutputData

    use Parameters

    implicit none
    private


    public output_data

    contains

    subroutine output_data(positions)
        real(8), intent(in) :: positions(:,:)
        integer :: i

        open(unit = 24, file="out_positions.txt")

        do i = 1, N
            write(24,*) positions(:, i)
        end do

        close(24)
    
    end subroutine

end module
