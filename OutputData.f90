module OutputData

    use Parameters

    implicit none
    private


    public output_data
    public set_output

    contains
    subroutine set_output()
        open(unit =24,file=filename)
        close(24)
    end subroutine

    subroutine output_data(positions)
        real(8), intent(in) :: positions(:,:)
        integer :: i

        open(unit = 24, file=filename,access="APPEND")

        do i = 1, N
            write(24,*) positions(:, i)
        end do

        close(24)
    
    end subroutine

end module
