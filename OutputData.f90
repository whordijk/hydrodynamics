module OutputData

    use Parameters

    implicit none
    private


    public output_end
    public output_continuous
    public set_output

    contains
    subroutine set_output()
        open(unit =24,file=out_file)
            write(24,*) "# ", N
    end subroutine

    subroutine output_end(positions)
        real(8), intent(in) :: positions(:,:)
        integer :: i

        open(unit = 26, file=in_file)

        do i = 1, N
            write(26,*) positions(:, i)
        end do

        close(26)
        close(24)
    
    end subroutine

    subroutine output_continuous(positions)
        real(8), intent(in) :: positions(:,:)
        integer :: i
        do i = 1, N
            write(24,*) positions(:, i)
        end do
    end subroutine

end module
