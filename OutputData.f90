module OutputData
    implicit none
    private

    public output_data(positions)

    contains

    subroutine output_data(positions)
        real(8), intent(in) :: positions(:,:)

        open(unit = 24, file="out_positions.txt")

        write(24,*) positions

        close(24)
end module
