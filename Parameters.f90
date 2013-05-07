module Parameters

    implicit none

    integer, parameter :: N = 512
    integer, parameter :: T = 3000
    real(8), parameter :: dt = 1d-3
    real(8), parameter :: c_s = 10, mu = 1, h = 2
    character(len=*), parameter :: filename="out_positions.txt"

end module
