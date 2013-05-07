module Parameters

    implicit none

    integer, parameter :: N = 125
    integer, parameter :: T = 300
    real(8), parameter :: dt = 1d-3
    real(8), parameter :: c_s = 1, mu = 1
    character(len=*), parameter :: filename="out_positions.txt"

end module
