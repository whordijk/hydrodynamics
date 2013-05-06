module FindNeighbors
    implicit none
    private

    public find_neighbors

    contains

    subroutine find_neighbors(positions, distances)

        real(8),intent(in) ::  positions(:,:)
        real(8),intent(out) :: distances(:,:)
        integer :: N,i,j

        N = size(positions,dim=1)
        
        do i=1,N
            do j=1,N
                distances(:,(i-1)*N+j) = positions(:,i)-positions(:,j)
            end do
        end do


        do i=1,N
            distances(1,i*N)=999
            distances(2,i*N)=999
            distances(3,i*N)=999
        end do

    end subroutine

end module
