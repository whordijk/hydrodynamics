module TimeIntegrate

    use Parameters

    implicit none
    private
    
    public update

    real(8) :: rho(N), pressure(N), V(3, N, N)

contains

    subroutine update(positions, velocities, accelerations)

        real(8), intent(inout) :: positions(:, :), velocities(:, :), accelerations(:, :)

        velocities = velocities + accelerations * dt / 2
        positions = positions + velocities * dt
        call update_accelerations(positions, velocities, accelerations)
        velocities = velocities + accelerations * dt / 2

    end subroutine

    subroutine update_accelerations(positions, velocities, accelerations)

        real(8), intent(inout) :: positions(:, :), velocities(:, :), accelerations(:, :)
        integer :: i, j
        real(8) :: q

        accelerations = 0

        rho = 0
        do i = 1, N
            do j = 1, N
                q = sqrt(sum((positions(:, i) - positions(:, j))**2))
                if (q <= h) then
                    rho(i) = rho(i) + Wd(q)
                end if
            end do
        end do
        rho = mass * rho
        
        call calc_pressure(positions,accelerations)
        call calc_viscosity(positions, velocities,accelerations)
        call calc_surface(positions, accelerations)
        call calc_boundaries(positions,velocities, accelerations)

        accelerations(3, :) = accelerations(3, :) - 9.81d0
        
    end subroutine

    real(8) function Wd(q) result(weight)

        real(8), intent(in) :: q

        weight = 315 * (h**2 - q**2)**3 / (64 * pi * h**9)

    end function

    function delWd(q, rij)

        real(8), dimension(3) :: delWd
        real(8), intent(in) :: q
        real(8), intent(in) :: rij(:)

        delWd = -945 * (h**2 - q**2)**2 * rij / (32 * pi * h**9) 

    end function
    
    real(8) function del2Wd(q) result(weight)

        real(8), intent(in) :: q

        weight = -945 / (32 * pi * h**9) * (h**2 - q**2) * (3 * h**2 - 7 * q**2)

    end function
    
    function delWp(q, rij)
    
        real(8), dimension(3) :: delWp
        real(8), intent(in) :: q
        real(8), intent(in) :: rij(:)

        delWp = -45 * (h - q)**2 / (pi * h**6 * q) * rij

    end function

    real(8) function del2Wv(q) result(weight)

        real(8), intent(in) :: q

        weight = 45 * (h - q) / (pi * h**6)

    end function

    subroutine calc_pressure(positions,a)

        real(8) :: positions(:,:),a(:,:), P(3), rij(3)
        integer :: i, j

        pressure = c_s**2 * (rho - rho_0)
        do i = 1, N
            do j = i + 1, N        
                rij = positions(:,j) - positions(:,i)
                if (sum(rij**2) < h**2) then                   
                    P = mass * (pressure(i) / rho(i)**2 + pressure(j) / rho(j)**2) * delWp(sqrt(sum(rij**2)),rij) 
                    a(:, i) = a(:, i) + P                                            
                    a(:, j) = a(:, j) - P
                end if
            end do                                           
        end do                                                               
                                                                            
    end subroutine                                                           
                                                                             
   subroutine calc_viscosity(positions, velocities, a)                                  
                                                                            
        real(8) :: a(:,:)                                                    
        real(8), intent(in) :: positions(:, :), velocities(:, :)                             
        integer :: i, j                                                
        real(8) :: V(3), rij(3)                                                     
                                                                            
        do  i = 1, N
            do j = i + 1, N
                rij = positions(:, j) - positions(:, i)
                if (sum(rij**2) < h**2) then
                    V = mu * mass * (velocities(:, j) - velocities(:, i)) / (rho(i)  * rho(j)) &
                        * del2Wv(sqrt(sum(rij**2)))
                    a(:, i) = a(:,i) + V                        
                    a(:, j) = a(:,j) - V
                end if                               
            end do
       end do                                                              
                                                                            
   end subroutine                                                          
                                                                             
    subroutine calc_surface(r, a)                                               
                                                                             
        real(8) :: normal(3, N), norm, K(N), rij(3)                            
        real(8), intent(inout) :: a(:, :)         
        real(8), intent(in) :: r(:, :)                           
        integer :: i, j                                                      

        normal = 0
        do i = 1, N
            do j = 1, N
                if (i==j) then
                    cycle
                end if 
                rij = r(:, j) - r(:, i)
                if (sum(rij**2) < h**2) then
                    normal(:, i) = normal(:, i) + mass * delWd(sqrt(sum(rij**2)), rij) / rho(j)
                end if
            end do
        end do

        K = 0
        do i = 1, N
            do j = 1, N
                rij = r(:, j) - r(:, i)
                K(i) = K(i) + mass / rho(j) * del2Wd(sqrt(sum(rij**2)))
            end do
        end do

        do i = 1, N
            norm = sqrt(sum(normal(:, i)**2))
            if (norm > 0) then
                a(:, i) = a(:, i) + sigma / rho(i) * K(i) * normal(:, i) / norm
            end if
        end do

    end subroutine                                                          
                                                                            
    subroutine calc_boundaries(positions, velocities, a)                     
                                                                             
        real(8) :: a(:,:)                                                    
        real(8) :: normal(3,5), normal_k(3), d(5), d_k
        integer :: i,j,k
        real(8), intent(in):: positions(:,:), velocities(:, :)
        real(8) :: proj(3,N), test(N)
 
        normal(:,1) = [1, 0, 0]
        normal(:,2) = [-1, 0, 0]
        normal(:,3) = [0, 1, 0]
        normal(:,4) = [0, -1, 0]
        normal(:,5) = [0, 0, 1]
        !d = 4 * [init_size / 2, init_size / 2, init_size / 2, init_size / 2, 0d0]
        d = [0.5d0, 0.5d0, 0.5d0, 0.5d0, 0d0]        
 
        do k =1,5
            normal_k = normal(:,k)
            d_k = d(k)
            proj = 0
            do i =1, 3
                proj(i,:) = proj(i,:) + positions(i,:) * normal_k(i)
            end do
            test = sum(proj, dim=1) + d_k
            do j = 1, N
                if (test(j) < 0) then
                    a(:, j) = a(:, j) - f * 100 * exp(-test(j)) * velocities(:, j) / sqrt(sum(velocities(:, j)**2))
                    a(:, j) = a(:, j) + 100 * exp(-test(j)) * normal(:, k)
                end if
            end do
        end do
 
    end subroutine
    
end module
