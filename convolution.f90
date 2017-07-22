!This program will perform convolution on a given matrix (mat) with the kernal
!The output will be the convoluted matrix (totals) 
program convolution
	implicit none
	
	!Matrix to be convoluted
	integer, dimension(0:4,0:4) :: mat(0:4,0:4) = reshape((/0,0,0,0,0, &
															0,1,4,7,0, &
															0,2,5,8,0, &
															0,3,6,9,0, &
															0,0,0,0,0/),(/5,5/))
	
	!Kernal is the projected matrix
	integer, dimension(2,2) :: kernal(0:2,0:2) = reshape((/-1,0,1,-2,0,2,-1,0,1/),(/3,3/))
	
	!Flipped results of calling subroutine flip
	integer, dimension(0:2,0:2) :: flip_kernal(0:2,0:2)
	
	!The results of the convolution		
	integer, dimension(0:2,0:2) :: totals = 0
	
	!Helper variables
		!i and j always start with value of 1 when the subroutine is called
		!m and n position the kernal onto the correct square in mat
		!this_total is a placeholder for part of the subroutine's calculations
		!total is returned as the sum of all the products of the 9 squares on a particular coordinate
		!temp is the return variable as the subroutine calls itself, which adds its value to the total
	integer :: i = 1, j = 1, m = 0, n = 0, this_total = 0, total = 0, temp = 0
	
	!Print out the matrix to be convoluted
	write(*,*) "mat: "
	call printArray(mat, 0, 4) 	
	
	!Print out the kernal before it is flipped
	write(*,*) "kernal: "
	call printArray(kernal, 0, 2) 	
		
	!Performing the flip to receive the new kernal to do the convolution with in the next step
	call flip(kernal, flip_kernal, 0, 2)
	
	!Print out the flipped kernal to be projected onto mat
	write(*,*) "flipped kernal: "
	call printArray(flip_kernal, 0, 2) 	
	
	!m is the row index
	do m = 0,2
		!n is the column index
		do n = 0,2
			!Need to set these 4 varaibles before the subroutine is called again
			i = 1
			j = 1
			this_total = 0
			total = 0
			
			!The total variable is the return value of this subroutine, which is what the value should be 
			!in the final convoluted matrix
     		call calc(flip_kernal, mat, i, j, m, n, this_total, total, temp)
			
			totals(m,n) = total		
		end do
	end do
	
	!Print out the final result of the convolution
	write(*,*) "totals: "
	call printArray(totals, 0, 2)
	

end program convolution

!Flips array vertically and horizontally
!The return value is the flip_array (array returned as horizontally and vertically flipped)
subroutine flip(array, flip_array, m, n)
		implicit none
		
		!m and n are the start and end index of the arrays
		!i and j are used in the do loops and set with the m and n values
		integer :: m, n, i, j
		
		!Used only within the subroutine
		integer, intent(in), dimension(m:n,m:n) :: array
		
		!Will be returned at the end of the subroutine
		integer, intent(out), dimension(m:n,m:n) :: flip_array
		
		do i = m, n
			do j = n, m, -1			
				flip_array(i,(j-n)*(-1)) = array((i-n)*(-1),j)			
			end do
		end do
		
end subroutine flip

!When this subroutine is called from the program, one square is calculated by scanning the 9 squares and adding up their products
!The return value is the value that will be placed in the totals array 
recursive subroutine calc(kernal, mat, i, j, addToi, addToj, this_total, total, temp)
	 implicit none 
	
	 !declare calling parameter types and definitions
	
	 !to calculate the kernal onto the mat
	 integer, intent(in), dimension(0:4,0:4) :: mat
	 integer, intent(in), dimension(3,3) :: kernal

	 !Affects the i and j position of where the kernal is placed
	 integer, intent(in) :: addToi, addToj
	 
	 !Can be calculated with
	 integer, intent(out) :: i, j, this_total
	 	 
	 !The sum of the products of the 9 squares that kernal overlapped onto the mat
	 integer, intent(out) :: total
	
	 !temp variable
	 integer :: temp
	 
	!j may be out of bounds
    if (j > 3) then
		i = i + 1
		j = 1
	end if
	!i may be out of bounds
	if (i <= 3) then
		
		!The product of the kernal square over its parallel position on the mat
		this_total = this_total + kernal(i,j) * mat((i + addToi) - 1, (j + addToj) - 1)
		
		!Add the above product to the overall total to be outputted
		total = total + this_total
		
		!Increase j so the next move can be called
	    j = j+1	
		call calc(kernal, mat, i, j, addToi, addToj, this_total, total, temp)
			
		total = this_total + temp
	else
		total = 0
	end if
	
end subroutine calc

!Prints out the given array with the start and end indexes (m, n)
subroutine printArray(array, m, n)
	implicit none
	
	!Used only within the subroutine to print the array
	integer, intent(in), dimension(m:n, m:n) :: array
	
	!r will loop through the values from m to n
	integer :: r, m, n
	
	do r = m, n
		write(*,*) array(r,:) 
	end do
	write(*,*) " "

end subroutine printArray
