!
!  Description: A messy fortran code to check demangling subroutine in tools
!
!                               written by JaeHyuk Kwack (jkwack@anl.gov)
! ------------------------------------------------------------------------------
module JK_shared
   use omp_lib
   implicit none


   type::data_type
      integer                                   :: n
      real(kind=8),dimension(:),allocatable     :: DP
      real(kind=4),dimension(:),allocatable     :: SP
   end type data_type


   type::L_type
      character(len=50)                         :: names= ' '
      integer                                   :: level
      integer                                   :: max_level
      type(data_type)                           :: D
   end type L_type


   contains


   subroutine alloc_data(this_data,n)
      integer,intent(in)                       :: n
      type(data_type),intent(inout)            :: this_data

      this_data%n = n;
      allocate(this_data%DP(n));
      allocate(this_data%SP(n));

   end subroutine alloc_data


   subroutine init_data(this_data)
      type(data_type),intent(inout)            :: this_data
      integer                                  :: i,j,n
      real(kind=8),dimension(:),allocatable    :: tmp

      n = this_data%n
      allocate(tmp(n))

      !$omp target enter data map (to:this_data)

      !$omp target teams distribute parallel do map(tmp)
         do i=1,n
            !this_data%DP(i) = i*100.0
            tmp(i) = i*100.0
            do j=1,i
               tmp(i) = tmp(i) + j
            enddo
         enddo 
      !$omp end target teams distribute parallel do

   end subroutine init_data


   subroutine init_level(names,level,this_level)
      character(len=50),intent(in)              :: names
      integer,intent(in)                        :: level
      type(L_type),intent(inout)                :: this_level
      type(L_type)                              :: sub_level
      character(len=50)                         :: sub_name

      this_level%names = names
      this_level%level = level
      
      write(*,'(5x,A40,I6)') 'Initializing a level for ',level

      write(*,'(30x,A)') '- allocating array for the level'
      call alloc_data(this_level%D,level*100)

      write(*,'(30x,A)') '- initializing array for the level'
      call init_data(this_level%D)

      if (level .gt. 1) then
         write(sub_name,"(A5,I6)") "Level",level-1
         call create_sublevel(sub_name,level-1,sub_level)
      endif

   end subroutine init_level


   subroutine create_sublevel(names,l,sub_level)
      character(len=50),intent(in)              :: names
      integer,intent(in)                        :: l
      type(L_type),intent(inout)                 :: sub_level

      call init_level(names,l,sub_level)

   end subroutine create_sublevel



end module JK_shared


program main
   use JK_shared
   implicit none

   character(len=32)                            :: arg
   integer                                      :: max_level = 10

   type(L_type)                                 :: L0
   character(len=50)                            :: tmp_names

   integer                                      :: i,j

   ! Reading command line arguments
   do i=1,iargc()
      call getarg(i,arg)
      if (i==1) read(arg,*) max_level
   enddo
   write(*,'(5x,A40,I6)') 'Maximum level for testing: ',max_level

   tmp_names='max_level'
   call init_level(tmp_names,max_level,L0)

end program main
!
!
!
