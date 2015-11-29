!Copyright (c) 2015 M. Zapukhlyak
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!-----------------------------------------------------------------------
!Module singleton_module implements simple singleton design pattern
!-----------------------------------------------------------------------

! Due to Fortran OOP specifics it is impossible to mimic this pattern
! exactly like in java or C++. Іn accordance with agreement among Fortran
! developers the singleton pattern is assumed to be a declared public
! single instance (pointer) of singleton_type while the derived
! singleton_type itself remains private.
! This prevents declaration of additional instances of that type. The
! manipulation of singleton content is only allowed through this pointer
! instance.

! In that way almost all criteria of singleton are satisfied:
!
! * there is only one such object in the program
! * one can get access to it from anywhere in the program
! * it is an object and one can retrieve data from it and store new data
! * the instance is polymorphic and is in principle extensible (but only
! in original singleton_module) and clients are able to use an extended
! instance without modifying their code.
!
! We do not consider here in details deleting of singleton instance
! because it is "a non-trivial design problem" in almost all languages.

!tested with GNU gfortran 5.2.1 and Intel ifort 16.0
! ifort -assume realloc_lhs singleton_pattern.f90 -o singleton_pattern.x
! gfortran singleton_pattern.f90 -o singleton_pattern.x

!-----------------------------------------------------------------------
!Module singleton_module
!-----------------------------------------------------------------------
module singleton_module
   implicit none
   private ! all by default
   public :: singleton, singleton_c, singleton_d

   type singleton_type
      integer, dimension(:), allocatable :: some_data
   contains
      procedure, pass :: get => get_singleton_data
      procedure, pass :: set => set_singleton_data
      final :: finalize_singleton
   end type singleton_type

   class(singleton_type), pointer :: singleton => null() ! <---- singleton instance

   type, extends(singleton_type) :: extended_singleton_type
      integer, dimension(:), allocatable :: more_data
   contains
      procedure, pass :: get => get_extended_singleton_data
      procedure, pass :: set => set_extended_singleton_data
      final :: finalize_extended_singleton
   end type extended_singleton_type

   ! constructor interface for singleton and extended singleton
   interface singleton_c
      module procedure create_singleton
      module procedure create_extended_singleton
   end interface singleton_c

   ! destructor interface for singleton and extended singleton
   interface singleton_d
      module procedure destroy_singleton
      !       module procedure destroy_extended_singleton ! not needed
   end interface singleton_d

contains

   !-----------------------------------------------------------------------
   !Subroutine create_singleton
   !-----------------------------------------------------------------------
   subroutine create_singleton(some_data)
      integer, dimension(:), intent(in) :: some_data
      write(*,*)"create_singleton: "
      if (.not. associated(singleton)) then
         allocate(singleton, mold = singleton_type())
         ! and init some_data of singleton
         allocate(singleton % some_data(size(some_data)), source=some_data)
      endif
   end subroutine create_singleton

   !-----------------------------------------------------------------------
   !Subroutine finalize_singleton
   !-----------------------------------------------------------------------
   subroutine finalize_singleton(this)
      type(singleton_type) :: this
      write(*,*)"finalize_singleton: "
      ! delete some_data of singleton
      if(allocated(this % some_data)) deallocate(this % some_data)
   end subroutine finalize_singleton

   !-----------------------------------------------------------------------
   !Subroutine destroy_singleton
   !-----------------------------------------------------------------------
   subroutine destroy_singleton()
      write(*,*)"destroy_singleton: "
      if (associated(singleton)) deallocate(singleton)
      singleton => null()
   end subroutine destroy_singleton

   !-----------------------------------------------------------------------
   !Function get_singleton_data
   !-----------------------------------------------------------------------
   function get_singleton_data(this, id) result(some_data)
      class(singleton_type) :: this
      integer, optional, intent(in) :: id
      integer, dimension(:), allocatable :: some_data
      write(*,*)"get_singleton_data: "
      if(present(id)) then
         select case (id)
          case (1)
            some_data = this % some_data
          case default
            error stop "unknown data id"
         end select
      else
         some_data = this % some_data
      endif
   end function get_singleton_data

   !-----------------------------------------------------------------------
   !Subroutine set_singleton_data
   !-----------------------------------------------------------------------
   subroutine set_singleton_data(this, idata, id)
      class(singleton_type) :: this
      integer, dimension(:), intent(in) :: idata
      integer, optional, intent(in) :: id
      write(*,*)"set_singleton_data: "
      this % some_data = idata ! reallocation on assignment not suppoerted by default in ifort, use -assume realloc_lhs or:
      ! if(allocated(this % some_data)) deallocate(this % some_data)
      ! allocate(this % some_data(size(idata)), source=idata)
   end subroutine set_singleton_data

   ! and for extended_singleton_type

   !-----------------------------------------------------------------------
   !Subroutine create_extended_singleton
   !-----------------------------------------------------------------------
   subroutine create_extended_singleton(some_data, more_data)
      integer, dimension(:), intent(in) :: some_data
      integer, dimension(:), intent(in) :: more_data
      write(*,*)"create_extended_singleton: "
      if (.not. associated(singleton)) then
         allocate(singleton, mold = extended_singleton_type())
         ! init some_data and more_data of singleton
         allocate(singleton % some_data(size(some_data)), source=some_data)
         select type (singleton)
          type is (extended_singleton_type)
            allocate(singleton % more_data(size(more_data)), source=more_data)
          class default
            error stop "wrong singleton type"
         end select
      endif
   end subroutine create_extended_singleton

   !-----------------------------------------------------------------------
   !Subroutine finalize_extended_singleton
   !-----------------------------------------------------------------------
   subroutine finalize_extended_singleton(this)
      type(extended_singleton_type) :: this
      write(*,*)"finalize_extended_singleton: "
      ! delete more_data of extended_singleton
      ! some_data will be deallocated in final of singleton_type: finalize_singleton
      if(allocated(this % more_data)) deallocate(this % more_data)
   end subroutine finalize_extended_singleton

   !-----------------------------------------------------------------------
   !Function get_extended_singleton_data
   !-----------------------------------------------------------------------
   function get_extended_singleton_data(this, id) result(idata)
      class(extended_singleton_type) :: this
      integer, optional, intent(in) :: id
      integer, dimension(:), allocatable :: idata
      write(*,*)"get_extended_singleton_data: "
      if(present(id)) then
         select case (id)
          case (1)
            idata = this % some_data
          case (2)
            idata = this % more_data
          case default
            error stop "unknown data id"
         end select
      else
         idata = this % some_data
      endif
   end function get_extended_singleton_data

   !-----------------------------------------------------------------------
   !Subroutine set_extended_singleton_data
   !-----------------------------------------------------------------------
   subroutine set_extended_singleton_data(this, idata, id)
      class(extended_singleton_type) :: this
      integer, dimension(:), intent(in) :: idata
      integer, optional, intent(in) :: id
      write(*,*)"set_extended_singleton_data: "
      if(present(id)) then
         select case (id)
          case (1)
            this % some_data = idata
            ! if(allocated(this % some_data)) deallocate(this % some_data)
            ! allocate(this % some_data(size(idata)), source=idata)
          case (2)
            this % more_data = idata
            ! if(allocated(this % more_data)) deallocate(this % more_data)
            ! allocate(this % more_data(size(idata)), source=idata)
          case default
            error stop "unknown data id"
         end select
      else
         this % some_data = idata
         ! if(allocated(this % some_data)) deallocate(this % some_data)
         ! allocate(this % some_data(size(idata)), source=idata)
      endif
   end subroutine set_extended_singleton_data

end module singleton_module

!-----------------------------------------------------------------------
!Main program test_singleton
!-----------------------------------------------------------------------
program    test_singleton
   use singleton_module
   implicit none
   integer, dimension(5) :: some_data, more_data, idata

   some_data = [1,2,3,4,5]
   more_data = [6,7,8,9,10]
   idata = [0,1,0,1,0]

   ! create singleton object
   call singleton_c(some_data)
   ! check constructor
   call singleton_c(some_data) ! no new instance
   write(*,*) singleton % get()
   call singleton % set(idata)
   write(*,*) singleton % get()
   write(*,*) singleton % get(1)
   call singleton_d()
   call singleton_d()  ! already destroyed

   ! create and use extended singleton object
   ! using similar interface but with more_data
   call singleton_c(some_data, more_data)
   ! check constructor
   call singleton_c(some_data) ! no new instance
   call singleton_c(some_data, more_data) ! no new instance

   write(*,*) singleton % get()
   write(*,*) singleton % get(1)
   write(*,*) singleton % get(2)

   call singleton % set(idata)
   write(*,*) singleton % get()
   call singleton % set(idata,1)
   write(*,*) singleton % get(1)

   call singleton % set(idata,2)
   write(*,*) singleton % get(2)

   call singleton_d()
   call singleton_d()  ! already destroyed
end program test_singleton
