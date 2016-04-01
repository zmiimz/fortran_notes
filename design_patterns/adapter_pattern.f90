!Copyright (c) 2016 M. Zapukhlyak
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!This set of modules implements adapter design pattern based on the well-known uml diagram
!tested with GNU gfortran 5.3.1 and Intel ifort 16.0
! Client calls operations of target or adapter instance and adapter calls adaptee operations
! Adapter (wrapper) makes things work after they're designed, it has publicly the interface
! of the target type, and privately inherits the implementation of the adaptee type.

!-----------------------------------------------------------------------
!Module target_module
!-----------------------------------------------------------------------
module target_module
   implicit none
   private ! all by default
   public :: target_type

   ! target_type has interface that client can use
   type target_type
   contains
      procedure, pass :: request => target_request
   end type target_type

contains
   !-----------------------------------------------------------------------
   !Subroutine target_request
   !-----------------------------------------------------------------------
   subroutine  target_request(this, one)
      implicit none
      class(target_type), intent(in) :: this
      integer, intent(in) :: one
      write(*,*) "  target_request"
      write(*,*) "  one = ", one
   end subroutine target_request

end module target_module

!-----------------------------------------------------------------------
!Module client_module
!-----------------------------------------------------------------------
module client_module
   use target_module
   implicit none
   private ! all by default
   public :: client_type
   ! interacts  with objects conforming to target_type interface.
   type client_type
   contains
      procedure, pass :: run => client_run
   end type client_type

contains

   !-----------------------------------------------------------------------
   !Subroutine client_run
   !-----------------------------------------------------------------------
   subroutine  client_run(this, object)
      implicit none
      class(client_type), intent(in) :: this
      class(target_type), intent(in) :: object
      write(*,*) "client_run"
      call object % request(1)

   end subroutine client_run

end module client_module

!-----------------------------------------------------------------------
!Module adaptee_module
!-----------------------------------------------------------------------
module adaptee_module
   implicit none
   private ! all by default
   public :: adaptee_type

   !  adaptee has interface that is incompatible with client and needs adapting.
   type adaptee_type
   contains
      procedure, pass :: specific_request
   end type adaptee_type

contains

   !-----------------------------------------------------------------------
   !Function specific_request
   !-----------------------------------------------------------------------
   function  specific_request(this, two, three, and_something_else)
      implicit none
      class(adaptee_type), intent(in) :: this
      integer, intent(in) :: two 
      integer, intent(in) :: three
      real ::  and_something_else
      logical :: specific_request

      write(*,*) "  specific_request"
      write(*,*) "  two = ", two, " three = ", three, " and_something_else = ", and_something_else
      specific_request = .true.
   end function specific_request

end module adaptee_module

!-----------------------------------------------------------------------
!Module adapter_module
! type contains only pointer on adaptee instance
!-----------------------------------------------------------------------
module adapter_module
   use target_module
   use adaptee_module
   implicit none
   private ! all by default
   public :: adapter_type

   ! adapter_type adapts the interface of adaptee to the target interface and
   ! provides the link between the incompatible client and adaptee classes
   type, extends(target_type) :: adapter_type
      integer :: two
      integer :: three
      real :: and_something_else
      class(adaptee_type), private, pointer :: current_adaptee => null() ! private instance of the adaptee_type
   contains
      procedure, pass :: request => adapter_request
      procedure, pass :: init => adapter_init
   end type adapter_type

contains

   !-----------------------------------------------------------------------
   !Subroutine adapter_init
   ! pass additional info needed for specific_request in adaptee
   ! we obtain adaptee instance as argument
   !-----------------------------------------------------------------------
   subroutine  adapter_init(this, two, three, and_something_else, adaptee)
      implicit none
      class(adapter_type), intent(inout) :: this
      integer, intent(in) :: two, three
      real, intent(in) :: and_something_else
      class(adaptee_type), target, intent(in) :: adaptee

      write(*,*) "  adapter_init"
      this % two = two
      this % three = three
      this % and_something_else = and_something_else
      this % current_adaptee => adaptee
   end subroutine adapter_init

   !-----------------------------------------------------------------------
   !Subroutine adapter_request
   !-----------------------------------------------------------------------
   subroutine  adapter_request(this, one)
      implicit none
      class(adapter_type), intent(in) :: this
      integer, intent(in) :: one
      logical :: res
      write(*,*) "  adapter_request"
      if(.not. associated(this % current_adaptee)) stop ".not. associated(this % current_adaptee)"
      res = this % current_adaptee % specific_request(this % two, this % three, this % and_something_else)
      write(*,*) "  res = ", res

   end subroutine adapter_request
end module adapter_module


!-----------------------------------------------------------------------
!Module adapter_simple_module
! type contains private instance of adaptee
!-----------------------------------------------------------------------
module adapter_simple_module
   use target_module
   use adaptee_module
   implicit none
   private ! all by default
   public :: adapter_simple_type

   ! adapter_type adapts the interface of adaptee to the target interface
   type, extends(target_type) :: adapter_simple_type
      integer :: two
      integer :: three
      real :: and_something_else
      class(adaptee_type), private, allocatable :: current_adaptee ! private instance of the adaptee_type
   contains
      procedure, pass :: request => adapter_simple_request
      procedure, pass :: init => adapter_simple_init
   end type adapter_simple_type

contains

   !-----------------------------------------------------------------------
   !Subroutine adapter_simple_init
   ! pass additional info needed for specific_request in adaptee
   ! we obtain adaptee instance as argument
   !-----------------------------------------------------------------------
   subroutine  adapter_simple_init(this, two, three, and_something_else)
      implicit none
      class(adapter_simple_type), intent(inout) :: this
      integer, intent(in) :: two, three
      real, intent(in) :: and_something_else

      write(*,*) "  adapter_simple_init"
      this % two = two
      this % three = three
      this % and_something_else = and_something_else
      allocate(adaptee_type :: this % current_adaptee) ! typed allocation
   end subroutine adapter_simple_init

   !-----------------------------------------------------------------------
   !Subroutine adapter_simple_request
   !-----------------------------------------------------------------------
   subroutine  adapter_simple_request(this, one)
      implicit none
      class(adapter_simple_type), intent(in) :: this
      integer, intent(in) :: one
      logical :: res
      write(*,*) "  adapter_simple_request"
      if(.not. allocated(this % current_adaptee))  stop ".not. allocated(this % current_adaptee)"
      res = this % current_adaptee % specific_request(this % two, this % three, this % and_something_else)
      write(*,*) "  res = ", res

   end subroutine adapter_simple_request
end module adapter_simple_module

!-----------------------------------------------------------------------
!Main program test
!-----------------------------------------------------------------------
program    test
   use client_module
   use target_module
   use adapter_module
   use adapter_simple_module
   use adaptee_module
   implicit none

   type(client_type) :: client
   type(target_type) :: t

   type(adapter_type) :: a
   type(adapter_simple_type) :: as

   type(adaptee_type), target :: adaptee

   ! init adaptor using constructor
   ! a = adapter_type(2, 3, 3.1415, adaptee) ! internal compiler error in gfortran
   ! as = adapter_simple_type(2, 3, 3.1415)

   call client % run(t)
   write(*,*)"------------------------------------------------------------"
   call a % init(2, 3, 3.1415, adaptee) ! init adapter using specific method and pass adaptee instance as pointer into
   call client % run(a)
   write(*,*)"------------------------------------------------------------"
   call as % init(2, 3, 3.1415) ! init adapter using specific method (adaptee instance created internally)
   call client % run(as)

end program test
