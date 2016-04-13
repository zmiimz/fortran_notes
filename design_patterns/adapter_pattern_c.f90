!Copyright (c) 2016 M. Zapukhlyak
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!This set of modules implements adapter design pattern based on the well-known uml diagram
!tested with GNU gfortran 5.3.1 and Intel ifort 16.0
! This kind of adapter pattern approximates (partly) a "class adapter pattern"
! note that there is no multiple inheritance in fortran and we are not allowed
! to modify target_type nor adaptee_type
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
! type adapter_type is derived from adaptee and contains procedure pointer
! component with target method interface
!-----------------------------------------------------------------------
module adapter_module
   use target_module
   use adaptee_module
   implicit none
   private ! all by default
   public :: adapter_type

   ! adapter_type adapts the interface of adaptee to the target interface and
   ! provides the link between the incompatible client and adaptee classes
   type, extends(adaptee_type) :: adapter_type
      integer :: two
      integer :: three
      real :: and_something_else
      class(target_type), private, pointer :: concrete_target
      procedure (target_type_procedure), pointer :: request => NULL()
   contains
      procedure, pass :: init => adapter_init
   end type adapter_type

   ! target method interface but for adapter_type
   interface
      subroutine target_type_procedure(this, one)
         import  :: adapter_type
         class(adapter_type), intent(in) :: this
         integer, intent(in) :: one
      end subroutine target_type_procedure
   end interface

contains

   !-----------------------------------------------------------------------
   !Subroutine adapter_request
   !-----------------------------------------------------------------------
   subroutine  adapter_request(this, one)
      implicit none
      class(adapter_type), intent(in) :: this
      integer, intent(in) :: one
      logical :: res
      write(*,*) "  adapter_request"
      if(.not. associated(this % request)) stop ".not. associated(this % request)"
      res = this % specific_request(this % two, this % three, this % and_something_else)
      write(*,*) "  res = ", res
   end subroutine adapter_request

   !-----------------------------------------------------------------------
   !Subroutine adapter_init
   ! pass additional info needed for specific_request in adaptee
   ! we have adaptee interface already
   !-----------------------------------------------------------------------
   subroutine  adapter_init(this, two, three, and_something_else, target_object)
      implicit none
      class(adapter_type), intent(inout) :: this
      integer, intent(in) :: two, three
      real, intent(in) :: and_something_else
      class(target_type), target, intent(in) :: target_object ! sometimes it is needed in adapter
      write(*,*) "  adapter_init"
      this % two = two
      this % three = three
      this % and_something_else = and_something_else
      this % request => adapter_request ! bind target-like method
      this % concrete_target => target_object
   end subroutine adapter_init

end module adapter_module

!-----------------------------------------------------------------------
!Module client_module
!-----------------------------------------------------------------------
module client_module
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
   !
   !-----------------------------------------------------------------------
   subroutine  client_run(this, object)
      use target_module
      use adapter_module
      implicit none
      class(client_type), intent(in) :: this
      class(*), intent(in) :: object ! some polymorphic object
      write(*,*) "client_run"

      ! object has method similar to the target method
      select type (object)
       class is (target_type)
         call object % request(1)
       class is(adapter_type)
         call object % request(1)
       class default
         stop "unknown object"
      end select
   end subroutine client_run

end module client_module

!-----------------------------------------------------------------------
!Main program test
!-----------------------------------------------------------------------
program    test
   use client_module
   use target_module
   use adapter_module
   implicit none

   type(client_type) :: client
   type(target_type) :: t
   type(adapter_type) :: a

   call client % run(t)
   write(*,*)"------------------------------------------------------------"
   call a % init(2, 3, 3.1415,t) ! init adapter
   call client % run(a)

end program test
