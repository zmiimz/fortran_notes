!Copyright (c) 2015 M. Zapukhlyak
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!-----------------------------------------------------------------------
!This set of modules implements example of bridge design pattern:
! -> "decouple an abstraction from its implementation so that the two can vary independently"
! works with GNU gfortran 7.2.1 and ifort 18
! simple implementation of bridge = abstract abstraction_type + abstract implementor_type
! using allocatable implementor in the "abstraction" type
!
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
!Module abstract_implementor_module
!-----------------------------------------------------------------------
module abstract_implementor_module
   implicit none
   private ! all by default
   public :: abstract_implementor_type

   type, abstract :: abstract_implementor_type
      !
   contains
      procedure (abstract_implementor_type_procedure), deferred, pass :: operation_imp
   end type abstract_implementor_type

   abstract interface
      subroutine abstract_implementor_type_procedure(this)
         import  :: abstract_implementor_type
         class(abstract_implementor_type), intent(inout) :: this
      end subroutine abstract_implementor_type_procedure
   end interface


end module abstract_implementor_module

!-----------------------------------------------------------------------
!Module abstraction_module
!-----------------------------------------------------------------------
module abstract_abstraction_module
   use abstract_implementor_module
   implicit none
   private ! all by default
   public :: abstract_abstraction_type

   type, abstract :: abstract_abstraction_type
      class(abstract_implementor_type), allocatable :: implementor
   contains
      procedure, pass :: operation
      procedure(abstraction_type_init_procedure), pass, deferred :: init
   end type abstract_abstraction_type

   abstract interface
      subroutine abstraction_type_init_procedure(this, implementor)
         import :: abstract_abstraction_type
         import :: abstract_implementor_type
         class(abstract_abstraction_type), intent(inout) :: this
         class(abstract_implementor_type), intent(in) :: implementor
      end subroutine abstraction_type_init_procedure
   end interface


 contains

   !-----------------------------------------------------------------------
   !Subroutine operation
   !-----------------------------------------------------------------------
   subroutine  operation(this)
      implicit none
      class(abstract_abstraction_type), intent(inout) :: this
      if(.not. allocated(this % implementor)) stop "implementor not allocated"
      call this % implementor % operation_imp()
   end subroutine operation


end module abstract_abstraction_module

!-----------------------------------------------------------------------
!Module concrete_implementor_a_module
!-----------------------------------------------------------------------
module concrete_implementor_a_module
   use abstract_implementor_module
   implicit none
   private ! all by default
   public :: concrete_implementor_a_type

   type, extends(abstract_implementor_type) :: concrete_implementor_a_type
   contains
      procedure, pass :: operation_imp => operation_imp_concrete_implementor_a
   end type concrete_implementor_a_type

 contains

   !-----------------------------------------------------------------------
   !Subroutine operation_imp_concrete_implementor_a
   !-----------------------------------------------------------------------
   subroutine  operation_imp_concrete_implementor_a(this)
      implicit none
      class(concrete_implementor_a_type), intent(inout) :: this
      write(*,*) "operation_imp_concrete_implementor_a"
   end subroutine operation_imp_concrete_implementor_a

end module concrete_implementor_a_module

!-----------------------------------------------------------------------
!Module concrete_implementor_b_module
!-----------------------------------------------------------------------
module concrete_implementor_b_module
   use abstract_implementor_module
   implicit none
   private ! all by default
   public :: concrete_implementor_b_type

   type, extends(abstract_implementor_type) :: concrete_implementor_b_type

   contains
      procedure, pass :: operation_imp => operation_imp_concrete_implementor_b
   end type concrete_implementor_b_type

 contains

   !-----------------------------------------------------------------------
   !Subroutine operation_imp_concrete_implementor_b
   !-----------------------------------------------------------------------
   subroutine  operation_imp_concrete_implementor_b(this)
      implicit none
      class(concrete_implementor_b_type), intent(inout) :: this
      write(*,*) "operation_imp_concrete_implementor_b"
   end subroutine operation_imp_concrete_implementor_b

end module concrete_implementor_b_module


!-----------------------------------------------------------------------
!Module refined_abstraction_module
!-----------------------------------------------------------------------
module refined_abstraction_module

   use abstract_implementor_module
   use abstract_abstraction_module
   implicit none
   private ! all by default
   public :: refined_abstraction_type


   type, extends(abstract_abstraction_type) :: refined_abstraction_type
   contains
      procedure, pass :: operation => operation_refined_abstraction

      procedure, pass :: init => init_refined_abstraction
      final :: finalise_refined_abstraction
   end type refined_abstraction_type

 contains

   !-----------------------------------------------------------------------
   !Subroutine init_refined_abstraction
   !-----------------------------------------------------------------------
   subroutine init_refined_abstraction(this, implementor)
      class(refined_abstraction_type), intent(inout) :: this
      class(abstract_implementor_type), intent(in) :: implementor
      if (allocated( this % implementor)) deallocate(this % implementor )

      this % implementor = implementor

   end subroutine init_refined_abstraction


   !-----------------------------------------------------------------------
   !Subroutine operation_refined_abstraction
   !-----------------------------------------------------------------------
   subroutine  operation_refined_abstraction(this)
      implicit none
      class(refined_abstraction_type), intent(inout) :: this
      ! do something else

      if(.not. allocated(this % implementor)) stop "implementor not allocated"
      call this % implementor % operation_imp()
   end subroutine operation_refined_abstraction


   !-----------------------------------------------------------------------
   !Subroutine finalise_refined_abstraction
   !-----------------------------------------------------------------------
   elemental subroutine  finalise_refined_abstraction(this)
      implicit none
      type(refined_abstraction_type), intent(inout) :: this
      if (allocated(this % implementor)) deallocate(this % implementor)
   end subroutine finalise_refined_abstraction

end module refined_abstraction_module

!-----------------------------------------------------------------------
!Module client_module
!-----------------------------------------------------------------------
module client_module
   use abstract_abstraction_module
   implicit none
   private ! all by default
   public :: client_type

   type client_type
      class(abstract_abstraction_type), allocatable :: abstraction
   contains
      procedure, pass ::  create_abstraction
   end type client_type

 contains

   !-----------------------------------------------------------------------
   !Subroutine create_abstraction
   !-----------------------------------------------------------------------
   subroutine  create_abstraction(this, abstraction)
      implicit none
      class(client_type), intent(inout) :: this
      class(abstract_abstraction_type), intent(in) :: abstraction


      if (allocated(this % abstraction ))  then
         deallocate(this % abstraction)
      endif
      this % abstraction = abstraction
   end subroutine create_abstraction


end module client_module

!-----------------------------------------------------------------------
!Main program test
!-----------------------------------------------------------------------
program    test
   use client_module
   use refined_abstraction_module
   use concrete_implementor_a_module
   use concrete_implementor_b_module
   implicit none

   type(client_type) :: client
   integer :: i
   type(refined_abstraction_type) :: a

   call client % create_abstraction(a)

   do i = 1, 10
      call client % abstraction % init(concrete_implementor_a_type())
      call client % abstraction % operation()
      call client % abstraction % init(concrete_implementor_b_type())
      call client % abstraction % operation()
   enddo

end program test
