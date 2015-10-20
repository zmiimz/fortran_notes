!Copyright (c) 2015 M. Zapukhlyak
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!-----------------------------------------------------------------------
!Module factory_method_module implements factory method design pattern
!-----------------------------------------------------------------------
module factory_method_module
   !use
   implicit none
   private ! all by default
   public :: abstract_product, concrete_product_a, concrete_product_b
   public :: abstract_creator, concrete_creator_a, concrete_creator_b


   ! product definitions
   type, abstract :: abstract_product
   contains
      procedure (abstract_product_procedure), deferred, pass :: get_name
   end type abstract_product

   abstract interface
      function abstract_product_procedure(this) result(name)
         import  :: abstract_product
         class(abstract_product), intent(inout) :: this
         character(len=:), allocatable :: name
      end function abstract_product_procedure
   end interface

   type, extends(abstract_product) :: concrete_product_a
   contains
      procedure, pass :: get_name => get_name_concrete_product_a
   end type concrete_product_a

   type, extends(abstract_product) :: concrete_product_b
   contains
      procedure, pass :: get_name => get_name_concrete_product_b
   end type concrete_product_b

   ! creator definitions
   type, abstract :: abstract_creator
   contains
      procedure (abstract_creator_procedure), deferred, pass :: factory_method
   end type abstract_creator

   abstract interface
      subroutine abstract_creator_procedure(this, prod)
         import  :: abstract_creator
         import  :: abstract_product
         class(abstract_creator), intent(in) :: this
         class(abstract_product), allocatable, intent(out) :: prod
      end subroutine abstract_creator_procedure
   end interface

   type, extends(abstract_creator) :: concrete_creator_a
   contains
      procedure, pass :: factory_method => factory_method_concrete_creator_a
   end type concrete_creator_a

   type, extends(abstract_creator) :: concrete_creator_b
   contains
      procedure, pass :: factory_method => factory_method_concrete_creator_b
   end type concrete_creator_b

contains
   !-----------------------------------------------------------------------
   !Function
   !-----------------------------------------------------------------------
   function  get_name_concrete_product_a(this) result(name)
      implicit none
      class(concrete_product_a), intent(inout) :: this
      character(len=:),allocatable :: name
      name = "concrete_product_a"
   end function get_name_concrete_product_a
   !-----------------------------------------------------------------------
   !Function
   !-----------------------------------------------------------------------
   function  get_name_concrete_product_b(this) result(name)
      implicit none
      class(concrete_product_b), intent(inout) :: this
      character(len=:),allocatable :: name
      name = "concrete_product_b"
   end function get_name_concrete_product_b
   !-----------------------------------------------------------------------
   !Function
   !-----------------------------------------------------------------------
   subroutine  factory_method_concrete_creator_a(this, prod)
      implicit none
      class(concrete_creator_a), intent(in) :: this
      class(abstract_product), allocatable, intent(out) :: prod
      ! prod = concrete_product_a() ! Assignment to an allocatable polymorphic variable at (1) is not yet supported
      allocate(prod, source=concrete_product_a())
   end subroutine factory_method_concrete_creator_a
   !-----------------------------------------------------------------------
   !Function
   !-----------------------------------------------------------------------
   subroutine  factory_method_concrete_creator_b(this, prod)
      implicit none
      class(concrete_creator_b), intent(in) :: this
      class(abstract_product), allocatable, intent(out) :: prod
      ! prod = concrete_product_b() ! Assignment to an allocatable polymorphic variable at (1) is not yet supported
      allocate(prod, source=concrete_product_b())
   end subroutine factory_method_concrete_creator_b

end module factory_method_module

!-----------------------------------------------------------------------
!Main program test_factory_method_module
!-----------------------------------------------------------------------
program    test_factory_method_module
   use factory_method_module
   implicit none
   integer, parameter :: n = 2
   type(concrete_creator_a), target :: creator_a
   type(concrete_creator_b), target :: creator_b

   type creator_container
      class(abstract_creator), pointer :: c
   end type creator_container

   class(creator_container), allocatable, dimension(:) :: creators
   class(abstract_product), allocatable :: prod
   integer :: i

   call creator_a % factory_method(prod)
   write(*,*) prod % get_name()

   select type (prod)
    class is (abstract_product)
      write(*,*) "class is abstract_product???"
    type is (concrete_product_a)
      write(*,*) "type is concrete_product_a"
    type is (concrete_product_b)
      write(*,*) "type is concrete_product_b"
    class default
      write(*,*) "type is unknown?"
   end select

   deallocate(prod)

   call creator_b % factory_method(prod)

   write(*,*) prod % get_name()
   select type (prod)
    class is (abstract_product)
      write(*,*) "class is abstract_product???"
    type is (concrete_product_a)
      write(*,*) "type is concrete_product_a"
    type is (concrete_product_b)
      write(*,*) "type is concrete_product_b"
    class default
      write(*,*) "type is unknown?"
   end select

   deallocate(prod)

   allocate(creators(n))
   creators(1) % c => creator_a
   creators(2) % c => creator_b

   do i = 1, n
      call creators(i) % c % factory_method(prod)
      write(*,*) prod % get_name()
      deallocate(prod)
   enddo

end program test_factory_method_module

