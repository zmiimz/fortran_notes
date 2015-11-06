!Copyright (c) 2015 M. Zapukhlyak
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!This set of modules implements abstract factory design pattern from the well-known uml diagram

!-----------------------------------------------------------------------
!Module abstract_product_module
!-----------------------------------------------------------------------
module abstract_product_module

   implicit none
   private ! all by default
   public :: abstract_product_a, abstract_product_b

   ! product definitions
   type, abstract :: abstract_product_a
   contains
      procedure (abstract_product_a_procedure), deferred, pass :: get_name
   end type abstract_product_a

   type, abstract :: abstract_product_b
   contains
      procedure (abstract_product_b_procedure), deferred, pass :: get_name
   end type abstract_product_b

   abstract interface
      function abstract_product_a_procedure(this) result(name)
         import  :: abstract_product_a
         class(abstract_product_a), intent(inout) :: this
         character(len=:), allocatable :: name
      end function abstract_product_a_procedure
   end interface

   abstract interface
      function abstract_product_b_procedure(this) result(name)
         import  :: abstract_product_b
         class(abstract_product_b), intent(inout) :: this
         character(len=:), allocatable :: name
      end function abstract_product_b_procedure
   end interface

end module abstract_product_module

!-----------------------------------------------------------------------
!Module abstract_factory_module
!-----------------------------------------------------------------------
module abstract_factory_module
   use abstract_product_module
   implicit none
   private ! all by default
   public :: abstract_factory

   ! abstract_factory definitions
   type, abstract :: abstract_factory
   contains
      procedure (abstract_factory_procedure_a), deferred, pass :: create_product_a
      procedure (abstract_factory_procedure_b), deferred, pass :: create_product_b
   end type abstract_factory

   abstract interface
      function abstract_factory_procedure_a(this) result(prod)
         import  :: abstract_factory
         import  :: abstract_product_a
         class(abstract_factory), intent(in) :: this
         class(abstract_product_a), allocatable :: prod
      end function abstract_factory_procedure_a

      function abstract_factory_procedure_b(this) result(prod)
         import  :: abstract_factory
         import  :: abstract_product_b
         class(abstract_factory), intent(in) :: this
         class(abstract_product_b), allocatable :: prod
      end function abstract_factory_procedure_b
   end interface

end module abstract_factory_module

!-----------------------------------------------------------------------
!Module concrete_product_module
!-----------------------------------------------------------------------
module concrete_product_module
   use abstract_product_module
   implicit none
   private ! all by default
   public :: concrete_product_a1, concrete_product_b1, concrete_product_a2, concrete_product_b2

   type, extends(abstract_product_a) :: concrete_product_a1
   contains
      procedure, pass :: get_name => get_name_concrete_product_a1
   end type concrete_product_a1

   type, extends(abstract_product_a) :: concrete_product_a2
   contains
      procedure, pass :: get_name => get_name_concrete_product_a2
   end type concrete_product_a2

   type, extends(abstract_product_b) :: concrete_product_b1
   contains
      procedure, pass :: get_name => get_name_concrete_product_b1
   end type concrete_product_b1

   type, extends(abstract_product_b) :: concrete_product_b2
   contains
      procedure, pass :: get_name => get_name_concrete_product_b2
   end type concrete_product_b2

contains

   !-----------------------------------------------------------------------
   !Function
   !-----------------------------------------------------------------------
   function  get_name_concrete_product_a1(this) result(name)
      implicit none
      class(concrete_product_a1), intent(inout) :: this
      character(len=:),allocatable :: name
      name = "concrete_product_a1"
   end function get_name_concrete_product_a1

   !-----------------------------------------------------------------------
   !Function
   !-----------------------------------------------------------------------
   function  get_name_concrete_product_a2(this) result(name)
      implicit none
      class(concrete_product_a2), intent(inout) :: this
      character(len=:),allocatable :: name
      name = "concrete_product_a2"
   end function get_name_concrete_product_a2

   !-----------------------------------------------------------------------
   !Function
   !-----------------------------------------------------------------------
   function  get_name_concrete_product_b1(this) result(name)
      implicit none
      class(concrete_product_b1), intent(inout) :: this
      character(len=:),allocatable :: name
      name = "concrete_product_b1"
   end function get_name_concrete_product_b1

   !-----------------------------------------------------------------------
   !Function
   !-----------------------------------------------------------------------
   function  get_name_concrete_product_b2(this) result(name)
      implicit none
      class(concrete_product_b2), intent(inout) :: this
      character(len=:),allocatable :: name
      name = "concrete_product_b2"
   end function get_name_concrete_product_b2

end module concrete_product_module


!-----------------------------------------------------------------------
!Module concrete_factory_module
!-----------------------------------------------------------------------
module concrete_factory_module
   use abstract_factory_module
   use abstract_product_module
   use concrete_product_module
   implicit none
   private ! all by default
   public :: concrete_factory1, concrete_factory2

   type, extends(abstract_factory) :: concrete_factory1
   contains
      procedure, pass :: create_product_a => create_product_a_concrete_factory1
      procedure, pass :: create_product_b => create_product_b_concrete_factory1
   end type concrete_factory1

   type, extends(abstract_factory) :: concrete_factory2
   contains
      procedure, pass :: create_product_a => create_product_a_concrete_factory2
      procedure, pass :: create_product_b => create_product_b_concrete_factory2
   end type concrete_factory2

contains

   !-----------------------------------------------------------------------
   !Function
   !-----------------------------------------------------------------------
   function  create_product_a_concrete_factory1(this) result(prod)
      implicit none
      class(concrete_factory1), intent(in) :: this
      class(abstract_product_a), allocatable :: prod
      ! prod = concrete_product_a1() ! Assignment to an allocatable polymorphic variable at (1) is not yet supported
      allocate(prod, source=concrete_product_a1())

   end function create_product_a_concrete_factory1

   !-----------------------------------------------------------------------
   !Function
   !-----------------------------------------------------------------------
   function  create_product_b_concrete_factory1(this) result(prod)
      implicit none
      class(concrete_factory1), intent(in) :: this
      class(abstract_product_b), allocatable :: prod
      ! prod = concrete_product_b1() ! Assignment to an allocatable polymorphic variable at (1) is not yet supported
      allocate(prod, source=concrete_product_b1())
   end function create_product_b_concrete_factory1

   !-----------------------------------------------------------------------
   !Function
   !-----------------------------------------------------------------------
   function  create_product_a_concrete_factory2(this) result(prod)
      implicit none
      class(concrete_factory2), intent(in) :: this
      class(abstract_product_a), allocatable :: prod
      ! prod = concrete_product_a2() ! Assignment to an allocatable polymorphic variable at (1) is not yet supported
      allocate(prod, source=concrete_product_a2())
   end function create_product_a_concrete_factory2

   !-----------------------------------------------------------------------
   !Function
   !-----------------------------------------------------------------------
   function  create_product_b_concrete_factory2(this) result(prod)
      implicit none
      class(concrete_factory2), intent(in) :: this
      class(abstract_product_b), allocatable :: prod
      ! prod = concrete_product_b2() ! Assignment to an allocatable polymorphic variable at (1) is not yet supported
      allocate(prod, source=concrete_product_b2())
   end function create_product_b_concrete_factory2

end module concrete_factory_module

!-----------------------------------------------------------------------
!Module client_module
! added some ad hoc methods in order to show interaction with factories
!-----------------------------------------------------------------------
module client_module
   use abstract_product_module
   use abstract_factory_module
   implicit none
   private ! all by default
   public :: client

   type products_set
      class(abstract_product_a), allocatable :: pa
      class(abstract_product_b), allocatable :: pb
   contains
      procedure, pass :: get_info => get_info_products
   end type products_set

   type client
      type(products_set), dimension(:), allocatable :: ps
   contains
      procedure, nopass :: create_set
   end type client

contains

   !-----------------------------------------------------------------------
   !Subroutine get_info_products
   !-----------------------------------------------------------------------
   subroutine  get_info_products(this)
      implicit none
      class(products_set), intent(inout) :: this
      if(.not. allocated(this % pa)) stop "error, .not. allocated(this % pa)"
      if(.not. allocated(this % pb)) stop "error, .not. allocated(this % pb)"
      write(*,*) this % pa % get_name()
      write(*,*) this % pb % get_name()
   end subroutine get_info_products

   !-----------------------------------------------------------------------
   !Function create_set
   !-----------------------------------------------------------------------
   function  create_set(factory) result(set)
      implicit none
      class(abstract_factory) :: factory
      type(products_set) :: set
      allocate(set % pa, source = factory % create_product_a())
      allocate(set % pb, source = factory % create_product_b())
   end function create_set

end module client_module

!-----------------------------------------------------------------------
!Main program test_abstract_factory_pattern
!-----------------------------------------------------------------------
program    test_abstract_factory_pattern
   use client_module
   use concrete_factory_module
   implicit none

   type(client) :: c
   type(concrete_factory1) :: f1
   type(concrete_factory2) :: f2
   integer, parameter :: ns = 4
   integer :: i

   allocate(c % ps(ns))

   do i = 1, ns-1, 2
      c % ps(i) = c % create_set(f1)
      call c % ps(i) % get_info()
      write(*,*) "----"
      c % ps(i+1) = c % create_set(f2)
      call c % ps(i+1) % get_info()
      write(*,*) "----"
   enddo
end program test_abstract_factory_pattern
