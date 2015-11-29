!Copyright (c) 2015 M. Zapukhlyak
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!-----------------------------------------------------------------------
!Module prototype_module implements prototype design pattern with cache
! NOTE: proper cloning (commented assignments with  "Assignment to an allocatable polymorphic variable")
! not supported yet by GNU gfortran (5.2.1) and intel ifort (16)
! but supported by NAG nagfor (6.0(Hibiya))
!-----------------------------------------------------------------------

module prototype_module
   implicit none
   private ! all by default
   public :: abstract_prototype, concrete_prototype_a, concrete_prototype_b

   type, abstract :: abstract_prototype
   contains
      procedure (abstract_prototype_procedure1), deferred, pass :: clone
      procedure (abstract_prototype_procedure2), deferred, pass :: get_name
   end type abstract_prototype

   abstract interface
      function abstract_prototype_procedure1(this) result(prototype)
         import  :: abstract_prototype
         class(abstract_prototype), intent(in) :: this
         class(abstract_prototype), allocatable :: prototype
      end function abstract_prototype_procedure1
      function  abstract_prototype_procedure2(this) result(name)
         import  :: abstract_prototype
         class(abstract_prototype), intent(in) :: this
         character(len=:),allocatable :: name
      end function abstract_prototype_procedure2
   end interface

   type, extends(abstract_prototype) :: concrete_prototype_a
   contains
      procedure, pass :: clone => clone_concrete_prototype_a
      procedure, pass :: get_name => get_name_concrete_prototype_a
   end type concrete_prototype_a

   type, extends(abstract_prototype) :: concrete_prototype_b
   contains
      procedure, pass :: clone => clone_concrete_prototype_b
      procedure, pass :: get_name => get_name_concrete_prototype_b
   end type concrete_prototype_b

contains
   !-----------------------------------------------------------------------
   !Function
   !-----------------------------------------------------------------------
   function  clone_concrete_prototype_a(this) result(prototype)
      implicit none
      class(concrete_prototype_a), intent(in) :: this
      class(abstract_prototype), allocatable :: prototype
      ! prototype = concrete_prototype_a() ! Assignment to an allocatable polymorphic variable at (1) is not yet supported
      allocate(prototype, source=concrete_prototype_a())
   end function clone_concrete_prototype_a

   !-----------------------------------------------------------------------
   !Function
   !-----------------------------------------------------------------------
   function  clone_concrete_prototype_b(this) result(prototype)
      implicit none
      class(concrete_prototype_b), intent(in) :: this
      class(abstract_prototype), allocatable :: prototype
      ! prototype = concrete_prototype_b() ! Assignment to an allocatable polymorphic variable at (1) is not yet supported
      allocate(prototype, source=concrete_prototype_b())
   end function clone_concrete_prototype_b

   !-----------------------------------------------------------------------
   !Function
   !-----------------------------------------------------------------------
   function  get_name_concrete_prototype_a(this) result(name)
      implicit none
      class(concrete_prototype_a), intent(in) :: this
      character(len=:),allocatable :: name
      name = "concrete_prototype_a"
   end function get_name_concrete_prototype_a
   !-----------------------------------------------------------------------
   !Function
   !-----------------------------------------------------------------------
   function  get_name_concrete_prototype_b(this) result(name)
      implicit none
      class(concrete_prototype_b), intent(in) :: this
      character(len=:),allocatable :: name
      name = "concrete_prototype_b"
   end function get_name_concrete_prototype_b

end module prototype_module

!-----------------------------------------------------------------------
!Module prototype_cache_module
!-----------------------------------------------------------------------
module prototype_cache_module
   use prototype_module
   implicit none
   private ! all by default
   public :: prototype_cache

   ! special fortran derived type construction in order to implement array of pointers
   type prototypes_cache_box
      class(abstract_prototype), pointer :: p => null()
   end type prototypes_cache_box

   type prototype_cache
      type(prototypes_cache_box), allocatable, dimension(:) :: prototypes ! array of pointers on concrete prototypes
   contains
      procedure, pass :: get_clone => get_clone_from_prototype_cache
      procedure, pass :: init => init_prototype_cache
   end type prototype_cache

contains

   !-----------------------------------------------------------------------
   !Subroutine init_prototype_cache is a constructor
   !-----------------------------------------------------------------------
   subroutine  init_prototype_cache(this)
      implicit none
      class(prototype_cache), intent(inout) :: this
      write(*,*)": init_prototype_cache"
      ! TODO: check if allocated already
      ! allocate cache
      allocate(this % prototypes(2))
      ! and fill in cache
      allocate (this % prototypes(1) % p, source=concrete_prototype_a())
      allocate (this % prototypes(2) % p, source=concrete_prototype_b())
   end subroutine init_prototype_cache

   !-----------------------------------------------------------------------
   !Function
   !-----------------------------------------------------------------------
   function  get_clone_from_prototype_cache(this, id) result(prototype)
      implicit none
      class(prototype_cache), intent(inout) :: this
      integer, intent(in) :: id
      class(abstract_prototype), allocatable :: prototype
      write(*,*) ": get_clone_from_prototype_cache"
      !TODO: check if cache allocated and filled
      ! create clone using clone method from cached object with id and return cloned prototype
      ! proper cloning
      ! prototype = this % prototypes(id) % p % clone() ! Assignment to an allocatable polymorphic variable at (1) is not yet supported
      allocate (prototype, source= this % prototypes(id) % p % clone()) ! workaround
   end function get_clone_from_prototype_cache

end module prototype_cache_module

!-----------------------------------------------------------------------
!Module client_module
!-----------------------------------------------------------------------
module client_module
   use prototype_module
   use prototype_cache_module
   implicit none
   private ! all by default
   public :: client

   type client
      type(prototype_cache) :: cache
   contains
      procedure, pass :: operate => operate_from_client
   end type client

contains

   !-----------------------------------------------------------------------
   !Subroutine operate_from_client
   !-----------------------------------------------------------------------
   subroutine  operate_from_client(this)
      implicit none
      class(client), intent(inout) :: this
      class(abstract_prototype), allocatable :: prototype1
      class(abstract_prototype), allocatable :: prototype2

      call this % cache % init()
      ! proper cloning
      ! prototype1 = this % cache % get_clone(1) ! Assignment to an allocatable polymorphic variable at (1) is not yet supported
      allocate (prototype1, source=this % cache % get_clone(1)) ! workaround
      write(*,*) prototype1 % get_name()
      ! proper cloning
      ! prototype2 = this % cache % get_clone(2) ! Assignment to an allocatable polymorphic variable at (1) is not yet supported
      allocate (prototype2, source=this % cache % get_clone(2)) ! workaround
      write(*,*) prototype2 % get_name()

   end subroutine operate_from_client

end module client_module

!-----------------------------------------------------------------------
!Main program test_prototype_pattern
!-----------------------------------------------------------------------
program    test_prototype_pattern
   use client_module
   implicit none
   type(client) :: c

   call c % operate()
end program test_prototype_pattern
