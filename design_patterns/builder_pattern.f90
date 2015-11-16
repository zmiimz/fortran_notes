!Copyright (c) 2015 M. Zapukhlyak
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!This set of modules implements builder design pattern based on the well-known uml diagram
!tested with GNU gfortran 5.2.1 and Intel ifort 16.0
!-----------------------------------------------------------------------
!Module parts_module
! different parts derived from  an abstract_part type
!-----------------------------------------------------------------------
module parts_module
   implicit none
   private ! all by default
   public :: abstract_part, part1, part2, part3, part4, part5

   type, abstract :: abstract_part
      character(len=:),allocatable :: name
   contains
      ! should be overloaded in extended types if some new data fields added there
      procedure, pass :: print => print_part
   end type abstract_part

   ! different concrete parts (simplified with no new data fields)
   type, extends(abstract_part) :: part1
   end type part1
   type, extends(abstract_part) :: part2
   end type part2
   type, extends(abstract_part) :: part3
   end type part3
   type, extends(abstract_part) :: part4
   end type part4
   type, extends(abstract_part) :: part5
   end type part5
contains
   !-----------------------------------------------------------------------
   !Subroutine print_part
   !-----------------------------------------------------------------------
   subroutine  print_part(this)
      implicit none
      class(abstract_part), intent(in) :: this
      write(*,*) "            part->print_part: "
      select type (this)
       type is (part1)
         write(*,*) "part 1"
       type is (part2)
         write(*,*) "part 2"
       type is (part3)
         write(*,*) "part 3"
       type is (part4)
         write(*,*) "part 4"
       type is (part5)
         write(*,*) "part 5"
       class default
         stop "            part->print_part: unknown type"
      end select
      if(allocated(this % name)) write(*,*) "name = ", this % name
   end subroutine print_part
end module parts_module

!-----------------------------------------------------------------------
!Module product_module
! products created by the concrete builders usually have a significantly different structure
! 1) there is no reason to derive different products from a common parent class.
! 2) products are implemented here as polymorphic containers with different parts
!-----------------------------------------------------------------------
module product_module
   use parts_module
   implicit none
   private ! all by default
   public :: concrete_product1, concrete_product2

   ! polymorphic type-container for different "extended" parts
   type parts_container
      class(abstract_part), allocatable :: pp
   end type parts_container

   ! concrete products as containers
   type concrete_product1
      integer :: nparts
      integer, private :: iparts
      type(parts_container), private, dimension(:), allocatable :: parts ! <----- here are parts of product
   contains
      procedure, pass :: init => init_concrete_product1
      procedure, pass :: add => add_part_concrete_product1
      procedure, pass :: get => get_part_concrete_product1
      procedure, pass :: print => print_concrete_product1
   end type concrete_product1

   ! duplicated type in order to show that this is a different product of different type
   type concrete_product2
      integer :: nparts
      integer, private :: iparts
      type(parts_container), private, dimension(:), allocatable :: parts
   contains
      procedure, pass :: add => add_part_concrete_product2
      procedure, pass :: get => get_part_concrete_product2
      procedure, pass :: init => init_concrete_product2
      procedure, pass :: print => print_concrete_product2
   end type concrete_product2
contains
   !-----------------------------------------------------------------------
   !Subroutine init_concrete_product1
   !-----------------------------------------------------------------------
   subroutine  init_concrete_product1(this, nparts)
      implicit none
      class(concrete_product1), intent(inout) :: this
      integer, intent(in) :: nparts
      write(*,*) "        product->init_concrete_product1: "
      if(nparts<=0) stop "        product->init_concrete_product1: nparts<=0"
      if(.not. allocated(this % parts)) allocate(this % parts(nparts))
      this % nparts = nparts
      this % iparts = 0
   end subroutine init_concrete_product1

   !-----------------------------------------------------------------------
   !Subroutine add_part_concrete_product1
   !-----------------------------------------------------------------------
   subroutine  add_part_concrete_product1(this, part)
      implicit none
      class(concrete_product1), intent(inout) :: this
      class(abstract_part), intent(in) :: part
      write(*,*) "        product->add_part_concrete_product1: "
      ! check size
      if(this % iparts == this % nparts) then
         write(*,*) "        product->add_part_concrete_product1: container is full"
         return
      endif
      allocate (this % parts(this % iparts + 1) % pp, source=part)
      ! this % parts(this % iparts + 1) % pp = part ! Assignment to an allocatable polymorphic variable at (1) is not yet supported
      this % iparts = this % iparts + 1
   end subroutine add_part_concrete_product1

   !-----------------------------------------------------------------------
   !Function get_part_concrete_product1
   !-----------------------------------------------------------------------
   function  get_part_concrete_product1(this, ipart) result(part)
      implicit none
      class(concrete_product1), intent(inout) :: this
      integer, intent(in) :: ipart
      class(abstract_part), allocatable :: part
      write(*,*) "        product->get_part_concrete_product1: "
      if(ipart > this % nparts) stop "        product->get_part_concrete_product1: ipart > this % nparts"
      if(allocated(this % parts(ipart) % pp)) then
         ! make copy
         allocate (part, source=this % parts(ipart) % pp)
         ! part = this % parts(ipart) % pp ! Assignment to an allocatable polymorphic variable at (1) is not yet supported
      else
         stop "        product->get_part_concrete_product1: this % parts(ipart) % pp not allocated"
      endif
   end function get_part_concrete_product1

   !-----------------------------------------------------------------------
   !Subroutine print_concrete_product1
   !-----------------------------------------------------------------------
   subroutine  print_concrete_product1(this)
      implicit none
      class(concrete_product1), intent(in) :: this
      integer :: i
      write(*,*) "        product->print_concrete_product1: "
      write(*,*)"____________________________________________________________"
      do i = 1 , this % iparts
         call this % parts(i) % pp % print()
      enddo
      write(*,*)"____________________________________________________________"
   end subroutine print_concrete_product1

   !-----------------------------------------------------------------------
   !Subroutine init_concrete_product2
   !-----------------------------------------------------------------------
   subroutine  init_concrete_product2(this, nparts)
      implicit none
      class(concrete_product2), intent(inout) :: this
      integer, intent(in) :: nparts
      write(*,*) "        product->init_concrete_product2: "
      if(nparts<=0) stop "        product->init_concrete_product2: nparts<=0"
      if(.not. allocated(this % parts)) allocate(this % parts(nparts))
      this % nparts = nparts
      this % iparts = 0
   end subroutine init_concrete_product2

   !-----------------------------------------------------------------------
   !Subroutine add_part_concrete_product2
   !-----------------------------------------------------------------------
   subroutine  add_part_concrete_product2(this, part)
      implicit none
      class(concrete_product2), intent(inout) :: this
      class(abstract_part), intent(in) :: part
      write(*,*) "        product->add_part_concrete_product2: "
      ! check size
      if(this % iparts == this % nparts) then
         write(*,*) "        product->add_part_concrete_product2: container is full"
         return
      endif
      allocate (this % parts(this % iparts + 1) % pp, source=part)
      ! this % parts(this % iparts + 1) % pp = part ! Assignment to an allocatable polymorphic variable at (1) is not yet supported
      this % iparts = this % iparts + 1
   end subroutine add_part_concrete_product2

   !-----------------------------------------------------------------------
   !Function get_part_concrete_product2
   !-----------------------------------------------------------------------
   function  get_part_concrete_product2(this, ipart) result(part)
      implicit none
      class(concrete_product2), intent(inout) :: this
      integer, intent(in) :: ipart
      class(abstract_part), allocatable :: part
      write(*,*) "        product->get_part_concrete_product2: "
      if(ipart > this % nparts) stop "        product->get_part_concrete_product2: ipart > this % nparts"
      if(allocated(this % parts(ipart) % pp)) then
         ! make copy
         allocate (part, source=this % parts(ipart) % pp)
         ! part = this % parts(ipart) % pp ! Assignment to an allocatable polymorphic variable at (1) is not yet supported
      else
         stop "        product->get_part_concrete_product2: this % parts(ipart) % pp not allocated"
      endif
   end function get_part_concrete_product2

   !-----------------------------------------------------------------------
   !Subroutine print_concrete_product2
   !-----------------------------------------------------------------------
   subroutine  print_concrete_product2(this)
      implicit none
      class(concrete_product2), intent(in) :: this
      integer :: i
      write(*,*) "        product->print_concrete_product2: "
      write(*,*)"____________________________________________________________"
      do i = 1 , this % iparts
         call this % parts(i) % pp % print()
      enddo
      write(*,*)"____________________________________________________________"
   end subroutine print_concrete_product2
end module product_module

!-----------------------------------------------------------------------
!Module builder_module
!-----------------------------------------------------------------------
module builder_module
   use parts_module
   use product_module
   implicit none
   private ! all by default
   public :: abstract_builder, concrete_builder1, concrete_builder2

   ! abstract builder
   type, abstract :: abstract_builder
   contains
      ! universal build part procedure but you can also implement specific procedures for each part
      procedure (abstract_builder_procedure1), deferred, pass :: build_part
   end type abstract_builder

   abstract interface
      subroutine abstract_builder_procedure1(this, ipart)
         import :: abstract_builder
         import :: abstract_part
         class(abstract_builder), intent(inout) :: this
         integer, intent(in) :: ipart
         class(abstract_part), allocatable :: part
      end subroutine abstract_builder_procedure1
   end interface

   ! concrete builders
   ! concrete_builder1 for concrete_product1
   type, extends(abstract_builder) :: concrete_builder1
      type(concrete_product1) :: prod ! <------------- product to build
   contains
      procedure, pass :: build_part => build_part_concrete_builder1
      procedure, pass :: get_product => get_product_concrete_builder1
   end type concrete_builder1

   ! concrete_builder2 for concrete_product2
   type, extends(abstract_builder) :: concrete_builder2
      type(concrete_product2) :: prod ! <------------- product to build
   contains
      procedure, pass :: build_part => build_part_concrete_builder2
      procedure, pass :: get_product => get_product_concrete_builder2
   end type concrete_builder2

contains
   ! universal procedure but you can also implement specific procedures for each part
   !-----------------------------------------------------------------------
   !Subroutine build_part_concrete_builder1
   !-----------------------------------------------------------------------
   subroutine build_part_concrete_builder1(this, ipart)
      class(concrete_builder1), intent(inout) :: this
      integer, intent(in) :: ipart
      class(abstract_part), allocatable :: part
      write(*,*) "    builder->build_part_concrete_builder1: "
      select case (ipart)
       case (1)
         allocate (part, source=part1())
         part % name = "part 1a"
         call this % prod % add(part)
       case (2)
         allocate (part, source=part2())
         part % name = "part 2a"
         call this % prod % add(part)
       case (3)
         allocate (part, source=part3())
         part % name = "part 3a"
         call this % prod % add(part)
       case (4)
         allocate (part, source=part4())
         part % name = "part 4a"
         call this % prod % add(part)
       case (5)
         allocate (part, source=part5())
         part % name = "part 5a"
         call this % prod % add(part)
       case default
         write(*,*) "    builder->build_part_concrete_builder1: ipart unknown"
         return
      end select
   end  subroutine build_part_concrete_builder1

   !-----------------------------------------------------------------------
   !Function get_product_concrete_builder1
   !-----------------------------------------------------------------------
   function  get_product_concrete_builder1(this) result(prod)
      class(concrete_builder1), intent(in) :: this
      type(concrete_product1) :: prod
      write(*,*) "    builder->get_product_concrete_builder1: "
      prod = this % prod
   end function get_product_concrete_builder1

   !-----------------------------------------------------------------------
   !Subroutine build_part_concrete_builder2
   !-----------------------------------------------------------------------
   subroutine build_part_concrete_builder2(this, ipart)
      class(concrete_builder2), intent(inout) :: this
      integer, intent(in) :: ipart
      class(abstract_part), allocatable :: part
      write(*,*) "    builder->build_part_concrete_builder2: "
      select case (ipart)
       case (1)
         allocate (part, source=part1())
         part % name = "part 1b"
         call this % prod % add(part)
       case (2)
         allocate (part, source=part2())
         part % name = "part 2b"
         call this % prod % add(part)
       case (3)
         allocate (part, source=part3())
         part % name = "part 3b"
         call this % prod % add(part)
       case (4)
         allocate (part, source=part4())
         part % name = "part 4b"
         call this % prod % add(part)
       case (5)
         allocate (part, source=part5())
         part % name = "part 5b"
         call this % prod % add(part)
       case default
         write(*,*) "    builder->build_part_concrete_builder2: ipart unknown"
         return
      end select
   end  subroutine build_part_concrete_builder2

   !-----------------------------------------------------------------------
   !Function get_product_concrete_builder2
   !-----------------------------------------------------------------------
   function  get_product_concrete_builder2(this) result(prod)
      class(concrete_builder2), intent(in) :: this
      type(concrete_product2) :: prod
      write(*,*) "    builder->get_product_concrete_builder2: "
      prod = this % prod
   end function get_product_concrete_builder2
end module builder_module

!-----------------------------------------------------------------------
!Module director_module
!-----------------------------------------------------------------------
module director_module
   use builder_module
   implicit none
   private ! all by default
   public :: director

   type director
   contains
      procedure, pass :: construct
   end type director
contains
   !-----------------------------------------------------------------------
   !Subroutine construct
   !-----------------------------------------------------------------------
   subroutine  construct(this, builder)
      implicit none
      class(director), intent(in) :: this
      class(abstract_builder), intent(inout) :: builder
      integer, dimension(:), allocatable :: iparts
      integer :: i
      write(*,*) "  director->construct: "
      allocate(iparts(5)) ! bug or non-standard semantics in ifort -assume realloc_lhs needed without this
      iparts = [1,2,3,4,5] ! or select some specific ids only
      ! init product
      select type (builder)
       type is (concrete_builder1)
         call builder % prod % init(5)
       type is (concrete_builder2)
         call builder % prod % init(5)
       class default
         stop "  construct: unknown type"
      end select

      ! build product from parts with id in iparts
      do i = 1, size(iparts)
         call builder % build_part(iparts(i))
      enddo
   end subroutine construct
end module director_module

!-----------------------------------------------------------------------
!Module client_module
!-----------------------------------------------------------------------
module client_module
   use director_module
   use builder_module
   use product_module
   implicit none
   private ! all by default
   public :: client

   type client
      type(concrete_product1) , allocatable :: p1
      type(concrete_product2) , allocatable :: p2
   contains
      procedure, pass :: order_product1
      procedure, pass :: order_product2
   end type client
contains
   !-----------------------------------------------------------------------
   !Subroutine order_product1
   !-----------------------------------------------------------------------
   subroutine  order_product1(this)
      implicit none
      class(client), intent(inout) :: this
      type(director) :: d
      type(concrete_builder1) :: b1
      write(*,*) "client->order_product1: "
      call d % construct(b1)
      this % p1 = b1 % get_product()
   end subroutine order_product1

   !-----------------------------------------------------------------------
   !Subroutine order_product2
   !-----------------------------------------------------------------------
   subroutine  order_product2(this)
      implicit none
      class(client), intent(inout) :: this
      type(director) :: d
      type(concrete_builder2) :: b2
      write(*,*) "client->order_product2: "
      call d % construct(b2)
      this % p2 = b2 % get_product()
   end subroutine order_product2
end module client_module

!-----------------------------------------------------------------------
!Main program test_builder_pattern
!-----------------------------------------------------------------------
program    test_builder_pattern
   use client_module
   implicit none
   type(client) :: client1

   call client1 % order_product1()
   write(*,*) "client1 % p1 % print(): "
   call client1 % p1 % print()

   call client1 % order_product2()
   write(*,*) "client1 % p2 % print(): "
   call client1 % p2 % print()
end program test_builder_pattern
