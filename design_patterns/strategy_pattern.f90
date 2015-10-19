!Copyright (c) 2015 zmiimz
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!-----------------------------------------------------------------------
!Module sort_strategy_module
!-----------------------------------------------------------------------

module strategy_module

   public :: abstract_strategy, concrete_strategy_a, concrete_strategy_b

   ! abstract type
   type, abstract :: abstract_strategy
   contains
      procedure (strategy_procedure), deferred, pass :: execute
   end type abstract_strategy

   abstract interface
      subroutine strategy_procedure(this)
         import  :: abstract_strategy
         class(abstract_strategy), intent(inout) :: this
      end subroutine strategy_procedure
   end interface

   type, extends(abstract_strategy) :: concrete_strategy_a
   contains
      procedure, pass :: execute => execute_concrete_strategy_a
   end type concrete_strategy_a

   type, extends(abstract_strategy) :: concrete_strategy_b
   contains
      procedure, pass :: execute => execute_concrete_strategy_b
   end type concrete_strategy_b


contains

   subroutine execute_concrete_strategy_a(this)
      class(concrete_strategy_a), intent(inout) :: this
      write(*,*) "execute_concrete_strategy_a"
   end subroutine execute_concrete_strategy_a


   subroutine execute_concrete_strategy_b(this)
      class(concrete_strategy_b), intent(inout) :: this
      write(*,*) "execute_concrete_strategy_b"
   end subroutine execute_concrete_strategy_b

end module strategy_module


!-----------------------------------------------------------------------
!Module UserStrategy_module
!-----------------------------------------------------------------------
module context_module
   use strategy_module
   implicit none
   private ! all by default
   public :: context

   type context
      class(abstract_strategy), allocatable :: strategy
   contains
      procedure, pass :: init
      procedure, pass :: change_strategy
      procedure, pass :: execute
   end type context


contains

   !-----------------------------------------------------------------------
   !Subroutine init
   !-----------------------------------------------------------------------
   subroutine  init(this, s)
      implicit none

      class(context), intent(inout) :: this
      class(abstract_strategy), intent(in) :: s

      allocate(this % strategy, source = s)

   end subroutine init

   !-----------------------------------------------------------------------
   !Subroutine change_strategy
   !-----------------------------------------------------------------------
   subroutine  change_strategy(this, s)
      implicit none
      class(context), intent(inout) :: this
      class(abstract_strategy), intent(in) :: s
      ! overloaded assignment?
      ! Assignment to an allocatable polymorphic variable at (1) is not yet supported
      ! not implemented yet [F2008] gfortran 5.2.1, ifort 16.0
      ! this % strategy = s

      ! workaround?
      deallocate(this % strategy)
      allocate(this % strategy, source = s)
   end subroutine change_strategy

   !-----------------------------------------------------------------------
   !Subroutine sort
   !-----------------------------------------------------------------------
   subroutine  execute(this)
      implicit none
      class(context), intent(inout) :: this
      call this % strategy % execute()
   end subroutine execute

end module context_module


!-----------------------------------------------------------------------
!Main strategy_main
!-----------------------------------------------------------------------
program    strategy_main
   use strategy_module
   use context_module
   implicit none

   type(concrete_strategy_a) :: sa
   type(concrete_strategy_b) :: sb
   type(context) :: co


   call co % init(sa)

   call co % execute()

   call co % change_strategy(sb)
   call co % execute()

   call co % change_strategy(sa)
   call co % execute()

   call co % change_strategy(sb)
   call co % execute()

   call co % change_strategy(sb)
   call co % execute()

end program strategy_main

