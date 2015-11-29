!Copyright (c) 2015 M. Zapukhlyak
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!-----------------------------------------------------------------------
!Module state_module implements state design pattern
! works with GNU gfortran 5.2.1 and intel ifort 16.0.0
! implementation uses allocatable states, -> intensive reallocation
! TODO: use pointers, static states and flyweight pattern and reduce memory and CPU overhead
!-----------------------------------------------------------------------
module state_module
   ! use
   implicit none
   private ! all by default
   public :: abstract_state, concrete_state_a, concrete_state_b, concrete_state_c
   public :: abstract_context, context

   ! abstract context
   ! no private data, used only to declare properly procedures passing reference to different class object
   type, abstract :: abstract_context
   contains
      procedure (context_procedure), deferred, pass :: set_state
   end type abstract_context

   ! abstract state
   type, abstract :: abstract_state
   contains
      procedure (state_procedure), deferred, pass :: operate
   end type abstract_state

   ! interface to set_state in abstract context
   abstract interface
      subroutine context_procedure(this, state)
         import  :: abstract_state
         import  :: abstract_context
         class(abstract_context), intent(inout) :: this
         class(abstract_state), intent(in) :: state
      end subroutine context_procedure
   end interface

   ! interface to operate in abstract state
   abstract interface
      subroutine state_procedure(this, cont)
         import  :: abstract_state
         import  :: abstract_context
         class(abstract_state), intent(inout) :: this
         class(abstract_context), intent(inout) :: cont
      end subroutine state_procedure
   end interface

  ! concrete states a,b,c
   type, extends(abstract_state) :: concrete_state_a
   contains
      procedure, pass :: operate => operate_concrete_state_a
   end type concrete_state_a

   type, extends(abstract_state) :: concrete_state_b
   contains
      procedure, pass :: operate => operate_concrete_state_b
   end type concrete_state_b

   type, extends(abstract_state) :: concrete_state_c
   contains
      procedure, pass :: operate => operate_concrete_state_c
   end type concrete_state_c

   ! concrete context
   type, extends(abstract_context)  :: context
      class(abstract_state), allocatable, private :: current !< --- current state
   contains
      procedure, pass :: init => init_context
      procedure, pass :: operate => operate_context
      procedure, pass :: set_state => set_state_context
   end type context

contains
   !-----------------------------------------------------------------------
   !Subroutine operate_concrete_state_a
   !-----------------------------------------------------------------------
   subroutine  operate_concrete_state_a(this, cont)
      implicit none
      class(concrete_state_a), intent(inout) :: this
      class(abstract_context), intent(inout) :: cont
      class(abstract_state), allocatable :: state
      write(*,*) "operate_concrete_state_a"
      ! do something and change state to b
      allocate(state, source=concrete_state_b())
      call cont % set_state(state)
   end subroutine operate_concrete_state_a

   !-----------------------------------------------------------------------
   !Subroutine operate_concrete_state_b
   !-----------------------------------------------------------------------
   subroutine  operate_concrete_state_b(this, cont)
      implicit none
      class(concrete_state_b), intent(inout) :: this
      class(abstract_context), intent(inout) :: cont
      class(abstract_state), allocatable :: state
      write(*,*) "operate_concrete_state_b"
      ! do something and change state to c
      allocate(state, source=concrete_state_c())
      call cont % set_state(state)
   end subroutine operate_concrete_state_b

   !-----------------------------------------------------------------------
   !Subroutine operate_concrete_state_c
   !-----------------------------------------------------------------------
   subroutine  operate_concrete_state_c(this, cont)
      implicit none
      class(concrete_state_c), intent(inout) :: this
      class(abstract_context), intent(inout) :: cont
      class(abstract_state), allocatable :: state
      write(*,*) "operate_concrete_state_c"
      ! do something and change state to a
      allocate(state, source=concrete_state_a())
      call cont % set_state(state)
   end subroutine operate_concrete_state_c

   !-----------------------------------------------------------------------
   !Subroutine init_context
   !-----------------------------------------------------------------------
   subroutine  init_context(this)
      implicit none
      class(context), intent(inout) :: this
      ! prepare and set default current state to state a
      if(allocated(this % current)) deallocate(this % current)
      allocate(this % current, source=concrete_state_a())
   end subroutine init_context

   !-----------------------------------------------------------------------
   !Subroutine operate_context
   !-----------------------------------------------------------------------
   subroutine  operate_context(this)
      implicit none
      class(context), target, intent(inout) :: this
      call this % current % operate(this)
   end subroutine operate_context

   !-----------------------------------------------------------------------
   !Subroutine set_state_context
   !-----------------------------------------------------------------------
   subroutine  set_state_context(this, state)
      implicit none
      class(context), intent(inout) :: this
      class(abstract_state), intent(in) :: state
      ! detection if(this % current /= state) else do nothing!
      if(.not. same_type_as(state,this % current)) then
      ! this % current = state ! Assignment to an allocatable polymorphic variable at (1) is not yet supported
      ! workaround
      if(allocated(this % current)) deallocate(this % current)
      allocate (this % current, source=state)
      endif
   end subroutine set_state_context

end module state_module

!-----------------------------------------------------------------------
!Main program test_state_pattern
!-----------------------------------------------------------------------
program    test_state_pattern
   use state_module
   implicit none
   integer :: i
   integer, parameter :: ni = 10
   type(context) :: ct

   call ct % init()
   do i=1,ni
   call ct % operate()
   enddo
end program test_state_pattern
