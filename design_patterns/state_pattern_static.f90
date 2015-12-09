!Copyright (c) 2015 M. Zapukhlyak
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!-----------------------------------------------------------------------
!This set of modules implements example of state design pattern
! works with GNU gfortran 5.2.1 and intel ifort 16.0.0, and nagfor 6.0(Hibiya)
! implementation uses static states container and pointers ->
! * avoids intensive reallocation
! * reduces memory and CPU overhead
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!Module abstract_types_module
!-----------------------------------------------------------------------
module abstract_types_module
   !use
   implicit none
   private ! all by default
   public :: abstract_state, abstract_context

   ! abstract state
   type, abstract :: abstract_state
   contains
      procedure (state_procedure), deferred, pass :: operate
   end type abstract_state

   ! abstract context
   ! no private data, used only to declare properly procedures passing reference to different class object
   type, abstract :: abstract_context
   contains
      procedure (context_procedure), deferred, pass :: set_state
   end type abstract_context

   abstract interface
      ! interface to operate in abstract state
      subroutine state_procedure(this, cont)
         import  :: abstract_state
         import  :: abstract_context
         class(abstract_state), intent(inout) :: this
         class(abstract_context), intent(inout) :: cont
      end subroutine state_procedure
   end interface

   abstract interface
      ! interface to set_state in abstract context
      subroutine context_procedure(this, istate)
         import  :: abstract_context
         class(abstract_context), intent(inout) :: this
         integer, intent(in) :: istate
      end subroutine context_procedure
   end interface

end module abstract_types_module

!-----------------------------------------------------------------------
!Module state_module
!-----------------------------------------------------------------------
module state_module
   use abstract_types_module
   implicit none
   private ! all by default
   public :: concrete_state_a, concrete_state_b, concrete_state_c

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

contains

   !-----------------------------------------------------------------------
   !Subroutine operate_concrete_state_a
   !-----------------------------------------------------------------------
   subroutine  operate_concrete_state_a(this, cont)
      implicit none
      class(concrete_state_a), intent(inout) :: this
      class(abstract_context), intent(inout) :: cont
      write(*,*) "    operate_concrete_state_a"
      write(*,*) "    do something and change state to b"
      call cont % set_state(2)
   end subroutine operate_concrete_state_a

   !-----------------------------------------------------------------------
   !Subroutine operate_concrete_state_b
   !-----------------------------------------------------------------------
   subroutine  operate_concrete_state_b(this, cont)
      implicit none
      class(concrete_state_b), intent(inout) :: this
      class(abstract_context), intent(inout) :: cont
      write(*,*) "    operate_concrete_state_b"
      write(*,*) "    do something and change state to c"
      call cont % set_state(3)
   end subroutine operate_concrete_state_b

   !-----------------------------------------------------------------------
   !Subroutine operate_concrete_state_c
   !-----------------------------------------------------------------------
   subroutine  operate_concrete_state_c(this, cont)
      implicit none
      class(concrete_state_c), intent(inout) :: this
      class(abstract_context), intent(inout) :: cont
      write(*,*) "    operate_concrete_state_c"
      write(*,*) "    do something and change state to a"
      call cont % set_state(1)
   end subroutine operate_concrete_state_c

end module state_module

!-----------------------------------------------------------------------
!Module states_container_module
!-----------------------------------------------------------------------
module states_container_module
   use abstract_types_module
   use state_module
   implicit none
   private ! all by default
   public :: states_container, init_states_container, states_container_type, destroy_states_container

   ! special fortran construction for array of pointers
   type box_with_state_pointers
      class(abstract_state), pointer :: s => null()
   end type box_with_state_pointers

   type states_container_type
      integer, private :: nstates = 3  ! fixed number of states here: state_a, state_b, state_c
      type(box_with_state_pointers), allocatable, dimension(:) :: states ! arrays of pointers on state_type
      logical, private :: first = .true.
   contains
      procedure, pass :: state => get_state_from_container
      procedure, pass :: init => init_container
      final :: finalise_container
   end type states_container_type

   ! WARNING: global states
   ! polymorphic container for three state types
   ! NOTE: make this object thread private for OPENMP etc.
   type(states_container_type), allocatable, target :: states_container

contains

   !-----------------------------------------------------------------------
   !Subroutine init_states_container
   !-----------------------------------------------------------------------
   subroutine  init_states_container()
      implicit none
      write(*,*) "  init_states_container"
      if(.not. allocated(states_container)) then
         allocate (states_container)
         call states_container % init()
      endif
   end subroutine init_states_container

   !-----------------------------------------------------------------------
   !Subroutine destroy_states_container
   !-----------------------------------------------------------------------
   subroutine  destroy_states_container()
      implicit none
      write(*,*) "  destroy_states_container"
      if(allocated(states_container)) deallocate (states_container)
   end subroutine destroy_states_container

   !-----------------------------------------------------------------------
   !Subroutine init_container
   !-----------------------------------------------------------------------
   subroutine  init_container(this)
      implicit none
      class(states_container_type), intent(inout) :: this
      integer :: stat_msg
      write(*,*) "  init_container"
      stat_msg = 0
      ! check
      if(.not. this % first) stop "init_container: first == .false."
      ! and allocate
      allocate(this % states(this % nstates), stat=stat_msg)
      if (stat_msg/=0) stop "init_container: allocation failed"

      ! typed allocation
      allocate (concrete_state_a :: this % states(1) % s)
      allocate (concrete_state_b :: this % states(2) % s)
      allocate (concrete_state_c :: this % states(3) % s)

      ! finish and disable init
      this % first = .false.
      ! check and ensure that states are associated
      if(.not. associated(this % states(1) % s)) stop "init_container: .not. associated(this % states(1) % s"
      if(.not. associated(this % states(2) % s)) stop "init_container: .not. associated(this % states(2) % s"
      if(.not. associated(this % states(3) % s)) stop "init_container: .not. associated(this % states(3) % s"
   end subroutine init_container

   !-----------------------------------------------------------------------
   !Function
   !-----------------------------------------------------------------------
   function  get_state_from_container(this, istate) result(state)
      implicit none
      class(states_container_type), intent(inout) :: this
      integer, intent(in) :: istate
      class(abstract_state), pointer :: state
      write(*,*) "  get_state_from_container: ", istate
      if (istate<1 .or. istate>this % nstates) stop "get_state_from_container: istate not in range"
      if (.not. allocated(this % states)) stop "get_state_from_container: .not. allocated(this % states)"
      if (.not. associated(this % states(istate) % s)) stop "get_state_from_container: .not. associated(this % states(istate) % s"
      state => this % states(istate) % s
   end function get_state_from_container

   !-----------------------------------------------------------------------
   !Subroutine finalise_container
   !-----------------------------------------------------------------------
   subroutine  finalise_container(this)
      implicit none
      type(states_container_type), intent(inout) :: this
      write(*,*) "  finalise_container"
      ! free memory
      deallocate(this % states(1) % s)
      deallocate(this % states(2) % s)
      deallocate(this % states(3) % s)
      deallocate(this % states)
   end subroutine finalise_container
end module states_container_module

!-----------------------------------------------------------------------
!Module context_module
!-----------------------------------------------------------------------
module context_module
   use abstract_types_module
   use states_container_module
   implicit none
   private ! all by default
   public :: context

   ! concrete context
   type, extends(abstract_context)  :: context
      class(states_container_type), pointer, private :: states_container => null() !< --- pointer to states_container
      class(abstract_state), pointer, private :: current => null() !< --- current state
   contains
      procedure, pass :: init => init_context
      procedure, pass :: operate => operate_context
      procedure, pass :: set_state => set_state_context
      procedure, pass :: finalise => finalise_context
   end type context

contains

   !-----------------------------------------------------------------------
   !Subroutine init_context
   !-----------------------------------------------------------------------
   subroutine  init_context(this)
      implicit none
      class(context), intent(inout) :: this
      write(*,*)"init_context"
      ! init container with static states
      call init_states_container()
      ! prepare and set default current state to state a
      ! check global states
      if(.not. allocated(states_container)) stop "init_context: .not. allocated(states_container)"
      ! link states container with pointer
      this % states_container => states_container
      ! prepare and set default state to state_a
      this % current => this % states_container % state(1)
   end subroutine init_context

   !-----------------------------------------------------------------------
   !Subroutine operate_context
   !-----------------------------------------------------------------------
   subroutine  operate_context(this)
      implicit none
      class(context), target, intent(inout) :: this
      write(*,*)"operate_context"
      call this % current % operate(this)
   end subroutine operate_context

   !-----------------------------------------------------------------------
   !Subroutine set_state_context
   !-----------------------------------------------------------------------
   subroutine  set_state_context(this, istate)
      implicit none
      class(context), intent(inout) :: this
      integer, intent(in) :: istate
      write(*,*)"set_state_context"
      if(.not. associated(this % current)) stop "set_state_context: .not. associated(this % current)"
      if(.not. associated(this % states_container)) stop "set_state_context: .not. associated(this % states_container)"
      this % current => this % states_container % state(istate)
   end subroutine set_state_context

   !-----------------------------------------------------------------------
   !Subroutine finalise_context
   !-----------------------------------------------------------------------
   subroutine  finalise_context(this)
      implicit none
      class(context) :: this
      write(*,*) "  finalise_context"
      ! free memory
      if(associated(this % states_container)) then
         deallocate(states_container)
         nullify(this %  states_container)
      endif
      nullify(this % current)
   end subroutine finalise_context

end module context_module

!-----------------------------------------------------------------------
!Main program test_state_pattern
!-----------------------------------------------------------------------
program    test_state_pattern
   use context_module
   implicit none
   integer :: i
   integer, parameter :: ni = 10
   type(context) :: ct

   call ct % init()
   do i=1,ni
      call ct % operate()
   enddo
   call ct % finalise()
end program test_state_pattern
