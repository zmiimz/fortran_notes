!Copyright (c) 2015 M. Zapukhlyak
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

! Simple example of doubly linked list container (non-generic!)
! with user defined data. Tested but can still contain some bugs!
! gfortran -cpp dl_list_module.f90 -o dl_list_module.x
! ifort -fpp dl_list_module.f90 -o dl_list_module.x

!-----------------------------------------------------------------------
!Module data_module
!-----------------------------------------------------------------------
module list_data_module
   implicit none
   private ! all by default
   public :: data_type, data_ptr

   type data_type
      real :: x
      real :: y
      real :: z
      real, dimension(10) :: a
   contains
      procedure, pass :: print => data_type_print
      procedure, pass :: delete => data_type_delete
   end type data_type

   ! Container for storing data_t pointers
   type :: data_ptr
      type(data_type), pointer :: p
   contains
      procedure, pass :: print => data_ptr_print
      procedure, pass :: delete => data_ptr_delete
   end type data_ptr

contains

   !-----------------------------------------------------------------------
   !Subroutine
   !-----------------------------------------------------------------------
   subroutine  data_type_print(this)
      implicit none
      class(data_type), intent(in) :: this
      write(*,*)"------------------------------------------------------------"
      write(*,*) "x = ", this % x
      write(*,*) "y = ", this % y
      write(*,*) "z = ", this % z
      write(*,*)"------------------------------------------------------------"
   end subroutine data_type_print

   !-----------------------------------------------------------------------
   !Subroutine
   !-----------------------------------------------------------------------
   subroutine  data_type_delete(this)
      implicit none
      class(data_type), intent(inout) :: this
      ! TODO
      !       deallocate(this)
   end subroutine data_type_delete

   !-----------------------------------------------------------------------
   !Subroutine
   !-----------------------------------------------------------------------
   subroutine  data_ptr_print(this)
      implicit none
      class(data_ptr), intent(in) :: this
      call this % p % print()
   end subroutine data_ptr_print

   !-----------------------------------------------------------------------
   !Subroutine
   !-----------------------------------------------------------------------
   subroutine  data_ptr_delete(this)
      implicit none
      class(data_ptr), intent(inout) :: this
      call this % p % delete()
      this % p => null()
   end subroutine data_ptr_delete

end module list_data_module

!-----------------------------------------------------------------------
!Module list_debug_module
!-----------------------------------------------------------------------
module list_debug_module
   !use
   implicit none
   private ! all by default
   public :: debug
   logical, parameter :: debug = .false.

end module list_debug_module

!-----------------------------------------------------------------------
!Module dl_list_node_module
!-----------------------------------------------------------------------
module dl_list_node_module
   use list_data_module, only : data_type, data_ptr
   use list_debug_module
   implicit none
   private ! all by default
   public :: list_node_type, list_node

   type list_node_type
      private
      type(data_ptr) :: data
      class(list_node_type), pointer :: next => null()
      class(list_node_type), pointer :: prev => null()
   contains
      procedure, pass :: get_data => list_node_get_data
      procedure, pass :: set_data => list_node_set_data
      procedure, pass :: get_next => list_node_get_next
      procedure, pass :: set_next => list_node_set_next
      procedure, pass :: get_prev => list_node_get_prev
      procedure, pass :: set_prev => list_node_set_prev
      procedure, pass :: print => list_node_print_data
      procedure, pass :: delete_data => list_node_delete_data
      final           :: delete_node
   end type list_node_type
contains

   !-----------------------------------------------------------------------
   !Subroutine list_node constructor
   ! creates new node
   !-----------------------------------------------------------------------
   function  list_node(prev, next, data_p)
      implicit none
      class(list_node_type), pointer :: prev
      class(list_node_type), pointer :: next
      type(data_ptr), intent(in), optional :: data_p
      class(list_node_type), pointer :: list_node
      if(debug) write(*,*) "list_node"

      allocate(list_node)
      if(associated(prev)) then
         call list_node % set_prev(prev)
         !          list_node % prev => prev
      else
         call list_node % set_prev(null())
         !          list_node % prev => null()
      endif

      if(associated(next)) then
         call list_node % set_next(next)
         !          list_node % next => next
      else
         call list_node % set_next(null())
         !          list_node % next => null()
      endif

      if(present(data_p)) then
         call list_node % set_data(data_p)
         !          list_node % data = data_p
      endif
   end function list_node

   !-----------------------------------------------------------------------
   !Subroutine delete_node ! deallocate current node (deep)
   !-----------------------------------------------------------------------
   subroutine  delete_node(this)
      type(list_node_type) :: this
      if(debug) write(*,*) "delete_node"
      call this % delete_data()
   end subroutine  delete_node

   !-----------------------------------------------------------------------
   !Function get_prev ! access prev node
   !-----------------------------------------------------------------------
   function  list_node_get_prev(this) result(get_prev)
      implicit none
      class(list_node_type), intent(in) :: this
      class(list_node_type), pointer :: get_prev
      if(debug) write(*,*) "get_prev"
      if(.not. associated(this % prev)) then
         get_prev => null()
      else
         get_prev => this % prev
      endif
   end function list_node_get_prev

   !-----------------------------------------------------------------------
   !Subroutine set_prev ! set prev node
   !-----------------------------------------------------------------------
   subroutine list_node_set_prev(this,prev)
      class(list_node_type), intent(inout) :: this
      class(list_node_type), pointer :: prev
      if(debug) write(*,*) "set_next"

      if(.not. associated(prev)) then
         this % prev => null()
      else
         this % prev => prev
      endif
   end subroutine list_node_set_prev

   !-----------------------------------------------------------------------
   !Function get_next ! access next node
   !-----------------------------------------------------------------------
   function  list_node_get_next(this) result(get_next)
      implicit none
      class(list_node_type), intent(in) :: this
      class(list_node_type), pointer :: get_next
      if(debug) write(*,*) "get_next"
      if(.not. associated(this % next)) then
         get_next => null()
      else
         get_next => this % next
      endif
   end function list_node_get_next

   !-----------------------------------------------------------------------
   !Subroutine set_next ! set next node
   !-----------------------------------------------------------------------
   subroutine list_node_set_next(this,next)
      class(list_node_type), intent(inout) :: this
      class(list_node_type), pointer :: next
      if(debug) write(*,*) "set_next"

      if(.not. associated(next)) then
         this % next => null()
      else
         this % next => next
      endif
   end subroutine list_node_set_next

   !-----------------------------------------------------------------------
   !Subroutine print_data ! print data of current node
   !-----------------------------------------------------------------------
   subroutine list_node_print_data(this)
      class(list_node_type), intent(in) :: this
      if(debug) write(*,*) "print_data"
      call this % data % print()
   end subroutine list_node_print_data

   !-----------------------------------------------------------------------
   !Function list_node_get_data ! access data of node
   !-----------------------------------------------------------------------
   function  list_node_get_data(this) result(get_data)
      implicit none
      class(list_node_type), intent(in) :: this
      type(data_ptr) :: get_data
      if(debug) write(*,*) "get_data"
      get_data = this % data
   end function list_node_get_data

   !-----------------------------------------------------------------------
   !Subroutine list_node_get_data ! access data of node
   !-----------------------------------------------------------------------
   subroutine  list_node_set_data(this, data_p)
      implicit none
      class(list_node_type), intent(inout) :: this
      type(data_ptr), intent(in) :: data_p
      if(debug) write(*,*) "set_data"
      this % data = data_p
   end subroutine list_node_set_data

   !-----------------------------------------------------------------------
   !Subroutine delete_data ! deallocate data of current node (deep)
   !-----------------------------------------------------------------------
   subroutine  list_node_delete_data(this)
      class(list_node_type), intent(inout) :: this
      if(debug) write(*,*) "delete_data"
      call this % data % delete()
   end subroutine  list_node_delete_data

end module dl_list_node_module

!-----------------------------------------------------------------------
!Module dl_list_module
!-----------------------------------------------------------------------
module dl_list_module
   use list_data_module, only : data_type,data_ptr
   use dl_list_node_module, only : list_node_type, list_node => list_node
   use list_debug_module
   implicit none
   private ! all by default
   public :: list_type

   type list_type
      private
      class(list_node_type), pointer :: head => null()
      class(list_node_type), pointer :: tail => null()
      integer :: items_count
   contains
      procedure, pass :: print        => list_print_all
      procedure, pass :: get_first    => list_get_first
      procedure, pass :: get_last     => list_get_last
      procedure, pass :: get_at       => list_get_at
      procedure, pass :: set_at       => list_set_at
      procedure, pass :: is_empty     => list_is_empty
      procedure, pass :: get_size     => list_get_size
      procedure, pass :: add_last     => list_add_last
      procedure, pass :: add_first    => list_add_first
      procedure, pass :: add_after    => list_add_after
      procedure, pass :: add_before   => list_add_before
      procedure, pass :: delete_last  => list_delete_last
      procedure, pass :: delete_first => list_delete_first
      procedure, pass :: delete_at    => list_delete_at
      procedure, pass :: delete       => list_delete_all
      procedure, pass, private :: set_size     => list_set_size
   end type list_type

contains

   !-----------------------------------------------------------------------
   !Subroutine get_first ! access first node
   !-----------------------------------------------------------------------
   function  list_get_first(this) result(get_first)
      implicit none
      class(list_type), intent(in) :: this
      class(list_node_type), pointer :: get_first
      if(debug) write(*,*) "get_first"
      get_first => this % head
   end function list_get_first

   !-----------------------------------------------------------------------
   !Function get_last ! access last node
   !-----------------------------------------------------------------------
   function  list_get_last(this) result(get_last)
      implicit none
      class(list_type), intent(in) :: this
      class(list_node_type), pointer :: get_last
      integer :: i, items_count
      if(debug) write(*,*) "get_last"
      get_last => this % tail
   end function list_get_last

   !-----------------------------------------------------------------------
   !Subroutine get_at ! access node with nindex
   !-----------------------------------------------------------------------
   function  list_get_at(this, nindex) result(get_at)
      implicit none
      class(list_type), intent(in) :: this
      integer, intent(in) :: nindex
      class(list_node_type), pointer :: get_at
      integer :: i
      integer :: items_count

      if(debug) write(*,*) "get_at"

      if (this % is_empty()) then
         write(*,*)"Error, program reached the line",__LINE__," in file ",__FILE__
         stop
      endif

      if(nindex <=0) then
         write(*,*)"Error, program reached the line",__LINE__," in file ",__FILE__
         stop
      endif

      items_count = this % get_size()

      if(nindex > items_count) then
         write(*,*)"Error, program reached the line",__LINE__," in file ",__FILE__
         stop
      endif

      if(nindex == 1) then
         get_at => this % get_first()
         return
      endif

      if(nindex == items_count) then
         get_at => this % get_last()
         return
      endif

      ! doubly linked list:
      ! NOTE:  check items_count compared to nindex and start from the closest side
      if(nindex < items_count/2) then
         ! use head and get_next
         get_at => this % get_first()
         do i = 1, nindex-1
            get_at => get_at % get_next()
         enddo
      else
         ! use tail and get_prev
         get_at => this % get_last()
         do i = 1, (items_count-nindex)
            get_at => get_at % get_prev()
         enddo
      endif
   end function list_get_at

   !-----------------------------------------------------------------------
   !Subroutine set_at ! modify node with nindex
   !-----------------------------------------------------------------------
   subroutine  list_set_at(this, nindex, data_p)
      implicit none
      class(list_type), intent(inout) :: this
      integer, intent(in) :: nindex
      type(data_ptr), intent(in) :: data_p
      class(list_node_type), pointer :: curr
      integer :: items_count

      if(debug) write(*,*) "set_at"

      if (this % is_empty()) then
         write(*,*)"Error, program reached the line",__LINE__," in file ",__FILE__
         stop
      endif

      if(nindex <=0) then
         write(*,*)"Error, program reached the line",__LINE__," in file ",__FILE__
         stop
      endif

      items_count = this % get_size()
      if(nindex > items_count) then
         write(*,*)"Error, program reached the line",__LINE__," in file ",__FILE__
         stop
      endif

      curr => this % get_at(nindex)
      call curr % set_data (data_p)
   end subroutine list_set_at

   !-----------------------------------------------------------------------
   !Subroutine print_all ! print all nodes of the list (invoke print method of data nodes)
   !-----------------------------------------------------------------------
   subroutine  list_print_all(this)
      implicit none
      class(list_type), intent(in) :: this

      class(list_node_type), pointer :: curr
      class(list_node_type), pointer :: head
      integer :: items_count
      if(debug) write(*,*) "print_all"

      if (this % is_empty()) then
         write(*,*)"------------------------------------------------------------"
         write(*,*)"List is empty"
         write(*,*)"------------------------------------------------------------"
         return
      else
         items_count = this % get_size()
         head => this % get_first()
         call head % print()

         if(items_count > 1) then
            curr => head
            !       do i = 1, items_count
            !          curr => curr % get_next()
            !          call curr % print()
            !       enddo
            do while(associated(curr % get_next()))
               curr => curr % get_next()
               call curr % print()
            end do
         endif
      endif
   end subroutine list_print_all

   !-----------------------------------------------------------------------
   !Subroutine delete_all
   !-----------------------------------------------------------------------
   subroutine  list_delete_all(this)
      implicit none
      class(list_type), intent(inout) :: this

      class(list_node_type), pointer :: curr
      class(list_node_type), pointer :: next
      integer :: items_count

      if(debug) write(*,*) "delete_all"

      if (this % is_empty()) return

      items_count = this % get_size()
      curr => this % get_first()
      next => curr % get_next()
      call curr % delete_data()
      deallocate(curr)

      do while(associated(next))
         curr => next
         next => next % get_next()
         call curr % delete_data()
         deallocate(curr)
      end do

      this % head => null()
      this % tail => null()
      call this % set_size(0)
   end subroutine list_delete_all

   !-----------------------------------------------------------------------
   !function is_empty ! test whether container is empty
   !-----------------------------------------------------------------------
   function  list_is_empty(this) result(is_empty)
      implicit none
      class(list_type), intent(in) :: this
      logical ::  is_empty
      if(debug) write(*,*) "is_empty"
      if (associated(this % head) .and. associated(this % tail) .and. (this % items_count > 0)) then
         is_empty = .false.
      else
         is_empty = .true. ! or list is broken? TODO: add checks
      endif
   end function list_is_empty

   !-----------------------------------------------------------------------
   !Function get_size ! return size - count number of nodes
   !-----------------------------------------------------------------------
   function  list_get_size(this) result(get_size)
      implicit none
      class(list_type), intent(in) :: this
      integer :: get_size
      if(debug) write(*,*) "get_size"
      get_size = this % items_count
   end function list_get_size

   !-----------------------------------------------------------------------
   !Subroutine set_size ! set new size - count number of nodes
   !-----------------------------------------------------------------------
   subroutine  list_set_size(this, new_size)
      implicit none
      class(list_type), intent(inout) :: this
      integer, intent(in) :: new_size
      if(debug) write(*,*) "set_size"
      this % items_count = new_size
   end subroutine list_set_size

   !-----------------------------------------------------------------------
   !Subroutine add_last !  insert node at the end
   !-----------------------------------------------------------------------
   subroutine  list_add_last(this, data_p)
      implicit none
      class(list_type), intent(inout) :: this
      type(data_ptr), optional, intent(in) :: data_p
      class(list_node_type), pointer :: new_node
      class(list_node_type), pointer :: curr
      !       class(list_node_type), pointer :: prev
      class(list_node_type), pointer :: next
      if(debug) write(*,*) "add_last"
      if (this % is_empty()) then
         if(present(data_p)) then
            this % head => list_node(null(), this % head, data_p)
         else
            this % head => list_node(null(),this % head)
         endif

         this % tail => this % head
         this % items_count = 1
      else
         curr => this % get_last()
         !          prev => curr % get_prev()
         next => curr % get_next() ! actually should be null

         if(present(data_p)) then
            new_node => list_node(curr, next, data_p)
         else
            new_node => list_node(curr, next)
         endif
         ! update links of neighbourhood nodes
         call curr % set_next(new_node)
         this % tail => new_node
         this % items_count = this % items_count  + 1
      end if
   end subroutine list_add_last

   !-----------------------------------------------------------------------
   !Subroutine add_first !  insert node at the beginning
   !-----------------------------------------------------------------------
   subroutine  list_add_first(this, data_p)
      implicit none
      class(list_type), intent(inout) :: this
      type(data_ptr), optional, intent(in) :: data_p
      class(list_node_type), pointer :: new_node
      class(list_node_type), pointer :: curr
      if(debug) write(*,*) "add_first"
      if (this % is_empty()) then
         if(present(data_p)) then
            this % head => list_node(null(), this % head, data_p)
         else
            this % head => list_node(null(), this % head)
         endif
         this % tail => this % head
         this % items_count = 1
      else
         curr => this % get_first()

         if(present(data_p)) then
            new_node => list_node(null(), curr, data_p)
         else
            new_node => list_node(null(), curr)
         endif

         ! update links of neighbourhood nodes
         call curr % set_prev(new_node)
         this % head => new_node
         this % items_count = this % items_count  + 1
      end if
   end subroutine list_add_first

   !-----------------------------------------------------------------------
   !Subroutine add_after !  insert node after the node with nindex
   !-----------------------------------------------------------------------
   subroutine  list_add_after(this, nindex, data_p)
      implicit none
      class(list_type), intent(inout) :: this
      integer, intent(in) :: nindex
      type(data_ptr), optional, intent(in) :: data_p

      class(list_node_type), pointer :: new_node
      class(list_node_type), pointer :: curr
      !       class(list_node_type), pointer :: prev
      class(list_node_type), pointer :: next
      integer :: items_count
      if(debug) write(*,*) "add_after"

      if(nindex == 0) then
         write(*,*)"Error, program reached the line",__LINE__," in file ",__FILE__
         stop
      endif

      items_count = this % get_size()

      if(nindex>items_count) then
         write(*,*)"Error, program reached the line",__LINE__," in file ",__FILE__
         stop
      endif

      if(nindex==items_count) then
         if(present(data_p)) then
            call this % add_last(data_p)
         else
            call this % add_last()
         endif
         return
      endif
      ! all other  1<=nindex<items_count
      curr => this % get_at(nindex)
      !        prev => curr % get_prev()
      next => curr % get_next()

      if(present(data_p)) then
         new_node => list_node(curr, next, data_p)
      else
         new_node => list_node(curr, next)
      endif

      ! update links of neighbourhood nodes
      call curr % set_next(new_node)
      call next % set_prev(new_node)
      this % items_count = this % items_count + 1
   end subroutine list_add_after


   !-----------------------------------------------------------------------
   !Subroutine add_before !  insert node before the node with nindex
   !-----------------------------------------------------------------------
   subroutine  list_add_before(this, nindex, data_p)
      implicit none
      class(list_type), intent(inout) :: this
      integer, intent(in) :: nindex
      type(data_ptr), optional, intent(in) :: data_p

      class(list_node_type), pointer :: new_node
      class(list_node_type), pointer :: curr
      class(list_node_type), pointer :: prev
      !       class(list_node_type), pointer :: next
      integer :: items_count

      if(debug) write(*,*) "add_before"

      if(nindex == 0) then
         write(*,*)"Error, program reached the line",__LINE__," in file ",__FILE__
         stop
      endif

      items_count = this % get_size()

      if(nindex>items_count) then
         write(*,*)"Error, program reached the line",__LINE__," in file ",__FILE__
         stop
      endif

      if(nindex == 1) then
         if(present(data_p)) then
            call this % add_first(data_p)
         else
            call this % add_first()
         endif
         return
      endif
      ! all other  1<nindex<=items_count
      curr => this % get_at(nindex)
      prev => curr % get_prev()
      !        next => curr % get_next()

      if(present(data_p)) then
         new_node => list_node(prev, curr, data_p)
      else
         new_node => list_node(prev, curr % get_next())
      endif

      ! update links of neighbourhood nodes
      call prev % set_next(new_node)
      call curr % set_prev(new_node)

      this % items_count = this % items_count + 1
   end subroutine list_add_before

   !-----------------------------------------------------------------------
   !Subroutine delete_first !  delete node at the beginning
   ! and return optionally  data from removed node
   !-----------------------------------------------------------------------
   subroutine  list_delete_first(this, data_p)
      implicit none
      class(list_type), intent(inout) :: this
      type(data_ptr), optional, intent(out) :: data_p
      class(list_node_type), pointer :: curr
      class(list_node_type), pointer :: next

      if(debug) write(*,*) "delete_first"

      if (this % is_empty()) return

      curr => this % get_first()
      next => curr % get_next()

      if(present(data_p)) then
         data_p = curr % get_data()
      else
         call curr % delete_data()
      endif

      ! update link to prev
      if(associated(next)) then
         call next % set_prev(null())
      else
         ! what? TODO
      endif

      deallocate(curr)
      this % items_count = this % items_count -1
      ! update head
      this % head => next
      if(this % items_count == 1) this % tail => this % head
      if (.not. associated(this % head) .or. this % items_count == 0) this % tail => null()
   end subroutine list_delete_first

   !-----------------------------------------------------------------------
   !Subroutine delete_last !  delete node at the end
   ! and return optionally  data from removed node
   !-----------------------------------------------------------------------
   subroutine  list_delete_last(this, data_p)
      implicit none
      class(list_type), intent(inout) :: this
      type(data_ptr), optional, intent(out) :: data_p
      class(list_node_type), pointer :: curr
      class(list_node_type), pointer :: prev

      integer :: items_count

      if(debug) write(*,*) "delete_last"

      if (this % is_empty()) return

      ! just do it in one run
      items_count = this % get_size()
      if(items_count == 1) then
         if(present(data_p)) then
            call this % delete_first(data_p)
         else
            call this % delete_first()
         endif
         return
      endif

      curr => this % get_last()
      prev => curr % get_prev()

      if(present(data_p)) then
         data_p = curr % get_data()
      else
         call curr % delete_data()
      endif

      ! update links of neighbourhood
      if(associated(prev)) then
         call prev%set_next(null())
      else
         ! what? TODO
      endif

      deallocate(curr)
      this % items_count = this % items_count -1
      this % tail => prev
      if(this % items_count == 1) this % head => this % tail
   end subroutine list_delete_last

   !-----------------------------------------------------------------------
   !Subroutine list_delete_at !  delete node at the position nindex
   ! and return optionally  data from removed node
   !-----------------------------------------------------------------------
   subroutine  list_delete_at(this, nindex, data_p)
      implicit none
      class(list_type), intent(inout) :: this
      integer, intent(in) :: nindex
      type(data_ptr), optional, intent(out) :: data_p
      class(list_node_type), pointer :: prev
      class(list_node_type), pointer :: curr
      class(list_node_type), pointer :: next
      integer :: items_count

      if(debug) write(*,*) "delete_at"

      if(nindex == 0) then
         write(*,*)"Error, program reached the line",__LINE__," in file ",__FILE__
         stop
      endif

      items_count = this % get_size()

      if(nindex>items_count) then
         write(*,*)"Error, program reached the line",__LINE__," in file ",__FILE__
         stop
      endif

      if(nindex == 1) then
         if(present(data_p)) then
            call this % delete_first(data_p)
         else
            call this % delete_first()
         endif
         return
      endif

      if(nindex == items_count) then
         if(present(data_p)) then
            call this % delete_last(data_p)
         else
            call this % delete_last()
         endif
         return
      endif

      ! all other cases, nindex is between head+1 and tail-1
      curr => this % get_at(nindex)
      prev => curr % get_prev()
      next => curr % get_next()

      if(present(data_p)) then
         data_p = curr % get_data()
      else
         call curr % delete_data()
      endif

      deallocate(curr)
      ! bind list
      call prev % set_next(next)
      call next % set_prev(prev)

      this % items_count = this % items_count -1
   end subroutine list_delete_at

end module dl_list_module


!-----------------------------------------------------------------------
!Main program test_list
!-----------------------------------------------------------------------
program    test_list_random
   use dl_list_module
   use dl_list_node_module
   use list_data_module, only : data_type, data_ptr
   implicit none
   integer, parameter :: n1 = 5
   integer, parameter :: n2 = 50000
   type(data_type), dimension(n1), target :: data
   type(data_type), dimension(n2), target :: data_big
   type(data_ptr) :: dp
   type(list_type) :: list
   integer :: random_pos, nsize
   character(len=1):: dummy

   type(list_node_type), pointer ::  np
   integer :: i

   call init_random_seed()
   ! init
   do i = 1, n1
      data(i) % x = i*1.0
      data(i) % y = i*1.0
      data(i) % z = i*1.0
   enddo

   do i = 1, n2
      data_big(i) % x = i*1.0
      data_big(i) % y = i*1.0
      data_big(i) % z = i*1.0
   enddo

   ! create list #1
   write(*,*)"create list #1"
   dp % p => data(1)
   call list % add_first(dp)
   do i = 2, n1
      dp % p => data(i)
      call list % add_first(dp)
   enddo

   call list % print()
   write(*,*) "is empty = ", list % is_empty()
   write(*,*) "count = ", list % get_size()
   call list % delete()
   call list % print()
   write(*,*) "is empty = ", list % is_empty()
   write(*,*) "count = ", list % get_size()


   ! create list #2
   write(*,*)"create list #2"
   dp % p => data(1)
   call list % add_first(dp)
   dp % p => data(2)
   call list % add_before(1, dp)

   do i = 3, n1
      dp % p => data(i)
      call list % add_before(1, dp)
   enddo

   call list % print()
   call list % delete_first()
   write(*,*) "is empty = ", list % is_empty()
   write(*,*) "count = ", list % get_size()
   call list % print()
   call list % delete_last()
   write(*,*) "is empty = ", list % is_empty()
   write(*,*) "count = ", list % get_size()
   call list % print()
   call list % delete_at(2)
   write(*,*) "is empty = ", list % is_empty()
   write(*,*) "count = ", list % get_size()
   call list % print()
   dp % p => data(5)
   call list % set_at(1, dp)
   write(*,*) "is empty = ", list % is_empty()
   write(*,*) "count = ", list % get_size()
   call list % print()
   call list % delete()
   call list % print()


   ! create list #3
   write(*,*)"create big list #3"
   do i = 1, n2
      dp % p => data_big(i)
      call list % add_last(dp)
   enddo
   write(*,*) "is empty = ", list % is_empty()
   write(*,*) "count = ", list % get_size()
   nsize = list % get_size()
   ! random access (delete) performance
   do while(nsize>0)
      random_pos = get_random_pos(nsize)
      !       write(*,*) "nsize = ", nsize, " random_pos = ", random_pos
      call list % delete_at(random_pos)
      nsize = list % get_size()
      !       write(*,*) "is empty = ", list % is_empty()
      !       write(*,*) "count = ", list % get_size()
   enddo


   ! create list #4
   write(*,*)"create big list #4"
   do i = 1, n2
      dp % p => data_big(i)
      call list % add_last(dp)
   enddo
   write(*,*) "is empty = ", list % is_empty()
   write(*,*) "count = ", list % get_size()

   nsize = list % get_size()
   ! random access (delete and insert) performance
   do i = 1, n2
      random_pos = get_random_pos(nsize)
      call list % delete_at(random_pos)
      random_pos = get_random_pos(nsize-1)
      dp % p => data(random_pos+1)
      call list % add_after(random_pos,dp)
   enddo
   write(*,*) "is empty = ", list % is_empty()
   write(*,*) "count = ", list % get_size()
   call list % delete()
   call list % print()
   write(*,*) "is empty = ", list % is_empty()
   write(*,*) "count = ", list % get_size()

contains
   !-----------------------------------------------------------------------
   ! Subroutine init_random_seed
   ! init the random seed based on the system's time
   !-----------------------------------------------------------------------
   subroutine init_random_seed()
      integer :: i, n, clock
      integer, dimension(:), allocatable :: seed

      call random_seed(size = n)
      allocate(seed(n))

      call system_clock(count=clock)
      seed = clock + 37 * (/ (i - 1, i = 1, n) /)
      call random_seed(put = seed)
      deallocate(seed)
   end subroutine init_random_seed

   function get_random_pos(nsize)
      integer, intent(in) :: nsize
      integer :: get_random_pos
      real :: random
      !random number between 0 and 1
      call random_number(random)
      !random int between 1 and nsize
      ! n + floor((m+1-n)*urandom)
      get_random_pos = 1 + floor(real(nsize)*random)
   end function get_random_pos

end program test_list_random
