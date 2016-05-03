!Copyright (c) 2015 M. Zapukhlyak
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

! Smple implementation of hashtable with doubly linked list container (non-generic!)
! Tested but can still contain some bugs!

!-----------------------------------------------------------------------
!Module zmi_doubly_linked_list_node_module
!-----------------------------------------------------------------------
module zmi_doubly_linked_list_node_module
   implicit none
   private ! all by default
   public :: dl_list_node_type

   character(len=*), parameter :: format_int = "(A60,I16)"
   character(len=*), parameter :: format_2int = "(A60,2I16)"
   character(len=*), parameter :: format_real = "(A60,E24.16)"
   character(len=*), parameter :: format_str = "(A60,A)"
   character(len=*), parameter :: format_logic = "(A60,L2)"

   type dl_list_node_type
      integer, private :: key = 0
      integer, private :: val = 0
      type(dl_list_node_type), pointer :: next => null()
      type(dl_list_node_type), pointer :: prev => null()
   contains
      procedure, pass :: clear => clear_dl_list_node
      procedure, pass :: get_key => get_key_dl_list_node
      procedure, pass :: get_next => get_next_dl_list_node
      procedure, pass :: get_prev => get_prev_dl_list_node
      procedure, pass :: get_storage_size => get_storage_size_dl_list_node
      procedure, pass :: get_val => get_val_dl_list_node
      procedure, pass :: print => print_dl_list_node
      procedure, pass :: set_key => set_key_dl_list_node
      procedure, pass :: set_next => set_next_dl_list_node
      procedure, pass :: set_prev => set_prev_dl_list_node
      procedure, pass :: set_val => set_val_dl_list_node
   end type dl_list_node_type

contains

   !-----------------------------------------------------------------------
   !Subroutine clear
   !-----------------------------------------------------------------------
   impure elemental subroutine  clear_dl_list_node(this)
      implicit none
      class(dl_list_node_type), intent(inout) :: this
      integer :: size_in_bits
      this % key = 0
      this % val = 0
      this % next => null()
      this % prev => null()
   end subroutine clear_dl_list_node


   !-----------------------------------------------------------------------
   !Function get_storage_size
   !-----------------------------------------------------------------------
   pure function  get_storage_size_dl_list_node(this) result(size_in_bits)
      implicit none
      class(dl_list_node_type), intent(in) :: this
      integer :: size_in_bits
      size_in_bits = storage_size(this % key) + storage_size(this % val) + storage_size(this % next) + storage_size(this % prev)
   end function get_storage_size_dl_list_node


   !-----------------------------------------------------------------------
   !Function get_prev ! access prev node
   !-----------------------------------------------------------------------
   function  get_prev_dl_list_node(this) result(prev_node)
      implicit none
      class(dl_list_node_type), intent(in) :: this
      type(dl_list_node_type), pointer :: prev_node
      if(.not. associated(this % prev)) then
         prev_node => null()
      else
         prev_node => this % prev
      endif
   end function get_prev_dl_list_node

   !-----------------------------------------------------------------------
   !Subroutine set_prev ! set prev node
   !-----------------------------------------------------------------------
   subroutine set_prev_dl_list_node(this, prev_node)
      class(dl_list_node_type), intent(inout) :: this
      type(dl_list_node_type), pointer, intent(in) :: prev_node

      if(.not. associated(prev_node)) then
         this % prev => null()
      else
         this % prev => prev_node
      endif
   end subroutine set_prev_dl_list_node

   !-----------------------------------------------------------------------
   !Function get_next ! access next node
   !-----------------------------------------------------------------------
   function  get_next_dl_list_node(this) result(next_node)
      implicit none
      class(dl_list_node_type), intent(in) :: this
      type(dl_list_node_type), pointer :: next_node
      if(associated(this % next)) then
         next_node => this % next
      else
         next_node => null()
      endif
   end function get_next_dl_list_node

   !-----------------------------------------------------------------------
   !Subroutine set_next ! set next node
   !-----------------------------------------------------------------------
   subroutine set_next_dl_list_node(this, next_node)
      class(dl_list_node_type), intent(inout) :: this
      type(dl_list_node_type), pointer, intent(in) :: next_node
      if(associated(next_node)) then
         this % next => next_node
      else
         this % next => null()
      endif
   end subroutine set_next_dl_list_node

   !-----------------------------------------------------------------------
   !Function get_key
   !-----------------------------------------------------------------------
   pure function  get_key_dl_list_node(this) result(key)
      implicit none
      class(dl_list_node_type), intent(in) :: this
      integer :: key
      key = this % key
   end function get_key_dl_list_node

   !-----------------------------------------------------------------------
   !Subroutine set_key
   !-----------------------------------------------------------------------
   pure subroutine  set_key_dl_list_node(this, key)
      implicit none
      class(dl_list_node_type), intent(inout) :: this
      integer, intent(in) :: key
      this % key = key
   end subroutine set_key_dl_list_node

   !-----------------------------------------------------------------------
   !Function get_val
   !-----------------------------------------------------------------------
   pure function  get_val_dl_list_node(this) result(val)
      implicit none
      class(dl_list_node_type), intent(in) :: this
      integer :: val
      val = this % val
   end function get_val_dl_list_node

   !-----------------------------------------------------------------------
   !Subroutine set_val
   !-----------------------------------------------------------------------
   pure subroutine  set_val_dl_list_node(this, val)
      implicit none
      class(dl_list_node_type), intent(inout) :: this
      integer, intent(in) :: val
      this % val = val
   end subroutine set_val_dl_list_node

   !-----------------------------------------------------------------------
   !Subroutine print
   !-----------------------------------------------------------------------
   subroutine print_dl_list_node(this)
      class(dl_list_node_type), intent(in) :: this
      write(*,*)"------------------------------------------------------------"
      write(*,*)"print_dl_list_node"
      write(*,*)"------------------------------------------------------------"
      write(*,format_int) "key = ", this % key
      write(*,format_int) "val = ", this % val
      write(*,format_logic) "associated(this % prev) = ", associated(this % prev)
      write(*,format_logic) "associated(this % next) = ", associated(this % next)
   end subroutine print_dl_list_node

end module zmi_doubly_linked_list_node_module

!-----------------------------------------------------------------------
!Module zmi_doubly_linked_list_module
!-----------------------------------------------------------------------
module zmi_doubly_linked_list_module
   use zmi_doubly_linked_list_node_module
   implicit none
   private ! all by default
   public :: dl_list_type

   type dl_list_type
      private
      type(dl_list_node_type), pointer :: head => null()
      type(dl_list_node_type), pointer :: tail => null()
      integer :: items_count = 0
   contains
      generic, public :: assignment(=) => assignment_operator_dl_list
      generic, public :: operator(==) => is_equal_to_operator_dl_list
      procedure, pass :: add_first => add_first_dl_list
      procedure, pass :: add_last => add_last_dl_list
      procedure, pass :: assignment_operator_dl_list
      procedure, pass :: delete => delete_all_dl_list
      procedure, pass :: delete_first => delete_first_dl_list
      procedure, pass :: delete_key => delete_key_dl_list
      procedure, pass :: delete_last => delete_last_dl_list
      procedure, pass :: export => export_all_dl_list
      procedure, pass :: get_first => get_first_dl_list
      procedure, pass :: get_last => get_last_dl_list
      procedure, pass :: get_size => get_size_dl_list
      procedure, pass :: get_storage_size => get_storage_size_dl_list
      procedure, pass :: is_empty => is_empty_dl_list
      procedure, pass :: is_equal_to_operator_dl_list
      procedure, pass :: move => move_dl_list
      procedure, pass :: print => print_all_dl_list
      procedure, pass :: search_key => search_key_dl_list
      final :: finalise_dl_list
   end type dl_list_type

contains

   !-----------------------------------------------------------------------
   !Subroutine is move_dl_list ! move nodes from rhs to this
   !-----------------------------------------------------------------------
   impure elemental subroutine move_dl_list(this,rhs)
      implicit none
      class(dl_list_type), intent(inout) :: this ! LHS
      type(dl_list_type), intent(inout) :: rhs ! RHS

      type(dl_list_node_type), pointer :: node_rhs, node
      integer :: key
      integer :: val
      integer :: this_size, rhs_size, i

      if(rhs % is_empty()) then
         call this % delete() ! clear
         return
      endif

      if(.not. this % is_empty()) call this % delete() ! clear LHS
      !move everything from right side to the left

      this % head => rhs % head
      this % tail => rhs % tail
      this % items_count = rhs % items_count

      rhs_size = rhs % get_size()
      this_size = this % get_size()
      if(this_size /= rhs_size) stop "assignment_operator_dl_list"

      rhs % head => null()
      rhs % tail => null()
      rhs % items_count = 0
   end subroutine move_dl_list


   !-----------------------------------------------------------------------
   !Subroutine is overloaded "equal" operator generic, public :: operator(==) => name
   !nodes are equal if keys and values are identical
   !-----------------------------------------------------------------------
   impure elemental function is_equal_to_operator_dl_list(this,rhs) result(is_equal)
      implicit none
      class(dl_list_type), intent(in) :: this ! LHS
      type(dl_list_type), intent(in) :: rhs ! RHS
      logical :: is_equal

      type(dl_list_node_type), pointer :: node1, node2
      integer :: key1, key2
      integer :: val1, val2
      integer :: this_size, rhs_size, i

      is_equal = .false. ! default value

      rhs_size = rhs % get_size()
      this_size = this % get_size()

      if(this_size /= rhs_size) return

      if(this_size == 0) then ! assumed  .and. this_size == rhs_size but check
         if(rhs_size == 0) then
            is_equal = .true.
         endif
      else ! this_size /= 0
         node1 => rhs % get_first()
         node2 => this % get_first()
         do i = 1, rhs_size
            key1 = node1 % get_key()
            key2 = node2 % get_key()
            val1 = node1 % get_val()
            val2 = node2 % get_val()
            if(key1 /= key2 .or. val1 /= val2 ) return ! false
            node1 => node1 % get_next()
            node2 => node2 % get_next()
         enddo
         is_equal = .true.
      endif

   end function is_equal_to_operator_dl_list


   !-----------------------------------------------------------------------
   !Subroutine is overloaded assignment operator generic, public :: assignment(=) => name
   !-----------------------------------------------------------------------
   impure elemental subroutine assignment_operator_dl_list(this,rhs)
      implicit none
      class(dl_list_type), intent(inout) :: this ! LHS
      type(dl_list_type), intent(in) :: rhs ! RHS
      type(dl_list_node_type), pointer :: node
      integer :: key
      integer :: val
      integer :: this_size, rhs_size, i

      if(rhs % is_empty()) then
         call this % delete() ! clear
         return
      endif

      if(.not. this % is_empty()) call this % delete() ! clear LHS
      !copy everything from right side to the left
      rhs_size = rhs % get_size()
      node => rhs % get_first()
      do i = 1, rhs_size
         key = node % get_key()
         val = node % get_val()
         call this % add_last(key,val)
         node => node % get_next()
      enddo
      ! check
      this_size = this % get_size()
      if(this_size /= rhs_size) stop "assignment_operator_dl_list"
   end subroutine assignment_operator_dl_list


   !-----------------------------------------------------------------------
   !Subroutine finalise_dl_list ! deallocate list (deep)
   !-----------------------------------------------------------------------
   subroutine finalise_dl_list(this)
      type(dl_list_type), intent(inout) :: this
      call this % delete()
   end subroutine  finalise_dl_list

   !-----------------------------------------------------------------------
   !Function get_storage_size_dl_list
   !-----------------------------------------------------------------------
   function  get_storage_size_dl_list(this) result(size_in_bits)
      implicit none
      class(dl_list_type), intent(in) :: this
      integer :: size_in_bits
      type(dl_list_node_type), pointer :: node
      integer :: items_count, i

      size_in_bits = 0
      size_in_bits = storage_size(this)

      if (this % is_empty()) then
         return
      else
         items_count = this % get_size()
         node => this % get_first()
         do i = 1, items_count
            size_in_bits = size_in_bits + node % get_storage_size()
            node => node % get_next()
         enddo
      endif

   end function get_storage_size_dl_list


   !-----------------------------------------------------------------------
   !Subroutine get_first ! access first node
   !-----------------------------------------------------------------------
   function  get_first_dl_list(this) result(first_node)
      implicit none
      class(dl_list_type), intent(in) :: this
      type(dl_list_node_type), pointer :: first_node
      first_node => this % head
   end function get_first_dl_list

   !-----------------------------------------------------------------------
   !Function get_last ! access last node
   !-----------------------------------------------------------------------
   function  get_last_dl_list(this) result(last_node)
      implicit none
      class(dl_list_type), intent(in) :: this
      type(dl_list_node_type), pointer :: last_node
      last_node => this % tail
   end function get_last_dl_list

   !-----------------------------------------------------------------------
   !function is_empty ! test whether dl_list is empty
   !-----------------------------------------------------------------------
   pure function  is_empty_dl_list(this) result(is_empty)
      implicit none
      class(dl_list_type), intent(in) :: this
      logical ::  is_empty

      is_empty = .false.
      if(this % items_count == 0) is_empty = .true.

   end function is_empty_dl_list


   !-----------------------------------------------------------------------
   !Function get_size
   !-----------------------------------------------------------------------
   pure function  get_size_dl_list(this) result(list_size)
      implicit none
      class(dl_list_type), intent(in) :: this
      integer :: list_size
      list_size = this % items_count
   end function get_size_dl_list

   !-----------------------------------------------------------------------
   !Subroutine add_first !  insert node at the beginning
   !-----------------------------------------------------------------------
   pure subroutine  add_first_dl_list(this, key, val)
      implicit none
      class(dl_list_type), intent(inout) :: this
      integer, intent(in) :: key
      integer, intent(in), optional :: val
      type(dl_list_node_type), pointer :: new_node

      if (this % is_empty()) then
         allocate(this % head)
         call this % head % set_key(key)
         if(present(val)) call this % head % set_val(val)
         this % tail => this % head
         this % items_count = 1
      else
         allocate(new_node)
         call new_node % set_key(key)
         if(present(val)) call new_node % set_val(val)
         new_node % next => this % head
         new_node % next % prev => new_node
         this % head => new_node
         this % items_count = this % items_count  + 1
      end if
   end subroutine add_first_dl_list

   !-----------------------------------------------------------------------
   !Subroutine add_last !  insert node at the end
   !-----------------------------------------------------------------------
   pure subroutine  add_last_dl_list(this, key, val)
      implicit none
      class(dl_list_type), intent(inout) :: this
      integer, intent(in) :: key
      integer, intent(in), optional :: val
      type(dl_list_node_type), pointer :: new_node

      if (this % is_empty()) then
         if(present(val)) then
            call this % add_first(key, val)
         else
            call this % add_first(key)
         endif
      else
         allocate(new_node)
         call new_node % set_key(key)
         if(present(val)) call new_node % set_val(val)
         new_node % prev => this % tail
         new_node % prev % next => new_node
         this % tail => new_node
         this % items_count = this % items_count  + 1
      endif
   end subroutine add_last_dl_list

   !-----------------------------------------------------------------------
   !Subroutine delete_last !  delete node at the end
   !-----------------------------------------------------------------------
   pure subroutine  delete_last_dl_list(this)
      implicit none
      class(dl_list_type), intent(inout) :: this
      type(dl_list_node_type), pointer :: node
      if (this % is_empty()) return

      node => this % tail
      if(associated(this % tail % prev)) then
         this % tail => this % tail % prev
      else
         this % tail => null()
      endif
      if(associated(node)) then
         deallocate(node)
         this % items_count = this % items_count - 1
      endif

      if (this % is_empty())  then
         this % head => null()
         this % tail => null()
      endif
   end subroutine delete_last_dl_list

   !-----------------------------------------------------------------------
   !Subroutine delete_first !  delete node at the beginning
   !-----------------------------------------------------------------------
   pure subroutine  delete_first_dl_list(this)
      implicit none
      class(dl_list_type), intent(inout) :: this
      type(dl_list_node_type), pointer :: node
      if (this % is_empty()) return

      node => this % head
      if(associated(this % head % next)) then
         this % head => this % head % next
      else
         this % head => null()
      endif
      if(associated(node)) then
         deallocate(node)
         this % items_count = this % items_count - 1
      endif

      if (this % is_empty())  then
         this % head => null()
         this % tail => null()
      endif

   end subroutine delete_first_dl_list

   !-----------------------------------------------------------------------
   !Subroutine delete
   !-----------------------------------------------------------------------
   pure subroutine  delete_all_dl_list(this)
      implicit none
      class(dl_list_type), intent(inout) :: this
      integer :: items_count, i

      if (this % is_empty()) return

      items_count = this % get_size()
      do i = 1, items_count
         call this % delete_first()
         ! call this % delete_last()
      enddo

   end subroutine delete_all_dl_list


   !-----------------------------------------------------------------------
   !Subroutine search_key, no duplicates
   !-----------------------------------------------------------------------
   subroutine search_key_dl_list(this, key, val, nindex)
      implicit none
      class(dl_list_type), intent(in) :: this
      integer, intent(in) :: key
      integer, intent(out) :: val
      integer, intent(out) :: nindex

      integer :: items_count
      type(dl_list_node_type), pointer :: node
      integer :: i

      nindex = 0
      val = 0

      if (this % is_empty()) return

      items_count = this % get_size()
      node => this % get_first()

      do i = 1, items_count
         if(node % get_key() == key) then
            nindex = i ! found
            val = node % get_val()
            return
         endif
         node => node % get_next()
      enddo
   end subroutine search_key_dl_list


   !-----------------------------------------------------------------------
   !Subroutine delete_key  ! no search for duplicates ?  TODO!!!
   !-----------------------------------------------------------------------
   subroutine delete_key_dl_list(this, key, val, nindex)
      implicit none
      class(dl_list_type), intent(inout) :: this
      integer, intent(in) :: key
      integer, intent(out) :: val
      integer, intent(out) :: nindex

      integer :: ind
      integer :: items_count
      integer :: i
      type(dl_list_node_type), pointer :: node


      nindex = 0
      val = 0
      if (this % is_empty()) return

      items_count = this % get_size()
      node => this % get_first()
      do i = 1, items_count

         if(node % get_key() == key) then
            nindex = i ! found
            val = node % get_val()
            ! delete node
            if(nindex == 1) then
               call this % delete_first()
            else if(nindex == items_count) then
               call this % delete_last()
            else
               node % prev % next => node % next
               node % next % prev => node % prev
               this % items_count = this % items_count - 1
               deallocate(node)
            endif
            return
         endif
         node => node % get_next()
      enddo
   end subroutine delete_key_dl_list


   !-----------------------------------------------------------------------
   !Subroutine export ! export all keys, values of the list
   !-----------------------------------------------------------------------
   subroutine  export_all_dl_list(this, keys, values)
      implicit none
      class(dl_list_type), intent(in) :: this
      integer, dimension(:), intent(out) :: keys, values
      type(dl_list_node_type), pointer :: node
      integer :: items_count, i

      keys = 0
      values = 0

      if (this % is_empty()) then
         return
      else
         items_count = this % get_size()
         !          if(size(keys) < items_count) stop "export_all_dl_list"
         !          if(size(values) < items_count) stop "export_all_dl_list"
         node => this % get_first()
         do i = 1, items_count
            keys(i) = node % get_key()
            values(i) = node % get_val()
            node => node % get_next()
         enddo
      endif
   end subroutine export_all_dl_list


   !-----------------------------------------------------------------------
   !Subroutine print ! print all nodes of the list
   !-----------------------------------------------------------------------
   subroutine  print_all_dl_list(this)
      implicit none
      class(dl_list_type), intent(in) :: this
      type(dl_list_node_type), pointer :: node
      integer :: items_count, i

      node => null()

      if (this % is_empty()) then
         return
      else
         items_count = this % get_size()
         node => this % get_first()
         do i = 1, items_count
            call node % print()
            node => node % get_next()
         enddo
      endif
   end subroutine print_all_dl_list

end module zmi_doubly_linked_list_module


!-----------------------------------------------------------------------
!Module string_utils_module
!-----------------------------------------------------------------------
module string_utils_module
   use iso_fortran_env
   implicit none

   interface csize
      module procedure csize32
      module procedure csize64
   end interface csize

   interface int2char
      module procedure int2char32
      module procedure int2char64
   end interface int2char

contains


   pure function csize32(i) result (sz)
      implicit none
      integer, intent (in) :: i
      integer :: sz

      if(i==0) then
         sz=1
      else if(i<0) then
         sz = floor(log10(real(abs(i),kind(1.0d0)))) + 1 + 1 ! additional  1 for minus sign
      else
         sz = floor(log10(real(i,kind(1.0d0)))) + 1
      endif

   end function csize32

   pure function csize64(i) result (sz)
      implicit none
      integer(int64), intent (in) :: i
      integer :: sz

      if(i==0) then
         sz=1
      else if(i<0) then
         sz = floor(log10(real(abs(i),kind(1.0d0)))) + 1 + 1 ! additional  1 for minus sign
      else
         sz = floor(log10(real(i,kind(1.0d0)))) + 1
      endif

   end function csize64


   !-----------------------------------------------------------------------
   !Function integer to character
   !-----------------------------------------------------------------------
   function int2char32(i) result (c)
      implicit none
      integer, intent (in) :: i
      character (len=csize(i)) :: c
      if(i<0) then
         write (c,'(A,i0)') "-",abs(i)
      else
         write (c,'(i0)') i
      end if

   end function int2char32

   !-----------------------------------------------------------------------
   !Function integer to character
   !-----------------------------------------------------------------------
   function int2char64(i) result (c)
      implicit none
      integer(int64), intent (in) :: i
      character (len=csize(i)) :: c
      if(i<0) then
         write (c,'(A,i0)') "-",abs(i)
      else
         write (c,'(i0)') i
      end if

   end function int2char64


   !-----------------------------------------------------------------------
   !Function character to integer
   !-----------------------------------------------------------------------
   function char2int32(c) result (i)
      implicit none
      character (*), intent (in) :: c
      integer :: i
      character (20) :: ifmt
      integer :: ierror
      write (ifmt,'("(i",i0,")")') len_trim(c)
      if( len_trim(c) > csize(huge(1)) ) stop "char2int32 error, use char2int64?"
      read (c,fmt=ifmt, iostat=ierror) i
      if ( ierror /= 0 ) stop "char2int32 error, not integer"
   end function char2int32

   !-----------------------------------------------------------------------
   !Function character to integer
   !-----------------------------------------------------------------------
   function char2int64(c) result (i)
      implicit none
      character (*), intent (in) :: c
      integer(int64) :: i
      character (20) :: ifmt
      integer :: ierror
      write (ifmt,'("(i",i0,")")') len_trim(c)
      if( len_trim(c) > csize(huge(1_int64)) ) stop "char2int64 error, use char2int128?"
      read (c,fmt=ifmt, iostat=ierror) i
      if ( ierror /= 0 ) stop "char2int64 error, not integer"
   end function char2int64

end module string_utils_module

!-----------------------------------------------------------------------
! zmi_hash_functions_library_module contains many popular hash functions implemented in fortran
! for integer (int32) argument (similar to but not exactly compatible with C versions)
! This code inspired by http://www.partow.net/programming/hashfunctions
!-----------------------------------------------------------------------
module zmi_hash_functions_library_module
   use, intrinsic :: iso_fortran_env
   implicit none

   integer(int32), parameter :: INT32_MAX = huge(1_int32)

   abstract interface
      function  hashfunc_procedure_interface(number) result(hash_32)
         use, intrinsic :: iso_fortran_env
         implicit none
         integer(int32), intent(in) :: number
         integer(int32) :: hash_32
      end function hashfunc_procedure_interface
   end interface

contains

   !-----------------------------------------------------------------------
   !Function elf_hash_fortran
   ! published hash algorithm used in the UNIX ELF format for object files.
   !-----------------------------------------------------------------------
   function elf_hash(number) result(hash_32)
      implicit none
      integer(int32), intent(in) :: number
      integer(int32) :: hash_32, g

      hash_32 = 0_int32
      hash_32 = shiftl(hash_32,4_int32) + number
      g = int(iand(hash_32, z'f0000000'), int32) ! 4026531840 -> int32
      if(g /= 0_int32) hash_32 = ieor(hash_32, shiftr(g,24_int32))
      hash_32 = iand(hash_32, not(g))
      hash_32 = iand(hash_32, INT32_MAX) ! remove sign bit
   end function elf_hash

   !-----------------------------------------------------------------------
   !Function apartow_hash
   ! AP Hash is an algorithm invented by Arash Partow
   !-----------------------------------------------------------------------
   function apartow_hash(number) result(hash_32)
      implicit none
      integer(int32), intent(in) :: number
      integer(int32) :: hash_32

      hash_32 = -1431655766 ! uint_to_int(int(z'aaaaaaaa'- z'ffffffff' - 1,int64))
      hash_32 = ieor(hash_32,ieor(shiftl(hash_32,7_int32), number * shiftr(hash_32,3_int32)))
      hash_32 = iand(hash_32, INT32_MAX) ! remove sign bit
   end function apartow_hash

   !-----------------------------------------------------------------------
   !Function knuth_hash
   ! The algorithm was presented in Donald E. Knuth "The Art Of Computer Programming"
   ! Volume 3, Chapter 6.4, Topic: Sorting and search.
   !-----------------------------------------------------------------------
   function knuth_hash(number) result(hash_32)
      implicit none
      integer(int32), intent(in) :: number
      integer(int32) :: hash_32


      hash_32 = 1_int32 ! zmi changed
      hash_32 = ieor(ieor(shiftl(hash_32,5_int32),shiftr(hash_32,27_int32)),number)
      hash_32 = iand(hash_32, INT32_MAX) ! remove sign bit
   end function knuth_hash

   !-----------------------------------------------------------------------
   !Function tmueller_hash
   ! http://stackoverflow.com/questions/664014/what-integer-hash-function-are-good-that-accepts-an-integer-hash-key
   ! Thomas Mueller
   !-----------------------------------------------------------------------
   function tmueller_hash(number) result(hash_32)
      implicit none
      integer(int32), intent(in) :: number
      integer(int32) :: hash_32

      hash_32 = 0_int32
      hash_32 = hash_32 + number
      hash_32 = ieor(shiftr(hash_32, 16_int32),hash_32) * 73244475_int32 ! z'45d9f3b'
      hash_32 = ieor(shiftr(hash_32, 16_int32),hash_32) * 73244475_int32 ! z'45d9f3b'
      hash_32 = ieor(shiftr(hash_32, 16_int32),hash_32)
      hash_32 = iand(hash_32, INT32_MAX) ! remove sign bit
   end function tmueller_hash

   !-----------------------------------------------------------------------
   !Function sax_hash
   ! hash function for hashing character strings using shift-add-xor (SAX)
   !-----------------------------------------------------------------------
   function sax_hash(number) result(hash_32)
      implicit none
      integer(int32), intent(in) :: number
      integer(int32) :: hash_32

      hash_32 = 0_int32
      hash_32 = ieor(hash_32, shiftl(hash_32, 5_int32) + shiftr(hash_32, 2_int32) + number) ! hash ^= ( hash << 5 ) + ( hash >> 2 ) + key[i];
      hash_32 = iand(hash_32, INT32_MAX) ! remove sign bit
   end function sax_hash


   !-----------------------------------------------------------------------
   !Function djbx_hash to integer
   ! hashing algorithm developed by Daniel J. Bernstein
   !-----------------------------------------------------------------------
   function djbx_hash(number) result(hash_32)
      implicit none
      integer(int32), intent(in) :: number
      integer(int32) :: hash_32

      hash_32 = 5381_int32
      hash_32 = (ishft(hash_32,5_int32) + hash_32) + number ! hash = 33 * hash + key[i];
      hash_32 = ieor(hash_32, shiftr(hash_32,16_int32))  ! hash ^ (hash >> 16)
      hash_32 = iand(hash_32, INT32_MAX) ! remove sign bit
   end function djbx_hash

   !-----------------------------------------------------------------------
   !Function djb_hash to integer
   ! hashing algorithm developed by Daniel J. Bernstein
   ! https://groups.google.com/forum/#!topic/comp.lang.c/lSKWXiuNOAk
   !-----------------------------------------------------------------------
   function djb_hash(number) result(hash_32)
      implicit none
      integer(int32), intent(in) :: number
      integer(int32) :: hash_32

      hash_32 = 5381_int32
      hash_32 = (ishft(hash_32,5_int32) + hash_32) + number
      hash_32 = iand(hash_32, INT32_MAX) ! remove sign bit
   end function djb_hash

   !-----------------------------------------------------------------------
   ! The djb2_hash function is ``h = ((h << 5) + h) ^ c'', with a starting
   ! hash of 5381.
   ! hashing algorithm developed by Daniel J. Bernstein
   !-----------------------------------------------------------------------
   function djb2_hash(number) result(hash_32)
      implicit none
      integer(int32), intent(in) :: number
      integer(int32) :: hash_32

      hash_32 = 5381_int32
      hash_32 = ieor((ishft(hash_32,5) + hash_32), number)
      hash_32 = iand(hash_32, INT32_MAX) ! remove sign bit
   end function djb2_hash

   !-----------------------------------------------------------------------
   !Function sdbm_hash
   ! this algorithm was created for sdbm (a public-domain reimplementation of ndbm) database library.
   !-----------------------------------------------------------------------
   function sdbm_hash(number) result(hash_32)
      implicit none
      integer(int32), intent(in) :: number
      integer(int32) :: hash_32

      hash_32 = 0_int32 ! 5381_int32 !
      hash_32 = number + shiftl(hash_32,6_int32) + shiftl(hash_32,16_int32) - hash_32 !  hash = (*str) + (hash << 6) + (hash << 16) - hash;
      hash_32 = iand(hash_32, INT32_MAX) ! remove sign bit
   end function sdbm_hash

   !-----------------------------------------------------------------------
   !Function fnv1_32_hash
   ! http://www.isthe.com/chongo/tech/comp/fnv/
   ! http://stackoverflow.com/questions/34595/what-is-a-good-hash-function
   !-----------------------------------------------------------------------
   function fnv1_32_hash(number) result(hash_32)
      implicit none
      integer(int32), intent(in) :: number
      integer(int32) :: hash_32
      integer(int32), parameter :: FNV_prime = z'01000193' !  2^24 + 2^8 + 0x93 = 16777619

      hash_32 = -2128831035 ! 2166136261 ! uint_to_int(int(z'811c9dc5'- z'ffffffff' - 1,int64))  !
      hash_32 = ieor((hash_32 * FNV_prime), number) ! hash = ( hash * 16777619 ) ^ key[i];
      hash_32 = iand(hash_32, INT32_MAX) ! remove sign bit
   end function fnv1_32_hash

   !-----------------------------------------------------------------------
   !Function fnv1a_32_hash
   !-----------------------------------------------------------------------
   function  fnv1a_32_hash(number) result(hash_32)
      implicit none
      integer(int32), intent(in) :: number
      integer(int32) :: hash_32
      integer(int32), parameter :: FNV_prime = z'01000193' !  2^24 + 2^8 + 0x93 = 16777619

      hash_32 = -2128831035 ! 2166136261 ! uint_to_int(int(z'811c9dc5'- z'ffffffff' - 1,int64))  !
      hash_32 = ieor(hash_32, number) * FNV_prime
      hash_32 = iand(hash_32, INT32_MAX) ! remove sign bit
   end function fnv1a_32_hash


   !-----------------------------------------------------------------------
   !Function jenkins_oat_hash
   !  Bob Jenkins's one-at-a-time hash an expanded version of his Dr. Dobbs article.
   !http://www.burtleburtle.net/bob/hash/doobs.html
   !-----------------------------------------------------------------------
   function  jenkins_oat_hash(number) result(hash_32)
      implicit none
      integer(int32), intent(in) :: number
      integer(int32) :: hash_32

      hash_32 = 0
      hash_32 = hash_32 + number
      hash_32 = hash_32 + shiftl(hash_32, 10)
      hash_32 = ieor(hash_32, shiftr(hash_32, 6))
      hash_32 = hash_32 + shiftl(hash_32, 3)
      hash_32 = ieor(hash_32, shiftr(hash_32, 11))
      hash_32 = hash_32 + shiftl(hash_32, 15)
      hash_32 = iand(hash_32, INT32_MAX) ! remove sign bit
   end function jenkins_oat_hash

end module zmi_hash_functions_library_module

!-----------------------------------------------------------------------
!Module zmi_hash_functions_hf_container_module
!-----------------------------------------------------------------------
module zmi_hash_functions_hf_container_module
   use zmi_hash_functions_library_module
   use string_utils_module
   implicit none
   private ! all by default
   public :: zmi_hashfunc_hf_container_type

   character(len=*), parameter :: format_int = "(A60,I16)"
   character(len=*), parameter :: format_2int = "(A60,2I16)"
   character(len=*), parameter :: format_real = "(A60,E24.16)"
   character(len=*), parameter :: format_str = "(A60,A)"
   character(len=*), parameter :: format_logic = "(A60,L2)"

   type zmi_hashfunc_pointer_type
      procedure (hashfunc_procedure_interface), pointer, nopass :: f => NULL()
   end type zmi_hashfunc_pointer_type

   type zmi_hashfunc_hf_container_type
      logical :: first = .true.
      integer :: n = 12
      type(zmi_hashfunc_pointer_type), dimension(:), allocatable :: functions
      character(len=16),dimension(12) :: names = [&
      "elf_hash        ", &
      "jenkins_oat_hash", &
      "apartow_hash    ", &
      "knuth_hash      ", &
      "tmueller_hash   ", &
      "sax_hash        ", &
      "djbx_hash       ", &
      "djb_hash        ", &
      "djb2_hash       ", &
      "sdbm_hash       ", &
      "fnv1_32_hash    ", &
      "fnv1a_32_hash   "&
      ]
   contains
      procedure, pass :: init => init_zmi_hashfunc_hf_container
      procedure, pass :: print => print_zmi_hashfunc_hf_container
      final :: finalise_zmi_hashfunc_hf_container
   end type zmi_hashfunc_hf_container_type

contains

   !-----------------------------------------------------------------------
   !Subroutine finalise_zmi_hashfunc_hf_container
   !-----------------------------------------------------------------------
   subroutine  finalise_zmi_hashfunc_hf_container(this)
      implicit none
      type(zmi_hashfunc_hf_container_type), intent(inout) :: this
      if(allocated(this % functions)) deallocate(this % functions)
      this % first = .true.
   end subroutine finalise_zmi_hashfunc_hf_container

   !-----------------------------------------------------------------------
   !Subroutine init_zmi_hashfunc_hf_container
   !-----------------------------------------------------------------------
   subroutine  init_zmi_hashfunc_hf_container(this)
      implicit none
      class(zmi_hashfunc_hf_container_type), intent(inout) :: this
      if(.not. this % first) stop "init error"
      if(allocated(this % functions)) deallocate(this % functions)
      allocate(this %functions(this % n))
      this % functions(1) % f => elf_hash
      this % functions(2) % f => jenkins_oat_hash
      this % functions(3) % f => apartow_hash
      this % functions(4) % f => knuth_hash
      this % functions(5) % f => tmueller_hash
      this % functions(6) % f => sax_hash
      this % functions(7) % f => djbx_hash
      this % functions(8) % f => djb_hash
      this % functions(9) % f => djb2_hash
      this % functions(10) % f => sdbm_hash
      this % functions(11) % f => fnv1_32_hash
      this % functions(12) % f => fnv1a_32_hash

      this % first = .false.
      write(*,*) "init_zmi_hashfunc_hf_container done"
   end subroutine init_zmi_hashfunc_hf_container

   !-----------------------------------------------------------------------
   !Subroutine print_zmi_hashfunc_hf_container
   !-----------------------------------------------------------------------
   subroutine  print_zmi_hashfunc_hf_container(this)
      implicit none
      class(zmi_hashfunc_hf_container_type), intent(in) :: this
      integer :: i

      if(.not. this % first) then
         write(*,*)"------------------------------------------------------------"
         write(*,*)"print_zmi_hashfunc_hf_container"
         write(*,*)"------------------------------------------------------------"
         write(*,format_logic) "first = ", this % first
         write(*,format_int) "n = ", this % n


         do i = 1, this % n
            write(*,format_logic) "associated(this % functions("//int2char(i)//") = ", associated(this % functions(i) % f)
            write(*,format_str) "this % names("//int2char(i)//") = ", this % names(i)
         enddo
      endif

   end subroutine print_zmi_hashfunc_hf_container

end module zmi_hash_functions_hf_container_module

!-----------------------------------------------------------------------
!Module zmi_hashtable_module
!-----------------------------------------------------------------------
module zmi_hashtable_module
   use, intrinsic :: iso_fortran_env
   use zmi_hash_functions_library_module
   use zmi_doubly_linked_list_module
   use zmi_hash_functions_hf_container_module
   use string_utils_module
   implicit none
   private ! all by default
   public :: zmi_hashtable_type

   integer, parameter :: dp = selected_real_kind(2*precision(1.0))

   type zmi_hashtable_type
      private
      logical :: first = .true.
      integer :: ne_max = 0 ! optimized table size, capacity
      integer :: ne_init = 0 ! table size at init
      integer :: nc_max = 0 ! current max number of collisions in table element (number of elements in list - 1)
      integer :: ne = 0 ! current number of elements in table
      integer :: hf_index = 0 ! index of used hash function from hf_container
      real(dp) :: nc_mean = 0.0_dp ! mean of  number of collisions in the table = nc_sum / ne_max
      real(dp) :: load_factor = 0.0_dp ! the average number ne / ne_max
      integer :: nc_sum = 0 ! sum of all collisions in the table

      type(dl_list_type), dimension(:), allocatable :: table ! hashtable
      type(dl_list_type), dimension(:), allocatable :: rtable ! aux hashtable for resize
      procedure (hashfunc_procedure_interface), pointer, nopass :: hf_pointer => null() ! selected hash function
      type(zmi_hashfunc_hf_container_type), allocatable :: hf_container ! hf_container with hash functions
   contains
      generic, public :: hash2index => hash2index_int32
      procedure, nopass, private :: get_higher_prime_number
      procedure, nopass, private :: get_optimal_prime_number
      procedure, nopass, private :: get_predefined_prime_number
      procedure, nopass, private :: hash2index_int32
      procedure, pass :: clear_deep => clear_deep_zmi_hashtable ! remove everything (deep)
      procedure, pass :: contains_key => contains_key_zmi_hashtable
      procedure, pass :: get_capacity => get_capacity_zmi_hashtable
      procedure, pass :: get_storage_size => get_storage_size_zmi_hashtable
      procedure, pass :: init => init_zmi_hashtable
      procedure, pass :: insert => insert_key_in_zmi_hashtable
      procedure, pass :: is_empty => is_empty_zmi_hashtable
      procedure, pass :: print => print_zmi_hashtable
      procedure, pass :: remove => remove_key_in_zmi_hashtable
      procedure, pass :: search => search_key_in_zmi_hashtable
      procedure, pass, private :: clear_deep_rtable => clear_deep_rtable_zmi_hashtable
      procedure, pass, private :: clear_deep_table => clear_deep_table_zmi_hashtable
      procedure, pass, private :: clear_rtable => clear_rtable_zmi_hashtable
      procedure, pass, private :: clear_table => clear_table_zmi_hashtable
      procedure, pass, private :: resize => resize_zmi_hashtable
      procedure, pass, private :: set_capacity => set_capacity_zmi_hashtable
      !TODO:
      !       procedure, pass :: clone ! shallow copy, no keys and values are copied
      !       procedure, pass :: contains_value
      !       procedure, pass :: export_keys
      !       procedure, pass :: export_values
      !       procedure, pass :: export_all
      !       procedure, pass :: get_hashfunc

      final :: finalise_zmi_hashtable
   end type zmi_hashtable_type


contains

   !-----------------------------------------------------------------------
   !Subroutine init_zmi_hashtable
   !-----------------------------------------------------------------------
   subroutine  init_zmi_hashtable(this, hf_index, n_elements)
      implicit none
      class(zmi_hashtable_type), intent(inout) :: this
      integer, optional, intent(in) :: hf_index
      integer, optional, intent(in) :: n_elements
      integer :: hfi, ne

      if(.not. this % first) stop "init_zmi_hashtable error"
      allocate(this % hf_container)
      call this % hf_container % init()
      if(present(hf_index)) then
         if(hf_index > this % hf_container % n)  stop "init_zmi_hashtable error"
         hfi = hf_index
      else
         hfi = 10 ! sdbm_hash
      endif

      this % hf_pointer => this % hf_container % functions(hfi) % f
      this % hf_index = hfi
      write(*,*) "hash function selected: ", this % hf_container % names(hfi)

      if(present(n_elements)) then
         ne = n_elements
      else
         ne = 100 ! default initial size
      endif
      call this % set_capacity(ne) ! allocate table
      this % ne_init = ne
      this % first = .false.
      write(*,*) "init_zmi_hashtable done"
   end subroutine init_zmi_hashtable



   !-----------------------------------------------------------------------
   !Subroutine set_capacity_zmi_hashtable
   !-----------------------------------------------------------------------
   subroutine  set_capacity_zmi_hashtable(this, capacity)
      implicit none
      class(zmi_hashtable_type), intent(inout) :: this
      integer, intent(in) :: capacity
      integer :: i
      integer, parameter :: method_index = 1

      this % ne_max = get_optimal_prime_number(capacity, method_index)

      if(allocated(this % table)) then
         do i = 1, size(this % table)
            call this % table(i) % delete()
         enddo
         deallocate(this % table)
      endif
      allocate(this % table(this % ne_max))

   end subroutine set_capacity_zmi_hashtable


   !-----------------------------------------------------------------------
   !Function get_optimal_prime_number
   !-----------------------------------------------------------------------
   function  get_optimal_prime_number(capacity, method_index)
      implicit none
      integer, intent(in) :: capacity
      integer, intent(in) :: method_index
      integer :: get_optimal_prime_number

      select case (method_index)
       case (1)
         get_optimal_prime_number = get_higher_prime_number(capacity)
       case (2)
         get_optimal_prime_number = get_predefined_prime_number(capacity)
       case default
         stop "get_optimal_prime_number: unknown method_index"
      end select

   end function get_optimal_prime_number

   !-----------------------------------------------------------------------
   !Function get_higher_prime_number
   !returns the nearest prime number which is greater than given source number
   !-----------------------------------------------------------------------
   pure function  get_higher_prime_number(number) result(prime_number)
      implicit none
      integer, intent(in) :: number
      integer :: prime_number
      integer :: i, num

      num = (number/2)*2 + 3
      do
         i = 3
         do while(i*i <= num)
            if(mod(num, i) == 0) exit
            i = i + 2
         enddo

         if(i*i > num) then
            prime_number = num
            return
         endif
         num = num + 2
      enddo
   end function get_higher_prime_number

   !-----------------------------------------------------------------------
   !Function
   !-----------------------------------------------------------------------
   function  get_predefined_prime_number(number) result(prime_number)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer, intent(in) :: number
      integer :: prime_number
      integer, parameter :: td = 30
      integer :: i
      integer :: ind
      logical :: answer
      integer, dimension(td), parameter :: ranges = [ (2**i, i=1,td) ] ! bug ifort ! https://software.intel.com/en-us/forums/intel-fortran-compiler-for-linux-and-mac-os-x/topic/624989
      !       integer, dimension(td), parameter :: ranges = [2, ..., 2**30] !, 2**31]

      ! http://planetmath.org/goodhashtableprimes
      integer, dimension(td),parameter :: prime_numbers_a = [3,7,13,31,53,97,193,389,769,1543,3079,6151,12289,24593,49157,98317,&
      196613,393241,786433,1572869,3145739,6291469,12582917,25165843,50331653,100663319,201326611,402653189,805306457,1610612741] !,3221225479]
      ! Robert Sedgewick, Kevin Wayne) Algorithms part 1 (ISBN 0133798690)(531s)
      integer, dimension(td),parameter :: prime_numbers_b = [3,7,13,31,61,127,251,509,1021,2039,4093,8191,16381,32749,65521,131071,&
      262139,524287,1048573,2097143,4194301,8388593,16777213,33554393,67108859,134217689,268435399,536870909,1073741789,2147483647] !, 4294967291 ]

      if(number <= 0) stop "get_predefined_prime_number error"
      if(number > ranges(td))  stop "get_predefined_prime_number error"
      call binary_search_approx(number, ranges, ind, answer)
      if(.not. answer)  stop "get_predefined_prime_number error"
      !       prime_number = prime_numbers_b(ind)
      prime_number = prime_numbers_a(ind)

   contains

      !-----------------------------------------------------------------------
      !Subroutine implements simple binary search algorithm to find index in ordered_list
      !such that !element < ordered_list(i) in !!!sorted!!! array
      !-----------------------------------------------------------------------
      pure subroutine binary_search_approx(element, ordered_list, position, found)
         integer, intent(in) :: element
         integer, dimension(:), intent(in) :: ordered_list
         integer, intent(out) :: position
         logical, intent(out) :: found
         integer :: first, half, last, s


         found = .false. ! default value, nothing found
         position = 0
         ! array not sorted
         if(.not. is_sorted(ordered_list)) return

         s = size(ordered_list)
         if(s == 0) return

         !simple return, element not in the range
         if(element <= 0) return
         if(element > ordered_list(s)) return

         if(element <= ordered_list(1)) then
            position = 1
            found = .true.
            return
         endif

         if(element == ordered_list(s)) then
            position = s
            found = .true.
            return
         endif

         first = 1
         last = s
         do  while (last - first >=1) ! do until two elements remain
            ! half = (first + last) / 2 ! http://bugs.java.com/bugdatabase/view_bug.do?bug_id=5045582
            half = first + ((last - first) / 2)
            if (element <= ordered_list(half)) then
               ! Discard second half
               last = half
            else
               ! discard first half
               first = half + 1
            end if
         end do
         ! element < last
         position = last
         found = .true.

      end subroutine binary_search_approx

      !-----------------------------------------------------------------------
      !Function
      !-----------------------------------------------------------------------
      pure function  is_sorted(a) result(answer)
         implicit none
         integer, dimension(:), intent(in) :: a
         logical :: answer
         integer :: i,l
         l=size(a)
         answer=.true.

         do i=1,l/2-1
            if( (a(i)>a(i+1)) .or. (a(l-i+1) < a(l-i)) ) then
               answer=.false.
               exit
            endif
         end do
      end function is_sorted

   end function get_predefined_prime_number


   !-----------------------------------------------------------------------
   !Function get_capacity_zmi_hashtable
   !-----------------------------------------------------------------------
   pure function  get_capacity_zmi_hashtable(this)
      implicit none
      class(zmi_hashtable_type), intent(in) :: this
      integer :: get_capacity_zmi_hashtable

      get_capacity_zmi_hashtable = this % ne_max

   end function get_capacity_zmi_hashtable


   pure function hash2index_int32(hash, table_size) result(hash_index)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: hash
      integer(int32), intent(in) :: table_size
      integer :: hash_index
      hash_index = int(mod(hash,table_size) + 1, int32)
      !       if(hash_index <= 0) stop "hash_index <= 0"
   end function hash2index_int32

   !-----------------------------------------------------------------------
   !Subroutine insert_key_in_zmi_hashtable
   !-----------------------------------------------------------------------
   subroutine  insert_key_in_zmi_hashtable(this, key, val)
      implicit none
      class(zmi_hashtable_type), intent(inout) :: this
      integer, intent(in) :: key
      integer, intent(in) :: val
      integer :: ind, depth

      ind = this % hash2index(this % hf_pointer(key), this % ne_max)

      call this % table(ind) % add_first(key, val)
      this % ne = this % ne + 1
      depth = this % table(ind) % get_size()

      !update stat; depth == 1 - no collision, only one element in the list
      if(depth > 1) then
         this % nc_sum = this % nc_sum + 1
         !          this % nc_mean = this % nc_sum / real(this % ne, dp) ! BUG
         this % nc_mean = this % nc_sum / real(this % ne_max , dp)

      endif
      if(this % nc_max < depth - 1) this % nc_max = depth - 1 ! depth-1 == collision number
      this % load_factor = this % ne / real(this % ne_max , dp)
      call this % resize() ! if needed

   end subroutine insert_key_in_zmi_hashtable

   !-----------------------------------------------------------------------
   !Subroutine search_key_in_zmi_hashtable
   !-----------------------------------------------------------------------
   subroutine  search_key_in_zmi_hashtable(this, key, val, nindex, found)
      implicit none
      class(zmi_hashtable_type), intent(in) :: this
      integer, intent(in) :: key
      integer, intent(out) :: val
      integer, intent(out) :: nindex
      logical, intent(out) :: found
      integer :: ind

      found = .false.
      ind = this % hash2index(this % hf_pointer(key), this % ne_max)
      call this % table(ind) % search_key(key, val, nindex)
      if(nindex > 0) found = .true.

   end subroutine search_key_in_zmi_hashtable


   !-----------------------------------------------------------------------
   !Subroutine remove_key_in_zmi_hashtable
   !-----------------------------------------------------------------------
   subroutine  remove_key_in_zmi_hashtable(this, key, val, nindex, deleted)
      implicit none
      class(zmi_hashtable_type), intent(inout) :: this
      integer, intent(in) :: key
      integer, intent(out) :: val
      integer, intent(out) :: nindex
      logical, intent(out) :: deleted
      integer :: ind, depth

      deleted = .false.
      ind = this % hash2index(this % hf_pointer(key), this % ne_max)
      call this % table(ind) % delete_key(key, val, nindex)
      if(nindex > 0) then
         deleted = .true.
         this % ne = this % ne - 1
         depth = this % table(ind) % get_size()
         !update stat, depth > 0 now =>  we removed element counted as collision
         if(depth > 0) then
            this % nc_sum = this % nc_sum - 1
            this % nc_mean = this % nc_sum / real(this % ne, dp)
         endif
         this % load_factor = this % ne / real(this % ne_max , dp)
      endif
      
   end subroutine remove_key_in_zmi_hashtable

   !-----------------------------------------------------------------------
   !Function is_empty_zmi_hashtable
   !-----------------------------------------------------------------------
   pure function  is_empty_zmi_hashtable(this)
      implicit none
      class(zmi_hashtable_type), intent(in) :: this
      logical :: is_empty_zmi_hashtable

      is_empty_zmi_hashtable = .true.
      if (this % ne > 0) is_empty_zmi_hashtable = .false.

   end function is_empty_zmi_hashtable

   !-----------------------------------------------------------------------
   !Subroutine print_zmi_hashtable
   !-----------------------------------------------------------------------
   subroutine  print_zmi_hashtable(this, print_table)
      implicit none
      class(zmi_hashtable_type), intent(inout) :: this
      logical, optional, intent(in) :: print_table
      integer :: i
      character(len=*), parameter :: format_int = "(A60,I16)"
      character(len=*), parameter :: format_2int = "(A60,2I16)"
      character(len=*), parameter :: format_real = "(A60,E24.16)"
      character(len=*), parameter :: format_str = "(A60,A)"
      character(len=*), parameter :: format_logic = "(A60,L2)"

      write(*,*)"------------------------------------------------------------"
      write(*,*)"print_zmi_hashtable"
      write(*,*)"------------------------------------------------------------"
      write(*,format_logic) "first = ", this % first
      write(*,format_int) "ne_max = ", this % ne_max
      write(*,format_int) "ne = ", this % ne
      write(*,format_int) "ne_init = ", this % ne_init
      write(*,format_int) "nc_max = ", this % nc_max
      write(*,format_real) "nc_mean = ", this % nc_mean
      write(*,format_int) "nc_sum = ", this % nc_sum
      write(*,format_real) "load_factor = ", this % load_factor
      write(*,format_int) "hf_index = ", this % hf_index
      write(*,format_logic) "associated(hf_pointer) = ", associated(this % hf_pointer)
      write(*,format_str) "hf_name = ", this % hf_container % names(this % hf_index)
      call this % hf_container % print()

      if(present(print_table)) then
         if(print_table) then
            do i = 1, this % ne_max
               if(.not. this % table(i) % is_empty()) write(*,format_int) "this % table("//int2char(i)//") = ", i
               call this % table(i) % print()
            enddo
         endif
      endif

   end subroutine print_zmi_hashtable

   !-----------------------------------------------------------------------
   !Function get_storage_size
   !-----------------------------------------------------------------------
   function get_storage_size_zmi_hashtable(this) result(size_in_bits)
      implicit none
      class(zmi_hashtable_type), intent(in) :: this
      integer(int64) :: size_in_bits
      integer :: i

      size_in_bits = storage_size(this)
      do i = 1, this % ne_max
         size_in_bits = size_in_bits + storage_size(this % table(i) % get_storage_size()) + &
         storage_size(this % table(i))
      enddo

   end function get_storage_size_zmi_hashtable

   !-----------------------------------------------------------------------
   !Subroutine clear_table_zmi_hashtable
   !-----------------------------------------------------------------------
   elemental subroutine  clear_table_zmi_hashtable(this)
      implicit none
      class(zmi_hashtable_type), intent(inout) :: this
      integer :: i, ts

      if(allocated(this % table)) then
         ts = size(this % table)
         do i = 1, ts
            call this % table(i) % delete()
         enddo
         deallocate(this % table)
      endif

   end subroutine clear_table_zmi_hashtable

   !-----------------------------------------------------------------------
   !Subroutine clear_deep_table_zmi_hashtable
   !-----------------------------------------------------------------------
   elemental subroutine  clear_deep_table_zmi_hashtable(this)
      implicit none
      class(zmi_hashtable_type), intent(inout) :: this
      integer :: i, ts

      if(allocated(this % table)) then
         ts = size(this % table)
         do i = 1, ts
            call this % table(i) % delete()
         enddo
         deallocate(this % table)
      endif

   end subroutine clear_deep_table_zmi_hashtable

   !-----------------------------------------------------------------------
   !Subroutine clear_rtable_zmi_hashtable
   !-----------------------------------------------------------------------
   elemental subroutine  clear_rtable_zmi_hashtable(this)
      implicit none
      class(zmi_hashtable_type), intent(inout) :: this

      if(allocated(this % rtable)) then
         deallocate(this % rtable)
      endif

   end subroutine clear_rtable_zmi_hashtable

   !-----------------------------------------------------------------------
   !Subroutine clear_deep_rtable_zmi_hashtable
   !-----------------------------------------------------------------------
   elemental subroutine  clear_deep_rtable_zmi_hashtable(this)
      implicit none
      class(zmi_hashtable_type), intent(inout) :: this

      if(allocated(this % rtable)) then
         deallocate(this % rtable)
      endif

   end subroutine clear_deep_rtable_zmi_hashtable

   !-----------------------------------------------------------------------
   !Subroutine clear_deep_zmi_hashtable
   !-----------------------------------------------------------------------
   elemental subroutine clear_deep_zmi_hashtable(this)
      implicit none
      class(zmi_hashtable_type), intent(inout) :: this

      call this % clear_deep_table()
      call this % clear_deep_rtable()
      this % ne_max = 0
      this % ne_init = 0
      this % nc_max = 0
      this % ne = 0
      this % hf_index = 0
      this % nc_mean = 0.0_dp
      this % load_factor = 0.0_dp
      this % nc_sum = 0
      if(allocated(this % hf_container)) deallocate(this % hf_container)
      this % hf_pointer => null()
      this % first = .true.

   end subroutine clear_deep_zmi_hashtable

   !-----------------------------------------------------------------------
   !Subroutine clear_zmi_hashtable (shallow) memory leaks possible if used improperly
   !-----------------------------------------------------------------------
   elemental subroutine clear_zmi_hashtable(this)
      implicit none
      class(zmi_hashtable_type), intent(inout) :: this

      call this % clear_table()
      call this % clear_rtable()
      this % ne_max = 0
      this % ne_init = 0
      this % nc_max = 0
      this % ne = 0
      this % hf_index = 0
      this % nc_mean = 0.0_dp
      this % load_factor = 0.0_dp
      this % nc_sum = 0
      if(allocated(this % hf_container)) deallocate(this % hf_container)
      this % hf_pointer => null()
      this % first = .true.

   end subroutine clear_zmi_hashtable

   !-----------------------------------------------------------------------
   !Subroutine finalise_zmi_hashtable
   !doing it explicitely
   !-----------------------------------------------------------------------
   elemental subroutine  finalise_zmi_hashtable(this)
      implicit none
      type(zmi_hashtable_type), intent(inout) :: this
      call this % clear_deep()
   end subroutine finalise_zmi_hashtable

   !-----------------------------------------------------------------------
   !Subroutine resize_zmi_hashtable - expand table, 
   ! TODO: make it more efficient
   ! TODO: add shrink functionality
   !-----------------------------------------------------------------------
   subroutine  resize_zmi_hashtable(this)
      implicit none
      class(zmi_hashtable_type), intent(inout) :: this
      integer, dimension(:), allocatable :: keys
      integer, dimension(:), allocatable :: values
      integer :: ind, depth, new_depth
      integer :: new_ne_max
      integer :: new_ne
      integer :: new_nc_max
      real(dp) :: new_nc_mean
      integer :: new_nc_sum
      real(dp) :: new_load_factor
      integer :: i,j, ts
      real(dp), parameter :: LOAD_FACTOR_MAX = 0.75_dp ! LOAD_FACTOR_MAX  ad hoc, TODO: optimize it
      real(dp), parameter :: NC_MEAN_FACTOR_MAX = 1.0_dp ! ad hoc, TODO: optimize it
!             real(dp), parameter :: FB =(1.0_dp+sqrt(5.0_dp))/2.0_dp  ! 1.6180339887498948482045868343656381177203091798057628621_dp
      real(dp), parameter :: FB = 2.0_dp ! ad hoc, TODO: optimize it

      !       if(this % load_factor > LOAD_FACTOR_MAX) then ! do resize hash
      if(this % nc_mean > NC_MEAN_FACTOR_MAX) then ! do resize hash
         write(*,*) "rehashing ...", this % load_factor, this % nc_max, this % nc_mean
         new_ne = 0
         new_nc_max = 0
         new_nc_mean = 0.0_dp
         new_nc_sum = 0
         new_load_factor = 0.0_dp
         new_ne_max = this % get_optimal_prime_number(int(this % ne_max*FB) + 1, 1) ! twice + 1; TODO: optimize it

         if(new_ne_max == this % ne_max) then
            write(*,*) "skip rehashing ..."
            return ! skip resize
         endif

         allocate(this % rtable(new_ne_max))
         ! max possible number of nodes in list (each element of this table) == nc_max + 1 == max number of collisions + one(no collision case)
         allocate(keys(this % nc_max + 1))
         allocate(values(this % nc_max + 1))

         do i = 1, this % ne_max
            depth = this % table(i) % get_size()

            if(depth > 0) then ! list is not empty
               call this % table(i) % export(keys, values)
               do j = 1, depth
                  ind = this % hash2index(this % hf_pointer(keys(j)), new_ne_max)
                  call this % rtable(ind) % add_first(keys(j), values(j))
                  new_ne = new_ne + 1
                  new_depth = this % rtable(ind) % get_size()
                  !update stat, if collision
                  if(new_depth > 1) then
                     new_nc_sum = new_nc_sum + 1
                     new_nc_mean = new_nc_sum / real(new_ne, dp)
                  endif
                  if(new_nc_max < new_depth - 1) new_nc_max = new_depth - 1
               enddo
            endif
         enddo
         ! consistancy check
         !       write(*,*) "n = ", this % n, "new_n = ", new_n
         if(this % ne /= new_ne) stop "resize_zmi_hashtable error"

         ! delete table
         call this % clear_deep_table()

         ts = size(this % rtable)
         !move everything from rtable to empty table
         allocate(this % table(ts))
         do i = 1, ts
            call this % table(i) % move(this % rtable(i)) ! move list nodes from rtable(i) to table(i)

         enddo

         call this % clear_deep_rtable() ! clear now empty rtable
         !         deallocate(this % rtable) ! already done in clear_deep
         deallocate(keys)
         deallocate(values)
         ! update coefs
         new_load_factor = new_ne / real(new_ne_max,dp)
         this % ne_max = new_ne_max
         this % ne = new_ne
         this % nc_max = new_nc_max
         this % nc_mean = new_nc_mean
         this % nc_sum = new_nc_sum
         this % load_factor =  new_load_factor
      endif

   end subroutine resize_zmi_hashtable


   !-----------------------------------------------------------------------
   !Function contains_key
   !-----------------------------------------------------------------------
   function  contains_key_zmi_hashtable(this, key) result(answer)
      implicit none
      class(zmi_hashtable_type), intent(in) :: this
      integer, intent(in) :: key
      integer :: val
      integer :: nindex
      logical :: answer
      call this % search(key, val, nindex, answer)
   end function contains_key_zmi_hashtable

end module zmi_hashtable_module

!-----------------------------------------------------------------------
!Module timer_module simple cpu_time timer
!-----------------------------------------------------------------------
module timer_module
   implicit none
   private ! all
   public :: timer_type

   integer, parameter :: dp = kind(1.0d0)

   type :: timer_type
      private ! all
      logical :: started = .false.
      logical :: stopped = .false.
      real(dp) :: start_time = 0.0_dp
      real(dp) :: finish_time = 0.0_dp
   contains
      procedure, pass :: init => init_timer
      procedure, pass :: start => start_timer
      procedure, pass :: stop  => stop_timer
      procedure, pass :: elapsed_time
      procedure, pass :: print => print_timer
   end type timer_type

contains

   subroutine init_timer(this)
      implicit none
      class(timer_type), intent(inout) :: this
      this % started = .false.
      this % stopped = .false.
      this % start_time = 0.0_dp
      this % finish_time = 0.0_dp
   end subroutine init_timer

   subroutine start_timer(this)
      implicit none
      class(timer_type), intent(inout) :: this
      this % started = .true.
      call cpu_time(this % start_time)
   end subroutine start_timer

   subroutine stop_timer(this)
      implicit none
      class(timer_type), intent(inout) :: this
      call cpu_time(this % finish_time)
      this % stopped = .true.
   end subroutine stop_timer

   function elapsed_time(this)
      implicit none
      class(timer_type), intent(inout) :: this
      real(dp)  :: elapsed_time

      if ( .not.this % started ) then
         elapsed_time = 0.0_dp
         return
      end if

      if ( .not.this % stopped ) call this % stop()

      elapsed_time =  this % finish_time - this % start_time

   end function elapsed_time

   subroutine print_timer(this)
      implicit none
      class(timer_type), intent(inout) :: this
      real(dp)  :: elapsed_time
      write(*,*)"------------------------------------------------------------"
      write(*,*)"started = ", this % started
      write(*,*)"stopped = ", this % stopped
      write(*,*)"start_time = ", this % start_time
      write(*,*)"finish_time = ", this % finish_time
      elapsed_time = this % elapsed_time()
      write(*,*) "elapsed_time = ", elapsed_time
      write(*,*)"------------------------------------------------------------"
   end subroutine print_timer


end module timer_module

!-----------------------------------------------------------------------
!Main program test_zmi_hashtable
!-----------------------------------------------------------------------
program    test_zmi_hashtable
   use, intrinsic :: iso_fortran_env
   use, intrinsic :: iso_c_binding
   use zmi_hashtable_module
   use timer_module
   implicit none

   integer :: k
   integer, parameter :: dp = selected_real_kind(2*precision(1.0))
   integer, parameter :: hsize = 1000!10000000
   type(timer_type) :: timer

   do k = 1, 12
      call test_delete_random_with_reheash(k)
      write(*,*) "-----------------------------------------------------------------------"
   enddo

contains

   !-----------------------------------------------------------------------
   !Subroutine test_delete_random_with_reheash
   !-----------------------------------------------------------------------
   subroutine  test_delete_random_with_reheash(ind)
      implicit none
      integer, intent(in) :: ind
      integer :: i
      integer, dimension(:), allocatable :: keys
      integer :: val
      logical :: found
      logical :: deleted
      integer :: nindex
      type(zmi_hashtable_type), allocatable :: ht
      write(*,*) "test_delete_random_with_reheash"
      val = 0

      call init_random_seed()

      call timer % init()
      call timer % start()
      allocate(ht)
      allocate(keys(hsize))
      call ht % init(ind, hsize)
!       call ht % init(ind) ! default capacity + rehashing   <============= change here to RESIZE
      call timer % stop()
      write(*,*) "init time = ", timer % elapsed_time()
      write(*,*) allocated(ht), "storage_size approx ", ht % get_storage_size()/(8.*1024.*1024.), "Mb"


      call timer % init()
      call timer % start()
      write(*,*) "generate random_permutations"
      do i = 1, hsize
         keys(i) = i
      enddo
      call shuffle_integer_array(keys)
      call timer % stop()
      write(*,*) "random_permutations time = ", timer % elapsed_time()


      call timer % init()
      call timer % start()
      do i = 1, hsize
         call ht % insert(keys(i),keys(i))
      enddo
      call timer % stop()
      write(*,*) "insert time = ", timer % elapsed_time()
      write(*,*) allocated(ht), "storage_size approx ", ht % get_storage_size()/(8.*1024.*1024.), "Mb"
      !       call ht % print()

      call timer % init()
      call timer % start()
      do i = 1, hsize
         call ht % search(keys(i), val, nindex, found)
         if(.not. found) stop  "not found error"
         if(nindex<=0) stop  "nindex <=0 error"
         if(val /= keys(i))  stop  "val /= keys(i) error"

         if (.not. ht % contains_key(keys(i)))   stop  ".not. ht % includes(keys(i)) error"
      enddo
      call timer % stop()
      write(*,*) "search time = ", timer % elapsed_time()
      !       call ht % print()

      call timer % init()
      call timer % start()
      do i = 1, hsize
         deleted = .false.
         call ht % remove(keys(i), val, nindex, deleted)
         if (.not. deleted) stop "not deleted error"
         !          write(*,*) i, val, found
      enddo
      call timer % stop()
      write(*,*) "delete time = ", timer % elapsed_time()
      !       call ht % print()

      if(allocated(keys)) deallocate(keys)
      
      call timer % init()
      call timer % start()
      call ht % clear_deep()
      call timer % stop()
      write(*,*) "clear_deep time = ", timer % elapsed_time()
      !       call ht % print()      
      if(allocated(ht)) deallocate(ht)
   end subroutine test_delete_random_with_reheash

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

   !-----------------------------------------------------------------------
   !Function random_permutation is algorithm P in in Knuth's book:
   ! TODO: check
   ! NOTE:  is probably the fastest.
   ! NOTE:  The modern version of the Fisher–Yates shuffle, designed for computer use,
   ! was introduced by Richard Durstenfeld in 1964[2] and popularized by Donald E.
   ! Knuth in The Art of Computer Programming as "Algorithm P".
   !-----------------------------------------------------------------------
   function random_permutation(num)
      implicit none
      integer, intent(in) :: num

      integer :: i, j, k, temp
      integer, dimension(num) :: random_permutation
      real(dp) :: randn

      ! init array
      do i=1,num
         random_permutation(i) = i
      enddo

      do j=num,2,-1
         call random_number(randn)
         k = floor(j*randn) + 1 
         ! exchange p(k) and p(j)
         temp = random_permutation(k)
         random_permutation(k) = random_permutation(j)
         random_permutation(j) = temp
      end do

   end function random_permutation

   !-----------------------------------------------------------------------
   !Subroutine shuffle_integer_array is algorithm P in in Knuth's book:
   ! TODO: check
   ! NOTE:  in-place shuffle of integer array
   !-----------------------------------------------------------------------
   subroutine shuffle_integer_array(array)
      implicit none
      integer, dimension(:), intent(inout) :: array

      integer :: j, k, temp, num
      real(dp) :: randn

      num = size(array)

      do j=num,2,-1
         call random_number(randn)
         k = floor(j*randn) + 1
         ! exchange array(k) and array(j)
         temp = array(k)
         array(k) = array(j)
         array(j) = temp
      end do

   end subroutine shuffle_integer_array

end program test_zmi_hashtable
