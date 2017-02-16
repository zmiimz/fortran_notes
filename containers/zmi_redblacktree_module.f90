!-----------------------------------------------------------------------
!Copyright (c) 2017 M. Zapukhlyak
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

! red-black tree data structure for quick search, insert and delete operations
! with integer key and integer value  (non-generic!)
! Tested but can still contain some bugs!


!-----------------------------------------------------------------------
!Module zmi_utils_module
!-----------------------------------------------------------------------
module zmi_utils_module
   implicit none
   private ! all by default
   public :: int2char, array2char

contains

   !-----------------------------------------------------------------------
   !Function integer array to string
   ! simple but not thoroughly tested...
   !-----------------------------------------------------------------------
   function array2char(ia) result (sa)
      implicit none
      integer, dimension(:), intent (in) :: ia
      character(len=:),allocatable :: sa
      integer :: length, slength
      integer, dimension(:), allocatable :: ialength
      integer :: i, lpos,rpos

      length = size(ia)

      if (length == 0) then
         allocate(character(len = 0) :: sa) ! empty, should be ok in f90
      else
         allocate(ialength(length))

         do i = 1,length
            ialength(i) = csize(ia(i))
         enddo
         slength = sum(ialength)
         allocate(character(len = slength) :: sa)
         lpos = 1
         do i = 1,length
            rpos = lpos + ialength(i) - 1
            sa(lpos:rpos) = int2char(ia(i))
            lpos = rpos + 1
         enddo
      endif

   end function array2char

   !-----------------------------------------------------------------------
   !Function integer to character
   !-----------------------------------------------------------------------
   function int2char(i) result (c)
      implicit none
      integer, intent (in) :: i
      character (len=csize(i)) :: c
      if(i<0) then
         write (c,'(A,i0)') "-",abs(i)
      else
         write (c,'(i0)') i
      end if

   end function int2char

   pure function csize(i) result (sz)
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

   end function csize

end module zmi_utils_module

!-----------------------------------------------------------------------
!Module zmi_redblacktree_module
!-----------------------------------------------------------------------
module zmi_redblacktree_module
   use zmi_utils_module
   implicit none
   private ! all
   public :: rbtree_t
   logical, parameter, private :: debug_flag = .false.

   ! colours
   integer, parameter :: RED = 1
   integer, parameter :: BLACK = 0

   ! three node pointer type
   type :: rbtree_node_ref_t
      logical, allocatable :: dummy !workaround: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=61767  https://gcc.gnu.org/bugzilla/show_bug.cgi?id=66577
      type(rbtree_node_t), pointer :: item => null()
   contains
      procedure, pass :: print => print_rbtree_node_ref_t
   end type rbtree_node_ref_t

   ! three node type
   type :: rbtree_node_t
      integer :: key
      integer :: value
      type(rbtree_node_ref_t) :: left
      type(rbtree_node_ref_t) :: right
      type(rbtree_node_ref_t) :: parent
      integer :: colour = -1 ! undefined by default
   contains
      procedure, pass :: print => print_rbtree_node_t
      final :: rbtree_node_t_final_scalar
   end type rbtree_node_t


   ! rb tree type
   type, extends(rbtree_node_ref_t) :: rbtree_t
      integer :: tree_size = 0
   contains
      procedure, pass :: export => export_rbtree
      procedure, pass :: insert => insert_node_rbtree
      procedure, pass :: delete => delete_node_rbtree
      procedure, pass :: includes => includes_node_rbtree
      procedure, pass :: set_value => set_value_rbtree
      procedure, pass :: query_value => query_value_rbtree
      procedure, pass :: max_key => get_max_key_rbtree
      procedure, pass :: min_key => get_min_key_rbtree
      procedure, pass :: print_node => print_node_rbtree
      procedure, pass :: print_tree => print_rbtree
      final :: rbtree_final_scalar
   end type rbtree_t

   interface operator(==)
      procedure equals
   end interface operator(==)

contains

   subroutine  print_node_rbtree(this, key, comment)
      implicit none
      class(rbtree_t), intent(in) :: this
      integer, intent(in) :: key
      character(len=*), optional :: comment
      type(rbtree_node_ref_t) :: node

      node = lookup_node(this, key)

      if(isvalid(node)) then
         if(present(comment)) then
            call print_rbtree_node_ref_t(node, comment)
         else
            call print_rbtree_node_ref_t(node)
         endif
      else
         write(*,*)"____________________________________________________________"
         write(*,*) "NODE with key = ", key, " NOT FOUND"
         write(*,*)"____________________________________________________________"
      endif
   end subroutine  print_node_rbtree

   subroutine  print_rbtree_node_ref_t(this, comment)
      implicit none
      class(rbtree_node_ref_t), intent(in) :: this
      character(len=*), optional :: comment
      if(associated(this % item)) then
         if(present(comment)) then
            call this % item % print(comment)
         else
            call this % item % print()
         endif
      else
         write(*,*)"____________________________________________________________"
         write(*,*) "associated(this % item) = ", associated(this % item)
         write(*,*)"____________________________________________________________"
      endif

   end subroutine print_rbtree_node_ref_t

   subroutine  print_rbtree_node_t(this, comment)
      implicit none
      class(rbtree_node_t), intent(in) :: this
      character(len=*), optional :: comment

      write(*,*)"____________________________________________________________"
      if(present(comment)) write(*,*) comment
      write(*,*) "node % key = ", this % key
      write(*,*) "node % value = ", this % value
      write(*,*) "node % colour = ", this % colour
      write(*,*) "node % parent % item = ", associated(this % parent % item)
      write(*,*) "node % left % item = ", associated(this % left % item)
      write(*,*) "node % right % item = ", associated(this % right % item)
      write(*,*)"____________________________________________________________"
   end subroutine print_rbtree_node_t

   recursive subroutine rbtree_node_t_final_scalar(scalar)
      implicit none
      type(rbtree_node_t), intent(inout) :: scalar
      if (isvalid( scalar%left)) deallocate(scalar%left%item)
      if (isvalid( scalar%right)) deallocate(scalar%right%item)
   end subroutine rbtree_node_t_final_scalar

   elemental subroutine rbtree_final_scalar(scalar)
      implicit none
      type(rbtree_t), intent(inout) :: scalar
      if (isvalid(scalar%rbtree_node_ref_t)) deallocate(scalar%item)
   end subroutine rbtree_final_scalar

   pure function isvalid(n)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n
      logical :: isvalid

      isvalid = associated(n%item)
   end function isvalid

   pure function isinvalid(n)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n
      logical :: isinvalid

      isinvalid = .not. associated(n%item)
   end function isinvalid

   pure function equals(lhs, rhs)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: lhs
      type(rbtree_node_ref_t), intent(in) :: rhs
      logical :: equals

      equals = associated(lhs%item, rhs%item)
   end function equals

   function get_parent(n)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n
      type(rbtree_node_ref_t) :: get_parent

      get_parent = n%item%parent

      if (debug_flag .and. (get_parent == n))  &
         error stop 'get_parent: node is its own parent.'
   end function get_parent

   subroutine set_parent(n, new_parent)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n
      type(rbtree_node_ref_t), intent(in) :: new_parent

      n%item%parent = new_parent
   end subroutine set_parent

   function get_left(n)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n
      type(rbtree_node_ref_t) :: get_left

      get_left = n%item%left
   end function get_left

   subroutine set_left(n, new_left)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n
      type(rbtree_node_ref_t), intent(in) :: new_left
      n%item%left = new_left
   end subroutine set_left

   function get_right(n)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n
      type(rbtree_node_ref_t) :: get_right

      get_right = n%item%right
   end function get_right

   subroutine set_right(n, new_right)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n
      type(rbtree_node_ref_t), intent(in) :: new_right

      n%item%right = new_right
   end subroutine set_right

   function get_grandparent(n)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n
      type(rbtree_node_ref_t) :: get_grandparent

      if (debug_flag .and. (isinvalid(get_parent(n)))) stop 'get_grandparent: called with the root node'
      get_grandparent = get_parent( get_parent(n))
      if (debug_flag .and. isinvalid(get_grandparent)) stop 'get_grandparent: called with a child node of root'
      if (debug_flag .and. (get_grandparent == n)) stop "get_grandparent: node is it's own grandparent"
   end function get_grandparent

   function isleft(n)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n
      logical :: isleft

      isleft = n == get_left(get_parent(n))
   end function isleft

   function isright(n)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n
      logical :: isright

      isright = n == get_right(get_parent(n))
   end function isright

   function get_sibling(n)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n
      type(rbtree_node_ref_t) :: get_sibling

      if (debug_flag .and. (isinvalid(get_parent(n)))) stop 'get_sibling: node has no parent'

      if (isleft(n)) then
         get_sibling = get_right(get_parent(n))
      else if (isright(n)) then
         get_sibling = get_left(get_parent(n))
      else
         error stop 'get_sibling: node has no siblings'
      end if
   end function get_sibling


   function get_uncle(n)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n
      type(rbtree_node_ref_t) :: get_uncle

      if (debug_flag .and. (isinvalid(get_parent(n)))) stop 'get_uncle: root node has no uncle'
      if (debug_flag .and. (isinvalid(get_grandparent(n)))) stop 'get_uncle: children of root have no uncle'

      get_uncle = get_sibling(get_parent(n))
   end function get_uncle

   function get_colour(n)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n
      integer :: get_colour

      if (isvalid(n)) then
         get_colour = n%item%colour
      else
         get_colour = BLACK ! Property 3:  All leaves (NIL) are black.
      end if
   end function get_colour

   subroutine set_colour(n, c)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n
      integer, intent(in) :: c

      n%item%colour = c
   end subroutine set_colour

   function hasleft(n)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n
      logical :: hasleft

      hasleft = isvalid(get_left(n))
   end function hasleft

   function hasright(n)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n
      logical :: hasright

      hasright = isvalid(get_right(n))
   end function hasright

   ! n must be valid.
   function query_value(n)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n
      integer :: query_value

      query_value = n%item%value
   end function query_value

   ! n may be invalid.
   subroutine get_value(n, value)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n
      integer, intent(out), pointer :: value

      if (isvalid(n)) then
         value => n%item%value
      else
         value => null()
      end if
   end subroutine get_value

   subroutine set_value(n, value)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n
      integer, intent(in) :: value

      n%item%value = value
   end subroutine set_value

   function get_key(n)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n
      integer :: get_key
      get_key = -1
      if (isvalid(n)) then
         get_key = n%item%key
      end if
   end function get_key

   subroutine set_key(n, key)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n
      integer, intent(in) :: key

      n%item%key = key
   end subroutine set_key

   function maximum_node(n)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n
      type(rbtree_node_ref_t) :: maximum_node

      if (debug_flag .and. isinvalid(n)) stop 'maximum_node: argument invalid'

      maximum_node = n
      do while (hasright(maximum_node))
         maximum_node = get_right(maximum_node)
      end do
   end function maximum_node

   function get_max_key_rbtree(this)
      implicit none
      class(rbtree_t), intent(in) :: this
      integer :: get_max_key_rbtree
      get_max_key_rbtree = get_maximum_node_key(this%rbtree_node_ref_t)
   end function get_max_key_rbtree

   function get_maximum_node_key(n)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n
      type(rbtree_node_ref_t) :: max_node
      integer :: get_maximum_node_key

      if (debug_flag .and. isinvalid(n)) stop 'get_maximum_node_key: argument invalid'
      max_node = n
      do while (hasright(max_node))
         max_node = get_right(max_node)
      end do
      get_maximum_node_key = get_key(max_node)
   end function get_maximum_node_key

   function minimum_node(n)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n
      type(rbtree_node_ref_t) :: minimum_node

      if (debug_flag .and. isinvalid(n)) stop 'minimum_node: argument invalid'
      minimum_node = n
      do while (hasleft(minimum_node))
         minimum_node = get_left(minimum_node)
      end do
   end function minimum_node

   function get_min_key_rbtree(this)
      implicit none
      class(rbtree_t), intent(in) :: this
      integer :: get_min_key_rbtree
      get_min_key_rbtree = get_minimum_node_key(this%rbtree_node_ref_t)
   end function get_min_key_rbtree

   function get_minimum_node_key(n)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n
      type(rbtree_node_ref_t) :: min_node
      integer :: get_minimum_node_key

      if (debug_flag .and. isinvalid(n)) stop 'get_minimum_node_key: argument invalid'
      min_node = n
      do while (hasleft(min_node))
         min_node = get_left(min_node)
      end do
      get_minimum_node_key = get_key(min_node)
   end function get_minimum_node_key

   function node_key(n)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n
      integer :: node_key

      if (debug_flag .and. isinvalid(n)) stop 'node_key: argument invalid'
      node_key = get_key(n)
   end function node_key

   function new_node(key, value, colour, left, right) result(n)
      implicit none
      type(rbtree_node_ref_t) :: n
      integer, intent(in) :: key
      integer, intent(in) :: value
      integer, intent(in) :: colour
      type(rbtree_node_ref_t), intent(in), optional :: left
      type(rbtree_node_ref_t), intent(in), optional :: right

      allocate(n%item)
      n%item%key = key
      n%item%value = value
      call set_colour(n, colour)

      if (present(left)) then
         call set_left(n, left)
         if (isvalid(left)) call set_parent(left, n)
      end if

      if (present(right)) then
         call set_right(n, right)
         if (isvalid(right)) call set_parent(right, n)
      end if
   end function new_node

   subroutine destroy_node(n)
      implicit none
      type(rbtree_node_ref_t), intent(inout) :: n

      if (associated(n%item)) deallocate(n%item)
   end subroutine destroy_node

   subroutine release_node(n)
      implicit none
      type(rbtree_node_ref_t), intent(inout) :: n

      n%item => null()
   end subroutine release_node

   subroutine verify_properties(t)
      implicit none
      type(rbtree_t), intent(in) :: t

      call verify_property_1(t%rbtree_node_ref_t)
      call verify_property_2(t%rbtree_node_ref_t)
      ! verify_property_3 : 3 All leaves (NIL) are black. it is always true, see function get_colour(n)
      call verify_property_4(t%rbtree_node_ref_t)
      call verify_property_5(t%rbtree_node_ref_t)
   end subroutine verify_properties

   ! 1 Each node is either red or black.
   recursive subroutine verify_property_1(n)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n

      if (get_colour(n)  /= RED .and. get_colour(n)  /= BLACK) stop 'Error in verify_property_1: node colour is not set'
      if (isinvalid(n)) return
      if (n == get_parent(n)) stop 'Error in verify_property_1: invalid parent node'
      if (n == get_left(n)) stop 'Error in verify_property_1: invalid left node'
      if (n == get_right(n)) stop 'Error in verify_property_1: invalid right node'
      call verify_property_1(get_left(n))
      call verify_property_1(get_right(n))
   end subroutine verify_property_1

   ! 2 The root is black.
   subroutine verify_property_2(root)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: root
      if (get_colour(root)  /= BLACK) stop 'Error in verify_property_2: root is not black'
   end subroutine verify_property_2

   ! 4 If a node is red, then both its children are black.
   recursive subroutine verify_property_4(n)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n

      if (get_colour(n)  == RED) then
         if (get_colour(get_left(n))  /= BLACK) stop 'Error in verify_property_4: - left node is not black'
         if (get_colour(get_right(n))  /= BLACK) stop 'Error in verify_property_4: - right node is not black'
         if (get_colour(get_parent(n))  /= BLACK) stop 'Error in verify_property_4: - parent node is not black'
      end if
      if (isinvalid(n)) return
      call verify_property_4(get_left(n))
      call verify_property_4(get_right(n))
   end subroutine verify_property_4

   ! 5 Every path from a given node to any of its descendant NIL nodes contains the same number of black nodes.
   subroutine verify_property_5(root)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: root
      integer :: path_black_count

      path_black_count = -1
      call verify_property_5_helper(root, 0, path_black_count)
   end subroutine verify_property_5

   recursive subroutine verify_property_5_helper(n, black_count, path_black_count)
      implicit none
      type(rbtree_node_ref_t), intent(in) :: n
      integer, value :: black_count
      integer, intent(inout) :: path_black_count

      if (get_colour(n)  == BLACK) black_count = black_count + 1
      if (isinvalid(n)) then
         if (path_black_count == -1) then
            path_black_count = black_count
         else
            if (black_count /= path_black_count)  &
               error stop 'Error in verify_property_5_helper: wrong count of black nodes in path'
         end if
         return
      end if
      call verify_property_5_helper(get_left(n) , black_count, path_black_count)
      call verify_property_5_helper(get_right(n) , black_count, path_black_count)
   end subroutine verify_property_5_helper

   function includes_node_rbtree(this, key) result(answer)
      implicit none
      class(rbtree_t), intent(in) :: this
      integer, intent(in) :: key
      logical :: answer
      type(rbtree_node_ref_t) :: node
      answer = .false.
      node = lookup_node(this, key)
      if (isvalid(node) .and. (key == get_key(node))) answer = .true.
   end function includes_node_rbtree

   function lookup_node(t, key) result(n)
      implicit none
      type(rbtree_t), intent(in) :: t
      integer, intent(in) :: key
      type(rbtree_node_ref_t) :: n

      n = t%rbtree_node_ref_t
      do while(isvalid(n))
         if (key == get_key(n)) then
            return
         else if (key < get_key(n)) then
            n = get_left(n)
         else ! key > get_key(n)
            n = get_right(n)
         end if
      end do
   end function lookup_node

   function query(t, key) result(v)
      implicit none
      type(rbtree_t), intent(in) :: t
      integer, intent(in) :: key
      integer :: v
      type(rbtree_node_ref_t) :: n
      n = lookup_node(t, key)
      if (isvalid(n)) then
         v = query_value(n)
      else
         v = 0
      end if
   end function query

   function query_value_rbtree(this, key) result(v)
      implicit none
      class(rbtree_t), intent(in) :: this
      integer, intent(in) :: key
      integer :: v
      v = query(this, key)
   end function query_value_rbtree

   subroutine set_value_rbtree(this, key, v)
      implicit none
      class(rbtree_t), intent(inout) :: this
      integer, intent(in) :: key
      integer, intent(in) :: v
      type(rbtree_node_ref_t) :: n

      if (v == query(this, key)) return ! no change
      n = lookup_node(this, key)
      call set_value(n, v)
   end subroutine set_value_rbtree

   subroutine get(t, key, value)
      implicit none
      type(rbtree_t), intent(in) :: t
      integer, intent(in) :: key
      integer, intent(out), pointer :: value
      type(rbtree_node_ref_t) :: n

      n = lookup_node(t, key)
      call get_value(n, value)
   end subroutine get

   subroutine rotate_left(t, n)
      implicit none
      type(rbtree_t), intent(inout) :: t
      type(rbtree_node_ref_t), intent(in) :: n
      type(rbtree_node_ref_t) :: r

      r = get_right(n)
      call replace_node(t, n, r)
      call set_right(n, get_left(r))
      if (hasleft(r)) then
         call set_parent(get_left(r) , n)
      end if
      call set_left(r, n)
      call set_parent(n, r)
   end subroutine rotate_left

   subroutine rotate_right(t, n)
      implicit none
      type(rbtree_t), intent(inout) :: t
      type(rbtree_node_ref_t), intent(in):: n
      type(rbtree_node_ref_t) :: l

      l = get_left(n)
      call replace_node(t, n, l)
      call set_left(n, get_right(l))
      if (hasright(l)) then
         call set_parent(get_right(l) , n)
      end if
      call set_right(l, n)
      call set_parent(n, l)
   end subroutine rotate_right

   subroutine replace_node(t, oldn, newn)
      implicit none
      type(rbtree_t), intent(inout) :: t
      type(rbtree_node_ref_t), intent(in) :: oldn
      type(rbtree_node_ref_t), intent(in) :: newn

      if (isvalid(get_parent(oldn))) then
         if (isleft(oldn)) then
            call set_left(get_parent(oldn) , newn)
         else if (isright(oldn)) then
            call set_right(get_parent(oldn) , newn)
         else
            error stop 'Error in replace_node: node is neither left nor right'
         end if
      else
         t%rbtree_node_ref_t = newn
      end if
      if (isvalid(newn)) then
         call set_parent(newn, get_parent(oldn))
      end if
   end subroutine replace_node

   subroutine insert_node_rbtree(this, key, value)
      implicit none
      class(rbtree_t), intent(inout) :: this
      integer, intent(in) :: key
      integer, intent(in) :: value

      call insert(this, key, value)
   end subroutine insert_node_rbtree

   subroutine insert(t, key, value)
      implicit none
      type(rbtree_t), intent(inout) :: t
      integer, intent(in) :: key
      integer, intent(in) :: value
      type(rbtree_node_ref_t) :: inserted_node
      type(rbtree_node_ref_t) :: n

      inserted_node = new_node(key, value, RED)

      if (isvalid(t%rbtree_node_ref_t)) then
         n = t%rbtree_node_ref_t
         do
            if (key == get_key(n)) then
               call set_value(n, value)
               call destroy_node(inserted_node)
               return
            else if (key < get_key(n)) then
               if (isinvalid(get_left(n))) then
                  call set_left(n, inserted_node)
                  exit
               else
                  n = get_left(n)
               end if
            else ! key > get_key(n)
               if (isinvalid(get_right(n))) then
                  call set_right(n, inserted_node)
                  exit
               else
                  n = get_right(n)
               end if
            end if
         end do
         call set_parent(inserted_node, n)
      else
         t%rbtree_node_ref_t = inserted_node
      end if
      call insert_case1(t, inserted_node)
      t % tree_size = t % tree_size + 1
      if (debug_flag) call verify_properties(t)
   end subroutine insert

   recursive subroutine insert_case1(t, n)
      implicit none
      type(rbtree_t), intent(inout) :: t
      type(rbtree_node_ref_t), intent(in) :: n

      if (isvalid(get_parent(n)) ) then
         call insert_case2(t, n)
      else
         call set_colour(n, BLACK)
      endif
   end subroutine insert_case1

   recursive subroutine insert_case2(t, n)
      implicit none
      type(rbtree_t), intent(inout) :: t
      type(rbtree_node_ref_t), intent(in) :: n

      if (get_colour(get_parent(n))  == BLACK) then
         return ! Tree is still valid
      else
         call insert_case3(t, n)
      endif
   end subroutine insert_case2

   recursive subroutine insert_case3(t, n)
      implicit none
      type(rbtree_t), intent(inout) :: t
      type(rbtree_node_ref_t), intent(in) :: n

      if (get_colour(get_uncle(n)) == RED) then
         call set_colour(get_parent(n) , BLACK)
         call set_colour(get_uncle(n), BLACK)
         call set_colour(get_grandparent(n) , RED)
         call insert_case1(t, get_grandparent(n))
      else
         call insert_case4(t, n)
      end if
   end subroutine insert_case3

   recursive subroutine insert_case4(t, n)
      implicit none
      type(rbtree_t), intent(inout) :: t
      type(rbtree_node_ref_t) :: n

      if (isright(n)  .and. isleft(get_parent(n))) then
         call rotate_left(t, get_parent(n))
         n = get_left(n)
      else if (isleft(n)  .and. isright(get_parent(n))) then
         call rotate_right(t, get_parent(n))
         n = get_right(n)
      end if
      call insert_case5(t, n)
   end subroutine insert_case4

   recursive subroutine insert_case5(t, n)
      implicit none
      type(rbtree_t), intent(inout) :: t
      type(rbtree_node_ref_t), intent(in) :: n

      call set_colour(get_parent(n) , BLACK)
      call set_colour(get_grandparent(n) , RED)
      if (isleft(n)  .and. isleft(get_parent(n))) then
         call rotate_right(t, get_grandparent(n));
      else if (isright(n)  .and. isright(get_parent(n))) then
         call rotate_left(t, get_grandparent(n))
      else
         error stop 'insert case5: parent and node not both left or both right'
      end if
   end subroutine insert_case5

   subroutine delete_node_rbtree(this, key)
      implicit none
      class(rbtree_t), intent(inout) :: this
      integer, intent(in) :: key
      call delete(this, key)
   end subroutine delete_node_rbtree

   subroutine delete(t, key)
      implicit none
      type(rbtree_t), intent(inout) :: t
      integer, intent(in) :: key
      type(rbtree_node_ref_t) :: n
      type(rbtree_node_ref_t) :: child

      n = lookup_node(t, key)

      if (isinvalid(n)) return

      if (hasleft(n)  .and. hasright(n)) then
         ! Copy key/value from predecessor and then delete it instead.
         block
            type(rbtree_node_ref_t) :: pred
            pred = maximum_node(get_left(n))
            call set_key(n , get_key(pred))
            call set_value(n, query_value(pred))
            n = pred

         end block
      end if

      if ( debug_flag  .and. (hasleft(n)  .and. hasright(n)) )  &
         stop 'delete: both left and right valid'

      if (isinvalid(get_right(n))) then
         child = get_left(n)
      else
         child = get_right(n)
      end if

      if (get_colour(n)  == BLACK) then
         call set_colour(n, get_colour(child))
         call delete_case1(t, n)
      end if
      call replace_node(t, n, child)
      if (isinvalid(get_parent(n)) .and. isvalid(child)) then
         ! root should be black
         call set_colour(child, BLACK)
      end if

      call set_left(n, rbtree_node_ref_t())
      call set_right(n, rbtree_node_ref_t())
      call destroy_node(n)
      t % tree_size = t % tree_size - 1
      if (debug_flag) call verify_properties(t)
   end subroutine delete

   recursive subroutine delete_case1(t, n)
      implicit none
      type(rbtree_t), intent(inout) :: t
      type(rbtree_node_ref_t), intent(in) :: n

      if (isinvalid(get_parent(n))) then
         return
      else
         call delete_case2(t, n)
      end if
   end subroutine delete_case1

   recursive subroutine delete_case2(t, n)
      implicit none
      type(rbtree_t), intent(inout) :: t
      type(rbtree_node_ref_t), intent(in) :: n

      if (get_colour(get_sibling( n)) == RED) then
         call set_colour(get_parent(n) , RED)
         call set_colour(get_sibling( n), BLACK)
         if (isleft(n)) then
            call rotate_left(t, get_parent(n))
         else
            call rotate_right(t, get_parent(n))
         end if
      end if
      call delete_case3(t, n)
   end subroutine delete_case2

   recursive subroutine delete_case3(t, n)
      implicit none
      type(rbtree_t), intent(inout) :: t
      type(rbtree_node_ref_t), intent(in) :: n

      if ( (get_colour(get_parent(n)) == BLACK)  &
         .and. (get_colour(get_sibling( n))  == BLACK)  &
         .and. (get_colour(get_left(get_sibling( n))) == BLACK)  &
         .and. (get_colour(get_right(get_sibling( n)))  == BLACK)) then
         call set_colour(get_sibling( n), RED)
         call delete_case1(t, get_parent(n))
      else
         call delete_case4(t, n)
      end if
   end subroutine delete_case3

   subroutine delete_case4(t, n)
      implicit none
      type(rbtree_t), intent(inout) :: t
      type(rbtree_node_ref_t), intent(in) :: n

      if ( (get_colour(get_parent(n))  == RED)  &
         .and. (get_colour(get_sibling( n)) == BLACK)  &
         .and. (get_colour(get_left(get_sibling( n))) == BLACK)  &
         .and. (get_colour(get_right(get_sibling( n))) == BLACK)) then
         call set_colour(get_sibling( n), RED)
         call set_colour(get_parent(n) , BLACK)
      else
         call delete_case5(t, n)
      end if
   end subroutine delete_case4

   subroutine delete_case5(t, n)
      implicit none
      type(rbtree_t), intent(inout) :: t
      type(rbtree_node_ref_t), intent(in) :: n

      if ( (isleft(n))  &
         .and. (get_colour(get_sibling( n)) == BLACK)  &
         .and. (get_colour(get_left(get_sibling(n))) == RED)  &
         .and. (get_colour(get_right(get_sibling(n))) == BLACK)) then
         call set_colour(get_sibling(n) , RED)
         call set_colour(get_left(get_sibling(n)), BLACK)
         call rotate_right(t, get_sibling(n))
      else if ( (isright(n))  &
         .and. (get_colour(get_sibling(n)) == BLACK)  &
         .and. (get_colour(get_right(get_sibling(n))) == RED)  &
         .and. (get_colour(get_left(get_sibling(n))) == BLACK)) then
         call set_colour(get_sibling(n) , RED)
         call set_colour(get_right(get_sibling(n)) , BLACK)
         call rotate_left(t, get_sibling(n))
      end if
      call delete_case6(t, n)
   end subroutine delete_case5

   subroutine delete_case6(t, n)
      implicit none
      type(rbtree_t), intent(inout) :: t
      type(rbtree_node_ref_t), intent(in) :: n

      call set_colour(get_sibling(n) , get_colour(get_parent(n)))
      call set_colour(get_parent(n) , BLACK)
      if (isleft(n)) then
         if(isinvalid(get_sibling(n))) error stop 'Error in delete_case6: sibling node is invalid (isleft)'
         if(isinvalid(get_right(get_sibling(n)))) error stop 'Error in delete_case6: right node of sibling is invalid'
         if ((get_colour(get_right(get_sibling(n))) /= RED)) error stop 'Error in delete_case6: right node of sibling is not red'

         call set_colour(get_right(get_sibling(n)), BLACK)
         call rotate_left(t, get_parent(n))
      else
         if(isinvalid(get_sibling(n))) error stop 'Error in delete_case6: sibling node is invalid'
         if(isinvalid(get_left(get_sibling(n)))) error stop 'Error in delete_case6: left node of sibling is invalid'

         if ((get_colour(get_left(get_sibling(n))) /= RED)) error stop 'Error in delete_case6: left node of sibling is not red'

         call set_colour(get_left(get_sibling(n)) , BLACK)
         call rotate_right(t, get_parent(n))
      endif
   end subroutine delete_case6

   subroutine print_rbtree(this, unit)
      use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
      implicit none
      class(rbtree_t), intent(in) :: this
      integer, optional, intent(in) :: unit

      if(present(unit)) then
         call print_tree_helper(this%rbtree_node_ref_t, unit, 0)
         write(unit,*)
      else
         call print_tree_helper(this%rbtree_node_ref_t, OUTPUT_UNIT, 0)
         write(OUTPUT_UNIT,*)
      endif
   end subroutine print_rbtree

   recursive subroutine print_tree_helper(n, unit, indent)
      implicit none
      integer, intent(in) :: unit
      type(rbtree_node_ref_t), intent(in) :: n
      integer, intent(in) :: indent
      integer, parameter :: INDENT_STEP = 4

      if (isinvalid(n)) then
         write (unit, '(a)') '<empty tree>'
         return
      end if
      if (hasright(n)) then
         call print_tree_helper(get_right(n), unit, indent + INDENT_STEP)
      end if
      write(unit, fmt='(a)', advance='no') repeat(' ',indent)
      if (get_colour(n)  == BLACK) then
         write(unit, '(i0)') get_key(n)
      else
         write(unit, '(a,i0,a)') '<',get_key(n) ,'>' ! red node
      end if
      if (hasleft(n)) then
         call print_tree_helper(get_left(n) , unit, indent + INDENT_STEP)
      end if
   end subroutine print_tree_helper

   subroutine export_rbtree(this, filename)
      implicit none
      class(rbtree_t), intent(in) :: this
      character(len=*), intent(in):: filename
      call export_to_graphviz(this, filename)
   end subroutine export_rbtree

   !-----------------------------------------------------------------------
   !Subroutine export_tree_to_graphviz http://www.graphviz.org/
   !for printing tree structures in GraphViz dot files
   !-----------------------------------------------------------------------
   subroutine  export_to_graphviz(this, filename)
      implicit none
      type(rbtree_t), intent(in) :: this
      character(len=*):: filename
      integer :: stream, ios
      integer, save :: nullcount = 0
      integer :: key

      open(newunit=stream, file=filename//".gv", iostat=ios, access="stream", form = "unformatted", status='replace')

      write(stream) 'digraph BST {',char(13)
      write(stream) '    node [fontname="Arial"];',char(13)

      if(isinvalid(this%rbtree_node_ref_t)) then
         write(stream) char(13)
      elseif( (.not. (hasleft(this%rbtree_node_ref_t))) .and. (.not. (hasright(this%rbtree_node_ref_t)))) then
         key = get_key(this%rbtree_node_ref_t)
         if(get_colour(this%rbtree_node_ref_t)  == RED) then
            write(stream) '    '//int2char(key)//' [label='//int2char(key)//',color=red] ;',char(13)
         else
            write(stream) '    '//int2char(key)//' [label='//int2char(key)//',color=black] ;',char(13)
         endif
         write(stream) '    null'//int2char(1)//' [shape=point];',char(13)
         write(stream) '    '//int2char(key)//' -> null'//int2char(1)//';',char(13)
         write(stream) '    null'//int2char(2)//' [shape=point];',char(13)
         write(stream) '    '//int2char(key)//' -> null'//int2char(2)//';',char(13)
      else
         call rec_print_tree_graphviz(this%rbtree_node_ref_t , stream)
      endif
      write(stream) '}', char(13)
      close(stream)
      ! convert to postscript
      call execute_command_line("dot -Tps "//filename//".gv"//" -o "//filename//".ps",exitstat=ios)

      ! reset nullcount
      nullcount = 0

   contains
      !-----------------------------------------------------------------------
      !Subroutine rec_print_postorder_tree
      !-----------------------------------------------------------------------
      recursive subroutine rec_print_tree_graphviz(node, stream)
         implicit none
         type(rbtree_node_ref_t), intent(in) :: node
         integer, intent(in) :: stream
         integer :: key, key_left, key_right

         key = get_key(node)

         if(get_colour(node)  == RED) then
            write(stream) '    '//int2char(key)//' [label='//int2char(key)//',color=red] ;',char(13)
         else
            write(stream) '    '//int2char(key)//' [label='//int2char(key)//',color=black] ;',char(13)
         endif

         if(hasleft(node)) then
            key_left = get_key(get_left(node))
            write(stream) '    '//int2char(key)//' -> '//int2char(key_left)//';',char(13)
            call rec_print_tree_graphviz(get_left(node) , stream)
         else
            nullcount = nullcount + 1
            write(stream) '    null'//int2char(nullcount)//' [shape=point];',char(13)
            write(stream) '    '//int2char(key)//' -> null'//int2char(nullcount)//';',char(13)
         endif


         if(hasright(node)) then
            key_right = get_key(get_right(node))
            write(stream) '    '//int2char(key)//' -> '//int2char(key_right)//';',char(13)
            call rec_print_tree_graphviz(get_right(node) , stream)
         else
            nullcount = nullcount + 1
            write(stream) '    null'//int2char(nullcount)//' [shape=point];',char(13)
            write(stream) '    '//int2char(key)//' -> null'//int2char(nullcount)//';',char(13)
         endif
      end subroutine rec_print_tree_graphviz

   end subroutine export_to_graphviz

end module zmi_redblacktree_module


!-----------------------------------------------------------------------
!Main program test_rbtree
! test program for red-black tree module
!-----------------------------------------------------------------------
program test_rbtree
   use zmi_redblacktree_module
   use zmi_utils_module
   implicit none
   integer, parameter :: dp = kind(1.d0)
   integer :: big = 1000000


   call my_test_simple()
   call my_test_perm(big)

contains

   !-----------------------------------------------------------------------
   !subroutine my_test_simple
   ! simple test with small red-black tree
   ! export tree and deletion to graphviz and postscript
   !-----------------------------------------------------------------------
   subroutine  my_test_simple()
      implicit none
      type(rbtree_t) :: t
      integer :: i
      integer, parameter :: keys(10) = [10, 9, 2, 3, 1, 4, 6, 8, 5, 7] ! keys

      write(*,*)"    create, populate and print small red-black tree         "
      write(*,*)"____________________________________________________________"


      write(*,*) "t % tree_size = ", t % tree_size

      write(*,*) "populating tree"
      do i=1,10
         call t % insert(keys(i), keys(i)) ! use keys also as values
      end do
      write(*,*)" t % tree_size =", t % tree_size
      write(*,*) "printing tree"
      call t % print_tree()
      write(*,*) "exporting to graphviz"
      call t % export("rbtree_full"//array2char(keys))

      write(*,*) "deleting tree"
      do i = 1, 10
         write(*,*) "delete: key = ", keys(i)
         call t % delete(keys(i))
         call t % export("rbtree_full"//array2char(keys)//"_minus"//array2char(keys(1:i)))
         call t % print_tree()
      end do

      write(*,*) "t % tree_size = ", t % tree_size
   end subroutine my_test_simple

   !-----------------------------------------------------------------------
   !subroutine my_test_perm
   !  create, populate, search and delete randomly large red-black tree
   !-----------------------------------------------------------------------
   subroutine my_test_perm(bigsize)
      implicit none
      integer, intent(in) :: bigsize
      type(rbtree_t) :: t
      integer, dimension(:), allocatable :: keys
      integer :: i

      real :: start, finish

      write(*,*)" create, populate, search and delete large red-black tree   "
      write(*,*)"____________________________________________________________"


      call init_random_seed()

      call cpu_time(start)
      write(*,*) "allocate keys"
      allocate(keys(bigsize))
      call cpu_time(finish)
      write(*,*) "time = ",finish-start, " seconds"



      call cpu_time(start)
      write(*,*) "generate keys"
      do i = 1, bigsize
         keys(i) = i !get_random_pos(bigsize*10)
      enddo
      call shuffle_integer_array(keys)
      call cpu_time(finish)
      write(*,*) "time = ",finish-start, " seconds"



      call cpu_time(start)
      write(*,*) "populating red-black tree"
      do i=1, bigsize
         if(t % includes(keys(i)))  error stop "duplicate !"
         call t % insert(keys(i), keys(i)) ! use keys also as values
         write(*,'(1x,a20,f6.2,a1,a1,$)') "progress= ", 100.0*real(i)/real(bigsize),"%",char(13)
      enddo
      call cpu_time(finish)
      write(*,*) "time = ",finish-start, " seconds"

      ! now shuffle keys before searching
      call shuffle_integer_array(keys)


      call cpu_time(start)
      write(*,*) "searching keys and querying values in red-black tree"
      do i=1, bigsize

         if(t % includes(keys(i))) then
            if (t % query_value(keys(i)) /= keys(i)) then
               error stop "value <=> key inconsistancy"
            endif
         else
            stop "key not found"
         endif
         write(*,'(1x,a20,f6.2,a1,a1,$)') "progress= ", 100.0*real(i)/real(bigsize),"%",char(13)
      enddo
      call cpu_time(finish)
      write(*,*) "time = ",finish-start, " seconds"

      write(*,*) "tree_size = ", t % tree_size

      ! shuffle again before deletion
      call shuffle_integer_array(keys)


      call cpu_time(start)
      write(*,*) "deleting randomly red-black tree"
      do i = 1, bigsize
         call t % delete(keys(i))
         write(*,'(1x,a20,f6.2,a1,a1,$)') "progress= ", 100.0*real(i)/real(bigsize),"%",char(13)
      enddo
      call cpu_time(finish)
      write(*,*) "time = ",finish-start, " seconds"

      write(*,*) "tree_size = ", t % tree_size

      deallocate(keys)

   end subroutine my_test_perm


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


end program test_rbtree
