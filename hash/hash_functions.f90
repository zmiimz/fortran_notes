!Copyright (c) 2015 M. Zapukhlyak
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!-----------------------------------------------------------------------
!Module string_utils_module
!-----------------------------------------------------------------------
module string_utils_module
   implicit none

contains

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

   !-----------------------------------------------------------------------
   !Function character to integer
   !-----------------------------------------------------------------------
   function char2int(c) result (i)
      implicit none
      character (*), intent (in) :: c
      integer :: i
      character (20) :: ifmt
      write (ifmt,'("(i",i0,")")') len_trim(c)
      read (c,fmt=ifmt) i
   end function char2int

   pure function c_to_f_string(c_string) result(f_string)
      use, intrinsic :: iso_c_binding, only: c_char, c_null_char
      character(kind=c_char,len=1), intent(in) :: c_string(:)
      character(len=:), allocatable :: f_string
      integer i, n
      i = 1
      do
         if (c_string(i) == c_null_char) exit
         i = i + 1
      end do
      n = i - 1  ! exclude c_null_char
      allocate(character(len=n) :: f_string)
      f_string = transfer(c_string(1:n), f_string)
   end function c_to_f_string

   pure function f_to_c_string (f_string) result (c_string)
      use, intrinsic :: iso_c_binding, only: c_char, c_null_char
      implicit none
      character(len=*), intent(in) :: f_string
      character(len=1,kind=c_char), dimension(len_trim(f_string)+1) :: c_string
      integer :: n, i

      n = len_trim(f_string)
      do i = 1, n
         c_string(i) = f_string(i:i)
      end do
      c_string(n + 1) = c_null_char

   end function f_to_c_string

end module string_utils_module


!-----------------------------------------------------------------------
!Module integer_utils_module
!-----------------------------------------------------------------------
module integer_utils_module
   use iso_fortran_env
   implicit none

   private ! all
   public :: int_to_uint, uint_to_int

   interface int_to_uint
      module procedure int32_to_int64
      module procedure int64_to_int64
   end interface int_to_uint

   interface uint_to_int
      module procedure int64_to_int32
#if defined(__GFORTRAN__)
      module procedure int128_to_int32
#endif
      module procedure int32_to_int32
   end interface uint_to_int

contains

   !-----------------------------------------------------------------------
   !Function
   !-----------------------------------------------------------------------
   pure function  int32_to_int32(num)
      implicit none
      integer(int32), intent(in) :: num
      integer(int32) :: int32_to_int32
      integer(int64), parameter :: UINT32_MAX = z'ffffffff'
      int32_to_int32 =  num - UINT32_MAX - 1
   end function int32_to_int32

   !-----------------------------------------------------------------------
   !Function
   !-----------------------------------------------------------------------
   pure function  int64_to_int32(num)
      implicit none
      integer(int64), intent(in) :: num
      integer(int32) :: int64_to_int32
      integer(int64), parameter :: UINT32_MAX = z'ffffffff'
      int64_to_int32 =  num - UINT32_MAX - 1
   end function int64_to_int32

#if defined(__GFORTRAN__)
   !-----------------------------------------------------------------------
   !Function
   !-----------------------------------------------------------------------
   pure function  int128_to_int32(num)
      implicit none
      integer(kind=16), intent(in) :: num
      integer(int32) :: int128_to_int32
      integer(int64), parameter :: UINT32_MAX = z'ffffffff'
      int128_to_int32 =  num - UINT32_MAX - 1
   end function int128_to_int32
#endif
   !-----------------------------------------------------------------------
   !Function
   !-----------------------------------------------------------------------
   pure function  int32_to_int64(num)
      implicit none
      integer(int32), intent(in) :: num
      integer(int64) :: int32_to_int64
      integer(int64), parameter :: UINT32_MAX = z'ffffffff'
      int32_to_int64 = int(num, kind=int64)
      if(num < 0) int32_to_int64 = UINT32_MAX + 1 + int(num, kind=int64)
   end function int32_to_int64

   !-----------------------------------------------------------------------
   !Function
   !-----------------------------------------------------------------------
   pure function  int64_to_int64(num)
      implicit none
      integer(int64), intent(in) :: num
      integer(int64) :: int64_to_int64
      integer(int64), parameter :: UINT32_MAX = z'ffffffff'
      int64_to_int64 = int(num, kind=int64)
      if(num < 0) int64_to_int64 = UINT32_MAX + 1 + int(num, kind=int64)
   end function int64_to_int64

end module integer_utils_module

!-----------------------------------------------------------------------
! hash_functions_library_module contains many popular hash functions implemented in fortran
! all hash functions are compatible with C version
! there is no unsigned int type in fortran and we are forced to use int64 integers
! in order to emulate uint32 range of C
! This code inspired by http://www.partow.net/programming/hashfunctions
!-----------------------------------------------------------------------
module hash_functions_library_module
   use iso_c_binding
   use iso_fortran_env
   use string_utils_module
   use integer_utils_module
   implicit none

contains

   !-----------------------------------------------------------------------
   !Function elf_hash_fortran
   ! published hash algorithm used in the UNIX ELF format for object files.
   !-----------------------------------------------------------------------
   function elf_hash_fortran(text) result(hash)
      implicit none
      character(len=*),intent(in) :: text
      character(len=1,kind=c_char), target, dimension(len_trim(text)+1) :: text_c
      integer(int64) :: hash
      integer(int32) :: hash_32, g
      integer(int32) :: i

      text_c = f_to_c_string(text) ! convert to c string for compatibility
      hash_32 = 0_int32

      do i=1,size(text_c)
         hash_32 = shiftl(hash_32,4_int32) + ichar(text_c(i))
         g = iand(hash_32, z'f0000000')
         if(g /= 0_int32) hash_32 = ieor(hash_32, shiftr(g,24_int32))
         hash_32 = iand(hash_32, not(g))
      end do
      hash = int_to_uint(hash_32)
   end function elf_hash_fortran

   !-----------------------------------------------------------------------
   !Function bpreiss_hash_fortran
   ! hash function from Bruno Preiss's book:
   ! Data Structures and Algorithms with Object-Oriented Design Patterns in Java,''
   ! John Wiley & Sons, 2000
   !-----------------------------------------------------------------------
   function bpreiss_hash_fortran(text) result(hash)
      implicit none
      character(len=*),intent(in) :: text
      character(len=1,kind=c_char), target, dimension(len_trim(text)+1) :: text_c
      integer(int64) :: hash
      integer(int32) :: hash_32
      integer(int32) :: i

      text_c = f_to_c_string(text) ! convert to c string for compatibility
      hash_32 = 0_int32

      do i=1,size(text_c)
         hash_32 = ieor(shiftl(hash_32,7_int32),ichar(text_c(i)))
      end do
      hash = int_to_uint(hash_32)
   end function bpreiss_hash_fortran

   !-----------------------------------------------------------------------
   !Function apartow_hash_fortran
   ! AP Hash is an algorithm invented by Arash Partow
   !-----------------------------------------------------------------------
   function apartow_hash_fortran(text) result(hash)
      implicit none
      character(len=*),intent(in) :: text
      character(len=1,kind=c_char), target, dimension(len_trim(text)+1) :: text_c
      integer(int64) :: hash
      integer(int32) :: hash_32
      integer(int32) :: i

      text_c = f_to_c_string(text) ! convert to c string for compatibility
      hash_32 = uint_to_int(int(z'aaaaaaaa'- z'ffffffff' - 1,int64))

      do i=1,size(text_c)
         if(iand(i-1_int32,1_int32) == 0_int32) then
            hash_32 = ieor(hash_32,ieor(shiftl(hash_32,7_int32),ichar(text_c(i)) * shiftr(hash_32,3_int32)))
         else
            hash_32 = ieor(hash_32,not(shiftl(hash_32,11_int32) + ieor(ichar(text_c(i)),shiftr(hash_32,5_int32))))
         endif
      end do
      hash = int_to_uint(hash_32)
   end function apartow_hash_fortran

   !-----------------------------------------------------------------------
   !Function knuth_hash_fortran
   ! The algorithm was presented in Donald E. Knuth "The Art Of Computer Programming"
   ! Volume 3, Chapter 6.4, Topic: Sorting and search.
   !-----------------------------------------------------------------------
   function knuth_hash_fortran(text) result(hash)
      implicit none
      character(len=*),intent(in) :: text
      character(len=1,kind=c_char), target, dimension(len_trim(text)+1) :: text_c
      integer(int64) :: hash
      integer(int32) :: hash_32
      integer(int32) :: i

      text_c = f_to_c_string(text) ! convert to c string for compatibility
      hash_32 = int(size(text_c),int32)

      do i=1,size(text_c)
         hash_32 =   ieor(ieor(shiftl(hash_32,5_int32),shiftr(hash_32,27_int32)),ichar(text_c(i)))
      end do
      hash = int_to_uint(hash_32)
   end function knuth_hash_fortran

   !-----------------------------------------------------------------------
   !Function tmueller_hash_fortran
   ! http://stackoverflow.com/questions/664014/what-integer-hash-function-are-good-that-accepts-an-integer-hash-key
   ! Thomas Mueller
   !-----------------------------------------------------------------------
   function tmueller_hash_fortran(text) result(hash)
      implicit none
      character(len=*),intent(in) :: text
      character(len=1,kind=c_char), target, dimension(len_trim(text)+1) :: text_c
      integer(int64) :: hash
      integer(int32) :: hash_32
      integer(int32) :: i

      text_c = f_to_c_string(text) ! convert to c string for compatibility
      hash_32 = 0_int32
      do i=1,size(text_c)
         hash_32 = hash_32 + ichar(text_c(i))
         hash_32 = ieor(shiftr(hash_32, 16_int32),hash_32) * z'45d9f3b'
         hash_32 = ieor(shiftr(hash_32, 16_int32),hash_32) * z'45d9f3b'
         hash_32 = ieor(shiftr(hash_32, 16_int32),hash_32)
      end do
      hash = int_to_uint(hash_32)
   end function tmueller_hash_fortran

   !-----------------------------------------------------------------------
   !Function sax_hash_fortran
   ! hash function for hashing character strings using shift-add-xor (SAX)
   !-----------------------------------------------------------------------
   function sax_hash_fortran(text) result(hash)
      implicit none
      character(len=*),intent(in) :: text
      character(len=1,kind=c_char), target, dimension(len_trim(text)+1) :: text_c
      integer(int64) :: hash
      integer(int32) :: hash_32
      integer(int32) :: i

      text_c = f_to_c_string(text) ! convert to c string for compatibility
      hash_32 = 0_int32
      do i=1,size(text_c)
         hash_32 = ieor(hash_32, shiftl(hash_32, 5_int32) + shiftr(hash_32, 2_int32) + ichar(text_c(i))) ! hash ^= ( hash << 5 ) + ( hash >> 2 ) + key[i];
      end do
      hash = int_to_uint(hash_32)
   end function sax_hash_fortran


   !-----------------------------------------------------------------------
   !Function djbx_hash to integer
   ! hashing algorithm developed by Daniel J. Bernstein
   !-----------------------------------------------------------------------
   function djbx_hash_fortran(text) result(hash)
      implicit none
      character(len=*),intent(in) :: text
      character(len=1,kind=c_char), target, dimension(len_trim(text)+1) :: text_c
      integer(int64) :: hash
      integer(int32) :: hash_32
      integer(int32) :: i

      text_c = f_to_c_string(text) ! convert to c string for compatibility
      hash_32 = 5381_int32
      do i=1,size(text_c)
         hash_32 = (ishft(hash_32,5_int32) + hash_32) + ichar(text_c(i)) ! hash = 33 * hash + key[i];
      end do
      hash_32 = ieor(hash_32, shiftr(hash_32,16_int32))  ! hash ^ (hash >> 16)
      hash = int_to_uint(hash_32)
   end function djbx_hash_fortran

   !-----------------------------------------------------------------------
   !Function djb_hash to integer
   ! hashing algorithm developed by Daniel J. Bernstein
   ! https://groups.google.com/forum/#!topic/comp.lang.c/lSKWXiuNOAk
   !-----------------------------------------------------------------------
   function djb_hash_fortran(text) result(hash)
      implicit none
      character(len=*),intent(in) :: text
      character(len=1,kind=c_char), target, dimension(len_trim(text)+1) :: text_c
      integer(int64) :: hash
      integer(int32) :: hash_32
      integer(int32) :: i

      text_c = f_to_c_string(text) ! convert to c string for compatibility
      hash_32 = 5381_int32
      do i=1,size(text_c)
         hash_32 = (ishft(hash_32,5_int32) + hash_32) + ichar(text_c(i))
         !             hash = hash*33 + ichar(text(i:i))
      end do
      hash = int_to_uint(hash_32)
   end function djb_hash_fortran

   !-----------------------------------------------------------------------
   ! The djb2_hash_fortran function is ``h = ((h << 5) + h) ^ c'', with a starting
   ! hash of 5381.
   ! hashing algorithm developed by Daniel J. Bernstein
   !-----------------------------------------------------------------------
   function djb2_hash_fortran(text) result(hash)
      implicit none
      character(len=*),intent(in) :: text
      character(len=1,kind=c_char), target, dimension(len_trim(text)+1) :: text_c
      integer(int64) :: hash
      integer(int32) :: hash_32
      integer(int32) :: i

      text_c = f_to_c_string(text) ! convert to c string for compatibility

      hash_32 = 5381_int32
      do i=1,size(text_c)
         !hash = hash * 33 ^ text[i]
         hash_32 = ieor((ishft(hash_32,5) + hash_32), ichar(text_c(i)))
      end do
      hash = int_to_uint(hash_32)
   end function djb2_hash_fortran

   !-----------------------------------------------------------------------
   !Function sdbm_hash_fortran
   ! this algorithm was created for sdbm (a public-domain reimplementation of ndbm) database library.
   !-----------------------------------------------------------------------
   function sdbm_hash_fortran(text) result(hash)
      implicit none
      character(len=*),intent(in) :: text
      character(len=1,kind=c_char), target, dimension(len_trim(text)+1) :: text_c
      integer(int64) :: hash
      integer(int32) :: hash_32
      integer(int32) :: i

      text_c = f_to_c_string(text) ! convert to c string for compatibility
      hash_32 = 0_int32 ! 5381_int32 !
      do i=1,size(text_c)
         hash_32 = ichar(text_c(i)) + shiftl(hash_32,6_int32) + shiftl(hash_32,16_int32) - hash_32 !  hash = (*str) + (hash << 6) + (hash << 16) - hash;
      end do
      hash = int_to_uint(hash_32)
   end function sdbm_hash_fortran

   !-----------------------------------------------------------------------
   !Function fnv1_32_hash_fortran
   ! http://www.isthe.com/chongo/tech/comp/fnv/
   ! http://stackoverflow.com/questions/34595/what-is-a-good-hash-function
   !-----------------------------------------------------------------------
   function fnv1_32_hash_fortran(text) result(hash)
      implicit none
      character(len=*),intent(in) :: text
      character(len=1,kind=c_char), target, dimension(len_trim(text)+1) :: text_c
      integer(int64) :: hash
      integer(int32) :: hash_32
      integer(int32) :: i
      integer(int32), parameter :: FNV_prime = z'01000193' !  2^24 + 2^8 + 0x93 = 16777619

      text_c = f_to_c_string(text) ! convert to c string for compatibility
      hash_32 = uint_to_int(int(z'811c9dc5'- z'ffffffff' - 1,int64))  ! 2166136261

      do i=1,size(text_c)
         hash_32 = ieor((hash_32 * FNV_prime), ichar(text_c(i))) ! hash = ( hash * 16777619 ) ^ key[i];
      end do
      hash = int_to_uint(hash_32)
   end function fnv1_32_hash_fortran

   !-----------------------------------------------------------------------
   !Function fnv1a_32_hash_fortran
   !-----------------------------------------------------------------------
   function  fnv1a_32_hash_fortran(text) result(hash)
      use iso_c_binding
      use iso_fortran_env
      implicit none
      character(len=*),intent(in) :: text
      character(len=1,kind=c_char), target, dimension(len_trim(text)+1) :: text_c
      integer(int64) :: hash
      integer(int32) :: hash_32
      integer(int32) :: i
      integer(int32), parameter :: FNV_prime = z'01000193' !  2^24 + 2^8 + 0x93 = 16777619

      text_c = f_to_c_string(text) ! convert to c string for compatibility
      hash_32 = uint_to_int(int(z'811c9dc5'- z'ffffffff' - 1,int64))  ! 2166136261

      do i = 1, size(text_c)
         hash_32 = ieor(hash_32, iachar(text_c(i))) * FNV_prime
      enddo
      hash = int_to_uint(hash_32)
   end function fnv1a_32_hash_fortran


   !-----------------------------------------------------------------------
   !Function jenkins_oat_hash
   !  Bob Jenkins's one-at-a-time hash an expanded version of his Dr. Dobbs article.
   !http://www.burtleburtle.net/bob/hash/doobs.html
   !-----------------------------------------------------------------------
   function  jenkins_oat_hash_fortran(text) result(hash)
      use iso_c_binding
      use iso_fortran_env
      implicit none
      character (len=*), target, intent(in) :: text
      character(len=1,kind=c_char), target, dimension(len_trim(text)+1) :: text_c
      integer(int64) :: hash ! unsigned 32-bit integers assumed (int64 used for shifted region)
      integer :: i
      integer(int32) :: hash_32

      text_c = f_to_c_string(text) ! convert to c string for compatibility
      hash_32 = 0
      do i = 1, size(text_c)
         hash_32 = hash_32 + iachar(text_c(i))
         hash_32 = hash_32 + shiftl(hash_32, 10)
         hash_32 = ieor(hash_32, shiftr(hash_32, 6))
      enddo
      hash_32 = hash_32 + shiftl(hash_32, 3)
      hash_32 = ieor(hash_32, shiftr(hash_32, 11))
      hash_32 = hash_32 + shiftl(hash_32, 15)
      hash = int_to_uint(hash_32)
   end function jenkins_oat_hash_fortran

end module hash_functions_library_module


!-----------------------------------------------------------------------
!Module hash_c_functions_library_module
! interface to C hash functions
!-----------------------------------------------------------------------
module hash_c_functions_library_module
   use iso_c_binding
   use iso_fortran_env
   use string_utils_module
   use integer_utils_module
   implicit none

   ! interface to c functions
   interface

      function get_UINT32_MAX() result(UINT32_MAX) bind(c, name="get_UINT32_MAX")
         use iso_c_binding
         integer(c_int64_t) :: UINT32_MAX
      end function

      function jenkins_oat_hash(key, length) result(jenkins_hash) bind(c, name="jenkins_one_at_a_time_hash")
         use iso_c_binding
         character(kind=c_char),dimension(*) :: key

         integer(c_size_t), value :: length
         integer(c_int32_t) :: jenkins_hash
      end function jenkins_oat_hash

      function fnv1a_32_hash(key, length) bind(c, name="fnv1a_32_hash")
         use iso_c_binding
         character(kind=c_char),dimension(*) :: key

         integer(c_size_t), value :: length
         integer(c_int32_t) :: fnv1a_32_hash
      end function fnv1a_32_hash

      function fnv1_32_hash(key, length) bind(c, name="fnv1_32_hash")
         use iso_c_binding
         character(kind=c_char),dimension(*) :: key

         integer(c_size_t), value :: length
         integer(c_int32_t) :: fnv1_32_hash
      end function fnv1_32_hash

      function fnv1a_64_hash(key, length) bind(c, name="fnv1a_64_hash")
         use iso_c_binding
         character(kind=c_char),dimension(*) :: key

         integer(c_size_t), value :: length
         integer(c_int64_t) :: fnv1a_64_hash
      end function fnv1a_64_hash

      function sdbm_hash(key, length) bind(c, name="sdbm_hash")
         use iso_c_binding
         character(kind=c_char),dimension(*) :: key

         integer(c_size_t), value :: length
         integer(c_int32_t) :: sdbm_hash
      end function sdbm_hash

      function djbx_hash(key, length) bind(c, name="djbx_hash")
         use iso_c_binding
         character(kind=c_char),dimension(*) :: key

         integer(c_size_t), value :: length
         integer(c_int32_t) :: djbx_hash
      end function djbx_hash

      function djb_hash(key, length) bind(c, name="djb_hash")
         use iso_c_binding
         character(kind=c_char),dimension(*) :: key

         integer(c_size_t), value :: length
         integer(c_int32_t) :: djb_hash
      end function djb_hash

      function djb2_hash(key, length) bind(c, name="djb2_hash")
         use iso_c_binding
         character(kind=c_char),dimension(*) :: key

         integer(c_size_t), value :: length
         integer(c_int32_t) :: djb2_hash
      end function djb2_hash

      function sax_hash(key, length) bind(c, name="sax_hash")
         use iso_c_binding
         character(kind=c_char),dimension(*) :: key

         integer(c_size_t), value :: length
         integer(c_int32_t) :: sax_hash
      end function sax_hash

      function oat_hash(key, length) bind(c, name="oat_hash")
         use iso_c_binding
         character(kind=c_char),dimension(*) :: key

         integer(c_size_t), value :: length
         integer(c_int32_t) :: oat_hash
      end function oat_hash

      function elf_hash(key, length) bind(c, name="elf_hash")
         use iso_c_binding
         character(kind=c_char),dimension(*) :: key

         integer(c_size_t), value :: length
         integer(c_int32_t) :: elf_hash
      end function elf_hash

      function tmueller_hash(key, length) bind(c, name="tmueller_hash")
         use iso_c_binding
         character(kind=c_char),dimension(*) :: key

         integer(c_size_t), value :: length
         integer(c_int32_t) :: tmueller_hash
      end function tmueller_hash

      function knuth_hash(key, length) bind(c, name="knuth_hash")
         use iso_c_binding
         character(kind=c_char),dimension(*) :: key

         integer(c_size_t), value :: length
         integer(c_int32_t) :: knuth_hash
      end function knuth_hash

      function apartow_hash(key, length) bind(c, name="apartow_hash")
         use iso_c_binding
         character(kind=c_char),dimension(*) :: key

         integer(c_size_t), value :: length
         integer(c_int32_t) :: apartow_hash
      end function apartow_hash

      function bpreiss_hash(key, length) bind(c, name="bpreiss_hash")
         use iso_c_binding
         character(kind=c_char),dimension(*) :: key

         integer(c_size_t), value :: length
         integer(c_int32_t) :: bpreiss_hash
      end function bpreiss_hash

   end interface

   abstract interface
      function function_hash_template(key, length) bind(c)
         use iso_c_binding
         character(kind=c_char),dimension(*) :: key
         integer(c_size_t), value :: length
         integer(c_int32_t) :: function_hash_template
      end function function_hash_template
   end interface

contains

   function jenkins_oat_hash_wrap(text) result(hash)
      implicit none
      character(len=*),intent(in) :: text
      integer(int64) :: hash
      procedure(function_hash_template), pointer :: fun
      fun => jenkins_oat_hash
      hash = hash_wrap(text, fun)
   end function jenkins_oat_hash_wrap

   function fnv1_32_hash_wrap(text) result(hash)
      implicit none
      character(len=*),intent(in) :: text
      integer(int64) :: hash
      procedure(function_hash_template), pointer :: fun
      fun => fnv1_32_hash
      hash = hash_wrap(text, fun)
   end function fnv1_32_hash_wrap

   function fnv1a_32_hash_wrap(text) result(hash)
      implicit none
      character(len=*),intent(in) :: text
      integer(int64) :: hash
      procedure(function_hash_template), pointer :: fun
      fun => fnv1a_32_hash
      hash = hash_wrap(text, fun)
   end function fnv1a_32_hash_wrap

   function sdbm_hash_wrap(text) result(hash)
      implicit none
      character(len=*),intent(in) :: text
      integer(int64) :: hash
      procedure(function_hash_template), pointer :: fun
      fun => sdbm_hash
      hash = hash_wrap(text, fun)
   end function sdbm_hash_wrap

   function djb2_hash_wrap(text) result(hash)
      implicit none
      character(len=*),intent(in) :: text
      integer(int64) :: hash
      procedure(function_hash_template), pointer :: fun
      fun => djb2_hash
      hash = hash_wrap(text, fun)
   end function djb2_hash_wrap

   function djb_hash_wrap(text) result(hash)
      implicit none
      character(len=*),intent(in) :: text
      integer(int64) :: hash
      procedure(function_hash_template), pointer :: fun
      fun => djb_hash
      hash = hash_wrap(text, fun)
   end function djb_hash_wrap

   function djbx_hash_wrap(text) result(hash)
      implicit none
      character(len=*),intent(in) :: text
      integer(int64) :: hash
      procedure(function_hash_template), pointer :: fun
      fun => djbx_hash
      hash = hash_wrap(text, fun)
   end function djbx_hash_wrap

   function sax_hash_wrap(text) result(hash)
      implicit none
      character(len=*),intent(in) :: text
      integer(int64) :: hash
      procedure(function_hash_template), pointer :: fun
      fun => sax_hash
      hash = hash_wrap(text, fun)
   end function sax_hash_wrap

   function elf_hash_wrap(text) result(hash)
      implicit none
      character(len=*),intent(in) :: text
      integer(int64) :: hash
      procedure(function_hash_template), pointer :: fun
      fun => elf_hash
      hash = hash_wrap(text, fun)
   end function elf_hash_wrap

   function tmueller_hash_wrap(text) result(hash)
      implicit none
      character(len=*),intent(in) :: text
      integer(int64) :: hash
      procedure(function_hash_template), pointer :: fun
      fun => tmueller_hash
      hash = hash_wrap(text, fun)
   end function tmueller_hash_wrap

   function knuth_hash_wrap(text) result(hash)
      implicit none
      character(len=*),intent(in) :: text
      integer(int64) :: hash
      procedure(function_hash_template), pointer :: fun
      fun => knuth_hash
      hash = hash_wrap(text, fun)
   end function knuth_hash_wrap

   function apartow_hash_wrap(text) result(hash)
      implicit none
      character(len=*),intent(in) :: text
      integer(int64) :: hash
      procedure(function_hash_template), pointer :: fun
      fun => apartow_hash
      hash = hash_wrap(text, fun)
   end function apartow_hash_wrap

   function bpreiss_hash_wrap(text) result(hash)
      implicit none
      character(len=*),intent(in) :: text
      integer(int64) :: hash
      procedure(function_hash_template), pointer :: fun
      fun => bpreiss_hash
      hash = hash_wrap(text, fun)
   end function bpreiss_hash_wrap

   !-----------------------------------------------------------------------
   !Function
   !-----------------------------------------------------------------------
   function  hash_wrap(text, fun) result(hash)
      implicit none
      character (len=*), target, intent(in) :: text
      procedure(function_hash_template), pointer :: fun
      integer(c_int64_t) :: hash
      character(kind=c_char), dimension(len_trim(text)+1) :: text_c
      integer(c_int32_t) :: hash_c_32
      integer(c_size_t) :: length

      text_c = f_to_c_string(text) ! convert to c string for compatibility
      length = len_trim(text) + 1
      hash_c_32 = fun(text_c,length)
      hash = int_to_uint(hash_c_32)

   end function hash_wrap

end module hash_c_functions_library_module

!-----------------------------------------------------------------------
!Main program test
!-----------------------------------------------------------------------
program    test
   use iso_fortran_env
   use hash_functions_library_module
   use hash_c_functions_library_module
   use string_utils_module

   implicit none
   integer :: i,j
   integer(int64) :: k
   integer(int64) :: elements
   integer, parameter :: hash_count = 12
   integer(int64), dimension(:), allocatable :: a,b
   character(len=24), dimension(:), allocatable :: hash_name
   character(len=:),allocatable :: hash

   allocate(a(hash_count))
   allocate(b(hash_count))
   allocate(hash_name(hash_count))
   elements = 10000000

   hash_name(1) = "jenkins_oat_hash"
   hash_name(2) = "fnv1a_32_hash"
   hash_name(3) = "fnv1_32_hash"
   hash_name(4) = "djbx_hash"
   hash_name(5) = "djb2_hash"
   hash_name(6) = "sax_hash"
   hash_name(7) = "sdbm_hash"
   hash_name(8) = "tmueller_hash"
   hash_name(9) = "knuth_hash"
   hash_name(10) = "apartow_hash"
   hash_name(11) = "bpreiss_hash"
   hash_name(12) = "elf_hash"

   do i = 1,elements
      write(*,'(1x,A10,I0,A30,f6.2,A1,A1,$)') "i = ", i, "current progress= ", 100.0*i/dble(elements),"%",char(13)
      ! test integers
      hash = int2char(i)
      !! or random strings
      ! hash = repeat("A",40) ! allocate
      ! call random_word(hash)
      !! write(*,*) hash
      
      do j = 1, hash_count
         select case (j)
          case (1)
            a(j) = jenkins_oat_hash_fortran(hash)
            b(j) = jenkins_oat_hash_wrap(hash)
          case (2)
            a(j) = fnv1a_32_hash_fortran(hash)
            b(j) = fnv1a_32_hash_wrap(hash)
          case (3)
            a(j) = fnv1_32_hash_fortran(hash)
            b(j) = fnv1_32_hash_wrap(hash)
          case (4)
            a(j) = djbx_hash_fortran(hash)
            b(j) = djbx_hash_wrap(hash)
          case (5)
            a(j) = djb2_hash_fortran(hash)
            b(j) = djb2_hash_wrap(hash)
          case (6)
            a(j) = sax_hash_fortran(hash)
            b(j) = sax_hash_wrap(hash)
          case (7)
            a(j) = sdbm_hash_fortran(hash)
            b(j) = sdbm_hash_wrap(hash)
          case (8)
            a(j) = tmueller_hash_fortran(hash)
            b(j) = tmueller_hash_wrap(hash)
          case (9)
            a(j) = knuth_hash_fortran(hash)
            b(j) = knuth_hash_wrap(hash)
          case (10)
            a(j) = apartow_hash_fortran(hash)
            b(j) = apartow_hash_wrap(hash)
          case (11)
            a(j) = bpreiss_hash_fortran(hash)
            b(j) = bpreiss_hash_wrap(hash)
          case (12)
            a(j) = elf_hash_fortran(hash)
            b(j) = elf_hash_wrap(hash)
          case default
         end select

         if( (a(j) /= b(j)) .or. (a(j) < 0) .or. (b(j) < 0)) then
            write(*,*) "i = ",i, " j = ",j ," a = ", a(j), " b = ", b(j), hash_name(j)
            write(*,*) hash
            stop
         endif
      enddo

      deallocate(hash)
   enddo

   deallocate(a)
   deallocate(b)
   deallocate(hash_name)
   write(*,*) "done!"

contains

   !-----------------------------------------------------------------------
   !Subroutine random_word
   ! fill a string with random sequence of letters
   ! http://flibs.sourceforge.net/
   ! Arjen Markus
   !-----------------------------------------------------------------------
   subroutine random_word( f_string )
      implicit none
      character(len=*):: f_string
      integer :: i,k,l
      real :: r
      character(len=26), parameter :: lower = 'abcdefghijklmnopqrstuvwxyz'
      f_string = ' '
      l = len(f_string)
      call random_number( r )
      l = 2 + (l-2) * r
      do i = 1,l
         call random_number( r )
         k = 1 + 26 * r
         if ( k > 26 ) k = 1
         f_string(i:i) = lower(k:k)
      enddo
   end subroutine random_word

end program test
