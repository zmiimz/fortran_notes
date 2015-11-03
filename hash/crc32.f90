!Copyright (c) 2015 M. Zapukhlyak
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!-----------------------------------------------------------------------
!Module crc_module  for CRC-32, IEEE 802 computation
! This implementation inspired by C version in "Reversing CRC – Theory and Practice."
! HU Berlin Public Report SAR-PR-2006-05, May 2006,
! Authors: Martin Stigge, Henryk Plötz, Wolf Müller, Jens-Peter Redlich
! https://sar.informatik.hu-berlin.de/research/publications/SAR-PR-2006-05/SAR-PR-2006-05_.pdf
!
! gfortran -cpp crc32.f90 -o crc32.x
! ifort -fpp crc32.f90 -o crc32.x
! pgfortran -Mpreprocess crc32.f90 -o crc32.x
! nagfor -fpp crc32.f90 -o crc32.x
!-----------------------------------------------------------------------
module crc_module
#if defined(__GFORTRAN__) || defined(NAGFOR)
   use,intrinsic :: iso_fortran_env, only: int64
#endif
   implicit none
   private ! all by default
   public :: crc_hash
#if defined(__INTEL_COMPILER) || defined(__PGI)
   integer, parameter :: int64 = selected_int_kind (8)
#endif
   type crc_hash
      integer(int64), dimension(256) :: table
      integer(int64), dimension(256) :: table_orig
      logical :: have_table = .false.
      integer(int64) :: crcpoly = int(z'edb88320',int64)
      integer(int64) :: crcinv = int(z'5b358fd3',int64)
      integer(int64) :: initxor = int(z'ffffffff',int64)
      integer(int64) :: finalxor = int(z'ffffffff',int64)
   contains
      procedure, pass :: init => init_crc32
      procedure, pass :: crc32 => calc_crc32
   end type

contains

   !-----------------------------------------------------------------------
   !subroutine
   !-----------------------------------------------------------------------
   subroutine  init_crc32(this) ! bind(c, name='init_crc32')
      implicit none
      class(crc_hash), intent(inout) :: this
      integer(int64) :: rem
      integer :: i,j

      if (.not. this % have_table) then
         !Calculate CRC table
         do i = 1, 256
            rem = int(i-1,int64);  !/* remainder from polynomial division */
            do j=1,8
               if( iand(rem, 1_int64) /= 0_int64) then
                  rem = ieor(ishft(rem, -abs(1_int64)), this % crcpoly)
               else
                  rem = ishft(rem, -abs(1_int64))
               endif
            enddo
            this % table(i) = rem
         enddo
         ! for check only
         this % table_orig = [&
         int(z'00000000',int64), int(z'77073096',int64), int(z'EE0E612C',int64), int(z'990951BA',int64), &
         int(z'076DC419',int64), int(z'706AF48F',int64), int(z'E963A535',int64), int(z'9E6495A3',int64), &
         int(z'0EDB8832',int64), int(z'79DCB8A4',int64), int(z'E0D5E91E',int64), int(z'97D2D988',int64), &
         int(z'09B64C2B',int64), int(z'7EB17CBD',int64), int(z'E7B82D07',int64), int(z'90BF1D91',int64), &
         int(z'1DB71064',int64), int(z'6AB020F2',int64), int(z'F3B97148',int64), int(z'84BE41DE',int64), &
         int(z'1ADAD47D',int64), int(z'6DDDE4EB',int64), int(z'F4D4B551',int64), int(z'83D385C7',int64), &
         int(z'136C9856',int64), int(z'646BA8C0',int64), int(z'FD62F97A',int64), int(z'8A65C9EC',int64), &
         int(z'14015C4F',int64), int(z'63066CD9',int64), int(z'FA0F3D63',int64), int(z'8D080DF5',int64), &
         int(z'3B6E20C8',int64), int(z'4C69105E',int64), int(z'D56041E4',int64), int(z'A2677172',int64), &
         int(z'3C03E4D1',int64), int(z'4B04D447',int64), int(z'D20D85FD',int64), int(z'A50AB56B',int64), &
         int(z'35B5A8FA',int64), int(z'42B2986C',int64), int(z'DBBBC9D6',int64), int(z'ACBCF940',int64), &
         int(z'32D86CE3',int64), int(z'45DF5C75',int64), int(z'DCD60DCF',int64), int(z'ABD13D59',int64), &
         int(z'26D930AC',int64), int(z'51DE003A',int64), int(z'C8D75180',int64), int(z'BFD06116',int64), &
         int(z'21B4F4B5',int64), int(z'56B3C423',int64), int(z'CFBA9599',int64), int(z'B8BDA50F',int64), &
         int(z'2802B89E',int64), int(z'5F058808',int64), int(z'C60CD9B2',int64), int(z'B10BE924',int64), &
         int(z'2F6F7C87',int64), int(z'58684C11',int64), int(z'C1611DAB',int64), int(z'B6662D3D',int64), &
         int(z'76DC4190',int64), int(z'01DB7106',int64), int(z'98D220BC',int64), int(z'EFD5102A',int64), &
         int(z'71B18589',int64), int(z'06B6B51F',int64), int(z'9FBFE4A5',int64), int(z'E8B8D433',int64), &
         int(z'7807C9A2',int64), int(z'0F00F934',int64), int(z'9609A88E',int64), int(z'E10E9818',int64), &
         int(z'7F6A0DBB',int64), int(z'086D3D2D',int64), int(z'91646C97',int64), int(z'E6635C01',int64), &
         int(z'6B6B51F4',int64), int(z'1C6C6162',int64), int(z'856530D8',int64), int(z'F262004E',int64), &
         int(z'6C0695ED',int64), int(z'1B01A57B',int64), int(z'8208F4C1',int64), int(z'F50FC457',int64), &
         int(z'65B0D9C6',int64), int(z'12B7E950',int64), int(z'8BBEB8EA',int64), int(z'FCB9887C',int64), &
         int(z'62DD1DDF',int64), int(z'15DA2D49',int64), int(z'8CD37CF3',int64), int(z'FBD44C65',int64), &
         int(z'4DB26158',int64), int(z'3AB551CE',int64), int(z'A3BC0074',int64), int(z'D4BB30E2',int64), &
         int(z'4ADFA541',int64), int(z'3DD895D7',int64), int(z'A4D1C46D',int64), int(z'D3D6F4FB',int64), &
         int(z'4369E96A',int64), int(z'346ED9FC',int64), int(z'AD678846',int64), int(z'DA60B8D0',int64), &
         int(z'44042D73',int64), int(z'33031DE5',int64), int(z'AA0A4C5F',int64), int(z'DD0D7CC9',int64), &
         int(z'5005713C',int64), int(z'270241AA',int64), int(z'BE0B1010',int64), int(z'C90C2086',int64), &
         int(z'5768B525',int64), int(z'206F85B3',int64), int(z'B966D409',int64), int(z'CE61E49F',int64), &
         int(z'5EDEF90E',int64), int(z'29D9C998',int64), int(z'B0D09822',int64), int(z'C7D7A8B4',int64), &
         int(z'59B33D17',int64), int(z'2EB40D81',int64), int(z'B7BD5C3B',int64), int(z'C0BA6CAD',int64), &
         int(z'EDB88320',int64), int(z'9ABFB3B6',int64), int(z'03B6E20C',int64), int(z'74B1D29A',int64), &
         int(z'EAD54739',int64), int(z'9DD277AF',int64), int(z'04DB2615',int64), int(z'73DC1683',int64), &
         int(z'E3630B12',int64), int(z'94643B84',int64), int(z'0D6D6A3E',int64), int(z'7A6A5AA8',int64), &
         int(z'E40ECF0B',int64), int(z'9309FF9D',int64), int(z'0A00AE27',int64), int(z'7D079EB1',int64), &
         int(z'F00F9344',int64), int(z'8708A3D2',int64), int(z'1E01F268',int64), int(z'6906C2FE',int64), &
         int(z'F762575D',int64), int(z'806567CB',int64), int(z'196C3671',int64), int(z'6E6B06E7',int64), &
         int(z'FED41B76',int64), int(z'89D32BE0',int64), int(z'10DA7A5A',int64), int(z'67DD4ACC',int64), &
         int(z'F9B9DF6F',int64), int(z'8EBEEFF9',int64), int(z'17B7BE43',int64), int(z'60B08ED5',int64), &
         int(z'D6D6A3E8',int64), int(z'A1D1937E',int64), int(z'38D8C2C4',int64), int(z'4FDFF252',int64), &
         int(z'D1BB67F1',int64), int(z'A6BC5767',int64), int(z'3FB506DD',int64), int(z'48B2364B',int64), &
         int(z'D80D2BDA',int64), int(z'AF0A1B4C',int64), int(z'36034AF6',int64), int(z'41047A60',int64), &
         int(z'DF60EFC3',int64), int(z'A867DF55',int64), int(z'316E8EEF',int64), int(z'4669BE79',int64), &
         int(z'CB61B38C',int64), int(z'BC66831A',int64), int(z'256FD2A0',int64), int(z'5268E236',int64), &
         int(z'CC0C7795',int64), int(z'BB0B4703',int64), int(z'220216B9',int64), int(z'5505262F',int64), &
         int(z'C5BA3BBE',int64), int(z'B2BD0B28',int64), int(z'2BB45A92',int64), int(z'5CB36A04',int64), &
         int(z'C2D7FFA7',int64), int(z'B5D0CF31',int64), int(z'2CD99E8B',int64), int(z'5BDEAE1D',int64), &
         int(z'9B64C2B0',int64), int(z'EC63F226',int64), int(z'756AA39C',int64), int(z'026D930A',int64), &
         int(z'9C0906A9',int64), int(z'EB0E363F',int64), int(z'72076785',int64), int(z'05005713',int64), &
         int(z'95BF4A82',int64), int(z'E2B87A14',int64), int(z'7BB12BAE',int64), int(z'0CB61B38',int64), &
         int(z'92D28E9B',int64), int(z'E5D5BE0D',int64), int(z'7CDCEFB7',int64), int(z'0BDBDF21',int64), &
         int(z'86D3D2D4',int64), int(z'F1D4E242',int64), int(z'68DDB3F8',int64), int(z'1FDA836E',int64), &
         int(z'81BE16CD',int64), int(z'F6B9265B',int64), int(z'6FB077E1',int64), int(z'18B74777',int64), &
         int(z'88085AE6',int64), int(z'FF0F6A70',int64), int(z'66063BCA',int64), int(z'11010B5C',int64), &
         int(z'8F659EFF',int64), int(z'F862AE69',int64), int(z'616BFFD3',int64), int(z'166CCF45',int64), &
         int(z'A00AE278',int64), int(z'D70DD2EE',int64), int(z'4E048354',int64), int(z'3903B3C2',int64), &
         int(z'A7672661',int64), int(z'D06016F7',int64), int(z'4969474D',int64), int(z'3E6E77DB',int64), &
         int(z'AED16A4A',int64), int(z'D9D65ADC',int64), int(z'40DF0B66',int64), int(z'37D83BF0',int64), &
         int(z'A9BCAE53',int64), int(z'DEBB9EC5',int64), int(z'47B2CF7F',int64), int(z'30B5FFE9',int64), &
         int(z'BDBDF21C',int64), int(z'CABAC28A',int64), int(z'53B39330',int64), int(z'24B4A3A6',int64), &
         int(z'BAD03605',int64), int(z'CDD70693',int64), int(z'54DE5729',int64), int(z'23D967BF',int64), &
         int(z'B3667A2E',int64), int(z'C4614AB8',int64), int(z'5D681B02',int64), int(z'2A6F2B94',int64), &
         int(z'B40BBE37',int64), int(z'C30C8EA1',int64), int(z'5A05DF1B',int64), int(z'2D02EF8D',int64)]

         this % have_table = .true.
      endif

      do i = 1, 256
         !          write(*,'(z16,2X,z16)') this % table(i), this % table_orig(i)
         if(this % table(i) /= this % table_orig(i)) stop "error in table"
      enddo

   end subroutine init_crc32

   !-----------------------------------------------------------------------
   !Function generates a CRC-32 checksum for the ASCII encoded string
   !-----------------------------------------------------------------------
   function  calc_crc32(this, buf)
      implicit none
      class(crc_hash), intent(in) :: this
      character(len=*) :: buf
      integer(int64) :: calc_crc32
      integer :: l
      integer :: i,j
      integer(int64) :: crc
      integer(int64) :: ff = int(z'ff',int64)

      crc = this % initxor
      l = len(buf(:))
      do i = 1, l
         j = int(iand(ieor(crc,int(iachar(buf(i:i)),int64)),ff))
         crc = ieor(ishft(crc, -abs(8_int64)), this % table(j+1))
      enddo
      calc_crc32 = ieor(crc, this % finalxor)
   end function calc_crc32
end module crc_module


!-----------------------------------------------------------------------
!Main program crc32_test
!-----------------------------------------------------------------------
program    crc32_test
#if defined(__GFORTRAN__) || defined(NAGFOR)
   use,intrinsic :: iso_fortran_env, only: int64
#endif
   use crc_module
   implicit none
#if defined(__INTEL_COMPILER) || defined(__PGI)
   integer, parameter :: int64 = selected_int_kind (8)
#endif
   type(crc_hash) :: hash
   character(len=*), parameter :: s = "The quick brown fox jumps over the lazy dog"
   integer :: i
   integer(int64) :: crc
   ! init hash
   call hash % init()
   ! calculate hash from string s
   crc = hash % crc32(s)
   ! print hex of string s
   write(*,'(43z2)') (int(iachar(s(i:i)),int64), i=1,len(s(:)))
   ! print crc32
   write(*,'(z8)') crc
   ! print known crc32 for s
   write(*,'(z8)') z'414fa339'

end program crc32_test
