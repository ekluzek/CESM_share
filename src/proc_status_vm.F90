module proc_status_vm
  use iso_c_binding
  implicit none
  private
  public :: VmStatusInfo, get_vm_status, prt_vm_status
  public :: log_addr, get_smaps_status
  public :: shr_malloc_trim
  public :: shr_count_malloc
  integer, parameter :: max_line = 256

  type :: VmStatusInfo
    integer :: VmPeak = -1
    integer :: VmSize = -1
    integer :: VmLck  = -1
    integer :: VmPin  = -1
    integer :: VmHWM  = -1
    integer :: VmRSS  = -1
    integer :: VmData = -1
    integer :: VmStk  = -1
    integer :: VmExe  = -1
    integer :: VmLib  = -1
    integer :: VmPTE  = -1
    integer :: VmPMD  = -1
    integer :: VmSwap = -1
    integer :: HugetlbPages = -1
  end type

contains

subroutine shr_malloc_trim()
   interface
     subroutine malloc_trim(pad) bind(C, name="malloc_trim")
       use iso_c_binding
       integer(c_int), value :: pad
     end subroutine malloc_trim
   end interface
   call malloc_trim(0) 

end subroutine shr_malloc_trim

#if 0
subroutine jemalloc_print_stats
    interface
        subroutine malloc_stats_print(write_cb, cbopaque, opts) bind(C, name="malloc_stats_print")
            import :: c_ptr
            type(c_ptr), value :: write_cb
            type(c_ptr), value :: cbopaque
            type(c_ptr), value :: opts
        end subroutine
    end interface

    call malloc_stats_print(c_null_ptr, c_null_ptr, c_null_ptr)
end subroutine jemalloc_print_stats
#endif

subroutine shr_count_malloc(msg)
  character(len=*), intent(in) :: msg

#ifdef __NVCOMPILER__
  write (*,*) 'DBG REGION '//trim(msg)//':' 
  call __pgf90_dbg_print_allo()
#endif

end subroutine shr_count_malloc

subroutine log_addr(msg, addr, file, line)
  character(len=*), intent(in) :: msg, file
  integer, intent(in)          :: line
  integer(kind=8), intent(in)  :: addr
  character(len=20) :: line_str
  character(len=256) :: loc_str

  ! Convert line number to string
  write(line_str, '(I0)') line

  ! Construct location string
  loc_str = ' [' // trim(file) // ':' // trim(line_str) // ']'

  ! Log output
  write(*,'(A,A,A,Z16.16)') trim(msg), trim(loc_str), '  0x', addr
end subroutine log_addr

subroutine get_vm_status(info)
  use iso_fortran_env, only: int32
  type(VmStatusInfo), intent(out) :: info
  character(len=max_line) :: line, tag
  integer(int32) :: val, ios, colon_pos
  character(len=16) :: value_str

  ! Init all fields to -1
  info = VmStatusInfo()

  open(unit=89, file="/proc/self/status", status="old", action="read")
  do
    read(89, '(A)', iostat=ios) line
    if (ios /= 0) exit

    if (index(line, 'Vm') == 1) then
      colon_pos = index(line, ':')
      !print *,'colon_pos: ',colon_pos
      if (colon_pos > 0) then
        tag = adjustl(line(1:colon_pos-1))
        ! Extract value as a string and parse it
        value_str = adjustl(scan_int_part(line(colon_pos+1:)))
        !print *,'value_str: ',value_str
        read(value_str, *, iostat=ios) val
        !print *,'val: ',val
        if (ios /= 0) cycle

        select case (trim(tag))
        case ("VmPeak")         ; info%VmPeak = val
        case ("VmSize")         ; info%VmSize = val
        case ("VmLck")          ; info%VmLck  = val
        case ("VmPin")          ; info%VmPin  = val
        case ("VmHWM")          ; info%VmHWM  = val
        case ("VmRSS")          ; info%VmRSS  = val
        case ("VmData")         ; info%VmData = val
        case ("VmStk")          ; info%VmStk  = val
        case ("VmExe")          ; info%VmExe  = val
        case ("VmLib")          ; info%VmLib  = val
        case ("VmPTE")          ; info%VmPTE  = val
        case ("VmPMD")          ; info%VmPMD  = val
        case ("VmSwap")         ; info%VmSwap = val
        case ("HugetlbPages")   ; info%HugetlbPages = val
        end select
      end if
    end if
  end do
  close(89)
end subroutine get_vm_status

subroutine get_smaps_status()

  integer, parameter :: maxlen = 512
  character(len=maxlen) :: line
  character(len=32) :: label
  integer :: ios, size_kb, rss_kb
  real :: usage
  integer :: pos1, pos2
  character(len=64) :: addr_range
  character(len=maxlen) :: region_info
  real, parameter :: threshold = 0.75  ! 10% usage threshold
  integer :: szThreshold = 16
  logical :: is_candidate

  open(unit=88, file='/proc/self/smaps', status='old', action='read', iostat=ios)
  if (ios /= 0) then
    print *, "Failed to open /proc/self/smaps"
    stop 1
  end if
  print *,'get_smaps_status: point #1'
  do
    read(88, '(A)', iostat=ios) line
    !print *,'line: (',line,')'
    if (ios /= 0) exit

    ! Check for memory region header (starts with address range)
    if (index(line, '-') > 0 .and. index(line, ' r') > 0) then
      region_info = adjustl(line)
      addr_range = adjustl(line(1:index(line, ' ') - 1))  ! extract address range

      ! Determine if this region is [heap] or anonymous (blank at end)
!      is_candidate = (index(region_info, '[heap]') > 0) .or. (trim(region_info) == addr_range)
      is_candidate = (index(region_info, '[heap]') > 0) .or. &
               (index(region_info, '00:00 0') > 0)


      size_kb = -1
      rss_kb = -1
    end if
    ! print *,line
    if(.not. is_candidate) cycle

    ! Extract Size:
    if (index(line, 'Size:') == 1) then
      read(line, *, iostat=ios) label, size_kb
    end if

    ! Extract Rss:
    if (index(line, 'Rss:') == 1) then
      read(line, *, iostat=ios) label, rss_kb
    end if

    ! Once we have both size and rss, compute and print if low usage
    !print *,'SIZE RSS: ',size_kb, rss_kb
    if (size_kb > 0 .and. rss_kb >= 0) then
      usage = real(rss_kb) / real(size_kb)
      if (usage < threshold .and. size_kb > szThreshold) then
        print '(A,1X,A,1X,F5.1,A,I8,A)', 'Low usage region:', trim(addr_range), 100.0*usage, '% used of ',size_kb,' kB'
      end if
      size_kb = -1
      rss_kb = -1
    end if
  end do

  close(88)

end subroutine get_smaps_status




subroutine prt_vm_status(location)
  character(len=*) location
  type(VmStatusInfo) :: meminfo
  character(len=10) :: vmPeak_c,vmSize_c,vmLck_c,vmPin_c, &
        vmHWM_c, vmRSS_c,vmData_c,vmStk_c,vmExe_c,vmLib_c, &
        vmPTE_c,vmPMD_c,vmSwap_c

  call get_vm_status(meminfo)


  write(vmPeak_c,'(I10)') meminfo%VmPeak
  write(vmSize_c,'(I10)') meminfo%VmSize
  write(vmLck_c,'(I10)') meminfo%VmLck
  write(vmPin_c,'(I10)') meminfo%VmPin
  write(vmHWM_c,'(I10)') meminfo%VmHWM
  write(vmRSS_c,'(I10)') meminfo%VmRSS
  write(vmData_c,'(I10)') meminfo%VmData
  write(vmStk_c,'(I10)') meminfo%VmStk
  write(vmExe_c,'(I10)') meminfo%VmExe
  write(vmLib_c,'(I10)') meminfo%VmLib
  write(vmPTE_c,'(I10)') meminfo%VmPTE
  write(vmPMD_c,'(I10)') meminfo%VmPMD
  write(vmSwap_c,'(I10)') meminfo%VmSwap

  print '(13A10)', "VmPeak",  "VmSize",  "VmLck",  "VmPin", &
          "VmHWM","VmRSS","VmData","VmStk","VmExe","VmLib", &
          "VmPTE","vmPMD","VmSwap"
  ! Print values (space-separated, matching order)
  print '(13A10,1x,A)', VmPeak_c,VmSize_c,VmLck_c,VmPin_c, &
          VmHWM_c,VmRSS_c,VmData_c,VmStk_c,VmExe_c,VmLib_c, &
          VmPTE_c,vmPMD_c,VmSwap_c,"[VmStatus] " // trim(location)

end subroutine prt_vm_status 

pure function scan_int_part(str) result(numstr)
  character(len=*), intent(in) :: str
  character(len=16) :: numstr
  integer :: i, j

  numstr = ""
  j = 1
  do i = 1, len_trim(str)
    if (str(i:i) >= '0' .and. str(i:i) <= '9') then
      numstr(j:j) = str(i:i)
      j = j + 1
      if (j > len(numstr)) exit
    else if (j > 1) then
      ! we already started collecting digits and hit non-digit => stop
      exit
    end if
  end do
end function

end module 

