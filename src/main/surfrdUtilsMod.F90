module surfrdUtilsMod
  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Contains utility methods that can be used when reading surface datasets or similar
  ! datasets (such as the landuse_timeseries dataset)
  !
  ! !USES:
#include "shr_assert.h"
  use shr_kind_mod , only : r8 => shr_kind_r8
  use clm_varctl   , only : iulog
  use abortutils   , only : endrun
  use shr_log_mod  , only : errMsg => shr_log_errMsg
  use spmdMod      , only : masterproc
  !
  ! !PUBLIC TYPES:
  implicit none
  private
  save
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public :: check_sums_equal_1  ! Confirm that sum(arr(n,:)) == 1 for all n
  public :: renormalize         ! Renormalize an array
  public :: convert_cft_to_pft  ! Conversion of crop CFT to natural veg PFT:w
  public :: collapse_crop_types ! Collapse unused crop types into types used in this run

  character(len=*), parameter, private :: sourcefile = &
       __FILE__

  !-----------------------------------------------------------------------

contains
  
  !-----------------------------------------------------------------------
  subroutine check_sums_equal_1(arr, lb, name, caller, ier)
    !
    ! !DESCRIPTION:
    ! Confirm that sum(arr(n,:)) == 1 for all n. If this isn't true for any n, abort with a message.
    !
    ! !ARGUMENTS:
    integer         , intent(in) :: lb           ! lower bound of the first dimension of arr
    real(r8)        , intent(in) :: arr(lb:,:)   ! array to check
    character(len=*), intent(in) :: name         ! name of array
    character(len=*), intent(in) :: caller       ! identifier of caller, for more meaningful error messages
    integer, optional, intent(out):: ier         ! Return an error code rather than abort
    !
    ! !LOCAL VARIABLES:
    logical :: found
    integer :: nl
    integer :: nindx
    real(r8), parameter :: eps = 1.e-13_r8
    !-----------------------------------------------------------------------

    if( present(ier) ) ier = 0
    found = .false.

    do nl = lbound(arr, 1), ubound(arr, 1)
       if (abs(sum(arr(nl,:)) - 1._r8) > eps) then
          found = .true.
          nindx = nl
          exit
       end if
    end do

    if (found) then
       write(iulog,*) trim(caller), ' ERROR: sum of ', trim(name), ' not 1.0 at nl=', nindx
       write(iulog,*) 'sum is: ', sum(arr(nindx,:))
       if( present(ier) ) then
          ier = -10
       else
          call endrun(msg=errMsg(sourcefile, __LINE__))
       end if
    end if

  end subroutine check_sums_equal_1

  !-----------------------------------------------------------------------
  subroutine renormalize(arr, lb, normal)
    !
    ! !DESCRIPTION:
    ! Re normalize an array so that it sums to the input value
    !
    ! !ARGUMENTS:
    integer         , intent(in) :: lb            ! lower bound of the first dimension of arr
    real(r8)        , intent(inout) :: arr(lb:,:) ! array to check
    real(r8)        , intent(in) :: normal        ! normal to sum to
    !
    ! !LOCAL VARIABLES:
    integer :: nl        ! Array index
    real(r8) :: arr_sum  ! sum of array
    real(r8) :: ratio    ! ratio to multiply by
    !-----------------------------------------------------------------------

    do nl = lbound(arr, 1), ubound(arr, 1)
       arr_sum = sum(arr(nl,:))
       if ( arr_sum /= 0.0_r8 )then
          ratio     = normal / arr_sum
          arr(nl,:) = arr(nl,:) * ratio
       end if
    end do

  end subroutine renormalize

!-----------------------------------------------------------------------
  subroutine convert_cft_to_pft( begg, endg, cftsize, wt_cft )
    !
    ! !DESCRIPTION:
    !        Convert generic crop types that were read in as seperate CFT's on
    !        a crop landunit, and put them on the vegetated landunit.
    ! !USES:
    use clm_instur      , only : wt_lunit, wt_nat_patch, fert_cft
    use clm_varpar      , only : cft_size, natpft_size
    use pftconMod       , only : nc3crop
!Edit by Lei Cai--start
    use landunit_varcon , only : istsoil, istsoil_li, istsoil_mi, istsoil_hi, istcrop
!Edit by Lei Cai--end
    ! !ARGUMENTS:
    implicit none
    integer          , intent(in)    :: begg, endg
    integer          , intent(in)    :: cftsize          ! CFT size
    real(r8)         , intent(inout) :: wt_cft(begg:,:)  ! CFT weights
    !
    ! !LOCAL VARIABLES:
    integer :: g    ! index
!-----------------------------------------------------------------------
    SHR_ASSERT_ALL((ubound(wt_cft) == (/endg, cftsize/)), errMsg(sourcefile, __LINE__))
    SHR_ASSERT_ALL((ubound(wt_nat_patch) == (/endg, nc3crop+cftsize-1/)), errMsg(sourcefile, __LINE__))

    do g = begg, endg
       if ( wt_lunit(g,istcrop) > 0.0_r8 )then
          ! Move CFT over to PFT and do weighted average of the crop and soil parts
!Edit by Lei Cai--start
          wt_nat_patch(g,:) = wt_nat_patch(g,:) * (wt_lunit(g,istsoil) + wt_lunit(g,istsoil_li) + &
		                      wt_lunit(g,istsoil_mi) + wt_lunit(g,istsoil_hi))
!Edit by Lei Cai--end
          wt_cft(g,:)       = wt_cft(g,:) * wt_lunit(g,istcrop)
          wt_nat_patch(g,nc3crop:) = wt_cft(g,:)      ! Add crop CFT's to end of natural veg PFT's
!Edit by Lei Cai--start
          wt_lunit(g,istsoil) = (wt_lunit(g,istsoil) + wt_lunit(g,istsoil_li) + wt_lunit(g,istsoil_mi) &
		                         + wt_lunit(g,istsoil_hi) + wt_lunit(g,istcrop)) ! Add crop landunit to soil landunit
!Edit by Lei Cai--end
          wt_nat_patch(g,:)   =  wt_nat_patch(g,:) / wt_lunit(g,istsoil)   !KSA2019: should this include all istsoil alternatives?
          wt_lunit(g,istcrop) = 0.0_r8                ! Zero out crop CFT's
       else
          wt_nat_patch(g,nc3crop:) = 0.0_r8    ! Make sure generic crops are zeroed out
       end if
    end do

  end subroutine convert_cft_to_pft

  !-----------------------------------------------------------------------
  subroutine collapse_crop_types(wt_cft, fert_cft, begg, endg, verbose)
    !
    ! !DESCRIPTION:
    ! Collapse unused crop types into types used in this run.
    !
    ! Should only be called if using prognostic crops - otherwise, wt_cft is meaningless
    !
    ! !USES:
    use clm_varctl , only : irrigate
    use clm_varpar , only : cft_lb, cft_ub, cft_size
    use pftconMod  , only : nc3crop, nc3irrig, npcropmax, pftcon
    !
    ! !ARGUMENTS:

    ! Note that we use begg and endg rather than 'bounds', because bounds may not be
    ! available yet when this is called
    integer, intent(in) :: begg     ! Beginning grid cell index
    integer, intent(in) :: endg     ! Ending grid cell index

    ! Weight and fertilizer of each CFT in each grid cell; dimensioned [g, cft_lb:cft_ub]
    ! This array is modified in-place
    real(r8), intent(inout) :: wt_cft(begg:, cft_lb:)
    real(r8), intent(inout) :: fert_cft(begg:, cft_lb:)

    logical, intent(in) :: verbose  ! If true, print some extra information
    !
    ! !LOCAL VARIABLES:
    integer :: g
    integer :: m
    real(r8) :: wt_cft_to
    real(r8) :: wt_cft_from
    real(r8) :: wt_cft_merge

    character(len=*), parameter :: subname = 'collapse_crop_types'
    !-----------------------------------------------------------------------

    SHR_ASSERT_ALL((ubound(wt_cft) == (/endg, cft_ub/)), errMsg(sourcefile, __LINE__))

    if (cft_size <= 0) then
       call endrun(msg = subname//' can only be called if cft_size > 0' // &
            errMsg(sourcefile, __LINE__))
    end if

    ! ------------------------------------------------------------------------
    ! If not using irrigation, merge irrigated CFTs into rainfed CFTs
    ! ------------------------------------------------------------------------

    if (.not. irrigate) then
       if (verbose .and. masterproc) then
          write(iulog,*) trim(subname)//' crop=.T. and irrigate=.F., so merging irrigated pfts with rainfed'
       end if

       do g = begg, endg
          ! Left Hand Side: merged rainfed+irrigated crop pfts from nc3crop to
          !                 npcropmax-1, stride 2
          ! Right Hand Side: rainfed crop pfts from nc3crop to npcropmax-1,
          !                  stride 2
          ! plus             irrigated crop pfts from nc3irrig to npcropmax,
          !                  stride 2
          ! where stride 2 means "every other"
          wt_cft(g, nc3crop:npcropmax-1:2) = &
               wt_cft(g, nc3crop:npcropmax-1:2) + wt_cft(g, nc3irrig:npcropmax:2)
          wt_cft(g, nc3irrig:npcropmax:2)  = 0._r8
       end do

       call check_sums_equal_1(wt_cft, begg, 'wt_cft', subname//': irrigation')
    end if

    ! ------------------------------------------------------------------------
    ! Merge CFTs into the list of crops that CLM knows how to model
    ! ------------------------------------------------------------------------

    if (verbose .and. masterproc) then
       write(iulog, *) trim(subname) // ' merging wheat, barley, and rye into temperate cereals'
       write(iulog, *) trim(subname) // ' clm knows how to model corn, temperate cereals, and soybean'
       write(iulog, *) trim(subname) // ' all other crops are lumped with the generic crop pft'
    end if

    do g = begg, endg
       do m = 1, npcropmax
          if (m /= pftcon%mergetoclmpft(m)) then
             wt_cft_to = wt_cft(g, pftcon%mergetoclmpft(m))
             wt_cft_from = wt_cft(g, m)
             wt_cft_merge = wt_cft_to + wt_cft_from
             wt_cft(g, pftcon%mergetoclmpft(m)) = wt_cft_merge
             wt_cft(g, m) = 0._r8
             if (wt_cft_merge > 0._r8) then
                fert_cft(g,pftcon%mergetoclmpft(m)) = (wt_cft_to * fert_cft(g,pftcon%mergetoclmpft(m)) + &
                                                      wt_cft_from * fert_cft(g,m)) / wt_cft_merge
             end if
          end if
       end do

    end do

    call check_sums_equal_1(wt_cft, begg, 'wt_cft', subname//': mergetoclmpft')

  end subroutine collapse_crop_types


end module surfrdUtilsMod
