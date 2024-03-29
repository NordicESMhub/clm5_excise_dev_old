module filterMod

#include "shr_assert.h"

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Module of filters used for processing columns and pfts of particular
  ! types, including lake, non-lake, urban, soil, snow, non-snow, and
  ! naturally-vegetated patches.
  !
  ! !USES:
  use shr_kind_mod   , only : r8 => shr_kind_r8
  use shr_log_mod    , only : errMsg => shr_log_errMsg
  use abortutils     , only : endrun
  use clm_varctl     , only : iulog
  use decompMod      , only : bounds_type  
  use GridcellType   , only : grc
  use LandunitType   , only : lun                
  use ColumnType     , only : col                
  use PatchType      , only : patch
  use glcBehaviorMod , only : glc_behavior_type
  !
  ! !PUBLIC TYPES:
  implicit none
  save
  private
  !
  type clumpfilter
     integer, pointer :: allc(:)         ! all columns
     integer :: num_allc                 ! number of points in allc filter

     integer, pointer :: natvegp(:)      ! CNDV nat-vegetated (present) filter (pfts)
     integer :: num_natvegp              ! number of pfts in nat-vegetated filter

     integer, pointer :: pcropp(:)       ! prognostic crop filter (pfts)
     integer :: num_pcropp               ! number of pfts in prognostic crop filter
     integer, pointer :: soilnopcropp(:) ! soil w/o prog. crops (pfts)
     integer :: num_soilnopcropp         ! number of pfts in soil w/o prog crops

     integer, pointer :: lakep(:)        ! lake filter (pfts)
     integer :: num_lakep                ! number of pfts in lake filter
     integer, pointer :: nolakep(:)      ! non-lake filter (pfts)
     integer :: num_nolakep              ! number of pfts in non-lake filter
     integer, pointer :: lakec(:)        ! lake filter (columns)
     integer :: num_lakec                ! number of columns in lake filter
     integer, pointer :: nolakec(:)      ! non-lake filter (columns)
     integer :: num_nolakec              ! number of columns in non-lake filter

     integer, pointer :: soilc(:)        ! soil filter (columns)
     integer :: num_soilc                ! number of columns in soil filter 
     integer, pointer :: soilp(:)        ! soil filter (pfts)
     integer :: num_soilp                ! number of pfts in soil filter 

     integer, pointer :: snowc(:)        ! snow filter (columns) 
     integer :: num_snowc                ! number of columns in snow filter 
     integer, pointer :: nosnowc(:)      ! non-snow filter (columns) 
     integer :: num_nosnowc              ! number of columns in non-snow filter 

     integer, pointer :: lakesnowc(:)    ! snow filter (columns) 
     integer :: num_lakesnowc            ! number of columns in snow filter 
     integer, pointer :: lakenosnowc(:)  ! non-snow filter (columns) 
     integer :: num_lakenosnowc          ! number of columns in non-snow filter 

     integer, pointer :: exposedvegp(:)  ! patches where frac_veg_nosno is non-zero
     integer :: num_exposedvegp          ! number of patches in exposedvegp filter
     integer, pointer :: noexposedvegp(:)! patches where frac_veg_nosno is 0 (does NOT include lake or urban)
     integer :: num_noexposedvegp        ! number of patches in noexposedvegp filter

     integer, pointer :: hydrologyc(:)   ! hydrology filter (columns)
     integer :: num_hydrologyc           ! number of columns in hydrology filter 

     integer, pointer :: urbanl(:)       ! urban filter (landunits)
     integer :: num_urbanl               ! number of landunits in urban filter 
     integer, pointer :: nourbanl(:)     ! non-urban filter (landunits)
     integer :: num_nourbanl             ! number of landunits in non-urban filter 

     integer, pointer :: urbanc(:)       ! urban filter (columns)
     integer :: num_urbanc               ! number of columns in urban filter
     integer, pointer :: nourbanc(:)     ! non-urban filter (columns)
     integer :: num_nourbanc             ! number of columns in non-urban filter

     integer, pointer :: urbanp(:)       ! urban filter (pfts)
     integer :: num_urbanp               ! number of pfts in urban filter
     integer, pointer :: nourbanp(:)     ! non-urban filter (pfts)
     integer :: num_nourbanp             ! number of pfts in non-urban filter

     integer, pointer :: nolakeurbanp(:) ! non-lake, non-urban filter (pfts)
     integer :: num_nolakeurbanp         ! number of pfts in non-lake, non-urban filter

     integer, pointer :: icemecc(:)      ! glacier mec filter (cols)
     integer :: num_icemecc              ! number of columns in glacier mec filter
     
     integer, pointer :: do_smb_c(:)     ! glacier+bareland SMB calculations-on filter (cols)
     integer :: num_do_smb_c             ! number of columns in glacier+bareland SMB mec filter         

  end type clumpfilter
  public clumpfilter

  ! This is the standard set of filters, which should be used in most places in the code.
  ! These filters only include 'active' points.
  type(clumpfilter), allocatable, public :: filter(:)
  
  ! --- DO NOT USING THE FOLLOWING VARIABLE UNLESS YOU KNOW WHAT YOU'RE DOING! ---
  !
  ! This is a separate set of filters that contains both inactive and active points. It is
  ! rarely appropriate to use these, but they are needed in a few places, e.g., where
  ! quantities are computed before weights, active flags and filters are updated due to
  ! landuse change. Note that, for the handful of filters that are computed outside of
  ! setFiltersOneGroup (including the CNDV natvegp filter and the snow filters), these
  ! filters are NOT included in this variable - so they can only be used from the main
  ! 'filter' variable.
  !
  ! Ideally, we would like to restructure the initialization code and driver ordering so
  ! that this version of the filters is never needed. At that point, we could remove this
  ! filter_inactive_and_active variable, and simplify filterMod to look the way it did
  ! before this variable was added (i.e., when there was only a single group of filters).
  !
  type(clumpfilter), allocatable, public :: filter_inactive_and_active(:)
  !
  public allocFilters         ! allocate memory for filters
  public setFilters           ! set filters
  public setExposedvegpFilter ! set the exposedvegp and noexposedvegp filters
  
  private allocFiltersOneGroup  ! allocate memory for one group of filters
  private setFiltersOneGroup    ! set one group of filters

  character(len=*), parameter, private :: sourcefile = &
       __FILE__
  !
  ! !REVISION HISTORY:
  ! Created by Mariana Vertenstein
  ! 11/13/03, Peter Thornton: Added soilp and num_soilp
  ! Jan/08, S. Levis: Added crop-related filters
  ! June/13, Bill Sacks: Change main filters to just work over 'active' points; 
  ! add filter_inactive_and_active
  !-----------------------------------------------------------------------

contains

  !------------------------------------------------------------------------
  subroutine allocFilters()
    !
    ! !DESCRIPTION:
    ! Allocate CLM filters.
    !
    ! !REVISION HISTORY:
    ! Created by Bill Sacks
    !------------------------------------------------------------------------

    call allocFiltersOneGroup(filter)
    call allocFiltersOneGroup(filter_inactive_and_active)

  end subroutine allocFilters

  !------------------------------------------------------------------------
  subroutine allocFiltersOneGroup(this_filter)
    !
    ! !DESCRIPTION:
    ! Allocate CLM filters, for one group of filters.
    !
    ! !USES:
    use decompMod , only : get_proc_clumps, get_clump_bounds
    !
    ! !ARGUMENTS:
    type(clumpfilter), intent(inout), allocatable :: this_filter(:)  ! the filter to allocate
    !
    ! LOCAL VARAIBLES:
    integer :: nc          ! clump index
    integer :: nclumps     ! total number of clumps on this processor
    integer :: ier         ! error status
    type(bounds_type) :: bounds  
    !------------------------------------------------------------------------

    ! Determine clump variables for this processor

    nclumps = get_proc_clumps()

    ier = 0
    if( .not. allocated(this_filter)) then
       allocate(this_filter(nclumps), stat=ier)
    end if
    if (ier /= 0) then
       write(iulog,*) 'allocFiltersOneGroup(): allocation error for clumpsfilters'
       call endrun(msg=errMsg(sourcefile, __LINE__))
    end if

    ! Loop over clumps on this processor

!$OMP PARALLEL DO PRIVATE (nc,bounds)
    do nc = 1, nclumps
       call get_clump_bounds(nc, bounds)

       allocate(this_filter(nc)%allc(bounds%endc-bounds%begc+1))

       allocate(this_filter(nc)%lakep(bounds%endp-bounds%begp+1))
       allocate(this_filter(nc)%nolakep(bounds%endp-bounds%begp+1))
       allocate(this_filter(nc)%nolakeurbanp(bounds%endp-bounds%begp+1))

       allocate(this_filter(nc)%lakec(bounds%endc-bounds%begc+1))
       allocate(this_filter(nc)%nolakec(bounds%endc-bounds%begc+1))

       allocate(this_filter(nc)%soilc(bounds%endc-bounds%begc+1))
       allocate(this_filter(nc)%soilp(bounds%endp-bounds%begp+1))

       allocate(this_filter(nc)%snowc(bounds%endc-bounds%begc+1))
       allocate(this_filter(nc)%nosnowc(bounds%endc-bounds%begc+1))

       allocate(this_filter(nc)%lakesnowc(bounds%endc-bounds%begc+1))
       allocate(this_filter(nc)%lakenosnowc(bounds%endc-bounds%begc+1))

       allocate(this_filter(nc)%exposedvegp(bounds%endp-bounds%begp+1))
       allocate(this_filter(nc)%noexposedvegp(bounds%endp-bounds%begp+1))

       allocate(this_filter(nc)%natvegp(bounds%endp-bounds%begp+1))

       allocate(this_filter(nc)%hydrologyc(bounds%endc-bounds%begc+1))

       allocate(this_filter(nc)%urbanp(bounds%endp-bounds%begp+1))
       allocate(this_filter(nc)%nourbanp(bounds%endp-bounds%begp+1))

       allocate(this_filter(nc)%urbanc(bounds%endc-bounds%begc+1))
       allocate(this_filter(nc)%nourbanc(bounds%endc-bounds%begc+1))

       allocate(this_filter(nc)%urbanl(bounds%endl-bounds%begl+1))
       allocate(this_filter(nc)%nourbanl(bounds%endl-bounds%begl+1))

       allocate(this_filter(nc)%pcropp(bounds%endp-bounds%begp+1))
       allocate(this_filter(nc)%soilnopcropp(bounds%endp-bounds%begp+1))

       allocate(this_filter(nc)%icemecc(bounds%endc-bounds%begc+1))      
       allocate(this_filter(nc)%do_smb_c(bounds%endc-bounds%begc+1))       
       
    end do
!$OMP END PARALLEL DO

  end subroutine allocFiltersOneGroup

  !------------------------------------------------------------------------
  subroutine setFilters(bounds, glc_behavior)
    !
    ! !DESCRIPTION:
    ! Set CLM filters.
    use decompMod , only : BOUNDS_LEVEL_CLUMP
    !
    ! !ARGUMENTS:
    type(bounds_type)       , intent(in) :: bounds
    type(glc_behavior_type) , intent(in) :: glc_behavior
    !------------------------------------------------------------------------

    SHR_ASSERT(bounds%level == BOUNDS_LEVEL_CLUMP, errMsg(sourcefile, __LINE__))

    call setFiltersOneGroup(bounds, &
         filter, include_inactive = .false., &
         glc_behavior = glc_behavior)

    ! At least as of June, 2013, the 'inactive_and_active' version of the filters is
    ! static in time. Thus, we could have some logic saying whether we're in
    ! initialization, and if so, skip this call. But this is problematic for two reasons:
    ! (1) it requires that the caller of this routine (currently reweight_wrapup) know
    ! whether it is in initialization; and (2) it assumes that the filter definitions
    ! won't be changed in the future in a way that creates some variability in time. So
    ! for now, it seems cleanest and safest to just update these filters whenever the main
    ! filters are updated. But if this proves to be a performance problem, we could
    ! introduce an argument saying whether we're in initialization, and if so, skip this
    ! call.
    
    call setFiltersOneGroup(bounds, &
         filter_inactive_and_active, include_inactive = .true., &
         glc_behavior = glc_behavior)
    
  end subroutine setFilters


  !------------------------------------------------------------------------
  subroutine setFiltersOneGroup(bounds, this_filter, include_inactive, glc_behavior)
    !
    ! !DESCRIPTION:
    ! Set CLM filters for one group of filters.
    !
    ! "Standard" filters only include active points. However, this routine can be used to set
    ! alternative filters that also apply over inactive points, by setting include_inactive =
    ! .true.
    !
    ! This routine sets filters that are determined by subgrid type, "active" status of
    ! patch, col or landunit, and the like. Filters based on model state (e.g., snow
    ! cover) should generally be set elsewhere, to ensure that the routine that sets them
    ! is called at the right time in the driver loop.
    !
    ! !USES:
    use decompMod       , only : BOUNDS_LEVEL_CLUMP
    use pftconMod       , only : npcropmin
!Edit by Lei Cai--start
    use landunit_varcon , only : istsoil, istsoil_li, istsoil_mi, istsoil_hi, istcrop, istice_mec
!Edit by Lei Cai--end
    !
    ! !ARGUMENTS:
    type(bounds_type)       , intent(in)    :: bounds  
    type(clumpfilter)       , intent(inout) :: this_filter(:)   ! the group of filters to set
    logical                 , intent(in)    :: include_inactive ! whether inactive points should be included in the filters
    type(glc_behavior_type) , intent(in)    :: glc_behavior
    !
    ! LOCAL VARAIBLES:
    integer :: nc          ! clump index
    integer :: c,l,p       ! column, landunit, patch indices
    integer :: fl          ! lake filter index
    integer :: fnl,fnlu    ! non-lake filter index
    integer :: fs          ! soil filter index
    integer :: f, fn       ! general indices
    integer :: g           !gridcell index
    !------------------------------------------------------------------------

    SHR_ASSERT(bounds%level == BOUNDS_LEVEL_CLUMP, errMsg(sourcefile, __LINE__))

    nc = bounds%clump_index

    ! Create filter of all columns
    fl = 0
    do c = bounds%begc,bounds%endc
       if (col%active(c) .or. include_inactive) then
          fl = fl + 1
          this_filter(nc)%allc(fl) = c
       end if
    end do
    this_filter(nc)%num_allc = fl

    ! Create lake and non-lake filters at column-level 

    fl = 0
    fnl = 0
    do c = bounds%begc,bounds%endc
       if (col%active(c) .or. include_inactive) then
          l =col%landunit(c)
          if (lun%lakpoi(l)) then
             fl = fl + 1
             this_filter(nc)%lakec(fl) = c
          else
             fnl = fnl + 1
             this_filter(nc)%nolakec(fnl) = c
          end if
       end if
    end do
    this_filter(nc)%num_lakec = fl
    this_filter(nc)%num_nolakec = fnl

    ! Create lake and non-lake filters at patch-level 

    fl = 0
    fnl = 0
    fnlu = 0
    do p = bounds%begp,bounds%endp
       if (patch%active(p) .or. include_inactive) then
          l =patch%landunit(p)
          if (lun%lakpoi(l) ) then
             fl = fl + 1
             this_filter(nc)%lakep(fl) = p
          else
             fnl = fnl + 1
             this_filter(nc)%nolakep(fnl) = p
             if (.not. lun%urbpoi(l)) then
                fnlu = fnlu + 1
                this_filter(nc)%nolakeurbanp(fnlu) = p
             end if
          end if
       end if
    end do
    this_filter(nc)%num_lakep = fl
    this_filter(nc)%num_nolakep = fnl
    this_filter(nc)%num_nolakeurbanp = fnlu

    ! Create soil filter at column-level

    fs = 0
    do c = bounds%begc,bounds%endc
       if (col%active(c) .or. include_inactive) then
          l =col%landunit(c)
		  
!Edit by Lei Cai--start
          if (lun%itype(l) == istsoil .or. lun%itype(l) == istsoil_li .or. &
              lun%itype(l) == istsoil_mi .or. lun%itype(l) == istsoil_hi .or. &
			  lun%itype(l) == istcrop) then
!Edit by Lei Cai--end

             fs = fs + 1
             this_filter(nc)%soilc(fs) = c
          end if
       end if
    end do
    this_filter(nc)%num_soilc = fs

    ! Create soil filter at patch-level

    fs = 0
    do p = bounds%begp,bounds%endp
       if (patch%active(p) .or. include_inactive) then
          l =patch%landunit(p)
		  
!Edit by Lei Cai--start
          if (lun%itype(l) == istsoil .or. lun%itype(l) == istsoil_li .or. &
		      lun%itype(l) == istsoil_mi .or. lun%itype(l) == istsoil_hi .or. &
			  lun%itype(l) == istcrop) then
!Edit by Lei Cai--end

             fs = fs + 1
             this_filter(nc)%soilp(fs) = p
          end if
       end if
    end do
    this_filter(nc)%num_soilp = fs

    ! Create column-level hydrology filter (soil and Urban pervious road cols) 

    f = 0
    do c = bounds%begc,bounds%endc
       if (col%active(c) .or. include_inactive) then
          if (col%hydrologically_active(c)) then
             f = f + 1
             this_filter(nc)%hydrologyc(f) = c
          end if
       end if
    end do
    this_filter(nc)%num_hydrologyc = f
!    write(*,*) 'bounds%begc,bounds%endc:', bounds%begc,bounds%endc !KSA2019 DEBUG

    ! Create prognostic crop and soil w/o prog. crop filters at patch-level
    ! according to where the crop model should be used

    fl  = 0
    fnl = 0
    do p = bounds%begp,bounds%endp
       if (patch%active(p) .or. include_inactive) then
          if (patch%itype(p) >= npcropmin) then !skips 2 generic crop types
             fl = fl + 1
             this_filter(nc)%pcropp(fl) = p
          else
             l =patch%landunit(p)
!Edit by Lei Cai--start
             if (lun%itype(l) == istsoil .or. lun%itype(l) == istsoil_li .or. &
			     lun%itype(l) == istsoil_mi .or. lun%itype(l) == istsoil_hi .or. & 
				 lun%itype(l) == istcrop) then
!Edit by Lei Cai--end
                fnl = fnl + 1
                this_filter(nc)%soilnopcropp(fnl) = p
             end if
          end if
       end if
    end do
    this_filter(nc)%num_pcropp   = fl
    this_filter(nc)%num_soilnopcropp = fnl   ! This wasn't being set before...

    ! Create landunit-level urban and non-urban filters

    f = 0
    fn = 0
    do l = bounds%begl,bounds%endl
       if (lun%active(l) .or. include_inactive) then
          if (lun%urbpoi(l)) then
             f = f + 1
             this_filter(nc)%urbanl(f) = l
          else
             fn = fn + 1
             this_filter(nc)%nourbanl(fn) = l
          end if
       end if
    end do
    this_filter(nc)%num_urbanl = f
    this_filter(nc)%num_nourbanl = fn

    ! Create column-level urban and non-urban filters

    f = 0
    fn = 0
    do c = bounds%begc,bounds%endc
       if (col%active(c) .or. include_inactive) then
          l = col%landunit(c)
          if (lun%urbpoi(l)) then
             f = f + 1
             this_filter(nc)%urbanc(f) = c
          else
             fn = fn + 1
             this_filter(nc)%nourbanc(fn) = c
          end if
       end if
    end do
    this_filter(nc)%num_urbanc = f
    this_filter(nc)%num_nourbanc = fn

    ! Create patch-level urban and non-urban filters

    f = 0
    fn = 0
    do p = bounds%begp,bounds%endp
       if (patch%active(p) .or. include_inactive) then
          l = patch%landunit(p)
          if (lun%urbpoi(l)) then
             f = f + 1
             this_filter(nc)%urbanp(f) = p
          else
             fn = fn + 1
             this_filter(nc)%nourbanp(fn) = p 
          end if
       end if
    end do
    this_filter(nc)%num_urbanp = f
    this_filter(nc)%num_nourbanp = fn

    f = 0
    do c = bounds%begc,bounds%endc
       if (col%active(c) .or. include_inactive) then
          l = col%landunit(c)
          if (lun%itype(l) == istice_mec) then
             f = f + 1
             this_filter(nc)%icemecc(f) = c
          end if
       end if
    end do
    this_filter(nc)%num_icemecc = f

    f = 0
    do c = bounds%begc,bounds%endc
       if (col%active(c) .or. include_inactive) then
          l = col%landunit(c)
          g = col%gridcell(c)

          ! Only compute SMB in regions where we replace ice melt with new ice:
          ! Elsewhere (where ice melt remains in place), we cannot compute a sensible
          ! negative SMB.
          !
          ! In addition to istice_mec columns, we also compute SMB for any soil column in
          ! this region, in order to provide SMB forcing for the bare ground elevation
          ! class (elevation class 0).
          if ( glc_behavior%melt_replaced_by_ice_grc(g) .and. &
               (lun%itype(l) == istice_mec .or. lun%itype(l) == istsoil .or.  & !KSA2019
	       lun%itype(l) == istsoil_li .or. lun%itype(l) == istsoil_mi .or. &
	       lun%itype(l) == istsoil_hi)) then
             f = f + 1
             this_filter(nc)%do_smb_c(f) = c
          end if
       end if
    end do
    this_filter(nc)%num_do_smb_c = f    

    ! Note: snow filters are reconstructed each time step in
    ! LakeHydrology and SnowHydrology
    ! Note: CNDV "pft present" filter is reconstructed each time CNDV is run

  end subroutine setFiltersOneGroup

  !-----------------------------------------------------------------------
  subroutine setExposedvegpFilter(bounds, frac_veg_nosno)
    !
    ! !DESCRIPTION:
    ! Sets the exposedvegp and noexposedvegp filters for one clump.
    !
    ! The exposedvegp filter includes points for which frac_veg_nosno > 0. noexposedvegp
    ! includes points for which frac_veg_nosno <= 0. However, note that neither filter
    ! includes urban or lake points!
    !
    ! Should be called from within a loop over clumps.
    !
    ! Only sets this filter in the main 'filter' variable, NOT in
    ! filter_inactive_and_active.
    ! 
    ! Note that this is done separately from the main setFilters routine, because it may
    ! need to be called at a different time in the driver loop. 
    !
    ! !USES:
    use decompMod , only : BOUNDS_LEVEL_CLUMP
    !
    ! !ARGUMENTS:
    type(bounds_type) , intent(in) :: bounds  
    integer           , intent(in) :: frac_veg_nosno( bounds%begp: ) ! fraction of vegetation not covered by snow [patch]
    !
    ! !LOCAL VARIABLES:
    integer :: nc     ! clump index
    integer :: fp     ! filter index
    integer :: p      ! patch index
    integer :: fe, fn ! filter counts
    
    character(len=*), parameter :: subname = 'setExposedvegpFilter'
    !-----------------------------------------------------------------------

    SHR_ASSERT(bounds%level == BOUNDS_LEVEL_CLUMP, errMsg(sourcefile, __LINE__))
    SHR_ASSERT_ALL((ubound(frac_veg_nosno) == (/bounds%endp/)), errMsg(sourcefile, __LINE__))

    nc = bounds%clump_index

    fe = 0
    fn = 0
    do fp = 1, filter(nc)%num_nolakeurbanp
       p = filter(nc)%nolakeurbanp(fp)
       if (frac_veg_nosno(p) > 0) then
          fe = fe + 1
          filter(nc)%exposedvegp(fe) = p
       else
          fn = fn + 1
          filter(nc)%noexposedvegp(fn) = p
       end if
    end do
    filter(nc)%num_exposedvegp = fe
    filter(nc)%num_noexposedvegp = fn

  end subroutine setExposedvegpFilter


end module filterMod
