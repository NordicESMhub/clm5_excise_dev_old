module initSubgridMod

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Lower-level routines for initializing the subgrid structure. This module is shared
  ! between both the production code (via initGridCellsMod) and unit testing code.
  !
  ! !USES:
  use shr_kind_mod   , only : r8 => shr_kind_r8
  use shr_log_mod    , only : errMsg => shr_log_errMsg
  use spmdMod        , only : masterproc
  use abortutils     , only : endrun
  use clm_varctl     , only : iulog, use_fates
  use clm_varcon     , only : namep, namec, namel
  use decompMod      , only : bounds_type
  use GridcellType   , only : grc                
  use LandunitType   , only : lun                
  use ColumnType     , only : col                
  use PatchType      , only : patch                
  use column_varcon  , only : is_hydrologically_active
  !
  ! !PUBLIC TYPES:
  implicit none
  private
  save
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public :: clm_ptrs_compdown ! fill in data pointing down
  public :: clm_ptrs_check    ! checks and writes out a summary of subgrid data
  public :: add_landunit      ! add an entry in the landunit-level arrays
  public :: add_column        ! add an entry in the column-level arrays
  public :: add_patch         ! add an entry in the patch-level arrays
  !

  character(len=*), parameter, private :: sourcefile = &
       __FILE__
  !-----------------------------------------------------------------------

contains
  
  !------------------------------------------------------------------------------
  subroutine clm_ptrs_compdown(bounds)
    !
    ! !DESCRIPTION:
    ! Assumes the part of the subgrid pointing up has been set.  Fills 
    ! in the data pointing down.  Up is p_c, p_l, p_g, c_l, c_g, and l_g.
    !
    ! This algorithm assumes all indices besides grid cell are monotonically
    ! increasing.  (Note that grid cell index is NOT monotonically increasing,
    ! hence we cannot set initial & final indices at the grid cell level - 
    ! grc%luni, grc%lunf, etc.)
    !
    ! Algorithm works as follows.  The p, c, and l loops march through
    ! the full arrays (nump, numc, and numl) checking the "up" indexes.
    ! As soon as the "up" index of the current (p,c,l) cell changes relative 
    ! to the previous (p,c,l) cell, the *i array will be set to point down 
    ! to that cell.  The *f array follows the same logic, so it's always the
    ! last "up" index from the previous cell when an "up" index changes.
    !
    ! For example, a case where p_c(1:4) = 1 and p_c(5:12) = 2.  This 
    ! subroutine will set c_pi(1) = 1, c_pf(1) = 4, c_pi(2) = 5, c_pf(2) = 12.
    !
    ! !USES
    use clm_varcon, only : ispval
    !
    ! !ARGUMENTS
    implicit none
    type(bounds_type), intent(in) :: bounds  ! bounds
    !
    ! !LOCAL VARIABLES:
    integer :: l,c,p               ! loop counters
    integer :: curg,curl,curc,curp ! tracks g,l,c,p indexes in arrays
    integer :: ltype               ! landunit type
    !------------------------------------------------------------------------------

    !--- Set the current c,l (curc, curl) to zero for initialization,
    !---   these indices track the current "up" index.
    !--- Take advantage of locality of l/c/p cells
    !--- Loop p through full local begp:endp length
    !--- Separately check the p_c, p_l, and p_g indexes for a change in
    !---   the "up" index.
    !--- If there is a change, verify that the current c,l,g is within the 
    !---   valid range, and set c_pi, l_pi, or g_pi to that current c,l,g
    !--- Constantly update the c_pf, l_pf, and g_pf array.  When the
    !---   g, l, c index changes, the *_pf array will be set correctly
    !--- Do the same for cols setting c_li, c_gi, c_lf, c_gf and
    !---   lunits setting l_gi, l_gf.

    curc = 0
    curl = 0
    do p = bounds%begp,bounds%endp
       if (patch%column(p) /= curc) then
          curc = patch%column(p)
          if (curc < bounds%begc .or. curc > bounds%endc) then
             write(iulog,*) 'clm_ptrs_compdown ERROR: pcolumn ',p,curc,bounds%begc,bounds%endc
             call endrun(decomp_index=p, clmlevel=namep, msg=errMsg(sourcefile, __LINE__))
          endif
          col%patchi(curc) = p
       endif
       col%patchf(curc) = p
       col%npatches(curc) = col%patchf(curc) - col%patchi(curc) + 1
       if (patch%landunit(p) /= curl) then
          curl = patch%landunit(p)
          if (curl < bounds%begl .or. curl > bounds%endl) then
             write(iulog,*) 'clm_ptrs_compdown ERROR: plandunit ',p,curl,bounds%begl,bounds%endl
             call endrun(decomp_index=p, clmlevel=namep, msg=errMsg(sourcefile, __LINE__))
          endif
          lun%patchi(curl) = p
       endif
       lun%patchf(curl) = p
       lun%npatches(curl) = lun%patchf(curl) - lun%patchi(curl) + 1
    enddo

    curl = 0
    do c = bounds%begc,bounds%endc
       if (col%landunit(c) /= curl) then
          curl = col%landunit(c)
          if (curl < bounds%begl .or. curl > bounds%endl) then
             write(iulog,*) 'clm_ptrs_compdown ERROR: clandunit ',c,curl,bounds%begl,bounds%endl
             call endrun(decomp_index=c, clmlevel=namec, msg=errMsg(sourcefile, __LINE__))
          endif
          lun%coli(curl) = c
       endif
       lun%colf(curl) = c
       lun%ncolumns(curl) = lun%colf(curl) - lun%coli(curl) + 1
    enddo

    ! Determine landunit_indices: indices into landunit-level arrays for each grid cell.
    ! Note that landunits not present in a given grid cell are set to ispval.
    grc%landunit_indices(:,bounds%begg:bounds%endg) = ispval
    do l = bounds%begl,bounds%endl
       ltype = lun%itype(l)
       curg = lun%gridcell(l)
       if (curg < bounds%begg .or. curg > bounds%endg) then
          write(iulog,*) 'clm_ptrs_compdown ERROR: landunit_indices ', l,curg,bounds%begg,bounds%endg
          call endrun(decomp_index=l, clmlevel=namel, msg=errMsg(sourcefile, __LINE__))
       end if

       if (grc%landunit_indices(ltype, curg) == ispval) then
          grc%landunit_indices(ltype, curg) = l
       else
          write(iulog,*) 'clm_ptrs_compdown ERROR: This landunit type has already been set for this gridcell'
          write(iulog,*) 'l, ltype, curg = ', l, ltype, curg
          call endrun(decomp_index=l, clmlevel=namel, msg=errMsg(sourcefile, __LINE__))
       end if
    end do

  end subroutine clm_ptrs_compdown

  !------------------------------------------------------------------------------
  subroutine clm_ptrs_check(bounds)
    !
    ! !DESCRIPTION:
    ! Checks and writes out a summary of subgrid data
    !
    ! !USES
    use clm_varcon, only : ispval
    use landunit_varcon, only : max_lunit
    !
    ! !ARGUMENTS
    implicit none
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:
    integer :: g,l,c,p       ! loop counters
    integer :: l_prev        ! l value of previous point
    integer :: ltype         ! landunit type
    logical :: error         ! error flag
    !------------------------------------------------------------------------------

    associate( &
         begg => bounds%begg, &
         endg => bounds%endg, &
         begl => bounds%begl, &
         endl => bounds%endl, &
         begc => bounds%begc, &
         endc => bounds%endc, &
         begp => bounds%begp, &
         endp => bounds%endp  &
         )
    
    if (masterproc) write(iulog,*) ' '
    if (masterproc) write(iulog,*) '---clm_ptrs_check:'

    !--- check index ranges ---
    error = .false.
    do g = begg, endg
       do ltype = 1, max_lunit
          l = grc%landunit_indices(ltype, g)
          if (l /= ispval) then
             if (l < begl .or. l > endl) error = .true.
          end if
       end do
    end do
    if (error) then
       write(iulog,*) '   clm_ptrs_check: g index ranges - ERROR'
       call endrun(msg=errMsg(sourcefile, __LINE__))
    end if
    if (masterproc) write(iulog,*) '   clm_ptrs_check: g index ranges - OK'

    error = .false.
    if (minval(lun%gridcell(begl:endl)) < begg .or. maxval(lun%gridcell(begl:endl)) > endg) error=.true.
    if (minval(lun%coli(begl:endl)) < begc .or. maxval(lun%coli(begl:endl)) > endc) error=.true.
    if (minval(lun%colf(begl:endl)) < begc .or. maxval(lun%colf(begl:endl)) > endc) error=.true.
    if (minval(lun%patchi(begl:endl)) < begp .or. maxval(lun%patchi(begl:endl)) > endp) error=.true.
    if (minval(lun%patchf(begl:endl)) < begp .or. maxval(lun%patchf(begl:endl)) > endp) error=.true.
    if (error) then
       write(iulog,*) '   clm_ptrs_check: l index ranges - ERROR'
       call endrun(msg=errMsg(sourcefile, __LINE__))
    endif
    if (masterproc) write(iulog,*) '   clm_ptrs_check: l index ranges - OK'

    error = .false.
    if (minval(col%gridcell(begc:endc)) < begg .or. maxval(col%gridcell(begc:endc)) > endg) error=.true.
    if (minval(col%landunit(begc:endc)) < begl .or. maxval(col%landunit(begc:endc)) > endl) error=.true.
    if (minval(col%patchi(begc:endc)) < begp .or. maxval(col%patchi(begc:endc)) > endp) error=.true.
    if (minval(col%patchf(begc:endc)) < begp .or. maxval(col%patchf(begc:endc)) > endp) error=.true.
    if (error) then
       write(iulog,*) '   clm_ptrs_check: c index ranges - ERROR'
       call endrun(msg=errMsg(sourcefile, __LINE__))
    endif
    if (masterproc) write(iulog,*) '   clm_ptrs_check: c index ranges - OK'

    error = .false.
    if (minval(patch%gridcell(begp:endp)) < begg .or. maxval(patch%gridcell(begp:endp)) > endg) error=.true.
    if (minval(patch%landunit(begp:endp)) < begl .or. maxval(patch%landunit(begp:endp)) > endl) error=.true.
    if (minval(patch%column(begp:endp)) < begc .or. maxval(patch%column(begp:endp)) > endc) error=.true.
    if (error) then
       write(iulog,*) '   clm_ptrs_check: p index ranges - ERROR'
       call endrun(msg=errMsg(sourcefile, __LINE__))
    endif
    if (masterproc) write(iulog,*) '   clm_ptrs_check: p index ranges - OK'

    !--- check that indices in arrays are monotonically increasing ---
    error = .false.
    do l=begl+1,endl
      if ((lun%itype(l) == lun%itype(l-1)) .and. &
           lun%gridcell(l) < lun%gridcell(l-1)) then
         ! grid cell indices should be monotonically increasing for a given landunit type
         error = .true.
      end if
      if (lun%coli(l) < lun%coli(l-1)) error = .true.
      if (lun%colf(l) < lun%colf(l-1)) error = .true.
      if (lun%patchi(l) < lun%patchi(l-1)) error = .true.
      if (lun%patchf(l) < lun%patchf(l-1)) error = .true.
      if (error) then
         write(iulog,*) '   clm_ptrs_check: l mono increasing - ERROR'
         call endrun(decomp_index=l, clmlevel=namel, msg=errMsg(sourcefile, __LINE__))
      endif
    enddo
    if (masterproc) write(iulog,*) '   clm_ptrs_check: l mono increasing - OK'

    error = .false.
    do c=begc+1,endc
      l = col%landunit(c)
      l_prev = col%landunit(c-1)
      if ((lun%itype(l) == lun%itype(l_prev)) .and. &
           col%gridcell(c) < col%gridcell(c-1)) then
         ! grid cell indices should be monotonically increasing for a given landunit type
         error = .true.
      end if
      if (col%landunit(c) < col%landunit(c-1)) error = .true.
      if (col%patchi(c) < col%patchi(c-1)) error = .true.
      if (col%patchf(c) < col%patchf(c-1)) error = .true.
      if (error) then
         write(iulog,*) '   clm_ptrs_check: c mono increasing - ERROR'
         call endrun(decomp_index=c, clmlevel=namec, msg=errMsg(sourcefile, __LINE__))
      endif
    enddo
    if (masterproc) write(iulog,*) '   clm_ptrs_check: c mono increasing - OK'

    error = .false.
    do p=begp+1,endp
      l = patch%landunit(p)
      l_prev = patch%landunit(p-1)
      if ((lun%itype(l) == lun%itype(l_prev)) .and. &
           patch%gridcell(p) < patch%gridcell(p-1)) then
         ! grid cell indices should be monotonically increasing for a given landunit type
         error = .true.
      end if
      if (patch%landunit(p) < patch%landunit(p-1)) error = .true.
      if (patch%column  (p) < patch%column  (p-1)) error = .true.
      if (error) then
         write(iulog,*) '   clm_ptrs_check: p mono increasing - ERROR'
         call endrun(decomp_index=p, clmlevel=namep, msg=errMsg(sourcefile, __LINE__))
      endif
    enddo
    if (masterproc) write(iulog,*) '   clm_ptrs_check: p mono increasing - OK'

    !--- check that the tree is internally consistent ---
    error = .false.
    do g = begg, endg
       do ltype = 1, max_lunit
          l = grc%landunit_indices(ltype, g)

          ! skip l == ispval, which implies that this landunit type doesn't exist on this grid cell
          if (l /= ispval) then
             if (lun%itype(l) /= ltype) error = .true.
             if (lun%gridcell(l) /= g) error = .true.
             if (error) then
                write(iulog,*) '   clm_ptrs_check: tree consistent - ERROR'
                call endrun(decomp_index=l, clmlevel=namel, msg=errMsg(sourcefile, __LINE__))
             endif
             do c = lun%coli(l),lun%colf(l)
                if (col%gridcell(c) /= g) error = .true.
                if (col%landunit(c) /= l) error = .true.
                if (error) then
                   write(iulog,*) '   clm_ptrs_check: tree consistent - ERROR'
                   call endrun(decomp_index=c, clmlevel=namec, msg=errMsg(sourcefile, __LINE__))
                endif
                do p = col%patchi(c),col%patchf(c)
                   if (patch%gridcell(p) /= g) error = .true.
                   if (patch%landunit(p) /= l) error = .true.
                   if (patch%column(p)   /= c) error = .true.
                   if (error) then
                      write(iulog,*) '   clm_ptrs_check: tree consistent - ERROR'
                      call endrun(decomp_index=p, clmlevel=namep, msg=errMsg(sourcefile, __LINE__))
                   endif
                enddo  ! p
             enddo  ! c
          end if  ! l /= ispval
       enddo  ! ltype
    enddo  ! g
    if (masterproc) write(iulog,*) '   clm_ptrs_check: tree consistent - OK'
    if (masterproc) write(iulog,*) ' '

    end associate
    
  end subroutine clm_ptrs_check

  !-----------------------------------------------------------------------
  subroutine add_landunit(li, gi, ltype, wtgcell)
    !
    ! !DESCRIPTION:
    ! Add an entry in the landunit-level arrays. li gives the index of the last landunit
    ! added; the new landunit is added at li+1, and the li argument is incremented
    ! accordingly.
    !
    ! !USES:
    use landunit_varcon , only : istice_mec, istdlak, isturb_MIN, isturb_MAX, landunit_is_special
    !
    ! !ARGUMENTS:
    integer  , intent(inout) :: li      ! input value is index of last landunit added; output value is index of this newly-added landunit
    integer  , intent(in)    :: gi      ! grid cell index on which this landunit should be placed
    integer  , intent(in)    :: ltype   ! landunit type
    real(r8) , intent(in)    :: wtgcell ! weight of the landunit relative to the grid cell
    !
    ! !LOCAL VARIABLES:
    
    character(len=*), parameter :: subname = 'add_landunit'
    !-----------------------------------------------------------------------
    
    li = li + 1

    lun%gridcell(li) = gi
    lun%wtgcell(li) = wtgcell
    lun%itype(li) = ltype

    lun%ifspecial(li) = landunit_is_special(ltype)

    if (ltype == istice_mec) then
       lun%glcmecpoi(li) = .true.
    else
       lun%glcmecpoi(li) = .false.
    end if

    if (ltype == istdlak) then
       lun%lakpoi(li) = .true.
    else
       lun%lakpoi(li) = .false.
    end if

    if (ltype >= isturb_MIN .and. ltype <= isturb_MAX) then
       lun%urbpoi(li) = .true.
    else
       lun%urbpoi(li) = .false.
    end if

  end subroutine add_landunit

  !-----------------------------------------------------------------------
  subroutine add_column(ci, li, ctype, wtlunit, type_is_dynamic)
    !
    ! !DESCRIPTION:
    ! Add an entry in the column-level arrays. ci gives the index of the last column
    ! added; the new column is added at ci+1, and the ci argument is incremented
    ! accordingly.
    !
    ! !ARGUMENTS:
    integer  , intent(inout) :: ci      ! input value is index of last column added; output value is index of this newly-added column
    integer  , intent(in)    :: li      ! landunit index on which this column should be placed (assumes this landunit has already been created)
    integer  , intent(in)    :: ctype   ! column type
    real(r8) , intent(in)    :: wtlunit ! weight of the column relative to the landunit

    ! whether this column's type can change at runtime; if not provided, assumed to be false
    logical  , intent(in), optional :: type_is_dynamic
    !
    ! !LOCAL VARIABLES:
    logical :: l_type_is_dynamic  ! local version of type_is_dynamic

    character(len=*), parameter :: subname = 'add_column'
    !-----------------------------------------------------------------------

    l_type_is_dynamic = .false.
    if (present(type_is_dynamic)) then
       l_type_is_dynamic = type_is_dynamic
    end if

    ci = ci + 1

    col%landunit(ci) = li
    col%gridcell(ci) = lun%gridcell(li)
    col%wtlunit(ci) = wtlunit
    col%itype(ci) = ctype
    col%type_is_dynamic(ci) = l_type_is_dynamic
    col%hydrologically_active(ci) = is_hydrologically_active( &
         col_itype = ctype, &
         lun_itype = lun%itype(li))

  end subroutine add_column

  !-----------------------------------------------------------------------
  subroutine add_patch(pi, ci, ptype, wtcol)
    !
    ! !DESCRIPTION:
    ! Add an entry in the patch-level arrays. pi gives the index of the last patch added; the
    ! new patch is added at pi+1, and the pi argument is incremented accordingly.
    !
    ! !USES:
    use clm_varcon      , only : ispval
	
!Edit by Lei Cai--start
    use landunit_varcon , only : istsoil, istsoil_li, istsoil_mi, istsoil_hi, istcrop
!Edit by Lei Cai--end

    use clm_varpar      , only : natpft_lb
    !
    ! !ARGUMENTS:
    integer  , intent(inout) :: pi    ! input value is index of last patch added; output value is index of this newly-added patch
    integer  , intent(in)    :: ci    ! column index on which this patch should be placed (assumes this column has already been created)
    integer  , intent(in)    :: ptype ! patch type
    real(r8) , intent(in)    :: wtcol ! weight of the patch relative to the column
    !
    ! !LOCAL VARIABLES:
    integer :: li        ! landunit index
    integer :: lb_offset ! offset between natpft_lb and 1
    
    character(len=*), parameter :: subname = 'add_patch'
    !-----------------------------------------------------------------------
    
    pi = pi + 1

    patch%column(pi) = ci
    li = col%landunit(ci)
    patch%landunit(pi) = li
    patch%gridcell(pi) = col%gridcell(ci)

    patch%wtcol(pi) = wtcol

    ! TODO (MV, 10-17-14): The following must be commented out because
    ! currently patch%itype is used in CanopyTemperatureMod to calculate 
    ! z0m(p) and displa(p) - and is still called even when fates is on

    !if (.not. use_fates) then
    patch%itype(pi) = ptype
    !end if

!Edit by Lei Cai--start
    if (lun%itype(li) == istsoil .or. lun%itype(li) == istsoil_li .or. &
	    lun%itype(li) == istsoil_mi .or. lun%itype(li) == istsoil_hi .or. &
		lun%itype(li) == istcrop) then
!Edit by Lei Cai--end

       lb_offset = 1 - natpft_lb
       patch%mxy(pi) = ptype + lb_offset
    else
       patch%mxy(pi) = ispval
    end if
    

  end subroutine add_patch


end module initSubgridMod
