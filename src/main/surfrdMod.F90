module surfrdMod

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Contains methods for reading in surface data file and determining
  ! subgrid weights
  !
  ! !USES:
#include "shr_assert.h"
  use shr_kind_mod    , only : r8 => shr_kind_r8
  use shr_log_mod     , only : errMsg => shr_log_errMsg
  use abortutils      , only : endrun
  use clm_varpar      , only : nlevsoifl, numpft
  use landunit_varcon , only : numurbl
  use clm_varcon      , only : grlnd
  use clm_varctl      , only : iulog, scmlat, scmlon, single_column
  use clm_varctl      , only : use_cndv, use_crop
  use surfrdUtilsMod  , only : check_sums_equal_1, collapse_crop_types
  use ncdio_pio       , only : file_desc_t, var_desc_t, ncd_pio_openfile, ncd_pio_closefile
  use ncdio_pio       , only : ncd_io, check_var, ncd_inqfdims, check_dim, ncd_inqdid
  use pio
  use spmdMod                         
  !
  ! !PUBLIC TYPES:
  implicit none
  save
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public :: surfrd_get_globmask  ! Reads global land mask (needed for setting domain decomp)
  public :: surfrd_get_grid      ! Read grid/ladnfrac data into domain (after domain decomp)
  public :: surfrd_get_data      ! Read surface dataset and determine subgrid weights
  !
  ! !PRIVATE MEMBER FUNCTIONS:
  private :: surfrd_special             ! Read the special landunits
  private :: surfrd_veg_all             ! Read all of the vegetated landunits
  private :: surfrd_veg_dgvm            ! Read vegetated landunits for DGVM mode
  private :: surfrd_pftformat           ! Read crop pfts in file format where they are part of the vegetated land unit
  private :: surfrd_cftformat           ! Read crop pfts in file format where they are on their own landunit
  !
  ! !PRIVATE DATA MEMBERS:
  ! default multiplication factor for epsilon for error checks
  real(r8), private, parameter :: eps_fact = 2._r8

  character(len=*), parameter, private :: sourcefile = &
       __FILE__
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine surfrd_get_globmask(filename, mask, ni, nj)
    !
    ! !DESCRIPTION:
    ! Read the surface dataset grid related information:
    ! This is the first routine called by clm_initialize 
    ! NO DOMAIN DECOMPOSITION  HAS BEEN SET YET
    !
    ! !USES:
    use fileutils , only : getfil
    !
    ! !ARGUMENTS:
    character(len=*), intent(in)    :: filename  ! grid filename
    integer         , pointer       :: mask(:)   ! grid mask 
    integer         , intent(out)   :: ni, nj    ! global grid sizes
    !
    ! !LOCAL VARIABLES:
    logical :: isgrid2d
    integer :: dimid,varid         ! netCDF id's
    integer :: ns                  ! size of grid on file
    integer :: n,i,j               ! index 
    integer :: ier                 ! error status
    type(file_desc_t)  :: ncid     ! netcdf id
    type(var_desc_t)   :: vardesc  ! variable descriptor
    character(len=256) :: varname  ! variable name
    character(len=256) :: locfn    ! local file name
    logical :: readvar             ! read variable in or not
    integer , allocatable :: idata2d(:,:)
    character(len=32) :: subname = 'surfrd_get_globmask' ! subroutine name
    !-----------------------------------------------------------------------

    if (filename == ' ') then
       mask(:) = 1
       RETURN
    end if

    if (masterproc) then
       if (filename == ' ') then
          write(iulog,*) trim(subname),' ERROR: filename must be specified '
          call endrun(msg=errMsg(sourcefile, __LINE__))
       endif
    end if

    call getfil( filename, locfn, 0 )
    call ncd_pio_openfile (ncid, trim(locfn), 0)

    ! Determine dimensions and if grid file is 2d or 1d

    call ncd_inqfdims(ncid, isgrid2d, ni, nj, ns)
    if (masterproc) then
       write(iulog,*)'lat/lon grid flag (isgrid2d) is ',isgrid2d
    end if

    allocate(mask(ns))
    mask(:) = 1

    if (isgrid2d) then
       allocate(idata2d(ni,nj))
       idata2d(:,:) = 1	
       call ncd_io(ncid=ncid, varname='LANDMASK', data=idata2d, flag='read', readvar=readvar)
       if (.not. readvar) then
          call ncd_io(ncid=ncid, varname='mask', data=idata2d, flag='read', readvar=readvar)
       end if
       if (readvar) then
          do j = 1,nj
          do i = 1,ni
             n = (j-1)*ni + i	
             mask(n) = idata2d(i,j)
          enddo
          enddo
       end if
       deallocate(idata2d)
    else
       call ncd_io(ncid=ncid, varname='LANDMASK', data=mask, flag='read', readvar=readvar)
       if (.not. readvar) then
          call ncd_io(ncid=ncid, varname='mask', data=mask, flag='read', readvar=readvar)
       end if
    end if
    if (.not. readvar) call endrun( msg=' ERROR: landmask not on fatmlndfrc file'//errMsg(sourcefile, __LINE__))

    call ncd_pio_closefile(ncid)

  end subroutine surfrd_get_globmask

  !-----------------------------------------------------------------------
  subroutine surfrd_get_grid(begg, endg, ldomain, filename, glcfilename)
    !
    ! !DESCRIPTION:
    ! THIS IS CALLED AFTER THE DOMAIN DECOMPOSITION HAS BEEN CREATED
    ! Read the surface dataset grid related information:
    ! o real latitude  of grid cell (degrees)
    ! o real longitude of grid cell (degrees)
    !
    ! !USES:
    use clm_varcon, only : spval, re
    use domainMod , only : domain_type, domain_init, domain_clean, lon1d, lat1d
    use fileutils , only : getfil
    !
    ! !ARGUMENTS:
    integer          ,intent(in)    :: begg, endg 
    type(domain_type),intent(inout) :: ldomain   ! domain to init
    character(len=*) ,intent(in)    :: filename  ! grid filename
    character(len=*) ,optional, intent(in) :: glcfilename ! glc mask filename
    !
    ! !LOCAL VARIABLES:
    type(file_desc_t) :: ncid               ! netcdf id
    type(var_desc_t)  :: vardesc            ! variable descriptor
    integer :: beg                          ! local beg index
    integer :: end                          ! local end index
    integer :: ni,nj,ns                     ! size of grid on file
    integer :: dimid,varid                  ! netCDF id's
    integer :: start(1), count(1)           ! 1d lat/lon array sections
    integer :: ier,ret                      ! error status
    logical :: readvar                      ! true => variable is on input file 
    logical :: isgrid2d                     ! true => file is 2d lat/lon
    logical :: istype_domain                ! true => input file is of type domain
    real(r8), allocatable :: rdata2d(:,:)   ! temporary
    character(len=16) :: vname              ! temporary
    character(len=256):: locfn              ! local file name
    integer :: n                            ! indices
    real(r8):: eps = 1.0e-12_r8             ! lat/lon error tolerance
    character(len=32) :: subname = 'surfrd_get_grid'     ! subroutine name
!-----------------------------------------------------------------------

    if (masterproc) then
       if (filename == ' ') then
          write(iulog,*) trim(subname),' ERROR: filename must be specified '
          call endrun(msg=errMsg(sourcefile, __LINE__))
       endif
    end if

    call getfil( filename, locfn, 0 )
    call ncd_pio_openfile (ncid, trim(locfn), 0)

    ! Determine dimensions
    call ncd_inqfdims(ncid, isgrid2d, ni, nj, ns)

    ! Determine isgrid2d flag for domain
    call domain_init(ldomain, isgrid2d=isgrid2d, ni=ni, nj=nj, nbeg=begg, nend=endg)

    ! Determine type of file - old style grid file or new style domain file
    call check_var(ncid=ncid, varname='xc', vardesc=vardesc, readvar=readvar) 
    if (readvar)then
        istype_domain = .true.
    else
        istype_domain = .false.
    end if

    ! Read in area, lon, lat

    if (istype_domain) then
       call ncd_io(ncid=ncid, varname= 'area', flag='read', data=ldomain%area, &
            dim1name=grlnd, readvar=readvar)
       ! convert from radians**2 to km**2
       ldomain%area = ldomain%area * (re**2)
       if (.not. readvar) call endrun( msg=' ERROR: area NOT on file'//errMsg(sourcefile, __LINE__))
       
       call ncd_io(ncid=ncid, varname= 'xc', flag='read', data=ldomain%lonc, &
            dim1name=grlnd, readvar=readvar)
       if (.not. readvar) call endrun( msg=' ERROR: xc NOT on file'//errMsg(sourcefile, __LINE__))
       
       call ncd_io(ncid=ncid, varname= 'yc', flag='read', data=ldomain%latc, &
            dim1name=grlnd, readvar=readvar)
       if (.not. readvar) call endrun( msg=' ERROR: yc NOT on file'//errMsg(sourcefile, __LINE__))
    else
       call endrun( msg=" ERROR: can no longer read non domain files" )
    end if

    if (isgrid2d) then
       allocate(rdata2d(ni,nj), lon1d(ni), lat1d(nj))
       if (istype_domain) vname = 'xc'
       call ncd_io(ncid=ncid, varname=trim(vname), data=rdata2d, flag='read', readvar=readvar)
       lon1d(:) = rdata2d(:,1)
       if (istype_domain) vname = 'yc'
       call ncd_io(ncid=ncid, varname=trim(vname), data=rdata2d, flag='read', readvar=readvar)
       lat1d(:) = rdata2d(1,:)
       deallocate(rdata2d)
    end if

    ! Check lat limited to -90,90

    if (minval(ldomain%latc) < -90.0_r8 .or. &
        maxval(ldomain%latc) >  90.0_r8) then
       write(iulog,*) trim(subname),' WARNING: lat/lon min/max is ', &
            minval(ldomain%latc),maxval(ldomain%latc)
       ! call endrun( msg=' ERROR: lat is outside [-90,90]'//errMsg(sourcefile, __LINE__))
       ! write(iulog,*) trim(subname),' Limiting lat/lon to [-90/90] from ', &
       !     minval(domain%latc),maxval(domain%latc)
       ! where (ldomain%latc < -90.0_r8) ldomain%latc = -90.0_r8
       ! where (ldomain%latc >  90.0_r8) ldomain%latc =  90.0_r8
    endif
    if ( any(ldomain%lonc < 0.0_r8) )then
       call endrun( msg=' ERROR: lonc is negative and currently can NOT be (see https://github.com/ESCOMP/ctsm/issues/507)' &
                      //errMsg(sourcefile, __LINE__))
    endif

    call ncd_io(ncid=ncid, varname='mask', flag='read', data=ldomain%mask, &
         dim1name=grlnd, readvar=readvar)
    if (.not. readvar) then
       call endrun( msg=' ERROR: LANDMASK NOT on fracdata file'//errMsg(sourcefile, __LINE__))
    end if

    call ncd_io(ncid=ncid, varname='frac', flag='read', data=ldomain%frac, &
         dim1name=grlnd, readvar=readvar)
    if (.not. readvar) then
       call endrun( msg=' ERROR: LANDFRAC NOT on fracdata file'//errMsg(sourcefile, __LINE__))
    end if

    call ncd_pio_closefile(ncid)

  end subroutine surfrd_get_grid

  !-----------------------------------------------------------------------
  subroutine surfrd_get_data (begg, endg, ldomain, lfsurdat)
    !
    ! !DESCRIPTION:
    ! Read the surface dataset and create subgrid weights.
    ! The model's surface dataset recognizes 6 basic land cover types within a grid
    ! cell: lake, wetland, urban, glacier, glacier_mec and vegetated. The vegetated
    ! portion of the grid cell is comprised of up to [maxpatch_pft] patches. These
    ! subgrid patches are read in explicitly for each grid cell. This is in
    ! contrast to LSMv1, where the patches were built implicitly from biome types.
    !    o real latitude  of grid cell (degrees)
    !    o real longitude of grid cell (degrees)
    !    o integer surface type: 0 = ocean or 1 = land
    !    o integer soil color (1 to 20) for use with soil albedos
    !    o real soil texture, %sand, for thermal and hydraulic properties
    !    o real soil texture, %clay, for thermal and hydraulic properties
    !    o real % of cell covered by lake    for use as subgrid patch
    !    o real % of cell covered by wetland for use as subgrid patch
    !    o real % of cell that is urban      for use as subgrid patch
    !    o real % of cell that is glacier    for use as subgrid patch
    !    o real % of cell that is glacier_mec for use as subgrid patch
    !    o integer PFTs
    !    o real % abundance PFTs (as a percent of vegetated area)
    !
    ! !USES:
    use clm_varctl  , only : create_crop_landunit
    use fileutils   , only : getfil
    use domainMod   , only : domain_type, domain_init, domain_clean
    use clm_instur  , only : wt_lunit, topo_glc_mec
    !
    ! !ARGUMENTS:
    integer,          intent(in) :: begg, endg      
    type(domain_type),intent(in) :: ldomain     ! land domain
    character(len=*), intent(in) :: lfsurdat    ! surface dataset filename
    !
    ! !LOCAL VARIABLES:
    type(var_desc_t)  :: vardesc              ! pio variable descriptor
    type(domain_type) :: surfdata_domain      ! local domain associated with surface dataset
    character(len=256):: locfn                ! local file name
    integer           :: n                    ! loop indices
    integer           :: ni,nj,ns             ! domain sizes
    character(len=16) :: lon_var, lat_var     ! names of lat/lon on dataset
    logical           :: readvar              ! true => variable is on dataset
    real(r8)          :: rmaxlon,rmaxlat      ! local min/max vars
    type(file_desc_t) :: ncid                 ! netcdf id
    logical           :: istype_domain        ! true => input file is of type domain
    logical           :: isgrid2d             ! true => intut grid is 2d 
    character(len=32) :: subname = 'surfrd_get_data'    ! subroutine name
    !-----------------------------------------------------------------------

    if (masterproc) then
       write(iulog,*) 'Attempting to read surface boundary data .....'
       if (lfsurdat == ' ') then
          write(iulog,*)'lfsurdat must be specified'
          call endrun(msg=errMsg(sourcefile, __LINE__))
       endif
    endif

    wt_lunit(:,:) = 0._r8
    topo_glc_mec(:,:) = 0._r8

    ! Read surface data

    call getfil( lfsurdat, locfn, 0 )
    call ncd_pio_openfile (ncid, trim(locfn), 0)

    ! Read in patch mask - this variable is only on the surface dataset - but not
    ! on the domain dataset

    call ncd_io(ncid=ncid, varname= 'PFTDATA_MASK', flag='read', data=ldomain%pftm, &
         dim1name=grlnd, readvar=readvar)
    if (.not. readvar) call endrun( msg=' ERROR: pftm NOT on surface dataset'//errMsg(sourcefile, __LINE__))

    ! Check if fsurdat grid is "close" to fatmlndfrc grid, exit if lats/lon > 0.001

    call check_var(ncid=ncid, varname='xc', vardesc=vardesc, readvar=readvar) 
    if (readvar) then
       istype_domain = .true.
    else
       call check_var(ncid=ncid, varname='LONGXY', vardesc=vardesc, readvar=readvar) 
       if (readvar) then
          istype_domain = .false.
       else
          call endrun( msg=' ERROR: unknown domain type'//errMsg(sourcefile, __LINE__))
       end if
    end if
    if (istype_domain) then
       lon_var  = 'xc'
       lat_var  = 'yc'
    else
       lon_var  = 'LONGXY'
       lat_var  = 'LATIXY'
    end if
    if ( masterproc )then
       write(iulog,*) trim(subname),' lon_var = ',trim(lon_var),' lat_var =',trim(lat_var)
    end if

    call ncd_inqfdims(ncid, isgrid2d, ni, nj, ns)
    call domain_init(surfdata_domain, isgrid2d, ni, nj, begg, endg, clmlevel=grlnd)

    call ncd_io(ncid=ncid, varname=lon_var, flag='read', data=surfdata_domain%lonc, &
         dim1name=grlnd, readvar=readvar)
    if (.not. readvar) call endrun( msg=' ERROR: lon var NOT on surface dataset'//errMsg(sourcefile, __LINE__))

    call ncd_io(ncid=ncid, varname=lat_var, flag='read', data=surfdata_domain%latc, &
         dim1name=grlnd, readvar=readvar)
    if (.not. readvar) call endrun( msg=' ERROR: lat var NOT on surface dataset'//errMsg(sourcefile, __LINE__))

    rmaxlon = 0.0_r8
    rmaxlat = 0.0_r8
    do n = begg,endg
       if (ldomain%lonc(n)-surfdata_domain%lonc(n) > 300.) then
          rmaxlon = max(rmaxlon,abs(ldomain%lonc(n)-surfdata_domain%lonc(n)-360._r8))
       elseif (ldomain%lonc(n)-surfdata_domain%lonc(n) < -300.) then
          rmaxlon = max(rmaxlon,abs(ldomain%lonc(n)-surfdata_domain%lonc(n)+360._r8))
       else
          rmaxlon = max(rmaxlon,abs(ldomain%lonc(n)-surfdata_domain%lonc(n)))
       endif
       rmaxlat = max(rmaxlat,abs(ldomain%latc(n)-surfdata_domain%latc(n)))
    enddo
    if (rmaxlon > 0.001_r8 .or. rmaxlat > 0.001_r8) then
       write(iulog,*)' ERROR: surfdata/fatmgrid lon/lat mismatch error', rmaxlon,rmaxlat
       call endrun(msg=errMsg(sourcefile, __LINE__))
    end if

    !~! TODO(SPM, 022015) - if we deallocate and clean ldomain here, then you
    !~! get errors in htape_timeconst where the information is needed to write
    !~! the *.h0* file
    !~!call domain_clean(surfdata_domain)

    ! Obtain special landunit info

    call surfrd_special(begg, endg, ncid, ldomain%ns)

    ! Obtain vegetated landunit info

    call surfrd_veg_all(begg, endg, ncid, ldomain%ns)

    if (use_cndv) then
       call surfrd_veg_dgvm(begg, endg)
    end if

    call ncd_pio_closefile(ncid)

    call check_sums_equal_1(wt_lunit, begg, 'wt_lunit', subname)

    if ( masterproc )then
       write(iulog,*) 'Successfully read surface boundary data'
       write(iulog,*)
    end if

  end subroutine surfrd_get_data

!-----------------------------------------------------------------------
  subroutine surfrd_special(begg, endg, ncid, ns)
    !
    ! !DESCRIPTION:
    ! Determine weight with respect to gridcell of all special "patches" as well
    ! as soil color and percent sand and clay
    !
    ! !USES:
    use clm_varpar      , only : maxpatch_glcmec, nlevurb
    use landunit_varcon , only : isturb_MIN, isturb_MAX, istdlak, istwet, istice_mec
    use clm_instur      , only : wt_lunit, urban_valid, wt_glc_mec, topo_glc_mec
    use UrbanParamsType , only : CheckUrban
    !
    ! !ARGUMENTS:
    integer          , intent(in)    :: begg, endg 
    type(file_desc_t), intent(inout) :: ncid   ! netcdf id
    integer          , intent(in)    :: ns     ! domain size
    !
    ! !LOCAL VARIABLES:
    integer  :: n,nl,nurb,g                ! indices
    integer  :: dimid,varid                ! netCDF id's
    real(r8) :: nlevsoidata(nlevsoifl)
    logical  :: found                      ! temporary for error check
    integer  :: nindx                      ! temporary for error check
    integer  :: ier                        ! error status
    logical  :: readvar
    real(r8),pointer :: pctgla(:)      ! percent of grid cell is glacier
    real(r8),pointer :: pctlak(:)      ! percent of grid cell is lake
    real(r8),pointer :: pctwet(:)      ! percent of grid cell is wetland
    real(r8),pointer :: pcturb(:,:)    ! percent of grid cell is urbanized
    integer ,pointer :: urban_region_id(:)
    real(r8),pointer :: pcturb_tot(:)  ! percent of grid cell is urban (sum over density classes)
    real(r8),pointer :: pctspec(:)     ! percent of spec lunits wrt gcell
    integer  :: dens_index             ! urban density index
    character(len=32) :: subname = 'surfrd_special'  ! subroutine name
    real(r8) closelat,closelon
    integer, parameter :: urban_invalid_region = 0   ! urban_region_id indicating invalid point
!-----------------------------------------------------------------------

    allocate(pctgla(begg:endg))
    allocate(pctlak(begg:endg))
    allocate(pctwet(begg:endg))
    allocate(pcturb(begg:endg,numurbl))
    allocate(pcturb_tot(begg:endg))
    allocate(urban_region_id(begg:endg))
    allocate(pctspec(begg:endg))

    call check_dim(ncid, 'nlevsoi', nlevsoifl)

       ! Obtain non-grid surface properties of surface dataset other than percent patch

    call ncd_io(ncid=ncid, varname='PCT_WETLAND', flag='read', data=pctwet, &
         dim1name=grlnd, readvar=readvar)
    if (.not. readvar) call endrun( msg=' ERROR: PCT_WETLAND  NOT on surfdata file'//errMsg(sourcefile, __LINE__))

    call ncd_io(ncid=ncid, varname='PCT_LAKE'   , flag='read', data=pctlak, &
         dim1name=grlnd, readvar=readvar)
    if (.not. readvar) call endrun( msg=' ERROR: PCT_LAKE NOT on surfdata file'//errMsg(sourcefile, __LINE__))

    call ncd_io(ncid=ncid, varname='PCT_GLACIER', flag='read', data=pctgla, &
         dim1name=grlnd, readvar=readvar)
    if (.not. readvar) call endrun( msg=' ERROR: PCT_GLACIER NOT on surfdata file'//errMsg(sourcefile, __LINE__))

    ! Read urban info
    if (nlevurb == 0) then
      ! If PCT_URBAN is not multi-density then set pcturb to zero 
      pcturb = 0._r8
      urban_valid(begg:endg) = .false.
      write(iulog,*)'PCT_URBAN is not multi-density, pcturb set to 0'
    else
      call ncd_io(ncid=ncid, varname='PCT_URBAN'  , flag='read', data=pcturb, &
           dim1name=grlnd, readvar=readvar)
      if (.not. readvar) call endrun( msg=' ERROR: PCT_URBAN NOT on surfdata file'//errMsg(sourcefile, __LINE__))

      call ncd_io(ncid=ncid, varname='URBAN_REGION_ID', flag='read', data=urban_region_id, &
           dim1name=grlnd, readvar=readvar)
      if (.not. readvar) call endrun( msg= ' ERROR: URBAN_REGION_ID NOT on surfdata file'//errMsg(sourcefile, __LINE__))
      where (urban_region_id == urban_invalid_region)
         urban_valid = .false.
      elsewhere
         urban_valid = .true.
      end where
    end if
    if ( nlevurb == 0 )then
       if ( any(pcturb > 0.0_r8) ) then
          call endrun( msg=' ERROR: PCT_URBAN MUST be zero when nlevurb=0'//errMsg(sourcefile, __LINE__))
       end if
    end if

    pcturb_tot(:) = 0._r8
    do n = 1, numurbl
       do nl = begg,endg
          pcturb_tot(nl) = pcturb_tot(nl) + pcturb(nl,n)
       enddo
    enddo

    ! Read glacier info

    call check_dim(ncid, 'nglcec',   maxpatch_glcmec   )
    call check_dim(ncid, 'nglcecp1', maxpatch_glcmec+1 )

    call ncd_io(ncid=ncid, varname='PCT_GLC_MEC', flag='read', data=wt_glc_mec, &
         dim1name=grlnd, readvar=readvar)
    if (.not. readvar) call endrun( msg=' ERROR: PCT_GLC_MEC NOT on surfdata file'//errMsg(sourcefile, __LINE__))

    wt_glc_mec(:,:) = wt_glc_mec(:,:) / 100._r8
    call check_sums_equal_1(wt_glc_mec, begg, 'wt_glc_mec', subname)

    call ncd_io(ncid=ncid, varname='TOPO_GLC_MEC',  flag='read', data=topo_glc_mec, &
         dim1name=grlnd, readvar=readvar)
    if (.not. readvar) call endrun( msg=' ERROR: TOPO_GLC_MEC NOT on surfdata file'//errMsg(sourcefile, __LINE__))

    topo_glc_mec(:,:) = max(topo_glc_mec(:,:), 0._r8)

    pctspec = pctwet + pctlak + pcturb_tot + pctgla

    ! Error check: glacier, lake, wetland, urban sum must be less than 100

    found = .false.
    do nl = begg,endg
       if (pctspec(nl) > 100._r8+1.e-04_r8) then
          found = .true.
          nindx = nl
          exit
       end if
       if (found) exit
    end do
    if ( found ) then
       write(iulog,*)'surfrd error: patch cover>100 for nl=',nindx
       call endrun(msg=errMsg(sourcefile, __LINE__))
    end if

    ! Determine wt_lunit for special landunits

    do nl = begg,endg

       wt_lunit(nl,istdlak)     = pctlak(nl)/100._r8

       wt_lunit(nl,istwet)      = pctwet(nl)/100._r8

       wt_lunit(nl,istice_mec)  = pctgla(nl)/100._r8

       do n = isturb_MIN, isturb_MAX
          dens_index = n - isturb_MIN + 1
          wt_lunit(nl,n)        = pcturb(nl,dens_index) / 100._r8
       end do

    end do

    call CheckUrban(begg, endg, pcturb(begg:endg,:), subname)

    deallocate(pctgla,pctlak,pctwet,pcturb,pcturb_tot,urban_region_id,pctspec)

  end subroutine surfrd_special

!-----------------------------------------------------------------------
  subroutine surfrd_cftformat( ncid, begg, endg, wt_cft, cftsize, natpft_size )
    !
    ! !DESCRIPTION:
    !     Handle generic crop types for file format where they are on their own
    !     crop landunit and read in as Crop Function Types.
    ! !USES:
    use clm_instur      , only : fert_cft, wt_nat_patch
    use clm_varpar      , only : cft_size, cft_lb, natpft_lb
    ! !ARGUMENTS:
    implicit none
    type(file_desc_t), intent(inout) :: ncid         ! netcdf id
    integer          , intent(in)    :: begg, endg
    integer          , intent(in)    :: cftsize      ! CFT size
    real(r8), pointer, intent(inout) :: wt_cft(:,:)  ! CFT weights
    integer          , intent(in)    :: natpft_size  ! natural PFT size
    !
    ! !LOCAL VARIABLES:
    logical  :: readvar                        ! is variable on dataset
    real(r8),pointer :: array2D(:,:)              ! local array
    character(len=32) :: subname = 'surfrd_cftformat'! subroutine name
!-----------------------------------------------------------------------
    SHR_ASSERT_ALL((lbound(wt_cft) == (/begg, cft_lb/)), errMsg(sourcefile, __LINE__))
    SHR_ASSERT_ALL((ubound(wt_cft, dim=1) == (/endg/)), errMsg(sourcefile, __LINE__))
    SHR_ASSERT_ALL((ubound(wt_cft, dim=2) >= (/cftsize+1-cft_lb/)), errMsg(sourcefile, __LINE__))
    SHR_ASSERT_ALL((ubound(wt_nat_patch) >= (/endg,natpft_size-1+natpft_lb/)), errMsg(sourcefile, __LINE__))

    call check_dim(ncid, 'cft', cftsize)
    call check_dim(ncid, 'natpft', natpft_size)

    call ncd_io(ncid=ncid, varname='PCT_CFT', flag='read', data=wt_cft, &
            dim1name=grlnd, readvar=readvar)
    if (.not. readvar) call endrun( msg=' ERROR: PCT_CFT NOT on surfdata file'//errMsg(sourcefile, __LINE__)) 

    if ( cft_size > 0 )then
       call ncd_io(ncid=ncid, varname='CONST_FERTNITRO_CFT', flag='read', data=fert_cft, &
               dim1name=grlnd, readvar=readvar)
       if (.not. readvar) then
          if ( masterproc ) &
                write(iulog,*) ' WARNING: CONST_FERTNITRO_CFT NOT on surfdata file zero out'
          fert_cft = 0.0_r8
       end if
    else
       fert_cft = 0.0_r8
    end if

    allocate( array2D(begg:endg,1:natpft_size) )
    call ncd_io(ncid=ncid, varname='PCT_NAT_PFT', flag='read', data=array2D, &
         dim1name=grlnd, readvar=readvar)
    if (.not. readvar) call endrun( msg=' ERROR: PCT_NAT_PFT NOT on surfdata file'//errMsg(sourcefile, __LINE__))
    wt_nat_patch(begg:,natpft_lb:natpft_size-1+natpft_lb) = array2D(begg:,:)
    deallocate( array2D )

  end subroutine surfrd_cftformat

!-----------------------------------------------------------------------
  subroutine surfrd_pftformat( begg, endg, ncid )
    !
    ! !DESCRIPTION:
    !     Handle generic crop types for file format where they are part of the
    !     natural vegetation landunit.
    ! !USES:
    use clm_instur      , only : fert_cft, wt_nat_patch
    use clm_varpar      , only : natpft_size, cft_size, natpft_lb
    ! !ARGUMENTS:
    implicit none
    integer, intent(in) :: begg, endg
    type(file_desc_t), intent(inout) :: ncid                    ! netcdf id
    !
    ! !LOCAL VARIABLES:
    logical  :: cft_dim_exists                 ! does the dimension 'cft' exist on the dataset?
    integer  :: dimid                          ! netCDF id's
    logical  :: readvar                        ! is variable on dataset
    character(len=32) :: subname = 'surfrd_pftformat'! subroutine name
!-----------------------------------------------------------------------
    SHR_ASSERT_ALL((ubound(wt_nat_patch) == (/endg, natpft_size-1+natpft_lb/)), errMsg(sourcefile, __LINE__))

    call check_dim(ncid, 'natpft', natpft_size)
    ! If cft_size == 0, then we expect to be running with a surface dataset
    ! that does
    ! NOT have a PCT_CFT array (or CONST_FERTNITRO_CFT array), and thus does not have a 'cft' dimension.
    ! Make sure
    ! that's the case.
    call ncd_inqdid(ncid, 'cft', dimid, cft_dim_exists)
    if (cft_dim_exists) then
       call endrun( msg= ' ERROR: unexpectedly found cft dimension on dataset when cft_size=0'// &
               ' (if the surface dataset has a separate crop landunit, then the code'// &
               ' must also have a separate crop landunit, and vice versa)'//&
               errMsg(sourcefile, __LINE__))
    end if
    call ncd_io(ncid=ncid, varname='CONST_FERTNITRO_CFT', flag='read', data=fert_cft, &
            dim1name=grlnd, readvar=readvar)
    if (readvar) then
       call endrun( msg= ' ERROR: unexpectedly found CONST_FERTNITRO_CFT on dataset when cft_size=0'// &
               ' (if the surface dataset has a separate crop landunit, then the code'// &
               ' must also have a separate crop landunit, and vice versa)'//&
               errMsg(sourcefile, __LINE__))
    end if
    fert_cft = 0.0_r8

    call ncd_io(ncid=ncid, varname='PCT_NAT_PFT', flag='read', data=wt_nat_patch, &
         dim1name=grlnd, readvar=readvar)
    if (.not. readvar) call endrun( msg=' ERROR: PCT_NAT_PFT NOT on surfdata file'//errMsg(sourcefile, __LINE__))

  end subroutine surfrd_pftformat

!-----------------------------------------------------------------------
  subroutine surfrd_veg_all(begg, endg, ncid, ns)
    !
    ! !DESCRIPTION:
    ! Determine weight arrays for non-dynamic landuse mode
    !
    ! !USES:
    use clm_varctl      , only : create_crop_landunit, use_fates
    use clm_varpar      , only : natpft_lb, natpft_ub, natpft_size, cft_size, cft_lb
    use clm_instur      , only : wt_lunit, wt_nat_patch, wt_cft, fert_cft
!Edit by Lei Cai--start
    use landunit_varcon , only : istsoil, istsoil_li, istsoil_mi, istsoil_hi, istcrop
!Edit by Lei Cai--end
    use surfrdUtilsMod  , only : convert_cft_to_pft
    !
    ! !ARGUMENTS:
    implicit none
    integer, intent(in) :: begg, endg
    type(file_desc_t),intent(inout) :: ncid   ! netcdf id
    integer          ,intent(in)    :: ns     ! domain size
    !
    ! !LOCAL VARIABLES:
    integer  :: dimid                                ! netCDF id's
    integer  :: cftsize                              ! size of CFT's
    logical  :: readvar                              ! is variable on dataset
    logical  :: cft_dim_exists                       ! does the dimension 'cft' exist on the dataset?
    real(r8),pointer :: arrayl(:)                    ! local array
    real(r8),pointer :: array2D(:,:)                 ! local 2D array
    character(len=32) :: subname = 'surfrd_veg_all'  ! subroutine name
!-----------------------------------------------------------------------
    !
    ! Read in variables that are handled the same for all formats
    !
    ! Check dimension size
    call check_dim(ncid, 'lsmpft', numpft+1)

    ! This temporary array is needed because ncd_io expects a pointer, so we can't
    ! directly pass wt_lunit(begg:endg,istsoil)
    allocate(arrayl(begg:endg))

!Edit by Lei Cai--start
    call ncd_io(ncid=ncid, varname='PCT_NATVEG_NI', flag='read', data=arrayl, &
         dim1name=grlnd, readvar=readvar)
    if (.not. readvar) call endrun( msg=' ERROR: PCT_NATVEG_NI NOT on surfdata file'//errMsg(sourcefile, __LINE__))
    wt_lunit(begg:endg,istsoil) = arrayl(begg:endg)

    call ncd_io(ncid=ncid, varname='PCT_NATVEG_LI', flag='read', data=arrayl, &
         dim1name=grlnd, readvar=readvar)
    if (.not. readvar) call endrun( msg=' ERROR: PCT_NATVEG_LI NOT on surfdata file'//errMsg(sourcefile, __LINE__))
    wt_lunit(begg:endg,istsoil_li) = arrayl(begg:endg)

	call ncd_io(ncid=ncid, varname='PCT_NATVEG_MI', flag='read', data=arrayl, &
         dim1name=grlnd, readvar=readvar)
    if (.not. readvar) call endrun( msg=' ERROR: PCT_NATVEG_MI NOT on surfdata file'//errMsg(sourcefile, __LINE__))
    wt_lunit(begg:endg,istsoil_mi) = arrayl(begg:endg)
	
	call ncd_io(ncid=ncid, varname='PCT_NATVEG_HI', flag='read', data=arrayl, &
         dim1name=grlnd, readvar=readvar)
    if (.not. readvar) call endrun( msg=' ERROR: PCT_NATVEG_HI NOT on surfdata file'//errMsg(sourcefile, __LINE__))
    wt_lunit(begg:endg,istsoil_hi) = arrayl(begg:endg)
!Edit by Lei Cai--end

!    call ncd_io(ncid=ncid, varname='PCT_NATVEG', flag='read', data=arrayl, &
!         dim1name=grlnd, readvar=readvar)
!    if (.not. readvar) call endrun( msg=' ERROR: PCT_NATVEG NOT on surfdata file'//errMsg(sourcefile, __LINE__))
!    wt_lunit(begg:endg,istsoil) = arrayl(begg:endg)

    call ncd_io(ncid=ncid, varname='PCT_CROP', flag='read', data=arrayl, &
         dim1name=grlnd, readvar=readvar)
    if (.not. readvar) call endrun( msg=' ERROR: PCT_CROP NOT on surfdata file'//errMsg(sourcefile, __LINE__))
    wt_lunit(begg:endg,istcrop) = arrayl(begg:endg)

    deallocate(arrayl)

    ! Check the file format for CFT's and handle accordingly
    call ncd_inqdid(ncid, 'cft', dimid, cft_dim_exists)
    if ( cft_dim_exists .and. create_crop_landunit )then
       call surfrd_cftformat( ncid, begg, endg, wt_cft, cft_size, natpft_size )  ! Format where CFT's is read in a seperate landunit
    else if ( (.not. cft_dim_exists) .and. (.not. create_crop_landunit) )then
       if ( masterproc ) write(iulog,*) "WARNING: The PFT format is an unsupported format that will be removed in th future!"
       call surfrd_pftformat( begg, endg, ncid )                                 ! Format where crop is part of the natural veg. landunit
    else if ( cft_dim_exists .and. .not. create_crop_landunit )then
       if ( masterproc ) write(iulog,*) "WARNING: New CFT-based format surface datasets should be run with create_crop_landunit=T"
       if ( use_fates ) then
          if ( masterproc ) write(iulog,*) "WARNING: When fates is on we allow new CFT based surface datasets ", &
                                           "to be used with create_crop_land FALSE"
          cftsize = 2
          allocate(array2D(begg:endg,cft_lb:cftsize-1+cft_lb))
          call surfrd_cftformat( ncid, begg, endg, array2D, cftsize, natpft_size-cftsize ) ! Read crops in as CFT's
          call convert_cft_to_pft( begg, endg, cftsize, array2D )                          ! Convert from CFT to natural veg. landunit
          deallocate(array2D)
       else
          call endrun( msg=' ERROR: New format surface datasets require create_crop_landunit TRUE'//errMsg(sourcefile, __LINE__))
       end if
    end if

    ! Do some checking
    
    if ( (cft_size == 0) .and. any(wt_lunit(begg:endg,istcrop) > 0._r8) ) then
       call endrun( msg=' ERROR: if PCT_CROP > 0 anywhere, then cft_size must be > 0'// &
               ' (if the surface dataset has a separate crop landunit, then the code'// &
               ' must also have a separate crop landunit, and vice versa)'//&
               errMsg(sourcefile, __LINE__))
    end if
    ! Convert from percent to fraction, check sums of nat vegetation add to 1
    if ( cft_size > 0 )then
       wt_cft(begg:endg,:) = wt_cft(begg:endg,:) / 100._r8
       call check_sums_equal_1(wt_cft, begg, 'wt_cft', subname)
    end if
    wt_lunit(begg:endg,istsoil) = wt_lunit(begg:endg,istsoil) / 100._r8
!Edit by Lei Cai--start
    wt_lunit(begg:endg,istsoil_li) = wt_lunit(begg:endg,istsoil_li) / 100._r8
	wt_lunit(begg:endg,istsoil_mi) = wt_lunit(begg:endg,istsoil_mi) / 100._r8
	wt_lunit(begg:endg,istsoil_hi) = wt_lunit(begg:endg,istsoil_hi) / 100._r8
!Edit by Lei Cai--end	
    wt_lunit(begg:endg,istcrop) = wt_lunit(begg:endg,istcrop) / 100._r8
    wt_nat_patch(begg:endg,:)   = wt_nat_patch(begg:endg,:) / 100._r8
    call check_sums_equal_1(wt_nat_patch, begg, 'wt_nat_patch', subname)

    ! Collapse crop landunits down when prognostic crops are on
    if (use_crop) then
       call collapse_crop_types(wt_cft(begg:endg, :), fert_cft(begg:endg, :), begg, endg, verbose=.true.)
    end if

  end subroutine surfrd_veg_all

  !-----------------------------------------------------------------------
  subroutine surfrd_veg_dgvm(begg, endg)
    !
    ! !DESCRIPTION:
    ! Determine weights for CNDV mode.
    !
    ! !USES:
    use pftconMod , only : noveg
    use clm_instur, only : wt_nat_patch
    !
    ! !ARGUMENTS:
    integer, intent(in) :: begg, endg  
    !
    ! !LOCAL VARIABLES:
    character(len=*), parameter :: subname = 'surfrd_veg_dgvm'
    !-----------------------------------------------------------------------

    ! Bare ground gets 100% weight; all other natural patches are zeroed out
    wt_nat_patch(begg:endg, :)     = 0._r8
    wt_nat_patch(begg:endg, noveg) = 1._r8

    call check_sums_equal_1(wt_nat_patch, begg, 'wt_nat_patch', subname)

  end subroutine surfrd_veg_dgvm

end module surfrdMod
