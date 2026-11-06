C
C*************************** COMMON BLOCK MMM_CBLOCK *******************
C
C Common block for all shared variables associated specifically with
C the Michaelis-Menten-Monod (MMM) scheme.
C
C MMM_FLAG (integer scalar)
C   = 1 to activate the MMM scheme.
C   = 0 to skip all MMM scheme input and processing.
C   When MMM_FLAG = 0, MMM_NCOMP is set to 1 (and also MMM_MOB_FLAG(1) and
C   MMM_UPTAKE_FLAG(1) are set to 1), so that in the case of a transport
C   simulation (TRAFLAG = 1, or identically TRANSP = TRUE) but with no MMM,
C   the same data structure is used as for the case when MMM_FLAG = 1.
C MMM_NRC (integer scalar)
C   Number of reactions.
C   The corresponding dimensioning parameter in CATHY.H is MMM_MAX_NRC.
C MMM_NM (integer scalar)
C   Number of molecules.
C   The corresponding dimensioning parameter in CATHY.H is MMM_MAX_NM.
C MMM_NBG (integer scalar)
C   Number of bacteria groups.
C   The corresponding dimensioning parameter in CATHY.H is MMM_MAX_NBG.
C MMM_NCOMP (integer scalar)
C   Number of components (or species), equal to MMM_NM + MMM_NBG.
C   The corresponding dimensioning parameter in CATHY.H is MMM_MAX_NCOMP, which
C   is set to MMM_MAX_NM + MMM_MAX_NBG.
C MMM_MOBILE_COUNTER (integer scalar)
C   Counter for mobile components needed in the "MMM_NCOMP loop" block.
C
C MMM_MOB_FLAG (integer array, dimension MMM_MAX_NCOMP)
C   Flag indicating whether a molecule (first MMM_NM entries) or bacteria group
C   (entries MMM_NM + 1 to MMM_NM + MMM_NBG) are "mobile" or not. Subsurface
C   transport (advection-dispersion-transport, or ADR) is skipped for components
C   that are not mobile.
C   = 1 for a mobile component.
C   = 0 for a component that is not mobile.
C MMM_UPTAKE_FLAG (integer array, dimension MMM_MAX_NCOMP)
C   Flag indicating whether a molecule (first MMM_NM entries) or bacteria group
C   (entries MMM_NM + 1 to MMM_NM + MMM_NBG) are susceptible to plant uptake
C   with the sink term computed from the RWU / transpiration module.
C   = 1 for subject to uptake.
C   = 0 for not subject to uptake.
C
C MMM_SC_INHIB (real scalar, units [???])
C   Accumulator for the inhibition contribution in the MMM scheme.
C MMM_SC_KM (real scalar, units [???])
C   Accumulator for the K_m contribution in the MMM scheme.
C MMM_TMP (real scalar, units [???])
C   Scratch scalar used in the MMM scheme.
C
C MMM_CONVERT (real array, dimension MMM_MAX_NM, units [mg/mol])
C   Conversion factors to switch molecule concentrations from [mg/l] in the
C   ADR solver block to [mol/l] needed in the MMM scheme block, and then back
C   to [mg/l] at the end of the MMM block.
C MMM_ST_COEFF (real array, dimension MMM_MAX_NRC x MMM_MAX_NM, units [-])
C   Stoichiometric coefficients for each reaction.
C MMM_RC_RATE (real array, dimension MMM_MAX_NRC, units [mol/l/T] where T = s)
C   Rate of each reaction.
C MMM_INHIB (real array, dimension MMM_MAX_NRC x MMM_MAX_NM, units [mol/l])
C   Inhibition coefficients for each reaction. MMM_INHIB(i,j) = 0.0 means that
C   molecule "j" is not active with regards to reaction "i".
C MMM_K_M (real array, dimension MMM_MAX_NRC x MMM_MAX_NM, units [mol/l])
C   Michaelis constants K_m for each reaction.
C MMM_INTER (real array, dimension MMM_MAX_NRC x MMM_MAX_NM x MMM_MAX_NM,
C            units [-])
C   For each reaction, the embedded MMM_NM x MMM_NM array describes how the
C   molecules interact (or compete) with each other. The diagonal of each of
C   the NRC embedded matrices is zero. 
C MMM_BIO_Y (real array, dimension MMM_MAX_NRC x MMM_MAX_NBG,
C            units [mg-wet-bio/mol])
C   Biomass yield coefficients for each reaction. MMM_BIO_Y(i,j) = 0.0 means
C   that bacteria group "j" is not active with regards to reaction "i".
C MMM_BMORT (real array, dimension MMM_MAX_NBG, units [1/T] where T = s)
C   Bacteria mortality coefficient.
C
C [ The "concentration" and related arrays in this next block are dimensioned
C   to MMM_MAX_NCOMP x "q" (where "q" is NMAX for node-wise variables, NTEMAX
C   for element-wise variables, NODMAX for surface-node-wise variables, and
C   MAXCEL for surface-cell-wise variables).
C   Appropriate indexing is used to track and evolve molecules (first MMM_NM
C   rows) and bacteria groups (rows MMM_NM + 1 to MMM_NM + MMM_NBG).
C   All "concentration" arrays below are in units [mg/l]. The equations for
C   the MMM scheme are in [mol/l] for molecules (contaminant, substrate,
C   reagent, dissolved oxygen, etc concentrations) and in [mg/l] for bacteria
C   groups (microbial biomass concentrations). Thus at the beginning of the
C   MMM scheme block we divide molecule concentrations by the conversion factors
C   in MMM_CONVERT, and at the end of the block we multiply by these conversion
C   factors. ]
C MMM_CNNEW (real array, dimension MMM_MAX_NCOMP x NMAX, units [mg/l])
C   ...
C MMM_CNOLD (real array, dimension MMM_MAX_NCOMP x NMAX, units [mg/l])
C   ...
C MMM_CNEW (real array, dimension MMM_MAX_NCOMP x NTEMAX, units [mg/l])
C   ...
C MMM_COLD (real array, dimension MMM_MAX_NCOMP x NTEMAX, units [mg/l])
C   ...
C MMM_COLDOLD (real array, dimension MMM_MAX_NCOMP x NTEMAX, units [mg/l])
C   ...
C MMM_CDIFF (real array, dimension MMM_MAX_NCOMP x NTEMAX, units [mg/l])
C   ...
C MMM_FLUX (real array, dimension MMM_MAX_NCOMP x NTEMAX, units [???/???])
C   ...
C MMM_SAVED_SWNEW (real array, dimension NTEMAX, units ...)
C   ...
C MMM_SAVED_SWOLD (real array, dimension NTEMAX, units ...)
C   ...
C MMM_CONCNOD_PERDIR (real array, dimension MMM_MAX_NCOMP x NODMAX, units ...)
C   ...
C MMM_CONCNOD_PERDIR_TIMEP (real array, dimension MMM_MAX_NCOMP x NODMAX,
C                           units ...)
C   ...
C MMM_CONCNOD_PERDIR_INT (real array, dimension MMM_MAX_NCOMP x NODMAX,
C                         units ...)
C   ...
C MMM_CONCNODUPD (real array, dimension MMM_MAX_NCOMP x NODMAX, units ...)
C   ...
C MMM_TRAFLNOD (real array, dimension MMM_MAX_NCOMP x NODMAX, units ...)
C   ...
C MMM_TRAFLNOD_FLOW (real array, dimension MMM_MAX_NCOMP x NODMAX, units ...)
C   ...
C MMM_SOURCE_MIXING (real array, dimension MMM_MAX_NCOMP x NODMAX, units ...)
C   ...
C MMM_MIX_CORRECT (real array, dimension MMM_MAX_NCOMP x MAXCEL, units ...)
C   ...
C
C MMM_MASSINTOTADV (real array, dimension MMM_MAX_NCOMP, units ...)
C   ...
C MMM_MASSOUTTOTADV (real array, dimension MMM_MAX_NCOMP, units ...)
C   ...
C MMM_MASSINTOTCUM (real array, dimension MMM_MAX_NCOMP, units ...)
C   ...
C MMM_MASSOUTTOTCUM (real array, dimension MMM_MAX_NCOMP, units ...)
C   ...
C MMM_MASST (real array, dimension MMM_MAX_NCOMP, units [mg??])
C   Total mass of each component in the subsurface.
C
C MMM_UPTAKE_N (real array, dimension NMAX, units [M/T])
C   Mass sink term at each node of the subsurface domain for plant uptake, as
C   calculated from the RWU / transpiration module.
C MMM_UPTAKE_NT (real array, dimension NTEMAX, units [M/T])
C   Mass sink term at each element of the subsurface domain for plant uptake, as
C   calculated from the RWU / transpiration module.
C MMM_UPTAKE_CUM (real array, dimension MMM_MAX_NCOMP, units [M/T])
C   Cumulative (over the entire subsurface domain) mass sink flux.
C
C MMM_DELTAC (real array, dimension MMM_MAX_NRC, units [???])
C   "Delta C" term in the MMM scheme.
C MMM_SCRATCH_N (real array, dimension NMAX)
C   Scratch array used to temporarily store node-based variables.
C MMM_SCRATCH_NT (real array, dimension NTEMAX)
C   Scratch array used to temporarily store element-based variables.
C
C MMM_RC_NAMES (character array, dimension MMM_MAX_NRC)
C   Reaction names.
C MMM_MOL_NAMES (character array, dimension MMM_MAX_NM)
C   Molecule names.
C MMM_BG_NAMES (character array, dimension MMM_MAX_NBG)
C   Bacteria group names.
C
C***********************************************************************
C
      INTEGER       MMM_FLAG, MMM_NRC, MMM_NM, MMM_NBG, MMM_NCOMP
      INTEGER       MMM_MOBILE_COUNTER
C
      INTEGER       MMM_MOB_FLAG(MMM_MAX_NCOMP)
      INTEGER       MMM_UPTAKE_FLAG(MMM_MAX_NCOMP)
C
      REAL*8        MMM_SC_INHIB, MMM_SC_KM, MMM_TMP
C
      REAL*8        MMM_CONVERT(MMM_MAX_NM)
      REAL*8        MMM_ST_COEFF(MMM_MAX_NRC,MMM_MAX_NM)
      REAL*8        MMM_RC_RATE(MMM_MAX_NRC)
      REAL*8        MMM_INHIB(MMM_MAX_NRC,MMM_MAX_NM)
      REAL*8        MMM_K_M(MMM_MAX_NRC,MMM_MAX_NM)
      REAL*8        MMM_INTER(MMM_MAX_NRC,MMM_MAX_NM,MMM_MAX_NM)
      REAL*8        MMM_BIO_Y(MMM_MAX_NRC,MMM_MAX_NBG)
      REAL*8        MMM_BMORT(MMM_MAX_NBG)
C
      REAL*8        MMM_CNNEW(MMM_MAX_NCOMP,NMAX)
      REAL*8        MMM_CNOLD(MMM_MAX_NCOMP,NMAX)
      REAL*8        MMM_CNEW(MMM_MAX_NCOMP,NTEMAX)
      REAL*8        MMM_COLD(MMM_MAX_NCOMP,NTEMAX)
      REAL*8        MMM_COLDOLD(MMM_MAX_NCOMP,NTEMAX)
      REAL*8        MMM_CDIFF(MMM_MAX_NCOMP,NTEMAX)
      REAL*8        MMM_FLUX(MMM_MAX_NCOMP,NTEMAX)
      REAL*8        MMM_SAVED_SWNEW(NTEMAX)
      REAL*8        MMM_SAVED_SWOLD(NTEMAX)
      REAL*8        MMM_CONCNOD_PERDIR(MMM_MAX_NCOMP,NODMAX)
      REAL*8        MMM_CONCNOD_PERDIR_TIMEP(MMM_MAX_NCOMP,NODMAX)
      REAL*8        MMM_CONCNOD_PERDIR_INT(MMM_MAX_NCOMP,NODMAX)
      REAL*8        MMM_CONCNODUPD(MMM_MAX_NCOMP,NODMAX)
      REAL*8        MMM_TRAFLNOD(MMM_MAX_NCOMP,NODMAX)
      REAL*8        MMM_TRAFLNOD_FLOW(MMM_MAX_NCOMP,NODMAX)
      REAL*8        MMM_SOURCE_MIXING(MMM_MAX_NCOMP,NODMAX)
      REAL*8        MMM_MIX_CORRECT(MMM_MAX_NCOMP,MAXCEL)
C
      REAL*8        MMM_MASSINTOTADV(MMM_MAX_NCOMP)
      REAL*8        MMM_MASSOUTTOTADV(MMM_MAX_NCOMP)
      REAL*8        MMM_MASSINTOTCUM(MMM_MAX_NCOMP)
      REAL*8        MMM_MASSOUTTOTCUM(MMM_MAX_NCOMP)
      REAL*8        MMM_MASST(MMM_MAX_NCOMP)
C
      REAL*8        MMM_UPTAKE_N(NMAX)
      REAL*8        MMM_UPTAKE_NT(NTEMAX)
      REAL*8        MMM_UPTAKE_CUM(MMM_MAX_NCOMP)
C
      REAL*8        MMM_DELTAC(MMM_MAX_NRC)
      REAL*8        MMM_SCRATCH_N(NMAX)
      REAL*8        MMM_SCRATCH_NT(NTEMAX)
C
      CHARACTER*60  MMM_RC_NAMES(MMM_MAX_NRC)
      CHARACTER*60  MMM_MOL_NAMES(MMM_MAX_NM)
      CHARACTER*60  MMM_BG_NAMES(MMM_MAX_NBG)
C
      COMMON  /MMM_CBLOCK/
     1        MMM_CONVERT, MMM_ST_COEFF, MMM_RC_RATE, MMM_INHIB,
     2        MMM_K_M, MMM_INTER, MMM_BIO_Y, MMM_BMORT,
     3        MMM_CNNEW, MMM_CNOLD, MMM_CNEW, MMM_COLD,
     4        MMM_COLDOLD, MMM_CDIFF, MMM_FLUX,
     5        MMM_SAVED_SWNEW, MMM_SAVED_SWOLD,
     6        MMM_CONCNOD_PERDIR, MMM_CONCNOD_PERDIR_TIMEP,
     7        MMM_CONCNOD_PERDIR_INT, MMM_CONCNODUPD,
     8        MMM_TRAFLNOD, MMM_TRAFLNOD_FLOW,
     9        MMM_SOURCE_MIXING, MMM_MIX_CORRECT,
     A        MMM_MASSINTOTADV, MMM_MASSOUTTOTADV,
     B        MMM_MASSINTOTCUM, MMM_MASSOUTTOTCUM, MMM_MASST,
     C        MMM_UPTAKE_N, MMM_UPTAKE_NT, MMM_UPTAKE_CUM,
     D        MMM_DELTAC, MMM_SCRATCH_N, MMM_SCRATCH_NT,
     E        MMM_SC_INHIB, MMM_SC_KM, MMM_TMP,
     F        MMM_MOB_FLAG, MMM_UPTAKE_FLAG,
     G        MMM_FLAG, MMM_NRC, MMM_NM, MMM_NBG, MMM_NCOMP,
     H        MMM_MOBILE_COUNTER,
     I        MMM_RC_NAMES, MMM_MOL_NAMES, MMM_BG_NAMES
