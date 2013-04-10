" Vim syntax file
" Language:	FreeFem++
" Maintainer:	Richard Michel <Richard.Michel@lepmi.inpg.fr>
" Last Change:	2007 May 03 (for FreeFem++ v2.16-2)

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Read the Cpp syntax to start with
if version < 600
  so <sfile>:p:h/cpp.vim
else
  runtime! syntax/cpp.vim
  unlet b:current_syntax
endif

" FF++ extentions
syn keyword ffBoolean            false true
syn keyword ffConstantEF         P0 P0edge P0VF P1 P1b P1dc P1nc P2 P2b P2dc P2h RT0 RT0Ortho RTmodif
syn keyword ffConstantNum        i pi
syn keyword ffConstantQF         qf1pE qf1pElump qf1pT qf1pTlump qf2pE qf2pT qf2pT4P1 qf3pE qf5pT qf7pT qf9pT
syn keyword ffConstantSolver     CG Cholesky Crout GMRES LU UMFPACK
syn keyword ffException          catch throw try
syn keyword ffFunctionField      average jump mean otherside
syn keyword ffFunctionDiff       dn dx dxx dxy dy dyx dyy dz
syn keyword ffFunctionFE         interpolate
syn keyword ffFunctionInt        int1d int2d intalledges on
syn keyword ffFunctionMath       abs acos acosh arg asin asinh atan atan2 atanh ceil conj cos cosh exp
syn keyword ffFunctionMath       floor imag log log10 max min norm polar pow sin sinh sqrt tan tanh
syn keyword ffFunctionMath       randinit randint31 randint32 randreal1 randreal2 randreal3 randres53
syn keyword ffFunctionMatrix     set
syn keyword ffFunctionMesh       adaptmesh buildmesh buildmeshborder checkmovemesh emptymesh
syn keyword ffFunctionMesh       movemesh readmesh savemesh splitmesh square triangulate trunc
syn keyword ffFunctionPara       broadcast processor
syn keyword ffFunctionPlot       plot
syn keyword ffFunctionSolver     BFGS convect EigenValue LinearCG LinearGMRES Newton NLCG
syn keyword ffFunctionSystem     assert clock dumptable exec exit
syn keyword ffGlobal             area cin cout HaveUMFPACK hTriangle label lenEdge N NoUseOfWait
syn keyword ffGlobal             nTonEdge nuEdge nuTriangle P region verbosity version x y z
syn keyword ffmethodCoordo       x y z
syn keyword ffmethodFespace      ndof ndofK
syn keyword ffmethodStream       default noshowbase noshowpos showbase showpos precision scientific
syn keyword ffmethodStream       eof good
syn keyword ffmethodMatrix       coef diag m n nbcoef resize
syn keyword ffmethodMesh         area label nt nuTriangle nv region
syn keyword ffmethodString       find length rfind size
syn keyword ffmethodVector       im l1 l2 linfty max min re resize sum
syn keyword ffParameter          abserror anisomax append aspectratio bb binside bmat bw
syn keyword ffParameter          cadna clean cmm cutoff dimKrylov eps err errg factorize fill
syn keyword ffParameter          grey hmax hmin hsv init inquire inside IsMetric iso ivalue
syn keyword ffParameter          keepbackvertices label maxit maxsubdiv metric nbarrow nev nbiso
syn keyword ffParameter          nbiter nbiterline nbjacoby nbsmooth nbvx ncv nomeshgeneration omega
syn keyword ffParameter          op optimize periodic power precon ps
syn keyword ffParameter          qfe qfnbpE qfnbpT qforder qft ratio
syn keyword ffParameter          rescaling save sigma solver split splitin2 splitpbedge
syn keyword ffParameter          strategy sym t tgv thetamax tol tolpivot tolpivotsym value varrow
syn keyword ffParameter          vector veps verbosity viso wait
syn keyword ffSyntax             break continue else end endl for if include load mpirank mpisize return while
syn keyword ffType               BoundaryProblem bool border complex fespace func ifstream int matrix mesh
syn keyword ffType               ofstream problem R3 real solve string varf
" syn keyword ffUnclear
 


" Default highlighting
if version >= 508 || !exists("did_ff_syntax_inits")
  if version < 508
    let did_ff_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink ffBoolean              Boolean
  HiLink ffConstantEF           Constant
  HiLink ffConstantNum          Number
  HiLink ffConstantQF           Constant
  HiLink ffConstantSolver       Constant
  HiLink ffFunctionField        Function   
  HiLink ffFunctionDiff         Function
  HiLink ffFunctionFE           Function
  HiLink ffFunctionInt          Function
  HiLink ffFunctionMath         Function
  HiLink ffFunctionMatrix       Function
  HiLink ffFunctionMesh         Function
  HiLink ffFunctionPara         Function
  HiLink ffFunctionPlot         Function
  HiLink ffFunctionSolver       Function
  HiLink ffFunctionSystem       Function
  HiLink ffGlobal               Function
  HiLink ffmethodCoordo         Function
  HiLink ffmethodFespace        Function
  HiLink ffmethodMatrix         Function
  HiLink ffmethodMesh           Function
  HiLink ffmethodStream         Function
  HiLink ffmethodString         Function
  HiLink ffmethodVector         Function
  HiLink ffParameter            Function
  HiLink ffSyntax               Statement 
  HiLink ffType                 Type 
  HiLink ffUnclear              Error         " En attendant...

  delcommand HiLink
endif

let b:current_syntax = "edp"

