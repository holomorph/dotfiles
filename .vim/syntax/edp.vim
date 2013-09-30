" Vim syntax file
" Language:	FreeFem++
" Contributor:	Richard Michel <Richard.Michel@lepmi.inpg.fr>
" Contributor:	Paul Cazeaux
" Last Change:	2011 September 22 (for FreeFem++ v3.14)

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

" FF++ extensions
syn keyword	ffBoolean	false true
syn keyword	ffConstEF	P0 P0edge P0VF P1 P1b P1dc P1nc P2 P2b P2dc P2h RT0 RT0Ortho RTmodif
syn keyword	ffConstNum	pi
syn keyword	ffConstQF	qf1pE qf1pElump qf1pT qf1pTlump qf2pE qf2pT qf2pT4P1 qf3pE qf5pT
syn keyword	ffConstQF	qf7pT qf9pT
syn keyword	ffConstSolver	CG Cholesky Crout GMRES LU UMFPACK sparsesolver
syn keyword	ffFuncField	average jump mean otherside
syn keyword	ffFuncDiff	dn dx dxx dxy dy dyx dyy dz
syn keyword	ffFuncFE	interpolate
syn keyword	ffFuncInt	int1d int2d intalledges on
syn keyword	ffFuncMath	abs acos acosh arg asin asinh atan atan2 atanh ceil conj cos cosh
syn keyword	ffFuncMath	exp floor imag log log10 max min norm polar pow sin sinh sqrt tan
syn keyword	ffFuncMath	tanh randinit randint31 randint32 randreal1 randreal2 randreal3
syn keyword	ffFuncMath	randres53
syn keyword	ffFuncMatrix	set
syn keyword	ffFuncMesh	adaptmesh buildmesh buildmeshborder checkmovemesh emptymesh
syn keyword	ffFuncMesh	movemesh readmesh savemesh splitmesh square triangulate trunc
syn keyword	ffFuncPara	broadcast processor
syn keyword	ffFuncPlot	plot
syn keyword	ffFuncSolver	BFGS convect EigenValue LinearCG LinearGMRES Newton NLCG
syn keyword	ffFuncSystem	assert clock dumptable exec exit
syn keyword	ffGlobal	area cin cout HaveUMFPACK hTriangle lenEdge N NoUseOfWait nTonEdge
syn keyword	ffGlobal	nuEdge nuTriangle P region verbosity version t x y z x0 y0
syn keyword	ffMethodFESpace	ndof ndofK
syn keyword	ffMethodStream	default noshowbase noshowpos showbase showpos precision scientific
syn keyword	ffMethodStream	eof good
syn keyword	ffMethodMatrix	coef diag m n nbcoef resize
syn keyword	ffMethodMesh	nt nv
syn keyword	ffMethodString	find length rfind size
syn keyword	ffMethodVector	im l1 l2 linfty max min re resize sum
syn keyword	ffParameter	abserror anisomax append aspectratio bb binside bmat bw cadna clean
syn keyword	ffParameter	cmm cutoff dimKrylov eps err errg factorize fill grey hmax hmin hsv
syn keyword	ffParameter	init inquire inside IsMetric iso ivalue keepbackvertices label
syn keyword	ffParameter	maxit maxsubdiv metric nbarrow nev nbiso nbiter nbiterline nbjacoby
syn keyword	ffParameter	nbsmooth nbvx ncv nomeshgeneration omega op optimize periodic power
syn keyword	ffParameter	precon prev ps qfe qfnbpE qfnbpT qforder qft ratio rescaling save
syn keyword	ffParameter	sigma solver split splitin2 splitpbedge strategy sym tgv thetamax
syn keyword	ffParameter	tol tolpivot tolpivotsym value varrow vector veps verbosity viso
syn keyword	ffParameter	wait WindowIndex

syn keyword	ffMacro		macro
syn keyword	ffStatement	mpirank mpisize
" syn region	ffMacro		display matchgroup=ffMacroDelim start="macro" end="//" contains=ALL transparent
syn keyword	ffEOM		contained EOM
syn match	ffError		'^\s*\#.*$'

" Chapter 11
syn keyword	ffSolverFunc	defaulttoMUMPS realdefaulttoSuperLUdist complexdefaulttoSuperLUdist
syn keyword	ffSolverFunc	realdefaulttopastix complexdefaulttopastix defaulttohips
syn keyword	ffSolverFunc	defaulttohypre defaulttoparms
syn keyword	ffSolverParm	lparams dparams sparams datafilename
syn keyword	ffDSolverParm	permr permc scaler scalec

syn keyword	ffType		BoundaryProblem bool border complex fespace func ifstream matrix
syn keyword	ffType		mesh ofstream problem R3 real solve string varf

syn region	cIncluded	display contained start=+"+ skip=+\\\\\|\\"+ end=+"+
syn match	cIncluded	display contained "<[^>]*>"
syn match	cInclude	display "^\s*\(include\|load\)\>\s*["<]" contains=cIncluded

syn cluster	ffFunctions	contains=ffFuncField,ffFuncDiff,ffFuncFE,ffFuncInt,ffFuncMath
syn cluster	ffFunctions	add=ffFuncMatrix,ffFuncMesh,ffFuncPara,ffFuncPlot,ffFuncSolver
syn cluster	ffFunctions	add=ffFunSystem

" Default highlighting
if version >= 508 || !exists("did_ff_syntax_inits")
  if version < 508
    let did_ff_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink ffBoolean		Boolean
  HiLink ffConstEF		Constant
  HiLink ffConstNum		Number
  HiLink ffConstQF		Constant
  HiLink ffConstSolver		Constant
  HiLink ffEOM			Delimiter
  HiLink ffError		Error
  HiLink ffFuncField		Function
  HiLink ffFuncDiff		Function
  HiLink ffFuncFE		Function
  HiLink ffFuncInt		Function
  HiLink ffFuncMath		Function
  HiLink ffFuncMatrix		Function
  HiLink ffFuncMesh		Function
  HiLink ffFuncPara		Function
  HiLink ffFuncPlot		Function
  HiLink ffFuncSolver		Function
  HiLink ffFuncSystem		Function
  HiLink ffGlobal		Keyword
  HiLink ffMacro		Macro
  HiLink ffMethodFESpace	Function
  HiLink ffMethodMatrix		Identifier
  HiLink ffMethodMesh		Function
  HiLink ffMethodStream		Function
  HiLink ffMethodString		Function
  HiLink ffMethodVector		Function
  HiLink ffParameter		Identifier
  HiLink ffStatement		Statement
  HiLink ffType			Type
  delcommand HiLink
endif

let b:current_syntax = "edp"

" vim: sw=8 sts=0 ts=8 tw=100
