" Vim syntax file
" Language:     Pentadactyl configuration file
" Constributor: Doug Kearns <dougkearns@gmail.com>
" Last Change:  2010 Oct 1

" TODO: make this pentadactyl specific - shared dactyl config?

if exists("b:current_syntax")
finish
endif

let s:cpo_save = &cpo
set cpo&vim

syn include @javascriptTop syntax/javascript.vim
unlet b:current_syntax

syn include @cssTop syntax/css.vim
unlet b:current_syntax

syn match pentadactylCommandStart "\%(^\s*:\=\)\@<=" nextgroup=pentadactylCommand,pentadactylAutoCmd

syn keyword pentadactylCommand contained
            \ contexts gr[oup] delg[roup] fini[sh] if elsei[f] elif el[se] en[dif] fi sty[le] downl[oads] dl dlc[lear]
            \ javas[cript] js frameo[nly] ha[rdcopy] pa[geinfo] pagest[yle] pas re[load] sav[eas] w[rite] st[op] vie[wsource]
            \ zo[om] colo[rscheme] hi[ghlight] let unl[et] cd chd[ir] pw[d] mkv[imruntime] runt[ime] scrip[tnames] so[urce] run
            \ noh[lfind] com[mand] delc[ommand] comp[letions] y[ank] sa[nitize] cookies ck addo[ns] ao exta[dd] ab[breviate]
            \ una[bbreviate] delmac[ros] mac[ros] mes[sages] messc[lear] sil[ent] bma[rk] bmarks delbm[arks] map no[remap] unm[ap]
            \ feedkeys fk reg[isters] dia[log] em[enu] exe[cute] loadplugins lpl norm[al] pr[ivate] exit x q[uit] reh[ash]
            \ res[tart] time verb[ose] ve[rsion] delqm[arks] qma[rk] qmarks delm[arks] ma[rk] marks au[tocmd] o[pen] redr[aw]
            \ ba[ck] fo[rward] fw hist[ory] hs ju[mps] pin[tab] unpin[tab] keepa[lt] tab background bg tabd[o] bufd[o] tabl[ast]
            \ bl[ast] tabp[revious] tp[revious] tabn[ext] tn[ext] bp[revious] bn[ext] tabn[ext] tn[ext] bn[ext] tabr[ewind]
            \ tabfir[st] br[ewind] bf[irst] b[uffer] buffers files ls tabs quita[ll] qa[ll] reloada[ll] stopa[ll] tabm[ove]
            \ tabo[nly] tabopen t[open] tabnew tabde[tach] tabdu[plicate] taba[ttach] u[ndo] undoa[ll] wqa[ll] wq xa[ll]
            \ nmap vmap

" syn keyword pentadactylCommand run ab[breviate] addo[ns] au[tocmd] ba[ck] bd[elete] bw[ipeout] bun[load]
" \ tabc[lose] bma[rk] bmarks b[uffer] buffers files ls tabs ca[bbrev] cabc[lear] cd chd[ir] cm[ap] cmapc[lear] cno[remap]
" \ colo[rscheme] comc[lear] com[mand] contexts cuna[bbrev] cunm[ap] delbm[arks] delc[ommand] delmac[ros] delm[arks] delqm[arks]
" \ dels[tyle] dia[log] doautoa[ll] do[autocmd] downl[oads] dl ec[ho] echoe[rr] echom[sg] em[enu] exe[cute] exta[dd] extde[lete]
" \ extd[isable] exte[nable] extens[ions] exts exto[ptions] extp[references] exu[sage] fini[sh] fo[rward] fw frameo[nly]
" \ ha[rdcopy] h[elp] helpa[ll] hi[ghlight] hist[ory] hs ia[bbrev] iabc[lear] im[ap] imapc[lear] ino[remap] iuna[bbrev] iunm[ap]
" \ javas[cript] js ju[mps] keepa[lt] let loadplugins lpl macros map mapc[lear] ma[rk] marks mes[sages] messc[lear]
" \ mkp[entadactylrc] nm[ap] nmapc[lear] nno[remap] noh[lsearch] no[remap] norm[al] nunm[ap] o[pen] optionu[sage] pa[geinfo]
" \ pagest[yle] pas pref[erences] prefs pw[d] qma[rk] qmarks q[uit] quita[ll] qa[ll] redr[aw] re[load] reloada[ll] res[tart]
" \ runt[ime] sa[nitize] sav[eas] w[rite] sbcl[ose] scrip[tnames] se[t] setg[lobal] setl[ocal] sideb[ar] sb[ar] sbope[n]
" \ sil[ent] so[urce] st[op] stopa[ll] sty[le] styled[isable] styd[isable] stylee[nable] stye[nable] stylet[oggle] styt[oggle]
" \ tab taba[ttach] tabde[tach] tabd[o] bufd[o] tabdu[plicate] tabl[ast] bl[ast] tabm[ove] tabn[ext] tn[ext] bn[ext] tabo[nly]
" \ tabopen t[open] tabnew tabp[revious] tp[revious] tabN[ext] tN[ext] bp[revious] bN[ext] tabr[ewind] tabfir[st] br[ewind]
" \ bf[irst] time toolbarh[ide] tbh[ide] toolbars[how] tbs[how] toolbart[oggle] tbt[oggle] una[bbreviate] u[ndo] undoa[ll]
" \ unl[et] unm[ap] verb[ose] ve[rsion] vie[wsource] viu[sage] vm[ap] vmapc[lear] vno[remap] vunm[ap] winc[lose] wc[lose]
" \ wind[ow] winon[ly] wino[pen] wo[pen] wqa[ll] wq xa[ll] zo[om]
" \ contained

syn keyword pentadactylHighlight
            \ Boolean Function Null Number Object String Comment Key Enabled Disabled FontFixed FontCode FontProportional
            \ CmdCmdLine CmdErrorMsg CmdInfoMsg CmdModeMsg CmdMoreMsg CmdNormal CmdQuestion CmdWarningMsg StatusCmdLine
            \ StatusErrorMsg StatusInfoMsg StatusModeMsg StatusMoreMsg StatusNormal StatusQuestion StatusWarningMsg Normal
            \ StatusNormal ErrorMsg InfoMsg StatusInfoMsg LineNr ModeMsg StatusModeMsg MoreMsg StatusMoreMsg Message Message
            \ String NonText Preview Question StatusQuestion WarningMsg StatusWarningMsg Disabled CmdLine CmdPromp CmdInput
            \ CmdOutput MOW Comp CompGroup CompTitle CompTitleSep CompMsg CompItem CompIcon CompResult CompDesc CompLess CompMore
            \ Dense EditorEditing EditorError EditorBlink1 EditorBlink2 REPL Usage UsageHead UsageBody UsageItem Indicator Filter
            \ Keyword Tag Link LinkInfo StatusLine StatusLineNormal StatusLineBroken StatusLineSecure StatusLineExtended TabClose
            \ TabIcon TabText TabNumber TabIconNumber Title URL URLExtra FrameIndicator Bell Hinting Hint HintElem HintActive
            \ HintImage Button Buttons DownloadCell Downloads DownloadHead Download DownloadButtons DownloadPercent
            \ DownloadProgress DownloadProgressHave DownloadProgressTotal DownloadSource DownloadState DownloadTime DownloadTitle
            \ AddonCell Addons AddonHead Addon AddonButtons AddonDescription AddonName AddonStatus AddonVersion

syn match pentadactylCommand "!" contained

syn keyword pentadactylAutoCmd au[tocmd] contained nextgroup=pentadactylAutoEventList skipwhite

syn keyword pentadactylAutoEvent BookmarkAdd BookmarkChange BookmarkRemove ColorScheme DOMLoad DownloadPost Fullscreen
\ LocationChange PageLoadPre PageLoad PrivateMode Sanitize ShellCmdPost Enter LeavePre Leave
\ contained

syn match pentadactylAutoEventList "\(\a\+,\)*\a\+" contained contains=pentadactylAutoEvent

syn region pentadactylSet matchgroup=pentadactylCommand start="\%(^\s*:\=\)\@<=\<\%(setl\%[ocal]\|setg\%[lobal]\|set\=\)\=\>"
\ end="$" keepend oneline contains=pentadactylOption,pentadactylString

syn match pentadactylColor contained "\<transparent\>"
syn match pentadactylColor contained "\<white\>"
syn match pentadactylColor contained "#[0-9A-Fa-f]\{3\}\>"
syn match pentadactylColor "#[0-9A-Fa-f]\{6\}\>"

syn keyword pentadactylColorProp color
syn match pentadactylColorProp "\<background\(-\(color\|image\|attachment\|position\|repeat\)\)\=\>"

syn keyword pentadactylOption contained
            \ altwildmode awim autocomplete au complete cpt wildanchor wia wildcase wic wildmode wim wildsort wis downloadcolumns
            \ dlc downloadsort dlsort dls jsdebugger jsd encoding enc iskeyword isk jumptags jt linenumbers ln nextpattern
            \ previouspattern pageinfo pa scroll scr showstatuslinks ssli scrolltime sct scrollsteps scs usermode um yankshort ys
            \ banghist bh fileencoding fenc cdpath cd runtimepath rtp shell sh shellcmdflag shcf wildignore wig hlfind hlf
            \ findcase fc findflags ff incfind if private pornmode sanitizeitems si sanitizeshutdown ss sanitizetimespan sts
            \ cookies ck cookieaccept ca cookielifetime cl passunknown pu showmode smd passkeys pk strictfocus sf timeout tmo
            \ timeoutlen tmol history hi maxitems messages msgs defsearch ds suggestengines more editor insertmode im spelllang
            \ spl errorbells eb exrc ex fullscreen fs guioptions go helpfile hf loadplugins lpl titlestring urlseparator urlsep us
            \ verbose vbs visualbell vb eventignore ei extendedhinttags eht hinttags ht hintkeys hk hinttimeout hto followhints fh
            \ hintmatching hm wordseparators wsp hintinputs hin showtabline stal activate act newtab popups pps

let s:toggleOptions = ["banghist", "bh", "errorbells", "eb", "exrc", "ex", "flashblock", "fb", "fullscreen", "fs", "hlsearch",
            \ "hls", "ignorecase", "ic", "incsearch", "is", "insertmode", "im", "jsdebugger", "jsd", "linksearch", "lks", "more",
            \ "online", "private", "showmode", "smd", "smartcase", "scs", "strictfocus", "sf", "usermode", "um", "visualbell",
            \ "vb"]
execute 'syn match pentadactylOption "\<\%(no\|inv\)\=\%(' .
\ join(s:toggleOptions, '\|') .
\ '\)\>!\=" contained nextgroup=pentadactylSetMod'

syn match pentadactylSetMod "\%(\<[a-z_]\+\)\@<=&" contained

syn region pentadactylJavaScript start="\%(^\s*\%(javascript\|js\)\s\+\)\@<=" end="$" contains=@javascriptTop keepend oneline
syn region pentadactylJavaScript matchgroup=pentadactylJavaScriptDelimiter
\ start="\%(^\s*\%(javascript\|js\)\s\+\)\@<=<<\s*\z(\h\w*\)"hs=s+2 end="^\z1$" contains=@javascriptTop fold

let s:cssRegionStart = '\%(^\s*sty\%[le]!\=\s\+\%(-\%(n\|name\)\%(\s\+\|=\)\S\+\s\+\)\=[^-]\S\+\s\+\)\@<='
execute 'syn region pentadactylCss start="' . s:cssRegionStart . '" end="$" contains=@cssTop keepend oneline'
execute 'syn region pentadactylCss matchgroup=pentadactylCssDelimiter'
\ 'start="' . s:cssRegionStart . '<<\s*\z(\h\w*\)"hs=s+2 end="^\z1$" contains=@cssTop fold'

syn match pentadactylNotation "<[0-9A-Za-z-]\+>"

syn match pentadactylComment +".*$+ contains=pentadactylTodo,@Spell
syn keyword pentadactylTodo FIXME NOTE TODO XXX contained

syn region pentadactylString start="\z(["']\)" end="\z1" skip="\\\\\|\\\z1" oneline

syn match pentadactylLineComment +^\s*".*$+ contains=pentadactylTodo,@Spell
syn match pentadactylLineComment +\/\*.*\*\/+ contains=pentadactylTodo,@Spell

" NOTE: match vim.vim highlighting group names
hi def link pentadactylAutoCmd              pentadactylCommand
hi def link pentadactylAutoEvent            Type
hi def link pentadactylCommand              Statement
hi def link pentadactylComment              Comment
hi def link pentadactylJavaScriptDelimiter  Delimiter
hi def link pentadactylCssDelimiter         Delimiter
hi def link pentadactylNotation             Special
hi def link pentadactylLineComment          Comment
hi def link pentadactylOption               PreProc
hi def link pentadactylSetMod               pentadactylOption
hi def link pentadactylString               String
hi def link pentadactylTodo                 Todo
hi def link pentadactylHighlight            Identifier
hi def link pentadactylColor                Constant
hi def link pentadactylColorProp            StorageClass

let b:current_syntax = "pentadactyl"

let &cpo = s:cpo_save
unlet s:cpo_save

" vim: tw=130 et ts=4 sw=4:
