%% =====================================================================
%%  bleisatz.cls
%%  Bleisatz for documents: papers, reports, handouts. The companion of
%%  beamerthemeBleisatz.sty: the same two values (#000000 ink on #FFFFFF
%%  paper), the same type, the same running head and progress marks.
%%  No greys, no tints, no transparency, no rounded corners, no shadows.
%%
%%  Version 1.3 (2026-11-19) · LPPL 1.3c
%%  Engines: pdfLaTeX (Overleaf default), LuaLaTeX, XeLaTeX
%%
%%  Class options. Without options: Bleisatz 1-bit, light, IBM Plex,
%%  progress bar, A4.
%%    a4paper | letterpaper   paper size (default a4paper)
%%    dark            white on black pages, for reading on screen
%%    total           folio shows "3 / 8" instead of "3"
%%    nohead, head    no running head | running head (default)
%%    justified       justified text (default: ragged right, hyphenated)
%%    progress=bar    the head rule is a progress bar with a dot per section (default)
%%    progress=dots   one dot per page beside the folio, grouped by section
%%    progress=none   the plain head rule
%%    font=plex       IBM Plex Sans, Sans Condensed and Mono (default)
%%    font=mono       IBM Plex Mono throughout
%%    font=c64        C64 Pro Mono throughout, on cells one em wide
%%                    (LuaLaTeX; the font is not part of this class, get it
%%                    from https://style64.org/c64-truetype; without it:
%%                    font=mono)
%%    font=etbb       ET Book (ETbb, in TeX Live), Edward Tufte's typeface
%%  Tufte: every aspect is an option of its own, Bleisatz value first (default).
%%    palette=onebit|grey|tufte   two values | plus one grey | plus Tufte's
%%                    red and green
%%    onebit, grey    short for palette=onebit, palette=grey
%%    font=etbb       ET Book (see above)
%%    layout=bleisatz|tufte    134 mm text block | Tufte's handout: 110 mm
%%                    column, 52 mm margin, captions in the margin, figures
%%                    reaching into it
%%    notes=foot|side          footnotes | numbered sidenotes in the margin
%%    rules=heavy|hair         heavy head, section and block rules | hairlines,
%%                    no head rule
%%    typography=bleisatz|tufte  weight for structure | italic, small capitals
%%                    and size; unnumbered italic headings, grey markers and sources
%%    emphasis=inverse|italic  \inv, alertblock and inverse panels reversed | italic
%%    figures=bleisatz|tufte   \figurefolder searches the Bleisatz folders first |
%%                    the Tufte set first; pgfplots in Tufte's style
%%    tufte           all of them at once: palette=tufte, font=etbb, layout=tufte,
%%                    notes=side, rules=hair, typography=tufte, emphasis=italic,
%%                    figures=tufte, progress=none. Options given beside it win.
%%  \newthought, epigraph, fullwidth, \marginfigure, margintable, \marginnote,
%%  \sidecite and \sparkline work with every option.
%%
%%  The grid: the text block is 134 mm wide, the width of the slide text
%%  block, so the inferplotR figure slots fit at 100 %: full 134 mm, side
%%  86 mm and half 64 mm, each with its caption beside it.
%%
%%  Front matter  \title \subtitle \shorttitle \author \institute \venue
%%                \date \correspondence \keywords, then \maketitle and
%%                the abstract environment
%%  Figures       \figurefolder{figures}, \sidefigure, \fullfigure, \source
%%  Tables        booktabs rules, \tablenote
%%  Emphasis      \alert \inv \smalllabel \sidenote \lightface \condensed
%%  Blocks        block, exampleblock, alertblock, inverse, statement,
%%                pullquote, \keyfigure (the same names as on the slides)
%% =====================================================================
\NeedsTeXFormat{LaTeX2e}[2020/10/01]
\ProvidesClass{bleisatz}
  [2026/11/19 v1.3 Bleisatz: strictly black-and-white document class]

%% ---------------------------------------------------------------------
%%  Options
%% ---------------------------------------------------------------------
\newif\ifbz@dark      \bz@darkfalse
\newif\ifbz@total     \bz@totalfalse
\newif\ifbz@head      \bz@headtrue
\newif\ifbz@justified \bz@justifiedfalse
\newif\ifbz@tufte     \bz@tuftefalse
\newif\ifbz@progressset \newif\ifbz@fontset
\def\bz@palette{}
\def\bz@progress{bar}
\def\bz@font{plex}
% the Tufte aspects (empty = not given: Bleisatz, or Tufte under tufte)
\def\bz@layout{}\def\bz@notes{}\def\bz@rules{}\def\bz@typography{}
\def\bz@emphasis{}\def\bz@figures{}
\def\bz@papersize{a4paper}
\def\bz@passopts{}
\DeclareOption{dark}{\bz@darktrue}
\DeclareOption{total}{\bz@totaltrue}
\DeclareOption{nohead}{\bz@headfalse}
\DeclareOption{head}{\bz@headtrue}
\DeclareOption{justified}{\bz@justifiedtrue}
\DeclareOption{progress}{\def\bz@progress{bar}\bz@progresssettrue}
\DeclareOption{tufte}{\bz@tuftetrue}
\DeclareOption{onebit}{\def\bz@palette{onebit}}
\DeclareOption{grey}{\def\bz@palette{grey}}
\DeclareOption{a4paper}{\def\bz@papersize{a4paper}}
\DeclareOption{letterpaper}{\def\bz@papersize{letterpaper}}
% key=value options (font=..., progress=...); anything else goes to article
\def\bz@option{\expandafter\bz@option@a\CurrentOption==\@nil}
\def\bz@option@a#1=#2=#3\@nil{%
  \def\bz@key{#1}\def\bz@val{#2}%
  \ifx\bz@val\@empty
    \PassOptionsToClass{#1}{article}\edef\bz@passopts{\bz@passopts,#1}%
  \else
    \def\bz@tmp{font}\ifx\bz@key\bz@tmp \let\bz@font\bz@val\bz@fontsettrue\else
    \def\bz@tmp{progress}\ifx\bz@key\bz@tmp \let\bz@progress\bz@val\bz@progresssettrue\else
    \def\bz@tmp{palette}\ifx\bz@key\bz@tmp \let\bz@palette\bz@val\else
    \def\bz@tmp{layout}\ifx\bz@key\bz@tmp \let\bz@layout\bz@val\else
    \def\bz@tmp{notes}\ifx\bz@key\bz@tmp \let\bz@notes\bz@val\else
    \def\bz@tmp{rules}\ifx\bz@key\bz@tmp \let\bz@rules\bz@val\else
    \def\bz@tmp{typography}\ifx\bz@key\bz@tmp \let\bz@typography\bz@val\else
    \def\bz@tmp{emphasis}\ifx\bz@key\bz@tmp \let\bz@emphasis\bz@val\else
    \def\bz@tmp{figures}\ifx\bz@key\bz@tmp \let\bz@figures\bz@val\else
      \ClassWarning{bleisatz}{Unknown option \CurrentOption}\fi\fi\fi\fi\fi\fi%
    \fi\fi\fi
  \fi}
\DeclareOption*{\bz@option}
\ProcessOptions\relax
\LoadClass{article}
% Packages loaded later see only the options meant for them.
\edef\@classoptionslist{\expandafter\@gobble\bz@passopts\@empty}

\def\bz@str@bar{bar}\def\bz@str@dots{dots}\def\bz@str@none{none}
\ifx\bz@progress\bz@str@bar\else\ifx\bz@progress\bz@str@dots\else\ifx\bz@progress\bz@str@none\else
  \ClassWarning{bleisatz}{Unknown value progress=\bz@progress; using none}%
  \def\bz@progress{none}%
\fi\fi\fi
\def\bz@str@plex{plex}\def\bz@str@mono{mono}\def\bz@str@csf{c64}\def\bz@str@etbb{etbb}
% tufte: Tufte's defaults wherever no option said otherwise
\ifbz@tufte
  \ifx\bz@palette\@empty\def\bz@palette{tufte}\fi
  \ifbz@fontset\else\def\bz@font{etbb}\fi
  \ifbz@progressset\else\def\bz@progress{none}\fi
\else
  \ifx\bz@palette\@empty\def\bz@palette{onebit}\fi
\fi
% one flag per aspect: true = Tufte's way
\newif\ifbz@tlayout \newif\ifbz@side \newif\ifbz@hair \newif\ifbz@ttype
\newif\ifbz@italic \newif\ifbz@tfigs
\def\bz@aspect#1#2#3#4{% value macro, Bleisatz value, Tufte value, flag name
  \ifx#1\@empty\ifbz@tufte\def#1{#3}\else\def#1{#2}\fi\fi
  \def\bz@tmp{#3}\ifx#1\bz@tmp\csname bz@#4true\endcsname\else
  \def\bz@tmp{#2}\ifx#1\bz@tmp\else
    \ClassWarning{bleisatz}{Unknown value \detokenize\expandafter{#1}; using #2}%
    \def#1{#2}\fi\fi}
\bz@aspect\bz@layout{bleisatz}{tufte}{tlayout}
\bz@aspect\bz@notes{foot}{side}{side}
\bz@aspect\bz@rules{heavy}{hair}{hair}
\bz@aspect\bz@typography{bleisatz}{tufte}{ttype}
\bz@aspect\bz@emphasis{inverse}{italic}{italic}
\bz@aspect\bz@figures{bleisatz}{tufte}{tfigs}
\ifx\bz@font\bz@str@plex\else\ifx\bz@font\bz@str@mono\else\ifx\bz@font\bz@str@csf\else
\ifx\bz@font\bz@str@etbb\else
  \ClassWarning{bleisatz}{Unknown value font=\bz@font; using plex}%
  \def\bz@font{plex}%
\fi\fi\fi\fi
\def\bz@str@onebit{onebit}\def\bz@str@grey{grey}\def\bz@str@tpal{tufte}
\newif\ifbz@grey   \bz@greyfalse    % the one grey is allowed
\newif\ifbz@colour \bz@colourfalse  % Tufte's red and green are allowed
\ifx\bz@palette\bz@str@tpal\bz@greytrue\bz@colourtrue\else
\ifx\bz@palette\bz@str@grey\bz@greytrue\else
\ifx\bz@palette\bz@str@onebit\else
  \ClassWarning{bleisatz}{Unknown value palette=\bz@palette; using onebit}%
  \def\bz@palette{onebit}%
\fi\fi\fi

\RequirePackage{iftex}
\RequirePackage{etoolbox}
\RequirePackage{xcolor}
\RequirePackage{graphicx}
\RequirePackage{tikz}
\usetikzlibrary{patterns,calc}
\RequirePackage{booktabs}
\RequirePackage{ragged2e}
\RequirePackage{enumitem}

%% ---------------------------------------------------------------------
%%  Grid
%%  Text block 134 mm (= the slide text block), set off-centre to the
%%  left; the wider right margin carries side notes.
%% ---------------------------------------------------------------------
% layout=tufte: the handout grid, a narrower text column (about 70
% characters) and a wide margin for sidenotes, margin figures and captions.
% Figures of 86 and 64 mm sit in the column, 134 mm ones reach into the margin.
\ifbz@tlayout
  \edef\bz@geometry{\bz@papersize,
    left=22mm, textwidth=110mm, marginparsep=7mm, marginparwidth=52mm,
    top=31mm, headheight=18pt, headsep=9mm, bottom=24mm, footskip=10mm,
    heightrounded}
\else
  \edef\bz@geometry{\bz@papersize,
    left=26mm, textwidth=134mm, marginparsep=5mm, marginparwidth=35mm,
    top=31mm, headheight=18pt, headsep=9mm, bottom=24mm, footskip=10mm,
    heightrounded}
\fi
\expandafter\RequirePackage\expandafter[\bz@geometry]{geometry}
% layout=tufte or notes=side: margin notes and figures keep clear of each
% other and of the page foot
\ifbz@tlayout\def\bz@mf{}\fi
\ifbz@side\def\bz@mf{}\fi
\ifdefined\bz@mf\IfFileExists{marginfix.sty}{\RequirePackage{marginfix}}{}\fi
\newlength\bz@fullwidth
\setlength\bz@fullwidth{\dimexpr\textwidth+\marginparsep+\marginparwidth\relax}
\newlength\bz@heavy  \setlength\bz@heavy{1.1pt}   % head rule, block and section rule
\newlength\bz@hair   \setlength\bz@hair{0.4pt}    % everything else
\newlength\bz@fill   \setlength\bz@fill{1.1pt}    % progress bar fill
% rules=hair: the smallest effective difference; no heavy rules anywhere
\ifbz@hair\setlength\bz@heavy{\bz@hair}\fi

%% ---------------------------------------------------------------------
%%  Colour: exactly two values
%% ---------------------------------------------------------------------
\ifbz@dark
  \definecolor{bzink}{RGB}{255,255,255}
  \definecolor{bzpaper}{RGB}{0,0,0}
\else
  \definecolor{bzink}{RGB}{0,0,0}
  \definecolor{bzpaper}{RGB}{255,255,255}
\fi
\def\bz@ink{bzink}
\def\bz@paper{bzpaper}
% The palette beyond the two values: one grey (palette=grey|tufte), Tufte's
% red and green (palette=tufte). Where the palette lacks them they are ink.
% C64: the VIC-II colours.
\ifbz@grey
  \ifx\bz@font\bz@str@csf\definecolor{bzgrey}{RGB}{149,149,149}%
  \else\definecolor{bzgrey}{RGB}{153,153,153}\fi
\else\colorlet{bzgrey}{bzink}\fi
\ifbz@colour
  \ifx\bz@font\bz@str@csf
    \ifbz@dark\definecolor{bzred}{RGB}{203,126,117}\definecolor{bzgreen}{RGB}{154,226,155}%
    \else\definecolor{bzred}{RGB}{159,78,68}\definecolor{bzgreen}{RGB}{92,171,94}\fi
  \else
    \ifbz@dark\definecolor{bzred}{RGB}{229,72,77}\definecolor{bzgreen}{RGB}{93,183,96}%
    \else\definecolor{bzred}{RGB}{179,32,42}\definecolor{bzgreen}{RGB}{46,125,50}\fi
  \fi
\else\colorlet{bzred}{bzink}\colorlet{bzgreen}{bzink}\fi
% \muted{} context and sources (grey); \inred{} / \ingreen{} link numbers to
% the marks of the same colour (palette=tufte; otherwise italic / plain ink)
\DeclareRobustCommand\muted[1]{{\color{bzgrey}#1}}
\DeclareRobustCommand\inred[1]{{\ifbz@colour\color{bzred}#1\else\itshape#1\fi}}
\DeclareRobustCommand\ingreen[1]{{\ifbz@colour\color{bzgreen}\fi#1}}
% swap ink and paper inside a group (alert blocks, inverse panels, \inv)
\newcommand\bz@swap{\let\bz@tmpc\bz@ink\let\bz@ink\bz@paper\let\bz@paper\bz@tmpc
  \color{\bz@ink}}
\AtBeginDocument{%
  \ifbz@dark\pagecolor{bzpaper}\fi
  \color{bzink}\let\default@color\current@color}

%% ---------------------------------------------------------------------
%%  Typefaces (the same set-up as the Beamer theme)
%%  font=plex  pdfLaTeX: Type 1 versions from the "plex" package;
%%             Lua/XeLaTeX: the OpenType files of the same package
%%  font=mono  IBM Plex Mono for every role
%%  font=c64   C64 Pro Mono for every role (LuaLaTeX or XeLaTeX, installed)
%%  Fallbacks: font=c64 -> font=mono (pdfLaTeX, or C64 Pro Mono missing);
%%             Plex missing -> TeX Gyre Heros / Latin Modern Mono
%% ---------------------------------------------------------------------
\ifx\bz@font\bz@str@csf
  \ifPDFTeX
    \ClassWarning{bleisatz}{font=c64 needs LuaLaTeX (or XeLaTeX); using font=mono}%
    \def\bz@font{mono}%
  \fi
\fi
\newif\ifbz@plex
\newif\ifbz@umath \bz@umathfalse
\newif\ifbz@etbb
\ifPDFTeX
  \RequirePackage[T1]{fontenc}
  \ifx\bz@font\bz@str@etbb
    \IfFileExists{ETbb.sty}{\bz@etbbtrue}{%
      \ClassWarning{bleisatz}{ET Book (ETbb) not found; using font=plex}%
      \def\bz@font{plex}}%
  \fi
  \ifbz@etbb
    % ET Book: oldstyle proportional figures in text, lining tabular in
    % tables and key figures; IBM Plex Mono stays the monospaced face
    \RequirePackage[p,osf]{ETbb}
    \renewcommand\sfdefault{ETbb-OsF}
    \IfFileExists{T1plxMono-TLF.fd}{\renewcommand\ttdefault{plxMono-TLF}}{}
    \newcommand\bz@light{\fontfamily{ETbb-OsF}\fontseries{m}\selectfont}
    \newcommand\bz@cond{\fontfamily{ETbb-OsF}\fontseries{m}\selectfont}
    \newcommand\bz@condbf{\fontfamily{ETbb-OsF}\fontseries{m}\fontshape{it}\selectfont}
    \newcommand\bz@tabnums{\fontfamily{ETbb-TLF}\selectfont}
    \newcommand\bz@liningnums{\fontfamily{ETbb-LF}\selectfont}
  \else
  \IfFileExists{T1plxSans-TLF.fd}{\bz@plextrue}{\bz@plexfalse}
  \ifbz@plex
    \ifx\bz@font\bz@str@mono
      \renewcommand\sfdefault{plxMono-TLF}
      \renewcommand\rmdefault{plxMono-TLF}
      \renewcommand\ttdefault{plxMono-TLF}
      \renewcommand\bfdefault{sb}
      \newcommand\bz@light{\fontfamily{plxMono-TLF}\fontseries{l}\selectfont}
      \newcommand\bz@cond{\fontfamily{plxMono-TLF}\fontseries{m}\selectfont}
      \newcommand\bz@condbf{\fontfamily{plxMono-TLF}\fontseries{sb}\selectfont}
    \else
      \renewcommand\sfdefault{plxSans-TLF}
      \renewcommand\rmdefault{plxSerif-TLF}
      \renewcommand\ttdefault{plxMono-TLF}
      \renewcommand\bfdefault{sb}
      \newcommand\bz@light{\fontfamily{plxSans-TLF}\fontseries{l}\selectfont}
      \newcommand\bz@cond{\fontfamily{plxSans-TLF}\fontseries{c}\selectfont}
      \newcommand\bz@condbf{\fontfamily{plxSans-TLF}\fontseries{sbc}\selectfont}
    \fi
  \else
    \ClassWarning{bleisatz}{IBM Plex not found, using TeX Gyre Heros}
    \renewcommand\sfdefault{qhv}
    \renewcommand\ttdefault{lmtt}
    \ifx\bz@font\bz@str@mono\renewcommand\sfdefault{lmtt}\fi
    \newcommand\bz@light{\fontfamily{\sfdefault}\fontseries{m}\selectfont}
    \newcommand\bz@cond{\fontfamily{\sfdefault}\fontseries{m}\selectfont}
    \newcommand\bz@condbf{\fontfamily{\sfdefault}\fontseries{b}\selectfont}
  \fi
  \fi
\else
  \RequirePackage{fontspec}
  \def\bz@csfname{}
  \ifx\bz@font\bz@str@csf
    \IfFontExistsTF{C64 Pro Mono}{\def\bz@csfname{C64 Pro Mono}}{%
      \IfFontExistsTF{C64_Pro_Mono-STYLE.ttf}{\def\bz@csfname{C64_Pro_Mono-STYLE.ttf}}{%
        \IfFontExistsTF{C64_Pro_Mono-STYLE.otf}{\def\bz@csfname{C64_Pro_Mono-STYLE.otf}}{}}}%
    \ifx\bz@csfname\@empty
      \ClassWarning{bleisatz}{C64 Pro Mono is not installed
        (https://style64.org/c64-truetype); using font=mono}%
      \def\bz@font{mono}%
    \fi
  \fi
  \IfFontExistsTF{IBMPlexSans-Regular.otf}{\bz@plextrue}{\bz@plexfalse}
  \ifx\bz@font\bz@str@etbb
    \IfFontExistsTF{ETbb-Regular.otf}{\bz@etbbtrue}{%
      \ClassWarning{bleisatz}{ET Book (ETbb) not found; using font=plex}%
      \def\bz@font{plex}}%
  \fi
  \ifbz@etbb
    \defaultfontfeatures{Ligatures=TeX}
    \setsansfont{ETbb}[Extension=.otf, UprightFont=*-Regular, ItalicFont=*-Italic,
      BoldFont=*-Bold, BoldItalicFont=*-BoldItalic, Numbers={OldStyle,Proportional}]
    \setmainfont{ETbb}[Extension=.otf, UprightFont=*-Regular, ItalicFont=*-Italic,
      BoldFont=*-Bold, BoldItalicFont=*-BoldItalic, Numbers={OldStyle,Proportional}]
    \ifbz@plex
      \setmonofont{IBMPlexMono}[Extension=.otf, UprightFont=*-Regular, ItalicFont=*-Italic,
        BoldFont=*-SemiBold, BoldItalicFont=*-SemiBoldItalic]
    \fi
    \newcommand\bz@light{\sffamily}
    \newcommand\bz@cond{\sffamily}
    \newcommand\bz@condbf{\sffamily\itshape}
    \newcommand\bz@tabnums{\addfontfeatures{Numbers={Lining,Monospaced}}}
    \newcommand\bz@liningnums{\addfontfeatures{Numbers={Lining}}}
  \else
  \ifx\bz@font\bz@str@csf
    % One face for every role: one weight, no italics. Under LuaLaTeX "--"
    % becomes one hyphen and missing glyphs (umlauts, the section sign) come
    % from IBM Plex Mono.
    \ifLuaTeX
      \directlua{luaotfload.add_fallback("bleisatzcsixtyfour",
        {"file:IBMPlexMono-Regular.otf:mode=node;"})}%
      \directlua{fonts.handlers.otf.addfeature{name = "bzhyphen", type = "ligature",
        data = {[0x2D] = {0x2D, 0x2D}}}}%
      \def\bz@csffallback{RawFeature={fallback=bleisatzcsixtyfour;+bzhyphen}}%
    \else
      \def\bz@csffallback{}%
      \ClassWarning{bleisatz}{font=c64 under XeLaTeX: glyphs that C64 Pro Mono
        lacks (umlauts, section sign) are dropped; LuaLaTeX fills them from
        IBM Plex Mono}%
    \fi
    \edef\bz@csffeatures{Ligatures=TeXOff, BoldFont={\bz@csfname},
      ItalicFont={\bz@csfname}, BoldItalicFont={\bz@csfname},
      SlantedFont={\bz@csfname}, \bz@csffallback}
    \edef\bz@tmp{%
      \noexpand\setsansfont{\bz@csfname}[\bz@csffeatures]%
      \noexpand\setmainfont{\bz@csfname}[\bz@csffeatures]%
      \noexpand\setmonofont{\bz@csfname}[\bz@csffeatures]%
      \noexpand\newfontfamily\noexpand\bz@light{\bz@csfname}[\bz@csffeatures]}%
    \bz@tmp
    \let\bz@cond\bz@light
    \let\bz@condbf\bz@light
    % What the C64 had no glyph for, written as it would have been typed.
    \RequirePackage{newunicodechar}
    \newunicodechar{≥}{>=}\newunicodechar{≤}{<=}\newunicodechar{±}{+/-}
    \newunicodechar{×}{x}\newunicodechar{−}{-}\newunicodechar{–}{-}
    \newunicodechar{—}{--}\newunicodechar{·}{/}\newunicodechar{…}{...}
    \UndeclareTextCommand{\textendash}{TU}\DeclareTextCommand{\textendash}{TU}{-}
    \UndeclareTextCommand{\textemdash}{TU}\DeclareTextCommand{\textemdash}{TU}{--}
    \UndeclareTextCommand{\textperiodcentered}{TU}\DeclareTextCommand{\textperiodcentered}{TU}{/}
    \UndeclareTextCommand{\textellipsis}{TU}\DeclareTextCommand{\textellipsis}{TU}{...}
    % Math: letters and digits from the same face; relations as typed.
    \edef\bz@tmp{\noexpand\DeclareSymbolFont{bzcsf}{TU}{\sfdefault}{m}{n}}\bz@tmp
    \def\bz@mc#1#2{\Umathcode`#2="#1 \symbzcsf`#2\relax}
    \AtBeginDocument{%
      \count@=`a \loop\Umathcode\count@="7 \symbzcsf\count@
        \advance\count@\@ne\ifnum\count@<`\{\repeat
      \count@=`A \loop\Umathcode\count@="7 \symbzcsf\count@
        \advance\count@\@ne\ifnum\count@<`\[\repeat
      \count@=`0 \loop\Umathcode\count@="7 \symbzcsf\count@
        \advance\count@\@ne\ifnum\count@<`\:\repeat
      \bz@mc2+\bz@mc2-\bz@mc3=\bz@mc3<\bz@mc3>\bz@mc3:\bz@mc4(\bz@mc4[%
      \bz@mc5)\bz@mc5]\bz@mc6,\bz@mc6;\bz@mc0.\bz@mc0/\bz@mc0|\bz@mc5!\bz@mc5?%
      \renewcommand\geq{\mathrel{\mbox{>=}}}\let\ge\geq
      \renewcommand\leq{\mathrel{\mbox{<=}}}\let\le\leq
      \renewcommand\times{\mathbin{\mbox{x}}}%
      \renewcommand\pm{\mathbin{\mbox{+/-}}}}%
  \else\ifbz@plex
    \defaultfontfeatures{Ligatures=TeX}
    \ifx\bz@font\bz@str@mono
      \setsansfont{IBMPlexMono}[Extension=.otf,
        UprightFont=*-Regular, ItalicFont=*-Italic,
        BoldFont=*-SemiBold, BoldItalicFont=*-SemiBoldItalic]
      \setmainfont{IBMPlexMono}[Extension=.otf,
        UprightFont=*-Regular, ItalicFont=*-Italic,
        BoldFont=*-SemiBold, BoldItalicFont=*-SemiBoldItalic]
      \setmonofont{IBMPlexMono}[Extension=.otf,
        UprightFont=*-Regular, ItalicFont=*-Italic,
        BoldFont=*-SemiBold, BoldItalicFont=*-SemiBoldItalic]
      \newfontfamily\bz@light{IBMPlexMono}[Extension=.otf,
        UprightFont=*-Light, ItalicFont=*-LightItalic,
        BoldFont=*-Medium, BoldItalicFont=*-MediumItalic]
      \newcommand\bz@cond{\ttfamily}
      \newcommand\bz@condbf{\ttfamily\bfseries}
    \else
      \setsansfont{IBMPlexSans}[Extension=.otf,
        UprightFont=*-Regular, ItalicFont=*-Italic,
        BoldFont=*-SemiBold, BoldItalicFont=*-SemiBoldItalic]
      \setmainfont{IBMPlexSerif}[Extension=.otf,
        UprightFont=*-Regular, ItalicFont=*-Italic,
        BoldFont=*-SemiBold, BoldItalicFont=*-SemiBoldItalic]
      \setmonofont{IBMPlexMono}[Extension=.otf,
        UprightFont=*-Regular, ItalicFont=*-Italic,
        BoldFont=*-SemiBold, BoldItalicFont=*-SemiBoldItalic]
      \newfontfamily\bz@light{IBMPlexSans}[Extension=.otf,
        UprightFont=*-Light, ItalicFont=*-LightItalic,
        BoldFont=*-Medium, BoldItalicFont=*-MediumItalic]
      \newfontfamily\bz@cond{IBMPlexSansCondensed}[Extension=.otf,
        UprightFont=*-Regular, ItalicFont=*-Italic,
        BoldFont=*-SemiBold, BoldItalicFont=*-SemiBoldItalic]
      \newcommand\bz@condbf{\bz@cond\bfseries}
      % IBM Plex Math, where the installed Plex has it (and unicode-math works)
      \IfFontExistsTF{IBMPlexMath-Regular.otf}{\IfFileExists{unicode-math.sty}{%
        \ifLuaTeX\IfFileExists{lualatex-math.sty}{\bz@umathtrue}{}\else\bz@umathtrue\fi}{}}{}
    \fi
  \else
    \ClassWarning{bleisatz}{IBM Plex not found, using TeX Gyre Heros}
    \setsansfont{TeX Gyre Heros}
    \ifx\bz@font\bz@str@mono\setsansfont{Latin Modern Mono}\fi
    \newcommand\bz@light{\sffamily}
    \newcommand\bz@cond{\sffamily}
    \newcommand\bz@condbf{\sffamily\bfseries}
  \fi\fi
  \fi
\fi
\providecommand\bz@tabnums{}
\providecommand\bz@liningnums{}
\renewcommand\familydefault{\sfdefault}
% Math in the text face: IBM Plex Math (Lua/XeLaTeX, if installed), else
% letters and digits from the text font (mathastext); C64: see above.
\ifx\bz@font\bz@str@csf\else
  \ifbz@umath
    \RequirePackage{unicode-math}
    \setmathfont{IBMPlexMath-Regular.otf}
  \else
    \AtEndPreamble{\RequirePackage[italic]{mathastext}}
  \fi
\fi
\ifbz@justified
  \RequirePackage[protrusion=true,expansion=true]{microtype}
\else
  \RequirePackage[protrusion=true,expansion=false]{microtype}
\fi
\ifx\bz@font\bz@str@csf\microtypesetup{protrusion=false}\fi

% public font switches (as in the Beamer theme)
\newcommand\lightface{\bz@light}
\newcommand\condensed{\bz@cond}

%% ---------------------------------------------------------------------
%%  Sizes. Body 10 on 14 pt; the roles keep the sizes of the slides.
%% ---------------------------------------------------------------------
\renewcommand\normalsize{%
  \@setfontsize\normalsize{10}{14}%
  \abovedisplayskip 7\p@ \@plus2\p@ \@minus4\p@
  \abovedisplayshortskip \z@ \@plus3\p@
  \belowdisplayshortskip 4\p@ \@plus3\p@ \@minus3\p@
  \belowdisplayskip \abovedisplayskip
  \let\@listi\@listI}
\renewcommand\small{\@setfontsize\small{9}{12.5}}
\renewcommand\footnotesize{\@setfontsize\footnotesize{8}{10.5}}
\renewcommand\scriptsize{\@setfontsize\scriptsize{7}{8.5}}
\renewcommand\tiny{\@setfontsize\tiny{6.5}{8}}
\renewcommand\large{\@setfontsize\large{12}{15}}
\renewcommand\Large{\@setfontsize\Large{14}{17}}
\renewcommand\LARGE{\@setfontsize\LARGE{22}{26}}
\renewcommand\huge{\@setfontsize\huge{27}{30}}
\renewcommand\Huge{\@setfontsize\Huge{48}{48}}
% role fonts: the names of the Beamer theme
\newcommand\bz@f@head{\bz@cond\fontsize{7}{8.5}\selectfont}
\newcommand\bz@f@label{\bz@condbf\fontsize{8}{10}\selectfont}
\newcommand\bz@f@caption{\normalfont\fontsize{8}{10.5}\selectfont}
\newcommand\bz@f@source{\bz@cond\fontsize{6.5}{8}\selectfont}
\newcommand\bz@f@title{\bz@light\fontsize{27}{30}\selectfont}
\newcommand\bz@f@subtitle{\normalfont\fontsize{12}{15}\selectfont}
\newcommand\bz@f@author{\normalfont\fontsize{9}{12.5}\selectfont}
\newcommand\bz@f@statement{\bz@light\fontsize{22}{26}\selectfont}
\newcommand\bz@f@keyfigure{\bz@light\fontsize{48}{48}\selectfont}
\newcommand\bz@f@section{\normalfont\bfseries\fontsize{14}{17}\selectfont}
\newcommand\bz@f@subsection{\normalfont\bfseries\fontsize{10}{14}\selectfont}
\newcommand\bz@f@subsubsection{\normalfont\itshape\fontsize{10}{14}\selectfont}
\newcommand\bz@f@toc{\normalfont\fontsize{10}{14}\selectfont}
\newcommand\bz@alert@font{\bfseries}

% the section sign of the running head; C64 Pro Mono has none
\newcommand\bz@secsign{\textsection\,}
\ifx\bz@font\bz@str@csf\renewcommand\bz@secsign{}\fi

% font=c64: every glyph sits on a cell one em wide and one em tall. Display
% lines are set solid (leading = size); running text gets half a cell of
% line gap so that pages of it stay readable. One weight, no italics.
\ifx\bz@font\bz@str@csf
  \renewcommand\normalsize{%
    \@setfontsize\normalsize{9}{13.5}%
    \abovedisplayskip 6.75\p@ \@plus2\p@ \@minus3\p@
    \abovedisplayshortskip \z@ \@plus3\p@
    \belowdisplayshortskip 4.5\p@ \@plus2\p@ \@minus2\p@
    \belowdisplayskip \abovedisplayskip
    \let\@listi\@listI}
  \renewcommand\small{\@setfontsize\small{8}{12}}
  \renewcommand\footnotesize{\@setfontsize\footnotesize{7}{10.5}}
  \renewcommand\scriptsize{\@setfontsize\scriptsize{6}{9}}
  \renewcommand\tiny{\@setfontsize\tiny{5}{7.5}}
  \renewcommand\large{\@setfontsize\large{10}{10}}
  \renewcommand\Large{\@setfontsize\Large{12}{12}}
  \renewcommand\LARGE{\@setfontsize\LARGE{16}{16}}
  \renewcommand\huge{\@setfontsize\huge{18}{18}}
  \renewcommand\Huge{\@setfontsize\Huge{48}{48}}
  \renewcommand\bz@f@head{\bz@cond\fontsize{6}{6}\selectfont}
  \renewcommand\bz@f@label{\bz@condbf\fontsize{7}{7}\selectfont}
  \renewcommand\bz@f@caption{\normalfont\fontsize{7}{10.5}\selectfont}
  \renewcommand\bz@f@source{\bz@cond\fontsize{6}{6}\selectfont}
  \renewcommand\bz@f@title{\bz@light\fontsize{18}{18}\selectfont}
  \renewcommand\bz@f@subtitle{\normalfont\fontsize{10}{10}\selectfont}
  \renewcommand\bz@f@author{\normalfont\fontsize{8}{8}\selectfont}
  \renewcommand\bz@f@statement{\bz@light\fontsize{16}{16}\selectfont}
  \renewcommand\bz@f@keyfigure{\bz@light\fontsize{48}{48}\selectfont}
  \renewcommand\bz@f@section{\normalfont\fontsize{12}{12}\selectfont}
  \renewcommand\bz@f@subsection{\normalfont\fontsize{9}{13.5}\selectfont}
  \renewcommand\bz@f@subsubsection{\normalfont\fontsize{9}{13.5}\selectfont}
  \renewcommand\bz@f@toc{\normalfont\fontsize{9}{13.5}\selectfont}
  % one weight: \alert is reverse video, as RVS ON on the C64
  \renewcommand\bz@alert@font{}
\fi
%% font=etbb: ET Book at 11 on 15 pt (its x-height is small)
\ifbz@etbb
  \renewcommand\normalsize{%
    \@setfontsize\normalsize{11}{15}%
    \abovedisplayskip 7.5\p@ \@plus2\p@ \@minus4\p@
    \abovedisplayshortskip \z@ \@plus3\p@
    \belowdisplayshortskip 4\p@ \@plus3\p@ \@minus3\p@
    \belowdisplayskip \abovedisplayskip
    \let\@listi\@listI}
  \renewcommand\small{\@setfontsize\small{10}{13.5}}
  \renewcommand\footnotesize{\@setfontsize\footnotesize{9}{12}}
  \renewcommand\scriptsize{\@setfontsize\scriptsize{8}{10}}
  \renewcommand\tiny{\@setfontsize\tiny{7}{8.5}}
  \renewcommand\large{\@setfontsize\large{13}{16}}
  \renewcommand\Large{\@setfontsize\Large{15}{19}}
  \renewcommand\LARGE{\@setfontsize\LARGE{22}{26}}
  \renewcommand\huge{\@setfontsize\huge{28}{32}}
\fi
%% typography=tufte: the handout's type; structure by italic, small capitals
%% and size, never by weight
\ifbz@ttype
  \ifx\bz@font\bz@str@csf\else
    \ifbz@etbb
      \renewcommand\bz@f@head{\normalfont\fontsize{8.5}{10}\selectfont\scshape}
    \else
      \renewcommand\bz@f@head{\bz@cond\fontsize{7.5}{9}\selectfont}
    \fi
    \renewcommand\bz@f@label{\normalfont\footnotesize\itshape}
    \renewcommand\bz@f@caption{\normalfont\footnotesize}
    \renewcommand\bz@f@source{\normalfont\scriptsize\color{bzgrey}}
    \renewcommand\bz@f@title{\normalfont\huge}
    \renewcommand\bz@f@subtitle{\normalfont\large\itshape}
    \renewcommand\bz@f@author{\normalfont\small\itshape}
    \renewcommand\bz@f@statement{\normalfont\LARGE\itshape}
    \renewcommand\bz@f@keyfigure{\normalfont\fontsize{48}{48}\selectfont\bz@liningnums}
    \renewcommand\bz@f@section{\normalfont\Large\itshape}
    \renewcommand\bz@f@subsection{\normalfont\large\itshape}
    \renewcommand\bz@f@subsubsection{\normalfont\itshape}
    \renewcommand\bz@f@toc{\normalfont}
  \fi
  \renewcommand\bz@secsign{}
\fi
%% emphasis=italic: \alert in italic (C64 keeps reverse video)
\ifbz@italic\ifx\bz@font\bz@str@csf\else\renewcommand\bz@alert@font{\itshape}\fi\fi
\normalsize

%% ---------------------------------------------------------------------
%%  Paragraphs
%%  Ragged right with hyphenation (option justified: block), first line
%%  indented, no space between paragraphs. C64: words wrap whole.
%% ---------------------------------------------------------------------
\setlength\parindent{1.2em}
\setlength\parskip{0pt}
\setlength\RaggedRightParindent{1.2em}
\setlength\RaggedRightRightskip{0pt plus 2.4em}
\setlength\emergencystretch{1.5em}
% monospaced faces: words wrap whole, as on a typewriter or a screen
\ifx\bz@font\bz@str@mono
  \setlength\RaggedRightRightskip{0pt plus 1fil}
\fi
\ifx\bz@font\bz@str@csf
  \setlength\parindent{2em}\setlength\RaggedRightParindent{2em}
  \setlength\RaggedRightRightskip{0pt plus 1fil}
  \hyphenpenalty=10000 \exhyphenpenalty=10000
\fi
\ifbz@justified\else\AtBeginDocument{\RaggedRight}\fi
\clubpenalty=10000 \widowpenalty=10000 \displaywidowpenalty=10000
\raggedbottom
\frenchspacing
% captions, notes, sources: ragged right, no first-line indent
\newcommand\bz@ragged{\RaggedRight\parindent\z@}
% after a block, panel or statement the next paragraph starts flush left
\newcommand\bz@flushnext{\@afterindentfalse\@afterheading}
\newcommand\bz@afterdisplay{\aftergroup\bz@flushnext}
% display type: no hyphenation, plain ragged right
\newcommand\bz@nohyph{\hyphenpenalty=10000 \exhyphenpenalty=10000
  \leftskip=\z@ \rightskip=\z@ plus 1fil\relax \parfillskip=\z@ plus 1fil\relax}

%% ---------------------------------------------------------------------
%%  Running head:  § 2  Methods ......................................  3
%%                 ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
%%  The first page carries institute and date instead, as the title slide.
%% ---------------------------------------------------------------------
\def\bz@institute{}\def\bz@shorttitle{}\def\bz@subtitle{}\def\bz@venue{}
\def\bz@keywords{}\def\bz@correspondence{}
\newcommand\institute[1]{\def\bz@institute{#1}}
\newcommand\shorttitle[1]{\def\bz@shorttitle{#1}}
\newcommand\subtitle[1]{\def\bz@subtitle{#1}}
\newcommand\venue[1]{\def\bz@venue{#1}}
\newcommand\keywords[1]{\def\bz@keywords{#1}}
\newcommand\correspondence[1]{\def\bz@correspondence{#1}}

% pages: counted at shipout, the total is read from the .aux file
\newcount\bz@shipped
\AddToHook{shipout/after}{\global\advance\bz@shipped\@ne}
\def\bz@totpages{0}
\newcommand\bz@settot[1]{\gdef\bz@totpages{#1}}
\AtBeginDocument{\if@filesw\immediate\write\@auxout{%
  \string\providecommand\string\bz@settot[1]{}%
  \string\providecommand\string\bz@addsecmark[1]{}}\fi}
\AddToHook{enddocument/afterlastpage}{\if@filesw
  \immediate\write\@auxout{\string\bz@settot{\the\bz@shipped}}\fi}

% section starts: the page each \section lands on, written at shipout
\def\bz@secmarks{}
\newcommand\bz@addsecmark[1]{\xifinlist{#1}\bz@secmarks{}{\listxadd\bz@secmarks{#1}}}
\newcommand\bz@marksection{\if@filesw
  \write\@auxout{\string\bz@addsecmark{\the\numexpr\bz@shipped+1\relax}}\fi}

\newcommand\bz@folio{\thepage\ifbz@total\,/\,\bz@totpages\fi}
% the current section; before the first one, the short title
\newbox\bz@markbox
\newcommand\bz@headleft{%
  \setbox\bz@markbox\hbox{\leftmark}%
  \ifdim\wd\bz@markbox=\z@ \bz@shorttitle\else\unhbox\bz@markbox\fi}

%% ---------------------------------------------------------------------
%%  Progress (option progress=bar|dots), counted in pages
%%  bar:  the head rule is the bar. A hairline track runs the full width,
%%        the heavy rule fills it up to the current page; one dot per
%%        section start, filled once that section has begun.
%%  dots: one dot per page left of the folio, grouped by section.
%%  The total is known from the second run on (latexmk does both).
%% ---------------------------------------------------------------------
\newcount\bz@cur  \newcount\bz@tot  \newcount\bz@i  \newcount\bz@s
\newdimen\bz@x    \newdimen\bz@w    \newdimen\bz@pitch
\newdimen\bz@gap  \newdimen\bz@r    \newdimen\bz@foliow
\newcommand\bz@progressvalues{%
  \bz@cur=\numexpr\bz@shipped+1\relax
  \bz@tot=\bz@totpages\relax
  \ifnum\bz@tot<2 \bz@tot=0 \fi
  \ifnum\bz@cur>\bz@tot \bz@cur=\bz@tot \fi}
\newlength\bz@headwidth
\setlength\bz@headwidth{\ifbz@tlayout\bz@fullwidth\else\textwidth\fi}
% rules=hair: no head rule (a quiet head as in Tufte's handouts)
\newcommand\bz@rule@none{\ifbz@hair\else{\color{\bz@ink}\rule{\bz@headwidth}{\bz@heavy}}\fi}
\newcommand\bz@rule@bar{%
  \bz@progressvalues
  \ifnum\bz@tot=0 \bz@rule@none\else
  \bz@w=\bz@headwidth
  \bz@r=0.62mm
  \begin{pgfpicture}%
    \pgfpathrectangle{\pgfpointorigin}{\pgfpoint{\bz@w}{\bz@fill}}%
    \pgfusepath{use as bounding box}%
    \pgfsetfillcolor{\bz@ink}%
    \pgfpathrectangle{\pgfpoint{0pt}{\dimexpr(\bz@fill-\bz@hair)/2\relax}}{\pgfpoint{\bz@w}{\bz@hair}}%
    \pgfusepath{fill}%
    \bz@x=\dimexpr\bz@w*\bz@cur/\bz@tot\relax
    \pgfpathrectangle{\pgfpointorigin}{\pgfpoint{\bz@x}{\bz@fill}}%
    \pgfusepath{fill}%
    \pgfsetlinewidth{\bz@hair}%
    \pgfsetstrokecolor{\bz@ink}%
    \forlistloop\bz@secdot\bz@secmarks
  \end{pgfpicture}%
  \fi}
\newcommand\bz@secdot[1]{%
  \bz@s=#1\relax
  \ifnum\bz@s>\bz@tot\else
    \bz@x=\dimexpr\bz@w*(\bz@s-1)/\bz@tot\relax
    \begin{pgfscope}%
      \ifnum\bz@cur<\bz@s \pgfsetfillcolor{\bz@paper}\fi
      \pgfpathcircle{\pgfpoint{\bz@x}{\dimexpr\bz@fill/2\relax}}{\bz@r}%
      \ifnum\bz@cur<\bz@s \pgfusepath{fill,stroke}\else\pgfusepath{fill}\fi
    \end{pgfscope}%
  \fi}
\newcommand\bz@countgap[1]{\ifnum#1>1 \ifnum#1>\bz@tot\else\advance\bz@i 1 \fi\fi}
\newcommand\bz@progressdots{%
  \bz@progressvalues
  \ifnum\bz@tot=0 \else
  \bz@i=0
  \forlistloop\bz@countgap\bz@secmarks
  \bz@pitch=\dimexpr 60mm/(\bz@tot+\bz@i)\relax
  \ifdim\bz@pitch>1.9mm \bz@pitch=1.9mm \fi
  \bz@gap=\bz@pitch
  \bz@r=0.29\bz@pitch
  \begin{pgfpicture}%
    \pgfsetbaseline{0pt}%
    \pgfsetlinewidth{\bz@hair}%
    \pgfsetstrokecolor{\bz@ink}%
    \pgfsetfillcolor{\bz@ink}%
    \bz@x=0pt \bz@i=0
    \loop\ifnum\bz@i<\bz@tot
      \advance\bz@i 1
      \ifnum\bz@i>1
        \advance\bz@x\bz@pitch
        \xifinlist{\the\bz@i}\bz@secmarks{\advance\bz@x\bz@gap}{}%
      \fi
      \pgfpathcircle{\pgfpoint{\bz@x}{0.5ex}}{\bz@r}%
      \ifnum\bz@i>\bz@cur \pgfusepath{stroke}\else\pgfusepath{fill}\fi
    \repeat
    \pgfpathrectangle{\pgfpoint{-\bz@r}{0pt}}{\pgfpoint{\dimexpr\bz@x+2\bz@r\relax}{0pt}}%
    \pgfusepath{use as bounding box}%
  \end{pgfpicture}%
  \fi}
\newcommand\bz@headright{%
  \ifx\bz@progress\bz@str@dots
    \settowidth\bz@foliow{\bz@totpages\ifbz@total\,/\,\bz@totpages\fi}%
    \bz@progressdots\hskip 2.6mm\hbox to\bz@foliow{\hss\bz@folio}%
  \else
    \bz@folio
  \fi}
\newcommand\bz@headrule{%
  \ifx\bz@progress\bz@str@bar\bz@rule@bar\else\bz@rule@none\fi}
\newcommand\bz@head[2]{%
  \vbox to\headheight{\vss
    \hbox to\textwidth{\hbox to\bz@headwidth{\color{\bz@ink}\bz@f@head\vphantom{Ag}#1\hfill#2}\hss}%
    \nointerlineskip\vskip 1.5mm
    \hbox to\textwidth{\bz@headrule\hss}}}
\def\ps@bleisatz{%
  \def\@oddhead{\ifbz@head\bz@head{\bz@headleft}{\bz@headright}\fi}%
  \let\@evenhead\@oddhead
  \def\@oddfoot{}\let\@evenfoot\@oddfoot}
\def\ps@bleisatztitle{%
  \def\@oddhead{\ifbz@head\bz@head{\bz@institute}{\@date}\fi}%
  \let\@evenhead\@oddhead
  \def\@oddfoot{}\let\@evenfoot\@oddfoot}
\pagestyle{bleisatz}

%% ---------------------------------------------------------------------
%%  Title block: flush left, Light title, a hairline, author and venue
%% ---------------------------------------------------------------------
\renewcommand\maketitle{\par
  \thispagestyle{bleisatztitle}%
  \begingroup\color{\bz@ink}%
  \vspace*{6mm}%
  \noindent\begin{minipage}[b]{0.86\linewidth}
    \bz@f@title\bz@nohyph\@title\par
    \ifx\bz@subtitle\@empty\else
      \vskip 3.5mm{\bz@f@subtitle\bz@nohyph\bz@subtitle\par}%
    \fi
  \end{minipage}\par
  \vskip 9mm
  \ifbz@hair\else\noindent\rule{\linewidth}{\bz@hair}\par\fi
  \vskip 2mm
  \noindent\begin{minipage}[t]{0.62\linewidth}
    \bz@f@author\bz@ragged\@author\par
  \end{minipage}\hfill
  \begin{minipage}[t]{0.34\linewidth}
    \bz@f@author\RaggedLeft\parindent\z@\bz@venue\par
  \end{minipage}\par
  \ifx\bz@correspondence\@empty\else
    \vskip 2mm\noindent{\ifbz@ttype\bz@f@caption\else\bz@f@head\fi\bz@ragged\bz@correspondence\par}%
  \fi
  \vskip 9mm
  \endgroup
  \suppressfloats[t]%
  \@afterindentfalse\@afterheading}
\AtBeginDocument{\ifx\bz@shorttitle\@empty\let\bz@shorttitle\@title\fi}

% Abstract: label, text a step smaller, keywords; a hairline closes it
\renewenvironment{abstract}{%
  \par\noindent{\bz@f@label\abstractname\par}\nobreak\vskip 1mm
  \small\noindent\ignorespaces}{%
  \par
  \ifx\bz@keywords\@empty\else
    \vskip 2mm\noindent{\bz@f@head\bz@ragged
      {\bz@condbf Keywords}\enspace\bz@keywords\par}%
  \fi
  \ifbz@hair\vskip 6mm\else
  \vskip 3mm\noindent{\color{\bz@ink}\rule{\linewidth}{\bz@hair}}\par
  \vskip 4mm\fi\@afterindentfalse\@afterheading}

%% ---------------------------------------------------------------------
%%  Headings. Numbers hang in the left margin, text aligns with the body.
%%    \section        heavy rule above, semibold 14 pt   (slide: block)
%%    \subsection     semibold at body size
%%    \subsubsection  italic at body size, unnumbered
%%    \paragraph      run-in semibold
%% ---------------------------------------------------------------------
\setcounter{secnumdepth}{2}
\setcounter{tocdepth}{2}
\renewcommand\@seccntformat[1]{\llap{\csname the#1\endcsname\hskip 0.9em}}
\newcommand\bz@secstyle{%
  \par\noindent{\color{\bz@ink}\rule{\linewidth}{\bz@heavy}}\par
  \nobreak\nointerlineskip\vskip 2.2mm
  \bz@f@section\bz@nohyph}
\renewcommand\section{\@startsection{section}{1}{\z@}%
  {-8mm \@plus -2mm \@minus -1mm}{3.2mm}{\bz@secstyle}}
\renewcommand\subsection{\@startsection{subsection}{2}{\z@}%
  {-5mm \@plus -1.5mm \@minus -1mm}{1.2mm}{\bz@f@subsection\bz@nohyph}}
\renewcommand\subsubsection{\@startsection{subsubsection}{3}{\z@}%
  {-4mm \@plus -1mm \@minus -1mm}{0.6mm}{\bz@f@subsubsection\bz@nohyph}}
\renewcommand\paragraph{\@startsection{paragraph}{4}{\z@}%
  {2.5mm \@plus 1mm \@minus 0.5mm}{-0.6em}{\normalfont\bfseries}}
\renewcommand\sectionmark[1]{%
  \markboth{\ifnum\c@secnumdepth>\z@\bz@secsign\thesection\enspace\fi #1}{}%
  \bz@marksection}
% typography=tufte: a- and b-heads only, unnumbered, italic, no rules
\ifbz@ttype
  \setcounter{secnumdepth}{0}
  \renewcommand\bz@secstyle{\bz@f@section\bz@nohyph}
  \renewcommand\section{\@startsection{section}{1}{\z@}%
    {-7mm \@plus -2mm \@minus -1mm}{2.4mm}{\bz@secstyle}}
  \renewcommand\paragraph{\@startsection{paragraph}{4}{\z@}%
    {2.5mm \@plus 1mm \@minus 0.5mm}{-0.6em}{\normalfont\itshape}}
\fi
\renewcommand\subsectionmark[1]{}

% Contents: numbers in a 2.2 em column, page numbers flush right, no leaders
\renewcommand\tableofcontents{\section*{\contentsname}\markboth{\contentsname}{}%
  \@starttoc{toc}}
\newcommand\bz@tocrow[4]{% indent, number width, text, page
  \begingroup
  \leftskip=\dimexpr#1+#2\relax \rightskip=2.5em plus 1fil \parfillskip=-2.5em
  \parindent=\z@ \@tempdima=#2\relax
  \bz@f@toc\noindent\hskip-#2\relax #3\nobreak\hfill\nobreak
  \hbox to 2.5em{\hss\bz@cond #4}\par
  \endgroup}
\renewcommand*\l@section[2]{\addpenalty{-\@highpenalty}\vskip 1.2mm\@plus\p@
  \bz@tocrow{\z@}{2.2em}{#1}{#2}}
\renewcommand*\l@subsection[2]{\bz@tocrow{2.2em}{2.6em}{#1}{#2}}
\renewcommand*\l@subsubsection[2]{}

%% ---------------------------------------------------------------------
%%  Lists: markers hang in the margin, text aligns with the body
%% ---------------------------------------------------------------------
\setlist{itemsep=0.35ex plus 0.2ex, parsep=0pt, topsep=0.9ex plus 0.3ex, partopsep=0pt}
\newcommand\bz@mark[1]{\ifbz@ttype\textcolor{bzgrey}{#1}\else#1\fi}
\setlist[itemize,1]{label=\bz@mark{\textendash}, leftmargin=0pt, labelsep=0.55em,
  labelwidth=1em, itemindent=0pt, labelindent=!, align=right}
\setlist[itemize,2]{label=\bz@mark{\textendash}, leftmargin=1.1em, labelsep=0.55em,
  labelwidth=1em, itemindent=0pt, labelindent=!, align=right}
\setlist[itemize,3]{label=\bz@mark{\textperiodcentered}, leftmargin=1.1em, labelsep=0.55em,
  labelwidth=1em, itemindent=0pt, labelindent=!, align=right}
\setlist[enumerate,1]{label=\arabic*, leftmargin=0pt, labelsep=0.55em,
  labelwidth=1.4em, itemindent=0pt, labelindent=!, align=right}
\setlist[enumerate,2]{label=\arabic{enumi}.\arabic*, leftmargin=1.1em, labelsep=0.55em,
  labelwidth=1.8em, itemindent=0pt, labelindent=!, align=right}
\setlist[description]{font=\normalfont\bfseries, leftmargin=0pt, labelsep=0.6em}

%% ---------------------------------------------------------------------
%%  Emphasis
%% ---------------------------------------------------------------------
% \alert{...}: weight, never colour (C64: reverse video)
\newsavebox\bz@rvs
\ifx\bz@font\bz@str@csf
  \DeclareRobustCommand\alert[1]{\begin{lrbox}{\bz@rvs}\color{\bz@paper}#1\end{lrbox}%
    {\setlength\fboxsep{0pt}\colorbox{\bz@ink}{\usebox\bz@rvs}}}
\else
  \DeclareRobustCommand\alert[1]{{\bz@alert@font #1}}
\fi
% inline inversion for the one word that matters
\ifbz@italic\ifx\bz@font\bz@str@csf\else\def\bz@invitalic{}\fi\fi
\DeclareRobustCommand\inv[1]{\ifdefined\bz@invitalic{\itshape #1}\else\bz@invbox{#1}\fi}
\newcommand\bz@invbox[1]{{\setlength\fboxsep{0.15em}%
  \colorbox{\bz@ink}{\color{\bz@paper}\vphantom{Ågy}#1}}}
% small label (condensed semibold) for panels, table heads, side notes
\DeclareRobustCommand\smalllabel[1]{{\bz@f@label #1}}
% side note in the right margin
\newcommand\sidenote[1]{\marginpar{\bz@f@caption\bz@ragged #1\par}}

%% ---------------------------------------------------------------------
%%  Blocks — no boxes. Rules and inversion only (as on the slides).
%%    block         heavy rule above, title in semibold
%%    exampleblock  hairline above and below, title in italic
%%    alertblock    inverted: paper on ink
%% ---------------------------------------------------------------------
\newcommand\bz@ifempty[1]{\if\relax\detokenize{#1}\relax\expandafter\@firstoftwo
  \else\expandafter\@secondoftwo\fi}
\newcommand\bz@blockfont{\ifbz@ttype\itshape\else\bfseries\fi}
\newcommand\bz@softrule{\ifbz@hair bzgrey\else\bz@ink\fi}
\newenvironment{block}[1]{%
  \par\addvspace{\medskipamount}%
  \ifbz@hair\else\noindent{\color{\bz@ink}\rule{\linewidth}{\bz@heavy}}\par\fi
  \nobreak\nointerlineskip\vskip 1.6mm
  \bz@ifempty{#1}{}{{\noindent\bz@blockfont\bz@nohyph #1\par}\nobreak\vskip 0.6mm}%
  \noindent\ignorespaces}{%
  \par\addvspace{\medskipamount}\bz@afterdisplay}
\newenvironment{exampleblock}[1]{%
  \par\addvspace{\medskipamount}%
  \noindent{\color{\bz@softrule}\rule{\linewidth}{\bz@hair}}\par
  \nobreak\nointerlineskip\vskip 1.6mm
  \bz@ifempty{#1}{}{{\noindent\itshape\bz@nohyph #1\par}\nobreak\vskip 0.6mm}%
  \noindent\ignorespaces}{%
  \par\nobreak\vskip 1.4mm
  \noindent{\color{\bz@softrule}\rule{\linewidth}{\bz@hair}}\par
  \addvspace{\medskipamount}\bz@afterdisplay}
\newbox\bz@abox
\newlength\bz@pad \setlength\bz@pad{2.4mm}
\newenvironment{alertblock}[1]{%
  \par\addvspace{\medskipamount}%
  \setbox\bz@abox\vbox\bgroup
    \hsize=\dimexpr\linewidth-2\bz@pad\relax \linewidth=\hsize
    \bz@swap
    \bz@ifempty{#1}{}{{\noindent\bfseries\bz@nohyph #1\par}\vskip 0.6mm}%
    \noindent\ignorespaces}{%
  \par\egroup
  \noindent{\setlength\fboxsep{\bz@pad}\colorbox{\bz@ink}{\box\bz@abox}}\par
  \addvspace{\medskipamount}\bz@afterdisplay}
% inverse panel: the document counterpart of an inverse frame
\newenvironment{inverse}{%
  \par\addvspace{\bigskipamount}%
  \setbox\bz@abox\vbox\bgroup
    \hsize=\dimexpr\linewidth-12mm\relax \linewidth=\hsize
    \bz@swap\vskip 2mm\noindent\ignorespaces}{%
  \par\vskip 3mm\egroup
  \noindent{\setlength\fboxsep{6mm}\colorbox{\bz@ink}{\box\bz@abox}}\par
  \addvspace{\bigskipamount}\bz@afterdisplay}
% emphasis=italic: no reversed type. The alert block is set off by ink
% hairlines and an italic title, an inverse panel by white space alone.
\ifbz@italic
  \renewenvironment{alertblock}[1]{%
    \par\addvspace{\medskipamount}%
    \noindent{\color{\bz@ink}\rule{\linewidth}{\bz@hair}}\par
    \nobreak\nointerlineskip\vskip 1.6mm
    \bz@ifempty{#1}{}{{\noindent\itshape\bz@nohyph #1\par}\nobreak\vskip 0.6mm}%
    \noindent\ignorespaces}{%
    \par\nobreak\vskip 1.4mm
    \noindent{\color{\bz@ink}\rule{\linewidth}{\bz@hair}}\par
    \addvspace{\medskipamount}\bz@afterdisplay}
  \renewenvironment{inverse}{\par\addvspace{\bigskipamount}}{%
    \par\addvspace{\bigskipamount}\bz@afterdisplay}
\fi

%% Statement: one sentence, set large. Pull quote: hanging quotation marks.
\newenvironment{statement}{%
  \par\addvspace{\bigskipamount}\noindent\begin{minipage}{0.84\linewidth}%
  \bz@f@statement\bz@nohyph}{%
  \par\end{minipage}\par\addvspace{\bigskipamount}\bz@afterdisplay}
\newenvironment{pullquote}[1][]{%
  \def\bz@attrib{#1}%
  \par\addvspace{\bigskipamount}\noindent\begin{minipage}{0.84\linewidth}%
  \bz@f@statement\bz@nohyph
  \leavevmode\llap{\textquotedblleft}\ignorespaces}{%
  \unskip\textquotedblright\par
  \ifx\bz@attrib\@empty\else
    \vskip 4mm{\normalfont\normalsize\textendash\enspace\bz@attrib\par}%
  \fi
  \end{minipage}\par\addvspace{\bigskipamount}\bz@afterdisplay}

%% Key figure: one number, always with what it measures and its uncertainty
%%   \keyfigure{43\,\%}{of patients ...}{95\,\% CI 38--48 \textbullet\ n = 312}
\newcommand\keyfigure[3]{%
  \par\addvspace{\bigskipamount}%
  \noindent{\bz@f@keyfigure #1\par}\nobreak\vskip 3mm
  \noindent\begin{minipage}{0.62\linewidth}\bz@nohyph
    {\bz@f@subtitle #2\par}\vskip 1.5mm
    {\bz@f@head #3\par}%
  \end{minipage}\par\addvspace{\bigskipamount}\@afterindentfalse\@afterheading}

%% ---------------------------------------------------------------------
%%  Figures and tables
%%  Figures are included at 100 % so their text stays at 8-9 pt (APA).
%%    \sidefigure[<pos>]{<file>}{<caption>}[<source>]   caption beside
%%    \fullfigure[<pos>]{<file>}{<caption>}[<source>]   caption below
%%  <file> may also be any graphic code (\includegraphics[...]{...}, tikz).
%%  \figurefolder{figures} points \graphicspath at the matching mode
%%  folder: figures/light/ or figures/dark/, figures/c64/<mode>/ for c64.
%% ---------------------------------------------------------------------
\newcommand\bz@figmode{\ifbz@dark dark\else light\fi}
\newcommand\figurefolder[1]{%
  \edef\bz@tmp{\noexpand\graphicspath{{#1/\ifx\bz@font\bz@str@csf c64/\fi\bz@figmode/}}}%
  \bz@tmp}
\newcommand\bz@graphic[1]{\in@{\includegraphics}{#1}%
  \ifin@ #1\else\in@{\begin}{#1}\ifin@ #1\else\includegraphics{#1}\fi\fi}
\renewcommand\figurename{Figure}
\renewcommand\tablename{Table}
\setlength\abovecaptionskip{2.2mm}
\setlength\belowcaptionskip{0pt}
\newif\ifbz@inlinecap
\def\bz@capsrc{}
\long\def\@makecaption#1#2{%
  \ifbz@tlayout
    \ifbz@inlinecap\bz@tuftecaption{#1}{#2}\else\bz@margincaption{#1}{#2}\fi
  \else
  \vskip\abovecaptionskip
  {\color{\bz@ink}\bz@f@label\bz@ragged #1\par}\nobreak\vskip 0.8mm
  {\color{\bz@ink}\bz@f@caption\bz@ragged #2\par}%
  \vskip\belowcaptionskip
  \fi}
% tufte captions: label run in, in italic; small; in the margin, level with
% the top of what follows (zero height in the column), or in the column
\long\def\bz@tuftecaption#1#2{%
  \vskip\abovecaptionskip
  {\color{\bz@ink}\bz@f@caption\bz@ragged{\bz@f@label #1.}\enspace #2\par
   \ifx\bz@capsrc\@empty\else\vskip 1mm{\bz@f@source\bz@ragged\bz@capsrc\par}\fi}}
\long\def\bz@margincaption#1#2{%
  \par\nointerlineskip
  \vbox to\z@{\moveright\dimexpr\linewidth+\marginparsep\relax
    \vtop{\hsize\marginparwidth \linewidth\hsize \kern\z@
      \color{\bz@ink}\bz@f@caption\bz@ragged{\bz@f@label #1.}\enspace #2\par
      \ifx\bz@capsrc\@empty\else\vskip 1mm{\bz@f@source\bz@ragged\bz@capsrc\par}\fi}\vss}%
  \nointerlineskip}
\newcommand\source[1]{\par\vskip 1.2mm{\color{\bz@ink}\bz@f@source\bz@ragged #1\par}}
\newsavebox\bz@gbox
\newlength\bz@capgap \setlength\bz@capgap{4mm}
\NewDocumentCommand\sidefigure{O{tbp} m m o}{%
  \begin{figure}[#1]%
    \sbox\bz@gbox{\bz@graphic{#2}}%
    \noindent\begin{minipage}[t]{\wd\bz@gbox}\vspace{0pt}\usebox\bz@gbox\end{minipage}%
    \hfill
    \begin{minipage}[t]{\dimexpr\linewidth-\wd\bz@gbox-\bz@capgap\relax}\vspace{0pt}%
      \setlength\abovecaptionskip{0pt}%
      \caption{#3}%
      \IfValueT{#4}{\source{#4}}%
    \end{minipage}%
  \end{figure}}
\NewDocumentCommand\fullfigure{O{tbp} m m o}{%
  \begin{figure}[#1]%
    \noindent\bz@graphic{#2}\par
    \noindent\begin{minipage}[t]{0.72\linewidth}%
      \caption{#3}%
      \IfValueT{#4}{\source{#4}}%
    \end{minipage}%
  \end{figure}}
%% layout=tufte: \sidefigure keeps the graphic in the column and its caption in the
%% margin; \fullfigure lets 134 mm graphics reach into the margin, caption
%% below. \marginfigure and margintable put small ones beside the text;
%% fullwidth spans column and margin.
\ifbz@tlayout
  \RenewDocumentCommand\sidefigure{O{tbp} m m o}{%
    \begin{figure}[#1]%
      \IfValueTF{#4}{\def\bz@capsrc{#4}}{\def\bz@capsrc{}}%
      \caption{#3}%
      \noindent\makebox[\linewidth][l]{\bz@graphic{#2}}\par
    \end{figure}}
  \RenewDocumentCommand\fullfigure{O{tbp} m m o}{%
    \begin{figure}[#1]%
      \noindent\makebox[\linewidth][l]{\bz@graphic{#2}}\par
      \bz@inlinecaptrue
      \IfValueTF{#4}{\def\bz@capsrc{#4}}{\def\bz@capsrc{}}%
      \noindent\begin{minipage}[t]{\linewidth}\caption{#3}\end{minipage}%
    \end{figure}}
\fi
\NewDocumentCommand\marginfigure{O{0pt} m m o}{%
  \marginpar{\vspace*{#1}\bz@inlinecaptrue\def\@captype{figure}%
    \setlength\abovecaptionskip{1.5mm}%
    \IfValueTF{#4}{\def\bz@capsrc{#4}}{\def\bz@capsrc{}}%
    \in@{\includegraphics}{#2}%
    \ifin@ #2\else\includegraphics[width=\linewidth]{#2}\fi\par
    \caption{#3}\ifbz@tlayout\else\IfValueT{#4}{\source{#4}}\fi}}
\NewDocumentEnvironment{margintable}{O{0pt} +b}{%
  \marginpar{\vspace*{#1}\bz@inlinecaptrue\def\@captype{table}%
    \setlength\abovecaptionskip{0pt}\setlength\belowcaptionskip{1.5mm}%
    \footnotesize\bz@tabnums #2}}{}
\newenvironment{fullwidth}{%
  \list{}{\leftmargin\z@ \rightmargin-\dimexpr\marginparsep+\marginparwidth\relax
    \listparindent\parindent \itemindent\z@ \parsep\z@ \topsep\z@ \partopsep\z@}%
  \item\relax}{\endlist}
% tables: caption above (APA), body a step smaller, booktabs rules
\newcommand\bz@tablefont{\ifx\bz@font\bz@str@csf\footnotesize\else\small\fi}
\AtBeginEnvironment{table}{%
  \setlength\abovecaptionskip{0pt}%
  \ifbz@tlayout\setlength\belowcaptionskip{0pt}\else\setlength\belowcaptionskip{2.2mm}\fi
  \bz@tablefont}
\AtBeginEnvironment{tabular}{\bz@tabnums}
\AtBeginEnvironment{tabular*}{\bz@tabnums}
% C64: half a cell between columns, so tables keep to the cell grid
\ifx\bz@font\bz@str@csf\setlength\tabcolsep{0.5em}\fi
\setlength\heavyrulewidth{\bz@heavy}
\setlength\lightrulewidth{\bz@hair}
\setlength\cmidrulewidth{\bz@hair}
%% \figurefolder{figures}: \graphicspath for the options of this document
%%   figures/<mode>/, C64 figures/c64/<mode>/, and the Tufte set
%%   figures/tufte/<mode><palette>/ (palette tufte: "", grey: -grey, onebit: -1bit);
%%   with figures=tufte the Tufte set is searched first, otherwise last, so
%%   the same file names work with the switch on and off
\newcommand\bz@palsuffix{\ifbz@colour\else\ifbz@grey -grey\else -1bit\fi\fi}
\renewcommand\figurefolder[1]{%
  \edef\bz@tmp{\noexpand\graphicspath{%
    \ifbz@tfigs{#1/\ifx\bz@font\bz@str@csf c64/\fi tufte/\bz@figmode\bz@palsuffix/}\fi
    {#1/\ifx\bz@font\bz@str@csf c64/\fi\bz@figmode/}%
    \ifbz@tfigs\else{#1/\ifx\bz@font\bz@str@csf c64/\fi tufte/\bz@figmode\bz@palsuffix/}\fi}}%
  \bz@tmp}

%% ---------------------------------------------------------------------
%%  Sparklines (Tufte, Beautiful Evidence): data-intense, design-simple,
%%  word-sized. No axes, no labels; write the numbers beside them.
%%    \sparkline[<options>]{9.0, 16.9, 20.7, 19.1, 17.2, 13.3, 10.1, 9.4}
%%  Options (key=value):
%%    width=4em  height=1.5ex
%%    marks=last|minmax|both|none   last value in Tufte's red, highs and lows in
%%                      green (palette=tufte); otherwise filled and open ink dots
%%    band={lo,hi}      a normal range behind the line (grey; 1-bit: dotted rules)
%%    range={min,max}   fix the vertical scale (to compare several sparklines)
%%    at={0,2,4,...}    x positions (e.g. hours) for unevenly spaced samples
%% ---------------------------------------------------------------------
\pgfkeys{/bz/spark/.is family, /bz/spark,
  width/.store in=\bz@sk@w, height/.store in=\bz@sk@h, marks/.store in=\bz@sk@marks,
  band/.store in=\bz@sk@band, range/.store in=\bz@sk@range, at/.store in=\bz@sk@at}
\def\bz@sk@split#1,#2\@nil#3#4{\def#3{#1}\def#4{#2}}
\newdimen\bz@skW \newdimen\bz@skH \newdimen\bz@skA \newdimen\bz@skB
% a mark: f = filled ink, o = open, r = Tufte's red (last value), g = green (high/low);
% without colour in the palette red and green fall back to filled ink
\newcommand\bz@sk@dot[3]{% x, y, kind
  \begin{pgfscope}%
  \pgfpathcircle{\pgfpoint{#1}{#2}}{0.8pt}%
  \if o#3\pgfsetfillcolor{\bz@paper}\pgfusepath{fill,stroke}%
  \else
    \if r#3\ifbz@colour\pgfsetfillcolor{bzred}\fi\fi
    \if g#3\ifbz@colour\pgfsetfillcolor{bzgreen}\fi\fi
    \pgfusepath{fill}%
  \fi
  \end{pgfscope}}
\DeclareRobustCommand\sparkline[2][]{%
  \begingroup
  \pgfkeys{/bz/spark, width=4em, height=1.5ex, marks=last, band=, range=, at=, #1}%
  \gdef\bz@sk@lo{16000}\gdef\bz@sk@hi{-16000}\gdef\bz@sk@n{0}%
  \foreach \bz@v in {#2}{%
    \pgfmathsetmacro\bz@t{min(\bz@sk@lo,\bz@v)}\xdef\bz@sk@lo{\bz@t}%
    \pgfmathsetmacro\bz@t{max(\bz@sk@hi,\bz@v)}\xdef\bz@sk@hi{\bz@t}%
    \xdef\bz@sk@n{\the\numexpr\bz@sk@n+1\relax}}%
  \let\bz@sk@dlo\bz@sk@lo \let\bz@sk@dhi\bz@sk@hi
  \ifx\bz@sk@band\@empty\else
    \expandafter\bz@sk@split\bz@sk@band\@nil\bz@sk@blo\bz@sk@bhi
    \pgfmathsetmacro\bz@sk@lo{min(\bz@sk@lo,\bz@sk@blo)}%
    \pgfmathsetmacro\bz@sk@hi{max(\bz@sk@hi,\bz@sk@bhi)}%
  \fi
  \ifx\bz@sk@range\@empty\else
    \expandafter\bz@sk@split\bz@sk@range\@nil\bz@sk@lo\bz@sk@hi
  \fi
  \pgfmathsetmacro\bz@sk@span{\bz@sk@hi-\bz@sk@lo}%
  \pgfmathtruncatemacro\bz@t{\bz@sk@span==0 ? 1 : 0}\ifnum\bz@t=1 \def\bz@sk@span{1}\fi
  \bz@skW=\bz@sk@w\relax \bz@skH=\bz@sk@h\relax
  \def\bz@sk@y##1{\pgfmathparse{(##1-\bz@sk@lo)/\bz@sk@span*\the\bz@skH}}%
  \ifx\bz@sk@at\@empty
    \def\bz@sk@x##1{\pgfmathparse{\bz@sk@n<2 ? 0 : (##1-1)/(\bz@sk@n-1)*\the\bz@skW}}%
  \else
    \foreach \bz@a [count=\bz@k] in \bz@sk@at {%
      \expandafter\xdef\csname bz@sk@at@\bz@k\endcsname{\bz@a}}%
    \edef\bz@sk@a{\csname bz@sk@at@1\endcsname}%
    \edef\bz@sk@b{\csname bz@sk@at@\bz@sk@n\endcsname}%
    \def\bz@sk@x##1{\edef\bz@t{\csname bz@sk@at@##1\endcsname}%
      \pgfmathparse{(\bz@t-\bz@sk@a)/(\bz@sk@b-\bz@sk@a)*\the\bz@skW}}%
  \fi
  \leavevmode\begin{pgfpicture}%
    \pgfpathrectangle{\pgfpointorigin}{\pgfpoint{\bz@skW}{\bz@skH}}%
    \pgfusepath{use as bounding box}%
    \pgfsetlinewidth{0.5pt}\pgfsetroundcap\pgfsetroundjoin
    \pgfsetstrokecolor{\bz@ink}\pgfsetfillcolor{\bz@ink}%
    \ifx\bz@sk@band\@empty\else
      \bz@sk@y{\bz@sk@blo}\bz@skA=\pgfmathresult pt
      \bz@sk@y{\bz@sk@bhi}\bz@skB=\pgfmathresult pt
      \advance\bz@skB-\bz@skA
      \begin{pgfscope}%
        \ifbz@grey
          \pgfsetfillcolor{bzgrey}%
          \pgfpathrectangle{\pgfpoint{0pt}{\bz@skA}}{\pgfpoint{\bz@skW}{\bz@skB}}%
          \pgfusepath{fill}%
        \else
          \pgfsetlinewidth{0.3pt}\pgfsetdash{{0.6pt}{0.9pt}}{0pt}%
          \advance\bz@skB\bz@skA
          \pgfpathmoveto{\pgfpoint{0pt}{\bz@skA}}\pgfpathlineto{\pgfpoint{\bz@skW}{\bz@skA}}%
          \pgfpathmoveto{\pgfpoint{0pt}{\bz@skB}}\pgfpathlineto{\pgfpoint{\bz@skW}{\bz@skB}}%
          \pgfusepath{stroke}%
        \fi
      \end{pgfscope}%
    \fi
    \foreach \bz@v [count=\bz@ski] in {#2}{%
      \bz@sk@x{\bz@ski}\let\bz@px\pgfmathresult
      \bz@sk@y{\bz@v}\let\bz@py\pgfmathresult
      \ifnum\bz@ski=1 \pgfpathmoveto{\pgfpoint{\bz@px pt}{\bz@py pt}}%
      \else\pgfpathlineto{\pgfpoint{\bz@px pt}{\bz@py pt}}\fi}%
    \pgfusepath{stroke}%
    \pgfsetlinewidth{0.4pt}%
    \def\bz@sk@doneMax{0}\def\bz@sk@doneMin{0}%
    \foreach \bz@v [count=\bz@ski] in {#2}{%
      \bz@sk@x{\bz@ski}\let\bz@px\pgfmathresult
      \bz@sk@y{\bz@v}\let\bz@py\pgfmathresult
      \def\bz@mk{}%
      \ifnum\bz@ski=\bz@sk@n\relax
        \def\bz@t{last}\ifx\bz@sk@marks\bz@t\def\bz@mk{r}\fi
        \def\bz@t{both}\ifx\bz@sk@marks\bz@t\def\bz@mk{r}\fi
      \fi
      \def\bz@t{minmax}\ifx\bz@sk@marks\bz@t\def\bz@mm{1}\else
        \def\bz@t{both}\ifx\bz@sk@marks\bz@t\def\bz@mm{1}\else\def\bz@mm{0}\fi\fi
      \ifnum\bz@mm=1
        \pgfmathtruncatemacro\bz@t{abs(\bz@v-\bz@sk@dhi)<1e-4 ? 1 : 0}%
        \ifnum\bz@t=1 \ifx\bz@mk\@empty\def\bz@mk{g}\fi\fi
        \pgfmathtruncatemacro\bz@t{abs(\bz@v-\bz@sk@dlo)<1e-4 ? 1 : 0}%
        \ifnum\bz@t=1 \ifx\bz@mk\@empty\ifbz@colour\def\bz@mk{g}\else\def\bz@mk{o}\fi\fi\fi
      \fi
      \ifx\bz@mk\@empty\else\bz@sk@dot{\bz@px pt}{\bz@py pt}{\bz@mk}\fi}%
  \end{pgfpicture}%
  \endgroup}

\newcommand\tablenote[1]{\par\vskip 1.4mm{\color{\bz@ink}\bz@f@caption\bz@ragged
  \textit{Note.}\enspace #1\par}}
\setlength\floatsep{6mm plus 2mm minus 1mm}
\setlength\textfloatsep{7mm plus 2mm minus 1mm}
\setlength\intextsep{6mm plus 2mm minus 1mm}
\renewcommand\topfraction{0.8}
\renewcommand\bottomfraction{0.6}
\renewcommand\textfraction{0.15}
\renewcommand\floatpagefraction{0.7}

%% ---------------------------------------------------------------------
%%  Footnotes: hairline, hanging numbers, ragged right
%% ---------------------------------------------------------------------
\renewcommand\footnoterule{\kern-3\p@{\color{\bz@ink}\hrule width 25mm height\bz@hair}\kern 2.6\p@}
%% notes=side: footnotes become numbered sidenotes in the margin, level with
%% their mark; \marginnote (= \sidenote) is the unnumbered kind
\ifbz@side
  \RenewDocumentCommand\footnote{o m}{%
    \IfValueTF{#1}{\setcounter{footnote}{#1}}{\stepcounter{footnote}}%
    \textsuperscript{\bz@liningnums\thefootnote}%
    \marginpar{\bz@f@caption\bz@ragged
      \leavevmode\llap{\textsuperscript{\bz@liningnums\thefootnote}\,}#2\par}}
\fi
\newcommand\marginnote[1]{\sidenote{#1}}
% \newthought{In his later books}, Tufte starts ... : a new thought without a
% heading opens in small capitals after a blank line
\newcommand\newthought[1]{\par\addvspace{1.6\baselineskip}%
  \noindent{\ifbz@etbb\scshape\else\itshape\fi #1}}
% epigraph: \begin{epigraph}[source] quotation \end{epigraph}
\newenvironment{epigraph}[1][]{\def\bz@attrib{#1}%
  \par\addvspace{\medskipamount}\begin{list}{}{\leftmargin 2.2em \rightmargin 2.2em}\item\relax
  \itshape\bz@ragged}{\par
  \ifx\bz@attrib\@empty\else\vskip 1mm{\upshape\small\textemdash\enspace\bz@attrib\par}\fi
  \end{list}\addvspace{\medskipamount}}
\renewcommand\@makefntext[1]{%
  \RaggedRight\parindent\z@\hangindent1.2em\hangafter\@ne
  \noindent\hbox to1.2em{\@thefnmark\hss}#1}
\setlength\footnotesep{8.5pt}

%% ---------------------------------------------------------------------
%%  Late set-up: hyperref (no boxes, no colour), bibliography
%% ---------------------------------------------------------------------
\AtEndPreamble{%
  \@ifpackageloaded{biblatex}{%
    \renewcommand*\bibfont{\small}%
    \setlength\bibhang{1.2em}%
    \setlength\bibitemsep{0.35ex}%
    % references ragged without hyphenation; a DOI never breaks after "https:"
    \appto\bibsetup{\setlength\RaggedRightRightskip{0pt plus 1fil}\RaggedRight}%
    \setcounter{biburlbigbreakpenalty}{10000}%
    \setcounter{biburllcpenalty}{7000}\setcounter{biburlucpenalty}{8000}%
    \setcounter{biburlnumpenalty}{7000}%
    \defbibheading{bibliography}[\refname]{\section*{#1}\markboth{#1}{}}%
    \defbibheading{bibintoc}[\refname]{\section*{#1}\markboth{#1}{}%
      \addcontentsline{toc}{section}{#1}}%
    % \sidecite[pages]{key}: the full reference as a numbered sidenote
    % (tufte) or footnote
    \providecommand\sidecite[2][]{\footnote{\fullcite[#1]{#2}}}}{%
    \providecommand\sidecite[2][]{\footnote{\cite[#1]{#2}}}}%
  \@ifpackageloaded{hyperref}{}{\RequirePackage[hidelinks,pdfusetitle]{hyperref}}%
  \hypersetup{hidelinks}}

%% ---------------------------------------------------------------------
%%  Charts (pgfplots): line style, marker shape and hatch instead of colour
%% ---------------------------------------------------------------------
\AtEndPreamble{%
  \@ifpackageloaded{pgfplots}{%
    \pgfplotscreateplotcyclelist{bleisatz}{%
      {bzink, solid, mark=*, mark options={solid, fill=bzink}},%
      {bzink, densely dashed, mark=o, mark options={solid, fill=bzpaper}},%
      {bzink, densely dotted, mark=square*, mark options={solid, fill=bzink}},%
      {bzink, dashdotted, mark=triangle, mark options={solid, fill=bzpaper}}}%
    \pgfplotscreateplotcyclelist{bleisatz bars}{%
      {draw=bzink, fill=bzink},%
      {draw=bzink, fill=bzpaper},%
      {draw=bzink, pattern=north east lines, pattern color=bzink},%
      {draw=bzink, pattern=dots, pattern color=bzink},%
      {draw=bzink, pattern=crosshatch, pattern color=bzink}}%
    \pgfplotsset{
      bleisatz/.style={
        axis lines=left,
        axis line style={bzink, line width=0.5pt, -},
        every tick/.style={bzink, line width=0.4pt},
        major tick length=1.1mm, tick align=outside,
        every tick label/.append style={font=\bz@f@source},
        label style={font=\bz@f@caption},
        legend style={draw=none, fill=none, font=\bz@f@caption},
        cycle list name=bleisatz,
        every axis plot/.append style={line width=0.8pt},
        mark size=1.5pt, clip=false},
      bleisatz bars/.style={
        bleisatz, cycle list name=bleisatz bars,
        every axis plot/.append style={line width=0.5pt},
        axis x line*=bottom, axis y line*=left}}%
    % tufte: axes end with the data and stand off it; bars without hatching
    \pgfplotscreateplotcyclelist{bleisatz tufte bars}{%
      {draw=none, fill=bzgrey},%
      {draw=none, fill=bzink},%
      {draw=none, fill=bzred},%
      {draw=bzink, fill=bzpaper}}%
    \pgfplotscreateplotcyclelist{bleisatz tufte}{%
      {bzink, solid, mark=*, mark options={solid, fill=bzink}},%
      {bzred, solid, mark=*, mark options={solid, fill=bzred}},%
      {bzgrey, solid, mark=*, mark options={solid, fill=bzgrey}},%
      {bzink, densely dashed, mark=o, mark options={solid, fill=bzpaper}}}%
    \ifbz@tfigs
      \pgfplotsset{
        bleisatz/.append style={
          enlargelimits=false, axis line style={line width=0.4pt},
          axis x line shift=1.5mm, axis y line shift=1.5mm,
          cycle list name=bleisatz tufte, mark size=1.1pt},
        bleisatz bars/.append style={cycle list name=bleisatz tufte bars,
          axis y line=none, ymajorgrids=false}}%
    \fi
  }{}}

\endinput
