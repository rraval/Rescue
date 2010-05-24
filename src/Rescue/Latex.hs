module Rescue.Latex where

    data Resume a = Resume (String -> String)

    instance Monad Resume where
        (Resume r) >> f = Resume $ r . a
            where (Resume a) = f

        (>>=)  = undefined
        return = undefined

    instance Show (Resume a) where
        show (Resume a) = a ""

    date :: String -> String
    date = id

    newline :: String -> String
    newline = id

    surround :: String -> String -> Resume a -> Resume a
    surround start end resume = do
      Resume (start++)
      resume
      Resume (end++)
                      
    section :: String -> Resume a -> Resume a
    section s = surround ("\\begin{ressection}{" ++ s ++ "}\n")
                         "\\end{ressection}\n"

    subsection :: String -> Resume a -> Resume a
    subsection s = surround ("\\begin{reslist}{" ++ s ++ "}\n")
                            "\\end{reslist}\n"

    item :: String -> Resume a
    item s = Resume $ \n -> "\\resitem{" ++ s ++ "}\n" ++ n

    bigitem :: String -> String -> String -> Resume a
    bigitem loc time desc = Resume $ \n ->
      "\\resbigitem{" ++ loc ++ "}{" ++ time ++ "}{" ++ desc ++ "}\n" ++ n

    eduitem :: String -> String -> String -> String -> Resume a
    eduitem title loc time desc = Resume $ \n ->
      "\\eduitem{" ++ title ++ "}{" ++ loc ++ "}{" ++ time ++ "}{" ++ desc ++ "}\n" ++ n

    postscript :: Resume a
    postscript = Resume $ ("\\end{document}\n"++)
                      
    preamble :: String -> String -> String -> String -> Resume a
    preamble name addr phone email = Resume $ \n -> unlines [
      "\\documentclass[11pt,oneside]{article}",
      "\\usepackage{geometry}",
      "\\usepackage[T1]{fontenc}",
      "\\usepackage{hyperref}",

      "\\pagestyle{empty}",
      "\\geometry{letterpaper,tmargin=1in,bmargin=1in,lmargin=1in,rmargin=1in,",
      "    headheight=0in,headsep=0in,footskip=.3in}",

      "\\setlength{\\parindent}{0in}",
      "\\setlength{\\parskip}{0in}",
      "\\setlength{\\itemsep}{0in}",
      "\\setlength{\\topsep}{0in}",
      "\\setlength{\\tabcolsep}{0in}",

      "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%",
      "% New commands and environments",

      "% This defines how the name looks",
      "\\newcommand{\\bigname}[1]{",
      "    \\begin{center}\\fontfamily{phv}\\selectfont\\Huge\\scshape#1\\end{center}",
      "}",

      "\\newcommand{\\respara}[1]{",
      "    \\vspace{4pt}",
      "    {\\fontfamily{phv} \\selectfont \\Large #1} \\\\",
      "    \\vspace{4pt}",
      "    \\hspace{19pt}",
      "}",

      "% A ressection is a main section (<H1>Section</H1>)",
      "\\newenvironment{ressection}[1]{",
      "    \\respara{#1}",
      "    \\begin{itemize}",
      "    \\vspace{-20pt}",
      "}{",
      "    \\end{itemize}",
      "}",

      "% A resitem is a simple list element in a ressection (first level)",
      "\\newcommand{\\resitem}[1]{",
      "    \\vspace{-4pt}",
      "    \\item \\begin{flushleft} #1 \\end{flushleft}",
      "}",

      "\\newcommand{\\resrightitem}[1]{",
      "    \\vspace{-4pt}",
      "    \\item \\begin{flushright} #1 \\end{flushright}",
      "}",

      "% A ressubitem is a simple list element in anything but a ressection (second level)",
      "\\newcommand{\\ressubitem}[1]{",
      "    \\vspace{-1pt}",
      "    \\item \\begin{flushleft} #1 \\end{flushleft}",
      "}",

      "% A resbigitem is a complex list element for stuff like jobs and education:",
      "%  Arg 1: Name of company or university",
      "%  Arg 2: Location",
      "%  Arg 3: Title and/or date range",
      "\\newcommand{\\resbigitem}[3]{",
      "    %\\vspace{-5pt}",
      "    \\item",
      "    \\textbf{#1} \\hfill \\textit{#2} \\\\",
      "    #3",
      "}",

      "\\newcommand{\\eduitem}[4]{",
      "  \\item",
      "    \\textbf{#1} \\hfill \\textit{#2} \\\\ \\mbox{} \\hfill \\textit{#3} \\\\",
      "    #4",
      "}",

      "% This is a list that comes with a resbigitem",
      "\\newenvironment{ressubsec}[3]{",
      "    \\resbigitem{#1}{#2}{#3}",
      "    \\vspace{-2pt}",
      "    \\begin{itemize}",
      "    \\vspace{-1.7em}",
      "}{",
      "    \\end{itemize}",
      "}",

      "% This is a simple sublist",
      "\\newenvironment{reslist}[1]{",
      "    \\resitem{\\textbf{#1}}",
      "    \\vspace{-5pt}",
      "    \\begin{itemize}",
      "}{",
      "    \\end{itemize}",
      "}",



      "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%",
      "% Now for the actual document:",

      "\\begin{document}",

      "\\fontfamily{ppl} \\selectfont",

      "% Name with horizontal rule",
      "\\bigname{" ++ name ++ "}",
      "\\vspace{-8pt} \\rule{\\textwidth}{1pt}",
      "\\vspace{-1pt} {\\small\\itshape " ++ addr ++ " \\hfill "
          ++ phone ++ "; " ++ email ++ "}",
      "\\vspace{8 pt}\n\n"] ++ n