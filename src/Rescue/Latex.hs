module Rescue.Latex where

    import Rescue.Base

    date :: String -> String
    date = id                          -- The default version is the same in LaTeX

    newline :: String -> String
    newline = replace "\n" "\\\\\\\\"  -- yes, there's 8 backslashes...
                                       -- and it only translates into 2 in the output
                      
    section :: String -> Resume a -> Resume a
    section s = surround ("\\begin{ressection}{" ++ s ++ "}\n")
                         "\\end{ressection}\n"

    subsection :: String -> Resume a -> Resume a
    subsection s = surround ("\\begin{ressubsection}{" ++ s ++ "}\n")
                            "\\end{ressubsection}\n"

    header :: String -> String -> String -> Resume a -> Resume a
    header title date comment = surround ("\\begin{resheader}{" ++ title ++ "}{"
                                          ++ date ++ "}{" ++ comment ++ "}\n")
                                         "\\end{resheader}\n"

    bigitem :: String -> String -> String -> Resume a
    bigitem loc time desc = Resume $ \n ->
      "\\resbigitem{" ++ loc ++ "}{" ++ time ++ "}{" ++ desc ++ "}\n" ++ n
                                         
    item :: String -> Resume a
    item s = Resume $ \n -> "\\resitem{" ++ s ++ "}\n" ++ n

    subitem :: String -> Resume a
    subitem s = Resume $ \n -> "\\ressubitem{" ++ s ++ "}\n" ++ n
                      
    preamble :: String -> String -> String -> String -> Resume a
    preamble name addr phone email = Resume $ \n -> unlines [
      "\\documentclass[11pt,oneside]{article}",
      "\\usepackage{geometry}",
      "\\usepackage[T1]{fontenc}",
      "\\usepackage{hyperref}",

      "\\pagestyle{empty}",
      "\\geometry{letterpaper,tmargin=1in,bmargin=1in,lmargin=0.75in,rmargin=0.75in,",
      "    headheight=0in,headsep=0in,footskip=.3in}",

      "\\setlength{\\parindent}{0in}",
      "\\setlength{\\parskip}{0in}",
      "\\setlength{\\itemsep}{0in}",
      "\\setlength{\\topsep}{0in}",
      "\\setlength{\\tabcolsep}{0in}",

      "%% New commands and environments",

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

      "% A subsection is nested within a section",
      "\\newenvironment{ressubsection}[1]{",
      "    \\resitem{\\textbf{#1}}",
      "    \\vspace{-5pt}",
      "    \\begin{itemize}",
      "}{",
      "    \\end{itemize}",
      "}",

      "% A sublist for complicated things",
      "%   Arg 1: Title",
      "%   Arg 2: Date",
      "%   Arg 3: Comment before subitems",
      "\\newenvironment{resheader}[3]{",
      "    \\resbigitem{#1}{#2}{#3}",
      "    \\vspace{-2pt}",
      "    \\begin{itemize}",
      "    \\vspace{-1.7em}",
      "}{",
      "    \\end{itemize}",
      "}",

      "% A resitem is a simple list element in a ressection (first level)",
      "\\newcommand{\\resitem}[1]{",
      "    \\vspace{-4pt}",
      "    \\item \\begin{flushleft} #1 \\end{flushleft}",
      "}",

      "% A resbigitem is a complex list element for stuff like jobs and education:",
      "%  Arg 1: Title",
      "%  Arg 2: Date",
      "%  Arg 3: Description",
      "\\newcommand{\\resbigitem}[3]{",
      "    \\item",
      "    \\textbf{#1} \\hfill \\textit{#2} \\\\",
      "    #3",
      "}",

      "% A ressubitem is a simple list element in anything but a ressection (second level)",
      "\\newcommand{\\ressubitem}[1]{",
      "    \\vspace{-1pt}",
      "    \\item \\begin{flushleft} #1 \\end{flushleft}",
      "}",

      "%% The actual document.",
      "\\begin{document}",

      "\\fontfamily{ppl} \\selectfont",

      "% Name with horizontal rule",
      "\\bigname{" ++ name ++ "}",
      "\\vspace{-8pt} \\rule{\\textwidth}{1pt}",
      "\\vspace{-1pt} {\\small\\itshape " ++ addr ++ " \\hfill "
          ++ phone ++ "; " ++ email ++ "}",
      "\\vspace{8 pt}\n\n"] ++ n

    postscript :: Resume a
    postscript = Resume $ ("\\end{document}\n"++)