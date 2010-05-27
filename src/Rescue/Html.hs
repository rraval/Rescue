module Rescue.Html where

    import Rescue.Base

    -- String formatting functions
        
    date :: String -> String
    date = replace "--" "&ndash;"
        
    newline :: String -> String
    newline = replace "\n" "<br/>"

    -- Markup functions

    section :: String -> Resume a -> Resume a
    section s = surround ("<h2>" ++ s ++ "</h2><ul>")
                         "</ul>"

    subsection :: String -> Resume a -> Resume a
    subsection s = surround ("<li><strong>" ++ s ++ "</strong><ul>")
                            "</ul></li>"

    header :: String -> String -> String -> Resume a -> Resume a
    header title date comment = surround ("<li><strong>" ++ title
                                          ++ "</strong> &mdash; <em>" ++ date
                                          ++ "</em><br/>" ++ comment ++ "<ul>")
                                         "</ul></li>"

    bigitem :: String -> String -> String -> Resume a
    bigitem title date comment = Resume $ \n -> "<li><strong>" ++ title
                                 ++ "</strong> &mdash; <em>" ++ date
                                 ++ "</em><br/>" ++ comment ++ "</li>" ++ n
                                         
    item :: String -> Resume a
    item s = Resume $ \n -> "<li>" ++ s ++ "</li>" ++n

    subitem :: String -> Resume a
    subitem = item

    preamble :: String -> String -> String -> String -> Resume a
    preamble name addr phone email = Resume $ \n -> unlines [
      "<html>",
      "<head>",
      "<title>" ++ name ++ "</title>",
      "<body>",
      "<center><h1>" ++ name ++ "</h1></center>",
      "<hr/>",
      "<table width=\"100%\">",
      "<tr>",
      "<td align=\"left\"><em>" ++ addr ++ "</em></td>",
      "<td align=\"right\"><em>" ++ phone ++ "; <a href=\"mailto:"
        ++ email ++ "\">" ++ email ++ "</a></em></td>",
      "</tr>",
      "</table>"] ++ n

    postscript :: Resume a
    postscript = Resume ("</body></html>"++)