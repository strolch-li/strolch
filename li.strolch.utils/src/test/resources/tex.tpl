\documentclass[12pt,a4paper]{report}
\usepackage[utf8]{inputenc}
\usepackage{amsmath}
\usepackage{amsfonts}
\usepackage{amssymb}
\usepackage{graphicx}
\usepackage[dvips,a4paper,top=2.5cm, bottom=2cm, left=1.5cm, right=1.5cm]{geometry}

\usepackage{ragged2e}
\usepackage{longtable}
\usepackage{array}
\newcolumntype{L}[1]{>{\raggedright\let\newline\\\arraybackslash\hspace{0pt}}m{#1}}
\newcolumntype{C}[1]{>{\centering\let\newline\\\arraybackslash\hspace{0pt}}m{#1}}
\newcolumntype{R}[1]{>{\raggedleft\let\newline\\\arraybackslash\hspace{0pt}}m{#1}}

\author{${doc_author}}
\title{${doc_title}}

\begin{document}

\clearpage
\thispagestyle{empty}

\raggedright

\textbf{${obj_name}} \newline

%{obj}: ${obj_id} \newline

%{last_modified}: ${obj_last_modified} \newline

%{validity}: ${validity_from} - ${validity_to} \newline

%{valid_for}: ${valid_for} \newline

\end{document}
