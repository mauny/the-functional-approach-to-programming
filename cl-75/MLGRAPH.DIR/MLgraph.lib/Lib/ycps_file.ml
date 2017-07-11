(*                                                                       *)
(*                     Projet      Formel                                *)
(*                                                                       *)
(* CAML-light: MLgraph library *)
(*                                                                       *)
(*************************************************************************)
(*                                                                       *)
(*                            LIENS                                      *)
(*                        45 rue d'Ulm                                   *)
(*                         75005 PARIS                                   *)
(*                            France                                     *)
(*                                                                       *)
(*************************************************************************)


#open "MLgraph";;

modify_body [
"%(*************************************************************************)";
"%(*                                                                       *)";
"%(*                     Projet      Formel                                *)";
"%(*                                                                       *)";
"%(*                    CAML-light: users' library                         *)";
"%(*                                                                       *)";
"%(*************************************************************************)";
"%(*                                                                       *)";
"%(*                            LIENS                                      *)";
"%(*                        45 rue d'Ulm                                   *)";
"%(*                         75005 PARIS                                   *)";
"%(*                            France                                     *)";
"%(*                                                                       *)";
"%(*************************************************************************)";
"";
"% $Id: cps_file.ps,v 1.1 1997/08/14 11:37:15 emmanuel Exp $";
"%(* cps_file.ps  The PostScript interface between Caml and PostScript     *)";
"%(*              prelude of the output file                               *)";
"%(*              Emmanuel Chailloux                                       *)";
"";
"%(*         -------  begining of the prelude   --------                   *)";
"";
"%(* 2000 = 1500 (caml) + 450 (gensym) + 15 (local)                        *)";
"";
"2000 dict begin";
"/maxnumsymb 450 def ";
"/numsymb 1 def";
"/st (G   ) def";
"";
"%(*  gensym     : automatic genaration of symbols from G1 to Gmaxnumsymb  *)";
"";
"/gensym";
"{numsymb maxnumsymb lt  ";
" {";
"  /sc numsymb (   ) cvs def";
"  /sl sc length def";
"  st 1 sc putinterval st 0 sl 1 add getinterval   cvx cvn      ";
"  /numsymb numsymb 1 add def";
" }";
" {[ numsymb /gensymoverflow gensymoverflow]}";
" ifelse";
"}";
"def";
"";
"/stack_to_symb { gensym dup 3 2 roll   def } def";
"/sts { stack_to_symb} bind def";
"";
"";
"%(* new image operators                                                    *)";
"";
"/newimage { {currentfile picstr readhexstring pop} image ";
"	    currentfile picstr readline pop pop} def";
"";
"/newimagemask { { currentfile picstr readhexstring pop} imagemask";
"                  currentfile picstr readline pop pop} def";
"";
"";
"%(* convertion angle  : unused with standard PostScript                *)";
"/ca {} def  ";
"";
"%(* fast find, scale and setfont                                       *)";
"/F {findfont exch scalefont setfont} bind def";
"";
"%(* string gray size fontname    F_gray_show ";
"%   string color size fontname   F_rgb_show";
"%   string color size fontname   F_hsb_show *)";
"";
"/F_gray_show {findfont exch scalefont setfont setgray 0.0 0.0 moveto show} ";
"bind def";
"/F_rgb_show {findfont exch scalefont setfont setrgbcolor 0.0 0.0 moveto show} ";
"bind def";
"/F_hsb_show {findfont exch scalefont setfont sethsbcolor 0.0 0.0 moveto show} ";
"bind def";
"";
"%(*  gray width cap join F_gray_linestyle";
"%   color width cap join F_rgb_linestyle";
"%   color width cap join F_hsb_linestyle *)";
"";
"/F_gray_linestyle {setlinejoin setlinecap setlinewidth setgray} bind def";
"/F_rgb_linestyle {setlinejoin setlinecap setlinewidth setrgbcolor} bind def";
"/F_hsb_linestyle {setlinejoin setlinecap setlinewidth sethsbcolor} bind def";
"";
"%(*            -------  end prolog   --------                          *)";
"%%EndProlog";
"";
"";
"";
"";
""];;
