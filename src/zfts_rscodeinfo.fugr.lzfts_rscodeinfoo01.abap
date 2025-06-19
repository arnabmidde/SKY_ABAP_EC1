*----------------------------------------------------------------------*
***INCLUDE LZFTS_RSCODEINFOO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  CUSTOM_BUTTON  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE custom_button INPUT.
CASE sy-ucomm.
    WHEN 'ASCEND'.
      PERFORM f_sort_ascend.
    WHEN 'DESCEND'.
      PERFORM f_sort_descend.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form sort_descending
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_sort_descend .
  DATA: lT_rscode TYPE TABLE OF zfts_rscodeinfo.
        lt_rscode = extract[].

SORT lt_rscode BY CUSTNUM DESCENDING MVNTYPE DESCENDING.
     extract[] = lt_rscode.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form sort_ascending
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_sort_ascend .
DATA: lT_rscode TYPE TABLE OF zfts_rscodeinfo.
      lt_rscode = extract[].

SORT lt_rscode BY CUSTNUM ASCENDING MVNTYPE ASCENDING.
      extract[] = lt_rscode.
ENDFORM.
