CLASS zcl_fts_order_reason_code DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

*"* public components of class ZCL_FTS_ORDER_REASON_CODE
*"* do not include other source files here!!!
  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES if_ex_mb_migo_badi .

    TYPES: BEGIN OF gty_set_rescd,
             zeile     TYPE mblpo,
             set_rescd TYPE xfeld,
           END OF gty_set_rescd.

    CLASS-DATA z_reas_cd TYPE ztt_order_reason .
    DATA gt_set_rescd TYPE STANDARD TABLE OF gty_set_rescd .
protected section.
*"* protected components of class ZCL_FTS_ORDER_REASON_CODE
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_FTS_ORDER_REASON_CODE
*"* do not include other source files here!!!

  data GT_EXTDATA type TY_T_EXTDATA .
  constants GC_CLASS_ID type MIGO_CLASS_ID value 'ZCL_FTS_ORDER_REASON_CODE' ##NO_TEXT.
  data GV_NO_INPUT type XFELD .
  data GV_CANCEL type XFELD .
  "DATA: GV_zz_augru TYPE zdev_control-name4.
ENDCLASS.



CLASS ZCL_FTS_ORDER_REASON_CODE IMPLEMENTATION.


METHOD IF_EX_MB_MIGO_BADI~CHECK_HEADER .
ENDMETHOD.                    "IF_EX_MB_MIGO_BADI~CHECK_HEADER


METHOD if_ex_mb_migo_badi~check_item .



ENDMETHOD.                    "IF_EX_MB_MIGO_BADI~CHECK_ITEM


METHOD IF_EX_MB_MIGO_BADI~HOLD_DATA_DELETE .
ENDMETHOD.


METHOD IF_EX_MB_MIGO_BADI~HOLD_DATA_LOAD .
ENDMETHOD.                    "if_ex_mb_migo_badi~hold_data_load


METHOD IF_EX_MB_MIGO_BADI~HOLD_DATA_SAVE .
ENDMETHOD.                    "if_ex_mb_migo_badi~hold_data_save


METHOD if_ex_mb_migo_badi~init.


ENDMETHOD.


METHOD IF_EX_MB_MIGO_BADI~LINE_DELETE.
ENDMETHOD.                    "IF_EX_MB_MIGO_BADI~LINE_DELETE


METHOD if_ex_mb_migo_badi~line_modify.
*& Confidential Property of Stryker
*& All Rights Reserved
*&************************************************************************&*
*& Program      : ZFTS_EI_ORDER_REASON_CODE                               &*
*& Company      : Stryker Project Accelerate                              &*
*& Author       : Langalia Mit                                            &*
*& Date         : July 29 2024                                            &*
*& Title        : Enhancement Implementation for Order Reason Code        &*
*& FD #         : FTS.EXT.353                                             &*
*&************************************************************************&*
*& DESCRIPTION  : Enhancement Implementation created for                  &*
*&                managing order reason code                              &*
*&************************************************************************&*
*& H I S T O R Y       O F       R E V I S I O N S &                      &*
*&************************************************************************&*
*& Date        Author           Description              Transport Number &*
*& 07/29/2024  MLANGALI     R7-Ortho_FTS_EXT_353_Update     EC2K905264    &*
*&                          Reason code in consigned                      &*
*&                          inventory adjustments - Initial               &*
*&                          Version                                       &*
*& 12/20/2024  PAKBARI     R7-Ortho_FTS_EXT_361 - CR943443  EC1K923495    &*
*&      Update Reason code only for capitalized batch-managed materils    &*
*&************************************************************************&*
**---->Begin of change R7-ORTHO/PAKBARI/CR943443/EC1K923495
  DATA: lwa_rescd TYPE gty_set_rescd,
        lv_atwrt  TYPE atwrt,
        lv_atinn  TYPE atinn.

  CONSTANTS: lc_atnam TYPE atnam      VALUE 'ZCAP_CATEGORIES',
             lc_atwrt TYPE atwrt      VALUE 'Y_BATCH'.
**---->End of change R7-ORTHO/PAKBARI/CR943443/EC1K923495

  IF ( cs_goitem-bwart IS NOT INITIAL AND
       cs_goitem-sobkz IS NOT INITIAL AND
       cs_goitem-kunnr IS NOT INITIAL AND
       cs_goitem-matnr IS NOT INITIAL ). "++R7-ORTHO/PAKBARI/CR943443/EC1K923495

**---->Begin of change R7-ORTHO/PAKBARI/CR943443/EC1K923495
    " Get Internal characteristic value for ZCAP_CATEGORIES
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = lc_atnam
      IMPORTING
        output = lv_atinn.
    CLEAR lv_atwrt.
    " Check if material is capitalized batch managed
    SELECT atwrt
      INTO @lv_atwrt
      FROM ausp
      UP TO 1 ROWS
    WHERE objek = @cs_goitem-matnr
    AND   atinn = @lv_atinn
    AND   atwrt = @lc_atwrt.
    ENDSELECT.
    " Process only if the material is capitalized batch managed
    IF sy-subrc = 0.
**---->End of change R7-ORTHO/PAKBARI/CR943443/EC1K923495

      "Fetch the corresponding reason code from zfts_rscodeinfo table
      SELECT rscode,description
             FROM zfts_rscodeinfo
             INTO (@DATA(lv_rscode), @DATA(lv_desc))
              UP TO 1 ROWS
            WHERE custnum   = @cs_goitem-kunnr
              AND mvntype   = @cs_goitem-bwart
              AND splind    = @cs_goitem-sobkz.
      ENDSELECT.

      "Display the reason code in Reason for movement field
      IF sy-subrc = 0.
        cs_goitem-grund = lv_rscode.
*      cs_goitem-grtxt = lv_desc. " -- del  R7-ORTHO/PAKBARI/CR943443/EC1K923495
**** Begin of Ins R7-ORTHO/PAKBARI/CR943443/EC1K923495
        IF NOT line_exists( gt_set_rescd[ zeile = cs_goitem-zeile ] ).
          APPEND VALUE #( zeile = cs_goitem-zeile set_rescd = abap_true ) TO gt_set_rescd.
*      if line exists, it'll still have it as X set, so no modification required.
        ENDIF.
**** End of Ins R7-ORTHO/PAKBARI/CR943443/EC1K923495
      ELSEIF line_exists( gt_set_rescd[ zeile = cs_goitem-zeile set_rescd = abap_true ] ). "++R7-ORTHO/PAKBARI/CR943443/EC1K923495

        CLEAR: cs_goitem-grund." cs_goitem-grtxt.  "++R7-ORTHO/PAKBARI/CR943443/EC1K923495
      ENDIF.
    ELSEIF line_exists( gt_set_rescd[ zeile = cs_goitem-zeile set_rescd = abap_true ] ). "++R7-ORTHO/PAKBARI/CR943443/EC1K923495

      CLEAR: cs_goitem-grund." cs_goitem-grtxt.  "++R7-ORTHO/PAKBARI/CR943443/EC1K923495
    ENDIF.    "++R7-ORTHO/PAKBARI/CR943443/EC1K923495
  ENDIF.

*  ENDIF.
ENDMETHOD.                    "IF_EX_MB_MIGO_BADI~LINE_MODIFY


  METHOD if_ex_mb_migo_badi~maa_line_id_adjust.        "Begin of 1987428
  ENDMETHOD.                                             "End of 1987428


METHOD if_ex_mb_migo_badi~mode_set.



ENDMETHOD.                    "IF_EX_MB_MIGO_BADI~MODE_SET


METHOD if_ex_mb_migo_badi~pai_detail.

ENDMETHOD.                    "IF_EX_MB_MIGO_BADI~PAI_DETAIL


METHOD IF_EX_MB_MIGO_BADI~PAI_HEADER .
ENDMETHOD.                    "IF_EX_MB_MIGO_BADI~PAI_HEADER


METHOD if_ex_mb_migo_badi~pbo_detail.



ENDMETHOD.                    "IF_EX_MB_MIGO_BADI~PBO_DETAIL


METHOD IF_EX_MB_MIGO_BADI~PBO_HEADER .
ENDMETHOD.                    "IF_EX_MB_MIGO_BADI~PBO_HEADER


METHOD if_ex_mb_migo_badi~post_document.



ENDMETHOD.                    "IF_EX_MB_MIGO_BADI~POST_DOCUMENT


method IF_EX_MB_MIGO_BADI~PROPOSE_SERIALNUMBERS.
endmethod.


METHOD if_ex_mb_migo_badi~publish_material_item.
ENDMETHOD.


METHOD if_ex_mb_migo_badi~reset.


ENDMETHOD.                    "IF_EX_MB_MIGO_BADI~RESET


METHOD IF_EX_MB_MIGO_BADI~STATUS_AND_HEADER .
ENDMETHOD.                    "IF_EX_MB_MIGO_BADI~STATUS_AND_HEADER
ENDCLASS.
