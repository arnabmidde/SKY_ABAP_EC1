class ZCL_IM_FTS_EI_REASON_CODE definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_LE_SHP_GOODSMOVEMENT .

  class-data GR_BWART type FIP_T_BWART_RANGE .
  class-data GT_TVARVC type TVARVC_T .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_FTS_EI_REASON_CODE IMPLEMENTATION.


  METHOD constructor.
*& Confidential Property of Stryker
*& All Rights Reserved
*&************************************************************************&*
*& Program      : ZFTS_EI_ORDER_REASON_CODE                               &*
*& Company      : Stryker Project Accelerate                              &*
*& Author       : Priyanshi Akbari                                        &*
*& Date         : 06 Jan 2025                                             &*
*& Title        : Enhancement Implementation for Order Reason Code        &*
*& FD #         : FTS.EXT.361                                             &*
*&************************************************************************&*
*& DESCRIPTION  : Enhancement Implementation created for                  &*
*&                managing order reason code                              &*
*&************************************************************************&*
*& H I S T O R Y       O F       R E V I S I O N S &                      &*
*&************************************************************************&*
*& Date        Author           Description              Transport Number &*
*& 01/06/2024  PAKBARI     R7-Initial Build CR943443        EC1K923495    &*
*&************************************************************************&*

    CONSTANTS: lc_name    TYPE rvari_vnam VALUE 'Z_FTS.EXT.361%',
               lc_mvntype TYPE rvari_vnam VALUE 'Z_FTS.EXT.361_MVNTYPE'.

    CALL METHOD zcl_otc_tvarv_utility=>z_get_tvarvc_tab_like
      EXPORTING
        i_variable_name = lc_name
      RECEIVING
        r_tvarvc_tab    = gt_tvarvc.

    IF NOT gt_tvarvc IS INITIAL.
      gr_bwart = VALUE fip_t_bwart_range( FOR <lfs_tvarvc> IN gt_tvarvc WHERE ( name   = lc_mvntype )
                                                                              ( sign   = <lfs_tvarvc>-sign
                                                                                option = <lfs_tvarvc>-opti
                                                                                low    = <lfs_tvarvc>-low
                                                                                high   = <lfs_tvarvc>-high ) ).
    ENDIF.

    ENDMETHOD.


  METHOD if_ex_le_shp_goodsmovement~change_input_header_and_items.
*& Confidential Property of Stryker
*& All Rights Reserved
*&************************************************************************&*
*& Program      : ZFTS_EI_ORDER_REASON_CODE                               &*
*& Company      : Stryker Project Accelerate                              &*
*& Author       : MOHAMED ASHIQ                                           &*
*& Date         : July 07 2024                                            &*
*& Title        : Enhancement Implementation for Order Reason Code        &*
*& FD #         : FTS.EXT.361                                             &*
*&************************************************************************&*
*& DESCRIPTION  : Enhancement Implementation created for                  &*
*&                managing order reason code                              &*
*&************************************************************************&*
*& H I S T O R Y       O F       R E V I S I O N S &                      &*
*&************************************************************************&*
*& Date        Author           Description              Transport Number &*
*& 08/19/2024  MLANGALI     R7-Ortho_FTS_EXT_361_           EC2K905264    &*
*&                          Transfer_Reason SO                            &*
*&                          - Initial Version                             &*
*& 12/20/2024  PAKBARI     R7-Ortho_FTS_EXT_361 - CR943443  EC1K923495    &*
*&      Update Reason code only for capitalized batch-managed materils    &*
*&************************************************************************&*

    CONSTANTS: lc_ordrea   TYPE rvari_vnam VALUE 'Z_FTS.EXT.361_ORDREA',
**---->Begin of change R7-ORTHO/PAKBARI/CR943443/EC1K923495
               lc_r7deactv TYPE rvari_vnam VALUE 'Z_FTS.EXT.361_R7DEACTV',
               lc_atnam    TYPE atnam      VALUE 'ZCAP_CATEGORIES',
               lc_atwrt    TYPE atwrt      VALUE 'Y_BATCH'.

    DATA: lv_atinn  TYPE atinn.

    IF ct_ximseg IS NOT INITIAL AND VALUE #( gt_tvarvc[ name = lc_r7deactv ]-low OPTIONAL ) IS INITIAL AND gr_bwart IS NOT INITIAL.

      DATA(lt_ximseg) = ct_ximseg.
      DELETE lt_ximseg WHERE bwart NOT IN gr_bwart.

      IF lt_ximseg IS NOT INITIAL.
        SORT lt_ximseg BY kdauf_sd kdpos_sd.
        DELETE ADJACENT DUPLICATES FROM lt_ximseg COMPARING kdauf_sd kdpos_sd.
        "Get Internal charecteristic value
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = lc_atnam
          IMPORTING
            output = lv_atinn.

        SELECT a~vbeln, a~augru, b~posnr, b~matnr
            FROM vbak AS a
            INNER JOIN vbap AS b
            ON a~vbeln = b~vbeln
            INNER JOIN ausp AS c
            ON b~matnr = c~objek
            INTO TABLE @DATA(lt_vbak)
            FOR ALL ENTRIES IN @lt_ximseg
          WHERE a~vbeln = @lt_ximseg-kdauf_sd
          AND   b~posnr = @lt_ximseg-kdpos_sd
          AND   c~atinn = @lv_atinn
          AND   c~atwrt = @lc_atwrt.
        IF sy-subrc = 0.
          SORT lt_vbak BY vbeln posnr.
          DELETE lt_vbak WHERE augru EQ ' '.

          LOOP AT ct_ximseg ASSIGNING FIELD-SYMBOL(<lfs_ximseg>)
                                   WHERE bwart IN gr_bwart.

            READ TABLE lt_vbak
              ASSIGNING FIELD-SYMBOL(<lfs_vbak>)
              WITH KEY vbeln = <lfs_ximseg>-kdauf_sd
                       posnr = <lfs_ximseg>-kdpos_sd
                       BINARY SEARCH.
            IF sy-subrc = 0 AND <lfs_vbak> IS ASSIGNED AND <lfs_vbak>-augru IS NOT INITIAL.
              <lfs_ximseg>-grund = VALUE #( gt_tvarvc[ name = lc_ordrea
                                                       low = <lfs_vbak>-augru ]-high OPTIONAL ).
            ENDIF.
          ENDLOOP.

        ENDIF.
      ENDIF.
**---->End of change R7-ORTHO/PAKBARI/CR943443/EC1K923495
    ENDIF.
  ENDMETHOD.
ENDCLASS.
