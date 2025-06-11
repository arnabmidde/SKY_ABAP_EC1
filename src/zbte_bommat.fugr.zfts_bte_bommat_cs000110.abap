FUNCTION zfts_bte_bommat_cs000110.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IDOC_CONTROL) LIKE  EDIDC STRUCTURE  EDIDC
*"  TABLES
*"      IDOC_DATA STRUCTURE  EDIDD
*"  EXCEPTIONS
*"      ERROR
*"      IDOC_ERROR
*"----------------------------------------------------------------------

* DO NOT CHANGE EXISTING SEGMENTS IN IDOC_DATA. IT MAY EFFECT THE
* WHOLE BILL OF MATERIAL YOU ARE CURRENTLY DISTRIBUTING.
* DO ONLY ADD OR INSERT YOUR OWN CUSTOMER-SEGMENTS.
*&------------------------------------------------------------------------------&*
*& Confidential Property of Stryker                                             &*
*& All Rights Reserved                                                          &*
*&******************************************************************************&*
*& Function Module : ZFTS_BTE_BOMMAT_CS000110                                   &*
*& Company         : Stryker Project Accelerate                                 &*
*& Author          : Bharathi Purum                                             &*
*& Date            : 20/08/2024                                                 &*
*& Title           : BOMs from SAP to WebOps                                    &*
*& FD #            : FTS.INT.224-01                                             &*
*&******************************************************************************&*
*& DESCRIPTION  : Need the ability to create SAP BOMs for prebuilt   EC2K905575 &*
*&                implant, instrumentation and combo kits and send   EC2K905577 &*
*&                (via interface) BOMs to WebOps.                               &*
*&******************************************************************************&*
*local workarea declaration
  DATA : lwa_elmastm  TYPE e1mastm,
         lwa_zelmastm TYPE ze1mastm,
         lwa_edidd    TYPE edidd,
         lv_tabix     TYPE syst_tabix,
         lt_model     TYPE STANDARD TABLE OF bdi_model INITIAL SIZE 0. " Sbrahma
*Constants declaration
  CONSTANTS : lc_e1mastm       TYPE edilsegtyp VALUE 'E1MASTM',
              lc_ze1mastm      TYPE edilsegtyp VALUE 'ZE1MASTM',
              lc_mestyp_bommat TYPE rvari_vnam VALUE 'ZMSG_TYPE'.
*Get data from table TVARVC based on name 'ZMSG_TYPE'
  zcl_otc_tvarv_utility=>z_get_tvarvc( EXPORTING i_variable_name = lc_mestyp_bommat
                                       IMPORTING e_tvarvc        = DATA(lwa_mestyp_bommat) ).
*Check message type 'BOMMAT' satisfied then the below logic will be executed.
  CHECK idoc_control-mestyp EQ lwa_mestyp_bommat-low.
*Read the Material number from the IDOC segment 'E1MASTM'
  READ TABLE idoc_data INTO DATA(lwa_idoc_e1mastm) WITH KEY segnam = lc_e1mastm.
  CHECK sy-subrc IS INITIAL.
*Pass segment data 'E1MASTM' to variable
  lwa_elmastm = lwa_idoc_e1mastm-sdata.
*Check Material number is exist or not
  CHECK  lwa_elmastm-matnr IS NOT INITIAL.
*Get the Product hierarchy from MARA based on Material Number
  SELECT SINGLE prdha
    FROM mara
    INTO @DATA(lv_prdha)
    WHERE matnr EQ @lwa_elmastm-matnr.
  IF sy-subrc IS NOT INITIAL.
    CLEAR : lv_prdha.
  ENDIF.
*Get the Product Scheduling profile and plant from MARC based on Material Number and Plant
  SELECT SINGLE werks,
    sfcpf
    FROM marc
    INTO @DATA(lv_sfcpf)
    WHERE matnr EQ @lwa_elmastm-matnr
    AND   werks EQ @lwa_elmastm-werks.
  IF sy-subrc IS NOT INITIAL.
    CLEAR : lv_sfcpf.
  ENDIF.
  CALL FUNCTION 'ALE_MODEL_INFO_GET'
    EXPORTING
      message_type           = idoc_control-mestyp
*     RECEIVING_SYSTEM       = ' '
*     SENDING_SYSTEM         = ' '
      validdate              = sy-datum
    TABLES
      model_data             = lt_model
    EXCEPTIONS
      no_model_info_found    = 1
      own_system_not_defined = 2
      OTHERS                 = 3.
  IF sy-subrc = 0.
    READ TABLE lt_model ASSIGNING FIELD-SYMBOL(<lfs_model>) WITH KEY
                        objtype = 'WERK'
                        objvalue = lv_sfcpf-werks.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ENDIF.
*Populating the Product hierarchy and Product Scheduling profile data to custom segment with index
  IF lv_prdha IS NOT INITIAL AND lv_sfcpf IS NOT INITIAL.
*LOOP AT idoc_data INTO DATA(lwa_idoc_data) WHERE segnam EQ lc_e1mastm. ##NEEDED
    READ TABLE idoc_data TRANSPORTING NO FIELDS WITH KEY segnam = lc_e1mastm.
    IF sy-subrc IS INITIAL.
      lv_tabix = sy-tabix.
      lwa_zelmastm-werks = lv_sfcpf-werks. " Sbrahma
      lwa_zelmastm-prdha = lv_prdha+0(6).
      lwa_zelmastm-sfcpf = lv_sfcpf-sfcpf.
      lwa_edidd-segnam = lc_ze1mastm.
      lwa_edidd-sdata = lwa_zelmastm.
      lv_tabix = lv_tabix + 1.
      INSERT lwa_edidd INTO idoc_data INDEX lv_tabix.
      CLEAR : lwa_edidd, lwa_zelmastm.
    ENDIF.
  ENDIF.

  DATA(lo_bom) = NEW zcl_mm_idoc_bommat( ).
  CALL METHOD lo_bom->adjust_segments
    EXPORTING
      is_idoc_control = idoc_control
    CHANGING
      ct_idoc_data    = idoc_data[].

ENDFUNCTION.
