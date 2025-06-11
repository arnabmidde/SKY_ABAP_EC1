class ZCL_IM_MM_IDOC_CRE_CHECK definition
  public
  final
  create public .

public section.

  interfaces IF_EX_IDOC_CREATION_CHECK .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_MM_IDOC_CRE_CHECK IMPLEMENTATION.


METHOD if_ex_idoc_creation_check~idoc_data_check.
*&------------------------------------------------------------------------------&*
*& Confidential Property of Stryker                                             &*
*& All Rights Reserved                                                          &*
*&******************************************************************************&*
*& Method  name : IDOC_DATA_CHECK                                               &*
*& Company      : Stryker Project Accelerate                                    &*
*& Author       : Bharathi Purum                                                &*
*& Date         : 16/08/2024                                                    &*
*& Title        : BOMs from SAP to WebOps                                       &*
*& FD #         : FTS.INT.224-01                                                &*
*&******************************************************************************&*
*& DESCRIPTION  : Need the ability to create SAP BOMs for prebuilt   EC2K905575 &*
*&                implant, instrumentation and combo kits and send   EC2K905577 &*
*&                (via interface) BOMs to WebOps.                               &*
*&******************************************************************************&*
*Internal table and workarea declarations
  DATA: lt_mat_types TYPE fagl_mm_t_range_mtart,
        wa_mat_types TYPE fagl_mm_range_mtart.
*local workarea declaration
  DATA : wa_elmastm TYPE e1mastm.
*Constants declaration
  CONSTANTS : lc_e1mastm       TYPE edilsegtyp VALUE 'E1MASTM',
              lc_mestyp_bommat TYPE rvari_vnam VALUE 'ZMSG_TYPE',
              lc_material_type TYPE rvari_vnam VALUE 'ZMATERIAL_TYPE'.
*Get data from table TVARVC based on name 'ZMSG_TYPE'
  zcl_otc_tvarv_utility=>z_get_tvarvc( EXPORTING i_variable_name = lc_mestyp_bommat
                                       IMPORTING e_tvarvc        = DATA(wa_mestyp_bommat) ).
*Read the data from table TVARVC based on name 'ZMATERIAL_TYPE'
  DATA(lt_material_type) = zcl_otc_tvarv_utility=>z_get_tvarvc_tab( EXPORTING i_variable_name = lc_material_type ).
*Fill Material types range
  DATA(lt_mat_types_rng) = VALUE fagl_mm_t_range_mtart( FOR ls_material_type IN lt_material_type
                                                              ( sign    = ls_material_type-sign
                                                                option  = ls_material_type-opti
                                                                low     = ls_material_type-low ) ).
*Check message type 'BOMMAT' satisfied then the below logic will be executed.
  CHECK idoc_control-mestyp EQ wa_mestyp_bommat-low.
*Read the Material number from the IDOC segment 'E1MASTM'
  READ TABLE idoc_data INTO DATA(wa_idoc_e1mastm) WITH KEY segnam = lc_e1mastm.
  CHECK sy-subrc IS INITIAL.
*Pass segment data 'E1MASTM' to variable
  wa_elmastm = wa_idoc_e1mastm-sdata.
*Check Material number is exist or not
  CHECK  wa_elmastm-matnr IS NOT INITIAL.
*Get the Material type from MARA based on Material Number
  SELECT SINGLE mtart
    FROM mara
    INTO @DATA(lv_mtart)
    WHERE matnr EQ @wa_elmastm-matnr.
  IF sy-subrc IS INITIAL.
*If the material type is not belongs to 'ZFRT' or 'ZMAT', then should not create the IDOC
    IF lv_mtart NOT IN lt_mat_types_rng AND lt_mat_types_rng IS NOT INITIAL.
      CLEAR create_idoc.
    ENDIF.
  ENDIF.

ENDMETHOD.
ENDCLASS.
