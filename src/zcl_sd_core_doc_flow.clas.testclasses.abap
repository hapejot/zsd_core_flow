CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      order_item_rejection_reasons FOR TESTING RAISING cx_static_check,
      inline_group FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_main IMPLEMENTATION.

  METHOD order_item_rejection_reasons.
    DATA: lt_exp_flow TYPE zcl_sd_core_doc_flow=>mtt_flow.
    DATA(lo_cat) = NEW zcl_sd_core_doc_flow( ).

    lo_cat->prepare_flow( ).
    lt_exp_flow = VALUE #( ( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'flow' exp = lt_exp_flow act = lo_cat->mt_flow ).

  ENDMETHOD.

  METHOD inline_group.

    TYPES: BEGIN OF row,
             vbeln   TYPE vbeln,
             vbtyp_n TYPE vbtyp,
           END OF row.
    DATA mt_flow TYPE STANDARD TABLE OF row WITH DEFAULT KEY.
    DATA lt_list TYPE STANDARD TABLE OF vbeln WITH DEFAULT KEY.

    mt_flow = VALUE #(
    ( vbeln = '200000' vbtyp_n = 'X' )
    ( vbeln = '100000' vbtyp_n = 'X' )
    ( vbeln = '200000' vbtyp_n = 'X' )
    ( vbeln = '100000' vbtyp_n = 'X' )
    ( vbeln = '300000' vbtyp_n = 'X' )
    ( vbeln = '100000' vbtyp_n = 'X' )
    ( vbeln = '300000' vbtyp_n = 'X' )
    ).


    lt_list = VALUE #( FOR GROUPS value OF <line> IN mt_flow WHERE ( vbtyp_n = 'X' )
                         GROUP BY <line>-vbeln WITHOUT MEMBERS ( value ) ).


    DATA lt_exp TYPE STANDARD TABLE OF vbeln WITH DEFAULT KEY.
    lt_exp = VALUE #(  ( '200000' ) ( '100000' ) ( '300000' ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'msg' exp = lt_exp act = lt_list ).

  ENDMETHOD.

ENDCLASS.
