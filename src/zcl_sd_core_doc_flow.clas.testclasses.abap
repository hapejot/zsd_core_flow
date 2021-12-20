class ltcl_main definition final for testing
  duration short
  risk level harmless.

  private section.
    methods:
      order_item_rejection_reasons for testing raising cx_static_check.
endclass.


class ltcl_main implementation.

  method order_item_rejection_reasons.
    DATA: lt_exp_flow TYPE zcl_sd_core_doc_flow=>mtt_flow.
    data(lo_cat) = new zcl_sd_core_doc_flow( ).

    lo_cat->prepare_flow( ).
    lt_exp_flow = value #( ( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'flow' exp = lt_exp_flow act = lo_cat->mt_flow ).

  endmethod.

endclass.
