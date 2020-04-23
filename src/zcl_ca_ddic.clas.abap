"! <p class="shorttext synchronized" lang="en">Common object: Check of / informations to data objects</p>
CLASS zcl_ca_ddic DEFINITION PUBLIC
                             FINAL
                             CREATE PUBLIC.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      zif_ca_c_bool,
      if_fsbp_const_range,
      if_xo_const_message.

*   a l i a s e s
    ALIASES:
*     Boolean flags
      c_false             FOR  zif_ca_c_bool~c_false,
      c_true              FOR  zif_ca_c_bool~c_true,
*     Signs and options for RANGES/SELECT-OPTIONS
      c_sign_i            FOR  if_fsbp_const_range~sign_include,
      c_sign_e            FOR  if_fsbp_const_range~sign_exclude,
      c_opt_eq            FOR  if_fsbp_const_range~option_equal,
      c_opt_ne            FOR  if_fsbp_const_range~option_not_equal,
      c_opt_cp            FOR  if_fsbp_const_range~option_contains_pattern,
      c_opt_bt            FOR  if_fsbp_const_range~option_between.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Validate value against fixed values of a domain</p>
      "!
      "! @parameter iv_data        | <p class="shorttext synchronized" lang="en">Value under test (best to use when typed with corr. DE)</p>
      "! @parameter iv_data_elem   | <p class="shorttext synchronized" lang="en">Name of DDIC table field or data element</p>
      "! @parameter iv_raise_excep | <p class="shorttext synchronized" lang="en">1 = Raise exception when invalid</p>
      "! @parameter iv_param_name  | <p class="shorttext synchronized" lang="en">Name of field/parameter for output in error message</p>
      "! @parameter rv_is_valid    | <p class="shorttext synchronized" lang="en">1 = Value is valid</p>
      "! @raising   zcx_ca_param   | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      check_fixed_values
        IMPORTING
          iv_data            TYPE simple
          iv_data_elem       TYPE rollname     OPTIONAL
          iv_raise_excep     TYPE dml_boolean  DEFAULT c_false
          iv_param_name      TYPE csequence    DEFAULT 'IV_DATA' ##no_text
        RETURNING
          VALUE(rv_is_valid) TYPE dml_boolean
        RAISING
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Return a list with type descriptions for a structure/table</p>
      "!
      "! @parameter iv_name       | <p class="shorttext synchronized" lang="en">DDIC object name</p>
      "! @parameter iv_data       | <p class="shorttext synchronized" lang="en">Structure or internal table</p>
      "! @parameter ir_data       | <p class="shorttext synchronized" lang="en">Reference of a structure or internal table</p>
      "! @parameter iv_level      | <p class="shorttext synchronized" lang="en">Level of resolution of the struc. (9 = deepest resolution)</p>
      "! @parameter iv_langu      | <p class="shorttext synchronized" lang="en">Language key for DDIC text, e.g. for field labels</p>
      "! @parameter iv_param_name | <p class="shorttext synchronized" lang="en">Name of field/parameter for output in error message</p>
      "! @parameter rt_components | <p class="shorttext synchronized" lang="en">Component list to requested structure or table</p>
      "! @raising   zcx_ca_param  | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      get_component_list
        IMPORTING
          iv_name              TYPE csequence   OPTIONAL
          iv_data              TYPE data        OPTIONAL
          ir_data              TYPE REF TO data OPTIONAL
          iv_level             TYPE i           DEFAULT 9
          iv_langu             TYPE sylangu     DEFAULT sy-langu
          iv_param_name        TYPE csequence   OPTIONAL
        RETURNING
          VALUE(rt_components) TYPE abap_component_view_tab
        RAISING
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Return field list for a Structure/(internal) table (DFIES)</p>
      "!
      "! @parameter iv_name       | <p class="shorttext synchronized" lang="en">DDIC object name</p>
      "! @parameter iv_data       | <p class="shorttext synchronized" lang="en">Structure or internal table</p>
      "! @parameter ir_data       | <p class="shorttext synchronized" lang="en">Reference of a structure or internal table</p>
      "! @parameter iv_langu      | <p class="shorttext synchronized" lang="en">Language key for DDIC text, e.g. for field labels</p>
      "! @parameter iv_param_name | <p class="shorttext synchronized" lang="en">Name of field/parameter for output in error message</p>
      "! @parameter rt_fields     | <p class="shorttext synchronized" lang="en">Field list (DFIES)</p>
      "! @raising   zcx_ca_param  | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      get_field_list
        IMPORTING
          iv_name          TYPE csequence   OPTIONAL
          iv_data          TYPE data        OPTIONAL
          ir_data          TYPE REF TO data OPTIONAL
          iv_langu         TYPE sylangu     DEFAULT sy-langu
          iv_param_name    TYPE csequence   OPTIONAL
        RETURNING
          VALUE(rt_fields) TYPE ddfields
        RAISING
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Determination of fixed values to a table field/data element</p>
      "!
      "! @parameter iv_data       | <p class="shorttext synchronized" lang="en">Value under test (best to use when typed with corr. DE)</p>
      "! @parameter iv_data_elem  | <p class="shorttext synchronized" lang="en">Name of DDIC table field or data element</p>
      "! @parameter iv_param_name | <p class="shorttext synchronized" lang="en">Name of field/parameter for output in error message</p>
      "! @parameter rt_fixed_vals | <p class="shorttext synchronized" lang="en">Fixed values</p>
      "! @raising   zcx_ca_param  | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      get_fixed_values
        IMPORTING
          iv_data              TYPE simple    OPTIONAL
          iv_data_elem         TYPE rollname  OPTIONAL
          iv_param_name        TYPE csequence DEFAULT 'IV_DATA' ##no_text
        RETURNING
          VALUE(rt_fixed_vals) TYPE ddfixvalues
        RAISING
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Create RTTI type description object</p>
      "!
      "! @parameter iv_name      | <p class="shorttext synchronized" lang="en">Object name (DDIC or class / interface)</p>
      "! @parameter iv_data      | <p class="shorttext synchronized" lang="en">Data field / value</p>
      "! @parameter ir_data      | <p class="shorttext synchronized" lang="en">Reference of a data object / value</p>
      "! @parameter io_object    | <p class="shorttext synchronized" lang="en">Reference of an object (class / interface)</p>
      "! @parameter ro_type_desc | <p class="shorttext synchronized" lang="en">Type description object</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      get_type_desc
        IMPORTING
          iv_name             TYPE csequence     OPTIONAL
          iv_data             TYPE data          OPTIONAL
          ir_data             TYPE REF TO data   OPTIONAL
          io_object           TYPE REF TO object OPTIONAL
        RETURNING
          VALUE(ro_type_desc) TYPE REF TO cl_abap_typedescr
        RAISING
          zcx_ca_param.

* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   a l i a s e s
    ALIASES:
*     Message types
      c_msgty_e            FOR  if_xo_const_message~error,
      c_msgty_i            FOR  if_xo_const_message~info,
      c_msgty_s            FOR  if_xo_const_message~success,
      c_msgty_w            FOR  if_xo_const_message~warning.

*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
      "! <p class="shorttext synchronized" lang="en">Type description object</p>
      mo_type_desc TYPE REF TO cl_abap_typedescr.
ENDCLASS.



CLASS zcl_ca_ddic IMPLEMENTATION.

  METHOD check_fixed_values.
    "-----------------------------------------------------------------*
    "   Check a value against defined fixed values of a domain
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_param_name TYPE symsgv.

    "Getting fixed values
    DATA(lt_fixvals) = get_fixed_values( iv_data       = iv_data
                                         iv_data_elem  = iv_data_elem
                                         iv_param_name = iv_param_name ).

    "Search for value
    rv_is_valid = c_false.
    LOOP AT lt_fixvals INTO DATA(ls_fixval).
      CASE ls_fixval-option.
        WHEN c_opt_eq.
          IF iv_data EQ ls_fixval-low.
            rv_is_valid = c_true.
          ENDIF.

        WHEN c_opt_bt.
          IF iv_data BETWEEN ls_fixval-low AND ls_fixval-high.
            rv_is_valid = c_true.
          ENDIF.
      ENDCASE.

      IF rv_is_valid EQ c_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    "Raise exception if is requested
    IF iv_raise_excep EQ c_true   AND
       rv_is_valid    EQ c_false.
      "Determine parameter name for error message
      IF iv_data_elem IS     SUPPLIED AND
         iv_data_elem IS NOT INITIAL.
        lv_param_name = 'IV_DATA_ELEM' ##no_text.

      ELSE.
        lv_param_name = 'IV_DATA' ##no_text.
      ENDIF.

      IF iv_param_name IS     SUPPLIED AND
         iv_param_name IS NOT INITIAL.
        lv_param_name = iv_param_name.
      ENDIF.

      WRITE iv_data TO sy-msgv2.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_param
        EXPORTING
          textid   = zcx_ca_param=>param_invalid
          mv_msgty = c_msgty_e
          mv_msgv1 = lv_param_name
          mv_msgv2 = sy-msgv2.
    ENDIF.
  ENDMETHOD.                    "check_fixed_values


  METHOD get_component_list.
    "-----------------------------------------------------------------*
    "   Get components of a structured object, also tables
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lo_stru_desc  TYPE REF TO cl_abap_structdescr,
      lv_param_name TYPE symsgv.

    "Set parameter name if it is not set by caller
    IF iv_param_name IS SUPPLIED.
      lv_param_name = iv_param_name.
    ELSEIF iv_name IS SUPPLIED.
      lv_param_name = 'IV_NAME' ##no_text.
    ELSEIF iv_data IS SUPPLIED.
      lv_param_name = 'IV_DATA' ##no_text.
    ELSEIF ir_data IS SUPPLIED.
      lv_param_name = 'IR_DATA' ##no_text.
    ENDIF.

    "Get object description
    IF iv_name IS SUPPLIED.
      get_type_desc( iv_name = iv_name ).
    ELSEIF iv_data IS SUPPLIED.
      get_type_desc( iv_data = iv_data ).
    ELSEIF ir_data IS SUPPLIED.
      get_type_desc( ir_data = ir_data ).
    ENDIF.

    "If object is a table determine structure definition
    CASE mo_type_desc->type_kind.
      WHEN mo_type_desc->typekind_table.
        "It is a table
        DATA(lo_tabl_desc) = CAST cl_abap_tabledescr( mo_type_desc ).
        lo_stru_desc ?= lo_tabl_desc->get_table_line_type( ).

      WHEN mo_type_desc->typekind_struct1 OR
           mo_type_desc->typekind_struct2.
        "It is a flat or complex structure
        lo_stru_desc ?= mo_type_desc.

      WHEN OTHERS.
        "Object (in parameter) &1 is no structure and no table
        RAISE EXCEPTION TYPE zcx_ca_intern
          EXPORTING
            textid   = zcx_ca_intern=>no_struct_no_table
            mv_msgty = c_msgty_e
            mv_msgv1 = lv_param_name.
    ENDCASE.

    "Get components of structure
    rt_components = lo_stru_desc->get_included_view( iv_level ).
  ENDMETHOD.                    "get_component_list


  METHOD get_field_list.
    "-----------------------------------------------------------------*
    "   Get field list of a structured object, also tables
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lo_stru_desc  TYPE REF TO cl_abap_structdescr,
      ls_dfies      TYPE dfies,
      lv_field_pos  TYPE tabfdpos,
      lv_param_name TYPE symsgv.

    "Set parameter name if it is not set by caller
    IF iv_param_name IS SUPPLIED.
      lv_param_name = iv_param_name.
    ELSEIF iv_name IS SUPPLIED.
      lv_param_name = 'IV_NAME' ##no_text.
    ELSEIF iv_data IS SUPPLIED.
      lv_param_name = 'IV_DATA' ##no_text.
    ELSEIF ir_data IS SUPPLIED.
      lv_param_name = 'IR_DATA' ##no_text.
    ENDIF.

    "Get object description
    IF iv_name IS SUPPLIED.
      get_type_desc( iv_name = iv_name ).
    ELSEIF iv_data IS SUPPLIED.
      get_type_desc( iv_data = iv_data ).
    ELSEIF ir_data IS SUPPLIED.
      get_type_desc( ir_data = ir_data ).
    ENDIF.

    "If object is a table determine structure definition
    CASE mo_type_desc->type_kind.
      WHEN mo_type_desc->typekind_table.
        "It is a table
        DATA(lo_tabl_desc) = CAST cl_abap_tabledescr( mo_type_desc ).
        lo_stru_desc ?= lo_tabl_desc->get_table_line_type( ).

      WHEN mo_type_desc->typekind_struct1 OR
           mo_type_desc->typekind_struct2.
        "It is a flat or complex structure
        lo_stru_desc ?= mo_type_desc.

      WHEN OTHERS.
        "Object (in parameter) &1 is no structure and no table
        RAISE EXCEPTION TYPE zcx_ca_intern
          EXPORTING
            textid   = zcx_ca_intern=>no_struct_no_table
            mv_msgty = c_msgty_e
            mv_msgv1 = lv_param_name.
    ENDCASE.

    "Get field list of structure
    CASE lo_stru_desc->is_ddic_type( ).
      WHEN abap_true.
        "It is a DDIC type
        lo_stru_desc->get_ddic_field_list(
                                      EXPORTING
                                        p_langu      = iv_langu
                                      RECEIVING
                                        p_field_list = rt_fields
                                      EXCEPTIONS
                                        not_found    = 1
                                        no_ddic_type = 2
                                        OTHERS       = 3 ).
        IF sy-subrc NE 0.
          DATA(lx_error) = zcx_ca_intern=>create_exception(
                                              iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                              iv_class    = 'CL_ABAP_STRUCTDESCR'
                                              iv_method   = 'GET_DDIC_FIELD_LIST'
                                              iv_subrc    = sy-subrc ) ##no_text.
          IF lx_error IS BOUND.
            RAISE EXCEPTION lx_error.
          ENDIF.
        ENDIF.

      WHEN abap_false.
        "It is NOT a DDIC type - prepare DFIES list from single components
        "Resolve structure with this method to get also components of includes
        DATA(lt_comps) = lo_stru_desc->get_included_view( 9 ).
        LOOP AT lt_comps INTO DATA(ls_comp).
          CLEAR ls_dfies.
          "Set position
          ADD 1 TO lv_field_pos.
          ls_dfies-position = lv_field_pos.

          "Elementary types
          IF ls_comp-type->kind EQ ls_comp-type->kind_elem.
            "Cast data description into elementary description
            DATA(lo_elem_desc) = CAST cl_abap_elemdescr( ls_comp-type ).
            CASE ls_comp-type->is_ddic_type( ).
              WHEN abap_true.
                "D D I C   t y p e
                "This method return the data element name in TABNAME ...
                ls_dfies = lo_elem_desc->get_ddic_field( ).
                "... so it will be corrected here
                ls_dfies-tabname   = lo_stru_desc->get_relative_name( ).
                ls_dfies-fieldname = ls_comp-name.
                ls_dfies-comptype  = 'E' ##no_text.     " overwrite value of method

              WHEN abap_false.
                "L o c a l   t y p e  - set only available values
                ls_dfies-tabname   = lo_stru_desc->get_relative_name( ).
                ls_dfies-fieldname = ls_comp-name.
                ls_dfies-langu     = iv_langu.
                ls_dfies-leng      = lo_elem_desc->length /
                                                  cl_abap_char_utilities=>charsize.
                ls_dfies-intlen    = lo_elem_desc->length.
                ls_dfies-outputlen = lo_elem_desc->output_length.
                ls_dfies-decimals  = lo_elem_desc->decimals.
*              ls_dfies-datatype  = lo_elem_desc->?
                ls_dfies-inttype   = lo_elem_desc->type_kind.
                ls_dfies-mask      = lo_elem_desc->edit_mask.
                ls_dfies-masklen   = strlen( lo_elem_desc->edit_mask ).
                ls_dfies-dynpfld   = abap_true.

                "The first three types are new and not available in target systems
                IF "lo_elem_desc->type_kind EQ lo_elem_desc->typekind_decfloat   OR
                   "lo_elem_desc->type_kind EQ lo_elem_desc->typekind_decfloat16 OR
                   "lo_elem_desc->type_kind EQ lo_elem_desc->typekind_decfloat34 OR
                   lo_elem_desc->type_kind EQ lo_elem_desc->typekind_float      OR
                   lo_elem_desc->type_kind EQ lo_elem_desc->typekind_packed     OR
                   lo_elem_desc->type_kind EQ lo_elem_desc->typekind_int        OR " INT4.
                   lo_elem_desc->type_kind EQ lo_elem_desc->typekind_int2       OR
                   lo_elem_desc->type_kind EQ lo_elem_desc->typekind_int1.
                  ls_dfies-sign = abap_true.
                ENDIF.
            ENDCASE.

          ELSE.
            "No elementary data object
            "Others are complex
            ls_dfies-tabname   = lo_stru_desc->get_relative_name( ).
            ls_dfies-fieldname = ls_comp-name.
            ls_dfies-langu     = iv_langu.
            ls_dfies-inttype   = ls_comp-type->type_kind.
            ls_dfies-intlen    = ls_comp-type->length.

            "Set component type and set ROLLNAME
            CASE ls_comp-type->kind.
              WHEN ls_comp-type->kind_ref.
                ls_dfies-comptype = 'R' ##no_text.
                ls_dfies-genkey   = abap_true.

              WHEN ls_comp-type->kind_table.
                ls_dfies-comptype = 'L' ##no_text.
                DATA(lo_tabl_desc_det) = CAST cl_abap_tabledescr( ls_comp-type ).
                ls_dfies-rollname = lo_tabl_desc_det->get_relative_name( ).

              WHEN ls_comp-type->kind_struct.
                ls_dfies-comptype      = 'S' ##no_text.
                DATA(lo_stru_desc_det) = CAST cl_abap_tabledescr( ls_comp-type ).
                ls_dfies-rollname      = lo_stru_desc_det->get_relative_name( ).
                DATA(lt_dfies_det) =
                          get_field_list(
                                 iv_name       = lo_stru_desc_det->get_relative_name( )
                                 iv_param_name = iv_param_name ).
                lv_field_pos = lv_field_pos + lines( lt_dfies_det ).
            ENDCASE.
          ENDIF.

          "Attach component to fields list
          APPEND ls_dfies TO rt_fields.
        ENDLOOP.
    ENDCASE.
  ENDMETHOD.                    "get_field_list


  METHOD get_fixed_values.
    "-----------------------------------------------------------------*
    "   Get fixed values including descriptions of a domain
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lo_elem_desc  TYPE REF TO cl_abap_elemdescr,
      lv_param_name TYPE symsgv.

    IF iv_data_elem IS     SUPPLIED AND
       iv_data_elem IS NOT INITIAL.
      "Get techn. descr. of domain and set param. name for exception
      lo_elem_desc ?= get_type_desc( iv_name = iv_data_elem ).
      lv_param_name = 'IV_DATA_ELEM' ##no_text.

    ELSEIF iv_data IS SUPPLIED.
      "Get techn. descr. of data field and set param. name for exception
      lo_elem_desc ?= get_type_desc( iv_data = iv_data ).
      lv_param_name = 'IV_DATA' ##no_text.

    ELSE.
      "At least one of the following parameters must be set: &1 &2 &3 &4
      RAISE EXCEPTION TYPE zcx_ca_param
        EXPORTING
          textid   = zcx_ca_param=>at_least_one
          mv_msgty = c_msgty_e
          mv_msgv1 = 'IV_DATA'
          mv_msgv2 = 'IV_DATA_ELEM' ##no_text.
    ENDIF.

    "Set parameter name for error message
    IF iv_param_name IS     SUPPLIED AND
       iv_param_name IS NOT INITIAL.
      lv_param_name = iv_param_name.
    ENDIF.

    "Is DDIC type?
    IF lo_elem_desc->is_ddic_type( ) EQ abap_false.
      "Object (in parameter) &1 is not defined in DDIC
      RAISE EXCEPTION TYPE zcx_ca_intern
        EXPORTING
          textid   = zcx_ca_intern=>obj_not_ddic_type
          mv_msgty = c_msgty_e
          mv_msgv1 = lv_param_name.
    ENDIF.

    "Get fixed values
    rt_fixed_vals = lo_elem_desc->get_ddic_fixed_values( ).
    IF rt_fixed_vals IS INITIAL.
      "Object (in parameter) &1 has no fixed values - has no or is no domain
      RAISE EXCEPTION TYPE zcx_ca_intern
        EXPORTING
          textid   = zcx_ca_intern=>has_no_fixed_vals
          mv_msgty = c_msgty_e
          mv_msgv1 = lv_param_name.
    ENDIF.
  ENDMETHOD.                    "get_fixed_values


  METHOD get_type_desc.
    "-----------------------------------------------------------------*
    "   Get technical description (RTTI) of an object
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lx_error             TYPE REF TO zcx_ca_intern.

    IF iv_name IS NOT INITIAL.
      "Get technical description by name
      cl_abap_typedescr=>describe_by_name(
                                      EXPORTING
                                        p_name         = iv_name
                                      RECEIVING
                                        p_descr_ref    = mo_type_desc
                                      EXCEPTIONS
                                        type_not_found = 1
                                        OTHERS         = 2 ).
      CASE sy-subrc.
        WHEN 0.
          "Everything is fine

        WHEN 1.
          "Parameter '&1' has invalid value '&2'
          RAISE EXCEPTION TYPE zcx_ca_param
            EXPORTING
              textid   = zcx_ca_param=>param_invalid
              mv_msgty = c_msgty_e
              mv_msgv1 = 'IV_NAME' ##no_text
              mv_msgv2 = CONV #( iv_name ).

        WHEN OTHERS.
          lx_error = zcx_ca_intern=>create_exception(
                                        iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                        iv_class    = 'CL_ABAP_TYPEDESCR'
                                        iv_method   = 'DESCRIBE_BY_NAME'
                                        iv_subrc    = sy-subrc ) ##no_text.
          IF lx_error IS BOUND.
            RAISE EXCEPTION lx_error.
          ENDIF.
      ENDCASE.

    ELSEIF ir_data IS BOUND.
      "Get technical description by reference of data field / value
      cl_abap_typedescr=>describe_by_data_ref(
                                          EXPORTING
                                            p_data_ref           = ir_data
                                          RECEIVING
                                            p_descr_ref          = mo_type_desc
                                          EXCEPTIONS
                                            reference_is_initial = 1
                                            OTHERS               = 2 ).
      CASE sy-subrc.
        WHEN 0.
          "Everything is fine

        WHEN 1.
          "Parameter '&1' has invalid value '&2'
          RAISE EXCEPTION TYPE zcx_ca_param
            EXPORTING
              textid   = zcx_ca_param=>param_invalid
              mv_msgty = c_msgty_e
              mv_msgv1 = 'IR_DATA' ##no_text
              mv_msgv2 = 'Data reference'(drf).

        WHEN OTHERS.
          lx_error = zcx_ca_intern=>create_exception(
                                          iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                          iv_class    = 'CL_ABAP_TYPEDESCR'
                                          iv_method   = 'DESCRIBE_BY_DATA_REF'
                                          iv_subrc    = sy-subrc ) ##no_text.
          IF lx_error IS BOUND.
            RAISE EXCEPTION lx_error.
          ENDIF.
      ENDCASE.

    ELSEIF io_object IS BOUND.
      "Get technical description by a class / interface reference
      cl_abap_typedescr=>describe_by_object_ref(
                                          EXPORTING
                                            p_object_ref         = io_object
                                          RECEIVING
                                            p_descr_ref          = mo_type_desc
                                          EXCEPTIONS
                                            reference_is_initial = 1
                                            OTHERS               = 2 ).
      CASE sy-subrc.
        WHEN 0.
          "Everything is fine

        WHEN 1.
          "Parameter '&1' has invalid value '&2'
          RAISE EXCEPTION TYPE zcx_ca_param
            EXPORTING
              textid   = zcx_ca_param=>param_invalid
              mv_msgty = c_msgty_e
              mv_msgv1 = 'IO_OBJECT' ##no_text
              mv_msgv2 = 'Object reference'(orf).

        WHEN OTHERS.
          lx_error = zcx_ca_intern=>create_exception(
                                         iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                         iv_class    = 'CL_ABAP_TYPEDESCR'
                                         iv_method   = 'DESCRIBE_BY_OBJECT_REF'
                                         iv_subrc    = sy-subrc ) ##no_text.
          IF lx_error IS BOUND.
            RAISE EXCEPTION lx_error.
          ENDIF.
      ENDCASE.

    ELSEIF iv_data IS SUPPLIED.
      "Get technical description by data field / value
      mo_type_desc = cl_abap_typedescr=>describe_by_data( iv_data ).

    ELSE.
      "At least one of the following parameters must be set: &1 &2 &3 &4
      RAISE EXCEPTION TYPE zcx_ca_param
        EXPORTING
          textid   = zcx_ca_param=>at_least_one
          mv_msgty = c_msgty_e
          mv_msgv1 = 'IV_NAME'
          mv_msgv2 = 'IV_DATA'
          mv_msgv3 = 'IR_DATA'
          mv_msgv4 = 'IR_OBJECT' ##no_text.
    ENDIF.

    "Return type description or use it internal
    IF ro_type_desc IS SUPPLIED.
      ro_type_desc = mo_type_desc.
    ENDIF.
  ENDMETHOD.                    "get_type_desc

ENDCLASS.

