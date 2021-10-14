REPORT zminesweeper_superbutton.

PARAMETERS: p_x TYPE numc2 DEFAULT 30, " = max
            p_y TYPE numc2 DEFAULT 16, " = max
            p_m TYPE numc2 DEFAULT 99. " = max

CLASS minesweeper_superbutton DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS class_constructor .
    CLASS-METHODS mark_field
      IMPORTING
        !iv_ucomm        TYPE syucomm
        !iv_set_mineflag TYPE abap_bool DEFAULT abap_false .
    CLASS-METHODS set_dimension
      IMPORTING
        !iv_x     TYPE num2 DEFAULT 16
        !iv_y     TYPE num2 DEFAULT 30
        !iv_mines TYPE num2 DEFAULT 99 .
    CLASS-METHODS at_selection_screen_output .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF mts_minefield,
        x            TYPE numc2,
        y            TYPE numc2,
        mine         TYPE abap_bool,
        discovered   TYPE abap_bool,
        guessed_mine TYPE abap_bool,
        text         TYPE string,
      END OF mts_minefield .
    TYPES:
      mtt_minefield TYPE HASHED TABLE OF mts_minefield WITH UNIQUE KEY x y .

    CLASS-DATA mv_x TYPE numc2 VALUE 30 ##NO_TEXT.
    CLASS-DATA mv_y TYPE numc2 VALUE 16 ##NO_TEXT.
    CLASS-DATA mv_mines TYPE numc2 VALUE 99 ##NO_TEXT.
    CLASS-DATA mt_minefield TYPE mtt_minefield .
    CLASS-DATA mv_mark_mine TYPE flag VALUE '' ##NO_TEXT.

    CLASS-METHODS get_minefield_info
      IMPORTING
        !iv_x          TYPE numc2
        !iv_y          TYPE numc2
      RETURNING
        VALUE(rv_info) TYPE char4 .
    CLASS-METHODS build_minefield
      IMPORTING
        !iv_x TYPE numc2
        !iv_y TYPE numc2 .
    CLASS-METHODS discover_field
      IMPORTING
        !iv_x TYPE numc2
        !iv_y TYPE numc2 .
ENDCLASS.



CLASS minesweeper_superbutton IMPLEMENTATION.

  METHOD at_selection_screen_output.
    DATA: lv_x TYPE numc2,
          lv_y TYPE numc2.
    LOOP AT SCREEN.

      IF screen-group1 = 'INT'.
        screen-intensified = '1'.
      ENDIF.

      FIND REGEX 'GV_(\d{2})_(\d{2})\s*' IN screen-name SUBMATCHES lv_x lv_y.
      IF sy-subrc = 0.
* hide excess fields
        IF lv_x > mv_x
          OR lv_y > mv_y.
          screen-input     = '0'.
          screen-invisible = '1'.
        ELSE.
          DATA(lv_pushbutton) = |(ZMINESWEEPER_SUPERBUTTON){ screen-name }|.
          ASSIGN (lv_pushbutton) TO FIELD-SYMBOL(<lv_pushbutton>).
          IF sy-subrc = 0.
            <lv_pushbutton> = get_minefield_info( iv_x = lv_x iv_y = lv_y ).
          ENDIF.
        ENDIF.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.


  METHOD build_minefield.

    DATA: ls_minefield TYPE mts_minefield,
          lv_mines     TYPE i.

*--------------------------------------------------------------------*
* Build minefield
*--------------------------------------------------------------------*
    DO mv_x TIMES.
      ls_minefield-x = sy-index.
      DO mv_y TIMES.
        ls_minefield-y = sy-index.
* First click will always be a "discovered" one
        IF    iv_x = ls_minefield-x
          AND iv_y = ls_minefield-y.
          ls_minefield-discovered = abap_true.
          INSERT ls_minefield INTO TABLE mt_minefield ASSIGNING FIELD-SYMBOL(<ls_initial_click>).
        ELSE.
          ls_minefield-discovered = abap_false.
          INSERT ls_minefield INTO TABLE mt_minefield.
        ENDIF.
      ENDDO.
    ENDDO.

*--------------------------------------------------------------------*
* Set mines - make sure no discovered field is set to mine to have first click always be ok
*--------------------------------------------------------------------*
    DATA(lo_random_x) = cl_abap_random_int=>create( seed = CONV i( sy-uzeit )
                                                    min  = 1
                                                    max  = CONV i( mv_x * mv_y ) ).
    WHILE lv_mines < mv_mines.
      DATA(lv_i) = lo_random_x->get_next( ).
      ls_minefield-x = lv_i MOD mv_x + 1.
      ls_minefield-y = ( lv_i - 1 ) DIV mv_x + 1.
      READ TABLE mt_minefield ASSIGNING FIELD-SYMBOL(<ls_minefield>) WITH TABLE KEY x = ls_minefield-x
                                                                                    y = ls_minefield-y.
      IF    sy-subrc = 0
        AND <ls_minefield>-discovered = abap_false
        AND <ls_minefield>-mine       = abap_false.
        ADD 1 TO lv_mines.
        <ls_minefield>-mine = abap_true.
      ENDIF.

    ENDWHILE.

* Reset disovered field to allow autodiscover in case we have no surroundings
    <ls_initial_click>-discovered = abap_false.
  ENDMETHOD.


  METHOD class_constructor.

    DATA h TYPE d020s.
    DATA f TYPE TABLE OF d021s.
    DATA e TYPE TABLE OF d022s.
    DATA m TYPE TABLE OF d023s.
    DATA: BEGIN OF id,
            p TYPE progname,
            d TYPE sydynnr,
          END OF id.

    id = VALUE #( p = 'ZMINESWEEPER_SUPERBUTTON' d = '4000' ).
    IMPORT DYNPRO h f e m ID id.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    id = VALUE #( p = 'ZMINESWEEPER_SUPERBUTTON' d = '0100' ). " This is the generated screen
    IMPORT DYNPRO h f e m ID id.

    LOOP AT f ASSIGNING FIELD-SYMBOL(<ls_f>) WHERE fnam(3) = 'GV_'.
      <ls_f>-res1+228(8) = <ls_f>-fnam.

    ENDLOOP.
    h-dnum = '4000'.
    h-fnum = '4000'.
    id = VALUE #( p = 'ZMINESWEEPER_SUPERBUTTON' d = '4000' ).
    EXPORT DYNPRO h f e m ID id.


  ENDMETHOD.


  METHOD discover_field.

    DATA: ls_minefield               TYPE mts_minefield.


    READ TABLE mt_minefield ASSIGNING FIELD-SYMBOL(<ls_field>) WITH TABLE KEY x = iv_x
                                                                              y = iv_y.
    ASSERT sy-subrc = 0.
    IF <ls_field>-discovered = abap_true. " already discovered --> don't act again
      RETURN.
    ENDIF.

    <ls_field>-discovered = abap_true.

    IF get_minefield_info( iv_x = iv_x iv_y = iv_y ) = space. " No mine --> expand
* No surrounding mines --> autodiscover all surrounding fields
      DO 3 TIMES.
        ls_minefield-x = iv_x - 2 + sy-index. " from -1 to +1 .
        DO 3 TIMES.
          ls_minefield-y = iv_y - 2 + sy-index. " from -1 to +1 .
          READ TABLE mt_minefield ASSIGNING FIELD-SYMBOL(<ls_minefield>) WITH TABLE KEY x = ls_minefield-x
                                                                                        y = ls_minefield-y.
          IF sy-subrc = 0.
            discover_field( iv_x = ls_minefield-x iv_y = ls_minefield-y ).
          ENDIF.
        ENDDO.
      ENDDO.

    ENDIF.

  ENDMETHOD.


  METHOD get_minefield_info.
    DATA: ls_minefield               TYPE mts_minefield,
          lv_count_surrounding_mines TYPE i.

    FIELD-SYMBOLS: <ls_minefield> LIKE LINE OF mt_minefield.

    READ TABLE mt_minefield ASSIGNING <ls_minefield> WITH TABLE KEY x = iv_x
                                                                    y = iv_y.
    IF   sy-subrc <> 0.
      rv_info = '.'.
      RETURN.
    ENDIF.

    IF <ls_minefield>-guessed_mine = abap_true.
*        rv_info = icon_defect.
*        rv_info = icon_red_xcircle.
      rv_info = icon_status_critical.
      RETURN.
    ENDIF.

    IF <ls_minefield>-discovered = abap_false.
      rv_info = '.'.
      RETURN.
    ENDIF.



    IF <ls_minefield>-mine = abap_true.
      rv_info = icon_led_red.
      RETURN.
    ENDIF.

* Obviously we have a discovered field .  Count surrounding mines
    DO 3 TIMES.
      ls_minefield-x = iv_x - 2 + sy-index. " from -1 to +1 .
      DO 3 TIMES.
        ls_minefield-y = iv_y - 2 + sy-index. " from -1 to +1 .
        READ TABLE mt_minefield ASSIGNING <ls_minefield> WITH TABLE KEY x = ls_minefield-x
                                                                        y = ls_minefield-y.
        IF sy-subrc = 0
          AND <ls_minefield>-mine = abap_true.
          ADD 1 TO lv_count_surrounding_mines.
        ENDIF.
      ENDDO.
    ENDDO.

    IF lv_count_surrounding_mines = 0.
      rv_info = space.
    ELSE.
      rv_info = |{ lv_count_surrounding_mines }|.
    ENDIF.

  ENDMETHOD.


  METHOD mark_field.
    DATA: lv_x TYPE numc2,
          lv_y TYPE numc2.
*--------------------------------------------------------------------*
* Find pressed button
*--------------------------------------------------------------------*
    FIND REGEX 'GV_(\d{2})_(\d{2})\s*' IN iv_ucomm SUBMATCHES lv_x lv_y.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF mt_minefield IS INITIAL.
      build_minefield( iv_x = lv_x iv_y = lv_y ).
      discover_field( iv_x = lv_x iv_y = lv_y  ).
    ELSE.
      READ TABLE mt_minefield ASSIGNING FIELD-SYMBOL(<ls_minefield>) WITH TABLE KEY x = lv_x y = lv_y.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      IF <ls_minefield>-discovered = abap_true. " once is enough ( for marking )
        RETURN.
      ENDIF.
      IF iv_set_mineflag = abap_true.
* just toggle the guessed mine field
        IF <ls_minefield>-guessed_mine = abap_true.
          <ls_minefield>-guessed_mine = abap_false.
        ELSE.
          <ls_minefield>-guessed_mine = abap_true.
        ENDIF.
      ELSE.
* Set field as free ( hopefully no mine )
        discover_field( iv_x = lv_x iv_y = lv_y  ).
        IF <ls_minefield>-mine = abap_true.
          MESSAGE 'You lost' TYPE 'I' DISPLAY LIKE 'E'.
          DO mv_x TIMES.
            lv_x = sy-index.
            DO mv_y TIMES.
              lv_y = sy-index.
              discover_field( iv_x = lv_x iv_y = lv_y  ).
            ENDDO.
          ENDDO.
        ENDIF.
      ENDIF.
    ENDIF.

    cl_gui_cfw=>set_new_ok_code( 'MARKED_FIELD' ).


  ENDMETHOD.


  METHOD set_dimension.

    IF    iv_x >= 0
      AND iv_x <= 30.
      mv_x = iv_x.
    ELSEIF iv_x >= 0.
      mv_x = 30.
    ELSE.
      mv_x = 1.
    ENDIF.

    IF    iv_y >= 0
      AND iv_y <= 16.
      mv_y = iv_y.
    ELSEIF iv_y >= 0.
      mv_y = 16.
    ELSE.
      mv_y = 1.
    ENDIF.

    IF iv_mines > 0.
      mv_mines = iv_mines.
    ELSE.
      mv_mines = 1.
    ENDIF.

    CLEAR mt_minefield.

  ENDMETHOD.
ENDCLASS.



DEFINE minesweeper_field.
  SELECTION-SCREEN PUSHBUTTON (4) gv_&1_&2 USER-COMMAND gv_&1_&2 VISIBLE LENGTH 2.
END-OF-DEFINITION.

DEFINE minesweeper_line.
  SELECTION-SCREEN BEGIN OF LINE.
    minesweeper_field 01 &1.
    minesweeper_field 02 &1.
    minesweeper_field 03 &1.
    minesweeper_field 04 &1.
    minesweeper_field 05 &1.
    minesweeper_field 06 &1.
    minesweeper_field 07 &1.
    minesweeper_field 08 &1.
    minesweeper_field 09 &1.
    minesweeper_field 10 &1.
    minesweeper_field 11 &1.
    minesweeper_field 12 &1.
    minesweeper_field 13 &1.
    minesweeper_field 14 &1.
    minesweeper_field 15 &1.
    minesweeper_field 16 &1.
    minesweeper_field 17 &1.
    minesweeper_field 18 &1.
    minesweeper_field 19 &1.
    minesweeper_field 20 &1.
    minesweeper_field 21 &1.
    minesweeper_field 22 &1.
    minesweeper_field 23 &1.
    minesweeper_field 24 &1.
    minesweeper_field 25 &1.
    minesweeper_field 26 &1.
    minesweeper_field 27 &1.
    minesweeper_field 28 &1.
    minesweeper_field 29 &1.
    minesweeper_field 30 &1.
  SELECTION-SCREEN END OF LINE.
END-OF-DEFINITION.


SELECTION-SCREEN BEGIN OF SCREEN 0100 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (20) TEXT-if1  MODIF ID int. "Left click for free
SELECTION-SCREEN COMMENT (10) TEXT-spc  MODIF ID int.
SELECTION-SCREEN COMMENT (20) TEXT-if2  MODIF ID int. "Right click for mine
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP 2.
minesweeper_line 01.
minesweeper_line 02.
minesweeper_line 03.
minesweeper_line 04.
minesweeper_line 05.
minesweeper_line 06.
minesweeper_line 07.
minesweeper_line 08.
minesweeper_line 09.
minesweeper_line 10.
minesweeper_line 11.
minesweeper_line 12.
minesweeper_line 13.
minesweeper_line 14.
minesweeper_line 15.
minesweeper_line 16.
SELECTION-SCREEN END OF SCREEN 0100.


DEFINE minesweeper_form.
  FORM on_ctmenu_gv_&1_&2 USING l_menu TYPE REF TO cl_ctmenu.
     minesweeper_superbutton=>mark_field( iv_ucomm = 'GV_&1_&2' iv_set_mineflag = abap_true ).
  ENDFORM.
END-OF-DEFINITION.

DEFINE minesweeper_form_line.

  minesweeper_form 01 &1.
  minesweeper_form 02 &1.
  minesweeper_form 03 &1.
  minesweeper_form 04 &1.
  minesweeper_form 05 &1.
  minesweeper_form 06 &1.
  minesweeper_form 07 &1.
  minesweeper_form 08 &1.
  minesweeper_form 09 &1.
  minesweeper_form 10 &1.
  minesweeper_form 11 &1.
  minesweeper_form 12 &1.
  minesweeper_form 13 &1.
  minesweeper_form 14 &1.
  minesweeper_form 15 &1.
  minesweeper_form 16 &1.
  minesweeper_form 17 &1.
  minesweeper_form 18 &1.
  minesweeper_form 19 &1.
  minesweeper_form 20 &1.
  minesweeper_form 21 &1.
  minesweeper_form 22 &1.
  minesweeper_form 23 &1.
  minesweeper_form 24 &1.
  minesweeper_form 25 &1.
  minesweeper_form 26 &1.
  minesweeper_form 27 &1.
  minesweeper_form 28 &1.
  minesweeper_form 29 &1.
  minesweeper_form 30 &1.
END-OF-DEFINITION.

minesweeper_form_line 01.
minesweeper_form_line 02.
minesweeper_form_line 03.
minesweeper_form_line 04.
minesweeper_form_line 05.
minesweeper_form_line 06.
minesweeper_form_line 07.
minesweeper_form_line 08.
minesweeper_form_line 09.
minesweeper_form_line 10.
minesweeper_form_line 11.
minesweeper_form_line 12.
minesweeper_form_line 13.
minesweeper_form_line 14.
minesweeper_form_line 15.
minesweeper_form_line 16.



AT SELECTION-SCREEN OUTPUT.
  IF sy-dynnr = '4000'.
    minesweeper_superbutton=>at_selection_screen_output( ).
  ENDIF.

AT SELECTION-SCREEN.
  IF sy-dynnr = '4000'.
    minesweeper_superbutton=>mark_field( sy-ucomm ).
  ENDIF.


END-OF-SELECTION.
  minesweeper_superbutton=>set_dimension( iv_x = p_x iv_y = p_y iv_mines = p_m ).
  CALL SCREEN 9000.
*&---------------------------------------------------------------------*
*& Module STATUS_9000 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS '9000'.
*  minesweeper_superbutton=>at_selection_screen_output( ).

  CLEAR sy-ucomm.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  CASE sy-ucomm.
    WHEN 'ABBR'
      OR 'EXIT'
      OR 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
*      minesweeper_superbutton=>mark_field( sy-ucomm ).
  ENDCASE.
ENDMODULE.
