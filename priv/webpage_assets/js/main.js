/* eslint-disable no-undef */
/* eslint-disable no-invalid-this */

(function ($) {
  'use strict';


  /* ==================================================================
    [ Validate ]*/
  const input = $('.validate-input .input100');

  $('.validate-form').on('submit', function () {
    let check = true;

    for (let i=0; i<input.length; i++) {
      if (validate(input[i]) == false) {
        showValidate(input[i]);
        check=false;
      }
    }

    return check;
  });


  $('.validate-form .input100').each(function () {
    $(this).focus(function () {
      hideValidate(this);
    });
  });

  function validate(input) {
    if ($(input).val().trim() == '') {
      return false;
    }
    return true;
  }

  function showValidate(input) {
    const thisAlert = $(input).parent();

    $(thisAlert).addClass('alert-validate');
  }

  function hideValidate(input) {
    const thisAlert = $(input).parent();

    $(thisAlert).removeClass('alert-validate');
  }
})(jQuery);
