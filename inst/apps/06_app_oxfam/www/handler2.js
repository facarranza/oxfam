$( document ).ready(function() {

$(document).on('click', '.needed', function () {
  Shiny.onInputChange('last_click',this.id);
 });

$(document).on('click', '.needed', function(){
    $('.needed').removeClass('basic_active');
    $(this).addClass('basic_active');
});


});
