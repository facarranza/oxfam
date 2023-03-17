
$( document ).ready(function() {

$(document).on('click', '.needed_sub', function () {
  Shiny.onInputChange('last_click_sub',this.id);
 });

$(document).on('click', '.needed_sub', function(){
    $('.needed_sub').removeClass('basic_active_sub');
    $(this).addClass('basic_active_sub');
});


});
