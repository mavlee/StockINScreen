$(document).ready(function() {
  $('.stockdata').first().removeClass('hidden');

  $('.master tbody tr').click(function() {
    var text = this.firstElementChild.innerHTML;
    $('.stockdata').addClass('hidden');
    var b = text.split(" ");
    var selector = "." + b[0] + "." + b[1];
    $(selector).removeClass('hidden');
    $('html, body').animate({ scrollTop: 0 }, 'slow');
  });
});
