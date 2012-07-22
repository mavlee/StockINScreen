$(document).ready(function() {
  console.log('[ function : ready() start ]');
  $('.stockdata').first().removeClass('hidden');

  $('.master tbody tr').click(function() {
    var text = this.firstElementChild.innerHTML;
    $('.stockdata').addClass('hidden');
    var b = text.split(" ");
    var selector = "." + b[0] + "." + b[1];
    $(selector).removeClass('hidden');
    $('html, body').animate({ scrollTop: 0 }, 'slow');
  });

  /*** Event registrations ***/
  // filter button click event
  $('.filterButton').click(function() {
    applyFilters();
  });

  console.log('[ function : ready() end ]');
});

function applyFilters() {
  console.log('[ function : applyFilters() start ]');

  var maxPrice = parseFloat($('.maxPrice').val()),
      minPrice = parseFloat($('.minPrice').val()),
      maxCap = parseFloat($('.maxCap').val()),
      minCap = parseFloat($('.minCap').val()),
      maxPE = parseFloat($('.maxPE').val()),
      minPE = parseFloat($('.minPE').val()),
      maxDiv = parseFloat($('.maxDiv').val()),
      minDiv = parseFloat($('.minDiv').val()),
      maxConns = parseFloat($('.maxConns').val()),
      minConns = parseFloat($('.minConns').val()),
      profileIndustry = $('.profile-industry').text();

  // Check whether filter fields contains valid number value.
  if (isNaN(maxPrice) ||
      isNaN(minPrice) ||
      isNaN(maxCap) ||
      isNaN(minCap) ||
      isNaN(maxPE) ||
      isNaN(minPE) ||
      isNaN(maxDiv) ||
      isNaN(minDiv) ||
      isNaN(maxConns) ||
      isNaN(minConns)) {
    alert('Invalid values in filters!');
    return;
  }	

  // Check the maximum value bigger than minimum value.
  if (minPrice > maxPrice ||
      minCap > maxCap ||
      minPE > maxPE ||
      minDiv > maxDiv ||
      minConns > maxConns) {
    alert('Minimum value should not bigger than maximum value!');
    return;
  } 

  // Iterate all rows and check whether each row satisfy the filters.
  $('.stockRecord').each(function() {
    var price = parseFloat($(this).find('.price').text()),
        CAP = parseFloat($(this).find('.CAP').text()),
        PE = parseFloat($(this).find('.PE').text()),
        Div = parseFloat($(this).find('.DV').text()),
        conns = parseFloat($(this).find('.conns').attr('data')),
        industry = $(this).find('.industry').text(),
        isValid = true;

    // Check each values are valid or not.
    if (price > maxPrice || price < minPrice) {
      isValid = false;
    } else if (CAP > maxCap || CAP < minCap) {
      isValid = false;
    } else if (PE > maxPE || PE < minPE) {
      isValid = false;
    } else if (Div > maxDiv || Div < minDiv) {
      isValid = false;
    } else if (conns > maxConns || conns < minConns) {
      isValid = false;
    } else if ($('.inMyIndustry').is(':checked'))  {
      isValid = (industry == profileIndustry);
    }

    // Hide or show based on the validation value.
    if (isValid) {
	$(this).removeClass('hidden');
    } else {
	$(this).addClass('hidden');
    }
  });

  console.log('[ function : applyFilters() end ]');
}
