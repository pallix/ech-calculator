var updateEating = function() {


}

$( document ).ready(function() {
  // define what element should be observed by the observer
  // and what types of mutations trigger the callback

  eatedFoodRatioOriginal = $('#flare-component-5')
  $('#layer #Eating').prepend('<div id="eatedFoodRatioLayered"></div>')
  eatedFoodRatioLayered = $('#eatedFoodRatioLayered').slider({
    slide: function(event, ui) {
      eatedFoodRatioOriginal.val(ui.value/100);
      var elem = eatedFoodRatioOriginal.get(0)
      var ev = new Event('input')
      elem.dispatchEvent(ev);
      return true;
    }
  });

  eatedFoodRatioLayered.slider( "value", eatedFoodRatioOriginal.val()*100)

});
