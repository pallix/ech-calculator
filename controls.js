$( document ).ready(function() {
  // define what element should be observed by the observer
  // and what types of mutations trigger the callback

  $('#cog').on('click', function () {
      $('#cog').addClass("hidden");
      $("#controls").removeClass("hidden");
      swiperScenario.update()
      swiperScale.update()
      swiperTime.update()
  })

  $("#controls").append('<img src="/images/close.svg" height="22px" id="close"/>');

  $('#close').on('click', function () {
    $("#controls").addClass("hidden");
    $('#cog').removeClass("hidden");
  })

  // Scale

  $("#controls").append('<div id="swiper-scale" class="swiper-container"><h4>Scale</h2><div class="swiper-wrapper"></div><div class="swiper-pagination"></div><div class="swiper-button-next"></div><div class="swiper-button-prev"></div></div>')

  $('#flare-component-4 > option').each(function(i, e) {
    $("#swiper-scale .swiper-wrapper").append('<div class="swiper-slide"><h2>' + e.text + '</h2></div>')
  })

  var swiperScale = new Swiper('#swiper-scale', {
      pagination: '#swiper-scale .swiper-pagination',
      paginationClickable: true,
      nextButton: '#swiper-scale .swiper-button-next',
      prevButton: '#swiper-scale .swiper-button-prev',
      spaceBetween: 30
  });


  scaleSelect = $('#flare-component-4').get(0);

  swiperScale.on("slideChangeEnd", function(ev) {
    scaleSelect.options[ev.activeIndex].selected = true
    var ev = new Event('change');
    scaleSelect.dispatchEvent(ev);
  })


  // Time

  $("#controls").append('<div id="swiper-time" class="swiper-container"><h4>Time</h2><div class="swiper-wrapper"></div><div class="swiper-pagination"></div><div class="swiper-button-next"></div><div class="swiper-button-prev"></div></div>')

  $('#flare-component-5 > option').each(function(i, e) {
    $("#swiper-time .swiper-wrapper").append('<div class="swiper-slide"><h2>' + e.text + '</h2></div>')
  })

  var swiperTime = new Swiper('#swiper-time', {
      pagination: '#swiper-time .swiper-pagination',
      paginationClickable: true,
      nextButton: '#swiper-time .swiper-button-next',
      prevButton: '#swiper-time .swiper-button-prev',
      spaceBetween: 30
  });


  timeSelect = $('#flare-component-5').get(0);

  swiperTime.on("slideChangeEnd", function(ev) {
    timeSelect.options[ev.activeIndex].selected = true
    var ev = new Event('change');
    timeSelect.dispatchEvent(ev);
  })

  // Scenario

  $("#controls").append('<div id="swiper-scenario" class="swiper-container"><h4>Scenario</h2><div class="swiper-wrapper"></div><div class="swiper-pagination"></div><div class="swiper-button-next"></div><div class="swiper-button-prev"></div></div>')

  $('#flare-component-3 > option').each(function(i, e) {
    $("#swiper-scenario .swiper-wrapper").append('<div class="swiper-slide"><h2>' + e.text + '</h2></div>')
  })

  var swiperScenario = new Swiper('#swiper-scenario', {
      pagination: '#swiper-scenario .swiper-pagination',
      paginationClickable: true,
      nextButton: '#swiper-scenario .swiper-button-next',
      prevButton: '#swiper-scenario .swiper-button-prev',
      spaceBetween: 30
  });

  scenarioSelect = $('#flare-component-3').get(0);

  swiperScenario.on("slideChangeEnd", function(ev) {
    scenarioSelect.options[ev.activeIndex].selected = true
    var ev = new Event('change');
    scenarioSelect.dispatchEvent(ev);
  })


  var hammertime = new Hammer($("#layer .hexGrid").get(0));
  hammertime.get('swipe').set({ direction: Hammer.DIRECTION_ALL });

  function nextOpt(sel, previous) {
    console.log(sel)
    console.log(sel.options)
    var i = sel.selectedIndex + sel.options.length * 100;
    if (previous) {
      sel.options[--i%sel.options.length].selected = true;
    } else {
      sel.options[++i%sel.options.length].selected = true;
    }
    var ev = new Event('change');
    sel.dispatchEvent(ev);
  }

  hammertime.on('swipe', function(ev) {
    console.log(ev)
    if (ev.direction == Hammer.DIRECTION_UP) {
      $('#cog').addClass("hidden");
      $("#controls").removeClass("hidden");
      swiperScenario.update()
      swiperScale.update()
      swiperTime.update()
    }
    if (ev.direction == Hammer.DIRECTION_DOWN) $("#controls").hide();
    if (ev.direction == Hammer.DIRECTION_RIGHT) nextOpt(scenarioSelect, true)
    if (ev.direction == Hammer.DIRECTION_LEFT) nextOpt(scenarioSelect)
  });

  eatedFoodRatioOriginal = $('#flare-component-6')
  $('#layer #Eating').prepend('<div id="eatedFoodRatioLayered" class="layer-control"></div>')

  // eatedFoodRatioLayered = $('#eatedFoodRatioLayered').slider({
  //   value: eatedFoodRatioOriginal.val()*100,
  //   start: function(event, ui) {
  //     eatedFoodRatioOriginal.val(ui.value/100);
  //     var elem = eatedFoodRatioOriginal.get(0)
  //     var ev = new Event('input')
  //     elem.dispatchEvent(ev);
  //     return true;
  //   }
  // });

  eatedFoodRatioLayered = $('#eatedFoodRatioLayered').get(0)

  // noUiSlider.create(eatedFoodRatioLayered, {
  //   start: [ eatedFoodRatioOriginal.val()*100 ],
  // 	range: {
  // 		'min': [  0 ],
  // 		'max': [ 100 ]
  // 	}
  // })
  //
  // eatedFoodRatioLayered.noUiSlider.on('end', function(values){
  //   eatedFoodRatioOriginal.val(values[0]/100);
  //   var elem = eatedFoodRatioOriginal.get(0);
  //   var ev = new Event('input');
  //   elem.dispatchEvent(ev);
  //   // return true;
  // });

  scaleControl = $('#layer #Scale')
  timeControl = $('#layer #Time')

  scaleControl.on('click', function(event) {

  })

});

// class="ui-input-text ui-body-null ui-corner-all ui-shadow-inset ui-body-c ui-slider-input"
