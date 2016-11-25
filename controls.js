$( document ).ready(function() {
  // define what element should be observed by the observer
  // and what types of mutations trigger the callback

  $("#output .hexGrid").addClass("fadeIn");

  //
  // Bottom Menu
  //
  //

  $('#cog').on('click', function () {
      $('#cog').addClass("hidden");
      $("#controls").removeClass("hidden");
      swiperScenario.update()
      // swiperScale.update()
      swiperTime.update()
  })

  $("#controls").append('<img src="/images/close.svg" height="22px" id="close"/>');

  $('#close').on('click', function () {
    $("#controls").addClass("hidden");
    $('#cog').removeClass("hidden");
  })

  // Scale
  //
  // $("#controls").append('<div id="swiper-scale" class="swiper-container"><h4>Scale</h2><div class="swiper-wrapper"></div><div class="swiper-pagination"></div><div class="swiper-button-next"></div><div class="swiper-button-prev"></div></div>')
  //
  // $('#flare-component-4 > option').each(function(i, e) {
  //   $("#swiper-scale .swiper-wrapper").append('<div class="swiper-slide"><h2>' + e.text + '</h2></div>')
  // })
  //
  // var swiperScale = new Swiper('#swiper-scale', {
  //     pagination: '#swiper-scale .swiper-pagination',
  //     paginationClickable: true,
  //     nextButton: '#swiper-scale .swiper-button-next',
  //     prevButton: '#swiper-scale .swiper-button-prev',
  //     spaceBetween: 30
  // });
  //
  //
  // scaleSelect = $('#flare-component-4').get(0);
  //
  // swiperScale.on("slideChangeEnd", function(ev) {
  //   scaleSelect.options[ev.activeIndex].selected = true
  //   var ev = new Event('change');
  //   scaleSelect.dispatchEvent(ev);
  // })


  // Time

  $("#controls").append('<div id="swiper-time" class="swiper-container"><h4>Period</h2><div class="swiper-wrapper"></div><div class="swiper-pagination"></div><div class="swiper-button-next"></div><div class="swiper-button-prev"></div></div>')

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
    $("#output .hexGrid").fadeOut();
    scenarioSelect.options[ev.activeIndex].selected = true
    var ev = new Event('change');
    scenarioSelect.dispatchEvent(ev);
    window.setTimeout(function() {
      $("#output .hexGrid").addClass("fadeIn");
    },250);
  })


  var hexes = $("#grid").hammer({domEvents:true})

  hexes.on('tap', '.hex', function(ev) {
    var scenario = $("#output .processes").attr('id');
    console.log(ev.target.id.split("-")[0])
    switch(scenario) {
      case "Calculator.Model.EatingOnly":
        if (ev.target.id == "Eating-0") $("#output .hexGrid #Eating a").toggleClass("hover")
        if (ev.target.id == "Shopping-0") $("#output .hexGrid #Shopping a").toggleClass("hover")
        break;
      case "Calculator.Model.EatingBinning":
        $("#output .hexGrid #"+ ev.target.id.split("-") + " a").toggleClass("hover")
        if (ev.target.id == "Composting") $("#output .hexGrid #Binning a").toggleClass("hover")
        break;
      case "Calculator.Model.EatingBinningWormComposting":
      case "Calculator.Model.EatingBinningWormCompostingFoodGardening":
      case "Calculator.Model.EatingBinningWormCompostingFoodGardenWatering":
      case "Calculator.Model.EatingBinningWormCompostingFoodGardenRainwater":
      case "Calculator.Model.EatingBinningFoodSharing":
      case "Calculator.Model.EatingBinningWormCompostingFoodSharing":
        break;
      default:
        if (ev.target.id) $("#output .hexGrid #"+ ev.target.id.split("-") + " a").toggleClass("hover")
    }

    if (ev.target.id) console.log($("#output .hexGrid #"+ ev.target.id.split("-")[0] + " a"))
  })

  function nextOpt(sel, previous) {
    var i = sel.selectedIndex;
    if (previous) {
      if (i>0) {
        swiperScenario.slideTo(--i)
      }
    } else {
      if (i<sel.options.length) {
        swiperScenario.slideTo(++i)
      }
    }
  }

  var layer = $("#grid").hammer();
  // layer.data("hammer").get('swipe').set({ direction: Hammer.DIRECTION_ALL });

  layer.on('swipe', function(ev) {
    console.log(ev)
    if (ev.gesture) {
      if (ev.gesture.direction == Hammer.DIRECTION_UP) {
        $('#cog').addClass("hidden");
        $("#controls").removeClass("hidden");
        swiperScenario.update()
        // swiperScale.update()
        swiperTime.update()
      }
      if (ev.gesture.direction == Hammer.DIRECTION_DOWN) $("#controls").hide();
      if (ev.gesture.direction == Hammer.DIRECTION_RIGHT) nextOpt(scenarioSelect, true)
      if (ev.gesture.direction == Hammer.DIRECTION_LEFT) nextOpt(scenarioSelect)
    }
  });
    // var layer = $("#layer ul").get(0);
    // var layer_item = $("#layer ul li");
    // var hammertime = new Hammer(layer, {domEvents: true});



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
