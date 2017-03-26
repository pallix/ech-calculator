$( document ).ready(function() {

  $("#output .hexGrid").addClass("fadeIn");

  //
  // Bottom Menu
  //
  //
  $('#cog').addClass("hidden");
  // $("#controls").removeClass("hidden");
  $("#controls").addClass("fadeIn")

  $('#cog').on('click', function () {
      $('#cog').addClass("hidden");
      $("#controls").removeClass("hidden");
      swiperScenario.update()
      // swiperScale.update()
      swiperTime.update()
  })

  // $("#controls").append('<img src="/images/close.svg" height="22px" id="close"/>');

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

  $('#flare-component-6 > option').each(function(i, e) {
    $("#swiper-time .swiper-wrapper").append('<div class="swiper-slide"><h2>' + e.text + '</h2></div>')
  })

  var swiperTime = new Swiper('#swiper-time', {
      pagination: '#swiper-time .swiper-pagination',
      paginationClickable: true,
      nextButton: '#swiper-time .swiper-button-next',
      prevButton: '#swiper-time .swiper-button-prev',
      spaceBetween: 30
  });


  timeSelect = $('#flare-component-6').get(0);

  var timeRange = {
  	'min': [     0 ],
  	// '10%': [   500,  500 ],
  	// '50%': [  4000, 1000 ],
  	'max': [ 10000 ]
  };

  // var timeSlider = document.getElementById('time-slider');
  //
  // noUiSlider.create(timeSlider, {
  // 	range: timeRange,
  //   start: [ 500, 4000 ],
	//   connect: true,
  //   behaviour: 'drag-fixed',
  // 	orientation: 'vertical',
  // 	pips: {
  // 		mode: 'range',
  // 		density: 3
  // 	}
  // });

  swiperTime.on("slideChangeEnd", function(ev) {
    console.log("swiperTime.slideChangeEnd", ev)
    $(".control-layer").remove()
    timeSelect.options[ev.activeIndex].selected = true
    var ev = new Event('change');
    timeSelect.dispatchEvent(ev);
    $("#output .hexGrid").addClass("fadeIn");
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
    console.log("swiperScenario.slideChangeEnd", ev)
    $(".control-layer").remove()
    $("#output .hexGrid").fadeOut();
    scenarioSelect.options[ev.activeIndex].selected = true
    var ev = new Event('change');
    scenarioSelect.dispatchEvent(ev);
    setupControls()
    window.setTimeout(function() {
      $("#output .hexGrid").addClass("fadeIn");
    },50);
  })

  function isClosed(control, process, ev) {
    return ev.target.id == control + "-Control" && !$("#output .hexGrid [id='" + process + "'] a").hasClass("hover")
  }

  function isOpened(control, process, ev) {
    return ev.target.id == control + "-Close" && $("#output .hexGrid [id='" + process + "'] a").hasClass("hover")
  }

  function Open(control, process) {
    $("#output .hexGrid [id='" + process + "'] a").addClass("hover")
    $("#" + control + "-Control").append('<img id="' + control + '-Close" class="control-layer" src="/images/close.svg" height="8px">')
  }

  function Close(control, process) {
    $("#output .hexGrid [id='" + process + "'] a").removeClass("hover")
    $("#" + control + "-Control img").remove();
  }

  function Handle(control, process, ev) {
    if (isClosed(control, process, ev)) {
      Open(control, process)
    } else if (isOpened(control, process, ev)) {
      Close(control, process)
    }
  }

  var evFlareInput = new Event('input');

  var numberEatingHouseholdsFlare = $('#flare-component-7')

  function HandleRainwaterTank(control, process, ev) {
    if (isClosed(control, process, ev)) {
      Open(control, process)
      $("#" + control + "-Control").append('<div id="eatingControl" class="control-layer range"><p id="eatingNumberControl"># of Flats</p><div id="eatingNumberControl" class="slider"></div></div>')
      var numberEatingHouseholdsSlider = $("#" + control + "-Control div#eatingControl div.slider").get(0)
      noUiSlider.create(numberEatingHouseholdsSlider, {
      	start: [numberEatingHouseholdsFlare.val()],
        step: 1,
      	tooltips: [true],
        range: {
      		'min': 0,
      		'max': 121
      	},
        format: wNumb({decimals: 0})
      });
      numberEatingHouseholdsSlider.noUiSlider.on("change", function(value) {
        numberEatingHouseholdsFlare.val(value)
        console.log(value)
        numberEatingHouseholdsFlare.get(0).dispatchEvent(evFlareInput);
        Open(control, process)
        $("#output .hexGrid").addClass("fadeIn");
      })
    } else if (isOpened(control, process, ev) && ev.target.id!="eatingControl") {
      $("#" + control + "-Control #eatingControl").remove()
      Close(control, process)
    }
  }

  function HandleEating(control, process, ev) {
    if (isClosed(control, process, ev)) {
      Open(control, process)
      $("#" + control + "-Control").append('<div id="eatingControl" class="control-layer range"><p id="eatingNumberControl"># of Flats</p><div id="eatingNumberControl" class="slider"></div></div>')
      var numberEatingHouseholdsSlider = $("#" + control + "-Control div#eatingControl div.slider").get(0)
      noUiSlider.create(numberEatingHouseholdsSlider, {
      	start: [numberEatingHouseholdsFlare.val()],
        step: 1,
      	tooltips: [true],
        range: {
      		'min': 0,
      		'max': 121
      	},
        format: wNumb({decimals: 0})
      });
      numberEatingHouseholdsSlider.noUiSlider.on("change", function(value) {
        numberEatingHouseholdsFlare.val(value)
        console.log(value)
        numberEatingHouseholdsFlare.get(0).dispatchEvent(evFlareInput);
        Open(control, process)
        $("#output .hexGrid").addClass("fadeIn");
      })
    } else if (isOpened(control, process, ev) && ev.target.id!="eatingControl") {
      $("#" + control + "-Control #eatingControl").remove()
      Close(control, process)
    }
  }

  var numberCompactorFlare = $('#flare-component-8')

  function HandleCompactor(control, process, control2, process2, ev) {
    if (isClosed(control, process, ev)) {
      Open(control, process)
      $("#" + control + "-Control").append('<div id="compactorControl" class="control-layer range"><p id="compactorNumberControl"># of Compactors</p><div id="compactorNumberControl" class="slider"></div></div>')
      var numberCompactorSlider = $("#" + control + "-Control div#compactorControl div.slider").get(0)
      noUiSlider.create(numberCompactorSlider, {
      	start: [numberCompactorFlare.val()],
        step: 1,
      	tooltips: [true],
        range: {
      		'min': 0,
      		'max': 121
      	},
        format: wNumb({decimals: 0})
      });

      numberCompactorSlider.noUiSlider.on("change", function(ev) {
        numberCompactorFlare.val(ev)
        numberCompactorFlare.get(0).dispatchEvent(evFlareInput);
        Open(control, process)
        Open(control2, process2)
        $("#output .hexGrid").addClass("fadeIn");
      })
    } else if (isOpened(control, process, ev) && ev.target.id!="compactorControl") {
      console.log(ev)
      $("#" + control + "-Control #compactorControl").remove()
      Close(control, process)
    }
  }

  var numberWormeriesFlare = $('#flare-component-9')

  function HandleWormery(control, process, control2, process2, ev) {
    if (isClosed(control, process, ev)) {
      Open(control, process)
      $("#" + control + "-Control").append('<div id="' + control + 'Control" class="control-layer range"><p id="' + control + 'Control"># of Wormeries</p><div id="' + control + 'Control" class="slider"></div></div>')
      var numberWormeriesSlider = $("#" + control + "-Control div#" + control + "Control div.slider").get(0)
      noUiSlider.create(numberWormeriesSlider, {
      	start: [numberWormeriesFlare.val()],
        step: 1,
      	tooltips: [true],
        range: {
      		'min': 0,
      		'max': 10
      	},
        format: wNumb({decimals: 0})
      });
      numberWormeriesSlider.noUiSlider.on("change", function(value) {
        numberWormeriesFlare.val(value)
        console.log(value)
        numberWormeriesFlare.get(0).dispatchEvent(evFlareInput);
        Open(control, process)
        Open(control2, process2)
        $("#output .hexGrid").addClass("fadeIn");
      })
    } else if (isOpened(control, process, ev) && ev.target.id!= (control + 'Control')) {
      $("#" + control + "-Control #" + control + "Control").remove()
      Close(control, process)
    }
  }

  var gardenSurfaceFlare = $('#flare-component-10')

  function HandleGarden(control, process, ev) {
    if (isClosed(control, process, ev)) {
      Open(control, process)
      $("#" + control + "-Control").append('<div id="' + control + 'Control" class="control-layer range"><p id="' + control + 'Control">Garden Surface (m2)</p><div id="' + control + 'Control" class="slider"></div></div>')
      var gardenSurfaceSlider = $("#" + control + "-Control div#" + control + "Control div.slider").get(0)
      noUiSlider.create(gardenSurfaceSlider, {
      	start: [gardenSurfaceFlare.val()],
        step: 1,
      	tooltips: [true],
        range: {
      		'min': 0,
      		'max': 100
      	},
        format: wNumb({decimals: 0})
      });
      gardenSurfaceSlider.noUiSlider.on("change", function(value) {
        gardenSurfaceFlare.val(value)
        console.log(value)
        gardenSurfaceFlare.get(0).dispatchEvent(evFlareInput);
        Open(control, process)
        // Open(control2, process2)
        $("#output .hexGrid").addClass("fadeIn");
      })
    } else if (isOpened(control, process, ev) && ev.target.id!= (control + 'Control')) {
      $("#" + control + "-Control #" + control + "Control").remove()
      Close(control, process)
    }
  }

  var roofSurfaceFlare = $('#flare-component-11')

  function HandleCollecting(control, process, ev) {
    console.log(control)
    console.log(process)
    console.log(ev)
    if (isClosed(control, process, ev)) {
      Open(control, process)
      $("#" + control + "-Control").append('<div id="' + control + 'Control" class="control-layer range"><p id="' + control + 'Control"># block roofs</p><div id="' + control + 'Control" class="slider"></div></div>')
      var roofSurfaceSlider = $("#" + control + "-Control div#" + control + "Control div.slider").get(0)
      noUiSlider.create(roofSurfaceSlider, {
        start: [roofSurfaceFlare.val()],
        step: 1,
        tooltips: [true],
        range: {
          'min': 0,
          'max': 8
        },
        format: wNumb({decimals: 0})
      });
      roofSurfaceSlider.noUiSlider.on("change", function(value) {
        roofSurfaceFlare.val(value)
        console.log(value)
        roofSurfaceFlare.get(0).dispatchEvent(evFlareInput);
        Open(control, process)
        // Open(control2, process2)
        $("#output .hexGrid").addClass("fadeIn");
      })
    } else if (isOpened(control, process, ev) && ev.target.id!= (control + 'Control')) {
      $("#" + control + "-Control #" + control + "Control").remove()
      Close(control, process)
    }
  }

  var sharingHouseholdsFlare = $('#flare-component-12')

  function HandleSharing(control, process, ev) {
    console.log(control)
    console.log(process)
    console.log(ev)
    if (isClosed(control, process, ev)) {
      Open(control, process)
      $("#" + control + "-Control").append('<div id="' + control + 'Control" class="control-layer range"><p id="' + control + 'Control"># sharing flats</p><div id="' + control + 'Control" class="slider"></div></div>')
      var sharingHouseholdSlider = $("#" + control + "-Control div#" + control + "Control div.slider").get(0)
      noUiSlider.create(sharingHouseholdSlider, {
        start: [sharingHouseholdsFlare.val()],
        step: 1,
        tooltips: [true],
        range: {
          'min': 0,
          'max': 121
        },
        format: wNumb({decimals: 0})
      });
      sharingHouseholdSlider.noUiSlider.on("change", function(value) {
        sharingHouseholdsFlare.val(value)
        console.log(value)
        sharingHouseholdsFlare.get(0).dispatchEvent(evFlareInput);
        Open(control, process)
        // Open(control2, process2)
        $("#output .hexGrid").addClass("fadeIn");
      })
    } else if (isOpened(control, process, ev) && ev.target.id!= (control + 'Control')) {
      $("#" + control + "-Control #" + control + "Control").remove()
      Close(control, process)
    }
  }
  //
  // function ToggleCompactor(control, process, control2, process2, ev) {
  //   // if (ev.target.id=="compactorControl" && toggleCompactorFlare.val() == '0' ) {
  //   //   $("#" + control + "-Control div#compactorControl").addClass("on");
  //   //   toggleCompactorFlare.val('0.3')
  //   //   toggleCompactorFlare.get(0).dispatchEvent(evFlareInput);
  //   //   Open(control, process)
  //   //   Open(control2, process2)
  //   //   $("#" + control + "-Control div p.toggle").replaceWith('<p id="compactorControl" class="toggle">On (30%)</p>')
  //   //   $("#output .hexGrid").addClass("fadeIn");
  //   // } else if (ev.target.id=="compactorControl" && !toggleCompactorFlare.val() == '0') {
  //   //   $("#" + control + "-Control div#compactorControl").removeClass("on");
  //   //   toggleCompactorFlare.val('0')
  //   //   toggleCompactorFlare.get(0).dispatchEvent(evFlareInput);
  //   //   Open(control, process)
  //   //   Open(control2, process2)
  //   //   $("#" + control + "-Control div p.toggle").replaceWith('<p id="compactorControl" class="toggle">Off (0%)</p>')
  //   //   $("#output .hexGrid").addClass("fadeIn");
  //   // }
  // }

  var hexes = $("#grid").hammer({domEvents:true})

  hexes.on('tap', '.hex', function(ev) {
    var scenario = $("#output .processes").attr('id');
    if (ev.target.id) {
      console.log(scenario)
      switch(scenario) {
        case "Calculator.Model.RainwaterHarvestingTank":
          HandleRainwaterTank("Eating-0", "Tank", ev)
          Handle("Shopping-0", "Rainfall", ev)
          Handle("ManagedWaste-0", "Wastewater", ev)
          break;
        // case "Calculator.Model.EatingOnly":
        //   HandleEating("Eating-0", "Eating", ev)
        //   Handle("Shopping-0", "Shopped Food", ev)
        //   Handle("ManagedWaste-0", "Managed Waste", ev)
        //   break;
        // case "Calculator.Model.EatingBinning":
        //   HandleEating("Eating", "Eating", ev)
        //   Handle("Shopping", "Shopped Food", ev)
        //   Handle("Garden", "Managed Waste", ev)
        //   HandleCompactor("Composting", "Binning", "Garden", "Managed Waste", ev)
        //   break;
        // case "Calculator.Model.EatingBinningWormComposting":
        //   $("#Binning-Control").addClass("side");
        //   HandleEating("Eating", "Eating", ev)
        //   HandleCompactor("Binning", "Binning", "ManagedWaste", "Managed Waste", ev)
        //   Handle("Shopping", "Shopped Food", ev)
        //   Handle("ManagedWaste", "Managed Waste", ev)
        //   HandleWormery("Composting", "Wormery", ev)
        //   break;
        // case "Calculator.Model.EatingBinningWormCompostingFoodGardening":
        //   $("#Binning-Control").addClass("side");
        //   $("#Garden-Control").addClass("top");
        //   HandleEating("Eating", "Eating", ev)
        //   HandleCompactor("Binning", "Binning", "ManagedWaste", "Managed Waste", ev)
        //   Handle("Shopping", "Shopped Food", ev)
        //   Handle("ManagedWaste", "Managed Waste", ev)
        //   HandleWormery("Composting", "Wormery", "Garden", "Food Garden", ev)
        //   HandleGarden("Garden", "Food Garden", ev)
        //   break;
        // case "Calculator.Model.EatingBinningWormCompostingFoodGardenWatering":
        //   HandleEating("Eating", "Eating", ev)
        //   HandleCompactor("Binning", "Binning", "ManagedWaste", "Managed Waste", ev)
        //   Handle("Shopping", "Shopped Food", ev)
        //   Handle("ManagedWaste", "Managed Waste", ev)
        //   HandleGarden("Garden", "Food Garden", ev)
        //   HandleCollecting("RainwaterCollecting", "Rainwater Collection", ev)
        //   HandleWormery("Composting", "Wormery", "Garden", "Food Garden", ev)
        //   break;
        // case "Calculator.Model.EatingBinningWormCompostingFoodGardenRainwater":
        //   $("#Binning-Control").addClass("side");
        //   $("#Garden-Control").addClass("top");
        //   HandleEating("Eating", "Eating", ev)
        //   HandleCompactor("Binning", "Binning", "ManagedWaste", "Managed Waste", ev)
        //   Handle("Shopping", "Shopped Food", ev)
        //   Handle("ManagedWaste", "Managed Waste", ev)
        //   HandleGarden("Garden", "Food Garden", ev)
        //   HandleCollecting("RainwaterCollecting", "Rainwater Collection", ev)
        //   HandleWormery("Composting", "Wormery", "Garden", "Food Garden", ev)
        //   break;
        // case "Calculator.Model.EatingBinningFoodSharing":
        //   $("#Binning-Control").addClass("side");
        //   $("#Garden-Control").addClass("top");
        //   HandleEating("Eating", "Eating", ev)
        //   HandleCompactor("Binning", "Binning", "ManagedWaste", "Managed Waste", ev)
        //   Handle("Shopping", "Shopped Food", ev)
        //   Handle("ManagedWaste", "Managed Waste", ev)
        //   HandleGarden("Garden", "Food Garden", ev)
        //   HandleWormery("Composting", "Wormery", ev)
        //   break;
        // case "Calculator.Model.EatingBinningWormCompostingFoodSharing":
        //   $("#Binning-Control").addClass("side");
        //   $("#Garden-Control").addClass("top");
        //   Handle("Eating", "Eating", ev)
        //   HandleCompactor("Binning", "Binning", "ManagedWaste", "Managed Waste", ev)
        //   Handle("Shopping", "Shopped Food", ev)
        //   Handle("ManagedWaste", "Managed Waste", ev)
        //   HandleSharing("Rainwater", "Food Sharing", ev)
        //   HandleGarden("Garden", "Food Garden", ev)
        //   HandleWormery("Composting", "Wormery", "Garden", "Food Garden", ev)
        //   break;
        default:
          if (ev.target.id) $("#output .hexGrid #"+ ev.target.id.split("-") + " a").toggleClass("hover")
      }
    }


    // if (ev.target.id) console.log($("#output .hexGrid #"+ ev.target.id.split("-")[0] + " a"))
  })

  // hexes.on('tap', '.hex div', function(ev) {
  //   var scenario = $("#output .processes").attr('id');
  //   if (ev.target.id) {
  //     switch(scenario) {
  //       case "Calculator.Model.EatingOnly":
  //         // ToggleEating("Eating-0", "Eating", ev)
  //         break;
  //       case "Calculator.Model.EatingBinning":
  //         ToggleCompactor("Composting", "Binning", "Garden", "Managed Waste", ev)
  //         break;
  //       case "Calculator.Model.EatingBinningWormComposting":
  //         ToggleCompactor("Binning", "Binning", "ManagedWaste", "Managed Waste", ev)
  //         break;
  //       case "Calculator.Model.EatingBinningWormCompostingFoodGardening":
  //         ToggleCompactor("Binning", "Binning", "ManagedWaste", "Managed Waste", ev)
  //         break;
  //       case "Calculator.Model.EatingBinningWormCompostingFoodGardenWatering":
  //         ToggleCompactor("Binning", "Binning", "ManagedWaste", "Managed Waste", ev)
  //         break;
  //       case "Calculator.Model.EatingBinningWormCompostingFoodGardenRainwater":
  //         ToggleCompactor("Binning", "Binning", "ManagedWaste", "Managed Waste", ev)
  //         break;
  //       case "Calculator.Model.EatingBinningFoodSharing":
  //         ToggleCompactor("Binning", "Binning", "ManagedWaste", "Managed Waste", ev)
  //         break;
  //       case "Calculator.Model.EatingBinningWormCompostingFoodSharing":
  //         break;
  //       default:
  //         if (ev.target.id) $("#output .hexGrid #"+ ev.target.id.split("-") + " a").toggleClass("hover")
  //     }
  //   }
  //
  //
  //   // if (ev.target.id) console.log($("#output .hexGrid #"+ ev.target.id.split("-")[0] + " a"))
  // })

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

  function setupControls() {
    var scenario = $("#output .processes").attr('id');
    switch(scenario) {
      case "Calculator.Model.EatingOnly":
        $('#eatedFoodRatioLayered').remove()
        break;
      case "Calculator.Model.EatingBinning":
        break;
      case "Calculator.Model.EatingBinningWormComposting":
      case "Calculator.Model.EatingBinningWormCompostingFoodGardening":
      case "Calculator.Model.EatingBinningWormCompostingFoodGardenWatering":
      case "Calculator.Model.EatingBinningWormCompostingFoodGardenRainwater":
      case "Calculator.Model.EatingBinningFoodSharing":
      case "Calculator.Model.EatingBinningWormCompostingFoodSharing":
      case "Calculator.Model.RainwaterHarvestingTank":
      case "Calculator.Model.RainwaterHarvestingDemand":
      case "Calculator.Model.RainwaterHarvestingCollection":
        break;
      default:
        if (ev.target.id) $("#output .hexGrid #"+ ev.target.id.split("-") + " a").toggleClass("hover")
    }
  }


  scaleControl = $('#layer #Scale')
  timeControl = $('#layer #Time')

  scaleControl.on('click', function(event) {

  })

});

// class="ui-input-text ui-body-null ui-corner-all ui-shadow-inset ui-body-c ui-slider-input"
