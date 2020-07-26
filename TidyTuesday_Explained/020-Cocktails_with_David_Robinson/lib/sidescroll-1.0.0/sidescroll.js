
var isMobile;

function doResize(event, el, ui) {

    var scale;
    var translationY = 50;
    var topPerc = 50;


    scale =   Math.min(
      ui.body.height / 1000 ,
      ui.body.width / 1700
      );
      
    el.css({
     top: topPerc + "%",
     left: "50%", 
     transform: "translate(-" + translationY + "% , -50%) " + "scale(" + scale + ") ",
     });

}

function set_slide_container_size(event,el,ui){
  el.css({
    height: ui.body.height, /* make a scoche smaller than the body */
    width: ui.body.width,
    margin: "auto"
  });
}


$(document).ready(function(){
  
    // make swipable if mobile/touch screen
    // from https://stackoverflow.com/questions/11381673/detecting-a-mobile-browser
    var touchDevice = (navigator.maxTouchPoints > 0 || 'ontouchstart' in document.documentElement);
    isMobile = /iPhone|iPod|iPad|Android/i.test(navigator.userAgent);

		$('.slide_master').slick({
			accessibility: true,
			dots: true,
			infinite: false,
			speed: 250,
			slidesToShow: 1,
			centerMode: true,
			variableWidth: true,
			swipe: false,
			arrows: !touchDevice,
			touchThreshold: 8
			});

		var $slide_wrapper = $(".slide_master_wrapper");
		var $slide_master = $(".slide_master");
	  var $wrapper = $("body");

	  set_slide_container_size(null,
		  $slide_wrapper,
		  { body: {
		      height: window.innerHeight,
		      width: window.innerWidth
		    }
		});

		doResize(null,
		  $slide_master,
		  { body: {
		      height: window.innerHeight,
		      width: window.innerWidth
		    }
		  });


	$('.slide_master').on('afterChange', function(event, slick, currentSlide, nextSlide){
    var generic_slide = $(".slick-current").find(".slide_container").find(".generic");
    generic_slide.css('visibility','visible');
  });
  
  $(this).on('wheel', (function(e) {
    e.preventDefault();
  
    if (e.originalEvent.deltaY < 0) {
      $('.slide_master').slick('slickPrev');
    } else {
      $('.slide_master').slick('slickNext');
    }
  }));
  
 $(this).keydown(function(e) {
    var code = (e.keyCode ? e.keyCode : e.which);
    if (code === 32 | code === 34 | code === 39 | code === 40) {
      $('.slide_master').slick('slickNext');
    }
    if (code === 33 | code === 37 | code === 38){
      $('.slide_master').slick("slickPrev");
    }
  });

  $('.slide_master').on( "swipeleft", function(e){
      $('.slide_master').slick('slickNext');
  });
  
  $('.slide_master').on( "swiperight", function(e){
      $('.slide_master').slick('slickPrev');
  });


});

$(window).resize(function() {
  	var $slide_wrapper = $(".slide_master_wrapper");
		var $slide_master = $(".slide_master");
	  var $wrapper = $("body");

		doResize(null,
		  $slide_master,
		  { body: {
		      height: window.innerHeight,
		      width: window.innerWidth
		    }
		  });
		set_slide_container_size(null,
		  $slide_wrapper,
		  { body: {
		      height: window.innerHeight,
		      width: window.innerWidth
		    }
		});
});






