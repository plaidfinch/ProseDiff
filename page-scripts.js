
jQuery('document').ready(function() {

   jQuery('#submit-button').click(function() {
      var analyzed_HTML = prosediff.core.process(jQuery('#left-analysis').text(),
                                                 jQuery('#right-analysis').text());
      //alert(analyzed_HTML);
      jQuery('#left-analysis').html(analyzed_HTML[0]);
      jQuery('#right-analysis').html(analyzed_HTML[1]);
   });

   jQuery('body').on({
      click: function(){
         jQuery(this).toggleClass("pd-clicked-block");
      },
      mouseenter: function(){
         jQuery(this).addClass("pd-hovered-block");
         blocks = jQuery(this).attr("class")
                              .split(" ")
                              .filter(function(s) { return s.match("pd-block-") != null });
         blocks.forEach((function(blockID) {
            jQuery('.' + blockID).addClass("pd-active-block");
         }));
      },
      mouseleave: function(){
         jQuery(this).removeClass("pd-hovered-block");
         blocks = jQuery(this).attr("class")
                              .split(" ")
                              .filter(function(s) { return s.match("pd-block-") != null });
         blocks.forEach((function(blockID) {
            jQuery('.' + blockID).removeClass("pd-active-block");
         }));
      }}, '.pd-move');

});
