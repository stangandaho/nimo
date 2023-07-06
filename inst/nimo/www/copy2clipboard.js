Shiny.addCustomMessageHandler('copybtnUpdate', function(data) {

      var btn = $('button#' + data.id);
      var ico = ' ';

    if(data.hasOwnProperty('label')) {
        btn.html(ico + data.label);
     };

    if(data.hasOwnProperty('data-clipboard-text')) {
      btn.attr('data-clipboard-text', data['data-clipboard-text']);
    };

   if(data.hasOwnProperty('onclick')) {
      btn.attr('onClick', data.onClick);
    };

  if(data.hasOwnProperty('icon')) {
    var ch = btn.children();
    if(ch.length == 0) {
      btn.prepend(data.icon);
    } else {
      btn.find('i').replaceWith(data.icon);
    };
  };

    })